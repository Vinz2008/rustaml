#ifndef STATIC
#ifndef STD_BUILD
#define STATIC
#include "prelude.h"
#endif
#endif

// using PCG64
typedef struct {
    bool is_seeded;
    __uint128_t state;
    __uint128_t inc;
} RandState;

static RandState rand_state = {false, 0, 0};


#ifdef _WIN32
    #include <windows.h>
    #include <sys/timeb.h>
#else
    #include <sys/time.h>
    #include <unistd.h>
#endif

#ifndef FREESTANDING
static void fallback_seed() {

#ifdef _WIN32
    time_t t = time(NULL);
    uint64_t time1 = (uint64_t)t;

    // Millisecond-precision time
    struct _timeb tb;
    _ftime(&tb);
    uint64_t time2 = ((uint64_t)tb.time << 16) ^ (uint64_t)tb.millitm;

    // Use address entropy and Windows-specific performance counter
    uint64_t entropy1 = (uintptr_t)&tb;
    LARGE_INTEGER perf;
    QueryPerformanceCounter(&perf);
    uint64_t entropy2 = (uint64_t)perf.QuadPart;
#else
    time_t t = time(NULL);
    uint64_t time1 = (uint64_t)t;

    // Microsecond-precision time
    struct timeval tv;
    gettimeofday(&tv, NULL);
    uint64_t time2 = ((uint64_t)tv.tv_sec << 20) ^ tv.tv_usec;

    // Address entropy and clock ticks
    uint64_t entropy1 = (uintptr_t)&tv;
    uint64_t entropy2 = (uint64_t)clock();
#endif

    rand_state.state = time1 ^ (entropy1 << 21) ^ (time2 << 7);
    rand_state.inc = time2 ^ (entropy2 >> 3) ^ (entropy1 >> 11);

}
#endif

// if unix
#ifdef SYSTEM_UNIX
#include <fcntl.h>

static void seed_random(){
    int fd = open("/dev/urandom", O_RDONLY);

    if (fd >= 0) {
        read(fd, &rand_state.state, sizeof(sizeof(uint64_t) * 2));
        close(fd);
    } else {
        fallback_seed();
    }
}

#elif defined(FREESTANDING) && (defined(__x86_64__) || defined(__i386__))
// freestanding
#ifdef __i386
extern __inline__ uint64_t rdtsc(void) {
  uint64_t x;
  __asm__ volatile ("rdtsc" : "=A" (x));
  return x;
}
#elif defined __x86_64__
extern __inline__ uint64_t rdtsc(void) {
  uint64_t a, d;
  __asm__ volatile ("rdtsc" : "=a" (a), "=d" (d));
  return (d<<32) | a;
}
#endif
// A simple mixing function to reduce correlation between consecutive rdtsc values
static inline uint64_t mix(uint64_t x) {
    x ^= x >> 33;
    x *= 0xff51afd7ed558ccdULL;
    x ^= x >> 33;
    x *= 0xc4ceb9fe1a85ec53ULL;
    x ^= x >> 33;
    return x;
}

static void seed_random(){
    uint64_t r[8];
    for (int i = 0; i < 8; i++) {
        r[i] = mix(rdtsc());
        // Insert small delay or dummy computation if needed to increase entropy
        for (volatile int j = 0; j < 100; j++) {}  // crude busy wait
    }

    // Combine values to form two 128-bit numbers:
    *(uint64_t*)(&rand_state.state + sizeof(uint64_t)) = r[0] ^ r[1];
    *(uint64_t*)(&rand_state.state) = r[2] ^ r[3];
    *(uint64_t*)(&rand_state.inc + sizeof(uint64_t)) = r[4] ^ r[5];
    *(uint64_t*)(&rand_state.inc) = r[6] ^ r[7];
}

#else
static void seed_random(){
    fallback_seed();
}

#endif

#define PCG_INCREMENT ((__uint128_t)6364136223846793005ULL << 64 | (__uint128_t)1442695040888963407ULL)

uint64_t pcg_random(){
    /* cheap (half-width) multiplier */
	const uint64_t mul = 15750249268501108917ULL;
	/* linear congruential generator */
	__uint128_t state = rand_state.state;
	rand_state.state = state * mul + rand_state.inc;
	/* DXSM (double xor shift multiply) permuted output */
	uint64_t hi = (uint64_t)(state >> 64);
	uint64_t lo = (uint64_t)(state | 1);
	hi ^= hi >> 32;
	hi *= mul;
	hi ^= hi >> 48;
	hi *= lo;
    return hi;
}

int64_t __rand(){
    if (!rand_state.is_seeded){
        seed_random();
        /* must ensure rng.inc is odd */
        const __uint128_t inc = PCG_INCREMENT;
        rand_state.inc = (rand_state.inc > 0) ? (rand_state.inc << 1) | 1 : inc;
        rand_state.state += inc;
        pcg_random();
        rand_state.is_seeded = true;
    }
    uint64_t res = pcg_random();
	return INTO_TYPE(int64_t, res);
}