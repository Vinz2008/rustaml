#include <stdint.h>
#include <stdlib.h>
#include <stdio.h>
#include <stdbool.h>
#include <string.h>
#include <assert.h>
#include <time.h>
#include <stdarg.h>
#include <math.h>

#if defined(__unix__) || defined(__unix) || \
        (defined(__APPLE__) && defined(__MACH__))
#define SYSTEM_UNIX
#endif

#if defined __has_include
#if __has_include(<inttypes.h>)
#include <inttypes.h>
#else
#define PRId64 "ld"
#endif

#else
#include <inttypes.h>
#endif

// TODO
#ifdef _GC_
#include <gc.h>
#define MALLOC(size) GC_malloc(size)
#define REALLOC(ptr, new_size) GC_realloc(ptr, new_size)
#define FREE(ptr) GC_free(ptr)

void __gc_init(){
    GC_INIT();
}

#else
#define MALLOC(size) malloc(size)
#define FREE(ptr) free(ptr)
#define REALLOC(ptr, new_size) realloc(ptr, new_size)
#endif

enum TypeTag {
    INT_TYPE = 0,
    FLOAT_TYPE = 1,
    BOOL_TYPE = 2,
    FUNCTION_TYPE = 3,
    STR_TYPE = 4,
    LIST_TYPE = 5,
};

typedef uint64_t Val;

// do a memcpy to avoid UB with this type punning, the compiler will optimize this, finger crossed
// the block expr syntax is supported on clang and gcc only, create an alternative on cl.exe ? (TODO?)
#define INTO_TYPE(t, var) ({ \
        t _dst;\
        memcpy(&_dst, &(var), sizeof(t)); \
        _dst; \
    }) \

struct ListNode {
    uint8_t type_tag;
    Val val; // can be a i64, a ptr or a f64 depending on type_tag
    struct ListNode* next; // if empty null 
};

uint8_t __str_cmp(const char* s1, const char* s2){
    while (*s1 != '\0' && *s1 == *s2){
        s1++;
        s2++;
    }

    return *s1 == *s2;
}

char* __str_append(const char* s1, const char* s2){
    size_t len_s1 = strlen(s1);
    size_t len_s2 = strlen(s2);
    char* ret = MALLOC(len_s1 + len_s2 + 1);
    memcpy(ret, s1, len_s1);
    memcpy(ret + len_s1, s2, len_s2);
    ret[len_s1 + len_s2] = '\0';
    return ret;
}

static struct ListNode* list_node_init(uint8_t type_tag, Val val){
    struct ListNode* l = MALLOC(sizeof(struct ListNode));
    l->type_tag = type_tag;
    l->val = val;
    l->next = NULL;
    return l;
}

struct ListNode* __list_node_append(struct ListNode* list, uint8_t type_tag, Val val){
    if (list == NULL){
        return list_node_init(type_tag, val);
    }
    
    // TODO : add asserts
    struct ListNode* current = list;
    while (current != NULL && current->next != NULL){
        current = current->next;
    }

    current->next = list_node_init(type_tag, val);
    return list;
    
}


static Val list_node_val(struct ListNode* list){
    return list->val;
}


static struct ListNode* list_node_tail(struct ListNode* list){
    return list->next;
}

static bool list_node_cmp(uint8_t tag1, Val val1, uint8_t tag2, Val val2){
    if (tag1 != tag2){
        return false;
    }
    
    if (tag1 == INT_TYPE && tag2 == INT_TYPE) {
        return val1 == val2;
    } else if (tag1 == FLOAT_TYPE && tag2 == FLOAT_TYPE){
        // no need to convert to double normally, the only difference with the standard is that some NaNs will be the same even though every comparison with NaNs should be false, but no one cares (I hope)
        return val1 == val2;
    } else if (tag1 == BOOL_TYPE && tag2 == BOOL_TYPE){
        uint8_t bool1 = INTO_TYPE(uint8_t, val1);
        uint8_t bool2 = INTO_TYPE(uint8_t, val2);
        return bool1 == bool2;
    } else if (tag1 == STR_TYPE && tag2 == STR_TYPE){
        char* str1 = INTO_TYPE(char*, val1);
        char* str2 = INTO_TYPE(char*, val2);
        return __str_cmp(str1, str2);
    } else if (tag1 == LIST_TYPE && tag2 == LIST_TYPE){
        struct ListNode* list1 = INTO_TYPE(struct ListNode*, val1);
        struct ListNode* list2 = INTO_TYPE(struct ListNode*, val2);
        return list_node_cmp(list1->type_tag, list1->val, list2->type_tag, list2->val);
    }

    fprintf(stderr, "ERROR : WRONG TAGS IN LIST IN CMP (BUG IN COMPILER  \?\?)\n");
    exit(1);
}


// TODO ? generate this with LLVM ?
uint8_t __list_cmp(struct ListNode* list1, struct ListNode* list2){
    while (list1 != NULL && list2 != NULL){
        if (!list_node_cmp(list1->type_tag, list1->val, list2->type_tag, list2->val)){
            return false;
        }
        list1 = list1->next;
        list2 = list2->next;
    }

    return list1 == list2; // both are NULL
}

const char* __bool_to_str(bool b){
    if (b) {
        return "true";
    } else {
        return "false";
    }
}

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

void fallback_seed() {

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

#elif __STDC__HOSTED__ == 0 && (defined(__x86_64__) || defined(__i386__))
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
    return x
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


static void list_print_no_new_line(struct ListNode* list);

// TODO : transform in the future into a print_val function
static void list_node_print(uint8_t tag, Val val){
    if (tag == INT_TYPE) {
        printf("%" PRId64 "", INTO_TYPE(int64_t, val));
    } else if (tag == FLOAT_TYPE){
        printf("%f", INTO_TYPE(double, val));
    } else if (tag == BOOL_TYPE){
        printf("%s", __bool_to_str(INTO_TYPE(bool, val)));
    } else if (tag == STR_TYPE){
        printf("%s", INTO_TYPE(char*, val));
    } else if (tag == LIST_TYPE){
        list_print_no_new_line(INTO_TYPE(struct ListNode*, val));
    } else {
        fprintf(stderr, "ERROR : WRONG TAGS IN LIST IN PRINT (BUG IN COMPILER  \?\?)\n");
        exit(1);
    }
}

static void list_print_no_new_line(struct ListNode* list){
    bool first = true;
    printf("[");
    while (list != NULL){
        if (!first){
            printf(", ");
        } 
        list_node_print(list->type_tag, list->val);
        list = list->next;
        first = false;
    }
    printf("]");
}

void __list_print(struct ListNode* list){
    list_print_no_new_line(list);
    printf("\n");
}

static void str_append_with_realloc(char** s, size_t* current_len, size_t* current_capacity, char c){
    if (*current_len + 1 >= *current_capacity){
        *current_capacity = *current_capacity * 1.5;
        *s = REALLOC(*s, *current_capacity);
    }
    //printf("current_len : %d, current_capacity : %d\n", *current_len, *current_capacity);
    (*s)[*current_len] = c;
    *current_len += 1;
}

static uint64_t absolute(int64_t i){
    uint64_t u = (uint64_t)i;
    if (i < 0){
        return ~u + 1; // equivalent to doing  ((uint64_t)(i + 1)) + 1 to hande INT_MIN where the - would do an overflow
    } else {
        return u;
    }
}

static int digit_nb_unsigned(uint64_t u){
    if (u == 0){
        return 1;
    }
    int d_nb = (int)log10((double)u);
    if (d_nb < 0) d_nb = 0; // clamp value to help LLVM optimizations (wrong range from LLVM in not inlined function) 
    return d_nb + 1;
}

// TODO : optimize this
static int digit_nb(int64_t i){
    uint64_t u = absolute(i);
    return digit_nb_unsigned(u);
}

// TODO : optimize this
static void int_to_string_impl(char* buf, int64_t integer, int digit_number){
    int start = 0;
    if (integer < 0){
        buf[0] = '-';
        start = 1;
    }
    uint64_t u = absolute(integer);
    for (int i = 0; i < digit_number; i++){
        int digit = u % 10;
        buf[start + digit_number - i - 1] = digit + '0';
        u = u/10;
    }
}

/*static int max_double_digit_nb(){
    return 32; // 24 would be safe, but use 32 because it will be cheaper on bdwgc
    // could also do floor(log10(d)) but would use more CPU
}*/

#define MAX_DIGIT_POSSIBLE_DOUBLE 32 // 24 would be safe, but use 32 because it will be cheaper on bdwgc
    // could also do floor(log10(d)) in a function but would use more CPU

// TODO : implement ryu or grisu implementation (or both with grisu with ryu as fallback ?)
static size_t double_to_string_impl(char* buf, double d, int max_char_nb){
    if (d != d){
        // NAN
        buf[0] = 'n';
        buf[1] = 'a';
        buf[2] = 'n';
        return 3;
    }

    if (d == INFINITY){
        buf[0] = 'i';
        buf[1] = 'n';
        buf[2] = 'f';
        return 3;
    }

    if (d == -INFINITY){
        buf[0] = '-';
        buf[1] = 'i';
        buf[2] = 'n';
        buf[3] = 'f';
        return 4;
    }

    size_t pos = 0;

    if (d < 0){
        buf[pos] = '-';
        pos++;
        d = -d;
    }


    uint64_t int_part = (uint64_t)d;
    int int_part_digit_nb = digit_nb_unsigned(int_part);
    double frac_part = d - (double)int_part;

    int_to_string_impl(buf + pos, (int64_t)int_part, int_part_digit_nb);
    pos += int_part_digit_nb;

    if (frac_part > 0.0){
        buf[pos] = '.';
        pos++;

        while (pos < max_char_nb) {
            frac_part *= 10;
            int digit = (int)frac_part;
            buf[pos] = '0' + digit;
            pos++;
            frac_part -= digit;
        }
    }
    return pos;
}

static void list_format(char** buf, struct ListNode* list, size_t* current_len, size_t* current_capacity);
static void ensure_size_string(char** s, size_t* current_capacity, size_t size);

// TODO : deduplicate this code with the normal format
static void list_node_format(char** buf, uint8_t tag, Val val, size_t* current_len, size_t* current_capacity){
    if (tag == INT_TYPE) {
        int64_t i = INTO_TYPE(int64_t, val);
        int digit_number = digit_nb(i);
        size_t buf_size = (i < 0) ? digit_number + 1 : digit_number;
        ensure_size_string(buf, current_capacity, *current_len + buf_size);
        int_to_string_impl(*buf + *current_len, i, digit_number);
        *current_len += buf_size;
    } else if (tag == FLOAT_TYPE){
        double d = INTO_TYPE(double, val);
        ensure_size_string(buf, current_capacity, *current_len + MAX_DIGIT_POSSIBLE_DOUBLE);
        *current_len += double_to_string_impl(*buf + *current_len, d, MAX_DIGIT_POSSIBLE_DOUBLE);
    } else if (tag == BOOL_TYPE){
        bool b = INTO_TYPE(bool, val);
        const char* bool_str = __bool_to_str(b);
        size_t bool_str_len = strlen(bool_str);
        ensure_size_string(buf, current_capacity, *current_len + bool_str_len);
        memcpy(*buf, bool_str, bool_str_len);
        *current_len += bool_str_len;
    } else if (tag == STR_TYPE){
        char* s = INTO_TYPE(char*, val);
        size_t len_s = strlen(s);
        ensure_size_string(buf, current_capacity, *current_len + len_s);
        memcpy(*buf, s, len_s);
        *current_len += len_s;
    } else if (tag == LIST_TYPE){
        list_format(buf, INTO_TYPE(struct ListNode*, val), current_len, current_capacity);
    } else {
        fprintf(stderr, "ERROR : WRONG TAGS IN LIST IN FORMAT (%d) (BUG IN COMPILER  \?\?)\n", tag);
        exit(1);
    }
}

static void list_format(char** buf, struct ListNode* list, size_t* current_len, size_t* current_capacity){
    bool first = true;
    (*buf)[*current_len] = '[';
    (*current_len)++;
    
    while (list != NULL){
        if (!first){
            (*buf)[*current_len] = ',';
            (*current_len)++;
            (*buf)[*current_len] = ' ';
            (*current_len)++;
        } 
        list_node_format(buf, list->type_tag, list->val, current_len, current_capacity);
        list = list->next;
        first = false;
    }

    (*buf)[*current_len] = ']';
    (*current_len)++;
}


static void ensure_size_string(char** s, size_t* current_capacity, size_t size){
    if (size >= *current_capacity){
        *current_capacity = size * 1.5; // do I need this factor here (TODO ?)
        *s = REALLOC(*s, *current_capacity);
    }
}

static char* vformat_string(char* format, va_list va){
    size_t current_capacity = 5;
    size_t current_len = 0;
    char* str = MALLOC(sizeof(char) * current_capacity);
    int digit_number;
    size_t buf_size;
    int64_t i;
    double d;
    char* s;
    struct ListNode* l;
    size_t written_amount;
    while (*format != '\0'){
        switch (*format) {
            case '%':
                format++;
                switch (*format){
                    case 'd':
                        i = va_arg(va, int64_t);
                        digit_number = digit_nb(i);
                        buf_size = (i < 0) ? digit_number + 1 : digit_number;
                        ensure_size_string(&str, &current_capacity, current_len + buf_size);
                        int_to_string_impl(str + current_len, i, digit_number);
                        current_len += buf_size;
                        break;
                    case 'f':
                        d = va_arg(va, double);
                        // TODO : create custom function for this
                        buf_size = MAX_DIGIT_POSSIBLE_DOUBLE;
                        ensure_size_string(&str, &current_capacity, current_len + buf_size);
                        written_amount = double_to_string_impl(str + current_len, d, buf_size);
                        current_len += written_amount;
                        break;
                    case 's':
                        s = va_arg(va, char*);
                        size_t str_len = strlen(s);
                        ensure_size_string(&str, &current_capacity, current_len + str_len);
                        memcpy(str + current_len, s, str_len);
                        current_len += str_len;
                        break;
                    case 'l':
                        l = va_arg(va, struct ListNode*);
                        list_format(&str, l, &current_len, &current_capacity);
                        break;
                    default:
                        fprintf(stderr, "ERROR : Unknown format\n");
                        exit(1);
                }
                break;
            default:
                str_append_with_realloc(&str, &current_len, &current_capacity, *format);
                break;
        }
        format++;
    }
    ensure_size_string(&str, &current_capacity, current_len + 1);
    str[current_len] = '\0';
    return str;
}

char* __format_string(char* format, ...){
    va_list va;
    va_start(va, format);
    char* s = vformat_string(format, va);
    va_end(va);
    return s;
}

/*int main(){
    char* s = __format_string("test before %d test after", 123);
    puts(s);
    FREE(s);
}*/




// TODO : maybe add function to print values with the type tag
