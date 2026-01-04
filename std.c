#if __STDC_HOSTED__ == 0
#define FREESTANDING
#endif

#include <stdint.h>
#include <stddef.h>
#include <stdarg.h>
#include <stdbool.h>
#include <limits.h>

#if defined __has_include
#if __has_include(<inttypes.h>)
#include <inttypes.h>
#else
#define PRId64 "ld"
#endif

#else
#include <inttypes.h>
#endif

#ifndef FREESTANDING
// NOT FREESTANDING
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <assert.h>
#include <time.h>
#include <math.h>

#if defined(__unix__) || defined(__unix) || \
        (defined(__APPLE__) && defined(__MACH__))
#define SYSTEM_UNIX
#endif


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

#else
// FREESTANDING MODE

__attribute__((weak)) void* memcpy(void* dest, const void* src, size_t size){
    unsigned char* dest_c = dest;
    const unsigned char* src_c = src;
    size_t idx = 0;
    while (idx < size){
        *dest_c++ = *src_c++;
        idx++;
    }
    return dest;
}

#define MALLOC(size) malloc(size)
#define FREE(ptr) free(ptr)
#define REALLOC(ptr, new_size) realloc(ptr, new_size)

struct Metadata {
    size_t size;
};

#define MALLOC_SIZE_WEAK (1024 * 1024) // 1Mib

// weak symbol, can be overriden in freestanding
__attribute__((weak)) void* malloc(size_t size){
    static char buf[MALLOC_SIZE_WEAK] = {};
    static size_t pos = 0;

    size_t alloc_size = sizeof(struct Metadata) + size;

    size_t align = _Alignof(max_align_t);
    pos = (pos + align - 1) & ~(align - 1);
    if (pos >= MALLOC_SIZE_WEAK){
        __builtin_trap();
        while (1){}
    }
    void* ptr = buf + pos;
    struct Metadata* metadata = (struct Metadata*) ptr;
    metadata->size = size;
    pos += alloc_size;
    return (void*)(ptr + sizeof(struct Metadata));
}

__attribute__((weak)) void* realloc(void* ptr, size_t size){
    void* new_buf = malloc(size);
    struct Metadata* metadata = (struct Metadata*)(ptr - sizeof(struct Metadata));
    size_t old_size = metadata->size;
    memcpy(new_buf, ptr, old_size < size ? old_size : size);
    return new_buf;
}

__attribute__((weak)) void free(void* ptr){
    (void)ptr;
}

// https://git.musl-libc.org/cgit/musl/tree/src/string/strlen.c
#define ONES ((size_t)-1/UCHAR_MAX)
#define HIGHS (ONES * (UCHAR_MAX/2+1))
#define HASZERO(x) ((x)-ONES & ~(x) & HIGHS)

__attribute__((weak)) size_t strlen(const char* s){
    const char *a = s;
#ifdef __GNUC__
	typedef size_t __attribute__((__may_alias__)) word;
    while (((uintptr_t)s % sizeof(size_t)) != 0){
        if (!*s){
            return s-a;
        }
        s++;
    }
    const word* w = (const void*)s;
    while (!HASZERO(*w)){
        w++;
    }

	s = (const void *)w;
#endif
    while (*s){
        s++;
    }

	return s-a;
}


#define INFINITY __builtin_inff()

typedef struct {} FILE;
static FILE stderr_impl;
__attribute__((weak)) FILE* stderr = &stderr_impl;


__attribute__((weak)) int fprintf(FILE* stream, const char* format, ... ){
    (void)stream;
    (void)format;
    return 0;
}


void exit(int exit_code)  __attribute__ ((__noreturn__));

__attribute__((weak)) void exit(int exit_code) {
    (void)exit_code;
    __builtin_trap();
    while(1){}
}

__attribute__((noreturn)) void __stack_chk_fail(void) {
    __builtin_trap();
    while (1) { }
}


__attribute__((weak)) int printf(const char* format, ...){
    (void)format;
    return 0;
}

#ifndef assert
#ifdef	NDEBUG
#define assert(e) ((void)0)
#else
__attribute__((noinline))
static void assert_fail(){
    __builtin_trap();
    while (1){}
}
#define assert(e) ((e) ? ((void)0) : assert_fail ())
#endif
#endif

#endif

#define ALLOC_ERROR(...) ({ \
        fprintf(stderr, "ALLOC ERROR in %s:", __func__); \
        fprintf(stderr, __VA_ARGS__); \
        fprintf(stderr, "\n"); \
        exit(1); \
    }) 

#define ASSERT_BOOL(id_bool) assert(id_bool < 2 && #id_bool " should be true or false")
#define ASSERT_NOT_NULL(not_null) assert(not_null && #not_null " should not be NULL")

enum TypeTag {
    INT_TYPE = 0,
    FLOAT_TYPE = 1,
    BOOL_TYPE = 2,
    FUNCTION_TYPE = 3,
    STR_TYPE = 4,
    LIST_TYPE = 5,
    CHAR_TYPE = 6,
};

typedef uint64_t Val;

// do a memcpy to avoid UB with this type punning, the compiler will optimize this, finger crossed
// the block expr syntax is supported on clang and gcc only
#define INTO_TYPE(t, var) ({ \
        t _dst;\
        memcpy(&_dst, &(var), sizeof(t)); \
        _dst; \
    }) \


// TODO : optimize by putting the type_tag in a List struct which will have the tag and just the head of the list (so the tag is only stored one time -> 24 to 16 bytes for each node)
// TODO : put a list len in it ?
struct ListNode {
    struct ListNode* next; // if empty null
    Val val; // can be a i64, a ptr or a f64 depending on type_tag 
    uint8_t type_tag;
};

uint8_t __str_cmp(const char* s1, const char* s2){
    ASSERT_NOT_NULL(s1 && s2);

    while (*s1 != '\0' && *s1 == *s2){
        s1++;
        s2++;
    }

    return *s1 == *s2;
}

char* __str_append(const char* s1, const char* s2){
    ASSERT_NOT_NULL(s1 && s2);

    size_t len_s1 = strlen(s1);
    size_t len_s2 = strlen(s2);
    char* ret = MALLOC(len_s1 + len_s2 + 1);
    memcpy(ret, s1, len_s1);
    memcpy(ret + len_s1, s2, len_s2);
    ret[len_s1 + len_s2] = '\0';
    return ret;
}

static void _list_node_init(struct ListNode* l, uint8_t type_tag, Val val) {
    l->type_tag = type_tag;
    l->val = val;
    l->next = NULL;
}

static struct ListNode* list_node_init(uint8_t type_tag, Val val) {
    struct ListNode* l = MALLOC(sizeof(struct ListNode));
    if (!l){
        ALLOC_ERROR("ListNode");
    }
    _list_node_init(l, type_tag, val);
    return l;
}

// optimization, improve cache locality
struct ListNode* __list_node_init_static(uint8_t type_tag, Val* vals_static, int64_t len){
    ASSERT_NOT_NULL(vals_static);
    assert(len > 0 && "len should be greater than 0");

    struct ListNode* list_nodes = MALLOC(len * sizeof(struct ListNode));
    if (!list_nodes){
        ALLOC_ERROR("ListNode static");
    }
    for (int64_t i = 0; i < len; i++){
        list_nodes[i] = (struct ListNode){
            .type_tag = type_tag,
            .val = vals_static[i],
            .next = NULL,
        };
        if (i < len-1){
            list_nodes[i].next = list_nodes + i + 1;
        }
    }
    return list_nodes;
}

// appends at the front
struct ListNode* __list_node_append(struct ListNode* list, uint8_t type_tag, Val val){
    struct ListNode* ret = list_node_init(type_tag, val);
    ASSERT_NOT_NULL(ret);

    ret->next = list;
    return ret;
}

struct ListNode* __list_node_append_back(struct ListNode* list, uint8_t type_tag, Val val){
    if (list == NULL){
        return list_node_init(type_tag, val);
    }
    
    struct ListNode* current = list;
    while (current->next != NULL){
        current = current->next;
    }

    current->next = list_node_init(type_tag, val);
    return list;
}

int64_t __list_len(struct ListNode* list){
    int64_t count = 0;
    while (list != NULL){
        list = list->next;
        count++;
    }
    return count;
}

struct ListBuilder {
    struct ListNode* head;
    struct ListNode* tail;
    struct ListNode* nodes_buf; // if preallocated the number of nodes, not NULL, else NULL
    size_t nodes_buf_idx; // only valid if nodes_buf is not null
};

static struct ListBuilder list_builder_init(struct ListNode* nodes_buf){
    return (struct ListBuilder){
        .head = NULL,
        .tail = NULL,
        .nodes_buf = nodes_buf,
        .nodes_buf_idx = 0,
    };
}

static void list_builder_append_back(struct ListBuilder* list_builder, uint8_t type_tag, Val val){
    ASSERT_NOT_NULL(list_builder);
    struct ListNode* new;
    if (list_builder->nodes_buf){
        new = list_builder->nodes_buf + list_builder->nodes_buf_idx;
        _list_node_init(new, type_tag, val);
        list_builder->nodes_buf_idx++;
    } else {
        new = list_node_init(type_tag, val);
    }
    if (list_builder->tail == NULL){
        list_builder->head = new;
        list_builder->tail = new;
    } else {
        list_builder->tail->next = new;
        list_builder->tail = new;
    }
}

// clone the list nodes, but doesn't clone the list vals
static struct ListBuilder clone_list(struct ListNode* list){
    struct ListNode* current = list;
    int64_t list_len = __list_len(list);
    struct ListNode* list_nodes_buf = MALLOC(list_len * sizeof(struct ListNode));
    struct ListBuilder list_builder = list_builder_init(list_nodes_buf);
    
    while (current != NULL){
        list_builder_append_back(&list_builder, current->type_tag, current->val);
        current = current->next;
    }
    return list_builder;
}

struct ListNode* __list_node_merge(struct ListNode* list1, struct ListNode* list2){
    struct ListBuilder list1_cloned_builder = clone_list(list1);
    struct ListNode* list1_cloned_head = list1_cloned_builder.head;
    struct ListNode* list1_cloned_tail = list1_cloned_builder.tail;

    if (list1_cloned_tail == NULL) {
        list1_cloned_head = list2;
    } else {
        list1_cloned_tail->next = list2;
    }

    return list1_cloned_head;
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
        ASSERT_BOOL(bool1);
        uint8_t bool2 = INTO_TYPE(uint8_t, val2);
        ASSERT_BOOL(bool2);
        return bool1 == bool2;
    } else if (tag1 == STR_TYPE && tag2 == STR_TYPE){
        char* str1 = INTO_TYPE(char*, val1);
        ASSERT_NOT_NULL(str1);
        char* str2 = INTO_TYPE(char*, val2);
        ASSERT_NOT_NULL(str2);
        return __str_cmp(str1, str2);
    } else if (tag1 == LIST_TYPE && tag2 == LIST_TYPE){
        struct ListNode* list1 = INTO_TYPE(struct ListNode*, val1);
        ASSERT_NOT_NULL(list1);
        struct ListNode* list2 = INTO_TYPE(struct ListNode*, val2);
        ASSERT_NOT_NULL(list2);
        return list_node_cmp(list1->type_tag, list1->val, list2->type_tag, list2->val);
    }

    fprintf(stderr, "ERROR : WRONG TAGS IN LIST IN CMP (BUG IN COMPILER  \?\?)\n");
    exit(1);
}


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

// use this to prevent errors from pessimizing the fast path of execution 
__attribute__((cold, noreturn))
static void utf8_error(const char* msg, uint8_t* b){
    if (b){
        fprintf(stderr, "Invalid UTF-8 : %s (%x)\n", msg, *b);
    } else {
        fprintf(stderr, "Invalid UTF-8 : %s\n", msg);
    }
    exit(1);
}

#define IS_CONTINUATION_BYTE(c) ((c & 0xC0) == 0x80) // & 11000000 == 10000000

// the data are on the low 6 bits with the 2 highest bits as 0
#define GET_CONTINUATION_BYTE_DATA(c) (c & 0x3F) // & 00111111

struct ListNode* __chars(const char* s){
    size_t bytes_len = strlen(s);
    if (bytes_len == 0){
        return NULL;
    }
    struct ListNode* list_nodes_buf = MALLOC(bytes_len * sizeof(struct ListNode)); // bytes_len is the upper bound
    struct ListBuilder list_builder = list_builder_init(list_nodes_buf);
    size_t i = 0;

    // first fast loop
    while (i < bytes_len && (s[i] & 0x80) == 0) {
        list_builder_append_back(&list_builder, CHAR_TYPE, (Val)s[i]);
        i++;
    }

    while (i < bytes_len){
        uint8_t c = s[i];

        if ((c & 0x80) == 0){ // & 10000000
            // no need to use INTO_TYPE because it is just widening all unsigned (so just zext)
            list_builder_append_back(&list_builder, CHAR_TYPE, (Val)c);
            i++;
        } else {
            if (IS_CONTINUATION_BYTE(c)){
                utf8_error("continuation byte not expected", &c);
            }
            uint32_t code_point;
            if ((c & 0xE0) == 0xC0){ // & 11100000 == 11000000
                // 2 bytes codepoints
                if (i + 1 >= bytes_len){
                    utf8_error("missing bytes", NULL);
                }
                uint8_t byte_1 = c;
                uint8_t byte_2 = s[i+1];
                if (!IS_CONTINUATION_BYTE(byte_2)){
                    utf8_error("should be continuation byte", &byte_2);
                }
                // (byte_1 & 00011111) | (byte_2 & 01111111)
                code_point = (((uint32_t)(byte_1 & 0x1F)) << 6) | (uint32_t)GET_CONTINUATION_BYTE_DATA(byte_2) ; 
                if (code_point < 0x80) {
                    utf8_error("overlong encoding", NULL);
                }
                i += 2;
            } else if ((c & 0xF0) == 0xE0){ // & 11110000 == 11100000
                // 3 bytes codepoints
                if (i + 2 >= bytes_len){
                    utf8_error("missing bytes", NULL);
                }
                uint8_t byte_1 = c;
                uint8_t byte_2 = s[i+1];
                uint8_t byte_3 = s[i+2];
                if (!IS_CONTINUATION_BYTE(byte_2)){
                    utf8_error("should be continuation byte", &byte_2);
                }
                if (!IS_CONTINUATION_BYTE(byte_3)){
                    utf8_error("should be continuation byte", &byte_3);
                }
                // (byte_1 & 00001111)
                code_point = ((uint32_t)(byte_1 & 0xF) << 12) | ((uint32_t)GET_CONTINUATION_BYTE_DATA(byte_2) << 6) | ((uint32_t)GET_CONTINUATION_BYTE_DATA(byte_3));
                if (code_point < 0x800) {
                    utf8_error("overlong encoding", NULL);
                }
                
                i += 3;
            } else if ((c & 0xF8) == 0xF0) { // & 11111000 == 11110000
                // 4 bytes codepoints
                if (i + 3 >= bytes_len){
                    utf8_error("missing bytes", NULL);
                }
                uint8_t byte_1 = c;
                uint8_t byte_2 = s[i+1];
                uint8_t byte_3 = s[i+2];
                uint8_t byte_4 = s[i+3];
                if (!IS_CONTINUATION_BYTE(byte_2)){
                    utf8_error("should be continuation byte", &byte_2);
                }
                if (!IS_CONTINUATION_BYTE(byte_3)){
                    utf8_error("should be continuation byte", &byte_3);
                }
                if (!IS_CONTINUATION_BYTE(byte_4)){
                    utf8_error("should be continuation byte", &byte_4);
                }
                // (byte_1 & 00000111)
                code_point = ((uint32_t)(byte_1 & 0x7) << 18) | ((uint32_t)GET_CONTINUATION_BYTE_DATA(byte_2) << 12) | ((uint32_t)GET_CONTINUATION_BYTE_DATA(byte_3) << 6) | (uint32_t)GET_CONTINUATION_BYTE_DATA(byte_4);
                if (code_point < 0x10000) {
                    utf8_error("overlong encoding", NULL);
                }
                i += 4;
            } else {
                utf8_error("unknown leading bits", &c);
            }
            if (code_point > 0x10FFFF || (code_point >= 0xD800 && code_point <= 0xDFFF)) {
                utf8_error("invalid Unicode scalar", NULL);
            }
            list_builder_append_back(&list_builder, CHAR_TYPE, (Val)code_point);
        }
    }

    size_t used_nodes = list_builder.nodes_buf_idx;
    if (used_nodes < bytes_len-1){
        list_nodes_buf = REALLOC(list_nodes_buf, used_nodes * sizeof(struct ListNode));
    }
    return list_builder.head;
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

#ifndef FREESTANDING
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


struct str {
    char* buf;
    size_t capacity;
    size_t len;
};

static void list_print_no_new_line(struct ListNode* list);
static void format_char(struct str* str, uint32_t c);

// TODO : transform in the future into a print_val function
static void list_node_print(uint8_t tag, Val val){
    // TODO : transform this into a switch ?
    if (tag == INT_TYPE) {
        printf("%" PRId64 "", INTO_TYPE(int64_t, val));
    } else if (tag == FLOAT_TYPE){
        printf("%f", INTO_TYPE(double, val));
    } else if (tag == BOOL_TYPE){
        uint8_t b = INTO_TYPE(uint8_t, val);
        ASSERT_BOOL(b);
        printf("%s", __bool_to_str((bool)b));
    } else if (tag == STR_TYPE){
        const char* s = INTO_TYPE(char*, val);
        ASSERT_NOT_NULL(s);
        printf("%s", s);
    } else if (tag == CHAR_TYPE){
        // use this instead of format_char to prevent useless heap allocations
        char buf[5];
        struct str s = (struct str){
            .buf = buf,
            .len = 0,
            .capacity = 5,
        };
        uint32_t c = INTO_TYPE(uint32_t, val);
        // TODO : assert valid codepoint ? (but already in format_char ?)
        format_char(&s, c);
        buf[s.len] = '\0';
        printf("%s", buf);
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

static void str_append_with_realloc(struct str* str, char c){
    ASSERT_NOT_NULL(str);
    if (str->len + 1 >= str->capacity){
        str->capacity = str->capacity + str->capacity/2; // str->capacity * 1.5
        str->buf = REALLOC(str->buf, str->capacity);
    }
    //printf("current_len : %d, current_capacity : %d\n", str->len, str->capacity);
    str->buf[str->len] = c;
    str->len += 1;
}

const char* __char_to_str(uint32_t c){
    const int buf_size = 5; // 4 bytes max for codepoint + null byte
    // it should not call realloc on the str, if it does it would do UB
    struct str s = (struct str){
        .buf = MALLOC(sizeof(char) * buf_size),
        .len = 0,
        .capacity = buf_size,
    };
    format_char(&s, c);
    s.buf[s.len] = '\0';
    return s.buf;
}

static uint64_t absolute(int64_t i){
    uint64_t u = (uint64_t)i;
    if (i < 0){
        return ~u + 1; // equivalent to doing  ((uint64_t)(i + 1)) + 1 to hande INT_MIN where the - would do an overflow
    } else {
        return u;
    }
}

static const uint64_t powers_10[] = {
    1ULL,
    10ULL,
    100ULL,
    1000ULL,
    10000ULL,
    100000ULL,
    1000000ULL,
    10000000ULL,
    100000000ULL,
    1000000000ULL,
    10000000000ULL,
    100000000000ULL,
    1000000000000ULL,
    10000000000000ULL,
    100000000000000ULL,
    1000000000000000ULL,
    10000000000000000ULL,
    100000000000000000ULL,
    1000000000000000000ULL,
    10000000000000000000ULL
};

static inline int clz64(uint64_t x) {
#if defined(_MSC_VER)
    unsigned long index;
    _BitScanReverse64(&index, x);
    return 63 - index;
#else
    return __builtin_clzll(x);
#endif
}

static int digit_nb_unsigned(uint64_t u){
    if (u == 0){
        return 1;
    }

    int leading_zeros_count = 64 - clz64(u); // log2(u) + 1
    int t = (leading_zeros_count) * 1233 >> 12; // * log10(2) to change base to base 10 (optimized here : log10(2) around 0.30103 which is around 1233 / 4096 = 1233 >> 12) 
    
    return t + (u >= powers_10[t]); // use the power10 table to correct the approximation (could be off by 1)
}

static int digit_nb(int64_t i){
    uint64_t u = absolute(i);
    return digit_nb_unsigned(u);
}

static const char digit_pairs[] = {
    '0', '0', '0', '1', '0', '2', '0', '3', '0', '4', '0', '5', '0', '6', '0', '7', '0', '8', '0', '9', 
    '1', '0', '1', '1', '1', '2', '1', '3', '1', '4', '1', '5', '1', '6', '1', '7', '1', '8', '1', '9', 
    '2', '0', '2', '1', '2', '2', '2', '3', '2', '4', '2', '5', '2', '6', '2', '7', '2', '8', '2', '9', 
    '3', '0', '3', '1', '3', '2', '3', '3', '3', '4', '3', '5', '3', '6', '3', '7', '3', '8', '3', '9', 
    '4', '0', '4', '1', '4', '2', '4', '3', '4', '4', '4', '5', '4', '6', '4', '7', '4', '8', '4', '9', 
    '5', '0', '5', '1', '5', '2', '5', '3', '5', '4', '5', '5', '5', '6', '5', '7', '5', '8', '5', '9', 
    '6', '0', '6', '1', '6', '2', '6', '3', '6', '4', '6', '5', '6', '6', '6', '7', '6', '8', '6', '9', 
    '7', '0', '7', '1', '7', '2', '7', '3', '7', '4', '7', '5', '7', '6', '7', '7', '7', '8', '7', '9', 
    '8', '0', '8', '1', '8', '2', '8', '3', '8', '4', '8', '5', '8', '6', '8', '7', '8', '8', '8', '9', 
    '9', '0', '9', '1', '9', '2', '9', '3', '9', '4', '9', '5', '9', '6', '9', '7', '9', '8', '9', '9',
};

static void int_to_string_impl(char* buf, int64_t integer, int digit_number){
    int start = 0;
    if (integer < 0){
        buf[0] = '-';
        start = 1;
    }
    uint64_t u = absolute(integer);
    int i;
    for (i = 0; i < digit_number - 1; i += 2){
        int two_digits = u % 100;
        memcpy(buf + start  + digit_number - i - 2, digit_pairs + two_digits * 2, 2);
        u /= 100;
    }
    if (u != 0){
        buf[start + digit_number - i - 1] = (char)u + '0';
    }
}

/*static int max_double_digit_nb(){
    return 32; // 24 would be safe, but use 32 because it will be cheaper on bdwgc
    // could also do floor(log10(d)) but would use more CPU
}*/

#define MAX_DIGIT_POSSIBLE_DOUBLE 32 // 24 would be safe, but use 32 because it will be cheaper on bdwgc
    // could also do floor(log10(d)) in a function but would use more CPU

// TODO : implement ryu or grisu implementation (or both with grisu with ryu as fallback ?)
static size_t double_to_string_impl(char* buf, double d, size_t max_char_nb){
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

static void list_format(struct str* str, struct ListNode* list);
static void ensure_size_string(struct str* s, size_t size);

static void format_int(struct str* str, int64_t i){
    int digit_number = digit_nb(i);
    size_t buf_size = (i < 0) ? digit_number + 1 : digit_number;
    ensure_size_string(str, str->len + buf_size);
    int_to_string_impl(str->buf + str->len, i, digit_number);
    str->len += buf_size;
}

static void format_float(struct str* str, double d){
    ensure_size_string(str, str->len + MAX_DIGIT_POSSIBLE_DOUBLE);
    str->len += double_to_string_impl(str->buf + str->len, d, MAX_DIGIT_POSSIBLE_DOUBLE);
}

static void format_bool(struct str* str, bool b){
    const char* bool_str = __bool_to_str(b);
    size_t bool_str_len = strlen(bool_str);
    ensure_size_string(str, str->len + bool_str_len);
    memcpy(str->buf + str->len, bool_str, bool_str_len);
    str->len += bool_str_len;
}

static void format_str(struct str* str, char* s){
    size_t len_s = strlen(s);
    ensure_size_string(str, str->len + len_s);
    memcpy(str->buf + str->len, s, len_s);
    str->len += len_s;
}


static void format_char(struct str* str, uint32_t c){
    if (c > 0x10FFFF || (c >= 0xD800 && c <= 0xDFFF)) {
        fprintf(stderr, "Invalid UTF-8 codepoint %d\n", c);
        exit(1);
    }

    if (c <= 0x007F){
        ensure_size_string(str, str->len + 1);
        str->buf[str->len] = (char)c;
        str->len += 1;
    } else if (c <= 0x07FF){
        ensure_size_string(str, str->len + 2);
        uint8_t byte_1 = (uint8_t)((c >> 6 | 0xC0) & 0xDF); // | 11000000 & 11011111
        uint8_t byte_2 = ((uint8_t)c | 0x80) & 0xBF; // | 10000000 & 10111111
        (str->buf + str->len)[0] = byte_1;
        (str->buf + str->len)[1] = byte_2;
        str->len += 2;
    } else if (c <= 0xFFFF){
        ensure_size_string(str, str->len + 3);
        uint8_t bytes[3];
        bytes[0] = ((uint8_t)(c >> 12) | 0xE0) & 0xEF; // | 11100000 & 11101111
        bytes[1] = ((uint8_t)(c >> 6) | 0x80) & 0xBF;
        bytes[2] = ((uint8_t)c | 0x80) & 0xBF;
        memcpy(str->buf + str->len, bytes, 3);
        str->len += 3;
    } else if (c <= 0x10FFFF){
        ensure_size_string(str, str->len + 4);
        uint8_t bytes[4];
        bytes[0] = ((uint8_t)(c >> 18) | 0xF0) & 0xF7; // | 11110000 & 11110111
        bytes[1] = ((uint8_t)(c >> 12) | 0x80) & 0xBF;
        bytes[2] = ((uint8_t)(c >> 6) | 0x80) & 0xBF;
        bytes[3] = ((uint8_t)c | 0x80) & 0xBF;
        memcpy(str->buf + str->len, bytes, 4);
        str->len += 4;
    }
}

// TODO : deduplicate this code with the normal format
static void list_node_format(struct str* str, uint8_t tag, Val val){
    switch (tag) {
        case INT_TYPE:
            format_int(str, INTO_TYPE(int64_t, val));
            break;
        case FLOAT_TYPE:
            format_float(str, INTO_TYPE(double, val));
            break;
        case BOOL_TYPE:
            format_bool(str, INTO_TYPE(bool, val));
            break;
        case STR_TYPE:
            format_str(str, INTO_TYPE(char*, val));
            break;
        case LIST_TYPE:
            list_format(str, INTO_TYPE(struct ListNode*, val));
            break;
        case CHAR_TYPE:
            format_char(str, INTO_TYPE(uint32_t, val));
            break;
        default:
            fprintf(stderr, "ERROR : WRONG TAGS IN LIST IN FORMAT (BUG IN COMPILER  \?\?)\n");
            exit(1);
    }
}

static void list_format(struct str* str, struct ListNode* list){
    bool first = true;
    str->buf[str->len] = '[';
    str->len++;
    
    while (list != NULL){
        if (!first){
            str->buf[str->len] = ',';
            str->len++;
            str->buf[str->len] = ' ';
            str->len++;
        } 
        list_node_format(str, list->type_tag, list->val);
        list = list->next;
        first = false;
    }

    str->buf[str->len] = ']';
    str->len++;
}


static void ensure_size_string(struct str* s, size_t size){
    if (size >= s->capacity){
        while (size >= s->capacity){
            s->capacity *= 2;
        }
        s->buf = REALLOC(s->buf, s->capacity);
    }
}

static struct str str_init(size_t default_capacity) {
    return (struct str){
        .buf = MALLOC(sizeof(char) * default_capacity),
        .capacity = default_capacity,
        .len = 0,
    };
}

static char* vformat_string(char* format, va_list va){
    struct str str = str_init(5);

    while (*format != '\0'){
        switch (*format) {
            case '%':
                format++;
                switch (*format){
                    case 'd':
                        format_int(&str, va_arg(va, int64_t));
                        break;
                    case 'f':
                        format_float(&str, va_arg(va, double));
                        break;
                    case 's':
                        format_str(&str, va_arg(va, char*));
                        break;
                    case 'c':
                        format_char(&str, va_arg(va, uint32_t));
                        break;
                    case 'b':
                        format_bool(&str, (bool)va_arg(va, int));
                        break;
                    case 'l':
                        list_format(&str, va_arg(va, struct ListNode*));
                        break;
                    default:
                        fprintf(stderr, "ERROR : Unknown format\n");
                        exit(1);
                }
                break;
            default:
                str_append_with_realloc(&str, *format);
                break;
        }
        format++;
    }
    ensure_size_string(&str, str.len + 1);
    str.buf[str.len] = '\0';
    return str.buf;
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


// TODO : instead of using the fprintf(stderr, ..) and exit(1), use a macro for errors that would be this in low optimizations levels/with a flag transformed to a trap instruction like __builtin_trap()