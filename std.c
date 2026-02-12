#if __STDC_HOSTED__ == 0
#define FREESTANDING
#endif

#include <stdint.h>
#include <stddef.h>
#include <stdarg.h>
#include <stdbool.h>
#include <limits.h>

#define PURE __attribute__((pure))
#define CONST __attribute__((const))
#define WEAK __attribute__((weak))

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
#define MALLOC_NO_PTR(size) GC_malloc_atomic(size) // allocate memory where no ptr will be written (ex : str -> yes, list node : no) 
#define REALLOC(ptr, new_size) GC_realloc(ptr, new_size)
#define FREE(ptr) GC_free(ptr)

void gc_init(){
    GC_INIT();
}

#else
#define MALLOC(size) malloc(size)
#define MALLOC_NO_PTR(size) malloc(size)
#define FREE(ptr) free(ptr)
#define REALLOC(ptr, new_size) realloc(ptr, new_size)
void gc_init(){}
#endif

#else
// FREESTANDING MODE

WEAK void* memcpy(void* restrict dest, const void* restrict src, size_t size){
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
#define MALLOC_NO_PTR(size) malloc(size)
#define FREE(ptr) free(ptr)
#define REALLOC(ptr, new_size) realloc(ptr, new_size)
void gc_init(){}

struct Metadata {
    size_t size;
};

#define MALLOC_SIZE_WEAK (1024 * 1024) // 1Mib

// weak symbol, can be overriden in freestanding
__attribute__((malloc))
WEAK void* malloc(size_t size){
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

WEAK void* realloc(void* ptr, size_t size){
    void* new_buf = malloc(size);
    struct Metadata* metadata = (struct Metadata*)(ptr - sizeof(struct Metadata));
    size_t old_size = metadata->size;
    memcpy(new_buf, ptr, old_size < size ? old_size : size);
    return new_buf;
}

WEAK void free(void* ptr){
    (void)ptr;
}

// https://git.musl-libc.org/cgit/musl/tree/src/string/strlen.c
#define ONES ((size_t)-1/UCHAR_MAX)
#define HIGHS (ONES * (UCHAR_MAX/2+1))
#define HASZERO(x) ((x)-ONES & ~(x) & HIGHS)


WEAK PURE size_t strlen(const char* s){
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
WEAK FILE* stderr = &stderr_impl;
static FILE stdout_impl;
WEAK FILE* stdout = &stdout_impl;


WEAK int fprintf(FILE* stream, const char* format, ... ){
    (void)stream;
    (void)format;
    return 0;
}


void exit(int exit_code)  __attribute__ ((__noreturn__));

WEAK void exit(int exit_code) {
    (void)exit_code;
    __builtin_trap();
    while(1){}
}

WEAK __attribute__((noreturn)) void __stack_chk_fail(void) {
    __builtin_trap();
    while (1) { }
}

#ifndef assert
#ifdef NDEBUG
#define assert(e) ((void)0)
#else
__attribute__((noinline, cold, noreturn))
static void assert_fail(){
    __builtin_trap();
    while (1){}
}
#define assert(e) ((e) ? ((void)0) : assert_fail ())
#endif
#endif

#endif

__attribute__((cold, noreturn))
static void alloc_error(const char* func_name, const char* str){
    fprintf(stderr, "ALLOC ERROR in %s: %s\n", func_name, str);
    fprintf(stderr, "\n");
    exit(1);
}

#define ALLOC_ERROR(s) alloc_error(__func__, s)

#define TODO(str) do { \
        fprintf(stderr, "TODO : " str "\n"); \
        exit(1); \
    } while(0);

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
    UNIT_TYPE = 7,
};

typedef uint64_t Val;

// do a memcpy to avoid UB with this type punning, the compiler will optimize this, finger crossed
// the block expr syntax is supported on clang and gcc only
#define INTO_TYPE(t, var) ({ \
        t _dst;\
        memcpy(&_dst, &(var), sizeof(t)); \
        _dst; \
    }) \


// TODO : small vec optimization ?
struct VecVal {
    uint8_t element_type_tag;
    uint32_t size;
    void* buf;
};

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
    char* ret = MALLOC_NO_PTR(len_s1 + len_s2 + 1);
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

__attribute__((malloc, alloc_size(1)))
static struct ListNode* list_node_init(uint8_t type_tag, Val val) {
    struct ListNode* l = MALLOC(sizeof(struct ListNode));
    if (!l){
        ALLOC_ERROR("ListNode");
    }
    _list_node_init(l, type_tag, val);
    return l;
}

// optimization, improve cache locality
__attribute__((returns_nonnull))
struct ListNode* __list_node_init_static(uint8_t type_tag, const Val* vals_static, int64_t len){
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
__attribute__((returns_nonnull))
struct ListNode* __list_node_append(struct ListNode* list, uint8_t type_tag, Val val){
    struct ListNode* ret = list_node_init(type_tag, val);
    ASSERT_NOT_NULL(ret);

    ret->next = list;
    return ret;
}

__attribute__((returns_nonnull))
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

PURE int64_t __list_len(const struct ListNode* list){
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
static struct ListBuilder clone_list(const struct ListNode* list){
    const struct ListNode* current = list;
    int64_t list_len = __list_len(list);
    struct ListNode* list_nodes_buf = MALLOC(list_len * sizeof(struct ListNode));
    struct ListBuilder list_builder = list_builder_init(list_nodes_buf);
    
    while (current != NULL){
        list_builder_append_back(&list_builder, current->type_tag, current->val);
        current = current->next;
    }
    return list_builder;
}

struct ListNode* __list_node_merge(const struct ListNode* list1, struct ListNode* list2){
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

// used for JIT
struct JITValue {
    uint8_t tag; // same layout as the tag for ListNode
    uint64_t val;
};

struct JitWrappedList {
    struct JITValue* vals;  
    uint64_t len;
};

struct ListNode* __list_node_jit_unwrap_val(const struct JitWrappedList* list){
    struct ListNode* list_nodes_buf = MALLOC(list->len * sizeof(struct ListNode));
    struct ListBuilder list_builder = list_builder_init(list_nodes_buf);
    for (uint64_t i = 0; i < list->len; i++){
        struct JITValue jit_val = list->vals[i];
        uint8_t type_tag = jit_val.tag;
        Val val = jit_val.val; // should be binary compatible (check it ? TODO ?)
        list_builder_append_back(&list_builder, type_tag, val);
    }
    return list_builder.head;
}

struct JitWrappedList* __list_node_jit_wrap_return_val(const struct ListNode* list){
    struct JitWrappedList* wrapped_list = MALLOC(sizeof(struct JitWrappedList));
    if (!wrapped_list){
        ALLOC_ERROR("error in alloc of JitWrappedList");
    }
    uint64_t len = (uint64_t)__list_len(list);
    wrapped_list->vals = MALLOC(sizeof(struct JITValue) * len);
    if (!wrapped_list->vals){
        ALLOC_ERROR("error in alloc of vals of JitWrappedList");
    }
    wrapped_list->len = len;
    const struct ListNode* current = list;
    size_t i = 0;
    while (current){
        wrapped_list->vals[i] = (struct JITValue){
            .tag = current->type_tag,
            .val = current->val,
        };
        current = current->next;
        i++;
    }
    
    return wrapped_list;
}

// end of code for JIT


static PURE bool list_node_cmp(uint8_t tag1, Val val1, uint8_t tag2, Val val2){
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


PURE uint8_t __list_cmp(const struct ListNode* list1, const struct ListNode* list2){
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
static void utf8_error(const char* msg, const uint8_t* b){
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

static uint32_t utf8_decode_char(const char* s, size_t* pos, size_t bytes_len){
    size_t current_pos = *pos;
    uint8_t c = s[current_pos];
    if ((c & 0x80) == 0){ // & 10000000
        (*pos)++;
        return (uint32_t)c;
    }
        
    if (IS_CONTINUATION_BYTE(c)){
        utf8_error("continuation byte not expected", &c);
    }
    uint32_t code_point;
    if ((c & 0xE0) == 0xC0){ // & 11100000 == 11000000
        // 2 bytes codepoints
        if (current_pos + 1 >= bytes_len){
            utf8_error("missing bytes", NULL);
        }
        uint8_t byte_1 = c;
        uint8_t byte_2 = s[current_pos+1];
        if (!IS_CONTINUATION_BYTE(byte_2)){
            utf8_error("should be continuation byte", &byte_2);
        }
        // (byte_1 & 00011111) | (byte_2 & 01111111)
        code_point = (((uint32_t)(byte_1 & 0x1F)) << 6) | (uint32_t)GET_CONTINUATION_BYTE_DATA(byte_2) ; 
        if (code_point < 0x80) {
            utf8_error("overlong encoding", NULL);
        }
        (*pos) += 2;
    } else if ((c & 0xF0) == 0xE0){ // & 11110000 == 11100000
        // 3 bytes codepoints
        if (current_pos + 2 >= bytes_len){
            utf8_error("missing bytes", NULL);
        }
        uint8_t byte_1 = c;
        uint8_t byte_2 = s[current_pos+1];
        uint8_t byte_3 = s[current_pos+2];
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
                
        (*pos) += 3;
    } else if ((c & 0xF8) == 0xF0) { // & 11111000 == 11110000
        // 4 bytes codepoints
        if (current_pos + 3 >= bytes_len){
            utf8_error("missing bytes", NULL);
        }
        uint8_t byte_1 = c;
        uint8_t byte_2 = s[current_pos+1];
        uint8_t byte_3 = s[current_pos+2];
        uint8_t byte_4 = s[current_pos+3];
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
        (*pos) += 4;
    } else {
        utf8_error("unknown leading bits", &c);
    }
    if (code_point > 0x10FFFF || (code_point >= 0xD800 && code_point <= 0xDFFF)) {
        utf8_error("invalid Unicode scalar", NULL);
    }
    
    return code_point;
}


struct ListNode* __chars(const char* s){
    size_t bytes_len = strlen(s);
    if (bytes_len == 0){
        return NULL;
    }
    struct ListNode* list_nodes_buf = MALLOC(bytes_len * sizeof(struct ListNode)); // bytes_len is the upper bound
    if (!list_nodes_buf){
        ALLOC_ERROR("__chars list nodes buf");
    }
    struct ListBuilder list_builder = list_builder_init(list_nodes_buf);
    size_t i = 0;

    // first fast loop
    while (i < bytes_len && (s[i] & 0x80) == 0) {
        list_builder_append_back(&list_builder, CHAR_TYPE, (Val)s[i]);
        i++;
    }


    while (i < bytes_len){
        uint32_t code_point = utf8_decode_char(s, &i, bytes_len);
        list_builder_append_back(&list_builder, CHAR_TYPE, (Val)code_point);
    }

    return list_builder.head;
}

__attribute__((returns_nonnull))
CONST const char* __bool_to_str(bool b){
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


struct str {
    char* buf;
    size_t capacity;
    size_t len;
};

static void str_append_with_realloc(struct str* str, char c){
    ASSERT_NOT_NULL(str);
    if (str->len + 1 >= str->capacity){
        str->capacity = str->capacity + str->capacity/2; // str->capacity * 1.5
        str->buf = REALLOC(str->buf, str->capacity);
    }
    str->buf[str->len] = c;
    str->len += 1;
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
    if (i < digit_number){
        buf[start + digit_number - i - 1] = (char)u + '0';
    }
}

#define MAX_INT_BUF_SIZE 22
#define MAX_DOUBLE_BUF_SIZE 24

#if defined(__SIZEOF_INT128__) && !defined(_MSC_VER)
#define HAS_UINT128
#elif defined(_MSC_VER) && defined(_M_X64)
#define HAS_64_BIT_INTRINSICS
#endif

#define MANTISSA_NB_BITS 52
#define DOUBLE_BIAS 1023

#define EXPONENT_ALL_1 0x7FF

// represent a double that is of value mantissa * 10^exponent
struct double_decimal {
    int32_t exponent; // exponent in base 10
    uint64_t mantissa;
};

// to enable fast path for doubles that are can be represented as just an int
// in the small int case, also create the double_decimal struct
static bool is_small_int(uint64_t ieeeMantissa, uint32_t ieeeExponent, struct double_decimal* v){
    uint64_t mantissa2 = (1ULL << MANTISSA_NB_BITS) | ieeeMantissa; // create a 1 then the digits of the mantissa (1[MANTISSA]) to have the same as 1.mantissa
    // the 1.mantissa is mantissa2/2^52; but the double value is mantissa * 2^(exponent-bias) = (mantissa2/2^52) * 2^(exponent-bias)
    // = mantissa2 * 2^(exponent - bias - 53)
    int32_t exponent2 = (int32_t)(ieeeExponent - DOUBLE_BIAS - MANTISSA_NB_BITS);

    if (exponent2 > 0){
        // don't consider as a small int a number that has a bigger exponent than 0, so that fills the 53 bits of the mantissa2 (value >= 2^53)
        return false;
    }

    if (exponent2 < -52){
        // is necessarily a non whole number
        return false;
    }

    // exponent 2 is 0 or negative
    // find if the "fractionnal" part (after the division by 2^(-exponent2)) is 0
    uint64_t mask = (1ULL << (-exponent2)) - 1; // is 2^(-exponent2)-1 so -exponent1 1 bits in the lower part
    uint64_t fraction = mantissa2 & mask;
    if (fraction != 0){
        return false;
    }

    v->mantissa = mantissa2 >> -exponent2; // mantissa2 / 2^(-exponent2)
    v->exponent = 0;
    return true;
}

#define DOUBLE_POW5_INV_BITCOUNT 125
#define DOUBLE_POW5_BITCOUNT 125

#define DOUBLE_POW5_INV_TABLE_SIZE 342

static const uint64_t DOUBLE_POW5_INV_SPLIT[DOUBLE_POW5_INV_TABLE_SIZE][2] = {
  {                    1u, 2305843009213693952u }, { 11068046444225730970u, 1844674407370955161u },
  {  5165088340638674453u, 1475739525896764129u }, {  7821419487252849886u, 1180591620717411303u },
  {  8824922364862649494u, 1888946593147858085u }, {  7059937891890119595u, 1511157274518286468u },
  { 13026647942995916322u, 1208925819614629174u }, {  9774590264567735146u, 1934281311383406679u },
  { 11509021026396098440u, 1547425049106725343u }, { 16585914450600699399u, 1237940039285380274u },
  { 15469416676735388068u, 1980704062856608439u }, { 16064882156130220778u, 1584563250285286751u },
  {  9162556910162266299u, 1267650600228229401u }, {  7281393426775805432u, 2028240960365167042u },
  { 16893161185646375315u, 1622592768292133633u }, {  2446482504291369283u, 1298074214633706907u },
  {  7603720821608101175u, 2076918743413931051u }, {  2393627842544570617u, 1661534994731144841u },
  { 16672297533003297786u, 1329227995784915872u }, { 11918280793837635165u, 2126764793255865396u },
  {  5845275820328197809u, 1701411834604692317u }, { 15744267100488289217u, 1361129467683753853u },
  {  3054734472329800808u, 2177807148294006166u }, { 17201182836831481939u, 1742245718635204932u },
  {  6382248639981364905u, 1393796574908163946u }, {  2832900194486363201u, 2230074519853062314u },
  {  5955668970331000884u, 1784059615882449851u }, {  1075186361522890384u, 1427247692705959881u },
  { 12788344622662355584u, 2283596308329535809u }, { 13920024512871794791u, 1826877046663628647u },
  {  3757321980813615186u, 1461501637330902918u }, { 10384555214134712795u, 1169201309864722334u },
  {  5547241898389809503u, 1870722095783555735u }, {  4437793518711847602u, 1496577676626844588u },
  { 10928932444453298728u, 1197262141301475670u }, { 17486291911125277965u, 1915619426082361072u },
  {  6610335899416401726u, 1532495540865888858u }, { 12666966349016942027u, 1225996432692711086u },
  { 12888448528943286597u, 1961594292308337738u }, { 17689456452638449924u, 1569275433846670190u },
  { 14151565162110759939u, 1255420347077336152u }, {  7885109000409574610u, 2008672555323737844u },
  {  9997436015069570011u, 1606938044258990275u }, {  7997948812055656009u, 1285550435407192220u },
  { 12796718099289049614u, 2056880696651507552u }, {  2858676849947419045u, 1645504557321206042u },
  { 13354987924183666206u, 1316403645856964833u }, { 17678631863951955605u, 2106245833371143733u },
  {  3074859046935833515u, 1684996666696914987u }, { 13527933681774397782u, 1347997333357531989u },
  { 10576647446613305481u, 2156795733372051183u }, { 15840015586774465031u, 1725436586697640946u },
  {  8982663654677661702u, 1380349269358112757u }, { 18061610662226169046u, 2208558830972980411u },
  { 10759939715039024913u, 1766847064778384329u }, { 12297300586773130254u, 1413477651822707463u },
  { 15986332124095098083u, 2261564242916331941u }, {  9099716884534168143u, 1809251394333065553u },
  { 14658471137111155161u, 1447401115466452442u }, {  4348079280205103483u, 1157920892373161954u },
  { 14335624477811986218u, 1852673427797059126u }, {  7779150767507678651u, 1482138742237647301u },
  {  2533971799264232598u, 1185710993790117841u }, { 15122401323048503126u, 1897137590064188545u },
  { 12097921058438802501u, 1517710072051350836u }, {  5988988032009131678u, 1214168057641080669u },
  { 16961078480698431330u, 1942668892225729070u }, { 13568862784558745064u, 1554135113780583256u },
  {  7165741412905085728u, 1243308091024466605u }, { 11465186260648137165u, 1989292945639146568u },
  { 16550846638002330379u, 1591434356511317254u }, { 16930026125143774626u, 1273147485209053803u },
  {  4951948911778577463u, 2037035976334486086u }, {   272210314680951647u, 1629628781067588869u },
  {  3907117066486671641u, 1303703024854071095u }, {  6251387306378674625u, 2085924839766513752u },
  { 16069156289328670670u, 1668739871813211001u }, {  9165976216721026213u, 1334991897450568801u },
  {  7286864317269821294u, 2135987035920910082u }, { 16897537898041588005u, 1708789628736728065u },
  { 13518030318433270404u, 1367031702989382452u }, {  6871453250525591353u, 2187250724783011924u },
  {  9186511415162383406u, 1749800579826409539u }, { 11038557946871817048u, 1399840463861127631u },
  { 10282995085511086630u, 2239744742177804210u }, {  8226396068408869304u, 1791795793742243368u },
  { 13959814484210916090u, 1433436634993794694u }, { 11267656730511734774u, 2293498615990071511u },
  {  5324776569667477496u, 1834798892792057209u }, {  7949170070475892320u, 1467839114233645767u },
  { 17427382500606444826u, 1174271291386916613u }, {  5747719112518849781u, 1878834066219066582u },
  { 15666221734240810795u, 1503067252975253265u }, { 12532977387392648636u, 1202453802380202612u },
  {  5295368560860596524u, 1923926083808324180u }, {  4236294848688477220u, 1539140867046659344u },
  {  7078384693692692099u, 1231312693637327475u }, { 11325415509908307358u, 1970100309819723960u },
  {  9060332407926645887u, 1576080247855779168u }, { 14626963555825137356u, 1260864198284623334u },
  { 12335095245094488799u, 2017382717255397335u }, {  9868076196075591040u, 1613906173804317868u },
  { 15273158586344293478u, 1291124939043454294u }, { 13369007293925138595u, 2065799902469526871u },
  {  7005857020398200553u, 1652639921975621497u }, { 16672732060544291412u, 1322111937580497197u },
  { 11918976037903224966u, 2115379100128795516u }, {  5845832015580669650u, 1692303280103036413u },
  { 12055363241948356366u, 1353842624082429130u }, {   841837113407818570u, 2166148198531886609u },
  {  4362818505468165179u, 1732918558825509287u }, { 14558301248600263113u, 1386334847060407429u },
  { 12225235553534690011u, 2218135755296651887u }, {  2401490813343931363u, 1774508604237321510u },
  {  1921192650675145090u, 1419606883389857208u }, { 17831303500047873437u, 2271371013423771532u },
  {  6886345170554478103u, 1817096810739017226u }, {  1819727321701672159u, 1453677448591213781u },
  { 16213177116328979020u, 1162941958872971024u }, { 14873036941900635463u, 1860707134196753639u },
  { 15587778368262418694u, 1488565707357402911u }, {  8780873879868024632u, 1190852565885922329u },
  {  2981351763563108441u, 1905364105417475727u }, { 13453127855076217722u, 1524291284333980581u },
  {  7073153469319063855u, 1219433027467184465u }, { 11317045550910502167u, 1951092843947495144u },
  { 12742985255470312057u, 1560874275157996115u }, { 10194388204376249646u, 1248699420126396892u },
  {  1553625868034358140u, 1997919072202235028u }, {  8621598323911307159u, 1598335257761788022u },
  { 17965325103354776697u, 1278668206209430417u }, { 13987124906400001422u, 2045869129935088668u },
  {   121653480894270168u, 1636695303948070935u }, {    97322784715416134u, 1309356243158456748u },
  { 14913111714512307107u, 2094969989053530796u }, {  8241140556867935363u, 1675975991242824637u },
  { 17660958889720079260u, 1340780792994259709u }, { 17189487779326395846u, 2145249268790815535u },
  { 13751590223461116677u, 1716199415032652428u }, { 18379969808252713988u, 1372959532026121942u },
  { 14650556434236701088u, 2196735251241795108u }, {   652398703163629901u, 1757388200993436087u },
  { 11589965406756634890u, 1405910560794748869u }, {  7475898206584884855u, 2249456897271598191u },
  {  2291369750525997561u, 1799565517817278553u }, {  9211793429904618695u, 1439652414253822842u },
  { 18428218302589300235u, 2303443862806116547u }, {  7363877012587619542u, 1842755090244893238u },
  { 13269799239553916280u, 1474204072195914590u }, { 10615839391643133024u, 1179363257756731672u },
  {  2227947767661371545u, 1886981212410770676u }, { 16539753473096738529u, 1509584969928616540u },
  { 13231802778477390823u, 1207667975942893232u }, {  6413489186596184024u, 1932268761508629172u },
  { 16198837793502678189u, 1545815009206903337u }, {  5580372605318321905u, 1236652007365522670u },
  {  8928596168509315048u, 1978643211784836272u }, { 18210923379033183008u, 1582914569427869017u },
  {  7190041073742725760u, 1266331655542295214u }, {   436019273762630246u, 2026130648867672343u },
  {  7727513048493924843u, 1620904519094137874u }, {  9871359253537050198u, 1296723615275310299u },
  {  4726128361433549347u, 2074757784440496479u }, {  7470251503888749801u, 1659806227552397183u },
  { 13354898832594820487u, 1327844982041917746u }, { 13989140502667892133u, 2124551971267068394u },
  { 14880661216876224029u, 1699641577013654715u }, { 11904528973500979224u, 1359713261610923772u },
  {  4289851098633925465u, 2175541218577478036u }, { 18189276137874781665u, 1740432974861982428u },
  {  3483374466074094362u, 1392346379889585943u }, {  1884050330976640656u, 2227754207823337509u },
  {  5196589079523222848u, 1782203366258670007u }, { 15225317707844309248u, 1425762693006936005u },
  {  5913764258841343181u, 2281220308811097609u }, {  8420360221814984868u, 1824976247048878087u },
  { 17804334621677718864u, 1459980997639102469u }, { 17932816512084085415u, 1167984798111281975u },
  { 10245762345624985047u, 1868775676978051161u }, {  4507261061758077715u, 1495020541582440929u },
  {  7295157664148372495u, 1196016433265952743u }, {  7982903447895485668u, 1913626293225524389u },
  { 10075671573058298858u, 1530901034580419511u }, {  4371188443704728763u, 1224720827664335609u },
  { 14372599139411386667u, 1959553324262936974u }, { 15187428126271019657u, 1567642659410349579u },
  { 15839291315758726049u, 1254114127528279663u }, {  3206773216762499739u, 2006582604045247462u },
  { 13633465017635730761u, 1605266083236197969u }, { 14596120828850494932u, 1284212866588958375u },
  {  4907049252451240275u, 2054740586542333401u }, {   236290587219081897u, 1643792469233866721u },
  { 14946427728742906810u, 1315033975387093376u }, { 16535586736504830250u, 2104054360619349402u },
  {  5849771759720043554u, 1683243488495479522u }, { 15747863852001765813u, 1346594790796383617u },
  { 10439186904235184007u, 2154551665274213788u }, { 15730047152871967852u, 1723641332219371030u },
  { 12584037722297574282u, 1378913065775496824u }, {  9066413911450387881u, 2206260905240794919u },
  { 10942479943902220628u, 1765008724192635935u }, {  8753983955121776503u, 1412006979354108748u },
  { 10317025513452932081u, 2259211166966573997u }, {   874922781278525018u, 1807368933573259198u },
  {  8078635854506640661u, 1445895146858607358u }, { 13841606313089133175u, 1156716117486885886u },
  { 14767872471458792434u, 1850745787979017418u }, {   746251532941302978u, 1480596630383213935u },
  {   597001226353042382u, 1184477304306571148u }, { 15712597221132509104u, 1895163686890513836u },
  {  8880728962164096960u, 1516130949512411069u }, { 10793931984473187891u, 1212904759609928855u },
  { 17270291175157100626u, 1940647615375886168u }, {  2748186495899949531u, 1552518092300708935u },
  {  2198549196719959625u, 1242014473840567148u }, { 18275073973719576693u, 1987223158144907436u },
  { 10930710364233751031u, 1589778526515925949u }, { 12433917106128911148u, 1271822821212740759u },
  {  8826220925580526867u, 2034916513940385215u }, {  7060976740464421494u, 1627933211152308172u },
  { 16716827836597268165u, 1302346568921846537u }, { 11989529279587987770u, 2083754510274954460u },
  {  9591623423670390216u, 1667003608219963568u }, { 15051996368420132820u, 1333602886575970854u },
  { 13015147745246481542u, 2133764618521553367u }, {  3033420566713364587u, 1707011694817242694u },
  {  6116085268112601993u, 1365609355853794155u }, {  9785736428980163188u, 2184974969366070648u },
  { 15207286772667951197u, 1747979975492856518u }, {  1097782973908629988u, 1398383980394285215u },
  {  1756452758253807981u, 2237414368630856344u }, {  5094511021344956708u, 1789931494904685075u },
  {  4075608817075965366u, 1431945195923748060u }, {  6520974107321544586u, 2291112313477996896u },
  {  1527430471115325346u, 1832889850782397517u }, { 12289990821117991246u, 1466311880625918013u },
  { 17210690286378213644u, 1173049504500734410u }, {  9090360384495590213u, 1876879207201175057u },
  { 18340334751822203140u, 1501503365760940045u }, { 14672267801457762512u, 1201202692608752036u },
  { 16096930852848599373u, 1921924308174003258u }, {  1809498238053148529u, 1537539446539202607u },
  { 12515645034668249793u, 1230031557231362085u }, {  1578287981759648052u, 1968050491570179337u },
  { 12330676829633449412u, 1574440393256143469u }, { 13553890278448669853u, 1259552314604914775u },
  {  3239480371808320148u, 2015283703367863641u }, { 17348979556414297411u, 1612226962694290912u },
  {  6500486015647617283u, 1289781570155432730u }, { 10400777625036187652u, 2063650512248692368u },
  { 15699319729512770768u, 1650920409798953894u }, { 16248804598352126938u, 1320736327839163115u },
  {  7551343283653851484u, 2113178124542660985u }, {  6041074626923081187u, 1690542499634128788u },
  { 12211557331022285596u, 1352433999707303030u }, {  1091747655926105338u, 2163894399531684849u },
  {  4562746939482794594u, 1731115519625347879u }, {  7339546366328145998u, 1384892415700278303u },
  {  8053925371383123274u, 2215827865120445285u }, {  6443140297106498619u, 1772662292096356228u },
  { 12533209867169019542u, 1418129833677084982u }, {  5295740528502789974u, 2269007733883335972u },
  { 15304638867027962949u, 1815206187106668777u }, {  4865013464138549713u, 1452164949685335022u },
  { 14960057215536570740u, 1161731959748268017u }, {  9178696285890871890u, 1858771135597228828u },
  { 14721654658196518159u, 1487016908477783062u }, {  4398626097073393881u, 1189613526782226450u },
  {  7037801755317430209u, 1903381642851562320u }, {  5630241404253944167u, 1522705314281249856u },
  {   814844308661245011u, 1218164251424999885u }, {  1303750893857992017u, 1949062802279999816u },
  { 15800395974054034906u, 1559250241823999852u }, {  5261619149759407279u, 1247400193459199882u },
  { 12107939454356961969u, 1995840309534719811u }, {  5997002748743659252u, 1596672247627775849u },
  {  8486951013736837725u, 1277337798102220679u }, {  2511075177753209390u, 2043740476963553087u },
  { 13076906586428298482u, 1634992381570842469u }, { 14150874083884549109u, 1307993905256673975u },
  {  4194654460505726958u, 2092790248410678361u }, { 18113118827372222859u, 1674232198728542688u },
  {  3422448617672047318u, 1339385758982834151u }, { 16543964232501006678u, 2143017214372534641u },
  {  9545822571258895019u, 1714413771498027713u }, { 15015355686490936662u, 1371531017198422170u },
  {  5577825024675947042u, 2194449627517475473u }, { 11840957649224578280u, 1755559702013980378u },
  { 16851463748863483271u, 1404447761611184302u }, { 12204946739213931940u, 2247116418577894884u },
  { 13453306206113055875u, 1797693134862315907u }, {  3383947335406624054u, 1438154507889852726u },
  { 16482362180876329456u, 2301047212623764361u }, {  9496540929959153242u, 1840837770099011489u },
  { 11286581558709232917u, 1472670216079209191u }, {  5339916432225476010u, 1178136172863367353u },
  {  4854517476818851293u, 1885017876581387765u }, {  3883613981455081034u, 1508014301265110212u },
  { 14174937629389795797u, 1206411441012088169u }, { 11611853762797942306u, 1930258305619341071u },
  {  5600134195496443521u, 1544206644495472857u }, { 15548153800622885787u, 1235365315596378285u },
  {  6430302007287065643u, 1976584504954205257u }, { 16212288050055383484u, 1581267603963364205u },
  { 12969830440044306787u, 1265014083170691364u }, {  9683682259845159889u, 2024022533073106183u },
  { 15125643437359948558u, 1619218026458484946u }, {  8411165935146048523u, 1295374421166787957u },
  { 17147214310975587960u, 2072599073866860731u }, { 10028422634038560045u, 1658079259093488585u },
  {  8022738107230848036u, 1326463407274790868u }, {  9147032156827446534u, 2122341451639665389u },
  { 11006974540203867551u, 1697873161311732311u }, {  5116230817421183718u, 1358298529049385849u },
  { 15564666937357714594u, 2173277646479017358u }, {  1383687105660440706u, 1738622117183213887u },
  { 12174996128754083534u, 1390897693746571109u }, {  8411947361780802685u, 2225436309994513775u },
  {  6729557889424642148u, 1780349047995611020u }, {  5383646311539713719u, 1424279238396488816u },
  {  1235136468979721303u, 2278846781434382106u }, { 15745504434151418335u, 1823077425147505684u },
  { 16285752362063044992u, 1458461940118004547u }, {  5649904260166615347u, 1166769552094403638u },
  {  5350498001524674232u, 1866831283351045821u }, {   591049586477829062u, 1493465026680836657u },
  { 11540886113407994219u, 1194772021344669325u }, {    18673707743239135u, 1911635234151470921u },
  { 14772334225162232601u, 1529308187321176736u }, {  8128518565387875758u, 1223446549856941389u },
  {  1937583260394870242u, 1957514479771106223u }, {  8928764237799716840u, 1566011583816884978u },
  { 14521709019723594119u, 1252809267053507982u }, {  8477339172590109297u, 2004494827285612772u },
  { 17849917782297818407u, 1603595861828490217u }, {  6901236596354434079u, 1282876689462792174u },
  { 18420676183650915173u, 2052602703140467478u }, {  3668494502695001169u, 1642082162512373983u },
  { 10313493231639821582u, 1313665730009899186u }, {  9122891541139893884u, 2101865168015838698u },
  { 14677010862395735754u, 1681492134412670958u }, {   673562245690857633u, 1345193707530136767u }
};

#define DOUBLE_POW5_TABLE_SIZE 326

static const uint64_t DOUBLE_POW5_SPLIT[DOUBLE_POW5_TABLE_SIZE][2] = {
  {                    0u, 1152921504606846976u }, {                    0u, 1441151880758558720u },
  {                    0u, 1801439850948198400u }, {                    0u, 2251799813685248000u },
  {                    0u, 1407374883553280000u }, {                    0u, 1759218604441600000u },
  {                    0u, 2199023255552000000u }, {                    0u, 1374389534720000000u },
  {                    0u, 1717986918400000000u }, {                    0u, 2147483648000000000u },
  {                    0u, 1342177280000000000u }, {                    0u, 1677721600000000000u },
  {                    0u, 2097152000000000000u }, {                    0u, 1310720000000000000u },
  {                    0u, 1638400000000000000u }, {                    0u, 2048000000000000000u },
  {                    0u, 1280000000000000000u }, {                    0u, 1600000000000000000u },
  {                    0u, 2000000000000000000u }, {                    0u, 1250000000000000000u },
  {                    0u, 1562500000000000000u }, {                    0u, 1953125000000000000u },
  {                    0u, 1220703125000000000u }, {                    0u, 1525878906250000000u },
  {                    0u, 1907348632812500000u }, {                    0u, 1192092895507812500u },
  {                    0u, 1490116119384765625u }, {  4611686018427387904u, 1862645149230957031u },
  {  9799832789158199296u, 1164153218269348144u }, { 12249790986447749120u, 1455191522836685180u },
  { 15312238733059686400u, 1818989403545856475u }, { 14528612397897220096u, 2273736754432320594u },
  { 13692068767113150464u, 1421085471520200371u }, { 12503399940464050176u, 1776356839400250464u },
  { 15629249925580062720u, 2220446049250313080u }, {  9768281203487539200u, 1387778780781445675u },
  {  7598665485932036096u, 1734723475976807094u }, {   274959820560269312u, 2168404344971008868u },
  {  9395221924704944128u, 1355252715606880542u }, {  2520655369026404352u, 1694065894508600678u },
  { 12374191248137781248u, 2117582368135750847u }, { 14651398557727195136u, 1323488980084844279u },
  { 13702562178731606016u, 1654361225106055349u }, {  3293144668132343808u, 2067951531382569187u },
  { 18199116482078572544u, 1292469707114105741u }, {  8913837547316051968u, 1615587133892632177u },
  { 15753982952572452864u, 2019483917365790221u }, { 12152082354571476992u, 1262177448353618888u },
  { 15190102943214346240u, 1577721810442023610u }, {  9764256642163156992u, 1972152263052529513u },
  { 17631875447420442880u, 1232595164407830945u }, {  8204786253993389888u, 1540743955509788682u },
  {  1032610780636961552u, 1925929944387235853u }, {  2951224747111794922u, 1203706215242022408u },
  {  3689030933889743652u, 1504632769052528010u }, { 13834660704216955373u, 1880790961315660012u },
  { 17870034976990372916u, 1175494350822287507u }, { 17725857702810578241u, 1469367938527859384u },
  {  3710578054803671186u, 1836709923159824231u }, {    26536550077201078u, 2295887403949780289u },
  { 11545800389866720434u, 1434929627468612680u }, { 14432250487333400542u, 1793662034335765850u },
  {  8816941072311974870u, 2242077542919707313u }, { 17039803216263454053u, 1401298464324817070u },
  { 12076381983474541759u, 1751623080406021338u }, {  5872105442488401391u, 2189528850507526673u },
  { 15199280947623720629u, 1368455531567204170u }, {  9775729147674874978u, 1710569414459005213u },
  { 16831347453020981627u, 2138211768073756516u }, {  1296220121283337709u, 1336382355046097823u },
  { 15455333206886335848u, 1670477943807622278u }, { 10095794471753144002u, 2088097429759527848u },
  {  6309871544845715001u, 1305060893599704905u }, { 12499025449484531656u, 1631326116999631131u },
  { 11012095793428276666u, 2039157646249538914u }, { 11494245889320060820u, 1274473528905961821u },
  {   532749306367912313u, 1593091911132452277u }, {  5277622651387278295u, 1991364888915565346u },
  {  7910200175544436838u, 1244603055572228341u }, { 14499436237857933952u, 1555753819465285426u },
  {  8900923260467641632u, 1944692274331606783u }, { 12480606065433357876u, 1215432671457254239u },
  { 10989071563364309441u, 1519290839321567799u }, {  9124653435777998898u, 1899113549151959749u },
  {  8008751406574943263u, 1186945968219974843u }, {  5399253239791291175u, 1483682460274968554u },
  { 15972438586593889776u, 1854603075343710692u }, {   759402079766405302u, 1159126922089819183u },
  { 14784310654990170340u, 1448908652612273978u }, {  9257016281882937117u, 1811135815765342473u },
  { 16182956370781059300u, 2263919769706678091u }, {  7808504722524468110u, 1414949856066673807u },
  {  5148944884728197234u, 1768687320083342259u }, {  1824495087482858639u, 2210859150104177824u },
  {  1140309429676786649u, 1381786968815111140u }, {  1425386787095983311u, 1727233711018888925u },
  {  6393419502297367043u, 2159042138773611156u }, { 13219259225790630210u, 1349401336733506972u },
  { 16524074032238287762u, 1686751670916883715u }, { 16043406521870471799u, 2108439588646104644u },
  {   803757039314269066u, 1317774742903815403u }, { 14839754354425000045u, 1647218428629769253u },
  {  4714634887749086344u, 2059023035787211567u }, {  9864175832484260821u, 1286889397367007229u },
  { 16941905809032713930u, 1608611746708759036u }, {  2730638187581340797u, 2010764683385948796u },
  { 10930020904093113806u, 1256727927116217997u }, { 18274212148543780162u, 1570909908895272496u },
  {  4396021111970173586u, 1963637386119090621u }, {  5053356204195052443u, 1227273366324431638u },
  { 15540067292098591362u, 1534091707905539547u }, { 14813398096695851299u, 1917614634881924434u },
  { 13870059828862294966u, 1198509146801202771u }, { 12725888767650480803u, 1498136433501503464u },
  { 15907360959563101004u, 1872670541876879330u }, { 14553786618154326031u, 1170419088673049581u },
  {  4357175217410743827u, 1463023860841311977u }, { 10058155040190817688u, 1828779826051639971u },
  {  7961007781811134206u, 2285974782564549964u }, { 14199001900486734687u, 1428734239102843727u },
  { 13137066357181030455u, 1785917798878554659u }, { 11809646928048900164u, 2232397248598193324u },
  { 16604401366885338411u, 1395248280373870827u }, { 16143815690179285109u, 1744060350467338534u },
  { 10956397575869330579u, 2180075438084173168u }, {  6847748484918331612u, 1362547148802608230u },
  { 17783057643002690323u, 1703183936003260287u }, { 17617136035325974999u, 2128979920004075359u },
  { 17928239049719816230u, 1330612450002547099u }, { 17798612793722382384u, 1663265562503183874u },
  { 13024893955298202172u, 2079081953128979843u }, {  5834715712847682405u, 1299426220705612402u },
  { 16516766677914378815u, 1624282775882015502u }, { 11422586310538197711u, 2030353469852519378u },
  { 11750802462513761473u, 1268970918657824611u }, { 10076817059714813937u, 1586213648322280764u },
  { 12596021324643517422u, 1982767060402850955u }, {  5566670318688504437u, 1239229412751781847u },
  {  2346651879933242642u, 1549036765939727309u }, {  7545000868343941206u, 1936295957424659136u },
  {  4715625542714963254u, 1210184973390411960u }, {  5894531928393704067u, 1512731216738014950u },
  { 16591536947346905892u, 1890914020922518687u }, { 17287239619732898039u, 1181821263076574179u },
  { 16997363506238734644u, 1477276578845717724u }, {  2799960309088866689u, 1846595723557147156u },
  { 10973347230035317489u, 1154122327223216972u }, { 13716684037544146861u, 1442652909029021215u },
  { 12534169028502795672u, 1803316136286276519u }, { 11056025267201106687u, 2254145170357845649u },
  { 18439230838069161439u, 1408840731473653530u }, { 13825666510731675991u, 1761050914342066913u },
  {  3447025083132431277u, 2201313642927583642u }, {  6766076695385157452u, 1375821026829739776u },
  {  8457595869231446815u, 1719776283537174720u }, { 10571994836539308519u, 2149720354421468400u },
  {  6607496772837067824u, 1343575221513417750u }, { 17482743002901110588u, 1679469026891772187u },
  { 17241742735199000331u, 2099336283614715234u }, { 15387775227926763111u, 1312085177259197021u },
  {  5399660979626290177u, 1640106471573996277u }, { 11361262242960250625u, 2050133089467495346u },
  { 11712474920277544544u, 1281333180917184591u }, { 10028907631919542777u, 1601666476146480739u },
  {  7924448521472040567u, 2002083095183100924u }, { 14176152362774801162u, 1251301934489438077u },
  {  3885132398186337741u, 1564127418111797597u }, {  9468101516160310080u, 1955159272639746996u },
  { 15140935484454969608u, 1221974545399841872u }, {   479425281859160394u, 1527468181749802341u },
  {  5210967620751338397u, 1909335227187252926u }, { 17091912818251750210u, 1193334516992033078u },
  { 12141518985959911954u, 1491668146240041348u }, { 15176898732449889943u, 1864585182800051685u },
  { 11791404716994875166u, 1165365739250032303u }, { 10127569877816206054u, 1456707174062540379u },
  {  8047776328842869663u, 1820883967578175474u }, {   836348374198811271u, 2276104959472719343u },
  {  7440246761515338900u, 1422565599670449589u }, { 13911994470321561530u, 1778206999588061986u },
  {  8166621051047176104u, 2222758749485077483u }, {  2798295147690791113u, 1389224218428173427u },
  { 17332926989895652603u, 1736530273035216783u }, { 17054472718942177850u, 2170662841294020979u },
  {  8353202440125167204u, 1356664275808763112u }, { 10441503050156459005u, 1695830344760953890u },
  {  3828506775840797949u, 2119787930951192363u }, {    86973725686804766u, 1324867456844495227u },
  { 13943775212390669669u, 1656084321055619033u }, {  3594660960206173375u, 2070105401319523792u },
  {  2246663100128858359u, 1293815875824702370u }, { 12031700912015848757u, 1617269844780877962u },
  {  5816254103165035138u, 2021587305976097453u }, {  5941001823691840913u, 1263492066235060908u },
  {  7426252279614801142u, 1579365082793826135u }, {  4671129331091113523u, 1974206353492282669u },
  {  5225298841145639904u, 1233878970932676668u }, {  6531623551432049880u, 1542348713665845835u },
  {  3552843420862674446u, 1927935892082307294u }, { 16055585193321335241u, 1204959932551442058u },
  { 10846109454796893243u, 1506199915689302573u }, { 18169322836923504458u, 1882749894611628216u },
  { 11355826773077190286u, 1176718684132267635u }, {  9583097447919099954u, 1470898355165334544u },
  { 11978871809898874942u, 1838622943956668180u }, { 14973589762373593678u, 2298278679945835225u },
  {  2440964573842414192u, 1436424174966147016u }, {  3051205717303017741u, 1795530218707683770u },
  { 13037379183483547984u, 2244412773384604712u }, {  8148361989677217490u, 1402757983365377945u },
  { 14797138505523909766u, 1753447479206722431u }, { 13884737113477499304u, 2191809349008403039u },
  { 15595489723564518921u, 1369880843130251899u }, { 14882676136028260747u, 1712351053912814874u },
  {  9379973133180550126u, 2140438817391018593u }, { 17391698254306313589u, 1337774260869386620u },
  {  3292878744173340370u, 1672217826086733276u }, {  4116098430216675462u, 2090272282608416595u },
  {   266718509671728212u, 1306420176630260372u }, {   333398137089660265u, 1633025220787825465u },
  {  5028433689789463235u, 2041281525984781831u }, { 10060300083759496378u, 1275800953740488644u },
  { 12575375104699370472u, 1594751192175610805u }, {  1884160825592049379u, 1993438990219513507u },
  { 17318501580490888525u, 1245899368887195941u }, {  7813068920331446945u, 1557374211108994927u },
  {  5154650131986920777u, 1946717763886243659u }, {   915813323278131534u, 1216698602428902287u },
  { 14979824709379828129u, 1520873253036127858u }, {  9501408849870009354u, 1901091566295159823u },
  { 12855909558809837702u, 1188182228934474889u }, {  2234828893230133415u, 1485227786168093612u },
  {  2793536116537666769u, 1856534732710117015u }, {  8663489100477123587u, 1160334207943823134u },
  {  1605989338741628675u, 1450417759929778918u }, { 11230858710281811652u, 1813022199912223647u },
  {  9426887369424876662u, 2266277749890279559u }, { 12809333633531629769u, 1416423593681424724u },
  { 16011667041914537212u, 1770529492101780905u }, {  6179525747111007803u, 2213161865127226132u },
  { 13085575628799155685u, 1383226165704516332u }, { 16356969535998944606u, 1729032707130645415u },
  { 15834525901571292854u, 2161290883913306769u }, {  2979049660840976177u, 1350806802445816731u },
  { 17558870131333383934u, 1688508503057270913u }, {  8113529608884566205u, 2110635628821588642u },
  {  9682642023980241782u, 1319147268013492901u }, { 16714988548402690132u, 1648934085016866126u },
  { 11670363648648586857u, 2061167606271082658u }, { 11905663298832754689u, 1288229753919426661u },
  {  1047021068258779650u, 1610287192399283327u }, { 15143834390605638274u, 2012858990499104158u },
  {  4853210475701136017u, 1258036869061940099u }, {  1454827076199032118u, 1572546086327425124u },
  {  1818533845248790147u, 1965682607909281405u }, {  3442426662494187794u, 1228551629943300878u },
  { 13526405364972510550u, 1535689537429126097u }, {  3072948650933474476u, 1919611921786407622u },
  { 15755650962115585259u, 1199757451116504763u }, { 15082877684217093670u, 1499696813895630954u },
  {  9630225068416591280u, 1874621017369538693u }, {  8324733676974063502u, 1171638135855961683u },
  {  5794231077790191473u, 1464547669819952104u }, {  7242788847237739342u, 1830684587274940130u },
  { 18276858095901949986u, 2288355734093675162u }, { 16034722328366106645u, 1430222333808546976u },
  {  1596658836748081690u, 1787777917260683721u }, {  6607509564362490017u, 2234722396575854651u },
  {  1823850468512862308u, 1396701497859909157u }, {  6891499104068465790u, 1745876872324886446u },
  { 17837745916940358045u, 2182346090406108057u }, {  4231062170446641922u, 1363966306503817536u },
  {  5288827713058302403u, 1704957883129771920u }, {  6611034641322878003u, 2131197353912214900u },
  { 13355268687681574560u, 1331998346195134312u }, { 16694085859601968200u, 1664997932743917890u },
  { 11644235287647684442u, 2081247415929897363u }, {  4971804045566108824u, 1300779634956185852u },
  {  6214755056957636030u, 1625974543695232315u }, {  3156757802769657134u, 2032468179619040394u },
  {  6584659645158423613u, 1270292612261900246u }, { 17454196593302805324u, 1587865765327375307u },
  { 17206059723201118751u, 1984832206659219134u }, {  6142101308573311315u, 1240520129162011959u },
  {  3065940617289251240u, 1550650161452514949u }, {  8444111790038951954u, 1938312701815643686u },
  {   665883850346957067u, 1211445438634777304u }, {   832354812933696334u, 1514306798293471630u },
  { 10263815553021896226u, 1892883497866839537u }, { 17944099766707154901u, 1183052186166774710u },
  { 13206752671529167818u, 1478815232708468388u }, { 16508440839411459773u, 1848519040885585485u },
  { 12623618533845856310u, 1155324400553490928u }, { 15779523167307320387u, 1444155500691863660u },
  {  1277659885424598868u, 1805194375864829576u }, {  1597074856780748586u, 2256492969831036970u },
  {  5609857803915355770u, 1410308106144398106u }, { 16235694291748970521u, 1762885132680497632u },
  {  1847873790976661535u, 2203606415850622041u }, { 12684136165428883219u, 1377254009906638775u },
  { 11243484188358716120u, 1721567512383298469u }, {   219297180166231438u, 2151959390479123087u },
  {  7054589765244976505u, 1344974619049451929u }, { 13429923224983608535u, 1681218273811814911u },
  { 12175718012802122765u, 2101522842264768639u }, { 14527352785642408584u, 1313451776415480399u },
  { 13547504963625622826u, 1641814720519350499u }, { 12322695186104640628u, 2052268400649188124u },
  { 16925056528170176201u, 1282667750405742577u }, {  7321262604930556539u, 1603334688007178222u },
  { 18374950293017971482u, 2004168360008972777u }, {  4566814905495150320u, 1252605225005607986u },
  { 14931890668723713708u, 1565756531257009982u }, {  9441491299049866327u, 1957195664071262478u },
  {  1289246043478778550u, 1223247290044539049u }, {  6223243572775861092u, 1529059112555673811u },
  {  3167368447542438461u, 1911323890694592264u }, {  1979605279714024038u, 1194577431684120165u },
  {  7086192618069917952u, 1493221789605150206u }, { 18081112809442173248u, 1866527237006437757u },
  { 13606538515115052232u, 1166579523129023598u }, {  7784801107039039482u, 1458224403911279498u },
  {   507629346944023544u, 1822780504889099373u }, {  5246222702107417334u, 2278475631111374216u },
  {  3278889188817135834u, 1424047269444608885u }, {  8710297504448807696u, 1780059086805761106u }
};

// TODO : manually inline these if used one time (or a couple of times ? keep the asserts ?)

// Returns e == 0 ? 1 : [log_2(5^e)]; requires 0 <= e <= 3528.
static uint32_t log10Pow2(int32_t e){
    // The first value this approximation fails for is 2^1651 which is just greater than 10^297.
    assert(e >= 0);
    assert(e <= 1650);
    return (((uint32_t)e) * 78913) >> 18;
}

// Returns floor(log_10(5^e)); requires 0 <= e <= 2620.
static uint32_t log10Pow5(int32_t e) {
  // The first value this approximation fails for is 5^2621 which is just greater than 10^1832.
  assert(e >= 0);
  assert(e <= 2620);
  return (((uint32_t) e) * 732923) >> 20;
}

static bool multipleOfPowerOf2(uint64_t value, uint32_t p) {
  assert(value != 0);
  assert(p < 64);
  // __builtin_ctzll doesn't appear to be faster here.
  return (value & ((1ull << p) - 1)) == 0;
}

// Returns e == 0 ? 1 : ceil(log_2(5^e)); requires 0 <= e <= 3528.
static int32_t pow5bits(int32_t e) {
  // This approximation works up to the point that the multiplication overflows at e = 3529.
  // If the multiplication were done in 64 bits, it would fail at 5^4004 which is just greater
  // than 2^9297.
  assert(e >= 0);
  assert(e <= 3528);
  return (int32_t)(((((uint32_t) e) * 1217359) >> 19) + 1);
}

static uint32_t pow5Factor(uint64_t value) {
  const uint64_t m_inv_5 = 14757395258967641293u; // 5 * m_inv_5 = 1 (mod 2^64)
  const uint64_t n_div_5 = 3689348814741910323u;  // #{ n | n = 0 (mod 2^64) } = 2^64 / 5
  uint32_t count = 0;
  while (true) {
    assert(value != 0);
    value *= m_inv_5;
    if (value > n_div_5){
      break;
    }
    count++;
  }
  return count;
}


// Returns true if value is divisible by 5^p.
static bool multipleOfPowerOf5(uint64_t value, uint32_t p) {
  // I tried a case distinction on p, but there was no performance difference.
  return pow5Factor(value) >= p;
}

#if defined(HAS_UINT128)
typedef __uint128_t uint128_t;

// Best case: use 128-bit type.
static uint64_t mulShift64(uint64_t m, const uint64_t* mul, int32_t j) {
  uint128_t b0 = ((uint128_t)m) * mul[0];
  uint128_t b2 = ((uint128_t)m) * mul[1];
  return (uint64_t) (((b0 >> 64) + b2) >> (j - 64));
}

static inline uint64_t mulShiftAll64(const uint64_t m, const uint64_t* const mul, const int32_t j,
  uint64_t* const upper_bound, uint64_t* const lower_bound, const uint32_t mmShift) {
  *upper_bound = mulShift64(4 * m + 2, mul, j);
  *lower_bound = mulShift64(4 * m - 1 - mmShift, mul, j);
  return mulShift64(4 * m, mul, j);
}

#elif defined(HAS_64_BIT_INTRINSICS)

#include <intrin.h>

// Returns the lower 64 bits of (hi*2^64 + lo) >> dist, with 0 < dist < 64.
static inline uint64_t shiftright128(const uint64_t lo, const uint64_t hi, const uint32_t dist) {
  // For the __shiftright128 intrinsic, the shift value is always
  // modulo 64.
  // In the current implementation of the double-precision version
  // of Ryu, the shift value is always < 64. (In the case
  // RYU_OPTIMIZE_SIZE == 0, the shift value is in the range [49, 58].
  // Otherwise in the range [2, 59].)
  // However, this function is now also called by s2d, which requires supporting
  // the larger shift range (TODO: what is the actual range?).
  // Check this here in case a future change requires larger shift
  // values. In this case this function needs to be adjusted.
  assert(dist < 64);
  return __shiftright128(lo, hi, (unsigned char) dist);
}

static uint64_t mulShift64(uint64_t m, const uint64_t* mul, int32_t j) {
  // m is maximum 55 bits
  uint64_t high1;                                   // 128
  const uint64_t low1 = _umul128(m, mul[1], &high1); // 64
  uint64_t high0;                                   // 64
  _umul128(m, mul[0], &high0);                       // 0
  const uint64_t sum = high0 + low1;
  if (sum < high0) {
    high1++; // overflow into high1
  }
  return shiftright128(sum, high1, j - 64);
}

static inline uint64_t mulShiftAll64(uint64_t m, const uint64_t* const mul, int32_t j,
  uint64_t* upper_bound, uint64_t* lower_bound, uint32_t mmShift) {
  *upper_bound = mulShift64(4 * m + 2, mul, j);
  *lower_bound = mulShift64(4 * m - 1 - mmShift, mul, j);
  return mulShift64(4 * m, mul, j);
}

#else

static uint64_t umul128(uint64_t a, uint64_t b, uint64_t* productHi) {
  // The casts here help MSVC to avoid calls to the __allmul library function.
  const uint32_t aLo = (uint32_t)a;
  const uint32_t aHi = (uint32_t)(a >> 32);
  const uint32_t bLo = (uint32_t)b;
  const uint32_t bHi = (uint32_t)(b >> 32);

  const uint64_t b00 = (uint64_t)aLo * bLo;
  const uint64_t b01 = (uint64_t)aLo * bHi;
  const uint64_t b10 = (uint64_t)aHi * bLo;
  const uint64_t b11 = (uint64_t)aHi * bHi;

  const uint32_t b00Lo = (uint32_t)b00;
  const uint32_t b00Hi = (uint32_t)(b00 >> 32);

  const uint64_t mid1 = b10 + b00Hi;
  const uint32_t mid1Lo = (uint32_t)(mid1);
  const uint32_t mid1Hi = (uint32_t)(mid1 >> 32);

  const uint64_t mid2 = b01 + mid1Lo;
  const uint32_t mid2Lo = (uint32_t)(mid2);
  const uint32_t mid2Hi = (uint32_t)(mid2 >> 32);

  const uint64_t pHi = b11 + mid1Hi + mid2Hi;
  const uint64_t pLo = ((uint64_t)mid2Lo << 32) | b00Lo;

  *productHi = pHi;
  return pLo;
}

static uint64_t shiftright128(uint64_t lo, uint64_t hi, uint32_t dist) {
  // We don't need to handle the case dist >= 64 here (see above).
  assert(dist < 64);
  assert(dist > 0);
  return (hi << (64 - dist)) | (lo >> dist);
}

static uint64_t mulShift64(uint64_t m, const uint64_t* mul, int32_t j) {
  // m is maximum 55 bits
  uint64_t high1;                                   // 128
  uint64_t low1 = umul128(m, mul[1], &high1); // 64
  uint64_t high0;                                   // 64
  umul128(m, mul[0], &high0);                       // 0
  uint64_t sum = high0 + low1;
  if (sum < high0) {
    high1++; // overflow into high1
  }
  return shiftright128(sum, high1, j - 64);
}

// This is faster if we don't have a 64x64->128-bit multiplication.
static inline uint64_t mulShiftAll64(uint64_t m, const uint64_t* const mul, const int32_t j,
  uint64_t* const vp, uint64_t* const vm, const uint32_t mmShift) {
  m <<= 1;
  // m is maximum 55 bits
  uint64_t tmp;
  const uint64_t lo = umul128(m, mul[0], &tmp);
  uint64_t hi;
  const uint64_t mid = tmp + umul128(m, mul[1], &hi);
  hi += mid < tmp; // overflow into hi

  const uint64_t lo2 = lo + mul[0];
  const uint64_t mid2 = mid + mul[1] + (lo2 < lo);
  const uint64_t hi2 = hi + (mid2 < mid);
  *vp = shiftright128(mid2, hi2, (uint32_t) (j - 64 - 1));

  if (mmShift == 1) {
    const uint64_t lo3 = lo - mul[0];
    const uint64_t mid3 = mid - mul[1] - (lo3 > lo);
    const uint64_t hi3 = hi - (mid3 > mid);
    *vm = shiftright128(mid3, hi3, (uint32_t) (j - 64 - 1));
  } else {
    const uint64_t lo3 = lo + lo;
    const uint64_t mid3 = mid + mid + (lo3 < lo);
    const uint64_t hi3 = hi + hi + (mid3 < mid);
    const uint64_t lo4 = lo3 - mul[0];
    const uint64_t mid4 = mid3 - mul[1] - (lo4 > lo3);
    const uint64_t hi4 = hi3 - (mid4 > mid3);
    *vm = shiftright128(mid4, hi4, (uint32_t) (j - 64));
  }

  return shiftright128(mid, hi, (uint32_t) (j - 64 - 1));
}
#endif

// create a double decimal, except for the small int case
static struct double_decimal create_double_decimal(uint64_t ieeeMantissa, uint32_t ieeeExponent){
    int32_t exponent2;
    uint64_t mantissa2;

    if (ieeeExponent == 0){
        // subnormal numbers
        // real mantissa form : 0.mantissa bits
        // Ryu comment : "We subtract 2 so that the bounds computation has 2 additional bits."
        exponent2 = 1 - DOUBLE_BIAS - MANTISSA_NB_BITS - 2;
        mantissa2 = ieeeMantissa;
    } else {
        // normal case
        // real mantisa form : 1.mantissa bits
        exponent2 = (int32_t)ieeeExponent - DOUBLE_BIAS - MANTISSA_NB_BITS - 2;
        mantissa2 = (1ULL << MANTISSA_NB_BITS) | ieeeMantissa; // add the 1 at the start
    }

    bool is_even = (mantissa2 & 1) == 0;
    bool accept_bounds = is_even; // if we should round to the boundary range (according to the IEEE-754 rounding rule)

    // Determine the interval of valid representation

    uint64_t scaled_mantissa = 4 * mantissa2;
    uint32_t mmShift = ieeeMantissa != 0 || ieeeExponent <= 1; // is 0 only when a power of two (special handling because lower bound is closer than the upper bound)

    // Convert to a decimal power base using 128-bit arithmetic

    int32_t e10;
    uint64_t scaled_decimal_value, upper_bound, lower_bound;
    bool lower_bound_is_trailing_zeros = false;
    bool scaled_val_is_trailing_zeros = false;
    if (exponent2 >= 0){
        // positive exponent (probably very large integer number)
        uint32_t q = log10Pow2(exponent2) - (exponent2 > 3); // around floor(log10(2^exponent2)) = how many decimal digits 2^exponent2 has
        e10 = (int32_t)q;

        // shift amounts
        int32_t k = DOUBLE_POW5_INV_BITCOUNT + pow5bits(q) - 1;
        int32_t i = -exponent2 + q + k;

        
        // multiply mantissa2 with the inverse fivth power plus bit shifts to find the scaled decimal value and the upper and lower bound
        scaled_decimal_value = mulShiftAll64(mantissa2, DOUBLE_POW5_INV_SPLIT[q], i, &upper_bound, &lower_bound, mmShift); 

        if (q <= 21) {
            // detect trailing zeros (rounding optimization)
            uint32_t scaled_mantissa_mod_5 = scaled_mantissa % 5;
            if (scaled_mantissa_mod_5 == 0){
                scaled_val_is_trailing_zeros = multipleOfPowerOf5(scaled_mantissa, q);
            } else if (accept_bounds){
                // Same as min(e2 + (~mm & 1), pow5Factor(mm)) >= q
                // <=> e2 + (~mm & 1) >= q && pow5Factor(mm) >= q
                // <=> true && pow5Factor(mm) >= q, since e2 >= q.
                scaled_val_is_trailing_zeros = multipleOfPowerOf5(scaled_mantissa - 1 - mmShift, q);
            } else {
                upper_bound -= multipleOfPowerOf5(scaled_mantissa + 2, q); // = min(e2 + 1, pow5Factor(mp)) >= q.
            }
        }
    } else {
        // fractional (negative binary exponent)

        uint32_t q = log10Pow5(-exponent2) - (-exponent2 > 1); // = max(0, log10Pow5(-e2) - 1)
        e10 = (int32_t)q + exponent2;
        int32_t i = -exponent2 - (int32_t) q;
        int32_t k = pow5bits(i) - DOUBLE_POW5_BITCOUNT;
        int32_t j = (int32_t) q - k;

        scaled_decimal_value = mulShiftAll64(mantissa2, DOUBLE_POW5_SPLIT[i], j, &upper_bound, &lower_bound, mmShift);

        if (q <= 1){
            // very small numbers, so ryu can deduce trailing zeros
            scaled_val_is_trailing_zeros = true;
            if (accept_bounds){
                lower_bound_is_trailing_zeros = mmShift == 1;
            } else {
                upper_bound--;
            }
        } else if (q < 63){
            // moderate q (checks if value has at least q zeros)
            scaled_val_is_trailing_zeros = multipleOfPowerOf2(scaled_mantissa, q); // is scaled_mantissa divisible by 2^q
        }
    }

    // find the shortest decimal representation between the lower and upper bound
    // try to remove digits while it is still between the bound (on average remove around 2 digits)

    uint8_t lastRemovedDigit = 0;
    int32_t digit_removed_nb = 0;
    uint64_t output;
    if (lower_bound_is_trailing_zeros || scaled_val_is_trailing_zeros){
        // general case, which happens rarely (around 0.7%)
        while (true){
            uint64_t upper_div_10 = upper_bound/10;
            uint64_t lower_div_10 = lower_bound/10;
            if (upper_div_10 <= lower_div_10){
                break;
            }
            uint32_t lower_mod_10 = ((uint32_t)lower_bound) - 10 * ((uint32_t)lower_div_10);

            uint64_t scaled_val_div_10 = scaled_decimal_value/10;
            uint32_t scaled_val_mod_10 = ((uint32_t)scaled_decimal_value) - 10 * ((uint32_t)scaled_val_div_10); 
            lower_bound_is_trailing_zeros &= lower_mod_10 == 0;
            scaled_val_is_trailing_zeros &= lastRemovedDigit == 0;
            lastRemovedDigit = (uint8_t)scaled_val_mod_10;
            
            scaled_decimal_value = scaled_val_div_10;
            upper_bound = upper_div_10;
            lower_bound = lower_div_10;
            digit_removed_nb++;
        }

        if (lower_bound_is_trailing_zeros){
            while (true){
                uint64_t lower_div_10 = lower_bound/10;
                uint32_t lower_mod_10 = ((uint32_t)lower_bound) - 10 * ((uint32_t)lower_div_10);
                if (lower_mod_10 != 0){
                    break;
                }
                uint64_t upper_div_10 = upper_bound/10;
                uint64_t scaled_val_div_10 = scaled_decimal_value/10;
                uint32_t scaled_val_mod_10 = ((uint32_t)scaled_decimal_value) - 10 * ((uint32_t)scaled_val_div_10);
                scaled_val_is_trailing_zeros &= lastRemovedDigit == 0;
                lastRemovedDigit = (uint8_t)scaled_val_mod_10;
                scaled_decimal_value = scaled_val_div_10;
                upper_bound = upper_div_10;
                lower_bound = lower_div_10;
                digit_removed_nb++;
            }
        }

        if (scaled_val_is_trailing_zeros && lastRemovedDigit == 5 && scaled_decimal_value % 2 == 0){
            // Round even if the exact number is .....50..0.
            lastRemovedDigit = 4;
        }
        output = scaled_decimal_value + ((scaled_decimal_value == lower_bound && (!accept_bounds || !lower_bound_is_trailing_zeros)) || lastRemovedDigit >= 5); 
    } else {
        // Specialized for the common case (~99.3%). Percentages below are relative to this.
        bool round_up = false;
        uint64_t upper_div_100 = upper_bound/100;
        uint64_t lower_div_100 = lower_bound/100;
        if (upper_div_100 > lower_div_100){ // remove two digits at a time (~86.2%)
            uint64_t scaled_val_div_100 = scaled_decimal_value/100;
            uint32_t saled_val_mod_100 = ((uint32_t)scaled_decimal_value) - 100 * ((uint32_t)scaled_val_div_100);
            round_up = saled_val_mod_100 >= 50;
            scaled_decimal_value = scaled_val_div_100;
            upper_bound = upper_div_100;
            lower_bound = lower_div_100;
            digit_removed_nb += 2;
        }
        
        // Loop iterations below (approximately), without optimization above:
        // 0: 0.03%, 1: 13.8%, 2: 70.6%, 3: 14.0%, 4: 1.40%, 5: 0.14%, 6+: 0.02%
        // Loop iterations below (approximately), with optimization above:
        // 0: 70.6%, 1: 27.8%, 2: 1.40%, 3: 0.14%, 4+: 0.02%
        while (true){
            uint64_t upper_div_10 = upper_bound/10;
            uint64_t lower_div_10 = lower_bound/10;
            if (upper_div_10 <= lower_div_10){
                break;
            }
            uint64_t scaled_val_div_10 = scaled_decimal_value/10;
            uint32_t scaled_val_mod_10 = ((uint32_t)scaled_decimal_value) - 10 * ((uint32_t)scaled_val_div_10);
            round_up = scaled_val_mod_10 >= 5;
            scaled_decimal_value = scaled_val_div_10;
            upper_bound = upper_div_10;
            lower_bound = lower_div_10;
            digit_removed_nb++;
        }
        output = scaled_decimal_value + (scaled_decimal_value == lower_bound || round_up);
    }

    int32_t exp = e10 + digit_removed_nb;

    return (struct double_decimal){
        .exponent = exp,
        .mantissa = output,
    };
}

#define INF_STR "inf"
#define NAN_STR "nan"
#define ZERO_STR "0.0"

static size_t double_to_str_special_strings(char* buf, bool is_negative, uint32_t ieeeExponent, uint64_t ieeeMantissa){
    if (ieeeMantissa != 0){
        memcpy(buf, NAN_STR, sizeof(NAN_STR));
        return sizeof(NAN_STR);
    }

    size_t pos = 0;
    if (is_negative){
        *buf = '-';
        pos = 1;
    }

    if (ieeeExponent != 0){
        // infinite
        memcpy(buf + pos, INF_STR, sizeof(INF_STR));
        return sizeof(INF_STR) + pos;
    } else {
        memcpy(buf + pos, ZERO_STR, sizeof(ZERO_STR));
        return sizeof(ZERO_STR) + pos;
    }
}


static uint32_t decimalLength17(uint64_t v) {
    // This is slightly faster than a loop.
    // The average output length is 16.38 digits, so we check high-to-low.
    // Function precondition: v is not an 18, 19, or 20-digit number.
    // (17 digits are sufficient for round-tripping.)
    assert(v < 100000000000000000L);
    if (v >= 10000000000000000L) { return 17; }
    if (v >= 1000000000000000L) { return 16; }
    if (v >= 100000000000000L) { return 15; }
    if (v >= 10000000000000L) { return 14; }
    if (v >= 1000000000000L) { return 13; }
    if (v >= 100000000000L) { return 12; }
    if (v >= 10000000000L) { return 11; }
    if (v >= 1000000000L) { return 10; }
    if (v >= 100000000L) { return 9; }
    if (v >= 10000000L) { return 8; }
    if (v >= 1000000L) { return 7; }
    if (v >= 100000L) { return 6; }
    if (v >= 10000L) { return 5; }
    if (v >= 1000L) { return 4; }
    if (v >= 100L) { return 3; }
    if (v >= 10L) { return 2; }
    return 1;
}


static int double_decimal_to_chars(struct double_decimal v, bool sign, char* result) {
    int index = 0;

    if (sign){
        result[index++] = '-';
    }

    uint64_t mantissa = v.mantissa;
    int32_t exp10 = v.exponent;

    uint32_t length = decimalLength17(mantissa);

    int32_t kk = (int32_t)length + exp10;

    if (-5 < kk && kk <= 16){
        // fixed format
        if (exp10 >= 0){
            // integer
            int_to_string_impl(result + index, (int64_t)mantissa, length);

            index += length;
            memset(result + index, '0', exp10);
            index += exp10;

            // append ".0"
            result[index++] = '.';
            result[index++] = '0';
            return index;
        } else if (kk > 0){
            // decimal inside digits
            int_to_string_impl(result + index, (int64_t)mantissa, length);
            memmove(result + index + kk + 1,
                    result + index + kk,
                    length - kk);

            result[index + kk] = '.';

            index += length + 1;
            return index;
        } else {
            // Case 3: leading zeros
            result[index++] = '0';
            result[index++] = '.';

            int32_t zeros = -kk;
            memset(result + index, '0', zeros);
            index += zeros;

            int_to_string_impl(result + index, (int64_t)mantissa, length);
            index += length;

            return index;
        }
    }

    int_to_string_impl(result + index, (int64_t)mantissa, length);

    // move first digit
    result[index] = result[index + 1];
    result[index + 1] = '.';

    result[index + length + 1] = 'e';

    int32_t exp = kk - 1;
    int exp_index = index + length + 2;

    if (exp < 0) {
        result[exp_index++] = '-';
        exp = -exp;
    }

    if (exp >= 100) {
        int32_t c = exp % 10;
        memcpy(result + exp_index, digit_pairs + 2 * (exp / 10), 2);
        result[exp_index + 2] = (char)('0' + c);
        exp_index += 3;
    } else if (exp >= 10) {
        memcpy(result + exp_index, digit_pairs + 2 * exp, 2);
        exp_index += 2;
    } else {
        result[exp_index++] = (char)('0' + exp);
    }

    return exp_index;
}

static size_t ryu_double_to_string_impl(char* buf, double d) {
    uint64_t d_bits = INTO_TYPE(uint64_t, d);
    bool is_negative = (d_bits >> 63) & 0x1;
    uint32_t ieeeExponent = (uint32_t)((d_bits >> MANTISSA_NB_BITS) & EXPONENT_ALL_1);
    uint64_t ieeeMantissa = d_bits & ((1ULL << MANTISSA_NB_BITS) - 1);


    if ((ieeeExponent == 0 && ieeeMantissa == 0) || ieeeExponent == EXPONENT_ALL_1){
        return double_to_str_special_strings(buf, is_negative, ieeeExponent, ieeeMantissa);
    }

    struct double_decimal v;
    bool is_small_integer = is_small_int(ieeeMantissa, ieeeExponent, &v);

    if (is_small_integer){
        // small integers in the range [1, 2^53)
        // remove trailing zeros in the mantissa
        while (true){
            uint64_t quotient = v.mantissa / 10;
            uint32_t rest = v.mantissa - quotient * 10; // same as v.mantissa % 10

            if (rest != 0){
                break;
            }

            v.mantissa = quotient;
            v.exponent++;
        }
    } else {
        v = create_double_decimal(ieeeMantissa, ieeeExponent);
    }

    return double_decimal_to_chars(v, is_negative, buf);
}


static void list_format(struct str* str, struct ListNode* list);
static void ensure_size_string(struct str* s, size_t size);

// TODO : make these functions wrappers for other functions for when we already know that we have allocated to not have to have check the capacity (for ex for the ryu formatting) 
static void format_int(struct str* str, int64_t i){
    int digit_number = digit_nb(i);
    size_t buf_size = (i < 0) ? digit_number + 1 : digit_number;
    ensure_size_string(str, str->len + buf_size);
    int_to_string_impl(str->buf + str->len, i, digit_number);
    str->len += buf_size;
}

static void format_float(struct str* str, double d){
    ensure_size_string(str, str->len + MAX_DOUBLE_BUF_SIZE);
    str->len += ryu_double_to_string_impl(str->buf + str->len, d);
}

static void format_bool(struct str* str, bool b){
    const char* bool_str = __bool_to_str(b);
    size_t bool_str_len = strlen(bool_str);
    ensure_size_string(str, str->len + bool_str_len);
    memcpy(str->buf + str->len, bool_str, bool_str_len);
    str->len += bool_str_len;
}

static void format_str(struct str* str, const char* s){
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

    ensure_size_string(str, str->len + 4); // need always at least 4 bytes

    if (c <= 0x007F){
        str->buf[str->len] = (char)c;
        str->len += 1;
    } else if (c <= 0x07FF){
        uint8_t byte_1 = (uint8_t)((c >> 6 | 0xC0) & 0xDF); // | 11000000 & 11011111
        uint8_t byte_2 = ((uint8_t)c | 0x80) & 0xBF; // | 10000000 & 10111111
        (str->buf + str->len)[0] = byte_1;
        (str->buf + str->len)[1] = byte_2;
        str->len += 2;
    } else if (c <= 0xFFFF){
        uint8_t bytes[3];
        bytes[0] = ((uint8_t)(c >> 12) | 0xE0) & 0xEF; // | 11100000 & 11101111
        bytes[1] = ((uint8_t)(c >> 6) | 0x80) & 0xBF;
        bytes[2] = ((uint8_t)c | 0x80) & 0xBF;
        memcpy(str->buf + str->len, bytes, 3);
        str->len += 3;
    } else if (c <= 0x10FFFF){
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
        case UNIT_TYPE:
            ensure_size_string(str, 2);
            const char* unit = "()";
            memcpy(str->buf + str->len, unit, 2);
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
    if (size > s->capacity){
        while (size > s->capacity){
            s->capacity *= 2;
        }
        s->buf = REALLOC(s->buf, s->capacity);
    }
}

static struct str str_init(size_t default_capacity) {
    char* buf = MALLOC_NO_PTR(sizeof(char) * default_capacity);
    if (!buf){
        ALLOC_ERROR("str init");
    }
    return (struct str){
        .buf = buf,
        .capacity = default_capacity,
        .len = 0,
    };
}

__attribute__((returns_nonnull))
static char* vformat_string(const char* format, va_list va){
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
                        format_bool(&str, (bool)va_arg(va, uint32_t));
                        break;
                    case 'l':
                        list_format(&str, va_arg(va, struct ListNode*));
                        break;
                    // TODO : add unit %u
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

__attribute__((returns_nonnull))
char* __format_string(const char* format, ...){
    va_list va;
    va_start(va, format);
    char* s = vformat_string(format, va);
    va_end(va);
    return s;
}

__attribute__((returns_nonnull))
const char* __char_to_str(uint32_t c){
    const int buf_size = 5; // 4 bytes max for codepoint + null byte
    char* buf = MALLOC_NO_PTR(sizeof(char) * buf_size);
    if (!buf){
        ALLOC_ERROR("char to str");
    }

    struct str s = (struct str){
        .buf = buf,
        .len = 0,
        .capacity = buf_size,
    };
    
    format_char(&s, c);
    s.buf[s.len] = '\0';
    return s.buf;
}

// TODO : return a struct instead ?
__attribute__((malloc, alloc_size(2), returns_nonnull))
static uint32_t* utf8_decode_str(const char* restrict str, size_t str_len, size_t* restrict codepoints_len){
    uint32_t* codepoint_buf = MALLOC_NO_PTR(sizeof(uint32_t) * str_len);
    if (!codepoint_buf){
        ALLOC_ERROR("utf8 decode str");
    }
    size_t i = 0;
    *codepoints_len = 0;
    while (i < str_len){
        codepoint_buf[*codepoints_len] = utf8_decode_char(str, &i, str_len);
        (*codepoints_len)++;
    }
    if (*codepoints_len < str_len){
        codepoint_buf = REALLOC(codepoint_buf, sizeof(uint32_t) * *codepoints_len);
    }

    return codepoint_buf;
}

static void list_write_file_no_new_line(struct ListNode* list, FILE* f);

// TODO : transform in the future into a print_val function
static void list_node_write_file(uint8_t tag, Val val, FILE* f){
    // TODO : transform this into a switch ?
    if (tag == INT_TYPE) {
        char buf[MAX_INT_BUF_SIZE];
        struct str s = (struct str){
            .buf = buf,
            .capacity = MAX_INT_BUF_SIZE,
            .len = 0,
        };
        int64_t i = INTO_TYPE(int64_t, val);
        format_int(&s, i);
        fwrite(buf, sizeof(char), s.len, f);
    } else if (tag == FLOAT_TYPE){
        char buf[MAX_DOUBLE_BUF_SIZE];
        struct str s = (struct str){
            .buf = buf,
            .capacity = MAX_DOUBLE_BUF_SIZE,
            .len = 0,
        };
        double d = INTO_TYPE(double, val);
        format_float(&s, d);
        fwrite(buf, sizeof(char), s.len, f);
    } else if (tag == BOOL_TYPE){
        uint8_t b = INTO_TYPE(uint8_t, val);
        ASSERT_BOOL(b);
        const char* s = __bool_to_str((bool)b);
        size_t s_len = strlen(s);
        fwrite(s, sizeof(char), s_len, f);
    } else if (tag == STR_TYPE){
        const char* s = INTO_TYPE(char*, val);
        ASSERT_NOT_NULL(s);
        size_t s_len = strlen(s);
        fwrite(s, sizeof(char), s_len, f);
    } else if (tag == CHAR_TYPE){
        // use this instead of format_char to prevent useless heap allocations
        char buf[4];
        struct str s = (struct str){
            .buf = buf,
            .len = 0,
            .capacity = 4,
        };
        uint32_t c = INTO_TYPE(uint32_t, val);
        format_char(&s, c);
        fwrite(buf, sizeof(char), s.len, f);
    } else if (tag == UNIT_TYPE){
        const char* unit = "()";
        fwrite(unit, sizeof(char), 2, f);
    } else if (tag == LIST_TYPE){
        struct ListNode* list = INTO_TYPE(struct ListNode*, val);
        ASSERT_NOT_NULL(list);
        list_write_file_no_new_line(list, f);
    } else {
        fprintf(stderr, "ERROR : WRONG TAGS IN LIST IN PRINT (BUG IN COMPILER  \?\?)\n");
        exit(1);
    }
}

static void list_write_file_no_new_line(struct ListNode* list, FILE* f){
    bool first = true;
    const char open_square_bracket = '[';
    fwrite(&open_square_bracket, sizeof(char), 1, f);
    const char* comma = ", ";
    while (list != NULL){
        if (!first){
            fwrite(comma, sizeof(char), 2, f);
        } 
        list_node_write_file(list->type_tag, list->val, f);
        list = list->next;
        first = false;
    }
    const char close_square_bracket = ']';
    fwrite(&close_square_bracket, sizeof(char), 1, f);
}

static void vec_write_file_no_new_line(struct VecVal* vec_val, FILE* f){
    const char* open_vec = "vec[";
    fwrite(open_vec, sizeof(char), 4, f);
    const char* comma = ", ";
    // TODO : put the switch to the outside of the for to optimize it ?
    uint8_t type_tag = vec_val->element_type_tag;
    if (type_tag == INT_TYPE){
        const int64_t* int_buf = vec_val->buf;
        char buf[MAX_INT_BUF_SIZE];
        for (uint32_t i = 0; i < vec_val->size; i++){
            if (i != 0){
                fwrite(comma, sizeof(char), 2, f);
            }
            memset(buf, 0, sizeof(buf));
            struct str s = (struct str){
                .buf = buf,
                .capacity = MAX_INT_BUF_SIZE,
                .len = 0,
            };
            int64_t vec_i = int_buf[i];
            format_int(&s, vec_i);
            fwrite(buf, sizeof(char), s.len, f);
        }
    } else if (type_tag == FLOAT_TYPE){
        const double* float_buf = vec_val->buf;
        for (uint32_t i = 0; i < vec_val->size; i++){
            if (i != 0){
                fwrite(comma, sizeof(char), 2, f);
            }
            char buf[MAX_DOUBLE_BUF_SIZE];
            struct str s = (struct str){
                .buf = buf,
                .capacity = MAX_DOUBLE_BUF_SIZE,
                .len = 0,
            };
            double d = float_buf[i];
            format_float(&s, d);
            fwrite(buf, sizeof(char), s.len, f);
        }
    } else {
        fprintf(stderr,"Unknown tag of vec : %d\n", vec_val->element_type_tag);
        exit(1);
    }

    const char close_vec = ']';
    fwrite(&close_vec, sizeof(char), 1, f);
}

// print with a \n
static void vwrite_val_file(const char* format, va_list va, FILE* f){
    if (*format == '%'){
        format++;
        switch (*format){
            case 'd':
                format++;
                int64_t i = va_arg(va, int64_t);
                char buf_int[MAX_INT_BUF_SIZE];
                struct str str_int = (struct str){
                    .buf = buf_int,
                    .capacity = MAX_INT_BUF_SIZE,
                    .len = 0,
                };
                format_int(&str_int, i);
                // TODO : use something lower level than fwrite ?
                fwrite(buf_int, sizeof(char), str_int.len, f);
                break;

            case 'l':
                format++;
                struct ListNode* list = va_arg(va, struct ListNode*);
                list_write_file_no_new_line(list, f);
                break;

            case 'f':
                format++;
                char buf_float[MAX_DOUBLE_BUF_SIZE];
                struct str str_float = (struct str){
                    .buf = buf_float,
                    .capacity = MAX_DOUBLE_BUF_SIZE,
                    .len = 0,
                };
                double d = va_arg(va, double);
                format_float(&str_float, d);
                fwrite(buf_float, sizeof(char), str_float.len, f);
                break;
            case 's':
                format++;
                char* s = va_arg(va, char*);
                size_t s_len = strlen(s);
                fwrite(s, sizeof(char), s_len, f);
                break;
            case 'c':
                format++;
                uint32_t c = va_arg(va, uint32_t);
                char buf_char[4];
                struct str str_char = (struct str){
                    .buf = buf_char,
                    .len = 0,
                    .capacity = 4,
                };
                format_char(&str_char, c);
                fwrite(buf_char, sizeof(char), str_char.len, f);
                break;
            case 'C':
                format++;
                // TODO : case of Ctypes
                if (*format == 'u'){
                    format++;
                    if (*format == '6' && format[1] == '4'){
                        format += 2;
                        // u64
                        fprintf(f, "%ld", va_arg(va, uint64_t)); // TODO ? (not use fprintf ?)
                    }
                }
                break;
            case 'b':
                format++;
                bool b = (bool)va_arg(va, uint32_t);
                const char* bool_str = __bool_to_str(b);
                size_t bool_str_len = strlen(bool_str);
                fwrite(bool_str, sizeof(char), bool_str_len, f);
                break;

            case 'u':
                format++;
                const char* s_unit = "()";
                fwrite(s_unit, sizeof(char), 2, f);
                break;

            case 'v':
                format++;
                struct VecVal* vec_val = (struct VecVal*)va_arg(va, struct VecVal*);
                vec_write_file_no_new_line(vec_val, f);
                break;
            
            case 'n':
                fprintf(stderr, "UNREACHABLE : trying to print a never type\n");
                exit(1);
            default:
                fprintf(stderr, "Unknown format in print : %%%c\n", *format);
                exit(1);
        }
    } else {
        char c = *format;
        fwrite(&c, sizeof(char), 1, stdout);
        format++;
    }
    char c = '\n';
    fwrite(&c, sizeof(char), 1, stdout);
}

void __print_val(const char* restrict format, ...){
    va_list va;
    va_start(va, format);
    vwrite_val_file(format, va, stdout);
    va_end(va);
}

// TODO : optimizations for simple regex (ex : just a word with no special chars, etc)

struct Node {
    // TODO : use variable size array (struct Transition[] but would need no Node array)
    // or fixed size maximum transitions (ex : struct Transition[X])
    struct Transition* outgoing_transitions;
    uint32_t transitions_nb;
};


typedef uint32_t NodeRef;
struct Transition {
    uint32_t char_start;
    uint32_t char_end;
    NodeRef end;
    bool is_epsilon;
};

struct Regex {
    // TODO : dfa
    NodeRef starting_state;
    NodeRef ending_state;
    struct Node* nfa_nodes;
    uint32_t node_count;
    uint32_t node_capacity; // TODO : put the capacity just in a local variable and pass a pointer to it in nfa_add_node to simplify the layout and reduce the size of Regex
};


static NodeRef nfa_add_node(struct Regex* re){
    if (re->nfa_nodes){
        if (re->node_count == re->node_capacity){
            re->node_capacity *= 2;
            re->nfa_nodes = REALLOC(re->nfa_nodes, re->node_capacity * sizeof(struct Node));
        }
    } else {
        re->nfa_nodes = MALLOC(sizeof(struct Node));
        re->node_capacity = 1;
    }
    NodeRef res = re->node_count;
    memset(re->nfa_nodes + res, 0, sizeof(struct Node));
    re->node_count++;
    return res;
}

static void _nfa_add_transition(struct Regex* re, NodeRef start, struct Transition transition){
    // add a capacity field to not realloc each time
    struct Node* node = &re->nfa_nodes[start];
    if (node->outgoing_transitions){
        node->outgoing_transitions = REALLOC(node->outgoing_transitions, (node->transitions_nb+1) * sizeof(struct Transition));
        node->outgoing_transitions[node->transitions_nb] = transition;
    } else {
        node->outgoing_transitions = MALLOC_NO_PTR(sizeof(struct Transition));
        *node->outgoing_transitions = transition;       
    }
    node->transitions_nb++;
}

static void nfa_add_transition_epsilon(struct Regex* re, NodeRef start, NodeRef end){
    _nfa_add_transition(re, start, (struct Transition){
        .is_epsilon = true,
        .char_start = 0,
        .char_end = 0,
        .end = end,
    });
}

static void nfa_add_transition(struct Regex* re, NodeRef start, uint32_t c, NodeRef end){
    _nfa_add_transition(re, start, (struct Transition){
        .char_start = c,
        .char_end = c,
        .end = end,
        .is_epsilon = false,
    });
}

static void nfa_add_transition_range(struct Regex* re, NodeRef start, uint32_t char_start, uint32_t char_end, NodeRef end){
    _nfa_add_transition(re, start, (struct Transition){
        .char_start = char_start,
        .char_end = char_end,
        .end = end,
        .is_epsilon = false,
    });
}


struct CharClassRange {
    uint32_t start;
    uint32_t end;
};

struct CharClassRanges {
    struct CharClassRange* ranges;
    size_t len;
};

struct CharClass {
    bool is_negated;
    // TODO : grow this with capacity
    struct CharClassRanges ranges;
};

struct BinaryNode {
    struct RegexASTNode* lhs;
    struct RegexASTNode* rhs;
};

// TODO : negated character class [^1-9] [^abc]
// TODO : have ranges transitions instead of single transitions (ex : range [a-z] is 1 transition instead of 26, 208 -> 8 bytes)
// TODO : multiple ranges (does [a-zA-z] should work ?) ?
// TODO : predefined classes : \d : digit, \w : word, \s : whitespace
// TODO : Start / end anchors ^ $
// TODO : Word boundary \b
// TODO : optimize exprs like x1|x2|x3|x4 (create multiple alternatives merged)
// TODO : flattened AST ?
// TODO : unicode
// TODO : escape regex special chars
// TODO : ensure that leftmost match wins, not longest (ex : a|ab matches "a")
// TODO : ensure no backreferences
// TODO : make all this parsing have more tail call ? (must tail attribute ?)
struct RegexASTNode {
    enum {
        AST_CHAR,
        AST_CHAR_CLASS,
        AST_CONCAT,
        AST_ALTER,
        AST_KLEENE,
        AST_PLUS,
        AST_OPTIONAL,
    } tag;
    union {
        uint32_t c;
        // TODO : put these in a ptr ?
        struct BinaryNode binary;
        struct RegexASTNode* unary_data;
        struct CharClass char_class;
    } data;
};


static void free_ast(struct RegexASTNode* ast){
    // TODO : should it be run when gc is enabled ?? (add a ifdef for it ?)
    switch (ast->tag){
        case AST_CHAR:
            break;
        case AST_CHAR_CLASS:
            // TODO
            FREE(ast->data.char_class.ranges.ranges);
            break;
        case AST_CONCAT:
        case AST_ALTER:
            free_ast(ast->data.binary.lhs);
            free_ast(ast->data.binary.rhs);
            break;
        case AST_KLEENE:
        case AST_PLUS:
        case AST_OPTIONAL:
            free_ast(ast->data.unary_data);
            break;
        default:
            fprintf(stderr, "Unknown tag in AST : %d\n", ast->tag);
            exit(1);
    }
    FREE(ast);
}

struct RegexParseContext {
    const uint32_t* codepoints;
    size_t codepoints_len;
    size_t pos;
};

#define ADVANCE(context) ({ \
        uint32_t c = context->codepoints[context->pos]; \
        context->pos++; \
        c; \
    })

#define PASS(CONTEXT, expected) do { \
        uint32_t c = ADVANCE(context); \
        if (c != (uint32_t)expected){ \
            fprintf(stderr, "error when parsing regex, expected %s, but got %s\n", __char_to_str(expected), __char_to_str(c)); \
            exit(1); \
        } \
    } while(0);

#define PEEK(context) context->codepoints[context->pos]

#define CHARS_AVAILABLE(context) (context->pos < context->codepoints_len)

static struct RegexASTNode* new_ast_node(struct RegexASTNode ast_node){
    struct RegexASTNode* res = MALLOC(sizeof(struct RegexASTNode));
    if (!res){
        ALLOC_ERROR("regex new node");
    }
    *res = ast_node;
    return res;
}

static struct RegexASTNode* parse_char(struct RegexParseContext* context){
    assert(CHARS_AVAILABLE(context));
    uint32_t c = ADVANCE(context);
    assert(c != U'|' && c != U')' && c != U'*' && c != U'+' && c != U'?');
    struct RegexASTNode* leaf = new_ast_node((struct RegexASTNode){
        .tag = AST_CHAR, 
        .data.c = c,
    });
    
    return leaf;
}

static void add_char_class_range(struct CharClassRanges* ranges, struct CharClassRange range){
    if (!ranges->ranges){
        ranges->ranges = MALLOC_NO_PTR(sizeof(struct CharClassRange));
        *ranges->ranges = range;
    } else {
        ranges->ranges = REALLOC(ranges->ranges, sizeof(struct CharClassRange) * (ranges->len + 1));
        ranges->ranges[ranges->len] = range;
    }
    ranges->len++;
}

static struct RegexASTNode* parse_char_class(struct RegexParseContext* context){
    // TODO : grow with capacity the patterns list
    struct CharClassRanges ranges = (struct CharClassRanges){0};
    PASS(context, '[');
    bool is_negated = false;
    if (CHARS_AVAILABLE(context) && PEEK(context) == U'^'){
        ADVANCE(context);
        is_negated = true;
    }
    
    bool has_prev_char = false;
    uint32_t prev_char;
    while (CHARS_AVAILABLE(context) && PEEK(context) != U']'){
        uint32_t c = ADVANCE(context);
        if (has_prev_char && c == U'-'){
            uint32_t end_range = ADVANCE(context);
            if (end_range < prev_char){
                fprintf(stderr, "Invalid range in char class : [%d-%d]\n", prev_char, end_range);
                exit(1);
            }
            struct CharClassRange range = (struct CharClassRange){
                .start = prev_char,
                .end = end_range,
            };
            add_char_class_range(&ranges, range);

            has_prev_char = false;
        } else {
            if (has_prev_char){
                struct CharClassRange range = (struct CharClassRange){
                    .start = prev_char,
                    .end = prev_char,
                };
                add_char_class_range(&ranges, range);
            }
            prev_char = c;
            has_prev_char = true;
        }
    }
    if (has_prev_char){
        struct CharClassRange range = (struct CharClassRange){
            .start = prev_char,
            .end = prev_char,
        };
        add_char_class_range(&ranges, range);
    }
    struct CharClass char_class = (struct CharClass){
        .is_negated = is_negated,
        .ranges = ranges,
    };
    struct RegexASTNode* char_class_node = new_ast_node((struct RegexASTNode){
        .tag = AST_CHAR_CLASS,
        .data.char_class = char_class,
    });
    PASS(context, ']');
    return char_class_node;
}

static struct RegexASTNode* parse_regex_ast(struct RegexParseContext* context);

static struct RegexASTNode* parse_regex_ast_leaf(struct RegexParseContext* context){
    assert(CHARS_AVAILABLE(context));
    uint32_t c = PEEK(context);
    if (c == U'('){
        ADVANCE(context);
        struct RegexASTNode* re = parse_regex_ast(context);
        PASS(context, ')');
        return re;
    } else if (c == U'['){
        return parse_char_class(context);
    }
    return parse_char(context);
}


// plus, kleene star and optional
static struct RegexASTNode* parse_postfix(struct RegexParseContext* context){
    struct RegexASTNode* re = parse_regex_ast_leaf(context);
    while (CHARS_AVAILABLE(context)){
        uint32_t c = PEEK(context);
        if (c == U'*'){
            ADVANCE(context);
            re = new_ast_node((struct RegexASTNode){
                .tag = AST_KLEENE,
                .data.unary_data = re,
            });
        } else if (c == U'+'){
            ADVANCE(context);
            re = new_ast_node((struct RegexASTNode){
                .tag = AST_PLUS,
                .data.unary_data = re,
            });
        } else if (c == U'?'){
            ADVANCE(context);
            re = new_ast_node((struct RegexASTNode){
                .tag = AST_OPTIONAL,
                .data.unary_data = re,
            });
        } else {
            break;
        }
    }
    return re;
}

static struct RegexASTNode* parse_concat(struct RegexParseContext* context){
    struct RegexASTNode* lhs = parse_postfix(context);
    while (CHARS_AVAILABLE(context)){
        uint32_t c = PEEK(context);
        if (c == U'|' || c == U')'){
            break;
        }
        struct RegexASTNode* rhs = parse_postfix(context);
        lhs = new_ast_node((struct RegexASTNode){
            .tag = AST_CONCAT,
            .data.binary = {
                .lhs = lhs,
                .rhs = rhs,
            },
        });
    }
    
    return lhs;
}

static struct RegexASTNode* parse_regex_alternative(struct RegexParseContext* context){
    struct RegexASTNode* lhs = parse_concat(context);
    while (CHARS_AVAILABLE(context) && PEEK(context) == '|'){
        ADVANCE(context);
        struct RegexASTNode* rhs = parse_concat(context);
        lhs = new_ast_node((struct RegexASTNode){
            .tag = AST_ALTER,
            .data.binary = {
                .lhs = lhs,
                .rhs = rhs,
            },
        });
    }
    return lhs;
}

static struct RegexASTNode* parse_regex_ast(struct RegexParseContext* context){
    return parse_regex_alternative(context);
}

static struct RegexASTNode* create_regex_ast(const char* str, size_t str_len) {
    // TODO : what to do if empty regex (for now bug/UB)

    size_t codepoints_len;
    uint32_t* codepoints = utf8_decode_str(str, str_len, &codepoints_len);

    struct RegexParseContext context = (struct RegexParseContext){
        .codepoints = codepoints,
        .codepoints_len = codepoints_len,
        .pos = 0,
    };
    struct RegexASTNode* ast = parse_regex_ast(&context);
    if (context.pos != context.codepoints_len) {
        fprintf(stderr, "error: trailing input in regex\n");
        exit(1);
    }
    FREE(codepoints);
    return ast;
}

struct NFAFragment {
    NodeRef start;
    NodeRef end;
};

// TODO : create functions to create epsilon transition and simple transition to make the code simpler ?

static struct NFAFragment regex_create_char_class(struct Regex* re, struct CharClass char_class){
    // TODO : need a lot of change for unicode support (especially for negation)
    bool is_negated = char_class.is_negated;
    NodeRef start = nfa_add_node(re);
    NodeRef end = nfa_add_node(re);
    
    struct CharClassRanges ranges_copy = (struct CharClassRanges){
        .ranges = MALLOC_NO_PTR(sizeof(struct CharClassRange) * char_class.ranges.len),
        .len = char_class.ranges.len,
    };
    memcpy(ranges_copy.ranges, char_class.ranges.ranges, sizeof(struct CharClassRange) * char_class.ranges.len);
    // TODO : put all these steps in separate functions ? 

    // TODO : use qsort instead ?
    // sort ranges
    for (size_t i = 1; i < ranges_copy.len; i++){
        struct CharClassRange range = ranges_copy.ranges[i];
        size_t j = i;
        while (j > 0 && ranges_copy.ranges[j-1].start > range.start){
            ranges_copy.ranges[j] = ranges_copy.ranges[j-1];
            j--; 
        }
        ranges_copy.ranges[j] = range;
    }

#define MIN(a, b) ((a) < (b)) ? (a) : (b)
#define MAX(a, b) ((a) > (b)) ? (a) : (b)

    // merge ranges
    size_t i = 1;
    while (i < ranges_copy.len){
        struct CharClassRange* range_first = &ranges_copy.ranges[i-1];
        struct CharClassRange* range_second = &ranges_copy.ranges[i];
        if (range_first->end + 1 >= range_second->start){
            *range_first = (struct CharClassRange){
                .start = MIN(range_first->start, range_second->start),
                .end = MAX(range_first->end, range_second->end),
            };
            // TODO : optimize to not have to memmove ?
            memmove(ranges_copy.ranges + i, ranges_copy.ranges + i + 1, (ranges_copy.len - i - 1) * sizeof(struct CharClassRange));
            ranges_copy.len--;
        } else {
            i++;
        }
    }

    struct CharClassRanges ranges_merged = ranges_copy;

    if (is_negated){
        for (size_t i = 0; i < ranges_merged.len; i++){
            // TODO : reverse the range list
        }
        fprintf(stderr, "TODO : negated char class is not implemented already\n");
        exit(1);
    } else {
        if (ranges_merged.len == 1 && ranges_merged.ranges[0].start == ranges_merged.ranges[0].end){
            nfa_add_transition(re, start, ranges_merged.ranges[0].start, end);
        } else {
            for (size_t i = 0; i < ranges_merged.len; i++){
                struct CharClassRange range = ranges_merged.ranges[i];
                nfa_add_transition_range(re, start, range.start, range.end, end);
            }
        }
    }
    
    FREE(ranges_copy.ranges);

    return (struct NFAFragment){
        .start = start,
        .end = end,
    };
}

static struct NFAFragment regex_create_from_ast(struct Regex* restrict re, const struct RegexASTNode* restrict ast){
    NodeRef start;
    NodeRef end;
    switch (ast->tag){
        case AST_CHAR: {
            start = nfa_add_node(re);
            end = nfa_add_node(re);
            uint32_t c = ast->data.c;
            nfa_add_transition(re, start, c, end);
            break;
        }
        case AST_CHAR_CLASS: {
            struct CharClass char_class = ast->data.char_class;
            struct NFAFragment fragment = regex_create_char_class(re, char_class);
            start = fragment.start;
            end = fragment.end;
            break;
        }
        case AST_CONCAT: {
            struct BinaryNode concat = ast->data.binary;
            struct NFAFragment fragment_lhs = regex_create_from_ast(re, concat.lhs);
            struct NFAFragment fragment_rhs = regex_create_from_ast(re, concat.rhs);
            nfa_add_transition_epsilon(re, fragment_lhs.end, fragment_rhs.start);
            start = fragment_lhs.start;
            end = fragment_rhs.end;
            break;
        }
        case AST_ALTER: {
            start = nfa_add_node(re);
            struct BinaryNode alter = ast->data.binary;
            struct NFAFragment fragment_lhs = regex_create_from_ast(re, alter.lhs);
            struct NFAFragment fragment_rhs = regex_create_from_ast(re, alter.rhs);
            nfa_add_transition_epsilon(re, start, fragment_lhs.start);
            nfa_add_transition_epsilon(re, start, fragment_rhs.start);
            
            end = nfa_add_node(re);

            nfa_add_transition_epsilon(re, fragment_lhs.end, end);

            nfa_add_transition_epsilon(re, fragment_rhs.end, end);

            break;
        }
        case AST_KLEENE: {
            start = nfa_add_node(re);
            struct RegexASTNode* kleene_node = ast->data.unary_data;
            struct NFAFragment fragment_node = regex_create_from_ast(re, kleene_node);
            nfa_add_transition_epsilon(re, fragment_node.end, fragment_node.start);
            end = nfa_add_node(re);
            nfa_add_transition_epsilon(re, start, fragment_node.start);
            nfa_add_transition_epsilon(re, fragment_node.end, end);
            nfa_add_transition_epsilon(re, start, end);
            break;
        }
        case AST_PLUS: {
            struct RegexASTNode* plus_node = ast->data.unary_data;
            struct NFAFragment fragment_node = regex_create_from_ast(re, plus_node);
            start = fragment_node.start;

            nfa_add_transition_epsilon(re, fragment_node.end, fragment_node.start);
            end = nfa_add_node(re);
            nfa_add_transition_epsilon(re, fragment_node.end, end);
            
            break;
        }
        case AST_OPTIONAL: {
            start = nfa_add_node(re);
            struct RegexASTNode* optional_node = ast->data.unary_data;
            struct NFAFragment fragment_node = regex_create_from_ast(re, optional_node);
            nfa_add_transition_epsilon(re, start, fragment_node.start);

            end = nfa_add_node(re);
            nfa_add_transition_epsilon(re, fragment_node.end, end);
            nfa_add_transition_epsilon(re, start, end);
            break;
        }
        default:
            fprintf(stderr, "Unknown ast tag %d\n", ast->tag);
            exit(1);
    }
    return (struct NFAFragment){
        .start = start,
        .end = end,
    };
}

// TODO : what should it return, for now return a ptr, in the future, make the layout of Regex stable and return it directly ?
struct Regex* __regex_create(const char* str){
    struct Regex* re = MALLOC(sizeof(struct Regex));
    size_t str_len = strlen(str);
    uint32_t start_capacity = str_len + str_len/2; // str_len * 1.5
    if (start_capacity == 0){
        start_capacity = 1;
    }
    // TODO : preallocate the nfa_nodes buffer using the fact that is thompson method (number of special ops/chars * 2)
    *re = (struct Regex){
        .starting_state = 0,
        .ending_state = 0,
        .nfa_nodes = MALLOC(start_capacity * sizeof(struct Node)),
        .node_count = 0,
        .node_capacity = start_capacity,
    };

    
    struct RegexASTNode* ast = create_regex_ast(str, str_len);
    struct NFAFragment nfa_fragment = regex_create_from_ast(re, ast);
    free_ast(ast);
    re->starting_state = nfa_fragment.start;
    re->ending_state = nfa_fragment.end;
    return re;
}

struct NodePointer {
    NodeRef node_ref;
    size_t str_pos;
};

struct NodeQueue {
    struct NodePointer* pointers;
    size_t capacity;
    size_t front;
    size_t len;
};

static struct NodeQueue init_queue(size_t start_capacity){
    assert(start_capacity > 0);
    struct NodePointer* pointers = MALLOC_NO_PTR(start_capacity * sizeof(struct NodePointer));
    if (!pointers){
        ALLOC_ERROR("regex init node pointers");
    }
    return (struct NodeQueue){
        .pointers = pointers,
        .capacity = start_capacity,
        .front = 0,
        .len = 0,
    };
}

static size_t queue_real_pos(const struct NodeQueue* node_pointers, size_t pos){
    return (node_pointers->front + pos) % node_pointers->capacity;
}

static void ensure_size_node_pointers(struct NodeQueue* node_pointers, size_t min_capacity){
    if (node_pointers->capacity < min_capacity){
        size_t new_capacity = node_pointers->capacity;
        while (new_capacity < min_capacity){
            new_capacity *= 2;
        }
        struct NodePointer* new_buf = MALLOC_NO_PTR(new_capacity * sizeof(struct NodePointer));
        if (!new_buf){
            ALLOC_ERROR("regex node pointers");
        }
        for (size_t i = 0; i < node_pointers->len; i++){
            new_buf[i] = node_pointers->pointers[queue_real_pos(node_pointers, i)];
        }
        FREE(node_pointers->pointers);
        node_pointers->pointers = new_buf;
        node_pointers->capacity = new_capacity;
        node_pointers->front = 0;
    }
}

static void push_node_pointer(struct NodeQueue* node_pointers, NodeRef node, size_t str_pos){
    ensure_size_node_pointers(node_pointers, node_pointers->len+1);
    
    node_pointers->pointers[queue_real_pos(node_pointers, node_pointers->len)] = (struct NodePointer){
        .node_ref = node,
        .str_pos = str_pos,
    };
    node_pointers->len++;
}

static struct NodePointer pop_node_pointer(struct NodeQueue* node_pointers){
    assert(node_pointers->len > 0);
    struct NodePointer node = node_pointers->pointers[node_pointers->front];
    node_pointers->front = (node_pointers->front + 1) % node_pointers->capacity;
    node_pointers->len--;
    return node;
}

static void free_queue(struct NodeQueue node_pointers){
    FREE(node_pointers.pointers);
}

//#define VISITED(node_ref, pos) visited[(node_ref) * pos_count + (pos)]

#define BITSET_BITSIZE 64

// use 64 bits to improve perfomance
static uint64_t* create_visited(size_t visited_count){
    size_t size = (visited_count + (BITSET_BITSIZE-1))/BITSET_BITSIZE;
    uint64_t* visited = MALLOC_NO_PTR(size * sizeof(uint64_t));
    if (!visited){
        ALLOC_ERROR("regex match visited NFA");
    }
    
    memset(visited, 0, size * sizeof(uint64_t));
    
    return visited;
}

static void mark_visited(uint64_t* visited, struct NodePointer node_pointer, size_t pos_count){
    size_t idx = node_pointer.node_ref * pos_count + node_pointer.str_pos;
    uint64_t* word = visited + idx/BITSET_BITSIZE;
    uint8_t bit = idx % BITSET_BITSIZE;

    *word = *word | ((uint64_t)1 << bit);
}

static bool is_visited(const uint64_t* visited, struct NodePointer node_pointer, size_t pos_count){
    size_t idx = node_pointer.node_ref * pos_count + node_pointer.str_pos;
    uint8_t word = visited[idx/BITSET_BITSIZE];
    uint8_t bit = idx % BITSET_BITSIZE;
    return (word >> bit) & 0x1;
}

uint8_t __regex_has_match(const struct Regex* restrict re, const char* restrict str){
    struct NodeQueue node_queue = init_queue(4);
    push_node_pointer(&node_queue, re->starting_state, 0);
    size_t str_len = strlen(str);
    size_t codepoints_len;
    uint32_t* codepoints = utf8_decode_str(str, str_len, &codepoints_len);
    
    size_t pos_count = codepoints_len+1;
    // find if already visited with same pos in str

    size_t visited_count = re->node_count * pos_count;
    uint64_t* visited = create_visited(visited_count);
    
    while (node_queue.len != 0){
        struct NodePointer node_pointer = pop_node_pointer(&node_queue);

        if (is_visited(visited, node_pointer, pos_count)){
            continue;
        }
        mark_visited(visited, node_pointer, pos_count);

        if (node_pointer.str_pos == codepoints_len && re->ending_state == node_pointer.node_ref){
            FREE(visited);
            free_queue(node_queue);
            return 1;
        }
        
        struct Node node = re->nfa_nodes[node_pointer.node_ref];
        for (uint32_t i = 0; i < node.transitions_nb; i++){
            struct Transition transition = node.outgoing_transitions[i];
            if (transition.is_epsilon){
                push_node_pointer(&node_queue, transition.end, node_pointer.str_pos);
            } else if (node_pointer.str_pos < codepoints_len && transition.char_start <= codepoints[node_pointer.str_pos] && codepoints[node_pointer.str_pos] <= transition.char_end){
                push_node_pointer(&node_queue, transition.end, node_pointer.str_pos+1);
            }
        }
    }

    FREE(visited);
    free_queue(node_queue);
    FREE(codepoints);

    return 0;
}

void __init(){
    gc_init();
}

/*int main(){
    const char* regex_str = "(a*)|(b(a+))|[0-9]|[cde]";
    struct Regex* re = __regex_create(regex_str);
    printf("starting state : %d\n", re->starting_state);
    for (uint32_t i = 0; i < re->node_count; i++){
        printf("state %d : \n", i);

        for (uint32_t j = 0; j < re->nfa_nodes[i].transitions_nb; j++){
            struct Transition transition = re->nfa_nodes[i].outgoing_transitions[j];
            if (transition.is_epsilon){
                printf("\ttransitions : %d -> %d\n", i, transition.end);
            } else {
                if (transition.char_start == transition.char_end){
                    printf("\ttransitions : %d -> %d (%s)\n", i, transition.end, __char_to_str(transition.char_start));
                } else {
                    printf("\ttransitions : %d -> %d (%s-%s)\n", i, transition.end, __char_to_str(transition.char_start), __char_to_str(transition.char_end));
                }
            }
        }
    }
    printf("ending state : %d\n", re->ending_state);

    printf("match re re(%s) with \"aa\": %d\n", regex_str, __regex_has_match(re, "aa"));
    printf("match re re(%s) with \"b\": %d\n", regex_str, __regex_has_match(re, "b"));
    printf("match re re(%s) with \"ba\": %d\n", regex_str, __regex_has_match(re, "ba"));
    printf("match re re(%s) with \"bc\": %d\n", regex_str, __regex_has_match(re, "bc"));
    printf("match re re(%s) with \"ab\": %d\n", regex_str, __regex_has_match(re, "ab"));
    printf("match re re(%s) with \"0\": %d\n", regex_str, __regex_has_match(re, "0"));
}*/

// TODO : instead of using the fprintf(stderr, ..) and exit(1), use a macro for errors that would be this in low optimizations levels/with a flag transformed to a trap instruction like __builtin_trap()