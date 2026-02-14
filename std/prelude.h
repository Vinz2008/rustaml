#ifndef STATIC
#define STATIC static
#endif

#include <stdint.h>
#include <stdbool.h>
#include <stddef.h>
#include <stdarg.h>

#define PURE __attribute__((pure))
#define CONST __attribute__((const))
#define WEAK __attribute__((weak))

#if __STDC_HOSTED__ == 0
#define FREESTANDING
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

void gc_init();

#else
#define MALLOC(size) malloc(size)
#define MALLOC_NO_PTR(size) malloc(size)
#define FREE(ptr) free(ptr)
#define REALLOC(ptr, new_size) realloc(ptr, new_size)
void gc_init();
#endif

#else
// FREESTANDING MODE

void* memcpy(void* restrict dest, const void* restrict src, size_t size);

#define MALLOC(size) malloc(size)
#define MALLOC_NO_PTR(size) malloc(size)
#define FREE(ptr) free(ptr)
#define REALLOC(ptr, new_size) realloc(ptr, new_size)
void gc_init();

struct Metadata {
    size_t size;
};

void* malloc(size_t size);

void* realloc(void* ptr, size_t size);

void free(void* ptr);


PURE size_t strlen(const char* s);

typedef struct {} FILE;
extern FILE* stderr;
extern FILE* stdout


int fprintf(FILE* stream, const char* format, ... );

void exit(int exit_code)  __attribute__ ((__noreturn__));


__attribute__((noreturn)) void __stack_chk_fail(void);

#ifndef assert
#ifdef NDEBUG
#define assert(e) ((void)0)
#else
__attribute__((noinline, cold, noreturn))
void assert_fail();
#define assert(e) ((e) ? ((void)0) : assert_fail ())
#endif
#endif

#endif

#define MAX_DOUBLE_BUF_SIZE 24

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

#define ALLOC_ERROR(s) alloc_error(__func__, s)

#define TODO(str) do { \
        fprintf(stderr, "TODO : " str "\n"); \
        exit(1); \
    } while(0);

#define ASSERT_BOOL(id_bool) assert(id_bool < 2 && #id_bool " should be true or false")
#define ASSERT_NOT_NULL(not_null) assert(not_null && #not_null " should not be NULL")

__attribute__((cold, noreturn))
STATIC void alloc_error(const char* func_name, const char* str);

__attribute__((malloc, alloc_size(2), returns_nonnull))
STATIC uint32_t* utf8_decode_str(const char* restrict str, size_t str_len, size_t* restrict codepoints_len);

__attribute__((returns_nonnull))
const char* __char_to_str(uint32_t c);

struct str {
    char* buf;
    size_t capacity;
    size_t len;
};

uint8_t __str_cmp(const char* s1, const char* s2);

// TODO : optimize by putting the type_tag in a List struct which will have the tag and just the head of the list (so the tag is only stored one time -> 24 to 16 bytes for each node)
// TODO : put a list len in it ?
struct ListNode {
    struct ListNode* next; // if empty null
    Val val; // can be a i64, a ptr or a f64 depending on type_tag 
    uint8_t type_tag;
};

struct ListBuilder {
    struct ListNode* head;
    struct ListNode* tail;
    struct ListNode* nodes_buf; // if preallocated the number of nodes, not NULL, else NULL
    size_t nodes_buf_idx; // only valid if nodes_buf is not null
};

STATIC struct ListBuilder list_builder_init(struct ListNode* nodes_buf);

STATIC void list_builder_append_back(struct ListBuilder* list_builder, uint8_t type_tag, Val val);


STATIC uint32_t utf8_decode_char(const char* s, size_t* pos, size_t bytes_len);

STATIC void format_int(struct str* str, int64_t i);

STATIC void format_float(struct str* str, double d);

STATIC void format_char(struct str* str, uint32_t c);

CONST const char* __bool_to_str(bool b);