#ifndef STATIC
#ifndef STD_BUILD
#define STATIC
#include "prelude.h"
#endif
#endif


#ifndef FREESTANDING
// NOT FREESTANDING

#if defined(__unix__) || defined(__unix) || \
        (defined(__APPLE__) && defined(__MACH__))
#define SYSTEM_UNIX
#endif


#ifdef _GC_

void gc_init(){
    GC_INIT();
}

#else
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
#ifndef NDEBUG
__attribute__((noinline, cold, noreturn))
static void assert_fail(){
    __builtin_trap();
    while (1){}
}
#endif
#endif

#endif

__attribute__((cold, noreturn))
STATIC void alloc_error(const char* func_name, const char* str){
    fprintf(stderr, "ALLOC ERROR in %s: %s\n", func_name, str);
    fprintf(stderr, "\n");
    exit(1);
}

void __init(){
    gc_init();
}

// TODO : instead of using the fprintf(stderr, ..) and exit(1), use a macro for errors that would be this in low optimizations levels/with a flag transformed to a trap instruction like __builtin_trap()