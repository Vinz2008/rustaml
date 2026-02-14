#ifndef STATIC
#ifndef STD_BUILD
#define STATIC
#include "prelude.h"
#endif
#endif

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