#include <stdint.h>
#include <stdlib.h>
#include <stdio.h>
#include <stdbool.h>
#include <string.h>
#include <assert.h>

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

void __gc_init(){
    GC_INIT();
}

#else
#define MALLOC(size) malloc(size)
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

const char* __bool_to_str(uint8_t b){
    if (b) {
        return "true";
    } else {
        return "false";
    }
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


// TODO : maybe add function to print values with the type tag