#include <stdint.h>
#include <stdlib.h>
#include <stdio.h>
#include <stdbool.h>
#include <string.h>

enum TypeTag {
    INT_TYPE = 0,
    FLOAT_TYPE = 1,
    BOOL_TYPE = 2,
    FUNCTION_TYPE = 3,
    STR_TYPE = 4,
    LIST_TYPE = 5,
};

struct ListNode {
    uint8_t type_tag;
    uint64_t val; // can be a i64, a ptr or a f64 depending on type_tag
    struct ListNode* next; // if empty null 
};


// TODO : generate this code directly in llvm instead of generating a call ?
struct ListNode* __list_node_init(uint8_t type_tag, uint64_t val){
    struct ListNode* l = malloc(sizeof(struct ListNode));
    l->type_tag = type_tag;
    l->val = val;
    l->next = NULL;
    return l;
}

void __list_node_append(struct ListNode* list, uint8_t type_tag, uint64_t val){
    // TODO : add asserts
    struct ListNode* current = list;
    while (current != NULL && current->next != NULL){
        current = current->next;
    }

    current->next = __list_node_init(type_tag, val);
}

// TODO : generate this code directly in llvm instead of generating a call ?
uint64_t __list_node_val(struct ListNode* list){
    return list->val;
}

// TODO : generate this code directly in llvm instead of generating a call ?
struct ListNode* __list_node_tail(struct ListNode* list){
    return list->next;
}

uint8_t __list_node_cmp(uint8_t tag1, uint64_t val1, uint8_t tag2, uint64_t val2){
    if (tag1 != tag2){
        return false;
    }
    
    if (tag1 == INT_TYPE && tag2 == INT_TYPE) {
        return val1 == val2;
    } else if (tag1 == FLOAT_TYPE && tag2 == FLOAT_TYPE){
        return val1 == val2;
    } else if (tag1 == BOOL_TYPE && tag2 == BOOL_TYPE){
        uint8_t* bool_addr1 = (uint8_t*)&val1;
        uint8_t* bool_addr2 = (uint8_t*)&val2;
        return *bool_addr1 == *bool_addr2;
    } else if (tag1 == STR_TYPE && tag2 == STR_TYPE){
        char* str1 = (char*)val1;
        char* str2 = (char*)val2;
        size_t len1 = strlen(str1);
        size_t len2 = strlen(str2);
        if (len1 != len2){
            return false;
        }
        return strcmp(str1, str2); 
    } else if (tag1 == LIST_TYPE && tag2 == LIST_TYPE){
        struct ListNode* list1 = (struct ListNode*)val1;
        struct ListNode* list2 = (struct ListNode*)val2;
        return __list_node_cmp(list1->type_tag, list1->val, list2->type_tag, list2->val);
    }

    fprintf(stderr, "ERROR : WRONG TAGS IN LIST (BUG IN COMPILER  \?\?)\n");
    exit(1);
}


// TODO ? generate this with LLVM ?
uint8_t __list_cmp(struct ListNode* list1, struct ListNode* list2){
    while (list1 != NULL && list2 != NULL){
        if (!__list_node_cmp(list1->type_tag, list1->val, list2->type_tag, list2->val)){
            return false;
        }
        list1 = list1->next;
        list2 = list2->next;
    }

    return list1 == list2; // both are NULL
}