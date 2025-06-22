#include <stdint.h>
#include <stdlib.h>

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