#ifndef STATIC
#ifndef STD_BUILD
#define STATIC
#include "prelude.h"
#endif
#endif

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

STATIC struct ListBuilder list_builder_init(struct ListNode* nodes_buf){
    return (struct ListBuilder){
        .head = NULL,
        .tail = NULL,
        .nodes_buf = nodes_buf,
        .nodes_buf_idx = 0,
    };
}

STATIC void list_builder_append_back(struct ListBuilder* list_builder, uint8_t type_tag, Val val){
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