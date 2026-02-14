#ifndef STATIC
#ifndef STD_BUILD
#define STATIC
#include "prelude.h"
#endif
#endif

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