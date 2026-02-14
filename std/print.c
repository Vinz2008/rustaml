#ifndef STATIC
#ifndef STD_BUILD
#define STATIC
#include "prelude.h"
#endif
#endif


static void list_write_file_no_new_line(struct ListNode* list, FILE* f);


#define MAX_INT_BUF_SIZE 22

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
        fprintf(stderr,"Unknown tag of vec in print : %d\n", vec_val->element_type_tag);
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

// TODO : move the printing of a vec to a differrent function that would be __print_vec to prevent stack spilling (look at simd.rml)