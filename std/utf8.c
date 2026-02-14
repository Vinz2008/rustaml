#ifndef STATIC
#ifndef STD_BUILD
#define STATIC
#include "prelude.h"
#endif
#endif

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

// TODO : return a struct instead ?
__attribute__((malloc, alloc_size(2), returns_nonnull))
STATIC uint32_t* utf8_decode_str(const char* restrict str, size_t str_len, size_t* restrict codepoints_len){
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