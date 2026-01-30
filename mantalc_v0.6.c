/*
 * Mantel Compiler v0.6.0 - Function Parameters & Arrays Edition
 * NEW WORKING: Function parameters, Array indexing
 * WORKING: For loops, Variables, While loops, If/else, All operators
 * BASE: v0.4.1 with working variables
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdarg.h>
#include <math.h>
#include <limits.h>

/* =========== Configuration =========== */
#define MAX_TOKENS 8192
#define MAX_ID_LEN 64
#define MAX_STR_LEN 256
#define MAX_SYMS 512
#define MAX_SCOPES 16
#define MAX_ERRORS 32

/* =========== Type System =========== */
typedef enum {
    TYPE_VOID,
    TYPE_BOOL,
    TYPE_INT8,
    TYPE_INT16,
    TYPE_INT32,
    TYPE_INT64,
    TYPE_UINT8,
    TYPE_UINT16,
    TYPE_UINT32,
    TYPE_UINT64,
    TYPE_FLOAT32,
    TYPE_FLOAT64,
    TYPE_POINTER,
    TYPE_ARRAY,
    TYPE_STRUCT,
    TYPE_STRING,
    TYPE_CHAR
} TypeKind;

typedef struct Type Type;

struct Type {
    TypeKind kind;
    int size;
    int align;
    bool is_const;
    bool is_unsigned;
    union {
        Type *base;
        struct {
            int length;
        } array;
        struct {
            char **fields;
            Type **field_types;
            int field_count;
        } struc;
    };
};

/* =========== Tokenization =========== */
typedef enum {
    /* Keywords */
    TOK_FUNC, TOK_VAR, TOK_CONST, TOK_RETURN, TOK_IF, TOK_ELSE,
    TOK_WHILE, TOK_FOR, TOK_BREAK, TOK_CONTINUE, TOK_STRUCT,
    TOK_TRUE, TOK_FALSE, TOK_NULL, TOK_SIZEOF, TOK_TYPEOF,
    
    /* New keywords */
    TOK_PRINT, TOK_PRINTLN, TOK_READ, TOK_INPUT, TOK_TYPE, 
    TOK_LEN, TOK_IMPORT, TOK_EXPORT, TOK_SWITCH, TOK_CASE, TOK_DEFAULT,
    
    /* Types */
    TOK_VOID, TOK_BOOL, TOK_INT8, TOK_INT16, TOK_INT32, TOK_INT64,
    TOK_UINT8, TOK_UINT16, TOK_UINT32, TOK_UINT64,
    TOK_FLOAT32, TOK_FLOAT64, TOK_STRING, TOK_CHAR,
    
    /* Operators */
    TOK_LPAREN, TOK_RPAREN, TOK_LBRACE, TOK_RBRACE,
    TOK_LBRACKET, TOK_RBRACKET, TOK_COMMA, TOK_COLON, TOK_SEMI,
    TOK_DOT, TOK_ARROW, TOK_AMP, TOK_ASTERISK, TOK_QUESTION,
    
    /* Assignments */
    TOK_ASSIGN, TOK_PLUSEQ, TOK_MINUSEQ, TOK_STAREQ, TOK_SLASHEQ,
    TOK_PERCENTEQ, TOK_ANDEQ, TOK_OREQ, TOK_XOREQ,
    TOK_LSHIFTEQ, TOK_RSHIFTEQ,
    
    /* Comparisons */
    TOK_EQ, TOK_NE, TOK_LT, TOK_GT, TOK_LE, TOK_GE,
    TOK_AND, TOK_OR, TOK_NOT,
    
    /* Arithmetic */
    TOK_PLUS, TOK_MINUS, TOK_SLASH, TOK_PERCENT,
    TOK_INC, TOK_DEC,
    
    /* Bitwise */
    TOK_BAND, TOK_BOR, TOK_BXOR, TOK_BNOT,
    TOK_LSHIFT, TOK_RSHIFT,
    
    /* Literals */
    TOK_INT, TOK_FLOAT, TOK_STRING_LIT, TOK_CHAR_LIT, TOK_ID,
    
    /* Special */
    TOK_EOF, TOK_ERROR
} TokenType;

typedef struct {
    TokenType type;
    char value[MAX_ID_LEN];
    int line;
    int col;
    union {
        int64_t int_val;
        double float_val;
        char char_val;
        char *str_val;
    };
} Token;

/* =========== AST Nodes =========== */
typedef enum {
    NODE_PROGRAM, NODE_FUNC, NODE_VARDECL, NODE_CONSTDECL,
    NODE_STRUCTDEF, NODE_RETURN, NODE_IF, NODE_WHILE, NODE_FOR,
    NODE_BREAK, NODE_CONTINUE, NODE_BLOCK,
    NODE_BINOP, NODE_UNOP, NODE_CALL, NODE_CAST,
    NODE_NUM, NODE_FLOAT, NODE_STRING, NODE_CHAR, NODE_BOOL,
    NODE_VAR, NODE_MEMBER, NODE_INDEX, NODE_DEREF, NODE_ADDR,
    NODE_ASSIGN, NODE_COMPOUND_ASSIGN,
    NODE_SIZEOF, NODE_TYPEOF, NODE_NULL,
    
    /* New nodes */
    NODE_PRINT, NODE_PRINTLN, NODE_READ, NODE_IMPORT,
    NODE_ARRAY_LIT, NODE_RANGE, NODE_TYPECAST, NODE_TERNARY,
    NODE_SWITCH, NODE_CASE, NODE_DEFAULT
} NodeType;

typedef struct Node Node;

struct Node {
    NodeType type;
    Type *data_type;
    int line;
    int col;
    
    union {
        int64_t int_val;
        double float_val;
        char *str_val;
        char char_val;
        bool bool_val;
        
        char name[MAX_ID_LEN];
        
        struct {
            Node *operand;
            char op;
        } unop;
        
        struct {
            Node *left;
            Node *right;
            char op[3];
        } binop;
        
        struct {
            Node *target;
            Node *value;
        } assign;
        
        struct {
            Node *func;
            Node **args;
            int arg_count;
        } call;
        
        struct {
            Node *cond;
            Node *then_branch;
            Node *else_branch;
        } if_stmt;
        
        struct {
            Node *cond;
            Node *body;
        } while_loop;
        
        struct {
            Node *init;
            Node *cond;
            Node *inc;
            Node *body;
        } for_loop;
        
        struct {
            Node **stmts;
            int stmt_count;
        } block;
        
        struct {
            char *name;
            Node **params;
            Type **param_types;
            int param_count;
            Node *body;
            Type *return_type;
        } func;
        
        struct {
            char *name;
            Type *var_type;
            Node *init;
        } vardecl;
        
        struct {
            Node *target;
            Node *index;
        } index;
    };
    
    Node *next;
};

/* =========== Symbol Table =========== */
typedef enum {
    SYM_VAR, SYM_FUNC, SYM_TYPE, SYM_CONST, SYM_STRUCT
} SymKind;

typedef struct Symbol {
    char name[MAX_ID_LEN];
    SymKind kind;
    Type *type;
    int scope_level;
    union {
        int offset;
        void *address;
        Node *node;
    };
    bool is_extern;
    bool is_defined;
} Symbol;

typedef struct Scope {
    Symbol symbols[MAX_SYMS];
    int symbol_count;
    int depth;
    struct Scope *parent;
} Scope;

/* =========== Parser State =========== */
typedef struct {
    Token *tokens;
    int pos;
    int token_count;
    Token current;
    Scope *current_scope;
    Scope *global_scope;
    bool optimize;
    int ptr_size;
    char *source;
} Parser;

/* =========== Code Generation Context =========== */
typedef struct {
    FILE *out;
    Scope *current_scope;
    int stack_offset;
    int label_counter;
    int string_counter;
    char **string_literals;
    int string_count;
    int string_capacity;
    bool is_windows;
    bool optimize;
    char *current_function;
    int shadow_space_size;
    int param_offset;
    int *var_offsets;
    int var_count;
    
    /* Variable tracking for proper stack allocation */
    char var_names[64][MAX_ID_LEN];  /* Variable names */
    int var_stack_offsets[64];        /* Stack offset for each variable */
    int num_local_vars;               /* Number of local variables */
    int current_var_offset;           /* Current offset being assigned */
} CodeGen;

/* =========== Global String Table =========== */
typedef struct {
    char *str;
    int id;
} StringLiteral;

StringLiteral *global_strings = NULL;
int global_string_count = 0;
int global_string_capacity = 0;

/* =========== Error Reporting =========== */
void report_error(int line, int col, const char *fmt, ...) {
    fprintf(stderr, "\033[1;31mError at %d:%d:\033[0m ", line, col);
    va_list args;
    va_start(args, fmt);
    vfprintf(stderr, fmt, args);
    va_end(args);
    fprintf(stderr, "\n");
}

void report_error_with_context(Parser *p, int line, int col, const char *fmt, ...) {
    if (!p->source) {
        report_error(line, col, fmt);
        return;
    }
    
    // Find the line in source
    int current_line = 1;
    const char *line_start = p->source;
    const char *ptr = p->source;
    
    while (*ptr && current_line < line) {
        if (*ptr == '\n') {
            current_line++;
            line_start = ptr + 1;
        }
        ptr++;
    }
    
    // Find line end
    const char *line_end = line_start;
    while (*line_end && *line_end != '\n') {
        line_end++;
    }
    
    // Print error message
    fprintf(stderr, "\033[1;31mError at %d:%d:\033[0m ", line, col);
    va_list args;
    va_start(args, fmt);
    vfprintf(stderr, fmt, args);
    va_end(args);
    fprintf(stderr, "\n\n");
    
    // Print the line
    fprintf(stderr, "\033[90m%5d | \033[0m", line);
    for (const char *c = line_start; c < line_end; c++) {
        fputc(*c, stderr);
    }
    fprintf(stderr, "\n\033[90m      | \033[0m");
    
    // Print error indicator
    for (int i = 0; i < col - 1; i++) {
        fputc(' ', stderr);
    }
    fprintf(stderr, "\033[1;31m^\033[0m\n\n");
}

/* =========== Type System =========== */
Type *create_type(TypeKind kind, int size, int align) {
    Type *type = malloc(sizeof(Type));
    memset(type, 0, sizeof(Type));
    type->kind = kind;
    type->size = size;
    type->align = align;
    return type;
}

Type *create_pointer_type(Type *base) {
    Type *type = create_type(TYPE_POINTER, 8, 8);
    type->base = base;
    return type;
}

Type *create_array_type(Type *base, int length) {
    if (length <= 0) {
        fprintf(stderr, "Error: Array length must be positive\n");
        return NULL;
    }
    
    long long total_size = (long long)base->size * (long long)length;
    if (total_size > INT_MAX) {
        fprintf(stderr, "Error: Array size too large\n");
        return NULL;
    }
    
    Type *type = create_type(TYPE_ARRAY, base->size * length, base->align);
    type->base = base;
    type->array.length = length;
    return type;
}

Type *create_string_type() {
    return create_pointer_type(create_type(TYPE_INT8, 1, 1));
}

Type *get_type_from_token(TokenType tok) {
    switch (tok) {
        case TOK_VOID: return create_type(TYPE_VOID, 0, 1);
        case TOK_BOOL: return create_type(TYPE_BOOL, 1, 1);
        case TOK_INT8: return create_type(TYPE_INT8, 1, 1);
        case TOK_INT16: return create_type(TYPE_INT16, 2, 2);
        case TOK_INT32: return create_type(TYPE_INT32, 4, 4);
        case TOK_INT64: return create_type(TYPE_INT64, 8, 8);
        case TOK_UINT8: {
            Type *t = create_type(TYPE_UINT8, 1, 1);
            t->is_unsigned = true;
            return t;
        }
        case TOK_UINT16: {
            Type *t = create_type(TYPE_UINT16, 2, 2);
            t->is_unsigned = true;
            return t;
        }
        case TOK_UINT32: {
            Type *t = create_type(TYPE_UINT32, 4, 4);
            t->is_unsigned = true;
            return t;
        }
        case TOK_UINT64: {
            Type *t = create_type(TYPE_UINT64, 8, 8);
            t->is_unsigned = true;
            return t;
        }
        case TOK_FLOAT32: return create_type(TYPE_FLOAT32, 4, 4);
        case TOK_FLOAT64: return create_type(TYPE_FLOAT64, 8, 8);
        case TOK_STRING: return create_string_type();
        case TOK_CHAR: return create_type(TYPE_CHAR, 1, 1);
        default: return NULL;
    }
}

/* =========== Lexer =========== */
bool is_keyword(const char *str) {
    static const char *keywords[] = {
        "func", "var", "const", "return", "if", "else", "while", "for",
        "break", "continue", "struct", "true", "false", "null",
        "sizeof", "typeof", "void", "bool", "int8", "int16", "int32", "int64",
        "uint8", "uint16", "uint32", "uint64", "float32", "float64",
        "string", "char", "print", "println", "read", "input",
        "len", "type", "import", "export", "switch", "case", "default",
        NULL
    };
    for (int i = 0; keywords[i]; i++) {
        if (strcmp(str, keywords[i]) == 0) return true;
    }
    return false;
}

TokenType keyword_to_token(const char *str) {
    struct { const char *word; TokenType type; } keywords[] = {
        {"func", TOK_FUNC}, {"var", TOK_VAR}, {"const", TOK_CONST},
        {"return", TOK_RETURN}, {"if", TOK_IF}, {"else", TOK_ELSE},
        {"while", TOK_WHILE}, {"for", TOK_FOR}, {"break", TOK_BREAK},
        {"continue", TOK_CONTINUE}, {"struct", TOK_STRUCT},
        {"true", TOK_TRUE}, {"false", TOK_FALSE}, {"null", TOK_NULL},
        {"sizeof", TOK_SIZEOF}, {"typeof", TOK_TYPEOF},
        {"void", TOK_VOID}, {"bool", TOK_BOOL},
        {"int8", TOK_INT8}, {"int16", TOK_INT16}, {"int32", TOK_INT32},
        {"int64", TOK_INT64}, {"uint8", TOK_UINT8}, {"uint16", TOK_UINT16},
        {"uint32", TOK_UINT32}, {"uint64", TOK_UINT64},
        {"float32", TOK_FLOAT32}, {"float64", TOK_FLOAT64},
        {"string", TOK_STRING}, {"char", TOK_CHAR},
        {"print", TOK_PRINT}, {"println", TOK_PRINTLN},
        {"read", TOK_READ}, {"input", TOK_INPUT},
        {"len", TOK_LEN}, {"type", TOK_TYPE},
        {"import", TOK_IMPORT}, {"export", TOK_EXPORT},
        {"switch", TOK_SWITCH}, {"case", TOK_CASE}, {"default", TOK_DEFAULT},
        {NULL, TOK_ERROR}
    };
    
    for (int i = 0; keywords[i].word; i++) {
        if (strcmp(str, keywords[i].word) == 0) {
            return keywords[i].type;
        }
    }
    return TOK_ID;
}

Token *tokenize(const char *source) {
    Token *tokens = malloc(MAX_TOKENS * sizeof(Token));
    if (!tokens) {
        fprintf(stderr, "Error: Memory allocation failed for tokens\n");
        return NULL;
    }
    
    int pos = 0, line = 1, col = 1, token_count = 0;
    
    while (source[pos] != '\0' && token_count < MAX_TOKENS - 1) {
        char c = source[pos++];
        col++;
        
        if (c == '\n') { line++; col = 1; continue; }
        if (isspace((unsigned char)c)) continue;
        
        Token *tok = &tokens[token_count];
        memset(tok, 0, sizeof(Token));
        tok->line = line;
        tok->col = col - 1;
        
        /* Comments */
        if (c == '/' && source[pos] == '/') {
            while (source[pos] != '\0' && source[pos] != '\n') pos++;
            continue;
        }
        
        if (c == '/' && source[pos] == '*') {
            pos++;
            while (source[pos] != '\0' && !(source[pos] == '*' && source[pos+1] == '/')) {
                if (source[pos] == '\n') { line++; col = 0; }
                pos++;
            }
            if (source[pos] == '*') pos += 2;
            continue;
        }
        
        /* Identifiers */
        if (isalpha((unsigned char)c) || c == '_') {
            int start = pos - 1;
            while (isalnum((unsigned char)source[pos]) || source[pos] == '_') pos++;
            int len = pos - start;
            if (len >= MAX_ID_LEN) len = MAX_ID_LEN - 1;
            
            char ident[MAX_ID_LEN];
            memset(ident, 0, sizeof(ident));
            strncpy(ident, &source[start], len);
            ident[len] = '\0';
            
            if (is_keyword(ident)) {
                tok->type = keyword_to_token(ident);
            } else {
                tok->type = TOK_ID;
                strncpy(tok->value, ident, MAX_ID_LEN-1);
                tok->value[MAX_ID_LEN-1] = '\0';
            }
            token_count++;
            continue;
        }
        
        /* Numbers */
        if (isdigit((unsigned char)c) || (c == '.' && isdigit((unsigned char)source[pos]))) {
            int start = pos - 1;
            bool is_float = false;
            
            while (isdigit((unsigned char)source[pos]) || source[pos] == '.' || 
                   source[pos] == 'e' || source[pos] == 'E' || source[pos] == '+' || source[pos] == '-') {
                if (source[pos] == '.') is_float = true;
                pos++;
            }
            
            int len = pos - start;
            if (len >= MAX_ID_LEN) len = MAX_ID_LEN - 1;
            char num[MAX_ID_LEN];
            memset(num, 0, sizeof(num));
            strncpy(num, &source[start], len);
            num[len] = '\0';
            
            if (is_float) {
                tok->type = TOK_FLOAT;
                tok->float_val = atof(num);
            } else {
                tok->type = TOK_INT;
                tok->int_val = atoll(num);
            }
            strncpy(tok->value, num, MAX_ID_LEN-1);
            tok->value[MAX_ID_LEN-1] = '\0';
            token_count++;
            continue;
        }
        
        /* Character literals */
        if (c == '\'') {
            int start = pos;
            
            // Handle escape sequences
            if (source[pos] == '\\') {
                pos++;
                switch (source[pos]) {
                    case 'n': tok->char_val = '\n'; break;
                    case 't': tok->char_val = '\t'; break;
                    case 'r': tok->char_val = '\r'; break;
                    case '0': tok->char_val = '\0'; break;
                    case '\\': tok->char_val = '\\'; break;
                    case '\'': tok->char_val = '\''; break;
                    case '"': tok->char_val = '"'; break;
                    default: tok->char_val = source[pos]; break;
                }
                pos++;
            } else {
                tok->char_val = source[pos++];
            }
            
            if (source[pos] != '\'') {
                report_error(line, col, "Unterminated character literal");
                tok->type = TOK_ERROR;
                token_count++;
                break;
            }
            pos++;
            
            tok->type = TOK_CHAR_LIT;
            token_count++;
            continue;
        }
        
        /* Strings */
        if (c == '"') {
            int start = pos;
            int len = 0;
            
            while (source[pos] != '"' && source[pos] != '\0') {
                if (len >= MAX_STR_LEN) {
                    report_error(line, col, "String literal too long");
                    break;
                }
                
                // Handle escape sequences
                if (source[pos] == '\\') {
                    pos++;
                    len++;
                }
                pos++;
                len++;
            }
            
            if (source[pos] == '\0') {
                report_error(line, col, "Unterminated string");
                tok->type = TOK_ERROR;
                token_count++;
                break;
            }
            
            char *str = malloc(len + 1);
            int dest_pos = 0;
            int src_pos = start;
            
            while (src_pos < pos) {
                if (source[src_pos] == '\\') {
                    src_pos++;
                    switch (source[src_pos]) {
                        case 'n': str[dest_pos++] = '\n'; break;
                        case 't': str[dest_pos++] = '\t'; break;
                        case 'r': str[dest_pos++] = '\r'; break;
                        case '0': str[dest_pos++] = '\0'; break;
                        case '\\': str[dest_pos++] = '\\'; break;
                        case '\'': str[dest_pos++] = '\''; break;
                        case '"': str[dest_pos++] = '"'; break;
                        default: str[dest_pos++] = source[src_pos]; break;
                    }
                } else {
                    str[dest_pos++] = source[src_pos];
                }
                src_pos++;
            }
            str[dest_pos] = '\0';
            pos++;
            
            tok->type = TOK_STRING_LIT;
            tok->str_val = str;
            token_count++;
            continue;
        }
        
        /* Operators */
        switch (c) {
            case '(': tok->type = TOK_LPAREN; break;
            case ')': tok->type = TOK_RPAREN; break;
            case '{': tok->type = TOK_LBRACE; break;
            case '}': tok->type = TOK_RBRACE; break;
            case '[': tok->type = TOK_LBRACKET; break;
            case ']': tok->type = TOK_RBRACKET; break;
            case ',': tok->type = TOK_COMMA; break;
            case ':': tok->type = TOK_COLON; break;
            case ';': tok->type = TOK_SEMI; break;
            case '.': 
                if (source[pos] == '.') {
                    tok->type = TOK_DOT; // Actually range operator, handle in parser
                    pos++;
                } else {
                    tok->type = TOK_DOT;
                }
                break;
            case '?': tok->type = TOK_QUESTION; break;
            case '&':
                if (source[pos] == '&') { tok->type = TOK_AND; pos++; }
                else tok->type = TOK_AMP;
                break;
            case '|':
                if (source[pos] == '|') { tok->type = TOK_OR; pos++; }
                else tok->type = TOK_BOR;
                break;
            case '^': tok->type = TOK_BXOR; break;
            case '*': tok->type = TOK_ASTERISK; break;
            case '/': tok->type = TOK_SLASH; break;
            case '%': tok->type = TOK_PERCENT; break;
            case '+':
                if (source[pos] == '+') { tok->type = TOK_INC; pos++; }
                else tok->type = TOK_PLUS;
                break;
            case '-':
                if (source[pos] == '-') { tok->type = TOK_DEC; pos++; }
                else if (source[pos] == '>') { tok->type = TOK_ARROW; pos++; }
                else tok->type = TOK_MINUS;
                break;
            case '=':
                if (source[pos] == '=') { tok->type = TOK_EQ; pos++; }
                else tok->type = TOK_ASSIGN;
                break;
            case '!':
                if (source[pos] == '=') { tok->type = TOK_NE; pos++; }
                else tok->type = TOK_NOT;
                break;
            case '<':
                if (source[pos] == '<') { tok->type = TOK_LSHIFT; pos++; }
                else if (source[pos] == '=') { tok->type = TOK_LE; pos++; }
                else tok->type = TOK_LT;
                break;
            case '>':
                if (source[pos] == '>') { tok->type = TOK_RSHIFT; pos++; }
                else if (source[pos] == '=') { tok->type = TOK_GE; pos++; }
                else tok->type = TOK_GT;
                break;
            default:
                report_error(line, col, "Unexpected character '%c'", c);
                tok->type = TOK_ERROR;
                break;
        }
        
        if (tok->type != TOK_ERROR) {
            token_count++;
        }
    }
    
    tokens[token_count].type = TOK_EOF;
    tokens[token_count].line = line;
    tokens[token_count].col = col;
    token_count++;
    
    return tokens;
}

/* =========== Parser =========== */
void parser_init(Parser *p, Token *tokens, int token_count, char *source) {
    p->tokens = tokens;
    p->pos = 0;
    p->token_count = token_count;
    p->current = tokens[0];
    p->global_scope = malloc(sizeof(Scope));
    memset(p->global_scope, 0, sizeof(Scope));
    p->current_scope = p->global_scope;
    p->optimize = true;
    p->ptr_size = 8;
    p->source = source;
}

void parser_next(Parser *p) {
    if (p->pos < p->token_count - 1) {
        p->pos++;
        p->current = p->tokens[p->pos];
    }
}

bool parser_match(Parser *p, TokenType type) {
    return p->current.type == type;
}

bool parser_consume(Parser *p, TokenType type) {
    if (parser_match(p, type)) {
        parser_next(p);
        return true;
    }
    return false;
}

void parser_error(Parser *p, const char *msg) {
    report_error_with_context(p, p->current.line, p->current.col, "%s", msg);
}

Node *create_node(NodeType type, int line, int col) {
    Node *node = malloc(sizeof(Node));
    memset(node, 0, sizeof(Node));
    node->type = type;
    node->line = line;
    node->col = col;
    return node;
}

/* =========== Expression Parsing =========== */
Node *parse_expression(Parser *p);
Node *parse_assignment(Parser *p);
Node *parse_ternary(Parser *p);
Node *parse_logical_or(Parser *p);
Node *parse_logical_and(Parser *p);
Node *parse_equality(Parser *p);
Node *parse_comparison(Parser *p);
Node *parse_term(Parser *p);
Node *parse_factor(Parser *p);
Node *parse_unary(Parser *p);
Node *parse_primary(Parser *p);
Node *parse_array_literal(Parser *p);
Node *parse_var_declaration_no_semi(Parser *p);  // For for loops

Node *parse_array_literal(Parser *p) {
    Token bracket_tok = p->current;
    parser_next(p); // Consume '['
    
    Node *node = create_node(NODE_ARRAY_LIT, bracket_tok.line, bracket_tok.col);
    node->call.args = malloc(sizeof(Node*) * 16);
    node->call.arg_count = 0;
    
    if (!parser_match(p, TOK_RBRACKET)) {
        do {
            Node *elem = parse_expression(p);
            if (elem) {
                node->call.args[node->call.arg_count++] = elem;
            }
        } while (parser_consume(p, TOK_COMMA));
    }
    
    parser_consume(p, TOK_RBRACKET);
    return node;
}

Node *parse_primary(Parser *p) {
    /* Integer literal */
    if (parser_match(p, TOK_INT)) {
        Node *node = create_node(NODE_NUM, p->current.line, p->current.col);
        node->int_val = p->current.int_val;
        node->data_type = create_type(TYPE_INT64, 8, 8);
        parser_next(p);
        return node;
    }
    
    /* Float literal */
    if (parser_match(p, TOK_FLOAT)) {
        Node *node = create_node(NODE_FLOAT, p->current.line, p->current.col);
        node->float_val = p->current.float_val;
        node->data_type = create_type(TYPE_FLOAT64, 8, 8);
        parser_next(p);
        return node;
    }
    
    /* String literal */
    if (parser_match(p, TOK_STRING_LIT)) {
        Node *node = create_node(NODE_STRING, p->current.line, p->current.col);
        node->str_val = strdup(p->current.str_val);
        node->data_type = create_string_type();
        
        // Add to global string table
        if (global_string_count >= global_string_capacity) {
            global_string_capacity = global_string_capacity ? global_string_capacity * 2 : 16;
            global_strings = realloc(global_strings, 
                                    sizeof(StringLiteral) * global_string_capacity);
        }
        global_strings[global_string_count].str = strdup(p->current.str_val);
        global_strings[global_string_count].id = global_string_count;
        node->int_val = global_string_count;
        global_string_count++;
        
        parser_next(p);
        return node;
    }
    
    /* Character literal */
    if (parser_match(p, TOK_CHAR_LIT)) {
        Node *node = create_node(NODE_CHAR, p->current.line, p->current.col);
        node->char_val = p->current.char_val;
        node->data_type = create_type(TYPE_CHAR, 1, 1);
        parser_next(p);
        return node;
    }
    
    /* Boolean literals */
    if (parser_match(p, TOK_TRUE) || parser_match(p, TOK_FALSE)) {
        Node *node = create_node(NODE_BOOL, p->current.line, p->current.col);
        node->bool_val = (p->current.type == TOK_TRUE);
        node->data_type = create_type(TYPE_BOOL, 1, 1);
        parser_next(p);
        return node;
    }
    
    /* Array literal */
    if (parser_match(p, TOK_LBRACKET)) {
        return parse_array_literal(p);
    }
    
    /* Built-in functions: len(), input(), read_int() */
    if (parser_match(p, TOK_LEN) || parser_match(p, TOK_INPUT) || parser_match(p, TOK_READ)) {
        TokenType builtin_type = p->current.type;
        Token builtin_tok = p->current;
        parser_next(p);
        
        if (!parser_consume(p, TOK_LPAREN)) {
            parser_error(p, "Expected '(' after builtin function");
            return NULL;
        }
        
        Node *call = create_node(NODE_CALL, builtin_tok.line, builtin_tok.col);
        Node *func = create_node(NODE_VAR, builtin_tok.line, builtin_tok.col);
        
        if (builtin_type == TOK_LEN) {
            strcpy(func->name, "len");
        } else if (builtin_type == TOK_INPUT) {
            strcpy(func->name, "input");
        } else {
            strcpy(func->name, "read_int");
        }
        
        call->call.func = func;
        call->call.args = malloc(sizeof(Node*) * 16);
        call->call.arg_count = 0;
        
        if (!parser_match(p, TOK_RPAREN)) {
            do {
                Node *arg = parse_expression(p);
                if (!arg) break;
                call->call.args[call->call.arg_count++] = arg;
            } while (parser_consume(p, TOK_COMMA));
        }
        
        if (!parser_consume(p, TOK_RPAREN)) {
            parser_error(p, "Expected ')' after builtin function arguments");
            return NULL;
        }
        
        return call;
    }
    
    /* Identifier (variable or function call) */
    if (parser_match(p, TOK_ID)) {
        Node *ident = create_node(NODE_VAR, p->current.line, p->current.col);
        strcpy(ident->name, p->current.value);
        parser_next(p); /* consume identifier */
        
        /* Check for array indexing */
        Node *current_node = ident;
        
        while (parser_match(p, TOK_LBRACKET)) {
            parser_next(p); /* consume '[' */
            Node *index_expr = parse_expression(p);
            
            if (!index_expr) {
                parser_error(p, "Expected array index expression");
                return NULL;
            }
            
            if (!parser_consume(p, TOK_RBRACKET)) {
                parser_error(p, "Expected ']' after array index");
                return NULL;
            }
            
            Node *index_node = create_node(NODE_INDEX, current_node->line, current_node->col);
            index_node->index.target = current_node;
            index_node->index.index = index_expr;
            current_node = index_node;
        }
        
        /* If followed by '(' it's a function call */
        if (parser_match(p, TOK_LPAREN)) {
            parser_next(p); /* consume '(' */
            Node *call = create_node(NODE_CALL, p->current.line, p->current.col);
            call->call.func = current_node;
            call->call.args = malloc(sizeof(Node*) * 16);
            call->call.arg_count = 0;
            
            if (!parser_match(p, TOK_RPAREN)) {
                do {
                    Node *arg = parse_expression(p);
                    if (!arg) break;
                    call->call.args[call->call.arg_count++] = arg;
                } while (parser_consume(p, TOK_COMMA));
            }
            
            if (!parser_consume(p, TOK_RPAREN)) {
                parser_error(p, "Expected ')' after function arguments");
                return NULL;
            }
            return call;
        }
        
        /* Return variable or indexed variable */
        return current_node;
    }
    
    /* Parenthesized expression */
    if (parser_match(p, TOK_LPAREN)) {
        parser_next(p);
        Node *expr = parse_expression(p);
        if (!parser_consume(p, TOK_RPAREN)) {
            parser_error(p, "Expected ')'");
            return NULL;
        }
        return expr;
    }
    
    parser_error(p, "Unexpected token in expression");
    parser_next(p);
    return NULL;
}

Node *parse_unary(Parser *p) {
    if (parser_match(p, TOK_MINUS) || parser_match(p, TOK_NOT) || 
        parser_match(p, TOK_BNOT) || parser_match(p, TOK_INC) || 
        parser_match(p, TOK_DEC)) {
        Token op = p->current;
        parser_next(p);
        Node *operand = parse_unary(p);
        
        Node *node = create_node(NODE_UNOP, op.line, op.col);
        node->unop.operand = operand;
        node->unop.op = op.type == TOK_MINUS ? '-' : 
                       op.type == TOK_NOT ? '!' : '~';
        return node;
    }
    
    return parse_primary(p);
}

Node *parse_factor(Parser *p) {
    Node *node = parse_unary(p);
    
    while (parser_match(p, TOK_ASTERISK) || parser_match(p, TOK_SLASH) || 
           parser_match(p, TOK_PERCENT)) {
        TokenType op_type = p->current.type;
        int line = p->current.line;
        int col = p->current.col;
        parser_next(p);
        Node *right = parse_unary(p);
        
        Node *binop = create_node(NODE_BINOP, line, col);
        binop->binop.left = node;
        binop->binop.right = right;
        
        if (op_type == TOK_ASTERISK) {
            strcpy(binop->binop.op, "*");
        } else if (op_type == TOK_SLASH) {
            strcpy(binop->binop.op, "/");
        } else {
            strcpy(binop->binop.op, "%");
        }
        
        node = binop;
    }
    
    return node;
}

Node *parse_term(Parser *p) {
    Node *node = parse_factor(p);
    
    while (parser_match(p, TOK_PLUS) || parser_match(p, TOK_MINUS)) {
        TokenType op_type = p->current.type;
        int line = p->current.line;
        int col = p->current.col;
        parser_next(p);
        Node *right = parse_factor(p);
        
        Node *binop = create_node(NODE_BINOP, line, col);
        binop->binop.left = node;
        binop->binop.right = right;
        
        strcpy(binop->binop.op, (op_type == TOK_PLUS) ? "+" : "-");
        
        node = binop;
    }
    
    return node;
}

Node *parse_comparison(Parser *p) {
    Node *node = parse_term(p);
    
    while (parser_match(p, TOK_LT) || parser_match(p, TOK_GT) ||
           parser_match(p, TOK_LE) || parser_match(p, TOK_GE)) {
        TokenType op_type = p->current.type;
        int line = p->current.line;
        int col = p->current.col;
        parser_next(p);
        Node *right = parse_term(p);
        
        Node *binop = create_node(NODE_BINOP, line, col);
        binop->binop.left = node;
        binop->binop.right = right;
        
        switch (op_type) {
            case TOK_LT: strcpy(binop->binop.op, "<"); break;
            case TOK_GT: strcpy(binop->binop.op, ">"); break;
            case TOK_LE: strcpy(binop->binop.op, "<="); break;
            case TOK_GE: strcpy(binop->binop.op, ">="); break;
            default: strcpy(binop->binop.op, "?"); break;
        }
        
        node = binop;
    }
    
    return node;
}

Node *parse_equality(Parser *p) {
    Node *node = parse_comparison(p);
    
    while (parser_match(p, TOK_EQ) || parser_match(p, TOK_NE)) {
        Token op = p->current;
        parser_next(p);
        Node *right = parse_comparison(p);
        
        Node *binop = create_node(NODE_BINOP, op.line, op.col);
        binop->binop.left = node;
        binop->binop.right = right;
        strcpy(binop->binop.op, (op.type == TOK_EQ) ? "==" : "!=");
        node = binop;
    }
    
    return node;
}

Node *parse_logical_and(Parser *p) {
    Node *node = parse_equality(p);
    
    while (parser_match(p, TOK_AND)) {
        Token op = p->current;
        parser_next(p);
        Node *right = parse_equality(p);
        
        Node *binop = create_node(NODE_BINOP, op.line, op.col);
        binop->binop.left = node;
        binop->binop.right = right;
        strcpy(binop->binop.op, "&&");
        node = binop;
    }
    
    return node;
}

Node *parse_logical_or(Parser *p) {
    Node *node = parse_logical_and(p);
    
    while (parser_match(p, TOK_OR)) {
        Token op = p->current;
        parser_next(p);
        Node *right = parse_logical_and(p);
        
        Node *binop = create_node(NODE_BINOP, op.line, op.col);
        binop->binop.left = node;
        binop->binop.right = right;
        strcpy(binop->binop.op, "||");
        node = binop;
    }
    
    return node;
}

Node *parse_ternary(Parser *p) {
    Node *cond = parse_logical_or(p);
    
    if (parser_consume(p, TOK_QUESTION)) {
        Node *true_expr = parse_expression(p);
        if (!parser_consume(p, TOK_COLON)) {
            parser_error(p, "Expected ':' in ternary operator");
            return NULL;
        }
        Node *false_expr = parse_expression(p);
        
        Node *node = create_node(NODE_TERNARY, cond->line, cond->col);
        node->if_stmt.cond = cond;
        node->if_stmt.then_branch = true_expr;
        node->if_stmt.else_branch = false_expr;
        return node;
    }
    
    return cond;
}

Node *parse_assignment(Parser *p) {
    Node *node = parse_ternary(p);
    
    if (parser_match(p, TOK_ASSIGN)) {
        Token op = p->current;
        parser_next(p);
        Node *right = parse_assignment(p);
        
        Node *assign = create_node(NODE_ASSIGN, op.line, op.col);
        assign->assign.target = node;
        assign->assign.value = right;
        return assign;
    }
    
    return node;
}

Node *parse_expression(Parser *p) {
    return parse_assignment(p);
}

/* =========== Statement Parsing =========== */
Node *parse_statement(Parser *p);
Node *parse_block(Parser *p);
Node *parse_print_statement(Parser *p, bool newline);
Node *parse_for_statement(Parser *p);
Node *parse_switch_statement(Parser *p);

// Special version of var declaration without semicolon for for loops
Node *parse_var_declaration_no_semi(Parser *p) {
    Token var_tok = p->current;
    parser_next(p);  // consume 'var'
    
    if (!parser_match(p, TOK_ID)) {
        parser_error(p, "Expected identifier");
        return NULL;
    }
    
    char name[MAX_ID_LEN];
    strcpy(name, p->current.value);
    parser_next(p);
    
    Type *type = NULL;
    Node *init = NULL;
    
    // Check for type annotation
    if (parser_consume(p, TOK_COLON)) {
        type = get_type_from_token(p->current.type);
        if (!type) {
            parser_error(p, "Expected type");
            return NULL;
        }
        parser_next(p);
    }
    
    // Check for initialization
    if (parser_consume(p, TOK_ASSIGN)) {
        init = parse_expression(p);
    } else if (!type) {
        parser_error(p, "Variable declaration must have type or initializer");
        return NULL;
    }
    
    // DON'T consume semicolon here - for loop handles it
    
    Node *node = create_node(NODE_VARDECL, var_tok.line, var_tok.col);
    node->vardecl.name = strdup(name);
    node->vardecl.var_type = type;
    node->vardecl.init = init;
    
    return node;
}

Node *parse_for_statement(Parser *p) {
    Token for_tok = p->current;
    parser_next(p);
    
    if (!parser_consume(p, TOK_LPAREN)) {
        parser_error(p, "Expected '(' after 'for'");
        return NULL;
    }
    
    // Parse initialization (could be var declaration or expression)
    Node *init = NULL;
    if (parser_match(p, TOK_VAR)) {
        init = parse_var_declaration_no_semi(p);
    } else if (!parser_match(p, TOK_SEMI)) {
        init = parse_expression(p);
    }
    
    if (!parser_consume(p, TOK_SEMI)) {
        parser_error(p, "Expected ';' after for loop initialization");
        return NULL;
    }
    
    // Parse condition
    Node *cond = NULL;
    if (!parser_match(p, TOK_SEMI)) {
        cond = parse_expression(p);
    }
    
    if (!parser_consume(p, TOK_SEMI)) {
        parser_error(p, "Expected ';' after for loop condition");
        return NULL;
    }
    
    // Parse increment
    Node *inc = NULL;
    if (!parser_match(p, TOK_RPAREN)) {
        inc = parse_expression(p);
    }
    
    if (!parser_consume(p, TOK_RPAREN)) {
        parser_error(p, "Expected ')' after for loop increment");
        return NULL;
    }
    
    Node *body = parse_statement(p);
    
    Node *node = create_node(NODE_FOR, for_tok.line, for_tok.col);
    node->for_loop.init = init;
    node->for_loop.cond = cond;
    node->for_loop.inc = inc;
    node->for_loop.body = body;
    return node;
}

Node *parse_if_statement(Parser *p) {
    Token if_tok = p->current;
    parser_next(p);
    
    if (!parser_consume(p, TOK_LPAREN)) {
        parser_error(p, "Expected '(' after 'if'");
        return NULL;
    }
    
    Node *cond = parse_expression(p);
    
    if (!parser_consume(p, TOK_RPAREN)) {
        parser_error(p, "Expected ')' after if condition");
        return NULL;
    }
    
    Node *then_branch = parse_statement(p);
    Node *else_branch = NULL;
    
    if (parser_consume(p, TOK_ELSE)) {
        else_branch = parse_statement(p);
    }
    
    Node *node = create_node(NODE_IF, if_tok.line, if_tok.col);
    node->if_stmt.cond = cond;
    node->if_stmt.then_branch = then_branch;
    node->if_stmt.else_branch = else_branch;
    return node;
}

Node *parse_while_statement(Parser *p) {
    Token while_tok = p->current;
    parser_next(p);
    
    if (!parser_consume(p, TOK_LPAREN)) {
        parser_error(p, "Expected '(' after 'while'");
        return NULL;
    }
    
    Node *cond = parse_expression(p);
    
    if (!parser_consume(p, TOK_RPAREN)) {
        parser_error(p, "Expected ')' after while condition");
        return NULL;
    }
    
    Node *body = parse_statement(p);
    
    Node *node = create_node(NODE_WHILE, while_tok.line, while_tok.col);
    node->while_loop.cond = cond;
    node->while_loop.body = body;
    return node;
}

Node *parse_switch_statement(Parser *p) {
    Token switch_tok = p->current;
    parser_next(p);
    
    if (!parser_consume(p, TOK_LPAREN)) {
        parser_error(p, "Expected '(' after 'switch'");
        return NULL;
    }
    
    Node *expr = parse_expression(p);
    
    if (!parser_consume(p, TOK_RPAREN)) {
        parser_error(p, "Expected ')' after switch expression");
        return NULL;
    }
    
    if (!parser_consume(p, TOK_LBRACE)) {
        parser_error(p, "Expected '{' after switch expression");
        return NULL;
    }
    
    Node *node = create_node(NODE_SWITCH, switch_tok.line, switch_tok.col);
    node->if_stmt.cond = expr;
    node->block.stmts = malloc(sizeof(Node*) * 16);
    node->block.stmt_count = 0;
    
    while (!parser_match(p, TOK_RBRACE) && !parser_match(p, TOK_EOF)) {
        if (parser_consume(p, TOK_CASE)) {
            Node *case_value = parse_expression(p);
            if (!parser_consume(p, TOK_COLON)) {
                parser_error(p, "Expected ':' after case value");
                break;
            }
            
            Node *case_node = create_node(NODE_CASE, p->current.line, p->current.col);
            case_node->if_stmt.cond = case_value;
            node->block.stmts[node->block.stmt_count++] = case_node;
        } else if (parser_consume(p, TOK_DEFAULT)) {
            if (!parser_consume(p, TOK_COLON)) {
                parser_error(p, "Expected ':' after default");
                break;
            }
            Node *default_node = create_node(NODE_DEFAULT, p->current.line, p->current.col);
            node->block.stmts[node->block.stmt_count++] = default_node;
        } else {
            Node *stmt = parse_statement(p);
            if (stmt) {
                node->block.stmts[node->block.stmt_count++] = stmt;
            }
        }
    }
    
    if (!parser_consume(p, TOK_RBRACE)) {
        parser_error(p, "Expected '}' to end switch statement");
        return NULL;
    }
    
    return node;
}

Node *parse_print_statement(Parser *p, bool newline) {
    Token print_tok = p->current;
    parser_next(p);
    
    Node *node = create_node(newline ? NODE_PRINTLN : NODE_PRINT, 
                            print_tok.line, print_tok.col);
    
    node->call.args = malloc(sizeof(Node*) * 16);
    node->call.arg_count = 0;
    
    if (parser_match(p, TOK_LPAREN)) {
        // Parse with parentheses: print(arg1, arg2)
        parser_next(p);
        
        if (!parser_match(p, TOK_RPAREN)) {
            do {
                Node *arg = parse_expression(p);
                if (arg) {
                    node->call.args[node->call.arg_count++] = arg;
                }
            } while (parser_consume(p, TOK_COMMA));
        }
        
        if (!parser_consume(p, TOK_RPAREN)) {
            parser_error(p, "Expected ')' after print arguments");
            return NULL;
        }
    } else if (!parser_match(p, TOK_SEMI) && !parser_match(p, TOK_RBRACE) && !parser_match(p, TOK_EOF)) {
        // Parse without parentheses: print arg1, arg2
        do {
            Node *arg = parse_expression(p);
            if (arg) {
                node->call.args[node->call.arg_count++] = arg;
            }
        } while (parser_consume(p, TOK_COMMA));
    }
    
    // Semicolon is optional (consume if present, but don't error if not)
    parser_consume(p, TOK_SEMI);
    
    return node;
}

Node *parse_return_statement(Parser *p) {
    Token return_tok = p->current;
    parser_next(p);
    
    Node *value = NULL;
    if (!parser_match(p, TOK_SEMI)) {
        value = parse_expression(p);
    }
    
    if (!parser_consume(p, TOK_SEMI)) {
        parser_error(p, "Expected ';' after return statement");
        return NULL;
    }
    
    Node *node = create_node(NODE_RETURN, return_tok.line, return_tok.col);
    node->binop.left = value;
    return node;
}

Node *parse_block(Parser *p) {
    if (!parser_consume(p, TOK_LBRACE)) {
        parser_error(p, "Expected '{'");
        return NULL;
    }
    
    Node *node = create_node(NODE_BLOCK, p->current.line, p->current.col);
    node->block.stmts = malloc(sizeof(Node*) * 64);
    node->block.stmt_count = 0;
    
    while (!parser_match(p, TOK_RBRACE) && !parser_match(p, TOK_EOF)) {
        Node *stmt = parse_statement(p);
        if (stmt) {
            node->block.stmts[node->block.stmt_count++] = stmt;
        } else {
            // Skip to next token on error
            parser_next(p);
        }
    }
    
    if (!parser_consume(p, TOK_RBRACE)) {
        parser_error(p, "Expected '}'");
        return NULL;
    }
    
    return node;
}

Node *parse_var_declaration(Parser *p) {
    Token var_tok = p->current;
    parser_next(p);
    
    if (!parser_match(p, TOK_ID)) {
        parser_error(p, "Expected identifier");
        return NULL;
    }
    
    char name[MAX_ID_LEN];
    strcpy(name, p->current.value);
    parser_next(p);
    
    Type *type = NULL;
    Node *init = NULL;
    
    // Check for type annotation
    if (parser_consume(p, TOK_COLON)) {
        type = get_type_from_token(p->current.type);
        if (!type) {
            parser_error(p, "Expected type");
            return NULL;
        }
        parser_next(p);
    }
    
    // Check for initialization
    if (parser_consume(p, TOK_ASSIGN)) {
        init = parse_expression(p);
        
        // Type inference if no explicit type
        if (!type && init) {
            if (init->type == NODE_NUM) {
                type = create_type(TYPE_INT64, 8, 8);
            } else if (init->type == NODE_FLOAT) {
                type = create_type(TYPE_FLOAT64, 8, 8);
            } else if (init->type == NODE_STRING) {
                type = create_string_type();
            } else if (init->type == NODE_BOOL) {
                type = create_type(TYPE_BOOL, 1, 1);
            } else if (init->type == NODE_CHAR) {
                type = create_type(TYPE_CHAR, 1, 1);
            } else {
                type = create_type(TYPE_INT64, 8, 8); // Default
            }
        }
    } else if (!type) {
        parser_error(p, "Variable declaration must have type or initializer");
        return NULL;
    }
    
    if (!parser_consume(p, TOK_SEMI)) {
        parser_error(p, "Expected ';' after variable declaration");
        return NULL;
    }
    
    Node *node = create_node(NODE_VARDECL, var_tok.line, var_tok.col);
    node->vardecl.name = strdup(name);
    node->vardecl.var_type = type;
    node->vardecl.init = init;
    
    return node;
}

Node *parse_import_statement(Parser *p) {
    Token import_tok = p->current;
    parser_next(p);
    
    if (!parser_match(p, TOK_STRING_LIT)) {
        parser_error(p, "Expected string literal for import");
        return NULL;
    }
    
    char *module_name = strdup(p->current.str_val);
    parser_next(p);
    
    if (!parser_consume(p, TOK_SEMI)) {
        parser_error(p, "Expected ';' after import statement");
        free(module_name);
        return NULL;
    }
    
    Node *node = create_node(NODE_IMPORT, import_tok.line, import_tok.col);
    node->str_val = module_name;
    
    return node;
}

Node *parse_statement(Parser *p) {
    if (parser_match(p, TOK_IF)) return parse_if_statement(p);
    if (parser_match(p, TOK_WHILE)) return parse_while_statement(p);
    if (parser_match(p, TOK_FOR)) return parse_for_statement(p);
    if (parser_match(p, TOK_SWITCH)) return parse_switch_statement(p);
    if (parser_match(p, TOK_RETURN)) return parse_return_statement(p);
    if (parser_match(p, TOK_LBRACE)) return parse_block(p);
    if (parser_match(p, TOK_VAR)) return parse_var_declaration(p);
    if (parser_match(p, TOK_PRINT)) return parse_print_statement(p, false);
    if (parser_match(p, TOK_PRINTLN)) return parse_print_statement(p, true);
    if (parser_match(p, TOK_IMPORT)) return parse_import_statement(p);
    if (parser_match(p, TOK_BREAK) || parser_match(p, TOK_CONTINUE)) {
        Node *node = create_node(parser_match(p, TOK_BREAK) ? NODE_BREAK : NODE_CONTINUE,
                                p->current.line, p->current.col);
        parser_next(p);
        if (!parser_consume(p, TOK_SEMI)) {
            parser_error(p, "Expected ';' after break/continue");
            return NULL;
        }
        return node;
    }
    
    /* Expression statement */
    Node *expr = parse_expression(p);
    if (!parser_consume(p, TOK_SEMI)) {
        parser_error(p, "Expected ';' after expression");
        return NULL;
    }
    return expr;
}

Node *parse_function(Parser *p) {
    Token func_tok = p->current;
    parser_next(p);
    
    if (!parser_match(p, TOK_ID)) {
        parser_error(p, "Expected function name");
        return NULL;
    }
    
    char name[MAX_ID_LEN];
    strcpy(name, p->current.value);
    parser_next(p);
    
    if (!parser_consume(p, TOK_LPAREN)) {
        parser_error(p, "Expected '(' after function name");
        return NULL;
    }
    
    Node *node = create_node(NODE_FUNC, func_tok.line, func_tok.col);
    node->func.name = strdup(name);
    node->func.params = malloc(sizeof(Node*) * 16);
    node->func.param_types = malloc(sizeof(Type*) * 16);
    node->func.param_count = 0;
    
    while (!parser_match(p, TOK_RPAREN)) {
        if (parser_match(p, TOK_ID)) {
            char param_name[MAX_ID_LEN];
            strcpy(param_name, p->current.value);
            parser_next(p);
            
            if (parser_consume(p, TOK_COLON)) {
                Type *param_type = get_type_from_token(p->current.type);
                if (!param_type) {
                    parser_error(p, "Expected type");
                    break;
                }
                parser_next(p);
                
                Node *param = create_node(NODE_VAR, p->current.line, p->current.col);
                strcpy(param->name, param_name);
                param->data_type = param_type;
                
                node->func.params[node->func.param_count] = param;
                node->func.param_types[node->func.param_count] = param_type;
                node->func.param_count++;
            } else {
                parser_error(p, "Expected type annotation for parameter");
                break;
            }
        }
        if (!parser_consume(p, TOK_COMMA)) break;
    }
    
    if (!parser_consume(p, TOK_RPAREN)) {
        parser_error(p, "Expected ')' after function parameters");
        return NULL;
    }
    
    /* Return type */
    if (parser_consume(p, TOK_ARROW)) {
        Type *ret_type = get_type_from_token(p->current.type);
        if (!ret_type) {
            ret_type = create_type(TYPE_VOID, 0, 1);
        }
        node->func.return_type = ret_type;
        parser_next(p);
    } else {
        node->func.return_type = create_type(TYPE_VOID, 0, 1);
    }
    
    node->func.body = parse_block(p);
    if (!node->func.body) {
        return NULL;
    }
    
    return node;
}

Node *parse_program(Parser *p) {
    Node *program = create_node(NODE_PROGRAM, 1, 1);
    Node *current = program;
    
    while (!parser_match(p, TOK_EOF)) {
        Node *node = NULL;
        
        if (parser_match(p, TOK_FUNC)) {
            node = parse_function(p);
        } else if (parser_match(p, TOK_VAR)) {
            node = parse_var_declaration(p);
        } else if (parser_match(p, TOK_IMPORT)) {
            node = parse_import_statement(p);
        } else {
            parser_error(p, "Expected function or variable declaration");
            parser_next(p);
            continue;
        }
        
        if (node) {
            current->next = node;
            current = node;
        }
    }
    
    return program;
}

/* =========== Code Generation =========== */
void codegen_init(CodeGen *cg, FILE *out, bool is_windows, bool optimize) {
    cg->out = out;
    cg->current_scope = NULL;
    cg->stack_offset = 0;
    cg->label_counter = 0;
    cg->string_counter = 0;
    cg->string_capacity = 16;
    cg->string_literals = malloc(sizeof(char*) * cg->string_capacity);
    cg->string_count = 0;
    cg->is_windows = is_windows;
    cg->optimize = optimize;
    cg->current_function = NULL;
    cg->shadow_space_size = is_windows ? 32 : 0;
    cg->param_offset = is_windows ? 48 : 16; // Skip return address and saved RBP
    cg->var_offsets = malloc(sizeof(int) * 64);
    cg->var_count = 0;
    
    /* Initialize variable tracking */
    cg->num_local_vars = 0;
    cg->current_var_offset = -8;  /* Start at rbp-8, then rbp-16, etc. */
    memset(cg->var_names, 0, sizeof(cg->var_names));
    memset(cg->var_stack_offsets, 0, sizeof(cg->var_stack_offsets));
}

void emit(CodeGen *cg, const char *fmt, ...) {
    va_list args;
    va_start(args, fmt);
    vfprintf(cg->out, fmt, args);
    va_end(args);
    fputc('\n', cg->out);
}

char *new_label(CodeGen *cg, const char *prefix) {
    size_t len = strlen(prefix) + 16;
    char *label = malloc(len);
    if (!label) {
        fprintf(stderr, "Error: Memory allocation failed for label\n");
        return NULL;
    }
    snprintf(label, len, ".%s%d", prefix, cg->label_counter++);
    return label;
}

void gen_expr(CodeGen *cg, Node *node);

void gen_num(CodeGen *cg, Node *node) {
    emit(cg, "    mov rax, %lld", (long long)node->int_val);
}

void gen_float(CodeGen *cg, Node *node) {
    emit(cg, "    mov rax, __float64__(%f)", node->float_val);
}

void gen_string(CodeGen *cg, Node *node) {
    emit(cg, "    lea rax, [str_%d]", node->int_val);
}

void gen_char(CodeGen *cg, Node *node) {
    emit(cg, "    mov rax, %d", (int)node->char_val);
}

void gen_bool(CodeGen *cg, Node *node) {
    emit(cg, "    mov rax, %d", node->bool_val ? 1 : 0);
}

/* =========== Variable Tracking Functions =========== */
int register_variable(CodeGen *cg, const char *name) {
    /* Check if variable already exists */
    for (int i = 0; i < cg->num_local_vars; i++) {
        if (strcmp(cg->var_names[i], name) == 0) {
            return cg->var_stack_offsets[i];  /* Already registered */
        }
    }
    
    /* Register new variable */
    if (cg->num_local_vars >= 64) {
        fprintf(stderr, "Error: Too many local variables (max 64)\n");
        return -8;  /* Fallback */
    }
    
    strncpy(cg->var_names[cg->num_local_vars], name, MAX_ID_LEN - 1);
    cg->var_stack_offsets[cg->num_local_vars] = cg->current_var_offset;
    cg->num_local_vars++;
    
    int offset = cg->current_var_offset;
    cg->current_var_offset -= 8;  /* Next variable goes 8 bytes lower */
    
    return offset;
}

int get_variable_offset(CodeGen *cg, const char *name) {
    for (int i = 0; i < cg->num_local_vars; i++) {
        if (strcmp(cg->var_names[i], name) == 0) {
            return cg->var_stack_offsets[i];
        }
    }
    
    /* Variable not found - register it now */
    return register_variable(cg, name);
}

void reset_variable_tracking(CodeGen *cg) {
    cg->num_local_vars = 0;
    cg->current_var_offset = -8;
    memset(cg->var_names, 0, sizeof(cg->var_names));
    memset(cg->var_stack_offsets, 0, sizeof(cg->var_stack_offsets));
}

void gen_var(CodeGen *cg, Node *node) {
    /* Get the offset for this variable */
    int offset = get_variable_offset(cg, node->name);
    emit(cg, "    mov rax, [rbp%d]", offset);
}

void gen_index(CodeGen *cg, Node *node) {
    // Generate base address (array)
    gen_expr(cg, node->index.target);
    emit(cg, "    push rax");  // Save base address
    
    // Generate index
    gen_expr(cg, node->index.index);
    emit(cg, "    pop rbx");  // Restore base address
    
    // Calculate offset: base + (index * element_size)
    // Assuming 64-bit integers (8 bytes)
    emit(cg, "    imul rax, 8");  // Multiply index by element size
    emit(cg, "    add rax, rbx");  // Add to base address
    
    // Load value at that address
    emit(cg, "    mov rax, [rax]");
}

void gen_binop(CodeGen *cg, Node *node) {
    gen_expr(cg, node->binop.right);
    emit(cg, "    push rax");
    gen_expr(cg, node->binop.left);
    emit(cg, "    pop rbx");
    
    // Division by zero check for / and %
    if (strcmp(node->binop.op, "/") == 0 || strcmp(node->binop.op, "%") == 0) {
        emit(cg, "    test rbx, rbx");
        char *div_error_label = new_label(cg, "div_zero");
        char *after_label = new_label(cg, "after_div");
        emit(cg, "    jz %s", div_error_label);
        
        if (strcmp(node->binop.op, "/") == 0) {
            emit(cg, "    xor rdx, rdx");
            emit(cg, "    idiv rbx");
        } else if (strcmp(node->binop.op, "%") == 0) {
            emit(cg, "    xor rdx, rdx");
            emit(cg, "    idiv rbx");
            emit(cg, "    mov rax, rdx");
        }
        
        emit(cg, "    jmp %s", after_label);
        emit(cg, "%s:", div_error_label);
        emit(cg, "    mov rax, 1");  // Exit code 1 for division by zero
        if (cg->is_windows) {
            emit(cg, "    mov rcx, rax");
            emit(cg, "    call ExitProcess");
        } else {
            emit(cg, "    mov rdi, rax");
            emit(cg, "    mov rax, 60");
            emit(cg, "    syscall");
        }
        emit(cg, "%s:", after_label);
    } else if (strcmp(node->binop.op, "+") == 0) {
        emit(cg, "    add rax, rbx");
    } else if (strcmp(node->binop.op, "-") == 0) {
        emit(cg, "    sub rax, rbx");
    } else if (strcmp(node->binop.op, "*") == 0) {
        emit(cg, "    imul rax, rbx");
    } else if (strcmp(node->binop.op, "==") == 0) {
        emit(cg, "    cmp rax, rbx");
        emit(cg, "    sete al");
        emit(cg, "    movzx rax, al");
    } else if (strcmp(node->binop.op, "!=") == 0) {
        emit(cg, "    cmp rax, rbx");
        emit(cg, "    setne al");
        emit(cg, "    movzx rax, al");
    } else if (strcmp(node->binop.op, "<") == 0) {
        emit(cg, "    cmp rax, rbx");
        emit(cg, "    setl al");
        emit(cg, "    movzx rax, al");
    } else if (strcmp(node->binop.op, ">") == 0) {
        emit(cg, "    cmp rax, rbx");
        emit(cg, "    setg al");
        emit(cg, "    movzx rax, al");
    } else if (strcmp(node->binop.op, "<=") == 0) {
        emit(cg, "    cmp rax, rbx");
        emit(cg, "    setle al");
        emit(cg, "    movzx rax, al");
    } else if (strcmp(node->binop.op, ">=") == 0) {
        emit(cg, "    cmp rax, rbx");
        emit(cg, "    setge al");
        emit(cg, "    movzx rax, al");
    } else if (strcmp(node->binop.op, "&&") == 0) {
        emit(cg, "    and rax, rbx");
        emit(cg, "    cmp rax, 0");
        emit(cg, "    setne al");
        emit(cg, "    movzx rax, al");
    } else if (strcmp(node->binop.op, "||") == 0) {
        emit(cg, "    or rax, rbx");
        emit(cg, "    cmp rax, 0");
        emit(cg, "    setne al");
        emit(cg, "    movzx rax, al");
    }
}

void gen_print(CodeGen *cg, Node *node, bool newline) {
    for (int i = 0; i < node->call.arg_count; i++) {
        Node *arg = node->call.args[i];
        
        // Generate the argument value
        gen_expr(cg, arg);
        
        // Choose the right print function based on type
        if (arg->data_type) {
            switch (arg->data_type->kind) {
                case TYPE_INT8:
                case TYPE_INT16:
                case TYPE_INT32:
                case TYPE_INT64:
                case TYPE_UINT8:
                case TYPE_UINT16:
                case TYPE_UINT32:
                case TYPE_UINT64:
                    emit(cg, "    mov rcx, rax");
                    emit(cg, "    call print_int");
                    break;
                    
                case TYPE_FLOAT32:
                case TYPE_FLOAT64:
                    // For floats, we need to use xmm0
                    emit(cg, "    movq xmm0, rax");
                    emit(cg, "    call print_float");
                    break;
                    
                case TYPE_BOOL:
                    emit(cg, "    mov rcx, rax");
                    emit(cg, "    call print_bool");
                    break;
                    
                case TYPE_CHAR:
                    emit(cg, "    mov rcx, rax");
                    emit(cg, "    call print_char");
                    break;
                    
                case TYPE_POINTER:
                    // Assume it's a string if it's a pointer
                    emit(cg, "    mov rcx, rax");
                    emit(cg, "    call print_string");
                    break;
                    
                default:
                    // Generic print
                    emit(cg, "    mov rcx, rax");
                    emit(cg, "    call print_int");
                    break;
            }
        } else {
            // No type info, assume integer
            emit(cg, "    mov rcx, rax");
            emit(cg, "    call print_int");
        }
        
        // Print space between arguments (except last)
        if (i < node->call.arg_count - 1) {
            emit(cg, "    mov rcx, ' '");
            emit(cg, "    call print_char");
        }
    }
    
    if (newline) {
        emit(cg, "    call println");
    }
}

void gen_call(CodeGen *cg, Node *node) {
    const char *arg_regs[] = {"rcx", "rdx", "r8", "r9"};
    
    // Handle builtin functions specially
    if (node->call.func->type == NODE_VAR) {
        const char *func_name = node->call.func->name;
        
        // len() function - returns string length
        if (strcmp(func_name, "len") == 0) {
            if (node->call.arg_count != 1) {
                fprintf(stderr, "Error: len() requires exactly one argument\n");
                return;
            }
            gen_expr(cg, node->call.args[0]);
            emit(cg, "    mov rcx, rax");
            emit(cg, "    call str_length");
            return;
        }
        
        // input() function - reads a line from stdin
        if (strcmp(func_name, "input") == 0) {
            // If there's a prompt, print it first
            if (node->call.arg_count > 0) {
                gen_expr(cg, node->call.args[0]);
                emit(cg, "    mov rcx, rax");
                emit(cg, "    call print_string");
            }
            
            // Allocate buffer for input (256 bytes on stack)
            emit(cg, "    sub rsp, 256");
            emit(cg, "    mov rcx, rsp");
            emit(cg, "    mov rdx, 256");
            emit(cg, "    call read_string");
            emit(cg, "    add rsp, 256");
            return;
        }
        
        // read_int() function - reads an integer from stdin
        if (strcmp(func_name, "read_int") == 0) {
            emit(cg, "    call read_int");
            return;
        }
    }
    
    // Regular function call - Windows x64 calling convention
    // First 4 args in rcx, rdx, r8, r9
    // Additional args on stack (right to left)
    // Need 32 bytes shadow space
    
    // Evaluate all arguments and save on stack (in reverse order)
    for (int i = node->call.arg_count - 1; i >= 0; i--) {
        gen_expr(cg, node->call.args[i]);
        emit(cg, "    push rax");
    }
    
    // Allocate shadow space
    if (cg->is_windows) {
        emit(cg, "    sub rsp, 32");
    }
    
    // Pop arguments into registers (first 4) and leave rest on stack
    for (int i = 0; i < node->call.arg_count && i < 4; i++) {
        emit(cg, "    pop %s", arg_regs[i]);
    }
    
    // Arguments 5+ are already on the stack in correct order
    
    emit(cg, "    call %s", node->call.func->name);
    
    // Clean up shadow space
    if (cg->is_windows) {
        emit(cg, "    add rsp, 32");
    }
    
    // Clean up stack arguments (if more than 4 args)
    if (node->call.arg_count > 4) {
        int stack_args = node->call.arg_count - 4;
        emit(cg, "    add rsp, %d", stack_args * 8);
    }
}

void gen_if(CodeGen *cg, Node *node) {
    gen_expr(cg, node->if_stmt.cond);
    char *else_label = new_label(cg, "else");
    char *end_label = new_label(cg, "endif");
    
    emit(cg, "    cmp rax, 0");
    emit(cg, "    je %s", else_label);
    
    gen_expr(cg, node->if_stmt.then_branch);
    emit(cg, "    jmp %s", end_label);
    
    emit(cg, "%s:", else_label);
    if (node->if_stmt.else_branch) {
        gen_expr(cg, node->if_stmt.else_branch);
    }
    
    emit(cg, "%s:", end_label);
}

void gen_while(CodeGen *cg, Node *node) {
    char *loop_label = new_label(cg, "while");
    char *end_label = new_label(cg, "endwhile");
    
    emit(cg, "%s:", loop_label);
    
    gen_expr(cg, node->while_loop.cond);
    emit(cg, "    cmp rax, 0");
    emit(cg, "    je %s", end_label);
    
    gen_expr(cg, node->while_loop.body);
    emit(cg, "    jmp %s", loop_label);
    
    emit(cg, "%s:", end_label);
}

void gen_for(CodeGen *cg, Node *node) {
    char *loop_label = new_label(cg, "for");
    char *inc_label = new_label(cg, "for_inc");
    char *end_label = new_label(cg, "endfor");
    
    // Initialization
    if (node->for_loop.init) {
        gen_expr(cg, node->for_loop.init);
    }
    
    emit(cg, "%s:", loop_label);
    
    // Condition check
    if (node->for_loop.cond) {
        gen_expr(cg, node->for_loop.cond);
        emit(cg, "    cmp rax, 0");
        emit(cg, "    je %s", end_label);
    }
    
    // Body
    gen_expr(cg, node->for_loop.body);
    
    emit(cg, "%s:", inc_label);
    
    // Increment
    if (node->for_loop.inc) {
        gen_expr(cg, node->for_loop.inc);
    }
    
    emit(cg, "    jmp %s", loop_label);
    emit(cg, "%s:", end_label);
}

void gen_return(CodeGen *cg, Node *node) {
    if (node->binop.left) {
        gen_expr(cg, node->binop.left);  // result → rax
    } else {
        emit(cg, "    mov rax, 0");
    }

    emit(cg, "    jmp .%s_epilogue", cg->current_function);
}

void gen_assign(CodeGen *cg, Node *node) {
    /* Check if this is an array index assignment: arr[i] = value */
    if (node->assign.target && node->assign.target->type == NODE_INDEX) {
        /* Calculate the address first */
        Node *index_node = node->assign.target;
        
        /* Generate the value to assign */
        gen_expr(cg, node->assign.value);
        emit(cg, "    push rax");  // Save value
        
        /* Generate base address (array) */
        gen_expr(cg, index_node->index.target);
        emit(cg, "    push rax");  // Save base address
        
        /* Generate index */
        gen_expr(cg, index_node->index.index);
        emit(cg, "    pop rbx");  // Restore base address
        
        /* Calculate offset: base + (index * element_size) */
        emit(cg, "    imul rax, 8");  // Multiply index by 8 (element size)
        emit(cg, "    add rax, rbx");  // Add to base address
        
        /* Store the value at that address */
        emit(cg, "    pop rbx");  // Restore value
        emit(cg, "    mov [rax], rbx");
        emit(cg, "    mov rax, rbx");  // Return the value in rax
        
    } else if (node->assign.target && node->assign.target->type == NODE_VAR) {
        /* Regular variable assignment */
        gen_expr(cg, node->assign.value);
        int offset = get_variable_offset(cg, node->assign.target->name);
        emit(cg, "    mov [rbp%d], rax", offset);
    } else {
        /* Fallback for other assignments */
        gen_expr(cg, node->assign.value);
        emit(cg, "    mov [rbp-8], rax");
    }
}

void gen_ternary(CodeGen *cg, Node *node) {
    char *false_label = new_label(cg, "ternary_false");
    char *end_label = new_label(cg, "ternary_end");
    
    gen_expr(cg, node->if_stmt.cond);
    emit(cg, "    cmp rax, 0");
    emit(cg, "    je %s", false_label);
    
    gen_expr(cg, node->if_stmt.then_branch);
    emit(cg, "    jmp %s", end_label);
    
    emit(cg, "%s:", false_label);
    gen_expr(cg, node->if_stmt.else_branch);
    
    emit(cg, "%s:", end_label);
}

void gen_expr(CodeGen *cg, Node *node) {
    if (!node) return;
    
    switch (node->type) {
        case NODE_NUM: gen_num(cg, node); break;
        case NODE_FLOAT: gen_float(cg, node); break;
        case NODE_STRING: gen_string(cg, node); break;
        case NODE_CHAR: gen_char(cg, node); break;
        case NODE_BOOL: gen_bool(cg, node); break;
        case NODE_VAR: gen_var(cg, node); break;
        case NODE_INDEX: gen_index(cg, node); break;
        case NODE_BINOP: gen_binop(cg, node); break;
        case NODE_CALL: gen_call(cg, node); break;
        case NODE_IF: gen_if(cg, node); break;
        case NODE_WHILE: gen_while(cg, node); break;
        case NODE_FOR: gen_for(cg, node); break;
        case NODE_RETURN: gen_return(cg, node); break;
        case NODE_ASSIGN: gen_assign(cg, node); break;
        case NODE_PRINT: gen_print(cg, node, false); break;
        case NODE_PRINTLN: gen_print(cg, node, true); break;
        case NODE_TERNARY: gen_ternary(cg, node); break;
        case NODE_BLOCK:
            for (int i = 0; i < node->block.stmt_count; i++) {
                gen_expr(cg, node->block.stmts[i]);
            }
            break;
        case NODE_VARDECL:
            if (node->vardecl.init) {
                gen_expr(cg, node->vardecl.init);
                int offset = get_variable_offset(cg, node->vardecl.name);
                emit(cg, "    mov [rbp%d], rax", offset);
            }
            break;
        default:
            break;
    }
}

void gen_function(CodeGen *cg, Node *node) {
    if (cg->is_windows && strcmp(node->func.name, "main") == 0) {
        free(node->func.name);
        node->func.name = strdup("mantel_main");
    }

    cg->current_function = node->func.name;
    
    /* Reset variable tracking for this function */
    reset_variable_tracking(cg);

    emit(cg, "%s:", node->func.name);

    /* Prologue */
    emit(cg, "    push rbp");
    emit(cg, "    mov rbp, rsp");
    emit(cg, "    push rbx");
    emit(cg, "    push rdi");
    emit(cg, "    push rsi");
    
    /* Allocate stack space for local variables (64 variables * 8 bytes = 512 bytes) */
    /* We'll allocate 512 bytes to support up to 64 local variables */
    emit(cg, "    sub rsp, 512");
    
    /* Initialize first few slots to zero (optional but good practice) */
    emit(cg, "    xor rax, rax");
    emit(cg, "    mov [rbp-8], rax");
    emit(cg, "    mov [rbp-16], rax");
    emit(cg, "    mov [rbp-24], rax");
    emit(cg, "    mov [rbp-32], rax");
    
    /* Handle function parameters - save from registers to local variables */
    if (node->func.param_count > 0) {
        const char *param_regs[] = {"rcx", "rdx", "r8", "r9"};
        
        for (int i = 0; i < node->func.param_count && i < 16; i++) {
            if (node->func.params[i] && node->func.params[i]->type == NODE_VAR) {
                const char *param_name = node->func.params[i]->name;
                
                /* Register the parameter as a variable */
                int offset = register_variable(cg, param_name);
                
                /* Save parameter from register or stack to local variable */
                if (i < 4) {
                    /* First 4 parameters come in registers (Windows x64) */
                    emit(cg, "    mov [rbp%d], %s", offset, param_regs[i]);
                } else {
                    /* Parameters 5+ are on the stack */
                    /* They're at rbp + 48 (return addr + saved rbp + 3 saved regs) + (i-4)*8 */
                    int stack_offset = 48 + (i - 4) * 8;
                    emit(cg, "    mov rax, [rbp+%d]", stack_offset);
                    emit(cg, "    mov [rbp%d], rax", offset);
                }
            }
        }
    }

    gen_expr(cg, node->func.body);

    /* Centralized epilogue */
    emit(cg, ".%s_epilogue:", node->func.name);
    emit(cg, "    add rsp, 512");
    emit(cg, "    pop rsi");
    emit(cg, "    pop rdi");
    emit(cg, "    pop rbx");
    emit(cg, "    pop rbp");
    emit(cg, "    ret");
}

void gen_program(CodeGen *cg, Node *program) {
    emit(cg, "bits 64");
    emit(cg, "default rel");
    emit(cg, "");
    emit(cg, "section .data");
    
    // Emit string literals
    for (int i = 0; i < global_string_count; i++) {
        // Escape special characters in string literals
        char *escaped = malloc(strlen(global_strings[i].str) * 2 + 1);
        char *dest = escaped;
        const char *src = global_strings[i].str;
        
        while (*src) {
            if (*src == '\n') {
                *dest++ = '\\';
                *dest++ = 'n';
            } else if (*src == '\t') {
                *dest++ = '\\';
                *dest++ = 't';
            } else if (*src == '\r') {
                *dest++ = '\\';
                *dest++ = 'r';
            } else if (*src == '"') {
                *dest++ = '\\';
                *dest++ = '"';
            } else if (*src == '\\') {
                *dest++ = '\\';
                *dest++ = '\\';
            } else {
                *dest++ = *src;
            }
            src++;
        }
        *dest = '\0';
        
        emit(cg, "str_%d: db \"%s\", 0", i, escaped);
        free(escaped);
    }
    
    Node *node = program->next;
    while (node) {
        if (node->type == NODE_VARDECL) {
            emit(cg, "    align %d", node->vardecl.var_type->align);
            emit(cg, "%s:", node->vardecl.name);
            if (node->vardecl.init) {
                if (node->vardecl.init->type == NODE_NUM) {
                    emit(cg, "    dq %lld", (long long)node->vardecl.init->int_val);
                } else if (node->vardecl.init->type == NODE_STRING) {
                    emit(cg, "    dq str_%d", node->vardecl.init->int_val);
                }
            } else {
                emit(cg, "    resb %d", node->vardecl.var_type->size);
            }
        }
        node = node->next;
    }
    
    emit(cg, "\nsection .text");
    
    /* Emit all functions first */
    node = program->next;
    while (node) {
        if (node->type == NODE_FUNC) {
            gen_function(cg, node);
        }
        node = node->next;
    }
    
    /* Now emit the platform-specific entry wrapper */
    if (cg->is_windows) {
        emit(cg, "global main");
        emit(cg, "extern ExitProcess");
        emit(cg, "extern print_int");
        emit(cg, "extern print_float");
        emit(cg, "extern print_string");
        emit(cg, "extern print_char");
        emit(cg, "extern print_bool");
        emit(cg, "extern println");
        emit(cg, "extern str_length");
        emit(cg, "extern read_string");
        emit(cg, "extern read_int");
        emit(cg, "\nmain:");
        emit(cg, "    sub rsp, 40");
        /* Call the user's main which we renamed to mantel_main */
        emit(cg, "    call mantel_main");
        emit(cg, "    add rsp, 40");
        emit(cg, "    mov rcx, rax");
        emit(cg, "    call ExitProcess");
    } else {
        emit(cg, "global _start");
        emit(cg, "extern print_int");
        emit(cg, "extern print_float");
        emit(cg, "extern print_string");
        emit(cg, "extern print_char");
        emit(cg, "extern print_bool");
        emit(cg, "extern println");
        emit(cg, "extern str_length");
        emit(cg, "extern read_string");
        emit(cg, "extern read_int");
        emit(cg, "\n_start:");
        emit(cg, "    call main");
        emit(cg, "    mov rdi, rax");
        emit(cg, "    mov rax, 60");
        emit(cg, "    syscall");
    }
}

/* =========== Memory Cleanup =========== */
void free_node(Node *node) {
    if (!node) return;
    
    switch (node->type) {
        case NODE_FUNC:
            free(node->func.name);
            free(node->func.params);
            free(node->func.param_types);
            free_node(node->func.body);
            break;
        case NODE_VARDECL:
            free(node->vardecl.name);
            if (node->vardecl.var_type) free(node->vardecl.var_type);
            free_node(node->vardecl.init);
            break;
        case NODE_CALL:
            free_node(node->call.func);
            for (int i = 0; i < node->call.arg_count; i++) {
                free_node(node->call.args[i]);
            }
            free(node->call.args);
            break;
        case NODE_BINOP:
            free_node(node->binop.left);
            free_node(node->binop.right);
            break;
        case NODE_IF:
            free_node(node->if_stmt.cond);
            free_node(node->if_stmt.then_branch);
            free_node(node->if_stmt.else_branch);
            break;
        case NODE_WHILE:
            free_node(node->while_loop.cond);
            free_node(node->while_loop.body);
            break;
        case NODE_FOR:
            free_node(node->for_loop.init);
            free_node(node->for_loop.cond);
            free_node(node->for_loop.inc);
            free_node(node->for_loop.body);
            break;
        case NODE_BLOCK:
            for (int i = 0; i < node->block.stmt_count; i++) {
                free_node(node->block.stmts[i]);
            }
            free(node->block.stmts);
            break;
        case NODE_STRING:
            if (node->str_val) free(node->str_val);
            break;
        case NODE_IMPORT:
            if (node->str_val) free(node->str_val);
            break;
        case NODE_PRINT:
        case NODE_PRINTLN:
            for (int i = 0; i < node->call.arg_count; i++) {
                free_node(node->call.args[i]);
            }
            free(node->call.args);
            break;
        case NODE_INDEX:
            free_node(node->index.target);
            free_node(node->index.index);
            break;
        case NODE_TERNARY:
            free_node(node->if_stmt.cond);
            free_node(node->if_stmt.then_branch);
            free_node(node->if_stmt.else_branch);
            break;
    }
    
    free_node(node->next);
    free(node);
}

void free_tokens(Token *tokens) {
    if (!tokens) return;
    
    for (int i = 0; tokens[i].type != TOK_EOF; i++) {
        if (tokens[i].type == TOK_STRING_LIT && tokens[i].str_val) {
            free(tokens[i].str_val);
        }
    }
    free(tokens);
}

void free_global_strings() {
    for (int i = 0; i < global_string_count; i++) {
        free(global_strings[i].str);
    }
    free(global_strings);
    global_strings = NULL;
    global_string_count = 0;
    global_string_capacity = 0;
}

/* =========== Main Compiler =========== */
int compile_file(const char *input_file, const char *output_asm, 
                 const char *output_exe, bool optimize, bool is_windows) {
    
    FILE *f = fopen(input_file, "rb");
    if (!f) {
        fprintf(stderr, "Error: Cannot open file %s\n", input_file);
        return 1;
    }
    
    fseek(f, 0, SEEK_END);
    long size = ftell(f);
    fseek(f, 0, SEEK_SET);
    
    char *source = malloc(size + 1);
    fread(source, 1, size, f);
    source[size] = '\0';
    fclose(f);
    
    printf("Tokenizing...\n");
    Token *tokens = tokenize(source);
    if (!tokens) {
        free(source);
        return 1;
    }
    
    /* Determine actual token count */
    int token_count = 0;
    while (tokens[token_count].type != TOK_EOF) token_count++;
    token_count++; /* include EOF */
    
    printf("Parsing...\n");
    Parser parser;
    parser_init(&parser, tokens, token_count, source);
    Node *ast = parse_program(&parser);
    
    if (!ast) {
        free_tokens(tokens);
        free(source);
        free_global_strings();
        return 1;
    }
    
    printf("Generating assembly...\n");
    FILE *asm_out = fopen(output_asm, "w");
    if (!asm_out) {
        fprintf(stderr, "Error: Cannot create output file %s\n", output_asm);
        free_tokens(tokens);
        free(source);
        free_global_strings();
        free_node(ast);
        return 1;
    }
    
    CodeGen cg;
    codegen_init(&cg, asm_out, is_windows, optimize);
    gen_program(&cg, ast);
    fclose(asm_out);
    
    printf("Generated: %s\n", output_asm);
    
    char obj_file[256];
    strcpy(obj_file, output_asm);
    char *dot = strrchr(obj_file, '.');
    if (dot) *dot = '\0';
    strcat(obj_file, ".obj");
    
    char cmd[512];
    if (is_windows) {
        sprintf(cmd, "nasm -f win64 -o \"%s\" \"%s\"", obj_file, output_asm);
    } else {
        sprintf(cmd, "nasm -f elf64 -o \"%s\" \"%s\"", obj_file, output_asm);
    }
    
    printf("Assembling...\n");
    if (system(cmd) != 0) {
        fprintf(stderr, "Error: Assembly failed\n");
        free_tokens(tokens);
        free(source);
        free_global_strings();
        free_node(ast);
        return 1;
    }
    
    if (is_windows) {
        sprintf(cmd, "gcc -m64 -o \"%s\" \"%s\" runtime.obj -lkernel32", output_exe, obj_file);
    } else {
        sprintf(cmd, "ld -o \"%s\" \"%s\" runtime.o", output_exe, obj_file);
    }
    
    printf("Linking...\n");
    if (system(cmd) != 0) {
        fprintf(stderr, "Error: Linking failed\n");
        free_tokens(tokens);
        free(source);
        free_global_strings();
        free_node(ast);
        return 1;
    }
    
    printf("\033[1;32m✓ Created executable: %s\033[0m\n", output_exe);
    
    free_tokens(tokens);
    free(source);
    free_global_strings();
    free_node(ast);
    
    return 0;
}

int main(int argc, char **argv) {
    if (argc < 2) {
        printf("Mantel Compiler v0.4.1\n");
        printf("BUG FIX: Variables now work correctly! Plus all v0.4.0 features.\n\n");
        printf("Usage: %s <input.mtl> [options]\n", argv[0]);
        printf("Options:\n");
        printf("  -o <output.exe>  Output executable name\n");
        printf("  -S <output.asm>  Output assembly file\n");
        printf("  -O0              Disable optimizations\n");
        printf("  -linux           Target Linux instead of Windows\n");
        printf("  -h, --help       Show this help\n\n");
        printf("Examples:\n");
        printf("  %s program.mtl\n", argv[0]);
        printf("  %s program.mtl -o myapp.exe -S output.asm\n", argv[0]);
        return 1;
    }
    
    for (int i = 1; i < argc; i++) {
        if (strcmp(argv[i], "-h") == 0 || strcmp(argv[i], "--help") == 0) {
            printf("Mantel Compiler v0.4.1 - Bug Fix Edition\n");
            printf("FIXED: Each variable now has unique memory location!\n");
            printf("Features: Functions, Variables, Loops, Conditionals, Printing,\n");
            printf("          Arrays, Strings, Comments, Input, len(), Proper Variables!\n\n");
            return 0;
        }
    }
    
    const char *input_file = argv[1];
    const char *output_exe = "a.exe";
    const char *output_asm = "output.asm";
    bool optimize = true;
    bool is_windows = true;
    
    for (int i = 2; i < argc; i++) {
        if (strcmp(argv[i], "-o") == 0 && i + 1 < argc) {
            output_exe = argv[++i];
        } else if (strcmp(argv[i], "-S") == 0 && i + 1 < argc) {
            output_asm = argv[++i];
        } else if (strcmp(argv[i], "-O0") == 0) {
            optimize = false;
        } else if (strcmp(argv[i], "-linux") == 0) {
            is_windows = false;
            if (strcmp(output_exe, "a.exe") == 0) {
                output_exe = "a.out";
            }
        }
    }
    
    printf("\033[1;36mCompiling %s for %s...\033[0m\n", 
           input_file, is_windows ? "Windows" : "Linux");
    return compile_file(input_file, output_asm, output_exe, optimize, is_windows);
}