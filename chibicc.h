#include <stdio.h>
#include <ctype.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>

typedef struct Type Type;
typedef struct Node Node;

// tokenizer

typedef enum {
    TK_IDENT,
    TK_PUNCT,
    TK_KEYWORD,
    TK_NUM,
    TK_EOF,
} TokenKind;

typedef struct Token Token;
struct Token {
    TokenKind kind;
    Token *next;
    int val;
    char *loc;
    int len;
};

Token *tokenizer(char *p);

// parser

typedef struct Obj Obj;
struct Obj {
    Obj *next;
    char *name; // Variable name
    Type *ty;   // Type
    int offset; // Offset from RBP
};

typedef struct Function Function;
struct Function {
    Node *body;
    Obj *locals;
    int stack_size;
};

typedef enum {
    ND_ADD,       // +
    ND_SUB,       // -
    ND_MUL,       // *
    ND_DIV,       // /
    ND_NEG,       // unary -
    ND_EQ,        // ==
    ND_NE,        // !=
    ND_LT,        // <
    ND_LE,        // <=
    ND_ASSIGN,    // =
    ND_ADDR,      // unary &
    ND_DEREF,     // unary *
    ND_RETURN,    // "return"
    ND_IF,        // "if"
    ND_FOR,       // "for" or "while"
    ND_BLOCK,     // { ... }
    ND_EXPR_STMT, // Expression statement
    ND_VAR,       // Variable
    ND_NUM,       // Integer
} NodeKind;

struct Node {
    NodeKind kind; // Node kind
    Node *next;    // Next node
    Type *ty;      // Type, e.g. int or pointer to int

    Node *lhs;     // Left node
    Node *rhs;     // Right node

    // "if" or "for" statement
    Node *cond;
    Node *then;
    Node *els;
    Node *init;
    Node *inc;

    // Block
    Node *body;    // Block or statement body

    Obj *var;      // Variable name. Used if kind is ND_VAR.
    int val;       // Value. Used if kind is ND_NUM.
};

Function *parse(Token *tok);

// type

typedef enum {
    TY_INT,
    TY_PTR,
} TypeKind;

struct Type {
    TypeKind kind; // Type kind
    Type *base;    // Pointer
    Token *name;   // Identifier
};

extern Type *ty_int;

Type *pointer_to(Type *base);

void add_type(Node *node);

// codegen

void code_gen(Function *prog);
