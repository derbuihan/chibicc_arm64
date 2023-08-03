#include <stdio.h>
#include <ctype.h>
#include <stdlib.h>
#include <string.h>


// tokenizer

typedef enum {
    TK_PUNCT,
    TK_IDENT,
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
    ND_EXPR_STMT, // Expression statement
    ND_VAR,       // Variable
    ND_NUM,       // Integer
} NodeKind;

typedef struct Node Node;
struct Node {
    NodeKind kind; // Node kind
    Node *next;    // Next node
    Node *lhs;     // Left node
    Node *rhs;     // Right node
    char name;     // Variable name. Used if kind is ND_VAR.
    int val;       // Value. Used if kind is ND_NUM.
};

Node *parse(Token *tok);

// codegen

void code_gen(Node *node);
