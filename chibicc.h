#include <stdio.h>
#include <ctype.h>
#include <stdlib.h>


// tokenizer

typedef enum {
    TK_PUNCT,
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
    ND_ADD,
    ND_SUB,
    ND_MUL,
    ND_DIV,
    ND_NUM,
} NodeKind;

typedef struct Node Node;
struct Node {
    NodeKind kind;
    Node *lhs;
    Node *rhs;
    int val;
};

Node *parse(Token *tok);

// codegen

void code_gen(Node *node);
