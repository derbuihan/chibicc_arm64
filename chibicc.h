#include <assert.h>
#include <ctype.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

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

bool equal(Token *tok, char *op);

Token *tokenizer(char *p);

// parser

typedef struct Obj Obj;
struct Obj {
  Obj *next;
  char *name;  // Variable name
  Type *ty;    // Type
  int offset;  // Offset from RBP
};

typedef struct Function Function;
struct Function {
  Function *next;
  char *name;
  Obj *params;

  Node *body;
  Obj *locals;
  int stack_size;
};

typedef enum {
  ND_ADD,        // +
  ND_SUB,        // -
  ND_MUL,        // *
  ND_DIV,        // /
  ND_NEG,        // unary -
  ND_EQ,         // ==
  ND_NE,         // !=
  ND_LT,         // <
  ND_LE,         // <=
  ND_ASSIGN,     // =
  ND_ADDR,       // unary &
  ND_DEREF,      // unary *
  ND_RETURN,     // "return"
  ND_IF,         // "if"
  ND_FOR,        // "for" or "while"
  ND_BLOCK,      // { ... }
  ND_FUNCALL,    // Function call
  ND_EXPR_STMT,  // Expression statement
  ND_VAR,        // Variable
  ND_NUM,        // Integer
} NodeKind;

struct Node {
  NodeKind kind;  // Node kind
  Node *next;     // Next node
  Type *ty;       // Type, e.g. int or pointer to int

  Node *lhs;  // Left node
  Node *rhs;  // Right node

  // "if" or "for" statement
  Node *cond;
  Node *then;
  Node *els;
  Node *init;
  Node *inc;

  // Block
  Node *body;  // Block or statement body

  // Function call
  char *funcname;
  Node *args;

  Obj *var;  // Variable name. Used if kind is ND_VAR.
  int val;   // Value. Used if kind is ND_NUM.
};

Function *parse(Token *tok);

// type

typedef enum {
  TY_INT,
  TY_PTR,
  TY_FUNC,
  TY_ARRAY,
} TypeKind;

struct Type {
  TypeKind kind;  // Type kind

  int size;  // sizeof() value

  // Pointer
  Type *base;

  // Declaration
  Token *name;

  // Array
  int array_len;

  // Function type
  Type *return_ty;
  Type *params;
  Type *next;
};

extern Type *ty_int;

Type *copy_type(Type *ty);

Type *pointer_to(Type *base);

Type *func_type(Type *return_ty);

Type *array_of(Type *base, int len);

void add_type(Node *node);

// codegen

void code_gen(Function *prog);
