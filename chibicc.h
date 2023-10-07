#include <assert.h>
#include <ctype.h>
#include <errno.h>
#include <stdarg.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

typedef struct Type Type;
typedef struct Node Node;
typedef struct Member Member;

// strings

char *format(char *fmt, ...);

// tokenizer

typedef enum {
  TK_IDENT,
  TK_PUNCT,
  TK_KEYWORD,
  TK_STR,
  TK_NUM,
  TK_EOF,
} TokenKind;

typedef struct Token Token;
struct Token {
  TokenKind kind;
  Token *next;
  int64_t val;
  char *loc;
  int len;
  Type *ty;
  char *str;
  int line_no;
};

void error(char *fmt, ...);
void error_at(char *loc, char *fmt, ...);
void error_tok(Token *tok, char *fmt, ...);
bool equal(Token *tok, char *op);
Token *tokenize_file(char *path);

#define unreachable() error("internal error at %s:%d", __FILE__, __LINE__);

// parser

typedef struct Obj Obj;
struct Obj {
  Obj *next;
  char *name;     // Name
  Type *ty;       // Type
  bool is_local;  // local or global/function

  // Local variable
  int offset;

  // Global variable of function
  bool is_function;
  bool is_definition;

  // Global variable
  char *init_data;

  // Function
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
  ND_COMMA,      // ,
  ND_MEMBER,     // . (struct member access)
  ND_ADDR,       // unary &
  ND_DEREF,      // unary *
  ND_RETURN,     // "return"
  ND_IF,         // "if"
  ND_FOR,        // "for" or "while"
  ND_BLOCK,      // { ... }
  ND_FUNCALL,    // Function call
  ND_EXPR_STMT,  // Expression statement
  ND_STMT_EXPR,  // Statement expression
  ND_VAR,        // Variable
  ND_NUM,        // Integer
  ND_CAST,       // Type cast
} NodeKind;

struct Node {
  NodeKind kind;  // Node kind
  Node *next;     // Next node
  Type *ty;       // Type, e.g. int or pointer to int
  Token *tok;     // Representative token

  Node *lhs;  // Left node
  Node *rhs;  // Right node

  // "if" or "for" statement
  Node *cond;
  Node *then;
  Node *els;
  Node *init;
  Node *inc;

  // Block or statement expression
  Node *body;

  // Struct member access
  Member *member;

  // Function call
  char *funcname;
  Type *func_ty;
  Node *args;

  Obj *var;     // Variable name. Used if kind is ND_VAR.
  int64_t val;  // Value. Used if kind is ND_NUM.
};

Node *new_node(NodeKind kind, Node *lhs, Node *rhs);

Obj *parse(Token *tok);

// type

typedef enum {
  TY_VOID,
  TY_BOOL,
  TY_CHAR,
  TY_SHORT,
  TY_INT,
  TY_LONG,
  TY_ENUM,
  TY_PTR,
  TY_FUNC,
  TY_ARRAY,
  TY_STRUCT,
  TY_UNION,
} TypeKind;

struct Type {
  TypeKind kind;  // Type kind
  int size;       // sizeof() value
  int align;      // alignment

  // Pointer
  Type *base;

  // Declaration
  Token *name;

  // Array
  int array_len;

  // Struct
  Member *members;

  // Function type
  Type *return_ty;
  Type *params;
  Type *next;
};

struct Member {
  Member *next;
  Type *ty;
  Token *name;
  int offset;
};

extern Type *ty_void;

extern Type *ty_bool;

extern Type *ty_char;

extern Type *ty_short;

extern Type *ty_int;

extern Type *ty_long;

bool is_integer(Type *ty);

Type *copy_type(Type *ty);

Type *pointer_to(Type *base);

Type *func_type(Type *return_ty);

Type *array_of(Type *base, int len);

Type *enum_type(void);

void add_type(Node *node);

// codegen

void code_gen(Obj *prog, FILE *out);

int align_to(int n, int align);
