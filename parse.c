#include "chibicc.h"

typedef struct VarScope VarScope;
struct VarScope {
  VarScope *next;
  char *name;
  Obj *var;
};

typedef struct TagScope TagScope;
struct TagScope {
  TagScope *next;
  char *name;
  Type *ty;
};

typedef struct Scope Scope;
struct Scope {
  Scope *next;
  VarScope *vars;
  TagScope *tags;
};

static Obj *locals;

static Obj *globals;

static Scope *scope = &(Scope){};

static Token *global_variable(Token *tok, Type *basety);

static Token *function(Token *tok, Type *basety);

static Type *declspec(Token **rest, Token *tok);

static Type *declarator(Token **rest, Token *tok, Type *ty);

static Type *type_suffix(Token **rest, Token *tok, Type *ty);

static Type *func_params(Token **rest, Token *tok, Type *ty);

static Type *struct_decl(Token **rest, Token *tok);

static Type *union_decl(Token **rest, Token *tok);

static Type *struct_union_decl(Token **rest, Token *tok);

static void struct_members(Token **rest, Token *tok, Type *ty);

static Node *declaration(Token **rest, Token *tok);

static Node *compound_stmt(Token **rest, Token *tok);

static Node *stmt(Token **rest, Token *tok);

static Node *expr_stmt(Token **rest, Token *tok);

static Node *expr(Token **rest, Token *tok);

static Node *assign(Token **rest, Token *tok);

static Node *equality(Token **rest, Token *tok);

static Node *relational(Token **rest, Token *tok);

static Node *add(Token **rest, Token *tok);

static Node *mul(Token **rest, Token *tok);

static Node *unary(Token **rest, Token *tok);

static Node *postfix(Token **rest, Token *tok);

static Node *primary(Token **rest, Token *tok);

static Node *ident(Token **rest, Token *tok);

static Node *funcall(Token **rest, Token *tok);

static Node *num(Token **rest, Token *tok);

Node *new_node(NodeKind kind, Node *lhs, Node *rhs) {
  Node *node = calloc(1, sizeof(Node));
  node->kind = kind;
  node->lhs = lhs;
  node->rhs = rhs;
  return node;
}

static void enter_scope(void) {
  Scope *sc = calloc(1, sizeof(Scope));
  sc->next = scope;
  scope = sc;
}

static void leave_scope(void) { scope = scope->next; }

static Obj *find_var(Token *tok) {
  for (Scope *sc = scope; sc; sc = sc->next) {
    for (VarScope *sc2 = sc->vars; sc2; sc2 = sc2->next) {
      if (equal(tok, sc2->name)) {
        return sc2->var;
      }
    }
  }
  return NULL;
}

static VarScope *push_scope(char *name, Obj *var) {
  VarScope *sc = calloc(1, sizeof(VarScope));
  sc->name = name;
  sc->var = var;
  sc->next = scope->vars;
  scope->vars = sc;
  return sc;
}

static Obj *new_var(char *name, Type *ty) {
  Obj *var = calloc(1, sizeof(Obj));
  var->name = name;
  var->ty = ty;
  push_scope(name, var);
  return var;
}

static Obj *new_lvar(char *name, Type *ty) {
  Obj *var = new_var(name, ty);
  var->is_local = true;
  var->next = locals;
  locals = var;
  return var;
}

static Obj *new_gvar(char *name, Type *ty) {
  Obj *var = new_var(name, ty);
  var->next = globals;
  globals = var;
  return var;
}

static char *new_unique_name(void) {
  static int id = 0;
  return format(".L..%d", id++);
}

static Obj *new_anon_gvar(Type *ty) { return new_gvar(new_unique_name(), ty); }

static Obj *new_string_literal(char *p, Type *ty) {
  Obj *var = new_anon_gvar(ty);
  var->init_data = p;
  return var;
}

static char *get_ident(Token *tok) {
  if (tok->kind != TK_IDENT) {
    error_tok(tok, "expected an identifier");
  }
  return strndup(tok->loc, tok->len);
}

static void create_param_lvars(Type *param) {
  if (param) {
    create_param_lvars(param->next);
    new_lvar(get_ident(param->name), param);
  }
}

static Type *find_tag(Token *tok) {
  for (Scope *sc = scope; sc; sc = sc->next) {
    for (TagScope *sc2 = sc->tags; sc2; sc2 = sc2->next) {
      if (equal(tok, sc2->name)) {
        return sc2->ty;
      }
    }
  }
  return NULL;
}

static void push_tag_scope(Token *tok, Type *ty) {
  TagScope *sc = calloc(1, sizeof(TagScope));
  sc->name = strndup(tok->loc, tok->len);
  sc->ty = ty;
  sc->next = scope->tags;
  scope->tags = sc;
}

// pointer + number
static Node *new_add(Node *lhs, Node *rhs, Token *tok) {
  add_type(lhs);
  add_type(rhs);

  // num + num
  if (is_integer(lhs->ty) && is_integer(rhs->ty)) {
    return new_node(ND_ADD, lhs, rhs);
  }

  // ptr + ptr
  if (lhs->ty->base && rhs->ty->base) {
    error_tok(tok, "invalid operands");
  }

  // num + ptr -> ptr + num
  if (is_integer(lhs->ty) && rhs->ty->base) {
    Node *tmp = lhs;
    lhs = rhs;
    rhs = tmp;
  }

  // ptr + num
  Node *num_node = new_node(ND_NUM, NULL, NULL);
  num_node->val = lhs->ty->base->size;
  return new_node(ND_ADD, lhs, new_node(ND_MUL, rhs, num_node));
}

// pointer - number
static Node *new_sub(Node *lhs, Node *rhs, Token *tok) {
  add_type(lhs);
  add_type(rhs);

  // num - num
  if (is_integer(lhs->ty) && is_integer(rhs->ty)) {
    return new_node(ND_SUB, lhs, rhs);
  }

  // ptr - num -> ptr
  if (lhs->ty->base && is_integer(rhs->ty)) {
    Node *num_node = new_node(ND_NUM, NULL, NULL);
    num_node->val = lhs->ty->base->size;
    return new_node(ND_SUB, lhs, new_node(ND_MUL, rhs, num_node));
  }

  // ptr - ptr = num
  if (lhs->ty->base && rhs->ty->base) {
    Node *num_node = new_node(ND_NUM, NULL, NULL);
    num_node->val = lhs->ty->base->size;
    return new_node(ND_DIV, new_node(ND_SUB, lhs, rhs), num_node);
  }

  error_tok(tok, "invalid operands");
}

static Member *get_struct_member(Type *ty, Token *tok) {
  for (Member *mem = ty->members; mem; mem = mem->next) {
    if (mem->name->len == tok->len &&
        !strncmp(mem->name->loc, tok->loc, tok->len)) {
      return mem;
    }
  }
  error_tok(tok, "no such member");
}

static Node *struct_ref(Node *lhs, Token *tok) {
  add_type(lhs);
  if (lhs->ty->kind != TY_STRUCT && lhs->ty->kind != TY_UNION) {
    error_tok(lhs->tok, "not a struct nor a union");
  }

  Node *node = new_node(ND_MEMBER, lhs, NULL);
  node->member = get_struct_member(lhs->ty, tok);
  return node;
}

static bool is_function(Token *tok) {
  if (equal(tok->next, ";")) {
    return false;
  }

  Type dummy = {};
  Type *ty = declarator(&tok, tok, &dummy);
  return ty->kind == TY_FUNC;
}

static bool is_typename(Token *tok) {
  char *kw[] = {"char", "short", "int", "long", "struct", "union"};
  for (int i = 0; i < sizeof(kw) / sizeof(*kw); i++) {
    if (equal(tok, kw[i])) {
      return true;
    }
  }
  return false;
}

// program = (declspec global-variable | declspec function)*
// global-variable = declarator ("," declarator)* ";"
// function = declarator (";" | "{" compound-stmt)
// declspec = "char" | "short" | "int" | "long"
//          | "struct" struct-decl | "union" union-decl
// declarator = "*"* ("(" ident ")" | "(" declarator ")" | ident) type-suffix
// type-suffix = "(" func-params
//             | "[" num "]" type-suffix
//             | ε
// func-params = (param ("," param)*)? ")"
// param = declspec declarator
// struct-decl = ident? "{" struct-members
// struct-decl = struct-union-decl
// union-decl = struct-union-decl
// struct-union-decl = ident? ( "{" struct-members )?
// struct-members = (declspec declarator ("," declarator)* ";")* "}"
// declaration = declspec (declarator ("=" expr)?
//                         ("," declarator ("=" expr)?)*)? ";"
// compound-stmt = (declaration | stmt)* "}"
// stmt = "return" expr ";"
//      | "if" "(" expr ")" stmt ("else" stmt)?
//      | "for" "(" expr-stmt expr? ";" expr? ";" ")" stmt
//      | "while" "(" expr ")" stmt
//      | "{" compound-stmt
//      | expr-stmt
// expr-stmt = expr? ";"
// expr = assign ("," expr)?
// assign = equality ("=" assign)?
// equality = relational ("==" relational | "!=" relational)*
// relational = add ("<" add | "<=" add | ">" add | ">=" add)*
// add = mul ("+" mul | "-" mul)*
// mul = unary ("*" unary | "/" unary)*
// unary = ("+" | "-" | "&" | "*") unary | postfix
// postfix = primary ("[" expr "]" | "." ident | "->" ident)*
// primary = "(" "{" stmt+ "}" ")"
//         | "(" expr ")"
//         | "sizeof" unary
//         | funcall
//         | ident
//         | str
//         | num
// ident = 'a', ..., 'Z', 'a1', ..., 'a_1', ...
// funcall = ident "(" (assign ("," assign)*)? ")"
// num = 1, 2, 3, ...

// global-variable = declarator ("," declarator)* ";"
Token *global_variable(Token *tok, Type *basety) {
  int count = 0;
  while (!equal(tok, ";")) {
    if (count++ > 0) {
      assert(equal(tok, ","));
      tok = tok->next;
    }
    Type *ty = declarator(&tok, tok, basety);
    new_gvar(get_ident(ty->name), ty);
  }

  assert(equal(tok, ";"));
  tok = tok->next;  // skip ";"
  return tok;
}

// function = declarator (";" | "{" compound-stmt)
Token *function(Token *tok, Type *basety) {
  Type *ty = declarator(&tok, tok, basety);
  Obj *fn = new_gvar(get_ident(ty->name), ty);
  fn->is_function = true;
  fn->is_definition = true;

  if (equal(tok, ";")) {
    tok = tok->next;  // skip ";"
    fn->is_definition = false;
    return tok;
  }

  locals = NULL;
  enter_scope();
  create_param_lvars(ty->params);
  fn->params = locals;

  assert(equal(tok, "{"));
  fn->body = compound_stmt(&tok, tok->next);
  fn->locals = locals;
  leave_scope();
  return tok;
}

// declspec = "char" | "short" | "int" | "long"
//          | "struct" struct-decl | "union" union-decl
Type *declspec(Token **rest, Token *tok) {
  if (equal(tok, "char")) {
    *rest = tok->next;  // skip "char"
    return ty_char;
  }

  if (equal(tok, "short")) {
    *rest = tok->next;  // skip "short"
    return ty_short;
  }

  if (equal(tok, "int")) {
    *rest = tok->next;  // skip "int"
    return ty_int;
  }

  if (equal(tok, "long")) {
    *rest = tok->next;  // skip "long"
    return ty_long;
  }

  if (equal(tok, "struct")) {
    return struct_decl(rest, tok->next);
  }

  if (equal(tok, "union")) {
    return union_decl(rest, tok->next);
  }

  error_tok(tok, "typename expected");
}

// declarator = "*"* ("(" ident ")" | "(" declarator ")" | ident) type-suffix
Type *declarator(Token **rest, Token *tok, Type *ty) {
  while (equal(tok, "*")) {
    ty = pointer_to(ty);
    *rest = tok = tok->next;
  }

  if (equal(tok, "(")) {
    Token *start = tok;
    Type dummy = {};
    declarator(&tok, start->next, &dummy);
    assert(equal(tok, ")"));
    tok = tok->next;  // skip ')'
    ty = type_suffix(rest, tok, ty);
    return declarator(&tok, start->next, ty);
  }

  if (tok->kind != TK_IDENT) {
    error_tok(tok, "expected a variable name");
  }

  ty = type_suffix(rest, tok->next, ty);
  ty->name = tok;
  return ty;
}

// type-suffix = "(" func-params
//             | "[" num "]" type-suffix
//             | ε
Type *type_suffix(Token **rest, Token *tok, Type *ty) {
  if (equal(tok, "(")) {
    return func_params(rest, tok->next, ty);
  }

  if (equal(tok, "[")) {
    tok = tok->next;  // skip "["
    assert(tok->kind == TK_NUM);
    int sz = tok->val;
    tok = tok->next;  // skip num
    assert(equal(tok, "]"));
    *rest = tok = tok->next;  // skip "]"
    ty = type_suffix(rest, tok, ty);
    return array_of(ty, sz);
  }

  *rest = tok;
  return ty;
}

// func-params = (param ("," param)*)? ")"
// param = declspec declarator
Type *func_params(Token **rest, Token *tok, Type *ty) {
  Type head = {};
  Type *cur = &head;

  while (!equal(tok, ")")) {
    if (cur != &head) {
      assert(equal(tok, ","));
      tok = tok->next;
    }
    Type *basety = declspec(&tok, tok);
    Type *ty = declarator(&tok, tok, basety);
    cur = cur->next = copy_type(ty);
  }

  ty = func_type(ty);
  ty->params = head.next;
  *rest = tok->next;  // skip ")"
  return ty;
}

// struct-decl = struct-union-decl
static Type *struct_decl(Token **rest, Token *tok) {
  Type *ty = struct_union_decl(rest, tok);
  ty->kind = TY_STRUCT;

  int offset = 0;
  for (Member *mem = ty->members; mem; mem = mem->next) {
    offset = align_to(offset, mem->ty->align);
    mem->offset = offset;
    offset += mem->ty->size;

    if (ty->align < mem->ty->align) {
      ty->align = mem->ty->align;
    }
  }
  ty->size = align_to(offset, ty->align);
  return ty;
}

// union-decl = struct-union-decl
static Type *union_decl(Token **rest, Token *tok) {
  Type *ty = struct_union_decl(rest, tok);
  ty->kind = TY_UNION;

  for (Member *mem = ty->members; mem; mem = mem->next) {
    if (ty->align < mem->ty->align) {
      ty->align = mem->ty->align;
    }
    if (ty->size < mem->ty->size) {
      ty->size = mem->ty->size;
    }
  }
  ty->size = align_to(ty->size, ty->align);
  return ty;
}

// struct-union-decl = ident? ( "{" struct-members )?
static Type *struct_union_decl(Token **rest, Token *tok) {
  Token *tag = NULL;
  if (tok->kind == TK_IDENT) {
    tag = tok;
    tok = tok->next;
  }

  if (tag && !equal(tok, "{")) {
    Type *ty = find_tag(tag);
    if (!ty) {
      error_tok(tag, "unknown struct type");
    }
    *rest = tok;
    return ty;
  }

  assert(equal(tok, "{"));
  tok = tok->next;  // skip "{"

  Type *ty = calloc(1, sizeof(Type));
  struct_members(rest, tok, ty);
  ty->align = 1;

  if (tag) {
    push_tag_scope(tag, ty);
  }
  return ty;
}

// struct-members = (declspec declarator ("," declarator)* ";")* "}"
static void struct_members(Token **rest, Token *tok, Type *ty) {
  Member head = {};
  Member *cur = &head;

  while (!equal(tok, "}")) {
    Type *basety = declspec(&tok, tok);
    int i = 0;

    while (!equal(tok, ";")) {
      if (i++) {
        assert(equal(tok, ","));
        tok = tok->next;  // skip ","
      }

      Member *mem = calloc(1, sizeof(Member));
      mem->ty = declarator(&tok, tok, basety);
      mem->name = mem->ty->name;
      cur = cur->next = mem;
    }

    assert(equal(tok, ";"));
    tok = tok->next;  // skip ";"
  }

  assert(equal(tok, "}"));
  *rest = tok->next;  // skip "}"
  ty->members = head.next;
}

// declaration = declspec (declarator ("=" expr)?
//                         ("," declarator ("=" expr)?)*)? ";"
Node *declaration(Token **rest, Token *tok) {
  Type *basety = declspec(&tok, tok);

  Node head = {};
  Node *cur = &head;
  int i = 0;

  while (!equal(tok, ";")) {
    if (i++ > 0) {
      assert(equal(tok, ","));
      *rest = tok = tok->next;  // skip ','
    }
    Type *ty = declarator(&tok, tok, basety);
    assert(ty->name->kind == TK_IDENT);

    Obj *var = new_lvar(get_ident(ty->name), ty);
    if (!equal(tok, "=")) {
      continue;
    }

    Node *lhs = new_node(ND_VAR, NULL, NULL);
    lhs->var = var;
    Node *rhs = assign(&tok, tok->next);
    Node *node = new_node(ND_ASSIGN, lhs, rhs);
    cur = cur->next = new_node(ND_EXPR_STMT, node, NULL);
  }

  Node *node = new_node(ND_BLOCK, NULL, NULL);
  node->body = head.next;
  *rest = tok->next;  // skip ';'
  return node;
}

// compound-stmt = (declaration | stmt)* "}"
Node *compound_stmt(Token **rest, Token *tok) {
  Node *node = new_node(ND_BLOCK, NULL, NULL);

  Node head = {};
  Node *cur = &head;

  enter_scope();

  while (!equal(tok, "}")) {
    if (is_typename(tok)) {
      cur = cur->next = declaration(&tok, tok);
    } else {
      cur = cur->next = stmt(&tok, tok);
    }
    add_type(cur);
  }

  leave_scope();

  node->body = head.next;
  assert(equal(tok, "}"));
  *rest = tok->next;
  return node;
}

// stmt = "return" expr ";"
//      | "if" "(" expr ")" stmt ("else" stmt)?
//      | "for" "(" expr-stmt expr? ";" expr? ";" ")" stmt
//      | "while" "(" expr ")" stmt
//      | "{" compound-stmt
//      | expr-stmt
Node *stmt(Token **rest, Token *tok) {
  if (equal(tok, "return")) {
    Node *node = new_node(ND_RETURN, expr(&tok, tok->next), NULL);
    assert(equal(tok, ";"));
    *rest = tok->next;  // skip ";"
    return node;
  }

  if (equal(tok, "if")) {
    Node *node = new_node(ND_IF, NULL, NULL);
    tok = tok->next;  // skip "if"

    assert(equal(tok, "("));
    node->cond = expr(&tok, tok->next);

    assert(equal(tok, ")"));
    node->then = stmt(&tok, tok->next);

    if (equal(tok, "else")) {
      node->els = stmt(&tok, tok->next);
    }

    *rest = tok;
    return node;
  }

  if (equal(tok, "for")) {
    Node *node = new_node(ND_FOR, NULL, NULL);
    tok = tok->next;  // skip "for"

    assert(equal(tok, "("));
    node->init = expr_stmt(&tok, tok->next);

    if (!equal(tok, ";")) {
      node->cond = expr(&tok, tok);
    }
    assert(equal(tok, ";"));
    tok = tok->next;

    if (!equal(tok, ")")) {
      node->inc = expr(&tok, tok);
    }
    assert(equal(tok, ")"));

    node->then = stmt(&tok, tok->next);

    *rest = tok;
    return node;
  }

  if (equal(tok, "while")) {
    Node *node = new_node(ND_FOR, NULL, NULL);
    tok = tok->next;  // skip "while"

    assert(equal(tok, "("));
    node->cond = expr(&tok, tok->next);

    assert(equal(tok, ")"));
    node->then = stmt(&tok, tok->next);

    *rest = tok;
    return node;
  }

  if (equal(tok, "{")) {
    Node *node = compound_stmt(&tok, tok->next);
    *rest = tok;
    return node;
  }

  Node *node = expr_stmt(&tok, tok);
  *rest = tok;
  return node;
}

// expr-stmt = expr? ";"
Node *expr_stmt(Token **rest, Token *tok) {
  if (equal(tok, ";")) {
    *rest = tok->next;
    return new_node(ND_BLOCK, NULL, NULL);
  }

  Node *node = new_node(ND_EXPR_STMT, expr(&tok, tok), NULL);
  assert(equal(tok, ";"));
  *rest = tok->next;  // skip ";"
  return node;
}

// expr = assign ("," expr)?
Node *expr(Token **rest, Token *tok) {
  Node *node = assign(&tok, tok);

  if (equal(tok, ",")) {
    return new_node(ND_COMMA, node, expr(rest, tok->next));
  }

  *rest = tok;
  return node;
}

// assign = equality ("=" assign)?
Node *assign(Token **rest, Token *tok) {
  Node *node = equality(&tok, tok);

  if (equal(tok, "=")) {
    node = new_node(ND_ASSIGN, node, assign(&tok, tok->next));
  }

  *rest = tok;
  return node;
}

// equality = relational ("==" relational | "!=" relational)*
Node *equality(Token **rest, Token *tok) {
  Node *node = relational(&tok, tok);

  for (;;) {
    if (equal(tok, "==")) {
      node = new_node(ND_EQ, node, relational(&tok, tok->next));
      continue;
    }
    if (equal(tok, "!=")) {
      node = new_node(ND_NE, relational(&tok, tok->next), node);
      continue;
    }

    *rest = tok;
    return node;
  }
}

// relational = add ("<" add | "<=" add | ">" add | ">=" add)*
Node *relational(Token **rest, Token *tok) {
  Node *node = add(&tok, tok);

  for (;;) {
    if (equal(tok, "<")) {
      node = new_node(ND_LT, node, add(&tok, tok->next));
      continue;
    }
    if (equal(tok, ">")) {
      node = new_node(ND_LT, add(&tok, tok->next), node);
      continue;
    }
    if (equal(tok, "<=")) {
      node = new_node(ND_LE, node, add(&tok, tok->next));
      continue;
    }
    if (equal(tok, ">=")) {
      node = new_node(ND_LE, add(&tok, tok->next), node);
      continue;
    }

    *rest = tok;
    return node;
  }
}

// add = mul ("+" mul | "-" mul)*
Node *add(Token **rest, Token *tok) {
  Node *node = mul(&tok, tok);

  for (;;) {
    Token *start = tok;

    if (equal(tok, "+")) {
      node = new_add(node, mul(&tok, tok->next), start);
      continue;
    }

    if (equal(tok, "-")) {
      node = new_sub(node, mul(&tok, tok->next), start);
      continue;
    }

    *rest = tok;
    return node;
  }
}

// mul = unary ("*" unary | "/" unary)*
Node *mul(Token **rest, Token *tok) {
  Node *node = unary(&tok, tok);

  for (;;) {
    if (equal(tok, "*")) {
      node = new_node(ND_MUL, node, unary(&tok, tok->next));
      continue;
    }
    if (equal(tok, "/")) {
      node = new_node(ND_DIV, node, unary(&tok, tok->next));
      continue;
    }

    *rest = tok;
    return node;
  }
}

// unary = ("+" | "-" | "&" | "*") unary | postfix
Node *unary(Token **rest, Token *tok) {
  if (equal(tok, "+")) {
    Node *node = unary(&tok, tok->next);
    *rest = tok;
    return node;
  }

  if (equal(tok, "-")) {
    Node *node = new_node(ND_NEG, unary(&tok, tok->next), NULL);
    *rest = tok;
    return node;
  }

  if (equal(tok, "&")) {
    Node *node = new_node(ND_ADDR, unary(&tok, tok->next), NULL);
    *rest = tok;
    return node;
  }

  if (equal(tok, "*")) {
    Node *node = new_node(ND_DEREF, unary(&tok, tok->next), NULL);
    *rest = tok;
    return node;
  }

  Node *node = postfix(&tok, tok);
  *rest = tok;
  return node;
}

// postfix = primary ("[" expr "]" | "." ident | "->" ident)*
Node *postfix(Token **rest, Token *tok) {
  Node *node = primary(&tok, tok);

  for (;;) {
    if (equal(tok, "[")) {
      // x[y] is short for *(x+y)
      Token *start = tok;
      Node *idx = expr(&tok, tok->next);
      assert(equal(tok, "]"));
      tok = tok->next;  // skip "]"
      node = new_node(ND_DEREF, new_add(node, idx, start), NULL);
      continue;
    }

    if (equal(tok, ".")) {
      node = struct_ref(node, tok->next);
      assert(equal(tok, "."));
      tok = tok->next;  // skip "."
      assert(tok->kind == TK_IDENT);
      tok = tok->next;  // skip ident
      continue;
    }

    if (equal(tok, "->")) {
      // x->y is short for (*x).y
      node = new_node(ND_DEREF, node, NULL);
      node = struct_ref(node, tok->next);
      assert(equal(tok, "->"));
      tok = tok->next;  // skip "->"
      assert(tok->kind == TK_IDENT);
      tok = tok->next;  // skip ident
      continue;
    }

    *rest = tok;
    return node;
  }
}

// primary = "(" "{" stmt+ "}" ")"
//         | "(" expr ")"
//         | "sizeof" unary
//         | funcall
//         | ident
//         | str
//         | num
Node *primary(Token **rest, Token *tok) {
  // "(" "{" stmt+ "}" ")"
  if (equal(tok, "(") && equal(tok->next, "{")) {
    Node *node = new_node(ND_STMT_EXPR, NULL, NULL);
    node->body = compound_stmt(&tok, tok->next->next)->body;
    *rest = tok;

    assert(equal(tok, ")"));
    *rest = tok->next;  // skip ")"
    return node;
  }

  // "(" expr ")"
  if (equal(tok, "(")) {
    Node *node = expr(&tok, tok->next);
    assert(equal(tok, ")"));
    *rest = tok->next;  // skip ")"
    return node;
  }

  // "sizeof" unary
  if (equal(tok, "sizeof")) {
    Node *tmp = unary(&tok, tok->next);
    add_type(tmp);
    Node *node = new_node(ND_NUM, NULL, NULL);
    node->val = tmp->ty->size;
    *rest = tok;
    return node;
  }

  // funcall
  if (equal(tok->next, "(")) {
    Node *node = funcall(&tok, tok);
    *rest = tok;
    return node;
  }

  // ident
  if (tok->kind == TK_IDENT) {
    Node *node = ident(&tok, tok);
    *rest = tok;
    return node;
  }

  // str
  if (tok->kind == TK_STR) {
    Obj *var = new_string_literal(tok->str, tok->ty);
    *rest = tok->next;
    Node *node = new_node(ND_VAR, NULL, NULL);
    node->var = var;
    return node;
  }

  // num
  if (tok->kind == TK_NUM) {
    Node *node = num(&tok, tok);
    *rest = tok;
    return node;
  }
}

// ident = 'a', ..., 'Z', 'a1', ..., 'a_1', ...
Node *ident(Token **rest, Token *tok) {
  // find var in locals
  Obj *var = find_var(tok);

  if (!var) {
    error_tok(tok, "undefined variable");
  }

  Node *node = new_node(ND_VAR, NULL, NULL);
  node->var = var;

  *rest = tok->next;
  return node;
}

// funcall = ident "(" (assign ("," assign)*)? ")"
Node *funcall(Token **rest, Token *tok) {
  Token *start = tok;

  tok = tok->next;  // skip '('
  assert(equal(tok, "("));
  tok = tok->next;

  Node head = {};
  Node *cur = &head;
  while (!equal(tok, ")")) {
    if (cur != &head) {
      assert(equal(tok, ","));
      tok = tok->next;  // skip ','
    }
    cur = cur->next = assign(&tok, tok);
  }
  assert(equal(tok, ")"));
  tok = tok->next;  // skip ')'
  *rest = tok;

  Node *node = new_node(ND_FUNCALL, NULL, NULL);
  node->funcname = strndup(start->loc, start->len);
  node->args = head.next;
  return node;
}

// num = 1, 2, 3, ...
Node *num(Token **rest, Token *tok) {
  Node *node = new_node(ND_NUM, NULL, NULL);
  node->val = tok->val;

  *rest = tok->next;
  return node;
}

// program = (declspec global-variable | declspec function)*
Obj *parse(Token *tok) {
  globals = NULL;
  while (tok->kind != TK_EOF) {
    Type *basety = declspec(&tok, tok);

    // global variable
    if (!is_function(tok)) {
      tok = global_variable(tok, basety);
      continue;
    }

    // function
    tok = function(tok, basety);
  }
  return globals;
}
