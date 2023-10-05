#include "chibicc.h"

typedef struct VarScope VarScope;
struct VarScope {
  VarScope *next;
  char *name;
  Obj *var;
  Type *type_def;
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

typedef struct {
  bool is_typedef;
} VarAttr;

static Obj *locals;

static Obj *globals;

static Scope *scope = &(Scope){};

static Obj *current_fn;

static Token *type_def(Token *tok, Type *basety);

static Token *global_variable(Token *tok, Type *basety);

static Token *function(Token *tok, Type *basety);

static Type *declspec(Token **rest, Token *tok, VarAttr *attr);

static Type *declarator(Token **rest, Token *tok, Type *ty);

static Type *type_suffix(Token **rest, Token *tok, Type *ty);

static Type *func_params(Token **rest, Token *tok, Type *ty);

static Type *struct_decl(Token **rest, Token *tok);

static Type *union_decl(Token **rest, Token *tok);

static Type *struct_union_decl(Token **rest, Token *tok);

static void struct_members(Token **rest, Token *tok, Type *ty);

static Node *declaration(Token **rest, Token *tok, Type *basety);

static Node *compound_stmt(Token **rest, Token *tok);

static Node *stmt(Token **rest, Token *tok);

static Node *expr_stmt(Token **rest, Token *tok);

static Node *expr(Token **rest, Token *tok);

static Node *assign(Token **rest, Token *tok);

static Node *equality(Token **rest, Token *tok);

static Node *relational(Token **rest, Token *tok);

static Node *add(Token **rest, Token *tok);

static Node *mul(Token **rest, Token *tok);

static Node *cast(Token **rest, Token *tok);

static Node *unary(Token **rest, Token *tok);

static Node *postfix(Token **rest, Token *tok);

static Node *primary(Token **rest, Token *tok);

static Type *type_name(Token **rest, Token *tok);

static Type *abstract_declarator(Token **rest, Token *tok, Type *ty);

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

static VarScope *find_var(Token *tok) {
  for (Scope *sc = scope; sc; sc = sc->next) {
    for (VarScope *sc2 = sc->vars; sc2; sc2 = sc2->next) {
      if (equal(tok, sc2->name)) {
        return sc2;
      }
    }
  }
  return NULL;
}

static VarScope *push_scope(char *name) {
  VarScope *sc = calloc(1, sizeof(VarScope));
  sc->name = name;
  sc->next = scope->vars;
  scope->vars = sc;
  return sc;
}

static Obj *new_var(char *name, Type *ty) {
  Obj *var = calloc(1, sizeof(Obj));
  var->name = name;
  var->ty = ty;
  push_scope(name)->var = var;
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
  num_node->ty = ty_long;
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
    num_node->ty = ty_long;
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

static Type *find_typedef(Token *tok) {
  if (tok->kind == TK_IDENT) {
    VarScope *sc = find_var(tok);
    if (sc) {
      return sc->type_def;
    }
  }
  return NULL;
}

static bool is_typename(Token *tok) {
  char *kw[] = {"void", "char",   "short", "int",
                "long", "struct", "union", "typedef"};
  for (int i = 0; i < sizeof(kw) / sizeof(*kw); i++) {
    if (equal(tok, kw[i])) {
      return true;
    }
  }
  return find_typedef(tok);
}

// program = (desclspec (type_def | global-variable | function))*
// type_def = declarator ("," declarator)* ";"
// global-variable = declarator ("," declarator)* ";"
// function = declarator (";" | "{" compound-stmt)
// declspec = ("void" | "char" | "short" | "int" | "long"
//            | "typedef" | typedef-name
//            | "struct" struct-decl | "union" union-decl)+
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
// declaration = (declarator ("=" expr)? ("," declarator ("=" expr)?)*)? ";"
// compound-stmt = (declspec (type_def | declaration) | stmt)* "}"
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
// mul = cast ("*" cast | "/" cast)*
// cast = "(" type-name ")" cast | unary
// unary = ("+" | "-" | "&" | "*") cast | postfix
// postfix = primary ("[" expr "]" | "." ident | "->" ident)*
// primary = "(" "{" stmt+ "}" ")"
//         | "(" expr ")"
//         | "sizeof" "(" type-name ")"
//         | "sizeof" unary
//         | funcall
//         | ident
//         | str
//         | num
// type-name = declspec abstract-declarator
// abstract-declarator = "*"* ("(" abstract-declarator ")")? type-suffix
// ident = 'a', ..., 'Z', 'a1', ..., 'a_1', ...
// funcall = ident "(" (assign ("," assign)*)? ")"
// num = 1, 2, 3, ...

// type_def = declarator ("," declarator)* ";"
Token *type_def(Token *tok, Type *basety) {
  bool first = true;
  while (!equal(tok, ";")) {
    if (!first) {
      assert(equal(tok, ","));
      tok = tok->next;  // skip ","
    }
    first = false;
    Type *ty = declarator(&tok, tok, basety);
    push_scope(get_ident(ty->name))->type_def = ty;
  }
  assert(equal(tok, ";"));
  tok = tok->next;  // skip ";"
  return tok;
}

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

  current_fn = fn;
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

// declspec = ("void" | "char" | "short" | "int" | "long"
//            | "typedef" | typedef-name
//            | "struct" struct-decl | "union" union-decl)+
Type *declspec(Token **rest, Token *tok, VarAttr *attr) {
  enum {
    VOID = 1 << 0,
    CHAR = 1 << 2,
    SHORT = 1 << 4,
    INT = 1 << 6,
    LONG = 1 << 8,
    OTHER = 1 << 10,
  };
  Type *ty = ty_int;
  int counter = 0;

  while (is_typename(tok)) {
    if (equal(tok, "typedef")) {
      if (!attr) {
        error_tok(tok,
                  "storage class specifier is not allowed in this context");
      }
      attr->is_typedef = true;
      tok = tok->next;
      continue;
    }

    Type *ty2 = find_typedef(tok);
    if (equal(tok, "struct") || equal(tok, "union") || ty2) {
      if (counter) {
        break;
      }
      if (equal(tok, "struct")) {
        ty = struct_decl(&tok, tok->next);
      } else if (equal(tok, "union")) {
        ty = union_decl(&tok, tok->next);
      } else {
        ty = ty2;
        tok = tok->next;
      }
      counter += OTHER;
      continue;
    }

    if (equal(tok, "void")) {
      counter += VOID;
    } else if (equal(tok, "char")) {
      counter += CHAR;
    } else if (equal(tok, "short")) {
      counter += SHORT;
    } else if (equal(tok, "int")) {
      counter += INT;
    } else if (equal(tok, "long")) {
      counter += LONG;
    } else {
      unreachable();
    }

    switch (counter) {
      case VOID:
        ty = ty_void;
        break;
      case CHAR:
        ty = ty_char;
        break;
      case SHORT:
      case SHORT + INT:
        ty = ty_short;
        break;
      case INT:
        ty = ty_int;
        break;
      case LONG:
      case LONG + INT:
      case LONG + LONG:
      case LONG + LONG + INT:
        ty = ty_long;
        break;
      default:
        error_tok(tok, "invalid type");
    }
    tok = tok->next;
  }

  *rest = tok;
  return ty;
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
    Type *basety = declspec(&tok, tok, NULL);
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
    Type *basety = declspec(&tok, tok, NULL);
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

// declaration = (declarator ("=" expr)? ("," declarator ("=" expr)?)*)? ";"
Node *declaration(Token **rest, Token *tok, Type *basety) {
  Node head = {};
  Node *cur = &head;
  int i = 0;

  while (!equal(tok, ";")) {
    if (i++ > 0) {
      assert(equal(tok, ","));
      *rest = tok = tok->next;  // skip ','
    }
    Type *ty = declarator(&tok, tok, basety);
    if (ty->kind == TY_VOID) {
      error_tok(ty->name, "variable declared void");
    }

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

// compound-stmt = (declspec (type_def | declaration) | stmt)* "}"
Node *compound_stmt(Token **rest, Token *tok) {
  Node *node = new_node(ND_BLOCK, NULL, NULL);

  Node head = {};
  Node *cur = &head;

  enter_scope();

  while (!equal(tok, "}")) {
    if (is_typename(tok)) {
      VarAttr attr = {};
      Type *basety = declspec(&tok, tok, &attr);

      if (attr.is_typedef) {
        tok = type_def(tok, basety);
        continue;
      }

      cur = cur->next = declaration(&tok, tok, basety);
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
    Node *node = new_node(ND_RETURN, NULL, NULL);
    Node *exp = expr(&tok, tok->next);
    assert(equal(tok, ";"));
    *rest = tok->next;  // skip ";"

    add_type(exp);
    node->lhs = new_node(ND_CAST, exp, NULL);
    node->lhs->ty = current_fn->ty->return_ty;
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

// mul = cast ("*" cast | "/" cast)*
Node *mul(Token **rest, Token *tok) {
  Node *node = cast(&tok, tok);

  for (;;) {
    if (equal(tok, "*")) {
      node = new_node(ND_MUL, node, cast(&tok, tok->next));
      continue;
    }
    if (equal(tok, "/")) {
      node = new_node(ND_DIV, node, cast(&tok, tok->next));
      continue;
    }

    *rest = tok;
    return node;
  }
}

// cast = "(" type-name ")" cast | unary
Node *cast(Token **rest, Token *tok) {
  if (equal(tok, "(") && is_typename(tok->next)) {
    Token *start = tok;
    Type *ty = type_name(&tok, tok->next);
    assert(equal(tok, ")"));
    tok = tok->next;  // skip ")"

    Node *lhs = cast(rest, tok);
    add_type(lhs);
    Node *node = new_node(ND_CAST, lhs, NULL);
    node->tok = lhs->tok;
    node->ty = copy_type(ty);

    return node;
  }
  return unary(rest, tok);
}

// unary = ("+" | "-" | "&" | "*") cast | postfix
Node *unary(Token **rest, Token *tok) {
  if (equal(tok, "+")) {
    Node *node = cast(&tok, tok->next);
    *rest = tok;
    return node;
  }

  if (equal(tok, "-")) {
    Node *node = new_node(ND_NEG, cast(&tok, tok->next), NULL);
    *rest = tok;
    return node;
  }

  if (equal(tok, "&")) {
    Node *node = new_node(ND_ADDR, cast(&tok, tok->next), NULL);
    *rest = tok;
    return node;
  }

  if (equal(tok, "*")) {
    Node *node = new_node(ND_DEREF, cast(&tok, tok->next), NULL);
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
//         | "sizeof" "(" type-name ")"
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

  // "sizeof" "(" type-name ")"
  if (equal(tok, "sizeof") && equal(tok->next, "(") &&
      is_typename(tok->next->next)) {
    Type *ty = type_name(&tok, tok->next->next);
    assert(equal(tok, ")"));
    tok = tok->next;  // skip ")"
    Node *node = new_node(ND_NUM, NULL, NULL);
    node->val = ty->size;
    *rest = tok;
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

// type-name = declspec abstract-declarator
Type *type_name(Token **rest, Token *tok) {
  Type *ty = declspec(&tok, tok, NULL);
  return abstract_declarator(rest, tok, ty);
}

// abstract-declarator = "*"* ("(" abstract-declarator ")")? type-suffix
Type *abstract_declarator(Token **rest, Token *tok, Type *ty) {
  while (equal(tok, "*")) {
    ty = pointer_to(ty);
    tok = tok->next;
  }

  if (equal(tok, "(")) {
    Token *start = tok;
    Type dummy = {};
    abstract_declarator(&tok, start->next, &dummy);
    assert(equal(tok, ")"));
    ty = type_suffix(rest, tok->next, ty);
    return abstract_declarator(&tok, start->next, ty);
  }

  return type_suffix(rest, tok, ty);
}

// ident = 'a', ..., 'Z', 'a1', ..., 'a_1', ...
Node *ident(Token **rest, Token *tok) {
  // find var in locals
  VarScope *sc = find_var(tok);

  if (!sc || !sc->var) {
    error_tok(tok, "undefined variable");
  }

  Node *node = new_node(ND_VAR, NULL, NULL);
  node->var = sc->var;

  *rest = tok->next;
  return node;
}

// funcall = ident "(" (assign ("," assign)*)? ")"
Node *funcall(Token **rest, Token *tok) {
  Token *start = tok;

  tok = tok->next;  // skip '('
  assert(equal(tok, "("));
  tok = tok->next;

  VarScope *sc = find_var(start);
  if (!sc) {
    error_tok(start, "implicit declaration of a function");
  }
  if (!sc->var || sc->var->ty->kind != TY_FUNC) {
    error_tok(start, "not a function");
  }

  Type *ty = sc->var->ty;
  Type *param_ty = ty->params;
  Node head = {};
  Node *cur = &head;
  while (!equal(tok, ")")) {
    if (cur != &head) {
      assert(equal(tok, ","));
      tok = tok->next;  // skip ','
    }
    Node *arg = assign(&tok, tok);
    add_type(arg);
    if (param_ty) {
      if (param_ty->kind == TY_STRUCT || param_ty->kind == TY_UNION) {
        error_tok(arg->tok, "passing a struct is not supported yet");
      }
      arg = new_node(ND_CAST, arg, NULL);
      arg->ty = param_ty;
      param_ty = param_ty->next;
    }
    cur = cur->next = arg;
  }
  assert(equal(tok, ")"));
  tok = tok->next;  // skip ')'
  *rest = tok;

  Node *node = new_node(ND_FUNCALL, NULL, NULL);
  node->funcname = strndup(start->loc, start->len);
  node->func_ty = ty;
  node->ty = ty->return_ty;
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

// program = (desclspec (type_def | global-variable | function))*
Obj *parse(Token *tok) {
  globals = NULL;
  while (tok->kind != TK_EOF) {
    VarAttr attr = {};
    Type *basety = declspec(&tok, tok, &attr);

    // typedef
    if (attr.is_typedef) {
      tok = type_def(tok, basety);
      continue;
    }

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
