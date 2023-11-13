#include "chibicc.h"

typedef struct VarScope VarScope;
struct VarScope {
  VarScope *next;
  char *name;
  Obj *var;
  Type *type_def;
  Type *enum_ty;
  int enum_val;
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
  bool is_static;
} VarAttr;

static Obj *locals;

static Obj *globals;

static Scope *scope = &(Scope){};

static Obj *current_fn;

static Node *gotos;

static Node *labels;

static char *brk_label;

static char *cont_label;

static Node *current_switch;

static Token *type_def(Token *tok, Type *basety);

static Token *global_variable(Token *tok, Type *basety);

static Token *function(Token *tok, Type *basety, VarAttr *attr);

static Type *declspec(Token **rest, Token *tok, VarAttr *attr);

static Type *declarator(Token **rest, Token *tok, Type *ty);

static Type *type_suffix(Token **rest, Token *tok, Type *ty);

static Type *func_params(Token **rest, Token *tok, Type *ty);

static Type *array_dimensions(Token **rest, Token *tok, Type *ty);

static Type *struct_decl(Token **rest, Token *tok);

static Type *union_decl(Token **rest, Token *tok);

static Type *struct_union_decl(Token **rest, Token *tok);

static void struct_members(Token **rest, Token *tok, Type *ty);

static Type *enum_specifier(Token **rest, Token *tok);

static Node *declaration(Token **rest, Token *tok, Type *basety);

static Node *compound_stmt(Token **rest, Token *tok);

static Node *stmt(Token **rest, Token *tok);

static Node *expr_stmt(Token **rest, Token *tok);

static Node *expr(Token **rest, Token *tok);

static Node *assign(Token **rest, Token *tok);

static int64_t const_expr(Token **rest, Token *tok);

static Node *conditional(Token **rest, Token *tok);

static Node *logor(Token **rest, Token *tok);

static Node *logand(Token **rest, Token *tok);

static Node * bitor (Token * *rest, Token *tok);

static Node *bitxor(Token **rest, Token *tok);

static Node *bitand(Token **rest, Token *tok);

static Node *equality(Token **rest, Token *tok);

static Node *relational(Token **rest, Token *tok);

static Node *shift(Token **rest, Token *tok);

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

Node *new_node(NodeKind kind, Node *lhs, Node *rhs, Token *tok) {
  Node *node = calloc(1, sizeof(Node));
  node->kind = kind;
  node->lhs = lhs;
  node->rhs = rhs;
  node->tok = tok;
  return node;
}

static void enter_scope(void) {
  Scope *sc = calloc(1, sizeof(Scope));
  sc->next = scope;
  scope = sc;
}

static void leave_scope(void) { scope = scope->next; }

static void resolve_goto_labels(void) {
  for (Node *x = gotos; x; x = x->goto_next) {
    for (Node *y = labels; y; y = y->goto_next) {
      if (!strcmp(x->label, y->label)) {
        x->unique_label = y->unique_label;
        break;
      }
    }
    if (x->unique_label == NULL) {
      error_tok(x->tok->next, "use of undeclared label");
    }
  }
  gotos = labels = NULL;
}

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
    return new_node(ND_ADD, lhs, rhs, tok);
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
  Node *num_node = new_node(ND_NUM, NULL, NULL, tok);
  num_node->val = lhs->ty->base->size;
  num_node->ty = ty_long;
  return new_node(ND_ADD, lhs, new_node(ND_MUL, rhs, num_node, tok), tok);
}

// pointer - number
static Node *new_sub(Node *lhs, Node *rhs, Token *tok) {
  add_type(lhs);
  add_type(rhs);

  // num - num
  if (is_integer(lhs->ty) && is_integer(rhs->ty)) {
    return new_node(ND_SUB, lhs, rhs, tok);
  }

  // ptr - num -> ptr
  if (lhs->ty->base && is_integer(rhs->ty)) {
    Node *num_node = new_node(ND_NUM, NULL, NULL, tok);
    num_node->val = lhs->ty->base->size;
    num_node->ty = ty_long;
    return new_node(ND_SUB, lhs, new_node(ND_MUL, rhs, num_node, tok), tok);
  }

  // ptr - ptr = num
  if (lhs->ty->base && rhs->ty->base) {
    Node *num_node = new_node(ND_NUM, NULL, NULL, tok);
    num_node->val = lhs->ty->base->size;
    return new_node(ND_DIV, new_node(ND_SUB, lhs, rhs, tok), num_node, tok);
  }

  error_tok(tok, "invalid operands");
}

// Convert `A op= B` to `tmp = &A, *tmp = *tmp op B`
// where tmp is a fresh pointer variable.
static Node *to_assign(Node *binary) {
  add_type(binary->lhs);
  add_type(binary->rhs);
  Token *tok = binary->tok;

  Node *tmp = new_node(ND_VAR, NULL, NULL, tok);
  tmp->var = new_lvar("", pointer_to(binary->lhs->ty));

  Node *expr1 =
      new_node(ND_ASSIGN, tmp, new_node(ND_ADDR, binary->lhs, NULL, tok), tok);
  Node *expr2 =
      new_node(ND_ASSIGN, new_node(ND_DEREF, tmp, NULL, tok),
               new_node(binary->kind, new_node(ND_DEREF, tmp, NULL, tok),
                        binary->rhs, tok),
               tok);

  return new_node(ND_COMMA, expr1, expr2, tok);
}

Node *new_cast(Node *expr, Type *ty) {
  add_type(expr);
  Node *node = new_node(ND_CAST, expr, NULL, expr->tok);
  node->ty = copy_type(ty);
  return node;
}

static Node *new_inc_dec(Node *node, Token *tok, int addend) {
  add_type(node);
  Node *num = new_node(ND_NUM, NULL, NULL, tok);
  num->val = addend;
  return new_cast(new_sub(to_assign(new_add(node, num, tok)), num, tok),
                  node->ty);
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

  Node *node = new_node(ND_MEMBER, lhs, NULL, tok);
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
  char *kw[] = {"void",   "_Bool", "char",    "short", "int",   "long",
                "struct", "union", "typedef", "enum",  "static"};
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
// declspec = ("void" | "_Bool" | "char" | "short" | "int" | "long"
//            | "typedef" | "static" | typedef-name
//            | "struct" struct-decl | "union" union-decl
//            | "enum" enum-specifier)+
// declarator = "*"* ("(" ident ")" | "(" declarator ")" | ident) type-suffix
// type-suffix = "(" func-params
//             | "[" array-dimensions
//             | ε
// func-params = (param ("," param)*)? ")"
// param = declspec declarator
// array-dimensions = const-expr? "]" type-suffix
// struct-decl = ident? "{" struct-members
// struct-decl = struct-union-decl
// union-decl = struct-union-decl
// struct-union-decl = ident? ( "{" struct-members )?
// struct-members = (declspec declarator ("," declarator)* ";")* "}"
// enum-specifier = ident? "{" enum-list? "}"
//                | ident ("{" enum-list? "}")?
// enum-list = ident ("=" const-expr)? ("," ident ("=" const-expr)?)*
// declaration = declspec (declarator ("=" expr)?
//                         ("," declarator ("=" expr)?)*)? ";"
// declaration = (declarator ("=" expr)? ("," declarator ("=" expr)?)*)? ";"
// compound-stmt = (declspec (type_def | declaration) | stmt)* "}"
// stmt = "return" expr ";"
//      | "if" "(" expr ")" stmt ("else" stmt)?
//      | "switch" "(" expr ")" stmt
//      | "case" const-expr ":" stmt
//      | "default" ":" stmt
//      | "for" "(" expr-stmt expr? ";" expr? ";" ")" stmt
//      | "while" "(" expr ")" stmt
//      | "goto" ident ";"
//      | "break" ";"
//      | "continue" ";"
//      | ident ":" stmt
//      | "{" compound-stmt
//      | expr-stmt
// expr-stmt = expr? ";"
// expr = assign ("," expr)?
// assign = conditional (assign-op assign)?
// assign-op = "=" | "+=" | "-=" | "*=" | "/=" | "%=" | "&=" | "|=" | "^="
//           | "<<=" | ">>="
// const-expr = conditional
// conditional = logor ("?" expr ":" conditional)?
// logor = logand ("||" logand)*
// logand = bitor ("&&" bitor)*
// bitor = bitxor ("|" bitxor)*
// bitxor = bitand ("^" bitand)*
// bitand = equality ("&" equality)*
// equality = relational ("==" relational | "!=" relational)*
// relational = shift ("<" shift | "<=" shift | ">" shift | ">=" shift)*
// shift = add ("<<" add | ">>" add)*
// add = mul ("+" mul | "-" mul)*
// mul = cast ("*" cast | "/" cast | "%" cast)*
// cast = "(" type-name ")" cast | unary
// unary = ("+" | "-" | "&" | "*" | "!" | "~")? cast
//       | ("++" | "--") unary
//       | postfix
// postfix = primary ("[" expr "]" | "." ident | "->" ident | "++" | "--")*
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
Token *function(Token *tok, Type *basety, VarAttr *attr) {
  Type *ty = declarator(&tok, tok, basety);
  Obj *fn = new_gvar(get_ident(ty->name), ty);
  fn->is_function = true;
  fn->is_definition = true;
  fn->is_static = attr->is_static;

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
  resolve_goto_labels();
  return tok;
}

// declspec = ("void" | "_Bool" | "char" | "short" | "int" | "long"
//            | "typedef" | "static" | typedef-name
//            | "struct" struct-decl | "union" union-decl
//            | "enum" enum-specifier)+
Type *declspec(Token **rest, Token *tok, VarAttr *attr) {
  enum {
    VOID = 1 << 0,
    BOOL = 1 << 2,
    CHAR = 1 << 4,
    SHORT = 1 << 6,
    INT = 1 << 8,
    LONG = 1 << 10,
    OTHER = 1 << 12,
  };
  Type *ty = ty_int;
  int counter = 0;

  while (is_typename(tok)) {
    if (equal(tok, "typedef") || equal(tok, "static")) {
      if (!attr) {
        error_tok(tok,
                  "storage class specifier is not allowed in this context");
      }
      if (equal(tok, "typedef")) {
        attr->is_typedef = true;
      } else {
        attr->is_static = true;
      }
      if (attr->is_typedef && attr->is_static) {
        error_tok(tok, "typedef and static may not be used together");
      }
      tok = tok->next;
      continue;
    }

    Type *ty2 = find_typedef(tok);
    if (equal(tok, "struct") || equal(tok, "union") || equal(tok, "enum") ||
        ty2) {
      if (counter) {
        break;
      }
      if (equal(tok, "struct")) {
        ty = struct_decl(&tok, tok->next);
      } else if (equal(tok, "union")) {
        ty = union_decl(&tok, tok->next);
      } else if (equal(tok, "enum")) {
        ty = enum_specifier(&tok, tok->next);
      } else {
        ty = ty2;
        tok = tok->next;
      }
      counter += OTHER;
      continue;
    }

    if (equal(tok, "void")) {
      counter += VOID;
    } else if (equal(tok, "_Bool")) {
      counter += BOOL;
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
      case BOOL:
        ty = ty_bool;
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
//             | "[" array-dimensions
//             | ε
Type *type_suffix(Token **rest, Token *tok, Type *ty) {
  if (equal(tok, "(")) {
    return func_params(rest, tok->next, ty);
  }

  if (equal(tok, "[")) {
    return array_dimensions(rest, tok->next, ty);
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
    Type *ty2 = declspec(&tok, tok, NULL);
    ty2 = declarator(&tok, tok, ty2);
    if (ty2->kind == TY_ARRAY) {
      Token *name = ty2->name;
      ty2 = pointer_to(ty2->base);
      ty2->name = name;
    }
    cur = cur->next = copy_type(ty2);
  }

  ty = func_type(ty);
  ty->params = head.next;
  *rest = tok->next;  // skip ")"
  return ty;
}

// array-dimensions = const-expr? "]" type-suffix
static Type *array_dimensions(Token **rest, Token *tok, Type *ty) {
  if (equal(tok, "]")) {
    ty = type_suffix(rest, tok->next, ty);
    return array_of(ty, -1);
  }
  int sz = const_expr(&tok, tok);
  assert(equal(tok, "]"));
  *rest = tok = tok->next;  // skip "]"
  ty = type_suffix(rest, tok, ty);
  return array_of(ty, sz);
}

// struct-decl = struct-union-decl
static Type *struct_decl(Token **rest, Token *tok) {
  Type *ty = struct_union_decl(rest, tok);
  ty->kind = TY_STRUCT;
  if (ty->size < 0) {
    return ty;
  }

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

  if (ty->size < 0) {
    return ty;
  }

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
    *rest = tok;
    Type *ty = find_tag(tag);
    if (ty) {
      return ty;
    }

    ty = struct_type();
    ty->size = -1;
    push_tag_scope(tag, ty);
    return ty;
  }

  assert(equal(tok, "{"));
  tok = tok->next;  // skip "{"

  Type *ty = struct_type();
  struct_members(rest, tok, ty);

  if (tag) {
    for (TagScope *sc = scope->tags; sc; sc = sc->next) {
      if (equal(tag, sc->name)) {
        *sc->ty = *ty;
        return sc->ty;
      }
    }
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

// enum-specifier = ident? "{" enum-list? "}"
//                | ident ("{" enum-list? "}")?
// enum-list = ident ("=" const-expr)? ("," ident ("=" const-expr)?)*
static Type *enum_specifier(Token **rest, Token *tok) {
  Type *ty = enum_type();

  Token *tag = NULL;
  if (tok->kind == TK_IDENT) {
    tag = tok;
    tok = tok->next;
  }

  if (tag && !equal(tok, "{")) {
    Type *ty = find_tag(tag);
    if (!ty) {
      error_tok(tag, "unknown enum type");
    }
    if (ty->kind != TY_ENUM) {
      error_tok(tag, "not an enum tag");
    }
    *rest = tok;
    return ty;
  }

  assert(equal(tok, "{"));
  tok = tok->next;  // skip "{"

  int i = 0;
  int val = 0;
  while (!equal(tok, "}")) {
    if (i++ > 0) {
      assert(equal(tok, ","));
      tok = tok->next;  // skip ","
    }

    char *name = get_ident(tok);
    tok = tok->next;  // skip ident

    if (equal(tok, "=")) {
      val = const_expr(&tok, tok->next);
    }

    VarScope *sc = push_scope(name);
    sc->enum_ty = ty;
    sc->enum_val = val++;
  }
  *rest = tok->next;  // skip "}"
  if (tag) {
    push_tag_scope(tag, ty);
  }
  return ty;
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
    if (ty->size < 0) {
      error_tok(tok, "variable has incomplete type");
    }
    if (ty->kind == TY_VOID) {
      error_tok(ty->name, "variable declared void");
    }

    assert(ty->name->kind == TK_IDENT);
    Obj *var = new_lvar(get_ident(ty->name), ty);
    if (!equal(tok, "=")) {
      continue;
    }

    Node *lhs = new_node(ND_VAR, NULL, NULL, tok);
    lhs->var = var;
    Node *rhs = assign(&tok, tok->next);
    Node *node = new_node(ND_ASSIGN, lhs, rhs, tok);
    cur = cur->next = new_node(ND_EXPR_STMT, node, NULL, tok);
  }

  Node *node = new_node(ND_BLOCK, NULL, NULL, tok);
  node->body = head.next;
  *rest = tok->next;  // skip ';'
  return node;
}

// compound-stmt = (declspec (type_def | declaration) | stmt)* "}"
Node *compound_stmt(Token **rest, Token *tok) {
  Node *node = new_node(ND_BLOCK, NULL, NULL, tok);

  Node head = {};
  Node *cur = &head;

  enter_scope();

  while (!equal(tok, "}")) {
    if (is_typename(tok) && !equal(tok->next, ":")) {
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
//      | "switch" "(" expr ")" stmt
//      | "case" const-expr ":" stmt
//      | "default" ":" stmt
//      | "for" "(" expr-stmt expr? ";" expr? ";" ")" stmt
//      | "while" "(" expr ")" stmt
//      | "goto" ident ";"
//      | "break" ";"
//      | "continue" ";"
//      | ident ":" stmt
//      | "{" compound-stmt
//      | expr-stmt
Node *stmt(Token **rest, Token *tok) {
  if (equal(tok, "return")) {
    Node *node = new_node(ND_RETURN, NULL, NULL, tok);
    Node *exp = expr(&tok, tok->next);
    assert(equal(tok, ";"));
    *rest = tok->next;  // skip ";"

    add_type(exp);
    node->lhs = new_cast(exp, current_fn->ty->return_ty);
    return node;
  }

  if (equal(tok, "if")) {
    Node *node = new_node(ND_IF, NULL, NULL, tok);
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

  if (equal(tok, "switch")) {
    Node *node = new_node(ND_SWITCH, NULL, NULL, tok);
    tok = tok->next;  // skip "switch"
    assert(equal(tok, "("));
    node->cond = expr(&tok, tok->next);
    assert(equal(tok, ")"));

    Node *sw = current_switch;
    current_switch = node;

    char *brk = brk_label;
    brk_label = node->brk_label = new_unique_name();

    node->then = stmt(&tok, tok->next);
    *rest = tok;

    current_switch = sw;
    brk_label = brk;
    return node;
  }

  if (equal(tok, "case")) {
    if (!current_switch) {
      error_tok(tok, "stray case");
    }
    tok = tok->next;  // skip "case"

    assert(tok->kind == TK_NUM);
    int val = const_expr(&tok, tok);
    Node *node = new_node(ND_CASE, NULL, NULL, tok);

    assert(equal(tok, ":"));
    tok = tok->next;  // skip ":"

    node->label = new_unique_name();
    node->lhs = stmt(&tok, tok);
    node->val = val;
    node->case_next = current_switch->case_next;
    current_switch->case_next = node;
    *rest = tok;
    return node;
  }

  if (equal(tok, "default")) {
    if (!current_switch) {
      error_tok(tok, "stray default");
    }
    Node *node = new_node(ND_CASE, NULL, NULL, tok);
    tok = tok->next;  // skip "default"
    assert(equal(tok, ":"));
    tok = tok->next;  // skip ":"
    node->label = new_unique_name();
    node->lhs = stmt(&tok, tok);
    current_switch->default_case = node;
    *rest = tok;
    return node;
  }

  if (equal(tok, "for")) {
    Node *node = new_node(ND_FOR, NULL, NULL, tok);
    tok = tok->next;  // skip "for"

    assert(equal(tok, "("));
    tok = tok->next;  // skip "("
    enter_scope();

    char *brk = brk_label;
    char *cont = cont_label;
    brk_label = node->brk_label = new_unique_name();
    cont_label = node->cont_label = new_unique_name();

    if (is_typename(tok)) {
      Type *basety = declspec(&tok, tok, NULL);
      node->init = declaration(&tok, tok, basety);
    } else {
      node->init = expr_stmt(&tok, tok);
    }

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
    leave_scope();
    *rest = tok;
    brk_label = brk;
    cont_label = cont;
    return node;
  }

  if (equal(tok, "while")) {
    Node *node = new_node(ND_FOR, NULL, NULL, tok);
    tok = tok->next;  // skip "while"
    assert(equal(tok, "("));
    node->cond = expr(&tok, tok->next);
    assert(equal(tok, ")"));

    char *brk = brk_label;
    char *cont = cont_label;
    brk_label = node->brk_label = new_unique_name();
    cont_label = node->cont_label = new_unique_name();
    node->then = stmt(&tok, tok->next);
    *rest = tok;
    brk_label = brk;
    cont_label = cont;
    return node;
  }

  if (equal(tok, "goto")) {
    Node *node = new_node(ND_GOTO, NULL, NULL, tok);
    tok = tok->next;  // skip "goto"
    node->label = get_ident(tok);
    node->goto_next = gotos;
    gotos = node;
    tok = tok->next;  // skip label
    assert(equal(tok, ";"));
    *rest = tok->next;  // skip ";"
    return node;
  }

  if (equal(tok, "break")) {
    if (!brk_label) {
      error_tok(tok, "stray break");
    }
    Node *node = new_node(ND_GOTO, NULL, NULL, tok);
    node->unique_label = brk_label;
    assert(equal(tok->next, ";"));
    *rest = tok = tok->next;  // skip ";"
    return node;
  }

  if (equal(tok, "continue")) {
    if (!cont_label) {
      error_tok(tok, "stray continue");
    }
    Node *node = new_node(ND_GOTO, NULL, NULL, tok);
    node->unique_label = cont_label;
    assert(equal(tok->next, ";"));
    *rest = tok = tok->next;  // skip ";"
    return node;
  }

  if (tok->kind == TK_IDENT && equal(tok->next, ":")) {
    Node *node = new_node(ND_LABEL, NULL, NULL, tok);
    node->label = strndup(tok->loc, tok->len);
    node->unique_label = new_unique_name();
    node->lhs = stmt(&tok, tok->next->next);
    *rest = tok;
    node->goto_next = labels;
    labels = node;
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
    return new_node(ND_BLOCK, NULL, NULL, tok);
  }

  Node *node = new_node(ND_EXPR_STMT, expr(&tok, tok), NULL, tok);
  assert(equal(tok, ";"));
  *rest = tok->next;  // skip ";"
  return node;
}

// expr = assign ("," expr)?
Node *expr(Token **rest, Token *tok) {
  Node *node = assign(&tok, tok);

  if (equal(tok, ",")) {
    node = new_node(ND_COMMA, node, expr(&tok, tok->next), tok);
  }

  *rest = tok;
  return node;
}

// assign = conditional (assign-op assign)?
// assign-op = "=" | "+=" | "-=" | "*=" | "/=" | "%=" | "&=" | "|=" | "^="
//           | "<<=" | ">>="
Node *assign(Token **rest, Token *tok) {
  Node *node = conditional(&tok, tok);

  if (equal(tok, "=")) {
    node = new_node(ND_ASSIGN, node, assign(&tok, tok->next), tok);
  }

  if (equal(tok, "+=")) {
    node = to_assign(new_add(node, assign(&tok, tok->next), tok));
  }

  if (equal(tok, "-=")) {
    node = to_assign(new_sub(node, assign(&tok, tok->next), tok));
  }

  if (equal(tok, "*=")) {
    node = to_assign(new_node(ND_MUL, node, assign(&tok, tok->next), tok));
  }

  if (equal(tok, "/=")) {
    node = to_assign(new_node(ND_DIV, node, assign(&tok, tok->next), tok));
  }

  if (equal(tok, "%=")) {
    node = to_assign(new_node(ND_MOD, node, assign(&tok, tok->next), tok));
  }

  if (equal(tok, "&=")) {
    node = to_assign(new_node(ND_BITAND, node, assign(&tok, tok->next), tok));
  }

  if (equal(tok, "|=")) {
    node = to_assign(new_node(ND_BITOR, node, assign(&tok, tok->next), tok));
  }

  if (equal(tok, "^=")) {
    node = to_assign(new_node(ND_BITXOR, node, assign(&tok, tok->next), tok));
  }

  if (equal(tok, "<<=")) {
    node = to_assign(new_node(ND_SHL, node, assign(&tok, tok->next), tok));
  }

  if (equal(tok, ">>=")) {
    node = to_assign(new_node(ND_SHR, node, assign(&tok, tok->next), tok));
  }

  *rest = tok;
  return node;
}

static int64_t eval(Node *node) {
  add_type(node);
  switch (node->kind) {
    case ND_ADD:
      return eval(node->lhs) + eval(node->rhs);
    case ND_SUB:
      return eval(node->lhs) - eval(node->rhs);
    case ND_MUL:
      return eval(node->lhs) * eval(node->rhs);
    case ND_DIV:
      return eval(node->lhs) / eval(node->rhs);
    case ND_NEG:
      return -eval(node->lhs);
    case ND_MOD:
      return eval(node->lhs) % eval(node->rhs);
    case ND_BITAND:
      return eval(node->lhs) & eval(node->rhs);
    case ND_BITOR:
      return eval(node->lhs) | eval(node->rhs);
    case ND_BITXOR:
      return eval(node->lhs) ^ eval(node->rhs);
    case ND_SHL:
      return eval(node->lhs) << eval(node->rhs);
    case ND_SHR:
      return eval(node->lhs) >> eval(node->rhs);
    case ND_EQ:
      return eval(node->lhs) == eval(node->rhs);
    case ND_NE:
      return eval(node->lhs) != eval(node->rhs);
    case ND_LT:
      return eval(node->lhs) < eval(node->rhs);
    case ND_LE:
      return eval(node->lhs) <= eval(node->rhs);
    case ND_COND:
      return eval(node->cond) ? eval(node->then) : eval(node->els);
    case ND_COMMA:
      return eval(node->rhs);
    case ND_NOT:
      return !eval(node->lhs);
    case ND_BITNOT:
      return ~eval(node->lhs);
    case ND_LOGAND:
      return eval(node->lhs) && eval(node->rhs);
    case ND_LOGOR:
      return eval(node->lhs) || eval(node->rhs);
    case ND_CAST:
      if (is_integer(node->ty)) {
        switch (node->ty->size) {
          case 1:
            return (int8_t)eval(node->lhs);
          case 2:
            return (int16_t)eval(node->lhs);
          case 4:
            return (int32_t)eval(node->lhs);
        }
      }
      return eval(node->lhs);
    case ND_NUM:
      return node->val;
  }
  error_tok(node->tok, "not a constant expression");
}

// const-expr = conditional
int64_t const_expr(Token **rest, Token *tok) {
  Node *node = conditional(&tok, tok);
  *rest = tok;
  return eval(node);
}

// conditional = logor ("?" expr ":" conditional)?
Node *conditional(Token **rest, Token *tok) {
  Node *cond = logor(&tok, tok);
  if (!equal(tok, "?")) {
    *rest = tok;
    return cond;
  }
  Node *node = new_node(ND_COND, NULL, NULL, tok);
  node->cond = cond;
  node->then = expr(&tok, tok->next);
  assert(equal(tok, ":"));
  node->els = conditional(&tok, tok->next);
  *rest = tok;
  return node;
}

// logor = logand ("||" logand)*
Node *logor(Token **rest, Token *tok) {
  Node *node = logand(&tok, tok);
  while (equal(tok, "||")) {
    Token *start = tok;
    node = new_node(ND_LOGOR, node, logand(&tok, tok->next), start);
  }
  *rest = tok;
  return node;
}

// logand = bitor ("&&" bitor)*
Node *logand(Token **rest, Token *tok) {
  Node *node = bitor (&tok, tok);
  while (equal(tok, "&&")) {
    Token *start = tok;
    node = new_node(ND_LOGAND, node, bitor (&tok, tok->next), start);
  }
  *rest = tok;
  return node;
}

// bitor = bitxor ("|" bitxor)*
Node * bitor (Token * *rest, Token *tok) {
  Node *node = bitxor(&tok, tok);
  while (equal(tok, "|")) {
    node = new_node(ND_BITOR, node, bitxor(&tok, tok->next), tok);
  }
  *rest = tok;
  return node;
}

// bitxor = bitand ("^" bitand)*
Node *bitxor(Token **rest, Token *tok) {
  Node *node = bitand(&tok, tok);
  while (equal(tok, "^")) {
    node = new_node(ND_BITXOR, node, bitand(&tok, tok->next), tok);
  }
  *rest = tok;
  return node;
};

// bitand = equality ("&" equality)*
Node *bitand(Token **rest, Token *tok) {
  Node *node = equality(&tok, tok);
  while (equal(tok, "&")) {
    node = new_node(ND_BITAND, node, equality(&tok, tok->next), tok);
  }
  *rest = tok;
  return node;
};

// equality = relational ("==" relational | "!=" relational)*
Node *equality(Token **rest, Token *tok) {
  Node *node = relational(&tok, tok);

  for (;;) {
    if (equal(tok, "==")) {
      node = new_node(ND_EQ, node, relational(&tok, tok->next), tok);
      continue;
    }
    if (equal(tok, "!=")) {
      node = new_node(ND_NE, relational(&tok, tok->next), node, tok);
      continue;
    }

    *rest = tok;
    return node;
  }
}

// relational = shift ("<" shift | "<=" shift | ">" shift | ">=" shift)*
Node *relational(Token **rest, Token *tok) {
  Node *node = shift(&tok, tok);

  for (;;) {
    if (equal(tok, "<")) {
      node = new_node(ND_LT, node, shift(&tok, tok->next), tok);
      continue;
    }
    if (equal(tok, ">")) {
      node = new_node(ND_LT, shift(&tok, tok->next), node, tok);
      continue;
    }
    if (equal(tok, "<=")) {
      node = new_node(ND_LE, node, shift(&tok, tok->next), tok);
      continue;
    }
    if (equal(tok, ">=")) {
      node = new_node(ND_LE, shift(&tok, tok->next), node, tok);
      continue;
    }

    *rest = tok;
    return node;
  }
}

// shift = add ("<<" add | ">>" add)*
Node *shift(Token **rest, Token *tok) {
  Node *node = add(&tok, tok);

  for (;;) {
    Token *start = tok;

    if (equal(tok, "<<")) {
      node = new_node(ND_SHL, node, add(&tok, tok->next), start);
      continue;
    }
    if (equal(tok, ">>")) {
      node = new_node(ND_SHR, node, add(&tok, tok->next), start);
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

// mul = cast ("*" cast | "/" cast | "%" cast)*
Node *mul(Token **rest, Token *tok) {
  Node *node = cast(&tok, tok);

  for (;;) {
    if (equal(tok, "*")) {
      node = new_node(ND_MUL, node, cast(&tok, tok->next), tok);
      continue;
    }
    if (equal(tok, "/")) {
      node = new_node(ND_DIV, node, cast(&tok, tok->next), tok);
      continue;
    }

    if (equal(tok, "%")) {
      node = new_node(ND_MOD, node, cast(&tok, tok->next), tok);
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
    Node *node = new_cast(lhs, ty);

    return node;
  }
  return unary(rest, tok);
}

// unary = ("+" | "-" | "&" | "*" | "!" | "~")? cast
//       | ("++" | "--") unary
//       | postfix
Node *unary(Token **rest, Token *tok) {
  if (equal(tok, "+")) {
    Node *node = cast(&tok, tok->next);
    *rest = tok;
    return node;
  }

  if (equal(tok, "-")) {
    Node *node = new_node(ND_NEG, cast(&tok, tok->next), NULL, tok);
    *rest = tok;
    return node;
  }

  if (equal(tok, "&")) {
    Node *node = new_node(ND_ADDR, cast(&tok, tok->next), NULL, tok);
    *rest = tok;
    return node;
  }

  if (equal(tok, "*")) {
    Node *node = new_node(ND_DEREF, cast(&tok, tok->next), NULL, tok);
    *rest = tok;
    return node;
  }

  if (equal(tok, "!")) {
    Node *node = new_node(ND_NOT, cast(&tok, tok->next), NULL, tok);
    *rest = tok;
    return node;
  }

  if (equal(tok, "++")) {
    Node *one = new_node(ND_NUM, NULL, NULL, tok);
    one->val = 1;
    Node *node = to_assign(new_add(unary(&tok, tok->next), one, tok));
    *rest = tok;
    return node;
  }

  if (equal(tok, "--")) {
    Node *one = new_node(ND_NUM, NULL, NULL, tok);
    one->val = 1;
    Node *node = to_assign(new_sub(unary(&tok, tok->next), one, tok));
    *rest = tok;
    return node;
  }

  if (equal(tok, "~")) {
    Node *node = new_node(ND_BITNOT, cast(&tok, tok->next), NULL, tok);
    *rest = tok;
    return node;
  }

  Node *node = postfix(&tok, tok);
  *rest = tok;
  return node;
}

// postfix = primary ("[" expr "]" | "." ident | "->" ident | "++" | "--")*
Node *postfix(Token **rest, Token *tok) {
  Node *node = primary(&tok, tok);

  for (;;) {
    if (equal(tok, "[")) {
      // x[y] is short for *(x+y)
      Token *start = tok;
      Node *idx = expr(&tok, tok->next);
      assert(equal(tok, "]"));
      tok = tok->next;  // skip "]"
      node = new_node(ND_DEREF, new_add(node, idx, start), NULL, tok);
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
      node = new_node(ND_DEREF, node, NULL, tok);
      node = struct_ref(node, tok->next);
      assert(equal(tok, "->"));
      tok = tok->next;  // skip "->"
      assert(tok->kind == TK_IDENT);
      tok = tok->next;  // skip ident
      continue;
    }

    if (equal(tok, "++")) {
      node = new_inc_dec(node, tok, 1);
      assert(equal(tok, "++"));
      tok = tok->next;  // skip "++"
      continue;
    }

    if (equal(tok, "--")) {
      node = new_inc_dec(node, tok, -1);
      assert(equal(tok, "--"));
      tok = tok->next;  // skip "--"
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
    Node *node = new_node(ND_STMT_EXPR, NULL, NULL, tok);
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
    Node *node = new_node(ND_NUM, NULL, NULL, tok);
    node->val = ty->size;
    *rest = tok;
    return node;
  }

  // "sizeof" unary
  if (equal(tok, "sizeof")) {
    Node *tmp = unary(&tok, tok->next);
    add_type(tmp);
    Node *node = new_node(ND_NUM, NULL, NULL, tok);
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
    Node *node = new_node(ND_VAR, NULL, NULL, tok);
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

  if (!sc || (!sc->var && !sc->enum_ty)) {
    error_tok(tok, "undefined variable");
  }

  Node *node;
  if (sc->var) {
    node = new_node(ND_VAR, NULL, NULL, tok);
    node->var = sc->var;
  } else {
    node = new_node(ND_NUM, NULL, NULL, tok);
    node->val = sc->enum_val;
  }

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
      arg = new_cast(arg, param_ty);
      param_ty = param_ty->next;
    }
    cur = cur->next = arg;
  }
  assert(equal(tok, ")"));
  tok = tok->next;  // skip ')'
  *rest = tok;

  Node *node = new_node(ND_FUNCALL, NULL, NULL, tok);
  node->funcname = strndup(start->loc, start->len);
  node->func_ty = ty;
  node->ty = ty->return_ty;
  node->args = head.next;
  return node;
}

// num = 1, 2, 3, ...
Node *num(Token **rest, Token *tok) {
  Node *node = new_node(ND_NUM, NULL, NULL, tok);
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
    tok = function(tok, basety, &attr);
  }
  return globals;
}
