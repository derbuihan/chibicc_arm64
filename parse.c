#include "chibicc.h"

static Obj *locals;

static Obj *globals;

static Token *function_definition(Token *tok, Type *basety);

static Type *declspec(Token **rest, Token *tok);

static Type *declarator(Token **rest, Token *tok, Type *ty);

static Type *type_suffix(Token **rest, Token *tok, Type *ty);

static Type *func_params(Token **rest, Token *tok, Type *ty);

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

static Obj *find_var(Token *tok) {
  for (Obj *var = locals; var; var = var->next) {
    if (equal(tok, var->name)) {
      return var;
    }
  }
  return NULL;
}

static Obj *new_var(char *name, Type *ty) {
  Obj *var = calloc(1, sizeof(Obj));
  var->name = name;
  var->ty = ty;
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

static char *get_ident(Token *tok) {
  assert(tok->kind == TK_IDENT);
  return strndup(tok->loc, tok->len);
}

static void create_param_lvars(Type *param) {
  if (param) {
    create_param_lvars(param->next);
    new_lvar(get_ident(param->name), param);
  }
}

// pointer + number
static Node *new_add(Node *lhs, Node *rhs) {
  add_type(lhs);
  add_type(rhs);

  // num + num
  if (lhs->ty->kind == TY_INT && rhs->ty->kind == TY_INT) {
    return new_node(ND_ADD, lhs, rhs);
  }

  // ptr + ptr
  if (lhs->ty->base && rhs->ty->base) {
    // TODO: ERR ptr + ptr
    exit(1);
  }

  // num + ptr -> ptr + num
  if (lhs->ty->kind == TY_INT && rhs->ty->base) {
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
static Node *new_sub(Node *lhs, Node *rhs) {
  add_type(lhs);
  add_type(rhs);

  // num - num
  if (lhs->ty->kind == TY_INT && rhs->ty->kind == TY_INT) {
    return new_node(ND_SUB, lhs, rhs);
  }

  // ptr - num -> ptr
  if (lhs->ty->base && rhs->ty->kind == TY_INT) {
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

  // TODO: ERR num - ptr
  exit(1);
}

// program = (function-definition | global-variable)*
// function-definition = declspec declarator "{" compound-stmt
// declspec = "int"
// declarator = "*"* ident type-suffix
// type-suffix = "(" func-params
//             | "[" num "]" type-suffix
//             | ε
// func-params = (param ("," param)*)? ")"
// param = declspec declarator
// declaration = declspec (declarator ("=" expr)?
//                         ("," declarator ("=" expr)?)*)? ";"
// compound-stmt = (declaration | stmt)* "}"
// stmt = "return" expr ";"
//      | "if" "(" expr ")" stmt ("else" stmt)?
//      | "for" "(" expr-stmt expr? ";" expr? ";" ")" stmt
//      | "while" "(" expr ")" stmt
//      | "{" compound-stmt
//      | expr-stmt
// declspec = "int"
// expr-stmt = expr? ";"
// expr = assign
// assign = equality ("=" assign)?
// equality = relational ("==" relational | "!=" relational)*
// relational = add ("<" add | "<=" add | ">" add | ">=" add)*
// add = mul ("+" mul | "-" mul)*
// mul = unary ("*" unary | "/" unary)*
// unary = ("+" | "-" | "&" | "*") unary
//       | postfix
// postfix = primary ("[" expr "]")*
// primary = "(" expr ")"
//         | "sizeof" unary
//         | funcall
//         | ident
//         | num
// ident = 'a', ..., 'Z', 'a1', ..., 'a_1', ...
// funcall = ident "(" (assign ("," assign)*)? ")"
// num = 1, 2, 3, ...

// function-definition = declspec declarator "{" compound-stmt
Token *function_definition(Token *tok, Type *basety) {
  Type *ty = declarator(&tok, tok, basety);
  Obj *fn = new_gvar(get_ident(ty->name), ty);
  fn->is_function = true;

  locals = NULL;

  create_param_lvars(ty->params);
  fn->params = locals;

  assert(equal(tok, "{"));
  fn->body = compound_stmt(&tok, tok->next);
  fn->locals = locals;

  return tok;
}

// declspec = "int"
Type *declspec(Token **rest, Token *tok) {
  assert(equal(tok, "int"));
  *rest = tok->next;  // skip "int";
  return ty_int;
}

// declarator = "*"* ident type-suffix
Type *declarator(Token **rest, Token *tok, Type *ty) {
  while (equal(tok, "*")) {
    ty = pointer_to(ty);
    *rest = tok = tok->next;
  }

  assert(tok->kind == TK_IDENT);

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
      *rest = tok->next;  // skip ','
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
  while (!equal(tok, "}")) {
    if (equal(tok, "int")) {
      cur = cur->next = declaration(&tok, tok);
    } else {
      cur = cur->next = stmt(&tok, tok);
    }
    add_type(cur);
  }
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

// expr = assign
Node *expr(Token **rest, Token *tok) {
  Node *node = assign(&tok, tok);
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
    if (equal(tok, "+")) {
      node = new_add(node, mul(&tok, tok->next));
      continue;
    }

    if (equal(tok, "-")) {
      node = new_sub(node, mul(&tok, tok->next));
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

// unary = ("+" | "-" | "&" | "*") unary
//       | postfix
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

// postfix = primary ("[" expr "]")*
Node *postfix(Token **rest, Token *tok) {
  Node *node = primary(&tok, tok);

  while (equal(tok, "[")) {
    Node *idx = expr(&tok, tok->next);
    assert(equal(tok, "]"));
    tok = tok->next;  // skip "]"
    node = new_node(ND_DEREF, new_add(node, idx), NULL);
  }

  *rest = tok;
  return node;
}

// primary = "(" expr ")"
//         | "sizeof" unary
//         | funcall
//         | ident
//         | num
Node *primary(Token **rest, Token *tok) {
  if (equal(tok, "(")) {
    Node *node = expr(&tok, tok->next);
    assert(equal(tok, ")"));
    *rest = tok->next;  // skip ")"
    return node;
  }

  // "sizeof" unary
  if (equal(tok, "sizeof")) {
    Node *node = new_node(ND_NUM, NULL, NULL);
    node->val = unary(&tok, tok->next)->ty->size;
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
    // TODO: ERR var is not defined
    exit(1);
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

// program = (function-definition | global-variable)*
Obj *parse(Token *tok) {
  globals = NULL;
  while (tok->kind != TK_EOF) {
    Type *basety = declspec(&tok, tok);
    tok = function_definition(tok, basety);
  }
  return globals;
}
