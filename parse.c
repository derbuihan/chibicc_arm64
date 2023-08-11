#include "chibicc.h"

Obj *locals;

static Node *program(Token **rest, Token *tok);

static Node *stmt(Token **rest, Token *tok);

static Node *compound_stmt(Token **rest, Token *tok);

static Node *declaration(Token **rest, Token *tok);

static Type *declarator(Token **rest, Token *tok, Type *ty);

static Type *declspec(Token **rest, Token *tok);

static Node *expr_stmt(Token **rest, Token *tok);

static Node *expr(Token **rest, Token *tok);

static Node *assign(Token **rest, Token *tok);

static Node *equality(Token **rest, Token *tok);

static Node *relational(Token **rest, Token *tok);

static Node *add(Token **rest, Token *tok);

static Node *mul(Token **rest, Token *tok);

static Node *unary(Token **rest, Token *tok);

static Node *primary(Token **rest, Token *tok);

static Node *ident(Token **rest, Token *tok);

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
        if (strlen(var->name) == tok->len && !strncmp(tok->loc, var->name, tok->len)) {
            return var;
        }
    }
    return NULL;
}

static Obj *new_lvar(char *name, Type *ty) {
    Obj *var = calloc(1, sizeof(Obj));
    var->name = name;
    var->ty = ty;
    var->next = locals;
    locals = var;
    return var;
}

// program = stmt*
// stmt = "return" expr ";"
//      | "if" "(" expr ")" stmt ("else" stmt)?
//      | "for" "(" expr-stmt expr? ";" expr? ";" ")" stmt
//      | "while" "(" expr ")" stmt
//      | "{" compound-stmt
//      | expr-stmt
// compound-stmt = (declaration | stmt)* "}"
// declaration = declspec (declarator ("=" expr)? ("," declarator ("=" expr)?)*)? ";"
// declarator = "*"* ident
// declspec = "int"
// expr-stmt = expr? ";"
// expr = assign
// assign = equality ("=" assign)?
// equality = relational ("==" relational | "!=" relational)*
// relational = add ("<" add | "<=" add | ">" add | ">=" add)*
// add = mul ("+" mul | "-" mul)*
// mul = unary ("*" unary | "/" unary)*
// unary = ("+" | "-" | "&" | "*") unary | primary
// primary = "(" expr ")" | ident | num
// ident = 'a', ..., 'Z', 'a1', ..., 'a_1', ...
// num = 1, 2, 3, ...

// program = stmt*
Node *program(Token **rest, Token *tok) {
    Node head = {};
    Node *cur = &head;
    while (tok->kind != TK_EOF) {
        cur->next = stmt(&tok, tok);
        cur = cur->next;
    }
    return head.next;
}

// stmt = "return" expr ";"
//      | "if" "(" expr ")" stmt ("else" stmt)?
//      | "for" "(" expr-stmt expr? ";" expr? ";" ")" stmt
//      | "while" "(" expr ")" stmt
//      | "{" compound-stmt
//      | expr-stmt
Node *stmt(Token **rest, Token *tok) {
    if (tok->len ==6 && !memcmp(tok->loc, "return", tok->len)){
        Node *node = new_node(ND_RETURN, expr(&tok, tok->next), NULL);
        assert(*tok->loc == ';');
        *rest = tok->next; // skip ";"
        return node;
    }

    if (tok->len == 2 && !memcmp(tok->loc, "if", 2)) {
        Node *node = new_node(ND_IF, NULL, NULL);
        tok = tok->next; // skip "if"

        assert(*tok->loc == '(');
        node->cond = expr(&tok, tok->next);

        assert(*tok->loc == ')');
        node->then = stmt(&tok, tok->next);

        if (tok->len == 4 && !memcmp(tok->loc, "else", 4)) {
            node->els = stmt(&tok, tok->next);
        }

        *rest = tok;
        return node;
    }

    if (tok->len == 3 && !memcmp(tok->loc, "for", 3)) {
        Node *node = new_node(ND_FOR, NULL, NULL);
        tok = tok->next; // skip "for"

        assert(*tok->loc == '(');
        node->init = expr_stmt(&tok, tok->next);

        if (*tok->loc != ';') {
            node->cond = expr(&tok, tok);
        }
        assert(*tok->loc == ';');
        tok = tok->next;

        if (*tok->loc != ')') {
            node->inc = expr(&tok, tok);
        }
        assert(*tok->loc == ')');

        node->then = stmt(&tok, tok->next);

        *rest = tok;
        return node;
    }

    if (tok->len == 5 && !memcmp(tok->loc, "while", 5)) {
        Node *node = new_node(ND_FOR, NULL, NULL);
        tok = tok->next; // skip "while"

        assert(*tok->loc == '(');
        node->cond = expr(&tok, tok->next);

        assert(*tok->loc == ')');
        node ->then = stmt(&tok, tok->next);

        *rest = tok;
        return node;
    }

    if (tok->len == 1 && *tok->loc == '{') {
        Node *node = compound_stmt(&tok, tok->next);
        *rest = tok;
        return node;
    }

    Node *node = expr_stmt(&tok, tok);
    *rest = tok;
    return node;
}

// compound-stmt = (declaration | stmt)* "}"
Node *compound_stmt(Token **rest, Token *tok) {
    Node *node = new_node(ND_BLOCK, NULL, NULL);

    Node head = {};
    Node *cur = &head;
    while (tok->len != 1 || *tok->loc != '}') {
        if (tok->len == 3 && memcmp(tok->loc, "int", tok->len) == 0) {
            cur->next = declaration(&tok, tok);
        } else {
            cur->next = stmt(&tok, tok);
        }
        cur = cur->next;
        add_type(cur);
    }
    node->body = head.next;
    assert(*tok->loc == '}');
    *rest = tok->next;
    return node;
}

// declaration = declspec (declarator ("=" expr)? ("," declarator ("=" expr)?)*)? ";"
Node *declaration(Token **rest, Token *tok){
    Type *basety = declspec(&tok, tok);

    Node head = {};
    Node *cur = &head;
    int i = 0;

    while (*tok->loc != ';' || tok->len != 1) {
        if (i++ > 0) {
            assert(*tok->loc == ',');
            *rest = tok->next; // skip ','
        }
        Type *ty = declarator(&tok, tok, basety);
        assert(ty->name->kind == TK_IDENT);

        Obj *var = new_lvar(strndup(ty->name->loc, ty->name->len), ty);
        if (*tok->loc != '=' || tok->len != 1) {
            continue;
        }

        Node *lhs = new_node(ND_VAR, NULL, NULL);
        lhs->var = var;
        Node *rhs = assign(&tok, tok->next);
        Node *node = new_node(ND_ASSIGN, lhs, rhs);
        cur->next = new_node(ND_EXPR_STMT, node, NULL);
        cur = cur->next;
    }

    Node *node = new_node(ND_BLOCK, NULL, NULL);
    node->body = head.next;
    *rest = tok->next; // skip ';'
    return node;
}

// declarator = "*"* ident
Type *declarator(Token **rest, Token *tok, Type *ty) {
    while (tok->len == 1 && *tok->loc == '*') {
        ty = pointer_to(ty);
        tok = tok->next;
        *rest = tok;
    }

    assert(tok->kind == TK_IDENT);

    ty->name = tok;
    *rest = tok->next;
    return ty;
}

// declspec = "int"
Type *declspec(Token **rest, Token *tok) {
    assert(memcmp(tok->loc, "int", tok->len) == 0);
    *rest = tok->next; // skip "int";
    return ty_int;
}

// expr-stmt = expr? ";"
Node *expr_stmt(Token **rest, Token *tok) {
    if (tok->len == 1 && *tok->loc == ';') {
        *rest = tok->next;
        return new_node(ND_BLOCK, NULL, NULL);
    }

    Node *node = new_node( ND_EXPR_STMT, expr(&tok, tok), NULL);
    assert(*tok->loc == ';');
    *rest = tok->next; // skip ";"
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

    if (tok->len == 1 && *tok->loc == '=') {
        node = new_node(
                ND_ASSIGN,
                node,
                assign(&tok, tok->next)
        );
    }

    *rest = tok;
    return node;
}

// equality = relational ("==" relational | "!=" relational)*
Node *equality(Token **rest, Token *tok) {
    Node *node = relational(&tok, tok);

    for (;;) {
        if (tok->len == 2 && !memcmp(tok->loc, "==", 2)) {
            node = new_node(
                    ND_EQ,
                    node,
                    relational(&tok, tok->next)
            );
            continue;
        }
        if (tok->len == 2 && !memcmp(tok->loc, "!=", 2)) {
            node = new_node(
                    ND_NE,
                    relational(&tok, tok->next),
                    node
            );
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
        if (tok->len == 1 && *tok->loc == '<') {
            node = new_node(
                    ND_LT,
                    node,
                    add(&tok, tok->next)
            );
            continue;
        }
        if (tok->len == 1 && *tok->loc == '>') {
            node = new_node(
                    ND_LT,
                    add(&tok, tok->next),
                    node
            );
            continue;
        }
        if (tok->len == 2 && !memcmp(tok->loc, "<=", 2)) {
            node = new_node(
                    ND_LE,
                    node,
                    add(&tok, tok->next)
            );
            continue;
        }
        if (tok->len == 2 && !memcmp(tok->loc, ">=", 2)) {
            node = new_node(
                    ND_LE,
                    add(&tok, tok->next),
                    node
            );
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
        if (tok->len == 1 && *tok->loc == '+') {
            Node *lhs = node;
            Node *rhs = mul(&tok, tok->next);
            add_type(lhs);
            add_type(rhs);

            // num + num
            if (lhs->ty->kind == TY_INT && rhs->ty->kind == TY_INT) {
                node = new_node(ND_ADD, lhs, rhs);
                continue;
            }

            // num + ptr -> ptr + num
            if (lhs->ty->kind == TY_INT && rhs->ty->base) {
                Node *tmp = lhs;
                lhs = rhs;
                rhs = tmp;
            }

            // ptr + num
            if (lhs->ty->base && rhs->ty->kind == TY_INT) {
                Node *num_node = new_node( ND_NUM, NULL, NULL );
                num_node->val = 16;
                node = new_node(ND_ADD,
                                lhs,
                                new_node( ND_MUL, rhs, num_node ));
                continue;
            }

            // TODO: ERR ptr + ptr

            continue;
        }

        if (tok->len == 1 && *tok->loc == '-') {
            Node *lhs = node;
            Node *rhs = mul(&tok, tok->next);
            add_type(lhs);
            add_type(rhs);

            // num - num
            if (lhs->ty->kind == TY_INT && rhs->ty->kind == TY_INT) {
                node = new_node(ND_SUB, lhs, rhs);
                continue;
            }

            // ptr - num = num
            if (lhs->ty->base && rhs->ty->kind == TY_INT) {
                Node *num_node = new_node( ND_NUM, NULL, NULL );
                num_node->val = 16;
                node = new_node(ND_SUB,
                                lhs,
                                new_node(ND_MUL, rhs, num_node));
                continue;
            }

            // ptr - ptr = num
            if (lhs->ty->base && rhs->ty->base) {
                Node *num_node = new_node( ND_NUM, NULL, NULL );
                num_node->val = 16;
                node = new_node(ND_DIV,
                                new_node( ND_SUB, lhs, rhs ),
                                num_node);
                continue;
            }

            // TODO: ERR num - ptr

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
        if (tok->len == 1 && *tok->loc == '*') {
            node = new_node(
                    ND_MUL,
                    node,
                    unary(&tok, tok->next)
            );
            continue;
        }
        if (tok->len == 1 && *tok->loc == '/') {
            node = new_node(
                    ND_DIV,
                    node,
                    unary(&tok, tok->next)
            );
            continue;
        }

        *rest = tok;
        return node;
    }
}

// unary = ("+" | "-" | "&" | "*") unary | primary
Node *unary(Token **rest, Token *tok) {
    if (tok->len == 1 && *tok->loc == '+') {
        Node *node = unary(&tok, tok->next);
        *rest = tok;
        return node;
    }

    if (tok->len == 1 && *tok->loc == '-') {
        Node *node = new_node(
                ND_NEG,
                unary(&tok, tok->next),
                NULL
        );
        *rest = tok;
        return node;
    }

    if (tok->len == 1 && *tok->loc == '&') {
        Node *node = new_node(
                ND_ADDR,
                unary(&tok, tok->next),
                NULL
        );
        *rest = tok;
        return node;
    }

    if (tok->len == 1 && *tok->loc == '*') {
        Node *node = new_node(
                ND_DEREF,
                unary(&tok, tok->next),
                NULL
        );
        *rest = tok;
        return node;
    }

    Node *node = primary(&tok, tok);
    *rest = tok;
    return node;
}

// primary = "(" expr ")" | ident | num
Node *primary(Token **rest, Token *tok) {
    if (tok->len == 1 && *tok->loc == '(') {
        Node *node = expr(&tok, tok->next);
        assert(*tok->loc == ')');
        *rest = tok->next; // skip ")"
        return node;
    }

    if (tok->kind == TK_IDENT) {
        Node *node = ident(&tok, tok);
        *rest = tok;
        return node;
    }

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

    if(!var) {
        // TODO: ERR var is not defined
    }

    Node *node = new_node(ND_VAR, NULL, NULL);
    node->var = var;

    *rest = tok->next;
    return node;
}

// num = 1, 2, 3, ...
Node *num(Token **rest, Token *tok) {
    Node *node = new_node(ND_NUM, NULL, NULL);
    node->val = tok->val;

    *rest = tok->next;
    return node;
}

Function *parse(Token *tok) {
    Node *node = program(&tok, tok);

    Function *prog = calloc(1, sizeof(Function));
    prog->body = node;
    prog->locals = locals;

    return prog;
}
