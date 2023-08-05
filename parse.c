#include "chibicc.h"

Obj *locals;

static Node *program(Token **rest, Token *tok);

static Node *stmt(Token **rest, Token *tok);

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

// program = stmt*
// stmt = "return" expr ";" | expr-stmt
// expr-stmt = expr ";"
// expr = assign
// assign = equality ("=" assign)?
// equality = relational ("==" relational | "!=" relational)*
// relational = add ("<" add | "<=" add | ">" add | ">=" add)*
// add = mul ("+" mul | "-" mul)*
// mul = unary ("*" unary | "/" unary)*
// unary = ("+" | "-") unary | primary
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

// stmt = "return" expr ";" | expr-stmt
Node *stmt(Token **rest, Token *tok) {
    if (!memcmp(tok->loc, "return", 6)) {
        Node *node = new_node(ND_RETURN, expr(&tok, tok->next), NULL);
        assert(*tok->loc == ';');
        *rest = tok->next; // skip ";"
        return node;
    }

    Node *node = expr_stmt(&tok, tok);
    *rest = tok;
    return node;
}

// expr-stmt = expr ";"
Node *expr_stmt(Token **rest, Token *tok) {
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

    if (tok->len == 1 && *(tok->loc) == '=') {
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
        if (tok->len == 2 && *(tok->loc) == '=' && *(tok->loc + 1) == '=') {
            node = new_node(
                    ND_EQ,
                    node,
                    relational(&tok, tok->next)
            );
            continue;
        }
        if (tok->len == 2 && *(tok->loc) == '!' && *(tok->loc + 1) == '=') {
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
        if (tok->len == 1 && *(tok->loc) == '<') {
            node = new_node(
                    ND_LT,
                    node,
                    add(&tok, tok->next)
            );
            continue;
        }
        if (tok->len == 1 && *(tok->loc) == '>') {
            node = new_node(
                    ND_LT,
                    add(&tok, tok->next),
                    node
            );
            continue;
        }
        if (tok->len == 2 && *(tok->loc) == '<' && *(tok->loc + 1) == '=') {
            node = new_node(
                    ND_LE,
                    node,
                    add(&tok, tok->next)
            );
            continue;
        }
        if (tok->len == 2 && *(tok->loc) == '>' && *(tok->loc + 1) == '=') {
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
        if (*(tok->loc) == '+') {
            node = new_node(
                    ND_ADD,
                    node,
                    mul(&tok, tok->next)
            );
            continue;
        }
        if (*(tok->loc) == '-') {
            node = new_node(
                    ND_SUB,
                    node,
                    mul(&tok, tok->next)
            );
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
        if (*(tok->loc) == '*') {
            node = new_node(
                    ND_MUL,
                    node,
                    unary(&tok, tok->next)
            );
            continue;
        }
        if (*(tok->loc) == '/') {
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

// unary = ("+" | "-") unary | primary
Node *unary(Token **rest, Token *tok) {
    if (*(tok->loc) == '+') {
        Node *node = unary(&tok, tok->next);
        *rest = tok;
        return node;
    }

    if (*(tok->loc) == '-') {
        Node *node = new_node(
                ND_NEG,
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
    if (*(tok->loc) == '(') {
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
    Obj *var;
    for (var = locals; var; var = var->next) {
        if (strlen(var->name) == tok->len && !strncmp(var->name, tok->loc, tok->len)) {
            break;
        }
    }

    // create var if not found
    if(!var) {
        var = calloc(1, sizeof(Obj));
        var->name = strndup(tok->loc, tok->len);
        var->next = locals;
        locals = var;
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
