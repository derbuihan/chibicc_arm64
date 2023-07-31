#include "chibicc.h"


static Node *expr(Token **rest, Token *tok);

static Node *equality(Token **rest, Token *tok);

static Node *relational(Token **rest, Token *tok);

static Node *add(Token **rest, Token *tok);

static Node *mul(Token **rest, Token *tok);

static Node *unary(Token **rest, Token *tok);

static Node *primary(Token **rest, Token *tok);

static Node *num(Token **rest, Token *tok);

Node *new_node(NodeKind kind, Node *lhs, Node *rhs) {
    Node *node = calloc(1, sizeof(Node));
    node->kind = kind;
    node->lhs = lhs;
    node->rhs = rhs;
    return node;
}

// expr = equality
// equality = relational ("==" relational | "!=" relational)*
// relational = add ("<" add | "<=" add | ">" add | ">=" add)*
// add = mul ("+" mul | "-" mul)*
// mul = unary ('*' unary | '/' unary)*
// unary = ('+' | '-')? primary
// primary = num | '(' expr ')'
// num = 1, 2, 3, ...

// expr = equality
Node *expr(Token **rest, Token *tok) {
    Node *node = equality(rest, tok);
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

// mul = unary ('*' unary | '/' unary)*
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

// unary = ('+' | '-')? primary
Node *unary(Token **rest, Token *tok) {
    if (*(tok->loc) == '+') {
        return unary(rest, tok->next);
    }
    if (*(tok->loc) == '-') {
        return new_node(
                ND_NEG,
                unary(rest, tok->next),
                NULL
        );
    }
    return primary(rest, tok);
}

// primary = num | '(' expr ')'
Node *primary(Token **rest, Token *tok) {
    if (tok->kind == TK_NUM) {
        Node *node = num(rest, tok);
        return node;
    }

    if (*(tok->loc) == '(') {
        Node *node = expr(&tok, tok->next);
        *rest = tok->next;
        return node;
    }
}

// num = 1, 2, 3, ...
Node *num(Token **rest, Token *tok) {
    Node *node = new_node(ND_NUM, NULL, NULL);
    node->val = tok->val;

    *rest = tok->next;
    return node;
}

Node *parse(Token *tok) {
    Node *node = expr(&tok, tok);
    return node;
}
