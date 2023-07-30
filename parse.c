#include "chibicc.h"

static Node *expr(Token **rest, Token *tok);
static Node *mul(Token **rest, Token *tok);
static Node *primary(Token **rest, Token *tok);
static Node *num(Token **rest, Token *tok);

Node *new_node(NodeKind kind, Node *lhs, Node *rhs) {
    Node *node = calloc(1, sizeof(Node));
    node->kind = kind;
    node->lhs = lhs;
    node->rhs = rhs;
    return node;
}

// expr = mul ("+" mul | "-" mul)*
Node *expr(Token **rest, Token *tok) {
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

// mul = primary ('*' primary | '/' primary)*
Node *mul(Token **rest, Token *tok) {
    Node *node = primary(&tok, tok);

    for(;;) {
        if (*(tok->loc) == '*') {
            node = new_node(
                    ND_MUL,
                    node,
                    primary(&tok, tok->next)
            );
            continue;
        }
        if (*(tok->loc) == '/') {
            node = new_node(
                    ND_DIV,
                    node,
                    primary(&tok, tok->next)
            );
            continue;
        }
        *rest = tok;
        return node;
    }
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


void print_nodes(Node *node) {
    if (node->kind == ND_NUM) {
        printf("%d ", node->val);
        return;
    }

    print_nodes(node->lhs);
    print_nodes(node->rhs);

    switch (node->kind) {
        case ND_ADD:
            printf("+ ");
            return;
        case ND_SUB:
            printf("- ");
            return;
        case ND_MUL:
            printf("* ");
            return;
        case ND_DIV:
            printf("/ ");
            return;
        default:
            return;
    }
}

