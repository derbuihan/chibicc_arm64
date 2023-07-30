#include <stdio.h>
#include <ctype.h>
#include <stdlib.h>


// tokenizer

typedef enum {
    TK_PUNCT,
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

Token *new_token(TokenKind kind, char *start, char *end) {
    Token *tok = calloc(1, sizeof(Token));
    tok->kind = kind;
    tok->loc = start;
    tok->len = end - start;
    return tok;
}

Token *tokenizer(char *p) {
    Token head = {};
    Token *cur = &head;

    while (*p) {
        if (isspace(*p)) {
            p++;
            continue;
        }

        if (isdigit(*p)) {
            char *q = p;
            int val = strtol(p, &p, 10);
            Token *tok = new_token(TK_NUM, q, p);
            tok->val = val;
            cur->next = tok;
            cur = cur->next;
            continue;
        }

        if (ispunct(*p)) {
            char *q = p;
            p++;
            Token *tok = new_token(TK_PUNCT, q, p);
            cur->next = tok;
            cur = cur->next;
            continue;
        }
    }
    Token *tok = new_token(TK_EOF, p, p);
    cur->next = tok;
    return head.next;
}

void print_tokens(Token *tok) {
    Token *now = tok;
    while (now->kind != TK_EOF) {
        for (int i = 0; i < now->len; i++) {
            printf("%c", *(now->loc + i));
        }
        printf(", ");
        now = now->next;
    }
    printf("\n");
}

// parser

typedef enum {
    ND_ADD,
    ND_SUB,
    ND_MUL,
    ND_DIV,
    ND_NUM,
} NodeKind;

typedef struct Node Node;
struct Node {
    NodeKind kind;
    Node *lhs;
    Node *rhs;
    int val;
};

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

// code generator

void gen_expr(Node *node) {
    if (node->kind == ND_NUM) {
        printf("    mov w0, %d\n", node->val);
        return;
    }

    gen_expr(node->rhs);
    printf("    str w0, [sp, -16]!\n"); // push
    gen_expr(node->lhs);
    printf("    ldr w1, [sp], 16\n"); // pop

    switch (node->kind) {
        case ND_ADD:
            printf("    add w0, w0, w1\n");
            return;
        case ND_SUB:
            printf("    sub w0, w0, w1\n");
            return;
        case ND_MUL:
            printf("    mul w0, w0, w1\n");
            return;
        case ND_DIV:
            printf("    sdiv w0, w0, w1\n");
            return;
        default:
            return;
    }
}

void gen_expr2(Node *node) {
    if (node->kind == ND_NUM) {
        printf("    mov w0, %d\n", node->val);
        printf("    str w0, [sp, -16]!\n"); // push
        return;
    }

    gen_expr2(node->lhs);
    gen_expr2(node->rhs);
    printf("    ldr w1, [sp], 16\n"); // pop
    printf("    ldr w0, [sp], 16\n"); // pop

    switch (node->kind) {
        case ND_ADD:
            printf("    add w0, w0, w1\n");
            break;
        case ND_SUB:
            printf("    sub w0, w0, w1\n");
            break;
        case ND_MUL:
            printf("    mul w0, w0, w1\n");
            break;
        case ND_DIV:
            printf("    sdiv w0, w0, w1\n");
            break;
        default:
            return;
    }
    printf("    str w0, [sp, -16]!\n"); // push
}

void code_gen(Node *node) {
    printf("    .globl _main\n");
    printf("    .p2align 2\n");
    printf("_main:\n");

    gen_expr2(node);

    printf("    ret\n");

}


int main(int argc, char **argv) {
    char *p = argv[1];

    Token *tok = tokenizer(p);
    //print_tokens(tok);

    Node *node = parse(tok);
    //print_nodes(node);

    code_gen(node);

    return 0;
}
