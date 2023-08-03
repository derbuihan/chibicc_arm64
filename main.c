#include "chibicc.h"


void print_tokens(Token *tok) {
    Token *now = tok;
    while (now->kind != TK_EOF) {
        for (int i = 0; i < now->len; i++) {
            printf("(%d, %c)", now->kind, *(now->loc + i));
        }
        printf(", ");
        now = now->next;
    }
    printf("\n");
}

void print_nodes(Node *node) {
    if (node->kind == ND_NUM) {
        printf("%d ", node->val);
        return;
    }
    if (node->kind == ND_NEG) {
        printf("- ");
        print_nodes(node->lhs);
        return;
    }
    if (node->kind == ND_VAR) {
        printf("%c ", node->name);
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
        case ND_EQ:
            printf("== ");
            return;
        case ND_NE:
            printf("!= ");
            return;
        case ND_LT:
            printf("< ");
            return;
        case ND_LE:
            printf("<= ");
            return;
        case ND_ASSIGN:
            printf("= ");
            return;
        default:
            return;
    }
}

int main(int argc, char **argv) {
    char *p = argv[1];

    Token *tok = tokenizer(p);
    // print_tokens(tok);

    Node *node = parse(tok);
    //for (Node *n = node; n; n = n->next) {
    //    if (n->kind == ND_EXPR_STMT) {
    //        print_nodes(n->lhs);
    //    }
    //    printf("\n");
    //}

    code_gen(node);

    return 0;
}
