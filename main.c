#include "chibicc.h"


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

int main(int argc, char **argv) {
    char *p = argv[1];

    Token *tok = tokenizer(p);
    // print_tokens(tok);

    Node *node = parse(tok);
    // print_nodes(node);
    // printf("\n");

    code_gen(node);

    return 0;
}
