#include "chibicc.h"


void gen_expr(Node *node) {
    if (node->kind == ND_NUM) {
        printf("    mov w0, %d\n", node->val);
        return;
    }
    if (node->kind == ND_NEG) {
        gen_expr(node->lhs);
        printf("    neg w0, w0\n");
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
        case ND_EQ:
            printf("    cmp w0, w1\n");
            printf("    cset w0, EQ\n");
            return;
        case ND_NE:
            printf("    cmp w0, w1\n");
            printf("    cset w0, NE\n");
            return;
        case ND_LT:
            printf("    cmp w0, w1\n");
            printf("    cset w0, LT\n");
            return;
        case ND_LE:
            printf("    cmp w0, w1\n");
            printf("    cset w0, LE\n");
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

    gen_expr(node);

    printf("    ret\n");
}

