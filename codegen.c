#include "chibicc.h"


void gen_expr(Node *node) {
    switch (node->kind) {
        case ND_NUM:
            printf("    mov w0, %d\n", node->val);
            return;
        case ND_NEG:
            gen_expr(node->lhs);
            printf("    neg w0, w0\n");
            return;
        case ND_VAR:
            printf("    sub x0, x29, %d\n", (node->name - 'a' + 1) * 16);
            printf("    ldr w0, [x0]\n");
            return;
        case ND_ASSIGN:
            printf("    sub x0, x29, %d\n", (node->lhs->name - 'a' + 1) * 16);
            printf("    str x0, [sp, -16]!\n");

            gen_expr(node->rhs);
            printf("    ldr x1, [sp], 16\n");
            printf("    str w0, [x1]\n");
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

void code_gen(Node *node) {
    printf("    .globl _main\n");
    printf("    .p2align 2\n");
    printf("_main:\n");

    printf("    stp x29, x30, [sp, -16]!\n");
    printf("    mov x29, sp\n");
    printf("    sub sp, sp, 416\n");
    for (Node *n = node; n; n = n->next) {
        if (n->kind == ND_EXPR_STMT) {
            gen_expr(n->lhs);
        }
    }

    printf("    mov sp, x29\n");
    printf("    ldp x29, x30, [sp], 16\n");
    printf("    ret\n");
}

