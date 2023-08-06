#include "chibicc.h"

static int count = 1;

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
            printf("    sub x0, x29, %d\n", node->var->offset);
            printf("    ldr w0, [x0]\n");
            return;
        case ND_ASSIGN:
            printf("    sub x0, x29, %d\n", node->lhs->var->offset);
            printf("    str x0, [sp, -16]!\n");

            gen_expr(node->rhs);
            printf("    ldr x1, [sp], 16\n");
            printf("    str w0, [x1]\n");
            return;
        default:
            break;
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
            break;
    }
}

void gen_stmt(Node *node) {
    switch (node->kind) {
        case ND_RETURN: {
            gen_expr(node->lhs);
            printf("    b .L.return\n");
            return;
        }
        case ND_IF: {
            int c = count++;
            gen_expr(node->cond);
            printf("    cbz w0, .L.else.%d\n", c);
            gen_stmt(node->then);
            printf("    b .L.end.%d\n", c);
            printf(".L.else.%d:\n", c);
            if (node->els) {
                gen_stmt(node->els);
            }
            printf(".L.end.%d:\n", c);
            return;
        }
        case ND_FOR: {
            int c = count++;
            if (node->init) {
                gen_stmt(node->init);
            }
            printf(".L.begin.%d:\n", c);
            if (node->cond) {
                gen_expr(node->cond);
                printf("    cbz w0, .L.end.%d\n", c);
            }
            gen_stmt(node->then);
            if (node->inc) {
                gen_expr(node->inc);
            }
            printf("    b .L.begin.%d\n", c);
            printf(".L.end.%d:\n", c);
            return;
        }
        case ND_BLOCK: {
            for (Node *n = node->body; n; n = n->next) {
                gen_stmt(n);
            }
            return;
        }
        case ND_EXPR_STMT: {
            gen_expr(node->lhs);
            return;
        }
    }
}

void code_gen(Function *prog) {
    int offset = 0;
    for (Obj *var = prog->locals; var; var = var->next) {
        offset += 32;
        var->offset = -offset;
    }
    prog->stack_size = (offset + 16 - 1) / 16 * 16;

    printf("    .globl _main\n");
    printf("    .p2align 2\n");
    printf("_main:\n");

    printf("    stp x29, x30, [sp, -16]!\n");
    printf("    mov x29, sp\n");
    printf("    sub sp, sp, %d\n", prog->stack_size);

    gen_stmt(prog->body);

    printf(".L.return:\n");
    printf("    mov sp, x29\n");
    printf("    ldp x29, x30, [sp], 16\n");
    printf("    ret\n");
}

