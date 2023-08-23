#include "chibicc.h"

static int count = 1;
static int depth = 0;
static char *argreg[] = {"x0", "x1", "x2", "x3", "x4", "x5", "x6", "x7"};

static Obj *current_fn;

static void gen_expr(Node *node);

static void push() {
  printf("    str x0, [sp, -16]!\n");  // push
  depth++;
}

static void pop(char *arg) {
  printf("    ldr %s, [sp], 16\n", arg);  // pop
  depth--;
}

static void load(Type *ty) {
  if (ty->kind == TY_ARRAY) {
    return;
  }
  printf("    ldr x0, [x0]\n");
}

static void store(void) {
  pop("x1");
  printf("    str x0, [x1]\n");
}

static int align_to(int n, int align) {
  return (n + align - 1) / align * align;
}

static void gen_addr(Node *node) {
  switch (node->kind) {
    case ND_VAR:
      if (node->var->is_local) {
        // Local variable
        printf("    add x0, x29, %d\n", node->var->offset);
      } else {
        // Global variable
        printf("    adrp x0, _%s@PAGE\n", node->var->name);
        printf("    add x0, x0, _%s@PAGEOFF\n", node->var->name);
      }
      return;
    case ND_DEREF:
      gen_expr(node->lhs);
      return;
  }
}

void gen_expr(Node *node) {
  switch (node->kind) {
    case ND_NUM:
      printf("    mov x0, %d\n", node->val);
      return;
    case ND_NEG:
      gen_expr(node->lhs);
      printf("    neg x0, x0\n");
      return;
    case ND_VAR:
      gen_addr(node);
      load(node->ty);
      return;
    case ND_DEREF:
      gen_expr(node->lhs);
      load(node->ty);
      return;
    case ND_ADDR:
      gen_addr(node->lhs);
      return;
    case ND_ASSIGN:
      gen_addr(node->lhs);
      push();
      gen_expr(node->rhs);
      store();
      return;
    case ND_FUNCALL: {
      int nargs = 0;
      for (Node *n = node->args; n; n = n->next) {
        gen_expr(n);
        push();
        nargs++;
      }
      for (int i = nargs - 1; i >= 0; i--) {
        pop(argreg[i]);
      }
      printf("    bl _%s\n", node->funcname);
      return;
    }
    default:
      break;
  }

  gen_expr(node->rhs);
  push();
  gen_expr(node->lhs);
  pop("x1");

  switch (node->kind) {
    case ND_ADD:
      printf("    add x0, x0, x1\n");
      return;
    case ND_SUB:
      printf("    sub x0, x0, x1\n");
      return;
    case ND_MUL:
      printf("    mul x0, x0, x1\n");
      return;
    case ND_DIV:
      printf("    sdiv x0, x0, x1\n");
      return;
    case ND_EQ:
      printf("    cmp x0, x1\n");
      printf("    cset x0, EQ\n");
      return;
    case ND_NE:
      printf("    cmp x0, x1\n");
      printf("    cset x0, NE\n");
      return;
    case ND_LT:
      printf("    cmp x0, x1\n");
      printf("    cset x0, LT\n");
      return;
    case ND_LE:
      printf("    cmp x0, x1\n");
      printf("    cset x0, LE\n");
      return;
    default:
      break;
  }
}

void gen_stmt(Node *node) {
  switch (node->kind) {
    case ND_IF: {
      int c = count++;
      gen_expr(node->cond);
      printf("    cbz x0, .L.else.%d\n", c);
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
        printf("    cbz x0, .L.end.%d\n", c);
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
    case ND_RETURN: {
      gen_expr(node->lhs);
      printf("    b .L.return.%s\n", current_fn->name);
      return;
    }
    case ND_EXPR_STMT: {
      gen_expr(node->lhs);
      return;
    }
  }
}

static void assign_lvar_offsets(Obj *prog) {
  for (Obj *fn = prog; fn; fn = fn->next) {
    if (!fn->is_function) {
      continue;
    }
    int offset = 0;
    for (Obj *var = fn->locals; var; var = var->next) {
      offset += var->ty->size;
      var->offset = -offset;
    }
    fn->stack_size = align_to(offset, 16);
  }
}

static void emit_data(Obj *prog) {
  for (Obj *fn = prog; fn; fn = fn->next) {
    if (fn->is_function) {
      continue;
    }

    printf("    .data\n");
    printf("    .globl _%s\n", fn->name);
    printf("    .p2align 2\n");
    printf("_%s:\n", fn->name);
    printf("    .zero %d\n\n", fn->ty->size);
  }
}

static void emit_text(Obj *prog) {
  for (Obj *fn = prog; fn; fn = fn->next) {
    if (!fn->is_function) {
      continue;
    }
    printf("    .text\n");
    printf("    .globl _%s\n", fn->name);
    printf("    .p2align 2\n");
    printf("_%s:\n", fn->name);
    current_fn = fn;

    printf("    stp x29, x30, [sp, -16]!\n");
    printf("    mov x29, sp\n");
    printf("    sub sp, sp, %d\n", fn->stack_size);

    int i = 0;
    for (Obj *var = fn->params; var; var = var->next) {
      printf("    str %s, [x29, %d]\n", argreg[i++], var->offset);
    }

    gen_stmt(fn->body);
    assert(depth == 0);

    printf(".L.return.%s:\n", fn->name);
    printf("    mov sp, x29\n");
    printf("    ldp x29, x30, [sp], 16\n");
    printf("    ret\n\n");
  }
}

void code_gen(Obj *prog) {
  assign_lvar_offsets(prog);
  emit_data(prog);
  emit_text(prog);
}
