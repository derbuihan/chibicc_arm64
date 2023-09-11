#include "chibicc.h"

static FILE *output_file;
static int depth = 0;
static int count = 1;
static char *argreg32[] = {"w0", "w1", "w2", "w3", "w4", "w5", "w6", "w7"};
static char *argreg64[] = {"x0", "x1", "x2", "x3", "x4", "x5", "x6", "x7"};

static Obj *current_fn;

static void gen_expr(Node *node);
static void gen_stmt(Node *node);

static void println(char *fmt, ...) {
  va_list ap;
  va_start(ap, fmt);
  vfprintf(output_file, fmt, ap);
  va_end(ap);
  fprintf(output_file, "\n");
}

static void push() {
  println("    str x0, [sp, -16]!");  // push
  depth++;
}

static void pop(char *arg) {
  println("    ldr %s, [sp], 16", arg);  // pop
  depth--;
}

static void load(Type *ty) {
  if (ty->kind == TY_ARRAY) {
    return;
  }
  if (ty->size == 1) {
    println("    ldrsb w0, [x0]");
  } else {
    println("    ldr x0, [x0]");
  }
}

static void store(Type *ty) {
  pop("x1");
  if (ty->size == 1) {
    println("    strb w0, [x1]");
  } else {
    println("    str x0, [x1]");
  }
}

int align_to(int n, int align) { return (n + align - 1) / align * align; }

static void gen_addr(Node *node) {
  switch (node->kind) {
    case ND_VAR:
      if (node->var->is_local) {
        // Local variable
        println("    add x0, x29, %d", node->var->offset);
      } else {
        // Global variable
        println("    adrp x0, _%s@PAGE", node->var->name);
        println("    add x0, x0, _%s@PAGEOFF", node->var->name);
      }
      return;
    case ND_DEREF:
      gen_expr(node->lhs);
      return;
    case ND_COMMA:
      gen_expr(node->lhs);
      gen_addr(node->rhs);
      return;
    case ND_MEMBER:
      gen_addr(node->lhs);
      println("    add x0, x0, %d", node->member->offset);
      return;
  }
}

void gen_expr(Node *node) {
  switch (node->kind) {
    case ND_NUM:
      println("    mov x0, %d", node->val);
      return;
    case ND_NEG:
      gen_expr(node->lhs);
      println("    neg x0, x0");
      return;
    case ND_VAR:
    case ND_MEMBER:
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
      store(node->ty);
      return;
    case ND_STMT_EXPR:
      for (Node *n = node->body; n; n = n->next) {
        gen_stmt(n);
      }
      return;
    case ND_COMMA:
      gen_expr(node->lhs);
      gen_expr(node->rhs);
      return;
    case ND_FUNCALL: {
      int nargs = 0;
      for (Node *arg = node->args; arg; arg = arg->next) {
        gen_expr(arg);
        push();
        nargs++;
      }
      for (int i = nargs - 1; i >= 0; i--) {
        pop(argreg64[i]);
      }
      println("    bl _%s", node->funcname);
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
      println("    add x0, x0, x1");
      return;
    case ND_SUB:
      println("    sub x0, x0, x1");
      return;
    case ND_MUL:
      println("    mul x0, x0, x1");
      return;
    case ND_DIV:
      println("    sdiv x0, x0, x1");
      return;
    case ND_EQ:
      println("    cmp x0, x1");
      println("    cset x0, EQ");
      return;
    case ND_NE:
      println("    cmp x0, x1");
      println("    cset x0, NE");
      return;
    case ND_LT:
      println("    cmp x0, x1");
      println("    cset x0, LT");
      return;
    case ND_LE:
      println("    cmp x0, x1");
      println("    cset x0, LE");
      return;
    default:
      break;
  }

  error_tok(node->tok, "invalid expression");
}

void gen_stmt(Node *node) {
  switch (node->kind) {
    case ND_IF: {
      int c = count++;
      gen_expr(node->cond);
      println("    cbz x0, .L.else.%d", c);
      gen_stmt(node->then);
      println("    b .L.end.%d", c);
      println(".L.else.%d:", c);
      if (node->els) {
        gen_stmt(node->els);
      }
      println(".L.end.%d:", c);
      return;
    }
    case ND_FOR: {
      int c = count++;
      if (node->init) {
        gen_stmt(node->init);
      }
      println(".L.begin.%d:", c);
      if (node->cond) {
        gen_expr(node->cond);
        println("    cbz x0, .L.end.%d", c);
      }
      gen_stmt(node->then);
      if (node->inc) {
        gen_expr(node->inc);
      }
      println("    b .L.begin.%d", c);
      println(".L.end.%d:", c);
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
      println("    b .L.return.%s", current_fn->name);
      return;
    }
    case ND_EXPR_STMT: {
      gen_expr(node->lhs);
      return;
    }
  }

  error_tok(node->tok, "invalid statement");
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
  for (Obj *var = prog; var; var = var->next) {
    if (var->is_function) {
      continue;
    }

    println("    .data");
    println("    .globl _%s", var->name);
    println("    .p2align 2");
    println("_%s:", var->name);

    if (var->init_data) {
      for (int i = 0; i < var->ty->size; i++) {
        println("    .byte %d", var->init_data[i]);
      }
    } else {
      println("    .zero %d", var->ty->size);
    }
  }
}

static void emit_text(Obj *prog) {
  for (Obj *fn = prog; fn; fn = fn->next) {
    if (!fn->is_function) {
      continue;
    }
    println("    .text");
    println("    .globl _%s", fn->name);
    println("    .p2align 2");
    println("_%s:", fn->name);
    current_fn = fn;

    println("    stp x29, x30, [sp, -16]!");
    println("    mov x29, sp");
    println("    sub sp, sp, %d", fn->stack_size);

    int i = 0;
    for (Obj *var = fn->params; var; var = var->next) {
      if (var->ty->size == 1) {
        println("    strb %s, [x29, %d]", argreg32[i++], var->offset);
      } else {
        println("    str %s, [x29, %d]", argreg64[i++], var->offset);
      }
    }

    gen_stmt(fn->body);
    assert(depth == 0);

    println(".L.return.%s:", fn->name);
    println("    mov sp, x29");
    println("    ldp x29, x30, [sp], 16");
    println("    ret");
  }
}

void code_gen(Obj *prog, FILE *out) {
  output_file = out;

  assign_lvar_offsets(prog);
  emit_data(prog);
  emit_text(prog);
}
