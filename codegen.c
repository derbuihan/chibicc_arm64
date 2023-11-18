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
  if (ty->kind == TY_ARRAY || ty->kind == TY_STRUCT || ty->kind == TY_UNION) {
    return;
  }
  if (ty->size == 1) {
    println("    ldrsb w0, [x0]");
  } else if (ty->size == 2) {
    println("    ldrsh w0, [x0]");
  } else if (ty->size == 4) {
    println("    ldr w0, [x0]");
  } else {
    println("    ldr x0, [x0]");
  }
}

static void store(Type *ty) {
  pop("x1");

  if (ty->kind == TY_STRUCT || ty->kind == TY_UNION) {
    for (int i = 0; i < ty->size; i++) {
      println("    ldrb w8, [x0, %d]", i);
      println("    strb w8, [x1, %d]", i);
    }
    return;
  }

  if (ty->size == 1) {
    println("    strb w0, [x1]");
  } else if (ty->size == 2) {
    println("    strh w0, [x1]");
  } else if (ty->size == 4) {
    println("    str w0, [x1]");
  } else {
    println("    str x0, [x1]");
  }
}

int align_to(int n, int align) { return (n + align - 1) / align * align; }

static void gen_addr(Node *node) {
  switch (node->kind) {
    case ND_VAR:
      println("; gen_addr: ND_VAR");
      if (node->var->is_local) {
        // Local variable
        println("    add x0, x29, %d", node->var->offset);
      } else {
        // Global variable
        println("    adrp x0, %s@PAGE", node->var->name);
        println("    add x0, x0, %s@PAGEOFF", node->var->name);
      }
      return;
    case ND_DEREF:
      println("; gen_addr: ND_DEREF");
      gen_expr(node->lhs);
      return;
    case ND_COMMA:
      println("; gen_addr: ND_COMMA");
      gen_expr(node->lhs);
      gen_addr(node->rhs);
      return;
    case ND_MEMBER:
      println("; gen_addr: ND_MEMBER");
      gen_addr(node->lhs);
      println("    add x0, x0, %d", node->member->offset);
      return;
  }
}

static void cmp_zero(Type *ty) {
  if (is_integer(ty) && ty->size <= 4) {
    println("    cmp w0, #0");
  } else {
    println("    cmp x0, #0");
  }
}

static enum { I8, I16, I32, I64 };

static int getTypeId(Type *ty) {
  switch (ty->kind) {
    case TY_CHAR:
      return I8;
    case TY_SHORT:
      return I16;
    case TY_INT:
      return I32;
  }
  return I64;
}

static char i32i8[] = "sxtb w0, w0";
static char i32i16[] = "sxth w0, w0";
static char i32i64[] = "sxtw x0, w0";

static char *cast_table[][4] = {
    {NULL, NULL, NULL, i32i64},
    {i32i8, NULL, NULL, i32i64},
    {i32i8, i32i16, NULL, i32i64},
    {i32i8, i32i16, NULL, NULL},
};

static void cast(Type *from, Type *to) {
  if (to->kind == TY_VOID) {
    return;
  }
  if (to->kind == TY_BOOL) {
    cmp_zero(from);
    println("    cset w0, NE");
  }
  int t1 = getTypeId(from);
  int t2 = getTypeId(to);
  if (cast_table[t1][t2]) {
    println("    %s", cast_table[t1][t2]);
  }
}

void gen_expr(Node *node) {
  switch (node->kind) {
    case ND_NULL_EXPR:
      println("; gen_expr: ND_NULL_EXPR");
      return;
    case ND_NUM:
      println("; gen_expr: ND_NUM");
      if (node->val > 281474976710655) {
        println("    mov x0, %d", node->val & 0xFFFF);
        println("    movk x0, %d, lsl #16", (node->val & 0xFFFF0000) >> 16);
        println("    movk x0, %d, lsl #32", (node->val & 0xFFFF00000000) >> 32);
        println("    movk x0, %d, lsl #48",
                (node->val & 0xFFFF000000000000) >> 48);
        return;
      }
      if (node->val > 4294967295) {
        println("    mov x0, %d", node->val & 0xFFFF);
        println("    movk x0, %d, lsl #16", (node->val & 0xFFFF0000) >> 16);
        println("    movk x0, %d, lsl #32", (node->val & 0xFFFF00000000) >> 32);
        return;
      }
      if (node->val > 65535) {
        println("    mov x0, %d", node->val & 0xFFFF);
        println("    movk x0, %d, lsl #16", (node->val & 0xFFFF0000) >> 16);
        return;
      }
      println("    mov x0, %d", node->val);
      return;
    case ND_NEG:
      println("; gen_expr: ND_NEG");
      gen_expr(node->lhs);
      println("    neg x0, x0");
      return;
    case ND_VAR:
    case ND_MEMBER:
      println("; gen_expr: ND_VAR, ND_MEMBER");
      gen_addr(node);
      load(node->ty);
      return;
    case ND_DEREF:
      println("; gen_expr: ND_DEREF");
      gen_expr(node->lhs);
      load(node->ty);
      return;
    case ND_ADDR:
      println("; gen_expr: ND_ADDR");
      gen_addr(node->lhs);
      return;
    case ND_ASSIGN:
      println("; gen_expr: ND_ASSIGN");
      gen_addr(node->lhs);
      push();
      gen_expr(node->rhs);
      store(node->ty);
      return;
    case ND_STMT_EXPR:
      println("; gen_expr: ND_STMT_EXPR");
      for (Node *n = node->body; n; n = n->next) {
        gen_stmt(n);
      }
      return;
    case ND_COMMA:
      println("; gen_expr: ND_COMMA");
      gen_expr(node->lhs);
      gen_expr(node->rhs);
      return;
    case ND_CAST:
      println("; gen_expr: ND_CAST");
      gen_expr(node->lhs);
      cast(node->lhs->ty, node->ty);
      return;
    case ND_MEMZERO: {
      println("; gen_expr: ND_MEMZERO");
      int c = count++;
      println("    mov x1, %d", node->var->ty->size);
      println("    add x0, x29, %d", node->var->offset);
      println(".L.memset.loop.%d:", c);
      println("    cmp x1, 0");
      println("    beq .L.memset.end.%d", c);
      println("    strb wzr, [x0]");
      println("    add x0, x0, 1");
      println("    sub x1, x1, 1");
      println("    b .L.memset.loop.%d", c);
      println(".L.memset.end.%d:", c);
      return;
    }
    case ND_COND: {
      println("; gen_expr: ND_COND");
      int c = count++;
      gen_expr(node->cond);
      println("    cbz x0, .L.else.%d", c);
      gen_expr(node->then);
      println("    b .L.end.%d", c);
      println(".L.else.%d:", c);
      gen_expr(node->els);
      println(".L.end.%d:", c);
      return;
    }
    case ND_NOT:
      println("; gen_expr: ND_NOT");
      gen_expr(node->lhs);
      println("    cmp x0, 0");
      println("    cset x0, EQ");
      return;
    case ND_BITNOT:
      println("; gen_expr: ND_BITNOT");
      gen_expr(node->lhs);
      println("    mvn x0, x0");
      return;
    case ND_LOGAND: {
      println("; gen_expr: ND_LOGAND");
      int c = count++;
      gen_expr(node->lhs);
      println("    cmp x0, 0");
      println("    beq .L.false.%d", c);
      gen_expr(node->rhs);
      println("    cmp x0, 0");
      println("    beq .L.false.%d", c);
      println("    mov x0, 1");
      println("    b .L.end.%d", c);
      println(".L.false.%d:", c);
      println("    mov x0, 0");
      println(".L.end.%d:", c);
      return;
    }
    case ND_LOGOR: {
      println("; gen_expr: ND_LOGOR");
      int c = count++;
      gen_expr(node->lhs);
      println("    cmp x0, 0");
      println("    bne .L.true.%d", c);
      gen_expr(node->rhs);
      println("    cmp x0, 0");
      println("    bne .L.true.%d", c);
      println("    mov x0, 0");
      println("    b .L.end.%d", c);
      println(".L.true.%d:", c);
      println("    mov x0, 1");
      println(".L.end.%d:", c);
      return;
    }
    case ND_FUNCALL: {
      println("; gen_expr: ND_FUNCALL");
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

  char *r0, *r1, *r9;
  if (node->lhs->ty->kind == TY_LONG || node->lhs->ty->base) {
    r0 = "x0";
    r1 = "x1";
    r9 = "x9";
  } else {
    r0 = "w0";
    r1 = "w1";
    r9 = "w9";
  }

  switch (node->kind) {
    case ND_ADD:
      println("; gen_expr: ND_ADD");
      println("    add %s, %s, %s", r0, r0, r1);
      return;
    case ND_SUB:
      println("; gen_expr: ND_SUB");
      println("    sub %s, %s, %s", r0, r0, r1);
      return;
    case ND_MUL:
      println("; gen_expr: ND_MUL");
      println("    mul %s, %s, %s", r0, r0, r1);
      return;
    case ND_DIV:
      println("; gen_expr: ND_DIV");
      println("    sdiv %s, %s, %s", r0, r0, r1);
      return;
    case ND_MOD:
      println("; gen_expr: ND_MOD");
      println("    sdiv %s, %s, %s", r9, r0, r1);
      println("    msub %s, %s, %s, %s", r0, r9, r1, r0);
      return;
    case ND_BITAND:
      println("; gen_expr: ND_BITAND");
      println("    and %s, %s, %s", r0, r0, r1);
      return;
    case ND_BITOR:
      println("; gen_expr: ND_BITOR");
      println("    orr %s, %s, %s", r0, r0, r1);
      return;
    case ND_BITXOR:
      println("; gen_expr: ND_BITXOR");
      println("    eor %s, %s, %s", r0, r0, r1);
      return;
    case ND_EQ:
    case ND_NE:
    case ND_LT:
    case ND_LE:
      println("; gen_expr: ND_EQ, ND_NE, ND_LT, ND_LE");
      println("    cmp %s, %s", r0, r1);
      if (node->kind == ND_EQ) {
        println("    cset %s, EQ", r0);
      } else if (node->kind == ND_NE) {
        println("    cset %s, NE", r0);
      } else if (node->kind == ND_LT) {
        println("    cset %s, LT", r0);
      } else if (node->kind == ND_LE) {
        println("    cset %s, LE", r0);
      }
      return;
    case ND_SHL:
      println("; gen_expr: ND_SHL");
      println("    lsl %s, %s, %s", r0, r0, r1);
      return;
    case ND_SHR:
      println("; gen_expr: ND_SHR");
      println("    asr %s, %s, %s", r0, r0, r1);
      return;
    default:
      break;
  }

  error_tok(node->tok, "invalid expression");
}

void gen_stmt(Node *node) {
  switch (node->kind) {
    case ND_IF: {
      println("; gen_stmt: ND_IF");
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
      println("; gen_stmt: ND_FOR");
      int c = count++;
      if (node->init) {
        gen_stmt(node->init);
      }
      println(".L.begin.%d:", c);
      if (node->cond) {
        gen_expr(node->cond);
        println("    cbz x0, %s", node->brk_label);
      }
      gen_stmt(node->then);
      println("%s:", node->cont_label);
      if (node->inc) {
        gen_expr(node->inc);
      }
      println("    b .L.begin.%d", c);
      println("%s:", node->brk_label);
      return;
    }
    case ND_SWITCH:
      println("; gen_stmt: ND_SWITCH");
      gen_expr(node->cond);
      for (Node *n = node->case_next; n; n = n->case_next) {
        char *reg = (node->cond->ty->size == 8) ? "x0" : "w0";
        println("    cmp %s, %ld", reg, n->val);
        println("    b.eq %s", n->label);
      }
      if (node->default_case) {
        println("    b %s", node->default_case->label);
      }
      println("    b %s", node->brk_label);
      gen_stmt(node->then);
      println("%s:", node->brk_label);
      return;
    case ND_CASE:
      println("; gen_stmt: ND_CASE");
      println("%s:", node->label);
      gen_stmt(node->lhs);
      return;
    case ND_BLOCK: {
      println("; gen_stmt: ND_BLOCK");
      for (Node *n = node->body; n; n = n->next) {
        gen_stmt(n);
      }
      return;
    }
    case ND_GOTO:
      println("; gen_stmt: ND_GOTO");
      println("    b %s", node->unique_label);
      return;
    case ND_LABEL:
      println("; gen_stmt: ND_LABEL");
      println("%s:", node->unique_label);
      gen_stmt(node->lhs);
      return;
    case ND_RETURN: {
      println("; gen_stmt: ND_RETURN");
      gen_expr(node->lhs);
      println("    b .L.return.%s", current_fn->name);
      return;
    }
    case ND_EXPR_STMT: {
      println("; gen_stmt: ND_EXPR_STMT");
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
      offset = align_to(offset, var->ty->align);
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
    println("    .globl %s", var->name);
    println("    .p2align 2");
    println("%s:", var->name);

    if (var->init_data) {
      Relocation *rel = var->rel;
      int pos = 0;
      while (pos < var->ty->size) {
        if (rel && rel->offset == pos) {
          println("    .quad %s%+ld", rel->label, rel->addend);
          rel = rel->next;
          pos += 8;
        } else {
          println("    .byte %d", var->init_data[pos++]);
        }
      }

    } else {
      println("    .zero %d", var->ty->size);
    }
  }
}

static void store_gp(int r, int offset, int sz) {
  switch (sz) {
    case 1:
      println("    strb %s, [x29, %d]", argreg32[r], offset);
      return;
    case 2:
      println("    strh %s, [x29, %d]", argreg32[r], offset);
      return;
    case 4:
      println("    str %s, [x29, %d]", argreg32[r], offset);
      return;
    case 8:
      println("    str %s, [x29, %d]", argreg64[r], offset);
      return;
  }
  unreachable();
}

static void emit_text(Obj *prog) {
  for (Obj *fn = prog; fn; fn = fn->next) {
    if (!fn->is_function || !fn->is_definition) {
      continue;
    }
    println("    .text");
    if (!fn->is_static) {
      println("    .globl _%s", fn->name);
    }
    println("    .p2align 2");
    println("_%s:", fn->name);
    current_fn = fn;

    println("; prologue %s", fn->name);
    println("    stp x29, x30, [sp, -16]!");
    println("    mov x29, sp");
    println("    sub sp, sp, %d", fn->stack_size);

    int i = 0;
    for (Obj *var = fn->params; var; var = var->next) {
      store_gp(i++, var->offset, var->ty->size);
    }

    gen_stmt(fn->body);
    assert(depth == 0);

    println("; epilogue %s", fn->name);
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
