#include "chibicc.h"

static FILE *output_file;
static int depth = 0;
static int count = 1;
static char *argreg32[] = {"w0", "w1", "w2", "w3", "w4", "w5", "w6", "w7"};
static char *argreg64[] = {"x0", "x1", "x2", "x3", "x4", "x5", "x6", "x7"};
static char *argregf32[] = {"s0", "s1", "s2", "s3", "s4", "s5", "s6", "s7"};
static char *argregf64[] = {"d0", "d1", "d2", "d3", "d4", "d5", "d6", "d7"};

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

static void pushf(void) {
  println("    str d0, [sp, -16]!");  // push
  depth++;
}

static void popf(char *arg) {
  println("    ldr %s, [sp], 16", arg);  // pop
  depth--;
}

static void load(Type *ty) {
  switch (ty->kind) {
    case TY_ARRAY:
    case TY_STRUCT:
    case TY_UNION:
    case TY_FUNC:
      return;
    case TY_FLOAT:
      println("    ldr s0, [x0]");
      return;
    case TY_DOUBLE:
      println("    ldr d0, [x0]");
      return;
  }

  char *insn = ty->is_unsigned ? "ldr" : "ldrs";

  if (ty->size == 1) {
    println("    %sb w0, [x0]", insn);
  } else if (ty->size == 2) {
    println("    %sh w0, [x0]", insn);
  } else if (ty->size == 4) {
    println("    ldr w0, [x0]");
  } else {
    println("    ldr x0, [x0]");
  }
}

static void store(Type *ty) {
  pop("x1");

  switch (ty->kind) {
    case TY_STRUCT:
    case TY_UNION:
      for (int i = 0; i < ty->size; i++) {
        println("    ldrb w8, [x0, %d]", i);
        println("    strb w8, [x1, %d]", i);
      }
      return;
    case TY_FLOAT:
      println("    str s0, [x1]");
      return;
    case TY_DOUBLE:
      println("    str d0, [x1]");
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
      // Local variable
      if (node->var->is_local) {
        // Local variable
        println("    mov x17, %d", node->var->offset);
        println("    add x0, fp, x17");
        return;
      }

      // Funtion
      if (node->ty->kind == TY_FUNC) {
        if (node->var->is_definition) {
          println("    adrp x0, _%s@PAGE", node->var->name);
          println("    add x0, x0, _%s@PAGEOFF", node->var->name);
          return;
        }
      }

      // Global variable
      if (strcmp(node->var->name, "__stdoutp") == 0) {
        println("    adrp x0, ___stdoutp@GOTPAGE");
        println("    ldr x0, [x0, ___stdoutp@GOTPAGEOFF]");
        return;
      }
      if (strcmp(node->var->name, "__stdinp") == 0) {
        println("    adrp x0, ___stdinp@GOTPAGE");
        println("    ldr x0, [x0, ___stdinp@GOTPAGEOFF]");
        return;
      }
      if (strcmp(node->var->name, "__stderrp") == 0) {
        println("    adrp x0, ___stderrp@GOTPAGE");
        println("    ldr x0, [x0, ___stderrp@GOTPAGEOFF]");
        return;
      }

      println("    adrp x0, _%s@PAGE", node->var->name);
      println("    add x0, x0, _%s@PAGEOFF", node->var->name);
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
  switch (ty->kind) {
    case TY_FLOAT:
      println("    fcmp s0, 0.0");
      return;
    case TY_DOUBLE:
      println("    fcmp d0, 0.0");
      return;
  }

  if (is_integer(ty) && ty->size <= 4) {
    println("    cmp w0, 0");
  } else {
    println("    cmp x0, 0");
  }
}

static enum { I8, I16, I32, I64, U8, U16, U32, U64, F32, F64 };

static int getTypeId(Type *ty) {
  switch (ty->kind) {
    case TY_CHAR:
      return ty->is_unsigned ? U8 : I8;
    case TY_SHORT:
      return ty->is_unsigned ? U16 : I16;
    case TY_INT:
      return ty->is_unsigned ? U32 : I32;
    case TY_LONG:
      return ty->is_unsigned ? U64 : I64;
    case TY_FLOAT:
      return F32;
    case TY_DOUBLE:
      return F64;
  }
  return U64;
}

static char i32i8[] = "sxtb w0, w0";
static char i32u8[] = "uxtb w0, w0";
static char i32i16[] = "sxth w0, w0";
static char i32u16[] = "uxth w0, w0";
static char i32f32[] = "scvtf s0, w0";
static char i32i64[] = "sxtw x0, w0";
static char i32f64[] = "scvtf d0, w0";

static char u32f32[] = "ucvtf s0, w0";
static char u32i64[] = "uxtw x0, w0";
static char u32f64[] = "ucvtf d0, w0";

static char i64f32[] = "scvtf s0, x0";
static char i64f64[] = "scvtf d0, x0";

static char u64f32[] = "ucvtf s0, x0";
static char u64f64[] = "ucvtf d0, x0";

static char f32i8[] = "fcvtzs w0, s0\n    sxtb w0, w0";
static char f32u8[] = "fcvtzs w0, s0\n    uxtb w0, w0";
static char f32i16[] = "fcvtzs w0, s0\n    sxth w0, w0";
static char f32u16[] = "fcvtzs w0, s0\n    uxth w0, w0";
static char f32i32[] = "fcvtzs w0, s0";
static char f32u32[] = "fcvtzu w0, s0";
static char f32i64[] = "fcvtzs x0, s0";
static char f32u64[] = "fcvtzu x0, s0";
static char f32f64[] = "fcvt d0, s0";

static char f64i8[] = "fcvtzs w0, d0\n    sxtb w0, w0";
static char f64u8[] = "fcvtzs w0, d0\n    uxtb w0, w0";
static char f64i16[] = "fcvtzs w0, d0\n    sxth w0, w0";
static char f64u16[] = "fcvtzs w0, d0\n    uxth w0, w0";
static char f64i32[] = "fcvtzs w0, d0";
static char f64u32[] = "fcvtzu w0, d0";
static char f64f32[] = "fcvt s0, d0\n    fcvtzs w0, s0";
static char f64i64[] = "fcvtzs x0, d0";
static char f64u64[] = "fcvtzu x0, d0";

static char *cast_table[][10] = {
    // i8   i16     i32     i64     u8     u16     u32     u64     f32     f64
    {NULL, NULL, NULL, i32i64, i32u8, i32u16, NULL, i32i64, i32f32,
     i32f64},  // i8
    {i32i8, NULL, NULL, i32i64, i32u8, i32u16, NULL, i32i64, i32f32,
     i32f64},  // i16
    {i32i8, i32i16, NULL, i32i64, i32u8, i32u16, NULL, i32i64, i32f32,
     i32f64},  // i32
    {i32i8, i32i16, NULL, NULL, i32u8, i32u16, NULL, NULL, i64f32,
     i64f64},  // i64

    {i32i8, NULL, NULL, i32i64, NULL, NULL, NULL, i32i64, i32f32,
     i32f64},  // u8
    {i32i8, i32i16, NULL, i32i64, i32u8, NULL, NULL, i32i64, i32f32,
     i32f64},  // u16
    {i32i8, i32i16, NULL, u32i64, i32u8, i32u16, NULL, u32i64, u32f32,
     u32f64},  // u32
    {i32i8, i32i16, NULL, NULL, i32u8, i32u16, NULL, NULL, u64f32,
     u64f64},  // u64

    {f32i8, f32i16, f32i32, f32i64, f32u8, f32u16, f32u32, f32u64, NULL,
     f32f64},  // f32
    {f64i8, f64i16, f64i32, f64i64, f64u8, f64u16, f64u32, f64u64, f64f32,
     NULL},  // f64
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

static int push_vargs(Node *args, int offset) {
  if (!args) {
    int align = align_to(offset, 16);
    println("    sub sp, sp, %d", align);
    return align;
  }

  int align = push_vargs(args->next, offset + 8);
  gen_expr(args);

  if (is_flonum(args->ty)) {
    println("    str d0, [sp, #%d]", offset);
  } else {
    println("    str x0, [sp, #%d]", offset);
  }

  return align;
}

static int arrange_args(Node *args, int carg, int const_nargs) {
  // expr varargs
  if (carg >= const_nargs) {
    int offset = 0;
    int align = push_vargs(args, offset);
    return align;
  }

  // expr args
  int align = arrange_args(args->next, carg + 1, const_nargs);
  gen_expr(args);
  if (is_flonum(args->ty)) {
    pushf();
  } else {
    push();
  }
  return align;
}

static void gen_num64(int64_t val) {
  println("    mov x0, %#lx", val & 0xFFFF);
  for (int i = 1; i < 4; i++) {
    uint16_t v = (val >> 16 * i) & 0xFFFF;
    if (v) {
      println("    movk x0, %#x, lsl %d", v, 16 * i);
    }
  }
  return;
}

static void gen_num32(int32_t val) {
  println("    mov w0, %#x", val & 0xFFFF);
  uint16_t v = (val >> 16) & 0xFFFF;
  if (v) {
    println("    movk w0, %#x, lsl 16", v);
  }
  return;
}

void gen_expr(Node *node) {
  switch (node->kind) {
    case ND_NULL_EXPR:
      println("; gen_expr: ND_NULL_EXPR");
      return;
    case ND_NUM:
      println("; gen_expr: ND_NUM");
      switch (node->ty->kind) {
        case TY_FLOAT: {
          float f = node->fval;
          int32_t val = *(int32_t *)&f;
          gen_num32(val);
          println("    fmov s0, w0");
          return;
        }
        case TY_DOUBLE: {
          double d = node->fval;
          int64_t val = *(int64_t *)&d;
          gen_num64(val);
          println("    fmov d0, x0");
          return;
        }
        case TY_LONG: {
          int64_t val = node->val;
          gen_num64(val);
          return;
        }
        default: {
          int32_t val = node->val;
          gen_num32(val);
          return;
        }
      }
    case ND_NEG:
      println("; gen_expr: ND_NEG");
      gen_expr(node->lhs);
      switch (node->ty->kind) {
        case TY_FLOAT:
          println("    fneg s0, s0");
          return;
        case TY_DOUBLE:
          println("    fneg d0, d0");
          return;
        default:
          println("    neg x0, x0");
          return;
      }
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
      println("    mov x17, %d", node->var->offset);
      println("    add x0, fp, x17");
      println("    mov x1, 0");
      println("    mov x2, %d", node->var->ty->size);
      println("    bl _memset");
      return;
    }
    case ND_COND: {
      println("; gen_expr: ND_COND");
      int c = count++;
      gen_expr(node->cond);
      cmp_zero(node->cond->ty);
      println("    beq .L.else.%d", c);
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
      cmp_zero(node->lhs->ty);
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
      cmp_zero(node->lhs->ty);
      println("    beq .L.is_false.%d", c);
      gen_expr(node->rhs);
      cmp_zero(node->rhs->ty);
      println("    beq .L.is_false.%d", c);
      println("    mov x0, 1");
      println("    b .L.end.%d", c);
      println(".L.is_false.%d:", c);
      println("    mov x0, 0");
      println(".L.end.%d:", c);
      return;
    }
    case ND_LOGOR: {
      println("; gen_expr: ND_LOGOR");
      int c = count++;
      gen_expr(node->lhs);
      cmp_zero(node->lhs->ty);
      println("    bne .L.is_true.%d", c);
      gen_expr(node->rhs);
      cmp_zero(node->rhs->ty);
      println("    bne .L.is_true.%d", c);
      println("    mov x0, 0");
      println("    b .L.end.%d", c);
      println(".L.is_true.%d:", c);
      println("    mov x0, 1");
      println(".L.end.%d:", c);
      return;
    }
    case ND_FUNCALL: {
      println("; gen_expr: ND_FUNCALL");

      int const_nargs = 0;
      for (Type *param = node->func_ty->params; param; param = param->next) {
        const_nargs++;
      }

      int carg = 0;
      int align = arrange_args(node->args, carg, const_nargs);

      Node *func = node->lhs;
      if (func->ty->kind != TY_FUNC) {
        gen_expr(func);
        println("    mov x8, x0");
      }

      int gp = 0, fp = 0;
      for (Node *arg = node->args; arg; arg = arg->next) {
        if (gp + fp >= const_nargs) {
          break;
        }
        if (is_flonum(arg->ty)) {
          popf(argregf64[fp++]);
        } else {
          pop(argreg64[gp++]);
        }
      }

      if (func->ty->kind != TY_FUNC) {
        println("    blr x8");
      } else {
        println("    bl _%s", func->var->name);
      }

      println("    add sp, sp, %d", align);

      switch (node->ty->kind) {
        case TY_BOOL:
          println("  uxtb w0, w0");
          return;
        case TY_CHAR:
          if (node->ty->is_unsigned) {
            println("  uxtb w0, w0");
          } else {
            println("  sxtb w0, w0");
          }
          return;
        case TY_SHORT:
          if (node->ty->is_unsigned) {
            println("  uxth w0, w0");
          } else {
            println("  sxth w0, w0");
          }
          return;
      }

      return;
    }
    default:
      break;
  }

  if (is_flonum(node->lhs->ty)) {
    gen_expr(node->rhs);
    pushf();
    gen_expr(node->lhs);
    popf("d1");

    char *r0, *r1;
    if (node->lhs->ty->kind == TY_FLOAT) {
      r0 = "s0";
      r1 = "s1";
    } else {
      r0 = "d0";
      r1 = "d1";
    }

    switch (node->kind) {
      case ND_ADD:
        println("; gen_expr: ND_ADD");
        println("    fadd %s, %s, %s", r0, r0, r1);
        return;
      case ND_SUB:
        println("; gen_expr: ND_SUB");
        println("    fsub %s, %s, %s", r0, r0, r1);
        return;
      case ND_MUL:
        println("; gen_expr: ND_MUL");
        println("    fmul %s, %s, %s", r0, r0, r1);
        return;
      case ND_DIV:
        println("; gen_expr: ND_DIV");
        println("    fdiv %s, %s, %s", r0, r0, r1);
        return;
      case ND_EQ:
      case ND_NE:
      case ND_LT:
      case ND_LE:
        println("; gen_expr: ND_EQ, ND_NE, ND_LT, ND_LE");
        println("    fcmp %s, %s", r0, r1);
        switch (node->kind) {
          case ND_EQ:
            println("    cset w0, EQ");
            return;
          case ND_NE:
            println("    cset w0, NE");
            return;
          case ND_LT:
            println("    cset w0, LT");
            return;
          case ND_LE:
            println("    cset w0, LE");
            return;
        }
        return;
    }

    error_tok(node->tok, "invalid expression");
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
      if (node->lhs->ty->is_unsigned) {
        println("    udiv %s, %s, %s", r0, r0, r1);
      } else {
        println("    sdiv %s, %s, %s", r0, r0, r1);
      }
      return;
    case ND_MOD:
      println("; gen_expr: ND_MOD");
      if (node->lhs->ty->is_unsigned) {
        println("    udiv %s, %s, %s", r9, r0, r1);
      } else {
        println("    sdiv %s, %s, %s", r9, r0, r1);
      }
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
      switch (node->kind) {
        case ND_EQ:
          println("    cset %s, EQ", r0);
          return;
        case ND_NE:
          println("    cset %s, NE", r0);
          return;
        case ND_LT:
          if (node->lhs->ty->is_unsigned) {
            println("    cset %s, LO", r0);
          } else {
            println("    cset %s, LT", r0);
          }
          return;
        case ND_LE:
          if (node->lhs->ty->is_unsigned) {
            println("    cset %s, LS", r0);
          } else {
            println("    cset %s, LE", r0);
          }
          return;
      }
    case ND_SHL:
      println("; gen_expr: ND_SHL");
      println("    lsl %s, %s, %s", r0, r0, r1);
      return;
    case ND_SHR:
      println("; gen_expr: ND_SHR");
      if (node->lhs->ty->is_unsigned) {
        println("    lsr %s, %s, %s", r0, r0, r1);
      } else {
        println("    asr %s, %s, %s", r0, r0, r1);
      }
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
      cmp_zero(node->cond->ty);
      println("    beq .L.else.%d", c);
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
        cmp_zero(node->cond->ty);
        println("    beq %s", node->brk_label);
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
    case ND_DO: {
      println("; gen_stmt: ND_DO");
      int c = count++;
      println(".L.begin.%d:", c);
      gen_stmt(node->then);
      println("%s:", node->cont_label);
      gen_expr(node->cond);
      cmp_zero(node->cond->ty);
      println("    bne .L.begin.%d", c);
      println("%s:", node->brk_label);
      return;
    }
    case ND_SWITCH:
      println("; gen_stmt: ND_SWITCH");
      gen_expr(node->cond);
      push();
      pop("x1");
      for (Node *n = node->case_next; n; n = n->case_next) {
        if (node->cond->ty->size == 8) {
          int64_t val = n->val;
          gen_num64(val);
          println("    cmp x0, x1");
        } else {
          int32_t val = n->val;
          gen_num32(val);
          println("    cmp w0, w1");
        }
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
      if (node->lhs) {
        gen_expr(node->lhs);
      }
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
      offset = align_to(offset, var->align);
      var->offset = -offset;
    }
    fn->stack_size = align_to(offset, 16);
  }
}

static void emit_data(Obj *prog) {
  for (Obj *var = prog; var; var = var->next) {
    if (var->is_function || !var->is_definition) {
      continue;
    }

    if (!var->is_static) {
      println("    .globl _%s", var->name);
    }

    if (var->init_data) {
      println("    .data");
      println("    .p2align %d", var->align);
      println("_%s:", var->name);

      Relocation *rel = var->rel;
      int pos = 0;
      while (pos < var->ty->size) {
        if (rel && rel->offset == pos) {
          println("    .quad _%s%+ld", rel->label, rel->addend);
          rel = rel->next;
          pos += 8;
        } else {
          println("    .byte %d", var->init_data[pos++]);
        }
      }
      continue;
    }
    println("    .zerofill __DATA,__bss,_%s,%d,%d", var->name, var->ty->size,
            var->align);
  }
}

static void store_fp(int r, int offset, int sz) {
  switch (sz) {
    case 4:
      println("    str %s, [fp, %d]", argregf32[r], offset);
      return;
    case 8:
      println("    str %s, [fp, %d]", argregf64[r], offset);
      return;
  }
  unreachable();
}

static void store_gp(int r, int offset, int sz) {
  println("    mov x17, %d", offset);
  println("    add x17, fp, x17");
  switch (sz) {
    case 1:
      println("    strb %s, [x17]", argreg32[r]);
      return;
    case 2:
      println("    strh %s, [x17]", argreg32[r]);
      return;
    case 4:
      println("    str %s, [x17]", argreg32[r]);
      return;
    case 8:
      println("    str %s, [x17]", argreg64[r]);
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

    println("; prologue _%s", fn->name);
    println("    stp fp, lr, [sp, -16]!");
    println("    mov fp, sp");
    println("    mov x17, %d", fn->stack_size);
    println("    sub sp, sp, x17");

    if (fn->va_area) {
      println("; pram: va_area");
      int off = fn->va_area->offset;
      println("    mov x17, %d", off);
      println("    add x17, fp, x17");
      println("    mov x18, fp");
      println("    add x18, x18, 16");
      println("    str x18, [x17]");
    }

    int gp = 0, fp = 0;
    for (Obj *var = fn->params; var; var = var->next) {
      println("; param: %s", var->name);
      if (is_flonum(var->ty)) {
        store_fp(fp++, var->offset, var->ty->size);
      } else {
        store_gp(gp++, var->offset, var->ty->size);
      }
    }

    gen_stmt(fn->body);
    assert(depth == 0);

    println("; epilogue %s", fn->name);
    println(".L.return.%s:", fn->name);
    println("    mov sp, fp");
    println("    ldp fp, lr, [sp], 16");
    println("    ret");
  }
}

void code_gen(Obj *prog, FILE *out) {
  output_file = out;

  assign_lvar_offsets(prog);
  emit_data(prog);
  emit_text(prog);
}
