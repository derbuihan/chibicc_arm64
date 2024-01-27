#include "test.h"

int ret11(void) { return 11; }
int add11(int a) { return a + 11; }
int add2(int a, int b) { return a + b; }
int sub2(int a, int b) { return a - b; }
int sub3(int a, int b, int c) { return a - b - c; }
int add7(int a1, int a2, int a3, int a4, int a5, int a6, int a7) {
  return a1 + a2 + a3 + a4 + a5 + a6 + a7;
}
int add8(int a1, int a2, int a3, int a4, int a5, int a6, int a7, int a8) {
  return a1 + a2 + a3 + a4 + a5 + a6 + a7 + a8;
}
int sub_char(char x, char y, char z) { return x - y - z; }
int sub_short(short x, short y, short z) { return x - y - z; }
int sub_long(long x, long y, long z) { return x - y - z; }
int fib(int x) {
  if (x <= 1) return 1;
  return fib(x - 1) + fib(x - 2);
}
int gcd(int x, int y) {
  if (y == 0) return x;
  return gcd(y, x % y);
}
int g1;
int *g1_ptr(void) { return &g1; }
char int_to_char(int x) { return x; }
int div_long(long x, long y) { return x / y; }
_Bool bool_fn_add(_Bool x) { return x + 1; }
_Bool bool_fn_sub(_Bool x) { return x - 1; }
static int static_fn(void) { return 3; }
int param_decay(int x[]) { return x[0]; }

int counter() {
  static int i;
  static int j = 1 + 1;
  return i++ + j++;
}

void ret_none() { return; }

_Bool true_fn();
_Bool false_fn();
char char_fn();
short short_fn();

unsigned char uchar_fn();
unsigned short ushort_fn();

char schar_fn();
short sshort_fn();

int add_all(int n, ...);
double add_all2(int n, ...);

typedef void va_list;
int sprintf(char *buf, char *fmt, ...);
int vsprintf(char *buf, char *fmt, va_list ap);
char *fmt(char *buf, char *fmt, ...) {
  va_list *ap;
  ap = __va_area__;
  vsprintf(buf, fmt, ap);
}

float add_float(float a, float b);
double add_double(double a, double b);

float add_float3(float x, float y, float z) { return x + y + z; }
double add_double3(double x, double y, double z) { return x + y + z; }

int (*fnptr(int (*fn)(int n, ...)))(int, ...) { return fn; }
int param_decay2(int x()) { return x(); }

char *func_fn(void) { return __func__; }

char *function_fn(void) { return __FUNCTION__; }

int main() {
  ASSERT(11, ret11());
  ASSERT(33, add11(22));
  ASSERT(33, add2(11, 22));
  ASSERT(11, sub2(22, 11));
  ASSERT(11, sub3(44, 22, 11));
  ASSERT(28, add7(1, 2, 3, 4, 5, 6, 7));
  ASSERT(36, add8(1, 2, 3, 4, 5, 6, 7, 8));
  ASSERT(44, add2(11 + ret11(), 22));
  ASSERT(44, add2(11, 22 + ret11()));
  ASSERT(55, fib(9));
  ASSERT(3, gcd(21, 30));
  ASSERT(1, ({ sub_char(7, 3, 3); }));
  ASSERT(2, ({ sub_char(7, 2, 3); }));
  ASSERT(1, ({ sub_short(7, 3, 3); }));
  ASSERT(1, ({ sub_long(7, 3, 3); }));

  g1 = 3;

  ASSERT(3, *g1_ptr());
  ASSERT(5, int_to_char(261));
  ASSERT(5, int_to_char(261));
  ASSERT(-5, div_long(-10, 2));

  ASSERT(1, bool_fn_add(3));
  ASSERT(0, bool_fn_sub(3));
  ASSERT(1, bool_fn_add(-3));
  ASSERT(0, bool_fn_sub(-3));
  ASSERT(1, bool_fn_add(0));
  ASSERT(1, bool_fn_sub(0));

  ASSERT(3, static_fn());

  ASSERT(3, ({
           int x[2];
           x[0] = 3;
           param_decay(x);
         }));

  ASSERT(2, counter());
  ASSERT(4, counter());
  ASSERT(6, counter());

  ret_none();

  ASSERT(1, true_fn());
  ASSERT(0, false_fn());
  ASSERT(3, char_fn());
  ASSERT(5, short_fn());

  ASSERT(6, add_all(3, 1, 2, 3));
  ASSERT(5, add_all(4, 1, 2, 3, -1));
  ASSERT(1, add_all2(3, 1.5, 2.5, 3.5) == 7.5);

  {
    char buf[100];
    fmt(buf, "%d %d %s", 1, 2, "foo");
    printf("%s\n", buf);
  }

  ASSERT(0, ({
           char buf[100];
           sprintf(buf, "%d %d %s", 1, 2, "foo");
           strcmp("1 2 foo", buf);
         }));

  ASSERT(0, ({
           char buf[100];
           fmt(buf, "%d %d %s", 1, 2, "foo");
           strcmp("1 2 foo", buf);
         }));

  ASSERT(251, uchar_fn());
  ASSERT(65528, ushort_fn());
  ASSERT(-5, schar_fn());
  ASSERT(-8, sshort_fn());

  ASSERT(6, add_float(2.3, 3.8));
  ASSERT(6, add_double(2.3, 3.8));

  ASSERT(7, add_float3(2.5, 2.5, 2.5));
  ASSERT(7, add_double3(2.5, 2.5, 2.5));

  ASSERT(0, ({
           char buf[100];
           sprintf(buf, "%.1f", (float)3.5);
           strcmp("3.5", buf);
         }));

  ASSERT(5, (add2)(2, 3));
  ASSERT(5, (&add2)(2, 3));
  ASSERT(7, ({
           int (*fn)(int, int) = add2;
           fn(2, 5);
         }));
  ASSERT(6, fnptr(add_all)(3, 1, 2, 3));

  ASSERT(11, param_decay2(ret11));

  ASSERT(5, sizeof(__func__));
  ASSERT(0, strcmp("main", __func__));
  ASSERT(0, strcmp("func_fn", func_fn()));
  ASSERT(0, strcmp("main", __FUNCTION__));
  ASSERT(0, strcmp("function_fn", function_fn()));

  printf("OK\n");

  return 0;
}