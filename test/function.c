#include "test.h"

int ret11() { return 11; }
int add11(int a) { return a + 11; }
int add2(int a, int b) { return a + b; }
int sub2(int a, int b) { return a - b; }
int sub3(int a, int b, int c) { return a - b - c; }
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

int main() {
  ASSERT(11, ret11());
  ASSERT(33, add11(22));
  ASSERT(33, add2(11, 22));
  ASSERT(11, sub2(22, 11));
  ASSERT(11, sub3(44, 22, 11));
  ASSERT(36, add8(1, 2, 3, 4, 5, 6, 7, 8));
  ASSERT(44, add2(11 + ret11(), 22));
  ASSERT(44, add2(11, 22 + ret11()));
  ASSERT(55, fib(9));
  ASSERT(1, ({ sub_char(7, 3, 3); }));
  ASSERT(1, ({ sub_short(7, 3, 3); }));
  ASSERT(1, ({ sub_long(7, 3, 3); }));

  return 0;
}