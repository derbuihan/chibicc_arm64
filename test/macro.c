int assert(int expected, int actual, char *code);

#include "include1.h"

#

/* */ #

int main() {
  assert(5, include1, "include1");
  assert(7, include2, "include2");

#if 0
#include "/no/such/file"
  assert(0, 1, "1");
#if nested
#endif
#endif

  int m = 0;
#if 1
  m = 5;
#endif
  assert(5, m, "m");

  return 0;
}
