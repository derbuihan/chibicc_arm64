#include "test.h"

int main() {
  ASSERT(1, 1);
  ASSERT(2, (2));
  ASSERT(3, 2 + 1);
  ASSERT(6, 3 + 2 + 1);
  ASSERT(10, 4 + 3 + 2 + 1);
  ASSERT(15, 1 + 2 + 3 + 4 + 5);
  ASSERT(5, 10 - 2 - 3);
  ASSERT(7, 1 + 2 * 3);
  ASSERT(5, 1 * 2 + 3);
  ASSERT(4, 12 / 3);
  ASSERT(3, 1 + 4 * 3 / 6);

  ASSERT(9, (1 + 2) * 3);
  ASSERT(15, (1 + 2 + 3) * (2 + 4 - 3) - 3 + (12 + 18) / 3 - 10);
  ASSERT(84, 12 * (3 + 4));

  ASSERT(2, +2);
  ASSERT(2, +- -2);
  ASSERT(1, 4 + -2 - +1);

  ASSERT(0, 0 == 1);
  ASSERT(1, 42 == 42);
  ASSERT(1, 0 != 1);
  ASSERT(0, 42 != 42);

  ASSERT(1, 0 < 1);
  ASSERT(0, 1 < 1);
  ASSERT(0, 2 < 1);
  ASSERT(1, 0 <= 1);
  ASSERT(1, 1 <= 1);
  ASSERT(0, 2 <= 1);

  ASSERT(1, 1 > 0);
  ASSERT(0, 1 > 1);
  ASSERT(0, 1 > 2);
  ASSERT(1, 1 >= 0);
  ASSERT(1, 1 >= 1);
  ASSERT(0, 1 >= 2);

  ASSERT(0, 1073741824 * 100 / 100);

  return 0;
}