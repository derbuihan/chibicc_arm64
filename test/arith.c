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

  ASSERT(7, ({
           int i = 2;
           i += 5;
           i;
         }));
  ASSERT(7, ({
           int i = 2;
           i += 5;
         }));
  ASSERT(3, ({
           int i = 5;
           i -= 2;
           i;
         }));
  ASSERT(3, ({
           int i = 5;
           i -= 2;
         }));
  ASSERT(6, ({
           int i = 3;
           i *= 2;
           i;
         }));
  ASSERT(6, ({
           int i = 3;
           i *= 2;
         }));
  ASSERT(3, ({
           int i = 6;
           i /= 2;
           i;
         }));
  ASSERT(3, ({
           int i = 6;
           i /= 2;
         }));

  ASSERT(3, ({
           int i = 2;
           ++i;
         }));
  ASSERT(2, ({
           int a[3];
           a[0] = 0;
           a[1] = 1;
           a[2] = 2;
           int *p = a + 1;
           ++*p;
         }));
  ASSERT(0, ({
           int a[3];
           a[0] = 0;
           a[1] = 1;
           a[2] = 2;
           int *p = a + 1;
           --*p;
         }));

  ASSERT(2, ({
           int i = 2;
           i++;
         }));
  ASSERT(2, ({
           int i = 2;
           i--;
         }));
  ASSERT(3, ({
           int i = 2;
           i++;
           i;
         }));
  ASSERT(1, ({
           int i = 2;
           i--;
           i;
         }));
  ASSERT(1, ({
           int a[3];
           a[0] = 0;
           a[1] = 1;
           a[2] = 2;
           int *p = a + 1;
           *p++;
         }));
  ASSERT(1, ({
           int a[3];
           a[0] = 0;
           a[1] = 1;
           a[2] = 2;
           int *p = a + 1;
           *p--;
         }));

  ASSERT(0, ({
           int a[3];
           a[0] = 0;
           a[1] = 1;
           a[2] = 2;
           int *p = a + 1;
           (*p++)--;
           a[0];
         }));
  ASSERT(0, ({
           int a[3];
           a[0] = 0;
           a[1] = 1;
           a[2] = 2;
           int *p = a + 1;
           (*(p--))--;
           a[1];
         }));
  ASSERT(2, ({
           int a[3];
           a[0] = 0;
           a[1] = 1;
           a[2] = 2;
           int *p = a + 1;
           (*p)--;
           a[2];
         }));
  ASSERT(2, ({
           int a[3];
           a[0] = 0;
           a[1] = 1;
           a[2] = 2;
           int *p = a + 1;
           (*p)--;
           p++;
           *p;
         }));

  ASSERT(0, ({
           int a[3];
           a[0] = 0;
           a[1] = 1;
           a[2] = 2;
           int *p = a + 1;
           (*p++)--;
           a[0];
         }));
  ASSERT(0, ({
           int a[3];
           a[0] = 0;
           a[1] = 1;
           a[2] = 2;
           int *p = a + 1;
           (*p++)--;
           a[1];
         }));
  ASSERT(2, ({
           int a[3];
           a[0] = 0;
           a[1] = 1;
           a[2] = 2;
           int *p = a + 1;
           (*p++)--;
           a[2];
         }));
  ASSERT(2, ({
           int a[3];
           a[0] = 0;
           a[1] = 1;
           a[2] = 2;
           int *p = a + 1;
           (*p++)--;
           *p;
         }));

  ASSERT(0, !1);
  ASSERT(0, !2);
  ASSERT(1, !0);
  ASSERT(1, !(char)0);
  ASSERT(0, !(long)3);
  ASSERT(4, sizeof(!(char)0));
  ASSERT(4, sizeof(!(long)0));

  ASSERT(-1, ~0);
  ASSERT(0, ~-1);

  ASSERT(5, 17 % 6);
  ASSERT(5, ((long)17) % 6);
  ASSERT(2, ({
           int i = 10;
           i %= 4;
           i;
         }));
  ASSERT(2, ({
           long i = 10;
           i %= 4;
           i;
         }));

  ASSERT(0, 0 & 1);
  ASSERT(1, 3 & 1);
  ASSERT(3, 7 & 3);
  ASSERT(10, -1 & 10);

  ASSERT(1, 0 | 1);
  ASSERT(0b10011, 0b10000 | 0b00011);

  ASSERT(0, 0 ^ 0);
  ASSERT(0, 0b1111 ^ 0b1111);
  ASSERT(0b110100, 0b111000 ^ 0b001100);

  ASSERT(2, ({
           int i = 6;
           i &= 3;
           i;
         }));
  ASSERT(7, ({
           int i = 6;
           i |= 3;
           i;
         }));
  ASSERT(10, ({
           int i = 15;
           i ^= 5;
           i;
         }));

  return 0;
}