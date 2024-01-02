#include "test.h"

int main() {
  ASSERT(131585, (int)8590066177);
  ASSERT(513, (short)8590066177);
  ASSERT(1, (char)8590066177);
  ASSERT(1, (long)1);
  ASSERT(0, (long)&*(int *)0);
  ASSERT(513, ({
           int x = 512;
           *(char *)&x = 1;
           x;
         }));
  ASSERT(5, ({
           int x = 5;
           long y = (long)&x;
           *(int *)y;
         }));
  (void)1;

  ASSERT(-1, (char)0xff);
  ASSERT(-1, (signed char)0xff);
  ASSERT(255, (unsigned char)0xff);
  ASSERT(-1, (short)0xffff);
  ASSERT(65535, (unsigned short)0xffff);
  ASSERT(-1, (int)0xffffffff);
  ASSERT(0xffffffff, (unsigned)0xffffffff);

  ASSERT(1, -1 < 1);
  ASSERT(0, -1 < (unsigned)1);
  ASSERT(0xfe, (char)0x7f + (char)0x7f);
  ASSERT(0xfffe, (short)0x7fff + (short)0x7fff);
  ASSERT(-1, -1 >> 1);
  ASSERT(-1, (unsigned long)-1);
  ASSERT(0x7fffffff, ((unsigned)-1) >> 1);
  ASSERT(-50, (-100) / 2);
  ASSERT(2147483598, ((unsigned)-100) / 2);
  ASSERT(9223372036854775758, ((unsigned long)-100) / 2);
  ASSERT(0, ((long)-1) / (unsigned)100);
  ASSERT(-2, (-100) % 7);
  ASSERT(2, ((unsigned)-100) % 7);
  ASSERT(6, ((unsigned long)-100) % 9);

  ASSERT(65535, (int)(unsigned short)65535);
  ASSERT(65535, ({
           unsigned short x = 65535;
           x;
         }));
  ASSERT(65535, ({
           unsigned short x = 65535;
           (int)x;
         }));

  ASSERT(-1, ({
    typedef short T;
    T x = 65535;
    (int)x;
  }));
  ASSERT(65535, ({
           typedef unsigned short T;
           T x = 65535;
           (int)x;
         }));

  return 0;
}
