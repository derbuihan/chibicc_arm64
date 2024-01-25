#include "test.h"

int main() {
  ASSERT(3, ({
           int x = 3;
           *&x;
         }));
  ASSERT(3, ({
           int x = 3;
           int *y = &x;
           int **z = &y;
           **z;
         }));
  ASSERT(5, ({
           int x = 3;
           int y = 5;
           *(&x + 1);
         }));
  ASSERT(3, ({
           int x = 3;
           int y = 5;
           *(&y - 1);
         }));
  ASSERT(5, ({
           int x = 3;
           int *y = &x;
           *y = 5;
           x;
         }));
  ASSERT(7, ({
           int x = 3;
           int y = 5;
           *(&x + 1) = 7;
           y;
         }));
  ASSERT(7, ({
           int x = 3;
           int y = 5;
           *(&y - 1) = 7;
           x;
         }));

  ASSERT(11, ({
           int x[1];
           *x = 11;
           *x;
         }));
  ASSERT(11, ({
           int x[2];
           *(x + 1) = 11;
           *(x + 1);
         }));
  ASSERT(11, ({
           int x[2];
           int *y = x;
           *y = 11;
           *x;
         }));
  ASSERT(11, ({
           int x[2];
           int *y = &x;
           *y = 11;
           *x;
         }));
  ASSERT(11, ({
           int x[2];
           int *y = x + 1;
           *y = 11;
           *(x + 1);
         }));

  ASSERT(3, ({
           int x[3];
           x[0] = 3;
           x[1] = 4;
           x[2] = 5;
           *x;
         }));
  ASSERT(4, ({
           int x[3];
           x[0] = 3;
           x[1] = 4;
           x[2] = 5;
           *(x + 1);
         }));
  ASSERT(5, ({
           int x[3];
           x[0] = 3;
           x[1] = 4;
           x[2] = 5;
           *(x + 2);
         }));
  ASSERT(5, ({
           int x[3];
           x[0] = 3;
           x[1] = 4;
           2 [x] = 5;
           *(x + 2);
         }));

  ASSERT(3, ({
           int x[2][3];
           int *y = x;
           y[0] = 3;
           x[0][0];
         }));
  ASSERT(4, ({
           int x[2][3];
           int *y = x;
           y[1] = 4;
           x[0][1];
         }));
  ASSERT(5, ({
           int x[2][3];
           int *y = x;
           y[2] = 5;
           x[0][2];
         }));
  ASSERT(6, ({
           int x[2][3];
           int *y = x;
           y[3] = 6;
           x[1][0];
         }));
  ASSERT(7, ({
           int x[2][3];
           int *y = x;
           y[4] = 7;
           x[1][1];
         }));
  ASSERT(8, ({
           int x[2][3];
           int *y = x;
           y[5] = 8;
           x[1][2];
         }));
  ASSERT(8, ({
           int x[2][3][4];
           int *y = x;
           y[23] = 8;
           x[1][2][3];
         }));

  printf("OK\n");

  return 0;
}