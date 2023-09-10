#include "test.h"

int main() {
  ASSERT(3, ({
           1;
           { 2; }
           3;
         }));
  ASSERT(5, ({
           ;
           ;
           ;
           5;
         }));

  ASSERT(3, ({
           int x;
           if (0) {
             x = 2;
           } else {
             x = 3;
           }
           x;
         }));
  ASSERT(3, ({
           int x;
           if (1 - 1) {
             x = 2;
           } else {
             x = 3;
           }
           x;
         }));
  ASSERT(2, ({
           int x;
           if (1) {
             x = 2;
           } else {
             x = 3;
           }
           x;
         }));
  ASSERT(2, ({
           int x;
           if (2 - 1) {
             x = 2;
           } else {
             x = 3;
           }
           x;
         }));

  ASSERT(6, ({
           int x;
           if (0) {
             if (0) {
               x = 3;
             } else {
               x = 4;
             }
           } else {
             if (0) {
               x = 5;
             } else {
               x = 6;
             }
           }
           x;
         }));
  ASSERT(5, ({
           int x;
           if (0) {
             if (0) {
               x = 3;
             } else {
               x = 4;
             }
           } else {
             if (1) {
               x = 5;
             } else {
               x = 6;
             }
           }
           x;
         }));
  ASSERT(6, ({
           int x;
           if (0) {
             if (1) {
               x = 3;
             } else {
               x = 4;
             }
           } else {
             if (0) {
               x = 5;
             } else {
               x = 6;
             }
           }
           x;
         }));
  ASSERT(4, ({
           int x;
           if (1) {
             if (0) {
               x = 3;
             } else {
               x = 4;
             }
           } else {
             if (0) {
               x = 5;
             } else {
               x = 6;
             }
           }
           x;
         }));
  ASSERT(5, ({
           int x;
           if (0) {
             if (1) {
               x = 3;
             } else {
               x = 4;
             }
           } else {
             if (1) {
               x = 5;
             } else {
               x = 6;
             }
           }
           x;
         }));
  ASSERT(4, ({
           int x;
           if (1) {
             if (0) {
               x = 3;
             } else {
               x = 4;
             }
           } else {
             if (1) {
               x = 5;
             } else {
               x = 6;
             }
           }
           x;
         }));
  ASSERT(3, ({
           int x;
           if (1) {
             if (1) {
               x = 3;
             } else {
               x = 4;
             }
           } else {
             if (0) {
               x = 5;
             } else {
               x = 6;
             }
           }
           x;
         }));
  ASSERT(3, ({
           int x;
           if (1) {
             if (1) {
               x = 3;
             } else {
               x = 4;
             }
           } else {
             if (1) {
               x = 5;
             } else {
               x = 6;
             }
           }
           x;
         }));

  ASSERT(55, ({
           int i = 0;
           int j = 0;
           for (i = 0; i <= 10; i = i + 1) {
             j = i + j;
           }
           j;
         }));

  ASSERT(10, ({
           int i = 0;
           while (i < 10) {
             i = i + 1;
           }
           i;
         }));
  ASSERT(55, ({
           int i = 0;
           int sum = 0;
           while (i < 10) {
             i = i + 1;
             sum = sum + i;
           }
           sum;
         }));
  ASSERT(97, ({
           int p = 1;
           int is_prime = 1;
           int i = 1;
           int j = 1;
           int ans = 1;
           for (p = 2; p < 100; p = p + 1) {
             is_prime = i = 1;
             while (i * i <= p) {
               i = i + 1;
               for (j = 1; i * j <= p; j = j + 1) {
                 if (i * j == p) {
                   is_prime = 0;
                 }
               }
             }
             if (is_prime) {
               ans = p;
             }
           }
           ans;
         }));

  ASSERT(3, (1, 2, 3));
  ASSERT(6, ({
           int i = 2, j = 3;
           (i = 5, j) = 6;
           j;
         }));

  return 0;
}