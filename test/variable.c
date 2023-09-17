#include "test.h"

int g1, g2, g3[4];

int main() {
  ASSERT(11, ({
           int a = 11;
           a;
         }));
  ASSERT(33, ({
           int d = 11 + 22;
           d;
         }));
  ASSERT(110, ({
           int a = 11 + 22;
           int b = 33 + 44;
           a + b;
         }));
  ASSERT(30, ({
           int a = 11;
           int b = a + 22;
           int c = b - 13;
           int d = c / 2;
           int e = d * 3;
           e;
         }));
  ASSERT(55, ({
           int j = 1;
           int i = 2;
           int h = 3;
           int g = 4;
           int f = 5;
           int e = 6;
           int d = 7;
           int c = 8;
           int b = 9;
           int a = 10;
           a + b + c + d + e + f + g + h + i + j;
         }));
  ASSERT(60, ({
           int z = 10;
           int y = 20;
           int x = 30;
           x + y + z;
         }));
  ASSERT(100, ({
           int a = 45;
           int z = 55;
           a + z;
         }));
  ASSERT(51, ({
           int a = 1;
           int b = a + 1;
           int c = b + 1;
           int d = c + 1;
           int e = d + 1;
           int f = e + 1;
           int g = f + 1;
           int h = g + 1;
           int i = h + 1;
           int j = i + 1;
           int k = j + 1;
           int l = k + 1;
           int m = l + 1;
           int n = m + 1;
           int o = n + 1;
           int p = o + 1;
           int q = p + 1;
           int r = q + 1;
           int s = r + 1;
           int t = s + 1;
           int u = t + 1;
           int v = u + 1;
           int w = v + 1;
           int x = w + 1;
           int y = x + 1;
           int z = y + 1;
           a + b + c + d + e + f + g + h + i + j + k + l + m + n + o + p + q +
               r + s + t + u + v + w + x + y + z - 300;
         }));
  ASSERT(51, ({
           int z = 1;
           int y = z + 1;
           int x = y + 1;
           int w = x + 1;
           int v = w + 1;
           int u = v + 1;
           int t = u + 1;
           int s = t + 1;
           int r = s + 1;
           int q = r + 1;
           int p = q + 1;
           int o = p + 1;
           int n = o + 1;
           int m = n + 1;
           int l = m + 1;
           int k = l + 1;
           int j = k + 1;
           int i = j + 1;
           int h = i + 1;
           int g = h + 1;
           int f = g + 1;
           int e = f + 1;
           int d = e + 1;
           int c = d + 1;
           int b = c + 1;
           int a = b + 1;
           a + b + c + d + e + f + g + h + i + j + k + l + m + n + o + p + q +
               r + s + t + u + v + w + x + y + z - 300;
         }));

  ASSERT(11, ({
           int foo = 11;
           foo;
         }));
  ASSERT(33, ({
           int foo = 11;
           int bar = 22;
           foo + bar;
         }));
  ASSERT(66, ({
           int _foo = 11;
           int _bar = 22;
           int _baz = 33;
           _foo + _bar + _baz;
         }));
  ASSERT(50, ({
           int a1 = 1;
           int a2 = 2;
           int a3 = 3;
           int a4 = 4;
           int a5 = 5;
           int a6 = 6;
           int a7 = 7;
           int a8 = 8;
           int a9 = 9;
           int a10 = 10;
           int a11 = 11;
           int a12 = 12;
           int a13 = 13;
           int a14 = 14;
           int a15 = 15;
           int a16 = 16;
           int a17 = 17;
           int a18 = 18;
           int a19 = 19;
           int a20 = 20;
           int a21 = 21;
           int a22 = 22;
           int a23 = 23;
           int a24 = 24;
           int a25 = 25;
           int a26 = 26;
           int a27 = 27;
           int a28 = 28;
           int a29 = 29;
           int a30 = 30;
           int a31 = 31;
           int a32 = 32;
           int a33 = 33;
           int a34 = 34;
           int a35 = 35;
           int a36 = 36;
           int a37 = 37;
           int a38 = 38;
           int a39 = 39;
           int a40 = 40;
           int a41 = 41;
           int a42 = 42;
           int a43 = 43;
           int a44 = 44;
           int a45 = 45;
           int a46 = 46;
           int a47 = 47;
           int a48 = 48;
           int a49 = 49;
           int a50 = 50;
           int a51 = 51;
           int a52 = 52;
           int a53 = 53;
           int a54 = 54;
           int a55 = 55;
           int a56 = 56;
           int a57 = 57;
           int a58 = 58;
           int a59 = 59;
           int a60 = 60;
           int a61 = 61;
           int a62 = 62;
           int a63 = 63;
           int a64 = 64;
           int a65 = 65;
           int a66 = 66;
           int a67 = 67;
           int a68 = 68;
           int a69 = 69;
           int a70 = 70;
           int a71 = 71;
           int a72 = 72;
           int a73 = 73;
           int a74 = 74;
           int a75 = 75;
           int a76 = 76;
           int a77 = 77;
           int a78 = 78;
           int a79 = 79;
           int a80 = 80;
           int a81 = 81;
           int a82 = 82;
           int a83 = 83;
           int a84 = 84;
           int a85 = 85;
           int a86 = 86;
           int a87 = 87;
           int a88 = 88;
           int a89 = 89;
           int a90 = 90;
           int a91 = 91;
           int a92 = 92;
           int a93 = 93;
           int a94 = 94;
           int a95 = 95;
           int a96 = 96;
           int a97 = 97;
           int a98 = 98;
           int a99 = 99;
           int a100 = 100;
           a1 + a2 + a3 + a4 + a5 + a6 + a7 + a8 + a9 + a10 + a11 + a12 + a13 +
               a14 + a15 + a16 + a17 + a18 + a19 + a20 + a21 + a22 + a23 + a24 +
               a25 + a26 + a27 + a28 + a29 + a30 + a31 + a32 + a33 + a34 + a35 +
               a36 + a37 + a38 + a39 + a40 + a41 + a42 + a43 + a44 + a45 + a46 +
               a47 + a48 + a49 + a50 + a51 + a52 + a53 + a54 + a55 + a56 + a57 +
               a58 + a59 + a60 + a61 + a62 + a63 + a64 + a65 + a66 + a67 + a68 +
               a69 + a70 + a71 + a72 + a73 + a74 + a75 + a76 + a77 + a78 + a79 +
               a80 + a81 + a82 + a83 + a84 + a85 + a86 + a87 + a88 + a89 + a90 +
               a91 + a92 + a93 + a94 + a95 + a96 + a97 + a98 + a99 + a100 -
               5000;
         }));
  ASSERT(15, ({
           int a_1 = 1;
           int a_2 = 2;
           int a_3 = 3;
           int a_4 = 4;
           int a_5 = 5;
           a_1 + a_2 + a_3 + a_4 + a_5;
         }));

  ASSERT(4, ({
           int x;
           sizeof(x);
         }));
  ASSERT(4, ({
           int x;
           sizeof x;
         }));
  ASSERT(8, ({
           int *x;
           sizeof(x);
         }));
  ASSERT(16, ({
           int x[4];
           sizeof(x);
         }));
  ASSERT(48, ({
           int x[3][4];
           sizeof(x);
         }));
  ASSERT(16, ({
           int x[3][4];
           sizeof(*x);
         }));
  ASSERT(4, ({
           int x[3][4];
           sizeof(**x);
         }));
  ASSERT(5, ({
           int x[3][4];
           sizeof(**x) + 1;
         }));

  ASSERT(0, g1);
  ASSERT(3, ({
           g1 = 3;
           g1;
         }));
  ASSERT(7, ({
           g1 = 3;
           g2 = 4;
           g1 + g2;
         }));
  ASSERT(0, ({
           g3[0] = 0;
           g3[1] = 1;
           g3[2] = 2;
           g3[3] = 3;
           g3[0];
         }));
  ASSERT(1, ({
           g3[0] = 0;
           g3[1] = 1;
           g3[2] = 2;
           g3[3] = 3;
           g3[1];
         }));
  ASSERT(2, ({
           g3[0] = 0;
           g3[1] = 1;
           g3[2] = 2;
           g3[3] = 3;
           g3[2];
         }));
  ASSERT(3, ({
           g3[0] = 0;
           g3[1] = 1;
           g3[2] = 2;
           g3[3] = 3;
           g3[3];
         }));
  ASSERT(4, ({ sizeof(g1); }));
  ASSERT(16, ({ sizeof(g3); }));

  ASSERT(1, ({
           char x = 1;
           x;
         }));
  ASSERT(1, ({
           char x = 1;
           char y = 2;
           x;
         }));
  ASSERT(2, ({
           char x = 1;
           char y = 2;
           y;
         }));
  ASSERT(1, ({
           char x;
           sizeof(x);
         }));
  ASSERT(10, ({
           char x[10];
           sizeof(x);
         }));

  ASSERT(2, ({
           int x = 2;
           { int x = 3; }
           x;
         }));
  ASSERT(2, ({
           int x = 2;
           { int x = 3; }
           int y = 4;
           x;
         }));
  ASSERT(3, ({
           int x = 2;
           { x = 3; }
           x;
         }));

  ASSERT(7, ({
           int x;
           int y;
           char z;
           char *a = &y;
           char *b = &z;
           b - a;
         }));
  ASSERT(1, ({
           int x;
           char y;
           int z;
           char *a = &y;
           char *b = &z;
           b - a;
         }));

  return 0;
}