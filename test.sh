#!/bin/bash

cat << EOF | gcc -xc -c -o tmp2.o -
  int ret11() {return 11;}
  int add11(int a) {return a + 11;}
  int add2(int a, int b) {return a+b;}
  int sub2(int a, int b) {return a-b;}
  int sub3(int a, int b, int c) {return a-b-c;}
  int add8(int a1, int a2, int a3, int a4, int a5, int a6, int a7, int a8) {return a1+a2+a3+a4+a5+a6+a7+a8;}
EOF

function test() {
  ./chibicc "$2" > tmp.s
  cc -o tmp tmp.s tmp2.o
  ./tmp
  ret=$?
  [ "$ret" = "$1" ] && echo "ok $2 = $ret" || ( echo "ng $2 != $ret"; exit 1 )

  if [ "$ret" == "$1" ]; then
    echo "ok $2 = $ret"
  else
    echo "ng $2 != $ret"
    exit 1
  fi
}

test 1 "int main() {return 1;}"
test 3 "int main() {return 2 + 1;}"
test 6 "int main() {return 1+2+3;}"
test 10 "int main() {return 4 + 3 + 2 + 1;}"
test 15 "int main() {return 1 + 2 + 3 + 4 + 5;}"
test 5 "int main() {return 10 - 2 - 3;}"
test 7 "int main() {return 1 + 2 * 3;}"
test 5 "int main() {return 1 * 2 + 3;}"
test 4 "int main() {return 12 / 3;}"
test 3 "int main() {return 1 + 4 * 3 / 6;}"

test 9 "int main() {return (1+2)*3;}"
test 15 "int main() {return (1+2+3)*(2+4-3)-3+(12+18)/3-10;}"
test 84 "int main() {return 12* (3 + 4);}"

test 2 "int main() {return +2;}"
test 2 "int main() {return + - - 2;}"
test 1 "int main() {return 4+-2-+1;}"

test 0 'int main() {return 0==1;}'
test 1 'int main() {return 42==42;}'
test 1 'int main() {return 0!=1;}'
test 0 'int main() {return 42!=42;}'

test 1 'int main() {return 0<1;}'
test 0 'int main() {return 1<1;}'
test 0 'int main() {return 2<1;}'
test 1 'int main() {return 0<=1;}'
test 1 'int main() {return 1<=1;}'
test 0 'int main() {return 2<=1;}'

test 1 'int main() {return 1>0;}'
test 0 'int main() {return 1>1;}'
test 0 'int main() {return 1>2;}'
test 1 'int main() {return 1>=0;}'
test 1 'int main() {return 1>=1;}'
test 0 'int main() {return 1>=2;}'

test 11 'int main() {int a=11; return a;}'
test 33 'int main() {int d=11+22; return d;}'
test 110 'int main() {int a=11+22; int b=33+44; return a+b;}'
test 30 'int main() {int a=11; int b=a+22; int c=b-13; int d=c/2; int e=d*3; return e;}'
test 55 'int main() {int j=1; int i=2; int h=3; int g=4; int f=5; int e=6; int d=7; int c=8; int b=9; int a=10; return a+b+c+d+e+f+g+h+i+j;}'
test 60 'int main() {int z=10; int y=20; int x=30; return x+y+z;}'
test 100 'int main() {int a=45; int z=55; return a+z;}'
test 51 'int main() {int a=1; int b=a+1; int c=b+1; int d=c+1; int e=d+1; int f=e+1; int g=f+1; int h=g+1; int i=h+1; int j=i+1; int k=j+1; int l=k+1; int m=l+1; int n=m+1; int o=n+1; int p=o+1; int q=p+1; int r=q+1; int s=r+1; int t=s+1; int u=t+1; int v=u+1; int w=v+1; int x=w+1; int y=x+1; int z=y+1; return a+b+c+d+e+f+g+h+i+j+k+l+m+n+o+p+q+r+s+t+u+v+w+x+y+z-300;}'
test 51 'int main() {int z=1; int y=z+1; int x=y+1; int w=x+1; int v=w+1; int u=v+1; int t=u+1; int s=t+1; int r=s+1; int q=r+1; int p=q+1; int o=p+1; int n=o+1; int m=n+1; int l=m+1; int k=l+1; int j=k+1; int i=j+1; int h=i+1; int g=h+1; int f=g+1; int e=f+1; int d=e+1; int c=d+1; int b=c+1; int a=b+1; return a+b+c+d+e+f+g+h+i+j+k+l+m+n+o+p+q+r+s+t+u+v+w+x+y+z-300;}'

test 11 'int main() {int foo=11; return foo;}'
test 33 'int main() {int foo=11; int bar=22; return foo+bar;}'
test 66 'int main() {int _foo=11; int _bar=22; int _baz=33; return _foo + _bar + _baz;}'
test 50 'int main() {int a1=1; int a2=2; int a3=3; int a4=4; int a5=5; int a6=6; int a7=7; int a8=8; int a9=9; int a10=10; int a11=11; int a12=12; int a13=13; int a14=14; int a15=15; int a16=16; int a17=17; int a18=18; int a19=19; int a20=20; int a21=21; int a22=22; int a23=23; int a24=24; int a25=25; int a26=26; int a27=27; int a28=28; int a29=29; int a30=30; int a31=31; int a32=32; int a33=33; int a34=34; int a35=35; int a36=36; int a37=37; int a38=38; int a39=39; int a40=40; int a41=41; int a42=42; int a43=43; int a44=44; int a45=45; int a46=46; int a47=47; int a48=48; int a49=49; int a50=50; int a51=51; int a52=52; int a53=53; int a54=54; int a55=55; int a56=56; int a57=57; int a58=58; int a59=59; int a60=60; int a61=61; int a62=62; int a63=63; int a64=64; int a65=65; int a66=66; int a67=67; int a68=68; int a69=69; int a70=70; int a71=71; int a72=72; int a73=73; int a74=74; int a75=75; int a76=76; int a77=77; int a78=78; int a79=79; int a80=80; int a81=81; int a82=82; int a83=83; int a84=84; int a85=85; int a86=86; int a87=87; int a88=88; int a89=89; int a90=90; int a91=91; int a92=92; int a93=93; int a94=94; int a95=95; int a96=96; int a97=97; int a98=98; int a99=99; int a100=100; return a1+a2+a3+a4+a5+a6+a7+a8+a9+a10+a11+a12+a13+a14+a15+a16+a17+a18+a19+a20+a21+a22+a23+a24+a25+a26+a27+a28+a29+a30+a31+a32+a33+a34+a35+a36+a37+a38+a39+a40+a41+a42+a43+a44+a45+a46+a47+a48+a49+a50+a51+a52+a53+a54+a55+a56+a57+a58+a59+a60+a61+a62+a63+a64+a65+a66+a67+a68+a69+a70+a71+a72+a73+a74+a75+a76+a77+a78+a79+a80+a81+a82+a83+a84+a85+a86+a87+a88+a89+a90+a91+a92+a93+a94+a95+a96+a97+a98+a99+a100-5000;}'
test 15 'int main() {int a_1=1; int a_2=2; int a_3=3; int a_4=4; int a_5=5; return a_1 + a_2 + a_3 + a_4 + a_5;}'

test 1 'int main() {return 1; 2; 3;}'
test 2 'int main() {1; return 2; 3;}'
test 3 'int main() {1; 2; return 3;}'
test 1 'int main() {return 1; return 2; return 3;}'

test 3 'int main() { { 1; {2;} return 3;} }'
test 5 'int main() { ;;; return 5;}'

test 3 'int main() {if (0) return 2; return 3;}'
test 3 'int main() {if (1-1) return 2; return 3;}'
test 2 'int main() {if (1) return 2; return 3;}'
test 2 'int main() {if (2-1) return 2; return 3;}'
test 4 'int main() {if (0) {1; 2; return 3;} else {return 4;}}'
test 3 'int main() {if (1) {1; 2; return 3;} else {return 4;}}'

test 6 'int main() {if (0) { if (0) {return 3;} else {return 4;} } else { if (0) {return 5;} else {return 6;} } return 7; }'
test 5 'int main() {if (0) { if (0) {return 3;} else {return 4;} } else { if (1) {return 5;} else {return 6;} } return 7; }'
test 6 'int main() {if (0) { if (1) {return 3;} else {return 4;} } else { if (0) {return 5;} else {return 6;} } return 7; }'
test 4 'int main() {if (1) { if (0) {return 3;} else {return 4;} } else { if (0) {return 5;} else {return 6;} } return 7; }'
test 5 'int main() {if (0) { if (1) {return 3;} else {return 4;} } else { if (1) {return 5;} else {return 6;} } return 7; }'
test 4 'int main() {if (1) { if (0) {return 3;} else {return 4;} } else { if (1) {return 5;} else {return 6;} } return 7; }'
test 3 'int main() {if (1) { if (1) {return 3;} else {return 4;} } else { if (0) {return 5;} else {return 6;} } return 7; }'
test 3 'int main() {if (1) { if (1) {return 3;} else {return 4;} } else { if (1) {return 5;} else {return 6;} } return 7; }'

test 3 'int main() {for (;;) return 3;}'
test 3 'int main() { for (;;) { return 3; } return 5; }'
test 55 'int main() {int i=0; int j=0; for (i=0; i<=10; i=i+1) j=i+j; return j;}'
test 55 'int main() {int i=0; int j=0; for (i=0; i<=10; i=i+1) {j=i+j;} return j;}'

test 10 'int main() {int i=0; while (i<10) {i = i+1;} return i;}'
test 55 'int main() {int i=0; int sum=0; while (i<10) {i = i+1; sum=sum+i;} return sum;}'
test 97 'int main() {int p=1; int is_prime=1; int i=1; int j=1; int ans=1; for (p = 2; p < 100; p = p + 1) { is_prime = i = 1; while (i * i <= p) { i = i + 1; for (j = 1; i * j <= p; j = j + 1) { if (i * j == p) { is_prime = 0; } } } if (is_prime) { ans = p; } } return ans; }'

test 3 'int main() {int x=3; return *&x;}'
test 3 'int main() {int x=3; int *y=&x; int **z=&y; return **z; }'
test 5 'int main() {int x=3; int y=5; return *(&x + 1); }'
test 3 'int main() {int x=3; int y=5; return *(&y - 1); }'
test 5 'int main() {int x=3; int *y=&x; *y=5; return x; }'
test 7 'int main() {int x=3; int y=5; *(&x + 1)=7; return y; }'
test 7 'int main() {int x=3; int y=5; *(&y - 1)=7; return x; }'

test 11 'int main() {return ret11();}'
test 33 'int main() {return add11(22);}'
test 33 'int main() {return add2(11, 22);}'
test 11 'int main() {return sub2(22, 11);}'
test 11 'int main() {return sub3(44, 22, 11);}'
test 36 'int main() {return add8(1, 2, 3, 4, 5, 6, 7, 8);}'

test 11 'int ret() {return 11;} int main() {return ret();}'
test 33 'int add(int x, int y) {return x + y;} int main() {return add(11, 22);}'
test 44 'int add(int x, int y) {return x + y + ret11();} int main() {return add(11, 22);}'
test 44 'int main() {return add(11, 22);} int add (int x, int y) { return x + y + ret11(); }'
test 55 'int main() {return fib(9);} int fib(int x) {if (x<=1) return 1; return fib(x-1) + fib(x-2);}'
test 42 'int main() {return fib(7) + fib(7);} int fib(int x) {if (x<=1) return 1; return fib(x-1) + fib(x-2);}'

test 11 'int main() {int x[1]; *x = 11; return *x;}'
test 11 'int main() {int x[2]; *(x+1) = 11; return *(x+1);}'
test 11 'int main() { int x[2]; int *y = x; *y = 11; return *x;}'
test 11 'int main() { int x[2]; int *y = &x; *y = 11; return *x;}'
test 11 'int main() { int x[2]; int *y = x+1; *y = 11; return *(x+1);}'

echo "OK"