
function test() {
  ./chibicc "$2" > tmp.s
  cc -o tmp tmp.s
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

test 1 "return 1;"
test 3 "return 2 + 1;"
test 6 "return 1+2+3;"
test 10 "return 4 + 3 + 2 + 1;"
test 15 "return 1 + 2 + 3 + 4 + 5;"
test 5 "return 10 - 2 - 3;"
test 7 "return 1 + 2 * 3;"
test 5 "return 1 * 2 + 3;"
test 4 "return 12 / 3;"
test 3 "return 1 + 4 * 3 / 6;"

test 9 "return (1+2)*3;"
test 15 "return (1+2+3)*(2+4-3)-3+(12+18)/3-10;"
test 84 "return 12* (3 + 4);"

test 2 "return +2;"
test 2 "return + - - 2;"
test 1 "return 4+-2-+1;"

test 0 'return 0==1;'
test 1 'return 42==42;'
test 1 'return 0!=1;'
test 0 'return 42!=42;'

test 1 'return 0<1;'
test 0 'return 1<1;'
test 0 'return 2<1;'
test 1 'return 0<=1;'
test 1 'return 1<=1;'
test 0 'return 2<=1;'

test 1 'return 1>0;'
test 0 'return 1>1;'
test 0 'return 1>2;'
test 1 'return 1>=0;'
test 1 'return 1>=1;'
test 0 'return 1>=2;'

test 11 'a=11; return a;'
test 33 'd=11+22; return d;'
test 110 'a=11+22; b=33+44; return a+b;'
test 30 'a=11; b=a+22; c=b-13; d=c/2; e=d*3; return e;'
test 55 'j=1; i=2; h=3; g=4; f=5; e=6; d=7; c=8; b=9; a=10; return a+b+c+d+e+f+g+h+i+j;'
test 60 'z=10; y=20; x=30; return x+y+z;'
test 100 'a=45; z=55; return a+z;'
test 51 'a=1; b=a+1; c=b+1; d=c+1; e=d+1; f=e+1; g=f+1; h=g+1; i=h+1; j=i+1; k=j+1; l=k+1; m=l+1; n=m+1; o=n+1; p=o+1; q=p+1; r=q+1; s=r+1; t=s+1; u=t+1; v=u+1; w=v+1; x=w+1; y=x+1; z=y+1; return a+b+c+d+e+f+g+h+i+j+k+l+m+n+o+p+q+r+s+t+u+v+w+x+y+z-300;'
test 51 'z=1; y=z+1; x=y+1; w=x+1; v=w+1; u=v+1; t=u+1; s=t+1; r=s+1; q=r+1; p=q+1; o=p+1; n=o+1; m=n+1; l=m+1; k=l+1; j=k+1; i=j+1; h=i+1; g=h+1; f=g+1; e=f+1; d=e+1; c=d+1; b=c+1; a=b+1; return a+b+c+d+e+f+g+h+i+j+k+l+m+n+o+p+q+r+s+t+u+v+w+x+y+z-300;'

test 11 'foo=11; return foo;'
test 33 'foo=11; bar=22; return foo+bar;'
test 66 '_foo=11; _bar=22; _baz=33; return _foo + _bar + _baz;'
test 50 'a1=1; a2=2; a3=3; a4=4; a5=5; a6=6; a7=7; a8=8; a9=9; a10=10; a11=11; a12=12; a13=13; a14=14; a15=15; a16=16; a17=17; a18=18; a19=19; a20=20; a21=21; a22=22; a23=23; a24=24; a25=25; a26=26; a27=27; a28=28; a29=29; a30=30; a31=31; a32=32; a33=33; a34=34; a35=35; a36=36; a37=37; a38=38; a39=39; a40=40; a41=41; a42=42; a43=43; a44=44; a45=45; a46=46; a47=47; a48=48; a49=49; a50=50; a51=51; a52=52; a53=53; a54=54; a55=55; a56=56; a57=57; a58=58; a59=59; a60=60; a61=61; a62=62; a63=63; a64=64; a65=65; a66=66; a67=67; a68=68; a69=69; a70=70; a71=71; a72=72; a73=73; a74=74; a75=75; a76=76; a77=77; a78=78; a79=79; a80=80; a81=81; a82=82; a83=83; a84=84; a85=85; a86=86; a87=87; a88=88; a89=89; a90=90; a91=91; a92=92; a93=93; a94=94; a95=95; a96=96; a97=97; a98=98; a99=99; a100=100; return a1+a2+a3+a4+a5+a6+a7+a8+a9+a10+a11+a12+a13+a14+a15+a16+a17+a18+a19+a20+a21+a22+a23+a24+a25+a26+a27+a28+a29+a30+a31+a32+a33+a34+a35+a36+a37+a38+a39+a40+a41+a42+a43+a44+a45+a46+a47+a48+a49+a50+a51+a52+a53+a54+a55+a56+a57+a58+a59+a60+a61+a62+a63+a64+a65+a66+a67+a68+a69+a70+a71+a72+a73+a74+a75+a76+a77+a78+a79+a80+a81+a82+a83+a84+a85+a86+a87+a88+a89+a90+a91+a92+a93+a94+a95+a96+a97+a98+a99+a100-5000;'
test 15 'a_1=1; a_2=2; a_3=3; a_4=4; a_5=5; a_1 + a_2 + a_3 + a_4 + a_5;'

test 1 'return 1; 2; 3;'
test 2 '1; return 2; 3;'
test 3 '1; 2; return 3;'
test 1 'return 1; return 2; return 3;'
test 10 'a=5; return b=10;'

