
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

test 1 "1;"
test 3 "2 + 1;"
test 6 "1+2+3;"
test 10 "4 + 3 + 2 + 1;"
test 15 "1 + 2 + 3 + 4 + 5;"
test 5 "10 - 2 - 3;"
test 7 "1 + 2 * 3;"
test 5 "1 * 2 + 3;"
test 4 "12 / 3;"
test 3 "1 + 4 * 3 / 6;"

test 9 "(1+2)*3;"
test 15 "(1+2+3)*(2+4-3)-3+(12+18)/3-10;"
test 84 "12* (3 + 4);"

test 2 "+2;"
test 2 "+ - - 2;"
test 1 "4+-2-+1;"

test 0 '0==1;'
test 1 '42==42;'
test 1 '0!=1;'
test 0 '42!=42;'

test 1 '0<1;'
test 0 '1<1;'
test 0 '2<1;'
test 1 '0<=1;'
test 1 '1<=1;'
test 0 '2<=1;'

test 1 '1>0;'
test 0 '1>1;'
test 0 '1>2;'
test 1 '1>=0;'
test 1 '1>=1;'
test 0 '1>=2;'

test 11 'a=11; a;'
test 33 'd=11+22; d;'
test 110 'a=11+22; b=33+44; a+b;'
test 30 'a=11; b=a+22; c=b-13; d=c/2; e=d*3; e;'
test 55 'j=1; i=2; h=3; g=4; f=5; e=6; d=7; c=8; b=9; a=10; a+b+c+d+e+f+g+h+i+j;'
test 60 'z=10; y=20; x=30; x+y+z;'
test 100 'a=45; z=55; a+z;'
test 51 'a=1; b=a+1; c=b+1; d=c+1; e=d+1; f=e+1; g=f+1; h=g+1; i=h+1; j=i+1; k=j+1; l=k+1; m=l+1; n=m+1; o=n+1; p=o+1; q=p+1; r=q+1; s=r+1; t=s+1; u=t+1; v=u+1; w=v+1; x=w+1; y=x+1; z=y+1; a+b+c+d+e+f+g+h+i+j+k+l+m+n+o+p+q+r+s+t+u+v+w+x+y+z-300;'
test 51 'z=1; y=z+1; x=y+1; w=x+1; v=w+1; u=v+1; t=u+1; s=t+1; r=s+1; q=r+1; p=q+1; o=p+1; n=o+1; m=n+1; l=m+1; k=l+1; j=k+1; i=j+1; h=i+1; g=h+1; f=g+1; e=f+1; d=e+1; c=d+1; b=c+1; a=b+1; a+b+c+d+e+f+g+h+i+j+k+l+m+n+o+p+q+r+s+t+u+v+w+x+y+z-300;'
