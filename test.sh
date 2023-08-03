
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
test 60 'z=10; y=20; x=30; x+y+z;'
