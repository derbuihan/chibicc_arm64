
function test() {
  ./chibicc "$2" > tmp.s
  cc -o tmp tmp.s
  ./tmp
  ret=$?
  [ "$ret" = "$1" ] && echo "ok $2 = $ret" || echo "ng $2 != $ret"
}

test 1 "1"
test 3 "2 + 1"
test 6 "1+2+3"
test 10 "4 + 3 + 2 + 1"
test 15 "1 + 2 + 3 + 4 + 5"
test 5 "10 - 2 - 3"
test 7 "1 + 2 * 3"
test 5 "1 * 2 + 3"
test 4 "12 / 3"
test 3 "1 + 4 * 3 / 6"

test 9 "(1+2)*3"
test 15 "(1+2+3)*(2+4-3)-3+(12+18)/3-10"
test 84 "12* (3 + 4)"

test 2 "+2"
test 2 "+ - - 2"
test 1 "4+-2-+1"

test 0 '0==1'
test 1 '42==42'
test 1 '0!=1'
test 0 '42!=42'

test 1 '0<1'
test 0 '1<1'
test 0 '2<1'
test 1 '0<=1'
test 1 '1<=1'
test 0 '2<=1'

test 1 '1>0'
test 0 '1>1'
test 0 '1>2'
test 1 '1>=0'
test 1 '1>=1'
test 0 '1>=2'
