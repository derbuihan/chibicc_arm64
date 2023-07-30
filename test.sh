
function test() {
  ./main "$2" > tmp.s
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
