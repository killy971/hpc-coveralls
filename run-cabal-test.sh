exec 5>&1
TEST_OUTPUT=$(cabal test $* | tee /dev/fd/5)
RESULT=`echo "$TEST_OUTPUT" | grep "^Test suite .*: FAIL$" | wc -l | tr -d ' '`
exit $RESULT
