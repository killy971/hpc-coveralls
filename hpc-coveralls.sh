RESULT=`echo $TRAVIS_TEST_RESULT | grep "^Test suite .*: FAIL$" | wc -l | tr -d ' '`
exit $RESULT
