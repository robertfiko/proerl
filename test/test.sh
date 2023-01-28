#!/bin/sh

# For sicstus
#RESULT=$(sicstus -l test/runner.pl --goal "run_tests(_), halt.")

# For Swi
RESULT=$(swipl -l test/runner.pl -g "run_tests(_), halt.")

echo "-----"
echo $RESULT
if [ "$RESULT" == "ALL TESTS PASSED" ]; then
  exit 0
else
  exit 1
fi