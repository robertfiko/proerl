#!/bin/sh

# For sicstus
#RESULT=$(sicstus -l test/runner.pl --goal "run_tests(_), halt.")

# For Swi
pwd
swipl -l test/runner.pl -g "run_tests(_), halt(1)."
exit $@