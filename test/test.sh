#!/bin/sh

RESULT=$(sicstus -l test/runner.pl --goal "run_tests(_), halt.")
echo "-----"
echo $RESULT