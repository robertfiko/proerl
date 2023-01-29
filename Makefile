docker:
	docker build -t proerl:swi .

.PHONY: test
test:
	docker run proerl:swi 

local:
	sicstus -l test/runner.pl --goal "run_tests(_), halt."

pl:
	sicstus -l src/erl.pl