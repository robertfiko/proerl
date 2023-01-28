docker:
	docker build -t proerl:swi .

.PHONY: test
test:
	docker run proerl:swi 