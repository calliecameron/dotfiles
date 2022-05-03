.PHONY: all
all: test

.PHONY: test
test:
	cd tests && make
