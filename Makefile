.PHONY: all
all: lint test

.PHONY: lint
lint:
	tests/find-shell-files.sh | xargs -d '\n' shellcheck

.PHONY: test
test:
	cd tests && make
