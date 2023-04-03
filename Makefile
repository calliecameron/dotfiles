.PHONY: all
all: lint test

.PHONY: lint
lint:
	tests/find-shell-files.sh | xargs -d '\n' shellcheck
	tests/find-python-files.sh | xargs -d '\n' pylint
	tests/find-python-files.sh | xargs -d '\n' flake8
	tests/find-python-files.sh | xargs -d '\n' black --check
	tests/find-python-files.sh | xargs -d '\n' mypy --strict

.PHONY: test
test:
	cd tests && make
