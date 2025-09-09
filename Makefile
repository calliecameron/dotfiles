.PHONY: all
all: lint test

.PHONY: lint
lint:
	pre-commit run -a

.PHONY: lint_internal
lint_internal:
	tests/find-shell-files.sh | xargs -d '\n' shellcheck
	tests/find-shell-files.sh | xargs -d '\n' shfmt -l -w -i 4
	tests/find-bats-files.sh | xargs -d '\n' shellcheck
	tests/find-bats-files.sh | xargs -d '\n' shfmt -l -w -i 4 -ln bats

.PHONY: test
test:
	cd tests && make

.PHONY: precommit
precommit: lint_internal

.PHONY: clean
clean:
	find . '(' -type f -name '*~' ')' -delete

.PHONY: deepclean
deepclean: clean
	find . -depth '(' -type d '(' -name '.mypy_cache' -o -name '.ruff_cache' -o -name '__pycache__' ')' ')' -exec rm -r '{}' ';'
	rm -rf node_modules
	rm -rf .venv
