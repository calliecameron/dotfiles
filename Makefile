.PHONY: all
all: lint test

.PHONY: deps
deps: .deps-installed

.deps-installed: package.json package-lock.json
	npm install
	touch .deps-installed

.PHONY: lint
lint:
	pre-commit run -a

.PHONY: lint_internal
lint_internal: deps
	tests/find-shell-files.sh | xargs -d '\n' shellcheck
	tests/find-shell-files.sh | xargs -d '\n' shfmt -l -w -i 4
	tests/find-bats-files.sh | xargs -d '\n' shellcheck
	tests/find-bats-files.sh | xargs -d '\n' shfmt -l -w -i 4 -ln bats
	tests/find-python-files.sh | xargs -d '\n' uv run ruff check --fix
	tests/find-python-files.sh | xargs -d '\n' uv run ruff format
	tests/find-python-files.sh | xargs -d '\n' uv run mypy
	npx prettier --write .
	uv run codespell -w

.PHONY: test
test: deps
	cd tests && make

.PHONY: precommit
precommit: lint_internal

.PHONY: clean
clean:
	find . '(' -type f -name '*~' ')' -delete

.PHONY: deepclean
deepclean: clean
	rm -f .deps-installed
	find . -depth '(' -type d '(' -name '.mypy_cache' -o -name '.ruff_cache' -o -name '__pycache__' ')' ')' -exec rm -r '{}' ';'
	rm -rf node_modules
	rm -rf .venv
