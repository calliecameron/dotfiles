.PHONY: all
all: lint test

.PHONY: deps
deps: .deps-installed

.deps-installed: package.json package-lock.json
	npm install
	touch .deps-installed

.PHONY: lint
lint: deps
	tests/find-shell-files.sh | xargs -d '\n' shellcheck
	tests/find-shell-files.sh | xargs -d '\n' shfmt -l -d -i 4
	tests/find-bats-files.sh | xargs -d '\n' shellcheck
	tests/find-bats-files.sh | xargs -d '\n' shfmt -l -d -i 4 -ln bats
	tests/find-python-files.sh | xargs -d '\n' uv run ruff check
	tests/find-python-files.sh | xargs -d '\n' uv run ruff format --diff
	tests/find-python-files.sh | xargs -d '\n' uv run mypy --strict

.PHONY: test
test: deps
	cd tests && make

.PHONY: clean
clean:
	rm -f .deps-installed
	find . -depth '(' -type d '(' -name '.mypy_cache' -o -name '.ruff_cache' -o -name '__pycache__' ')' ')' -exec rm -r '{}' ';'
	find . '(' -type f -name '*~' ')' -delete
