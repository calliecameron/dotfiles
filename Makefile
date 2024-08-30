.PHONY: all
all: lint test

.PHONY: deps
deps: .deps-installed

.deps-installed: requirements-dev.txt
	pip install -r requirements-dev.txt
	touch .deps-installed

requirements-dev.txt: requirements-dev.in
	pip-compile -q requirements-dev.in

.PHONY: lint
lint: deps
	tests/find-shell-files.sh | xargs -d '\n' shellcheck
	tests/find-shell-files.sh | xargs -d '\n' shfmt -l -d -i 4
	tests/find-bats-files.sh | xargs -d '\n' shellcheck
	tests/find-bats-files.sh | xargs -d '\n' shfmt -l -d -i 4 -ln bats
	tests/find-python-files.sh | xargs -d '\n' ruff check
	tests/find-python-files.sh | xargs -d '\n' ruff format --diff
	tests/find-python-files.sh | xargs -d '\n' mypy --strict

.PHONY: test
test: deps
	cd tests && make

.PHONY: clean
clean:
	rm -f .deps-installed *~
