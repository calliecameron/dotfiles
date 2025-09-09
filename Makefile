.PHONY: all
all: lint test

.PHONY: lint
lint:
	pre-commit run -a

.PHONY: test
test:
	cd tests && make

.PHONY: clean
clean:
	find . '(' -type f -name '*~' ')' -delete

.PHONY: deepclean
deepclean: clean
	find . -depth '(' -type d '(' -name '.mypy_cache' -o -name '.ruff_cache' -o -name '__pycache__' ')' ')' -exec rm -r '{}' ';'
	rm -rf node_modules
	rm -rf .venv
