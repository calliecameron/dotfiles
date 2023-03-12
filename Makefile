.PHONY: all
all: lint test

.PHONY: lint
lint:
	bash -c "( grep -rlE --exclude-dir=.git --exclude-dir=private '#!/bin/(ba)?sh' && find . -name '*.sh' -o -name '*.bash' ) | grep -vE '.*~$$' | sed 's|^./||g' | sort | uniq | xargs -d '\n' shellcheck"

.PHONY: test
test:
	cd tests && make
