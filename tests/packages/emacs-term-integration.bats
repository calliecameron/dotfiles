# shellcheck shell=bats

setup() {
    THIS_DIR="$(cd "$(dirname "${BATS_TEST_FILENAME}")" && pwd)"

    bats_load_library 'bats-support'
    bats_load_library 'bats-assert'
}

@test 'eat version' {
    local VERSION
    VERSION="$(grep '^EAT_VERSION' "${THIS_DIR}/../../packages/emacs-term-integration/install" | grep -E -o "'.+'" | sed "s/'//g")"
    grep "${VERSION}" "${THIS_DIR}/../../elpaca-lock.el"
}
