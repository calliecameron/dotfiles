# shellcheck shell=bats

setup() {
    THIS_DIR="$(cd "$(dirname "${BATS_TEST_FILENAME}")" && pwd)"

    bats_load_library 'bats-support'
    bats_load_library 'bats-assert'
}

@test 'python version' {
    local SYSTEM
    SYSTEM="$(/usr/bin/python3 --version | grep -E -o '3\.[0-9]+\.[0-9]+')"
    local LOCAL
    LOCAL="$(cat "${THIS_DIR}/../.python-version")"

    assert_equal "${LOCAL}" "${SYSTEM}"
}
