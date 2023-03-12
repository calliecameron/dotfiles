#!/usr/bin/env bats

setup() {
    THIS_DIR="$(cd "$(dirname "${BATS_TEST_FILENAME}")" && pwd)"
    BIN_DIR="${THIS_DIR}/../../core/bin"

    TMP_DIR="$(mktemp -d)"
    TEST_NEXT_INIT="${TMP_DIR}/next-init"
    TEST_NEXT_LOGIN="${TMP_DIR}/next-login"

    bats_load_library 'bats-support'
    bats_load_library 'bats-assert'
}

teardown() {
    rm -rf "${TMP_DIR}"
}

run_script() {
    run env -i -C "${TMP_DIR}" HOME="${TMP_DIR}" "${@}"
}

@test 'dotfiles-next-init usage' {
    run_script "${BIN_DIR}/dotfiles-next-init"
    assert_failure
    assert_line --partial 'Usage:'
}

@test 'dotfiles-next-init nonexistent' {
    run_script "DOTFILES_NEXT_INIT=${TEST_NEXT_INIT}" "${BIN_DIR}/dotfiles-next-init" 'foo' 'bar'
    assert_success
    refute_line --partial 'Usage:'
    assert [ "$(grep 'This file will be run' <"${TEST_NEXT_INIT}" | wc -l)" = '1' ]
    assert [ "$(grep 'foo bar' <"${TEST_NEXT_INIT}" | wc -l)" = '1' ]
}

@test 'dotfiles-next-init existing same command' {
    echo 'foo bar' >"${TEST_NEXT_INIT}"
    run_script "DOTFILES_NEXT_INIT=${TEST_NEXT_INIT}" "${BIN_DIR}/dotfiles-next-init" 'foo' 'bar'
    assert_success
    refute_line --partial 'Usage:'
    assert [ "$(grep 'foo bar' <"${TEST_NEXT_INIT}" | wc -l)" = '1' ]
}

@test 'dotfiles-next-init existing different command' {
    echo 'foo bar' >"${TEST_NEXT_INIT}"
    run_script "DOTFILES_NEXT_INIT=${TEST_NEXT_INIT}" "${BIN_DIR}/dotfiles-next-init" 'foo' 'baz'
    assert_success
    refute_line --partial 'Usage:'
    assert [ "$(grep 'foo bar' <"${TEST_NEXT_INIT}" | wc -l)" = '1' ]
    assert [ "$(grep 'foo baz' <"${TEST_NEXT_INIT}" | wc -l)" = '1' ]
}

@test 'dotfiles-next-login usage' {
    run_script "${BIN_DIR}/dotfiles-next-login"
    assert_failure
    assert_line --partial 'Usage:'
}

@test 'dotfiles-next-login nonexistent' {
    run_script "DOTFILES_NEXT_LOGIN=${TEST_NEXT_LOGIN}" "${BIN_DIR}/dotfiles-next-login" 'foo' 'bar'
    assert_success
    refute_line --partial 'Usage:'
    assert [ "$(grep 'This file will be run' <"${TEST_NEXT_LOGIN}" | wc -l)" = '1' ]
    assert [ "$(grep 'foo bar' <"${TEST_NEXT_LOGIN}" | wc -l)" = '1' ]
}

@test 'dotfiles-next-login existing same command' {
    echo 'foo bar' >"${TEST_NEXT_LOGIN}"
    run_script "DOTFILES_NEXT_LOGIN=${TEST_NEXT_LOGIN}" "${BIN_DIR}/dotfiles-next-login" 'foo' 'bar'
    assert_success
    refute_line --partial 'Usage:'
    assert [ "$(grep 'foo bar' <"${TEST_NEXT_LOGIN}" | wc -l)" = '1' ]
}

@test 'dotfiles-next-login existing different command' {
    echo 'foo bar' >"${TEST_NEXT_LOGIN}"
    run_script "DOTFILES_NEXT_LOGIN=${TEST_NEXT_LOGIN}" "${BIN_DIR}/dotfiles-next-login" 'foo' 'baz'
    assert_success
    refute_line --partial 'Usage:'
    assert [ "$(grep 'foo bar' <"${TEST_NEXT_LOGIN}" | wc -l)" = '1' ]
    assert [ "$(grep 'foo baz' <"${TEST_NEXT_LOGIN}" | wc -l)" = '1' ]
}
