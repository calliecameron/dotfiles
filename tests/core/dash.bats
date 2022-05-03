#!/usr/bin/env bats

setup() {
    bats_load_library 'bats-support'
    bats_load_library 'bats-assert'
    load 'helpers.bash'

    THIS_DIR="$(cd "$(dirname "${BATS_TEST_FILENAME}")" && pwd)"
    TMP_DIR="$(mktemp -d)"
    INSTALL="${THIS_DIR}/../../install.sh"

    setup_common
    HOME="${TMP_DIR}" "${INSTALL}"
}

teardown() {
    rm -rf "${TMP_DIR}"
}

function run_dash() {
    run env -i -C "${TMP_DIR}" HOME="${TMP_DIR}" dash "${@}" -c 'env' 3>&-
}

function assert_ran() {
    assert_output --partial 'DOTFILES'
    assert_package_env_run_by 'dash'
    assert_not_package_generic_aliases_run
    assert_not_package_bash_aliases_run
    assert_not_package_zsh_aliases_run
    assert_local_env_run_by 'dash'
    assert_not_local_generic_aliases_run
    assert_not_local_bash_aliases_run
    assert_not_local_zsh_aliases_run
    assert_line --regexp "^PATH=.*$(readlink -f "${THIS_DIR}/../..")/core/bin.*\$"
    assert_line --regexp "^PATH=.*$(readlink -f "${TMP_DIR}")/\\.bin.*\$"
    assert_line "TEST_PACKAGE_ROOTS=/bar:$(readlink -f "${THIS_DIR}/../..")/private/packages-pre:$(readlink -f "${THIS_DIR}/../..")/packages:$(readlink -f "${THIS_DIR}/../..")/private/packages:/foo"
}

@test 'run simple' {
    # Non-interactive, non-login - shouldn't load anything
    run_dash
    assert_success
    assert_nothing_ran
}

@test 'run interactive' {
    # Interactive, non-login - shouldn't load anything
    run_dash -i
    assert_success
    assert_nothing_ran
}

@test 'run login' {
    # Non-interactive, login - should load environment variables
    run_dash -l
    assert_success
    assert_ran
}

@test 'run interactive-login' {
    # Interactive, login - should load environment variables
    run_dash -i -l
    assert_success
    assert_ran
}

@test 'can-sudo nonexistent' {
    run_dash -l
    assert_success
    assert_ran
    assert_line "DOTFILES_CAN_SUDO="
}

@test 'can-sudo empty' {
    touch "${TMP_DIR}/.dotfiles-can-sudo"
    run_dash -l
    assert_success
    assert_ran
    assert_line "DOTFILES_CAN_SUDO="
}

@test 'can-sudo non-empty' {
    echo 'y' >"${TMP_DIR}/.dotfiles-can-sudo"
    run_dash -l
    assert_success
    assert_ran
    assert_line "DOTFILES_CAN_SUDO=y"
}

@test 'needs-logout nonexistent' {
    run_dash -l
    assert_success
    assert_ran
    assert [ ! -e "${TEST_NEEDS_LOGOUT}" ]
}

@test 'needs-logout existing' {
    touch "${TEST_NEEDS_LOGOUT}"
    run_dash -l
    assert_success
    assert_ran
    assert [ ! -e "${TEST_NEEDS_LOGOUT}" ]
}

@test 'package-messages nonexistent' {
    run_dash -l
    assert_success
    assert_ran
    assert [ ! -e "${TEST_PACKAGE_MESSAGES}" ]
}

@test 'package-messages existing' {
    touch "${TEST_PACKAGE_MESSAGES}"
    run_dash -l
    assert_success
    assert_ran
    assert [ ! -e "${TEST_PACKAGE_MESSAGES}" ]
}

@test 'package-problems nonexistent' {
    run_dash -l
    assert_success
    assert_ran
    assert [ ! -e "${TEST_PACKAGE_PROBLEMS}" ]
}

@test 'package-problems existing' {
    touch "${TEST_PACKAGE_PROBLEMS}"
    run_dash -l
    assert_success
    assert_ran
    assert [ ! -e "${TEST_PACKAGE_PROBLEMS}" ]
}

@test 'next-login nonexistent' {
    run_dash -l
    assert_success
    assert_ran
    assert [ ! -e "${TEST_NEXT_LOGIN}" ]
    refute_line 'TEST_NEXT_LOGIN'
}

@test 'next-login existing' {
    echo 'echo TEST_NEXT_LOGIN' >"${TEST_NEXT_LOGIN}"
    run_dash -l
    assert_success
    assert_ran
    assert [ ! -e "${TEST_NEXT_LOGIN}" ]
    assert_line 'TEST_NEXT_LOGIN'
}
