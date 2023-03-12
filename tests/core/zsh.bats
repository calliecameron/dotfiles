#!/usr/bin/env bats

setup() {
    bats_load_library 'bats-support'
    bats_load_library 'bats-assert'
    load 'helpers.bash'

    setup_common
    INSTALL="${THIS_DIR}/../../install.sh"
    HOME="${TMP_DIR}" "${INSTALL}" >/dev/null
}

teardown() {
    teardown_common
}

function assert_ran() {
    assert_stub_ran
    assert_package_env_run_by "${1}"
    assert_package_generic_aliases_run_by 'zsh'
    assert_not_package_bash_aliases_run
    assert_package_zsh_aliases_run_by 'zsh'
    assert_local_env_run_by "${1}"
    assert_local_generic_aliases_run_by 'zsh'
    assert_not_local_bash_aliases_run
    assert_local_zsh_aliases_run_by 'zsh'
    assert_path
    assert_package_roots
}

function assert_ran_env_only() {
    assert_stub_ran
    assert_package_env_run_by "${1}"
    assert_not_package_generic_aliases_run
    assert_not_package_bash_aliases_run
    assert_not_package_zsh_aliases_run
    assert_local_env_run_by "${1}"
    assert_not_local_generic_aliases_run
    assert_not_local_bash_aliases_run
    assert_not_local_zsh_aliases_run
    assert_path
    assert_package_roots
}

@test 'run simple' {
    # Non-interactive, non-login - shouldn't load anything
    run_zsh
    assert_success
    assert_nothing_ran
}

@test 'run interactive' {
    # Interactive, non-login - should load everything
    run_zsh -i
    assert_success
    assert_ran 'zsh'
}

@test 'run login' {
    # Non-interactive, login - should load env
    run_zsh -l
    assert_success
    assert_ran_env_only 'zsh'
}

@test 'run interactive-login' {
    # Interactive, login - should load everything
    run_zsh -i -l
    assert_success
    assert_ran 'zsh'
}

@test 'run simple after login' {
    # Non-interactive, non-login - should load env
    run_zsh_after_login
    assert_success
    assert_ran_env_only 'dash'
}

@test 'run interactive after login' {
    # Interactive, non-login - should load everything
    run_zsh_after_login -i
    assert_success
    assert_ran 'dash'
}

@test 'run login after login' {
    # Non-interactive, login - should load env
    run_zsh_after_login -l
    assert_success
    assert_ran_env_only 'dash'
}

@test 'run interactive-login after login' {
    # Interactive, login - should load everything
    run_zsh_after_login -i -l
    assert_success
    assert_ran 'dash'
}

@test 'can-sudo nonexistent' {
    run_zsh -i
    assert_success
    assert_ran 'zsh'
    assert_not_can_sudo
}

@test 'can-sudo empty' {
    touch "${TEST_CAN_SUDO}"
    run_zsh -i
    assert_success
    assert_ran 'zsh'
    assert_not_can_sudo
}

@test 'can-sudo non-empty' {
    echo 'y' >"${TEST_CAN_SUDO}"
    run_zsh -i
    assert_success
    assert_ran 'zsh'
    assert_can_sudo
}

@test 'needs-logout nonexistent' {
    run_zsh_after_login -i
    assert_success
    assert_ran 'dash'
    assert [ ! -e "${TEST_NEEDS_LOGOUT}" ]
    refute_line --partial 'Log out and log in again to set everything up correctly.'
}

@test 'needs-logout existing' {
    run_between_login_and_zsh "touch '${TEST_NEEDS_LOGOUT}'" -i
    assert_success
    assert_ran 'dash'
    assert [ -e "${TEST_NEEDS_LOGOUT}" ]
    assert_line --partial 'Log out and log in again to set everything up correctly.'
}

@test 'package-messages nonexistent' {
    run_zsh_after_login -i
    assert_success
    assert_ran 'dash'
    assert [ ! -e "${TEST_PACKAGE_MESSAGES}" ]
    refute_line --partial 'test package message'
}

@test 'package-messages existing' {
    run_between_login_and_zsh "echo 'test package message' > '${TEST_PACKAGE_MESSAGES}'" -i
    assert_success
    assert_ran 'dash'
    assert [ -e "${TEST_PACKAGE_MESSAGES}" ]
    assert_line --partial 'test package message'
}

@test 'package-problems nonexistent' {
    run_zsh_after_login -i
    assert_success
    assert_ran 'dash'
    assert [ ! -e "${TEST_PACKAGE_PROBLEMS}" ]
    refute_line --partial 'test package problem'
}

@test 'package-problems existing' {
    run_between_login_and_zsh "echo 'test package problem' > '${TEST_PACKAGE_PROBLEMS}'" -i
    assert_success
    assert_ran 'dash'
    assert [ -e "${TEST_PACKAGE_PROBLEMS}" ]
    assert_line --partial 'test package problem'
}

@test 'next-login nonexistent' {
    run_zsh -l -i
    assert_success
    assert_ran 'zsh'
    assert [ ! -e "${TEST_NEXT_LOGIN}" ]
    refute_line 'TEST_NEXT_LOGIN'
}

@test 'next-login existing' {
    echo 'echo TEST_NEXT_LOGIN' >"${TEST_NEXT_LOGIN}"
    run_zsh -l -i
    assert_success
    assert_ran 'zsh'
    assert [ ! -e "${TEST_NEXT_LOGIN}" ]
    assert_line 'TEST_NEXT_LOGIN'
}

@test 'next-init nonexistent' {
    run_zsh -l -i
    assert_success
    assert_ran 'zsh'
    assert [ ! -e "${TEST_NEXT_INIT}" ]
    refute_line 'TEST_NEXT_INIT'
}

@test 'next-init existing' {
    echo 'echo TEST_NEXT_INIT' >"${TEST_NEXT_INIT}"
    run_zsh -l -i
    assert_success
    assert_ran 'zsh'
    assert [ ! -e "${TEST_NEXT_INIT}" ]
    assert_line 'TEST_NEXT_INIT'
}

@test 'check-init-file' {
    echo 'true' >>"${TMP_DIR}/.zlogin"
    run_zsh -i
    assert_success
    assert_ran 'zsh'
    assert_line --partial "Your .zlogin doesn't look right - maybe something has tampered with it"
    refute_line --partial "Your .profile doesn't look right - maybe something has tampered with it"
}