#!/usr/bin/env bats

setup() {
    bats_load_library 'bats-support'
    bats_load_library 'bats-assert'
    load 'helpers.bash'

    setup_common
}

teardown() {
    teardown_common
}

function assert_ran() {
    assert_stub_ran

    assert_package_env_run_by "${TEST_PACKAGE_ROOT_1}" 'foo' "${1}"
    assert_not_package_env_run 'bar'
    assert_package_env_run_by "${TEST_PACKAGE_ROOT_1}" 'baz' "${1}"
    assert_not_package_env_run 'quux'
    assert_not_package_env_run 'blah'
    assert_not_package_env_run 'yay'
    assert_not_package_env_run 'stuff'
    assert_not_package_env_run 'other'
    assert_not_package_env_run 'foo_bar'

    assert_package_generic_aliases_run_by "${TEST_PACKAGE_ROOT_1}" 'foo' 'bash'
    assert_not_package_generic_aliases_run 'bar'
    assert_package_generic_aliases_run_by "${TEST_PACKAGE_ROOT_1}" 'baz' 'bash'
    assert_not_package_generic_aliases_run 'quux'
    assert_not_package_generic_aliases_run 'blah'
    assert_not_package_generic_aliases_run 'yay'
    assert_not_package_generic_aliases_run 'stuff'
    assert_not_package_generic_aliases_run 'other'
    assert_not_package_generic_aliases_run 'foo_bar'

    assert_package_bash_aliases_run_by "${TEST_PACKAGE_ROOT_1}" 'foo' 'bash'
    assert_not_package_bash_aliases_run 'bar'
    assert_package_bash_aliases_run_by "${TEST_PACKAGE_ROOT_1}" 'baz' 'bash'
    assert_not_package_bash_aliases_run 'quux'
    assert_not_package_bash_aliases_run 'blah'
    assert_not_package_bash_aliases_run 'yay'
    assert_not_package_bash_aliases_run 'stuff'
    assert_not_package_bash_aliases_run 'other'
    assert_not_package_bash_aliases_run 'foo_bar'

    assert_not_package_zsh_aliases_run 'foo'
    assert_not_package_zsh_aliases_run 'bar'
    assert_not_package_zsh_aliases_run 'baz'
    assert_not_package_zsh_aliases_run 'quux'
    assert_not_package_zsh_aliases_run 'blah'
    assert_not_package_zsh_aliases_run 'yay'
    assert_not_package_zsh_aliases_run 'stuff'
    assert_not_package_zsh_aliases_run 'other'
    assert_not_package_zsh_aliases_run 'foo_bar'

    assert_not_package_emacs_run 'foo'
    assert_not_package_emacs_run 'bar'
    assert_not_package_emacs_run 'baz'
    assert_not_package_emacs_run 'quux'
    assert_not_package_emacs_run 'blah'
    assert_not_package_emacs_run 'yay'
    assert_not_package_emacs_run 'stuff'
    assert_not_package_emacs_run 'other'
    assert_not_package_emacs_run 'foo_bar'

    assert_local_env_run_by "${1}"
    assert_local_generic_aliases_run_by 'bash'
    assert_local_bash_aliases_run_by 'bash'
    assert_not_local_zsh_aliases_run
    assert_not_local_emacs_run
    assert_path
    assert_package_roots
    assert_nemo_scripts
    assert_not_zsh_completions
    assert_packages_available
    assert_last_update_file
}

function assert_ran_env_only() {
    assert_stub_ran

    assert_package_env_run_by "${TEST_PACKAGE_ROOT_1}" 'foo' "${1}"
    assert_not_package_env_run 'bar'
    assert_package_env_run_by "${TEST_PACKAGE_ROOT_1}" 'baz' "${1}"
    assert_not_package_env_run 'quux'
    assert_not_package_env_run 'blah'
    assert_not_package_env_run 'yay'
    assert_not_package_env_run 'stuff'
    assert_not_package_env_run 'other'
    assert_not_package_env_run 'foo_bar'

    assert_not_package_generic_aliases_run 'foo'
    assert_not_package_generic_aliases_run 'bar'
    assert_not_package_generic_aliases_run 'baz'
    assert_not_package_generic_aliases_run 'quux'
    assert_not_package_generic_aliases_run 'blah'
    assert_not_package_generic_aliases_run 'yay'
    assert_not_package_generic_aliases_run 'stuff'
    assert_not_package_generic_aliases_run 'other'
    assert_not_package_generic_aliases_run 'foo_bar'

    assert_not_package_bash_aliases_run 'foo'
    assert_not_package_bash_aliases_run 'bar'
    assert_not_package_bash_aliases_run 'baz'
    assert_not_package_bash_aliases_run 'quux'
    assert_not_package_bash_aliases_run 'blah'
    assert_not_package_bash_aliases_run 'yay'
    assert_not_package_bash_aliases_run 'stuff'
    assert_not_package_bash_aliases_run 'other'
    assert_not_package_bash_aliases_run 'foo_bar'

    assert_not_package_zsh_aliases_run 'foo'
    assert_not_package_zsh_aliases_run 'bar'
    assert_not_package_zsh_aliases_run 'baz'
    assert_not_package_zsh_aliases_run 'quux'
    assert_not_package_zsh_aliases_run 'blah'
    assert_not_package_zsh_aliases_run 'yay'
    assert_not_package_zsh_aliases_run 'stuff'
    assert_not_package_zsh_aliases_run 'other'
    assert_not_package_zsh_aliases_run 'foo_bar'

    assert_not_package_emacs_run 'foo'
    assert_not_package_emacs_run 'bar'
    assert_not_package_emacs_run 'baz'
    assert_not_package_emacs_run 'quux'
    assert_not_package_emacs_run 'blah'
    assert_not_package_emacs_run 'yay'
    assert_not_package_emacs_run 'stuff'
    assert_not_package_emacs_run 'other'
    assert_not_package_emacs_run 'foo_bar'

    assert_local_env_run_by "${1}"
    assert_not_local_generic_aliases_run
    assert_not_local_bash_aliases_run
    assert_not_local_zsh_aliases_run
    assert_not_local_emacs_run
    assert_path
    assert_package_roots
    assert_nemo_scripts
    assert_not_zsh_completions
    assert_not_packages_available
    assert_last_update_file
}

@test 'run simple' {
    # Non-interactive, non-login - shouldn't load anything
    run_bash
    assert_success
    assert_nothing_ran
}

@test 'run interactive' {
    # Interactive, non-login - should load everything
    run_bash -i
    assert_success
    assert_ran 'bash'
}

@test 'run login' {
    # Non-interactive, login - should load env
    run_bash -l
    assert_success
    assert_ran_env_only 'bash'
}

@test 'run interactive-login' {
    # Interactive, login - should load everything
    run_bash -i -l
    assert_success
    assert_ran 'bash'
}

@test 'run simple after login' {
    # Non-interactive, non-login - should load env
    run_bash_after_login
    assert_success
    assert_ran_env_only 'dash'
}

@test 'run interactive after login' {
    # Interactive, non-login - should load everything
    run_bash_after_login -i
    assert_success
    assert_ran 'dash'
}

@test 'run login after login' {
    # Non-interactive, login - should load env
    run_bash_after_login -l
    assert_success
    assert_ran_env_only 'dash'
}

@test 'run interactive-login after login' {
    # Interactive, login - should load everything
    run_bash_after_login -i -l
    assert_success
    assert_ran 'dash'
}

@test 'needs-logout nonexistent' {
    run_bash_after_login -i
    assert_success
    assert_ran 'dash'
    assert [ ! -e "${TEST_NEEDS_LOGOUT}" ]
    refute_line --partial 'Log out and log in again to set everything up correctly.'
}

@test 'needs-logout existing' {
    run_between_login_and_bash "touch '${TEST_NEEDS_LOGOUT}'" -i
    assert_success
    assert_ran 'dash'
    assert [ -e "${TEST_NEEDS_LOGOUT}" ]
    assert_line --partial 'Log out and log in again to set everything up correctly.'
}

@test 'package-messages nonexistent' {
    run_bash_after_login -i
    assert_success
    assert_ran 'dash'
    assert [ ! -e "${TEST_PACKAGE_MESSAGES}" ]
    refute_line --partial 'test package message'
}

@test 'package-messages existing' {
    run_between_login_and_bash "echo 'test package message' > '${TEST_PACKAGE_MESSAGES}'" -i
    assert_success
    assert_ran 'dash'
    assert [ -e "${TEST_PACKAGE_MESSAGES}" ]
    assert_line --partial 'test package message'
}

@test 'package-problems nonexistent' {
    run_bash_after_login -i
    assert_success
    assert_ran 'dash'
    assert [ ! -e "${TEST_PACKAGE_PROBLEMS}" ]
    refute_line --partial 'test package problem'
}

@test 'package-problems existing' {
    run_between_login_and_bash "echo 'test package problem' > '${TEST_PACKAGE_PROBLEMS}'" -i
    assert_success
    assert_ran 'dash'
    assert [ -e "${TEST_PACKAGE_PROBLEMS}" ]
    assert_line --partial 'test package problem'
}

@test 'next-login nonexistent' {
    run_bash -l -i
    assert_success
    assert_ran 'bash'
    assert [ ! -e "${TEST_NEXT_LOGIN}" ]
    refute_line 'TEST_NEXT_LOGIN'
}

@test 'next-login existing' {
    echo 'echo TEST_NEXT_LOGIN' >"${TEST_NEXT_LOGIN}"
    run_bash -l -i
    assert_success
    assert_ran 'bash'
    assert [ ! -e "${TEST_NEXT_LOGIN}" ]
    assert_line 'TEST_NEXT_LOGIN'
}

@test 'next-init nonexistent' {
    run_bash -l -i
    assert_success
    assert_ran 'bash'
    assert [ ! -e "${TEST_NEXT_INIT}" ]
    refute_line 'TEST_NEXT_INIT'
}

@test 'next-init existing' {
    echo 'echo TEST_NEXT_INIT' >"${TEST_NEXT_INIT}"
    run_bash -l -i
    assert_success
    assert_ran 'bash'
    assert [ ! -e "${TEST_NEXT_INIT}" ]
    assert_line 'TEST_NEXT_INIT'
}

@test 'check-init-file' {
    echo 'true' >>"${TMP_DIR}/.zlogin"
    run_bash -i
    assert_success
    assert_ran 'bash'
    assert_line --partial "Your .zlogin doesn't look right - maybe something has tampered with it"
    refute_line --partial "Your .profile doesn't look right - maybe something has tampered with it"
}

@test 'package-mutex nonexistent' {
    run_bash -l -i
    assert_success
    assert_ran 'bash'
    refute_line --partial 'Another script is installing or updating packages'
    assert [ ! -e "${TEST_PACKAGE_MUTEX}" ]
}

@test 'package-mutex existing' {
    mkdir -p "${TEST_PACKAGE_MUTEX}"
    run_bash -l -i
    assert_success
    assert_ran 'bash'
    assert_line --partial 'Another script is installing or updating packages'
    assert [ -d "${TEST_PACKAGE_MUTEX}" ]
}

@test 'private-repo undefined' {
    run_bash -l -i
    assert_success
    assert_ran 'bash'
    refute_line --partial 'A private repo is specified'
    assert [ ! -e "${TEST_PRIVATE_DIR}" ]
}

@test 'private-repo nonexistent' {
    echo 'export DOTFILES_PRIVATE_REPO=foo' >>"${TEST_LOCAL_ENV_FILE}"
    run_bash -l -i
    assert_success
    assert_ran 'bash'
    assert_line --partial 'A private repo is specified'
    assert [ ! -e "${TEST_PRIVATE_DIR}" ]
}

@test 'private-repo existing' {
    echo 'export DOTFILES_PRIVATE_REPO=foo' >>"${TEST_LOCAL_ENV_FILE}"
    mkdir -p "${TEST_PRIVATE_DIR}"
    run_bash -l -i
    assert_success
    assert_ran 'bash'
    refute_line --partial 'A private repo is specified'
    assert [ -d "${TEST_PRIVATE_DIR}" ]
}

@test 'last-update-file existing' {
    local DATE
    DATE="$(date --date=yesterday '+%s')"
    echo "${DATE}" >"${TEST_PACKAGE_LAST_UPDATE_FILE}"
    run_bash -l -i
    assert_success
    assert_ran 'bash'
    assert [ "$(cat "${TEST_PACKAGE_LAST_UPDATE_FILE}")" = "${DATE}" ]
}

@test 'check-for-updates no updates' {
    run_bash -l -i
    assert_success
    assert_ran 'bash'
    refute_line --partial "It's been a while since packages were checked for updates"
}

@test 'check-for-updates updates' {
    local DATE
    DATE="$(date '--date=last year' '+%s')"
    echo "${DATE}" >"${TEST_PACKAGE_LAST_UPDATE_FILE}"
    run_bash -l -i
    assert_success
    assert_ran 'bash'
    assert_line --partial "It's been a while since packages were checked for updates"
}
