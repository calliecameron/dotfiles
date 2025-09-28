# shellcheck shell=bats
# bats file_tags=slow

setup() {
    bats_load_library 'bats-support'
    bats_load_library 'bats-assert'
    load 'helpers.bash'

    setup_common
}

function assert_ran() {
    assert_stub_ran

    assert_package_env_run_by "${TEST_PACKAGE_ROOT_1}" 'foo' 'dash'
    assert_not_package_env_run 'bar'
    assert_package_env_run_by "${TEST_PACKAGE_ROOT_1}" 'baz' 'dash'
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

    assert_local_env_run_by 'dash'
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

@test 'last-update-file existing' {
    local DATE
    DATE="$(date --date=yesterday '+%s')"
    echo "${DATE}" >"${TEST_PACKAGE_LAST_UPDATE_FILE}"
    run_dash -l
    assert_success
    assert_ran
    assert [ "$(cat "${TEST_PACKAGE_LAST_UPDATE_FILE}")" = "${DATE}" ]
}
