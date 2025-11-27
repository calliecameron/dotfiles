# shellcheck shell=bats
# bats file_tags=slow

setup() {
    bats_load_library 'bats-support'
    bats_load_library 'bats-assert'
    load 'helpers.bash'

    setup_common

    TEST_EMACS="$(command -v emacs)"
}

run_emacs() {
    run env -i -C "${BATS_TEST_TMPDIR}" HOME="${BATS_TEST_TMPDIR}" TERM='xterm-256color' dash -l -c "DOTFILES_EMACS_LOCK='${TEST_EMACS_LOCK_FILE}' '${TEST_EMACS}' -Q --script '${CORE_DIR}/dotfiles.el'" 3>&-
}

@test 'emacs' {
    run_emacs

    assert_success

    assert_package_emacs_run "${TEST_PACKAGE_ROOT_1}" 'foo'
    assert_not_package_emacs_run 'bar'
    assert_package_emacs_run "${TEST_PACKAGE_ROOT_1}" 'baz'
    assert_not_package_emacs_run 'quux'
    assert_not_package_emacs_run 'blah'
    assert_not_package_emacs_run 'yay'
    assert_not_package_emacs_run 'stuff'
    assert_not_package_emacs_run 'other'
    assert_not_package_emacs_run 'foo_bar'

    assert_local_emacs_run
    assert_emacs_load_path
}
