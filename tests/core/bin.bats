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

@test 'dotfiles-repo-is-clean usage' {
    run_script "${BIN_DIR}/dotfiles-repo-is-clean"
    assert_failure
    assert_line --partial 'Usage:'
}

@test 'dotfiles-repo-is-clean not a dir' {
    local REPO="${TMP_DIR}/a"
    touch "${REPO}"
    run_script "${BIN_DIR}/dotfiles-repo-is-clean" "${REPO}"
    assert_success
    refute_line --partial 'Usage:'
}

@test 'dotfiles-repo-is-clean not a repo' {
    local REPO="${TMP_DIR}/a"
    mkdir -p "${REPO}"
    run_script "${BIN_DIR}/dotfiles-repo-is-clean" "${REPO}"
    assert_success
    refute_line --partial 'Usage:'
}

@test 'dotfiles-repo-is-clean empty repo' {
    local REPO="${TMP_DIR}/a"
    mkdir -p "${REPO}"
    (cd "${REPO}" && git init .)
    run_script "${BIN_DIR}/dotfiles-repo-is-clean" "${REPO}"
    assert_success
    refute_line --partial 'Usage:'
}

@test 'dotfiles-repo-is-clean untracked file' {
    local REPO="${TMP_DIR}/a"
    mkdir -p "${REPO}"
    (cd "${REPO}" && git init .)
    touch "${REPO}/a"
    run_script "${BIN_DIR}/dotfiles-repo-is-clean" "${REPO}"
    assert_failure
    refute_line --partial 'Usage:'
}

@test 'dotfiles-repo-is-clean clean no upstream' {
    local REPO="${TMP_DIR}/a"
    mkdir -p "${REPO}"
    (cd "${REPO}" && git init .)
    touch "${REPO}/a"
    (cd "${REPO}" && git add a && git commit -m 'Foo')
    run_script "${BIN_DIR}/dotfiles-repo-is-clean" "${REPO}"
    assert_success
    refute_line --partial 'Usage:'
}

@test 'dotfiles-repo-is-clean modified file' {
    local REPO="${TMP_DIR}/a"
    mkdir -p "${REPO}"
    (cd "${REPO}" && git init .)
    touch "${REPO}/a"
    (cd "${REPO}" && git add a && git commit -m 'Foo')
    echo 'a' >"${REPO}/a"
    run_script "${BIN_DIR}/dotfiles-repo-is-clean" "${REPO}"
    assert_failure
    refute_line --partial 'Usage:'
}

@test 'dotfiles-repo-is-clean unpushed commit' {
    local UPSTREAM="${TMP_DIR}/a"
    local REPO="${TMP_DIR}/b"
    mkdir -p "${UPSTREAM}"
    (cd "${UPSTREAM}" && git init --bare .)
    git clone "${UPSTREAM}" "${REPO}"
    touch "${REPO}/a"
    (cd "${REPO}" && git add a && git commit -m 'Foo' && git push)
    echo 'a' >"${REPO}/a"
    (cd "${REPO}" && git add a && git commit -m 'Bar')
    run_script "${BIN_DIR}/dotfiles-repo-is-clean" "${REPO}"
    assert_failure
    refute_line --partial 'Usage:'
}

@test 'dotfiles-repo-is-clean pushed commit' {
    local UPSTREAM="${TMP_DIR}/a"
    local REPO="${TMP_DIR}/b"
    mkdir -p "${UPSTREAM}"
    (cd "${UPSTREAM}" && git init --bare .)
    git clone "${UPSTREAM}" "${REPO}"
    touch "${REPO}/a"
    (cd "${REPO}" && git add a && git commit -m 'Foo' && git push)
    echo 'a' >"${REPO}/a"
    (cd "${REPO}" && git add a && git commit -m 'Bar' && git push)
    run_script "${BIN_DIR}/dotfiles-repo-is-clean" "${REPO}"
    assert_success
    refute_line --partial 'Usage:'
}

@test 'dotfiles-echo-colour no args' {
    run_script "${BIN_DIR}/dotfiles-echo-colour"
    assert_failure
    assert_line --partial 'Usage:'
}

@test 'dotfiles-echo-colour one arg' {
    run_script "${BIN_DIR}/dotfiles-echo-colour" '31'
    assert_failure
    assert_line --partial 'Usage:'
}

@test 'dotfiles-echo-colour success' {
    run_script "${BIN_DIR}/dotfiles-echo-colour" '31' 'foo' 'bar baz'
    assert_success
    refute_line --partial 'Usage:'
    assert_line "$(echo -e "\e[31mfoo bar baz\e[0m")"
}

@test 'dotfiles-echo-red usage' {
    run_script PATH="${BIN_DIR}:${PATH}" "${BIN_DIR}/dotfiles-echo-red"
    assert_failure
    assert_line --partial 'Usage:'
}

@test 'dotfiles-echo-red success' {
    run_script PATH="${BIN_DIR}:${PATH}" "${BIN_DIR}/dotfiles-echo-red" 'foo' 'bar baz'
    assert_success
    refute_line --partial 'Usage:'
    assert_line "$(echo -e "\e[31mfoo bar baz\e[0m")"
}

@test 'dotfiles-echo-green usage' {
    run_script PATH="${BIN_DIR}:${PATH}" "${BIN_DIR}/dotfiles-echo-green"
    assert_failure
    assert_line --partial 'Usage:'
}

@test 'dotfiles-echo-green success' {
    run_script PATH="${BIN_DIR}:${PATH}" "${BIN_DIR}/dotfiles-echo-green" 'foo' 'bar baz'
    assert_success
    refute_line --partial 'Usage:'
    assert_line "$(echo -e "\e[32mfoo bar baz\e[0m")"
}

@test 'dotfiles-echo-yellow usage' {
    run_script PATH="${BIN_DIR}:${PATH}" "${BIN_DIR}/dotfiles-echo-yellow"
    assert_failure
    assert_line --partial 'Usage:'
}

@test 'dotfiles-echo-yellow success' {
    run_script PATH="${BIN_DIR}:${PATH}" "${BIN_DIR}/dotfiles-echo-yellow" 'foo' 'bar baz'
    assert_success
    refute_line --partial 'Usage:'
    assert_line "$(echo -e "\e[33mfoo bar baz\e[0m")"
}

@test 'dotfiles-echo-blue usage' {
    run_script PATH="${BIN_DIR}:${PATH}" "${BIN_DIR}/dotfiles-echo-blue"
    assert_failure
    assert_line --partial 'Usage:'
}

@test 'dotfiles-echo-blue success' {
    run_script PATH="${BIN_DIR}:${PATH}" "${BIN_DIR}/dotfiles-echo-blue" 'foo' 'bar baz'
    assert_success
    refute_line --partial 'Usage:'
    assert_line "$(echo -e "\e[34mfoo bar baz\e[0m")"
}

@test 'dotfiles-yn-y nothing' {
    run_script "${BIN_DIR}/dotfiles-yn-y" 'foo' <<<''
    assert_success
}

@test 'dotfiles-yn-y y' {
    run_script "${BIN_DIR}/dotfiles-yn-y" 'foo' <<<'y'
    assert_success
}

@test 'dotfiles-yn-y Y' {
    run_script "${BIN_DIR}/dotfiles-yn-y" 'foo' <<<'Y'
    assert_success
}

@test 'dotfiles-yn-y n' {
    run_script "${BIN_DIR}/dotfiles-yn-y" 'foo' <<<'n'
    assert_failure
}

@test 'dotfiles-yn-y N' {
    run_script "${BIN_DIR}/dotfiles-yn-y" 'foo' <<<'N'
    assert_failure
}

@test 'dotfiles-yn-y other' {
    run_script "${BIN_DIR}/dotfiles-yn-y" 'foo' <<<'foo'
    assert_success
}

@test 'dotfiles-yn-n nothing' {
    run_script "${BIN_DIR}/dotfiles-yn-n" 'foo' <<<''
    assert_failure
}

@test 'dotfiles-yn-n y' {
    run_script "${BIN_DIR}/dotfiles-yn-n" 'foo' <<<'y'
    assert_success
}

@test 'dotfiles-yn-n Y' {
    run_script "${BIN_DIR}/dotfiles-yn-n" 'foo' <<<'Y'
    assert_success
}

@test 'dotfiles-yn-n n' {
    run_script "${BIN_DIR}/dotfiles-yn-n" 'foo' <<<'n'
    assert_failure
}

@test 'dotfiles-yn-n N' {
    run_script "${BIN_DIR}/dotfiles-yn-n" 'foo' <<<'N'
    assert_failure
}

@test 'dotfiles-yn-n other' {
    run_script "${BIN_DIR}/dotfiles-yn-n" 'foo' <<<'foo'
    assert_failure
}

@test 'dotfiles-in-list no args' {
    run_script "${BIN_DIR}/dotfiles-in-list"
    assert_failure
    assert_line --partial 'Usage:'
}

@test 'dotfiles-in-list one arg' {
    run_script "${BIN_DIR}/dotfiles-in-list" 'foo'
    assert_failure
    assert_line --partial 'Usage:'
}

@test 'dotfiles-in-list empty list' {
    run_script "${BIN_DIR}/dotfiles-in-list" '' 'foo'
    assert_failure
    refute_line --partial 'Usage:'
}

@test 'dotfiles-in-list simple list present' {
    run_script "${BIN_DIR}/dotfiles-in-list" 'foo' 'foo'
    assert_success
    refute_line --partial 'Usage:'
}

@test 'dotfiles-in-list simple list absent' {
    run_script "${BIN_DIR}/dotfiles-in-list" 'foo' 'bar'
    assert_failure
    refute_line --partial 'Usage:'
}

@test 'dotfiles-in-list complex list present' {
    run_script "${BIN_DIR}/dotfiles-in-list" 'foo:bar:baz' 'bar'
    assert_success
    refute_line --partial 'Usage:'
}

@test 'dotfiles-in-list complex list absent' {
    run_script "${BIN_DIR}/dotfiles-in-list" 'foo:bar:baz' 'quux'
    assert_failure
    refute_line --partial 'Usage:'
}
