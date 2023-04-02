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
    run_script "PATH=${BIN_DIR}:${PATH}" "${BIN_DIR}/dotfiles-echo-red"
    assert_failure
    assert_line --partial 'Usage:'
}

@test 'dotfiles-echo-red success' {
    run_script "PATH=${BIN_DIR}:${PATH}" "${BIN_DIR}/dotfiles-echo-red" 'foo' 'bar baz'
    assert_success
    refute_line --partial 'Usage:'
    assert_line "$(echo -e "\e[31mfoo bar baz\e[0m")"
}

@test 'dotfiles-echo-green usage' {
    run_script "PATH=${BIN_DIR}:${PATH}" "${BIN_DIR}/dotfiles-echo-green"
    assert_failure
    assert_line --partial 'Usage:'
}

@test 'dotfiles-echo-green success' {
    run_script "PATH=${BIN_DIR}:${PATH}" "${BIN_DIR}/dotfiles-echo-green" 'foo' 'bar baz'
    assert_success
    refute_line --partial 'Usage:'
    assert_line "$(echo -e "\e[32mfoo bar baz\e[0m")"
}

@test 'dotfiles-echo-yellow usage' {
    run_script "PATH=${BIN_DIR}:${PATH}" "${BIN_DIR}/dotfiles-echo-yellow"
    assert_failure
    assert_line --partial 'Usage:'
}

@test 'dotfiles-echo-yellow success' {
    run_script "PATH=${BIN_DIR}:${PATH}" "${BIN_DIR}/dotfiles-echo-yellow" 'foo' 'bar baz'
    assert_success
    refute_line --partial 'Usage:'
    assert_line "$(echo -e "\e[33mfoo bar baz\e[0m")"
}

@test 'dotfiles-echo-blue usage' {
    run_script "PATH=${BIN_DIR}:${PATH}" "${BIN_DIR}/dotfiles-echo-blue"
    assert_failure
    assert_line --partial 'Usage:'
}

@test 'dotfiles-echo-blue success' {
    run_script "PATH=${BIN_DIR}:${PATH}" "${BIN_DIR}/dotfiles-echo-blue" 'foo' 'bar baz'
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

@test 'dotfiles-linux-variant none' {
    run_script "${BIN_DIR}/dotfiles-linux-variant" 'foo' 'bar'
    assert_failure
}

@test 'dotfiles-linux-variant success' {
    run_script 'DOTFILES_LINUX_VARIANT=foo' "${BIN_DIR}/dotfiles-linux-variant" 'foo' 'bar'
    assert_success
}

@test 'dotfiles-linux-variant failure' {
    run_script 'DOTFILES_LINUX_VARIANT=baz' "${BIN_DIR}/dotfiles-linux-variant" 'foo' 'bar'
    assert_failure
}

@test 'dotfiles-known-linux-variant success' {
    run_script 'DOTFILES_LINUX_VARIANT=foo' "${BIN_DIR}/dotfiles-known-linux-variant"
    assert_success
}

@test 'dotfiles-known-linux-variant failure' {
    run_script "${BIN_DIR}/dotfiles-known-linux-variant"
    assert_failure
}

@test 'dotfiles-is-graphical success' {
    run_script 'DISPLAY=foo' "${BIN_DIR}/dotfiles-is-graphical"
    assert_success
}

@test 'dotfiles-is-graphical failure' {
    run_script "${BIN_DIR}/dotfiles-is-graphical"
    assert_failure
}

@test 'dotfiles-symlink-dir-contents no args' {
    run_script "${BIN_DIR}/dotfiles-symlink-dir-contents"
    assert_failure
    assert_line --partial 'Usage:'
}

@test 'dotfiles-symlink-dir-contents one args' {
    run_script "${BIN_DIR}/dotfiles-symlink-dir-contents" 'a'
    assert_failure
    assert_line --partial 'Usage:'
}

@test 'dotfiles-symlink-dir-contents nonexistent link dir' {
    mkdir -p "${TMP_DIR}/a"
    touch "${TMP_DIR}/a/foo"
    run_script "${BIN_DIR}/dotfiles-symlink-dir-contents" "${TMP_DIR}/a" "${TMP_DIR}/b"
    assert_success
    refute_line --partial 'Usage:'
    assert [ ! -d "${TMP_DIR}/b" ]
}

@test 'dotfiles-symlink-dir-contents nonexistent file dir' {
    mkdir -p "${TMP_DIR}/b"
    run_script "${BIN_DIR}/dotfiles-symlink-dir-contents" "${TMP_DIR}/a" "${TMP_DIR}/b"
    assert_success
    refute_line --partial 'Usage:'
    assert [ "$(ls -1 "${TMP_DIR}/b" | wc -l)" = '0' ]
}

@test 'dotfiles-symlink-dir-contents success' {
    mkdir -p "${TMP_DIR}/a"
    touch "${TMP_DIR}/a/foo"
    touch "${TMP_DIR}/a/bar"
    touch "${TMP_DIR}/a/baz"
    mkdir -p "${TMP_DIR}/b"
    touch "${TMP_DIR}/b/bar"
    run_script "${BIN_DIR}/dotfiles-symlink-dir-contents" "${TMP_DIR}/a" "${TMP_DIR}/b"
    assert_success
    refute_line --partial 'Usage:'
    assert [ -h "${TMP_DIR}/b/foo" ]
    assert [ "$(readlink -f "${TMP_DIR}/b/foo")" = "${TMP_DIR}/a/foo" ]
    assert [ -f "${TMP_DIR}/b/bar" ]
    assert [ -h "${TMP_DIR}/b/baz" ]
    assert [ "$(readlink -f "${TMP_DIR}/b/baz")" = "${TMP_DIR}/a/baz" ]
    assert [ "$(ls -1 "${TMP_DIR}/b" | wc -l)" = '3' ]
}

@test 'dotfiles-log-package-message empty' {
    run_script "DOTFILES_PACKAGE_MESSAGES_FILE=${TMP_DIR}/a" "${BIN_DIR}/dotfiles-log-package-message" 'foo' 'bar baz'
    assert_success
    assert [ "$(cat "${TMP_DIR}/a")" = 'foo bar baz' ]
}

@test 'dotfiles-log-package-message existing' {
    echo 'foo' >"${TMP_DIR}/a"
    run_script "DOTFILES_PACKAGE_MESSAGES_FILE=${TMP_DIR}/a" "${BIN_DIR}/dotfiles-log-package-message" 'foo' 'bar baz'
    assert_success
    assert [ "$(cat "${TMP_DIR}/a")" = "$(printf 'foo\nfoo bar baz')" ]
}

@test 'dotfiles-log-package-problem empty' {
    run_script "DOTFILES_PACKAGE_PROBLEMS_FILE=${TMP_DIR}/a" "${BIN_DIR}/dotfiles-log-package-problem" 'foo' 'bar baz'
    assert_success
    assert [ "$(cat "${TMP_DIR}/a")" = 'foo bar baz' ]
}

@test 'dotfiles-log-package-problem existing' {
    echo 'foo' >"${TMP_DIR}/a"
    run_script "DOTFILES_PACKAGE_PROBLEMS_FILE=${TMP_DIR}/a" "${BIN_DIR}/dotfiles-log-package-problem" 'foo' 'bar baz'
    assert_success
    assert [ "$(cat "${TMP_DIR}/a")" = "$(printf 'foo\nfoo bar baz')" ]
}

@test 'dotfiles-home-link usage' {
    run_script "${BIN_DIR}/dotfiles-home-link"
    assert_failure
    assert_line --partial 'Usage:'
}

@test 'dotfiles-home-link nonexistent source' {
    run_script "PATH=${BIN_DIR}:${PATH}" "DOTFILES_PACKAGE_PROBLEMS_FILE=${TMP_DIR}/problems" "${BIN_DIR}/dotfiles-home-link" "${TMP_DIR}/a"
    assert_success
    refute_line --partial 'Usage:'
    assert [ ! -e "${TMP_DIR}/.a" ]
    assert [ ! -e "${TMP_DIR}/problems" ]
}

@test 'dotfiles-home-link one arg file nonexistent' {
    mkdir -p "${TMP_DIR}/src"
    touch "${TMP_DIR}/src/a"
    run_script "PATH=${BIN_DIR}:${PATH}" "DOTFILES_PACKAGE_PROBLEMS_FILE=${TMP_DIR}/problems" "${BIN_DIR}/dotfiles-home-link" "${TMP_DIR}/src/a"
    assert_success
    refute_line --partial 'Usage:'
    assert [ -h "${TMP_DIR}/.a" ]
    assert [ "$(readlink -f "${TMP_DIR}/.a")" = "${TMP_DIR}/src/a" ]
    assert [ ! -e "${TMP_DIR}/problems" ]
}

@test 'dotfiles-home-link one arg file correct symlink' {
    mkdir -p "${TMP_DIR}/src"
    touch "${TMP_DIR}/src/a"
    ln -s "${TMP_DIR}/src/a" "${TMP_DIR}/.a"
    run_script "PATH=${BIN_DIR}:${PATH}" "DOTFILES_PACKAGE_PROBLEMS_FILE=${TMP_DIR}/problems" "${BIN_DIR}/dotfiles-home-link" "${TMP_DIR}/src/a"
    assert_success
    refute_line --partial 'Usage:'
    assert [ -h "${TMP_DIR}/.a" ]
    assert [ "$(readlink -f "${TMP_DIR}/.a")" = "${TMP_DIR}/src/a" ]
    assert [ ! -e "${TMP_DIR}/problems" ]
}

@test 'dotfiles-home-link one arg file existing file' {
    mkdir -p "${TMP_DIR}/src"
    touch "${TMP_DIR}/src/a"
    touch "${TMP_DIR}/.a"
    run_script "PATH=${BIN_DIR}:${PATH}" "DOTFILES_PACKAGE_PROBLEMS_FILE=${TMP_DIR}/problems" "${BIN_DIR}/dotfiles-home-link" "${TMP_DIR}/src/a"
    assert_failure
    refute_line --partial 'Usage:'
    assert [ -f "${TMP_DIR}/.a" ]
    assert [ "$(wc -l <"${TMP_DIR}/problems")" = '1' ]
}

@test 'dotfiles-home-link one arg file incorrect symlink' {
    mkdir -p "${TMP_DIR}/src"
    touch "${TMP_DIR}/src/a"
    touch "${TMP_DIR}/src/b"
    ln -s "${TMP_DIR}/src/b" "${TMP_DIR}/.a"
    run_script "PATH=${BIN_DIR}:${PATH}" "DOTFILES_PACKAGE_PROBLEMS_FILE=${TMP_DIR}/problems" "${BIN_DIR}/dotfiles-home-link" "${TMP_DIR}/src/a"
    assert_failure
    refute_line --partial 'Usage:'
    assert [ -h "${TMP_DIR}/.a" ]
    assert [ "$(readlink -f "${TMP_DIR}/.a")" = "${TMP_DIR}/src/b" ]
    assert [ "$(wc -l <"${TMP_DIR}/problems")" = '1' ]
}

@test 'dotfiles-home-link one arg dir nonexistent' {
    mkdir -p "${TMP_DIR}/src/a"
    run_script "PATH=${BIN_DIR}:${PATH}" "DOTFILES_PACKAGE_PROBLEMS_FILE=${TMP_DIR}/problems" "${BIN_DIR}/dotfiles-home-link" "${TMP_DIR}/src/a"
    assert_success
    refute_line --partial 'Usage:'
    assert [ -h "${TMP_DIR}/a" ]
    assert [ "$(readlink -f "${TMP_DIR}/a")" = "${TMP_DIR}/src/a" ]
    assert [ ! -e "${TMP_DIR}/problems" ]
}

@test 'dotfiles-home-link one arg dir correct symlink' {
    mkdir -p "${TMP_DIR}/src/a"
    ln -s "${TMP_DIR}/src/a" "${TMP_DIR}/a"
    run_script "PATH=${BIN_DIR}:${PATH}" "DOTFILES_PACKAGE_PROBLEMS_FILE=${TMP_DIR}/problems" "${BIN_DIR}/dotfiles-home-link" "${TMP_DIR}/src/a"
    assert_success
    refute_line --partial 'Usage:'
    assert [ -h "${TMP_DIR}/a" ]
    assert [ "$(readlink -f "${TMP_DIR}/a")" = "${TMP_DIR}/src/a" ]
    assert [ ! -e "${TMP_DIR}/problems" ]
}

@test 'dotfiles-home-link one arg dir existing dir' {
    mkdir -p "${TMP_DIR}/src/a"
    mkdir -p "${TMP_DIR}/a"
    run_script "PATH=${BIN_DIR}:${PATH}" "DOTFILES_PACKAGE_PROBLEMS_FILE=${TMP_DIR}/problems" "${BIN_DIR}/dotfiles-home-link" "${TMP_DIR}/src/a"
    assert_failure
    refute_line --partial 'Usage:'
    assert [ -d "${TMP_DIR}/a" ]
    assert [ "$(wc -l <"${TMP_DIR}/problems")" = '1' ]
}

@test 'dotfiles-home-link one arg dir incorrect symlink' {
    mkdir -p "${TMP_DIR}/src/a"
    mkdir -p "${TMP_DIR}/src/b"
    ln -s "${TMP_DIR}/src/b" "${TMP_DIR}/a"
    run_script "PATH=${BIN_DIR}:${PATH}" "DOTFILES_PACKAGE_PROBLEMS_FILE=${TMP_DIR}/problems" "${BIN_DIR}/dotfiles-home-link" "${TMP_DIR}/src/a"
    assert_failure
    refute_line --partial 'Usage:'
    assert [ -h "${TMP_DIR}/a" ]
    assert [ "$(readlink -f "${TMP_DIR}/a")" = "${TMP_DIR}/src/b" ]
    assert [ "$(wc -l <"${TMP_DIR}/problems")" = '1' ]
}

@test 'dotfiles-home-link two args nonexistent' {
    mkdir -p "${TMP_DIR}/src"
    touch "${TMP_DIR}/src/a"
    run_script "PATH=${BIN_DIR}:${PATH}" "DOTFILES_PACKAGE_PROBLEMS_FILE=${TMP_DIR}/problems" "${BIN_DIR}/dotfiles-home-link" "${TMP_DIR}/src/a" "${TMP_DIR}/b/a"
    assert_success
    refute_line --partial 'Usage:'
    assert [ -h "${TMP_DIR}/b/a" ]
    assert [ "$(readlink -f "${TMP_DIR}/b/a")" = "${TMP_DIR}/src/a" ]
    assert [ ! -e "${TMP_DIR}/problems" ]
}

@test 'dotfiles-home-link two args correct symlink' {
    mkdir -p "${TMP_DIR}/src"
    touch "${TMP_DIR}/src/a"
    mkdir "${TMP_DIR}/b"
    ln -s "${TMP_DIR}/src/a" "${TMP_DIR}/b/a"
    run_script "PATH=${BIN_DIR}:${PATH}" "DOTFILES_PACKAGE_PROBLEMS_FILE=${TMP_DIR}/problems" "${BIN_DIR}/dotfiles-home-link" "${TMP_DIR}/src/a" "${TMP_DIR}/b/a"
    assert_success
    refute_line --partial 'Usage:'
    assert [ -h "${TMP_DIR}/b/a" ]
    assert [ "$(readlink -f "${TMP_DIR}/b/a")" = "${TMP_DIR}/src/a" ]
    assert [ ! -e "${TMP_DIR}/problems" ]
}

@test 'dotfiles-home-link two args existing file' {
    mkdir -p "${TMP_DIR}/src"
    touch "${TMP_DIR}/src/a"
    mkdir -p "${TMP_DIR}/b"
    touch "${TMP_DIR}/b/a"
    run_script "PATH=${BIN_DIR}:${PATH}" "DOTFILES_PACKAGE_PROBLEMS_FILE=${TMP_DIR}/problems" "${BIN_DIR}/dotfiles-home-link" "${TMP_DIR}/src/a" "${TMP_DIR}/b/a"
    assert_failure
    refute_line --partial 'Usage:'
    assert [ -f "${TMP_DIR}/b/a" ]
    assert [ "$(wc -l <"${TMP_DIR}/problems")" = '1' ]
}

@test 'dotfiles-home-link two args incorrect symlink' {
    mkdir -p "${TMP_DIR}/src"
    touch "${TMP_DIR}/src/a"
    touch "${TMP_DIR}/src/b"
    mkdir -p "${TMP_DIR}/b"
    ln -s "${TMP_DIR}/src/b" "${TMP_DIR}/b/a"
    run_script "PATH=${BIN_DIR}:${PATH}" "DOTFILES_PACKAGE_PROBLEMS_FILE=${TMP_DIR}/problems" "${BIN_DIR}/dotfiles-home-link" "${TMP_DIR}/src/a" "${TMP_DIR}/b/a"
    assert_failure
    refute_line --partial 'Usage:'
    assert [ -h "${TMP_DIR}/b/a" ]
    assert [ "$(readlink -f "${TMP_DIR}/b/a")" = "${TMP_DIR}/src/b" ]
    assert [ "$(wc -l <"${TMP_DIR}/problems")" = '1' ]
}

@test 'dotfiles-package-ignored usage' {
    run_script "${BIN_DIR}/dotfiles-package-ignored"
    assert_failure
    assert_line --partial 'Usage:'
}

@test 'dotfiles-package-ignored no file' {
    run_script "DOTFILES_PACKAGE_IGNORE_FILE=${TMP_DIR}/a" "${BIN_DIR}/dotfiles-package-ignored" 'foo'
    assert_failure
    refute_line --partial 'Usage:'
}

@test 'dotfiles-package-ignored empty file' {
    local IGNORE="${TMP_DIR}/a"
    touch "${IGNORE}"
    run_script "DOTFILES_PACKAGE_IGNORE_FILE=${IGNORE}" "${BIN_DIR}/dotfiles-package-ignored" 'foo'
    assert_failure
    refute_line --partial 'Usage:'
}

@test 'dotfiles-package-ignored success' {
    local IGNORE="${TMP_DIR}/a"
    printf "foo\nbar\nbaz\n" >"${IGNORE}"
    run_script "DOTFILES_PACKAGE_IGNORE_FILE=${IGNORE}" "${BIN_DIR}/dotfiles-package-ignored" 'foo'
    assert_success
    refute_line --partial 'Usage:'
}

@test 'dotfiles-package-ignored failure' {
    local IGNORE="${TMP_DIR}/a"
    printf "foo\nbar\nbaz\n" >"${IGNORE}"
    run_script "DOTFILES_PACKAGE_IGNORE_FILE=${IGNORE}" "${BIN_DIR}/dotfiles-package-ignored" 'quux'
    assert_failure
    refute_line --partial 'Usage:'
}

@test 'dotfiles-package-ignore usage' {
    run_script "PATH=${BIN_DIR}:${PATH}" "${BIN_DIR}/dotfiles-package-ignore"
    assert_failure
    assert_line --partial 'Usage:'
}

@test 'dotfiles-package-ignore no file' {
    local IGNORE_DIR="${TMP_DIR}/a"
    local IGNORE_FILE="${IGNORE_DIR}/a"
    run_script "PATH=${BIN_DIR}:${PATH}" "DOTFILES_PACKAGE_INSTALL_DIR=${IGNORE_DIR}" "DOTFILES_PACKAGE_IGNORE_FILE=${IGNORE_FILE}" "${BIN_DIR}/dotfiles-package-ignore" 'foo'
    assert_success
    refute_line --partial 'Usage:'
    assert [ "$(wc -l <"${IGNORE_FILE}")" = '1' ]
    assert [ "$(cat "${IGNORE_FILE}")" = 'foo' ]
}

@test 'dotfiles-package-ignore new' {
    local IGNORE_DIR="${TMP_DIR}/a"
    local IGNORE_FILE="${IGNORE_DIR}/a"
    mkdir -p "${IGNORE_DIR}"
    echo 'foo' >"${IGNORE_FILE}"
    run_script "PATH=${BIN_DIR}:${PATH}" "DOTFILES_PACKAGE_INSTALL_DIR=${IGNORE_DIR}" "DOTFILES_PACKAGE_IGNORE_FILE=${IGNORE_FILE}" "${BIN_DIR}/dotfiles-package-ignore" 'bar'
    assert_success
    refute_line --partial 'Usage:'
    assert [ "$(wc -l <"${IGNORE_FILE}")" = '2' ]
    assert [ "$(cat "${IGNORE_FILE}")" = "$(printf 'foo\nbar')" ]
}

@test 'dotfiles-package-ignore existing' {
    local IGNORE_DIR="${TMP_DIR}/a"
    local IGNORE_FILE="${IGNORE_DIR}/a"
    mkdir -p "${IGNORE_DIR}"
    echo 'foo' >"${IGNORE_FILE}"
    run_script "PATH=${BIN_DIR}:${PATH}" "DOTFILES_PACKAGE_INSTALL_DIR=${IGNORE_DIR}" "DOTFILES_PACKAGE_IGNORE_FILE=${IGNORE_FILE}" "${BIN_DIR}/dotfiles-package-ignore" 'foo'
    assert_success
    refute_line --partial 'Usage:'
    assert [ "$(wc -l <"${IGNORE_FILE}")" = '1' ]
    assert [ "$(cat "${IGNORE_FILE}")" = 'foo' ]
}

@test 'dotfiles-package-unignore usage' {
    run_script "PATH=${BIN_DIR}:${PATH}" "${BIN_DIR}/dotfiles-package-unignore"
    assert_failure
    assert_line --partial 'Usage:'
}

@test 'dotfiles-package-unignore no file' {
    local IGNORE_FILE="${TMP_DIR}/a"
    run_script "PATH=${BIN_DIR}:${PATH}" "DOTFILES_PACKAGE_IGNORE_FILE=${IGNORE_FILE}" "${BIN_DIR}/dotfiles-package-unignore" 'foo'
    assert_success
    refute_line --partial 'Usage:'
    assert [ ! -e "${IGNORE_FILE}" ]
}

@test 'dotfiles-package-unignore in file' {
    local IGNORE_FILE="${TMP_DIR}/a"
    printf 'foo\nbar\nbaz\n' >"${IGNORE_FILE}"
    run_script "PATH=${BIN_DIR}:${PATH}" "DOTFILES_PACKAGE_IGNORE_FILE=${IGNORE_FILE}" "${BIN_DIR}/dotfiles-package-unignore" 'bar'
    assert_success
    refute_line --partial 'Usage:'
    assert [ "$(wc -l <"${IGNORE_FILE}")" = '2' ]
    assert [ "$(cat "${IGNORE_FILE}")" = "$(printf 'foo\nbaz')" ]
}

@test 'dotfiles-package-unignore not in file' {
    local IGNORE_FILE="${TMP_DIR}/a"
    printf 'foo\nbar\nbaz\n' >"${IGNORE_FILE}"
    run_script "PATH=${BIN_DIR}:${PATH}" "DOTFILES_PACKAGE_IGNORE_FILE=${IGNORE_FILE}" "${BIN_DIR}/dotfiles-package-unignore" 'quux'
    assert_success
    refute_line --partial 'Usage:'
    assert [ "$(wc -l <"${IGNORE_FILE}")" = '3' ]
    assert [ "$(cat "${IGNORE_FILE}")" = "$(printf 'foo\nbar\nbaz')" ]
}

@test 'dotfiles-package-installed usage' {
    run_script "${BIN_DIR}/dotfiles-package-installed"
    assert_failure
    assert_line --partial 'Usage:'
}

@test 'dotfiles-package-installed success' {
    touch "${TMP_DIR}/foo.installed"
    run_script "DOTFILES_PACKAGE_INSTALL_DIR=${TMP_DIR}" "${BIN_DIR}/dotfiles-package-installed" 'foo'
    assert_success
    refute_line --partial 'Usage:'
}

@test 'dotfiles-package-installed failure' {
    run_script "DOTFILES_PACKAGE_INSTALL_DIR=${TMP_DIR}" "${BIN_DIR}/dotfiles-package-installed" 'foo'
    assert_failure
    refute_line --partial 'Usage:'
}

@test 'dotfiles-package-source-path usage' {
    run_script "${BIN_DIR}/dotfiles-package-source-path"
    assert_failure
    assert_line --partial 'Usage:'
}

@test 'dotfiles-package-source-path found' {
    mkdir -p "${TMP_DIR}/a"
    mkdir -p "${TMP_DIR}/b/foo"
    run_script "DOTFILES_PACKAGE_ROOTS=:${TMP_DIR}/a::${TMP_DIR}/b:${TMP_DIR}/c" "${BIN_DIR}/dotfiles-package-source-path" 'foo'
    assert_success
    assert_line "${TMP_DIR}/b/foo"
    refute_line --partial 'Usage:'
}

@test 'dotfiles-package-source-path not found' {
    mkdir -p "${TMP_DIR}/a"
    mkdir -p "${TMP_DIR}/b/foo"
    run_script "DOTFILES_PACKAGE_ROOTS=:${TMP_DIR}/a::${TMP_DIR}/b::${TMP_DIR}/c" "${BIN_DIR}/dotfiles-package-source-path" 'bar'
    assert_failure
    refute_output
    refute_line --partial 'Usage:'
}

@test 'dotfiles-package-source-path path' {
    run_script "${BIN_DIR}/dotfiles-package-source-path" 'foo/bar'
    assert_success
    assert_line "foo/bar"
    refute_line --partial 'Usage:'
}

@test 'dotfiles-package-has-installer usage' {
    run_script "${BIN_DIR}/dotfiles-package-has-installer"
    assert_failure
    assert_line --partial 'Usage:'
}

@test 'dotfiles-package-has-installer nonexistent package' {
    mkdir -p "${TMP_DIR}/a"
    run_script "PATH=${BIN_DIR}:${PATH}" "DOTFILES_PACKAGE_ROOTS=${TMP_DIR}/a" "${BIN_DIR}/dotfiles-package-has-installer" 'foo'
    assert_failure
    refute_line --partial 'Usage:'
}

@test 'dotfiles-package-has-installer success' {
    mkdir -p "${TMP_DIR}/a/foo"
    touch "${TMP_DIR}/a/foo/install"
    run_script "PATH=${BIN_DIR}:${PATH}" "DOTFILES_PACKAGE_ROOTS=${TMP_DIR}/a" "${BIN_DIR}/dotfiles-package-has-installer" 'foo'
    assert_success
    refute_line --partial 'Usage:'
}

@test 'dotfiles-package-has-installer failure' {
    mkdir -p "${TMP_DIR}/a/foo"
    run_script "PATH=${BIN_DIR}:${PATH}" "DOTFILES_PACKAGE_ROOTS=${TMP_DIR}/a" "${BIN_DIR}/dotfiles-package-has-installer" 'foo'
    assert_failure
    refute_line --partial 'Usage:'
}

@test 'dotfiles-package-has-installer path success' {
    mkdir -p "${TMP_DIR}/a/foo"
    touch "${TMP_DIR}/a/foo/install"
    run_script "PATH=${BIN_DIR}:${PATH}" "${BIN_DIR}/dotfiles-package-has-installer" "${TMP_DIR}/a/foo"
    assert_success
    refute_line --partial 'Usage:'
}

@test 'dotfiles-package-has-installer path failure' {
    mkdir -p "${TMP_DIR}/a/foo"
    run_script "PATH=${BIN_DIR}:${PATH}" "${BIN_DIR}/dotfiles-package-has-installer" "${TMP_DIR}/a/foo"
    assert_failure
    refute_line --partial 'Usage:'
}

@test 'dotfiles-package-can-install usage' {
    run_script "${BIN_DIR}/dotfiles-package-can-install"
    assert_failure
    assert_line --partial 'Usage:'
}

@test 'dotfiles-package-can-install nonexistent package' {
    mkdir -p "${TMP_DIR}/a"
    run_script "PATH=${BIN_DIR}:${PATH}" "DOTFILES_PACKAGE_ROOTS=${TMP_DIR}/a" "${BIN_DIR}/dotfiles-package-can-install" 'foo'
    assert_failure
    refute_line --partial 'Usage:'
}

@test 'dotfiles-package-can-install package without installer' {
    mkdir -p "${TMP_DIR}/a/foo"
    run_script "PATH=${BIN_DIR}:${PATH}" "DOTFILES_PACKAGE_ROOTS=${TMP_DIR}/a" "${BIN_DIR}/dotfiles-package-can-install" 'foo'
    assert_failure
    refute_line --partial 'Usage:'
}

@test 'dotfiles-package-can-install package with installer, no requirements' {
    mkdir -p "${TMP_DIR}/a/foo"
    touch "${TMP_DIR}/a/foo/install"
    run_script "PATH=${BIN_DIR}:${PATH}" "DOTFILES_PACKAGE_ROOTS=${TMP_DIR}/a" "${BIN_DIR}/dotfiles-package-can-install" 'foo'
    assert_success
    refute_line --partial 'Usage:'
}

@test 'dotfiles-package-can-install requirements met' {
    mkdir -p "${TMP_DIR}/a/foo"
    touch "${TMP_DIR}/a/foo/install"
    printf '#!/bin/bash\ntrue\n' >"${TMP_DIR}/a/foo/can-install"
    chmod u+x "${TMP_DIR}/a/foo/can-install"
    run_script "PATH=${BIN_DIR}:${PATH}" "DOTFILES_PACKAGE_ROOTS=${TMP_DIR}/a" "${BIN_DIR}/dotfiles-package-can-install" 'foo'
    assert_success
    refute_line --partial 'Usage:'
}

@test 'dotfiles-package-can-install requirements unmet' {
    mkdir -p "${TMP_DIR}/a/foo"
    touch "${TMP_DIR}/a/foo/install"
    printf '#!/bin/bash\nfalse\n' >"${TMP_DIR}/a/foo/can-install"
    chmod u+x "${TMP_DIR}/a/foo/can-install"
    run_script "PATH=${BIN_DIR}:${PATH}" "DOTFILES_PACKAGE_ROOTS=${TMP_DIR}/a" "${BIN_DIR}/dotfiles-package-can-install" 'foo'
    assert_failure
    refute_line --partial 'Usage:'
}

@test 'dotfiles-package-can-install nonexistent path' {
    mkdir -p "${TMP_DIR}/a"
    run_script "PATH=${BIN_DIR}:${PATH}" "${BIN_DIR}/dotfiles-package-can-install" "${TMP_DIR}/a/foo"
    assert_failure
    refute_line --partial 'Usage:'
}

@test 'dotfiles-package-can-install path without installer' {
    mkdir -p "${TMP_DIR}/a/foo"
    run_script "PATH=${BIN_DIR}:${PATH}" "${BIN_DIR}/dotfiles-package-can-install" "${TMP_DIR}/a/foo"
    assert_failure
    refute_line --partial 'Usage:'
}

@test 'dotfiles-package-can-install path with installer, no requirements' {
    mkdir -p "${TMP_DIR}/a/foo"
    touch "${TMP_DIR}/a/foo/install"
    run_script "PATH=${BIN_DIR}:${PATH}" "${BIN_DIR}/dotfiles-package-can-install" "${TMP_DIR}/a/foo"
    assert_success
    refute_line --partial 'Usage:'
}

@test 'dotfiles-package-can-install path requirements met' {
    mkdir -p "${TMP_DIR}/a/foo"
    touch "${TMP_DIR}/a/foo/install"
    printf '#!/bin/bash\ntrue\n' >"${TMP_DIR}/a/foo/can-install"
    chmod u+x "${TMP_DIR}/a/foo/can-install"
    run_script "PATH=${BIN_DIR}:${PATH}" "${BIN_DIR}/dotfiles-package-can-install" "${TMP_DIR}/a/foo"
    assert_success
    refute_line --partial 'Usage:'
}

@test 'dotfiles-package-can-install path requirements unmet' {
    mkdir -p "${TMP_DIR}/a/foo"
    touch "${TMP_DIR}/a/foo/install"
    printf '#!/bin/bash\nfalse\n' >"${TMP_DIR}/a/foo/can-install"
    chmod u+x "${TMP_DIR}/a/foo/can-install"
    run_script "PATH=${BIN_DIR}:${PATH}" "${BIN_DIR}/dotfiles-package-can-install" "${TMP_DIR}/a/foo"
    assert_failure
    refute_line --partial 'Usage:'
}

@test 'dotfiles-package-list' {
    local INSTALL_DIR="${TMP_DIR}/install"
    mkdir -p "${INSTALL_DIR}"
    local IGNORE_FILE="${TMP_DIR}/ignore"

    mkdir -p "${TMP_DIR}/a/foo"

    mkdir -p "${TMP_DIR}/a/bar"
    echo 'bar' >>"${IGNORE_FILE}"

    mkdir -p "${TMP_DIR}/a/baz"
    touch "${INSTALL_DIR}/baz.installed"

    mkdir -p "${TMP_DIR}/b/quux"
    touch "${TMP_DIR}/b/quux/install"

    mkdir -p "${TMP_DIR}/b/blah"
    touch "${TMP_DIR}/b/blah/install"
    printf "#!/bin/bash\ntrue\n" >"${TMP_DIR}/b/blah/can-install"
    chmod u+x "${TMP_DIR}/b/blah/can-install"

    mkdir -p "${TMP_DIR}/b/yay"
    touch "${TMP_DIR}/b/yay/install"
    printf "#!/bin/bash\nfalse\n" >"${TMP_DIR}/b/yay/can-install"
    chmod u+x "${TMP_DIR}/b/yay/can-install"

    mkdir -p "${TMP_DIR}/b/stuff"
    touch "${TMP_DIR}/b/stuff/install"
    echo 'stuff' >>"${IGNORE_FILE}"
    touch "${INSTALL_DIR}/stuff.installed"

    mkdir -p "${TMP_DIR}/b/foo"

    run_script "PATH=${BIN_DIR}:${PATH}" "DOTFILES_PACKAGE_INSTALL_DIR=${INSTALL_DIR}" "DOTFILES_PACKAGE_IGNORE_FILE=${IGNORE_FILE}" "DOTFILES_PACKAGE_ROOTS=${TMP_DIR}/a::${TMP_DIR}/b:${TMP_DIR}/c" "${BIN_DIR}/dotfiles-package-list"
    assert_success
    assert_output "WARNING: duplicate package names found; only the first instance of each will be used: foo

Active without installing
    foo

Installed
    baz

Available to install
    blah
    quux

Not available to install
    yay

Ignored
    bar
    stuff"
}
