# shellcheck shell=bats
# bats file_tags=slow

setup() {
    THIS_DIR="$(cd "$(dirname "${BATS_TEST_FILENAME}")" && pwd)"
    BIN_DIR="${THIS_DIR}/../../core/bin"
    PACKAGE_SCRIPTS_DIR="${THIS_DIR}/../../core/package-scripts"

    TEST_NEXT_INIT="${BATS_TEST_TMPDIR}/next-init"
    TEST_NEXT_LOGIN="${BATS_TEST_TMPDIR}/next-login"

    bats_load_library 'bats-support'
    bats_load_library 'bats-assert'
}

run_script() {
    run env -i -C "${BATS_TEST_TMPDIR}" HOME="${BATS_TEST_TMPDIR}" "${@}"
}

assert_num_matching_lines() {
    assert_equal "$(echo "${output}" | grep -E -c "${1}")" "${2}"
}

git_init() {
    local BRANCH="${1:-}"
    if [ -z "${BRANCH}" ]; then
        BRANCH='main'
    fi
    git init "--initial-branch=${BRANCH}" .
    git config --local user.name Foo
    git config --local user.email test@example.com
}

@test 'dotfiles-next-init usage' {
    run_script "${BIN_DIR}/dotfiles-next-init"
    assert_failure
    assert_line --partial 'Usage:'
}

@test 'dotfiles-next-init nonexistent' {
    run_script "DOTFILES_NEXT_INIT=${TEST_NEXT_INIT}" "${BIN_DIR}/dotfiles-next-init" 'foo' 'bar'
    assert_success
    refute_output --partial 'Usage:'
    assert [ "$(grep -c 'This file will be run' <"${TEST_NEXT_INIT}")" = '1' ]
    assert [ "$(grep -c 'foo bar' <"${TEST_NEXT_INIT}")" = '1' ]
}

@test 'dotfiles-next-init existing same command' {
    echo 'foo bar' >"${TEST_NEXT_INIT}"
    run_script "DOTFILES_NEXT_INIT=${TEST_NEXT_INIT}" "${BIN_DIR}/dotfiles-next-init" 'foo' 'bar'
    assert_success
    refute_output --partial 'Usage:'
    assert [ "$(grep -c 'foo bar' <"${TEST_NEXT_INIT}")" = '1' ]
}

@test 'dotfiles-next-init existing different command' {
    echo 'foo bar' >"${TEST_NEXT_INIT}"
    run_script "DOTFILES_NEXT_INIT=${TEST_NEXT_INIT}" "${BIN_DIR}/dotfiles-next-init" 'foo' 'baz'
    assert_success
    refute_output --partial 'Usage:'
    assert [ "$(grep -c 'foo bar' <"${TEST_NEXT_INIT}")" = '1' ]
    assert [ "$(grep -c 'foo baz' <"${TEST_NEXT_INIT}")" = '1' ]
}

@test 'dotfiles-next-login usage' {
    run_script "${BIN_DIR}/dotfiles-next-login"
    assert_failure
    assert_line --partial 'Usage:'
}

@test 'dotfiles-next-login nonexistent' {
    run_script "DOTFILES_NEXT_LOGIN=${TEST_NEXT_LOGIN}" "${BIN_DIR}/dotfiles-next-login" 'foo' 'bar'
    assert_success
    refute_output --partial 'Usage:'
    assert [ "$(grep -c 'This file will be run' <"${TEST_NEXT_LOGIN}")" = '1' ]
    assert [ "$(grep -c 'foo bar' <"${TEST_NEXT_LOGIN}")" = '1' ]
}

@test 'dotfiles-next-login existing same command' {
    echo 'foo bar' >"${TEST_NEXT_LOGIN}"
    run_script "DOTFILES_NEXT_LOGIN=${TEST_NEXT_LOGIN}" "${BIN_DIR}/dotfiles-next-login" 'foo' 'bar'
    assert_success
    refute_output --partial 'Usage:'
    assert [ "$(grep -c 'foo bar' <"${TEST_NEXT_LOGIN}")" = '1' ]
}

@test 'dotfiles-next-login existing different command' {
    echo 'foo bar' >"${TEST_NEXT_LOGIN}"
    run_script "DOTFILES_NEXT_LOGIN=${TEST_NEXT_LOGIN}" "${BIN_DIR}/dotfiles-next-login" 'foo' 'baz'
    assert_success
    refute_output --partial 'Usage:'
    assert [ "$(grep -c 'foo bar' <"${TEST_NEXT_LOGIN}")" = '1' ]
    assert [ "$(grep -c 'foo baz' <"${TEST_NEXT_LOGIN}")" = '1' ]
}

@test 'dotfiles-repo-is-clean usage' {
    run_script "${BIN_DIR}/dotfiles-repo-is-clean"
    assert_failure
    assert_line --partial 'Usage:'
}

@test 'dotfiles-repo-is-clean not a dir' {
    local REPO="${BATS_TEST_TMPDIR}/a"
    touch "${REPO}"
    run_script "${BIN_DIR}/dotfiles-repo-is-clean" "${REPO}"
    assert_success
    refute_output --partial 'Usage:'
}

@test 'dotfiles-repo-is-clean not a repo' {
    local REPO="${BATS_TEST_TMPDIR}/a"
    mkdir -p "${REPO}"
    run_script "${BIN_DIR}/dotfiles-repo-is-clean" "${REPO}"
    assert_success
    refute_output --partial 'Usage:'
}

@test 'dotfiles-repo-is-clean empty repo' {
    local REPO="${BATS_TEST_TMPDIR}/a"
    mkdir -p "${REPO}"
    (cd "${REPO}" && git_init)
    run_script "${BIN_DIR}/dotfiles-repo-is-clean" "${REPO}"
    assert_success
    refute_output --partial 'Usage:'
}

@test 'dotfiles-repo-is-clean untracked file' {
    local REPO="${BATS_TEST_TMPDIR}/a"
    mkdir -p "${REPO}"
    (cd "${REPO}" && git_init)
    touch "${REPO}/a"
    run_script "${BIN_DIR}/dotfiles-repo-is-clean" "${REPO}"
    assert_failure
    refute_output --partial 'Usage:'
}

@test 'dotfiles-repo-is-clean clean no upstream' {
    local REPO="${BATS_TEST_TMPDIR}/a"
    mkdir -p "${REPO}"
    (cd "${REPO}" && git_init)
    touch "${REPO}/a"
    (cd "${REPO}" && git add a && git commit -m 'Foo')
    run_script "${BIN_DIR}/dotfiles-repo-is-clean" "${REPO}"
    assert_success
    refute_output --partial 'Usage:'
}

@test 'dotfiles-repo-is-clean modified file' {
    local REPO="${BATS_TEST_TMPDIR}/a"
    mkdir -p "${REPO}"
    (cd "${REPO}" && git_init)
    touch "${REPO}/a"
    (cd "${REPO}" && git add a && git commit -m 'Foo')
    echo 'a' >"${REPO}/a"
    run_script "${BIN_DIR}/dotfiles-repo-is-clean" "${REPO}"
    assert_failure
    refute_output --partial 'Usage:'
}

@test 'dotfiles-repo-is-clean unpushed commit' {
    local UPSTREAM="${BATS_TEST_TMPDIR}/a"
    local REPO="${BATS_TEST_TMPDIR}/b"
    mkdir -p "${UPSTREAM}"
    (cd "${UPSTREAM}" && git init --bare .)
    git clone "${UPSTREAM}" "${REPO}"
    touch "${REPO}/a"
    (cd "${REPO}" && git add a && git commit -m 'Foo' && git push)
    echo 'a' >"${REPO}/a"
    (cd "${REPO}" && git add a && git commit -m 'Bar')
    run_script "${BIN_DIR}/dotfiles-repo-is-clean" "${REPO}"
    assert_failure
    refute_output --partial 'Usage:'
}

@test 'dotfiles-repo-is-clean pushed commit' {
    local UPSTREAM="${BATS_TEST_TMPDIR}/a"
    local REPO="${BATS_TEST_TMPDIR}/b"
    mkdir -p "${UPSTREAM}"
    (cd "${UPSTREAM}" && git init --bare .)
    git clone "${UPSTREAM}" "${REPO}"
    touch "${REPO}/a"
    (cd "${REPO}" && git add a && git commit -m 'Foo' && git push)
    echo 'a' >"${REPO}/a"
    (cd "${REPO}" && git add a && git commit -m 'Bar' && git push)
    run_script "${BIN_DIR}/dotfiles-repo-is-clean" "${REPO}"
    assert_success
    refute_output --partial 'Usage:'
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

@test 'dotfiles-in-list colon in item' {
    run_script "${BIN_DIR}/dotfiles-in-list" 'foo' 'foo:'
    assert_failure
    assert_line --partial 'Usage:'
}

@test 'dotfiles-in-list empty list' {
    run_script "${BIN_DIR}/dotfiles-in-list" '' 'foo'
    assert_failure
    refute_output --partial 'Usage:'
}

@test 'dotfiles-in-list simple list present' {
    run_script "${BIN_DIR}/dotfiles-in-list" 'foo' 'foo'
    assert_success
    refute_output --partial 'Usage:'
}

@test 'dotfiles-in-list simple list absent' {
    run_script "${BIN_DIR}/dotfiles-in-list" 'foo' 'bar'
    assert_failure
    refute_output --partial 'Usage:'
}

@test 'dotfiles-in-list complex list present' {
    run_script "${BIN_DIR}/dotfiles-in-list" 'foo:bar:baz' 'bar'
    assert_success
    refute_output --partial 'Usage:'
}

@test 'dotfiles-in-list complex list absent' {
    run_script "${BIN_DIR}/dotfiles-in-list" 'foo:bar:baz' 'quux'
    assert_failure
    refute_output --partial 'Usage:'
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
    mkdir -p "${BATS_TEST_TMPDIR}/a"
    touch "${BATS_TEST_TMPDIR}/a/foo"
    run_script "${BIN_DIR}/dotfiles-symlink-dir-contents" "${BATS_TEST_TMPDIR}/a" "${BATS_TEST_TMPDIR}/b"
    assert_success
    refute_output --partial 'Usage:'
    assert [ ! -d "${BATS_TEST_TMPDIR}/b" ]
}

@test 'dotfiles-symlink-dir-contents nonexistent file dir' {
    mkdir -p "${BATS_TEST_TMPDIR}/b"
    run_script "${BIN_DIR}/dotfiles-symlink-dir-contents" "${BATS_TEST_TMPDIR}/a" "${BATS_TEST_TMPDIR}/b"
    assert_success
    refute_output --partial 'Usage:'
    assert [ "$(find "${BATS_TEST_TMPDIR}/b" -mindepth 1 | wc -l)" = '0' ]
}

@test 'dotfiles-symlink-dir-contents success' {
    mkdir -p "${BATS_TEST_TMPDIR}/a"
    touch "${BATS_TEST_TMPDIR}/a/foo"
    touch "${BATS_TEST_TMPDIR}/a/bar"
    touch "${BATS_TEST_TMPDIR}/a/baz"
    mkdir -p "${BATS_TEST_TMPDIR}/b"
    touch "${BATS_TEST_TMPDIR}/b/bar"
    run_script "${BIN_DIR}/dotfiles-symlink-dir-contents" "${BATS_TEST_TMPDIR}/a" "${BATS_TEST_TMPDIR}/b"
    assert_success
    refute_output --partial 'Usage:'
    assert [ -h "${BATS_TEST_TMPDIR}/b/foo" ]
    assert [ "$(readlink -f "${BATS_TEST_TMPDIR}/b/foo")" = "${BATS_TEST_TMPDIR}/a/foo" ]
    assert [ -f "${BATS_TEST_TMPDIR}/b/bar" ]
    assert [ -h "${BATS_TEST_TMPDIR}/b/baz" ]
    assert [ "$(readlink -f "${BATS_TEST_TMPDIR}/b/baz")" = "${BATS_TEST_TMPDIR}/a/baz" ]
    assert [ "$(find "${BATS_TEST_TMPDIR}/b" -mindepth 1 | wc -l)" = '3' ]
}

@test 'dotfiles-log-package-message empty' {
    run_script "DOTFILES_PACKAGE_MESSAGES_FILE=${BATS_TEST_TMPDIR}/a" "${BIN_DIR}/dotfiles-log-package-message" 'foo' 'bar baz'
    assert_success
    assert [ "$(cat "${BATS_TEST_TMPDIR}/a")" = 'foo bar baz' ]
}

@test 'dotfiles-log-package-message existing' {
    echo 'foo' >"${BATS_TEST_TMPDIR}/a"
    run_script "DOTFILES_PACKAGE_MESSAGES_FILE=${BATS_TEST_TMPDIR}/a" "${BIN_DIR}/dotfiles-log-package-message" 'foo' 'bar baz'
    assert_success
    assert [ "$(cat "${BATS_TEST_TMPDIR}/a")" = "$(printf 'foo\nfoo bar baz')" ]
}

@test 'dotfiles-log-package-problem empty' {
    run_script "DOTFILES_PACKAGE_PROBLEMS_FILE=${BATS_TEST_TMPDIR}/a" "${BIN_DIR}/dotfiles-log-package-problem" 'foo' 'bar baz'
    assert_success
    assert [ "$(cat "${BATS_TEST_TMPDIR}/a")" = 'foo bar baz' ]
}

@test 'dotfiles-log-package-problem existing' {
    echo 'foo' >"${BATS_TEST_TMPDIR}/a"
    run_script "DOTFILES_PACKAGE_PROBLEMS_FILE=${BATS_TEST_TMPDIR}/a" "${BIN_DIR}/dotfiles-log-package-problem" 'foo' 'bar baz'
    assert_success
    assert [ "$(cat "${BATS_TEST_TMPDIR}/a")" = "$(printf 'foo\nfoo bar baz')" ]
}

@test 'dotfiles-home-link usage' {
    run_script "${BIN_DIR}/dotfiles-home-link"
    assert_failure
    assert_line --partial 'Usage:'
}

@test 'dotfiles-home-link nonexistent source' {
    run_script "PATH=${BIN_DIR}:${PATH}" "DOTFILES_PACKAGE_PROBLEMS_FILE=${BATS_TEST_TMPDIR}/problems" "${BIN_DIR}/dotfiles-home-link" "${BATS_TEST_TMPDIR}/a"
    assert_success
    refute_output --partial 'Usage:'
    assert [ ! -e "${BATS_TEST_TMPDIR}/.a" ]
    assert [ ! -e "${BATS_TEST_TMPDIR}/problems" ]
}

@test 'dotfiles-home-link one arg file nonexistent' {
    mkdir -p "${BATS_TEST_TMPDIR}/src"
    touch "${BATS_TEST_TMPDIR}/src/a"
    run_script "PATH=${BIN_DIR}:${PATH}" "DOTFILES_PACKAGE_PROBLEMS_FILE=${BATS_TEST_TMPDIR}/problems" "${BIN_DIR}/dotfiles-home-link" "${BATS_TEST_TMPDIR}/src/a"
    assert_success
    refute_output --partial 'Usage:'
    assert [ -h "${BATS_TEST_TMPDIR}/.a" ]
    assert [ "$(readlink -f "${BATS_TEST_TMPDIR}/.a")" = "${BATS_TEST_TMPDIR}/src/a" ]
    assert [ ! -e "${BATS_TEST_TMPDIR}/problems" ]
}

@test 'dotfiles-home-link one arg file correct symlink' {
    mkdir -p "${BATS_TEST_TMPDIR}/src"
    touch "${BATS_TEST_TMPDIR}/src/a"
    ln -s "${BATS_TEST_TMPDIR}/src/a" "${BATS_TEST_TMPDIR}/.a"
    run_script "PATH=${BIN_DIR}:${PATH}" "DOTFILES_PACKAGE_PROBLEMS_FILE=${BATS_TEST_TMPDIR}/problems" "${BIN_DIR}/dotfiles-home-link" "${BATS_TEST_TMPDIR}/src/a"
    assert_success
    refute_output --partial 'Usage:'
    assert [ -h "${BATS_TEST_TMPDIR}/.a" ]
    assert [ "$(readlink -f "${BATS_TEST_TMPDIR}/.a")" = "${BATS_TEST_TMPDIR}/src/a" ]
    assert [ ! -e "${BATS_TEST_TMPDIR}/problems" ]
}

@test 'dotfiles-home-link one arg file existing file' {
    mkdir -p "${BATS_TEST_TMPDIR}/src"
    touch "${BATS_TEST_TMPDIR}/src/a"
    touch "${BATS_TEST_TMPDIR}/.a"
    run_script "PATH=${BIN_DIR}:${PATH}" "DOTFILES_PACKAGE_PROBLEMS_FILE=${BATS_TEST_TMPDIR}/problems" "${BIN_DIR}/dotfiles-home-link" "${BATS_TEST_TMPDIR}/src/a"
    assert_failure
    refute_output --partial 'Usage:'
    assert [ -f "${BATS_TEST_TMPDIR}/.a" ]
    assert [ "$(wc -l <"${BATS_TEST_TMPDIR}/problems")" = '1' ]
}

@test 'dotfiles-home-link one arg file incorrect symlink' {
    mkdir -p "${BATS_TEST_TMPDIR}/src"
    touch "${BATS_TEST_TMPDIR}/src/a"
    touch "${BATS_TEST_TMPDIR}/src/b"
    ln -s "${BATS_TEST_TMPDIR}/src/b" "${BATS_TEST_TMPDIR}/.a"
    run_script "PATH=${BIN_DIR}:${PATH}" "DOTFILES_PACKAGE_PROBLEMS_FILE=${BATS_TEST_TMPDIR}/problems" "${BIN_DIR}/dotfiles-home-link" "${BATS_TEST_TMPDIR}/src/a"
    assert_failure
    refute_output --partial 'Usage:'
    assert [ -h "${BATS_TEST_TMPDIR}/.a" ]
    assert [ "$(readlink -f "${BATS_TEST_TMPDIR}/.a")" = "${BATS_TEST_TMPDIR}/src/b" ]
    assert [ "$(wc -l <"${BATS_TEST_TMPDIR}/problems")" = '1' ]
}

@test 'dotfiles-home-link one arg dir nonexistent' {
    mkdir -p "${BATS_TEST_TMPDIR}/src/a"
    run_script "PATH=${BIN_DIR}:${PATH}" "DOTFILES_PACKAGE_PROBLEMS_FILE=${BATS_TEST_TMPDIR}/problems" "${BIN_DIR}/dotfiles-home-link" "${BATS_TEST_TMPDIR}/src/a"
    assert_success
    refute_output --partial 'Usage:'
    assert [ -h "${BATS_TEST_TMPDIR}/a" ]
    assert [ "$(readlink -f "${BATS_TEST_TMPDIR}/a")" = "${BATS_TEST_TMPDIR}/src/a" ]
    assert [ ! -e "${BATS_TEST_TMPDIR}/problems" ]
}

@test 'dotfiles-home-link one arg dir correct symlink' {
    mkdir -p "${BATS_TEST_TMPDIR}/src/a"
    ln -s "${BATS_TEST_TMPDIR}/src/a" "${BATS_TEST_TMPDIR}/a"
    run_script "PATH=${BIN_DIR}:${PATH}" "DOTFILES_PACKAGE_PROBLEMS_FILE=${BATS_TEST_TMPDIR}/problems" "${BIN_DIR}/dotfiles-home-link" "${BATS_TEST_TMPDIR}/src/a"
    assert_success
    refute_output --partial 'Usage:'
    assert [ -h "${BATS_TEST_TMPDIR}/a" ]
    assert [ "$(readlink -f "${BATS_TEST_TMPDIR}/a")" = "${BATS_TEST_TMPDIR}/src/a" ]
    assert [ ! -e "${BATS_TEST_TMPDIR}/problems" ]
}

@test 'dotfiles-home-link one arg dir existing dir' {
    mkdir -p "${BATS_TEST_TMPDIR}/src/a"
    mkdir -p "${BATS_TEST_TMPDIR}/a"
    run_script "PATH=${BIN_DIR}:${PATH}" "DOTFILES_PACKAGE_PROBLEMS_FILE=${BATS_TEST_TMPDIR}/problems" "${BIN_DIR}/dotfiles-home-link" "${BATS_TEST_TMPDIR}/src/a"
    assert_failure
    refute_output --partial 'Usage:'
    assert [ -d "${BATS_TEST_TMPDIR}/a" ]
    assert [ "$(wc -l <"${BATS_TEST_TMPDIR}/problems")" = '1' ]
}

@test 'dotfiles-home-link one arg dir incorrect symlink' {
    mkdir -p "${BATS_TEST_TMPDIR}/src/a"
    mkdir -p "${BATS_TEST_TMPDIR}/src/b"
    ln -s "${BATS_TEST_TMPDIR}/src/b" "${BATS_TEST_TMPDIR}/a"
    run_script "PATH=${BIN_DIR}:${PATH}" "DOTFILES_PACKAGE_PROBLEMS_FILE=${BATS_TEST_TMPDIR}/problems" "${BIN_DIR}/dotfiles-home-link" "${BATS_TEST_TMPDIR}/src/a"
    assert_failure
    refute_output --partial 'Usage:'
    assert [ -h "${BATS_TEST_TMPDIR}/a" ]
    assert [ "$(readlink -f "${BATS_TEST_TMPDIR}/a")" = "${BATS_TEST_TMPDIR}/src/b" ]
    assert [ "$(wc -l <"${BATS_TEST_TMPDIR}/problems")" = '1' ]
}

@test 'dotfiles-home-link two args nonexistent' {
    mkdir -p "${BATS_TEST_TMPDIR}/src"
    touch "${BATS_TEST_TMPDIR}/src/a"
    run_script "PATH=${BIN_DIR}:${PATH}" "DOTFILES_PACKAGE_PROBLEMS_FILE=${BATS_TEST_TMPDIR}/problems" "${BIN_DIR}/dotfiles-home-link" "${BATS_TEST_TMPDIR}/src/a" "${BATS_TEST_TMPDIR}/b/a"
    assert_success
    refute_output --partial 'Usage:'
    assert [ -h "${BATS_TEST_TMPDIR}/b/a" ]
    assert [ "$(readlink -f "${BATS_TEST_TMPDIR}/b/a")" = "${BATS_TEST_TMPDIR}/src/a" ]
    assert [ ! -e "${BATS_TEST_TMPDIR}/problems" ]
}

@test 'dotfiles-home-link two args correct symlink' {
    mkdir -p "${BATS_TEST_TMPDIR}/src"
    touch "${BATS_TEST_TMPDIR}/src/a"
    mkdir "${BATS_TEST_TMPDIR}/b"
    ln -s "${BATS_TEST_TMPDIR}/src/a" "${BATS_TEST_TMPDIR}/b/a"
    run_script "PATH=${BIN_DIR}:${PATH}" "DOTFILES_PACKAGE_PROBLEMS_FILE=${BATS_TEST_TMPDIR}/problems" "${BIN_DIR}/dotfiles-home-link" "${BATS_TEST_TMPDIR}/src/a" "${BATS_TEST_TMPDIR}/b/a"
    assert_success
    refute_output --partial 'Usage:'
    assert [ -h "${BATS_TEST_TMPDIR}/b/a" ]
    assert [ "$(readlink -f "${BATS_TEST_TMPDIR}/b/a")" = "${BATS_TEST_TMPDIR}/src/a" ]
    assert [ ! -e "${BATS_TEST_TMPDIR}/problems" ]
}

@test 'dotfiles-home-link two args existing file' {
    mkdir -p "${BATS_TEST_TMPDIR}/src"
    touch "${BATS_TEST_TMPDIR}/src/a"
    mkdir -p "${BATS_TEST_TMPDIR}/b"
    touch "${BATS_TEST_TMPDIR}/b/a"
    run_script "PATH=${BIN_DIR}:${PATH}" "DOTFILES_PACKAGE_PROBLEMS_FILE=${BATS_TEST_TMPDIR}/problems" "${BIN_DIR}/dotfiles-home-link" "${BATS_TEST_TMPDIR}/src/a" "${BATS_TEST_TMPDIR}/b/a"
    assert_failure
    refute_output --partial 'Usage:'
    assert [ -f "${BATS_TEST_TMPDIR}/b/a" ]
    assert [ "$(wc -l <"${BATS_TEST_TMPDIR}/problems")" = '1' ]
}

@test 'dotfiles-home-link two args incorrect symlink' {
    mkdir -p "${BATS_TEST_TMPDIR}/src"
    touch "${BATS_TEST_TMPDIR}/src/a"
    touch "${BATS_TEST_TMPDIR}/src/b"
    mkdir -p "${BATS_TEST_TMPDIR}/b"
    ln -s "${BATS_TEST_TMPDIR}/src/b" "${BATS_TEST_TMPDIR}/b/a"
    run_script "PATH=${BIN_DIR}:${PATH}" "DOTFILES_PACKAGE_PROBLEMS_FILE=${BATS_TEST_TMPDIR}/problems" "${BIN_DIR}/dotfiles-home-link" "${BATS_TEST_TMPDIR}/src/a" "${BATS_TEST_TMPDIR}/b/a"
    assert_failure
    refute_output --partial 'Usage:'
    assert [ -h "${BATS_TEST_TMPDIR}/b/a" ]
    assert [ "$(readlink -f "${BATS_TEST_TMPDIR}/b/a")" = "${BATS_TEST_TMPDIR}/src/b" ]
    assert [ "$(wc -l <"${BATS_TEST_TMPDIR}/problems")" = '1' ]
}

@test 'dotfiles-home-bin-link no args' {
    run_script "${BIN_DIR}/dotfiles-home-bin-link"
    assert_failure
    assert_line --partial 'Usage:'
}

@test 'dotfiles-home-bin-link one arg' {
    run_script "${BIN_DIR}/dotfiles-home-bin-link" 'a'
    assert_failure
    assert_line --partial 'Usage:'
}

@test 'dotfiles-home-bin-link nonexistent command' {
    run_script "PATH=${BIN_DIR}:${PATH}" "DOTFILES_LOCAL_BIN=${BATS_TEST_TMPDIR}/a" "DOTFILES_PACKAGE_PROBLEMS_FILE=${BATS_TEST_TMPDIR}/problems" "${BIN_DIR}/dotfiles-home-bin-link" 'blahfoo' 'a'
    assert_success
    refute_output --partial 'Usage:'
    assert [ ! -e "${BATS_TEST_TMPDIR}/a" ]
    assert [ ! -e "${BATS_TEST_TMPDIR}/problems" ]
}

@test 'dotfiles-home-bin-link nonexisting' {
    run_script "PATH=${BIN_DIR}:${PATH}" "DOTFILES_LOCAL_BIN=${BATS_TEST_TMPDIR}/a" "DOTFILES_PACKAGE_PROBLEMS_FILE=${BATS_TEST_TMPDIR}/problems" "${BIN_DIR}/dotfiles-home-bin-link" 'dotfiles-home-bin-link' 'a' 'b'
    assert_success
    refute_output --partial 'Usage:'
    assert [ -h "${BATS_TEST_TMPDIR}/a/a" ]
    assert [ "$(readlink -f "${BATS_TEST_TMPDIR}/a/a")" = "$(readlink -f "${BIN_DIR}/dotfiles-home-bin-link")" ]
    assert [ -h "${BATS_TEST_TMPDIR}/a/b" ]
    assert [ "$(readlink -f "${BATS_TEST_TMPDIR}/a/b")" = "$(readlink -f "${BIN_DIR}/dotfiles-home-bin-link")" ]
    assert [ ! -e "${BATS_TEST_TMPDIR}/problems" ]
}

@test 'dotfiles-home-bin-link existing' {
    mkdir -p "${BATS_TEST_TMPDIR}/a"
    ln -s "$(readlink -f "${BIN_DIR}/dotfiles-home-bin-link")" "${BATS_TEST_TMPDIR}/a/a"
    ln -s "$(readlink -f "${BIN_DIR}/dotfiles-home-bin-link")" "${BATS_TEST_TMPDIR}/a/b"
    run_script "PATH=${BIN_DIR}:${PATH}" "DOTFILES_LOCAL_BIN=${BATS_TEST_TMPDIR}/a" "DOTFILES_PACKAGE_PROBLEMS_FILE=${BATS_TEST_TMPDIR}/problems" "${BIN_DIR}/dotfiles-home-bin-link" 'dotfiles-home-bin-link' 'a' 'b'
    assert_success
    refute_output --partial 'Usage:'
    assert [ -h "${BATS_TEST_TMPDIR}/a/a" ]
    assert [ "$(readlink -f "${BATS_TEST_TMPDIR}/a/a")" = "$(readlink -f "${BIN_DIR}/dotfiles-home-bin-link")" ]
    assert [ -h "${BATS_TEST_TMPDIR}/a/b" ]
    assert [ "$(readlink -f "${BATS_TEST_TMPDIR}/a/b")" = "$(readlink -f "${BIN_DIR}/dotfiles-home-bin-link")" ]
    assert [ ! -e "${BATS_TEST_TMPDIR}/problems" ]
}

@test 'dotfiles-home-bin-link incorrect symlink' {
    mkdir -p "${BATS_TEST_TMPDIR}/a"
    ln -s "$(readlink -f "${BIN_DIR}/dotfiles-home-link")" "${BATS_TEST_TMPDIR}/a/a"
    run_script "PATH=${BIN_DIR}:${PATH}" "DOTFILES_LOCAL_BIN=${BATS_TEST_TMPDIR}/a" "DOTFILES_PACKAGE_PROBLEMS_FILE=${BATS_TEST_TMPDIR}/problems" "${BIN_DIR}/dotfiles-home-bin-link" 'dotfiles-home-bin-link' 'a' 'b'
    assert_success
    refute_output --partial 'Usage:'
    assert [ -h "${BATS_TEST_TMPDIR}/a/a" ]
    assert [ "$(readlink -f "${BATS_TEST_TMPDIR}/a/a")" = "$(readlink -f "${BIN_DIR}/dotfiles-home-link")" ]
    assert [ -h "${BATS_TEST_TMPDIR}/a/b" ]
    assert [ "$(readlink -f "${BATS_TEST_TMPDIR}/a/b")" = "$(readlink -f "${BIN_DIR}/dotfiles-home-bin-link")" ]
    assert [ "$(wc -l <"${BATS_TEST_TMPDIR}/problems")" = '1' ]
}

@test 'dotfiles-home-bin-link incorrect file' {
    mkdir -p "${BATS_TEST_TMPDIR}/a"
    touch "${BATS_TEST_TMPDIR}/a/a"
    run_script "PATH=${BIN_DIR}:${PATH}" "DOTFILES_LOCAL_BIN=${BATS_TEST_TMPDIR}/a" "DOTFILES_PACKAGE_PROBLEMS_FILE=${BATS_TEST_TMPDIR}/problems" "${BIN_DIR}/dotfiles-home-bin-link" 'dotfiles-home-bin-link' 'a' 'b'
    assert_success
    refute_output --partial 'Usage:'
    assert [ -f "${BATS_TEST_TMPDIR}/a/a" ]
    assert [ -h "${BATS_TEST_TMPDIR}/a/b" ]
    assert [ "$(readlink -f "${BATS_TEST_TMPDIR}/a/b")" = "$(readlink -f "${BIN_DIR}/dotfiles-home-bin-link")" ]
    assert [ "$(wc -l <"${BATS_TEST_TMPDIR}/problems")" = '1' ]
}

@test 'dotfiles-logout-needed-set nonexistent' {
    run_script "PATH=${BIN_DIR}:${PATH}" "DOTFILES_NEEDS_LOGOUT=${BATS_TEST_TMPDIR}/logout" "${BIN_DIR}/dotfiles-logout-needed-set"
    assert_success
    assert [ -f "${BATS_TEST_TMPDIR}/logout" ]
}

@test 'dotfiles-logout-needed-set existing' {
    touch "${BATS_TEST_TMPDIR}/logout"
    run_script "PATH=${BIN_DIR}:${PATH}" "DOTFILES_NEEDS_LOGOUT=${BATS_TEST_TMPDIR}/logout" "${BIN_DIR}/dotfiles-logout-needed-set"
    assert_success
    assert [ -f "${BATS_TEST_TMPDIR}/logout" ]
}

@test 'dotfiles-logout-needed-check nonexistent' {
    run_script "PATH=${BIN_DIR}:${PATH}" "DOTFILES_NEEDS_LOGOUT=${BATS_TEST_TMPDIR}/logout" "${BIN_DIR}/dotfiles-logout-needed-check"
    assert_success
    assert [ ! -e "${BATS_TEST_TMPDIR}/logout" ]
    refute_output --partial 'Log out and log in again'
}

@test 'dotfiles-logout-needed-check existing' {
    touch "${BATS_TEST_TMPDIR}/logout"
    run_script "PATH=${BIN_DIR}:${PATH}" "DOTFILES_NEEDS_LOGOUT=${BATS_TEST_TMPDIR}/logout" "${BIN_DIR}/dotfiles-logout-needed-check"
    assert_success
    assert [ -f "${BATS_TEST_TMPDIR}/logout" ]
    assert_output --partial 'Log out and log in again'
}

@test 'dotfiles-logout-needed-check ignored' {
    touch "${BATS_TEST_TMPDIR}/logout"
    run_script "PATH=${BIN_DIR}:${PATH}" "DOTFILES_NEEDS_LOGOUT=${BATS_TEST_TMPDIR}/logout" "DOTFILES_NO_LOGOUT_NEEDED_CHECK=t" "${BIN_DIR}/dotfiles-logout-needed-check"
    assert_success
    assert [ -f "${BATS_TEST_TMPDIR}/logout" ]
    refute_output --partial 'Log out and log in again'
}

@test 'dotfiles-clone-or-update-repo no args' {
    run_script "PATH=${BIN_DIR}:${PATH}" "${BIN_DIR}/dotfiles-clone-or-update-repo"
    assert_failure
    assert_line --partial 'Usage:'
}

@test 'dotfiles-clone-or-update-repo one arg' {
    run_script "PATH=${BIN_DIR}:${PATH}" "${BIN_DIR}/dotfiles-clone-or-update-repo" 'foo'
    assert_failure
    assert_line --partial 'Usage:'
}

@test 'dotfiles-clone-or-update-repo two args' {
    run_script "PATH=${BIN_DIR}:${PATH}" "${BIN_DIR}/dotfiles-clone-or-update-repo" 'foo' 'bar'
    assert_failure
    assert_line --partial 'Usage:'
}

@test 'dotfiles-clone-or-update-repo clone fails' {
    mkdir -p "${BATS_TEST_TMPDIR}/foo"
    run_script "PATH=${BIN_DIR}:${PATH}" "${BIN_DIR}/dotfiles-clone-or-update-repo" "${BATS_TEST_TMPDIR}/foo" "${BATS_TEST_TMPDIR}/bar" 'main'
    assert_failure
    refute_line --partial 'Usage:'
    assert [ ! -e "${BATS_TEST_TMPDIR}/bar" ]
}

@test 'dotfiles-clone-or-update-repo clone default branch' {
    mkdir -p "${BATS_TEST_TMPDIR}/foo"
    touch "${BATS_TEST_TMPDIR}/foo/a"
    (cd "${BATS_TEST_TMPDIR}/foo" && git_init && git add a && git commit -m 'Foo')
    run_script "PATH=${BIN_DIR}:${PATH}" "${BIN_DIR}/dotfiles-clone-or-update-repo" "${BATS_TEST_TMPDIR}/foo" "${BATS_TEST_TMPDIR}/bar" 'main'
    assert_success
    refute_line --partial 'Usage:'
    assert [ -d "${BATS_TEST_TMPDIR}/bar" ]
    assert [ -f "${BATS_TEST_TMPDIR}/bar/a" ]
    (cd "${BATS_TEST_TMPDIR}/bar" && assert [ "$(git branch --show-current)" = 'main' ])
}

@test 'dotfiles-clone-or-update-repo clone other branch' {
    mkdir -p "${BATS_TEST_TMPDIR}/foo"
    touch "${BATS_TEST_TMPDIR}/foo/a"
    (cd "${BATS_TEST_TMPDIR}/foo" && git_init && git add a && git commit -m 'Foo' && git checkout -b dev)
    run_script "PATH=${BIN_DIR}:${PATH}" "${BIN_DIR}/dotfiles-clone-or-update-repo" "${BATS_TEST_TMPDIR}/foo" "${BATS_TEST_TMPDIR}/bar" 'dev'
    assert_success
    refute_line --partial 'Usage:'
    assert [ -d "${BATS_TEST_TMPDIR}/bar" ]
    assert [ -f "${BATS_TEST_TMPDIR}/bar/a" ]
    (cd "${BATS_TEST_TMPDIR}/bar" && assert [ "$(git branch --show-current)" = 'dev' ])
}

@test 'dotfiles-clone-or-update-repo clone tag' {
    mkdir -p "${BATS_TEST_TMPDIR}/foo"
    touch "${BATS_TEST_TMPDIR}/foo/a"
    (cd "${BATS_TEST_TMPDIR}/foo" && git_init && git add a && git commit -m 'Foo' && git tag foo)
    touch "${BATS_TEST_TMPDIR}/foo/b"
    (cd "${BATS_TEST_TMPDIR}/foo" && git add b && git commit -m 'Bar')
    run_script "PATH=${BIN_DIR}:${PATH}" "${BIN_DIR}/dotfiles-clone-or-update-repo" "${BATS_TEST_TMPDIR}/foo" "${BATS_TEST_TMPDIR}/bar" 'foo'
    assert_success
    refute_line --partial 'Usage:'
    assert [ -d "${BATS_TEST_TMPDIR}/bar" ]
    assert [ -f "${BATS_TEST_TMPDIR}/bar/a" ]
    assert [ ! -e "${BATS_TEST_TMPDIR}/bar/b" ]
    (cd "${BATS_TEST_TMPDIR}/bar" && assert [ "$(git branch --show-current)" = '' ])
    (cd "${BATS_TEST_TMPDIR}/bar" && assert [ "$(git tag --points-at)" = 'foo' ])
}

@test 'dotfiles-clone-or-update-repo clone bad branch' {
    mkdir -p "${BATS_TEST_TMPDIR}/foo"
    touch "${BATS_TEST_TMPDIR}/foo/a"
    (cd "${BATS_TEST_TMPDIR}/foo" && git_init && git add a && git commit -m 'Foo')
    run_script "PATH=${BIN_DIR}:${PATH}" "${BIN_DIR}/dotfiles-clone-or-update-repo" "${BATS_TEST_TMPDIR}/foo" "${BATS_TEST_TMPDIR}/bar" 'dev'
    assert_failure
    refute_line --partial 'Usage:'
    assert [ ! -e "${BATS_TEST_TMPDIR}/bar" ]
}

@test 'dotfiles-clone-or-update-repo pull clean same branch' {
    mkdir -p "${BATS_TEST_TMPDIR}/foo"
    touch "${BATS_TEST_TMPDIR}/foo/a"
    (cd "${BATS_TEST_TMPDIR}/foo" && git_init && git add a && git commit -m 'Foo')
    git clone "${BATS_TEST_TMPDIR}/foo" "${BATS_TEST_TMPDIR}/bar"
    echo 'foo' >"${BATS_TEST_TMPDIR}/foo/a"
    (cd "${BATS_TEST_TMPDIR}/foo" && git add a && git commit -m 'Bar')
    run_script "PATH=${BIN_DIR}:${PATH}" "${BIN_DIR}/dotfiles-clone-or-update-repo" "${BATS_TEST_TMPDIR}/foo" "${BATS_TEST_TMPDIR}/bar" 'main'
    assert_success
    refute_line --partial 'Usage:'
    assert [ -d "${BATS_TEST_TMPDIR}/bar" ]
    assert [ -f "${BATS_TEST_TMPDIR}/bar/a" ]
    assert [ "$(cat "${BATS_TEST_TMPDIR}/bar/a")" = 'foo' ]
    (cd "${BATS_TEST_TMPDIR}/bar" && assert [ "$(git branch --show-current)" = 'main' ])
}

@test 'dotfiles-clone-or-update-repo pull clean other branch' {
    mkdir -p "${BATS_TEST_TMPDIR}/foo"
    touch "${BATS_TEST_TMPDIR}/foo/a"
    (cd "${BATS_TEST_TMPDIR}/foo" && git_init && git add a && git commit -m 'Foo')
    git clone "${BATS_TEST_TMPDIR}/foo" "${BATS_TEST_TMPDIR}/bar"
    (cd "${BATS_TEST_TMPDIR}/foo" && git checkout -b dev)
    echo 'foo' >"${BATS_TEST_TMPDIR}/foo/a"
    (cd "${BATS_TEST_TMPDIR}/foo" && git add a && git commit -m 'Bar')
    run_script "PATH=${BIN_DIR}:${PATH}" "${BIN_DIR}/dotfiles-clone-or-update-repo" "${BATS_TEST_TMPDIR}/foo" "${BATS_TEST_TMPDIR}/bar" 'dev'
    assert_success
    refute_line --partial 'Usage:'
    assert [ -d "${BATS_TEST_TMPDIR}/bar" ]
    assert [ -f "${BATS_TEST_TMPDIR}/bar/a" ]
    assert [ "$(cat "${BATS_TEST_TMPDIR}/bar/a")" = 'foo' ]
    (cd "${BATS_TEST_TMPDIR}/bar" && assert [ "$(git branch --show-current)" = 'dev' ])
}

@test 'dotfiles-clone-or-update-repo pull clean tag' {
    mkdir -p "${BATS_TEST_TMPDIR}/foo"
    touch "${BATS_TEST_TMPDIR}/foo/a"
    (cd "${BATS_TEST_TMPDIR}/foo" && git_init && git add a && git commit -m 'Foo' && git tag foo)
    git clone "${BATS_TEST_TMPDIR}/foo" "${BATS_TEST_TMPDIR}/bar"
    (cd "${BATS_TEST_TMPDIR}/bar" && git checkout foo)
    echo 'foo' >"${BATS_TEST_TMPDIR}/foo/a"
    (cd "${BATS_TEST_TMPDIR}/foo" && git add a && git commit -m 'Bar')
    run_script "PATH=${BIN_DIR}:${PATH}" "${BIN_DIR}/dotfiles-clone-or-update-repo" "${BATS_TEST_TMPDIR}/foo" "${BATS_TEST_TMPDIR}/bar" 'foo'
    assert_success
    refute_line --partial 'Usage:'
    assert [ -d "${BATS_TEST_TMPDIR}/bar" ]
    assert [ -f "${BATS_TEST_TMPDIR}/bar/a" ]
    assert [ "$(cat "${BATS_TEST_TMPDIR}/bar/a")" = '' ]
    (cd "${BATS_TEST_TMPDIR}/bar" && assert [ "$(git branch --show-current)" = '' ])
    (cd "${BATS_TEST_TMPDIR}/bar" && assert [ "$(git tag --points-at)" = 'foo' ])
}

@test 'dotfiles-clone-or-update-repo pull clean bad branch' {
    mkdir -p "${BATS_TEST_TMPDIR}/foo"
    touch "${BATS_TEST_TMPDIR}/foo/a"
    (cd "${BATS_TEST_TMPDIR}/foo" && git_init && git add a && git commit -m 'Foo')
    git clone "${BATS_TEST_TMPDIR}/foo" "${BATS_TEST_TMPDIR}/bar"
    echo 'foo' >"${BATS_TEST_TMPDIR}/foo/a"
    (cd "${BATS_TEST_TMPDIR}/foo" && git add a && git commit -m 'Bar')
    run_script "PATH=${BIN_DIR}:${PATH}" "${BIN_DIR}/dotfiles-clone-or-update-repo" "${BATS_TEST_TMPDIR}/foo" "${BATS_TEST_TMPDIR}/bar" 'dev'
    assert_failure
    refute_line --partial 'Usage:'
    assert [ -d "${BATS_TEST_TMPDIR}/bar" ]
    assert [ -f "${BATS_TEST_TMPDIR}/bar/a" ]
    assert [ "$(cat "${BATS_TEST_TMPDIR}/bar/a")" = '' ]
    (cd "${BATS_TEST_TMPDIR}/bar" && assert [ "$(git branch --show-current)" = 'main' ])
}

@test 'dotfiles-clone-or-update-repo pull not a repo' {
    mkdir -p "${BATS_TEST_TMPDIR}/foo"
    touch "${BATS_TEST_TMPDIR}/foo/a"
    (cd "${BATS_TEST_TMPDIR}/foo" && git_init && git add a && git commit -m 'Foo')
    mkdir -p "${BATS_TEST_TMPDIR}/bar"
    run_script "PATH=${BIN_DIR}:${PATH}" "${BIN_DIR}/dotfiles-clone-or-update-repo" "${BATS_TEST_TMPDIR}/foo" "${BATS_TEST_TMPDIR}/bar" 'main'
    assert_failure
    refute_line --partial 'Usage:'
    assert [ -d "${BATS_TEST_TMPDIR}/bar" ]
    assert [ ! -e "${BATS_TEST_TMPDIR}/bar/a" ]
}

@test 'dotfiles-clone-or-update-repo pull dirty' {
    mkdir -p "${BATS_TEST_TMPDIR}/foo"
    touch "${BATS_TEST_TMPDIR}/foo/a"
    (cd "${BATS_TEST_TMPDIR}/foo" && git_init && git add a && git commit -m 'Foo')
    git clone "${BATS_TEST_TMPDIR}/foo" "${BATS_TEST_TMPDIR}/bar"
    echo 'foo' >"${BATS_TEST_TMPDIR}/bar/a"
    run_script "PATH=${BIN_DIR}:${PATH}" "${BIN_DIR}/dotfiles-clone-or-update-repo" "${BATS_TEST_TMPDIR}/foo" "${BATS_TEST_TMPDIR}/bar" 'main'
    assert_failure
    refute_line --partial 'Usage:'
    assert [ -d "${BATS_TEST_TMPDIR}/bar" ]
    assert [ -f "${BATS_TEST_TMPDIR}/bar/a" ]
    assert [ "$(cat "${BATS_TEST_TMPDIR}/bar/a")" = 'foo' ]
    (cd "${BATS_TEST_TMPDIR}/bar" && assert [ "$(git branch --show-current)" = 'main' ])
}

@test 'dotfiles-clone-or-update-repos no args' {
    run_script "PATH=${BIN_DIR}:${PATH}" "${BIN_DIR}/dotfiles-clone-or-update-repos"
    assert_failure
    assert_line --partial 'Usage:'
}

@test 'dotfiles-clone-or-update-repos one arg' {
    run_script "PATH=${BIN_DIR}:${PATH}" "${BIN_DIR}/dotfiles-clone-or-update-repos" 'foo'
    assert_failure
    assert_line --partial 'Usage:'
}

@test 'dotfiles-clone-or-update-repos two args' {
    run_script "PATH=${BIN_DIR}:${PATH}" "${BIN_DIR}/dotfiles-clone-or-update-repos" 'foo' 'bar'
    assert_failure
    assert_line --partial 'Usage:'
}

@test 'dotfiles-clone-or-update-repos clone' {
    mkdir -p "${BATS_TEST_TMPDIR}/foo"
    echo 'foo' >"${BATS_TEST_TMPDIR}/foo/a"
    (cd "${BATS_TEST_TMPDIR}/foo" && git_init && git add a && git commit -m 'Foo')
    mkdir -p "${BATS_TEST_TMPDIR}/bar.git"
    echo 'bar' >"${BATS_TEST_TMPDIR}/bar.git/b"
    (cd "${BATS_TEST_TMPDIR}/bar.git" && git_init master && git add b && git commit -m 'Foo')
    run_script "PATH=${BIN_DIR}:${PATH}" "${BIN_DIR}/dotfiles-clone-or-update-repos" "${BATS_TEST_TMPDIR}/dir" "${BATS_TEST_TMPDIR}/foo" 'main' "${BATS_TEST_TMPDIR}/bar.git" 'master'
    assert_success
    refute_line --partial 'Usage:'
    assert [ -d "${BATS_TEST_TMPDIR}/dir/foo" ]
    assert [ -f "${BATS_TEST_TMPDIR}/dir/foo/a" ]
    assert [ "$(cat "${BATS_TEST_TMPDIR}/dir/foo/a")" = 'foo' ]
    (cd "${BATS_TEST_TMPDIR}/dir/foo" && assert [ "$(git branch --show-current)" = 'main' ])
    assert [ -d "${BATS_TEST_TMPDIR}/dir/bar" ]
    assert [ -f "${BATS_TEST_TMPDIR}/dir/bar/b" ]
    assert [ "$(cat "${BATS_TEST_TMPDIR}/dir/bar/b")" = 'bar' ]
    (cd "${BATS_TEST_TMPDIR}/dir/bar" && assert [ "$(git branch --show-current)" = 'master' ])
}

@test 'dotfiles-clone-or-update-repos update' {
    mkdir -p "${BATS_TEST_TMPDIR}/dir"

    mkdir -p "${BATS_TEST_TMPDIR}/foo"
    echo 'foo' >"${BATS_TEST_TMPDIR}/foo/a"
    (cd "${BATS_TEST_TMPDIR}/foo" && git_init && git add a && git commit -m 'Foo')
    git clone "${BATS_TEST_TMPDIR}/foo" "${BATS_TEST_TMPDIR}/dir/foo"
    echo 'baz' >"${BATS_TEST_TMPDIR}/foo/a"
    (cd "${BATS_TEST_TMPDIR}/foo" && git add a && git commit -m 'Bar')

    mkdir -p "${BATS_TEST_TMPDIR}/bar"
    echo 'bar' >"${BATS_TEST_TMPDIR}/bar/b"
    (cd "${BATS_TEST_TMPDIR}/bar" && git_init master && git add b && git commit -m 'Foo')
    git clone "${BATS_TEST_TMPDIR}/bar" "${BATS_TEST_TMPDIR}/dir/bar"
    echo 'quux' >"${BATS_TEST_TMPDIR}/bar/b"
    (cd "${BATS_TEST_TMPDIR}/bar" && git add b && git commit -m 'Bar')

    run_script "PATH=${BIN_DIR}:${PATH}" "${BIN_DIR}/dotfiles-clone-or-update-repos" "${BATS_TEST_TMPDIR}/dir" "${BATS_TEST_TMPDIR}/foo" 'main' "${BATS_TEST_TMPDIR}/bar.git" 'master'
    assert_success
    refute_line --partial 'Usage:'
    assert [ -d "${BATS_TEST_TMPDIR}/dir/foo" ]
    assert [ -f "${BATS_TEST_TMPDIR}/dir/foo/a" ]
    assert [ "$(cat "${BATS_TEST_TMPDIR}/dir/foo/a")" = 'baz' ]
    (cd "${BATS_TEST_TMPDIR}/dir/foo" && assert [ "$(git branch --show-current)" = 'main' ])
    assert [ -d "${BATS_TEST_TMPDIR}/dir/bar" ]
    assert [ -f "${BATS_TEST_TMPDIR}/dir/bar/b" ]
    assert [ "$(cat "${BATS_TEST_TMPDIR}/dir/bar/b")" = 'quux' ]
    (cd "${BATS_TEST_TMPDIR}/dir/bar" && assert [ "$(git branch --show-current)" = 'master' ])
}

@test 'dotfiles-clone-or-update-repos failure not a repo' {
    mkdir -p "${BATS_TEST_TMPDIR}/foo"
    echo 'foo' >"${BATS_TEST_TMPDIR}/foo/a"
    mkdir -p "${BATS_TEST_TMPDIR}/bar.git"
    echo 'bar' >"${BATS_TEST_TMPDIR}/bar.git/b"
    (cd "${BATS_TEST_TMPDIR}/bar.git" && git_init master && git add b && git commit -m 'Foo')
    run_script "PATH=${BIN_DIR}:${PATH}" "${BIN_DIR}/dotfiles-clone-or-update-repos" "${BATS_TEST_TMPDIR}/dir" "${BATS_TEST_TMPDIR}/foo" 'main' "${BATS_TEST_TMPDIR}/bar.git" 'master'
    assert_failure
    refute_line --partial 'Usage:'
    assert [ ! -e "${BATS_TEST_TMPDIR}/dir/foo" ]
    assert [ ! -e "${BATS_TEST_TMPDIR}/dir/bar" ]
}

@test 'dotfiles-clone-or-update-repos failure missing branch arg' {
    mkdir -p "${BATS_TEST_TMPDIR}/foo"
    echo 'foo' >"${BATS_TEST_TMPDIR}/foo/a"
    (cd "${BATS_TEST_TMPDIR}/foo" && git_init && git add a && git commit -m 'Foo')
    mkdir -p "${BATS_TEST_TMPDIR}/bar.git"
    echo 'bar' >"${BATS_TEST_TMPDIR}/bar.git/b"
    (cd "${BATS_TEST_TMPDIR}/bar.git" && git_init master && git add b && git commit -m 'Foo')
    run_script "PATH=${BIN_DIR}:${PATH}" "${BIN_DIR}/dotfiles-clone-or-update-repos" "${BATS_TEST_TMPDIR}/dir" "${BATS_TEST_TMPDIR}/foo" 'main' "${BATS_TEST_TMPDIR}/bar.git"
    assert_failure
    assert_line --partial 'Usage:'
    assert [ -d "${BATS_TEST_TMPDIR}/dir/foo" ]
    assert [ -f "${BATS_TEST_TMPDIR}/dir/foo/a" ]
    assert [ "$(cat "${BATS_TEST_TMPDIR}/dir/foo/a")" = 'foo' ]
    (cd "${BATS_TEST_TMPDIR}/dir/foo" && assert [ "$(git branch --show-current)" = 'main' ])
    assert [ ! -e "${BATS_TEST_TMPDIR}/dir/bar" ]
}

@test 'dotfiles-package-root-valid usage' {
    run_script "${BIN_DIR}/dotfiles-package-root-valid"
    assert_failure
    assert_line --partial 'Usage:'
}

@test 'dotfiles-package-root-valid good' {
    run_script "${BIN_DIR}/dotfiles-package-root-valid" '/foo/bar'
    assert_success
    refute_output --partial 'Usage:'
}

@test 'dotfiles-package-root-valid bad relative' {
    run_script "${BIN_DIR}/dotfiles-package-root-valid" 'foo/bar'
    assert_failure
    refute_output --partial 'Usage:'
}

@test 'dotfiles-package-root-valid bad colon' {
    run_script "${BIN_DIR}/dotfiles-package-root-valid" '/foo:bar'
    assert_failure
    refute_output --partial 'Usage:'
}

@test 'dotfiles-package-root-valid bad newline' {
    run_script "${BIN_DIR}/dotfiles-package-root-valid" "$(printf "/foo\nbar")"
    assert_failure
    refute_output --partial 'Usage:'
}

@test 'dotfiles-package-name-valid usage' {
    run_script "${BIN_DIR}/dotfiles-package-name-valid"
    assert_failure
    assert_line --partial 'Usage:'
}

@test 'dotfiles-package-name-valid good' {
    run_script "${BIN_DIR}/dotfiles-package-name-valid" 'foo'
    assert_success
    refute_output --partial 'Usage:'
}

@test 'dotfiles-package-name-valid bad slash' {
    run_script "${BIN_DIR}/dotfiles-package-name-valid" 'foo/bar'
    assert_failure
    refute_output --partial 'Usage:'
}

@test 'dotfiles-package-name-valid bad space' {
    run_script "${BIN_DIR}/dotfiles-package-name-valid" 'foo bar'
    assert_failure
    refute_output --partial 'Usage:'
}

@test 'dotfiles-package-name-valid bad colon' {
    run_script "${BIN_DIR}/dotfiles-package-name-valid" 'foo:bar'
    assert_failure
    refute_output --partial 'Usage:'
}

@test 'dotfiles-package-name-valid bad newline' {
    run_script "${BIN_DIR}/dotfiles-package-name-valid" "$(printf "foo\nbar")"
    assert_failure
    refute_output --partial 'Usage:'
}

@test 'dotfiles-package-ignored usage' {
    run_script "${BIN_DIR}/dotfiles-package-ignored"
    assert_failure
    assert_line --partial 'Usage:'
}

@test 'dotfiles-package-ignored invalid package' {
    run_script "PATH=${BIN_DIR}:${PATH}" "${BIN_DIR}/dotfiles-package-ignored" 'foo/bar'
    assert_failure
    assert_line --partial 'Usage:'
}

@test 'dotfiles-package-ignored no file' {
    run_script "PATH=${BIN_DIR}:${PATH}" "DOTFILES_PACKAGE_IGNORE_FILE=${BATS_TEST_TMPDIR}/a" "${BIN_DIR}/dotfiles-package-ignored" 'foo'
    assert_failure
    refute_output --partial 'Usage:'
}

@test 'dotfiles-package-ignored empty file' {
    local IGNORE="${BATS_TEST_TMPDIR}/a"
    touch "${IGNORE}"
    run_script "PATH=${BIN_DIR}:${PATH}" "DOTFILES_PACKAGE_IGNORE_FILE=${IGNORE}" "${BIN_DIR}/dotfiles-package-ignored" 'foo'
    assert_failure
    refute_output --partial 'Usage:'
}

@test 'dotfiles-package-ignored success' {
    local IGNORE="${BATS_TEST_TMPDIR}/a"
    printf "foo\nbar\nbaz\n" >"${IGNORE}"
    run_script "PATH=${BIN_DIR}:${PATH}" "DOTFILES_PACKAGE_IGNORE_FILE=${IGNORE}" "${BIN_DIR}/dotfiles-package-ignored" 'foo'
    assert_success
    refute_output --partial 'Usage:'
}

@test 'dotfiles-package-ignored failure' {
    local IGNORE="${BATS_TEST_TMPDIR}/a"
    printf "foo\nbar\nbaz\n" >"${IGNORE}"
    run_script "PATH=${BIN_DIR}:${PATH}" "DOTFILES_PACKAGE_IGNORE_FILE=${IGNORE}" "${BIN_DIR}/dotfiles-package-ignored" 'quux'
    assert_failure
    refute_output --partial 'Usage:'
}

@test 'dotfiles-package-ignore usage' {
    run_script "PATH=${BIN_DIR}:${PATH}" "${BIN_DIR}/dotfiles-package-ignore"
    assert_failure
    assert_line --partial 'Usage:'
}

@test 'dotfiles-package-ignore invalid package' {
    run_script "PATH=${BIN_DIR}:${PATH}" "${BIN_DIR}/dotfiles-package-ignore" 'foo/bar'
    assert_failure
    assert_line --partial 'Usage:'
}

@test 'dotfiles-package-ignore no file' {
    local IGNORE_DIR="${BATS_TEST_TMPDIR}/a"
    local IGNORE_FILE="${IGNORE_DIR}/a"
    run_script "PATH=${BIN_DIR}:${PATH}" "DOTFILES_PACKAGE_INSTALL_DIR=${IGNORE_DIR}" "DOTFILES_PACKAGE_IGNORE_FILE=${IGNORE_FILE}" "${BIN_DIR}/dotfiles-package-ignore" 'foo' 'bar'
    assert_success
    refute_output --partial 'Usage:'
    assert [ "$(wc -l <"${IGNORE_FILE}")" = '2' ]
    assert [ "$(cat "${IGNORE_FILE}")" = "$(printf "foo\nbar")" ]
}

@test 'dotfiles-package-ignore new' {
    local IGNORE_DIR="${BATS_TEST_TMPDIR}/a"
    local IGNORE_FILE="${IGNORE_DIR}/a"
    mkdir -p "${IGNORE_DIR}"
    echo 'foo' >"${IGNORE_FILE}"
    run_script "PATH=${BIN_DIR}:${PATH}" "DOTFILES_PACKAGE_INSTALL_DIR=${IGNORE_DIR}" "DOTFILES_PACKAGE_IGNORE_FILE=${IGNORE_FILE}" "${BIN_DIR}/dotfiles-package-ignore" 'bar' 'baz'
    assert_success
    refute_output --partial 'Usage:'
    assert [ "$(wc -l <"${IGNORE_FILE}")" = '3' ]
    assert [ "$(cat "${IGNORE_FILE}")" = "$(printf 'foo\nbar\nbaz')" ]
}

@test 'dotfiles-package-ignore existing' {
    local IGNORE_DIR="${BATS_TEST_TMPDIR}/a"
    local IGNORE_FILE="${IGNORE_DIR}/a"
    mkdir -p "${IGNORE_DIR}"
    echo 'foo' >"${IGNORE_FILE}"
    run_script "PATH=${BIN_DIR}:${PATH}" "DOTFILES_PACKAGE_INSTALL_DIR=${IGNORE_DIR}" "DOTFILES_PACKAGE_IGNORE_FILE=${IGNORE_FILE}" "${BIN_DIR}/dotfiles-package-ignore" 'foo' 'bar'
    assert_success
    refute_output --partial 'Usage:'
    assert [ "$(wc -l <"${IGNORE_FILE}")" = '2' ]
    assert [ "$(cat "${IGNORE_FILE}")" = "$(printf "foo\nbar")" ]
}

@test 'dotfiles-package-unignore usage' {
    run_script "PATH=${BIN_DIR}:${PATH}" "${BIN_DIR}/dotfiles-package-unignore"
    assert_failure
    assert_line --partial 'Usage:'
}

@test 'dotfiles-package-unignore invalid package' {
    run_script "PATH=${BIN_DIR}:${PATH}" "${BIN_DIR}/dotfiles-package-unignore" 'foo/bar'
    assert_failure
    assert_line --partial 'Usage:'
}

@test 'dotfiles-package-unignore no file' {
    local IGNORE_FILE="${BATS_TEST_TMPDIR}/a"
    run_script "PATH=${BIN_DIR}:${PATH}" "DOTFILES_PACKAGE_IGNORE_FILE=${IGNORE_FILE}" "${BIN_DIR}/dotfiles-package-unignore" 'foo' 'bar'
    assert_success
    refute_output --partial 'Usage:'
    assert [ ! -e "${IGNORE_FILE}" ]
}

@test 'dotfiles-package-unignore in file' {
    local IGNORE_FILE="${BATS_TEST_TMPDIR}/a"
    printf 'foo\nbar\nbaz\n' >"${IGNORE_FILE}"
    run_script "PATH=${BIN_DIR}:${PATH}" "DOTFILES_PACKAGE_IGNORE_FILE=${IGNORE_FILE}" "${BIN_DIR}/dotfiles-package-unignore" 'foo' 'bar'
    assert_success
    refute_output --partial 'Usage:'
    assert [ "$(wc -l <"${IGNORE_FILE}")" = '1' ]
    assert [ "$(cat "${IGNORE_FILE}")" = 'baz' ]
}

@test 'dotfiles-package-unignore not in file' {
    local IGNORE_FILE="${BATS_TEST_TMPDIR}/a"
    printf 'foo\nbar\nbaz\n' >"${IGNORE_FILE}"
    run_script "PATH=${BIN_DIR}:${PATH}" "DOTFILES_PACKAGE_IGNORE_FILE=${IGNORE_FILE}" "${BIN_DIR}/dotfiles-package-unignore" 'quux' 'yay'
    assert_success
    refute_output --partial 'Usage:'
    assert [ "$(wc -l <"${IGNORE_FILE}")" = '3' ]
    assert [ "$(cat "${IGNORE_FILE}")" = "$(printf 'foo\nbar\nbaz')" ]
}

@test 'dotfiles-package-installed usage' {
    run_script "${BIN_DIR}/dotfiles-package-installed"
    assert_failure
    assert_line --partial 'Usage:'
}

@test 'dotfiles-package-installed invalid package' {
    run_script "PATH=${BIN_DIR}:${PATH}" "${BIN_DIR}/dotfiles-package-installed" 'foo/bar'
    assert_failure
    assert_line --partial 'Usage:'
}

@test 'dotfiles-package-installed success' {
    touch "${BATS_TEST_TMPDIR}/foo.installed"
    run_script "PATH=${BIN_DIR}:${PATH}" "DOTFILES_PACKAGE_INSTALL_DIR=${BATS_TEST_TMPDIR}" "${BIN_DIR}/dotfiles-package-installed" 'foo'
    assert_success
    refute_output --partial 'Usage:'
}

@test 'dotfiles-package-installed failure' {
    run_script "PATH=${BIN_DIR}:${PATH}" "DOTFILES_PACKAGE_INSTALL_DIR=${BATS_TEST_TMPDIR}" "${BIN_DIR}/dotfiles-package-installed" 'foo'
    assert_failure
    refute_output --partial 'Usage:'
}

@test 'dotfiles-package-source-path usage' {
    run_script "${BIN_DIR}/dotfiles-package-source-path"
    assert_failure
    assert_line --partial 'Usage:'
}

@test 'dotfiles-package-source-path name invalid' {
    run_script "PATH=${BIN_DIR}:${PATH}" "DOTFILES_PACKAGE_ROOTS=:${BATS_TEST_TMPDIR}/a::${BATS_TEST_TMPDIR}/b::${BATS_TEST_TMPDIR}/c" "${BIN_DIR}/dotfiles-package-source-path" 'foo bar'
    assert_failure
    assert_line --partial 'Usage:'
}

@test 'dotfiles-package-source-path name found' {
    mkdir -p "${BATS_TEST_TMPDIR}/a"
    mkdir -p "${BATS_TEST_TMPDIR}/b/foo"
    run_script "PATH=${BIN_DIR}:${PATH}" "DOTFILES_PACKAGE_ROOTS=:${BATS_TEST_TMPDIR}/a::${BATS_TEST_TMPDIR}/b:${BATS_TEST_TMPDIR}/c" "${BIN_DIR}/dotfiles-package-source-path" 'foo'
    assert_success
    assert_line "${BATS_TEST_TMPDIR}/b/foo"
    refute_output --partial 'Usage:'
}

@test 'dotfiles-package-source-path name not found' {
    mkdir -p "${BATS_TEST_TMPDIR}/a"
    mkdir -p "${BATS_TEST_TMPDIR}/b/foo"
    run_script "PATH=${BIN_DIR}:${PATH}" "DOTFILES_PACKAGE_ROOTS=:${BATS_TEST_TMPDIR}/a::${BATS_TEST_TMPDIR}/b::${BATS_TEST_TMPDIR}/c" "${BIN_DIR}/dotfiles-package-source-path" 'bar'
    assert_failure
    refute_output
    refute_output --partial 'Usage:'
}

@test 'dotfiles-package-source-path name skip invalid roots' {
    mkdir -p "${BATS_TEST_TMPDIR}/a"
    mkdir -p "$(printf '%s/a\nb/foo' "${BATS_TEST_TMPDIR}")"
    run_script "PATH=${BIN_DIR}:${PATH}" "$(printf '%s' "DOTFILES_PACKAGE_ROOTS=:${BATS_TEST_TMPDIR}/a::${BATS_TEST_TMPDIR}/a\nb::${BATS_TEST_TMPDIR}/c")" "${BIN_DIR}/dotfiles-package-source-path" 'foo'
    assert_failure
    refute_output
    refute_output --partial 'Usage:'
}

@test 'dotfiles-package-source-path path invalid root' {
    run_script "PATH=${BIN_DIR}:${PATH}" "${BIN_DIR}/dotfiles-package-source-path" 'foo/bar'
    assert_failure
    assert_line --partial 'Usage:'
}

@test 'dotfiles-package-source-path path invalid name' {
    run_script "PATH=${BIN_DIR}:${PATH}" "${BIN_DIR}/dotfiles-package-source-path" '/foo/bar baz'
    assert_failure
    assert_line --partial 'Usage:'
}

@test 'dotfiles-package-source-path path' {
    run_script "PATH=${BIN_DIR}:${PATH}" "${BIN_DIR}/dotfiles-package-source-path" '/foo/bar'
    assert_success
    assert_line "/foo/bar"
    refute_line --partial 'Usage:'
}

@test 'dotfiles-package-has-installer usage' {
    run_script "${BIN_DIR}/dotfiles-package-has-installer"
    assert_failure
    assert_line --partial 'Usage:'
}

@test 'dotfiles-package-has-installer nonexistent package' {
    mkdir -p "${BATS_TEST_TMPDIR}/a"
    run_script "PATH=${BIN_DIR}:${PATH}" "DOTFILES_PACKAGE_ROOTS=${BATS_TEST_TMPDIR}/a" "${BIN_DIR}/dotfiles-package-has-installer" 'foo'
    assert_failure
    refute_output --partial 'Usage:'
}

@test 'dotfiles-package-has-installer success' {
    mkdir -p "${BATS_TEST_TMPDIR}/a/foo"
    touch "${BATS_TEST_TMPDIR}/a/foo/install"
    run_script "PATH=${BIN_DIR}:${PATH}" "DOTFILES_PACKAGE_ROOTS=${BATS_TEST_TMPDIR}/a" "${BIN_DIR}/dotfiles-package-has-installer" 'foo'
    assert_success
    refute_output --partial 'Usage:'
}

@test 'dotfiles-package-has-installer failure' {
    mkdir -p "${BATS_TEST_TMPDIR}/a/foo"
    run_script "PATH=${BIN_DIR}:${PATH}" "DOTFILES_PACKAGE_ROOTS=${BATS_TEST_TMPDIR}/a" "${BIN_DIR}/dotfiles-package-has-installer" 'foo'
    assert_failure
    refute_output --partial 'Usage:'
}

@test 'dotfiles-package-has-installer path success' {
    mkdir -p "${BATS_TEST_TMPDIR}/a/foo"
    touch "${BATS_TEST_TMPDIR}/a/foo/install"
    run_script "PATH=${BIN_DIR}:${PATH}" "${BIN_DIR}/dotfiles-package-has-installer" "${BATS_TEST_TMPDIR}/a/foo"
    assert_success
    refute_output --partial 'Usage:'
}

@test 'dotfiles-package-has-installer path failure' {
    mkdir -p "${BATS_TEST_TMPDIR}/a/foo"
    run_script "PATH=${BIN_DIR}:${PATH}" "${BIN_DIR}/dotfiles-package-has-installer" "${BATS_TEST_TMPDIR}/a/foo"
    assert_failure
    refute_output --partial 'Usage:'
}

@test 'dotfiles-package-can-install usage' {
    run_script "${BIN_DIR}/dotfiles-package-can-install"
    assert_failure
    assert_line --partial 'Usage:'
}

@test 'dotfiles-package-can-install nonexistent package' {
    mkdir -p "${BATS_TEST_TMPDIR}/a"
    run_script "PATH=${BIN_DIR}:${PATH}" "DOTFILES_PACKAGE_ROOTS=${BATS_TEST_TMPDIR}/a" "${BIN_DIR}/dotfiles-package-can-install" 'foo'
    assert_failure
    refute_output --partial 'Usage:'
}

@test 'dotfiles-package-can-install package without installer' {
    mkdir -p "${BATS_TEST_TMPDIR}/a/foo"
    run_script "PATH=${BIN_DIR}:${PATH}" "DOTFILES_PACKAGE_ROOTS=${BATS_TEST_TMPDIR}/a" "${BIN_DIR}/dotfiles-package-can-install" 'foo'
    assert_failure
    refute_output --partial 'Usage:'
}

@test 'dotfiles-package-can-install package with installer, no requirements' {
    mkdir -p "${BATS_TEST_TMPDIR}/a/foo"
    touch "${BATS_TEST_TMPDIR}/a/foo/install"
    run_script "PATH=${BIN_DIR}:${PATH}" "DOTFILES_PACKAGE_ROOTS=${BATS_TEST_TMPDIR}/a" "${BIN_DIR}/dotfiles-package-can-install" 'foo'
    assert_success
    refute_output --partial 'Usage:'
}

@test 'dotfiles-package-can-install requirements met' {
    mkdir -p "${BATS_TEST_TMPDIR}/a/foo"
    touch "${BATS_TEST_TMPDIR}/a/foo/install"
    printf '#!/bin/bash\ntrue\n' >"${BATS_TEST_TMPDIR}/a/foo/can-install"
    chmod u+x "${BATS_TEST_TMPDIR}/a/foo/can-install"
    run_script "PATH=${BIN_DIR}:${PATH}" "DOTFILES_PACKAGE_ROOTS=${BATS_TEST_TMPDIR}/a" "${BIN_DIR}/dotfiles-package-can-install" 'foo'
    assert_success
    refute_output --partial 'Usage:'
}

@test 'dotfiles-package-can-install requirements unmet' {
    mkdir -p "${BATS_TEST_TMPDIR}/a/foo"
    touch "${BATS_TEST_TMPDIR}/a/foo/install"
    printf '#!/bin/bash\nfalse\n' >"${BATS_TEST_TMPDIR}/a/foo/can-install"
    chmod u+x "${BATS_TEST_TMPDIR}/a/foo/can-install"
    run_script "PATH=${BIN_DIR}:${PATH}" "DOTFILES_PACKAGE_ROOTS=${BATS_TEST_TMPDIR}/a" "${BIN_DIR}/dotfiles-package-can-install" 'foo'
    assert_failure
    refute_output --partial 'Usage:'
}

@test 'dotfiles-package-can-install nonexistent path' {
    mkdir -p "${BATS_TEST_TMPDIR}/a"
    run_script "PATH=${BIN_DIR}:${PATH}" "${BIN_DIR}/dotfiles-package-can-install" "${BATS_TEST_TMPDIR}/a/foo"
    assert_failure
    refute_output --partial 'Usage:'
}

@test 'dotfiles-package-can-install path without installer' {
    mkdir -p "${BATS_TEST_TMPDIR}/a/foo"
    run_script "PATH=${BIN_DIR}:${PATH}" "${BIN_DIR}/dotfiles-package-can-install" "${BATS_TEST_TMPDIR}/a/foo"
    assert_failure
    refute_output --partial 'Usage:'
}

@test 'dotfiles-package-can-install path with installer, no requirements' {
    mkdir -p "${BATS_TEST_TMPDIR}/a/foo"
    touch "${BATS_TEST_TMPDIR}/a/foo/install"
    run_script "PATH=${BIN_DIR}:${PATH}" "${BIN_DIR}/dotfiles-package-can-install" "${BATS_TEST_TMPDIR}/a/foo"
    assert_success
    refute_output --partial 'Usage:'
}

@test 'dotfiles-package-can-install path requirements met' {
    mkdir -p "${BATS_TEST_TMPDIR}/a/foo"
    touch "${BATS_TEST_TMPDIR}/a/foo/install"
    printf '#!/bin/bash\ntrue\n' >"${BATS_TEST_TMPDIR}/a/foo/can-install"
    chmod u+x "${BATS_TEST_TMPDIR}/a/foo/can-install"
    run_script "PATH=${BIN_DIR}:${PATH}" "${BIN_DIR}/dotfiles-package-can-install" "${BATS_TEST_TMPDIR}/a/foo"
    assert_success
    refute_output --partial 'Usage:'
}

@test 'dotfiles-package-can-install path requirements unmet' {
    mkdir -p "${BATS_TEST_TMPDIR}/a/foo"
    touch "${BATS_TEST_TMPDIR}/a/foo/install"
    printf '#!/bin/bash\nfalse\n' >"${BATS_TEST_TMPDIR}/a/foo/can-install"
    chmod u+x "${BATS_TEST_TMPDIR}/a/foo/can-install"
    run_script "PATH=${BIN_DIR}:${PATH}" "${BIN_DIR}/dotfiles-package-can-install" "${BATS_TEST_TMPDIR}/a/foo"
    assert_failure
    refute_output --partial 'Usage:'
}

@test 'dotfiles-package-list too many args' {
    local INSTALL_DIR="${BATS_TEST_TMPDIR}/install"
    local IGNORE_FILE="${BATS_TEST_TMPDIR}/ignore"
    run_script "PATH=${BIN_DIR}:${PATH}" "DOTFILES_PACKAGE_INSTALL_DIR=${INSTALL_DIR}" "DOTFILES_PACKAGE_IGNORE_FILE=${IGNORE_FILE}" "$(printf 'DOTFILES_PACKAGE_ROOTS=%s/a::%s/b:%s/c:%s/d\ne' "${BATS_TEST_TMPDIR}" "${BATS_TEST_TMPDIR}" "${BATS_TEST_TMPDIR}" "${BATS_TEST_TMPDIR}")" "${BIN_DIR}/dotfiles-package-list" 'a' 'b'
    assert_failure
    assert_line --partial 'Too many args'
}

@test 'dotfiles-package-list bad command' {
    local INSTALL_DIR="${BATS_TEST_TMPDIR}/install"
    local IGNORE_FILE="${BATS_TEST_TMPDIR}/ignore"
    run_script "PATH=${BIN_DIR}:${PATH}" "DOTFILES_PACKAGE_INSTALL_DIR=${INSTALL_DIR}" "DOTFILES_PACKAGE_IGNORE_FILE=${IGNORE_FILE}" "$(printf 'DOTFILES_PACKAGE_ROOTS=%s/a::%s/b:%s/c:%s/d\ne' "${BATS_TEST_TMPDIR}" "${BATS_TEST_TMPDIR}" "${BATS_TEST_TMPDIR}" "${BATS_TEST_TMPDIR}")" "${BIN_DIR}/dotfiles-package-list" 'a'
    assert_failure
    assert_line --partial 'Unknown command'
}

@test 'dotfiles-package-list status' {
    local INSTALL_DIR="${BATS_TEST_TMPDIR}/install"
    mkdir -p "${INSTALL_DIR}"
    local IGNORE_FILE="${BATS_TEST_TMPDIR}/ignore"

    mkdir -p "${BATS_TEST_TMPDIR}/a/foo"

    mkdir -p "${BATS_TEST_TMPDIR}/a/foo bar"

    mkdir -p "${BATS_TEST_TMPDIR}/a/bar"
    echo 'bar' >>"${IGNORE_FILE}"

    mkdir -p "${BATS_TEST_TMPDIR}/a/baz"
    touch "${BATS_TEST_TMPDIR}/a/baz/install"
    touch "${INSTALL_DIR}/baz.installed"

    mkdir -p "${BATS_TEST_TMPDIR}/b/quux"
    touch "${BATS_TEST_TMPDIR}/b/quux/install"

    mkdir -p "${BATS_TEST_TMPDIR}/b/blah"
    touch "${BATS_TEST_TMPDIR}/b/blah/install"
    printf "#!/bin/bash\ntrue\n" >"${BATS_TEST_TMPDIR}/b/blah/can-install"
    chmod u+x "${BATS_TEST_TMPDIR}/b/blah/can-install"

    mkdir -p "${BATS_TEST_TMPDIR}/b/yay"
    touch "${BATS_TEST_TMPDIR}/b/yay/install"
    printf "#!/bin/bash\nfalse\n" >"${BATS_TEST_TMPDIR}/b/yay/can-install"
    chmod u+x "${BATS_TEST_TMPDIR}/b/yay/can-install"

    mkdir -p "${BATS_TEST_TMPDIR}/b/stuff"
    touch "${BATS_TEST_TMPDIR}/b/stuff/install"
    echo 'stuff' >>"${IGNORE_FILE}"
    touch "${INSTALL_DIR}/stuff.installed"

    mkdir -p "${BATS_TEST_TMPDIR}/b/foo"

    mkdir -p "$(printf '%s/d\ne/other' "${BATS_TEST_TMPDIR}")"

    run_script "PATH=${BIN_DIR}:${PATH}" "DOTFILES_PACKAGE_INSTALL_DIR=${INSTALL_DIR}" "DOTFILES_PACKAGE_IGNORE_FILE=${IGNORE_FILE}" "$(printf 'DOTFILES_PACKAGE_ROOTS=%s/a::%s/b:%s/c:%s/d\ne' "${BATS_TEST_TMPDIR}" "${BATS_TEST_TMPDIR}" "${BATS_TEST_TMPDIR}" "${BATS_TEST_TMPDIR}")" "${BIN_DIR}/dotfiles-package-list"
    assert_success
    assert_output "WARNING: invalid package roots found: ['${BATS_TEST_TMPDIR}/d\\ne']

WARNING: invalid package names found: ['foo bar']

WARNING: duplicate package names found; only the first instance of each will be used: foo

Active without installing
    foo

Installed
    baz

Available to install ('dotfiles-package-install' to install)
    blah
    quux

Not available to install
    yay

Ignored ('dotfiles-package-unignore' to unignore)
    bar
    stuff"
}

@test 'dotfiles-package-list can-ignore' {
    local INSTALL_DIR="${BATS_TEST_TMPDIR}/install"
    mkdir -p "${INSTALL_DIR}"
    local IGNORE_FILE="${BATS_TEST_TMPDIR}/ignore"

    mkdir -p "${BATS_TEST_TMPDIR}/a/foo"

    mkdir -p "${BATS_TEST_TMPDIR}/a/foo bar"

    mkdir -p "${BATS_TEST_TMPDIR}/a/bar"
    echo 'bar' >>"${IGNORE_FILE}"

    mkdir -p "${BATS_TEST_TMPDIR}/a/baz"
    touch "${BATS_TEST_TMPDIR}/a/baz/install"
    touch "${INSTALL_DIR}/baz.installed"

    mkdir -p "${BATS_TEST_TMPDIR}/b/quux"
    touch "${BATS_TEST_TMPDIR}/b/quux/install"

    mkdir -p "${BATS_TEST_TMPDIR}/b/blah"
    touch "${BATS_TEST_TMPDIR}/b/blah/install"
    printf "#!/bin/bash\ntrue\n" >"${BATS_TEST_TMPDIR}/b/blah/can-install"
    chmod u+x "${BATS_TEST_TMPDIR}/b/blah/can-install"

    mkdir -p "${BATS_TEST_TMPDIR}/b/yay"
    touch "${BATS_TEST_TMPDIR}/b/yay/install"
    printf "#!/bin/bash\nfalse\n" >"${BATS_TEST_TMPDIR}/b/yay/can-install"
    chmod u+x "${BATS_TEST_TMPDIR}/b/yay/can-install"

    mkdir -p "${BATS_TEST_TMPDIR}/b/stuff"
    touch "${BATS_TEST_TMPDIR}/b/stuff/install"
    echo 'stuff' >>"${IGNORE_FILE}"
    touch "${INSTALL_DIR}/stuff.installed"

    mkdir -p "${BATS_TEST_TMPDIR}/b/foo"

    mkdir -p "$(printf '%s/d\ne/other' "${BATS_TEST_TMPDIR}")"

    run_script "PATH=${BIN_DIR}:${PATH}" "DOTFILES_PACKAGE_INSTALL_DIR=${INSTALL_DIR}" "DOTFILES_PACKAGE_IGNORE_FILE=${IGNORE_FILE}" "$(printf 'DOTFILES_PACKAGE_ROOTS=%s/a::%s/b:%s/c:%s/d\ne' "${BATS_TEST_TMPDIR}" "${BATS_TEST_TMPDIR}" "${BATS_TEST_TMPDIR}" "${BATS_TEST_TMPDIR}")" "${BIN_DIR}/dotfiles-package-list" 'can-ignore'
    assert_success
    assert_output "baz
blah
foo
quux"
}

@test 'dotfiles-package-list can-unignore' {
    local INSTALL_DIR="${BATS_TEST_TMPDIR}/install"
    mkdir -p "${INSTALL_DIR}"
    local IGNORE_FILE="${BATS_TEST_TMPDIR}/ignore"

    mkdir -p "${BATS_TEST_TMPDIR}/a/foo"

    mkdir -p "${BATS_TEST_TMPDIR}/a/foo bar"

    mkdir -p "${BATS_TEST_TMPDIR}/a/bar"
    echo 'bar' >>"${IGNORE_FILE}"

    mkdir -p "${BATS_TEST_TMPDIR}/a/baz"
    touch "${BATS_TEST_TMPDIR}/a/baz/install"
    touch "${INSTALL_DIR}/baz.installed"

    mkdir -p "${BATS_TEST_TMPDIR}/b/quux"
    touch "${BATS_TEST_TMPDIR}/b/quux/install"

    mkdir -p "${BATS_TEST_TMPDIR}/b/blah"
    touch "${BATS_TEST_TMPDIR}/b/blah/install"
    printf "#!/bin/bash\ntrue\n" >"${BATS_TEST_TMPDIR}/b/blah/can-install"
    chmod u+x "${BATS_TEST_TMPDIR}/b/blah/can-install"

    mkdir -p "${BATS_TEST_TMPDIR}/b/yay"
    touch "${BATS_TEST_TMPDIR}/b/yay/install"
    printf "#!/bin/bash\nfalse\n" >"${BATS_TEST_TMPDIR}/b/yay/can-install"
    chmod u+x "${BATS_TEST_TMPDIR}/b/yay/can-install"

    mkdir -p "${BATS_TEST_TMPDIR}/b/stuff"
    touch "${BATS_TEST_TMPDIR}/b/stuff/install"
    echo 'stuff' >>"${IGNORE_FILE}"
    touch "${INSTALL_DIR}/stuff.installed"

    mkdir -p "${BATS_TEST_TMPDIR}/b/foo"

    mkdir -p "$(printf '%s/d\ne/other' "${BATS_TEST_TMPDIR}")"

    run_script "PATH=${BIN_DIR}:${PATH}" "DOTFILES_PACKAGE_INSTALL_DIR=${INSTALL_DIR}" "DOTFILES_PACKAGE_IGNORE_FILE=${IGNORE_FILE}" "$(printf 'DOTFILES_PACKAGE_ROOTS=%s/a::%s/b:%s/c:%s/d\ne' "${BATS_TEST_TMPDIR}" "${BATS_TEST_TMPDIR}" "${BATS_TEST_TMPDIR}" "${BATS_TEST_TMPDIR}")" "${BIN_DIR}/dotfiles-package-list" 'can-unignore'
    assert_success
    assert_output "bar
stuff"
}

@test 'dotfiles-package-list can-install' {
    local INSTALL_DIR="${BATS_TEST_TMPDIR}/install"
    mkdir -p "${INSTALL_DIR}"
    local IGNORE_FILE="${BATS_TEST_TMPDIR}/ignore"

    mkdir -p "${BATS_TEST_TMPDIR}/a/foo"

    mkdir -p "${BATS_TEST_TMPDIR}/a/foo bar"

    mkdir -p "${BATS_TEST_TMPDIR}/a/bar"
    echo 'bar' >>"${IGNORE_FILE}"

    mkdir -p "${BATS_TEST_TMPDIR}/a/baz"
    touch "${BATS_TEST_TMPDIR}/a/baz/install"
    touch "${INSTALL_DIR}/baz.installed"

    mkdir -p "${BATS_TEST_TMPDIR}/b/quux"
    touch "${BATS_TEST_TMPDIR}/b/quux/install"

    mkdir -p "${BATS_TEST_TMPDIR}/b/blah"
    touch "${BATS_TEST_TMPDIR}/b/blah/install"
    printf "#!/bin/bash\ntrue\n" >"${BATS_TEST_TMPDIR}/b/blah/can-install"
    chmod u+x "${BATS_TEST_TMPDIR}/b/blah/can-install"

    mkdir -p "${BATS_TEST_TMPDIR}/b/yay"
    touch "${BATS_TEST_TMPDIR}/b/yay/install"
    printf "#!/bin/bash\nfalse\n" >"${BATS_TEST_TMPDIR}/b/yay/can-install"
    chmod u+x "${BATS_TEST_TMPDIR}/b/yay/can-install"

    mkdir -p "${BATS_TEST_TMPDIR}/b/stuff"
    touch "${BATS_TEST_TMPDIR}/b/stuff/install"
    echo 'stuff' >>"${IGNORE_FILE}"
    touch "${INSTALL_DIR}/stuff.installed"

    mkdir -p "${BATS_TEST_TMPDIR}/b/foo"

    mkdir -p "$(printf '%s/d\ne/other' "${BATS_TEST_TMPDIR}")"

    run_script "PATH=${BIN_DIR}:${PATH}" "DOTFILES_PACKAGE_INSTALL_DIR=${INSTALL_DIR}" "DOTFILES_PACKAGE_IGNORE_FILE=${IGNORE_FILE}" "$(printf 'DOTFILES_PACKAGE_ROOTS=%s/a::%s/b:%s/c:%s/d\ne' "${BATS_TEST_TMPDIR}" "${BATS_TEST_TMPDIR}" "${BATS_TEST_TMPDIR}" "${BATS_TEST_TMPDIR}")" "${BIN_DIR}/dotfiles-package-list" 'can-install'
    assert_success
    assert_output "blah
quux"
}

@test 'dotfiles-package-lock not locked' {
    run_script "PATH=${BIN_DIR}:${PATH}" "DOTFILES_PACKAGE_MUTEX=${BATS_TEST_TMPDIR}/a" "${BIN_DIR}/dotfiles-package-lock"
    assert_success
    assert [ -d "${BATS_TEST_TMPDIR}/a" ]
}

@test 'dotfiles-package-lock locked' {
    mkdir -p "${BATS_TEST_TMPDIR}/a"
    run_script "PATH=${BIN_DIR}:${PATH}" "DOTFILES_PACKAGE_MUTEX=${BATS_TEST_TMPDIR}/a" "${BIN_DIR}/dotfiles-package-lock"
    assert_failure
    assert [ -d "${BATS_TEST_TMPDIR}/a" ]
}

@test 'dotfiles-package-lock locked, no lock' {
    mkdir -p "${BATS_TEST_TMPDIR}/a"
    run_script "PATH=${BIN_DIR}:${PATH}" "DOTFILES_PACKAGE_MUTEX=${BATS_TEST_TMPDIR}/a" "DOTFILES_NO_PACKAGE_LOCK=t" "${BIN_DIR}/dotfiles-package-lock"
    assert_success
    assert [ -d "${BATS_TEST_TMPDIR}/a" ]
}

@test 'dotfiles-package-unlock not locked' {
    run_script "PATH=${BIN_DIR}:${PATH}" "DOTFILES_PACKAGE_MUTEX=${BATS_TEST_TMPDIR}/a" "${BIN_DIR}/dotfiles-package-unlock"
    assert_success
    assert [ ! -e "${BATS_TEST_TMPDIR}/a" ]
}

@test 'dotfiles-package-unlock locked' {
    mkdir -p "${BATS_TEST_TMPDIR}/a"
    run_script "PATH=${BIN_DIR}:${PATH}" "DOTFILES_PACKAGE_MUTEX=${BATS_TEST_TMPDIR}/a" "${BIN_DIR}/dotfiles-package-unlock"
    assert_success
    assert [ ! -e "${BATS_TEST_TMPDIR}/a" ]
}

@test 'dotfiles-package-unlock locked, no lock' {
    mkdir -p "${BATS_TEST_TMPDIR}/a"
    run_script "PATH=${BIN_DIR}:${PATH}" "DOTFILES_PACKAGE_MUTEX=${BATS_TEST_TMPDIR}/a" "DOTFILES_NO_PACKAGE_LOCK=t" "${BIN_DIR}/dotfiles-package-unlock"
    assert_success
    assert [ -d "${BATS_TEST_TMPDIR}/a" ]
}

@test 'dotfiles-private-repo needs lock' {
    local MUTEX="${BATS_TEST_TMPDIR}/mutex"
    mkdir -p "${MUTEX}"
    local PRIVATE="${BATS_TEST_TMPDIR}/private"
    run_script "PATH=${BIN_DIR}:${PATH}" "DOTFILES_PACKAGE_MUTEX=${MUTEX}" "DOTFILES_PRIVATE_DIR=${PRIVATE}" "DOTFILES_PRIVATE_REPO=" "${BIN_DIR}/dotfiles-private-repo-install"
    assert_failure
    assert [ -d "${MUTEX}" ]
    assert [ ! -e "${PRIVATE}" ]
}

@test 'dotfiles-private-repo-install nothing' {
    local MUTEX="${BATS_TEST_TMPDIR}/mutex"
    local PRIVATE="${BATS_TEST_TMPDIR}/private"
    local LOGOUT="${BATS_TEST_TMPDIR}/logout"
    run_script "PATH=${BIN_DIR}:${PATH}" "DOTFILES_PACKAGE_MUTEX=${MUTEX}" "DOTFILES_PRIVATE_DIR=${PRIVATE}" "DOTFILES_PRIVATE_REPO=" "DOTFILES_NEEDS_LOGOUT=${LOGOUT}" "${BIN_DIR}/dotfiles-private-repo-install"
    assert_success
    assert [ ! -e "${MUTEX}" ]
    assert [ ! -e "${PRIVATE}" ]
    assert [ ! -e "${LOGOUT}" ]
    refute_output --partial 'Log out and log in again'
}

@test 'dotfiles-private-repo-install clone, default branch' {
    local MUTEX="${BATS_TEST_TMPDIR}/mutex"
    local PRIVATE="${BATS_TEST_TMPDIR}/private"
    local LOGOUT="${BATS_TEST_TMPDIR}/logout"
    mkdir -p "${BATS_TEST_TMPDIR}/repo"
    touch "${BATS_TEST_TMPDIR}/repo/a"
    (cd "${BATS_TEST_TMPDIR}/repo" && git_init && git add a && git commit -m 'Foo')
    run_script "PATH=${BIN_DIR}:${PATH}" "DOTFILES_PACKAGE_MUTEX=${MUTEX}" "DOTFILES_PRIVATE_DIR=${PRIVATE}" "DOTFILES_PRIVATE_REPO=${BATS_TEST_TMPDIR}/repo" "DOTFILES_PRIVATE_BRANCH=" "DOTFILES_NEEDS_LOGOUT=${LOGOUT}" "${BIN_DIR}/dotfiles-private-repo-install"
    assert_success
    assert [ ! -e "${MUTEX}" ]
    assert [ -d "${PRIVATE}" ]
    assert [ -f "${PRIVATE}/a" ]
    (cd "${PRIVATE}" && assert [ "$(git branch --show-current)" = 'main' ])
    assert [ -f "${LOGOUT}" ]
    assert_line --partial 'Log out and log in again'
}

@test 'dotfiles-private-repo-install clone, other branch' {
    local MUTEX="${BATS_TEST_TMPDIR}/mutex"
    local PRIVATE="${BATS_TEST_TMPDIR}/private"
    local LOGOUT="${BATS_TEST_TMPDIR}/logout"
    mkdir -p "${BATS_TEST_TMPDIR}/repo"
    touch "${BATS_TEST_TMPDIR}/repo/a"
    (cd "${BATS_TEST_TMPDIR}/repo" && git_init && git add a && git commit -m 'Foo' && git checkout -b dev)
    run_script "PATH=${BIN_DIR}:${PATH}" "DOTFILES_PACKAGE_MUTEX=${MUTEX}" "DOTFILES_PRIVATE_DIR=${PRIVATE}" "DOTFILES_PRIVATE_REPO=${BATS_TEST_TMPDIR}/repo" "DOTFILES_PRIVATE_BRANCH=dev" "DOTFILES_NEEDS_LOGOUT=${LOGOUT}" "${BIN_DIR}/dotfiles-private-repo-install"
    assert_success
    assert [ ! -e "${MUTEX}" ]
    assert [ -d "${PRIVATE}" ]
    assert [ -f "${PRIVATE}/a" ]
    (cd "${PRIVATE}" && assert [ "$(git branch --show-current)" = 'dev' ])
    assert [ -f "${LOGOUT}" ]
    assert_line --partial 'Log out and log in again'
}

@test 'dotfiles-private-repo-install pull, clean' {
    local MUTEX="${BATS_TEST_TMPDIR}/mutex"
    local PRIVATE="${BATS_TEST_TMPDIR}/private"
    local LOGOUT="${BATS_TEST_TMPDIR}/logout"
    mkdir -p "${BATS_TEST_TMPDIR}/repo"
    touch "${BATS_TEST_TMPDIR}/repo/a"
    (cd "${BATS_TEST_TMPDIR}/repo" && git_init && git add a && git commit -m 'Foo')
    git clone "${BATS_TEST_TMPDIR}/repo" "${PRIVATE}"
    echo 'foo' >"${BATS_TEST_TMPDIR}/repo/a"
    (cd "${BATS_TEST_TMPDIR}/repo" && git add a && git commit -m 'Bar')
    run_script "PATH=${BIN_DIR}:${PATH}" "DOTFILES_PACKAGE_MUTEX=${MUTEX}" "DOTFILES_PRIVATE_DIR=${PRIVATE}" "DOTFILES_PRIVATE_REPO=${BATS_TEST_TMPDIR}/repo" "DOTFILES_PRIVATE_BRANCH=" "DOTFILES_NEEDS_LOGOUT=${LOGOUT}" "${BIN_DIR}/dotfiles-private-repo-install"
    assert_success
    assert [ ! -e "${MUTEX}" ]
    assert [ -d "${PRIVATE}" ]
    assert [ -f "${PRIVATE}/a" ]
    assert [ "$(cat "${PRIVATE}/a")" = 'foo' ]
    (cd "${PRIVATE}" && assert [ "$(git branch --show-current)" = 'main' ])
    assert [ -f "${LOGOUT}" ]
    assert_line --partial 'Log out and log in again'
}

@test 'dotfiles-private-repo-install pull, dirty' {
    local MUTEX="${BATS_TEST_TMPDIR}/mutex"
    local PRIVATE="${BATS_TEST_TMPDIR}/private"
    local LOGOUT="${BATS_TEST_TMPDIR}/logout"
    mkdir -p "${BATS_TEST_TMPDIR}/repo"
    touch "${BATS_TEST_TMPDIR}/repo/a"
    (cd "${BATS_TEST_TMPDIR}/repo" && git_init && git add a && git commit -m 'Foo')
    git clone "${BATS_TEST_TMPDIR}/repo" "${PRIVATE}"
    echo 'foo' >"${PRIVATE}/a"
    run_script "PATH=${BIN_DIR}:${PATH}" "DOTFILES_PACKAGE_MUTEX=${MUTEX}" "DOTFILES_PRIVATE_DIR=${PRIVATE}" "DOTFILES_PRIVATE_REPO=${BATS_TEST_TMPDIR}/repo" "DOTFILES_PRIVATE_BRANCH=" "DOTFILES_NEEDS_LOGOUT=${LOGOUT}" "${BIN_DIR}/dotfiles-private-repo-install"
    assert_failure
    assert [ ! -e "${MUTEX}" ]
    assert [ -d "${PRIVATE}" ]
    assert [ -f "${PRIVATE}/a" ]
    assert [ "$(cat "${PRIVATE}/a")" = 'foo' ]
    (cd "${PRIVATE}" && assert [ "$(git branch --show-current)" = 'main' ])
    assert [ ! -e "${LOGOUT}" ]
    refute_line --partial 'Log out and log in again'
}

@test 'dotfiles-package-install usage' {
    run_script "${BIN_DIR}/dotfiles-package-install"
    assert_failure
    assert_line --partial 'Usage:'
}

@test 'dotfiles-package-install needs lock' {
    local INSTALL_DIR="${BATS_TEST_TMPDIR}/install"
    mkdir -p "${INSTALL_DIR}"
    local IGNORE_FILE="${BATS_TEST_TMPDIR}/ignore"
    mkdir -p "${BATS_TEST_TMPDIR}/mutex"

    mkdir -p "${BATS_TEST_TMPDIR}/a/foo"
    printf "#!/bin/bash\necho 'FOO INSTALLED'\n" >"${BATS_TEST_TMPDIR}/a/foo/install"
    chmod u+x "${BATS_TEST_TMPDIR}/a/foo/install"

    run_script \
        "PATH=${BIN_DIR}:${PATH}" \
        "DOTFILES_PACKAGE_SCRIPTS=${PACKAGE_SCRIPTS_DIR}" \
        "DOTFILES_PACKAGE_INSTALL_DIR=${INSTALL_DIR}" \
        "DOTFILES_PACKAGE_IGNORE_FILE=${IGNORE_FILE}" \
        "DOTFILES_PACKAGES_LOADED_ENV=" \
        "DOTFILES_PACKAGE_MUTEX=${BATS_TEST_TMPDIR}/mutex" \
        "DOTFILES_PACKAGE_ROOTS=${BATS_TEST_TMPDIR}/a" \
        "DOTFILES_NEEDS_LOGOUT=${BATS_TEST_TMPDIR}/logout" \
        "${BIN_DIR}/dotfiles-package-install" 'foo'

    assert_failure
    refute_line --partial 'Usage:'
    assert [ -d "${BATS_TEST_TMPDIR}/mutex" ]
    assert [ ! -e "${BATS_TEST_TMPDIR}/logout" ]
    refute_line --partial 'Log out and log in again'

    refute_line 'FOO INSTALLED'
    assert [ ! -e "${BATS_TEST_TMPDIR}/install/foo" ]
    assert [ ! -e "${BATS_TEST_TMPDIR}/install/foo.installed" ]
}

@test 'dotfiles-package-install install' {
    local INSTALL_DIR="${BATS_TEST_TMPDIR}/install"
    mkdir -p "${INSTALL_DIR}"
    local IGNORE_FILE="${BATS_TEST_TMPDIR}/ignore"

    mkdir -p "${BATS_TEST_TMPDIR}/a/foo"
    printf "echo 'FOO ENV'\n" >"${BATS_TEST_TMPDIR}/a/foo/env.sh"
    cat >"${BATS_TEST_TMPDIR}/a/foo/install" <<EOF
#!/bin/bash
echo "TEST_FOO_ROOT=\${PACKAGE_ROOT}"
echo "TEST_FOO_NAME=\${PACKAGE_NAME}"
echo "TEST_FOO_SOURCE_DIR=\${PACKAGE_SOURCE_DIR}"
echo "TEST_FOO_INSTALL_DIR=\${PACKAGE_INSTALL_DIR}"
echo "TEST_FOO_CWD=\$(pwd)"
EOF
    chmod u+x "${BATS_TEST_TMPDIR}/a/foo/install"

    mkdir -p "${BATS_TEST_TMPDIR}/a/bar"
    cat >"${BATS_TEST_TMPDIR}/a/bar/install" <<EOF
#!/bin/bash
echo "TEST_BAR_ROOT=\${PACKAGE_ROOT}"
echo "TEST_BAR_NAME=\${PACKAGE_NAME}"
echo "TEST_BAR_SOURCE_DIR=\${PACKAGE_SOURCE_DIR}"
echo "TEST_BAR_INSTALL_DIR=\${PACKAGE_INSTALL_DIR}"
echo "TEST_BAR_CWD=\$(pwd)"
EOF
    chmod u+x "${BATS_TEST_TMPDIR}/a/bar/install"
    printf "#!/bin/bash\necho 'BAR CAN INSTALL'\n" >"${BATS_TEST_TMPDIR}/a/bar/can-install"
    chmod u+x "${BATS_TEST_TMPDIR}/a/bar/can-install"

    run_script \
        "PATH=${BIN_DIR}:${PATH}" \
        "DOTFILES_PACKAGE_SCRIPTS=${PACKAGE_SCRIPTS_DIR}" \
        "DOTFILES_PACKAGE_INSTALL_DIR=${INSTALL_DIR}" \
        "DOTFILES_PACKAGE_IGNORE_FILE=${IGNORE_FILE}" \
        "DOTFILES_PACKAGES_LOADED_ENV=" \
        "DOTFILES_PACKAGE_MUTEX=${BATS_TEST_TMPDIR}/mutex" \
        "DOTFILES_PACKAGE_ROOTS=${BATS_TEST_TMPDIR}/a" \
        "DOTFILES_NEEDS_LOGOUT=${BATS_TEST_TMPDIR}/logout" \
        "${BIN_DIR}/dotfiles-package-install" 'foo' 'bar'

    assert_success
    refute_line --partial 'Usage:'
    assert [ ! -e "${BATS_TEST_TMPDIR}/mutex" ]
    assert [ -f "${BATS_TEST_TMPDIR}/logout" ]
    assert_line --partial 'Log out and log in again'

    assert_line "TEST_FOO_ROOT=${BATS_TEST_TMPDIR}/a"
    assert_line "TEST_FOO_NAME=foo"
    assert_line "TEST_FOO_SOURCE_DIR=${BATS_TEST_TMPDIR}/a/foo"
    assert_line "TEST_FOO_INSTALL_DIR=${INSTALL_DIR}/foo"
    assert_line "TEST_FOO_CWD=${INSTALL_DIR}/foo"
    assert_line 'FOO ENV'
    assert_line 'Installed package foo'
    assert [ -d "${BATS_TEST_TMPDIR}/install/foo" ]
    assert [ -f "${BATS_TEST_TMPDIR}/install/foo.installed" ]

    assert_line 'BAR CAN INSTALL'
    assert_line "TEST_BAR_ROOT=${BATS_TEST_TMPDIR}/a"
    assert_line "TEST_BAR_NAME=bar"
    assert_line "TEST_BAR_SOURCE_DIR=${BATS_TEST_TMPDIR}/a/bar"
    assert_line "TEST_BAR_INSTALL_DIR=${INSTALL_DIR}/bar"
    assert_line "TEST_BAR_CWD=${INSTALL_DIR}/bar"
    assert_line 'Installed package bar'
    assert [ -d "${BATS_TEST_TMPDIR}/install/bar" ]
    assert [ -f "${BATS_TEST_TMPDIR}/install/bar.installed" ]
}

@test 'dotfiles-package-install reinstall' {
    local INSTALL_DIR="${BATS_TEST_TMPDIR}/install"
    mkdir -p "${INSTALL_DIR}"
    local IGNORE_FILE="${BATS_TEST_TMPDIR}/ignore"

    mkdir -p "${BATS_TEST_TMPDIR}/a/foo"
    printf "echo 'FOO ENV'\n" >"${BATS_TEST_TMPDIR}/a/foo/env.sh"
    cat >"${BATS_TEST_TMPDIR}/a/foo/install" <<EOF
#!/bin/bash
echo "TEST_FOO_ROOT=\${PACKAGE_ROOT}"
echo "TEST_FOO_NAME=\${PACKAGE_NAME}"
echo "TEST_FOO_SOURCE_DIR=\${PACKAGE_SOURCE_DIR}"
echo "TEST_FOO_INSTALL_DIR=\${PACKAGE_INSTALL_DIR}"
echo "TEST_FOO_CWD=\$(pwd)"
EOF
    chmod u+x "${BATS_TEST_TMPDIR}/a/foo/install"
    mkdir -p "${INSTALL_DIR}/foo"
    touch "${INSTALL_DIR}/foo.installed"

    mkdir -p "${BATS_TEST_TMPDIR}/a/bar"
    cat >"${BATS_TEST_TMPDIR}/a/bar/install" <<EOF
#!/bin/bash
echo "TEST_BAR_ROOT=\${PACKAGE_ROOT}"
echo "TEST_BAR_NAME=\${PACKAGE_NAME}"
echo "TEST_BAR_SOURCE_DIR=\${PACKAGE_SOURCE_DIR}"
echo "TEST_BAR_INSTALL_DIR=\${PACKAGE_INSTALL_DIR}"
echo "TEST_BAR_CWD=\$(pwd)"
EOF
    chmod u+x "${BATS_TEST_TMPDIR}/a/bar/install"
    printf "#!/bin/bash\necho 'BAR CAN INSTALL'\n" >"${BATS_TEST_TMPDIR}/a/bar/can-install"
    chmod u+x "${BATS_TEST_TMPDIR}/a/bar/can-install"
    mkdir -p "${INSTALL_DIR}/bar"
    touch "${INSTALL_DIR}/bar.installed"

    run_script \
        "PATH=${BIN_DIR}:${PATH}" \
        "DOTFILES_PACKAGE_SCRIPTS=${PACKAGE_SCRIPTS_DIR}" \
        "DOTFILES_PACKAGE_INSTALL_DIR=${INSTALL_DIR}" \
        "DOTFILES_PACKAGE_IGNORE_FILE=${IGNORE_FILE}" \
        "DOTFILES_PACKAGES_LOADED_ENV=" \
        "DOTFILES_PACKAGE_MUTEX=${BATS_TEST_TMPDIR}/mutex" \
        "DOTFILES_PACKAGE_ROOTS=${BATS_TEST_TMPDIR}/a" \
        "DOTFILES_NEEDS_LOGOUT=${BATS_TEST_TMPDIR}/logout" \
        "${BIN_DIR}/dotfiles-package-install" 'foo' 'bar'

    assert_success
    refute_line --partial 'Usage:'
    assert [ ! -e "${BATS_TEST_TMPDIR}/mutex" ]
    assert [ -f "${BATS_TEST_TMPDIR}/logout" ]
    assert_line --partial 'Log out and log in again'

    assert_line "TEST_FOO_ROOT=${BATS_TEST_TMPDIR}/a"
    assert_line "TEST_FOO_NAME=foo"
    assert_line "TEST_FOO_SOURCE_DIR=${BATS_TEST_TMPDIR}/a/foo"
    assert_line "TEST_FOO_INSTALL_DIR=${INSTALL_DIR}/foo"
    assert_line "TEST_FOO_CWD=${INSTALL_DIR}/foo"
    assert_line 'FOO ENV'
    assert_line 'Reinstalled package foo'
    assert [ -d "${BATS_TEST_TMPDIR}/install/foo" ]
    assert [ -f "${BATS_TEST_TMPDIR}/install/foo.installed" ]

    assert_line 'BAR CAN INSTALL'
    assert_line "TEST_BAR_ROOT=${BATS_TEST_TMPDIR}/a"
    assert_line "TEST_BAR_NAME=bar"
    assert_line "TEST_BAR_SOURCE_DIR=${BATS_TEST_TMPDIR}/a/bar"
    assert_line "TEST_BAR_INSTALL_DIR=${INSTALL_DIR}/bar"
    assert_line "TEST_BAR_CWD=${INSTALL_DIR}/bar"
    assert_line 'Reinstalled package bar'
    assert [ -d "${BATS_TEST_TMPDIR}/install/bar" ]
    assert [ -f "${BATS_TEST_TMPDIR}/install/bar.installed" ]
}

@test 'dotfiles-package-install nonexistent' {
    local INSTALL_DIR="${BATS_TEST_TMPDIR}/install"
    mkdir -p "${INSTALL_DIR}"
    local IGNORE_FILE="${BATS_TEST_TMPDIR}/ignore"

    mkdir -p "${BATS_TEST_TMPDIR}/a"

    run_script \
        "PATH=${BIN_DIR}:${PATH}" \
        "DOTFILES_PACKAGE_SCRIPTS=${PACKAGE_SCRIPTS_DIR}" \
        "DOTFILES_PACKAGE_INSTALL_DIR=${INSTALL_DIR}" \
        "DOTFILES_PACKAGE_IGNORE_FILE=${IGNORE_FILE}" \
        "DOTFILES_PACKAGES_LOADED_ENV=" \
        "DOTFILES_PACKAGE_MUTEX=${BATS_TEST_TMPDIR}/mutex" \
        "DOTFILES_PACKAGE_ROOTS=${BATS_TEST_TMPDIR}/a" \
        "DOTFILES_NEEDS_LOGOUT=${BATS_TEST_TMPDIR}/logout" \
        "${BIN_DIR}/dotfiles-package-install" 'foo'

    assert_failure
    refute_output --partial 'Usage:'
    assert [ ! -e "${BATS_TEST_TMPDIR}/mutex" ]
    assert [ ! -e "${BATS_TEST_TMPDIR}/logout" ]
    refute_output --partial 'Log out and log in again'
}

@test 'dotfiles-package-install ignored' {
    local INSTALL_DIR="${BATS_TEST_TMPDIR}/install"
    mkdir -p "${INSTALL_DIR}"
    local IGNORE_FILE="${BATS_TEST_TMPDIR}/ignore"

    mkdir -p "${BATS_TEST_TMPDIR}/a/foo"
    printf "echo 'FOO ENV'\n" >"${BATS_TEST_TMPDIR}/a/foo/env.sh"
    printf "#!/bin/bash\necho 'FOO INSTALLED'\n" >"${BATS_TEST_TMPDIR}/a/foo/install"
    chmod u+x "${BATS_TEST_TMPDIR}/a/foo/install"
    echo 'foo' >"${IGNORE_FILE}"

    run_script \
        "PATH=${BIN_DIR}:${PATH}" \
        "DOTFILES_PACKAGE_SCRIPTS=${PACKAGE_SCRIPTS_DIR}" \
        "DOTFILES_PACKAGE_INSTALL_DIR=${INSTALL_DIR}" \
        "DOTFILES_PACKAGE_IGNORE_FILE=${IGNORE_FILE}" \
        "DOTFILES_PACKAGES_LOADED_ENV=" \
        "DOTFILES_PACKAGE_MUTEX=${BATS_TEST_TMPDIR}/mutex" \
        "DOTFILES_PACKAGE_ROOTS=${BATS_TEST_TMPDIR}/a" \
        "DOTFILES_NEEDS_LOGOUT=${BATS_TEST_TMPDIR}/logout" \
        "${BIN_DIR}/dotfiles-package-install" 'foo'

    assert_failure
    refute_line --partial 'Usage:'
    assert [ ! -e "${BATS_TEST_TMPDIR}/mutex" ]
    assert [ ! -e "${BATS_TEST_TMPDIR}/logout" ]
    refute_line --partial 'Log out and log in again'

    refute_line 'FOO INSTALLED'
    refute_line 'FOO ENV'
    assert [ ! -e "${BATS_TEST_TMPDIR}/install/foo" ]
    assert [ ! -e "${BATS_TEST_TMPDIR}/install/foo.installed" ]
}

@test 'dotfiles-package-install no installer' {
    local INSTALL_DIR="${BATS_TEST_TMPDIR}/install"
    mkdir -p "${INSTALL_DIR}"
    local IGNORE_FILE="${BATS_TEST_TMPDIR}/ignore"

    mkdir -p "${BATS_TEST_TMPDIR}/a/foo"
    printf "echo 'FOO ENV'\n" >"${BATS_TEST_TMPDIR}/a/foo/env.sh"

    run_script \
        "PATH=${BIN_DIR}:${PATH}" \
        "DOTFILES_PACKAGE_SCRIPTS=${PACKAGE_SCRIPTS_DIR}" \
        "DOTFILES_PACKAGE_INSTALL_DIR=${INSTALL_DIR}" \
        "DOTFILES_PACKAGE_IGNORE_FILE=${IGNORE_FILE}" \
        "DOTFILES_PACKAGES_LOADED_ENV=" \
        "DOTFILES_PACKAGE_MUTEX=${BATS_TEST_TMPDIR}/mutex" \
        "DOTFILES_PACKAGE_ROOTS=${BATS_TEST_TMPDIR}/a" \
        "DOTFILES_NEEDS_LOGOUT=${BATS_TEST_TMPDIR}/logout" \
        "${BIN_DIR}/dotfiles-package-install" 'foo'

    assert_failure
    refute_line --partial 'Usage:'
    assert [ ! -e "${BATS_TEST_TMPDIR}/mutex" ]
    assert [ ! -e "${BATS_TEST_TMPDIR}/logout" ]
    refute_line --partial 'Log out and log in again'

    refute_line 'FOO ENV'
    assert [ ! -e "${BATS_TEST_TMPDIR}/install/foo" ]
    assert [ ! -e "${BATS_TEST_TMPDIR}/install/foo.installed" ]
}

@test 'dotfiles-package-install requirements unmet' {
    local INSTALL_DIR="${BATS_TEST_TMPDIR}/install"
    mkdir -p "${INSTALL_DIR}"
    local IGNORE_FILE="${BATS_TEST_TMPDIR}/ignore"

    mkdir -p "${BATS_TEST_TMPDIR}/a/foo"
    printf "echo 'FOO ENV'\n" >"${BATS_TEST_TMPDIR}/a/foo/env.sh"
    printf "#!/bin/bash\necho 'FOO INSTALLED'\n" >"${BATS_TEST_TMPDIR}/a/foo/install"
    chmod u+x "${BATS_TEST_TMPDIR}/a/foo/install"
    printf "#!/bin/bash\necho 'FOO CAN INSTALL'\nfalse\n" >"${BATS_TEST_TMPDIR}/a/foo/can-install"
    chmod u+x "${BATS_TEST_TMPDIR}/a/foo/can-install"

    run_script \
        "PATH=${BIN_DIR}:${PATH}" \
        "DOTFILES_PACKAGE_SCRIPTS=${PACKAGE_SCRIPTS_DIR}" \
        "DOTFILES_PACKAGE_INSTALL_DIR=${INSTALL_DIR}" \
        "DOTFILES_PACKAGE_IGNORE_FILE=${IGNORE_FILE}" \
        "DOTFILES_PACKAGES_LOADED_ENV=" \
        "DOTFILES_PACKAGE_MUTEX=${BATS_TEST_TMPDIR}/mutex" \
        "DOTFILES_PACKAGE_ROOTS=${BATS_TEST_TMPDIR}/a" \
        "DOTFILES_NEEDS_LOGOUT=${BATS_TEST_TMPDIR}/logout" \
        "${BIN_DIR}/dotfiles-package-install" 'foo'

    assert_failure
    refute_line --partial 'Usage:'
    assert [ ! -e "${BATS_TEST_TMPDIR}/mutex" ]
    assert [ ! -e "${BATS_TEST_TMPDIR}/logout" ]
    refute_line --partial 'Log out and log in again'

    assert_line 'FOO CAN INSTALL'
    refute_line 'FOO INSTALLED'
    refute_line 'FOO ENV'
    assert [ ! -e "${BATS_TEST_TMPDIR}/install/foo" ]
    assert [ ! -e "${BATS_TEST_TMPDIR}/install/foo.installed" ]
}

@test 'dotfiles-package-update-all needs lock' {
    local INSTALL_DIR="${BATS_TEST_TMPDIR}/install"
    mkdir -p "${INSTALL_DIR}"
    local IGNORE_FILE="${BATS_TEST_TMPDIR}/ignore"
    local LAST_UPDATE_FILE="${BATS_TEST_TMPDIR}/last-update"
    local ALREADY_UPDATED_FILE="${BATS_TEST_TMPDIR}/already-updated"
    local PRIVATE_DIR="${BATS_TEST_TMPDIR}/private"
    mkdir -p "${BATS_TEST_TMPDIR}/mutex"

    mkdir -p "${BATS_TEST_TMPDIR}/a/foo"
    printf "#!/bin/bash\ntrue\n" >"${BATS_TEST_TMPDIR}/a/foo/install"
    chmod u+x "${BATS_TEST_TMPDIR}/a/foo/install"
    mkdir -p "${BATS_TEST_TMPDIR}/install/foo"
    touch "${BATS_TEST_TMPDIR}/install/foo.installed"

    run_script \
        "PATH=${BIN_DIR}:${PATH}" \
        "DOTFILES_PACKAGE_MUTEX=${BATS_TEST_TMPDIR}/mutex" \
        "${BIN_DIR}/dotfiles-package-update-all"

    assert_failure
    assert [ -d "${BATS_TEST_TMPDIR}/mutex" ]
    assert [ ! -e "${BATS_TEST_TMPDIR}/logout" ]
    assert [ ! -e "${LAST_UPDATE_FILE}" ]
    assert [ ! -e "${ALREADY_UPDATED_FILE}" ]

    assert_line --partial 'Another script is installing'
    refute_line 'Reinstalled package foo'
}

@test 'dotfiles-package-update-all clean' {
    local INSTALL_DIR="${BATS_TEST_TMPDIR}/install"
    mkdir -p "${INSTALL_DIR}"
    local IGNORE_FILE="${BATS_TEST_TMPDIR}/ignore"
    local LAST_UPDATE_FILE="${BATS_TEST_TMPDIR}/last-update"
    local ALREADY_UPDATED_FILE="${BATS_TEST_TMPDIR}/already-updated"
    local PRIVATE_DIR="${BATS_TEST_TMPDIR}/private"

    mkdir -p "${BATS_TEST_TMPDIR}/repo"
    touch "${BATS_TEST_TMPDIR}/repo/a"
    (cd "${BATS_TEST_TMPDIR}/repo" && git_init && git add a && git commit -m 'Foo')

    mkdir -p "${BATS_TEST_TMPDIR}/a/foo"
    printf "#!/bin/bash\ntrue\n" >"${BATS_TEST_TMPDIR}/a/foo/install"
    chmod u+x "${BATS_TEST_TMPDIR}/a/foo/install"
    mkdir -p "${BATS_TEST_TMPDIR}/install/foo"
    touch "${BATS_TEST_TMPDIR}/install/foo.installed"

    mkdir -p "${BATS_TEST_TMPDIR}/a/bar"
    printf "#!/bin/bash\ntrue\n" >"${BATS_TEST_TMPDIR}/a/bar/install"
    chmod u+x "${BATS_TEST_TMPDIR}/a/bar/install"
    mkdir -p "${BATS_TEST_TMPDIR}/install/bar"
    touch "${BATS_TEST_TMPDIR}/install/bar.installed"
    echo 'bar' >"${BATS_TEST_TMPDIR}/ignore"

    mkdir -p "${BATS_TEST_TMPDIR}/a/baz"
    printf "#!/bin/bash\ntrue\n" >"${BATS_TEST_TMPDIR}/a/baz/install"
    chmod u+x "${BATS_TEST_TMPDIR}/a/baz/install"
    printf "#!/bin/bash\ntrue\n" >"${BATS_TEST_TMPDIR}/a/baz/can-install"
    chmod u+x "${BATS_TEST_TMPDIR}/a/baz/can-install"
    mkdir -p "${BATS_TEST_TMPDIR}/install/baz"
    touch "${BATS_TEST_TMPDIR}/install/baz.installed"

    mkdir -p "${BATS_TEST_TMPDIR}/a/quux"
    printf "#!/bin/bash\ntrue\n" >"${BATS_TEST_TMPDIR}/a/quux/install"
    chmod u+x "${BATS_TEST_TMPDIR}/a/quux/install"
    printf "#!/bin/bash\nfalse\n" >"${BATS_TEST_TMPDIR}/a/quux/can-install"
    chmod u+x "${BATS_TEST_TMPDIR}/a/quux/can-install"
    mkdir -p "${BATS_TEST_TMPDIR}/install/quux"
    touch "${BATS_TEST_TMPDIR}/install/quux.installed"

    mkdir -p "${BATS_TEST_TMPDIR}/a/blah"

    run_script \
        "PATH=${BIN_DIR}:${PATH}" \
        "DOTFILES_PACKAGE_SCRIPTS=${PACKAGE_SCRIPTS_DIR}" \
        "DOTFILES_PACKAGE_INSTALL_DIR=${INSTALL_DIR}" \
        "DOTFILES_PACKAGE_IGNORE_FILE=${IGNORE_FILE}" \
        "DOTFILES_PACKAGES_LOADED_ENV=" \
        "DOTFILES_PACKAGE_MUTEX=${BATS_TEST_TMPDIR}/mutex" \
        "DOTFILES_PACKAGE_ROOTS=${BATS_TEST_TMPDIR}/a" \
        "DOTFILES_NEEDS_LOGOUT=${BATS_TEST_TMPDIR}/logout" \
        "DOTFILES_PACKAGE_LAST_UPDATE_FILE=${LAST_UPDATE_FILE}" \
        "DOTFILES_PACKAGE_ALREADY_UPDATED_FILE=${ALREADY_UPDATED_FILE}" \
        "DOTFILES_PRIVATE_DIR=${PRIVATE_DIR}" \
        "DOTFILES_PRIVATE_REPO=${BATS_TEST_TMPDIR}/repo" \
        "DOTFILES_PRIVATE_BRANCH=" \
        "${BIN_DIR}/dotfiles-package-update-all"

    assert_success
    refute_line --partial 'Usage:'
    assert [ ! -e "${BATS_TEST_TMPDIR}/mutex" ]
    assert [ -f "${BATS_TEST_TMPDIR}/logout" ]
    assert [ -f "${LAST_UPDATE_FILE}" ]
    assert [ ! -e "${ALREADY_UPDATED_FILE}" ]
    assert [ -d "${BATS_TEST_TMPDIR}/private" ]
    assert [ -f "${BATS_TEST_TMPDIR}/private/a" ]

    assert_line 'Cloned or updated private repo'
    assert_line 'Reinstalled package foo'
    refute_line 'Reinstalled package bar'
    assert_line 'Reinstalled package baz'
    refute_line 'Reinstalled package quux'
    refute_line 'Installed package blah'
    refute_line 'Reinstalled package blah'
    assert_num_matching_lines 'Log out and log in again' '1'
}

@test 'dotfiles-package-update-all continue' {
    local INSTALL_DIR="${BATS_TEST_TMPDIR}/install"
    mkdir -p "${INSTALL_DIR}"
    local IGNORE_FILE="${BATS_TEST_TMPDIR}/ignore"
    local LAST_UPDATE_FILE="${BATS_TEST_TMPDIR}/last-update"
    local ALREADY_UPDATED_FILE="${BATS_TEST_TMPDIR}/already-updated"
    local PRIVATE_DIR="${BATS_TEST_TMPDIR}/private"

    mkdir -p "${BATS_TEST_TMPDIR}/repo"
    touch "${BATS_TEST_TMPDIR}/repo/a"
    (cd "${BATS_TEST_TMPDIR}/repo" && git_init && git add a && git commit -m 'Foo')
    git clone "${BATS_TEST_TMPDIR}/repo" "${BATS_TEST_TMPDIR}/private"

    mkdir -p "${BATS_TEST_TMPDIR}/a/foo"
    printf "#!/bin/bash\ntrue\n" >"${BATS_TEST_TMPDIR}/a/foo/install"
    chmod u+x "${BATS_TEST_TMPDIR}/a/foo/install"
    mkdir -p "${BATS_TEST_TMPDIR}/install/foo"
    touch "${BATS_TEST_TMPDIR}/install/foo.installed"
    echo 'foo' >"${ALREADY_UPDATED_FILE}"

    mkdir -p "${BATS_TEST_TMPDIR}/a/bar"
    printf "#!/bin/bash\ntrue\n" >"${BATS_TEST_TMPDIR}/a/bar/install"
    chmod u+x "${BATS_TEST_TMPDIR}/a/bar/install"
    mkdir -p "${BATS_TEST_TMPDIR}/install/bar"
    touch "${BATS_TEST_TMPDIR}/install/bar.installed"

    run_script \
        "PATH=${BIN_DIR}:${PATH}" \
        "DOTFILES_PACKAGE_SCRIPTS=${PACKAGE_SCRIPTS_DIR}" \
        "DOTFILES_PACKAGE_INSTALL_DIR=${INSTALL_DIR}" \
        "DOTFILES_PACKAGE_IGNORE_FILE=${IGNORE_FILE}" \
        "DOTFILES_PACKAGES_LOADED_ENV=" \
        "DOTFILES_PACKAGE_MUTEX=${BATS_TEST_TMPDIR}/mutex" \
        "DOTFILES_PACKAGE_ROOTS=${BATS_TEST_TMPDIR}/a" \
        "DOTFILES_NEEDS_LOGOUT=${BATS_TEST_TMPDIR}/logout" \
        "DOTFILES_PACKAGE_LAST_UPDATE_FILE=${LAST_UPDATE_FILE}" \
        "DOTFILES_PACKAGE_ALREADY_UPDATED_FILE=${ALREADY_UPDATED_FILE}" \
        "DOTFILES_PRIVATE_DIR=${PRIVATE_DIR}" \
        "DOTFILES_PRIVATE_REPO=${BATS_TEST_TMPDIR}/repo" \
        "DOTFILES_PRIVATE_BRANCH=" \
        "${BIN_DIR}/dotfiles-package-update-all"

    assert_success
    refute_line --partial 'Usage:'
    assert [ ! -e "${BATS_TEST_TMPDIR}/mutex" ]
    assert [ -f "${BATS_TEST_TMPDIR}/logout" ]
    assert [ -f "${LAST_UPDATE_FILE}" ]
    assert [ ! -e "${ALREADY_UPDATED_FILE}" ]
    assert [ -d "${BATS_TEST_TMPDIR}/private" ]
    assert [ -f "${BATS_TEST_TMPDIR}/private/a" ]

    assert_line 'Cloned or updated private repo'
    refute_line 'Reinstalled package foo'
    assert_line 'Reinstalled package bar'
    assert_num_matching_lines 'Log out and log in again' '1'
}

@test 'dotfiles-package-update-all failure' {
    local INSTALL_DIR="${BATS_TEST_TMPDIR}/install"
    mkdir -p "${INSTALL_DIR}"
    local IGNORE_FILE="${BATS_TEST_TMPDIR}/ignore"
    local LAST_UPDATE_FILE="${BATS_TEST_TMPDIR}/last-update"
    local ALREADY_UPDATED_FILE="${BATS_TEST_TMPDIR}/already-updated"
    local PRIVATE_DIR="${BATS_TEST_TMPDIR}/private"

    mkdir -p "${BATS_TEST_TMPDIR}/a/foo"
    printf "#!/bin/bash\ntrue\n" >"${BATS_TEST_TMPDIR}/a/foo/install"
    chmod u+x "${BATS_TEST_TMPDIR}/a/foo/install"
    mkdir -p "${BATS_TEST_TMPDIR}/install/foo"
    touch "${BATS_TEST_TMPDIR}/install/foo.installed"

    mkdir -p "${BATS_TEST_TMPDIR}/a/bar"
    printf "#!/bin/bash\nfalse\n" >"${BATS_TEST_TMPDIR}/a/bar/install"
    chmod u+x "${BATS_TEST_TMPDIR}/a/bar/install"
    mkdir -p "${BATS_TEST_TMPDIR}/install/bar"
    touch "${BATS_TEST_TMPDIR}/install/bar.installed"

    run_script \
        "PATH=${BIN_DIR}:${PATH}" \
        "DOTFILES_PACKAGE_SCRIPTS=${PACKAGE_SCRIPTS_DIR}" \
        "DOTFILES_PACKAGE_INSTALL_DIR=${INSTALL_DIR}" \
        "DOTFILES_PACKAGE_IGNORE_FILE=${IGNORE_FILE}" \
        "DOTFILES_PACKAGES_LOADED_ENV=" \
        "DOTFILES_PACKAGE_MUTEX=${BATS_TEST_TMPDIR}/mutex" \
        "DOTFILES_PACKAGE_ROOTS=${BATS_TEST_TMPDIR}/a" \
        "DOTFILES_NEEDS_LOGOUT=${BATS_TEST_TMPDIR}/logout" \
        "DOTFILES_PACKAGE_LAST_UPDATE_FILE=${LAST_UPDATE_FILE}" \
        "DOTFILES_PACKAGE_ALREADY_UPDATED_FILE=${ALREADY_UPDATED_FILE}" \
        "DOTFILES_PRIVATE_DIR=${PRIVATE_DIR}" \
        "DOTFILES_PRIVATE_REPO=" \
        "${BIN_DIR}/dotfiles-package-update-all"

    assert_failure
    refute_line --partial 'Usage:'
    assert [ ! -e "${BATS_TEST_TMPDIR}/mutex" ]
    assert [ ! -e "${BATS_TEST_TMPDIR}/logout" ]
    assert [ ! -e "${LAST_UPDATE_FILE}" ]
    assert [ -f "${ALREADY_UPDATED_FILE}" ]
    assert [ ! -e "${PRIVATE_DIR}" ]

    assert_line --partial 'Updating packages failed.'
    refute_line 'Reinstalled package foo'
    refute_line 'Reinstalled package bar'
    refute_line --partial 'Log out and log in again'
}
