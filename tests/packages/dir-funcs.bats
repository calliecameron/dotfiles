# shellcheck shell=bats

setup() {
    THIS_DIR="$(cd "$(dirname "${BATS_TEST_FILENAME}")" && pwd)"
    BIN_DIR="$(readlink -f "${THIS_DIR}/../../packages/dir-funcs/bin")"

    TMP_DIR="$(mktemp -d)"
    TEST_SAVED_PATHS="${TMP_DIR}/saved"
    TEST_FOLLOW="${TMP_DIR}/follow"

    bats_load_library 'bats-support'
    bats_load_library 'bats-assert'

    cd "${TMP_DIR}" || exit 1
}

teardown() {
    rm -rf "${TMP_DIR}"
}

run_script() {
    run env -i -C "${TMP_DIR}" HOME="${TMP_DIR}" "PATH=${BIN_DIR}:${PATH}" "${@}"
}

@test 'dotfiles-save-path default slot nonexisting' {
    run_script "DOTFILES_SAVED_PATHS=${TEST_SAVED_PATHS}" "${BIN_DIR}/dotfiles-save-path"
    assert_success
    refute_output --partial 'Usage:'
    assert [ -f "${TEST_SAVED_PATHS}/0" ]
    assert [ "$(cat "${TEST_SAVED_PATHS}/0")" = "${TMP_DIR}" ]
}

@test 'dotfiles-save-path default slot existing' {
    mkdir -p "${TEST_SAVED_PATHS}"
    echo 'foo' >"${TEST_SAVED_PATHS}/0"
    run_script "DOTFILES_SAVED_PATHS=${TEST_SAVED_PATHS}" "${BIN_DIR}/dotfiles-save-path"
    assert_success
    refute_output --partial 'Usage:'
    assert [ -f "${TEST_SAVED_PATHS}/0" ]
    assert [ "$(cat "${TEST_SAVED_PATHS}/0")" = "${TMP_DIR}" ]
}

@test 'dotfiles-save-path other slot nonexisting' {
    run_script "DOTFILES_SAVED_PATHS=${TEST_SAVED_PATHS}" "${BIN_DIR}/dotfiles-save-path" 'foo'
    assert_success
    refute_output --partial 'Usage:'
    assert [ -f "${TEST_SAVED_PATHS}/foo" ]
    assert [ "$(cat "${TEST_SAVED_PATHS}/foo")" = "${TMP_DIR}" ]
}

@test 'dotfiles-save-path other slot existing' {
    mkdir -p "${TEST_SAVED_PATHS}"
    echo 'foo' >"${TEST_SAVED_PATHS}/foo"
    run_script "DOTFILES_SAVED_PATHS=${TEST_SAVED_PATHS}" "${BIN_DIR}/dotfiles-save-path" 'foo'
    assert_success
    refute_output --partial 'Usage:'
    assert [ -f "${TEST_SAVED_PATHS}/foo" ]
    assert [ "$(cat "${TEST_SAVED_PATHS}/foo")" = "${TMP_DIR}" ]
}

@test 'dotfiles-save-path bad slot' {
    run_script "DOTFILES_SAVED_PATHS=${TEST_SAVED_PATHS}" "${BIN_DIR}/dotfiles-save-path" 'foo/bar'
    assert_failure
    assert_line --partial 'Usage:'
    assert [ ! -e "${TEST_SAVED_PATHS}" ]
}

@test 'dotfiles-saved-path default slot nonexisting' {
    run_script "DOTFILES_SAVED_PATHS=${TEST_SAVED_PATHS}" "${BIN_DIR}/dotfiles-saved-path"
    assert_failure
    refute_line --partial 'Usage:'
    assert_line 'No path saved.'
    assert [ ! -e "${TEST_SAVED_PATHS}" ]
}

@test 'dotfiles-saved-path default slot existing' {
    mkdir -p "${TEST_SAVED_PATHS}"
    echo 'foo' >"${TEST_SAVED_PATHS}/0"
    run_script "DOTFILES_SAVED_PATHS=${TEST_SAVED_PATHS}" "${BIN_DIR}/dotfiles-saved-path"
    assert_success
    refute_line --partial 'Usage:'
    assert_line 'foo'
    assert [ -f "${TEST_SAVED_PATHS}/0" ]
    assert [ "$(cat "${TEST_SAVED_PATHS}/0")" = 'foo' ]
}

@test 'dotfiles-saved-path other slot nonexisting' {
    run_script "DOTFILES_SAVED_PATHS=${TEST_SAVED_PATHS}" "${BIN_DIR}/dotfiles-saved-path" 'foo'
    assert_failure
    refute_line --partial 'Usage:'
    assert_line 'No path saved.'
    assert [ ! -e "${TEST_SAVED_PATHS}" ]
}

@test 'dotfiles-saved-path other slot existing' {
    mkdir -p "${TEST_SAVED_PATHS}"
    echo 'bar' >"${TEST_SAVED_PATHS}/foo"
    run_script "DOTFILES_SAVED_PATHS=${TEST_SAVED_PATHS}" "${BIN_DIR}/dotfiles-saved-path" 'foo'
    assert_success
    refute_line --partial 'Usage:'
    assert_line 'bar'
    assert [ -f "${TEST_SAVED_PATHS}/foo" ]
    assert [ "$(cat "${TEST_SAVED_PATHS}/foo")" = 'bar' ]
}

@test 'dotfiles-saved-path bad slot' {
    run_script "DOTFILES_SAVED_PATHS=${TEST_SAVED_PATHS}" "${BIN_DIR}/dotfiles-saved-path" 'foo/bar'
    assert_failure
    assert_line --partial 'Usage:'
    assert [ ! -e "${TEST_SAVED_PATHS}" ]
}

@test 'dotfiles-clear-saved-paths nonexisting' {
    run_script "DOTFILES_SAVED_PATHS=${TEST_SAVED_PATHS}" "${BIN_DIR}/dotfiles-clear-saved-paths"
    assert_success
    refute_output
    assert [ ! -e "${TEST_SAVED_PATHS}" ]
}

@test 'dotfiles-clear-saved-paths existing' {
    mkdir -p "${TEST_SAVED_PATHS}"
    touch "${TEST_SAVED_PATHS}/foo"
    run_script "DOTFILES_SAVED_PATHS=${TEST_SAVED_PATHS}" "${BIN_DIR}/dotfiles-clear-saved-paths"
    assert_success
    refute_output
    assert [ ! -e "${TEST_SAVED_PATHS}" ]
}

@test 'dotfiles-copy-to-saved-path no args' {
    run_script "DOTFILES_SAVED_PATHS=${TEST_SAVED_PATHS}" "${BIN_DIR}/dotfiles-copy-to-saved-path"
    assert_failure
    assert_line --partial 'Usage:'
    assert [ ! -e "${TEST_SAVED_PATHS}" ]
}

@test 'dotfiles-copy-to-saved-path one arg' {
    run_script "DOTFILES_SAVED_PATHS=${TEST_SAVED_PATHS}" "${BIN_DIR}/dotfiles-copy-to-saved-path" 'foo'
    assert_failure
    assert_line --partial 'Usage:'
    assert [ ! -e "${TEST_SAVED_PATHS}" ]
}

@test 'dotfiles-copy-to-saved-path nonexisting' {
    echo 'foo' >"${TMP_DIR}/a"
    echo 'bar' >"${TMP_DIR}/b"
    mkdir -p "${TMP_DIR}/target"
    run_script "DOTFILES_SAVED_PATHS=${TEST_SAVED_PATHS}" "${BIN_DIR}/dotfiles-copy-to-saved-path" '0' "${TMP_DIR}/a" "${TMP_DIR}/b"
    assert_failure
    refute_line --partial 'Usage:'
    assert_line 'No saved path.'
    assert [ ! -e "${TEST_SAVED_PATHS}" ]
    assert [ -f "${TMP_DIR}/a" ]
    assert [ "$(cat "${TMP_DIR}/a")" = 'foo' ]
    assert [ -f "${TMP_DIR}/b" ]
    assert [ "$(cat "${TMP_DIR}/b")" = 'bar' ]
    assert [ -d "${TMP_DIR}/target" ]
    assert [ ! -e "${TMP_DIR}/target/a" ]
    assert [ ! -e "${TMP_DIR}/target/b" ]
}

@test 'dotfiles-copy-to-saved-path existing' {
    echo 'foo' >"${TMP_DIR}/a"
    echo 'bar' >"${TMP_DIR}/b"
    mkdir -p "${TMP_DIR}/target"
    mkdir -p "${TEST_SAVED_PATHS}"
    echo "${TMP_DIR}/target" >"${TEST_SAVED_PATHS}/0"
    run_script "DOTFILES_SAVED_PATHS=${TEST_SAVED_PATHS}" "${BIN_DIR}/dotfiles-copy-to-saved-path" '0' "${TMP_DIR}/a" "${TMP_DIR}/b"
    assert_success
    refute_output --partial 'Usage:'
    refute_output --partial 'No saved path.'
    assert [ -d "${TEST_SAVED_PATHS}" ]
    assert [ -f "${TEST_SAVED_PATHS}/0" ]
    assert [ "$(cat "${TEST_SAVED_PATHS}/0")" = "${TMP_DIR}/target" ]
    assert [ -f "${TMP_DIR}/a" ]
    assert [ "$(cat "${TMP_DIR}/a")" = 'foo' ]
    assert [ -f "${TMP_DIR}/b" ]
    assert [ "$(cat "${TMP_DIR}/b")" = 'bar' ]
    assert [ -d "${TMP_DIR}/target" ]
    assert [ -f "${TMP_DIR}/target/a" ]
    assert [ "$(cat "${TMP_DIR}/target/a")" = 'foo' ]
    assert [ -f "${TMP_DIR}/target/b" ]
    assert [ "$(cat "${TMP_DIR}/target/b")" = 'bar' ]
}

@test 'dotfiles-copy-to-saved-path bad slot' {
    run_script "DOTFILES_SAVED_PATHS=${TEST_SAVED_PATHS}" "${BIN_DIR}/dotfiles-copy-to-saved-path" 'foo/bar' 'a'
    assert_failure
    assert_line --partial 'Usage:'
    assert [ ! -e "${TEST_SAVED_PATHS}" ]
}

@test 'dotfiles-move-to-saved-path no args' {
    run_script "DOTFILES_SAVED_PATHS=${TEST_SAVED_PATHS}" "${BIN_DIR}/dotfiles-move-to-saved-path"
    assert_failure
    assert_line --partial 'Usage:'
    assert [ ! -e "${TEST_SAVED_PATHS}" ]
}

@test 'dotfiles-move-to-saved-path one arg' {
    run_script "DOTFILES_SAVED_PATHS=${TEST_SAVED_PATHS}" "${BIN_DIR}/dotfiles-move-to-saved-path" 'foo'
    assert_failure
    assert_line --partial 'Usage:'
    assert [ ! -e "${TEST_SAVED_PATHS}" ]
}

@test 'dotfiles-move-to-saved-path nonexisting' {
    echo 'foo' >"${TMP_DIR}/a"
    echo 'bar' >"${TMP_DIR}/b"
    mkdir -p "${TMP_DIR}/target"
    run_script "DOTFILES_SAVED_PATHS=${TEST_SAVED_PATHS}" "${BIN_DIR}/dotfiles-move-to-saved-path" '0' "${TMP_DIR}/a" "${TMP_DIR}/b"
    assert_failure
    refute_line --partial 'Usage:'
    assert_line 'No saved path.'
    assert [ ! -e "${TEST_SAVED_PATHS}" ]
    assert [ -f "${TMP_DIR}/a" ]
    assert [ "$(cat "${TMP_DIR}/a")" = 'foo' ]
    assert [ -f "${TMP_DIR}/b" ]
    assert [ "$(cat "${TMP_DIR}/b")" = 'bar' ]
    assert [ -d "${TMP_DIR}/target" ]
    assert [ ! -e "${TMP_DIR}/target/a" ]
    assert [ ! -e "${TMP_DIR}/target/b" ]
}

@test 'dotfiles-move-to-saved-path existing' {
    echo 'foo' >"${TMP_DIR}/a"
    echo 'bar' >"${TMP_DIR}/b"
    mkdir -p "${TMP_DIR}/target"
    mkdir -p "${TEST_SAVED_PATHS}"
    echo "${TMP_DIR}/target" >"${TEST_SAVED_PATHS}/0"
    run_script "DOTFILES_SAVED_PATHS=${TEST_SAVED_PATHS}" "${BIN_DIR}/dotfiles-move-to-saved-path" '0' "${TMP_DIR}/a" "${TMP_DIR}/b"
    assert_success
    refute_output --partial 'Usage:'
    refute_output --partial 'No saved path.'
    assert [ -d "${TEST_SAVED_PATHS}" ]
    assert [ -f "${TEST_SAVED_PATHS}/0" ]
    assert [ "$(cat "${TEST_SAVED_PATHS}/0")" = "${TMP_DIR}/target" ]
    assert [ ! -e "${TMP_DIR}/a" ]
    assert [ ! -e "${TMP_DIR}/b" ]
    assert [ -d "${TMP_DIR}/target" ]
    assert [ -f "${TMP_DIR}/target/a" ]
    assert [ "$(cat "${TMP_DIR}/target/a")" = 'foo' ]
    assert [ -f "${TMP_DIR}/target/b" ]
    assert [ "$(cat "${TMP_DIR}/target/b")" = 'bar' ]
}

@test 'dotfiles-move-to-saved-path bad slot' {
    run_script "DOTFILES_SAVED_PATHS=${TEST_SAVED_PATHS}" "${BIN_DIR}/dotfiles-move-to-saved-path" 'foo/bar' 'a'
    assert_failure
    assert_line --partial 'Usage:'
    assert [ ! -e "${TEST_SAVED_PATHS}" ]
}

@test 'dotfiles-mv-cp-save-target no args' {
    run_script "DOTFILES_MV_CP_FOLLOW=${TEST_FOLLOW}" "${BIN_DIR}/dotfiles-mv-cp-save-target"
    assert_success
    refute_output
    assert [ ! -e "${TEST_FOLLOW}" ]
}

@test 'dotfiles-mv-cp-save-target one arg normal' {
    run_script "DOTFILES_MV_CP_FOLLOW=${TEST_FOLLOW}" "${BIN_DIR}/dotfiles-mv-cp-save-target" 'foo'
    assert_success
    refute_output
    assert [ ! -e "${TEST_FOLLOW}" ]
}

@test 'dotfiles-mv-cp-save-target one arg -t' {
    run_script "DOTFILES_MV_CP_FOLLOW=${TEST_FOLLOW}" "${BIN_DIR}/dotfiles-mv-cp-save-target" '-t'
    assert_success
    refute_output
    assert [ ! -e "${TEST_FOLLOW}" ]
}

@test 'dotfiles-mv-cp-save-target one arg -T' {
    run_script "DOTFILES_MV_CP_FOLLOW=${TEST_FOLLOW}" "${BIN_DIR}/dotfiles-mv-cp-save-target" '-T'
    assert_success
    refute_output
    assert [ ! -e "${TEST_FOLLOW}" ]
}

@test 'dotfiles-mv-cp-save-target normal' {
    run_script "DOTFILES_MV_CP_FOLLOW=${TEST_FOLLOW}" "${BIN_DIR}/dotfiles-mv-cp-save-target" 'foo' "${TMP_DIR}/bar"
    assert_success
    refute_output
    assert [ -f "${TEST_FOLLOW}" ]
    assert [ "$(cat "${TEST_FOLLOW}")" = "${TMP_DIR}" ]
}

@test 'dotfiles-mv-cp-save-target dir' {
    mkdir -p "${TMP_DIR}/bar"
    run_script "DOTFILES_MV_CP_FOLLOW=${TEST_FOLLOW}" "${BIN_DIR}/dotfiles-mv-cp-save-target" 'foo' "${TMP_DIR}/bar"
    assert_success
    refute_output
    assert [ -f "${TEST_FOLLOW}" ]
    assert [ "$(cat "${TEST_FOLLOW}")" = "${TMP_DIR}/bar" ]
}

@test 'dotfiles-mv-cp-save-target dir override' {
    mkdir -p "${TMP_DIR}/bar"
    run_script "DOTFILES_MV_CP_FOLLOW=${TEST_FOLLOW}" "${BIN_DIR}/dotfiles-mv-cp-save-target" '-T' 'foo' "${TMP_DIR}/bar"
    assert_success
    refute_output
    assert [ -f "${TEST_FOLLOW}" ]
    assert [ "$(cat "${TEST_FOLLOW}")" = "${TMP_DIR}" ]
}

@test 'dotfiles-mv-cp-save-target -t' {
    mkdir -p "${TMP_DIR}/bar"
    run_script "DOTFILES_MV_CP_FOLLOW=${TEST_FOLLOW}" "${BIN_DIR}/dotfiles-mv-cp-save-target" '-t' "${TMP_DIR}/bar" 'foo'
    assert_success
    refute_output
    assert [ -f "${TEST_FOLLOW}" ]
    assert [ "$(cat "${TEST_FOLLOW}")" = "${TMP_DIR}/bar" ]
}

@test 'dotfiles-mv normal' {
    echo 'foo' >"${TMP_DIR}/a"
    run_script "DOTFILES_MV_CP_FOLLOW=${TEST_FOLLOW}" "${BIN_DIR}/dotfiles-mv" "${TMP_DIR}/a" "${TMP_DIR}/b"
    assert_success
    refute_output
    assert [ -f "${TEST_FOLLOW}" ]
    assert [ "$(cat "${TEST_FOLLOW}")" = "${TMP_DIR}" ]
    assert [ ! -e "${TMP_DIR}/a" ]
    assert [ -f "${TMP_DIR}/b" ]
    assert [ "$(cat "${TMP_DIR}/b")" = 'foo' ]
}

@test 'dotfiles-mv dir' {
    echo 'foo' >"${TMP_DIR}/a"
    mkdir -p "${TMP_DIR}/b"
    run_script "DOTFILES_MV_CP_FOLLOW=${TEST_FOLLOW}" "${BIN_DIR}/dotfiles-mv" "${TMP_DIR}/a" "${TMP_DIR}/b"
    assert_success
    refute_output
    assert [ -f "${TEST_FOLLOW}" ]
    assert [ "$(cat "${TEST_FOLLOW}")" = "${TMP_DIR}/b" ]
    assert [ ! -e "${TMP_DIR}/a" ]
    assert [ -d "${TMP_DIR}/b" ]
    assert [ -f "${TMP_DIR}/b/a" ]
    assert [ "$(cat "${TMP_DIR}/b/a")" = 'foo' ]
}

@test 'dotfiles-mv target' {
    echo 'foo' >"${TMP_DIR}/a"
    mkdir -p "${TMP_DIR}/b"
    run_script "DOTFILES_MV_CP_FOLLOW=${TEST_FOLLOW}" "${BIN_DIR}/dotfiles-mv" '-t' "${TMP_DIR}/b" "${TMP_DIR}/a"
    assert_success
    refute_output
    assert [ -f "${TEST_FOLLOW}" ]
    assert [ "$(cat "${TEST_FOLLOW}")" = "${TMP_DIR}/b" ]
    assert [ ! -e "${TMP_DIR}/a" ]
    assert [ -d "${TMP_DIR}/b" ]
    assert [ -f "${TMP_DIR}/b/a" ]
    assert [ "$(cat "${TMP_DIR}/b/a")" = 'foo' ]
}

@test 'dotfiles-cp normal' {
    echo 'foo' >"${TMP_DIR}/a"
    run_script "DOTFILES_MV_CP_FOLLOW=${TEST_FOLLOW}" "${BIN_DIR}/dotfiles-cp" "${TMP_DIR}/a" "${TMP_DIR}/b"
    assert_success
    refute_output
    assert [ -f "${TEST_FOLLOW}" ]
    assert [ "$(cat "${TEST_FOLLOW}")" = "${TMP_DIR}" ]
    assert [ -f "${TMP_DIR}/a" ]
    assert [ "$(cat "${TMP_DIR}/a")" = 'foo' ]
    assert [ -f "${TMP_DIR}/b" ]
    assert [ "$(cat "${TMP_DIR}/b")" = 'foo' ]
}

@test 'dotfiles-cp dir' {
    echo 'foo' >"${TMP_DIR}/a"
    mkdir -p "${TMP_DIR}/b"
    run_script "DOTFILES_MV_CP_FOLLOW=${TEST_FOLLOW}" "${BIN_DIR}/dotfiles-cp" "${TMP_DIR}/a" "${TMP_DIR}/b"
    assert_success
    refute_output
    assert [ -f "${TEST_FOLLOW}" ]
    assert [ "$(cat "${TEST_FOLLOW}")" = "${TMP_DIR}/b" ]
    assert [ -f "${TMP_DIR}/a" ]
    assert [ "$(cat "${TMP_DIR}/a")" = 'foo' ]
    assert [ -d "${TMP_DIR}/b" ]
    assert [ -f "${TMP_DIR}/b/a" ]
    assert [ "$(cat "${TMP_DIR}/b/a")" = 'foo' ]
}

@test 'dotfiles-cp target' {
    echo 'foo' >"${TMP_DIR}/a"
    mkdir -p "${TMP_DIR}/b"
    run_script "DOTFILES_MV_CP_FOLLOW=${TEST_FOLLOW}" "${BIN_DIR}/dotfiles-cp" '-t' "${TMP_DIR}/b" "${TMP_DIR}/a"
    assert_success
    refute_output
    assert [ -f "${TEST_FOLLOW}" ]
    assert [ "$(cat "${TEST_FOLLOW}")" = "${TMP_DIR}/b" ]
    assert [ -f "${TMP_DIR}/a" ]
    assert [ "$(cat "${TMP_DIR}/a")" = 'foo' ]
    assert [ -d "${TMP_DIR}/b" ]
    assert [ -f "${TMP_DIR}/b/a" ]
    assert [ "$(cat "${TMP_DIR}/b/a")" = 'foo' ]
}
