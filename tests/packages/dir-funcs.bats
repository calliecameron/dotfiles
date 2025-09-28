# shellcheck shell=bats
# bats file_tags=slow

setup() {
    THIS_DIR="$(cd "$(dirname "${BATS_TEST_FILENAME}")" && pwd)"
    BIN_DIR="$(readlink -f "${THIS_DIR}/../../packages/dir-funcs/bin")"

    TEST_SAVED_PATHS="${BATS_TEST_TMPDIR}/saved"
    TEST_FOLLOW="${BATS_TEST_TMPDIR}/follow"

    bats_load_library 'bats-support'
    bats_load_library 'bats-assert'

    cd "${BATS_TEST_TMPDIR}" || exit 1
}

run_script() {
    run env -i -C "${BATS_TEST_TMPDIR}" HOME="${BATS_TEST_TMPDIR}" "PATH=${BIN_DIR}:${PATH}" "${@}"
}

@test 'dotfiles-save-path default slot nonexisting' {
    run_script "DOTFILES_SAVED_PATHS=${TEST_SAVED_PATHS}" "${BIN_DIR}/dotfiles-save-path"
    assert_success
    refute_output --partial 'Usage:'
    assert [ -f "${TEST_SAVED_PATHS}/0" ]
    assert [ "$(cat "${TEST_SAVED_PATHS}/0")" = "${BATS_TEST_TMPDIR}" ]
}

@test 'dotfiles-save-path default slot existing' {
    mkdir -p "${TEST_SAVED_PATHS}"
    echo 'foo' >"${TEST_SAVED_PATHS}/0"
    run_script "DOTFILES_SAVED_PATHS=${TEST_SAVED_PATHS}" "${BIN_DIR}/dotfiles-save-path"
    assert_success
    refute_output --partial 'Usage:'
    assert [ -f "${TEST_SAVED_PATHS}/0" ]
    assert [ "$(cat "${TEST_SAVED_PATHS}/0")" = "${BATS_TEST_TMPDIR}" ]
}

@test 'dotfiles-save-path other slot nonexisting' {
    run_script "DOTFILES_SAVED_PATHS=${TEST_SAVED_PATHS}" "${BIN_DIR}/dotfiles-save-path" 'foo'
    assert_success
    refute_output --partial 'Usage:'
    assert [ -f "${TEST_SAVED_PATHS}/foo" ]
    assert [ "$(cat "${TEST_SAVED_PATHS}/foo")" = "${BATS_TEST_TMPDIR}" ]
}

@test 'dotfiles-save-path other slot existing' {
    mkdir -p "${TEST_SAVED_PATHS}"
    echo 'foo' >"${TEST_SAVED_PATHS}/foo"
    run_script "DOTFILES_SAVED_PATHS=${TEST_SAVED_PATHS}" "${BIN_DIR}/dotfiles-save-path" 'foo'
    assert_success
    refute_output --partial 'Usage:'
    assert [ -f "${TEST_SAVED_PATHS}/foo" ]
    assert [ "$(cat "${TEST_SAVED_PATHS}/foo")" = "${BATS_TEST_TMPDIR}" ]
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
    echo 'foo' >"${BATS_TEST_TMPDIR}/a"
    echo 'bar' >"${BATS_TEST_TMPDIR}/b"
    mkdir -p "${BATS_TEST_TMPDIR}/target"
    run_script "DOTFILES_SAVED_PATHS=${TEST_SAVED_PATHS}" "${BIN_DIR}/dotfiles-copy-to-saved-path" '0' "${BATS_TEST_TMPDIR}/a" "${BATS_TEST_TMPDIR}/b"
    assert_failure
    refute_line --partial 'Usage:'
    assert_line 'No saved path.'
    assert [ ! -e "${TEST_SAVED_PATHS}" ]
    assert [ -f "${BATS_TEST_TMPDIR}/a" ]
    assert [ "$(cat "${BATS_TEST_TMPDIR}/a")" = 'foo' ]
    assert [ -f "${BATS_TEST_TMPDIR}/b" ]
    assert [ "$(cat "${BATS_TEST_TMPDIR}/b")" = 'bar' ]
    assert [ -d "${BATS_TEST_TMPDIR}/target" ]
    assert [ ! -e "${BATS_TEST_TMPDIR}/target/a" ]
    assert [ ! -e "${BATS_TEST_TMPDIR}/target/b" ]
}

@test 'dotfiles-copy-to-saved-path existing' {
    echo 'foo' >"${BATS_TEST_TMPDIR}/a"
    echo 'bar' >"${BATS_TEST_TMPDIR}/b"
    mkdir -p "${BATS_TEST_TMPDIR}/target"
    mkdir -p "${TEST_SAVED_PATHS}"
    echo "${BATS_TEST_TMPDIR}/target" >"${TEST_SAVED_PATHS}/0"
    run_script "DOTFILES_SAVED_PATHS=${TEST_SAVED_PATHS}" "${BIN_DIR}/dotfiles-copy-to-saved-path" '0' "${BATS_TEST_TMPDIR}/a" "${BATS_TEST_TMPDIR}/b"
    assert_success
    refute_output --partial 'Usage:'
    refute_output --partial 'No saved path.'
    assert [ -d "${TEST_SAVED_PATHS}" ]
    assert [ -f "${TEST_SAVED_PATHS}/0" ]
    assert [ "$(cat "${TEST_SAVED_PATHS}/0")" = "${BATS_TEST_TMPDIR}/target" ]
    assert [ -f "${BATS_TEST_TMPDIR}/a" ]
    assert [ "$(cat "${BATS_TEST_TMPDIR}/a")" = 'foo' ]
    assert [ -f "${BATS_TEST_TMPDIR}/b" ]
    assert [ "$(cat "${BATS_TEST_TMPDIR}/b")" = 'bar' ]
    assert [ -d "${BATS_TEST_TMPDIR}/target" ]
    assert [ -f "${BATS_TEST_TMPDIR}/target/a" ]
    assert [ "$(cat "${BATS_TEST_TMPDIR}/target/a")" = 'foo' ]
    assert [ -f "${BATS_TEST_TMPDIR}/target/b" ]
    assert [ "$(cat "${BATS_TEST_TMPDIR}/target/b")" = 'bar' ]
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
    echo 'foo' >"${BATS_TEST_TMPDIR}/a"
    echo 'bar' >"${BATS_TEST_TMPDIR}/b"
    mkdir -p "${BATS_TEST_TMPDIR}/target"
    run_script "DOTFILES_SAVED_PATHS=${TEST_SAVED_PATHS}" "${BIN_DIR}/dotfiles-move-to-saved-path" '0' "${BATS_TEST_TMPDIR}/a" "${BATS_TEST_TMPDIR}/b"
    assert_failure
    refute_line --partial 'Usage:'
    assert_line 'No saved path.'
    assert [ ! -e "${TEST_SAVED_PATHS}" ]
    assert [ -f "${BATS_TEST_TMPDIR}/a" ]
    assert [ "$(cat "${BATS_TEST_TMPDIR}/a")" = 'foo' ]
    assert [ -f "${BATS_TEST_TMPDIR}/b" ]
    assert [ "$(cat "${BATS_TEST_TMPDIR}/b")" = 'bar' ]
    assert [ -d "${BATS_TEST_TMPDIR}/target" ]
    assert [ ! -e "${BATS_TEST_TMPDIR}/target/a" ]
    assert [ ! -e "${BATS_TEST_TMPDIR}/target/b" ]
}

@test 'dotfiles-move-to-saved-path existing' {
    echo 'foo' >"${BATS_TEST_TMPDIR}/a"
    echo 'bar' >"${BATS_TEST_TMPDIR}/b"
    mkdir -p "${BATS_TEST_TMPDIR}/target"
    mkdir -p "${TEST_SAVED_PATHS}"
    echo "${BATS_TEST_TMPDIR}/target" >"${TEST_SAVED_PATHS}/0"
    run_script "DOTFILES_SAVED_PATHS=${TEST_SAVED_PATHS}" "${BIN_DIR}/dotfiles-move-to-saved-path" '0' "${BATS_TEST_TMPDIR}/a" "${BATS_TEST_TMPDIR}/b"
    assert_success
    refute_output --partial 'Usage:'
    refute_output --partial 'No saved path.'
    assert [ -d "${TEST_SAVED_PATHS}" ]
    assert [ -f "${TEST_SAVED_PATHS}/0" ]
    assert [ "$(cat "${TEST_SAVED_PATHS}/0")" = "${BATS_TEST_TMPDIR}/target" ]
    assert [ ! -e "${BATS_TEST_TMPDIR}/a" ]
    assert [ ! -e "${BATS_TEST_TMPDIR}/b" ]
    assert [ -d "${BATS_TEST_TMPDIR}/target" ]
    assert [ -f "${BATS_TEST_TMPDIR}/target/a" ]
    assert [ "$(cat "${BATS_TEST_TMPDIR}/target/a")" = 'foo' ]
    assert [ -f "${BATS_TEST_TMPDIR}/target/b" ]
    assert [ "$(cat "${BATS_TEST_TMPDIR}/target/b")" = 'bar' ]
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
    run_script "DOTFILES_MV_CP_FOLLOW=${TEST_FOLLOW}" "${BIN_DIR}/dotfiles-mv-cp-save-target" 'foo' "${BATS_TEST_TMPDIR}/bar"
    assert_success
    refute_output
    assert [ -f "${TEST_FOLLOW}" ]
    assert [ "$(cat "${TEST_FOLLOW}")" = "${BATS_TEST_TMPDIR}" ]
}

@test 'dotfiles-mv-cp-save-target dir' {
    mkdir -p "${BATS_TEST_TMPDIR}/bar"
    run_script "DOTFILES_MV_CP_FOLLOW=${TEST_FOLLOW}" "${BIN_DIR}/dotfiles-mv-cp-save-target" 'foo' "${BATS_TEST_TMPDIR}/bar"
    assert_success
    refute_output
    assert [ -f "${TEST_FOLLOW}" ]
    assert [ "$(cat "${TEST_FOLLOW}")" = "${BATS_TEST_TMPDIR}/bar" ]
}

@test 'dotfiles-mv-cp-save-target dir override' {
    mkdir -p "${BATS_TEST_TMPDIR}/bar"
    run_script "DOTFILES_MV_CP_FOLLOW=${TEST_FOLLOW}" "${BIN_DIR}/dotfiles-mv-cp-save-target" '-T' 'foo' "${BATS_TEST_TMPDIR}/bar"
    assert_success
    refute_output
    assert [ -f "${TEST_FOLLOW}" ]
    assert [ "$(cat "${TEST_FOLLOW}")" = "${BATS_TEST_TMPDIR}" ]
}

@test 'dotfiles-mv-cp-save-target -t' {
    mkdir -p "${BATS_TEST_TMPDIR}/bar"
    run_script "DOTFILES_MV_CP_FOLLOW=${TEST_FOLLOW}" "${BIN_DIR}/dotfiles-mv-cp-save-target" '-t' "${BATS_TEST_TMPDIR}/bar" 'foo'
    assert_success
    refute_output
    assert [ -f "${TEST_FOLLOW}" ]
    assert [ "$(cat "${TEST_FOLLOW}")" = "${BATS_TEST_TMPDIR}/bar" ]
}

@test 'dotfiles-mv normal' {
    echo 'foo' >"${BATS_TEST_TMPDIR}/a"
    run_script "DOTFILES_MV_CP_FOLLOW=${TEST_FOLLOW}" "${BIN_DIR}/dotfiles-mv" "${BATS_TEST_TMPDIR}/a" "${BATS_TEST_TMPDIR}/b"
    assert_success
    refute_output
    assert [ -f "${TEST_FOLLOW}" ]
    assert [ "$(cat "${TEST_FOLLOW}")" = "${BATS_TEST_TMPDIR}" ]
    assert [ ! -e "${BATS_TEST_TMPDIR}/a" ]
    assert [ -f "${BATS_TEST_TMPDIR}/b" ]
    assert [ "$(cat "${BATS_TEST_TMPDIR}/b")" = 'foo' ]
}

@test 'dotfiles-mv dir' {
    echo 'foo' >"${BATS_TEST_TMPDIR}/a"
    mkdir -p "${BATS_TEST_TMPDIR}/b"
    run_script "DOTFILES_MV_CP_FOLLOW=${TEST_FOLLOW}" "${BIN_DIR}/dotfiles-mv" "${BATS_TEST_TMPDIR}/a" "${BATS_TEST_TMPDIR}/b"
    assert_success
    refute_output
    assert [ -f "${TEST_FOLLOW}" ]
    assert [ "$(cat "${TEST_FOLLOW}")" = "${BATS_TEST_TMPDIR}/b" ]
    assert [ ! -e "${BATS_TEST_TMPDIR}/a" ]
    assert [ -d "${BATS_TEST_TMPDIR}/b" ]
    assert [ -f "${BATS_TEST_TMPDIR}/b/a" ]
    assert [ "$(cat "${BATS_TEST_TMPDIR}/b/a")" = 'foo' ]
}

@test 'dotfiles-mv target' {
    echo 'foo' >"${BATS_TEST_TMPDIR}/a"
    mkdir -p "${BATS_TEST_TMPDIR}/b"
    run_script "DOTFILES_MV_CP_FOLLOW=${TEST_FOLLOW}" "${BIN_DIR}/dotfiles-mv" '-t' "${BATS_TEST_TMPDIR}/b" "${BATS_TEST_TMPDIR}/a"
    assert_success
    refute_output
    assert [ -f "${TEST_FOLLOW}" ]
    assert [ "$(cat "${TEST_FOLLOW}")" = "${BATS_TEST_TMPDIR}/b" ]
    assert [ ! -e "${BATS_TEST_TMPDIR}/a" ]
    assert [ -d "${BATS_TEST_TMPDIR}/b" ]
    assert [ -f "${BATS_TEST_TMPDIR}/b/a" ]
    assert [ "$(cat "${BATS_TEST_TMPDIR}/b/a")" = 'foo' ]
}

@test 'dotfiles-cp normal' {
    echo 'foo' >"${BATS_TEST_TMPDIR}/a"
    run_script "DOTFILES_MV_CP_FOLLOW=${TEST_FOLLOW}" "${BIN_DIR}/dotfiles-cp" "${BATS_TEST_TMPDIR}/a" "${BATS_TEST_TMPDIR}/b"
    assert_success
    refute_output
    assert [ -f "${TEST_FOLLOW}" ]
    assert [ "$(cat "${TEST_FOLLOW}")" = "${BATS_TEST_TMPDIR}" ]
    assert [ -f "${BATS_TEST_TMPDIR}/a" ]
    assert [ "$(cat "${BATS_TEST_TMPDIR}/a")" = 'foo' ]
    assert [ -f "${BATS_TEST_TMPDIR}/b" ]
    assert [ "$(cat "${BATS_TEST_TMPDIR}/b")" = 'foo' ]
}

@test 'dotfiles-cp dir' {
    echo 'foo' >"${BATS_TEST_TMPDIR}/a"
    mkdir -p "${BATS_TEST_TMPDIR}/b"
    run_script "DOTFILES_MV_CP_FOLLOW=${TEST_FOLLOW}" "${BIN_DIR}/dotfiles-cp" "${BATS_TEST_TMPDIR}/a" "${BATS_TEST_TMPDIR}/b"
    assert_success
    refute_output
    assert [ -f "${TEST_FOLLOW}" ]
    assert [ "$(cat "${TEST_FOLLOW}")" = "${BATS_TEST_TMPDIR}/b" ]
    assert [ -f "${BATS_TEST_TMPDIR}/a" ]
    assert [ "$(cat "${BATS_TEST_TMPDIR}/a")" = 'foo' ]
    assert [ -d "${BATS_TEST_TMPDIR}/b" ]
    assert [ -f "${BATS_TEST_TMPDIR}/b/a" ]
    assert [ "$(cat "${BATS_TEST_TMPDIR}/b/a")" = 'foo' ]
}

@test 'dotfiles-cp target' {
    echo 'foo' >"${BATS_TEST_TMPDIR}/a"
    mkdir -p "${BATS_TEST_TMPDIR}/b"
    run_script "DOTFILES_MV_CP_FOLLOW=${TEST_FOLLOW}" "${BIN_DIR}/dotfiles-cp" '-t' "${BATS_TEST_TMPDIR}/b" "${BATS_TEST_TMPDIR}/a"
    assert_success
    refute_output
    assert [ -f "${TEST_FOLLOW}" ]
    assert [ "$(cat "${TEST_FOLLOW}")" = "${BATS_TEST_TMPDIR}/b" ]
    assert [ -f "${BATS_TEST_TMPDIR}/a" ]
    assert [ "$(cat "${BATS_TEST_TMPDIR}/a")" = 'foo' ]
    assert [ -d "${BATS_TEST_TMPDIR}/b" ]
    assert [ -f "${BATS_TEST_TMPDIR}/b/a" ]
    assert [ "$(cat "${BATS_TEST_TMPDIR}/b/a")" = 'foo' ]
}
