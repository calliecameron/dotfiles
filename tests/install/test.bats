#!/usr/bin/env bats

setup() {
    bats_load_library 'bats-support'
    bats_load_library 'bats-assert'

    THIS_DIR="$(cd "$(dirname "${BATS_TEST_FILENAME}")" && pwd)"
    TMP_DIR="$(mktemp -d)"
    PROCESSED_DIR="${TMP_DIR}/.dotfiles-processed"
    INSTALL="${THIS_DIR}/../../install.sh"
}

teardown() {
    rm -rf "${TMP_DIR}"
}

@test "install" {
    # Initial call, creates everything
    HOME="${TMP_DIR}" run "${INSTALL}"
    assert_success
    assert_output --partial 'Log out and log in again to set everything up correctly.'
    assert_equal "$(find "${TMP_DIR}" -maxdepth 1 -type f -name '.*' | wc -l)" '11'
    assert_equal "$(find "${PROCESSED_DIR}" -type f -name '.*' | wc -l)" '11'
    run rgrep '@' "${TMP_DIR}"
    assert_failure

    # Second call, no change
    HOME="${TMP_DIR}" run "${INSTALL}"
    assert_success
    assert_output --partial 'Log out and log in again to set everything up correctly.'
    assert_equal "$(find "${TMP_DIR}" -maxdepth 1 -type f -name '.*' | wc -l)" '11'
    assert_equal "$(find "${PROCESSED_DIR}" -type f -name '.*' | wc -l)" '11'
    run rgrep '@' "${TMP_DIR}"
    assert_failure

    # Modify a file, will be backed up
    echo 'foo' >>"${TMP_DIR}/.bashrc"
    HOME="${TMP_DIR}" run "${INSTALL}"
    assert_success
    assert_output --partial 'Log out and log in again to set everything up correctly.'
    assert_equal "$(find "${TMP_DIR}" -maxdepth 1 -type f -name '.*' | wc -l)" '12'
    assert_equal "$(find "${TMP_DIR}" -maxdepth 1 -type f -name '.*.backup' | wc -l)" '1'
    assert_equal "$(find "${PROCESSED_DIR}" -type f -name '.*' | wc -l)" '11'
    run rgrep '@' "${TMP_DIR}"
    assert_failure

    # Modify again, will fail because the backup already exists
    echo 'foo' >>"${TMP_DIR}/.bashrc"
    HOME="${TMP_DIR}" run "${INSTALL}"
    assert_failure
    assert_output --partial 'backup already exists'
    assert_equal "$(find "${TMP_DIR}" -maxdepth 1 -type f -name '.*' | wc -l)" '12'
    assert_equal "$(find "${TMP_DIR}" -maxdepth 1 -type f -name '.*.backup' | wc -l)" '1'
    assert_equal "$(find "${PROCESSED_DIR}" -type f -name '.*' | wc -l)" '11'
    run rgrep '@' "${TMP_DIR}"
    assert_failure
}
