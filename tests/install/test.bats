# shellcheck shell=bats

setup() {
    THIS_DIR="$(cd "$(dirname "${BATS_TEST_FILENAME}")" && pwd)"
    PROCESSED_DIR="${BATS_TEST_TMPDIR}/.dotfiles.d/processed"
    INSTALL="${THIS_DIR}/../../install.sh"

    bats_load_library 'bats-support'
    bats_load_library 'bats-assert'
}

@test "install" {
    # Initial call, creates everything
    HOME="${BATS_TEST_TMPDIR}" run "${INSTALL}"
    assert_success
    assert_output --partial 'Log out and log in again to set everything up correctly.'
    assert_equal "$(find "${BATS_TEST_TMPDIR}" -maxdepth 1 -type f -name '.*' | wc -l)" '11'
    assert_equal "$(find "${PROCESSED_DIR}" -type f -name '.*' | wc -l)" '11'
    run rgrep '@' "${BATS_TEST_TMPDIR}"
    assert_failure

    # Second call, no change
    HOME="${BATS_TEST_TMPDIR}" run "${INSTALL}"
    assert_success
    assert_output --partial 'Log out and log in again to set everything up correctly.'
    assert_equal "$(find "${BATS_TEST_TMPDIR}" -maxdepth 1 -type f -name '.*' | wc -l)" '11'
    assert_equal "$(find "${PROCESSED_DIR}" -type f -name '.*' | wc -l)" '11'
    run rgrep '@' "${BATS_TEST_TMPDIR}"
    assert_failure

    # Modify a file, will be backed up
    echo 'foo' >>"${BATS_TEST_TMPDIR}/.bashrc"
    HOME="${BATS_TEST_TMPDIR}" run "${INSTALL}"
    assert_success
    assert_output --partial 'Log out and log in again to set everything up correctly.'
    assert_equal "$(find "${BATS_TEST_TMPDIR}" -maxdepth 1 -type f -name '.*' | wc -l)" '12'
    assert_equal "$(find "${BATS_TEST_TMPDIR}" -maxdepth 1 -type f -name '.*.backup' | wc -l)" '1'
    assert_equal "$(find "${PROCESSED_DIR}" -type f -name '.*' | wc -l)" '11'
    run rgrep '@' "${BATS_TEST_TMPDIR}"
    assert_failure

    # Modify again, will fail because the backup already exists
    echo 'foo' >>"${BATS_TEST_TMPDIR}/.bashrc"
    HOME="${BATS_TEST_TMPDIR}" run "${INSTALL}"
    assert_failure
    assert_output --partial 'backup already exists'
    assert_equal "$(find "${BATS_TEST_TMPDIR}" -maxdepth 1 -type f -name '.*' | wc -l)" '12'
    assert_equal "$(find "${BATS_TEST_TMPDIR}" -maxdepth 1 -type f -name '.*.backup' | wc -l)" '1'
    assert_equal "$(find "${PROCESSED_DIR}" -type f -name '.*' | wc -l)" '11'
    run rgrep '@' "${BATS_TEST_TMPDIR}"
    assert_failure
}
