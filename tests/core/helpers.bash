# shellcheck disable=SC2034
function setup_common() {
    TEST_PACKAGE_ROOT="${TMP_DIR}/packages"
    mkdir -p "${TEST_PACKAGE_ROOT}/foo"
    echo "echo \"TEST_PACKAGE_ENV=\$(basename \"\${0}\")\"" >"${TEST_PACKAGE_ROOT}/foo/env.sh"
    echo "echo \"TEST_PACKAGE_GENERIC_ALIASES=\$(basename \"\${0}\")\"" >"${TEST_PACKAGE_ROOT}/foo/aliases.sh"
    echo "echo \"TEST_PACKAGE_BASH_ALIASES=\$(basename \"\${0}\")\"" >"${TEST_PACKAGE_ROOT}/foo/aliases.bash"
    echo "echo \"TEST_PACKAGE_ZSH_ALIASES=\$(basename \"\${0}\")\"" >"${TEST_PACKAGE_ROOT}/foo/aliases.zsh"

    TEST_LOCAL_ENV="${TMP_DIR}/.dotfiles-variables.sh"
    {
        echo "echo \"TEST_LOCAL_ENV=\$(basename \"\${0}\")\""
        echo "appendpackageroot /foo"
        echo "prependpackageroot /bar"
        echo "echo \"TEST_PACKAGE_ROOTS=\${DOTFILES_PACKAGE_ROOTS}\""
        echo "export DOTFILES_PACKAGE_ROOTS='${TEST_PACKAGE_ROOT}'"
    } >"${TEST_LOCAL_ENV}"

    TEST_LOCAL_GENERIC_ALIASES="${TMP_DIR}/.dotfiles-aliases.sh"
    echo "echo \"TEST_LOCAL_GENERIC_ALIASES=\$(basename \"\${0}\")\"" >"${TEST_LOCAL_GENERIC_ALIASES}"

    TEST_LOCAL_BASH_ALIASES="${TMP_DIR}/.dotfiles-aliases.bash"
    echo "echo \"TEST_LOCAL_BASH_ALIASES=\$(basename \"\${0}\")\"" >"${TEST_LOCAL_BASH_ALIASES}"

    TEST_LOCAL_ZSH_ALIASES="${TMP_DIR}/.dotfiles-aliases.zsh"
    echo "echo \"TEST_LOCAL_ZSH_ALIASES=\$(basename \"\${0}\")\"" >"${TEST_LOCAL_ZSH_ALIASES}"

    TEST_NEEDS_LOGOUT="${TMP_DIR}/.dotfiles-needs-logout"
    TEST_PACKAGE_MESSAGES="${TMP_DIR}/.dotfiles-package-messages"
    TEST_PACKAGE_PROBLEMS="${TMP_DIR}/.dotfiles-package-problems"
    TEST_NEXT_LOGIN="${TMP_DIR}/.dotfiles-next-login.bash"
}

function assert_num_matching_lines() {
    # shellcheck disable=SC2154
    assert_equal "$(echo "${output}" | grep -E -c "${1}")" "${2}"
}

function assert_package_env_run_by() {
    assert_line "TEST_PACKAGE_ENV=${1}"
    assert_num_matching_lines '^TEST_PACKAGE_ENV=' '1'
}

function assert_not_package_env_run() {
    refute_line --partial 'TEST_PACKAGE_ENV'
}

function assert_package_generic_aliases_run_by() {
    assert_line "TEST_PACKAGE_GENERIC_ALIASES=${1}"
    assert_num_matching_lines '^TEST_PACKAGE_GENERIC_ALIASES=' '1'
}

function assert_not_package_generic_aliases_run() {
    refute_line --partial 'TEST_PACKAGE_GENERIC_ALIASES'
}

function assert_package_bash_aliases_run_by() {
    assert_line "TEST_PACKAGE_BASH_ALIASES=${1}"
    assert_num_matching_lines '^TEST_PACKAGE_BASH_ALIASES=' '1'
}

function assert_not_package_bash_aliases_run() {
    refute_line --partial 'TEST_PACKAGE_BASH_ALIASES'
}

function assert_package_zsh_aliases_run_by() {
    assert_line "TEST_PACKAGE_ZSH_ALIASES=${1}"
    assert_num_matching_lines '^TEST_PACKAGE_ZSH_ALIASES=' '1'
}

function assert_not_package_zsh_aliases_run() {
    refute_line --partial 'TEST_PACKAGE_ZSH_ALIASES'
}

function assert_local_env_run_by() {
    assert_line "TEST_LOCAL_ENV=${1}"
    assert_num_matching_lines '^TEST_LOCAL_ENV=' '1'
}

function assert_not_local_env_run() {
    refute_line --partial 'TEST_LOCAL_ENV'
}

function assert_local_generic_aliases_run_by() {
    assert_line "TEST_LOCAL_GENERIC_ALIASES=${1}"
    assert_num_matching_lines '^TEST_LOCAL_GENERIC_ALIASES=' '1'
}

function assert_not_local_generic_aliases_run() {
    refute_line --partial 'TEST_LOCAL_GENERIC_ALIASES'
}

function assert_local_bash_aliases_run_by() {
    assert_line "TEST_LOCAL_BASH_ALIASES=${1}"
    assert_num_matching_lines '^TEST_LOCAL_BASH_ALIASES=' '1'
}

function assert_not_local_bash_aliases_run() {
    refute_line --partial 'TEST_LOCAL_BASH_ALIASES'
}

function assert_local_zsh_aliases_run_by() {
    assert_line "TEST_LOCAL_ZSH_ALIASES=${1}"
    assert_num_matching_lines '^TEST_LOCAL_ZSH_ALIASES=' '1'
}

function assert_not_local_zsh_aliases_run() {
    refute_line --partial 'TEST_LOCAL_ZSH_ALIASES'
}

function assert_nothing_ran() {
    refute_output --partial 'DOTFILES'
    assert_not_package_env_run
    assert_not_package_generic_aliases_run
    assert_not_package_bash_aliases_run
    assert_not_package_zsh_aliases_run
    assert_not_local_env_run
    assert_not_local_generic_aliases_run
    assert_not_local_bash_aliases_run
    assert_not_local_zsh_aliases_run
}
