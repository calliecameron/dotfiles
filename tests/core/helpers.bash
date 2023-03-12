# shellcheck disable=SC2034
function setup_common() {
    THIS_DIR="$(cd "$(dirname "${BATS_TEST_FILENAME}")" && pwd)"
    TMP_DIR="$(mktemp -d)"

    TEST_PACKAGE_ROOT="${TMP_DIR}/packages"
    mkdir -p "${TEST_PACKAGE_ROOT}/foo"
    echo "if [ -n \"\${ZSH_VERSION}\" ]; then export TEST_PACKAGE_ENV='zsh'; else export TEST_PACKAGE_ENV=\"\$(basename \"\${0}\")\"; fi" >"${TEST_PACKAGE_ROOT}/foo/env.sh"
    echo "if [ -n \"\${ZSH_VERSION}\" ]; then export TEST_PACKAGE_GENERIC_ALIASES='zsh'; else export TEST_PACKAGE_GENERIC_ALIASES=\"\$(basename \"\${0}\")\"; fi" >"${TEST_PACKAGE_ROOT}/foo/aliases.sh"
    echo "if [ -n \"\${ZSH_VERSION}\" ]; then export TEST_PACKAGE_BASH_ALIASES='zsh'; else export TEST_PACKAGE_BASH_ALIASES=\"\$(basename \"\${0}\")\"; fi" >"${TEST_PACKAGE_ROOT}/foo/aliases.bash"
    echo "if [ -n \"\${ZSH_VERSION}\" ]; then export TEST_PACKAGE_ZSH_ALIASES='zsh'; else export TEST_PACKAGE_ZSH_ALIASES=\"\$(basename \"\${0}\")\"; fi" >"${TEST_PACKAGE_ROOT}/foo/aliases.zsh"

    TEST_LOCAL_ENV_FILE="${TMP_DIR}/.dotfiles-variables.sh"
    {
        echo "if [ -n \"\${ZSH_VERSION}\" ]; then export TEST_LOCAL_ENV='zsh'; else export TEST_LOCAL_ENV=\"\$(basename \"\${0}\")\"; fi"
        echo "appendpackageroot /foo"
        echo "prependpackageroot /bar"
        echo "export TEST_PACKAGE_ROOTS=\"\${DOTFILES_PACKAGE_ROOTS}\""
        echo "export DOTFILES_PACKAGE_ROOTS='${TEST_PACKAGE_ROOT}'"
    } >"${TEST_LOCAL_ENV_FILE}"

    TEST_LOCAL_GENERIC_ALIASES_FILE="${TMP_DIR}/.dotfiles-aliases.sh"
    echo "if [ -n \"\${ZSH_VERSION}\" ]; then export TEST_LOCAL_GENERIC_ALIASES='zsh'; else export TEST_LOCAL_GENERIC_ALIASES=\"\$(basename \"\${0}\")\"; fi" >"${TEST_LOCAL_GENERIC_ALIASES_FILE}"

    TEST_LOCAL_BASH_ALIASES_FILE="${TMP_DIR}/.dotfiles-aliases.bash"
    echo "if [ -n \"\${ZSH_VERSION}\" ]; then export TEST_LOCAL_BASH_ALIASES='zsh'; else export TEST_LOCAL_BASH_ALIASES=\"\$(basename \"\${0}\")\"; fi" >"${TEST_LOCAL_BASH_ALIASES_FILE}"

    TEST_LOCAL_ZSH_ALIASES_FILE="${TMP_DIR}/.dotfiles-aliases.zsh"
    echo "if [ -n \"\${ZSH_VERSION}\" ]; then export TEST_LOCAL_ZSH_ALIASES='zsh'; else export TEST_LOCAL_ZSH_ALIASES=\"\$(basename \"\${0}\")\"; fi" >"${TEST_LOCAL_ZSH_ALIASES_FILE}"

    TEST_NEEDS_LOGOUT="${TMP_DIR}/.dotfiles-needs-logout"
    TEST_PACKAGE_MESSAGES="${TMP_DIR}/.dotfiles-package-messages"
    TEST_PACKAGE_PROBLEMS="${TMP_DIR}/.dotfiles-package-problems"
    TEST_NEXT_LOGIN="${TMP_DIR}/.dotfiles-next-login.bash"
    TEST_NEXT_INIT="${TMP_DIR}/.dotfiles-next-init.bash"
    TEST_CAN_SUDO="${TMP_DIR}/.dotfiles-can-sudo"
}

function teardown_common() {
    rm -rf "${TMP_DIR}"
}

function run_dash() {
    run env -i -C "${TMP_DIR}" HOME="${TMP_DIR}" TERM='xterm-256color' DOTFILES_NO_PACKAGE_UPDATES='t' dash "${@}" -c 'env | LC_ALL=C sort' 3>&-
}

function run_bash() {
    # TODO get rid of DOTFILES_NO_PACKAGE_UPDATES
    run env -i -C "${TMP_DIR}" HOME="${TMP_DIR}" TERM='xterm-256color' DOTFILES_NO_PACKAGE_UPDATES='t' bash "${@}" -c 'env | LC_ALL=C sort' 3>&-
}

function run_zsh() {
    # TODO get rid of DOTFILES_NO_PACKAGE_UPDATES
    run env -i -C "${TMP_DIR}" HOME="${TMP_DIR}" TERM='xterm-256color' DOTFILES_NO_PACKAGE_UPDATES='t' zsh "${@}" -c 'env | LC_ALL=C sort' 3>&-
}

function _after_login() {
    (
        # shellcheck source=/dev/null
        source <(env -i -C "${TMP_DIR}" HOME="${TMP_DIR}" TERM='xterm-256color' DOTFILES_NO_PACKAGE_UPDATES='t' dash -l -c 'env -0' | xargs -0 bash -c 'printf "export %q\n" "$@"' -- | LC_ALL=C sort)
        bash -c "${1}"
        "${@:2}"
    )
}

function run_after_login() {
    run _after_login true "${@}" 3>&-
}

function run_bash_after_login() {
    run_after_login bash "${@}" -c 'env | LC_ALL=C sort'
}

function run_zsh_after_login() {
    run_after_login zsh "${@}" -c 'env | LC_ALL=C sort'
}

function run_between_login_and_shell() {
    run _after_login "${@}" 3>&-
}

function run_between_login_and_bash() {
    run_between_login_and_shell "${1}" bash "${@:2}" -c 'env | LC_ALL=C sort'
}

function run_between_login_and_zsh() {
    run_between_login_and_shell "${1}" zsh "${@:2}" -c 'env | LC_ALL=C sort'
}

function assert_num_matching_lines() {
    # shellcheck disable=SC2154
    assert_equal "$(echo "${output}" | grep -E -c "${1}")" "${2}"
}

function assert_path() {
    assert_line --regexp "^PATH=.*$(readlink -f "${THIS_DIR}/../..")/core/bin.*\$"
    assert_line --regexp "^PATH=.*$(readlink -f "${TMP_DIR}")/\\.bin.*\$"
}

function assert_package_roots() {
    assert_line "TEST_PACKAGE_ROOTS=/bar:$(readlink -f "${THIS_DIR}/../..")/private/packages-pre:$(readlink -f "${THIS_DIR}/../..")/packages:$(readlink -f "${THIS_DIR}/../..")/private/packages:/foo"
}

function assert_can_sudo() {
    assert_line 'DOTFILES_CAN_SUDO=y'
}

function assert_not_can_sudo() {
    assert_line 'DOTFILES_CAN_SUDO='
}

function assert_stub_ran() {
    assert_line --partial 'DOTFILES_DIR'
}

function assert_not_stub_run() {
    refute_line --partial 'DOTFILES_DIR'
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
    assert_not_stub_run
    assert_not_package_env_run
    assert_not_package_generic_aliases_run
    assert_not_package_bash_aliases_run
    assert_not_package_zsh_aliases_run
    assert_not_local_env_run
    assert_not_local_generic_aliases_run
    assert_not_local_bash_aliases_run
    assert_not_local_zsh_aliases_run
}
