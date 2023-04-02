# shellcheck disable=SC2034

function init_load_file() {
    local PACKAGE_NAME="${1}"
    local TYPE="${2}"

    echo "
if [ -n \"\${ZSH_VERSION}\" ]; then
    export TEST_PACKAGE_${PACKAGE_NAME}_${TYPE}='zsh'
else
    export TEST_PACKAGE_${PACKAGE_NAME}_${TYPE}=\"\$(basename \"\${0}\")\"
fi

export TEST_PACKAGE_${PACKAGE_NAME}_${TYPE}_ROOT=\"\${PACKAGE_ROOT}\"
export TEST_PACKAGE_${PACKAGE_NAME}_${TYPE}_NAME=\"\${PACKAGE_NAME}\"
export TEST_PACKAGE_${PACKAGE_NAME}_${TYPE}_SOURCE_DIR=\"\${PACKAGE_SOURCE_DIR}\"
export TEST_PACKAGE_${PACKAGE_NAME}_${TYPE}_INSTALL_DIR=\"\${PACKAGE_INSTALL_DIR}\"
export TEST_PACKAGE_${PACKAGE_NAME}_${TYPE}_CWD=\"\$(pwd)\"
"
}

function init_package() {
    local PACKAGE_ROOT="${1}"
    local PACKAGE_NAME="${2}"
    local PACKAGE_SOURCE_DIR="${PACKAGE_ROOT}/${PACKAGE_NAME}"

    mkdir -p "${PACKAGE_SOURCE_DIR}"

    init_load_file "${PACKAGE_NAME}" 'ENV' >"${PACKAGE_SOURCE_DIR}/env.sh"
    init_load_file "${PACKAGE_NAME}" 'GENERIC_ALIASES' >"${PACKAGE_SOURCE_DIR}/aliases.sh"
    init_load_file "${PACKAGE_NAME}" 'BASH_ALIASES' >"${PACKAGE_SOURCE_DIR}/aliases.bash"
    init_load_file "${PACKAGE_NAME}" 'ZSH_ALIASES' >"${PACKAGE_SOURCE_DIR}/aliases.zsh"
}

function setup_common() {
    THIS_DIR="$(cd "$(dirname "${BATS_TEST_FILENAME}")" && pwd)"
    TMP_DIR="$(mktemp -d)"
    mkdir -p "${TMP_DIR}/.dotfiles.d"

    TEST_NEEDS_LOGOUT="${TMP_DIR}/.dotfiles.d/needs-logout"
    TEST_PACKAGE_MESSAGES="${TMP_DIR}/.dotfiles.d/package-messages"
    TEST_PACKAGE_PROBLEMS="${TMP_DIR}/.dotfiles.d/package-problems"
    TEST_NEXT_LOGIN="${TMP_DIR}/.dotfiles.d/next-login.bash"
    TEST_NEXT_INIT="${TMP_DIR}/.dotfiles.d/next-init.bash"
    TEST_PACKAGE_INSTALL_DIR="${TMP_DIR}/.dotfiles.d/packages"
    TEST_PACKAGE_IGNORE_FILE="${TEST_PACKAGE_INSTALL_DIR}/ignored.txt"
    TEST_PACKAGE_MUTEX="${TMP_DIR}/.dotfiles.d/package-mutex"
    TEST_PRIVATE_DIR="${TMP_DIR}/.dotfiles.d/private"

    mkdir -p "${TEST_PACKAGE_INSTALL_DIR}"
    mkdir -p "${TMP_DIR}/.local/share/nemo/scripts"

    TEST_PACKAGE_ROOT_1="${TMP_DIR}/packages1"
    TEST_PACKAGE_ROOT_2="${TMP_DIR}/packages2"

    # foo: active without installation
    init_package "${TEST_PACKAGE_ROOT_1}" 'foo'
    mkdir -p "${TEST_PACKAGE_ROOT_1}/foo/bin"
    mkdir -p "${TEST_PACKAGE_ROOT_1}/foo/nemo-scripts"
    touch "${TEST_PACKAGE_ROOT_1}/foo/nemo-scripts/a"
    mkdir -p "${TEST_PACKAGE_ROOT_1}/foo/zsh-completions"

    # bar: ignored, active without installation
    init_package "${TEST_PACKAGE_ROOT_1}" 'bar'
    echo 'bar' >>"${TEST_PACKAGE_IGNORE_FILE}"
    mkdir -p "${TEST_PACKAGE_ROOT_1}/bar/nemo-scripts"
    touch "${TEST_PACKAGE_ROOT_1}/bar/nemo-scripts/b"
    mkdir -p "${TEST_PACKAGE_ROOT_1}/bar/zsh-completions"

    # baz: installed
    init_package "${TEST_PACKAGE_ROOT_1}" 'baz'
    mkdir -p "${TEST_PACKAGE_ROOT_1}/baz/bin"
    touch "${TEST_PACKAGE_ROOT_1}/baz/install"
    mkdir -p "${TEST_PACKAGE_INSTALL_DIR}/baz"
    touch "${TEST_PACKAGE_INSTALL_DIR}/baz.installed"

    # quux: can install, no requirements
    init_package "${TEST_PACKAGE_ROOT_2}" 'quux'
    touch "${TEST_PACKAGE_ROOT_2}/quux/install"

    # blah: installation requirements met
    init_package "${TEST_PACKAGE_ROOT_2}" 'blah'
    touch "${TEST_PACKAGE_ROOT_2}/blah/install"
    printf "#!/bin/bash\ntrue\n" >"${TEST_PACKAGE_ROOT_2}/blah/can-install"
    chmod u+x "${TEST_PACKAGE_ROOT_2}/blah/can-install"

    # yay: installation requirements unmet
    init_package "${TEST_PACKAGE_ROOT_2}" 'yay'
    touch "${TEST_PACKAGE_ROOT_2}/yay/install"
    printf "#!/bin/bash\nfalse\n" >"${TEST_PACKAGE_ROOT_2}/yay/can-install"
    chmod u+x "${TEST_PACKAGE_ROOT_2}/yay/can-install"

    # stuff: installed and ignored
    init_package "${TEST_PACKAGE_ROOT_2}" 'stuff'
    mkdir -p "${TEST_PACKAGE_ROOT_2}/stuff/bin"
    touch "${TEST_PACKAGE_ROOT_2}/stuff/install"
    mkdir -p "${TEST_PACKAGE_INSTALL_DIR}/stuff"
    touch "${TEST_PACKAGE_INSTALL_DIR}/stuff.installed"
    echo 'stuff' >>"${TEST_PACKAGE_IGNORE_FILE}"

    # foo in second root: shadowed
    init_package "${TEST_PACKAGE_ROOT_2}" 'foo'

    TEST_LOCAL_ENV_FILE="${TMP_DIR}/.dotfiles.d/local-variables.sh"
    {
        echo "if [ -n \"\${ZSH_VERSION}\" ]; then export TEST_LOCAL_ENV='zsh'; else export TEST_LOCAL_ENV=\"\$(basename \"\${0}\")\"; fi"
        echo "appendpackageroot /foo"
        echo "prependpackageroot /bar"
        echo "export TEST_PACKAGE_ROOTS=\"\${DOTFILES_PACKAGE_ROOTS}\""
        echo "export DOTFILES_PACKAGE_ROOTS='${TEST_PACKAGE_ROOT_1}:${TEST_PACKAGE_ROOT_2}:${TMP_DIR}/packages3'"
    } >"${TEST_LOCAL_ENV_FILE}"

    TEST_LOCAL_GENERIC_ALIASES_FILE="${TMP_DIR}/.dotfiles.d/local-aliases.sh"
    echo "if [ -n \"\${ZSH_VERSION}\" ]; then export TEST_LOCAL_GENERIC_ALIASES='zsh'; else export TEST_LOCAL_GENERIC_ALIASES=\"\$(basename \"\${0}\")\"; fi" >"${TEST_LOCAL_GENERIC_ALIASES_FILE}"

    TEST_LOCAL_BASH_ALIASES_FILE="${TMP_DIR}/.dotfiles.d/local-aliases.bash"
    echo "if [ -n \"\${ZSH_VERSION}\" ]; then export TEST_LOCAL_BASH_ALIASES='zsh'; else export TEST_LOCAL_BASH_ALIASES=\"\$(basename \"\${0}\")\"; fi" >"${TEST_LOCAL_BASH_ALIASES_FILE}"

    TEST_LOCAL_ZSH_ALIASES_FILE="${TMP_DIR}/.dotfiles.d/local-aliases.zsh"
    echo "
if [ -n \"\${ZSH_VERSION}\" ]; then
    export TEST_LOCAL_ZSH_ALIASES='zsh'
    export TEST_FPATH=\"\${fpath}\"
else
    export TEST_LOCAL_ZSH_ALIASES=\"\$(basename \"\${0}\")\"
fi
" >"${TEST_LOCAL_ZSH_ALIASES_FILE}"
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
    assert_line --regexp "^PATH=.*$(readlink -f "${TMP_DIR}")/\\.dotfiles\\.d/local-bin.*\$"
    assert_line --regexp "^PATH=.*$(readlink -f "${TEST_PACKAGE_ROOT_1}")/foo/bin.*\$"
    assert_line --regexp "^PATH=.*$(readlink -f "${TEST_PACKAGE_ROOT_1}")/baz/bin.*\$"
    refute_line --regexp "^PATH=.*$(readlink -f "${TEST_PACKAGE_ROOT_2}")/stuff/bin.*\$"
}

function assert_not_path() {
    refute_line --regexp "^PATH=.*$(readlink -f "${THIS_DIR}/../..")/core/bin.*\$"
    refute_line --regexp "^PATH=.*$(readlink -f "${TMP_DIR}")/\\.dotfiles\\.d/local-bin.*\$"
    refute_line --regexp "^PATH=.*$(readlink -f "${TEST_PACKAGE_ROOT_1}")/foo/bin.*\$"
    refute_line --regexp "^PATH=.*$(readlink -f "${TEST_PACKAGE_ROOT_1}")/baz/bin.*\$"
    refute_line --regexp "^PATH=.*$(readlink -f "${TEST_PACKAGE_ROOT_2}")/stuff/bin.*\$"
}

function assert_package_roots() {
    assert_line "TEST_PACKAGE_ROOTS=/bar:$(readlink -f "${TMP_DIR}")/.dotfiles.d/private/packages-pre:$(readlink -f "${THIS_DIR}/../..")/packages:$(readlink -f "${TMP_DIR}")/.dotfiles.d/private/packages:/foo"
}

function assert_not_package_roots() {
    refute_line --partial "TEST_PACKAGE_ROOTS"
}

function assert_nemo_scripts() {
    assert [ -h "${TMP_DIR}/.local/share/nemo/scripts/a" ]
    assert [ "$(readlink -f "${TMP_DIR}/.local/share/nemo/scripts/a")" = "${TEST_PACKAGE_ROOT_1}/foo/nemo-scripts/a" ]
    assert [ ! -e "${TMP_DIR}/.local/share/nemo/scripts/b" ]
}

function assert_not_nemo_scripts() {
    assert [ ! -e "${TMP_DIR}/.local/share/nemo/scripts/a" ]
    assert [ ! -e "${TMP_DIR}/.local/share/nemo/scripts/b" ]
}

function assert_zsh_completions() {
    assert_line --regexp "^TEST_FPATH=.*$(readlink -f "${TEST_PACKAGE_ROOT_1}")/foo/zsh-completions.*\$"
    refute_line --regexp "^TEST_FPATH=.*$(readlink -f "${TEST_PACKAGE_ROOT_1}")/bar/zsh-completions.*\$"
}

function assert_not_zsh_completions() {
    refute_line --partial "TEST_FPATH"
}

function assert_stub_ran() {
    assert_line --partial 'DOTFILES_DIR'
}

function assert_not_stub_run() {
    refute_line --partial 'DOTFILES_DIR'
}

function assert_package_type_run_by() {
    local ROOT="${1}"
    local NAME="${2}"
    local TYPE="${3}"
    local RUN_BY="${4}"
    assert_line "TEST_PACKAGE_${NAME}_${TYPE}=${RUN_BY}"
    assert_num_matching_lines "^TEST_PACKAGE_${NAME}_${TYPE}=" '1'
    assert_line "TEST_PACKAGE_${NAME}_${TYPE}_ROOT=${ROOT}"
    assert_num_matching_lines "^TEST_PACKAGE_${NAME}_${TYPE}_ROOT=" '1'
    assert_line "TEST_PACKAGE_${NAME}_${TYPE}_NAME=${NAME}"
    assert_num_matching_lines "^TEST_PACKAGE_${NAME}_${TYPE}_NAME=" '1'
    assert_line "TEST_PACKAGE_${NAME}_${TYPE}_SOURCE_DIR=${ROOT}/${NAME}"
    assert_num_matching_lines "^TEST_PACKAGE_${NAME}_${TYPE}_SOURCE_DIR=" '1'
    assert_line "TEST_PACKAGE_${NAME}_${TYPE}_INSTALL_DIR=${TEST_PACKAGE_INSTALL_DIR}/${NAME}"
    assert_num_matching_lines "^TEST_PACKAGE_${NAME}_${TYPE}_INSTALL_DIR=" '1'
    assert_line "TEST_PACKAGE_${NAME}_${TYPE}_CWD=${ROOT}/${NAME}"
    assert_num_matching_lines "^TEST_PACKAGE_${NAME}_${TYPE}_CWD=" '1'
}

function assert_package_env_run_by() {
    assert_package_type_run_by "${1}" "${2}" 'ENV' "${3}"
}

function assert_not_package_env_run() {
    refute_line --partial "TEST_PACKAGE_${1}_ENV"
}

function assert_package_generic_aliases_run_by() {
    assert_package_type_run_by "${1}" "${2}" 'GENERIC_ALIASES' "${3}"
}

function assert_not_package_generic_aliases_run() {
    refute_line --partial "TEST_PACKAGE_${1}_GENERIC_ALIASES"
}

function assert_package_bash_aliases_run_by() {
    assert_package_type_run_by "${1}" "${2}" 'BASH_ALIASES' "${3}"
}

function assert_not_package_bash_aliases_run() {
    refute_line --partial "TEST_PACKAGE_${1}_BASH_ALIASES"
}

function assert_package_zsh_aliases_run_by() {
    assert_package_type_run_by "${1}" "${2}" 'ZSH_ALIASES' "${3}"
}

function assert_not_package_zsh_aliases_run() {
    refute_line --partial "TEST_PACKAGE_${1}_ZSH_ALIASES"
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

    assert_not_package_env_run 'foo'
    assert_not_package_env_run 'bar'
    assert_not_package_env_run 'baz'
    assert_not_package_env_run 'quux'
    assert_not_package_env_run 'blah'
    assert_not_package_env_run 'yay'
    assert_not_package_env_run 'stuff'

    assert_not_package_generic_aliases_run 'foo'
    assert_not_package_generic_aliases_run 'bar'
    assert_not_package_generic_aliases_run 'baz'
    assert_not_package_generic_aliases_run 'quux'
    assert_not_package_generic_aliases_run 'blah'
    assert_not_package_generic_aliases_run 'yay'
    assert_not_package_generic_aliases_run 'stuff'

    assert_not_package_bash_aliases_run 'foo'
    assert_not_package_bash_aliases_run 'bar'
    assert_not_package_bash_aliases_run 'baz'
    assert_not_package_bash_aliases_run 'quux'
    assert_not_package_bash_aliases_run 'blah'
    assert_not_package_bash_aliases_run 'yay'
    assert_not_package_bash_aliases_run 'stuff'

    assert_not_package_zsh_aliases_run 'foo'
    assert_not_package_zsh_aliases_run 'bar'
    assert_not_package_zsh_aliases_run 'baz'
    assert_not_package_zsh_aliases_run 'quux'
    assert_not_package_zsh_aliases_run 'blah'
    assert_not_package_zsh_aliases_run 'yay'
    assert_not_package_zsh_aliases_run 'stuff'

    assert_not_local_env_run
    assert_not_local_generic_aliases_run
    assert_not_local_bash_aliases_run
    assert_not_local_zsh_aliases_run
    assert_not_path
    assert_not_package_roots
    assert_not_nemo_scripts
    assert_not_zsh_completions
}
