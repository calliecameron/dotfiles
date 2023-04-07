# shellcheck shell=sh
#
# Environment variables go here, so they are visible to GUI programs as well as
# the shell. Must work in dash, bash and zsh.

[ -n "${DOTFILES_PROFILING}" ] && printf 'env ' && date --rfc-3339=ns

export DOTFILES_LINUX_VARIANT=''

if command -v lsb_release >/dev/null && lsb_release -a 2>/dev/null | grep 'Mint' >/dev/null; then
    export DOTFILES_LINUX_VARIANT='main'
fi

export DOTFILES_CORE_DIR="${DOTFILES_DIR}/core"

export DOTFILES_VARIABLES="${DOTFILES_CORE_DIR}/environment-variables.sh"
export DOTFILES_GENERIC_ALIASES="${DOTFILES_CORE_DIR}/generic-aliases.bash"
export DOTFILES_BASH_ALIASES="${DOTFILES_CORE_DIR}/bash-aliases.bash"
export DOTFILES_ZSH_ALIASES="${DOTFILES_CORE_DIR}/zsh-aliases.zsh"
export DOTFILES_LOGOUT_SCRIPT="${DOTFILES_CORE_DIR}/logout.sh"
export DOTFILES_EMACS_INIT="${DOTFILES_CORE_DIR}/dotfiles.el"
export DOTFILES_TEMPLATES="${DOTFILES_CORE_DIR}/templates"
export DOTFILES_CORE_BIN="${DOTFILES_CORE_DIR}/bin"

export DOTFILES_LOCAL_DIR="${HOME}/.dotfiles.d"
mkdir -p "${DOTFILES_LOCAL_DIR}"

export DOTFILES_PROCESSED_DIR="${DOTFILES_LOCAL_DIR}/processed"
export DOTFILES_LOCAL_VARIABLES="${HOME}/.dotfiles-variables.sh"
export DOTFILES_LOCAL_ALIASES="${HOME}/.dotfiles-aliases"
export DOTFILES_LOCAL_EMACS="${HOME}/.dotfiles-emacs.el"
export DOTFILES_LOCAL_BIN="${DOTFILES_LOCAL_DIR}/local-bin"

export DOTFILES_NEXT_LOGIN="${DOTFILES_LOCAL_DIR}/next-login.bash"
export DOTFILES_NEXT_INIT="${DOTFILES_LOCAL_DIR}/next-init.bash"
export DOTFILES_NEEDS_LOGOUT="${DOTFILES_LOCAL_DIR}/needs-logout"

if [ -e "${DOTFILES_NEEDS_LOGOUT}" ]; then
    rm -f "${DOTFILES_NEEDS_LOGOUT}"
fi

export DOTFILES_PACKAGE_SCRIPTS="${DOTFILES_CORE_DIR}/package-scripts"
export DOTFILES_PACKAGE_INSTALL_DIR="${DOTFILES_LOCAL_DIR}/packages"
export DOTFILES_PACKAGE_IGNORE_FILE="${DOTFILES_LOCAL_DIR}/package-ignored"
export DOTFILES_PACKAGE_MUTEX="${DOTFILES_LOCAL_DIR}/package-mutex"
export DOTFILES_PACKAGE_MESSAGES_FILE="${DOTFILES_LOCAL_DIR}/package-messages"
export DOTFILES_PACKAGE_PROBLEMS_FILE="${DOTFILES_LOCAL_DIR}/package-problems"
export DOTFILES_PACKAGE_LAST_UPDATE_FILE="${DOTFILES_LOCAL_DIR}/package-last-update"
export DOTFILES_PACKAGE_ALREADY_UPDATED_FILE="${DOTFILES_LOCAL_DIR}/package-already-updated"

export DOTFILES_PRIVATE_DIR="${DOTFILES_LOCAL_DIR}/private"
export DOTFILES_PRIVATE_REPO=''
export DOTFILES_PRIVATE_BRANCH=''

export DOTFILES_PACKAGE_ROOTS="${DOTFILES_PRIVATE_DIR}/packages-pre:${DOTFILES_DIR}/packages:${DOTFILES_PRIVATE_DIR}/packages"

if [ -d "${DOTFILES_PACKAGE_INSTALL_DIR}" ] && [ ! -e "${DOTFILES_PACKAGE_LAST_UPDATE_FILE}" ]; then
    date '+%s' >"${DOTFILES_PACKAGE_LAST_UPDATE_FILE}"
fi

if [ -e "${DOTFILES_PACKAGE_MESSAGES_FILE}" ]; then
    rm -f "${DOTFILES_PACKAGE_MESSAGES_FILE}"
fi

if [ -e "${DOTFILES_PACKAGE_PROBLEMS_FILE}" ]; then
    rm -f "${DOTFILES_PACKAGE_PROBLEMS_FILE}"
fi

export PATH="${DOTFILES_CORE_BIN}:${PATH}"

export EDITOR='nano'
export PAGER='less'
export LESS='FRMX'

# Create SSH agent if necessary
if [ -z "${SSH_AUTH_SOCK}" ] && [ -z "${DISPLAY}" ] && command -v ssh-agent >/dev/null; then
    eval "$(ssh-agent -s)" >/dev/null 2>/dev/null
    export DOTFILES_STARTED_SSH_AGENT='t'
    DOTFILES_SSH_ADDED_FILE="$(readlink -f "$(mktemp)")"
    export DOTFILES_SSH_ADDED_FILE
fi

appendpackageroot() {
    # shellcheck disable=SC2317
    if dotfiles-package-root-valid "${1}"; then
        export DOTFILES_PACKAGE_ROOTS="${DOTFILES_PACKAGE_ROOTS}:${1}"
    else
        dotfiles-log-package-problem "Invalid package root: ${1}"
    fi
}

prependpackageroot() {
    # shellcheck disable=SC2317
    if dotfiles-package-root-valid "${1}"; then
        export DOTFILES_PACKAGE_ROOTS="${1}:${DOTFILES_PACKAGE_ROOTS}"
    else
        dotfiles-log-package-problem "Invalid package root: ${1}"
    fi
}

# Use the ~/.dotfiles-variables.sh file for stuff that should be visible to GUI
# programs, but should not be version controlled.
if [ -f "${DOTFILES_LOCAL_VARIABLES}" ]; then
    # shellcheck source=/dev/null
    . "${DOTFILES_LOCAL_VARIABLES}"
fi

unset appendpackageroot prependpackageroot

# Load packages
# shellcheck source=/dev/null
. "${DOTFILES_PACKAGE_SCRIPTS}/load-all-packages-env.sh"

export PATH="${DOTFILES_LOCAL_BIN}:${PATH}"

if [ -f "${DOTFILES_NEXT_LOGIN}" ]; then
    bash "${DOTFILES_NEXT_LOGIN}"
    rm "${DOTFILES_NEXT_LOGIN}"
fi

export DOTFILES_ENV_LOADED='t'

[ -n "${DOTFILES_PROFILING}" ] && printf 'env ' && date --rfc-3339=ns
