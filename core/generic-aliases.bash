# -*- Shell-script -*-
#
# Stuff in here should work for both bash and zsh.
#

test ! -z "${DOTFILES_PROFILING}" && printf 'generic ' && date --rfc-3339=ns

if [ -z "${DOTFILES_OS}" ]; then
    # Haven't loaded environment variables, so have to use full path
    source "${DOTFILES_DIR}/core/environment-variables.sh"
fi

if [ -z "${DOTFILES_OS}" ]; then
    echo -e '\e[31mUnknown OS.\e[0m'
elif [ "${DOTFILES_OS}" = 'linux' ] && [ -z "${DOTFILES_LINUX_VARIANT}" ]; then
    echo -e '\e[31mUnknown Linux version.\e[0m'
fi

if [ "${DOTFILES_OS}" = 'linux' ] && [ ! -e "${DOTFILES_CAN_SUDO_FILE}" ] && [ -z "${DOTFILES_NO_PACKAGE_UPDATES}" ]; then
    if bash -c 'source "${DOTFILES_BASH_COMMON}"; yn-n "Can you sudo on this machine?"'; then
        echo 'y' > "${DOTFILES_CAN_SUDO_FILE}"
        export DOTFILES_CAN_SUDO='y'
    else
        touch "${DOTFILES_CAN_SUDO_FILE}"
    fi
    touch "${DOTFILES_NEEDS_LOGOUT}"
fi


function check-init-file() {
    local LOCALFILE="${1}"
    local PROCESSEDFILE="${1}.processed"
    local REALFILE="${2}"
    # shellcheck disable=SC2155
    local MSG="\e[33mYour $(basename "${REALFILE}") doesn't look right - maybe something has tampered with it, or you need to run install.sh again.\e[0m"

    if [ ! -e "${PROCESSEDFILE}" ]; then
        echo -e "${MSG}"
    elif [ "${LOCALFILE}" -nt "${PROCESSEDFILE}" ]; then
        echo -e "${MSG}"
    elif ! cmp "${PROCESSEDFILE}" "${REALFILE}" &>/dev/null; then
        echo -e "${MSG}"
    fi
}

check-init-file "${DOTFILES_DEFAULT_DOTFILES}/default-profile.sh" "${HOME}/.profile"
check-init-file "${DOTFILES_DEFAULT_DOTFILES}/default-bash-profile.bash" "${HOME}/.bash_profile"
check-init-file "${DOTFILES_DEFAULT_DOTFILES}/default-bash-login.bash" "${HOME}/.bash_login"
check-init-file "${DOTFILES_DEFAULT_DOTFILES}/default-bash-logout.bash" "${HOME}/.bash_logout"
check-init-file "${DOTFILES_DEFAULT_DOTFILES}/default-bashrc.bash" "${HOME}/.bashrc"
check-init-file "${DOTFILES_DEFAULT_DOTFILES}/default-zshenv.zsh" "${HOME}/.zshenv"
check-init-file "${DOTFILES_DEFAULT_DOTFILES}/default-zprofile.zsh" "${HOME}/.zprofile"
check-init-file "${DOTFILES_DEFAULT_DOTFILES}/default-zlogin.zsh" "${HOME}/.zlogin"
check-init-file "${DOTFILES_DEFAULT_DOTFILES}/default-zlogout.zsh" "${HOME}/.zlogout"
check-init-file "${DOTFILES_DEFAULT_DOTFILES}/default-zshrc.zsh" "${HOME}/.zshrc"
check-init-file "${DOTFILES_DEFAULT_DOTFILES}/default-emacs.el" "${HOME}/.emacs"

unset -f check-init-file


# Default dircolors; loaded here so packages can override them if desired
test -x /usr/bin/dircolors && eval "$(dircolors -b)"


# Load packages
test ! -z "${DOTFILES_PROFILING}" && printf 'packages ' && date --rfc-3339=ns
if [ -z "${DOTFILES_NO_PACKAGE_UPDATES}" ]; then
    if command mkdir "${DOTFILES_PACKAGE_MUTEX}" &>/dev/null; then
        "${DOTFILES_PACKAGE_SCRIPTS}/install-packages.sh"
        test ! -z "${DOTFILES_PROFILING}" && printf 'packages2 ' && date --rfc-3339=ns
        "${DOTFILES_PACKAGE_SCRIPTS}/update-packages.sh"
        test ! -z "${DOTFILES_PROFILING}" && printf 'packages3 ' && date --rfc-3339=ns
        command rmdir "${DOTFILES_PACKAGE_MUTEX}"
    else
        echo -e "\e[34mAnother shell is installing or updating packages; don't be surprised if things behave oddly in the meantime.\e[0m"
    fi
fi
source "${DOTFILES_PACKAGE_SCRIPTS}/load-packages-aliases.bash"
test ! -z "${DOTFILES_PROFILING}" && printf 'packages ' && date --rfc-3339=ns


if [ ! -z "${DOTFILES_STARTED_SSH_AGENT}" ]; then
    alias ssh='ssh-auto-add'
fi


# Use the ~/.dotfiles-aliases.{sh|bash|zsh} files for stuff that
# should not be version controlled.
function localaliases() {
    if [ -f "${DOTFILES_LOCAL_ALIASES}.${1}" ]; then
        source "${DOTFILES_LOCAL_ALIASES}.${1}"
    fi
}

localaliases 'sh'
localaliases "${DOTFILES_SHELL}"

unset -f localaliases


if [ -f "${DOTFILES_NEXT_INIT}" ]; then
    bash "${DOTFILES_NEXT_INIT}"
    command rm "${DOTFILES_NEXT_INIT}"
fi

if [ -e "${DOTFILES_PACKAGE_MESSAGES_FILE}" ]; then
    echo -en "\e[34m"
    cat "${DOTFILES_PACKAGE_MESSAGES_FILE}"
    echo -en "\e[0m"
fi

if [ -e "${DOTFILES_PACKAGE_PROBLEMS_FILE}" ]; then
    echo -en "\e[33m"
    cat "${DOTFILES_PACKAGE_PROBLEMS_FILE}"
    echo -en "\e[0m"
fi

if [ -e "${DOTFILES_NEEDS_LOGOUT}" ]; then
    echo -e "\e[33mLog out and log in again to set everything up correctly.\e[0m"
fi


test ! -z "${DOTFILES_PROFILING}" && printf 'generic ' && date --rfc-3339=ns
