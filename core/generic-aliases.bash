# Stuff in here should work for both bash and zsh.

[ -n "${DOTFILES_PROFILING}" ] && printf 'generic ' && date --rfc-3339=ns

if [ -z "${DOTFILES_ENV_LOADED}" ]; then
    # Haven't loaded environment variables, so have to use full path
    # shellcheck source=/dev/null
    source "${DOTFILES_DIR}/core/environment-variables.sh"
fi

if [ -z "${DOTFILES_LINUX_VARIANT}" ]; then
    echo -e '\e[31mUnknown Linux version.\e[0m'
fi

if [ "${TERM}" = 'dumb' ]; then
    # Not exporting, because we don't want this leaking into subprocesses
    DOTFILES_NO_ALIASES='t'
    echo "Not loading aliases because \$TERM is 'dumb' (probably TRAMP, which is easily confused)."
fi

function check-init-file() {
    local REPO_FILE="${DOTFILES_TEMPLATES}/${1}"
    local PROCESSED_FILE="${DOTFILES_PROCESSED_DIR}/${2}"
    local INSTALLED_FILE="${HOME}/${2}"
    local MSG
    MSG="\e[33mYour $(basename "${INSTALLED_FILE}") doesn't look right - maybe something has tampered with it, or you need to run install.sh again.\e[0m"

    if [ ! -e "${PROCESSED_FILE}" ] ||
        [ ! -e "${REPO_FILE}" ] ||
        [ "${REPO_FILE}" -nt "${PROCESSED_FILE}" ] || # '-nt' is 'newer than'
        ! cmp "${PROCESSED_FILE}" "${INSTALLED_FILE}" &>/dev/null; then
        echo -e "${MSG}"
    fi
}

check-init-file 'profile.sh' '.profile'
check-init-file 'bash-profile.bash' '.bash_profile'
check-init-file 'bash-login.bash' '.bash_login'
check-init-file 'bash-logout.bash' '.bash_logout'
check-init-file 'bashrc.bash' '.bashrc'
check-init-file 'zshenv.zsh' '.zshenv'
check-init-file 'zprofile.zsh' '.zprofile'
check-init-file 'zlogin.zsh' '.zlogin'
check-init-file 'zlogout.zsh' '.zlogout'
check-init-file 'zshrc.zsh' '.zshrc'
check-init-file 'emacs.el' '.emacs'

unset -f check-init-file

# Default dircolors; loaded here so packages can override them if desired
[ -x /usr/bin/dircolors ] && eval "$(dircolors -b)"

# Load packages
[ -n "${DOTFILES_PROFILING}" ] && printf 'packages ' && date --rfc-3339=ns
if [ -z "${DOTFILES_NO_PACKAGE_UPDATES}" ]; then
    if command mkdir "${DOTFILES_PACKAGE_MUTEX}" &>/dev/null; then
        "${DOTFILES_PACKAGE_SCRIPTS}/install-packages.sh"
        [ -n "${DOTFILES_PROFILING}" ] && printf 'packages2 ' && date --rfc-3339=ns
        "${DOTFILES_PACKAGE_SCRIPTS}/update-packages.sh"
        [ -n "${DOTFILES_PROFILING}" ] && printf 'packages3 ' && date --rfc-3339=ns
        command rmdir "${DOTFILES_PACKAGE_MUTEX}"
    else
        echo -e "\e[34mAnother shell is installing or updating packages; don't be surprised if things behave oddly in the meantime.\e[0m"
    fi
fi
if [ -z "${DOTFILES_NO_ALIASES}" ]; then
    # shellcheck source=/dev/null
    source "${DOTFILES_PACKAGE_SCRIPTS}/load-packages-aliases.bash"
fi
[ -n "${DOTFILES_PROFILING}" ] && printf 'packages ' && date --rfc-3339=ns

if [ -n "${DOTFILES_STARTED_SSH_AGENT}" ]; then
    alias ssh='dotfiles-ssh-auto-add'
fi

# Use the ~/.dotfiles-aliases.{sh|bash|zsh} files for stuff that should not be
# version controlled.
function localaliases() {
    if [ -f "${DOTFILES_LOCAL_ALIASES}.${1}" ]; then
        # shellcheck source=/dev/null
        source "${DOTFILES_LOCAL_ALIASES}.${1}"
    fi
}

if [ -z "${DOTFILES_NO_ALIASES}" ]; then
    localaliases 'sh'
    localaliases "${DOTFILES_SHELL}"
fi

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

[ -n "${DOTFILES_PROFILING}" ] && printf 'generic ' && date --rfc-3339=ns
