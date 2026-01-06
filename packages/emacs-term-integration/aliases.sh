# shellcheck shell=bash

# If you SSH into a host where 'hostname -f' doesn't give a fully qualified
# domain name (either because it isn't set in /etc/hostname (as on Ubuntu), or
# because it *doesn't have* a FQDN (as on a LAN), use the IP address instead.
# (Note that this isn't foolproof: if someone puts a dot in the hostname (is
# that even allowed?) it will get confused.)
if [[ "$(hostname -f)" != *.* ]] && [ -n "${SSH_CONNECTION}" ]; then
    EMACS_PROMPT_HOSTNAME="$(echo "${SSH_CONNECTION}" | awk '{print $3}')"
else
    EMACS_PROMPT_HOSTNAME="$(hostname)"
fi

export EMACS_PROMPT_HOSTNAME

function emacs-dir-tracking() {
    command emacs-dir-tracking
}

export PAGER='emacs-pager-wrapper'
alias less='emacs-pager-wrapper'

if [ -n "${EAT_SHELL_INTEGRATION_DIR}" ]; then
    DOTFILES_EAT_DIR="$(readlink -f "${EAT_SHELL_INTEGRATION_DIR}/..")"
else
    DOTFILES_EAT_DIR="${PACKAGE_INSTALL_DIR}/eat"
fi

export DOTFILES_EAT_DIR
export DOTFILES_EAT_INTEGRATION_DIR="${DOTFILES_EAT_DIR}/integration"
export DOTFILES_EAT_TERMINFO_DIR="${DOTFILES_EAT_DIR}/terminfo"

if [ "${TERM:0:4}" = "eat-" ] && [ -z "${TERMINFO}" ]; then
    export TERMINFO="${DOTFILES_EAT_TERMINFO_DIR}"
fi
