# shellcheck shell=sh

export DOTFILES_GVM_GO_VERSION='go1.20'

if [ -f "${HOME}/.gvm/scripts/gvm" ]; then
    . "${HOME}/.gvm/scripts/gvm"
    if gvm list | grep "${DOTFILES_GVM_GO_VERSION}" >/dev/null 2>/dev/null; then
        gvm use "${DOTFILES_GVM_GO_VERSION}"
    fi
fi
