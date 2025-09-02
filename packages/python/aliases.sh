# shellcheck shell=bash

function python-wrapper() {
    if [ -n "${DOTFILES_UV_VERSION}" ]; then
        if [ -z "${1}" ]; then
            uv run --with ipython ipython --no-confirm-exit
        else
            uv run python "${@}"
        fi
    else
        if [ -z "${1}" ] && type ipython3 &>/dev/null; then
            ipython3 --no-confirm-exit
        else
            python3 "${@}"
        fi
    fi
}

alias py='python-wrapper'

if type pip &>/dev/null; then
    eval "$(pip completion "--${DOTFILES_SHELL}")"
fi

if [ -n "${VIRTUAL_ENV}" ] &&
    ! type deactivate &>/dev/null &&
    [ -f "${VIRTUAL_ENV}/bin/activate" ]; then
    # We have probably been started from Emacs with a virtualenv set. To avoid
    # confusion, activate the same virtualenv in this shell.
    source "${VIRTUAL_ENV}/bin/activate"
fi

eval "$(pyenv init - "${DOTFILES_SHELL}")"
pyenv virtualenvwrapper

if [ "${DOTFILES_SHELL}" != 'zsh' ]; then
    eval "$(uv generate-shell-completion "${DOTFILES_SHELL}")"
    eval "$(uvx --generate-shell-completion "${DOTFILES_SHELL}")"
fi

function dotfiles-python-version-check() {
    DOTFILES_PYENV_VERSION="$(pyenv version-name)"
    unset DOTFILES_UV_VERSION
    local UV_LOCK
    UV_LOCK="$(dirname "$(pyenv version-file)")/uv.lock"
    if [ "${DOTFILES_PYENV_VERSION}" = 'system' ]; then
        unset DOTFILES_PYENV_VERSION
        unset DOTFILES_UV_VERSION
    elif [ -e "${UV_LOCK}" ]; then
        DOTFILES_UV_VERSION="${DOTFILES_PYENV_VERSION}"
        unset DOTFILES_PYENV_VERSION
    fi
}

dotfiles-python-version-check
