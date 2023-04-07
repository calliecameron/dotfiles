# shellcheck shell=bash

function python-wrapper() {
    if [ -z "${1}" ] && type ipython3 &>/dev/null; then
        ipython3 --no-confirm-exit
    else
        python3 "${@}"
    fi
}

alias py='python-wrapper'

if type virtualenvwrapper.sh &>/dev/null; then
    source virtualenvwrapper.sh

    function workon-wrapper() {
        if [ "${VIRTUAL_ENV}" != "${WORKON_HOME}/${1}" ]; then
            workon "${1}"
        fi
    }
else
    function workon-wrapper() {
        true
    }
fi

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

alias j='jupyter notebook'
