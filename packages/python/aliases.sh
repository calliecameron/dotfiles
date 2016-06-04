function python-wrapper2() {
    if [ -z "${1}" ] && type ipython2 &>/dev/null; then
        ipython2 --no-confirm-exit
    else
        python2 "${@}"
    fi
}

function python-wrapper3() {
    if [ -z "${1}" ] && type ipython3 &>/dev/null; then
        ipython3 --no-confirm-exit
    else
        python3 "${@}"
    fi
}

function python-wrapper() {
    if [ -z "${VIRTUAL_ENV}" ]; then
        python-wrapper3 "${@}"
    else
        if [ -z "${1}" ] && type ipython &>/dev/null; then
            ipython --no-confirm-exit
        else
            python "${@}"
        fi
    fi
}

alias py2='python-wrapper2'
alias py3='python-wrapper3'
alias py='python-wrapper'

if which virtualenvwrapper.sh &>/dev/null; then
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

if which activate.sh &>/dev/null; then
    source activate.sh
fi

if [ ! -z "${VIRTUAL_ENV}" ] &&
   ! type deactivate &>/dev/null &&
   [ -f "${VIRTUAL_ENV}/bin/activate" ]; then
    # We have probably been started from Emacs with a virtualenv
    # set. To avoid confusion, activate the same virtualenv in this
    # shell.
    source "${VIRTUAL_ENV}/bin/activate"
fi
