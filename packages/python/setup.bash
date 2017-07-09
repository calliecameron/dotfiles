function _install() {
    function do-pip2() {
        if os linux; then
            sudo -H python -m pip install --upgrade pip &&
            sudo -H pip install "${@}" || return 1
        elif os cygwin; then
            printf '\033[34m'
            cat <<EOF
Pip in Cygwin must be run as administrator, and cannot be done
automatically. Open a Cygwin terminal using 'Run as Administrator' and
do the following:

python -m pip install ${@}
EOF
            printf '\033[0m\n'
        fi
    }

    function do-pip3() {
        if os linux; then
            sudo -H python3 -m pip install --upgrade pip &&
            sudo -H pip3 install "${@}" || return 1
        elif os cygwin; then
            printf '\033[34m'
            cat <<EOF
Pip in Cygwin must be run as administrator, and cannot be done
automatically. Open a Cygwin terminal using 'Run as Administrator' and
do the following:

python3 -m pip install ${@}
EOF
            printf '\033[0m\n'
        fi
    }

    if (os linux && ! linux-variant pi && can-sudo) ||
       os cygwin; then
        do-pip2 ipython &&
        do-pip3 jedi flake8 importmagic autopep8 yapf rope pygments ipython virtualenv virtualenvwrapper autoenv || exit 1
    fi
    unset -f do-pip2 do-pip3
    return 0
}

function _update() {
    _install
}
