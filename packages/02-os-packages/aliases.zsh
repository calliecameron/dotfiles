if [ "${DOTFILES_OS}" = 'linux' ]; then
    alias u='sudo apt-get update && sudo apt-get upgrade && sudo apt-get dist-upgrade && hash -f'
    function i() {
        sudo apt-get install "${@}" &&
        hash -f
    }
fi
