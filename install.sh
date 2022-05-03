#!/bin/bash
# Setup the basic dotfiles system, assuming everything is in the core/stubs
# folder. Run this first, then log out and log in again.

set -eu

THIS_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
SRC_DIR="${THIS_DIR}/core/stubs"
PROCESSED_DIR="${HOME}/.dotfiles-processed"

function dofile() {
    local SRC="${SRC_DIR}/${1}"
    local PROCESSED="${PROCESSED_DIR}/${2}"
    local DST="${HOME}/${2}"

    if [ ! -e "${SRC}" ]; then
        echo -e "\e[31mError: ${SRC} does not exist.\e[0m"
        return 1
    fi

    sed "s|@@@@@|${THIS_DIR}|g" <"${SRC}" >"${PROCESSED}"

    if [ -e "${DST}" ]; then
        if ! cmp "${PROCESSED}" "${DST}" &>/dev/null; then
            # They're different; backup before copying
            if [ -e "${DST}.backup" ]; then
                echo "Error: ${PROCESSED} and ${DST} differ, but ${DST}.backup already exists"
                return 1
            fi
            mv "${DST}" "${DST}.backup"
        fi
    fi

    cp "${PROCESSED}" "${DST}"
}

mkdir -p "${PROCESSED_DIR}"

dofile 'profile.sh' '.profile'
dofile 'bash-profile.bash' '.bash_profile'
dofile 'bash-login.bash' '.bash_login'
dofile 'bash-logout.bash' '.bash_logout'
dofile 'bashrc.bash' '.bashrc'
dofile 'zshenv.zsh' '.zshenv'
dofile 'zprofile.zsh' '.zprofile'
dofile 'zlogin.zsh' '.zlogin'
dofile 'zlogout.zsh' '.zlogout'
dofile 'zshrc.zsh' '.zshrc'
dofile 'emacs.el' '.emacs'

echo -e "\e[33mLog out and log in again to set everything up correctly.\e[0m"
