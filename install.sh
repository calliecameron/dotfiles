#!/bin/bash
# Setup the basic dotfiles system, assuming everything is in the
# core/default-dotfiles folder. Run this first, then log out and log in again.

set -eu

THIS_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
SRC_DIR="${THIS_DIR}/core/default-dotfiles"
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

dofile 'default-profile.sh' '.profile'
dofile 'default-bash-profile.bash' '.bash_profile'
dofile 'default-bash-login.bash' '.bash_login'
dofile 'default-bash-logout.bash' '.bash_logout'
dofile 'default-bashrc.bash' '.bashrc'
dofile 'default-zshenv.zsh' '.zshenv'
dofile 'default-zprofile.zsh' '.zprofile'
dofile 'default-zlogin.zsh' '.zlogin'
dofile 'default-zlogout.zsh' '.zlogout'
dofile 'default-zshrc.zsh' '.zshrc'
dofile 'default-emacs.el' '.emacs'

echo -e "\e[33mLog out and log in again to set everything up correctly.\e[0m"
