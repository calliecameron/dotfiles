#!/bin/bash
# Setup the basic dotfiles system, assuming everything is in the
# core/default-dotfiles folder.  Run this first, then log out and log
# in again.

DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
TEMP_DOTFILES_DIR="$(readlink -f "${DIR}")"
SRC_DIR="${TEMP_DOTFILES_DIR}/core/default-dotfiles"

function dofile() {
    if [ ! -e "${1}" ]; then
        echo -e "\e[31mError: ${1} does not exist.\e[0m"
        exit 1
    fi

    local TMPFILE="${1}.processed"
    sed "s|@@@@@|${TEMP_DOTFILES_DIR}|g" <"${1}" >"${TMPFILE}"
    bash "${TEMP_DOTFILES_DIR}/core/bin/compare-and-replace" "${TMPFILE}" "${2}" || exit 1
}

dofile "${SRC_DIR}/default-profile.sh" "${HOME}/.profile" &&
dofile "${SRC_DIR}/default-bash-profile.bash" "${HOME}/.bash_profile" &&
dofile "${SRC_DIR}/default-bash-login.bash" "${HOME}/.bash_login" &&
dofile "${SRC_DIR}/default-bash-logout.bash" "${HOME}/.bash_logout" &&
dofile "${SRC_DIR}/default-bashrc.bash" "${HOME}/.bashrc" &&
dofile "${SRC_DIR}/default-zshenv.zsh" "${HOME}/.zshenv" &&
dofile "${SRC_DIR}/default-zprofile.zsh" "${HOME}/.zprofile" &&
dofile "${SRC_DIR}/default-zlogin.zsh" "${HOME}/.zlogin" &&
dofile "${SRC_DIR}/default-zlogout.zsh" "${HOME}/.zlogout" &&
dofile "${SRC_DIR}/default-zshrc.zsh" "${HOME}/.zshrc" &&
dofile "${SRC_DIR}/default-emacs.el" "${HOME}/.emacs" &&

echo -e "\e[33mLog out and log in again to set everything up correctly.\e[0m"
