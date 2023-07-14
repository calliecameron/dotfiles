# shellcheck shell=bash

function dotfiles-cd-new() {
    # Move into newly created directories
    if [ -n "${1}" ]; then
        command mkdir -p "${1}"
        cd "${1}" || return 1
    else
        cd || return 1
    fi
}

function dotfiles-follow() {
    if [ -f "${DOTFILES_MV_CP_FOLLOW}" ]; then
        cd "$(cat "${DOTFILES_MV_CP_FOLLOW}")" || return 1
    fi
}

function dotfiles-load-saved-path() {
    local SLOT='0'
    if [ -n "${1}" ]; then
        SLOT="${1}"
    fi

    if [[ "${SLOT}" == *'/'* ]]; then
        echo "Usage: dotfiles-load-saved-path [slot=0]"
        return 1
    fi

    local TARGET_DIR SHOW_EXIT
    TARGET_DIR="$(dotfiles-saved-path "${SLOT}")"
    SHOW_EXIT=$?

    if [ "${SHOW_EXIT}" != '0' ]; then
        echo 'No save path.'
        return "${SHOW_EXIT}"
    fi

    cd "${TARGET_DIR}" || return 1
}

alias mkdir='mkdir -p'
alias cn='dotfiles-cd-new'
alias cp='dotfiles-cp -ip'
alias mv='dotfiles-mv -i'
alias f='dotfiles-follow'
alias cds='dotfiles-save-path'
alias cdl='dotfiles-load-saved-path'
alias cdc='dotfiles-saved-path'
alias cdcl='dotfiles-clear-saved-paths'
alias cpc='dotfiles-copy-to-saved-path'
alias mvc='dotfiles-move-to-saved-path'
