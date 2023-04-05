# shellcheck shell=bash

complainunset 'DOTFILES_VC_NAME' 'Version control name'
complainunset 'DOTFILES_VC_EMAIL' 'Version control email'
complainunset 'DOTFILES_BITBUCKET_USERNAME' 'Bitbucket username'
complainunset 'DOTFILES_GITHUB_USERNAME' 'GitHub username'

alias g='git'
alias h='hg'

DOTFILES_REPO_ALIASES=''

function setup-git-aliases() {
    if [ "${DOTFILES_REPO_ALIASES}" != 'git' ]; then
        alias s='git status'
        alias a='git add'
        alias c='git commit -m'
        alias p='git push'
        alias pl='git pull'
        alias lg='git log'
        alias gd='LESS=FRX git diff'
        alias gdw='LESS=FRX git diff --word-diff'
        DOTFILES_REPO_ALIASES='git'
    fi
}

function setup-hg-aliases() {
    if [ "${DOTFILES_REPO_ALIASES}" != 'hg' ]; then
        alias s='hg status'
        alias a='hg add'
        alias c='hg commit -m'
        alias p='hg push'
        alias pl='hg pull'
        alias lg='hg log'
        alias gd='LESS=FRX hg diff'
        alias gdw='LESS=FRX hg wdiff'
        DOTFILES_REPO_ALIASES='hg'
    fi
}

function repo-type() {
    local DIR
    DIR="$(readlink -f .)"

    while true; do
        if [ -d "${DIR}/.git" ]; then
            echo 'git'
            return 0
        elif [ -d "${DIR}/.hg" ]; then
            echo 'hg'
            return 0
        elif [ "${DIR}" = '/' ]; then
            echo 'none'
            return 1
        else
            DIR="$(readlink -f "${DIR}/..")"
        fi
    done
}

function vc-alias-check() {
    case "$(repo-type)" in
    'hg') setup-hg-aliases ;;
    *) setup-git-aliases ;;
    esac
}

vc-alias-check
