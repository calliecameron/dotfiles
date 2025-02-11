# shellcheck shell=bash

complainunset 'DOTFILES_VC_NAME' 'Version control name'
complainunset 'DOTFILES_VC_EMAIL' 'Version control email'
complainunset 'DOTFILES_BITBUCKET_USERNAME' 'Bitbucket username'
complainunset 'DOTFILES_GITHUB_USERNAME' 'GitHub username'

alias g='git'
alias s='git status'
alias a='git add'
alias c='git commit -m'
alias p='git push'
alias pl='git pull'
alias lg='git log'
alias gd='LESS=FRX git diff'
alias gdw='LESS=FRX git diff --word-diff'
