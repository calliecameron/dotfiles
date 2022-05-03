# Custom aliases and other things that should only be visible to an interactive
# shell go in here.

# If not running interactively, don't do anything
[ -z "${PS1}" ] && return

# If you can't change the default shell, but always want to use zsh
# interactively, set this variable. Ugly, but sometimes necessary.
if [ -n "${DOTFILES_FORCE_ZSH}" ]; then
    exec zsh
fi

[ -n "${DOTFILES_PROFILING}" ] && printf 'bash ' && date --rfc-3339=ns

export DOTFILES_SHELL='bash'
# Might not have loaded environment variables yet, so have to do this with the
# full path
source "${DOTFILES_DIR}/core/generic-aliases.bash"

HISTCONTROL='ignoredups:ignorespace'
HISTSIZE=1000
HISTFILESIZE=2000

shopt -s histappend
shopt -s checkwinsize

if [ -f /etc/bash_completion ] && ! shopt -oq posix; then
    source /etc/bash_completion
fi

PS1='\u@\h:\w\n\$ '

# If this is an xterm set the title to user@host:dir
case "${TERM}" in
xterm* | rxvt*)
    PS1="\[\e]0;\u@\h: \w\a\]${PS1}"
    ;;
*) ;;

esac

[ -n "${DOTFILES_PROFILING}" ] && printf 'bash ' && date --rfc-3339=ns

true
