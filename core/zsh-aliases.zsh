# -*- Shell-script -*-
#
# Custom aliases and other things that should only be visible to an
# interactive shell go in here.
#

test ! -z "${DOTFILES_PROFILING}" && printf 'zsh ' && date --rfc-3339=ns

export DOTFILES_SHELL='zsh'
# Might not have loaded environment variables yet, so have to do this with the full path
source "${DOTFILES_DIR}/core/generic-aliases.bash"

if [ ! -z "${DOTFILES_NO_ALIASES}" ]; then
    # Disable all fancy stuff to avoid confusing TRAMP
    unsetopt zle
    unsetopt prompt_cr
    unsetopt prompt_subst
    if whence -w precmd >/dev/null; then
        unfunction precmd
    fi
    if whence -w preexec >/dev/null; then
        unfunction preexec
    fi
    PS1='$ '
elif [ -z "${DOTFILES_ZSH_NO_DEFAULT_PROMPT}" ]; then
PROMPT="%n@%m:%~
\$ "
fi

# Stop cd suggesting usernames
zstyle ':completion:*' users
zstyle ':completion:*' use-cache on

autoload -U compinit
compinit

test ! -z "${DOTFILES_PROFILING}" && printf 'zsh ' && date --rfc-3339=ns; true
