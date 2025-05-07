# shellcheck shell=bash

if [ -n "${BASH_VERSION}" ]; then
    export LP_PS1_POSTFIX="bash
\$ "
fi

# Stop it messing up the bash prompt command
if [ -n "${PROMPT_COMMAND}" ]; then
    SAVED_PROMPT_COMMAND="${PROMPT_COMMAND}"
fi

export LP_ENV_VARS=("DOTFILES_PYENV_VERSION py:%s" "DOTFILES_NVM_VERSION nd:%s")

source "${PACKAGE_INSTALL_DIR}/liquidprompt/liquidprompt"

if [ -n "${SAVED_PROMPT_COMMAND}" ]; then
    PROMPT_COMMAND="${PROMPT_COMMAND}; ${SAVED_PROMPT_COMMAND}"
    unset SAVED_PROMPT_COMMAND
fi
