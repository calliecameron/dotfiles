if [ -z "${PROMPT_COMMAND}" ]; then
    PROMPT_COMMAND='dotfiles-pyenv-version-check'
else
    PROMPT_COMMAND="${PROMPT_COMMAND}; dotfiles-pyenv-version-check"
fi
