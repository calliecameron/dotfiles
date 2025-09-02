if [ -z "${PROMPT_COMMAND}" ]; then
    PROMPT_COMMAND='dotfiles-python-version-check'
else
    PROMPT_COMMAND="${PROMPT_COMMAND}; dotfiles-python-version-check"
fi
