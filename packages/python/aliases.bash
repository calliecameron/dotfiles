if [ -z "${PROMPT_COMMAND}" ]; then
    PROMPT_COMMAND='pyenv-version-check'
else
    PROMPT_COMMAND="${PROMPT_COMMAND}; pyenv-version-check"
fi
