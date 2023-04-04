if [ -z "${PROMPT_COMMAND}" ]; then
    PROMPT_COMMAND='vc-alias-check'
else
    PROMPT_COMMAND="${PROMPT_COMMAND}; vc-alias-check"
fi
