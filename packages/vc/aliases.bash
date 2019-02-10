if os cygwin; then
    source "${PACKAGE_CONF_DIR}/git-completion.bash"
fi

if [ -z "${PROMPT_COMMAND}" ]; then
    PROMPT_COMMAND='vc-alias-check'
else
    PROMPT_COMMAND="${PROMPT_COMMAND}; vc-alias-check"
fi
