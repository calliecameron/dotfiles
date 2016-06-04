# Stop it messing up the bash prompt command
if [ ! -z "${PROMPT_COMMAND}" ]; then
    SAVED_PROMPT_COMMAND="${PROMPT_COMMAND}"
fi

source "${PACKAGE_INSTALL_DIR}/liquidprompt"

if [ ! -z "${SAVED_PROMPT_COMMAND}" ]; then
    PROMPT_COMMAND="${PROMPT_COMMAND}; ${SAVED_PROMPT_COMMAND}"
    unset SAVED_PROMPT_COMMAND
fi
