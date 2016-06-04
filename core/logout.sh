# -*- Shell-script -*-

if [ ! -z "${DOTFILES_STARTED_SSH_AGENT}" ] && [ ! -z "${SSH_AGENT_PID}" ]; then
    ssh-agent -k >/dev/null 2>/dev/null
fi

if [ ! -z "${DOTFILES_SSH_ADDED_FILE}" ]; then
    command rm -f "${DOTFILES_SSH_ADDED_FILE}"
fi
