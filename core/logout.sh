# shellcheck shell=sh

if [ -n "${DOTFILES_STARTED_SSH_AGENT}" ] && [ -n "${SSH_AGENT_PID}" ]; then
    ssh-agent -k >/dev/null 2>/dev/null
fi

if [ -n "${DOTFILES_SSH_ADDED_FILE}" ]; then
    command rm -f "${DOTFILES_SSH_ADDED_FILE}"
fi
