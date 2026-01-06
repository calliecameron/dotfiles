if [ -z "${PROMPT_COMMAND}" ]; then
    PROMPT_COMMAND='emacs-dir-tracking'
else
    PROMPT_COMMAND="${PROMPT_COMMAND}; emacs-dir-tracking"
fi

if [ "${TERM:0:4}" = "eat-" ] && [ -d "${DOTFILES_EAT_INTEGRATION_DIR}" ]; then
    ORIGINAL_PROMPT_COMMAND="${PROMPT_COMMAND}"
    source "${DOTFILES_EAT_INTEGRATION_DIR}/bash"
    PROMPT_COMMAND="${PROMPT_COMMAND}; ${ORIGINAL_PROMPT_COMMAND}"
    unset ORIGINAL_PROMPT_COMMAND
fi

if [ -f "${HOME}/.emacs.d/term-alert/setup.bash" ]; then
    source "${HOME}/.emacs.d/term-alert/setup.bash"
else
    source "${PACKAGE_INSTALL_DIR}/term-alert/setup/setup.bash"
fi
