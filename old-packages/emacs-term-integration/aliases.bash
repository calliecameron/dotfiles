if [ -z "${PROMPT_COMMAND}" ]; then
    PROMPT_COMMAND='emacs-dir-tracking'
else
    PROMPT_COMMAND="${PROMPT_COMMAND}; emacs-dir-tracking"
fi

if [ -f "${HOME}/.emacs.d/term-alert/setup.bash" ]; then
    source "${HOME}/.emacs.d/term-alert/setup.bash"
else
    source "${PACKAGE_INSTALL_DIR}/term-alert/setup/setup.bash"
fi
