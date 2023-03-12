if [ -z "${DOTFILES_DIR}" ]; then
    export DOTFILES_DIR='@@@@@'
fi

VARIABLES_FILE="${DOTFILES_DIR}/core/environment-variables.sh"
if [ -z "${DOTFILES_ENV_LOADED}" ]; then
    if [ -f "${VARIABLES_FILE}" ]; then
        # shellcheck source=/dev/null
        . "${VARIABLES_FILE}"
    else
        printf "\e[31mCan't find dotfiles.\e[0m\n"
    fi
fi
