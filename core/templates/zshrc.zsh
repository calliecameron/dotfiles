if [ -z "${DOTFILES_DIR}" ]; then
    export DOTFILES_DIR='@@@@@'
fi

# Uncomment the following line to enable profiling at startup
# DOTFILES_PROFILING='true'

ALIASES_FILE="${DOTFILES_DIR}/core/zsh-aliases.zsh"

if [ -f "${ALIASES_FILE}" ]; then
    # shellcheck source=/dev/null
    source "${ALIASES_FILE}"
else
    echo -e "\e[31mCan't find dotfiles.\e[0m"
fi
