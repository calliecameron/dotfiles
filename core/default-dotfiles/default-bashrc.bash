# -*- Shell-script -*-

if [ -z "${DOTFILES_DIR}" ]; then
   export DOTFILES_DIR='@@@@@'
fi

# Uncomment the following line to enable profiling at startup
# DOTFILES_PROFILING='true'

ALIASES_FILE="${DOTFILES_DIR}/core/bash-aliases.bash"

if [ -f "${ALIASES_FILE}" ]; then
    source "${ALIASES_FILE}"
else
    echo -e "\e[31mCan't find dotfiles.\e[0m"
fi
