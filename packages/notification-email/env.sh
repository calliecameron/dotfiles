export DOTFILES_SSMTP_CONFIG="${HOME}/.dotfiles-ssmtp-config"

if [ ! -e "${DOTFILES_SSMTP_CONFIG}" ]; then
    cp "${PACKAGE_SOURCE_DIR}/ssmtp-conf-template" "${DOTFILES_SSMTP_CONFIG}"
fi
