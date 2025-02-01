# shellcheck shell=sh

if [ -n "${DOTFILES_REDSHIFT_LAT}" ] && [ -n "${DOTFILES_REDSHIFT_LON}" ]; then
    sed "s|@@@@@1@@@@@|${DOTFILES_REDSHIFT_LAT}|g" <"${PACKAGE_SOURCE_DIR}/redshift-conf-template" |
        sed "s|@@@@@2@@@@@|${DOTFILES_REDSHIFT_LON}|g" >"${HOME}/.config/redshift.conf"
fi
