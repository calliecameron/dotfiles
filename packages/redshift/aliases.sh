# shellcheck shell=sh

if [ -z "${DOTFILES_REDSHIFT_LAT}" ] || [ -z "${DOTFILES_REDSHIFT_LON}" ]; then
    dotfiles-echo-blue 'DOTFILES_REDSHIFT_LAT and DOTFILES_REDSHIFT_LON must be set for redshift to work.'
fi
