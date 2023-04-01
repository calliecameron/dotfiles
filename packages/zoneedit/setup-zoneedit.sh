#!/bin/bash
# Make zoneedit run when the network goes up

THIS_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)" &&
source "${THIS_DIR}/env.sh" || exit 1

SCRIPT_SRC="${THIS_DIR}/zoneedit-at-net-up" &&
SCRIPT_DST='/etc/network/if-up.d/zoneedit-at-net-up' &&

TMPFILE="$(mktemp)" &&
cp "${SCRIPT_SRC}" "${TMPFILE}" &&
sed -i "s|@@@@@1@@@@@|${DOTFILES_ZONEEDIT_HOST}|g" "${TMPFILE}" &&
sed -i "s|@@@@@2@@@@@|${DOTFILES_ZONEEDIT_USERNAME}|g" "${TMPFILE}" &&
sed -i "s|@@@@@3@@@@@|${DOTFILES_ZONEEDIT_PASSWORD}|g" "${TMPFILE}" &&
sed -i "s|@@@@@4@@@@@|${DOTFILES_ETC_DIR}/zoneedit-last-run.log|g" "${TMPFILE}" &&
sudo mv "${TMPFILE}" "${SCRIPT_DST}" &&
sudo chown root:root "${SCRIPT_DST}" &&
sudo chmod go-rwx "${SCRIPT_DST}" &&
sudo chmod u=rx "${SCRIPT_DST}" || exit 1

if [ ! -e '/etc/cron.hourly/zoneedit-update' ]; then
    sudo ln -s "${SCRIPT_DST}" '/etc/cron.hourly/zoneedit-update' || exit 1
fi


function do-config-file() {
    if [ ! -e "${1}" ]; then
        sudo cp "${THIS_DIR}/$(basename "${1}")" "${1}" &&
        sudo chown root:root "${1}" &&
        sudo chmod go-rwx "${1}" || return 1
    fi
    return 0
}

do-config-file "${DOTFILES_ZONEEDIT_HOST}" &&
do-config-file "${DOTFILES_ZONEEDIT_USERNAME}" &&
do-config-file "${DOTFILES_ZONEEDIT_PASSWORD}" &&

dotfiles-echo-blue "ZoneEdit settings can be configured with 'zoneedit-cron-config'"
