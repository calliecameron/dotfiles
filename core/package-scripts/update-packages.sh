#!/bin/bash

source "${DOTFILES_PACKAGE_SCRIPTS}/update-package.bash" || exit 1

function fail() {
    dotfiles-echo-red 'Updating packages failed.'
    exit 1
}

UPDATE='0'

if [ -e "${DOTFILES_PACKAGE_INSTALL_DIR}" ]; then
    if [ -e "${UPDATE_FILE}" ]; then
        LAST_UPDATE="$(cat "${UPDATE_FILE}")" &&
        NOW="$(date '+%s')" &&
        DIFF=$(( NOW - LAST_UPDATE )) &&
        TARGET='2419200' || exit 1 # Seconds in four weeks

        if [ "${DIFF}" -ge "${TARGET}" ]; then
            if dotfiles-yn-y "It's been a while since packages were checked for updates. Check now?"; then
                UPDATE='1'
            fi
        fi
    else
        # No update file means we just installed; don't update, initialise things
        UPDATE='0'
        date '+%s' > "${UPDATE_FILE}"
    fi

    if [ "${UPDATE}" = '1' ]; then
        update-all-packages || fail
    fi
fi
