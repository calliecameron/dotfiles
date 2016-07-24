#!/bin/bash

source "${DOTFILES_PACKAGE_SCRIPTS}/update-package.bash" || exit 1

function fail() {
    if [ -z "${1}" ]; then
        echo-red 'Updating packages failed.'
    else
        echo-red "${1}"
    fi
    exit 1
}

UPDATE='0'

if [ -e "${DOTFILES_PACKAGE_INSTALL_DIR}" ]; then
    if [ -e "${UPDATE_FILE}" ]; then
        LAST_UPDATE="$(cat "${UPDATE_FILE}")" &&
        NOW="$(date '+%s')" &&
        DIFF=$(( NOW - LAST_UPDATE )) &&
        TARGET='1209600' || exit 1 # Seconds in two weeks

        if [ "${DIFF}" -ge "${TARGET}" ]; then
            if yn-y "It's been a while since packages were checked for updates. Check now?"; then
                UPDATE='1'
            fi
        fi
    else
        UPDATE='1'
    fi

    if [ "${UPDATE}" = '1' ]; then
        if [ -d "${DOTFILES_PRIVATE_DIR}" ]; then
            if dotfiles-repo-is-clean "${DOTFILES_PRIVATE_DIR}"; then
                # shellcheck disable=SC2015
                cd "${DOTFILES_PRIVATE_DIR}" &&
                git pull || fail 'Failed to update private repo.'
            else
                echo-yellow 'Private repo has uncommitted changes; not updating it.'
            fi
        fi

        touch "${UPDATED_ALREADY_FILE}" || exit 1

        while (($#)); do
            test -z "${1}" && fail 'Could not update packages: no configuration root specified.'
            PACKAGE_CONF_ROOT="${1}"

            if [ -d "${PACKAGE_CONF_ROOT}" ]; then
                TEMPFILE="$(mktemp)" &&
                ls -1 "${PACKAGE_CONF_ROOT}" > "${TEMPFILE}" &&
                exec 3< "${TEMPFILE}" || exit 1
                while read -r -u 3 line; do
                    update-package "${line}" || exit 1
                done
                rm "${TEMPFILE}"
            fi

            shift
        done

        date '+%s' > "${UPDATE_FILE}"

        rm "${UPDATED_ALREADY_FILE}"
    fi
fi
