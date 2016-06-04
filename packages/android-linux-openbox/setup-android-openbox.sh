#!/bin/bash

THIS_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"

function check() {
    local FILE="${HOME}/.config/openbox/${1}"
    if [ -e "${FILE}" ]; then
        if [ ! -h "${FILE}" ] || [ "$(readlink -f "${FILE}")" != "$(readlink -f "${2}")" ]; then
            echo -e "\e[31m${FILE} exists but is not a link to the shared version!\e[0m"
            return 1
        fi
    else
        ln -s "${2}" "${FILE}"
    fi
}

mkdir -p "${HOME}/.config/openbox" &&

check 'autostart' "${THIS_DIR}/openbox-autostart" &&
check 'environment' "${HOME}/.profile" &&
check 'menu.xml' "${THIS_DIR}/openbox-menu.xml" &&
check 'rc.xml' "${THIS_DIR}/openbox-rc.xml"
