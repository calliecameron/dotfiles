#!/bin/bash
# Create shortcuts for the hibernator on the desktop

THIS_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"

DESKTOP="${HOME}/Desktop"
SLEEP="${DESKTOP}/Sleep.desktop"
HIBERNATE="${DESKTOP}/Hibernate.desktop"
SHUTDOWN="${DESKTOP}/Shut Down.desktop"


# Args: file, name, command, icon, comment
function doshortcut() {
    sed "s|@@@@@1|${2}|g" < "${THIS_DIR}/launcher-template" | sed "s|@@@@@2|${3}|g" | sed "s|@@@@@3|${THIS_DIR}/${4}.png|g" | sed "s|@@@@@4|${5}|g" > "${1}" &&
    chmod ugo+x "${1}"
}


doshortcut "${SLEEP}" "Sleep" "dotfiles-sleep" "S" "Puts the computer into sleep mode" &&
doshortcut "${HIBERNATE}" "Hibernate" "dotfiles-hibernate" "H" "Puts the computer into hibernate mode" &&
doshortcut "${SHUTDOWN}" "Shut Down" "dotfiles-shutdown" "S" "Shuts the computer down immediately"
