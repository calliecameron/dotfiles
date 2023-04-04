#!/bin/bash
# Install the iptables rules to be loaded at startup

THIS_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)" &&
source "${THIS_DIR}/env.sh" || exit 1

SCRIPT_SRC="${THIS_DIR}/iptablesload"
SCRIPT_DST='/etc/network/if-pre-up.d/iptablesload'
RULES_SRC="${THIS_DIR}/iptables-rules"
RULES_DST="${DOTFILES_ETC_DIR}/iptables-rules"

TMPFILE="$(mktemp)" &&
cp "${SCRIPT_SRC}" "${TMPFILE}" &&
sed -i "s|@@@@@1@@@@@|${DOTFILES_IPTABLES_TCP_OPEN_BOOT}|g" "${TMPFILE}" &&
sed -i "s|@@@@@2@@@@@|${DOTFILES_IPTABLES_UDP_OPEN_BOOT}|g" "${TMPFILE}" &&
sed -i "s|@@@@@3@@@@@|${RULES_DST}|g" "${TMPFILE}" &&
sudo mv "${TMPFILE}" "${SCRIPT_DST}" &&
sudo chown root:root "${SCRIPT_DST}" &&
sudo chmod ugo=rx "${SCRIPT_DST}" &&

sudo mkdir -p "${DOTFILES_ETC_DIR}" &&
sudo cp "${RULES_SRC}" "${RULES_DST}"
