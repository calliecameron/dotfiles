#!/bin/bash

source "${DOTFILES_BASH_COMMON}" || exit 1

if yn-n "Open firewall port for Avahi daemon (local name resolution) (UDP port 5353)?"; then
    port open-at-boot 5353 udp &&
    port open 5353 udp || exit 1
else
    port dont-open-at-boot 5353 udp || exit 1
fi

exit 0
