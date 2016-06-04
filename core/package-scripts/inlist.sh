#!/bin/sh
# Check if an item is in a colon delimited list (PATH-style)
# Args: list item

LIST="${1}"
ITEM="${2}"
IFS=':'

for i in $LIST; do
    if [ "${i}" = "${ITEM}" ]; then
        exit 0
    fi
done
exit 1
