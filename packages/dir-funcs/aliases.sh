# shellcheck shell=bash

function cn() {
    # Move into newly created directories
    if [ -n "${1}" ]; then
        command mkdir -p "${1}"
        cd "${1}" || return 1
    else
        cd || return 1
    fi
}

alias mkdir='mkdir -p'

function mv-cp-arg-parse() {
    # Remember where the most recent cp or mv sent things, so we can follow them
    local TARGET_DIR=''
    local NEXT_ARG_IS_TARGET=''
    local HAS_DASH_BIG_T=''
    for arg in "${@}"; do
        if [ -n "${NEXT_ARG_IS_TARGET}" ]; then
            TARGET_DIR="$(readlink -f "${arg}")"
            break
        elif [ "${arg}" = '-t' ]; then
            NEXT_ARG_IS_TARGET='t'
        elif [ "${arg}" = '-T' ]; then
            HAS_DASH_BIG_T='t'
        fi
    done

    if [ -z "${TARGET_DIR}" ] && [ -n "${2}" ]; then
        # Last arg is the target; just need to see if it is a dir or not
        local LAST_ARG=${*: -1}
        if [ -n "${HAS_DASH_BIG_T}" ]; then
            TARGET_DIR="$(readlink -f "$(dirname "$(readlink -f "${LAST_ARG}")")")"
        elif [ -d "${LAST_ARG}" ]; then
            TARGET_DIR="$(readlink -f "${LAST_ARG}")"
        else
            TARGET_DIR="$(readlink -f "$(dirname "$(readlink -f "${LAST_ARG}")")")"
        fi
    fi

    if [ -n "${TARGET_DIR}" ]; then
        echo "${TARGET_DIR}" >"${DOTFILES_FOLLOW}"
    fi
}

function mv-wrapper() {
    mv-cp-arg-parse "${@}"
    command mv "${@}"
}

function cp-wrapper() {
    mv-cp-arg-parse "${@}"
    command cp "${@}"
}

function follow() {
    if [ -f "${DOTFILES_FOLLOW}" ]; then
        cd "$(cat "${DOTFILES_FOLLOW}")" || return 1
    fi
}

alias cp='cp-wrapper -ip'
alias mv='mv-wrapper -i'
alias f='follow'

function process-cdl-arg() {
    if [ -z "${1}" ]; then
        echo '1'
        return 0
    elif [ -n "${1}" ] && env printf '%d' "${1}" &>/dev/null; then
        if [ "${1}" -lt '0' ]; then
            return 1
        else
            local n="${1}"
            ((n += 1))
            echo "${n}"
            return 0
        fi
    fi

    return 1
}

function cds() {
    local LINENUM
    local TARGET
    local LINECOUNT
    LINENUM="$(process-cdl-arg "${1}")"

    if [ -z "${LINENUM}" ]; then
        echo 'Usage: cds [slot=0] [path=.]'
        return 1
    fi

    if [ -n "${2}" ]; then
        TARGET="$(readlink -f "${2}")"
    else
        TARGET="$(pwd)"
    fi

    touch "${DOTFILES_CDS_CDL}"

    if [ "${LINENUM}" -gt "$(wc -l <"${DOTFILES_CDS_CDL}")" ]; then
        LINECOUNT="$(wc -l <"${DOTFILES_CDS_CDL}")"
        local i="$((LINENUM - LINECOUNT - 1))"

        while [ "${i}" -gt '0' ]; do
            echo >>"${DOTFILES_CDS_CDL}"
            ((i -= 1))
        done

        echo "${TARGET}" >>"${DOTFILES_CDS_CDL}"
    else
        sed -i "${LINENUM}c ${TARGET}" "${DOTFILES_CDS_CDL}"
    fi
}

function cdl() {
    local LINENUM
    local RESULT
    LINENUM="$(process-cdl-arg "${1}")"

    if [ ! -f "${DOTFILES_CDS_CDL}" ] || [ -z "${LINENUM}" ] ||
        [ "${LINENUM}" -gt "$(wc -l <"${DOTFILES_CDS_CDL}")" ]; then
        echo 'No path saved.'
        return 1
    else
        RESULT="$(sed -n "${LINENUM}p" "${DOTFILES_CDS_CDL}")"

        if [ -z "${RESULT}" ]; then
            echo 'No path saved.'
            return 1
        else
            cd "${RESULT}" || return 1
        fi
    fi
}

function cdc() {
    local LINENUM
    local RESULT
    LINENUM="$(process-cdl-arg "${1}")"

    if [ ! -f "${DOTFILES_CDS_CDL}" ] || [ -z "${LINENUM}" ] ||
        [ "${LINENUM}" -gt "$(wc -l <"${DOTFILES_CDS_CDL}")" ]; then
        echo 'No path saved.'
        return 1
    else
        RESULT="$(sed -n "${LINENUM}p" "${DOTFILES_CDS_CDL}")"

        if [ -z "${RESULT}" ]; then
            echo 'No path saved.'
            return 1
        else
            echo "${RESULT}"
        fi
    fi
}

function cdcl() {
    if [ -f "${DOTFILES_CDS_CDL}" ]; then
        rm "${DOTFILES_CDS_CDL}"
    fi
}

function cpc() {
    local USAGE='Usage: cpc slot files...'

    if [ -z "${1}" ]; then
        echo "${USAGE}"
        return 1
    fi

    local TARGDIR
    local CDC_EXIT
    TARGDIR="$(cdc "${1}")"
    CDC_EXIT=$?

    if [ "${CDC_EXIT}" != '0' ]; then
        echo "${USAGE}"
        return "${CDC_EXIT}"
    fi

    if [ -z "${2}" ]; then
        echo "${USAGE}"
        return 1
    fi

    command cp -ai -t "${TARGDIR}" "${@:2}"
}

function mvc() {
    local USAGE='Usage: mvc slot files...'

    if [ -z "${1}" ]; then
        echo "${USAGE}"
        return 1
    fi

    local TARGDIR
    local CDC_EXIT
    TARGDIR="$(cdc "${1}")"
    CDC_EXIT=$?

    if [ "${CDC_EXIT}" != '0' ]; then
        echo "${USAGE}"
        return "${CDC_EXIT}"
    fi

    if [ -z "${2}" ]; then
        echo "${USAGE}"
        return 1
    fi

    command mv -i -t "${TARGDIR}" "${@:2}"
}
