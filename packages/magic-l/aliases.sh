function magic-l() {
    # Either ls or less, depending on context

    # Use the user's pager if specified, else fall back to less
    local USE_PAGER='less'
    if [ ! -z "${PAGER}" ]; then
        USE_PAGER="${PAGER}"
    fi

    if [ ! -z "${2}" ]; then
        # Two or more args; always ls
        ls "${@}"
    elif [ ! -z "${1}" ]; then
        # One arg; ls if it begins with a dash or is a directory, less otherwise
        if [[ "${1}" == -* ]] || [ -d "${1}" ]; then
            ls "${@}"
        else
            "${USE_PAGER}" "${@}"
        fi
    else
        # No args; less if something is being piped, else ls
        if tty -s; then
            ls
        else
            "${USE_PAGER}"
        fi
    fi
}

alias l='magic-l'
