#!/bin/bash

source "${DOTFILES_PACKAGE_SCRIPTS}/setup-common.bash" &&
source "${DOTFILES_PACKAGE_SCRIPTS}/common-funcs.sh" || exit 1

function fail() {
    if [ -z "${1}" ]; then
        dotfiles-echo-red 'Installing packages failed.'
    else
        dotfiles-echo-red "${1}"
    fi
    exit 1
}

function install-package-root() {
    test -z "${1}" && fail 'Could not install packages: no configuration root specified.'
    PACKAGE_ROOT="${1}"

    if [ -d "${PACKAGE_ROOT}" ]; then
        TEMPFILE="$(mktemp)" &&
        ls -1 "${PACKAGE_ROOT}" > "${TEMPFILE}" &&

        # shellcheck disable=SC2015
        exec 3< "${TEMPFILE}" || fail

        while read -r -u 3 line; do
            package-setup-vars "${line}"
            if [ -e "${PACKAGE_SETUP_FILE}" ] && ! dotfiles-package-ignored "${PACKAGE_NAME}" && ! dotfiles-package-installed "${PACKAGE_NAME}"; then
                cd "${PACKAGE_SOURCE_DIR}" || fail "Could not find configuration directory for '${PACKAGE_NAME}'; quitting."
                source "${PACKAGE_SETUP_FILE}" || fail "Could not load package setup file for '${PACKAGE_NAME}'; quitting."

                if ! type _can-install &>/dev/null || _can-install; then
                    if dotfiles-yn-y "Package '${PACKAGE_NAME}' is missing. Install it?"; then
                        mkdir -p "${DOTFILES_PACKAGE_INSTALL_DIR}" &&
                        # shellcheck disable=SC2015
                        cd "${DOTFILES_PACKAGE_INSTALL_DIR}" || fail "Could not find main package installation directory; quitting."

                        _install &&
                        touch "${PACKAGE_INSTALLED_FILE}" &&
                        # shellcheck disable=SC2015
                        touch "${DOTFILES_NEEDS_LOGOUT}" || fail "Package '${PACKAGE_NAME}' failed to install; quitting."
                    else
                        if dotfiles-yn-n "Remember decision?"; then
                            dotfiles-package-ignore "${PACKAGE_NAME}" || fail 'Could not ignore package.'
                        fi
                    fi
                fi

                package-cleanup
            fi
        done

        rm "${TEMPFILE}"
    fi
}


packagerootloop install-package-root


if [ -e "${DOTFILES_PACKAGE_INSTALL_DIR}" ]; then
    if [ ! -e "${UPDATE_FILE}" ]; then
        date '+%s' > "${UPDATE_FILE}"
    fi
fi
