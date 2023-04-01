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

if [ ! -z "${DOTFILES_PRIVATE_REPO}" ] && [ ! -d "${DOTFILES_PRIVATE_DIR}" ]; then
    if dotfiles-yn-y 'A private repo has been specified; clone it?'; then
        if git clone "${DOTFILES_PRIVATE_REPO}" "${DOTFILES_PRIVATE_DIR}"; then
            if [ ! -z "${DOTFILES_PRIVATE_BRANCH}" ]; then
                cd "${DOTFILES_PRIVATE_DIR}" &&
                # shellcheck disable=SC2015
                git checkout "${DOTFILES_PRIVATE_BRANCH}" || fail 'Could not check out private branch'
            fi
        else
            fail 'Could not clone private repo.'
        fi
    fi
fi

function install-package-root() {
    test -z "${1}" && fail 'Could not install packages: no configuration root specified.'
    PACKAGE_CONF_ROOT="${1}"

    if [ -d "${PACKAGE_CONF_ROOT}" ]; then
        TEMPFILE="$(mktemp)" &&
        ls -1 "${PACKAGE_CONF_ROOT}" > "${TEMPFILE}" &&

        # shellcheck disable=SC2015
        exec 3< "${TEMPFILE}" || fail

        while read -r -u 3 line; do
            package-setup-vars "${line}"
            if [ -e "${PACKAGE_SETUP_FILE}" ] && ! dotfiles-package-ignored "${PACKAGE_NAME}" && ! package-installed; then
                cd "${PACKAGE_CONF_DIR}" || fail "Could not find configuration directory for '${PACKAGE_NAME}'; quitting."
                source "${PACKAGE_SETUP_FILE}" || fail "Could not load package setup file for '${PACKAGE_NAME}'; quitting."

                if ! type _can-install &>/dev/null || _can-install; then
                    if dotfiles-yn-y "Package '${PACKAGE_NAME}' is missing. Install it?"; then
                        mkdir -p "${DOTFILES_PACKAGE_INSTALL_DIR}" &&
                        # shellcheck disable=SC2015
                        cd "${DOTFILES_PACKAGE_INSTALL_DIR}" || fail "Could not find main package installation directory; quitting."

                        if [ ! -z "${OFFER_GIT_SSH}" ]; then
                            if dotfiles-yn-y "Use SSH for git clone?"; then
                                # shellcheck disable=SC2034
                                USE_GIT_SSH='true'
                            fi
                        fi
                        _install &&
                        touch "${PACKAGE_INSTALLED_FILE}" &&
                        # shellcheck disable=SC2015
                        touch "${DOTFILES_NEEDS_LOGOUT}" || fail "Package '${PACKAGE_NAME}' failed to install; quitting."
                    else
                        if dotfiles-yn-n "Remember decision?"; then
                            ignore "${PACKAGE_NAME}" || fail 'Could not ignore package.'
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
