# This script will be sourced by $DOTFILES_VARIABLES, so must work in plain old sh

. "${DOTFILES_PACKAGE_SCRIPTS}/load-package-env.sh"
. "${DOTFILES_PACKAGE_SCRIPTS}/common-funcs.sh"

loadpackagesenv() {
    # shellcheck disable=SC2039
    local TEMPFILE

    if [ ! -z "${1}" ]; then
        PACKAGE_CONF_ROOT="${1}"

        if [ -d "${PACKAGE_CONF_ROOT}" ]; then

            if TEMPFILE="$(mktemp -p "${HOME}")"; then
                if ls -1 "${PACKAGE_CONF_ROOT}" > "${TEMPFILE}"; then

                    while read -r line; do
                        loadpackageenv "${line}" || problem "Could not load environment for package '${line}'."
                    done < "${TEMPFILE}"

                else
                    problem 'Could not load package environments; Unable to list packages.'
                fi
                rm "${TEMPFILE}"
            else
                problem 'Could not load package environments; unable to create temporary file.'
            fi
        fi

        unset PACKAGE_CONF_ROOT
    else
        problem 'Package configuration root must be specified.'
    fi
}


packagerootloop loadpackagesenv

unset -f loadpackagesenv
commonfuncscleanup
loadpackageenvcleanup
