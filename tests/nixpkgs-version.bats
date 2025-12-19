# shellcheck shell=bats

setup() {
    THIS_DIR="$(cd "$(dirname "${BATS_TEST_FILENAME}")" && pwd)"

    bats_load_library 'bats-support'
    bats_load_library 'bats-assert'
}

@test 'nixpkgs version' {
    local PACKAGE
    PACKAGE="$(grep '^export DOTFILES_NIXPKGS_VERSION' <"${THIS_DIR}/../packages/04-nix/env.sh" | grep -E -o '[0-9]+\.[0-9]+')"
    local CI
    CI="$(grep '^ *NIXPKGS_VERSION' "${THIS_DIR}/../.github/workflows/ci.yml" | grep -E -o '[0-9]+\.[0-9]+')"

    assert_equal "${PACKAGE}" "${CI}"
}
