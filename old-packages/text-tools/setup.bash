function _can-install() {
    dotfiles-linux-variant main && dotfiles-can-sudo && dotfiles-package-installed python
}

function _install() {
    sudo apt-get -y install asciidoc aspell biber catdoc chktex dict dictd dict-foldoc dict-gcide dict-jargon dict-wn epstool hunspell hunspell-en-gb markdown strip-nondeterminism texinfo texlive-full transfig unoconv || exit 1

    if dotfiles-linux-variant main; then
        if ! command -v pandoc &>/dev/null; then
            TMPDIR="$(mktemp -d)" &&
            wget -O "${TMPDIR}/pandoc.deb" https://github.com/jgm/pandoc/releases/download/2.19.2/pandoc-2.19.2-1-amd64.deb &&
            sudo dpkg -i "${TMPDIR}/pandoc.deb" &&
            rm "${TMPDIR}/pandoc.deb" &&
            rmdir "${TMPDIR}" || exit 1
        fi
    fi
}

function _update() {
    _install
}
