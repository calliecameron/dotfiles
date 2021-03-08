function _can-install() {
    os linux && linux-variant main pi && can-sudo
}

function _install() {
    sudo apt-get -y install asciidoc aspell biber chktex dict dictd dict-foldoc dict-gcide dict-jargon dict-wn epstool hunspell hunspell-en-gb markdown texinfo texlive-full transfig unoconv || exit 1

    if linux-variant main; then
        if ! which pandoc &>/dev/null; then
            TMPDIR="$(mktemp -d)" &&
            wget -O "${TMPDIR}/pandoc.deb" https://github.com/jgm/pandoc/releases/download/2.11.4/pandoc-2.11.4-1-amd64.deb &&
            sudo dpkg -i "${TMPDIR}/pandoc.deb" &&
            rm "${TMPDIR}/pandoc.deb" &&
            rmdir "${TMPDIR}" || exit 1
        fi
    fi
}
