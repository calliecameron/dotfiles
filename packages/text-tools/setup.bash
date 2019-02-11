function _can-install() {
    os linux && linux-variant main pi && can-sudo
}

function _install() {
    sudo apt-get -y install asciidoc aspell biber chktex dict dictd dict-foldoc dict-gcide dict-jargon dict-wn epstool hunspell hunspell-en-gb markdown pandoc texinfo texlive-full transfig unoconv
}
