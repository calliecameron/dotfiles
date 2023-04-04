dotfiles-home-bin-link ccache gcc g++ cc c++
dotfiles-home-bin-link ctags-exuberant ctags etags
export SHELLCHECK_OPTS="-e SC1090 -e SC1091"
BATS_LIB_PATH="$(readlink -f "$(dirname "$(nvm which current)")/../lib/node_modules")"
export BATS_LIB_PATH