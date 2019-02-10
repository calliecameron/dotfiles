For each package, create a folder here with the same name as the
package, containing any of the following files:

- `setup.bash`: if the package requires installing anything (i.e. it
  isn't just shell functions), this file should contain the bash
  function `_install`, and optionally `_update` and `_can-install` if
  any dependencies are required. If the package clones a git
  repository, and can optionally use SSH, the variable `OFFER_GIT_SSH`
  should be set, and the installation script will ask the user whether
  to clone using SSH. The result is stored in `USE_GIT_SSH`, which
  will be available to `_install`, and will be non-empty if SSH
  cloning is desired. `_install` is called with the working directory
  set to `DOTFILES_PACKAGE_INSTALL_DIR`. `_update` is called with
  the working directory set to `PACKAGE_INSTALL_DIR` if it exists,
  else `PACKAGE_CONF_DIR`.
- `env.sh`: sourceable by sh, loads environment variables; called with
  working directory set to `PACKAGE_CONF_DIR`.
- `aliases.sh`: generic aliases, sourceable by either bash or zsh;
  called with working directory set to `PACKAGE_CONF_DIR`.
- `aliases.{bash|zsh}`: shell-specific aliases; called with working
  directory set to `PACKAGE_CONF_DIR`.
- `emacs.el`: loaded by Emacs; called with working directory set to
  `this-package-conf-dir`.

The folder can also contain any of the following subdirectories:
- `bin`: will be prepended to the PATH and exported, before `env.sh`
  is sourced.
- `bin-linux`: if on Linux, will be prepended to the PATH and
  exported, before `env.sh` is sourced.
- `bin-cygwin`: if on Cygwin, will be prepended to the PATH and
  exported, before `env.sh` is sourced.
- `python`: will be prepended to PYTHONPATH and exported, before
  `env.sh` is sourced.
- `zsh-completions`: will be prepended to the zsh completions path
  before `aliases.zsh` is sourced.
- `elisp`: will be prepended to the Emacs load path before `emacs.el`
  is sourced.
- `nemo-scripts` contains files that will be symlinked into Nemo's
  scripts directory, so they show up on the right-click menu.
- `ipython-startup` contains files that will be symlinked into
  IPython's startup directory.

The folder name is the name of the package. All files are optional.

In shell files, the following environment variables are available:
- `PACKAGE_NAME`: the package's name, which is the name of the folder
  containing it.
- `PACKAGE_CONF_DIR`: the full path of the package's configuration
  folder.
- `PACKAGE_INSTALL_DIR`: the full path of where anything should be
  installed; clone repositories here, or if anything else needs to
  be installed, create this directory and put things in it.
- `DOTFILES_OS`: `linux` or `cygwin`. If `linux`, then
  `DOTFILES_LINUX_VARIANT` will also be set.
- `DOTFILES_LINUX_VARIANT`: only set on Linux; can be `main` (for
  current preferred distro), `pi` (for Raspbian), or empty for
  unknown.
- `DOTFILES_CAN_SUDO`: whether the user has sudo permissions on
  this machine; only set on Linux.

In env and alias files, and in setup.bash, the functions in
../core/package-scripts/common-funcs.sh are available. In setup.bash
only, the functions in ../core/package-scripts/setup-common.bash and
../core/common.bash are also available.

In the Emacs file, the following variables are available:
- `this-package-name`: the package's name, which is the name of the
  folder containing it.
- `this-package-conf-dir`: the full path of the package's
  configuration folder.
- `this-package-install-dir`: the full path of where anything should
  be installed.
