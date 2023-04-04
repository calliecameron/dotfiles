# Packages

For each package, create a folder here with the same name as the package,
containing any of the following files:

- `install`: if the package requires installing anything (i.e. it isn't just
  shell functions), this file should be an executable script that does the
  installation. It must be idempotent, since it will be called again during
  updates. Called with the working directory set to `PACKAGE_INSTALL_DIR`.
- `can-install`: executable script determining whether the package can currently
  be installed. Use for dependency checking.
- `env.sh`: loads environment variables. Called with working directory set to
  `PACKAGE_SOURCE_DIR`. Must work in dash, bash and zsh.
- `aliases.sh`: generic aliases. Called with working directory set to
  `PACKAGE_SOURCE_DIR`. Must work in bash and zsh.
- `aliases.{bash|zsh}`: shell-specific aliases. Called with working directory
   set to `PACKAGE_SOURCE_DIR`.
- `emacs.el`: loaded by Emacs. Called with working directory set to
  `this-package-source-dir`.

The folder can also contain any of the following subdirectories:

- `bin`: will be prepended to the PATH and exported, before `env.sh` is sourced.
- `zsh-completions`: will be prepended to the zsh completions path before
  `aliases.zsh` is sourced.
- `elisp`: will be prepended to the Emacs load path before `emacs.el` is
  sourced.
- `nemo-scripts` contains files that will be symlinked into Nemo's scripts
  directory, so they show up on the right-click menu.

In shell files, the following environment variables are available:

- `PACKAGE_NAME`: the package's name, which is the name of the folder containing
  it.
- `PACKAGE_SOURCE_DIR`: the full path of the package's source folder.
- `PACKAGE_INSTALL_DIR`: the full path of where anything should be installed;
  clone repositories here, or if anything else needs to be installed, put it
  here. Created before `install` is called.

In env and alias files, the functions in ../core/package-scripts/common-funcs.sh
are available.

In the Emacs file, the following variables are available:

- `this-package-name`: the package's name, which is the name of the folder
  containing it.
- `this-package-source-dir`: the full path of the package's source folder.
- `this-package-install-dir`: the full path of where anything should be
  installed.
