{ lib, config, ... }:
let
  inherit (builtins) map filter readDir;
  inherit (lib) mkOption;
  inherit (lib.attrsets) attrsToList;
  inherit (lib.types) lines;
in
{
  imports = [
    ./subordinate-package-manager.nix
  ] ++
  map
    (a: ./modules + "/${a.name}")
    (filter
      (a: a.value == "directory")
      (attrsToList (readDir ./modules)));

  options.dotfiles = {
    genericInitExtra = mkOption {
      type = lines;
      default = "";
    };
  };

  config = {
    dotfiles.bash.enable = true;
    dotfiles.zsh.enable = true;

    dotfiles.git.enable = true;
    dotfiles.magicL.enable = true;
  };
}
