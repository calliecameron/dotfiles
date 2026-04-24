{ lib, config, ... }:
let
  inherit (builtins) map filter readDir;
  inherit (lib.attrsets) attrsToList;
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

  config = {
    programs.bash.enable = true;
    programs.zsh.enable = true;
  };
}
