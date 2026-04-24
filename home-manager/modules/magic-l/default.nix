{ lib, config, ... }:
let
  inherit (builtins) readFile;
  inherit (lib) mkEnableOption mkIf;

  cfg = config.dotfiles.magicL;
in
{
  options.dotfiles.magicL = {
    enable = mkEnableOption "enable";
  };

  config = mkIf cfg.enable {
    dotfiles.genericInitExtra = readFile ./magic-l.sh;

    home.shellAliases = {
      l = "magic-l";
    };
  };
}
