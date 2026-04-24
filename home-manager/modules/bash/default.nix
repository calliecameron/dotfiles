{ lib, config, ... }:
let
  inherit (lib) mkEnableOption mkIf;

  cfg = config.dotfiles.bash;
in
{
  options.dotfiles.bash = {
    enable = mkEnableOption "enable";
  };

  config = mkIf cfg.enable {
    programs.bash.enable = true;
    programs.bash.initExtra = config.dotfiles.genericInitExtra;
  };
}
