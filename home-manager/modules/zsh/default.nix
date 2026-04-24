{ lib, config, ... }:
let
  inherit (lib) mkEnableOption mkIf;

  cfg = config.dotfiles.zsh;
in
{
  options.dotfiles.zsh = {
    enable = mkEnableOption "enable";
  };

  config = mkIf cfg.enable {
    programs.zsh.enable = true;
    programs.zsh.initContent = config.dotfiles.genericInitExtra;
  };
}
