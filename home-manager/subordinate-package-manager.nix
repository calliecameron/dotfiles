{ lib, config, ... }:
let
  inherit (lib) mkIf mkMerge mkOrder;
  inherit (lib.options) mkEnableOption;

  requireEnv = var:
    let
      value = builtins.getEnv var;
    in
      if value != "" then
        value
      else
        throw "Required environment variable '${var}' is empty";

  fakeHome = requireEnv "DOTFILES_HOME_MANAGER_FAKE_HOME";
in
{
  options.dotfiles = {
    subordinatePackageManager = mkEnableOption "subordinatePackageManager";
  };

  config = mkIf config.dotfiles.subordinatePackageManager {
    home.file = {
      ".profile".target = requireEnv "DOTFILES_HOME_MANAGER_PROFILE";
      ".bashrc".target = requireEnv "DOTFILES_HOME_MANAGER_BASHRC";
      ".bash_profile".target = "${fakeHome}/bash_profile";
      ".zshenv".enable = false;
    };

    programs.zsh.dotDir = fakeHome;
    programs.zsh.envExtra = "unset ZDOTDIR";
    programs.zsh.initContent = mkMerge [
      (mkOrder 509 ": '")
      (mkOrder 511 "'")
    ];
  };
}
