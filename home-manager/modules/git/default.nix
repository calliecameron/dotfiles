{ lib, config, ... }:
let
  inherit (lib) mkEnableOption mkIf mkMerge mkOption;
  inherit (lib.types) nonEmptyStr nullOr;

  requiredStr = mkOption {
    type = nonEmptyStr;
  };

  optionalStr = mkOption {
    type = nullOr nonEmptyStr;
    default = null;
  };

  cfg = config.dotfiles.git;
in
{
  options.dotfiles.git = {
    enable = mkEnableOption "enable";

    name = requiredStr;
    email = requiredStr;
    signingKey = optionalStr;

    username = {
      bitbucket = optionalStr;
      github = optionalStr;
      codeberg = optionalStr;
    };
  };

  config = mkIf cfg.enable {
    programs.git = {
      enable = true;

      settings = {
        user.name = cfg.name;
        user.email = cfg.email;

        color.ui = "auto";
        core.autocrlf = false;
        init.defaultBranch = "main";
        init.templateDir = "${./git-template}";
        log.mailmap = true;
        pager.status = true;
        pull.ff = "only";
        push.default = "simple";
        status.showUntrackedFiles = "all";
      };

      signing = mkIf (cfg.signingKey != null) {
        key = cfg.signingKey;
        signByDefault = true;
      };
    };

    home.sessionVariables =
      let
        bitbucket = cfg.username.bitbucket;
        github = cfg.username.github;
        codeberg = cfg.username.codeberg;
      in
        mkMerge [
          (mkIf (bitbucket != null) {
            BITBUCKET = "git@bitbucket.org:${bitbucket}";
            BITBUCKET_HTTPS = "https://${bitbucket}@bitbucket.org/${bitbucket}";
          })
          (mkIf (github != null) {
            GITHUB = "git@github.com:${github}";
            GITHUB_HTTPS = "https://github.com/${github}";
          })
          (mkIf (codeberg != null) {
            CODEBERG = "ssh://git@codeberg.org/${codeberg}";
            CODEBERG_HTTPS = "https://codeberg.org/${codeberg}";
          })
        ];

    home.shellAliases = {
      g = "git";
      s = "git status";
      a = "git add";
      c = "git commit -m";
      p = "git push";
      pl = "git pull";
      lg = "git log";
      gd = "LESS='FRX --mouse --wheel-lines=5' git diff";
      gdw = "LESS='FRX --mouse --wheel-lines=5' git diff --word-diff";
    };
  };
}
