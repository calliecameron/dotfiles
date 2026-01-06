((ace-isearch :source "elpaca-menu-lock-file" :recipe
              (:package "ace-isearch" :repo "tam17aki/ace-isearch"
                        :fetcher github :files
                        ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                         "*.texinfo" "doc/dir" "doc/*.info"
                         "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                         "docs/dir" "docs/*.info" "docs/*.texi"
                         "docs/*.texinfo"
                         (:exclude ".dir-locals.el" "test.el"
                                   "tests.el" "*-test.el" "*-tests.el"
                                   "LICENSE" "README*" "*-pkg.el"))
                        :source "elpaca-menu-lock-file" :protocol
                        https :inherit t :depth treeless :ref
                        "a24bfc626100f183dbad016bd7723eb12e238534"))
 (ace-jump-mode :source "elpaca-menu-lock-file" :recipe
                (:package "ace-jump-mode" :repo
                          "winterTTr/ace-jump-mode" :fetcher github
                          :files
                          ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                           "*.texinfo" "doc/dir" "doc/*.info"
                           "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                           "docs/dir" "docs/*.info" "docs/*.texi"
                           "docs/*.texinfo"
                           (:exclude ".dir-locals.el" "test.el"
                                     "tests.el" "*-test.el"
                                     "*-tests.el" "LICENSE" "README*"
                                     "*-pkg.el"))
                          :source "elpaca-menu-lock-file" :protocol
                          https :inherit t :depth treeless :ref
                          "8351e2df4fbbeb2a4003f2fb39f46d33803f3dac"))
 (alert :source "elpaca-menu-lock-file" :recipe
        (:package "alert" :fetcher github :repo "jwiegley/alert"
                  :files
                  ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                   "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi"
                   "doc/*.texinfo" "lisp/*.el" "docs/dir"
                   "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                   (:exclude ".dir-locals.el" "test.el" "tests.el"
                             "*-test.el" "*-tests.el" "LICENSE"
                             "README*" "*-pkg.el"))
                  :source "elpaca-menu-lock-file" :protocol https
                  :inherit t :depth treeless :ref
                  "79f6936ab4d85227530959811143429347a6971b"))
 (async :source "elpaca-menu-lock-file" :recipe
        (:package "async" :repo "jwiegley/emacs-async" :fetcher github
                  :files
                  ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                   "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi"
                   "doc/*.texinfo" "lisp/*.el" "docs/dir"
                   "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                   (:exclude ".dir-locals.el" "test.el" "tests.el"
                             "*-test.el" "*-tests.el" "LICENSE"
                             "README*" "*-pkg.el"))
                  :source "elpaca-menu-lock-file" :protocol https
                  :inherit t :depth treeless :ref
                  "31cb2fea8f4bc7a593acd76187a89075d8075500"))
 (auctex :source "elpaca-menu-lock-file" :recipe
         (:package "auctex" :repo
                   ("https://git.savannah.gnu.org/git/auctex.git"
                    . "auctex")
                   :branch "main" :files ("*" (:exclude ".git"))
                   :source "elpaca-menu-lock-file" :protocol https
                   :inherit t :depth treeless :ref
                   "d4ad28d66207d70424b49f46289a407967da7973"))
 (auto-dark :source "elpaca-menu-lock-file" :recipe
            (:package "auto-dark" :repo "LionyxML/auto-dark-emacs"
                      :fetcher github :files
                      ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                       "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi"
                       "doc/*.texinfo" "lisp/*.el" "docs/dir"
                       "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                       (:exclude ".dir-locals.el" "test.el" "tests.el"
                                 "*-test.el" "*-tests.el" "LICENSE"
                                 "README*" "*-pkg.el"))
                      :source "elpaca-menu-lock-file" :protocol https
                      :inherit t :depth treeless :ref
                      "a71e791e47d09c5bf4bcbc2bbd7300b71ff72f1a"))
 (back-button :source "elpaca-menu-lock-file" :recipe
              (:package "back-button" :repo "rolandwalker/back-button"
                        :fetcher github :files
                        ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                         "*.texinfo" "doc/dir" "doc/*.info"
                         "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                         "docs/dir" "docs/*.info" "docs/*.texi"
                         "docs/*.texinfo"
                         (:exclude ".dir-locals.el" "test.el"
                                   "tests.el" "*-test.el" "*-tests.el"
                                   "LICENSE" "README*" "*-pkg.el"))
                        :source "elpaca-menu-lock-file" :protocol
                        https :inherit t :depth treeless :ref
                        "f8783c98a7fefc1d0419959c1b462c7dcadce5a8"))
 (bm :source "elpaca-menu-lock-file" :recipe
     (:package "bm" :repo "joodland/bm" :fetcher github :files
               ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
                "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
                "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
                "docs/*.texinfo"
                (:exclude ".dir-locals.el" "test.el" "tests.el"
                          "*-test.el" "*-tests.el" "LICENSE" "README*"
                          "*-pkg.el"))
               :source "elpaca-menu-lock-file" :protocol https
               :inherit t :depth treeless :ref
               "b411b444ca999ba60bb9323a41af71889fe8925a"))
 (company :source "elpaca-menu-lock-file" :recipe
          (:package "company" :fetcher github :repo
                    "company-mode/company-mode" :files
                    (:defaults "icons"
                               ("images/small"
                                "doc/images/small/*.png"))
                    :source "elpaca-menu-lock-file" :protocol https
                    :inherit t :depth treeless :ref
                    "4ff89f7369227fbb89fe721d1db707f1af74cd0f"))
 (company-quickhelp :source "elpaca-menu-lock-file" :recipe
                    (:package "company-quickhelp" :fetcher github
                              :repo "company-mode/company-quickhelp"
                              :files
                              ("*.el" "*.el.in" "dir" "*.info"
                               "*.texi" "*.texinfo" "doc/dir"
                               "doc/*.info" "doc/*.texi"
                               "doc/*.texinfo" "lisp/*.el" "docs/dir"
                               "docs/*.info" "docs/*.texi"
                               "docs/*.texinfo"
                               (:exclude ".dir-locals.el" "test.el"
                                         "tests.el" "*-test.el"
                                         "*-tests.el" "LICENSE"
                                         "README*" "*-pkg.el"))
                              :source "elpaca-menu-lock-file"
                              :protocol https :inherit t :depth
                              treeless :ref
                              "5bda859577582cc42d16fc0eaf5f7c8bedfd9e69"))
 (cond-let
   :source "elpaca-menu-lock-file" :recipe
   (:package "cond-let" :fetcher github :repo "tarsius/cond-let"
             :files
             ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
              "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
              "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
              "docs/*.texinfo"
              (:exclude ".dir-locals.el" "test.el" "tests.el"
                        "*-test.el" "*-tests.el" "LICENSE" "README*"
                        "*-pkg.el"))
             :source "elpaca-menu-lock-file" :protocol https :inherit
             t :depth treeless :ref
             "288b7d36563223ebaf64cb220a3b270bdffb63f1"))
 (csv-mode :source "elpaca-menu-lock-file" :recipe
           (:package "csv-mode" :repo
                     ("https://github.com/emacsmirror/gnu_elpa"
                      . "csv-mode")
                     :branch "externals/csv-mode" :files
                     ("*" (:exclude ".git")) :source
                     "elpaca-menu-lock-file" :protocol https :inherit
                     t :depth treeless :ref
                     "ba5dc934b9dbdc2b57ab1917a669cdfd7d1838d3"))
 (dash :source "elpaca-menu-lock-file" :recipe
       (:package "dash" :fetcher github :repo "magnars/dash.el" :files
                 ("dash.el" "dash.texi") :source
                 "elpaca-menu-lock-file" :protocol https :inherit t
                 :depth treeless :ref
                 "fb443e7a6e660ba849cafcd01021d9aac3ac6764"))
 (diminish :source "elpaca-menu-lock-file" :recipe
           (:package "diminish" :fetcher github :repo
                     "myrjola/diminish.el" :files
                     ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                      "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi"
                      "doc/*.texinfo" "lisp/*.el" "docs/dir"
                      "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                      (:exclude ".dir-locals.el" "test.el" "tests.el"
                                "*-test.el" "*-tests.el" "LICENSE"
                                "README*" "*-pkg.el"))
                     :source "elpaca-menu-lock-file" :protocol https
                     :inherit t :depth treeless :ref
                     "fbd5d846611bad828e336b25d2e131d1bc06b83d"))
 (dirvish :source "elpaca-menu-lock-file" :recipe
          (:package "dirvish" :fetcher github :repo
                    "alexluigit/dirvish" :files
                    (:defaults "extensions/*.el") :source
                    "elpaca-menu-lock-file" :protocol https :inherit t
                    :depth treeless :ref
                    "d877433f957a363ad78b228e13a8e5215f2d6593"))
 (doom-modeline :source "elpaca-menu-lock-file" :recipe
                (:package "doom-modeline" :repo
                          "seagle0128/doom-modeline" :fetcher github
                          :files
                          ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                           "*.texinfo" "doc/dir" "doc/*.info"
                           "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                           "docs/dir" "docs/*.info" "docs/*.texi"
                           "docs/*.texinfo"
                           (:exclude ".dir-locals.el" "test.el"
                                     "tests.el" "*-test.el"
                                     "*-tests.el" "LICENSE" "README*"
                                     "*-pkg.el"))
                          :source "elpaca-menu-lock-file" :protocol
                          https :inherit t :depth treeless :ref
                          "85dc5f033e057135a90958f258c5c6362c5497de"))
 (doom-themes :source "elpaca-menu-lock-file" :recipe
              (:package "doom-themes" :fetcher github :repo
                        "doomemacs/themes" :files
                        (:defaults "themes/*.el" "themes/*/*.el"
                                   "extensions/*.el")
                        :source "elpaca-menu-lock-file" :protocol
                        https :inherit t :depth treeless :ref
                        "376cf4bdd7d296a3da94aa9a6c68761e7c38a252"))
 (eat :source "elpaca-menu-lock-file" :recipe
      (:package "eat" :repo
                ("https://codeberg.org/akib/emacs-eat" . "eat") :files
                ("*" (:exclude ".git")) :source
                "elpaca-menu-lock-file" :protocol https :inherit t
                :depth treeless :ref
                "c8d54d649872bfe7b2b9f49ae5c2addbf12d3b99"))
 (elisp-refs :source "elpaca-menu-lock-file" :recipe
             (:package "elisp-refs" :repo "Wilfred/elisp-refs"
                       :fetcher github :files
                       (:defaults (:exclude "elisp-refs-bench.el"))
                       :source "elpaca-menu-lock-file" :protocol https
                       :inherit t :depth treeless :ref
                       "541a064c3ce27867872cf708354a65d83baf2a6d"))
 (elpaca :source
   "elpaca-menu-lock-file" :recipe
   (:source nil :protocol https :inherit ignore :depth 1 :repo
            "https://github.com/progfolio/elpaca.git" :ref
            "1508298c1ed19c81fa4ebc5d22d945322e9e4c52" :files
            (:defaults "elpaca-test.el" (:exclude "extensions"))
            :build (:not elpaca--activate-package) :package "elpaca"))
 (elpaca-use-package :source "elpaca-menu-lock-file" :recipe
                     (:package "elpaca-use-package" :wait t :repo
                               "https://github.com/progfolio/elpaca.git"
                               :files
                               ("extensions/elpaca-use-package.el")
                               :main
                               "extensions/elpaca-use-package.el"
                               :build (:not elpaca--compile-info)
                               :source "elpaca-menu-lock-file"
                               :protocol https :inherit t :depth
                               treeless :ref
                               "1508298c1ed19c81fa4ebc5d22d945322e9e4c52"))
 (f :source "elpaca-menu-lock-file" :recipe
    (:package "f" :fetcher github :repo "rejeep/f.el" :files
              ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
               "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
               "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
               "docs/*.texinfo"
               (:exclude ".dir-locals.el" "test.el" "tests.el"
                         "*-test.el" "*-tests.el" "LICENSE" "README*"
                         "*-pkg.el"))
              :source "elpaca-menu-lock-file" :protocol https :inherit
              t :depth treeless :ref
              "931b6d0667fe03e7bf1c6c282d6d8d7006143c52"))
 (flycheck :source "elpaca-menu-lock-file" :recipe
           (:package "flycheck" :repo "flycheck/flycheck" :fetcher
                     github :files
                     ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                      "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi"
                      "doc/*.texinfo" "lisp/*.el" "docs/dir"
                      "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                      (:exclude ".dir-locals.el" "test.el" "tests.el"
                                "*-test.el" "*-tests.el" "LICENSE"
                                "README*" "*-pkg.el"))
                     :source "elpaca-menu-lock-file" :protocol https
                     :inherit t :depth treeless :ref
                     "1eafe2911d50c9f58efce81ff8abea59495e1ff3"))
 (flycheck-cask :source "elpaca-menu-lock-file" :recipe
                (:package "flycheck-cask" :repo
                          "flycheck/flycheck-cask" :fetcher github
                          :files
                          ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                           "*.texinfo" "doc/dir" "doc/*.info"
                           "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                           "docs/dir" "docs/*.info" "docs/*.texi"
                           "docs/*.texinfo"
                           (:exclude ".dir-locals.el" "test.el"
                                     "tests.el" "*-test.el"
                                     "*-tests.el" "LICENSE" "README*"
                                     "*-pkg.el"))
                          :source "elpaca-menu-lock-file" :protocol
                          https :inherit t :depth treeless :ref
                          "0eeec5197e9d31bfcfc39380b262d65259a87d91"))
 (flycheck-checkbashisms :source "elpaca-menu-lock-file" :recipe
                         (:package "flycheck-checkbashisms" :repo
                                   "cuonglm/flycheck-checkbashisms"
                                   :fetcher github :files
                                   ("*.el" "*.el.in" "dir" "*.info"
                                    "*.texi" "*.texinfo" "doc/dir"
                                    "doc/*.info" "doc/*.texi"
                                    "doc/*.texinfo" "lisp/*.el"
                                    "docs/dir" "docs/*.info"
                                    "docs/*.texi" "docs/*.texinfo"
                                    (:exclude ".dir-locals.el"
                                              "test.el" "tests.el"
                                              "*-test.el" "*-tests.el"
                                              "LICENSE" "README*"
                                              "*-pkg.el"))
                                   :source "elpaca-menu-lock-file"
                                   :protocol https :inherit t :depth
                                   treeless :ref
                                   "ca8f11679c77d6702f34e773bdde185ceb47a05d"))
 (flycheck-color-mode-line :source "elpaca-menu-lock-file" :recipe
                           (:package "flycheck-color-mode-line" :repo
                                     "flycheck/flycheck-color-mode-line"
                                     :fetcher github :files
                                     ("*.el" "*.el.in" "dir" "*.info"
                                      "*.texi" "*.texinfo" "doc/dir"
                                      "doc/*.info" "doc/*.texi"
                                      "doc/*.texinfo" "lisp/*.el"
                                      "docs/dir" "docs/*.info"
                                      "docs/*.texi" "docs/*.texinfo"
                                      (:exclude ".dir-locals.el"
                                                "test.el" "tests.el"
                                                "*-test.el"
                                                "*-tests.el" "LICENSE"
                                                "README*" "*-pkg.el"))
                                     :source "elpaca-menu-lock-file"
                                     :protocol https :inherit t :depth
                                     treeless :ref
                                     "df9be4c5bf26c4dc5ddaeed8179c4d66bdaa91f5"))
 (flycheck-package :source "elpaca-menu-lock-file" :recipe
                   (:package "flycheck-package" :fetcher github :repo
                             "purcell/flycheck-package" :files
                             ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                              "*.texinfo" "doc/dir" "doc/*.info"
                              "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                              "docs/dir" "docs/*.info" "docs/*.texi"
                              "docs/*.texinfo"
                              (:exclude ".dir-locals.el" "test.el"
                                        "tests.el" "*-test.el"
                                        "*-tests.el" "LICENSE"
                                        "README*" "*-pkg.el"))
                             :source "elpaca-menu-lock-file" :protocol
                             https :inherit t :depth treeless :ref
                             "a52e4e95f3151898b36739dfdb4a98b368626fc0"))
 (flycheck-pos-tip :source "elpaca-menu-lock-file" :recipe
                   (:package "flycheck-pos-tip" :repo
                             "flycheck/flycheck-pos-tip" :fetcher
                             github :files
                             ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                              "*.texinfo" "doc/dir" "doc/*.info"
                              "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                              "docs/dir" "docs/*.info" "docs/*.texi"
                              "docs/*.texinfo"
                              (:exclude ".dir-locals.el" "test.el"
                                        "tests.el" "*-test.el"
                                        "*-tests.el" "LICENSE"
                                        "README*" "*-pkg.el"))
                             :source "elpaca-menu-lock-file" :protocol
                             https :inherit t :depth treeless :ref
                             "dc57beac0e59669926ad720c7af38b27c3a30467"))
 (git-gutter :source "elpaca-menu-lock-file" :recipe
             (:package "git-gutter" :repo "emacsorphanage/git-gutter"
                       :fetcher github :files
                       ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                        "*.texinfo" "doc/dir" "doc/*.info"
                        "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                        "docs/dir" "docs/*.info" "docs/*.texi"
                        "docs/*.texinfo"
                        (:exclude ".dir-locals.el" "test.el"
                                  "tests.el" "*-test.el" "*-tests.el"
                                  "LICENSE" "README*" "*-pkg.el"))
                       :source "elpaca-menu-lock-file" :protocol https
                       :inherit t :depth treeless :ref
                       "101b1e29ec4f4609b29a17877990f95993452188"))
 (git-modes :source "elpaca-menu-lock-file" :recipe
            (:package "git-modes" :fetcher github :repo
                      "magit/git-modes" :old-names
                      (gitattributes-mode gitconfig-mode
                                          gitignore-mode)
                      :files
                      ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                       "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi"
                       "doc/*.texinfo" "lisp/*.el" "docs/dir"
                       "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                       (:exclude ".dir-locals.el" "test.el" "tests.el"
                                 "*-test.el" "*-tests.el" "LICENSE"
                                 "README*" "*-pkg.el"))
                      :source "elpaca-menu-lock-file" :protocol https
                      :inherit t :depth treeless :ref
                      "dfc450d79498b7997b1155ac76629ab01f7ef355"))
 (gntp :source "elpaca-menu-lock-file" :recipe
       (:package "gntp" :repo "tekai/gntp.el" :fetcher github :files
                 ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
                  "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
                  "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
                  "docs/*.texinfo"
                  (:exclude ".dir-locals.el" "test.el" "tests.el"
                            "*-test.el" "*-tests.el" "LICENSE"
                            "README*" "*-pkg.el"))
                 :source "elpaca-menu-lock-file" :protocol https
                 :inherit t :depth treeless :ref
                 "767571135e2c0985944017dc59b0be79af222ef5"))
 (guide-key :source "elpaca-menu-lock-file" :recipe
            (:package "guide-key" :repo "kai2nenobu/guide-key"
                      :fetcher github :files
                      ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                       "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi"
                       "doc/*.texinfo" "lisp/*.el" "docs/dir"
                       "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                       (:exclude ".dir-locals.el" "test.el" "tests.el"
                                 "*-test.el" "*-tests.el" "LICENSE"
                                 "README*" "*-pkg.el"))
                      :source "elpaca-menu-lock-file" :protocol https
                      :inherit t :depth treeless :ref
                      "8f8b839f42edd53af13d588254f07727108ae312"))
 (helm :source "elpaca-menu-lock-file" :recipe
       (:package "helm" :fetcher github :repo "emacs-helm/helm" :files
                 (:defaults "emacs-helm.sh"
                            (:exclude "helm-lib.el" "helm-source.el"
                                      "helm-multi-match.el"
                                      "helm-core.el"))
                 :source "elpaca-menu-lock-file" :protocol https
                 :inherit t :depth treeless :ref
                 "9540c7e7230c4743b4b7cb2449faa1aebb216baf"))
 (helm-bm :source "elpaca-menu-lock-file" :recipe
          (:package "helm-bm" :fetcher github :repo
                    "emacs-helm/helm-bm" :files
                    ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                     "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi"
                     "doc/*.texinfo" "lisp/*.el" "docs/dir"
                     "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                     (:exclude ".dir-locals.el" "test.el" "tests.el"
                               "*-test.el" "*-tests.el" "LICENSE"
                               "README*" "*-pkg.el"))
                    :source "elpaca-menu-lock-file" :protocol https
                    :inherit t :depth treeless :ref
                    "4744b5784df5800f36c3c54de5269034191155f5"))
 (helm-core :source "elpaca-menu-lock-file" :recipe
            (:package "helm-core" :repo "emacs-helm/helm" :fetcher
                      github :files
                      ("helm-core.el" "helm-lib.el" "helm-source.el"
                       "helm-multi-match.el")
                      :source "elpaca-menu-lock-file" :protocol https
                      :inherit t :depth treeless :ref
                      "9540c7e7230c4743b4b7cb2449faa1aebb216baf"))
 (helm-flyspell :source "elpaca-menu-lock-file" :recipe
                (:package "helm-flyspell" :repo
                          "pronobis/helm-flyspell" :fetcher github
                          :files
                          ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                           "*.texinfo" "doc/dir" "doc/*.info"
                           "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                           "docs/dir" "docs/*.info" "docs/*.texi"
                           "docs/*.texinfo"
                           (:exclude ".dir-locals.el" "test.el"
                                     "tests.el" "*-test.el"
                                     "*-tests.el" "LICENSE" "README*"
                                     "*-pkg.el"))
                          :source "elpaca-menu-lock-file" :protocol
                          https :inherit t :depth treeless :ref
                          "8d4d947c687cb650cb149aa2271ad5201ea92594"))
 (helpful :source "elpaca-menu-lock-file" :recipe
          (:package "helpful" :repo "Wilfred/helpful" :fetcher github
                    :files
                    ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                     "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi"
                     "doc/*.texinfo" "lisp/*.el" "docs/dir"
                     "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                     (:exclude ".dir-locals.el" "test.el" "tests.el"
                               "*-test.el" "*-tests.el" "LICENSE"
                               "README*" "*-pkg.el"))
                    :source "elpaca-menu-lock-file" :protocol https
                    :inherit t :depth treeless :ref
                    "03756fa6ad4dcca5e0920622b1ee3f70abfc4e39"))
 (highlight-indentation :source "elpaca-menu-lock-file" :recipe
                        (:package "highlight-indentation" :repo
                                  "antonj/Highlight-Indentation-for-Emacs"
                                  :fetcher github :files
                                  ("*.el" "*.el.in" "dir" "*.info"
                                   "*.texi" "*.texinfo" "doc/dir"
                                   "doc/*.info" "doc/*.texi"
                                   "doc/*.texinfo" "lisp/*.el"
                                   "docs/dir" "docs/*.info"
                                   "docs/*.texi" "docs/*.texinfo"
                                   (:exclude ".dir-locals.el"
                                             "test.el" "tests.el"
                                             "*-test.el" "*-tests.el"
                                             "LICENSE" "README*"
                                             "*-pkg.el"))
                                  :source "elpaca-menu-lock-file"
                                  :protocol https :inherit t :depth
                                  treeless :ref
                                  "d88db4248882da2d4316e76ed673b4ac1fa99ce3"))
 (list-utils :source "elpaca-menu-lock-file" :recipe
             (:package "list-utils" :repo "rolandwalker/list-utils"
                       :fetcher github :files
                       ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                        "*.texinfo" "doc/dir" "doc/*.info"
                        "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                        "docs/dir" "docs/*.info" "docs/*.texi"
                        "docs/*.texinfo"
                        (:exclude ".dir-locals.el" "test.el"
                                  "tests.el" "*-test.el" "*-tests.el"
                                  "LICENSE" "README*" "*-pkg.el"))
                       :source "elpaca-menu-lock-file" :protocol https
                       :inherit t :depth treeless :ref
                       "bbea0e7cc7ab7d96e7f062014bde438aa8ffcd43"))
 (llama :source "elpaca-menu-lock-file" :recipe
        (:package "llama" :fetcher github :repo "tarsius/llama" :files
                  ("llama.el" ".dir-locals.el") :source
                  "elpaca-menu-lock-file" :protocol https :inherit t
                  :depth treeless :ref
                  "e4803de8ab85991b6a944430bb4f543ea338636d"))
 (log4e :source "elpaca-menu-lock-file" :recipe
        (:package "log4e" :repo "aki2o/log4e" :fetcher github :files
                  ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                   "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi"
                   "doc/*.texinfo" "lisp/*.el" "docs/dir"
                   "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                   (:exclude ".dir-locals.el" "test.el" "tests.el"
                             "*-test.el" "*-tests.el" "LICENSE"
                             "README*" "*-pkg.el"))
                  :source "elpaca-menu-lock-file" :protocol https
                  :inherit t :depth treeless :ref
                  "6d71462df9bf595d3861bfb328377346aceed422"))
 (magit :source "elpaca-menu-lock-file" :recipe
        (:package "magit" :fetcher github :repo "magit/magit" :files
                  ("lisp/magit*.el" "lisp/git-*.el" "docs/magit.texi"
                   "docs/AUTHORS.md" "LICENSE" ".dir-locals.el"
                   (:exclude "lisp/magit-section.el"))
                  :source "elpaca-menu-lock-file" :protocol https
                  :inherit t :depth treeless :ref
                  "4800ace210d5eb82b00428f0ff74723c986181ba"))
 (magit-section :source "elpaca-menu-lock-file" :recipe
                (:package "magit-section" :fetcher github :repo
                          "magit/magit" :files
                          ("lisp/magit-section.el"
                           "docs/magit-section.texi"
                           "magit-section-pkg.el")
                          :source "elpaca-menu-lock-file" :protocol
                          https :inherit t :depth treeless :ref
                          "4800ace210d5eb82b00428f0ff74723c986181ba"))
 (markdown-mode :source "elpaca-menu-lock-file" :recipe
                (:package "markdown-mode" :fetcher github :repo
                          "jrblevin/markdown-mode" :files
                          ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                           "*.texinfo" "doc/dir" "doc/*.info"
                           "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                           "docs/dir" "docs/*.info" "docs/*.texi"
                           "docs/*.texinfo"
                           (:exclude ".dir-locals.el" "test.el"
                                     "tests.el" "*-test.el"
                                     "*-tests.el" "LICENSE" "README*"
                                     "*-pkg.el"))
                          :source "elpaca-menu-lock-file" :protocol
                          https :inherit t :depth treeless :ref
                          "b524618c3ed28906a7522482727f121428ce7e2e"))
 (nav-flash :source "elpaca-menu-lock-file" :recipe
            (:package "nav-flash" :repo "rolandwalker/nav-flash"
                      :fetcher github :files
                      ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                       "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi"
                       "doc/*.texinfo" "lisp/*.el" "docs/dir"
                       "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                       (:exclude ".dir-locals.el" "test.el" "tests.el"
                                 "*-test.el" "*-tests.el" "LICENSE"
                                 "README*" "*-pkg.el"))
                      :source "elpaca-menu-lock-file" :protocol https
                      :inherit t :depth treeless :ref
                      "5d4b48567862f6be0ca973d6b1dca90e4815cb9b"))
 (nerd-icons :source "elpaca-menu-lock-file" :recipe
             (:package "nerd-icons" :repo
                       "rainstormstudio/nerd-icons.el" :fetcher github
                       :files (:defaults "data") :source
                       "elpaca-menu-lock-file" :protocol https
                       :inherit t :depth treeless :ref
                       "772987a28d6408f840331c52c91d04b623a87048"))
 (package-lint :source "elpaca-menu-lock-file" :recipe
               (:package "package-lint" :fetcher github :repo
                         "purcell/package-lint" :files
                         (:defaults "data" (:exclude "*flymake.el"))
                         :source "elpaca-menu-lock-file" :protocol
                         https :inherit t :depth treeless :ref
                         "700fffc16364541c7f5d1b6b54ab05ce3fe5a893"))
 (pcache :source "elpaca-menu-lock-file" :recipe
         (:package "pcache" :repo "sigma/pcache" :fetcher github
                   :files
                   ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                    "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi"
                    "doc/*.texinfo" "lisp/*.el" "docs/dir"
                    "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                    (:exclude ".dir-locals.el" "test.el" "tests.el"
                              "*-test.el" "*-tests.el" "LICENSE"
                              "README*" "*-pkg.el"))
                   :source "elpaca-menu-lock-file" :protocol https
                   :inherit t :depth treeless :ref
                   "e287b5d116679f79789ee9ee22ee213dc6cef68c"))
 (persistent-soft :source "elpaca-menu-lock-file" :recipe
                  (:package "persistent-soft" :repo
                            "rolandwalker/persistent-soft" :fetcher
                            github :files
                            ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                             "*.texinfo" "doc/dir" "doc/*.info"
                             "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                             "docs/dir" "docs/*.info" "docs/*.texi"
                             "docs/*.texinfo"
                             (:exclude ".dir-locals.el" "test.el"
                                       "tests.el" "*-test.el"
                                       "*-tests.el" "LICENSE"
                                       "README*" "*-pkg.el"))
                            :source "elpaca-menu-lock-file" :protocol
                            https :inherit t :depth treeless :ref
                            "24e41d1952bef5953ef0af2288de146265c7ee10"))
 (popwin :source "elpaca-menu-lock-file" :recipe
         (:package "popwin" :fetcher github :repo
                   "emacsorphanage/popwin" :files
                   ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                    "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi"
                    "doc/*.texinfo" "lisp/*.el" "docs/dir"
                    "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                    (:exclude ".dir-locals.el" "test.el" "tests.el"
                              "*-test.el" "*-tests.el" "LICENSE"
                              "README*" "*-pkg.el"))
                   :source "elpaca-menu-lock-file" :protocol https
                   :inherit t :depth treeless :ref
                   "7adcffa7ae2adef81a9c6262bb05c6e4e2908b83"))
 (pos-tip :source "elpaca-menu-lock-file" :recipe
          (:package "pos-tip" :repo "pitkali/pos-tip" :fetcher github
                    :files
                    ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                     "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi"
                     "doc/*.texinfo" "lisp/*.el" "docs/dir"
                     "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                     (:exclude ".dir-locals.el" "test.el" "tests.el"
                               "*-test.el" "*-tests.el" "LICENSE"
                               "README*" "*-pkg.el"))
                    :source "elpaca-menu-lock-file" :protocol https
                    :inherit t :depth treeless :ref
                    "4889e08cf9077c8589ea6fea4e2ce558614dfcde"))
 (queue :source "elpaca-menu-lock-file" :recipe
        (:package "queue" :repo
                  ("https://github.com/emacsmirror/gnu_elpa" . "queue")
                  :branch "externals/queue" :files
                  ("*" (:exclude ".git")) :source
                  "elpaca-menu-lock-file" :protocol https :inherit t
                  :depth treeless :ref
                  "f986fb68e75bdae951efb9e11a3012ab6bd408ee"))
 (rainbow-delimiters :source "elpaca-menu-lock-file" :recipe
                     (:package "rainbow-delimiters" :fetcher github
                               :repo "Fanael/rainbow-delimiters"
                               :files
                               ("*.el" "*.el.in" "dir" "*.info"
                                "*.texi" "*.texinfo" "doc/dir"
                                "doc/*.info" "doc/*.texi"
                                "doc/*.texinfo" "lisp/*.el" "docs/dir"
                                "docs/*.info" "docs/*.texi"
                                "docs/*.texinfo"
                                (:exclude ".dir-locals.el" "test.el"
                                          "tests.el" "*-test.el"
                                          "*-tests.el" "LICENSE"
                                          "README*" "*-pkg.el"))
                               :source "elpaca-menu-lock-file"
                               :protocol https :inherit t :depth
                               treeless :ref
                               "f40ece58df8b2f0fb6c8576b527755a552a5e763"))
 (s :source "elpaca-menu-lock-file" :recipe
    (:package "s" :fetcher github :repo "magnars/s.el" :files
              ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
               "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
               "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
               "docs/*.texinfo"
               (:exclude ".dir-locals.el" "test.el" "tests.el"
                         "*-test.el" "*-tests.el" "LICENSE" "README*"
                         "*-pkg.el"))
              :source "elpaca-menu-lock-file" :protocol https :inherit
              t :depth treeless :ref
              "dda84d38fffdaf0c9b12837b504b402af910d01d"))
 (shrink-path :source "elpaca-menu-lock-file" :recipe
              (:package "shrink-path" :fetcher gitlab :repo
                        "bennya/shrink-path.el" :files
                        ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                         "*.texinfo" "doc/dir" "doc/*.info"
                         "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                         "docs/dir" "docs/*.info" "docs/*.texi"
                         "docs/*.texinfo"
                         (:exclude ".dir-locals.el" "test.el"
                                   "tests.el" "*-test.el" "*-tests.el"
                                   "LICENSE" "README*" "*-pkg.el"))
                        :source "elpaca-menu-lock-file" :protocol
                        https :inherit t :depth treeless :ref
                        "c14882c8599aec79a6e8ef2d06454254bb3e1e41"))
 (smartparens :source "elpaca-menu-lock-file" :recipe
              (:package "smartparens" :fetcher github :repo
                        "Fuco1/smartparens" :files
                        ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                         "*.texinfo" "doc/dir" "doc/*.info"
                         "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                         "docs/dir" "docs/*.info" "docs/*.texi"
                         "docs/*.texinfo"
                         (:exclude ".dir-locals.el" "test.el"
                                   "tests.el" "*-test.el" "*-tests.el"
                                   "LICENSE" "README*" "*-pkg.el"))
                        :source "elpaca-menu-lock-file" :protocol
                        https :inherit t :depth treeless :ref
                        "b629b4e893ba21ba5a381f6c0054bb72f8e96df2"))
 (smartrep :source "elpaca-menu-lock-file" :recipe
           (:package "smartrep" :repo "myuhe/smartrep.el" :fetcher
                     github :files
                     ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                      "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi"
                      "doc/*.texinfo" "lisp/*.el" "docs/dir"
                      "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                      (:exclude ".dir-locals.el" "test.el" "tests.el"
                                "*-test.el" "*-tests.el" "LICENSE"
                                "README*" "*-pkg.el"))
                     :source "elpaca-menu-lock-file" :protocol https
                     :inherit t :depth treeless :ref
                     "fdf135e3781b286174b5de4d613f12c318d2023c"))
 (switch-window :source "elpaca-menu-lock-file" :recipe
                (:package "switch-window" :repo
                          "dimitri/switch-window" :fetcher github
                          :files
                          ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                           "*.texinfo" "doc/dir" "doc/*.info"
                           "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                           "docs/dir" "docs/*.info" "docs/*.texi"
                           "docs/*.texinfo"
                           (:exclude ".dir-locals.el" "test.el"
                                     "tests.el" "*-test.el"
                                     "*-tests.el" "LICENSE" "README*"
                                     "*-pkg.el"))
                          :source "elpaca-menu-lock-file" :protocol
                          https :inherit t :depth treeless :ref
                          "8f771b571a1e60fac2d2a9845c0a5a52d5b440df"))
 (syntax-subword :source "elpaca-menu-lock-file" :recipe
                 (:package "syntax-subword" :fetcher github :repo
                           "jpkotta/syntax-subword" :files
                           ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                            "*.texinfo" "doc/dir" "doc/*.info"
                            "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                            "docs/dir" "docs/*.info" "docs/*.texi"
                            "docs/*.texinfo"
                            (:exclude ".dir-locals.el" "test.el"
                                      "tests.el" "*-test.el"
                                      "*-tests.el" "LICENSE" "README*"
                                      "*-pkg.el"))
                           :source "elpaca-menu-lock-file" :protocol
                           https :inherit t :depth treeless :ref
                           "9aa9b3f846bfe2474370642458a693ac4760d9fe"))
 (term-alert :source "elpaca-menu-lock-file" :recipe
             (:package "term-alert" :fetcher github :repo
                       "calliecameron/term-alert" :files
                       (:defaults "setup") :source
                       "elpaca-menu-lock-file" :protocol https
                       :inherit t :depth treeless :ref
                       "d2eea58abe95de009e9c8ed3f814bd0dce2a5e2f"))
 (term-cmd :source "elpaca-menu-lock-file" :recipe
           (:package "term-cmd" :fetcher github :repo
                     "calliecameron/term-cmd" :files (:defaults "bin")
                     :source "elpaca-menu-lock-file" :protocol https
                     :inherit t :depth treeless :ref
                     "26c5a8cb6b55ac0d6c6bc08f6ea1b1e53f6e2654"))
 (transient :source "elpaca-menu-lock-file" :recipe
            (:package "transient" :fetcher github :repo
                      "magit/transient" :files
                      ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                       "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi"
                       "doc/*.texinfo" "lisp/*.el" "docs/dir"
                       "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                       (:exclude ".dir-locals.el" "test.el" "tests.el"
                                 "*-test.el" "*-tests.el" "LICENSE"
                                 "README*" "*-pkg.el"))
                      :source "elpaca-menu-lock-file" :protocol https
                      :inherit t :depth treeless :ref
                      "453376f2f1a0beab45da06c84a9e57692afc0607"))
 (transpose-frame :source "elpaca-menu-lock-file" :recipe
                  (:package "transpose-frame" :fetcher github :repo
                            "emacsorphanage/transpose-frame" :files
                            ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                             "*.texinfo" "doc/dir" "doc/*.info"
                             "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                             "docs/dir" "docs/*.info" "docs/*.texi"
                             "docs/*.texinfo"
                             (:exclude ".dir-locals.el" "test.el"
                                       "tests.el" "*-test.el"
                                       "*-tests.el" "LICENSE"
                                       "README*" "*-pkg.el"))
                            :source "elpaca-menu-lock-file" :protocol
                            https :inherit t :depth treeless :ref
                            "94c87794d53883a2358d13da264ad8dab9a52daa"))
 (undo-tree :source "elpaca-menu-lock-file" :recipe
            (:package "undo-tree" :repo
                      ("https://gitlab.com/tsc25/undo-tree"
                       . "undo-tree")
                      :files ("*" (:exclude ".git")) :source
                      "elpaca-menu-lock-file" :protocol https :inherit
                      t :depth treeless :ref
                      "2bf5e230f1d11df7bbd9d8c722749e34482bc458"))
 (volatile-highlights :source "elpaca-menu-lock-file" :recipe
                      (:package "volatile-highlights" :repo
                                "k-talo/volatile-highlights.el"
                                :fetcher github :files
                                ("*.el" "*.el.in" "dir" "*.info"
                                 "*.texi" "*.texinfo" "doc/dir"
                                 "doc/*.info" "doc/*.texi"
                                 "doc/*.texinfo" "lisp/*.el"
                                 "docs/dir" "docs/*.info"
                                 "docs/*.texi" "docs/*.texinfo"
                                 (:exclude ".dir-locals.el" "test.el"
                                           "tests.el" "*-test.el"
                                           "*-tests.el" "LICENSE"
                                           "README*" "*-pkg.el"))
                                :source "elpaca-menu-lock-file"
                                :protocol https :inherit t :depth
                                treeless :ref
                                "b1e7754d7b502ef6583a13f2662e515a654f944d"))
 (wakib-keys :source "elpaca-menu-lock-file" :recipe
             (:package "wakib-keys" :repo "darkstego/wakib-keys"
                       :fetcher github :files
                       ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                        "*.texinfo" "doc/dir" "doc/*.info"
                        "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                        "docs/dir" "docs/*.info" "docs/*.texi"
                        "docs/*.texinfo"
                        (:exclude ".dir-locals.el" "test.el"
                                  "tests.el" "*-test.el" "*-tests.el"
                                  "LICENSE" "README*" "*-pkg.el"))
                       :source "elpaca-menu-lock-file" :protocol https
                       :inherit t :depth treeless :wait t :ref
                       "07258b0293c9f31ba11bd89298b9f90eb232a94c"))
 (wfnames :source "elpaca-menu-lock-file" :recipe
          (:package "wfnames" :fetcher github :repo
                    "thierryvolpiatto/wfnames" :files
                    ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                     "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi"
                     "doc/*.texinfo" "lisp/*.el" "docs/dir"
                     "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                     (:exclude ".dir-locals.el" "test.el" "tests.el"
                               "*-test.el" "*-tests.el" "LICENSE"
                               "README*" "*-pkg.el"))
                    :source "elpaca-menu-lock-file" :protocol https
                    :inherit t :depth treeless :ref
                    "164e4efa2a96bed201a0a5402e137ebeef15bcc6"))
 (with-editor :source "elpaca-menu-lock-file"
   :recipe
   (:package "with-editor" :fetcher github :repo "magit/with-editor"
             :files
             ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
              "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
              "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
              "docs/*.texinfo"
              (:exclude ".dir-locals.el" "test.el" "tests.el"
                        "*-test.el" "*-tests.el" "LICENSE" "README*"
                        "*-pkg.el"))
             :source "elpaca-menu-lock-file" :protocol https :inherit
             t :depth treeless :ref
             "72e80f1236237f346d9692ab8e793798b8c038c5"))
 (yaml-mode :source "elpaca-menu-lock-file" :recipe
            (:package "yaml-mode" :repo "yoshiki/yaml-mode" :fetcher
                      github :files
                      ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                       "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi"
                       "doc/*.texinfo" "lisp/*.el" "docs/dir"
                       "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                       (:exclude ".dir-locals.el" "test.el" "tests.el"
                                 "*-test.el" "*-tests.el" "LICENSE"
                                 "README*" "*-pkg.el"))
                      :source "elpaca-menu-lock-file" :protocol https
                      :inherit t :depth treeless :ref
                      "d91f878729312a6beed77e6637c60497c5786efa")))
