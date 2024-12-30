;;; carbon-theme.el --- 

;; Copyright 2024-present, All rights reserved
;;
;; Code licensed under the MIT license

;; Maintainer: Gopesh Human <gopesh.sharma.human@gmail.com>
;; Author: Gopesh Human
;; Version: 1.0
;; Package-Requires: ((emacs "24.3"))
;; URL: https://github.com/gopesh-human/carbon-theme

;;; Commentary:

;; TODO

;;; News:
;; Version: 1.0
;; The theme began.

;;; Code:
(deftheme carbon-light)

;;;; Configuration options:

(defgroup carbon-light nil
  "Carbon theme options.
The theme has to be reloaded after changing anything in this group."
  :group 'faces)

;;;; Theme definition:

(let ((colors '(;; Theme colors
                (bg        "#f2f4f8")
                (bg2       "#e0e0e0")
                (bg3       "#dde1e6")
                (bg4       "#525252")
                (fg        "#161616")
                (fg2       "#374747")
                (fg3       "#000000")
                (red       "#fa4d56")
                (orange    "#ff6f00")
                (teal      "#08bdba")
                (purple    "#673ab7")
                (blue      "#0f62fe")
                (pink      "#ee5396")
                (green     "#42be65")
                ))
      (faces '(;; default / basic faces
               (cursor :background ,fg2)
               (default :background ,bg :foreground ,fg)
               (error :foreground ,red)
               (ffap :foreground ,fg3)
               (fringe :background ,bg :foreground ,fg2)
               (header-line :inherit 'mode-line)
               (highlight :foreground ,fg3 :background ,bg3)
               (hl-line :background ,bg3)
               (info-quoted-name :foreground ,blue)
               (info-string :foreground ,green)
               (lazy-highlight :foreground ,fg3 :background ,bg3)
               (line-number :slant italic :foreground ,bg4 :background ,bg)
               (match :background ,purple :foreground ,bg)
               (menu :background ,bg2 :inverse-video nil :foreground ,fg)
               (minibuffer-prompt :weight bold :foreground ,pink)
               (mode-line :background ,bg2
                          :box ,bg4 :inverse-video nil :foreground ,fg)
               (mode-line-inactive
                 :background ,bg2 :inverse-video nil
                 :foreground ,fg2 :box ,bg2)
               (read-multiple-choice-face :inherit completions-first-difference)
               (region :background ,bg3 :extend nil)
               (shadow :foreground ,bg4)
               (success :foreground ,green)
               (tooltip :foreground ,fg :background ,bg2)
               (trailing-whitespace :background ,orange)
               (vertical-border :foreground ,bg2 :background ,bg2)
               (warning :foreground ,orange)
               ;; syntax / font-lock
               (font-lock-builtin-face :foreground ,purple)
               (font-lock-comment-face :inherit shadow)
               (font-lock-comment-delimiter-face :inherit shadow :weight bold)
               (font-lock-constant-face :weight bold)
               (font-lock-doc-face :inherit comment)
               (font-lock-function-name-face :foreground ,pink :weight bold)
               (font-lock-keyword-face :foreground ,blue :weight bold)
               (font-lock-negation-char-face :foreground ,teal)
               (font-lock-number-face :foreground ,purple)
               (font-lock-operator-face :foreground ,pink)
               (font-lock-preprocessor-face :foreground ,pink)
               (font-lock-regexp-grouping-backslash :foreground ,teal)
               (font-lock-regexp-grouping-construct :foreground ,purple)
               (font-lock-string-face :foreground ,green)
               (font-lock-type-face :inherit font-lock-builtin-face)
               (font-lock-variable-name-face :foreground ,fg :weight bold)
               (font-lock-warning-face :inherit warning)
               ;; auto-complete
               (ac-completion-face :underline t :foreground ,pink)
               ;; ;; ansi-color
               (ansi-color-black :foreground ,bg :background ,bg)
               (ansi-color-bright-black :foreground "black" :background "black")
               (ansi-color-blue :foreground ,blue :background ,blue)
               (ansi-color-bright-blue :foreground ,blue
                                       :background ,blue
                                       :weight bold)
               (ansi-color-cyan :foreground ,teal :background ,teal)
               (ansi-color-bright-cyan :foreground ,teal
                                       :background ,teal
                                       :weight bold)
               (ansi-color-green :foreground ,green :background ,green)
               (ansi-color-bright-green :foreground ,green
                                        :background ,green
                                        :weight bold)
               (ansi-color-magenta :foreground ,pink :background ,pink)
               (ansi-color-bright-magenta :foreground ,pink
                                          :background ,pink
                                          :weight bold)
               (ansi-color-red :foreground ,red :background ,red)
               (ansi-color-bright-red :foreground ,red
                                      :background ,red
                                      :weight bold)
               (ansi-color-white :foreground ,fg :background ,fg)
               (ansi-color-bright-white :foreground "white" :background "white")
               (ansi-color-yellow :foreground ,orange :background ,orange)
               (ansi-color-bright-yellow :foreground ,orange
                                         :background ,orange
                                         :weight bold)
               ;; bookmarks
               (bookmark-face :foreground ,pink)
               ;; company
               (company-echo-common :foreground ,bg :background ,fg)
               (company-preview :background ,bg2 :foreground ,blue)
               (company-preview-common :inherit company-preview
                                       :foreground ,pink)
               (company-preview-search :inherit company-preview
                                        :foreground ,green)
               (company-scrollbar-bg :background ,bg3)
               (company-scrollbar-fg :foreground ,teal)
               (company-tooltip :inherit tooltip)
               (company-tooltip-search :foreground ,green
                                       :underline t)
               (company-tooltip-search-selection :background ,green
                                                 :foreground ,bg)
               (company-tooltip-selection :inherit match)
               (company-tooltip-mouse :background ,bg)
               (company-tooltip-common :foreground ,pink :weight bold)
               (company-tooltip-common-selection :inherit company-tooltip-common)
               (company-tooltip-annotation :foreground ,teal)
               (company-tooltip-annotation-selection :inherit company-tooltip-annotation)
               ;; completions (minibuffer.el)
               (completions-annotations :inherit font-lock-comment-face)
               (completions-common-part :foreground ,green)
               (completions-first-difference :foreground ,pink :weight bold)
               ;; diff
               (diff-added :background ,green :foreground ,fg :extend t)
               (diff-removed :background ,red :foreground ,fg :extend t)
               (diff-refine-added :background ,green
                                  :foreground ,bg)
               (diff-refine-removed :background ,red
                                    :foreground ,fg)
               (diff-indicator-added :foreground ,green)
               (diff-indicator-removed :foreground ,red)
               (diff-indicator-changed :foreground ,pink)
               (diff-error :foreground ,red, :background ,bg
                           :weight bold)
               ;; diff-hl
               (diff-hl-change :foreground ,pink :background ,pink)
               (diff-hl-delete :foreground ,red :background ,red)
               (diff-hl-insert :foreground ,green :background ,green)
               ;; dired
               ;; (dired-directory :foreground ,dracula-green :weight normal)
               ;; (dired-flagged :foreground ,dracula-pink)
               ;; (dired-header :foreground ,fg3 :background ,dracula-bg)
               ;; (dired-ignored :inherit shadow)
               ;; (dired-mark :foreground ,dracula-fg :weight bold)
               ;; (dired-marked :foreground ,dracula-orange :weight bold)
               ;; (dired-perm-write :foreground ,fg3 :underline t)
               ;; (dired-symlink :foreground ,dracula-yellow :weight normal :slant italic)
               ;; (dired-warning :foreground ,dracula-orange :underline t)
               ;; (diredp-compressed-file-name :foreground ,fg3)
               ;; (diredp-compressed-file-suffix :foreground ,fg4)
               ;; (diredp-date-time :foreground ,dracula-fg)
               ;; (diredp-deletion-file-name :foreground ,dracula-pink :background ,dracula-current)
               ;; (diredp-deletion :foreground ,dracula-pink :weight bold)
               ;; (diredp-dir-heading :foreground ,fg2 :background ,bg3)
               ;; (diredp-dir-name :inherit dired-directory)
               ;; (diredp-dir-priv :inherit dired-directory)
               ;; (diredp-executable-tag :foreground ,dracula-orange)
               ;; (diredp-file-name :foreground ,dracula-fg)
               ;; (diredp-file-suffix :foreground ,fg4)
               ;; (diredp-flag-mark-line :foreground ,fg2 :slant italic :background ,dracula-current)
               ;; (diredp-flag-mark :foreground ,fg2 :weight bold :background ,dracula-current)
               ;; (diredp-ignored-file-name :foreground ,dracula-fg)
               ;; (diredp-mode-line-flagged :foreground ,dracula-orange)
               ;; (diredp-mode-line-marked :foreground ,dracula-orange)
               ;; (diredp-no-priv :foreground ,dracula-fg)
               ;; (diredp-number :foreground ,dracula-cyan)
               ;; (diredp-other-priv :foreground ,dracula-orange)
               ;; (diredp-rare-priv :foreground ,dracula-orange)
               ;; (diredp-read-priv :foreground ,dracula-purple)
               ;; (diredp-write-priv :foreground ,dracula-pink)
               ;; (diredp-exec-priv :foreground ,dracula-yellow)
               ;; (diredp-symlink :foreground ,dracula-orange)
               ;; (diredp-link-priv :foreground ,dracula-orange)
               ;; (diredp-autofile-name :foreground ,dracula-yellow)
               ;; (diredp-tagged-autofile-name :foreground ,dracula-yellow)
               ;; ;; ediff
               ;; (ediff-current-diff-A :background ,dark-red)
               ;; (ediff-fine-diff-A :background ,dracula-red :foreground ,dracula-fg)
               ;; (ediff-current-diff-B :background ,dark-green)
               ;; (ediff-fine-diff-B :background ,dracula-green :foreground ,dracula-bg)
               ;; (ediff-current-diff-C :background ,dark-blue)
               ;; (ediff-fine-diff-C :background ,dracula-cyan :foreground ,dracula-bg)
               ;; ;; eglot
               ;; (eglot-diagnostic-tag-unnecessary-face :inherit warning)
               ;; (eglot-diagnostic-tag-deprecated-face :inherit warning :strike-through t)
               ;; ;; eldoc-box
               ;; (eldoc-box-border :background ,dracula-current)
               ;; (eldoc-box-body :background ,dracula-current)
               ;; ;; elfeed
               ;; (elfeed-search-date-face :foreground ,dracula-comment)
               ;; (elfeed-search-title-face :foreground ,dracula-fg)
               ;; (elfeed-search-unread-title-face :foreground ,dracula-pink :weight bold)
               ;; (elfeed-search-feed-face :foreground ,dracula-fg :weight bold)
               ;; (elfeed-search-tag-face :foreground ,dracula-green)
               ;; (elfeed-search-last-update-face :weight bold)
               ;; (elfeed-search-unread-count-face :foreground ,dracula-pink)
               ;; (elfeed-search-filter-face :foreground ,dracula-green :weight bold)
               ;; ;;(elfeed-log-date-face :inherit font-lock-type-face)
               ;; (elfeed-log-error-level-face :foreground ,dracula-red)
               ;; (elfeed-log-warn-level-face :foreground ,dracula-orange)
               ;; (elfeed-log-info-level-face :foreground ,dracula-cyan)
               ;; (elfeed-log-debug-level-face :foreground ,dracula-comment)
               ;; ;; elpher
               ;; (elpher-gemini-heading1 :inherit bold :foreground ,dracula-pink
               ;;                         ,@(when dracula-enlarge-headings
               ;;                             (list :height dracula-height-title-1)))
               ;; (elpher-gemini-heading2 :inherit bold :foreground ,dracula-purple
               ;;                         ,@(when dracula-enlarge-headings
               ;;                             (list :height dracula-height-title-2)))
               ;; (elpher-gemini-heading3 :weight normal :foreground ,dracula-green
               ;;                         ,@(when dracula-enlarge-headings
               ;;                             (list :height dracula-height-title-3)))
               ;; (elpher-gemini-preformatted :inherit fixed-pitch
               ;;                             :foreground ,dracula-orange)
               ;; ;; enh-ruby
               ;; (enh-ruby-heredoc-delimiter-face :foreground ,dracula-yellow)
               ;; (enh-ruby-op-face :foreground ,dracula-pink)
               ;; (enh-ruby-regexp-delimiter-face :foreground ,dracula-yellow)
               ;; (enh-ruby-string-delimiter-face :foreground ,dracula-yellow)
               ;; ;; flyspell
               ;; (flyspell-duplicate :underline (:style wave :color ,dracula-orange))
               ;; (flyspell-incorrect :underline (:style wave :color ,dracula-red))
               ;; font-latex
               (font-latex-bold-face :foreground ,fg :weight bold)
               (font-latex-italic-face :foreground ,fg :slant italic)
               (font-latex-match-reference-keywords :foreground ,blue)
               (font-latex-match-variable-keywords :foreground ,teal :weight bold)
               (font-latex-string-face :foreground ,green)
               ;; ;; gemini
               ;; (gemini-heading-face-1 :inherit bold :foreground ,dracula-pink
               ;;                        ,@(when dracula-enlarge-headings
               ;;                            (list :height dracula-height-title-1)))
               ;; (gemini-heading-face-2 :inherit bold :foreground ,dracula-purple
               ;;                        ,@(when dracula-enlarge-headings
               ;;                            (list :height dracula-height-title-2)))
               ;; (gemini-heading-face-3 :weight normal :foreground ,dracula-green
               ;;                        ,@(when dracula-enlarge-headings
               ;;                            (list :height dracula-height-title-3)))
               ;; (gemini-heading-face-rest :weight normal :foreground ,dracula-yellow)
               ;; (gemini-quote-face :foreground ,dracula-purple)
               ;; ;; go-test
               ;; (go-test--ok-face :inherit success)
               ;; (go-test--error-face :inherit error)
               ;; (go-test--warning-face :inherit warning)
               ;; (go-test--pointer-face :foreground ,dracula-pink)
               ;; (go-test--standard-face :foreground ,dracula-cyan)
               ;; ;; gnus-group
               ;; (gnus-group-mail-1 :foreground ,dracula-pink :weight bold)
               ;; (gnus-group-mail-1-empty :inherit gnus-group-mail-1 :weight normal)
               ;; (gnus-group-mail-2 :foreground ,dracula-cyan :weight bold)
               ;; (gnus-group-mail-2-empty :inherit gnus-group-mail-2 :weight normal)
               ;; (gnus-group-mail-3 :foreground ,dracula-comment :weight bold)
               ;; (gnus-group-mail-3-empty :inherit gnus-group-mail-3 :weight normal)
               ;; (gnus-group-mail-low :foreground ,dracula-current :weight bold)
               ;; (gnus-group-mail-low-empty :inherit gnus-group-mail-low :weight normal)
               ;; (gnus-group-news-1 :foreground ,dracula-pink :weight bold)
               ;; (gnus-group-news-1-empty :inherit gnus-group-news-1 :weight normal)
               ;; (gnus-group-news-2 :foreground ,dracula-cyan :weight bold)
               ;; (gnus-group-news-2-empty :inherit gnus-group-news-2 :weight normal)
               ;; (gnus-group-news-3 :foreground ,dracula-comment :weight bold)
               ;; (gnus-group-news-3-empty :inherit gnus-group-news-3 :weight normal)
               ;; (gnus-group-news-4 :inherit gnus-group-news-low)
               ;; (gnus-group-news-4-empty :inherit gnus-group-news-low-empty)
               ;; (gnus-group-news-5 :inherit gnus-group-news-low)
               ;; (gnus-group-news-5-empty :inherit gnus-group-news-low-empty)
               ;; (gnus-group-news-6 :inherit gnus-group-news-low)
               ;; (gnus-group-news-6-empty :inherit gnus-group-news-low-empty)
               ;; (gnus-group-news-low :foreground ,dracula-current :weight bold)
               ;; (gnus-group-news-low-empty :inherit gnus-group-news-low :weight normal)
               ;; (gnus-header-content :foreground ,dracula-purple)
               ;; (gnus-header-from :foreground ,dracula-fg)
               ;; (gnus-header-name :foreground ,dracula-green)
               ;; (gnus-header-subject :foreground ,dracula-pink :weight bold)
               ;; (gnus-summary-markup-face :foreground ,dracula-cyan)
               ;; (gnus-summary-high-unread :foreground ,dracula-pink :weight bold)
               ;; (gnus-summary-high-read :inherit gnus-summary-high-unread :weight normal)
               ;; (gnus-summary-high-ancient :inherit gnus-summary-high-read)
               ;; (gnus-summary-high-ticked :inherit gnus-summary-high-read :underline t)
               ;; (gnus-summary-normal-unread :foreground ,dark-blue :weight bold)
               ;; (gnus-summary-normal-read :foreground ,dracula-comment :weight normal)
               ;; (gnus-summary-normal-ancient :inherit gnus-summary-normal-read :weight light)
               ;; (gnus-summary-normal-ticked :foreground ,dracula-pink :weight bold)
               ;; (gnus-summary-low-unread :foreground ,dracula-comment :weight bold)
               ;; (gnus-summary-low-read :inherit gnus-summary-low-unread :weight normal)
               ;; (gnus-summary-low-ancient :inherit gnus-summary-low-read)
               ;; (gnus-summary-low-ticked :inherit gnus-summary-low-read :underline t)
               ;; (gnus-summary-selected :inverse-video t)
               ;; haskell-mode
               (haskell-operator-face :foreground ,pink)
               (haskell-constructor-face :foreground ,purple)
               ;; ;; helm
               ;; (helm-bookmark-w3m :foreground ,dracula-purple)
               ;; (helm-buffer-not-saved :foreground ,dracula-purple :background ,dracula-bg)
               ;; (helm-buffer-process :foreground ,dracula-orange :background ,dracula-bg)
               ;; (helm-buffer-saved-out :foreground ,dracula-fg :background ,dracula-bg)
               ;; (helm-buffer-size :foreground ,dracula-fg :background ,dracula-bg)
               ;; (helm-candidate-number :foreground ,dracula-bg :background ,dracula-fg)
               ;; (helm-ff-directory :foreground ,dracula-green :background ,dracula-bg :weight bold)
               ;; (helm-ff-dotted-directory :foreground ,dracula-green :background ,dracula-bg :weight normal)
               ;; (helm-ff-executable :foreground ,dark-blue :background ,dracula-bg :weight normal)
               ;; (helm-ff-file :foreground ,dracula-fg :background ,dracula-bg :weight normal)
               ;; (helm-ff-invalid-symlink :foreground ,dracula-pink :background ,dracula-bg :weight bold)
               ;; (helm-ff-prefix :foreground ,dracula-bg :background ,dracula-pink :weight normal)
               ;; (helm-ff-symlink :foreground ,dracula-pink :background ,dracula-bg :weight bold)
               ;; (helm-grep-cmd-line :foreground ,dracula-fg :background ,dracula-bg)
               ;; (helm-grep-file :foreground ,dracula-fg :background ,dracula-bg)
               ;; (helm-grep-finish :foreground ,fg2 :background ,dracula-bg)
               ;; (helm-grep-lineno :foreground ,dracula-fg :background ,dracula-bg)
               ;; (helm-grep-match :inherit match)
               ;; (helm-grep-running :foreground ,dracula-green :background ,dracula-bg)
               ;; (helm-header :foreground ,fg2 :background ,dracula-bg :underline nil :box nil)
               ;; (helm-moccur-buffer :foreground ,dracula-green :background ,dracula-bg)
               ;; (helm-selection :background ,bg2 :underline nil)
               ;; (helm-selection-line :background ,bg2)
               ;; (helm-separator :foreground ,dracula-purple :background ,dracula-bg)
               ;; (helm-source-go-package-godoc-description :foreground ,dracula-yellow)
               ;; (helm-source-header :foreground ,dracula-pink :background ,dracula-bg :underline nil :weight bold)
               ;; (helm-time-zone-current :foreground ,dracula-orange :background ,dracula-bg)
               ;; (helm-time-zone-home :foreground ,dracula-purple :background ,dracula-bg)
               ;; (helm-visible-mark :foreground ,dracula-bg :background ,dracula-current)
               ;; ;; highlight-indentation minor mode
               ;; (highlight-indentation-face :background ,bg2)
               ;; ;; icicle
               ;; (icicle-whitespace-highlight :background ,dracula-fg)
               ;; (icicle-special-candidate :foreground ,fg2)
               ;; (icicle-extra-candidate :foreground ,fg2)
               ;; (icicle-search-main-regexp-others :foreground ,dracula-fg)
               ;; (icicle-search-current-input :foreground ,dracula-pink)
               ;; (icicle-search-context-level-8 :foreground ,dracula-orange)
               ;; (icicle-search-context-level-7 :foreground ,dracula-orange)
               ;; (icicle-search-context-level-6 :foreground ,dracula-orange)
               ;; (icicle-search-context-level-5 :foreground ,dracula-orange)
               ;; (icicle-search-context-level-4 :foreground ,dracula-orange)
               ;; (icicle-search-context-level-3 :foreground ,dracula-orange)
               ;; (icicle-search-context-level-2 :foreground ,dracula-orange)
               ;; (icicle-search-context-level-1 :foreground ,dracula-orange)
               ;; (icicle-search-main-regexp-current :foreground ,dracula-fg)
               ;; (icicle-saved-candidate :foreground ,dracula-fg)
               ;; (icicle-proxy-candidate :foreground ,dracula-fg)
               ;; (icicle-mustmatch-completion :foreground ,dracula-purple)
               ;; (icicle-multi-command-completion :foreground ,fg2 :background ,bg2)
               ;; (icicle-msg-emphasis :foreground ,dracula-green)
               ;; (icicle-mode-line-help :foreground ,fg4)
               ;; (icicle-match-highlight-minibuffer :foreground ,dracula-orange)
               ;; (icicle-match-highlight-Completions :foreground ,dracula-green)
               ;; (icicle-key-complete-menu-local :foreground ,dracula-fg)
               ;; (icicle-key-complete-menu :foreground ,dracula-fg)
               ;; (icicle-input-completion-fail-lax :foreground ,dracula-pink)
               ;; (icicle-input-completion-fail :foreground ,dracula-pink)
               ;; (icicle-historical-candidate-other :foreground ,dracula-fg)
               ;; (icicle-historical-candidate :foreground ,dracula-fg)
               ;; (icicle-current-candidate-highlight :foreground ,dracula-orange :background ,dracula-current)
               ;; (icicle-Completions-instruction-2 :foreground ,fg4)
               ;; (icicle-Completions-instruction-1 :foreground ,fg4)
               ;; (icicle-completion :foreground ,dracula-fg)
               ;; (icicle-complete-input :foreground ,dracula-orange)
               ;; (icicle-common-match-highlight-Completions :foreground ,dracula-purple)
               ;; (icicle-candidate-part :foreground ,dracula-fg)
               ;; (icicle-annotation :foreground ,fg4)
               ;; ;; icomplete
               ;; (icompletep-determined :foreground ,dracula-orange)
               ;; ;; ido
               ;; (ido-first-match
               ;;  ,@(if dracula-alternate-mode-line-and-minibuffer
               ;;        (list :weight 'normal :foreground dracula-green)
               ;;      (list :weight 'bold :foreground dracula-pink)))
               ;; (ido-only-match :foreground ,dracula-orange)
               ;; (ido-subdir :foreground ,dracula-yellow)
               ;; (ido-virtual :foreground ,dracula-cyan)
               ;; (ido-incomplete-regexp :inherit font-lock-warning-face)
               ;; (ido-indicator :foreground ,dracula-fg :background ,dracula-pink)
               ;; ;; ivy
               ;; (ivy-current-match
               ;;  ,@(if dracula-alternate-mode-line-and-minibuffer
               ;;        (list :weight 'normal :background dracula-current :foreground dracula-green)
               ;;      (list :weight 'bold :background dracula-current :foreground dracula-pink)))
               ;; ;; Highlights the background of the match.
               ;; (ivy-minibuffer-match-face-1 :background ,dracula-current)
               ;; ;; Highlights the first matched group.
               ;; (ivy-minibuffer-match-face-2 :background ,dracula-green
               ;;                              :foreground ,dracula-bg)
               ;; ;; Highlights the second matched group.
               ;; (ivy-minibuffer-match-face-3 :background ,dracula-yellow
               ;;                              :foreground ,dracula-bg)
               ;; ;; Highlights the third matched group.
               ;; (ivy-minibuffer-match-face-4 :background ,dracula-pink
               ;;                              :foreground ,dracula-bg)
               ;; (ivy-confirm-face :foreground ,dracula-orange)
               ;; (ivy-match-required-face :foreground ,dracula-red)
               ;; (ivy-subdir :foreground ,dracula-yellow)
               ;; (ivy-remote :foreground ,dracula-pink)
               ;; (ivy-virtual :foreground ,dracula-cyan)
               ;; ;; isearch
               ;; (isearch :inherit match :weight bold)
               ;; (isearch-fail :foreground ,dracula-bg :background ,dracula-orange)
               ;; ;; jde-java
               ;; (jde-java-font-lock-constant-face :foreground ,dracula-cyan)
               ;; (jde-java-font-lock-modifier-face :foreground ,dracula-pink)
               ;; (jde-java-font-lock-number-face :foreground ,dracula-fg)
               ;; (jde-java-font-lock-package-face :foreground ,dracula-fg)
               ;; (jde-java-font-lock-private-face :foreground ,dracula-pink)
               ;; (jde-java-font-lock-public-face :foreground ,dracula-pink)
               ;; ;; js2-mode
               ;; (js2-external-variable :foreground ,dracula-purple)
               ;; (js2-function-param :foreground ,dracula-cyan)
               ;; (js2-jsdoc-html-tag-delimiter :foreground ,dracula-yellow)
               ;; (js2-jsdoc-html-tag-name :foreground ,dark-blue)
               ;; (js2-jsdoc-value :foreground ,dracula-yellow)
               ;; (js2-private-function-call :foreground ,dracula-cyan)
               ;; (js2-private-member :foreground ,fg3)
               ;; ;; js3-mode
               ;; (js3-error-face :underline ,dracula-orange)
               ;; (js3-external-variable-face :foreground ,dracula-fg)
               ;; (js3-function-param-face :foreground ,dracula-pink)
               ;; (js3-instance-member-face :foreground ,dracula-cyan)
               ;; (js3-jsdoc-tag-face :foreground ,dracula-pink)
               ;; (js3-warning-face :underline ,dracula-pink)
               ;; ;; lsp
               ;; (lsp-ui-peek-peek :background ,dracula-bg)
               ;; (lsp-ui-peek-list :background ,bg2)
               ;; (lsp-ui-peek-filename :foreground ,dracula-pink :weight bold)
               ;; (lsp-ui-peek-line-number :foreground ,dracula-fg)
               ;; (lsp-ui-peek-highlight :inherit highlight :distant-foreground ,dracula-bg)
               ;; (lsp-ui-peek-header :background ,dracula-current :foreground ,fg3, :weight bold)
               ;; (lsp-ui-peek-footer :inherit lsp-ui-peek-header)
               ;; (lsp-ui-peek-selection :inherit match)
               ;; (lsp-ui-sideline-symbol :foreground ,fg4 :box (:line-width -1 :color ,fg4) :height 0.99)
               ;; (lsp-ui-sideline-current-symbol :foreground ,dracula-fg :weight ultra-bold
               ;;                                 :box (:line-width -1 :color ,dracula-fg) :height 0.99)
               ;; (lsp-ui-sideline-code-action :foreground ,dracula-yellow)
               ;; (lsp-ui-sideline-symbol-info :slant italic :height 0.99)
               ;; (lsp-ui-doc-background :background ,dracula-bg)
               ;; (lsp-ui-doc-header :foreground ,dracula-bg :background ,dracula-cyan)
               ;; magit
               (magit-branch-local :foreground ,purple)
               (magit-branch-remote :foreground ,green)
               (magit-refname :foreground ,blue)
               (magit-tag :foreground ,orange)
               (magit-hash :foreground ,bg4)
               (magit-dimmed :foreground ,bg4)
               (magit-section-heading :foreground ,pink :weight bold)
               (magit-section-highlight :background ,bg2 :extend t)
               (magit-diff-context :foreground ,fg3 :extend t)
               (magit-diff-context-highlight :inherit magit-section-highlight
                                             :foreground ,fg)
               (magit-diff-revision-summary :foreground ,teal
                                            :background ,bg
                                            :weight bold)
               (magit-diff-revision-summary-highlight :inherit magit-section-highlight
                                                      :foreground ,teal
                                                      :weight bold)
               (magit-diff-added :background ,bg :foreground ,green)
               (magit-diff-added-highlight :background ,bg2
                                           :foreground ,green)
               (magit-diff-removed :background ,bg :foreground ,red)
               (magit-diff-removed-highlight :background ,bg2
                                             :foreground ,red)
               (magit-diff-file-heading :foreground ,fg)
               (magit-diff-file-heading-highlight :inherit magit-section-highlight
                                                  :weight bold)
               (magit-diff-file-heading-selection
                :inherit magit-diff-file-heading-highlight
                :foreground ,pink)
               (magit-diff-hunk-heading :inherit magit-diff-context
                                        :background ,bg2)
               (magit-diff-hunk-heading-highlight
                :inherit magit-diff-context-highlight
                :weight bold)
               (magit-diff-hunk-heading-selection
                :inherit magit-diff-hunk-heading-highlight
                :foreground ,pink)
               (magit-diff-lines-heading
                :inherit magit-diff-hunk-heading-highlight
                :foreground ,pink)
               (magit-diff-lines-boundary :background ,pink)
               (magit-diffstat-added :foreground ,green)
               (magit-diffstat-removed :foreground ,red)
               (magit-log-author :foreground ,bg4)
               (magit-log-date :foreground ,bg4)
               (magit-log-graph :foreground ,blue)
               (magit-process-ng :foreground ,orange :weight bold)
               (magit-process-ok :foreground ,green :weight bold)
               (magit-signature-good :foreground ,green)
               (magit-signature-bad :foreground ,red :weight bold)
               (magit-signature-untrusted :foreground ,teal)
               (magit-signature-expired :foreground ,orange)
               (magit-signature-revoked :foreground ,purple)
               (magit-signature-error :foreground ,red)
               (magit-cherry-unmatched :foreground ,teal)
               (magit-cherry-equivalent :foreground ,purple)
               ;; window-divider
               (window-divider :foreground ,fg)
               ;; ;; markdown
               ;; (markdown-blockquote-face :foreground ,dracula-yellow
               ;;                           :slant italic)
               ;; (markdown-code-face :foreground ,dracula-orange)
               ;; (markdown-footnote-face :foreground ,dark-blue)
               ;; ;; (markdown-header-face :weight normal)
               ;; (markdown-header-face-1
               ;;  :inherit bold :foreground ,dracula-pink
               ;;  ,@(when dracula-enlarge-headings
               ;;      (list :height dracula-height-title-1)))
               ;; (markdown-header-face-2
               ;;  :inherit bold :foreground ,dracula-purple
               ;;  ,@(when dracula-enlarge-headings
               ;;      (list :height dracula-height-title-2)))
               ;; (markdown-header-face-3
               ;;  :foreground ,dracula-green
               ;;  ,@(when dracula-enlarge-headings
               ;;      (list :height dracula-height-title-3)))
               ;; (markdown-header-face-4 :foreground ,dracula-yellow)
               ;; (markdown-header-face-5 :foreground ,dracula-cyan)
               ;; (markdown-header-face-6 :foreground ,dracula-orange)
               ;; (markdown-header-face-7 :foreground ,dark-blue)
               ;; (markdown-header-face-8 :foreground ,dracula-fg)
               ;; (markdown-inline-code-face :foreground ,dracula-green)
               ;; (markdown-plain-url-face :inherit link)
               ;; (markdown-pre-face :foreground ,dracula-orange)
               ;; (markdown-table-face :foreground ,dracula-purple)
               ;; (markdown-list-face :foreground ,dracula-cyan)
               ;; (markdown-language-keyword-face :foreground ,dracula-comment)
               ;; ;; message
               ;; (message-header-to :foreground ,dracula-fg :weight bold)
               ;; (message-header-cc :foreground ,dracula-fg :bold bold)
               ;; (message-header-subject :foreground ,dracula-orange)
               ;; (message-header-newsgroups :foreground ,dracula-purple)
               ;; (message-header-other :foreground ,dracula-purple)
               ;; (message-header-name :foreground ,dracula-green)
               ;; (message-header-xheader :foreground ,dracula-cyan)
               ;; (message-separator :foreground ,dracula-cyan :slant italic)
               ;; (message-cited-text :foreground ,dracula-purple)
               ;; (message-cited-text-1 :foreground ,dracula-purple)
               ;; (message-cited-text-2 :foreground ,dracula-orange)
               ;; (message-cited-text-3 :foreground ,dracula-comment)
               ;; (message-cited-text-4 :foreground ,fg2)
               ;; (message-mml :foreground ,dracula-green :weight normal)
               ;; mini-modeline
               (mini-modeline-mode-line :inherit :height 0.1 :box nil)
               ;; ;; mu4e
               ;; (mu4e-unread-face :foreground ,dracula-pink :weight normal)
               ;; (mu4e-view-url-number-face :foreground ,dracula-purple)
               ;; (mu4e-highlight-face :background ,dracula-bg
               ;;                      :foreground ,dracula-yellow
               ;;                      :extend t)
               ;; (mu4e-header-highlight-face :background ,dracula-current
               ;;                             :foreground ,dracula-fg
               ;;                             :underline nil :weight bold
               ;;                             :extend t)
               ;; (mu4e-header-key-face :inherit message-mml)
               ;; (mu4e-header-marks-face :foreground ,dracula-purple)
               ;; (mu4e-cited-1-face :foreground ,dracula-purple)
               ;; (mu4e-cited-2-face :foreground ,dracula-orange)
               ;; (mu4e-cited-3-face :foreground ,dracula-comment)
               ;; (mu4e-cited-4-face :foreground ,fg2)
               ;; (mu4e-cited-5-face :foreground ,fg3)
               ;; ;; neotree
               ;; (neo-banner-face :foreground ,dracula-orange :weight bold)
               ;; ;;(neo-button-face :underline nil)
               ;; (neo-dir-link-face :foreground ,dracula-purple)
               ;; (neo-expand-btn-face :foreground ,dracula-fg)
               ;; (neo-file-link-face :foreground ,dracula-cyan)
               ;; (neo-header-face :background ,dracula-bg
               ;;                  :foreground ,dracula-fg
               ;;                  :weight bold)
               ;; (neo-root-dir-face :foreground ,dracula-purple :weight bold)
               ;; (neo-vc-added-face :foreground ,dracula-orange)
               ;; (neo-vc-conflict-face :foreground ,dracula-red)
               ;; (neo-vc-default-face :inherit neo-file-link-face)
               ;; (neo-vc-edited-face :foreground ,dracula-orange)
               ;; (neo-vc-ignored-face :foreground ,dracula-comment)
               ;; (neo-vc-missing-face :foreground ,dracula-red)
               ;; (neo-vc-needs-merge-face :foreground ,dracula-red
               ;;                          :weight bold)
               ;; ;;(neo-vc-needs-update-face :underline t)
               ;; ;;(neo-vc-removed-face :strike-through t)
               ;; (neo-vc-unlocked-changes-face :foreground ,dracula-red)
               ;; ;;(neo-vc-unregistered-face nil)
               ;; (neo-vc-up-to-date-face :foreground ,dracula-green)
               ;; (neo-vc-user-face :foreground ,dracula-purple)
               ;; org
               (org-agenda-date :foreground ,blue :underline t)
               (org-agenda-dimmed-todo-face :foreground ,bg4)
               (org-agenda-done :foreground ,green)
               (org-agenda-structure :foreground ,purple)
               (org-block :background ,bg2)
               (org-code :background ,bg2)
               (org-column :background ,bg3)
               (org-column-title :inherit org-column :weight bold :underline t)
               (org-date :foreground ,teal :underline t)
               (org-document-info :foreground ,blue)
               (org-document-info-keyword :foreground ,bg4)
               (org-document-title :weight bold :foreground ,fg)
               (org-done :foreground ,green)
               (org-ellipsis :foreground ,bg4)
               (org-footnote :foreground ,blue)
               (org-formula :foreground ,pink)
               (org-headline-done :foreground ,bg4
                                  :weight normal :strike-through t)
               (org-hide :foreground ,bg :background ,bg)
               (org-level-1 :inherit bold)
               (org-level-2 :inherit bold)
               (org-level-3 :weight bold)
               (org-level-4 :weight bold)
               (org-level-5 :weight bold)
               (org-level-6 :weight bold)
               (org-level-7 :weight bold)
               (org-level-8 :weight bold)
               (org-link :foreground ,teal :underline t)
               (org-priority :foreground ,teal)
               (org-quote :foreground ,green :slant italic)
               (org-scheduled :foreground ,green)
               (org-scheduled-previously :foreground ,orange)
               (org-scheduled-today :foreground ,green)
               (org-sexp-date :foreground ,fg3)
               (org-special-keyword :foreground ,orange)
               (org-table :foreground ,purple)
               (org-tag :foreground ,pink :weight bold)
               (org-todo :foreground ,orange :weight bold)
               (org-upcoming-deadline :foreground ,red)
               (org-verbatim :inherit org-quote)
               (org-warning :weight bold :foreground ,orange)
               ;; ;; outline
               ;; (outline-1 :foreground ,dracula-pink)
               ;; (outline-2 :foreground ,dracula-purple)
               ;; (outline-3 :foreground ,dracula-green)
               ;; (outline-4 :foreground ,dracula-yellow)
               ;; (outline-5 :foreground ,dracula-cyan)
               ;; (outline-6 :foreground ,dracula-orange)
               ;; ;; perspective
               ;; (persp-selected-face :weight bold :foreground ,dracula-pink)
               ;; ;; powerline
               ;; (powerline-active1 :background ,dracula-bg :foreground ,dracula-pink)
               ;; (powerline-active2 :background ,dracula-bg :foreground ,dracula-pink)
               ;; (powerline-inactive1 :background ,bg2 :foreground ,dracula-purple)
               ;; (powerline-inactive2 :background ,bg2 :foreground ,dracula-purple)
               ;; (powerline-evil-base-face :foreground ,bg2)
               ;; (powerline-evil-emacs-face :inherit powerline-evil-base-face :background ,dracula-yellow)
               ;; (powerline-evil-insert-face :inherit powerline-evil-base-face :background ,dracula-cyan)
               ;; (powerline-evil-motion-face :inherit powerline-evil-base-face :background ,dracula-purple)
               ;; (powerline-evil-normal-face :inherit powerline-evil-base-face :background ,dracula-green)
               ;; (powerline-evil-operator-face :inherit powerline-evil-base-face :background ,dracula-pink)
               ;; (powerline-evil-replace-face :inherit powerline-evil-base-face :background ,dracula-red)
               ;; (powerline-evil-visual-face :inherit powerline-evil-base-face :background ,dracula-orange)
               ;; rainbow-delimiters
               (rainbow-delimiters-depth-1-face :foreground ,pink :weight bold)
               (rainbow-delimiters-depth-2-face :foreground ,purple :weight bold)
               (rainbow-delimiters-depth-3-face :foreground ,teal :weight bold)
               (rainbow-delimiters-depth-4-face :foreground ,blue :weight bold)
               (rainbow-delimiters-depth-5-face :foreground ,green :weight bold)
               (rainbow-delimiters-depth-6-face :foreground ,orange :weight bold)
               (rainbow-delimiters-depth-7-face :foreground ,red :weight bold)
               (rainbow-delimiters-depth-8-face :foreground ,orange :weight bold)
               (rainbow-delimiters-unmatched-face :foreground ,red :weight bold)
               ;; ;; rpm-spec
               ;; (rpm-spec-dir-face :foreground ,dracula-green)
               ;; (rpm-spec-doc-face :foreground ,dracula-pink)
               ;; (rpm-spec-ghost-face :foreground ,dracula-purple)
               ;; (rpm-spec-macro-face :foreground ,dracula-yellow)
               ;; (rpm-spec-obsolete-tag-face :inherit font-lock-warning-face)
               ;; (rpm-spec-package-face :foreground ,dracula-purple)
               ;; (rpm-spec-section-face :foreground ,dracula-yellow)
               ;; (rpm-spec-tag-face :foreground ,dracula-cyan)
               ;; (rpm-spec-var-face :foreground ,dracula-orange)
               ;; ;; rst (reStructuredText)
               ;; (rst-level-1 :foreground ,dracula-pink :weight bold)
               ;; (rst-level-2 :foreground ,dracula-purple :weight bold)
               ;; (rst-level-3 :foreground ,dracula-green)
               ;; (rst-level-4 :foreground ,dracula-yellow)
               ;; (rst-level-5 :foreground ,dracula-cyan)
               ;; (rst-level-6 :foreground ,dracula-orange)
               ;; (rst-level-7 :foreground ,dark-blue)
               ;; (rst-level-8 :foreground ,dracula-fg)
               ;; ;; selectrum-mode
               ;; (selectrum-current-candidate :weight bold)
               ;; (selectrum-primary-highlight :foreground ,dracula-pink)
               ;; (selectrum-secondary-highlight :foreground ,dracula-green)
               ;; ;; show-paren
               ;; (show-paren-match-face :background unspecified
               ;;                        :foreground ,dracula-cyan
               ;;                        :weight bold)
               ;; (show-paren-match :background unspecified
               ;;                   :foreground ,dracula-cyan
               ;;                   :weight bold)
               ;; (show-paren-match-expression :inherit match)
               ;; (show-paren-mismatch :inherit font-lock-warning-face)
               ;; ;; shr
               ;; (shr-h1 :foreground ,dracula-pink :weight bold :height 1.3)
               ;; (shr-h2 :foreground ,dracula-purple :weight bold)
               ;; (shr-h3 :foreground ,dracula-green :slant italic)
               ;; (shr-h4 :foreground ,dracula-yellow)
               ;; (shr-h5 :foreground ,dracula-cyan)
               ;; (shr-h6 :foreground ,dracula-orange)
               ;; ;; slime
               ;; (slime-repl-inputed-output-face :foreground ,dracula-purple)
               ;; solaire-mode
               (solaire-default-face :background ,bg2)
               ;; ;; spam
               ;; (spam :inherit gnus-summary-normal-read :foreground ,dracula-orange
               ;;       :strike-through t :slant oblique)
               ;; ;; speedbar (and sr-speedbar)
               ;; (speedbar-button-face :foreground ,dracula-green)
               ;; (speedbar-file-face :foreground ,dracula-cyan)
               ;; (speedbar-directory-face :foreground ,dracula-purple)
               ;; (speedbar-tag-face :foreground ,dracula-yellow)
               ;; (speedbar-selected-face :foreground ,dracula-pink)
               ;; (speedbar-highlight-face :inherit match)
               ;; (speedbar-separator-face :background ,dracula-bg
               ;;                          :foreground ,dracula-fg
               ;;                          :weight bold)
               ;; ;; tab-bar & tab-line (since Emacs 27.1)
               ;; (tab-bar :foreground ,dracula-purple :background ,dracula-current
               ;;          :inherit variable-pitch)
               ;; (tab-bar-tab :foreground ,dracula-pink :background ,dracula-bg
               ;;              :box (:line-width 2 :color ,dracula-bg :style nil))
               ;; (tab-bar-tab-inactive :foreground ,dracula-purple :background ,bg2
               ;;                       :box (:line-width 2 :color ,bg2 :style nil))
               ;; (tab-line :foreground ,dracula-purple :background ,dracula-current
               ;;           :height 0.92 :inherit variable-pitch)
               ;; (tab-line-close-highlight :foreground ,dracula-red)
               ;; (tab-line-highlight :weight bold)
               ;; (tab-line-tab :foreground ,dracula-purple :background ,bg2
               ;;               :box (:line-width 4 :color ,bg2 :style nil))
               ;; (tab-line-tab-current :foreground ,dracula-pink :background ,dracula-bg
               ;;                       :box (:line-width 4 :color ,dracula-bg :style nil)
               ;;                       :weight bold)
               ;; (tab-line-tab-group :background ,dracula-comment)
               ;; (tab-line-tab-inactive :inherit tab-line-tab)
               ;; (tab-line-tab-inactive-alternate :background ,bg3)
               ;; (tab-line-tab-modified :slant italic)
               ;; (tab-line-tab-special :foreground ,dracula-green)
               ;; ;; telephone-line
               ;; (telephone-line-accent-active :background ,dracula-bg :foreground ,dracula-pink)
               ;; (telephone-line-accent-inactive :background ,bg2 :foreground ,dracula-purple)
               ;; (telephone-line-unimportant :background ,dracula-bg :foreground ,dracula-comment)
               ;; term
               (term :foreground ,fg :background ,bg)
               (term-color-black :foreground ,bg :background ,bg)
               (term-color-blue :foreground ,blue :background ,blue)
               (term-color-cyan :foreground ,teal :background ,teal)
               (term-color-green :foreground ,green :background ,green)
               (term-color-magenta :foreground ,pink :background ,pink)
               (term-color-red :foreground ,red :background ,red)
               (term-color-white :foreground ,fg :background ,fg)
               (term-color-yellow :foreground ,orange :background ,orange)
               ;; tree-sitter
               (tree-sitter-hl-face:attribute :inherit font-lock-constant-face)
               (tree-sitter-hl-face:comment :inherit font-lock-comment-face)
               (tree-sitter-hl-face:constant :inherit font-lock-constant-face)
               (tree-sitter-hl-face:constant.builtin :inherit font-lock-builtin-face)
               (tree-sitter-hl-face:constructor :inherit font-lock-constant-face)
               (tree-sitter-hl-face:escape :foreground ,pink)
               (tree-sitter-hl-face:function :inherit font-lock-function-name-face)
               (tree-sitter-hl-face:function.builtin :inherit font-lock-builtin-face)
               (tree-sitter-hl-face:function.call :inherit font-lock-function-name-face
                                                  :weight normal)
               (tree-sitter-hl-face:function.macro :inherit font-lock-preprocessor-face)
               (tree-sitter-hl-face:function.special :inherit font-lock-preprocessor-face)
               (tree-sitter-hl-face:keyword :inherit font-lock-keyword-face)
               (tree-sitter-hl-face:punctuation :foreground ,pink)
               (tree-sitter-hl-face:punctuation.bracket :foreground ,blue)
               (tree-sitter-hl-face:punctuation.delimiter :foreground ,blue)
               (tree-sitter-hl-face:punctuation.special :foreground ,pink)
               (tree-sitter-hl-face:string :inherit font-lock-string-face)
               (tree-sitter-hl-face:string.special :foreground ,teal)
               (tree-sitter-hl-face:tag :inherit font-lock-keyword-face)
               (tree-sitter-hl-face:type :inherit font-lock-type-face)
               (tree-sitter-hl-face:type.parameter :foreground ,pink)
               (tree-sitter-hl-face:variable :inherit font-lock-variable-name-face)
               (tree-sitter-hl-face:variable.parameter :inherit tree-sitter-hl-face:variable
                                                       :weight normal)
               ;; ;; undo-tree
               ;; (undo-tree-visualizer-current-face :foreground ,dracula-orange)
               ;; (undo-tree-visualizer-default-face :foreground ,fg2)
               ;; (undo-tree-visualizer-register-face :foreground ,dracula-purple)
               ;; (undo-tree-visualizer-unmodified-face :foreground ,dracula-fg)
               ;; ;; web-mode
               ;; (web-mode-builtin-face :inherit font-lock-builtin-face)
               ;; (web-mode-comment-face :inherit font-lock-comment-face)
               ;; (web-mode-constant-face :inherit font-lock-constant-face)
               ;; (web-mode-css-property-name-face :inherit font-lock-constant-face)
               ;; (web-mode-doctype-face :inherit font-lock-comment-face)
               ;; (web-mode-function-name-face :inherit font-lock-function-name-face)
               ;; (web-mode-html-attr-name-face :foreground ,dracula-purple)
               ;; (web-mode-html-attr-value-face :foreground ,dracula-green)
               ;; (web-mode-html-tag-face :foreground ,dracula-pink :weight bold)
               ;; (web-mode-keyword-face :foreground ,dracula-pink)
               ;; (web-mode-string-face :foreground ,dracula-yellow)
               ;; (web-mode-type-face :inherit font-lock-type-face)
               ;; (web-mode-warning-face :inherit font-lock-warning-face)
               ;; ;; which-func
               ;; (which-func :inherit font-lock-function-name-face)
               ;; ;; which-key
               ;; (which-key-key-face :inherit font-lock-builtin-face)
               ;; (which-key-command-description-face :inherit default)
               ;; (which-key-separator-face :inherit font-lock-comment-delimiter-face)
               ;; (which-key-local-map-description-face :foreground ,dracula-green)
               ;; ;; whitespace
               ;; (whitespace-big-indent :background ,dracula-red :foreground ,dracula-red)
               ;; (whitespace-empty :background ,dracula-orange :foreground ,dracula-red)
               ;; (whitespace-hspace :background ,dracula-current :foreground ,dracula-comment)
               ;; (whitespace-indentation :background ,dracula-orange :foreground ,dracula-red)
               ;; (whitespace-line :background ,dracula-bg :foreground ,dracula-pink)
               ;; (whitespace-newline :foreground ,dracula-comment)
               ;; (whitespace-space :background ,dracula-bg :foreground ,dracula-comment)
               ;; (whitespace-space-after-tab :background ,dracula-orange :foreground ,dracula-red)
               ;; (whitespace-space-before-tab :background ,dracula-orange :foreground ,dracula-red)
               ;; (whitespace-tab :background ,bg2 :foreground ,dracula-comment)
               ;; (whitespace-trailing :inherit trailing-whitespace)
               ;; ;; yard-mode
               ;; (yard-tag-face :inherit font-lock-builtin-face)
               ;; (yard-directive-face :inherit font-lock-builtin-face)
               )))

  (apply #'custom-theme-set-faces
         'carbon-light
         (let ((expand-with-func
                (lambda (func spec)
                  (let (reduced-color-list)
                    (dolist (col colors reduced-color-list)
                      (push (list (car col) (funcall func col))
                            reduced-color-list))
                    (eval `(let ,reduced-color-list
                             (backquote ,spec))))))
               whole-theme)
           (pcase-dolist (`(,face . ,spec) faces)
             (push `(,face
                     ((((min-colors 16777216)) ; fully graphical envs
                       ,(funcall expand-with-func 'cadr spec))
                      (((min-colors 256))      ; terminal withs 256 colors
                       ;; ,(if dracula-use-24-bit-colors-on-256-colors-terms
                       ;;      (funcall expand-with-func 'cadr spec)
                       ;;    (funcall expand-with-func 'caddr spec))
                       ;;
                       )
                      (t                       ; should be only tty-like envs
                       ,(funcall expand-with-func 'cadddr spec))))
                   whole-theme))
           whole-theme)))


;;;###autoload
(when load-file-name
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

(provide-theme 'carbon-light)

;; Local Variables:
;; indent-tabs-mode: nil
;; End:

;;; carbon-light-theme.el ends here
