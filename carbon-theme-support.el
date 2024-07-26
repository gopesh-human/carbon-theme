;;; carbon-theme-support.el --- Carbon theme -*- lexical-binding: t -*-

;; Copyright © 2024 Gopesh Sharma

;; Permission is hereby granted, free of charge, to any person obtaining a copy of this software
;; and associated documentation files (the “Software”), to deal in the Software without
;; restriction, including without limitation the rights to use, copy, modify, merge, publish,
;; distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the
;; Software is furnished to do so, subject to the following conditions:

;; The above copyright notice and this permission notice shall be included in all copies or
;; substantial portions of the Software.

;; THE SOFTWARE IS PROVIDED “AS IS”, WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING
;; BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM,
;; DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
;; FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

;;; Code:
(require 'disp-table)
(require 'cl-macs)

(defgroup carbon nil
  "Carbon"
  :group 'convenience)

(defgroup carbon-theme nil
  "Carbon Theme"
  :group 'carbon)

(defgroup carbon-theme-light nil
  "Light color palette"
  :group 'carbon-theme)

(defgroup carbon-theme-dark nil
  "Dark color palette"
  :group 'carbon-theme)

(defgroup carbon-theme-fonts nil
  "Font stack"
  :group 'carbon-theme)

(defcustom carbon-fonts-use nil
  "Whether to use font stack"
  :type 'boolean :group 'carbon-theme-fonts)

(defcustom carbon-window-divider-show nil
  "Whether to show the vertical window-divider"
  :type 'boolean :group 'carbon-theme)

(defface carbon-mono
  '((t (:family "Roboto Mono"
        :height 140
        :weight light)))
  "Default monospaced font (Roboto Mono Light, 14pt)."
  :group 'carbon-theme-fonts)

(defface carbon-mono-alt
  '((t (:family "Fira Code"
        :height 140
        :weight light)))
  "Alternative monospaced font (Fira Code Light, 14pt)."
  :group 'carbon-theme-fonts)

(defface carbon-sans
  '((t (:family "Roboto"
        :height 140
        :weight light)))
  "Default proportional sans font (Roboto Light, 14pt)."
  :group 'carbon-theme-fonts)

(defface carbon-serif
  '((t (:family "Roboto Slab"
        :height 140
        :weight light)))
  "Default proportional serif font (Roboto Slab Light, 14pt)."
  :group 'carbon-theme-fonts)

(defface carbon-italic
  '((t (:family "Victor Mono"
        :slant italic
        :height 140
        :weight regular)))
  "Default italic font (Victor Mono Italic Light, 14pt)."
  :group 'carbon-theme-fonts)

(defcustom carbon-light-foreground "#37474F" ;; Blue Grey / L800
  "Default foreground color"
  :type 'color :group 'carbon-theme-light)

(defcustom carbon-light-background "#FFFFFF" ;; White
  "Default background color"
  :type 'color :group 'carbon-theme-light)

(defcustom carbon-light-highlight "#FAFAFA" ;; Very Light Grey
  "Highlight color is used to highlight part of the screen."
  :type 'color :group 'carbon-theme-light)

(defcustom carbon-light-subtle "#ECEFF1" ;; Blue Grey / L50
  "Subtle color is used to suggest a physical area on the screen."
  :type 'color :group 'carbon-theme-light)

(defcustom carbon-light-faded "#90A4AE" ;; Blue Grey / L300
  "Faded face is for information that are less important."
  :type 'color :group 'carbon-theme-light)

(defcustom carbon-light-salient "#673AB7" ;; Deep Purple / L500
  "Salient color is used for information that are important."
  :type 'color :group 'carbon-theme-light)

(defcustom carbon-light-strong "#263238" ;; Blue Grey / L900
  "Strong color is used for information of a structural nature."
  :type 'color :group 'carbon-theme-light)

(defcustom carbon-light-popout "#FFAB91" ;; Deep Orange / L200
  "Popout colour is used for information that needs attention."
  :type 'color :group 'carbon-theme-light)

(defcustom carbon-light-critical "#FF6F00" ;; Amber / L900
  "Critical face is for information that requires immediate action."
  :type 'color :group 'carbon-theme-light)

(defcustom carbon-dark-foreground "#f4f4f4" ;; Snow Storm 3  / nord  6
  "Default foreground color"
  :type 'color :group 'carbon-theme-dark)

(defcustom carbon-dark-background "#000000" ;; Polar Night 0 / nord  0
  "Default background color"
  :type 'color :group 'carbon-theme-dark)

(defcustom carbon-dark-highlight "#262626" ;; Polar Night 1 / nord  1
  "Highdark color is used to highdark part of the screen."
  :type 'color :group 'carbon-theme-dark)

(defcustom carbon-dark-subtle "#161616" ;; Polar Night 2 / nord  2
  "Subtle color is used to suggest a physical area on the screen."
  :type 'color :group 'carbon-theme-dark)

(defcustom carbon-dark-faded "#393939" ;;
  "Faded face is for information that are less important."
  :type 'color :group 'carbon-theme-dark)

(defcustom carbon-dark-salient "#ee538b" ;; Frost         / nord  9
  "Salient color is used for information that are important."
  :type 'color :group 'carbon-theme-dark)

(defcustom carbon-dark-strong "#FFFFFF" ;; White
  "Strong color is used for information of a structural nature."
  :type 'color :group 'carbon-theme-dark)

(defcustom carbon-dark-popout "#be95ff" ;; Aurora        / nord 12
  "Popout colour is used for information that needs attention."
  :type 'color :group 'carbon-theme-dark)

(defcustom carbon-dark-critical  "#da1e28" ;; Aurora        / nord 11
  "Critical face is for information that requires immediate action."
  :type 'color :group 'carbon-theme-dark)

(defface carbon-critical nil
  "Critical face is for information that requires immediate action.
It should be of high constrast when compared to other faces. This
can be realized (for example) by setting an intense background
color, typically a shade of red. It must be used scarcely."
  :group nil)

(defface carbon-critical-i nil
  "Critical face inversed."
  :group nil)

(defface carbon-popout nil
  "Popout face is used for information that needs attention.
To achieve such effect, the hue of the face has to be
sufficiently different from other faces such that it attracts
attention through the popout effect."
  :group nil)

(defface carbon-popout-i nil
  "Popout face inversed."
  :group nil)

(defface carbon-strong nil
  "Strong face is used for information of a structural nature.
It has to be the same color as the default color and only the
weight differs by one level (e.g., light/regular or
regular/bold). IT is generally used for titles, keywords,
directory, etc."
  :group nil)

(defface carbon-strong-i nil
  "Strong face inversed."
  :group nil)

(defface carbon-salient nil
  "Salient face is used for information that are important.
To suggest the information is of the same nature but important,
the face uses a different hue with approximately the same
intensity as the default face. This is typically used for links."
  :group nil)

(defface carbon-salient-i nil
  "Strong face inversed."
  :group nil)

(defface carbon-faded nil
  "Faded face is for information that are less important.
It is made by using the same hue as the default but with a lesser
intensity than the default. It can be used for comments,
secondary information and also replace italic (which is generally
abused anyway)."
  :group nil)

(defface carbon-faded-i nil
  "Faded face inversed."
  :group nil)

(defface carbon-subtle nil
  "Subtle face is used to suggest a physical area on the screen.
It is important to not disturb too strongly the reading of
information and this can be made by setting a very light
background color that is barely perceptible."
  :group nil)

(defface carbon-subtle-i nil
  "Subtle face inversed."
  :group nil)

(defface carbon-default nil
  "Default face."
  :group nil)

(defface carbon-default-i nil
  "Default face inversed."
  :group nil)

(defun carbon-mode ()
  "Defaults settings for carbon (optional)"
  (interactive)

  ;; Use carbon fonts
  (setq carbon-fonts-use t)

  ;; No startup  screen
  (setq inhibit-startup-screen t)

  ;; No startup message
  (setq inhibit-startup-message t)
  (setq inhibit-startup-echo-area-message t)

  ;; No message in scratch buffer
  (setq initial-scratch-message nil)

  ;; Initial buffer
  (setq initial-buffer-choice nil)

  ;; No frame title
  (setq frame-title-format nil)

  ;; No file dialog
  (setq use-file-dialog nil)

  ;; No dialog box
  (setq use-dialog-box nil)

  ;; No popup windows
  (setq pop-up-windows nil)

  ;; No empty line indicators
  (setq indicate-empty-lines nil)

  ;; No cursor in inactive windows
  (setq cursor-in-non-selected-windows nil)

  ;; Text mode is initial mode
  (setq initial-major-mode 'text-mode)

  ;; Text mode is default major mode
  (setq default-major-mode 'text-mode)

  ;; Moderate font lock
  (setq font-lock-maximum-decoration t)

  ;; No limit on font lock (obsolete)
  ;; (setq font-lock-maximum-size nil)

  ;; No line break space points
  (setq auto-fill-mode nil)

  ;; Fill column at 80
  (setq fill-column 80)

  ;; Bar cursor
  (setq-default cursor-type '(hbar .  2))
  (setq-default cursor-in-non-selected-windows nil)
  (setq blink-cursor-mode nil)

  ;; No tooltips
  ;; (tooltip-mode -1)

  ;; No scroll bars
  ;; (scroll-bar-mode -1)

  ;; No toolbar
  ;; (tool-bar-mode -1)

  ;; Default frame settings
  (setq default-frame-alist
        (append (list
                 '(min-height . 1)  '(height . 45)
                 '(min-width  . 1)  '(width  . 81)
                 '(vertical-scroll-bars . nil)
                 '(internal-border-width . 24)
                 '(left-fringe . 0)
                 '(right-fringe . 0)
                 '(undecorated-round . t) ;; emacs-plu@29 only
                 '(scroll-bar-mode . -1)
                 '(tool-bar-lines . 0)
                 '(menu-bar-lines . 0))))

  ;; Line spacing (in pixels)
  ;; (setq line-spacing 0)

  ;; Vertical window divider
  (setq window-divider-default-right-width 24)
  (setq window-divider-default-places 'right-only)
  (window-divider-mode 1)

  ;; Nicer glyphs for continuation and wrap
  (set-display-table-slot standard-display-table
                          'truncation (make-glyph-code ?… 'carbon-faded))
  (set-display-table-slot standard-display-table
                          'wrap (make-glyph-code ?- 'carbon-faded))

  ;; Nerd font for glyph icons
  (let ((roboto-nerd (font-spec :name "RobotoMono Nerd Font")))
    (if (find-font roboto-nerd)
        (set-fontset-font t '(#xe000 . #xffdd) roboto-nerd)
      (message "Roboto Mono Nerd font has not been found on your system"))))


;; (defun inherit (face &optional inherit)
;;   "Extract face properties as a property list"

;;   (let ((tags (list :family :foundry :width :height :weight :slant :underline
;;                     :overline :strike-through :box :inverse-video :foreground
;;                     :background :stipple :extend :inherit))
;;  (properties))
;;     (if inherit
;;  `(:inherit ,face)
;;       (progn
;;  (dolist (tag tags)
;;    (let ((attribute (face-attribute face tag)))
;;     (when (not (eq attribute 'unspecified))
;;       (push attribute properties)
;;       (push tag properties)))))
;;       properties)))

(defun carbon-new-frame (&optional mode)
  "This funcion creates a new frame in light or dark MODE."

  (interactive)
  (let ((mode (or mode (frame-parameter nil 'background-mode)))
        (background-mode frame-background-mode)
        (selected-frame (selected-frame))
        (carbon-theme-frame-only (make-frame-command)))
    (carbon-theme nil mode)))


(defun carbon-frame-list-advice-selected (_frames)
  (list (selected-frame)))

(defun carbon-frame-list-advice-normal (frames)
  (seq-filter (lambda (f) (not (frame-parameter f 'carbon-theme-standalone))) frames))

(defun carbon-frame-enable (mode)
  "Enable carbon MODE for the current frame only."
  (let ((frame (selected-frame))
        (frame-background-mode mode))
    (set-frame-parameter frame 'carbon-theme-standalone mode)
    (frame-set-background-mode frame)
    (advice-add 'frame-list :filter-return #'carbon-frame-list-advice-selected)
    (enable-theme 'carbon)
    (advice-remove 'frame-list #'carbon-frame-list-advice-selected)))

(defun carbon-frame-dark ()
  "Load the carbon dark theme on current frame."
  (interactive)
  (carbon-frame-enable 'dark))

(defun carbon-frame-light ()
  "Load the carbon light theme on current frame."
  (interactive)
  (carbon-frame-enable 'light))

(defun carbon-theme-frame-toggle ()
  "Toggle theme on current frame only."
  (interactive)
  (if (eq (or (frame-parameter (selected-frame) 'carbon-theme-standalone) frame-background-mode) 'light)
      (carbon-frame-dark)
    (carbon-frame-light)))

(defun carbon-enable (mode)
  "Enable carbon MODE all other frames"
  (advice-add 'frame-list :filter-return #'carbon-frame-list-advice-normal)
  (carbon-theme 'carbon mode)
  (enable-theme 'carbon)
  (advice-remove 'frame-list #'carbon-frame-list-advice-normal))

(defun carbon-dark ()
  "Load the carbon dark theme on current frame."
  (interactive)
  (carbon-enable 'dark))

(defun carbon-light ()
  "Load the carbon light theme on current frame."
  (interactive)
  (carbon-enable 'light))

(defun carbon-theme-toggle ()
  "Toggle theme on all frames."
  (interactive)
  (if (eq frame-background-mode 'light)
      (carbon-dark)
    (carbon-light)))

(defun carbon-theme (theme mode)
  "Apply the carbon THEME according to MODE which can be 'dark or 'light."

  ;; (message (format "Theme applied: %s" mode))

  (advice-add 'frame-list :filter-return #'carbon-frame-list-advice-normal)

  (let ((light     '((background light)))
        (dark      '((background dark))))


    (setq default-frame-alist
          (assq-delete-all 'foreground-color
                           (assq-delete-all 'background-color
                                            (assq-delete-all 'background-mode default-frame-alist))))
    (add-to-list 'default-frame-alist `(background-mode . ,mode))
    (add-to-list 'default-frame-alist `(background-color . ,(if (eq mode 'light)
                                                                carbon-light-background
                                                              carbon-dark-background)))
    (add-to-list 'default-frame-alist `(foreground-color . ,(if (eq mode 'light)
                                                                carbon-light-foreground
                                                              carbon-dark-foreground)))
    (custom-theme-set-variables theme '(widget-image-enable nil)
                                '(x-underline-at-descent-line t))
    (setq frame-background-mode mode)
    (mapc #'frame-set-background-mode (frame-list))

    (when carbon-fonts-use
        (custom-theme-set-faces theme
         `(default ((,light (:foreground ,carbon-light-foreground
                             :weight     ,(face-attribute 'carbon-mono :weight)
                             :height     ,(face-attribute 'carbon-mono :height)
                             :family     ,(face-attribute 'carbon-mono :family)))
                    (,dark  (:foreground ,carbon-dark-foreground
                             :weight     ,(face-attribute 'carbon-mono :weight)
                             :height     ,(face-attribute 'carbon-mono :height)
                             :family     ,(face-attribute 'carbon-mono :family)))))
         `(italic ((,light (:foreground ,carbon-light-foreground
                             :weight     ,(face-attribute 'carbon-italic :weight)
                             :height     ,(face-attribute 'carbon-italic :height)
                             :slant      ,(face-attribute 'carbon-italic :slant)
                             :family     ,(face-attribute 'carbon-italic :family)))
                    (,dark  (:foreground ,carbon-dark-foreground
                             :weight     ,(face-attribute 'carbon-italic :weight)
                             :height     ,(face-attribute 'carbon-italic :height)
                             :slant      ,(face-attribute 'carbon-italic :slant)
                             :family     ,(face-attribute 'carbon-italic :family)))))
         `(carbon-strong ((,light (:weight normal ))
                        (,dark  (:weight normal ))))
         `(variable-pitch  ((t (:weight ,(face-attribute 'carbon-sans :weight)
                                :height ,(face-attribute 'carbon-sans :height)
                                :family ,(face-attribute 'carbon-sans :family)))))))

    (unless carbon-fonts-use
        (custom-theme-set-faces theme
         `(default ((,light (:foreground ,carbon-light-foreground))
                    (,dark  (:foreground ,carbon-dark-foreground))))
         `(carbon-strong ((,light (:weight bold :foreground ,carbon-light-strong))
                        (,dark  (:weight bold :foreground ,carbon-dark-strong))))))

    ;; --- Window divider ----------------------------------------------
    (if carbon-window-divider-show
        (custom-theme-set-faces theme
         `(window-divider ((,light (:foreground ,carbon-light-foreground))
                           (,dark  (:foreground ,carbon-dark-foreground))))
         `(vertical-border ((,light (:foreground ,carbon-light-foreground))
                            (,dark  (:foreground ,carbon-dark-foreground)))))
      (custom-theme-set-faces theme
       `(window-divider ((,light (:foreground ,carbon-light-background))
                         (,dark  (:foreground ,carbon-dark-background))))
       `(vertical-border ((,light (:foreground ,carbon-light-background))
                          (,dark  (:foreground ,carbon-dark-background))))))
    (custom-theme-set-faces theme
     '(window-divider-first-pixel ((t (:inherit window-divider))))
     '(window-divider-last-pixel ((t (:inherit window-divider)))))


    (custom-theme-set-faces theme

   ;; --- Base ---------------------------------------------------------

   `(default ((,light  (:background ,carbon-light-background
                        :foreground ,carbon-light-foreground))
              (,dark  (:background ,carbon-dark-background
                       :foreground ,carbon-dark-foreground))))

   `(cursor ((,light (:foreground ,carbon-light-background
                      :background ,carbon-light-foreground))
             (,dark  (:foreground ,carbon-dark-background
                      :background ,carbon-dark-foreground))))

   `(mouse ((,light (:foreground ,carbon-light-foreground
                     :background ,carbon-light-background))
             (,dark  (:foreground ,carbon-dark-foreground
                      :background ,carbon-dark-background))))

   `(highlight ((,light (:background ,carbon-light-highlight))
                (,dark  (:background ,carbon-dark-highlight))))

   `(carbon-subtle ((,light (:background ,carbon-light-subtle))
                  (,dark  (:background ,carbon-dark-subtle))))

   `(carbon-subtle-i ((,light (:foreground ,carbon-light-subtle))
                    (,dark  (:foreground ,carbon-dark-subtle))))

   `(carbon-faded ((,light  (:foreground ,carbon-light-faded))
                 (,dark  (:foreground ,carbon-dark-faded))))

   `(carbon-faded-i ((,light (:foreground ,carbon-light-background
                            :background ,carbon-light-faded))
                    (,dark  (:foreground ,carbon-dark-background
                             :background ,carbon-dark-faded))))

   `(carbon-default ((,light  (:foreground ,carbon-light-foreground))
                   (,dark  (:foreground ,carbon-dark-foreground))))

   `(carbon-default-i ((,light (:foreground ,carbon-light-background
                              :background ,carbon-light-foreground))
                     (,dark  (:foreground ,carbon-dark-background
                              :background ,carbon-dark-foreground))))


   `(carbon-salient ((,light (:foreground ,carbon-light-salient))
                   (,dark  (:foreground ,carbon-dark-salient))))

   `(carbon-salient-i ((,light (:foreground ,carbon-light-background
                              :background ,carbon-light-salient))
                     (,dark  (:foreground ,carbon-dark-background
                              :background ,carbon-dark-salient))))



   `(carbon-strong-i ((,light (:foreground ,carbon-light-background
                             :background ,carbon-light-strong
                             :weight normal))
                    (,dark  (:foreground ,carbon-dark-background
                             :background ,carbon-dark-strong
                             :weight normal))))

   `(carbon-popout ((,light (:foreground ,carbon-light-popout))
                  (,dark  (:foreground ,carbon-dark-popout))))

   `(carbon-popout-i ((,light (:foreground ,carbon-light-background
                             :background ,carbon-light-popout))
                    (,dark  (:foreground ,carbon-dark-background
                             :background ,carbon-dark-popout))))

   `(carbon-critical ((,light (:foreground ,carbon-light-critical
                             :weight normal))
                    (,dark  (:foreground ,carbon-dark-critical
                             :weight normal))))

   `(carbon-critical-i ((,light (:foreground ,carbon-light-background
                               :background ,carbon-light-critical
                               :weight normal))
                      (,dark  (:foreground ,carbon-dark-background
                               :background ,carbon-dark-critical
                               :weight normal))))

   ;; --- Header & mode line -------------------------------------------

   `(mode-line ((,light (:foreground ,carbon-light-background
                         :background ,carbon-light-foreground))
        (,dark  (:foreground ,carbon-dark-foreground
             :background ,carbon-dark-subtle))))
   `(mode-line-highlight ((t (:inherit carbon-popout))))
   `(mode-line-buffer-id ((t (:weight regular))))
   `(mode-line-emphasis  ((t (:weight regular))))

   `(mode-line-inactive ((,light (:foreground ,carbon-light-background
                                  :background ,carbon-light-faded
                                  :box (:line-width 3
                    :color ,carbon-light-faded
                    :style nil)))
             (,dark  (:foreground ,carbon-dark-faded
                                  :background ,carbon-dark-subtle
                                  :box (:line-width 3
                    :color ,carbon-dark-subtle
                    :style nil)))))

   `(header-line ((,light (:foreground ,carbon-light-foreground
                           :background ,carbon-light-subtle
                           :inherit nil
                           :box nil))
          (,dark  (:foreground ,carbon-dark-foreground
                   :background ,carbon-dark-subtle
                           :inherit nil
                           :box nil))))


   ;; --- Structural ---------------------------------------------------
   '(bold                        ((t (:inherit carbon-strong))))
   ;; '(italic                      ((t (:slant italic))))
   '(italic                      ((t (:inherit carbon-faded))))
   '(bold-italic                 ((t (:inherit carbon-strong))))
   '(region                      ((t (:inherit highlight :distant-foreground unspecified))))
   '(fringe                      ((t (:inherit (carbon-faded)))))
   '(hl-line                     ((t (:inherit highlight))))
   '(link                        ((t (:inherit carbon-salient))))
   '(fixed-pitch                 ((t (:inherit default))))
   '(fixed-pitch-serif           ((t (:inherit default))))

   ;; --- Semantic -----------------------------------------------------
   '(shadow                        ((t (:inherit carbon-faded))))
   '(success                       ((t (:inherit carbon-salient))))
   '(warning                       ((t (:inherit carbon-popout))))
   '(error                         ((t (:inherit carbon-critical))))
   '(match                         ((t (:inherit carbon-popout))))

   ;; --- General ------------------------------------------------------
   '(buffer-menu-buffer            ((t (:inherit carbon-strong))))
   '(minibuffer-prompt             ((t (:inherit carbon-strong))))
   '(isearch                       ((t (:inherit carbon-strong))))
   '(isearch-fail                  ((t (:inherit carbon-faded))))
   '(show-paren-match              ((t (:inherit carbon-strong))))
   '(show-paren-mismatch           ((t (:inherit carbon-critical))))
   '(lazy-highlight                ((t (:inherit highlight))))
   '(trailing-whitespace           ((t (:inherit carbon-subtle))))
   '(secondary-selection           ((t (:inherit carbon-subtle))))
   '(completions-annotations       ((t (:inherit carbon-faded))))
   '(completions-common-part       ((t (:inherit carbon-strong))))
   '(completions-first-difference  ((t (:inherit carbon-default))))
   '(tooltip                       ((t (:inherit carbon-subtle))))
   '(read-multiple-choice-face     ((t (:inherit carbon-strong))))
   '(nobreak-hyphen                ((t (:inherit carbon-popout))))
   '(nobreak-space                 ((t (:inherit carbon-popout))))
   '(help-argument-name            ((t (:inherit carbon-faded))))
   '(tabulated-list-fake-header    ((t (:inherit carbon-strong))))
   '(tool-bar                      ((t (:inherit carbon-faded-i))))

   ;; --- TTY faces ----------------------------------------------------
   '(tty-menu-disabled-face        ((t (:inherit carbon-faded-i))))
   '(tty-menu-enabled-face         ((t (:inherit carbon-default-i))))
   '(tty-menu-selected-face        ((t (:inherit carbon-salient-i))))

   ;; --- Tab bar ------------------------------------------------------
   '(tab-bar                       ((t (:inherit default))))
   '(tab-bar-tab                   ((t (:inherit default))))
   '(tab-bar-tab-inactive          ((t (:inherit carbon-faded))))
   '(tab-line                      ((t (:inherit default))))

   ;; --- Line numbers -------------------------------------------------
   '(line-number                  ((t (:inherit carbon-faded))))
   '(line-number-current-line     ((t (:inherit (carbon-strong hl-line)))))
   `(line-number-major-tick       ((t (:inherit carbon-default))))
   '(line-number-minor-tick       ((t (:inherit carbon-faded))))

   ;; --- Diff HL (fringe mode) ----------------------------------------
   '(diff-hl-change                  ((t (:inherit carbon-faded-i))))
   '(diff-hl-insert                  ((t (:inherit carbon-salient-i))))
   '(diff-hl-delete                  ((t (:inherit carbon-critical-i))))

   ;; --- Font lock ----------------------------------------------------
   '(font-lock-comment-face        ((t (:inherit carbon-faded))))
   '(font-lock-doc-face            ((t (:inherit carbon-faded))))
   '(font-lock-string-face         ((t (:inherit carbon-popout))))
   '(font-lock-constant-face       ((t (:inherit carbon-salient))))
   '(font-lock-warning-face        ((t (:inherit carbon-popout))))
   '(font-lock-function-name-face  ((t (:inherit carbon-strong))))
   '(font-lock-variable-name-face  ((t (:inherit (carbon-strong carbon-salient)))))
   '(font-lock-builtin-face        ((t (:inherit carbon-salient))))
   '(font-lock-type-face           ((t (:inherit carbon-salient))))
   '(font-lock-keyword-face        ((t (:inherit carbon-salient))))

   ;; --- Custom edit --------------------------------------------------
   '(widget-field                  ((t (:inherit carbon-subtle))))
   '(widget-button                 ((t (:inherit carbon-strong))))
   '(widget-single-line-field      ((t (:inherit carbon-subtle))))
   '(custom-group-subtitle         ((t (:inherit carbon-strong))))
   '(custom-group-tag              ((t (:inherit carbon-strong))))
   '(custom-group-tag-1            ((t (:inherit carbon-strong))))
   '(custom-comment                ((t (:inherit carbon-faded))))
   '(custom-comment-tag            ((t (:inherit carbon-faded))))
   '(custom-changed                ((t (:inherit carbon-salient))))
   '(custom-modified               ((t (:inherit carbon-salient))))
   '(custom-face-tag               ((t (:inherit carbon-strong))))
   '(custom-variable-tag           ((t (:inherit carbon-strong))))
   '(custom-invalid                ((t (:inherit carbon-popout))))
   '(custom-visibility             ((t (:inherit carbon-salient))))
   '(custom-state                  ((t (:inherit carbon-salient))))
   '(custom-link                   ((t (:inherit carbon-salient))))
   '(custom-variable-obsolete      ((t (:inherit carbon-faded))))

   ;; --- Company tooltip ----------------------------------------------
   '(company-tooltip                      ((t (:inherit carbon-subtle))))
   '(company-tooltip-mouse                ((t (:inherit carbon-faded-i))))
   '(company-tooltip-selection            ((t (:inherit carbon-salient-i))))

   '(company-scrollbar-fg                 ((t (:inherit carbon-default-i))))
   '(company-scrollbar-bg                 ((t (:inherit carbon-faded-i))))

   '(company-tooltip-scrollbar-thumb      ((t (:inherit carbon-default-i))))
   '(company-tooltip-scrollbar-track      ((t (:inherit carbon-faded-i))))

   '(company-tooltip-common               ((t (:inherit carbon-strong))))
   '(company-tooltip-common-selection     ((t (:inherit carbon-salient-i
                                                :weight normal))))
   '(company-tooltip-annotation           ((t (:inherit carbon-default))))
   '(company-tooltip-annotation-selection ((t (:inherit carbon-subtle))))

   ;; --- Compilation --------------------------------------------------
   '(compilation-error ((t (:inherit carbon-critical))))
   '(compilation-info ((t (:inherit carbon-default))))
   '(compilation-warning ((t (:inherit carbon-popout))))
   '(compilation-line-number ((t (:inherit carbon-default))))
   '(compilation-column-number ((t (:inherit carbon-default))))
   '(compilation-mode-line-run ((t (:inherit carbon-default-i))))
   '(compilation-mode-line-exit ((t (:inherit carbon-default-i))))
   '(compilation-mode-line-fail ((t (:inherit carbon-critical))))

   ;; --- Buttons ------------------------------------------------------
   `(custom-button
     ((,light (:foreground ,carbon-light-faded
               :background ,carbon-light-highlight
               :box nil))
      (,dark (:foreground ,carbon-dark-faded
              :background ,carbon-dark-highlight
              :box nil))))

   `(custom-button-mouse
     ((,light (:foreground ,carbon-light-foreground
           :background ,carbon-light-subtle
               :box nil))
      (,dark (:foreground ,carbon-dark-foreground
          :background ,carbon-dark-subtle
              :box nil))))

   `(custom-button-pressed
     ((,light (:foreground ,carbon-light-background
           :background ,carbon-light-foreground
               :box nil))
      (,dark (:foreground ,carbon-dark-background
          :background ,carbon-dark-foreground
              :box nil))))

   ;; --- Packages -----------------------------------------------------
   '(package-description            ((t (:inherit carbon-default))))
   '(package-help-section-name      ((t (:inherit carbon-default))))
   '(package-name                   ((t (:inherit carbon-salient))))
   '(package-status-avail-obso      ((t (:inherit carbon-faded))))
   '(package-status-available       ((t (:inherit carbon-default))))
   '(package-status-built-in        ((t (:inherit carbon-salient))))
   '(package-status-dependency      ((t (:inherit carbon-salient))))
   '(package-status-disabled        ((t (:inherit carbon-faded))))
   '(package-status-external        ((t (:inherit carbon-default))))
   '(package-status-held            ((t (:inherit carbon-default))))
   '(package-status-incompat        ((t (:inherit carbon-faded))))
   '(package-status-installed       ((t (:inherit carbon-salient))))
   '(package-status-new             ((t (:inherit carbon-default))))
   '(package-status-unsigned        ((t (:inherit carbon-default))))

   ;; --- Info ---------------------------------------------------------
   '(info-node                      ((t (:inherit carbon-strong))))
   '(info-menu-header               ((t (:inherit carbon-strong))))
   '(info-header-node               ((t (:inherit carbon-default))))
   '(info-index-match               ((t (:inherit carbon-salient))))
   '(Info-quoted                    ((t (:inherit carbon-faded))))
   '(info-title-1                   ((t (:inherit carbon-strong))))
   '(info-title-2                   ((t (:inherit carbon-strong))))
   '(info-title-3                   ((t (:inherit carbon-strong))))
   '(info-title-4                   ((t (:inherit carbon-strong))))

   ;; --- Helpful ------------------------------------------------------
   '(helpful-heading                ((t (:inherit carbon-strong))))

   ;; --- Carbon modeline ------------------------------------------------
;;   '(carbon-modeline-active               ((t (:inherit carbon-subtle))))
   '(carbon-modeline-active-name          ((t (:inherit (carbon-subtle carbon-modeline-active)))))
   '(carbon-modeline-active-primary       ((t (:inherit (carbon-subtle carbon-modeline-active)))))
   '(carbon-modeline-active-secondary     ((t (:inherit (carbon-subtle carbon-modeline-active)))))
   '(carbon-modeline-active-status-RO     ((t (:inherit (carbon-subtle carbon-strong)))))
   '(carbon-modeline-active-status-RW     ((t (:inherit (carbon-faded-i carbon-strong)))))
   '(carbon-modeline-active-status-**     ((t (:inherit (carbon-popout-i carbon-strong)))))

;;   '(carbon-modeline-inactive             ((t (:inherit carbon-subtle))))
   '(carbon-modeline-inactive-name        ((t (:inherit (carbon-faded carbon-modeline-inactive)))))
   '(carbon-modeline-inactive-primary     ((t (:inherit (carbon-faded carbon-modeline-inactive)))))
   '(carbon-modeline-inactive-secondary   ((t (:inherit (carbon-faded carbon-modeline-inactive)))))
   '(carbon-modeline-inactive-status-RO   ((t (:inherit (carbon-faded
                                                       carbon-strong carbon-modeline-inactive)))))
   '(carbon-modeline-inactive-status-RW   ((t (:inherit (carbon-faded
                                                       carbon-strong carbon-modeline-inactive)))))
   '(carbon-modeline-inactive-status-**   ((t (:inherit (carbon-popout
                                                       carbon-strong carbon-modeline-inactive)))))

   ;; --- carbon agenda ---------------------------------------------------------
   '(carbon-agenda-button               ((t (:inherit (carbon-faded)))))
   '(carbon-agenda-day-name             ((t (:inherit (carbon-faded)))))
   '(carbon-agenda-default              ((t (:inherit (carbon-default)))))
   '(carbon-agenda-holidays             ((t (:inherit (carbon-faded)))))
   '(carbon-agenda-month-name           ((t (:inherit (carbon-strong)))))
   '(carbon-agenda-mouse                ((t (:inherit (carbon-highlight)))))
   '(carbon-agenda-outday               ((t (:inherit (carbon-subtle-i)))))
   '(carbon-agenda-selected             ((t (:inherit (carbon-default-i)))))
   '(carbon-agenda-selected-today       ((t (:inherit (carbon-popout-i carbon-strong)))))
   '(carbon-agenda-today                ((t (:inherit (carbon-popout carbon-strong)))))
   '(carbon-agenda-weekend              ((t (:inherit (carbon-faded)))))

   ;; --- EPA ----------------------------------------------------------
   '(epa-field-body                 ((t (:inherit carbon-default))))
   '(epa-field-name                 ((t (:inherit carbon-strong))))
   '(epa-mark                       ((t (:inherit carbon-salient))))
   '(epa-string                     ((t (:inherit carbon-popout))))
   '(epa-validity-disabled          ((t (:inherit carbon-faded))))
   '(epa-validity-high              ((t (:inherit carbon-strong))))
   '(epa-validity-medium            ((t (:inherit carbon-default))))
   '(epa-validity-low               ((t (:inherit carbon-faded))))

   ;; --- Popup --------------------------------------------------------
   '(popup-face                       ((t (:inherit highlight))))
   '(popup-isearch-match              ((t (:inherit carbon-popout))))
   '(popup-menu-face                  ((t (:inherit carbon-subtle))))
   '(popup-menu-mouse-face            ((t (:inherit carbon-faded-i))))
   '(popup-menu-selection-face        ((t (:inherit carbon-salient-i))))
   '(popup-menu-summary-face          ((t (:inherit carbon-faded))))
   '(popup-scroll-bar-background-face ((t (:inherit carbon-subtle))))
   '(popup-scroll-bar-foreground-face ((t (:inherit carbon-subtle))))
   '(popup-summary-face               ((t (:inherit carbon-faded))))
   '(popup-tip-face                   ((t (:inherit carbon-popout-i))))

   ;; --- Diff ---------------------------------------------------------
   '(diff-header                    ((t (:inherit carbon-faded))))
   '(diff-file-header               ((t (:inherit carbon-strong))))
   '(diff-context                   ((t (:inherit carbon-default))))
   '(diff-removed                   ((t (:inherit carbon-faded))))
   '(diff-changed                   ((t (:inherit carbon-popout))))
   '(diff-added                     ((t (:inherit carbon-salient))))
   '(diff-refine-added              ((t (:inherit (carbon-salient
                                                   carbon-strong)))))
   '(diff-refine-changed            ((t (:inherit carbon-popout))))
   '(diff-refine-removed            ((t (:inherit carbon-faded
                                         :strike-through t))))

   ;; --- rainbow delimeters -----------------------------------------------
   '(rainbow-delimiters-depth-1-face ((t (:foreground "#be95ff"))))
   '(rainbow-delimiters-depth-1-face ((t (:foreground "#78a9ff"))))
   '(rainbow-delimiters-depth-1-face ((t (:foreground "#fa4d56"))))
   '(rainbow-delimiters-depth-1-face ((t (:foreground "#42be65"))))
   '(rainbow-delimiters-depth-1-face ((t (:foreground "#f1c21b"))))
   '(rainbow-delimiters-depth-1-face ((t (:foreground "#9f1853"))))
   '(rainbow-delimiters-depth-1-face ((t (:foreground "#08bdba"))))
   '(rainbow-delimiters-depth-1-face ((t (:foreground "#a7f0ba"))))
   '(rainbow-delimiters-depth-1-face ((t (:foreground "#00539a"))))
   '(rainbow-delimiters-unmatched-face ((t (:foreground "#a2191f"))))
   '(rainbow-delimiters-mismatched-face ((t (:foreground "#a2191f"))))
   
   ;; --- icomplete --------------------------------------------------------
   '(icomplete-first-match          ((t (:inherit carbon-strong))))
   '(icomplete-selected-match       ((t (:inherit carbon-strong))))
   '(icomplete-section              ((t (:inherit carbon-strong))))

   ;; --- Vertico --------------------------------------------------------
   '(vertico-current                       ((t (:inherit (carbon-strong
                                                          carbon-subtle)))))
   '(vertico-group-separator               ((t (:inherit carbon-faded))))
   '(vertico-group-title                   ((t (:inherit carbon-faded))))
   '(vertico-multiline                     ((t (:inherit carbon-faded))))

   ;; --- Citar --------------------------------------------------------
   '(citar                          ((t (:inherit carbon-faded))))
   '(citar-highlight                ((t (:inherit carbon-default))))

   ;; --- Corfu --------------------------------------------------------
   '(corfu-annotations              ((t (:inherit carbon-faded))))
   '(corfu-bar                      ((t (:inherit carbon-default-i))))
   '(corfu-border                   ((t (:inherit carbon-default-i))))
   '(corfu-current                  ((t (:inherit highlight))))
   '(corfu-default                  ((t (:inherit carbon-subtle))))
   '(corfu-deprecated               ((t (:inherit carbon-faded))))
   '(corfu-echo                     ((t (:inherit carbon-faded))))

   ;; --- Orderless ----------------------------------------------------
   '(orderless-match-face-0         ((t (:inherit (carbon-salient
                                                   carbon-strong)))))
   '(orderless-match-face-1         ((t (:inherit (carbon-strong)))))
   '(orderless-match-face-2         ((t (:inherit (carbon-strong)))))
   '(orderless-match-face-3         ((t (:inherit (carbon-strong)))))

   ;; --- Message ------------------------------------------------------
   '(message-cited-text-1           ((t (:inherit carbon-faded))))
   '(message-cited-text-2           ((t (:inherit carbon-faded))))
   '(message-cited-text-3           ((t (:inherit carbon-faded))))
   '(message-cited-text-4           ((t (:inherit carbon-faded))))
   '(message-cited-text             ((t (:inherit carbon-faded))))
   '(message-header-cc              ((t (:inherit carbon-default))))
   '(message-header-name            ((t (:inherit carbon-strong))))
   '(message-header-newsgroups      ((t (:inherit carbon-default))))
   '(message-header-other           ((t (:inherit carbon-default))))
   '(message-header-subject         ((t (:inherit carbon-salient))))
   '(message-header-to              ((t (:inherit carbon-salient))))
   '(message-header-xheader         ((t (:inherit carbon-default))))
   '(message-mml                    ((t (:inherit carbon-popout))))
   '(message-separator              ((t (:inherit carbon-faded))))

   ;; --- Outline ------------------------------------------------------
   '(outline-1                      ((t (:inherit carbon-strong))))
   '(outline-2                      ((t (:inherit carbon-strong))))
   '(outline-3                      ((t (:inherit carbon-strong))))
   '(outline-4                      ((t (:inherit carbon-strong))))
   '(outline-5                      ((t (:inherit carbon-strong))))
   '(outline-6                      ((t (:inherit carbon-strong))))
   '(outline-7                      ((t (:inherit carbon-strong))))
   '(outline-8                      ((t (:inherit carbon-strong))))

   ;; --- Fly spell ----------------------------------------------------
   '(flyspell-duplicate             ((t (:inherit carbon-popout
                                         :underline t))))
   '(flyspell-incorrect             ((t (:inherit carbon-popout
                                         :underline t))))

   ;; --- Org agenda ---------------------------------------------------
   '(org-agenda-calendar-event      ((t (:inherit carbon-default))))
   '(org-agenda-calendar-sexp       ((t (:inherit carbon-salient))))
   '(org-agenda-clocking            ((t (:inherit carbon-faded))))
   '(org-agenda-column-dateline     ((t (:inherit carbon-faded))))
   '(org-agenda-current-time        ((t (:inherit (carbon-strong
                                                   carbon-salient)))))
   '(org-agenda-date                ((t (:inherit carbon-strong))))
   '(org-agenda-date-today          ((t (:inherit (carbon-salient
                                                   carbon-strong)))))
   '(org-agenda-date-weekend        ((t (:inherit carbon-faded))))
   '(org-agenda-diary               ((t (:inherit carbon-faded))))
   '(org-agenda-dimmed-todo-face    ((t (:inherit carbon-faded))))
   '(org-agenda-done                ((t (:inherit carbon-faded))))
   '(org-agenda-filter-category     ((t (:inherit carbon-faded))))
   '(org-agenda-filter-effort       ((t (:inherit carbon-faded))))
   '(org-agenda-filter-regexp       ((t (:inherit carbon-faded))))
   '(org-agenda-filter-tags         ((t (:inherit carbon-faded))))
   '(org-agenda-property-face       ((t (:inherit carbon-faded))))
   '(org-agenda-restriction-lock    ((t (:inherit carbon-faded))))
   '(org-agenda-structure           ((t (:inherit carbon-strong))))

   ;; --- Org ----------------------------------------------------------
   '(org-archived                            ((t (:inherit carbon-faded))))
   '(org-block                               ((t (:inherit carbon-subtle))))
   `(org-block-begin-line                    ((t (:inherit carbon-faded
                                                 :underline ,(face-background 'carbon-subtle)))))
   `(org-block-end-line                      ((t (:inherit carbon-faded
                                                 :overline ,(face-background 'carbon-subtle)))))
   '(org-checkbox                            ((t (:inherit carbon-faded))))
   '(org-checkbox-statistics-done            ((t (:inherit carbon-faded))))
   '(org-checkbox-statistics-todo            ((t (:inherit carbon-faded))))
   '(org-clock-overlay                       ((t (:inherit carbon-faded))))
   '(org-code                                ((t (:inherit carbon-salient))))
   '(org-column                              ((t (:inherit carbon-faded))))
   '(org-column-title                        ((t (:inherit carbon-faded))))
   '(org-date                                ((t (:inherit carbon-faded))))
   '(org-date-selected                       ((t (:inherit carbon-faded))))
   '(org-default                             ((t (:inherit carbon-faded))))
   '(org-document-info                       ((t (:inherit carbon-faded))))
   '(org-document-info-keyword               ((t (:inherit carbon-faded))))
   '(org-document-title                      ((t (:inherit carbon-faded))))
   '(org-done                                ((t (:inherit carbon-faded))))
   '(org-drawer                              ((t (:inherit carbon-faded))))
   '(org-ellipsis                            ((t (:inherit carbon-faded))))
   '(org-footnote                            ((t (:inherit carbon-faded))))
   '(org-formula                             ((t (:inherit carbon-faded))))
   '(org-headline-done                       ((t (:inherit carbon-faded))))
   ;; '(org-hide                                ((t (:inherit carbon-subtle-i))))
   ;; '(org-indent                              ((t (:inherit carbon-subtle-i))))
   '(org-latex-and-related                   ((t (:inherit carbon-faded))))
   '(org-level-1                             ((t (:inherit carbon-strong))))
   '(org-level-2                             ((t (:inherit carbon-strong))))
   '(org-level-3                             ((t (:inherit carbon-strong))))
   '(org-level-4                             ((t (:inherit carbon-strong))))
   '(org-level-5                             ((t (:inherit carbon-strong))))
   '(org-level-6                             ((t (:inherit carbon-strong))))
   '(org-level-7                             ((t (:inherit carbon-strong))))
   '(org-level-8                             ((t (:inherit carbon-strong))))
   '(org-link                                ((t (:inherit carbon-salient))))
   '(org-list-dt                             ((t (:inherit carbon-faded))))
   '(org-macro                               ((t (:inherit carbon-faded))))
   '(org-meta-line                           ((t (:inherit carbon-faded))))
   '(org-mode-line-clock                     ((t (:inherit carbon-faded))))
   '(org-mode-line-clock-overrun             ((t (:inherit carbon-faded))))
   '(org-priority                            ((t (:inherit carbon-faded))))
   '(org-property-value                      ((t (:inherit carbon-faded))))
   '(org-quote                               ((t (:inherit carbon-faded))))
   '(org-scheduled                           ((t (:inherit carbon-faded))))
   '(org-scheduled-previously                ((t (:inherit carbon-faded))))
   '(org-scheduled-today                     ((t (:inherit carbon-faded))))
   '(org-sexp-date                           ((t (:inherit carbon-faded))))
   '(org-special-keyword                     ((t (:inherit carbon-faded))))
   '(org-table                               ((t (:inherit carbon-faded))))
   '(org-tag                                 ((t (:inherit carbon-popout))))
   '(org-tag-group                           ((t (:inherit carbon-faded))))
   '(org-target                              ((t (:inherit carbon-faded))))
   '(org-time-grid                           ((t (:inherit carbon-faded))))
   '(org-todo                                ((t (:inherit carbon-salient))))
   '(org-upcoming-deadline                   ((t (:inherit carbon-popout))))
   '(org-verbatim                            ((t (:inherit carbon-popout))))
   '(org-verse                               ((t (:inherit carbon-faded))))
   '(org-warning                             ((t (:inherit carbon-popout))))

   ;; --- Mu4e ---------------------------------------------------------
   '(mu4e-attach-number-face                ((t (:inherit carbon-strong))))
   '(mu4e-cited-1-face                       ((t (:inherit carbon-faded))))
   '(mu4e-cited-2-face                       ((t (:inherit carbon-faded))))
   '(mu4e-cited-3-face                       ((t (:inherit carbon-faded))))
   '(mu4e-cited-4-face                       ((t (:inherit carbon-faded))))
   '(mu4e-cited-5-face                       ((t (:inherit carbon-faded))))
   '(mu4e-cited-6-face                       ((t (:inherit carbon-faded))))
   '(mu4e-cited-7-face                       ((t (:inherit carbon-faded))))
   '(mu4e-compose-header-face                ((t (:inherit carbon-faded))))
   '(mu4e-compose-separator-face             ((t (:inherit carbon-faded))))
   '(mu4e-contact-face                     ((t (:inherit carbon-salient))))
   '(mu4e-context-face                       ((t (:inherit carbon-faded))))
   '(mu4e-draft-face                         ((t (:inherit carbon-faded))))
   '(mu4e-flagged-face                     ((t (:inherit carbon-salient))))
   '(mu4e-footer-face                        ((t (:inherit carbon-faded))))
   '(mu4e-forwarded-face                   ((t (:inherit carbon-default))))
   '(mu4e-header-face                      ((t (:inherit carbon-default))))
   '(mu4e-header-highlight-face               ((t (:inherit highlight))))
   '(mu4e-header-key-face                   ((t (:inherit carbon-strong))))
   '(mu4e-header-marks-face                  ((t (:inherit carbon-faded))))
   '(mu4e-header-title-face                 ((t (:inherit carbon-strong))))
   '(mu4e-header-field-face                 ((t (:inherit carbon-strong))))
   '(mu4e-header-value-face                ((t (:inherit carbon-default))))
   '(mu4e-highlight-face                    ((t (:inherit carbon-popout))))
   '(mu4e-link-face                        ((t (:inherit carbon-salient))))
   '(mu4e-modeline-face                      ((t (:inherit carbon-faded))))
   '(mu4e-moved-face                         ((t (:inherit carbon-faded))))
   '(mu4e-ok-face                            ((t (:inherit carbon-faded))))
   '(mu4e-region-code                        ((t (:inherit carbon-faded))))
   '(mu4e-replied-face                     ((t (:inherit carbon-default))))
   '(mu4e-special-header-value-face        ((t (:inherit carbon-default))))
   '(mu4e-system-face                        ((t (:inherit carbon-faded))))
   '(mu4e-related-face                       ((t (:inherit carbon-faded))))
   '(mu4e-title-face                        ((t (:inherit carbon-strong))))
   '(mu4e-trashed-face                       ((t (:inherit carbon-faded))))
   '(mu4e-unread-face                       ((t (:inherit carbon-strong))))
   '(mu4e-url-number-face                    ((t (:inherit carbon-faded))))
   '(mu4e-view-body-face                   ((t (:inherit carbon-default))))
   '(mu4e-warning-face                      ((t (:inherit carbon-popout))))

   ;; --- GNUS ---------------------------------------------------------
   '(gnus-button                            ((t (:inherit carbon-salient))))
   '(gnus-cite-1                            ((t (:inherit carbon-faded))))
   '(gnus-cite-10                           ((t (:inherit carbon-faded))))
   '(gnus-cite-11                           ((t (:inherit carbon-faded))))
   '(gnus-cite-2                            ((t (:inherit carbon-faded))))
   '(gnus-cite-3                            ((t (:inherit carbon-faded))))
   '(gnus-cite-4                            ((t (:inherit carbon-faded))))
   '(gnus-cite-5                            ((t (:inherit carbon-faded))))
   '(gnus-cite-6                            ((t (:inherit carbon-faded))))
   '(gnus-cite-7                            ((t (:inherit carbon-faded))))
   '(gnus-cite-8                            ((t (:inherit carbon-faded))))
   '(gnus-cite-9                            ((t (:inherit carbon-faded))))
   '(gnus-cite-attribution                  ((t (:inherit carbon-faded))))
   '(gnus-emphasis-bold                     ((t (:inherit carbon-faded))))
   '(gnus-emphasis-bold-italic              ((t (:inherit carbon-faded))))
   '(gnus-emphasis-highlight-words          ((t (:inherit carbon-faded))))
   '(gnus-emphasis-italic                   ((t (:inherit carbon-faded))))
   '(gnus-emphasis-strikethru               ((t (:inherit carbon-faded))))
   '(gnus-emphasis-underline                ((t (:inherit carbon-faded))))
   '(gnus-emphasis-underline-bold           ((t (:inherit carbon-faded))))
   '(gnus-emphasis-underline-bold-italic    ((t (:inherit carbon-faded))))
   '(gnus-emphasis-underline-italic         ((t (:inherit carbon-faded))))
   '(gnus-group-mail-1                      ((t (:inherit carbon-faded))))
   '(gnus-group-mail-1-empty                ((t (:inherit carbon-faded))))
   '(gnus-group-mail-2                      ((t (:inherit carbon-faded))))
   '(gnus-group-mail-2-empty                ((t (:inherit carbon-faded))))
   '(gnus-group-mail-3                      ((t (:inherit carbon-faded))))
   '(gnus-group-mail-3-empty                ((t (:inherit carbon-faded))))
   '(gnus-group-mail-low                    ((t (:inherit carbon-faded))))
   '(gnus-group-mail-low-empty              ((t (:inherit carbon-faded))))
   '(gnus-group-news-1                      ((t (:inherit carbon-faded))))
   '(gnus-group-news-1-empty                ((t (:inherit carbon-faded))))
   '(gnus-group-news-2                      ((t (:inherit carbon-faded))))
   '(gnus-group-news-2-empty                ((t (:inherit carbon-faded))))
   '(gnus-group-news-3                      ((t (:inherit carbon-faded))))
   '(gnus-group-news-3-empty                ((t (:inherit carbon-faded))))
   '(gnus-group-news-4                      ((t (:inherit carbon-faded))))
   '(gnus-group-news-4-empty                ((t (:inherit carbon-faded))))
   '(gnus-group-news-5                      ((t (:inherit carbon-faded))))
   '(gnus-group-news-5-empty                ((t (:inherit carbon-faded))))
   '(gnus-group-news-6                      ((t (:inherit carbon-faded))))
   '(gnus-group-news-6-empty                ((t (:inherit carbon-faded))))
   '(gnus-group-news-low                    ((t (:inherit carbon-faded))))
   '(gnus-group-news-low-empty              ((t (:inherit carbon-faded))))

   '(gnus-header-content                    ((t (:inherit carbon-faded))))
   '(gnus-header-from                       ((t (:inherit carbon-strong))))
   '(gnus-header-name                       ((t (:inherit carbon-strong))))
   '(gnus-header-newsgroups                 ((t (:inherit carbon-faded))))
   '(gnus-header-subject                    ((t (:inherit carbon-default))))

   '(gnus-signature                         ((t (:inherit carbon-faded))))
   '(gnus-splash                            ((t (:inherit carbon-faded))))
   '(gnus-summary-cancelled                 ((t (:inherit carbon-faded))))
   '(gnus-summary-high-ancient              ((t (:inherit carbon-faded))))
   '(gnus-summary-high-read                 ((t (:inherit carbon-faded))))
   '(gnus-summary-high-ticked               ((t (:inherit carbon-faded))))
   '(gnus-summary-high-undownloaded         ((t (:inherit carbon-faded))))
   '(gnus-summary-high-unread               ((t (:inherit carbon-faded))))
   '(gnus-summary-low-ancient               ((t (:inherit carbon-faded))))
   '(gnus-summary-low-read                  ((t (:inherit carbon-faded))))
   '(gnus-summary-low-ticked                ((t (:inherit carbon-faded))))
   '(gnus-summary-low-undownloaded          ((t (:inherit carbon-faded))))
   '(gnus-summary-low-unread                ((t (:inherit carbon-faded))))
   '(gnus-summary-normal-ancient            ((t (:inherit carbon-faded))))
   '(gnus-summary-normal-read               ((t (:inherit carbon-faded))))
   '(gnus-summary-normal-ticked             ((t (:inherit carbon-faded))))
   '(gnus-summary-normal-undownloaded       ((t (:inherit carbon-faded))))
   '(gnus-summary-normal-unread             ((t (:inherit carbon-faded))))
   '(gnus-summary-selected                  ((t (:inherit carbon-faded))))

   ;; --- Marginalia ---------------------------------------------------
   '(marginalia-archive                     ((t (:inherit carbon-faded))))
   '(marginalia-char                        ((t (:inherit carbon-faded))))
   '(marginalia-date                        ((t (:inherit carbon-faded))))
   '(marginalia-documentation               ((t (:inherit carbon-faded))))
   '(marginalia-file-name                   ((t (:inherit carbon-faded))))
   '(marginalia-file-owner                  ((t (:inherit carbon-faded))))
   '(marginalia-file-priv-dir               ((t (:inherit carbon-faded))))
   '(marginalia-file-priv-exec              ((t (:inherit carbon-faded))))
   '(marginalia-file-priv-link              ((t (:inherit carbon-faded))))
   '(marginalia-file-priv-no                ((t (:inherit carbon-faded))))
   '(marginalia-file-priv-other             ((t (:inherit carbon-faded))))
   '(marginalia-file-priv-rare              ((t (:inherit carbon-faded))))
   '(marginalia-file-priv-read              ((t (:inherit carbon-faded))))
   '(marginalia-file-priv-write             ((t (:inherit carbon-faded))))
   '(marginalia-function                    ((t (:inherit carbon-faded))))
   '(marginalia-installed                   ((t (:inherit carbon-faded))))
   '(marginalia-key                         ((t (:inherit carbon-faded))))
   '(marginalia-lighter                     ((t (:inherit carbon-faded))))
   '(marginalia-list                        ((t (:inherit carbon-faded))))
   '(marginalia-mode                        ((t (:inherit carbon-faded))))
   '(marginalia-modified                    ((t (:inherit carbon-faded))))
   '(marginalia-null                        ((t (:inherit carbon-faded))))
   '(marginalia-number                      ((t (:inherit carbon-faded))))
   '(marginalia-off                         ((t (:inherit carbon-faded))))
   '(marginalia-on                          ((t (:inherit carbon-faded))))
   '(marginalia-size                        ((t (:inherit carbon-faded))))
   '(marginalia-string                      ((t (:inherit carbon-faded))))
   '(marginalia-symbol                      ((t (:inherit carbon-faded))))
   '(marginalia-true                        ((t (:inherit carbon-faded))))
   '(marginalia-type                        ((t (:inherit carbon-faded))))
   '(marginalia-value                       ((t (:inherit carbon-faded))))
   '(marginalia-version                     ((t (:inherit carbon-faded))))

   ;; --- Elfeed -------------------------------------------------------
    '(elfeed-log-date-face                    ((t (:inherit carbon-faded))))
    '(elfeed-log-info-level-face            ((t (:inherit carbon-default))))
    '(elfeed-log-debug-level-face           ((t (:inherit carbon-default))))
    '(elfeed-log-warn-level-face             ((t (:inherit carbon-popout))))
    '(elfeed-log-error-level-face            ((t (:inherit carbon-popout))))
    '(elfeed-search-tag-face                  ((t (:inherit carbon-faded))))
    '(elfeed-search-date-face                 ((t (:inherit carbon-faded))))
    '(elfeed-search-feed-face               ((t (:inherit carbon-salient))))
    '(elfeed-search-filter-face               ((t (:inherit carbon-faded))))
    '(elfeed-search-last-update-face        ((t (:inherit carbon-salient))))
    '(elfeed-search-title-face              ((t (:inherit carbon-default))))
    '(elfeed-search-tag-face                  ((t (:inherit carbon-faded))))
    '(elfeed-search-unread-count-face        ((t (:inherit carbon-strong))))
    '(elfeed-search-unread-title-face        ((t (:inherit carbon-strong))))

    ;; --- Deft --------------------------------------------------------
    '(deft-filter-string-error-face         ((t (:inherit carbon-popout))))
    '(deft-filter-string-face              ((t (:inherit carbon-default))))
    '(deft-header-face                     ((t (:inherit carbon-salient))))
    '(deft-separator-face                    ((t (:inherit carbon-faded))))
    '(deft-summary-face                      ((t (:inherit carbon-faded))))
    '(deft-time-face                       ((t (:inherit carbon-salient))))
    '(deft-title-face                       ((t (:inherit carbon-strong))))

    ;; --- imenu-list ---------------------------------------------------
    '(imenu-list-entry-face                 ((t (:inherit carbon-default))))
    '(imenu-list-entry-face-0                ((t (:inherit carbon-strong))))
    '(imenu-list-entry-face-1               ((t ( ))))
    '(imenu-list-entry-face-2               ((t ( ))))
    '(imenu-list-entry-face-3               ((t ( ))))
    '(imenu-list-entry-subalist-face-0      ((t (:inherit carbon-strong))))
    '(imenu-list-entry-subalist-face-1      ((t ( ))))
    '(imenu-list-entry-subalist-face-2      ((t ( ))))
    '(imenu-list-entry-subalist-face-3      ((t ( ))))

    ;; --- Restructured text -------------------------------------------
    '(rst-adornment                           ((t (:inherit carbon-faded))))
    '(rst-block                             ((t (:inherit carbon-default))))
    '(rst-comment                             ((t (:inherit carbon-faded))))
    '(rst-definition                        ((t (:inherit carbon-salient))))
    '(rst-directive                         ((t (:inherit carbon-salient))))
    '(rst-emphasis1                           ((t (:inherit carbon-faded))))
    '(rst-emphasis2                          ((t (:inherit carbon-strong))))
    '(rst-external                          ((t (:inherit carbon-salient))))
    '(rst-level-1                            ((t (:inherit carbon-strong))))
    '(rst-level-2                            ((t (:inherit carbon-strong))))
    '(rst-level-3                            ((t (:inherit carbon-strong))))
    '(rst-level-4                            ((t (:inherit carbon-strong))))
    '(rst-level-5                            ((t (:inherit carbon-strong))))
    '(rst-level-6                            ((t (:inherit carbon-strong))))
    '(rst-literal                           ((t (:inherit carbon-salient))))
    '(rst-reference                         ((t (:inherit carbon-salient))))
    '(rst-transition                        ((t (:inherit carbon-default))))

    ;; --- Elpher ----------------------------------------------------
    '(elpher-gemini-heading1                 ((t (:inherit carbon-strong))))
    '(elpher-gemini-heading2                 ((t (:inherit carbon-strong))))
    '(elpher-gemini-heading3                 ((t (:inherit carbon-strong))))
 
    ;; ---SHR ---------------------------------------------------------
    '(shr-abbreviation                    ((t (:inherit carbon-popout))))
    '(shr-text                            ((t (:inherit carbon-default))))
    '(shr-h1                              ((t (:inherit carbon-strong))))
    '(shr-h2                              ((t (:inherit carbon-strong))))
    '(shr-h3                              ((t (:inherit carbon-strong))))
    '(shr-h4                              ((t (:inherit carbon-strong))))
    '(shr-h5                              ((t (:inherit carbon-strong))))
    '(shr-h6                              ((t (:inherit carbon-strong))))
    '(shr-link                           ((t (:inherit carbon-salient))))
    '(shr-selected-link      ((t (:inherit (carbon-salient carbon-subtle)))))
    '(shr-strike-through                   ((t (:inherit carbon-faded))))

    ;; --- Markdown ----------------------------------------------------
    '(markdown-blockquote-face              ((t (:inherit carbon-default))))
    '(markdown-bold-face                     ((t (:inherit carbon-strong))))
    '(markdown-code-face                    ((t (:inherit carbon-default))))
    '(markdown-comment-face                   ((t (:inherit carbon-faded))))
    '(markdown-footnote-marker-face         ((t (:inherit carbon-default))))
    '(markdown-footnote-text-face           ((t (:inherit carbon-default))))
    '(markdown-gfm-checkbox-face            ((t (:inherit carbon-default))))
    '(markdown-header-delimiter-face          ((t (:inherit carbon-faded))))
    '(markdown-header-face                   ((t (:inherit carbon-strong))))
    '(markdown-header-face-1                 ((t (:inherit carbon-strong))))
    '(markdown-header-face-2                 ((t (:inherit carbon-strong))))
    '(markdown-header-face-3                 ((t (:inherit carbon-strong))))
    '(markdown-header-face-4                 ((t (:inherit carbon-strong))))
    '(markdown-header-face-5                 ((t (:inherit carbon-strong))))
    '(markdown-header-face-6                ((t (:inherit carbon-strong))))
    '(markdown-header-rule-face             ((t (:inherit carbon-default))))
    '(markdown-highlight-face               ((t (:inherit carbon-default))))
    '(markdown-hr-face                      ((t (:inherit carbon-default))))
    '(markdown-html-attr-name-face          ((t (:inherit carbon-default))))
    '(markdown-html-attr-value-face         ((t (:inherit carbon-default))))
    '(markdown-html-entity-face             ((t (:inherit carbon-default))))
    '(markdown-html-tag-delimiter-face      ((t (:inherit carbon-default))))
    '(markdown-html-tag-name-face           ((t (:inherit carbon-default))))
    '(markdown-inline-code-face              ((t (:inherit carbon-popout))))
    '(markdown-italic-face                    ((t (:inherit carbon-faded))))
    '(markdown-language-info-face           ((t (:inherit carbon-default))))
    '(markdown-language-keyword-face        ((t (:inherit carbon-default))))
    '(markdown-line-break-face              ((t (:inherit carbon-default))))
    '(markdown-link-face                    ((t (:inherit carbon-salient))))
    '(markdown-link-title-face              ((t (:inherit carbon-default))))
    '(markdown-list-face                      ((t (:inherit carbon-faded))))
    '(markdown-markup-face                    ((t (:inherit carbon-faded))))
    '(markdown-math-face                    ((t (:inherit carbon-default))))
    '(markdown-metadata-key-face              ((t (:inherit carbon-faded))))
    '(markdown-metadata-value-face            ((t (:inherit carbon-faded))))
    '(markdown-missing-link-face            ((t (:inherit carbon-default))))
    '(markdown-plain-url-face               ((t (:inherit carbon-default))))
    '(markdown-pre-face                     ((t (:inherit carbon-default))))
    '(markdown-reference-face               ((t (:inherit carbon-salient))))
    '(markdown-strike-through-face            ((t (:inherit carbon-faded))))
    '(markdown-table-face                   ((t (:inherit carbon-default))))
    '(markdown-url-face                     ((t (:inherit carbon-salient))))

    ;; --- Magit (WIP) ---------------------------------------------------
    '(magit-blame-highlight                  ((t (:inherit (highlight)))))
    '(magit-diff-added-highlight             ((t (:inherit (highlight carbon-salient carbon-strong)))))
    '(magit-diff-base-highlight              ((t (:inherit (highlight)))))
    '(magit-diff-context-highlight           ((t (:inherit (highlight carbon-faded)))))
    '(magit-diff-file-heading-highlight      ((t (:inherit (highlight carbon-strong)))))
    '(magit-diff-hunk-heading-highlight      ((t (:inherit (carbon-default)))))
    '(magit-diff-our-highlight               ((t (:inherit (highlight)))))
    '(magit-diff-removed-highlight           ((t (:inherit (highlight carbon-popout carbon-strong)))))
    '(magit-diff-revision-summary-highlight  ((t (:inherit ()))))
    '(magit-diff-their-highlight             ((t (:inherit (highlight)))))
    '(magit-section-highlight                ((t (:inherit (highlight)))))

    '(magit-blame-heading                    ((t (:inherit (carbon-subtle carbon-strong)))))
    '(magit-diff-conflict-heading            ((t (:inherit (carbon-subtle carbon-strong)))))
    '(magit-diff-file-heading                ((t (:inherit (carbon-strong)))))
    '(magit-diff-hunk-heading                ((t (:inherit (carbon-subtle carbon-default)))))
    '(magit-diff-lines-heading               ((t (:inherit (carbon-subtle carbon-strong)))))
    '(magit-section-heading                  ((t (:inherit (carbon-salient carbon-strong)))))

    '(magit-bisect-bad                       ((t (:inherit carbon-default))))
    '(magit-bisect-good                      ((t (:inherit carbon-default))))
    '(magit-bisect-skip                      ((t (:inherit carbon-default))))
    '(magit-blame-date                       ((t (:inherit carbon-default))))
    '(magit-blame-dimmed                     ((t (:inherit carbon-default))))
    '(magit-blame-hash                       ((t (:inherit carbon-faded))))

    '(magit-blame-margin                     ((t (:inherit carbon-default))))
    '(magit-blame-name                       ((t (:inherit carbon-default))))
    '(magit-blame-summary                    ((t (:inherit carbon-default))))

    '(magit-branch-current                   ((t (:inherit (carbon-strong carbon-salient)))))
    '(magit-branch-local                     ((t (:inherit carbon-salient))))
    '(magit-branch-remote                    ((t (:inherit (carbon-salient)))))
    '(magit-branch-remote-head               ((t (:inherit (carbon-salient)))))
    '(magit-branch-upstream                  ((t (:inherit (carbon-salient)))))

    '(magit-cherry-equivalent                ((t (:inherit carbon-default))))
    '(magit-cherry-unmatched                 ((t (:inherit carbon-default))))

    '(magit-diff-added                       ((t (:inherit (highlight carbon-salient carbon-strong)))))
    '(magit-diff-base                        ((t (:inherit carbon-default))))
    '(magit-diff-context                     ((t (:inherit (highlight carbon-faded)))))
    '(magit-diff-file-heading-selection      ((t (:inherit carbon-default))))
    '(magit-diff-hunk-heading-selection      ((t (:inherit carbon-default))))
    '(magit-diff-hunk-region                 ((t (:inherit carbon-default))))
    '(magit-diff-lines-boundary              ((t (:inherit carbon-default))))
    '(magit-diff-our                         ((t (:inherit carbon-default))))
    '(magit-diff-removed                     ((t (:inherit (highlight carbon-popout carbon-strong)))))
    '(magit-diff-revision-summary            ((t (:inherit carbon-popout))))
    '(magit-diff-their                       ((t (:inherit carbon-default))))
    '(magit-diff-whitespace-warning          ((t (:inherit carbon-subtle))))
    '(magit-diffstat-added                   ((t (:inherit carbon-default))))
    '(magit-diffstat-removed                 ((t (:inherit carbon-default))))

    '(magit-dimmed                           ((t (:inherit carbon-faded))))
    '(magit-filename                         ((t (:inherit carbon-default))))
    '(magit-hash                             ((t (:inherit carbon-faded))))
    '(magit-head                             ((t (:inherit carbon-default))))
    '(magit-header-line                      ((t (:inherit carbon-default))))
    '(magit-header-line-key                  ((t (:inherit carbon-default))))
    '(magit-header-line-log-select           ((t (:inherit carbon-default))))

    '(magit-keyword                          ((t (:inherit carbon-salient))))
    '(magit-keyword-squash                   ((t (:inherit carbon-salient))))

    '(magit-log-author                       ((t (:inherit carbon-default))))
    '(magit-log-date                         ((t (:inherit carbon-default))))
    '(magit-log-graph                        ((t (:inherit carbon-default))))

    '(magit-mode-line-process                ((t (:inherit carbon-default))))
    '(magit-mode-line-process-error          ((t (:inherit carbon-critical))))

    '(magit-process-ng                       ((t (:inherit carbon-default))))
    '(magit-process-ok                       ((t (:inherit carbon-default))))

    '(magit-reflog-amend                     ((t (:inherit carbon-default))))
    '(magit-reflog-checkout                  ((t (:inherit carbon-default))))
    '(magit-reflog-cherry-pick               ((t (:inherit carbon-default))))
    '(magit-reflog-commit                    ((t (:inherit carbon-default))))
    '(magit-reflog-merge                     ((t (:inherit carbon-default))))
    '(magit-reflog-other                     ((t (:inherit carbon-default))))
    '(magit-reflog-rebase                    ((t (:inherit carbon-default))))
    '(magit-reflog-remote                    ((t (:inherit carbon-default))))
    '(magit-reflog-reset                     ((t (:inherit carbon-default))))
    '(magit-refname                          ((t (:inherit carbon-default))))
    '(magit-refname-pullreq                  ((t (:inherit carbon-default))))
    '(magit-refname-stash                    ((t (:inherit carbon-default))))
    '(magit-refname-wip                      ((t (:inherit carbon-default))))

    '(magit-section-heading-selection        ((t (:inherit carbon-default))))
    '(magit-section-secondary-heading        ((t (:inherit carbon-default))))
    '(magit-sequence-done                    ((t (:inherit carbon-default))))
    '(magit-sequence-drop                    ((t (:inherit carbon-default))))
    '(magit-sequence-exec                    ((t (:inherit carbon-default))))
    '(magit-sequence-head                    ((t (:inherit carbon-default))))
    '(magit-sequence-onto                    ((t (:inherit carbon-default))))
    '(magit-sequence-part                    ((t (:inherit carbon-default))))
    '(magit-sequence-pick                    ((t (:inherit carbon-default))))
    '(magit-sequence-stop                    ((t (:inherit carbon-default))))

    '(magit-signature-bad                    ((t (:inherit carbon-default))))
    '(magit-signature-error                  ((t (:inherit carbon-default))))
    '(magit-signature-expired                ((t (:inherit carbon-default))))
    '(magit-signature-expired-key            ((t (:inherit carbon-default))))
    '(magit-signature-good                   ((t (:inherit carbon-default))))
    '(magit-signature-revoked                ((t (:inherit carbon-default))))
    '(magit-signature-untrusted              ((t (:inherit carbon-default))))

    '(magit-tag                              ((t (:inherit carbon-strong))))

    ;; --- Transient ------------------------------------------------------
    ;; Set only faces that influence Magit.  See:
    ;; <https://github.com/rougier/carbon-theme/issues/43>
    '(transient-value                        ((t (:inherit default))))

    ;; --- ANSI colors ----------------------------------------------------

    '(ansi-color-black                       ((t (:inherit carbon-default))))
    '(ansi-color-bold                         ((t (:inherit carbon-strong))))
    '(ansi-color-bright-black                 ((t (:inherit carbon-strong))))
    '(ansi-color-faint                         ((t (:inherit carbon-faded))))
    '(ansi-color-fast-blink                    ((t (:inherit carbon-faded))))
    '(ansi-color-slow-blink                    ((t (:inherit carbon-faded))))
    '(ansi-color-inverse                   ((t (:inherit carbon-default-i))))
    '(ansi-color-italic                            ((t (:inherit italic))))
    '(ansi-color-underline                     ((t (:inherit carbon-faded))))
    '(ansi-color-blue           ((t (:foreground "#42A5F5")))) ;; material color blue L400
    '(ansi-color-bright-blue    ((t (:background "#BBDEFB")))) ;; material color blue L100
    '(ansi-color-cyan           ((t (:foreground "#26C6DA")))) ;; material color cyan L400
    '(ansi-color-bright-cyan    ((t (:background "#B2EBF2")))) ;; material color cyan L100
    '(ansi-color-green          ((t (:foreground "#66BB6A")))) ;; material color green L400
    '(ansi-color-bright-green   ((t (:background "#C8E6C9")))) ;; material color green L100
    '(ansi-color-magenta        ((t (:foreground "#AB47BC")))) ;; material color purple L400
    '(ansi-color-bright-magenta ((t (:background "#E1BEE7")))) ;; material color purple L100
    '(ansi-color-red            ((t (:foreground "#EF5350")))) ;; material color red L400
    '(ansi-color-bright-red     ((t (:background "#FFCDD2")))) ;; material color red L100
    '(ansi-color-white          ((t (:inherit carbon-subtle))))
    '(ansi-color-bright-white   ((t (:inherit default))))
    '(ansi-color-yellow         ((t (:foreground "#FFEE58")))) ;; material color yellow L400
    '(ansi-color-bright-yellow  ((t (:background "#FFF9C4")))) ;; material color yellow L100


    ;; --- Terminal ----------------------------------------------------
    '(term-bold        ((t (:inherit carbon-strong))))
    '(term-color-black ((t (:inherit default))))
    '(term-color-blue ((t (:foreground "#42A5F5"        ;; material color blue L400
                           :background "#BBDEFB"))))    ;; material color blue L100
    '(term-color-cyan ((t (:foreground "#26C6DA"        ;; material color cyan L400
                           :background "#B2EBF2"))))    ;; material color cyan L100
    '(term-color-green ((t (:foreground "#66BB6A"       ;; material color green L400
                            :background "#C8E6C9"))))   ;; material color green L100
    '(term-color-magenta ((t (:foreground "#AB47BC"     ;; material color purple L400
                              :background "#E1BEE7")))) ;; material color purple L100
    '(term-color-red ((t (:foreground "#EF5350"         ;; material color red L400
                          :background "#FFCDD2"))))     ;; material color red L100
    '(term-color-yellow ((t (:foreground "#FFEE58"      ;; material color yellow L400
                             :background "#FFF9C4"))))  ;; material color yellow L100
    ))

  (dolist (buffer (list " *Minibuf-0*" " *Echo Area 0*"
                        " *Minibuf-1*" " *Echo Area 1*"))
    (when (get-buffer buffer)
      (with-current-buffer buffer
        (face-remap-add-relative 'default 'carbon-faded))))
  (advice-remove 'frame-list #'carbon-frame-list-advice-selected))

;;;###autoload
(when load-file-name
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

(provide 'carbon-theme-support)
;;; carbon-theme-support.el ends here
