
(setq default-bg   (if (equal window-system 'mac) "#161616" "color-16"))
(setq mode-line-fg (if (equal window-system 'mac) "#f5deb3" "black"))
(setq mode-line-bg (if (equal window-system 'mac) "#b03000" "white"))



(deftheme nature
  "Daishizen")

(custom-theme-set-variables
 'nature
 '(comment-column 2)
 '(rainbow-delimiters-max-face-count 4)
 '(show-paren-delay 0)
 '(blink-cursor-blinks 0)
 '(blink-cursor-delay 60)
 '(blink-cursor-interval 0.1)
 '(blink-cursor-mode t)
 )

(custom-theme-set-faces
 'nature
 '(rainbow-delimiters-depth-1-face
   ((t (:inherit rainbow-delimiters-base-face :foreground "#f60"))))
 '(rainbow-delimiters-depth-2-face
   ((t (:inherit rainbow-delimiters-base-face :foreground "#fd0"))))
 '(rainbow-delimiters-depth-3-face
   ((t (:inherit rainbow-delimiters-base-face :foreground "#7e0"))))
 '(rainbow-delimiters-depth-4-face
   ((t (:inherit rainbow-delimiters-base-face :foreground "#0ed"))))

 `(default ((t (:foreground "#ccc" :background ,default-bg))))
 '(fringe  ((t (:background "#141414"))))
 '(cursor  ((t (:foreground "#fff" :background "#e5cead"))))

 '(highlight ((t (:foreground "#000" :background "#b0b09a"))))
 '(minibuffer-prompt ((t (:foreground "#ff8010"))))

 '(font-lock-function-name-face ((t (:foreground "#82eb00")))) ; 1
 '(font-lock-variable-name-face ((t (:foreground "#e6d000")))) ; 2
 '(font-lock-keyword-face       ((t (:foreground "#ffb500")))) ; 3
 '(font-lock-comment-face       ((t (:foreground "#703e20")))) ; 4
 '(font-lock-type-face          ((t (:foreground "#e07c00")))) ; 5
 '(font-lock-constant-face      ((t (:foreground "#8b5513" :bold t)))) ; 6
 '(font-lock-builtin-face       ((t (:foreground "#8b7755")))) ; 7
 '(font-lock-string-face        ((t (:foreground "#7ab610")))) ; 8

 '(font-lock-comment-delimiter-face ((t (:foreground "#58a974" :italic t))))
 '(font-lock-preprocessor-face      ((t (:foreground "#ffff00"))))
 '(font-lock-negation-char-face     ((t (:foreground "#ff0000"))))

 '(font-lock-doc-face        ((t (:foreground "orange"))))
 '(font-lock-doc-string-face ((t (:foreground "violet"))))
 '(font-lock-warning-face    ((t (:foreground "#d39" :bold t))))

 `(mode-line
   ((t (:foreground ,mode-line-fg :background ,mode-line-bg :box nil))))
 '(mode-line-highlight
   ((t (:foreground "#fff" :box nil))))
 '(mode-line-inactive
   ((t (:foreground "#ddc" :background "#252525" :box nil))))

 '(region ((t (:background "#455"))))
 '(show-paren-match ((t (:background "#477"))))
 '(whitespace-empty ((t (:background "saddle brown" :foreground "firebrick"))))
 '(whitespace-newline ((t (:foreground "gray42"))))
 '(whitespace-tab ((t (:foreground "gray35"))))
 '(whitespace-trailing ((t (:background "gray30")))))

(provide-theme 'nature)

