
(require 'package)
(add-to-list
 'package-archives
 '("melpa" . "http://melpa.milkbox.net/packages/")
 t)
(package-initialize)

(setq inhibit-splash-screen t)
(setq default-directory "~/")

(prefer-coding-system 'utf-8)

(when (fboundp 'mac-input-source)
  (defun my-mac-selected-keyboard-input-source-chage-function ()
    "英語のときはカーソルの色を***に、日本語のときは***にします."
    (let ((mac-input-source (mac-input-source)))
      (set-cursor-color
       (if (string-match "\\.US$" mac-input-source)
           "salmon" "dark orchid"))))
  (add-hook 'mac-selected-keyboard-input-source-change-hook
            'my-mac-selected-keyboard-input-source-chage-function))

(if (equal window-system 'mac)
    (progn
      (let* ((size 12)
             (asciifont "HackGen");(asciifont "monospace")
             (jpfont "HackGen");(jpfont "Osaka")
             (h (* size 10))
             (fontspec (font-spec :family asciifont))
             (jp-fontspec (font-spec :family jpfont)))
        (set-face-attribute 'default nil :family asciifont :height h)
        (set-fontset-font nil 'japanese-jisx0208 jp-fontspec))
      (set-frame-height (next-frame) 60)
      (set-frame-width (next-frame) 70)
      (set-scroll-bar-mode nil)
      (tool-bar-mode 0)
      (set-frame-parameter (selected-frame) 'alpha '(85 80))
      ;; (setq mac-command-modifier (quote meta))
      ;; (setq mac-option-modifier 'nil)
      ;; (define-key global-map [ns-drag-file] 'ns-find-file)
      ))

(menu-bar-mode 0)
(setq initial-scratch-message nil)
(setq next-line-add-newlines nil)
(setq line-number-mode t)
(setq column-number-mode t)
(setq scroll-step 1)
(setq scroll-conservatively 10)
(setq auto-show-shift-amount 1)
(setq truncate-partial-width-windows nil)

(show-paren-mode)
(global-font-lock-mode t)
(setq vc-follow-symlinks t)

(add-to-list 'auto-mode-alist '("\\.rb$" . enh-ruby-mode))
(add-to-list 'interpreter-mode-alist '("ruby" . enh-ruby-mode))

(setq backup-directory-alist '(("." . "~/.Emacs.Backups")))
(setq backup-by-copying t)
(setq version-control t)
(setq kept-old-versions 0)
(setq kept-new-versions 10)
(setq delete-old-versions t)

;;;;(iswitchb-default-keybindings)
;;;;(setq iswitchb-case nil)
;;;;(iswitchb-mode t)
(icomplete-mode)
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

(auto-image-file-mode t)
(auto-compression-mode t)

(setq indent-tabs-mode nil)

(defun set-tab-width (num)
  (setq-default tab-width num)
  (setq tab-stop-list ())
  (while (< num 256)
    (setq tab-stop-list (append tab-stop-list (list num)))
    (setq num (+ num tab-width))
    )
  )
(set-tab-width 4)



(global-set-key "\M-`" 'toggle-scroll-bar)
(global-set-key "\C-x\C-t" 'show-paren-mode)
(global-set-key "\C-h" 'backward-delete-char)
(global-set-key "\C-t" 'other-window)
(global-set-key "\C-x\C-z" nil)

(load "dired")
(define-key dired-mode-map "\C-t" 'other-window)

(global-set-key "\C-\\" 'indent-region)
;;(global-set-key "\C-\\" 'bury-buffer)

(global-set-key (kbd "M-z")
                (lambda () (interactive) (switch-to-buffer nil)))
(global-set-key (kbd "M-n") 'switch-to-next-buffer)
(global-set-key (kbd "M-p") 'switch-to-prev-buffer)

(global-set-key "\M-o" 'toggle-truncate-lines)
(global-set-key "\M-g" 'goto-line)
(global-set-key "\C-x\C-b" 'electric-buffer-list)
(global-set-key "\C-x\M-b" 'list-buffers)
(global-set-key "\C-x\C-q" 'read-only-mode)


(define-prefix-command 'quux)
(global-set-key (kbd "C-q") 'quux)

(global-set-key
 "\C-q\C-q"
 (lambda ()
   (interactive)
   (save-buffer)
   (read-only-mode)))

(global-set-key
 "\C-q\C-k"
 (lambda ()
   (interactive)
   (save-buffer)
   (kill-buffer)))

(global-set-key "\C-q\t" 'self-insert-command)

(setq auto-save-interval 1000)
(setq auto-save-timeout 600)

(if window-system
    (progn
      (set-face-foreground 'mode-line "white")
      (set-face-background 'mode-line "red")
      ;;(set-face-background 'mode-line "tomato3")
      (set-face-foreground 'default "gray83")
      (set-face-background 'default "black")
      ;;(set-face-background 'default "gray25")
      (set-face-background 'cursor "salmon")
      (set-face-background 'mouse "yellow")))

;; outline-mode
(setq outline-regexp "#+")
(add-to-list 'auto-mode-alist '("\\.diary\\.txt\\'" . outline-mode))
(add-to-list 'auto-mode-alist '("\\.note\\.txt\\'" . outline-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . outline-mode))
(add-hook 'outline-mode-hook 'outline-hide-body)

;; face
(set-face-foreground 'font-lock-function-name-face     "#92db00")          ; 1 88ff11
;;(set-face-bold-p 'font-lock-function-name-face t)                          ; 1
;;(set-face-background 'font-lock-function-name-face     "#5b6b00")          ; 1 6f770e
(set-face-foreground 'font-lock-variable-name-face     "#ffd700")          ; 2 gold
(set-face-foreground 'font-lock-keyword-face           "#ffa500")          ; 3 orange
(set-face-foreground 'font-lock-comment-face           "#cd6600")          ; 4 DarkOrange3
(set-face-foreground 'font-lock-type-face              "#a0522d")          ; 5 sienna
(set-face-foreground 'font-lock-constant-face          "#b8860b")          ; 6 dark goldenrod
(set-face-foreground 'font-lock-builtin-face           "#b8b50b")          ; 7
(set-face-foreground 'font-lock-string-face            "#9cd62a")          ; 8

(set-face-foreground 'font-lock-comment-delimiter-face "#58c924")          ; green
(set-face-foreground 'font-lock-preprocessor-face      "yellow")
(set-face-foreground 'font-lock-negation-char-face     "red")

;; white spaces
(setq whitespace-action '(cleanup auto-cleanup))
(setq whitespace-global-modes '(not dired-mode tar-mode))
(global-whitespace-mode t)
(setq whitespace-style '(face           ; faceで可視化
                         trailing       ; 行末
                         tabs           ; タブ
                         tab-mark
                         newline
                         newline-mark
                         ))
(setq whitespace-display-mappings
      '((tab-mark     ?\t    [?\xbb ?\t] )
        (newline-mark ?\n    [?\x2039 ?\n] )))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(blink-cursor-blinks 0)
 '(blink-cursor-delay 60)
 '(blink-cursor-interval 0.1)
 '(blink-cursor-mode t)
 '(comment-column 2)
 '(package-selected-packages (quote (magit yaml-mode go-mode enh-ruby-mode)))
 '(show-paren-delay 0)
 '(visible-cursor t))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(region ((t (:background "dark slate gray"))))
 '(show-paren-match ((t (:background "DarkCyan" :foreground "LightYellow"))))
 '(whitespace-empty ((t (:background "saddle brown" :foreground "firebrick"))))
 '(whitespace-newline ((t (:foreground "gray42"))))
 '(whitespace-tab ((t (:foreground "gray35"))))
 '(whitespace-trailing ((t (:background "gray30")))))

(add-hook 'emacs-lisp-mode-hook
          (lambda ()
            (setq indent-tabs-mode nil)
            (setq comment-column 4)))

(setq load-path (cons "~/nox/el" load-path))
(load-library "wrid.el")
(setq wrid-directory "~/D/diary")

(defun now ()
  (interactive)
  (insert (format-time-string "%H:%M:%S" (current-time))))

(defun sewing-machine (&optional length)
  (interactive "P")
  (if (equal length nil) (setq length 59))
  (back-to-indentation)
  (reindent-then-newline-and-indent)
  (previous-line)
  (indent-for-tab-command)
  (comment-indent)
  (while (< (current-column) length) (insert " -"))
  (back-to-indentation)
  (next-line))

(global-set-key "\C-q\C-g" 'keyboard-quit)
(global-set-key "\C-q\C-d" 'now)
(global-set-key "\C-q\C-n" 'sewing-machine)
(global-set-key "\C-qv" 'describe-variable)
(global-set-key "\C-qk" 'describe-key)
(global-set-key "\C-qf" 'describe-function)

(global-set-key "\C-q\C-m"
                (lambda ()
                  (interactive)
                  (outline-insert-heading)
                  (now)
                  (insert " ")))

;; copy bindings META to Super
(let* ((start ?a)
       (end   ?z)
       (i     start))
  (while (< i end)
    (global-set-key
     (kbd (format "s-%c" i))
     (lookup-key global-map (kbd (format "M-%c" i))))
    (setq i (1+ i))))

