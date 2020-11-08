
(require 'seq)

(setq hostname (let*
                   ((host (system-name))
                    (dot (seq-position host ?.)))
                 (downcase (seq-subseq host 0 dot))))

(setq load-path (cons "~/nox/el" load-path))
(load-library "local.el")
(load-library "random.el")
(load-library "misc.el")
(load-library "wrid.el")
(setq wrid-directory "~/D/Diary")

(require 'package)
(add-to-list
 'package-archives
 '("melpa" . "http://melpa.milkbox.net/packages/")
 t)
(package-initialize)

(setq inhibit-splash-screen t)
(setq default-directory "~/")

(prefer-coding-system 'utf-8)

(if (equal window-system 'mac)
    (progn
      (let* ((h (* my-fontsize 10))
             (fontspec (font-spec :family my-asciifont))
             (jp-fontspec (font-spec :family my-jpfont)))
        (set-face-attribute 'default nil :family my-asciifont :height h)
        (set-fontset-font nil 'japanese-jisx0208 jp-fontspec))
      (set-frame-height (next-frame) 60)
      (set-frame-width (next-frame) 70)
      (set-scroll-bar-mode nil)
      (tool-bar-mode 0)
      (set-frame-parameter (selected-frame) 'alpha my-window-alpha)))

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

(setq backup-directory-alist '(("." . "~/.backupfiles")))
(setq backup-by-copying nil)
(setq backup-by-copying-when-linked t)
(setq kept-old-versions 0)
(setq kept-new-versions 5)
(setq delete-old-versions t)

(setq version-control t)
(icomplete-mode 1)

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
    (setq num (+ num tab-width))))

(set-tab-width 4)



(global-set-key "\M-`" 'toggle-scroll-bar)
(global-set-key "\C-x\C-t" 'show-paren-mode)
(global-set-key "\C-h" 'backward-delete-char)
(global-set-key "\C-t" 'other-window)
(global-set-key "\C-x\C-z" nil)
(global-set-key "\M-z" 'zap-up-to-char)

(load "dired")
(define-key dired-mode-map "\C-t" 'other-window)

(global-set-key "\C-\\" 'indent-region)

(global-set-key "\M-n"(lambda () (interactive) (switch-to-buffer nil)))
(global-set-key "\M-p"(lambda () (interactive) (switch-to-buffer nil)))

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

;; outline-mode
(setq outline-regexp "#+")
(add-to-list 'auto-mode-alist '("\\.diary\\.txt\\'" . outline-mode))
(add-to-list 'auto-mode-alist '("\\.note\\.txt\\'" . outline-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . outline-mode))
(add-hook 'outline-mode-hook 'outline-hide-body)

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
      `((tab-mark     ?\t    [?\xbb ?\t] )
        (newline-mark ,@my-newline-mark)))




(add-hook 'emacs-lisp-mode-hook
          (lambda ()
            (setq indent-tabs-mode nil)
            (setq comment-column 4)))

(add-hook 'clojure-mode-hook 'enable-paredit-mode)
(add-hook 'emacs-lisp-mode-hook 'enable-paredit-mode)
(add-hook 'lisp-interacton-mode-hook 'enable-paredit-mode)
(add-hook 'emacs-lisp-mode-hook 'rainbow-delimiters-mode)
(add-hook 'lisp-interacton-mode-hook 'rainbow-delimiters-mode)

(global-company-mode)
(setq company-global-modes '(not outline-mode text-mode))
(setq company-idle-delay 0)
(setq company-minimum-prefix-length 3)
(setq company-selection-wrap-around t)
(define-key company-active-map (kbd "C-h") nil)
(define-key company-active-map (kbd "C-M-p") 'company-select-previous)
(define-key company-active-map (kbd "C-M-n") 'company-select-next)
(define-key company-search-map (kbd "C-M-p") 'company-select-previous)
(define-key company-search-map (kbd "C-M-n") 'company-select-next)
(define-key company-active-map (kbd "C-M-m") 'company-complete-selection)
(define-key company-search-map (kbd "C-M-m") 'company-complete-selection)
(define-key company-active-map (kbd "C-j") 'company-complete-selection)
(define-key company-search-map (kbd "C-j") 'company-complete-selection)
;(define-key company-active-map (kbd "M-m") 'company-complete-selection)
(define-key company-search-map (kbd "M-m") 'company-complete-selection)


(global-set-key "\C-q\C-g" 'keyboard-quit)
(global-set-key "\C-q\C-d" 'insert-now)
(global-set-key "\C-q\C-n" 'sewing-machine)
(global-set-key "\C-qv" 'describe-variable)
(global-set-key "\C-qk" 'describe-key)
(global-set-key "\C-qf" 'describe-function)

(global-set-key "\C-q\C-m"
                (lambda ()
                  (interactive)
                  (outline-insert-heading)
                  (insert-now)
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

(global-set-key (kbd "A-<tab>") 'other-frame)
(global-set-key (kbd "M-t") 'other-frame)

(defun revert-buffer-no-confirm ()
  (interactive)
  (cond ((not (buffer-modified-p))
         (revert-buffer :ignore-auto :noconfirm)
         (read-only-mode)
         (message "Reverted."))
        (t
         (error "The buffer has been modified"))))

(global-set-key "\M-r" 'revert-buffer-no-confirm)

