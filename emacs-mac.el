
(require 'package)
(require 'smartparens)
(require 'dash)

(setq package-archives
      '(("melpa" . "https://melpa.org/packages/")
        ("org" . "https://orgmode.org/elpa/")
        ("gnu" . "https://elpa.gnu.org/packages/")))
(package-initialize)

(setq load-path (cons "~/nox/el" load-path))
(load-library "random.el")
(load-library "misc.el")
(load-library "wrid.el")
(setq wrid-directory "~/a/wrid")

(prefer-coding-system 'utf-8)

(when (equal window-system 'mac)
  (let* ((h (* 13 10))
         (fontspec (font-spec :family "PlemolJP Console"))
         (jp-fontspec (font-spec :family "PlemolJP Console")))
    (set-face-attribute 'default nil :family "PlemolJP Console" :height h)
    (set-fontset-font nil 'japanese-jisx0208 jp-fontspec))
  (set-frame-height (next-frame) 30)
  (set-frame-width (next-frame) 100)
  (set-scroll-bar-mode nil)
  (tool-bar-mode 0))

(defun get-frame-alpha ()
  (or (cdr
       (-first
        (lambda (x) (eq (car x) 'alpha))
        (frame-parameters)))
      '(100 100)))

(defun decrease-frame-alpha (p)
  (let ((na (-map
             (lambda (i) (max 0 (- i p)))
             (get-frame-alpha))))
    (apply
     'set-frame-parameter
     (list nil 'alpha na))))

(defun increase-frame-alpha (p)
  (let ((na (-map
             (lambda (i) (min 100 (+ i p)))
             (get-frame-alpha))))
    (apply
     'set-frame-parameter
     (list nil 'alpha na))))

(global-set-key (kbd "C-c C--") (lambda () (interactive) (decrease-frame-alpha 3)))
(global-set-key (kbd "C-c C-=") (lambda () (interactive) (increase-frame-alpha 4)))



(setq inhibit-splash-screen t)
(setq default-directory "~/")
(menu-bar-mode 0)
(setq initial-scratch-message nil)
(setq next-line-add-newlines nil)
(setq line-number-mode t)
(setq column-number-mode t)
(setq scroll-step 1)
(setq scroll-conservatively 0)
(setq auto-show-shift-amount 1)
(setq truncate-partial-width-windows nil)

(show-paren-mode)
(global-font-lock-mode t)
(setq vc-follow-symlinks t)

(add-to-list 'auto-mode-alist '("\\.rb$" . enh-ruby-mode))
(add-to-list 'interpreter-mode-alist '("ruby" . enh-ruby-mode))
(add-to-list 'interpreter-mode-alist '("bb" . clojure-mode))
(add-to-list 'interpreter-mode-alist '("clojure" . clojure-mode))
(add-to-list 'interpreter-mode-alist '("clj" . clojure-mode))

(add-to-list 'magic-mode-alist '("^#!.*bb$" . clojure-mode))
(add-to-list 'auto-mode-alist '("\\.bb$" . clojure-mode))
(add-to-list 'auto-mode-alist '("\\.mjs$" . javascript-mode))

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
(global-set-key "\C-\M-\\" 'align-regexp)

(global-set-key (kbd "C-;") (lambda () (interactive) (switch-to-buffer nil)))

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
;; (global-set-key "\C-q)" 'self-insert-command)
;; (global-set-key "\C-q}" 'self-insert-command)
;; (global-set-key "\C-q]" 'self-insert-command)
;; (global-set-key "\C-q(" 'self-insert-command)
;; (global-set-key "\C-q{" 'self-insert-command)
;; (global-set-key "\C-q[" 'self-insert-command)
;; (global-set-key "\C-q\"" 'self-insert-command)
;; (global-set-key "\C-q'" 'self-insert-command)
;; (global-set-key "\C-q`" 'self-insert-command)

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
                         ;;newline
                         ;;newline-mark
                         ))
(setq whitespace-display-mappings
      `((tab-mark     ?\t [?\xbb ?\t] )
        (newline-mark ?\n [?\x21A9 ?\n])))

(add-hook 'emacs-lisp-mode-hook
          (lambda ()
            (setq indent-tabs-mode nil)
            (setq comment-column 4)))

(add-hook 'rust-mode-hook
          (lambda ()
            (setq indent-tabs-mode nil)
            (setq rust-indent-offset 2)))

(add-hook 'html-mode-hook
          (lambda ()
            (setq indent-tabs-mode nil)
            (setq html-indent-offset 2)))

(add-hook 'css-mode-hook
          (lambda ()
            (setq indent-tabs-mode nil)
            (setq css-indent-offset 2)))

(add-hook 'js-mode-hook
          (lambda ()
            (setq indent-tabs-mode nil)
            (setq js-indent-level 2)))

(add-hook 'conf-mode-hook
          (lambda ()
            (setq indent-tabs-mode nil)
            (setq conf-indent-offset 2)))

(add-hook 'c-mode-hook
          (lambda ()
            (setq indent-tabs-mode nil)))

(add-hook 'outline-mode-hook
          (lambda ()
            (setq indent-tabs-mode nil)))

(add-hook 'nxml-mode-hook
          (lambda ()
            (setq indent-tabs-mode nil)
            (setq nxml-child-indent 1)
            (setq nxml-slash-auto-complete-flag t)))

(sp-with-modes '(emacs-lisp-mode clojure-mode cider-repl-mode)
  (sp-local-pair "'" nil :actions nil))

(add-hook 'cider-repl-mode-hook      'smartparens-strict-mode)
(add-hook 'cider-repl-mode-hook      'rainbow-delimiters-mode)
(add-hook 'clojure-mode-hook         'smartparens-strict-mode)
(add-hook 'clojure-mode-hook         'rainbow-delimiters-mode)
(add-hook 'emacs-lisp-mode-hook      'smartparens-strict-mode)
(add-hook 'emacs-lisp-mode-hook      'rainbow-delimiters-mode)
(add-hook 'lisp-interacton-mode-hook 'smartparens-strict-mode)
(add-hook 'lisp-interacton-mode-hook 'rainbow-delimiters-mode)
(add-hook 'enh-ruby-mode-hook        'smartparens-strict-mode)
(add-hook 'enh-ruby-mode-hook        'rainbow-delimiters-mode)

(sp-use-paredit-bindings)

(global-company-mode)
(setq company-global-modes '(not outline-mode text-mode))
(setq company-idle-delay 0)
(setq company-minimum-prefix-length 3)
(setq company-selection-wrap-around t)
(define-key company-active-map (kbd "C-h") nil)
(define-key company-active-map (kbd "<tab>") 'company-complete-selection)
(define-key company-active-map (kbd "C-M-p") 'company-select-previous)
(define-key company-active-map (kbd "C-M-n") 'company-select-next)
(define-key company-search-map (kbd "C-M-p") 'company-select-previous)
(define-key company-search-map (kbd "C-M-n") 'company-select-next)
(define-key company-active-map (kbd "M-p") 'ignore)
(define-key company-active-map (kbd "M-n") 'ignore)
(define-key company-search-map (kbd "M-p") 'ignore)
(define-key company-search-map (kbd "M-n") 'ignore)
(define-key company-active-map (kbd "C-M-m") 'company-complete-selection)
(define-key company-search-map (kbd "C-M-m") 'company-complete-selection)
(define-key company-active-map (kbd "C-j") 'company-complete-selection)
(define-key company-search-map (kbd "C-j") 'company-complete-selection)
(define-key company-active-map (kbd "C-M-j") 'company-complete-selection)
(define-key company-search-map (kbd "C-M-j") 'company-complete-selection)
;(define-key company-active-map (kbd "M-m") 'company-complete-selection)
;(define-key company-search-map (kbd "M-m") 'company-complete-selection)


(global-set-key "\C-q\C-g" 'keyboard-quit)
(global-set-key "\C-q\C-d" 'insert-now)
(global-set-key "\C-q\C-n" 'sewing-machine)
(global-set-key "\C-qv" 'describe-variable)
(global-set-key "\C-qk" 'describe-key)
(global-set-key "\C-qf" 'describe-function)

(global-set-key "\C-q\C-m"
                (lambda ()
                  (interactive)
                  (move-end-of-line nil)
                  (insert "(")
                  (insert-now)
                  (insert ")\n")))

;; copy bindings META to Super
(let* ((start ?a)
       (end   ?z)
       (i     start))
  (while (<= i end)
    (global-set-key
     (kbd (format "s-%c" i))
     (lookup-key global-map (kbd (format "M-%c" i))))
    (setq i (1+ i))))

;(global-set-key (kbd "A-<tab>") 'other-frame)
;(global-set-key (kbd "M-t") 'other-frame)

(defun revert-buffer-no-confirm ()
  (interactive)
  (cond ((not (buffer-modified-p))
         (revert-buffer :ignore-auto :noconfirm)
         (read-only-mode)
         (message "Reverted."))
        (t
         (error "The buffer has been modified"))))

(global-set-key "\M-r" 'revert-buffer-no-confirm)

;; CIDER
;(setq cider-show-error-buffer 'always)
;;(setq cider-stacktrace-default-filters '(tooling dup java clj repl))
;;(setq cider-stacktrace-positive-filters '(all)) ;; project
;;(setq cider-show-error-buffer 'except-in-repl)
;;(setq cider-show-error-buffer 'only-in-repl)
;;(setq cider-stacktrace-detail-max 0)
;;(setq cider-stacktrace-suppressed-errors t)
;;(setq cider-stacktrace-fill-column nil)

;; clojure-mode
(defun clojure-indent-cond (indent-point state)
  (goto-char (elt state 1))
  (let ((pos -1)
        (base-col (current-column)))
    (forward-char 1)
    ;; `forward-sexp' will error if indent-point is after
    ;; the last sexp in the current sexp.
    (condition-case nil
        (while (and (<= (point) indent-point)
                    (not (eobp)))
          (clojure-forward-logical-sexp 1)
          (setq pos (1+ pos)))
      ;; If indent-point is _after_ the last sexp in the
      ;; current sexp, we detect that by catching the
      ;; `scan-error'. In that case, we should return the
      ;; indentation as if there were an extra sexp at point.
      (scan-error (setq pos (1+ pos))))
    (+ base-col (if (eq (logand pos 1) 0) 4 2))))

(add-hook 'clojure-mode-hook
          (lambda () (put-clojure-indent 'cond #'clojure-indent-cond)))

(add-to-list 'custom-theme-load-path "~/nox/el")
(setq custom-theme-directory "~/nox/el")

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes '(doom-gruvbox))
 '(custom-safe-themes
   '("636b135e4b7c86ac41375da39ade929e2bd6439de8901f53f88fde7dd5ac3561" "512ce140ea9c1521ccaceaa0e73e2487e2d3826cc9d287275550b47c04072bc4" "51c71bb27bdab69b505d9bf71c99864051b37ac3de531d91fdad1598ad247138" "545ab1a535c913c9214fe5b883046f02982c508815612234140240c129682a68" "2078837f21ac3b0cc84167306fa1058e3199bbd12b6d5b56e3777a4125ff6851" "ce4234c32262924c1d2f43e6b61312634938777071f1129c7cde3ebd4a3028da" "56044c5a9cc45b6ec45c0eb28df100d3f0a576f18eef33ff8ff5d32bac2d9700" "4fda8201465755b403a33e385cf0f75eeec31ca8893199266a6aeccb4adedfa4" "8d8207a39e18e2cc95ebddf62f841442d36fcba01a2a9451773d4ed30b632443" "251ed7ecd97af314cd77b07359a09da12dcd97be35e3ab761d4a92d8d8cf9a71" "944d52450c57b7cbba08f9b3d08095eb7a5541b0ecfb3a0a9ecd4a18f3c28948" "6945dadc749ac5cbd47012cad836f92aea9ebec9f504d32fe89a956260773ca4" "7a424478cb77a96af2c0f50cfb4e2a88647b3ccca225f8c650ed45b7f50d9525" "8a379e7ac3a57e64de672dd744d4730b3bdb88ae328e8106f95cd81cbd44e0b6" "49acd691c89118c0768c4fb9a333af33e3d2dca48e6f79787478757071d64e68" "2f8eadc12bf60b581674a41ddc319a40ed373dd4a7c577933acaff15d2bf7cc6" "a589c43f8dd8761075a2d6b8d069fc985660e731ae26f6eddef7068fece8a414" "4ff1c4d05adad3de88da16bd2e857f8374f26f9063b2d77d38d14686e3868d8d" "467dc6fdebcf92f4d3e2a2016145ba15841987c71fbe675dcfe34ac47ffb9195" "e3daa8f18440301f3e54f2093fe15f4fe951986a8628e98dcd781efbec7a46f2" "5586a5db9dadef93b6b6e72720205a4fa92fd60e4ccfd3a5fa389782eab2371b" "2853dd90f0d49439ebd582a8cbb82b9b3c2a02593483341b257f88add195ad76" "f053f92735d6d238461da8512b9c071a5ce3b9d972501f7a5e6682a90bf29725" "da75eceab6bea9298e04ce5b4b07349f8c02da305734f7c0c8c6af7b5eaa9738" "2721b06afaf1769ef63f942bf3e977f208f517b187f2526f0e57c1bd4a000350" "9d29a302302cce971d988eb51bd17c1d2be6cd68305710446f658958c0640f68" "b9761a2e568bee658e0ff723dd620d844172943eb5ec4053e2b199c59e0bcc22" "be84a2e5c70f991051d4aaf0f049fa11c172e5d784727e0b525565bb1533ec78" "680f62b751481cc5b5b44aeab824e5683cf13792c006aeba1c25ce2d89826426" "2dd4951e967990396142ec54d376cced3f135810b2b69920e77103e0bcedfba9" "f458b92de1f6cf0bdda6bce23433877e94816c3364b821eb4ea9852112f5d7dc" "a44e2d1636a0114c5e407a748841f6723ed442dc3a0ed086542dc71b92a87aee" "bf948e3f55a8cd1f420373410911d0a50be5a04a8886cabe8d8e471ad8fdba8e" "021321ae56a45794f43b41de09fb2bfca184e196666b7d7ff59ea97ec2114559" "991ca4dbb23cab4f45c1463c187ac80de9e6a718edc8640003892a2523cb6259" "1aa4243143f6c9f2a51ff173221f4fd23a1719f4194df6cef8878e75d349613d" "dc8285f7f4d86c0aebf1ea4b448842a6868553eded6f71d1de52f3dcbc960039" "683b3fe1689da78a4e64d3ddfce90f2c19eb2d8ab1bab1738a63d8263119c3f4" "3fe1ebb870cc8a28e69763dde7b08c0f6b7e71cc310ffc3394622e5df6e4f0da" "b99e334a4019a2caa71e1d6445fc346c6f074a05fcbb989800ecbe54474ae1b0" "a9abd706a4183711ffcca0d6da3808ec0f59be0e8336868669dc3b10381afb6f" "aec7b55f2a13307a55517fdf08438863d694550565dee23181d2ebd973ebd6b8" "2e05569868dc11a52b08926b4c1a27da77580daa9321773d92822f7a639956ce" "89d9dc6f4e9a024737fb8840259c5dd0a140fd440f5ed17b596be43a05d62e67" "ae426fc51c58ade49774264c17e666ea7f681d8cae62570630539be3d06fd964" "60ada0ff6b91687f1a04cc17ad04119e59a7542644c7c59fc135909499400ab8" "adaf421037f4ae6725aa9f5654a2ed49e2cd2765f71e19a7d26a454491b486eb" "443e2c3c4dd44510f0ea8247b438e834188dc1c6fb80785d83ad3628eadf9294" "5f128efd37c6a87cd4ad8e8b7f2afaba425425524a68133ac0efd87291d05874" "8b6506330d63e7bc5fb940e7c177a010842ecdda6e1d1941ac5a81b13191020e" "1cae4424345f7fe5225724301ef1a793e610ae5a4e23c023076dc334a9eb940a" "afa47084cb0beb684281f480aa84dab7c9170b084423c7f87ba755b15f6776ef" "5b9a45080feaedc7820894ebbfe4f8251e13b66654ac4394cb416fef9fdca789" "ddffe74bc4bf2c332c2c3f67f1b8141ee1de8fd6b7be103ade50abb97fe70f0c" "00cec71d41047ebabeb310a325c365d5bc4b7fab0a681a2a108d32fb161b4006" "1a1ac598737d0fcdc4dfab3af3d6f46ab2d5048b8e72bc22f50271fd6d393a00" "7e068da4ba88162324d9773ec066d93c447c76e9f4ae711ddd0c5d3863489c52" "7ea883b13485f175d3075c72fceab701b5bf76b2076f024da50dff4107d0db25" "8d3ef5ff6273f2a552152c7febc40eabca26bae05bd12bc85062e2dc224cde9a" "b54376ec363568656d54578d28b95382854f62b74c32077821fdfd604268616a" "e1f4f0158cd5a01a9d96f1f7cdcca8d6724d7d33267623cc433fe1c196848554" "7e377879cbd60c66b88e51fad480b3ab18d60847f31c435f15f5df18bdb18184" "a3010c151dc4f42d56dec26a85ae5640afc227bece71d058e394667718b66a49" "570263442ce6735821600ec74a9b032bc5512ed4539faf61168f2fdf747e0668" "70b596389eac21ab7f6f7eb1cf60f8e60ad7c34ead1f0244a577b1810e87e58c" "02f57ef0a20b7f61adce51445b68b2a7e832648ce2e7efb19d217b6454c1b644" "016f665c0dd5f76f8404124482a0b13a573d17e92ff4eb36a66b409f4d1da410" "631c52620e2953e744f2b56d102eae503017047fb43d65ce028e88ef5846ea3b" "a138ec18a6b926ea9d66e61aac28f5ce99739cf38566876dc31e29ec8757f6e2" "57e3f215bef8784157991c4957965aa31bac935aca011b29d7d8e113a652b693" "ff24d14f5f7d355f47d53fd016565ed128bf3af30eb7ce8cae307ee4fe7f3fd0" "d47f868fd34613bd1fc11721fe055f26fd163426a299d45ce69bef1f109e1e71" "8d7b028e7b7843ae00498f68fad28f3c6258eda0650fe7e17bfb017d51d0e2a2" "566c61d0e40a728d5b631ac276017ace6866574ee58a4b6fbc770e436e481d57" "333958c446e920f5c350c4b4016908c130c3b46d590af91e1e7e2a0611f1e8c5" "8146edab0de2007a99a2361041015331af706e7907de9d6a330a3493a541e5a6" "9b54ba84f245a59af31f90bc78ed1240fca2f5a93f667ed54bbf6c6d71f664ac" "1d44ec8ec6ec6e6be32f2f73edf398620bb721afeed50f75df6b12ccff0fbb15" "e2c926ced58e48afc87f4415af9b7f7b58e62ec792659fcb626e8cba674d2065" "c5ded9320a346146bbc2ead692f0c63be512747963257f18cc8518c5254b7bf5" "850bb46cc41d8a28669f78b98db04a46053eca663db71a001b40288a9b36796c" "d268b67e0935b9ebc427cad88ded41e875abfcc27abd409726a92e55459e0d01" "745d03d647c4b118f671c49214420639cb3af7152e81f132478ed1c649d4597d" "c9ddf33b383e74dac7690255dd2c3dfa1961a8e8a1d20e401c6572febef61045" "bf798e9e8ff00d4bf2512597f36e5a135ce48e477ce88a0764cfb5d8104e8163" "da53441eb1a2a6c50217ee685a850c259e9974a8fa60e899d393040b4b8cc922" "a7b20039f50e839626f8d6aa96df62afebb56a5bbd1192f557cb2efb5fcfb662" "835868dcd17131ba8b9619d14c67c127aa18b90a82438c8613586331129dda63" "846b3dc12d774794861d81d7d2dcdb9645f82423565bfb4dad01204fa322dbd5" "d6844d1e698d76ef048a53cefe713dbbe3af43a1362de81cdd3aefa3711eae0d" "5f19cb23200e0ac301d42b880641128833067d341d22344806cdad48e6ec62f6" "47db50ff66e35d3a440485357fb6acb767c100e135ccdf459060407f8baea7b2" "1704976a1797342a1b4ea7a75bdbb3be1569f4619134341bd5a4c1cfb16abad4" "c2aeb1bd4aa80f1e4f95746bda040aafb78b1808de07d340007ba898efa484f5" "cbdf8c2e1b2b5c15b34ddb5063f1b21514c7169ff20e081d39cf57ffee89bc1e" "1d5e33500bc9548f800f9e248b57d1b2a9ecde79cb40c0b1398dec51ee820daf" "97db542a8a1731ef44b60bc97406c1eb7ed4528b0d7296997cbb53969df852d6" "e6f3a4a582ffb5de0471c9b640a5f0212ccf258a987ba421ae2659f1eaa39b09" "266ecb1511fa3513ed7992e6cd461756a895dcc5fef2d378f165fed1c894a78c" "23c806e34594a583ea5bbf5adf9a964afe4f28b4467d28777bcba0d35aa0872e" "b186688fbec5e00ee8683b9f2588523abdf2db40562839b2c5458fcfb322c8a4" "234dbb732ef054b109a9e5ee5b499632c63cc24f7c2383a849815dacc1727cb6" "3d47380bf5aa650e7b8e049e7ae54cdada54d0637e7bac39e4cc6afb44e8463b" "4b0e826f58b39e2ce2829fab8ca999bcdc076dec35187bf4e9a4b938cb5771dc" "fe2539ccf78f28c519541e37dc77115c6c7c2efcec18b970b16e4a4d2cd9891d" "353ffc8e6b53a91ac87b7e86bebc6796877a0b76ddfc15793e4d7880976132ae" "a6e620c9decbea9cac46ea47541b31b3e20804a4646ca6da4cce105ee03e8d0e" "c0f4b66aa26aa3fded1cbefe50184a08f5132756523b640f68f3e54fd5f584bd" "cf922a7a5c514fad79c483048257c5d8f242b21987af0db813d3f0b138dfaf53" "f6665ce2f7f56c5ed5d91ed5e7f6acb66ce44d0ef4acfaa3a42c7cfe9e9a9013" "2035a16494e06636134de6d572ec47c30e26c3447eafeb6d3a9e8aee73732396" "3d4df186126c347e002c8366d32016948068d2e9198c496093a96775cc3b3eaa" "1f1b545575c81b967879a5dddc878783e6ebcca764e4916a270f9474215289e5" "a82ab9f1308b4e10684815b08c9cac6b07d5ccb12491f44a942d845b406b0296" "c4063322b5011829f7fdd7509979b5823e8eea2abf1fe5572ec4b7af1dd78519" "6c531d6c3dbc344045af7829a3a20a09929e6c41d7a7278963f7d3215139f6a7" "fe8e3a63340a8f3ae12de08abd2817e75eee25613e8503b65170745014f066b8" "36ca8f60565af20ef4f30783aa16a26d96c02df7b4e54e9900a5138fb33808da" default))
 '(package-selected-packages
   '(smartparens exec-path-from-shell zerodark-theme rainbow-mode rainbow-delimiters rainbow-blocks powerline haml-mode ewal-doom-themes enh-ruby-mode eink-theme company cider ample-theme afternoon-theme abyss-theme))
 '(warning-suppress-types '((comp))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
