(setq inferior-lisp-program "clisp")
(add-to-list 'custom-theme-load-path "~/.emacs.d/color-theme/")
;; Scheme config
;; Enable Quack mode

;; This hook lets you use your theme colours instead of quack's ones.
;(defun scheme-mode-quack-hook ()
;(require 'quack)
;(setq quack-fontify-style 'emacs))
;(add-hook 'scheme-mode-hook 'scheme-mode-quack-hook)
(setq scheme-program-name "mit-scheme")

(require 'package)
(add-to-list 'package-archives
	     '("elpa" . "http://trmoney.com/elpa/"))
(add-to-list 'package-archives
	     '("melpa" . "http://melpa.milkbox.net/packages/"))
; smartparens
(package-initialize)
(smartparens-global-mode t)
(sp-pair "'" nil :actions :rem)

; rainbow-delimiters
(add-to-list 'custom-theme-load-path "~/.emacs.d/elpa/rainbow-delimiters-20131015.404")
(require 'rainbow-delimiters)
(add-hook 'scheme-mode-hook 'rainbow-delimiters-mode)

;(switch-to-buffer (find-file "~/play/scheme/sicp2.scm"))
;(pop-to-buffer (find-file "~/play/scheme/sicp2.scm"))
;(initial-buffer-choice "~/play/scheme/sicp2.scm")
(desktop-save-mode 1)
