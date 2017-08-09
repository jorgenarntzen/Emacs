;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                  init.el                  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; For at emacs skal fungere optimalt, er det nødvendig å spesifisere
;; environment som er i overensstemmelse med Bash:
(setenv "PATH" "/usr/local/bin:/usr/local/sbin:/usr/bin:/usr/sbin:/home/jorgen/.local/bin:/home/jorgen/bin")

;; Følgende sørger for at pakker initialiseres:
(package-initialize)

;; Med benchmark-init kan det skrives rapporter om hvor lang tid hver modul
;; bruker på å initialisere ved oppstart av Emacs:
(add-to-list 'load-path "~/.emacs.d/elpa/benchmark-init-20150905.238/")
(require 'benchmark-init)
(benchmark-init/activate)

;; Identifiserer og laster inn fil med farger og variabler som er spesifisert
;; i innstillinger:
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)

;; Emacs skal kjøre som server:
(server-start)

;; Følgende variabel slår av alarmen når man treffer toppen eller bunnen av
;; bufferen:
(setq ring-bell-function 'ignore)

;; Følgende gjør at det holder å svare "y" eller "n" heller enn "yes" eller
;; "no":
(fset 'yes-or-no-p 'y-or-n-p)

;; Hvis Emacs kjører i vindu er det ønskelig med meny. I terminal er menyen
;; forstyrrende:
;;(if window-system
;;    (menu-bar-mode t)
;;  (menu-bar-mode -1))

;; Følgende pakke utvider Emacs Lisp med konvensjoner fra det vanlige
;; Lisp-språket:
(require 'cl-lib)

;; use-package er makroer som legger til rette for ryddig pakkebehandling:
(add-to-list 'load-path "~/.emacs.d/elpa/bind-key-20161218.1520/")
(require 'bind-key)

(add-to-list 'load-path "~/.emacs.d/elpa/diminish-20170419.1036/")
(require 'diminish)

(add-to-list 'load-path "~/.emacs.d/elpa/use-package-20170710.1234/")
(require 'use-package)

;; Denne seksjonen inneholder tilpasninger for oppstart, og er under
;; utvikling:
(defvar running-alternate-emacs nil)
(defvar running-development-emacs nil)

;; Følgende variabler gjemmer minor-modes fra modeline som ikke kan
;; spesifiseres i pakke-seksjonen:
(diminish 'auto-fill-function)
(diminish 'isearch-mode)
(diminish 'visual-line-mode)

(eval-after-load "footnote"              '(diminish 'footnote-mode))
(eval-after-load "highlight-indentation" '(diminish 'highlight-indentation-mode))
(eval-after-load "org-table"             '(diminish 'orgtbl-mode))
(eval-after-load "reftex"                '(diminish 'reftex-mode))

;; Følgende aktiverer deaktiverte kommandoer:
(put 'downcase-region             'disabled nil)
(put 'erase-buffer                'disabled nil)
(put 'eval-expression             'disabled nil)
(put 'narrow-to-page              'disabled nil)
(put 'narrow-to-region            'disabled nil)
(put 'set-goal-column             'disabled nil)
(put 'upcase-region               'disabled nil)
(put 'TeX-narrow-to-group         'disabled nil)
(put 'LaTeX-narrow-to-environment 'disabled nil)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;               Definisjoner                ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defconst emacs-start-time (current-time))

(defsubst hook-into-modes (func &rest modes)
  (dolist (mode-hook modes) (add-hook mode-hook func)))

(defvar lisp-modes '(emacs-lisp-mode
		     inferior-emacs-lisp-mode
		     ielm-mode
		     lisp-mode
		     inferior-lisp-mode
		     lisp-interaction-mode
		     slime-repl-mode))

(defvar lisp-mode-hooks
  (mapcar (function
           (lambda (mode)
             (intern
              (concat (symbol-name mode) "-hook"))))
          lisp-modes))


(unless noninteractive
  (message "Loading %s..." load-file-name))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                Keybindings                ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; C-

(bind-key* "<C-return>" #'other-window)

(bind-key "C-z" #'delete-other-windows)


;; M-

(bind-key "M-W" #'mark-word)


(defun mark-line (&optional arg)
  (interactive "p")
  (beginning-of-line)
  (let ((here (point)))
    (dotimes (i arg)
      (end-of-line))
    (set-mark (point))
    (goto-char here)))

(bind-key "M-L" #'mark-line)


(defun mark-sentence (&optional arg)
  (interactive "P")
  (backward-sentence)
  (mark-end-of-sentence arg))

(bind-key "M-S" #'mark-sentence)


(bind-key "M-X" #'mark-sexp)
(bind-key "M-D" #'mark-defun)

(bind-key "M-g c" #'goto-char)
(bind-key "M-g l" #'goto-line)


;; C-x

(defun delete-current-buffer-file ()
  "Delete the current buffer and the file connected with it"
  (interactive)
  (let ((filename (buffer-file-name))
        (buffer (current-buffer))
        (name (buffer-name)))
    (if (not (and filename (file-exists-p filename)))
        (kill-buffer buffer)
      (when (yes-or-no-p "Are you sure you want to remove this file? ")
        (delete-file filename)
        (kill-buffer buffer)
        (message "File '%s' successfully removed" filename)))))

(bind-key "C-x K" #'delete-current-buffer-file)


(bind-key "C-x v H" #'vc-region-history)


;; C-x C-

(defun find-alternate-file-with-sudo ()
  (interactive)
  (find-alternate-file (concat "/sudo::" (buffer-file-name))))

(bind-key "C-x C-v" #'find-alternate-file-with-sudo)


;; C-c

(defun delete-current-line (&optional arg)
  (interactive "p")
  (let ((here (point)))
    (beginning-of-line)
    (kill-line arg)
    (goto-char here)))

(bind-key "C-c d" #'delete-current-line)


(eval-when-compile
  (defvar emacs-min-height)
  (defvar emacs-min-width))

(defvar emacs-min-top 50)
(defvar emacs-min-left 550)
(defvar emacs-min-height 40)
(defvar emacs-min-width 85)
(defvar emacs-min-left-fringe 0)
(defvar emacs-min-right-fringe 0)

(let ((frame-alist
       (list (cons 'top          emacs-min-top)
             (cons 'left         emacs-min-left)
             (cons 'height       emacs-min-height)
             (cons 'width        emacs-min-width)
             (cons 'left-fringe  emacs-min-left-fringe)
             (cons 'right-fringe emacs-min-right-fringe))))
  (setq initial-frame-alist frame-alist))

(defun emacs-min ()
  (interactive)

  (set-frame-parameter (selected-frame) 'fullscreen nil)
  (set-frame-parameter (selected-frame) 'vertical-scroll-bars nil)
  (set-frame-parameter (selected-frame) 'horizontal-scroll-bars nil)

  (set-frame-parameter (selected-frame) 'top emacs-min-top)
  (set-frame-parameter (selected-frame) 'left emacs-min-left)
  (set-frame-parameter (selected-frame) 'height emacs-min-height)
  (set-frame-parameter (selected-frame) 'width emacs-min-width)
  (set-frame-parameter (selected-frame) 'left-fringe emacs-min-left-fringe)
  (set-frame-parameter (selected-frame) 'right-fringe emacs-min-right-fringe))

(if window-system
    (add-hook 'after-init-hook 'emacs-min))

(defun emacs-max ()
  (interactive)

  (set-frame-parameter (selected-frame) 'fullscreen 'fullboth)
  (set-frame-parameter (selected-frame) 'vertical-scroll-bars nil)
  (set-frame-parameter (selected-frame) 'horizontal-scroll-bars nil))

(defun emacs-toggle-size ()
  (interactive)
  (if (> (cdr (assq 'width (frame-parameters))) 100)
      (emacs-min)
    (emacs-max)))

(bind-key "C-c m" #'emacs-toggle-size)


(defun emacs-toggle-transparency ()
  (interactive)
  (let ((alpha (frame-parameter nil 'alpha)))
    (set-frame-parameter
     nil 'alpha
     (if (eql (cond ((numberp alpha) alpha)
                    ((numberp (cdr alpha)) (cdr alpha))
                    ((numberp (cadr alpha)) (cadr alpha)))
              100)
         '(85 . 85) '(100 . 100)))))

(bind-key "C-c t" #'emacs-toggle-transparency)


(defun view-clipboard ()
  (interactive)
  (delete-other-windows)
  (switch-to-buffer "*Clipboard*")
  (let ((inhibit-read-only t))
    (erase-buffer)
    (clipboard-yank)
    (goto-char (point-min))))

(bind-key "C-c V" #'view-clipboard)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                  Pakker                   ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Den første listen er biblioteker:

(use-package anaphora           :defer t :load-path "elpa/anaphora-20140728.1536/"         )
(use-package button-lock        :defer t :load-path "elpa/button-lock-20150223.554/"       )
(use-package concurrent         :defer t :load-path "elpa/concurrent-20161228.1930/"       )
(use-package ctable             :defer t :load-path "elpa/ctable-20140304.1659/"           )
(use-package dash               :defer t :load-path "elpa/dash-20170727.212/"              )
(use-package deferred           :defer t :load-path "elpa/deferred-20170531.2135/"         )
(use-package epc                :defer t :load-path "elpa/epc-20140609.2234/"              )
(use-package epl                :defer t :load-path "elpa/epl-20150517.433/"               )
(use-package f                  :defer t :load-path "elpa/f-20170404.1039/"                )
(use-package fuzzy              :defer t :load-path "elpa/fuzzy-20150729.2037/"            )
(use-package gh                 :defer t :load-path "elpa/gh-20170512.2049/"               )
(use-package git                :defer t :load-path "elpa/git-20140128.241/"               )
(use-package gntp               :defer t :load-path "elpa/gntp-20141024.1950/"             )
(use-package helm-core          :defer t :load-path "elpa/helm-core-20170806.2108/"        )
(use-package ht                 :defer t :load-path "elpa/ht-20161015.1945/"               )
(use-package jedi-core          :defer t :load-path "elpa/jedi-core-20170121.610/"         )
(use-package let-alist          :defer t :load-path "elpa/let-alist-1.0.5/"                )
(use-package log4e              :defer t :load-path "elpa/log4e-20170401.604/"             )
(use-package logito             :defer t :load-path "elpa/logito-20120225.1255/"           )
(use-package makey              :defer t :load-path "elpa/makey-20131231.630/"             )
(use-package marshal            :defer t :load-path "elpa/marshal-20160807.1954/"          )
(use-package pcache             :defer t :load-path "elpa/pcache-20170105.1414/"           )
(use-package pkg-info           :defer t :load-path "elpa/pkg-info-20150517.443/"          )
(use-package popup              :defer t :load-path "elpa/popup-20160709.729/"             )
(use-package popwin             :defer t :load-path "elpa/popwin-20150315.600/"            )
(use-package pos-tip            :defer t :load-path "elpa/pos-tip-20150318.813/"           )
(use-package powerline          :defer t :load-path "elpa/powerline-20170708.1442/"        )
(use-package python-environment :defer t :load-path "elpa/python-environment-20150310.153/")
(use-package pyvenv             :defer t :load-path "elpa/pyvenv-20170224.538/"            )
(use-package rich-minority      :defer t :load-path "elpa/rich-minority-20160725.1255/"    )
(use-package s                  :defer t :load-path "elpa/s-20170428.1026/"                )
(use-package seq                :defer t :load-path "elpa/seq-2.20/"                       )
(use-package tablist            :defer t :load-path "elpa/tablist-20170219.1935/"          )
(use-package uuidgen            :defer t :load-path "elpa/uuidgen-20140918.1601/"          )
(use-package web                :defer t :load-path "elpa/web-20141231.1201/"              )
(use-package web-server         :defer t :load-path "elpa/web-server-20140905.1706/"       )
(use-package websocket          :defer t :load-path "elpa/websocket-20170610.2117/"        )
(use-package xml-rpc            :defer t :load-path "elpa/xml-rpc-20160430.1458/"          )


;; Den andre listen er grafiske temaer:

(use-package solarized-theme    :defer t :load-path "elpa/solarized-theme-20170430.800/"   )
(use-package zenburn-theme      :defer t :load-path "elpa/zenburn-theme-20170511.1337/"    )


;; Den tredje listen er pakker som kommer pre-installert med Emacs:

(use-package abbrev
  :commands abbrev-mode
  :diminish abbrev-mode

  :init
  (hook-into-modes #'abbrev-mode
		   'text-mode-hook
		   'prog-mode-hook
		   'LaTeX-mode-hook)

  :config
  (if (file-exists-p abbrev-file-name)
      (quietly-read-abbrev-file))

  (add-hook 'expand-load-hook
	    (lambda ()
	      (add-hook 'expand-expand-hook 'indent-according-to-mode)
	      (add-hook 'expand-jump-hook 'indent-according-to-mode))))


(use-package align
  :bind (("M-[" . align-code)
	 ("C-c [" . align-regexp))
  :commands align

  :preface
  (defun align-code (beg end &optional arg)
    (interactive "rP")
    (if (null arg)
	(align beg end)
      (let ((end-mark (copy-marker end)))
	(indent-region beg end-mark nil)
	(align beg end-mark)))))


(use-package allout
  :disabled t
  :diminish allout-mode
  :commands allout-mode

  :config
  (defvar allout-unprefixed-keybindings nil)

  (defun my-allout-mode-hook ()
    (dolist (mapping '((?b . allout-hide-bodies)
		       (?c . allout-hide-current-entry)
		       (?l . allout-hide-current-leaves)
		       (?i . allout-show-current-branches)
		       (?e . allout-show-entry)
		       (?0 . allout-show-to-offshoot)))
      (eval `(bind-key ,(concat (format-kbd-macro allout-command-prefix)
				" " (char-to-string (car mapping)))
		       (quote ,(cdr mapping))
		       allout-mode-map)))

    (if (memq major-mode lisp-modes)
	(unbind-key "C-k" allout-mode-map)))

  (add-hook 'allout-mode-hook 'my-allout-mode-hook))


(use-package autorevert
  :commands auto-revert-mode
  :diminish auto-revert-mode

  :init
  (add-hook 'find-file-hook #'(lambda () (auto-revert-mode 1))))


(use-package bookmark
  :load-path "elpa/bookmark+-20170731.1658/"
  :defer 10
  :config
  (use-package bookmark+))


(use-package cmake-mode
  :mode ("\\.cmake\\'" . cmake-mode))


(use-package compile
  :bind (("C-c c" . compile)
	 ("M-O" . show-compilation))

  :preface
  (defun show-compilation ()
    (interactive)
    (let ((compile-buf
	   (catch 'found
	     (dolist (buf (buffer-list))
	       (if (string-match "\\*compilation\\*" (buffer-name buf))
		   (throw 'found buf))))))
      (if compile-buf
	  (switch-to-buffer-other-window compile-buf)
	(call-interactively 'compile))))

  (defun compilation-ansi-color-process-output ()
    (ansi-color-process-output nil)
    (set (make-local-variable 'comint-last-output-start)
	 (point-marker)))

  :config
  (add-hook 'compilation-filter-hook
	    #'compilation-ansi-color-process-output))


(use-package css-mode
  :mode "\\.css\\'")


(use-package dired
  :bind ("C-c J" . dired-double-jump)

  :preface
  (defvar mark-files-cache (make-hash-table :test #'equal))

  (defun mark-similar-versions (name)
    (let ((pat name))
      (if (string-match "^\\(.+?\\)-[0-9._-]+$" pat)
	  (setq pat (match-string 1 pat)))
      (or (gethash pat mark-files-cache)
	  (ignore (puthash pat t mark-files-cache)))))

  (defun dired-mark-similar-version ()
    (interactive)
    (setq mark-files-cache (make-hash-table :test #'equal))
    (dired-mark-sexp '(mark-similar-versions name)))

  (defun dired-double-jump (first-dir second-dir)
    (interactive
     (list (read-directory-name "First directory: "
				(expand-file-name "~")
				nil nil "dl/")
	   (read-directory-name "Second directory: "
				(expand-file-name "~")
				nil nil "Archives/")))
    (dired first-dir)
    (dired-other-window second-dir))

  :config
  (use-package dired-x
    :config
    (setq-default dired-omit-files-p t))

  (use-package dired+
    :load-path "elpa/dired+-20170630.752/"
    :config
    (unbind-key "M-s f" dired-mode-map))

  (use-package diredful
    :disabled t
    :load-path "elpa/diredful-20160529.1317/"
    :config
    (diredful-mode 1))

  (use-package dired-details
    :load-path "elpa/dired-details-20130824.458/"
    :config
    (dired-details-install))

  (bind-key "l" #'dired-up-directory dired-mode-map)

  (defun my-dired-switch-window ()
    (interactive)
    (if (eq major-mode 'sr-mode)
	(call-interactively #'sr-change-window)
      (call-interactively #'other-window)))

  (bind-key "<tab>" #'my-dired-switch-window dired-mode-map)

  (bind-key "M-!" #'async-shell-command dired-mode-map)
  (unbind-key "M-G" dired-mode-map)

  (defadvice dired-omit-startup (after diminish-dired-omit activate)
    "Make sure to remove \"Omit\" from the modeline."
    (diminish 'dired-omit-mode) dired-mode-map)

  (defadvice dired-next-line (around dired-next-line+ activate)
    "Replace current buffer if file is a directory."
    ad-do-it
    (while (and (not (eobp)) (not ad-return-value))
      (forward-line)
      (setq ad-return-value(dired-move-to-filename)))
    (when (eobp)
      (forward-line -1)
      (setq ad-return-value(dired-move-to-filename))))

  (defadvice dired-previous-line (around dired-previous-line+ activate)
    "Replace current buffer if file is a directory."
    ad-do-it
    (while (and (not (bopd)) (not ad-return-value))
      (forward-line -1)
      (setq ad-return-value(dired-move-to-filename)))
    (when (bopd)
      (call-interactively 'dired-next-line)))

  (defvar dired-omit-regexp-orig (symbol-function 'dired-omit-regexp))

  ;; Utelater filer som ignoreres av Git:
  (defun dired-omit-regexp ()
    (let ((file (expand-file-name ".git"))
	  parent-dir)
      (while (and (not (file-exists-p file))
		  (progn
		    (setq parent-dir
			  (file-name-directory
			   (directory-file-name
			    (file-name-directory file))))

		    ;; Gi opp hvis vi allerede er i den siste mappen:
		    (not (string= (file-name-directory file)
				  parent-dir))))

	;; Gå tilbake til overliggende mappe og prøv igjen:
	(setq file (expand-file-name ".git" parent-dir)))

      ;; Bruk loggede endringer hvis de finnes:
      (if (file-exists-p file)
	  (let ((regexp (funcall dired-omit-regexp-orig))
		(omitted-files
		 (shell-command-to-string "git clean -d -x -n")))
	    (if (= 0 (length omitted-files))
		regexp
	      (concat
	       regexp
	       (if (> (length regexp) 0)
		   "\\|" "")
	       "\\("
	       (mapconcat
		#'(lambda (str)
		    (concat
		     "^"
		     (regexp-quote
		      (substring str 13
				 (if (= ?/ (aref str (1- (length str))))
				     (1- (length str))
				   nil)))
		     "$"))
		(split-string omitted-files "\n" t)
		"\\|")
	       "\\)")))
	(funcall dired-omit-regexp-orig)))))


(use-package dired-toggle
  :load-path "elpa/dired-toggle-20140907.1349/"
  :bind ("C-. d" . dired-toggle)

  :preface
  (defun my-dired-toggle-mode-hook ()
    (interactive)
    (visual-line-mode 1)
    (setq-local visual-line-fringe-indicators '(nil right-curly-arrow))
    (setq-local word-wrap nil))

  :config
  (add-hook 'dired-toggle-mode-hook #'my-dired-toggle-mode-hook))


(use-package doc-view
  :if window-system
  :defer t

  :preface
  (defcustom doc-view-autofit-timer-start 1.0
    "Initial value (seconds) for the timer that delays the fitting when `doc-view-autofit-fit is called (Which is when a window configuration change occurs and a document needs to be fitted)."
    :type 'number
    :group 'doc-view)

  (defcustom doc-view-autofit-timer-inc 0.02
    "Value to increase (seconds) the timer (see `doc-view-autofit-timer-start') by, if there is another window configuration change occuring, before it runs out."
    :type 'number
    :group 'doc-view)

  (defcustom doc-view-autofit-default-fit 'width
    "The fitting type initially used when mode is enabled. Valid values are: width, height, page."
    :type 'symbol
    :group 'doc-view)

  (defvar doc-view-autofit-mode-map
    (let ((map (make-sparse-keymap)))
      (define-key map (kbd "C-c W") 'doc-view-autofit-width)
      (define-key map (kbd "C-c H") 'doc-view-autofit-height)
      (define-key map (kbd "C-c P") 'doc-view-autofit-page)
      map)
    "Keymap used by `doc-view-autofit-mode'.")

  (defvar last-displayed-doc-view-buffer nil)

  (defvar doc-view-autofit-timer nil)

  (defvar doc-view-autofit-type nil)

  (defun doc-view-autofit-set (type)
    "Set autofitting to TYPE for current buffer."
    (when doc-view-autofit-mode
      (setq doc-view-autofit-type type)
      (doc-view-autofit-fit)))

  (defun doc-view-autofit-width ()
    "Set autofitting to width for current buffer."
    (interactive) (doc-view-autofit-set 'width))

  (defun doc-view-autofit-height ()
    "Set autofitting to height for current buffer."
    (interactive) (doc-view-autofit-set 'height))

  (defun doc-view-autofit-page ()
    "Set autofitting to page for current buffer."
    (interactive) (doc-view-autofit-set 'page))

  (defun doc-view-autofit-fit ()
    "Fits the document in the selected window's buffer delayed with a timer, so multiple calls in succession don't cause as much overhead."
    (if (null doc-view-autofit-timer)
	(setq doc-view-autofit-timer
	      (run-with-timer doc-view-autofit-timer-start nil
			      (lambda ()
				(let* ((selected-window
					(cond
					 ((eq major-mode 'doc-view-mode)
					  (selected-window))
					 (t
					  (get-buffer-window last-displayed-doc-view-buffer))))
                                             (current-buffer
                                              (cond
                                               ((eq major-mode 'doc-view-mode)
                                                (current-buffer))
                                               (t
                                                (get-buffer last-displayed-doc-view-buffer))))
                                             (selected-fit
                                              (when (buffer-live-p (get-buffer current-buffer))
                                                (with-current-buffer
                                                    (get-buffer current-buffer)
                                                  doc-view-autofit-type))))
				  (when (window-live-p selected-window)
				    (with-selected-window selected-window
				      (when doc-view-autofit-timer (cancel-timer doc-view-autofit-timer))
				      (setq doc-view-autofit-timer nil)
				      (cond
				       ((eq 'width selected-fit)
					(doc-view-fit-width-to-window))
				       ((eq 'height selected-fit)
					(doc-view-fit-height-to-window))
				       ((eq 'page selected-fit)
					(doc-view-fit-page-to-window)))))))))
      (timer-inc-time doc-view-autofit-timer doc-view-autofit-timer-inc)))

  (defun get-last-displayed-doc-view-buffer ()
    (setq last-displayed-doc-view-buffer (current-buffer)))

  (define-minor-mode doc-view-autofit-mode
    "Minor mode for automatic (timer based) fitting in DocView."
    :lighter " AFit"
    :keymap doc-view-autofit-mode-map
    :group 'doc-view
    (when doc-view-autofit-mode
      (set (make-local-variable 'doc-view-autofit-type)
	   doc-view-autofit-default-fit)
      (set (make-local-variable 'doc-view-autofit-timer) nil)
      (add-hook 'window-configuration-change-hook
		'doc-view-autofit-fit nil t)
      (doc-view-autofit-fit))
    (when (not doc-view-autofit-mode)
      (remove-hook 'window-configuration-change-hook
		   'doc-view-autofit-fit t)
      (when doc-view-autofit-timer
	(cancel-timer doc-view-autofit-timer)
	(setq doc-view-autofit-timer nil))
      (setq doc-view-autofit-type nil)))

  :init
  (add-hook 'doc-view-mode-hook
	    (lambda ()
	      (auto-revert-mode)
	      (get-last-displayed-doc-view-buffer)
              (doc-view-autofit-mode t)
	      (diminish 'doc-view-autofit-mode))))


(use-package edebug
  :defer t

  :preface
  (defvar modi/fns-in-edebug nil
    "List of functions for which `edebug' is instrumented.")

  (defconst modi/fns-regexp
    (concat "(\\s-*"
	    "\\(defun\\|defmacro\\)\\s-+"
	    "\\(?1:\\(\\w\\|\\s_\\)+\\)\\_>")
    "Regexp to find defun or defmacro definition.")

  (defun modi/toggle-edebug-defun ()
    (interactive)
    (let (fn)
      (save-excursion
	(search-backward-regexp modi/fns-regexp)
	(setq fn (match-string 1))
	(mark-sexp)
	(narrow-to-region (point) (mark))
	(if (member fn modi/fns-in-edebug)
	    (progn
	      (setq modi/fns-in-edebug (delete fn modi/fns-in-edebug))
	      (eval-region (point) (mark))
	      (setq-default eval-expression-print-length 12)
	      (setq-default eval-expression-print-level 4)
	      (message "Edebug disabled: %s" fn))
	  (progn
	    (add-to-list 'modi/fns-in-edebug fn)
	    (setq-default eval-expression-print-length nil)
	    (setq-default eval-expression-print-level nil)
	    (edebug-defun)
	    (message "Edebug: %s" fn)))
	(widen)))))


(use-package ediff
  :bind (("C-. = b" . ediff-buffers)
	 ("C-. = B" . ediff-buffers3)
	 ("C-. = c" . compare-windows)
	 ("C-. = =" . ediff-files)
	 ("C-. = f" . ediff-files)
	 ("C-. = F" . ediff-files3)
	 ("C-. = r" . ediff-revision)
	 ("C-. = p" . ediff-patch-file)
	 ("C-. = P" . ediff-patch-buffer)
	 ("C-. = l" . ediff-regions-linewise)
	 ("C-. = w" . ediff-regions-wordwise))

  :init
  (defvar ctl-period-equals-map)
  (define-prefix-command 'ctl-period-equals-map)
  (bind-key "C-. =" #'ctl-period-equals-map))


(use-package epa-file
  :defer t)


(use-package eshell
  :commands (eshell eshell-command)

  :preface
  (defun eshell-initialize ()
    (defun eshell-spawn-external-command (beg end)
      "Parse and expand any history references in current input."
      (save-excursion
	(goto-char end)
	(when (looking-back "&!" beg)
	  (delete-region (match-beginning 0) (match-end 0))
	  (goto-char beg)
	  (insert "spawn"))))

    (add-hook 'eshell-expand-input-functions 'eshell-spawn-external-command)

    (defun ss (server)
      (interactive "sServer: ")
      (call-process "spawn" nil nil nil "ss" server))

    (use-package em-unix
      :defer t
      :config
      (unintern 'eshell/su nil)
      (unintern 'eshell/sudo nil)))

  :init
  (add-hook 'eshell-first-time-mode-hook 'eshell-initialize))


(use-package etags
  :bind ("M-T" . tags-search))


(use-package eww
  :bind ("A-M-g" . eww)

  :config
  (use-package eww-lnum
    :load-path "elpa/eww-lnum-20150102.712/"
    :config
    (bind-key "f" #'eww-lnum-follow eww-mode-map)
    (bind-key "F" #'eww-lnum-universal eww-mode-map)))


(use-package fetchmail-mode
  :commands fetchmail-mode)


(use-package flyspell
  :diminish flyspell-mode
  :bind (("C-c i b" . flyspell-buffer)
	 ("C-c i f" . flyspell-mode))

  :preface
  (defadvice flyspell-region (around fast-flyspell-region)
    (cl-flet ((sit-for (x) t)) ad-do-it))

  (ad-activate 'flyspell-region)

  :init
  (use-package ispell
    :bind (("C-c i c" . ispell-comments-and-strings)
	   ("C-c i d" . ispell-change-dictionary)
	   ("C-c i k" . ispell-kill-ispell)
	   ("C-c i m" . ispell-message)
	   ("C-c i r" . ispell-region))

    :init
    (setq-default ispell-program-name "/usr/bin/hunspell")
    (add-hook 'tex-mode-hook #'(lambda () (setq ispell-parser 'tex))))

  :config
  (unbind-key "C-." flyspell-mode-map))


(use-package grep
  :bind (("M-s d" . find-grep-dired)
	 ("M-s F" . find-grep)
	 ("M-s G" . grep))

  :config
  (add-hook 'grep-mode-hook #'(lambda () (use-package grep-ed)))

  (grep-apply-setting 'grep-command "egrep -NH -e ")
  (grep-apply-setting
   'grep-find-command
   '("find . -type f -print0 | xargs -P4 -0 egrep -nH " . 49)))


(use-package gud
  :disabled t
  :commands gud-gdb
  :bind ("C-. g" . show-debugger)

  :init
  (defun show-debugger ()
    (interactive)
    (let ((gud-buf
	   (catch 'found
	     (dolist (buf (buffer-list))
	       (if (string-match "\\*gud-" (buffer-name buf))
		   (throw 'found buf))))))
      (if gud-buf
	  (switch-to-buffer-other-window gud-buf)
	(call-interactively 'gud-gdb))))

  :config
  (progn
    (bind-key "<f9>" #'gud-cont)
    (bind-key "<f10>" #'gud-next)
    (bind-key "<f11>" #'gud-step)
    (bind-key "S-<f11>" #'gud-finish)))


(use-package hi-lock
  :bind (("M-o l" . highlight-lines-matching-regexp)
	 ("M-o r" . highlight-regexp)
	 ("M-o w" . highlight-phrase)))


(use-package hilit-chg
  :bind ("M-o C" . highlight-changes-mode))


(use-package hippie-exp
  :bind (("M-/" . dabbrev-expand)
	 ("M-?" . hippie-expand))

  :preface
  (autoload 'yas-expand "yasnippet" nil t)

  (defun my-yas-hippie-try-expand (first-time)
    (if (not first-time)
	(let ((yas-fallback-behavior 'return-nil))
	  (yas-expand))
      (undo 1)
      nil))

  (defun my-hippie-expand-completions (&optional hippie-expand-function)
    "Return the full list of possible generated by `hippie-expand'. The optional argument can be generated with `make-hippie-expand-function'."
    (let ((this-command 'my-hippie-expand-completions)
	  (last-command last-command)
	  (buffer-modified (buffer-modified-p))
	  (hippie-expand-function (or hippie-expand-function 'hippie-expand)))

      ;; Unngå (ding) hvis hippie-expand går tom for muligheter:
      (cl-flet ((ding))
	(while (progn
		 (funcall hippie-expand-function nil)
		 (setq last-command 'my-hippie-expand-completions)
		 (not (equal he-num -1)))))

      ;; Evaluering av fullførelsene modifiserer bufferen, men vi vil avslutte
      ;; i samme tilstand som vi begynte:
      (set-buffer-modified-p buffer-modified)

      ;; Tilbyr mulighetene i den rekkefølgen de normalt genereres:
      (delete he-search-string (reverse he-tried-table))))

  (defmacro my-ido-hippie-expand-with (hippie-expand-function)
    "Generate an interactively-callable function that offers ido-based completion using the specified hippie-expand function."
    `(call-interactively
      (lambda (&optional selection)
	(interactive
	 (let ((options (my-hippie-expand-completions ,hippie-expand-function)))
	   (if options
	       (list
		;; (ido-completing-read "Completions: " options
		(completing-read "Completions: " options)
		))))
	(if selection
	    (he-substitute-string selection t)
	  (message "No expansion found")))))

  (defun my-ido-hippie-expand ()
    "Offer ido-based completion for the word at point."
    (interactive)
    (my-ido-hippie-expand-with 'hippie-expand))

  (defun my-try-expand-company (old)
    (require 'company)
    (unless company-candidates
      (company-auto-begin))
    (if (not old)
	(progn
	  (he-init-string (he-lisp-symbol-beg) (point))
	  (if (not (he-string-member he-search-string he-tried-table))
	      (setq he-tried-table (cons he-search-string he-tried-table)))
	  (setq he-expand-list
		(and (not (equal he-search-string ""))
		     company candidates))))
    (while (and he-expand-list
		(he-string-member (car he-expand-list) he-tried-table))
      (setq he-expand-list (cdr he-expand-list)))
    (if (null he-expand-list)
	(progn
	  (if old (he-reset-string))
	  ())
      (progn
	(he-substitute-string (car he-expand-list))
	(setq he-expand-list (cdr he-expand-list))
	t)))

  (defun he-tag-beg ()
    (save-excursion
      (backward-word 1)
      (point)))

  (defun tags-complete-tag (string predicate what)
    (save-excursion

      ;; Tillat å spørre etter tag-tabellen:
      (if (eq what t)
	  (all-completions string (tags-completion-table) predicate)
	(try-completion string (tags-completion-table) predicate))))

  (defun try-expand-tag (old)
    (when tags-table-list
      (unless old
	(he-init-string (he-tag-beg) (point))
	(setq he-expand-list
	      (sort (all-completions he-search-string 'tags-complete--tag)
		    'string-lessp)))
      (while (and he-expand-list
		  (he-string-member (car he-expand-list) he tried-table))
	(setq he-expand-list (cdr he-expand-list)))
      (if (null he-expand-list)
	  (progn
	    (when old (he-reset-string))
	    ())
	(he-substitute-string (car he-expand-list))
	(setq he-expand-list (cdr he-expand-list))
	t)))

  (defun my-dabbrev-substring-search (pattern &optional reverse limit)
    (let ((result ())
	  (regpat (cond ((not hippie-expand-dabbrev-as-symbol)
			 (concat (regexp-quote pattern) "\\sw+"))
			((eq (char-syntax (aref pattern 0)) ?_)
			 (concat (regexp-quote pattern) "\\(\\sw\\|\\s_\\)+"))
			(t
			 (concat (regexp-quote pattern)
				 "\\(\\sw\\|\\s_\\)+")))))
      (while (and (not result)
		  (if reverse
		      (re-search-backward regpat limit t)
		    (re-search-forward regpat limit t)))
	(setq result (buffer-substring-no-properties
		      (save-excursion
			(goto-char (match-beginning 0))
			(skip-syntax-backward "w_")
			(point))
		      (match-end 0)))
	(if (he-string-member result he-tried-table t)
	    (setq result nil)))
      result))

  (defun try-my-dabbrev-substring (old)
    (let ((old-fun (symbol-function 'he-dabbrev-search)))
      (fset 'he-dabbrev-search (symbol-function 'my-dabbrev-substring-search))
      (unwind-protect
	  (try-expand-dabbrev old)
	(fset 'he-dabbrev-search old-fun))))

  (defun try-expand-flexible-abbrev (old)
    "Try to complete word using flexible matching. Flexible matching works by taking the search string and then interspersing it with a regexp for any character. So, if you try to do a flexible match for `foo' it will match the word `findOtherOtter' but also `fixTheBoringOrange' and `ifthisisboringstopreadingnow'. 

The argument OLD has to be nil the first call of this function, and t for subsequent calls (for further possible completions of the same string). It returns t if a new completion is found, nil otherwise."
    (if (not old)
	(progn
	  (he-init-string (he-lisp-symbol-beg) (point))
	  (if (not (he-string-member he-search-string he-tried-table))
	      (setq he-tried-table (cons he-search-string he-tried-table)))
	  (setq he-expand-list
		(and (not (equal he-search-string ""))
		     (he-flexible-abbrev-collect he-search-string)))))
    (while (and he-expand-list
		(he-string-member (car he-expand-list) he-tried-table))
      (setq he-expand-list (cdr he-expand-list)))
    (if (null he-expand-list)
	(progn
	  (if old (he-reset-string))
	  ())
      (progn
	(he-substitute-string (car he-expand-list))
	(setq he-expand-list (cdr he-expand-list))
	t)))

  (defun he-flexible-abbrev-collect (str)
    "Find and collect all words that flex-matches STR. See docstring for `try-expand-flexible-abbrev' for information about what flexible matching means in this context."
    (let ((collection nil)
	  (regexp (he-flexible-abbrev-regexp str)))
      (save-excursion
	(goto-char (point-min))
	(while (search-forward-regexp regexp nil t)
	  (setq collection (cons (thing-at-point 'word) collection))))
      collection))

  (defun he-flexible-abbrev-create-regexp (str)                                    
    "Generate regexp for flexible matching of STR. See docstring for `try-expand-flexible-abbrev' for information about what flexible matching means in this context."
    (concat "\\b" (mapconcat (lambda (x) (concat "\\w*" (list x))) str "")
	    "\\w*" "\\b"))

  (defun my-try-expand-dabbrev-visible (old)
    (save-excursion (try-expand-dabbrev-visible old)))

  :config
  (setq hippie-expand-try-functions-list
	'(my-yas-hippie-try-expand
	  my-try-expand-company
	  try-my-dabbrev-substring
	  my-try-expand-dabbrev-visible
	  try-expand-dabbrev
	  try-expand-dabbrev-all-buffers
	  try-expand-dabbrev-from-kill
	  try-expand-tag
	  try-expand-flexible-abbrev
	  try-complete-file-name-partially
	  try-complete-file-name
	  try-expand-all-abbrevs
	  try-expand-list
	  try-expand-list
	  try-expand-line
	  try-expand-line-all-buffers
	  try-complete-lisp-symbol-partially
	  try-complete-lisp-symbol))

  (bind-key "M-i" #'my-ido-hippie-expand)

  (defadvice he-substitute-string (after he-paredit-fix)
    "Remove extra paren when expanding line in paredit."
    (if (and paredit-mode (equal (substring str -1) ")"))
	(progn (backward-delete-char 1) (forward-char)))))


(use-package hl-line
  :commands hl-line-mode
  :bind ("M-o h" . hl-line-mode)

  :config
  (use-package hl-line+
    :load-path "elpa/hl-line+-20170621.734/"))


(use-package ibuffer
  :bind ("C-x C-b" . ibuffer)

  :init
  (add-hook 'ibuffer-mode-hook
	    #'(lambda ()
		(ibuffer-switch-to-saved-filter-groups "default"))))


(use-package ido
  :demand t
  :defines (ido-cur-item
	    ido-require-match
	    ido-selected
	    ido-final-text
	    ido-show-confirm-message)
  :bind (("C-x b" . ido-switch-buffer)
	 ("C-x B" . ido-switch-buffer-other-window))

  :preface
  (eval-when-compile
    (defvar ido-require-match)
    (defvar ido-cur-item)
    (defvar ido-show-confirm-message)
    (defvar ido-selected)
    (defvar ido-final-text))

  (defun ido-smart-select-text ()
    "Select the current completed item. Do NOT descend into directories."
    (interactive)
    (when (and (or (not ido-require-match)
		   (if (memq ido-require-match
			     '(confirm confirm-after-completion))
		       (if (or (eq ido-cur-item 'dir)
			       (eq last-command this-command))
			   t
			 (setq ido-show-confirm-message t)
			 nil))
		   (ido-existing-item-p))
	       (not ido-incomplete-regexp))
      (when ido-current-directory
	(setq ido-exit 'takeprompt)
	(unless (and ido-text (= 0 (length ido-text)))
	  (let ((match (ido-name (car ido-matches))))
	    (throw 'ido
		   (setq ido-selected
			 (if match
			     (replace-regexp-in-string "/\\'" "" match)
			   ido-text)
			 ido-text ido-selected
			 ido-final-text ido-text)))))
      (exit-minibuffer)))

  :config
  (use-package ido-hacks
    :demand t
    :load-path "elpa/ido-hacks-20150331.1209/"
    :bind ("M-x" . my-ido-hacks-execute-extended-command)

    :config
    (ido-hacks-mode 1)

    (defvar ido-hacks-completing-read (symbol-function 'completing-read))
    (fset 'completing-read ido-hacks-orgin-completing-read-function)
    (defun my-ido-hacks-execute-extended-command (&optional arg)
      (interactive "P")
      (cl-flet ((completing-read
		 (prompt collection &optional predicate require-match
			 initial-input hist def inherit-input method)
		 (funcall ido-hacks-completing-read
			  prompt collection predicate require-match
			  initial-input hist def inherit-input-method)))
	(ido-hacks-execute-extended-command arg))))

  (use-package flx-ido
    :disabled t
    :load-path "elpa/flx-20151030.1112/" "elpa/flx-ido-20151030.1112/"

    :config
    (flx-ido-mode 1))

  (add-hook 'ido-minibuffer-setup-hook
	    #'(lambda ()
		(bind-key "<return>" #'ido-smart-select-text
			  ido-file-completion-map))))


(use-package ielm
  :bind ("C-c :" . ielm)

  :config
  (defun my-ielm-return ()
    (interactive)
    (let ((end-of-sexp (save-excursion
			 (goto-char (point-max))
			 (skip-chars-backward " \t\n\r")
			 (point))))
      (if (>= (point) end-of-sexp)
	  (progn
	    (goto-char (point-max))
	    (skip-chars-backward " \t\n\r")
	    (delete-region (point) (point-max))
	    (call-interactively #'ielm-return))
	(call-interactively #'paredit-newline))))

  (add-hook 'ielm-mode-hook
	    (function
	     (lambda ()
	       (bind-key "<return>" #'my-ielm-return ielm-map)))
	    t))


(use-package image-file
  :config
  (auto-image-file-mode 1))


(use-package info
  :bind ("C-h C-i" . info-lookup-symbol)

  :init
  (remove-hook 'menu-bar-update-hook 'mac-setup-help-topics)

  :config
  (defadvice Info-exit (after remove-info-window activate)
    "When info mode is quit, remove the window."
    (if (> (length (window-list)) 1)
	(delete-window))))


(use-package info-look
  :commands info-lookup-add-help)


(use-package isearch
  :no-require t
  :bind (("C-M-r" . isearch-backward-other-window)
	 ("C-M-s" . isearch-forward-other-window))

  :preface
  (defun isearch-backward-other-window ()
    (interactive)
    (split-window-vertically)
    (call-interactively 'isearch-backward))

  (defun isearch-forward-other-window ()
    (interactive)
    (split-window-vertically)
    (call-interactively 'isearch-forward))

  :config
  (bind-key "C-c" #'isearch-toggle-case-fold isearch-mode-map)
  (bind-key "C-t" #'isearch-toggle-regexp isearch-mode-map)
  (bind-key "C-^" #'isearch-edit-string isearch-mode-map)
  (bind-key "C-i" #'isearch-complete isearch-mode-map))


(use-package lisp-mode
  :defer t

  :preface
  (defface esk-paren-face
    '((((class color) (background dark))
       (:foreground "grey50"))
      (((class color) (background light))
       (:foreground "grey55")))
    "Face used to dim parentheses."
    :group 'starter-kit-faces)

  (defvar slime-mode nil)
  (defvar lisp-mode-initialized nil)

  (defun my-lisp-mode-hook ()
    (unless lisp-mode-initialized
      (setq lisp-mode-initialized t)

      (use-package redshank
	:load-path "elpa/redshank-20120510.1230/"
	:diminish redshank-mode)

      (use-package elisp-slime-nav
	:load-path "elpa/elisp-slime-nav-20160128.1109/"
	:diminish elisp-slime-nav-mode)

      (use-package edebug)

      (use-package eldoc
	:diminish eldoc-mode
	:commands eldoc-mode
	:config
	(use-package eldoc-extension
	  :load-path "elpa/eldoc-extension-20140306.645/"
	  :disabled t
	  :defer t
	  :init
	  (add-hook 'emacs-lisp-mode-hook
		    #'(lambda () (require 'eldoc-extension)) t))

	(eldoc-add-command 'paredit-backward-delete
			   'paredit-close-round))

      (use-package ert
	:bind ("C-c e t" . ert-run-tests-interactively))

      (use-package elint
	:commands 'elint-initialize
	:preface
	(defun elint-current-buffer ()
	  (interactive)
	  (elint-initialize)
	  (elint-current-buffer))
	:config
	(add-to-list 'elint-standard-variables 'current-prefix-arg)
	(add-to-list 'elint-standard-variables 'command-line-args-left)
	(add-to-list 'elint-standard-variables 'buffer-file-coding-system)
	(add-to-list 'elint-standard-variables 'emacs-major-version)
	(add-to-list 'elint-standard-variables 'window-system))

      (use-package highlight-cl
	:load-path "elpa/highlight-cl-20091012.1030/"
	:init
	(mapc (function
	       (lambda (mode-hook)
		 (add-hook mode-hook
			   'highlight-cl-add-font-lock-keywords)))
	      lisp-mode-hooks))

      (defun my-elisp-indent-or-complete (&optional arg)
	(interactive "p")
	(call-interactively 'lisp-indent-line)
	(unless (or (looking-back "^\\s-*")
		    (bolp)
		    (not (looking-back "[-A-Za-z0-9_*+/=<>!?]+")))
	  (call-interactively 'lisp-complete-symbol)))

      (defun my-lisp-indent-or-complete (&optional arg)
	(interactive "p")
	(if (or (looking-back "^\\s-*") (bolp))
	    (call-interactively 'lisp-indent-line)
	  (call-interactively 'slime-indent-and-complete-symbol)))

      (defun my-byte-recompile-file ()
	(save-excursion
	  (byte-recompile-file buffer-file-name)))                                      
      (use-package testcover
	:commands testcover-this-defun)

      (mapc (lambda (mode)
	      (info-lookup-add-help
	       :mode mode
	       :regexp "[^][()'\" \t\n]+"
	       :ignore-case t
	       :doc-spec '(("(ansicl)Symbol Index" nil nil nil))))
	    lisp-modes))

    (auto-fill-mode 1)
    (paredit-mode 1)
    (redshank-mode 1)
    (elisp-slime-nav-mode 1)

    (local-set-key (kbd "<return>") 'paredit-newline)

    (add-hook 'after-save-hook 'check-parens nil t)

    (unless (memq major-mode
                  '(emacs-lisp-mode inferior-emacs-lisp-mode ielm-mode))                                      
      (bind-key "M-q" #'slime-reindent-defun lisp-mode-map)
      (bind-key "M-l" #'slime-selector lisp-mode-map)))

  :init
  (mapc (lambda (major-mode)
	  (font-lock-add-keywords
	   major-mode
	   '(("(\\(lambda\\)\\>"
	      (0 (ignore
                  (compose-region (match-beginning 1)
				  (match-end 1) ?λ))))
	     ("(\\|)" . 'esk-paren-face)
	     ("(\\(ert-deftest\\)\\>[         '(]*\\(setf[    ]+\\sw+\\|\\sw+\\)?"
	      (1 font-lock-keyword-face)
	      (2 font-lock-function-name-face nil t)))))
	lisp-modes)

  (apply #'hook-into-modes 'my-lisp-mode-hook lisp-mode-hooks))


(use-package midnight
  :defer 10)


(use-package mule
  :no-require t
  :defines x-select-request-type

  :config
  (prefer-coding-system 'utf-8)
  (set-terminal-coding-system 'utf-8)
  (set-keyboard-coding-system 'utf-8)
  (setq x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING)))


(use-package nroff-mode
  :commands nroff-mode

  :config
  (defun update-nroff-timestamp ()
    (save-excursion
      (goto-char (point-min))
      (when (re-search-forward "^\\.Dd ")
	(let ((stamp (format-time-string "%B %e, %Y")))
	  (unless (looking-at stamp)
	    (delete-region (point) (line-end-position))
	    (insert stamp)
	    (let (after-save-hook)
	      (save-buffer)))))))

  (add-hook 'nroff-mode-hook
	    #'(lambda ()
		(add-hook 'after-save-hook 'update-nroff-timestamp nil t))))


(use-package nxml-mode
  :commands nxml-mode

  :init
  (defalias 'xml-mode 'nxml-mode)

  :config
  (defun my-nxml-mode-hook ()
    (bind-key "<return>" #'newline-and-indent nxml-mode-map))

  (add-hook 'nxml-mode-hook 'my-nxml-mode-hook)

  (defun tidy-xml-buffer ()
    (interactive)
    (save-excursion
      (call-process-region (point-min) (point-max) "tidy" t t nil
			   "-xml" "-i" "-wrap" "0" "-omit" "-q" "-utf8")))

  (bind-key "C-c M-h" #'tidy-xml-buffer nxml-mode-map)

  (require 'hideshow)
  (require 'sgml-mode)

  (add-to-list 'hs-special-modes-alist
	       '(nxml-mode
		 "<!--\\|<[^/>]*[^/]>"
		 "-->\\|</[^/>]*[^/]>"
		 "<!--"
		 sgml-skip-tag-forward
		 nil))

  (add-hook 'nxml-mode-hook 'hs-minor-mode)

  (bind-key "C-c h" #'hs-toggle-hiding nxml-mode-map))


(use-package outline
  :diminish outline-minor-mode
  :commands outline-minor-mode

  :init
  (hook-into-modes #'outline-minor-mode
		   'emacs-lisp-mode-hook
		   'latex-mode-hook
		   'LaTeX-mode-hook
		   'markdown-mode-hook))


(use-package paredit
  :load-path "elpa/paredit-20170405.1149/"
  :commands paredit-mode
  :diminish paredit-mode

  :config
  (bind-key "C-M-l" #'paredit-recentre-on-sexp paredit-mode-map)

  (bind-key ")" #'paredit-close-round-and-newline paredit-mode-map)
  (bind-key "M-)" #'paredit-close-round paredit-mode-map)

  (bind-key "M-k" #'paredit-raise-sexp paredit-mode-map)
  (bind-key "M-I" #'paredit-splice-sexp paredit-mode-map)

  (unbind-key "M-r" paredit-mode-map)
  (unbind-key "M-s" paredit-mode-map)

  (bind-key "C-. D" #'paredit-forward-down paredit-mode-map)
  (bind-key "C-. B" #'paredit-splice-sexp-killing-backward paredit-mode-map)
  (bind-key "C-. C" #'paredit-convolute-sexp paredit-mode-map)
  (bind-key "C-. F" #'paredit-splice-sexp-killing-forward paredit-mode-map)
  (bind-key "C-. a" #'paredit-add-to-next-list paredit-mode-map)
  (bind-key "C-. A" #'paredit-add-to-previous-list paredit-mode-map)
  (bind-key "C-. j" #'paredit-join-with-next-list paredit-mode-map)
  (bind-key "C-. J" #'paredit-join-with-previous-list paredit-mode-map))


(use-package ps-print
  :defer t

  :config
  (defun ps-spool-to-pdf (beg end &rest ignore)
    (interactive "r")
    (let ((temp-file (concat (make-temp-name "ps2pdf") ".pdf")))
      (call-process-region beg end (executable-find "ps2pdf")
			   nil nil nil "-" temp-file)
      (call-process (executable-find "open") nil nil nil temp-file)))

  (setq ps-print-region-function 'ps-spool-to-pdf))


(use-package recentf
  :defer 10
  :commands (recentf-mode
	     recentf-add-file
	     recentf-apply-filename-handlers)

  :preface
  (defun recentf-add-dired-directory ()
    (if (and dired-directory
	     (file-directory-p dired-directory)
	     (not (string= "/" dired-directory)))
	(let ((last-idx (1- (length dired-directory))))
	  (recentf-add-file
	   (if (= ?/ (aref dired-directory last-idx))
	       (substring dired-directory 0 last-idx)
	     dired-directory)))))

  :init
  (add-hook 'dired-mode-hook 'recentf-add-dired-directory)

  :config
  (recentf-mode 1))


(use-package ruby-mode
  :mode ("\\.rb\\'" . ruby-mode)
  :interpreter ("ruby" . ruby-mode)
  :functions inf-ruby-keys

  :config
  (use-package yari
    :load-path "elpa/yari-20151127.2339/"
    :init
    (progn
      (defvar yari-helm-source-ri-pages
	'((name . "RI documentation")
	  (candidates . (lambda () (yari-ruby-obarray)))
	  (action ("Show with Yari" . yari))
	  (candidate-number-limit . 300)
	  (requires-pattern . 2)
	  "Source for completing RI documentation."))

      (defun helm-yari (&optional rehash)
	(interactive (list current-prefix-arg))
	(when current-prefix-arg (yari-ruby-obarray rehash))
	(helm 'yari-helm-source-ri-pages (yari-symbol-at-point)))))

  (defun my-ruby-smart-return ()
    (interactive)
    (when (memq (char-after) '(?\| ?\" ?\'))
      (forward-char))
    (call-interactively 'newline-and-indent))

  (defun my-ruby-mode-hook ()
    (require 'inf-ruby)
    (inf-ruby-keys)
    (bind-key "<return>" #'my-ruby-smart-return ruby-mode-map)
    (bind-key "C-h C-i" #'helm-yari ruby-mode-map))

  (add-hook 'ruby-mode-hook 'my-ruby-mode-hook))


(use-package sh-script
  :defer t

  :init
  (defvar sh-script-initialized nil)
  (defun initialize-sh-script ()
    (unless sh-script-initialized
      (setq sh-script-initialized t)
      (info-lookup-add-help :mode 'shell-script-mode
			    :regexp ".*"
			    :doc-spec
			    '(("(bash)Index")))))

  (add-hook 'shell-mode-hook 'initialize-sh-script))


(use-package smerge-mode
  :commands smerge-mode

  :config
  (setq smerge-command-prefix (kbd "C-. C-.")))


(use-package texinfo
  :defines texinfo-section-list
  :mode ("\\.texi\\'" . texinfo-mode)

  :config
  (defun my-texinfo-mode-hook ()
    (dolist (mapping 1((?b . "emph")
		       (?c . "code")
		       (?s . "samp")
		       (?d . "dfn")
		       (?o . "option")
		       (?x . "pxref")))
      (local-set-key (vector (list 'alt (car mapping)))
		     `(lambda () (interactive)
			(TeX-insert-macro ,(cdr mapping))))))

  (add-hook 'texinfo-mode-hook 'my-texinfo-mode-hook)

  (defun texinfo-outline-level ()
    (require 'texinfo)
    (save-excursion
      (if (bopd)
	  0
	(forward-char 1)
	(let* ((word (buffer-substring-no-properties
		      (point) (progn (forward-word 1) (point))))
	       (entry (assoc word texinfo-section-list)))
	  (if entry
	      (nth 1 entry)
	    5))))))


(use-package tramp-sh
  :defer t)


(use-package whitespace
  :diminish (global-whitespace-mode
	     whitespace-mode
	     whitespace-newline-mode)
  :commands (whitespace-buffer
	     whitespace-cleanup
	     whitespace-mode)
  :defines (whitespace-auto-cleanup
	    whitespace-rescan-timer-time
	    whitespace-silent)

  :preface
  (defun normalize-file ()
    (interactive)
    (save-excursion
      (goto-char (point-min))
      (whitespace-cleanup)
      (delete-trailing-whitespace)
      (goto-char (point-max))
      (delete-blank-lines)
      (set-buffer-file-coding-system 'unix)
      (goto-char (point-min))
      (while (re-search-forward "\r$" nil t)
	(replace-match ""))
      (set-buffer-file-coding-system 'utf-8)
      (let ((require-final-newline t))
	(save-buffer))))

  (defun maybe-turn-on-whitespace ()
    "Depending on the file, maybe clean up whitespace."
    (let ((file (expand-file-name ".clean"))
	  parent-dir)
      (while (and (not (file-exists-p file))
		  (progn
		    (setq parent-dir
			  (file-name-directory
			   (directory-file-name
			    (file-name-directory file))))

		    ;; Gi opp hvis vi allerede er i den siste mappen:
		    (not (string= (file-name-directory file)
				  parent-dir))))

	;; Gå tilbake til overliggende mappe og prøv igjen:
	(setq file (expand-file-name ".clean" parent-dir)))

      ;; Bruk loggede endringer hvis de finnes:
      (when (and (file-exists-p file)
		 (not (file-exists-p ".noclean"))
		 (not (and buffer-file-name
			   (string-match "\\.texi\\'" buffer-file-name))))
	(add-hook 'write-contents-hooks
		  #'(lambda () (ignore (whitespace-cleanup))) nil t)
	(whitespace-cleanup))))

  :init
  (add-hook 'find-file-hooks 'maybe-turn-on-whitespace t)

  :config
  (remove-hook 'find-file-hooks 'whitespace-buffer)
  (remove-hook 'kull-buffer-hook 'whitespace-buffer))


(use-package winner
  :if (not noninteractive)
  :defer 5
  :bind (("M-N" . winner-redo)
	 ("M-P" . winner-undo))

  :config
  (winner-mode 1))


;; Den fjerde listen er major modes:

(use-package csv-mode
  :load-path "elpa/csv-mode-1.6/"
  :mode ("\\.csv\\'" . csv-mode))


(use-package ess-site
  :load-path "elpa/ess-20170728.238/lisp/"
  :commands R
  :mode (("\\.R\\'" . R-mode)
         ("\\.Rnw\\'" . Rnw-mode)
         ("\\.Snw\\'" . Rnw-mode)
         ("\\.Rmd\\'" . Rnw-mode)))


(use-package gnuplot-mode
  :load-path "elpa/gnuplot-mode-20170727.1339/"
  :mode ("\\.gp\\'" . gnuplot-mode))


(use-package julia-mode
  :load-path "elpa/julia-mode-20170710.538/"
  :mode ("\\.jl\\'" . julia-mode))


(use-package ledger-mode
  :load-path "elpa/ledger-mode-20170714.1529/"
  :commands ledger-mode
  :mode (("\\.ledger\\'" . ledger-mode)
         ("\\.dat\\'" . ledger-mode))
  :bind ("C-c L" . my-ledger-start-entry))


(use-package lua-mode
  :load-path "elpa/lua-mode-20170130.435/"
  :mode "\\.lua\\'"
  :interpreter ("lua" . lua-mode))


(use-package magit
  :load-path "elpa/magit-20170803.828/" "elpa/with-editor-20170803.917/"
  :diminish auto-revert-mode
  :bind (("C-x g" . magit-status)
         ("C-x G" . magit-status-with-prefix)
         ("C-x M-g" . magit-dispatch-popup))

  :preface
  (defun magit-monitor (&optional no-display)
    "Start git-monitor in the current directory."
    (interactive)
    (when (string-match "\\*magit: \\(.+\\)" (buffer-name))
      (let ((name (format "*git-monitor: %s*"
                          (match-string 1 (buffer-name)))))
        (or (get-buffer name)
            (let ((buf (get-buffer-create name)))
              (ignore-errors
                (start-process "*git-monitor*" buf "git-monitor"
                               "-d" (expand-file-name default-directory)))
              buf)))))

  (defun magit-status-with-prefix ()
    (interactive)
    (let ((current-prefix-arg '(4)))
      (call-interactively 'magit-status)))

  (defun lusty-magit-status (dir &optional switch-function)
    (interactive (list (if current-prefix-arg
                           (lusty-read-directory)
                         (or (magit-get-top-dir)
                             (lusty-read-directory)))))
    (magit-status-internal dir switch-function))

  (defun eshell/git (&rest args)
    (cond
     ((or (null args)
          (and (string= (car args) "status") (null (cdr args))))
      (magit-status-internal default-directory))
     ((and (string= (car args) "log") (null (cdr args)))
      (magit-log "HEAD"))
     (t (throw 'eshell-replace-command
               (eshell-parse-command
                "*git"
                (eshell-stringify-list (eshell-flatten-list args)))))))

  :init
  (global-magit-file-mode)
  (add-hook 'magit-mode-hook 'hl-line-mode)

  :config
  (setenv "GIT_PAGER" "")

  (use-package git-commit
    :defer t
    :load-path "elpa/git-commit-20170609.2310/")

  (use-package magit-backup
    :disabled t
    :commands magit-backup-mode
    :config
    (magit-backup-mode -1))

  (use-package magit-commit
    :config
    (remove-hook 'server-switch-hook 'magit-commit-diff))

  (use-package magit-popup
    :defer t
    :load-path "elpa/magit-popup-20170730.2139/")

  (unbind-key "M-h" magit-mode-map)
  (unbind-key "M-s" magit-mode-map)
  (unbind-key "M-m" magit-mode-map)
  (unbind-key "M-w" magit-mode-map)
  (unbind-key "<C-return>" magit-file-section-map)

  (bind-key "U" #'magit-unstage-all magit-mode-map)

  (add-hook 'magit-log-edit-mode-hook
            #'(lambda ()
                (set-fill-column 72)
                (flyspell-mode)))

  (add-hook 'magit-status-mode-hook #'(lambda () (magit-monitor t))))


(use-package markdown-mode
  :load-path "elpa/markdown-mode-20170803.1101/"
  :mode (("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))

  :config
  (use-package markdown-preview-mode
    :load-path "elpa/markdown-preview-mode-20161211.1443/"
    :config
    (setq markdown-preview-stylesheets
          (list "http://ftp.newartisans.com/pub/github.css")))

  (add-hook 'markdown-mode-hook
            (lambda ()
              (pandoc-mode)
              (turn-off-auto-fill)
              (latex-unicode-simplified))))


(use-package pdf-tools
  :if window-system
  :load-path "elpa/pdf-tools-20170721.718/"

  :init
  (pdf-tools-install))


(use-package python-mode
  :load-path "elpa/python-0.25.2/"
  :mode ("\\.py\\'" . python-mode)
  :interpreter ("python" . python-mode)

  :init
  (use-package elpy
    :load-path "elpa/elpy-20170701.1412/"
    :diminish elpy-mode
    :init
    (elpy-enable))

  :config
  (use-package jedi
    :load-path "elpa/jedi-20160425.2156/"
    :init
    (add-to-list 'ac-sources 'ac-source-jedi-direct)

    (defvar jedi-config:with-virtualenv nil
      "Set to non-nil to point to a particular virtualenv.")

    (defvar jedi-config:vcs-root-sentinel ".git")
    (defvar jedi-config:python-module-sentinel "__init__.py")

    (defun get-project-root (buf repo-type init-file)
      (vc-find-root (expand-file-name (buffer-file-name buf)) repo-type))

    (defvar jedi-config:find-root-function 'get-project-root)

    (defun current-buffer-project-root ()
      (funcall jedi-config:find-root-function
               (current-buffer)
               jedi-config:vcs-root-sentinel
               jedi-config:python-module-sentinel))

    (defun jedi-config:setup-server-args ()
      (defmacro add-args (arg-list arg-name arg-value)
        '(setq ,arg-list (append ,arg-list (list ,arg-name ,arg-value))))
      (let ((project-root (current-buffer-project-root)))
        (make-local-variable 'jedi:server-args)
        (when project-root
          (add-args jedi:server-args "--sys-path" project-root))
        (when jedi-config:with-virtualenv
          (add-args jedi:server-args "--virtual-env"
                    jedi-config:with-virtualenv))))

    (defun jedi-config:setup-keys ()
      (local-set-key (kbd "M-.") 'jedi:goto-definition)
      (local-set-key (kbd "M-,") 'jedi:goto-definition-pop-marker)
      (local-set-key (kbd "M-?") 'jedi:show-doc)
      (local-set-key (kbd "M-/") 'jedi:get-in-function-call)))


  (use-package highlight-indentation
    :load-path "elpa/highlight-indentation-20170502.43/")

  (defvar python-mode-initialized nil)

  (defun my-python-mode-hook ()
    (unless python-mode-initialized
      (setq python-mode-initialized t)

      (info-lookup-add-help
       :mode 'python-mode
       :regexp "[a-zA-Z_0-9.]+"
       :doc-spec
       '(("(python)Python Module Index" )
         ("(python)Index"
          (lambda
            (item)
            (cond
             ((string-match
               "\\([A-Za-z0-9_]+\\)() (in module \\([A-Za-z0-9_.]+\\))" item)
              (format "%s.%s" (match-string 2 item)
                      (match-string 1 item)))))))))

    (setq indicate-empty-lines t)
    (set (make-local-variable 'parens-require-spaces) nil)
    (setq indent-tabs-mode nil)

    (bind-key "C-c C-z" #'python-shell python-mode-map)
    (unbind-key "C-c c" python-mode-map))

  (add-hook 'python-mode-hook
            (lambda ()
              (jedi:setup)
              (jedi-config:setup-server-args)
              (jedi-config:setup-keys)
              (elpy-mode)
              (highlight-indentation-mode)
              (my-python-mode-hook))))


(use-package yaml-mode
  :load-path "elpa/yaml-mode-20170406.241/"
  :mode ("\\.ya?ml\\'" . yaml-mode))


;; Den femte listen er minor-modes:

(use-package alert
  :load-path "elpa/alert-20170503.1714/"
  :commands alert)


(use-package ascii
  :load-path "elpa/ascii-20130824.500/"
  :commands acii-on
  :functions ascii-off

  :preface
  (defun ascii-toggle ()
    (interactive)
    (if ascii-display
        (ascii-off)
      (ascii-on))))


(use-package async
  :load-path "elpa/async-20170804.2158/")


(use-package auto-complete-config
  :load-path "elpa/auto-complete-20170124.1845/"
  :diminish auto-complete-mode

  :init
  (use-package pos-tip)
  (ac-config-default)
  (global-auto-complete-mode)
  (ac-flyspell-workaround)

  :config
  (ac-set-trigger-key "<backtab>")
  (bind-key "A-M-?" #'ac-last-help)
  (unbind-key "C-s" ac-completing-map)

  (use-package auto-complete-auctex
    :load-path "elpa/auto-complete-auctex-20140223.958/"))


(use-package auto-yasnippet
  :load-path "elpa/auto-yasnippet-20160925.225/"
  :bind (("C-. w" . aya-create)
         ("C-. y" . aya-expand)
         ("C-. o" . aya-open-line)))


(use-package avy
  :demand t
  :load-path "elpa/avy-20170804.1135/"
  :bind ("M-h" . avy-goto-char)

  :config
  (avy-setup-default))


(use-package awk-it
  :load-path "elpa/awk-it-20130917.1148/"
  :commands (awk-it
             awk-it-with-separator
             awk-it-single
             awk-it-single-with-separator
             awk-it-raw
             awk-it-file
             awk-it-with-file
             awk-it-to-kill-ring
             awk-it-to-file))


(use-package backup-each-save
  :load-path "elpa/backup-each-save-20130704.732/"
  :commands backup-each-save

  :preface
  (defun show-backups ()
    (interactive)
    (require 'find-dired)
    (let* ((file (make-backup-file-name (buffer-file-name)))
           (dir (file-name-directory file))
           (args (concat "-iname '" (file-name-nondirectory file)
                         ".~*~'"))
           (dired-buffers dired-buffers)
           (find-ls-option '("-print0 | xargs -0 ls -lta" . "-lta")))
      ;; Check that it's really a directory:
      (or (file-directory-p dir)
          (error "Backup directory does not exist: %s" dir))
      (with-current-buffer (get-buffer-create "*Backups*")
        (let ((find (get-buffer-process (current-buffer))))
          (when find
            (if (or (not (eq (process-status find) 'run))
                    (yes-or-no-p "A `find' process is running; kill it? "))
                (condition-case nil
                    (progn
                      (interrupt-process find)
                      (sit-for 1)
                      (delete-process find))
                  (error nil))
              (error "Cannot have two processes in `%s' at once"
                     (buffer-name)))))

        (widen)
        (kill-all-local-variables)
        (setq buffer-read-only nil)
        (erase-buffer)
        (setq default-directory dir
              args (concat
                    find-program " . "
                    (if (string= args "")
                        ""
                      (concat
                       (shell-quote-argument "(")
                       " " args " "
                       (shell-quote-argument ")")
                       " "))
                    (if (string-match "\\`\\(.*\\) {} \\(\\\\;\\|+\\)\\'"
                                      (car find-ls-option))
                        (format "%s %s %s"
                                (match-string 1 (car find-ls-option))
                                (shell-quote-argument "{}")
                                find-exec-terminator)
                      (car find-ls-option))))
        ;; Start the find process:
        (message "Looking for backup files...")
        (shell-command (concat args "&") (current-buffer))
        ;; The next statement will bomb in classic dired (no optional arg
        ;; allowed):
        (dired-mode dir (cdr find-ls-option))
        (let ((map (make-sparse-keymap)))
          (set-keymap-parent map (current-local-map))
          (define-key map "\C-c\C-k" 'kill-find)
          (use-local-map map))
        (make-local-variable 'dired-sort-inhibit)
        (setq dired-sort-inhibit t)
        (set (make-local-variable 'revert-buffer-function)
             `(lambda (ignore-auto noconfirm)
                (find-dired ,dir ,find-args)))
        ;; Set subdir-alist so that Tree Dired will work:
        (if (fboundp 'dired-simple-subdir-alist)
            ;; Will work even with nested dired format (dired-nstd.el,v 1.15
            ;; and later):
            (dired-simple-subdir-alist)
          ;; Else we have an ancient tree dired (or classic dired, where this
          ;; does no harm):
          (set (make-local-variable 'dired-subdir-alist)
               (list (cons default-directory (point-min-marker)))))
        (set (make-local-variable 'dired-subdir-switches)
             find-ls-subdir-switches)
        (setq buffer-read-only nil)
        ;; Subdir headlerline must come first because the first marker in
        ;; subdir-alist points there:
        (insert "  " dir ":\n")
        ;; Make second line a ``find'' line in analogy to the ``total'' or
        ;; ``wildcard'' line:
        (insert "  " args "\n")
        (setq buffer-read-only t)
        (let ((proc (get-buffer-process (current-buffer))))
          (set-process-filter proc (function find-dired-filter))
          (set-process-sentinel proc (function find-dired-sentinel))
          ;; Initialize the process marker; it is used by the filter:
          (move-marker (process-mark proc) 1 (current-buffer)))
        (setq mode-line-process '(":%s")))))

  (bind-key "C-x ~" #'show-backups)

  :init
  (defun my-make-backup-file-name (file)
    (make-backup-file-name-1 (file-truename file)))

  (add-hook 'after-save-hook 'backup-each-save)

  :config
  (defun backup-each-save-filter (filename)
    (not (string-match
          (concat "\\(^/tmp\\|\\.emacs\\.d/data\\(-alt\\)?/"
                  "\\|\\.newsrc\\(\\.eld\\)?\\|"
                  "\\(archive/sent/\\|recentf\\`\\)\\)")
          filename)))

  (setq backup-each-save-filter-function 'backup-each-save-filter)

  (defun my-dont-backup-files-p (filename)
    (unless (string-match filename "\\(archive/sent/\\|recentf\\`\\)")
      (normal-backup-enable-predicate filename)))

  (setq backup-enable-predicate 'my-dont-backup-files-p))


(use-package bbdb-com
  :load-path "elpa/bbdb-20170725.300/"
  :commands bbdb-create
  :bind ("M-B" . bbdb)

  :init
  (bbdb-initialize 'gnus
                   'mail
                   'message)

  (add-hook 'gnus-startup-hook 'bbdb-insinuate-gnus)
  (add-hook 'mail-setup-hook 'bbdb-insinuate-sendmail)

  (bbdb-insinuate-message))


(use-package browse-kill-ring
  :load-path "elpa/browse-kill-ring-20160125.9/"
  :defer 10

  :init
  (use-package browse-kill-ring+
    :load-path "elpa/browse-kill-ring+-20170221.757/"
    :commands browse-kill-ring))


(use-package cdlatex
  :load-path "elpa/cdlatex-20140707.426/" "elpa/auctex-11.91.0/")


(use-package company
  :load-path "elpa/company-20170715.1035/"
  :diminish company-mode
  :commands company-mode

  :config
  (defadvice company-pseudo-tooltip-unless-just-one-frontend
      (around only-show-tooltip-when-invoked activate)
    (when (company-explicit-action-p)
      ad-do-it)))


(use-package find-file-in-project
  :disabled t
  :load-path "elpa/find-file-in-project-20170725.133/")
;; :bind ("C-c f" . find-file-in-project))


(use-package flycheck
  :load-path "elpa/flycheck-20170802.22/"
  :diminish flycheck-mode
  :defer 5

  :init
  (add-hook 'after-init-hook #'global-flycheck-mode)

  :config
  (defalias 'flycheck-show-error-at-point-soon 'flycheck-show-error-at-point)
  (add-hook 'ess-mode-hook 'flycheck-mode t))


(use-package helm-config
  :demand t
  :load-path "elpa/helm-20170808.246/"
  :bind (("C-c h" . helm-command-prefix)
         ("C-h a" . helm-apropos)
         ("C-h m" . my-helm-apropos)
         ("C-x f" . helm-multi-files)
         ("M-s b" . helm-occur)
         ("M-s n" . my-helm-find)
         ("M-H" . helm-resume))

  :preface
  (defun my-helm-find ()
    (interactive)
    (helm-find nil))

  :config
  (use-package helm-files)
  (use-package helm-buffers)

  (use-package helm-mode
    :diminish helm-mode
    :init
    (helm-mode 1))

  (use-package helm-multi-match)

  (helm-autoresize-mode 1)

  (bind-key "<tab>" #'helm-execute-persistent-action helm-map)
  (bind-key "C-i" #'helm-execute-persistent-action helm-map)
  (bind-key "C-z" #'helm-select-action helm-map)
  (bind-key "A-v" #'helm-previous-page helm-map)

  (when (executable-find "curl")
    (setq helm-google-suggest-use-curl-p t)))


(use-package helm-descbinds
  :load-path "elpa/helm-descbinds-20160916.713/"
  :bind ("C-h b" . helm-descbinds)

  :init
  (fset 'describe-bindings 'helm-descbinds)

  :config
  (require 'helm-config))


(use-package helm-grep
  :commands helm-do-grep-1
  :bind (("M-s f" . my-helm-do-grep-r)
         ("M-s g" . my-helm-do-grep))

  :preface
  (defun my-helm-do-grep ()
    (interactive)
    (helm-do-grep-1 (list default-directory)))

  (defun my-helm-do-grep-r ()
    (interactive)
    (helm-do-grep-1 (list default-directory) t)))


(use-package helm-make
  :load-path "elpa/helm-make-20170430.1053/"
  :commands (helm-make helm-make-projectile))


(use-package helm-swoop
  :load-path "elpa/helm-swoop-20160619.953/"
  :bind (("M-s o" . helm-swoop)
         ("M-s /" . helm-multi-swoop)))


(use-package hydra
  :load-path "elpa/hydra-20170803.1319/"
  :defer 10

  :config
  (defhydra hydra-zoom (global-map "<f2>")
    "zoom"
    ("g" text-scale-increase "in")
    ("l" text-scale-decrease "out")))


(use-package isearch-dabbrev
  :load-path "elpa/isearch-dabbrev-20141223.2222/"

  :init
  (define-key isearch-mode-map (kbd "<tab>") 'isearch-dabbrev-expand))


(use-package ivy
  :disabled t
  :load-path "elpa/ivy-20170806.1231/")


(use-package lusty-explorer
  :demand t
  :load-path "elpa/lusty-explorer-20150508.1557/"
  :bind ("C-x C-f" . my-lusty-file-explorer)

  :preface
  (defun lusty-read-directory ()
    "Launch the file/directory mode of LustyExplorer."
    (interactive)
    (require 'lusty-explorer)
    (let ((lusty--active-mode :file-explorer))
      (lusty--define-mode-map)
      (let* ((lusty--ignored-extensions-regex
              (concat "\\(?:" (regexp-opt completion-ignored-extensions)
                      "\\)$"))
             (minibuffer-local-filename-completion-map lusty-mode-map)
             (lusty-only-directories t))
        (lusty--run 'read-directory-name default-directory ""))))

  (defun lusty-read-file-name ()
    "Launch the file/directory mode of LustyExplorer."
    (interactive)
    (require 'lusty-explorer)
    (let ((lusty--active-mode :file-explorer))
      (lusty--define-mode-map)
      (let* ((lusty--ignored-extensions-regex
              (concat "\\(?:" (regexp-opt completion-ignored-extensions)
                      "\\)$"))
             (minibuffer-local-filename-completion-map lusty-mode-map)
             (lusty-only-directories nil))
        (lusty--run 'read-file-name default-directory ""))))

  (defun my-lusty-file-explorer ()
    "Launch the file/directory mode of LustyExplorer."
    (interactive)
    (require 'lusty-explorer)
    (let ((lusty--active-mode :file-explorer)
          (helm-mode-prev (and (boundp 'helm-mode) helm-mode)))
      (if (fboundp 'helm-mode)
          (helm-mode -1))
      (unwind-protect
          (progn
            (lusty--define-mode-map)
            (let* ((lusty--ignored-extensions-regex
                    (concat "\\(?:" (regexp-opt
                                     completion-ignored-extensions) "\\)$"))
                   (minibuffer-local-filename-completion-map lusty-mode-map)
                   (file
                    ;; read-file-name is silly in that if the result is equal
                    ;; to the dir argument, it gets converted to the
                    ;; default-filename argument. Set it explicitly to "" so
                    ;; if lusty-launch-dired is called in the directory we
                    ;; start at, the result is that directory instead of the
                    ;; name of the current buffer.
                    (lusty--run 'read-file-name default-directory "")))
              (when file
                (switch-to-buffer
                 (find-file-noselect
                  (expand-file-name file))))))
        (if (fboundp 'helm-mode)
            (helm-mode (if helm-mode-prev 1 -1))))))

  :config
  (defun my-lusty-setup-hook ()
    (bind-key "SPC" #'lusty-select-match lusty-mode-map)
    (bind-key "C-d" #'exit-minibuffer lusty-mode-map))

  (add-hook 'lusty-setup-hook 'my-lusty-setup-hook)

  (defun lusty-open-this ()
    "Open the given file/directory/buffer, creating it if not already present."
    (interactive)
    (when lusty--active-mode
      (ecase lusty--active-mode
        (:file-explorer
         (let* ((path (minibuffer-contents-no-properties))
                (last-char (aref path (1- (length path)))))
           (lusty-select-match)
           (lusty-select-current-name)))
        (:buffer-explorer (lusty-select-match)))))

  (defvar lusty-only-directories nil)

  (defun lusty-file-explorer-matches (path)
    (let* ((dir (lusty-normalize-dir (file-name-directory path)))
           (file-portion (file-name-nondirectory path))
           (files
            (and dir
                 ;; NOTE: directory-files is quicker but doesn't append slash
                 ;; for directories.
                 ;; (directory-files dir nil nil t)
                 (file-name-all-completions "" dir)))
           (filtered (lusty-filter-files
                      file-portion
                      (if lusty-only-directories
                          (loop for f in files
                                when (= ?/ (aref f (1- (length f))))
                                collect f)
                        files))))
      (if (or (string= file-portion "")
              (string= file-portion "."))
          (sort filtered 'string<)
        (lusty-sort-by-fuzzy-score filtered file-portion)))))


(use-package macrostep
  :load-path "elpa/macrostep-20161120.1306/"
  :diminish macrostep-mode
  :bind ("C-c e m" . macrostep-expand))


(use-package pabbrev
  :load-path "elpa/pabbrev-20160320.1401/"
  :commands pabbrev-mode
  :diminish pabbrev-mode)


(use-package pandoc-mode
  :load-path "elpa/pandoc-mode-20170720.127/"
  :diminish pandoc-mode

  :config
  (add-hook 'pandoc-mode-hook
            (lambda ()
              (turn-on-reftex)
              (pandoc-load-default-settings))))


(use-package polymode
  :load-path "elpa/polymode-20170307.322/"
  :mode (("\\.md\\'" . poly-markdown-mode)
         ("\\.Snw\\'" . poly-noweb+r-mode)
         ("\\.Rnw\\'" . poly-noweb+r-mode)
         ("\\.Rmd\\'" . poly-markdown+r-mode))
  :diminish poly-markdown-mode
  :diminish poly-noweb+r-mode
  :diminish poly-markdown+r-mode

  :init
  (use-package poly-R)
  (use-package poly-markdown))


(use-package projectile
  :load-path "elpa/projectile-20170727.2351/"
  :diminish projectile-mode
  :commands projectile-mode
  :defer 5
  :bind-keymap ("C-c p" . projectile-command-map)

  :config
  (use-package helm-projectile
    :load-path "elpa/helm-projectile-20170613.14/"
    :config
    (setq projectile-completion-system 'helm)
    (helm-projectile-on))

  (projectile-mode)

  (bind-key "s s"
            #'(lambda ()
                (interactive)
                (helm-do-grep-1 (list (projectile-project-root)) t))
            'projectile-command-map))


(use-package rainbow-mode
  :load-path "elpa/rainbow-mode-0.13/"
  :commands rainbow-mode)


(use-package selected
  :load-path "elpa/selected-20170222.34/"
  :defer 5
  :diminish selected-minor-mode

  :config
  (selected-global-mode 1)

  (bind-key "[" #'align-entire selected-keymap)
  (bind-key "f" #'fill-region selected-keymap)
  (bind-key "U" #'unfill-region selected-keymap)
  (bind-key "d" #'downcase-region selected-keymap)
  (bind-key "r" #'reverse-region selected-keymap)
  (bind-key "s" #'sort-lines selected-keymap)
  (bind-key "u" #'upcase-region selected-keymap))


(use-package session
  :if (not noninteractive)
  :load-path "elpa/session-20120510.1700/"

  :preface
  (defun remove-session-use-package-from-settings ()
    (when (string= (file-name-nondirectory (buffer-file-name)) "custom.el")
      (save-excursion
        (goto-char (point-min))
        (when (re-search-forward "^ '(session-use-package " nil t)
          (delete-region (line-beginning-position)
                         (1+ (line-end-position)))))))

  (defun le::maybe-reveal ()
    (when (and (or (memq major-mode '(org-mode outline-mode))
                   (and (boundp 'outline-minor-mode)
                        outline-minor-mode))
               (outline-invisible-p))
      (if (eq major-mode 'org-mode)
          (org-reveal)
        (show-subtree))))

  (defvar server-process nil)

  (defun save-information ()
    (with-temp-message "Saving Emacs information..."
      (recentf-cleanup)

      (loop for func in kill-emacs-hook
            unless (memq func '(exit-gnus-on-exit server-force-stop))
            do (funcall func))

      (unless (or noninteractive
                  running-alternate-emacs
                  running-development-emacs
                  (and server-process
                       (eq 'listen (process-status server-process))))
        (server-start))))

  :config
  (add-hook 'before-save-hook 'remove-session-use-package-from-settings)
  (add-hook 'session-after-jump-to-last-change-hook 'le::maybe-reveal)
  (run-with-idle-timer 60 t 'save-information)
  (add-hook 'after-init-hook 'session-initialize t))


(use-package smart-mode-line
  :load-path "elpa/smart-mode-line-20170708.1317/"

  :preface
  (use-package smart-mode-line-powerline-theme
    :load-path "elpa/smart-mode-line-powerline-theme-20160705.1738/")

  :init
  (sml/setup))


(use-package smex
  :load-path "elpa/smex-20151212.1409/"
  :bind (("M-x" . smex)
         ("M-x" . smex-major-mode-commands)
         ("C-c C-c M-x" . execute-extended-command))

  :init
  (smex-initialize))


(use-package yasnippet
  :load-path "elpa/yasnippet-20170808.940/"
  :demand t
  :diminish yas-minor-mode
  :commands (yas-expand yas-minor-mode)
  :functions (yas-guess-snippet-directories yas-table-name)
  :defines (yas-guessed-modes)
  :mode ("/\\.emacs\\.d/snippets/" . snippet-mode)
  :bind (("C-c y TAB" . yas-expand)
         ("C-c y s" . yas-insert-snippet)
         ("C-c y n" . yas-new-snippet)
         ("C-c y v" . yas-visit-snippet-file))

  :preface
  (defun yas-new-snippet (&optional choose-instead-of-guess)
    (interactive "P")
    (let ((guessed-directories (yas-guess-snippet-directories)))
      (switch-to-buffer "*new snippet*")
      (erase-buffer)
      (kill-all-local-variables)
      (snippet-mode)
      (set (make-local-variable 'yas-guessed-modes)
           (mapcar #'(lambda (d) (intern (yas-table-name (car d))))
                   guessed-directories))
      (unless (and choose-instead-of-guess
                   (not (y-or-n-p
                         "Insert a snippet with useful headers? ")))
        (yas-expand-snippet
         (concat "\n"
                 "# -*- mode: snippet -*-\n"
                 "# name: $1\n"
                 "# --\n"
                 "$0\n")))))

  :config
  (yas-global-mode 1)
  (yas-load-directory "~/.emacs.d/snippets/")
  (bind-key "C-i" #'yas-next-field-or-maybe-expand yas-keymap))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                   Gnus                    ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package gnus
  :bind (("M-G" . gnus)
	 ("C-x m" . compose-mail))

  :preface
  (defun cg-feed-msmtp ()
    (if (message-mail-p)
	(save-excursion
	  (let* ((from
		  (save-restriction
		    (message-narrow-to-headers)
		    (message-fetch-field "from")))
		 (account
		  (cond
		   ((string-match "jorgen.arntzen@gmail.com" from)"gmail")
		   ((string-match "jorgenar@student.sv.uio.no" from)"uio"))))
	    (setq message-sendmail-extra-arguments (list '"-a" account))))))

  :init
  (add-hook 'message-send-mail-hook 'cg-feed-msmtp)

  (add-hook 'message-setup-hook 'mml-secure-message-encrypt)

  :config
  (eval-after-load 'gnus-topic
    '(progn
       (setq gnus-topic-topology '(("Gnus" visible)
                                   (("Gmail" visible nil nil))
                                   (("Universitetet i Oslo" visible nil nil))
                                   (("Diverse" visible))))

       (setq gnus-topic-alist
             '(("Gmail"
                "INBOX"
                "[Gmail]/Sendt e-post"
                "[Gmail]/Papirkurv")
               ("Universitetet i Oslo"
                "nnimap+uio:INBOX"
                "nnimap+uio:Sendte elementer"
                "nnimap+uio:Slettede elementer")
               ("Diverse"
                "nndraft:drafts")
               ("Gnus")))))

  (defun gnus-user-format-function-G (arg)
    (let ((mapped-name (assoc gnus-tmp-group group-name-map)))
      (if (null mapped-name)
	  gnus-tmp-group
	(cdr mapped-name))))

  (setq group-name-map '(("INBOX" . "Innboks")
			 ("[Gmail]/Sendt e-post" . "Sendt e-post")
			 ("[Gmail]/Papirkurv" . "Papirkurv")
                         ("nnimap+uio:INBOX" . "Innboks")
			 ("nnimap+uio:Sendte elementer" . "Sendt e-post")
			 ("nnimap+uio:Slettede elementer" . "Papirkurv")
			 ("nndraft:drafts" . "Utkast")))

  (use-package gnus-dired
    :commands gnus-dired-mode
    :init
    (add-hook 'dired-mode-hook 'gnus-dired-mode))

  (use-package gnus-demon
    :init
    (gnus-demon-add-handler 'gnus-demon-scan-mail 5 t)
    (gnus-demon-add-handler 'gnus-demon-scan-news 5 t))

  (use-package gnus-group
    :defer t
    :config
    (add-hook 'gnus-group-mode-hook
              (lambda ()
                (hl-line-mode))))

  (use-package gnus-sum
    :config
    (bind-key "F" #'gnus-summary-wide-reply-with-original
              gnus-summary-mode-map)
    (bind-key "F" #'gnus-article-wide-reply-with-original
              gnus-article-mode-map)

    (add-hook 'gnus-summary-mode-hook
              (lambda ()
                (hl-line-mode))))

  (use-package mml
    :defer t
    :diminish mml-mode))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                   LaTeX                   ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package tex-site
  :load-path "elpa/auctex-11.91.0/"
  :defines (latex-help-cmd-alist latex-help-file)
  :mode ("\\.tex\\'" . TeX-mode)

  :config
  (defun latex-help-cmd-alist ()
    "Scoop up the commands in the index of the LaTeX info manual. The values are saved in 'latex-help-cmd-alist' for speed."
    (if (not (assoc "\\begin" latex-help-cmd-alist))
	(save-window-excursion
	  (setq latex-help-cmd-alist nil)
	  (Info-goto-node (concat latex-help-file "Command Index"))
	  (goto-char (point-max))
	  (while (re-search-backward "^\\* \\(.+\\): *\\(.+\\)\\." nil t)
	    (let ((key (buffer-substring (match-beginning 1) (match-end 1)))
		  (value (buffer-substring (match-beginning 2)
					   (match-end 2))))
	      (add-to-list 'latex-help-cmd-alist (cons key value))))))
    latex-help-cmd-alist)

  (use-package ebib
    :load-path "elpa/ebib-20170711.1234/"
    :bind ("C-c E" . ebib)
    :preface
    (use-package parsebib
      :load-path "elpa/parsebib-20170501.347/"))

  (use-package latex-pretty-symbols
    :load-path "elpa/latex-pretty-symbols-20151112.244/")

  (use-package latex
    :defer t
    :config
    (add-hook 'latex-mode-hook
	      (lambda ()
		(local-set-key "\C-cb" 'ebib-insert-bibtex-key)
		(turn-on-cdlatex)
		(diminish' cdlatex-mode)
		(hl-line-mode 1)
                (pandoc-mode))))

  (add-hook 'LaTeX-mode-hook
	    (lambda ()
	      (LaTeX-math-mode)
	      (local-set-key "\C-cb" 'ebib-insert-bibtex-key)
	      (turn-on-cdlatex)
	      (diminish' cdlatex-mode)
	      (hl-line-mode 1)
              (pandoc-mode)))

  (info-lookup-add-help :mode 'LaTeX-mode
			:regexp ".*"
			:parse-rule "\\\\?[a-zA-Z]+\\|\\\\[^a-zA-Z]"
			:doc-spec '(("(latex2e)Concept Index" )
				    ("(latex2e)Command Index")))

  ;; PDF Tools settes som program for å vise PDF etter kompilering:
  (add-to-list 'TeX-view-program-list-builtin
               '("PDF Tools" TeX-pdf-tools-sync-view))

  (add-to-list 'TeX-view-program-selection
               '(output-pdf "PDF Tools"))

  ;; Følgende gjør det mulig å bruke Biber, i tillegg til BibTeX, i AUCTeX:
  (defun TeX-run-Biber (name command file)
    "Create a process for NAME using COMMAND to format FILE with Biber."
    (let ((process (TeX-run-command name command file)))
      (setq TeX-sentinel-function 'TeX-Biber-sentinel)
      (if TeX-process-asynchronous process
	(TeX-synchronous-sentinel name file process))))

  (defun TeX-Biber-sentinel (process name)
    "Cleanup TeX output buffer after running Biber."
    (goto-char (point-max))

    ;; Undersøker om Biber rapporterer advarsler eller feil:
    (cond ((re-search-backward (concat
				"^(There \\(?:was\\|were\\) \\([0-9]+\\)"
				"\\(warnings?\\|error messages?\\))") nil t)

           ;; Følgende forteller brukeren om antall advarsler eller feil, slik
           ;; at man kan undersøke om situasjonen blir bedre eller forverres:
	   (message (concat "Biber finished with %s %s."
			    "Type `%s' to display output.")
		    (match-string 1) (match-string 2)
		    (substitute-command-keys
		     "\\\\[TeX-recenter-output-buffer]")))
	  (t (message (concat "Biber finished successfully."
			      "Run LaTeX again to get citations right."))))
    (setq TeX-command-next TeX-command-default))

  (eval-after-load "tex"
    '(add-to-list 'TeX-command-list
		  '("Biber" "biber %s" TeX-run-Biber nil t :help "Run Biber"))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                 Org-mode                  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package org
  :load-path "elpa/org-20170807/"
  :diminish orgstruct-mode
  :mode ("\\.org\\'" . org-mode)
  :bind (("C-c l" . org-store-link)
	 ("C-c a" . org-agenda)
	 ("C-c c" . org-capture)
	 ("C-c b" . org-iswitchb)
	 ("C-c L" . org-insert-link-global)
	 ("C-c o" . org-open-at-point-global))

  :init
  ;; Det er fint å ha faktiske sirkulære punkter. Denne koden gjør et regulært
  ;; uttrykk for linjer, med begynnende strek med etterfølgende mellomrom, til
  ;; et Unicode-punkt:
  (font-lock-add-keywords
   'org-mode
   '(("^ +\\([-]\\) "
      (0 (prog1 ()
	   (compose-region (match-beginning 1)
			   (match-end 1)
			   "•"))))))

  :config
  (use-package org-agenda)
  (use-package org-bbdb)
  (use-package org-bibtex)
  (use-package org-crypt)
  (use-package ob-calc)
  (use-package ob-emacs-lisp)
  (use-package ob-gnuplot)
  (use-package ob-latex)
  (use-package ob-ledger)
  (use-package ob-python)
  (use-package ob-R)
  (use-package ob-shell)
  (use-package ox-md)

  ;; Følgende pakke sørger for fine punktlister:
  (use-package org-bullets
    :load-path "elpa/org-bullets-20140918.1137/")

  (use-package outorg
    :load-path "elpa/outorg-20170414.1215/"
    :bind ("C-c '" . outorg-edit-as-org)
    :config
    (require 'outshine))

  (use-package outshine
    :load-path "elpa/outshine-20170721.521/"
               "elpa/navi-mode-20170414.1228/"
    :commands outshine-hook-function
    :config
    (use-package navi-mode))

  (use-package poporg
    :load-path "elpa/poporg-20170403.51/"
    :bind ("C-c e o" . poporg-dwim))

  ;; Følgende innstillinger gjør at RefTeX fungerer i Org-mode. "C-c ("
  ;; spesifiseres som keybinding for ikke å overlappe med eksisterende
  ;; kommando:
  (defun org-mode-reftex-setup ()
    (load-library "reftex")
    (and (buffer-file-name)
         (file-exists-p (buffer-file-name))
         (reftex-parse-all))
    (define-key org-mode-map (kbd "C-c [") 'reftex-citation))

  ;; Følgende innstillinger gjør at Yasnippet fungerer ordentlig i Org-mode:
  (defun yas-org-very-safe-expand ()
    (let ((yas-fallback-behavior 'return-nil))
      (and (fboundp 'yas-expand) (yas-expand))))

  (add-hook 'org-mode-hook
	    (lambda ()
	      (org-bullets-mode 1)
	      (org-indent-mode t)
	      (diminish 'org-indent-mode)
	      (org-mode-reftex-setup)
	      (turn-on-org-cdlatex)
	      (diminish 'org-cdlatex-mode)
	      (yas-org-very-safe-expand)))

  ;; Følgende variabel legger LaTeX til i listen over språk som kan kan være
  ;; årsak til noweb-feil:
  (add-to-list 'org-babel-noweb-error-langs "latex")

  ;; Følgende blokk forteller TODO at oppgaver automatisk skal betegnes som
  ;; ferdige når alle underliggende oppgaver er ferdige:
  (defun org-summary-todo (n-done n-not-done)
    "Switch entry to DONE when all subentries are done, to TODO otherwise"
    (let (org-log-done org-log-states) ;; Slår av loggingen.
      (org-todo (if (= n-not-done 0) "DONE" "TODO"))))

  (add-hook 'org-after-todo-statistics-hook 'org-summary-todo))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;            Post-initialisering            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(when window-system
  (let ((elapsed (float-time (time-subtract (current-time)
					    emacs-start-time))))
    (message "Loading %s...done (%.3fs)" load-file-name elapsed))

  (add-hook 'after-init-hook
            `(lambda ()
               (let ((elapsed (float-time (time-subtract (current-time)
							 emacs-start-time))))
                 (message "Loading %s...done (%.3fs) [after-init]"
                          ,load-file-name elapsed)))
            t))
