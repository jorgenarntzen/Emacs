(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(font-lock-comment-face ((t (:foreground "#8d8d84")))))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(TeX-PDF-mode t)
 '(TeX-auto-save t)
 '(TeX-auto-untabify t)
 '(TeX-electric-escape t)
 '(TeX-engine (quote xetex))
 '(TeX-expand-list
   (quote
    (("%p" TeX-printer-query)
     ("%q"
      (lambda nil
        (TeX-printer-query t)))
     ("%V"
      (lambda nil
        (TeX-source-correlate-start-server-maybe)
        (TeX-view-command-raw)))
     ("%vv"
      (lambda nil
        (TeX-source-correlate-start-server-maybe)
        (TeX-output-style-check TeX-output-view-style)))
     ("%v"
      (lambda nil
        (TeX-source-correlate-start-server-maybe)
        (TeX-style-check TeX-view-style)))
     ("%r"
      (lambda nil
        (TeX-style-check TeX-print-style)))
     ("%l"
      (lambda nil
        (TeX-style-check LaTeX-command-style)))
     ("%(PDF)"
      (lambda nil
        (if
            (and
             (eq TeX-engine
                 (quote default))
             (or TeX-PDF-mode TeX-DVI-via-PDFTeX))
            "pdf" "")))
     ("%(PDFout)"
      (lambda nil
        (cond
         ((and
           (eq TeX-engine
               (quote xetex))
           (not TeX-PDF-mode))
          " -no-pdf")
         ((and
           (eq TeX-engine
               (quote luatex))
           (not TeX-PDF-mode))
          " --output-format=dvi")
         ((and
           (eq TeX-engine
               (quote default))
           (not TeX-PDF-mode)
           TeX-DVI-via-PDFTeX)
          " \"\\pdfoutput=0 \"")
         (t ""))))
     ("%(mode)"
      (lambda nil
        (if TeX-interactive-mode "" " -interaction=nonstopmode")))
     ("%(o?)"
      (lambda nil
        (if
            (eq TeX-engine
                (quote omega))
            "o" "")))
     ("%(tex)"
      (lambda nil
        (eval
         (nth 2
              (assq TeX-engine
                    (TeX-engine-alist))))))
     ("%(latex)"
      (lambda nil
        (eval
         (nth 3
              (assq TeX-engine
                    (TeX-engine-alist))))))
     ("%(execopts)" ConTeXt-expand-options)
     ("%S" TeX-source-correlate-expand-options)
     ("%dS" TeX-source-specials-view-expand-options)
     ("%cS" TeX-source-specials-view-expand-client)
     ("%(outpage)"
      (lambda nil
        (if TeX-source-correlate-output-page-function
            (funcall TeX-source-correlate-output-page-function)
          "1")))
     ("%s" file nil t)
     ("%t" file t t)
     ("%`"
      (lambda nil
        (setq TeX-command-pos t TeX-command-text "")))
     (" \"\\"
      (lambda nil
        (if
            (eq TeX-command-pos t)
            (setq TeX-command-pos pos pos
                  (+ 3 pos))
          (setq pos
                (1+ pos)))))
     ("\""
      (lambda nil
        (if
            (numberp TeX-command-pos)
            (setq TeX-command-text
                  (concat TeX-command-text
                          (substring command TeX-command-pos
                                     (1+ pos)))
                  command
                  (concat
                   (substring command 0 TeX-command-pos)
                   (substring command
                              (1+ pos)))
                  pos TeX-command-pos TeX-command-pos t)
          (setq pos
                (1+ pos)))))
     ("%'"
      (lambda nil
        (prog1
            (if
                (stringp TeX-command-text)
                (progn
                  (setq pos
                        (+
                         (length TeX-command-text)
                         9)
                        TeX-command-pos
                        (and
                         (string-match " "
                                       (funcall file t t))
                         "\""))
                  (concat TeX-command-text " \"\\input\""))
              (setq TeX-command-pos nil)
              "")
          (setq TeX-command-text nil))))
     ("%n" TeX-current-line)
     ("%d" file "dvi" t)
     ("%f" file "ps" t)
     ("%o"
      (lambda nil
        (funcall file
                 (TeX-output-extension)
                 t)))
     ("%b" TeX-current-file-name-master-relative)
     ("%m" preview-create-subdirectory)
     ("%O"
      (lambda nil
        (expand-file-name
         (funcall file
                  (TeX-output-extension)
                  t)))))))
 '(TeX-file-extensions
   (quote
    ("Snw" "Rnw" "nw" "tex" "sty" "cls" "ltx" "texi" "texinfo")))
 '(TeX-parse-self t)
 '(abbrev-file-name "~/.emacs.d/data/abbrev_defs")
 '(ac-auto-show-menu 1.0)
 '(ac-auto-start 3)
 '(ac-comphist-file "~/.emacs.d/data/ac-comphist.dat")
 '(ac-dwim nil)
 '(ac-ignore-case nil)
 '(ac-trigger-key "<tab>")
 '(ac-use-menu-map t)
 '(alert-default-style (quote notifier))
 '(allout-command-prefix ".")
 '(ansi-color-for-comint-mode (quote filter))
 '(auto-revert-interval 0.5)
 '(auto-save-file-name-transforms (quote ((".*" "~/.emacs.d/auto-save-list/" t))))
 '(auto-save-interval 64)
 '(auto-save-timeout 2)
 '(avy-case-fold-search nil)
 '(avy-keys (quote (97 111 101 117 105 100 104 116 110 115)))
 '(backup-directory-alist (quote (("." . "~/.emacs.d/backups/"))))
 '(bbdb-default-country "")
 '(bbdb-file "~/.emacs.d/data/bbdb")
 '(bbdb-pop-up-window-size 0.25)
 '(bbdb-silent t)
 '(bibtex-dialect "BibTeX")
 '(blink-cursor-mode t)
 '(byte-compile-verbose nil)
 '(column-number-mode t)
 '(comint-move-point-for-output t)
 '(comint-scroll-to-bottom-on-input t)
 '(company-frontends
   (quote
    (company-pseudo-tooltip-unless-just-one-frontend company-echo-metadata-frontend company-preview-frontend)))
 '(compilation-always-kill t)
 '(compilation-ask-about-save nil)
 '(compilation-context-lines 10)
 '(compilation-scroll-output (quote first-error))
 '(compilation-skip-threshold 2)
 '(compilation-window-height 100)
 '(csv-separators (quote ("," ";" "|" " ")))
 '(current-language-environment "UTF-8")
 '(cursor-type (quote bar))
 '(custom-raised-buttons nil)
 '(custom-safe-themes t)
 '(custom-theme-directory "~/.emacs.d/themes/")
 '(default-frame-alist
    (quote
     ((alpha 100 . 100)
      (top . 193)
      (left . 600)
      (height . 35)
      (width . 80)
      (left-fringe . 0)
      (right-fringe . 0)
      (vertical-scroll-bars)
      (horizontal-scroll-bars))))
 '(default-input-method "latin-1-prefix")
 '(delete-by-moving-to-trash t)
 '(delete-old-versions -1)
 '(diff-mode-hook
   (quote
    (diff-delete-empty-files diff-make-unified smerge-mode)))
 '(directory-free-space-args "-hk")
 '(dired-clean-up-buffers-too nil)
 '(dired-details-hidden-string "")
 '(dired-dwim-target t)
 '(dired-listing-switches "-lah")
 '(dired-no-confirm
   (quote
    (byte-compile chgrp chmod chown copy hardlink symlink touch)))
 '(dired-omit-files "^\\icon\\|^\\.\\|^#.#$\\|.~$")
 '(dired-recursive-copies (quote always))
 '(dired-recursive-deletes (quote always))
 '(display-time-24hr-format t)
 '(display-time-default-load-average nil)
 '(display-time-mode t)
 '(display-time-use-mail-icon t)
 '(doc-view-dvipdf-program "/usr/bin/dvipdf")
 '(doc-view-dvipdfm-program "/usr/bin/dvipdfm")
 '(doc-view-ghostscript-program "/usr/bin/gs")
 '(doc-view-odf->pdf-converter-function (quote doc-view-odf->pdf-converter-unoconv))
 '(doc-view-odf->pdf-converter-program "/usr/bin/unoconv")
 '(doc-view-pdftotext-program "/usr/bin/pdftotext")
 '(doc-view-ps2pdf-program "/usr/bin/ps2pdf")
 '(doc-view-resolution 300)
 '(ebib-autogenerate-keys t)
 '(electric-indent-mode nil)
 '(emacs-lisp-mode-hook
   (quote
    (turn-on-auto-fill
     (lambda nil
       (ignore-errors
         (diminish
          (quote auto-fill-function))))
     eldoc-mode
     (lambda nil
       (local-set-key
        [(meta 46)]
        (quote find-function))
       (local-set-key
        [(control 109)]
        (quote newline-and-indent))))))
 '(enable-recursive-minibuffers t)
 '(epg-gpg-program "/usr/bin/gpg2")
 '(eshell-history-size 1000)
 '(eshell-ls-dired-initial-args (quote ("-h")))
 '(eshell-ls-exclude-regexp "~\\'")
 '(eshell-ls-initial-args (quote ("-h")))
 '(eshell-modules-list
   (quote
    (eshell-alias eshell-basic eshell-cmpl eshell-dirs eshell-glob eshell-hist eshell-ls eshell-pred eshell-prompt eshell-rebind eshell-script eshell-smart eshell-term eshell-unix eshell-xtra)))
 '(eshell-prompt-function
   (lambda nil
     (concat
      (abbreviate-file-name
       (eshell/pwd))
      (if
          (=
           (user-uid)
           0)
          " # " " $ "))))
 '(eshell-save-history-on-exit t)
 '(eshell-stringify-t nil)
 '(eshell-term-name "ansi")
 '(ess-pdf-viewer-pref "emacsclient")
 '(ess-swv-plug-into-AUCTeX-p t)
 '(ess-swv-processor (quote knitr))
 '(eww-lnum-actions-link-alist
   (quote
    ("----  Link   ----"
     (102 eww-lnum-visit "Visit")
     (101
      (lambda
        (info)
        (eww-lnum-visit info nil t))
      "Edit URL and visit")
     (70
      (lambda
        (info)
        (eww-lnum-visit info t))
      "Visit in new buffer")
     (69
      (lambda
        (info)
        (eww-lnum-visit info t t))
      "Edit URL and visit in new buffer")
     (98
      (lambda
        (info)
        (eww-lnum-visit info :background))
      "Open in background")
     (66
      (lambda
        (info)
        (eww-lnum-visit info :background t))
      "Edit URL and open in background")
     (100
      (lambda
        (info)
        (save-excursion
          (goto-char
           (cadr info))
          (eww-download)))
      "Download")
     (119
      (lambda
        (info)
        (let
            ((url
              (car info)))
          (kill-new url)
          (message url)))
      "Copy")
     (38
      (lambda
        (info)
        (eww-browse-with-external-browser
         (car info)))
      "Open in external browser")
     (68
      (lambda
        (info)
        (shell-command
         (concat "aria2c -d ~/Downloads -x5 '"
                 (car info)
                 "' &")
         "*Aria*"))
      "Download with Aria"))))
 '(exec-path
   (quote
    ("/usr/local/bin" "/usr/local/sbin" "/usr/bin" "/usr/sbin" "/home/jorgen/.local/bin" "/home/jorgen/bin")))
 '(fill-column 78)
 '(flycheck-display-errors-delay 0.0)
 '(flycheck-standard-error-navigation nil)
 '(flymake-compilation-prevents-syntax-check nil)
 '(flyspell-abbrev-p nil)
 '(flyspell-use-meta-tab nil)
 '(font-latex-match-reference-keywords
   (quote
    (("cite" "[{")
     ("cites" "[{}]")
     ("footcite" "[{")
     ("footcites" "[{")
     ("parencite" "[{")
     ("textcite" "[{")
     ("fullcite" "[{")
     ("citetitle" "[{")
     ("citetitles" "[{")
     ("headlessfullcite" "[{"))))
 '(font-lock-support-mode (quote jit-lock-mode))
 '(font-lock-verbose nil)
 '(frame-title-format
   (quote
    (:eval
     (concat
      (if buffer-file-name default-directory "%b")
      "    "
      (number-to-string
       (cdr
        (assq
         (quote width)
         (frame-parameters))))
      "x"
      (number-to-string
       (cdr
        (assq
         (quote height)
         (frame-parameters))))))) t)
 '(garbage-collection-messages t)
 '(gc-cons-threshold 3500000)
 '(global-auto-complete-mode t)
 '(global-font-lock-mode t nil (font-lock))
 '(global-prettify-symbols-mode t)
 '(gnus-cache-directory "~/.emacs.d/gnus/news/cache/")
 '(gnus-directory "~/.emacs.d/gnus/news/")
 '(gnus-gcc-mark-as-read t)
 '(gnus-group-line-format "%M%S%5y: %uG
")
 '(gnus-group-mode-hook (quote (gnus-topic-mode gnus-agent-mode hl-line-mode)))
 '(gnus-home-directory "~/.emacs.d/gnus/")
 '(gnus-ignored-newsgroups
   "^to\\\\.\\\\|^[0-9. ]+\\\\( \\\\|$\\\\)\\\\|^[\\\"]\\\"[#'()]")
 '(gnus-inhibit-images nil)
 '(gnus-message-archive-group (quote ((format-time-string "sent.%Y"))))
 '(gnus-secondary-select-methods
   (quote
    ((nnimap "uio"
             (nnimap-address "imap.uio.no")
             (nnimap-server-port 993)
             (nnimap-stream ssl)
             (nnir-search-engine imap)))))
 '(gnus-select-method
   (quote
    (nnimap "gmail"
            (nnimap-address "imap.gmail.com")
            (nnimap-server-port 993)
            (nnimap-stream ssl)
            (nnir-search-engine imap))))
 '(gnus-summary-line-format
   "%0{%U%R%z%}%3{│%} %1{%d%} %3{│%}  %4{%-23,23f%}  %3{│%} %1{%B%}%s\\n")
 '(gnus-topic-line-format "%i[ %(%{%n%}%): %A ]%v
")
 '(gnus-use-cache t)
 '(gnus-verbose 4)
 '(grep-find-command (quote ("ag --noheading --column --ignore branches " . 43)))
 '(helm-adaptive-history-file "~/.emacs.d/data/helm-adaptive-history")
 '(helm-buffers-fuzzy-matching t)
 '(helm-completing-read-handlers-alist
   (quote
    ((describe-function . helm-completing-read-symbols)
     (describe-variable . helm-completing-read-symbols)
     (debug-on-entry . helm-completing-read-symbols)
     (find-function . helm-completing-read-symbols)
     (find-tag . helm-completing-read-with-cands-in-buffer)
     (ffap-alternate-file)
     (ffap)
     (tmm-menubar)
     (find-file)
     (magit-status . ido)
     (dired-do-copy . ido)
     (dired-do-rename . ido)
     (dired-create-directory . ido)
     (mml-attach-file . ido))))
 '(helm-delete-minibuffer-contents-from-point t)
 '(helm-ff-file-name-history-use-recentf t)
 '(helm-ff-search-library-in-sexp t)
 '(helm-ff-skip-boring-files t)
 '(helm-for-files-preferred-list
   (quote
    (helm-source-files-in-current-dir helm-source-recentf helm-source-bookmarks helm-source-file-cache helm-source-buffers-list helm-source-locate helm-source-ls-git)))
 '(helm-grep-default-recurse-command "rg --no-heading --color=always -j4 -n%cH -e %p %f")
 '(helm-ls-git-show-abs-or-relative (quote relative))
 '(helm-quick-update t)
 '(helm-recentf-fuzzy-match t)
 '(hippie-expand-try-functions-list
   (quote
    (yas-hippie-try-expand try-complete-file-name-partially try-complete-file-name try-expand-all-abbrevs try-expand-list try-expand-line try-expand-dabbrev try-expand-dabbrev-all-buffers try-expand-dabbrev-from-kill try-complete-lisp-symbol-partially try-complete-lisp-symbol)))
 '(history-delete-duplicates t)
 '(history-length 200)
 '(ibuffer-default-display-maybe-show-predicates t)
 '(ibuffer-expert t)
 '(ibuffer-formats
   (quote
    ((mark modified read-only " "
           (name 16 -1)
           " "
           (size 6 -1 :right)
           " "
           (mode 16 16)
           " " filename)
     (mark " "
           (name 16 -1)
           " " filename))))
 '(ibuffer-saved-filter-groups
   (quote
    (("default"
      ("Commands"
       (or
        (mode . shell-mode)
        (mode . eshell-mode)
        (mode . term-mode)
        (mode . compilation-mode)))
      ("Lisp"
       (mode . emacs-lisp-mode))
      ("Dired"
       (mode . dired-mode))
      ("Gnus"
       (or
        (mode . message-mode)
        (mode . mail-mode)
        (mode . gnus-group-mode)
        (mode . gnus-summary-mode)
        (mode . gnus-article-mode)
        (name . "^\\.newsrc-dribble")
        (name . "^\\*\\(sent\\|unsent\\|fetch\\)")
        (name . "^ \\*\\(nnimap\\|nntp\\|nnmail\\|gnus\\|server\\)")))
      ("Org"
       (or
        (name . "^\\*Calendar\\*$")
        (name . "^\\*Org Agenda")
        (name . "^ \\*Agenda")
        (name . "^diary$")
        (mode . org-mode)))
      ("Helm"
       (or
        (mode . helm-mode)
        (name . "\\<helm\\>")))
      ("Magit"
       (or
        (mode . magit-status-mode)
        (mode . magit-log-mode)
        (name . "^\\*magit")
        (name . "git-monitor")))
      ("Emacs"
       (or
        (name . "^\\*scratch\\*$")
        (name . "^\\*Messages\\*$")
        (name . "^\\*\\(Customize\\|Help\\)")
        (name . "\\*\\(Echo\\|Minibuf\\)")))
      ("ESS"
       (or
        (mode . ess-mode)
        (name . "^\\*ESS\\*$")
        (name . "^\\*R\\*$")))
      ("LaTeX"
       (or
        (mode . latex-mode)))))))
 '(ibuffer-show-empty-filter-groups nil)
 '(ibuffer-use-other-window t)
 '(ido-auto-merge-work-directories-length 0)
 '(ido-cannot-complete-command (quote ido-exit-minibuffer))
 '(ido-decorations
   (quote
    ("{" "}" ", " ", ..." "[" "]" " [No match]" " [Matched]" " [Not readable]" "[Too big]" " [Confirm]")))
 '(ido-enable-flex-matching t)
 '(ido-enable-last-directory-history nil)
 '(ido-enable-tramp-completion nil)
 '(ido-enter-matching-directory (quote first))
 '(ido-ignore-files
   (quote
    ("\\`CVS/" "\\`#" "\\`.#" "\\`\\.\\./" "\\`\\./" "\\`\\.DS_Store" "\\`\\.localized" "\\.sparsebundle/" "\\.dmg\\'" "\\Icon")))
 '(ido-mode (quote both) nil (ido))
 '(ido-save-directory-list-file "~/.emacs.d/data/ido.last")
 '(ido-use-virtual-buffers t)
 '(image-dired-cmd-create-temp-image-program "/usr/bin/convert")
 '(image-dired-cmd-create-thumbnail-program "/usr/bin/convert")
 '(image-dired-cmd-pngcrush-program "/usr/bin/pngcrush")
 '(image-dired-cmd-pngnq-program "/usr/bin/pngnq")
 '(image-dired-cmd-read-exif-data-program "/usr/bin/exiftool")
 '(image-dired-cmd-rotate-original-program "/usr/bin/jpegtran")
 '(image-dired-cmd-rotate-thumbnail-program "/usr/bin/mogrify")
 '(image-dired-cmd-write-exif-data-program "/usr/bin/exiftool")
 '(indent-tabs-mode nil)
 '(inferior-R-program-name "/usr/bin/R")
 '(inhibit-startup-screen t)
 '(initial-major-mode (quote org-mode))
 '(initial-scratch-message nil)
 '(ispell-dictionary "nb_NO")
 '(ispell-extra-args (quote ("--sug-mode=fast" "--keyboard=dvorak")))
 '(kill-do-not-save-duplicates t)
 '(kill-whole-line t)
 '(large-file-warning-threshold nil)
 '(ledger-binary-path "/usr/bin/ledger")
 '(ledger-post-use-completion-engine :ido)
 '(ledger-post-use-ido t)
 '(line-number-mode t)
 '(load-prefer-newer t)
 '(magit-completing-read-function (quote helm--completing-read-default))
 '(magit-fetch-arguments nil)
 '(magit-process-popup-time 15)
 '(mail-default-directory "~/.emacs.d/gnus/mail/")
 '(mail-from-style nil)
 '(mail-source-directory "~/.emacs.d/gnus/mail/")
 '(mail-source-report-new-mail-interval 5)
 '(mail-sources
   (quote
    ((imap :server "imap.gmail.com" :port 993 :user "jorgen.arntzen@gmail.com" :stream ssl)
     (imap :server "imap.uio.no" :port 993 :user "jorgenar" :stream ssl))))
 '(mail-user-agent (quote gnus-user-agent))
 '(markdown-command "pandoc -f markdown_mmd -S")
 '(markdown-command-needs-filename t)
 '(markdown-enable-math t)
 '(markdown-open-command "open-markdown")
 '(menu-bar-mode nil)
 '(message-directory "~/.emacs.d/gnus/mail/")
 '(message-fill-column 78)
 '(message-interactive t)
 '(message-log-max 16384)
 '(message-mode-hook
   (quote
    (abbrev-mode footnote-mode turn-on-flyspell turn-on-orgstruct++ turn-on-orgtbl turn-off-auto-fill)))
 '(message-send-mail-function (quote message-send-mail-with-sendmail))
 '(message-sendmail-envelope-from (quote header))
 '(message-sendmail-f-is-evil t)
 '(message-signature t)
 '(message-signature-file "~/.emacs.d/gnus/mail/.signature")
 '(mm-discouraged-alternatives (quote ("application/msword" "text/richtext" "text/html")))
 '(mm-text-html-renderer (quote gnus-w3m))
 '(mml-secure-openpgp-encrypt-to-self t)
 '(mml-secure-openpgp-signers (quote ("1049134F")))
 '(mml2015-encrypt-to-self t)
 '(mml2015-signers (quote ("1049134F")))
 '(mouse-autoselect-window t)
 '(mouse-wheel-progressive-speed nil)
 '(mouse-wheel-scroll-amount (quote (1)))
 '(next-line-add-newlines nil)
 '(nntp-authinfo-file "~/.authinfo.gpg")
 '(ns-alternate-modifier nil)
 '(ns-use-srgb-colorspace nil)
 '(org-M-RET-may-split-line (quote ((headline) (default . t))))
 '(org-adapt-indentation nil)
 '(org-agenda-auto-exclude-function (quote org-my-auto-exclude-function))
 '(org-agenda-deadline-leaders (quote ("!D!: " "D%02d: ")))
 '(org-agenda-default-appointment-duration 60)
 '(org-agenda-files (quote ("~/Dropbox/Todo.org")))
 '(org-agenda-fontify-priorities t)
 '(org-agenda-include-diary t)
 '(org-agenda-inhibit-startup t)
 '(org-agenda-log-mode-items (quote (closed clock state)))
 '(org-agenda-ndays 1)
 '(org-agenda-persistent-filter t)
 '(org-agenda-prefix-format
   (quote
    ((agenda . "  %-11c%?-12t% s")
     (timeline . "  % s")
     (todo . "  %-11c")
     (tags . "  %-11c"))))
 '(org-agenda-scheduled-leaders (quote ("" "S%d: ")))
 '(org-agenda-show-all-dates t)
 '(org-agenda-skip-deadline-if-done t)
 '(org-agenda-skip-scheduled-if-deadline-is-shown t)
 '(org-agenda-skip-scheduled-if-done t)
 '(org-agenda-skip-unavailable-files t)
 '(org-agenda-sorting-strategy
   (quote
    ((agenda habit-down time-up todo-state-up priority-down user-defined-down)
     (todo priority-down category-keep)
     (tags priority-down category-keep)
     (search category-keep))))
 '(org-agenda-start-on-weekday nil)
 '(org-agenda-start-with-log-mode nil)
 '(org-agenda-tags-column -100)
 '(org-agenda-use-time-grid nil)
 '(org-archive-location "TODO-archive::")
 '(org-babel-load-languages
   (quote
    ((calc . t)
     (emacs-lisp . t)
     (gnuplot . t)
     (latex . t)
     (ledger . t)
     (python . t)
     (R . t)
     (shell . t))))
 '(org-bullets-bullet-list (quote ("○")))
 '(org-completion-use-ido t)
 '(org-confirm-babel-evaluate nil)
 '(org-confirm-elisp-link-function nil)
 '(org-confirm-shell-link-function nil)
 '(org-crypt-disable-auto-save t)
 '(org-crypt-key "1049134F")
 '(org-crypt-tag-matcher "crypt")
 '(org-cycle-global-at-bob t)
 '(org-deadline-warning-days 14)
 '(org-default-notes-file "~/Org/Notater.org")
 '(org-directory "~/Org/")
 '(org-edit-src-content-indentation 0)
 '(org-enforce-todo-dependencies t)
 '(org-export-babel-evaluate nil)
 '(org-export-backends (quote (ascii beamer html icalendar latex md odt)))
 '(org-export-use-babel nil)
 '(org-extend-today-until 8)
 '(org-fast-tag-selection-single-key (quote expert))
 '(org-fontify-done-headline t)
 '(org-fontify-whole-heading-line t)
 '(org-footnote-section nil)
 '(org-hidden-keywords (quote (author date email title)))
 '(org-hide-emphasis-markers t)
 '(org-hide-leading-stars t)
 '(org-image-actual-width (quote (800)))
 '(org-insert-heading-respect-content t)
 '(org-latex-create-formula-image-program (quote imagemagick))
 '(org-modules
   (quote
    (org-bbdb org-bibtex org-docview org-gnus org-habit org-info)))
 '(org-pretty-entities t)
 '(org-preview-latex-default-process (quote imagemagick))
 '(org-return-follows-link t)
 '(org-reverse-note-order t)
 '(org-src-fontify-natively t)
 '(org-src-tab-acts-natively t)
 '(org-tags-column -97)
 '(org-time-clocksum-use-fractional t)
 '(org-todo-repeat-to-state "TODO")
 '(org-use-property-inheritance (quote ("AREA")))
 '(org-use-speed-commands t)
 '(org-use-tag-inheritance nil)
 '(pabbrev-idle-timer-verbose nil)
 '(package-archives
   (quote
    (("gnu" . "http://elpa.gnu.org/packages/")
     ("melpa" . "http://melpa.org/packages/")
     ("org" . "https://orgmode.org/elpa/"))))
 '(package-selected-packages
   (quote
    (alert anaphora ascii async auctex auto-complete auto-complete-auctex auto-yasnippet avy awk-it awk-it backup-each-save bbdb benchmark-init bind-key bookmark+ browse-kill-ring browse-kill-ring+ button-lock cdlatex company concurrent csv-mode ctable dash deferred diminish dired+ dired-details dired-toggle diredful ebib eldoc-extension elisp-slime-nav elpy epc epl ess eww-lnum f find-file-in-project flx flx-ido flycheck fuzzy gh git git-commit gntp gnuplot-mode helm helm-core helm-descbinds helm-make helm-projectile helm-swoop highlight-cl highlight-indentation hl-line+ ht hydra ido-hacks inf-ruby isearch-dabbrev ivy jedi jedi-core julia-mode latex-pretty-symbols ledger-mode let-alist log4e logito lua-mode lusty-explorer macrostep magit magit-popup makey markdown-mode markdown-preview-mode marshal navi-mode org org-bullets outorg outshine pabbrev pandoc-mode paredit parsebib pcache pdf-tools pkg-info polymode poporg popup popwin pos-tip powerline projectile python python-environment pyvenv rainbow-mode redshank rich-minority s selected seq session smart-mode-line smart-mode-line-powerline-theme smex solarized-theme tablist use-package uuidgen web web-server websocket with-editor xml-rpc yari yasnippet zenburn-theme)))
 '(parens-require-spaces t)
 '(pcomplete-compare-entry-function (quote file-newer-than-file-p))
 '(pdf-misc-print-programm "/usr/bin/lpr")
 '(pdf-tools-enabled-hook (quote ((lambda nil (auto-revert-mode)))))
 '(projectile-cache-file "~/.emacs.d/data/projectile.cache")
 '(projectile-enable-caching t)
 '(projectile-globally-ignored-files (quote ("TAGS" "GPATH" "GRTAGS" "GTAGS" "ID")))
 '(projectile-known-projects-file "~/.emacs.d/data/projectile-bookmarks.eld")
 '(projectile-switch-project-action (quote helm-projectile))
 '(read-buffer-function (quote ido-read-buffer))
 '(recentf-auto-cleanup (quote never))
 '(recentf-exclude
   (quote
    ("~\\'" "\\`out\\'" "\\.log\\'" "^/[^/]*:" "\\.el\\.gz\\'")))
 '(recentf-max-saved-items 2000)
 '(recentf-save-file "~/.emacs.d/data/recentf")
 '(reftex-cite-format
   (quote
    ((13 . "\\cite[]{%l}")
     (116 . "\\textcite{%l}")
     (97 . "\\autocite[]{%l}")
     (112 . "\\parencite{%l}")
     (102 . "\\footcite[][]{%l}")
     (70 . "\\fullcite[]{%l}")
     (120 . "[]{%l}")
     (88 . "{%l}"))))
 '(reftex-default-bibliography
   (quote
    ("~/Dropbox/Bibliografier/masteroppgave.bib" "~/Dropbox/Bibliografier/pandoc.bib")))
 '(reftex-enable-partial-scans t)
 '(reftex-file-extensions
   (quote
    (("Snw" "Rnw" "nw" "tex" ".tex" ".ltx")
     ("bib" ".bib"))))
 '(reftex-plug-into-AUCTeX t)
 '(reftex-save-parse-info t)
 '(reftex-trust-label-prefix t)
 '(reftex-use-multiple-selection-buffers t)
 '(same-window-buffer-names
   (quote
    ("*eshell*" "*shell*" "*mail*" "*inferior-lisp*" "*ielm*" "*scheme*")))
 '(save-abbrevs (quote silently))
 '(save-interprogram-paste-before-kill t)
 '(scroll-conservatively 101)
 '(scroll-step 1)
 '(select-enable-clipboard t)
 '(sendmail-program "/usr/bin/msmtp")
 '(sentence-end-double-space nil)
 '(session-globals-exclude (quote (load-history flyspell-auto-correct-ring)))
 '(session-globals-include
   (quote
    ((kill-ring 10 nil)
     (session-file-alist 200 t)
     (file-name-history 200 nil)
     search-ring regexp-search-ring sr-history-registry)))
 '(session-initialize (quote (session places keys)))
 '(session-name-disable-regexp "\\(\\`/tmp\\|COMMIT_EDITMSG\\)")
 '(session-registers (quote (t (0 . 127))))
 '(session-save-file "~/.emacs.d/data/session")
 '(smex-save-file "~/.emacs.d/data/smex-items")
 '(sml/theme (quote light-powerline))
 '(switch-to-buffer-preserve-window-point t)
 '(tags-case-fold-search nil)
 '(temp-buffer-resize-mode t nil (help))
 '(text-mode-hook
   (quote
    (turn-on-flyspell visual-line-mode turn-off-auto-fill
                      (lambda nil
                        (ignore-errors
                          (diminish
                           (quote auto-fill-function))))
                      (lambda nil
                        (abbrev-mode 1)))))
 '(tool-bar-mode nil)
 '(tramp-auto-save-directory "~/.backups")
 '(tramp-default-method-alist
   (quote
    (("\\`\\(127\\.0\\.0\\.1\\|::1\\|localhost6?\\)\\'" "\\`root\\'" "sudo"))))
 '(trash-directory "~/.local/share/Trash/files")
 '(undo-limit 800000)
 '(uniquify-buffer-name-style (quote post-forward-angle-brackets) nil (uniquify))
 '(url-configuration-directory "~/.emacs.d/data/url/")
 '(use-package-verbose t)
 '(user-full-name "Jørgen Knutsønn Arntzen")
 '(user-mail-address "jorgen.arntzen@gmail.com")
 '(vc-follow-symlinks t)
 '(vc-git-diff-switches (quote ("-w" "-U3")))
 '(vc-handled-backends (quote (GIT SVN CVS Bzr Hg)))
 '(vc-make-backup-files t)
 '(version-control t)
 '(visible-bell t)
 '(whitespace-line-column 80)
 '(whitespace-style (quote (face trailing lines space-before-tab empty)))
 '(x-stretch-cursor t)
 '(yas-prompt-functions
   (quote
    (yas-ido-prompt yas-completing-prompt yas-no-prompt)))
 '(yas-triggers-in-field t)
 '(yas-wrap-around-region t))
