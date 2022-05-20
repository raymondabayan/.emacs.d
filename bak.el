;; init file for Emacs

;;Description: Some Plugins may need to be installed by M-x package-install. Should be ;;applicable to text editing, file navigation (Vim-like), and scripting (python, R, matlab). ;;Also has a few GUI modifications to look pretty.
 

;; Package repos elpy and melpa
(require 'package)
(add-to-list 'package-archives
             '("elpy" . "http://jorgenschaefer.github.io/packages/"))
(add-to-list 'package-archives
	     '("melpa" . "https://melpa.org/packages/"))
;; (package-refresh-contents)
;; Uncomment this line if you want emacs to do this automatically on startup (will slow down start up time and will show an error if not connected to the internet
(package-initialize)

;; Use-package
(unless (package-installed-p 'use-package)
  (package-install 'use-package))
(setq use-package-always-ensure t)
  
;; Startup Performance
;; Garbage collection
;; Using garbage magic hack.
(use-package gcmh
  :config
  (gcmh-mode 1)
  )
;; Setting garbage collection threshold
(setq gc-cons-threshold 402653184
    gc-cons-percentage 0.6)
;; Can reduce startup time by ~2sec

;; Profile emacs startup to show startup stats
(add-hook 'emacs-startup-hook
          (lambda ()
            (message "*** Emacs loaded in %s with %d garbage collections."
                     (format "%.2f seconds"
                             (float-time
                              (time-subtract after-init-time before-init-time)))
                     gcs-done)))

;; Silence compiler warnings as they can be pretty disruptive
(setq comp-async-report-warnings-errors nil)

;; Native Compil
;; Silence compiler warnings as they can be pretty disruptive
(if (boundp 'comp-deferred-compilation)
    (setq comp-deferred-compilation nil)
    (setq native-comp-deferred-compilation nil)
    )
;; In noninteractive sessions, prioritize non-byte-compiled source files to
;; prevent the use of stale byte-code. Otherwise, it saves us a little IO time
;; to skip the mtime checks on every *.elc file.
(setq load-prefer-newer noninteractive)

;; Remove annoying error bells
(setq ring-bell-function 'ignore)

;; All The Icons
(use-package all-the-icons)

;; Dashboard
;; Configuration
    (use-package dashboard   
    :init ;; tweak dashboard config before loading it
    (setq dashboard-set-heading-icons t)
    (setq dashboard-set-file-icons t)
    (setq dashboard-banner-logo-title "Vmacs, Versatility of Vim + Extensibility of Emacs")
    (setq dashboard-startup-banner 'logo) ;; use standard emacs logo as banner
    (setq dashboard-startup-banner "~/.emacs.d/vimacs.png")  ;; use custom image as banner
    (setq dashboard-center-content t) ;; set to 't' for centered content
    (setq dashboard-items
	  '(
        (agenda . 9)
	    )
    )

 ;; Dashboard in emacsclient
(setq initial-buffer-choice (lambda () (get-buffer "*dashboard*")))


;; Delete Selection mode
(delete-selection-mode t)

;; Fonts
(set-face-attribute 'default nil
  :font "Fira Code"
  :height 110
  :weight 'medium)
(set-face-attribute 'variable-pitch nil
  :font "Fira Code"
  :height 120
  :weight 'medium)
(set-face-attribute 'fixed-pitch nil
  :font "Fira Code"
  :height 110
  :weight 'medium)
;; Makes commented text and keywords italics.
;; This is working in emacsclient but not emacs.
;; Your font must have an italic face available.
(set-face-attribute 'font-lock-comment-face nil
  :slant 'italic)
(set-face-attribute 'font-lock-keyword-face nil
  :slant 'italic)

;; Needed if using emacsclient. Otherwise, your fonts will be smaller than expected.
(add-to-list 'default-frame-alist '(font . "Fira Code"))
;; changes certain keywords to symbols, such as lamda!
(setq global-prettify-symbols-mode t)

;; Zooming in and out
;; zoom in/out like everywhere else.
(global-set-key (kbd "C-=") 'text-scale-increase)
(global-set-key (kbd "C--") 'text-scale-decrease)

;; GUI Tweaks
;; Disable Menubar, toolbars
(menu-bar-mode -1)
(tool-bar-mode -1)

;; Display line numbers and truncated lines
(global-display-line-numbers-mode)
(global-visual-line-mode t)
;; Change line number to be similar to vim relative numbers
(setq display-line-numbers-type 'relative)

;; Scrolling
(setq scroll-conservatively 300) ;; value greater than 100 gets rid of half page jumping
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1))) ;; how many lines at a time
(setq scroll-step 1) ;;keyboard scroll 1 line at a time
(setq mouse-wheel-progressive-speed nil) ;; accelerate scrolling
(setq scroll-margin 9) ;; keeps the cursor from hitting the top & bottom edges of the window
 
;; Theme
(use-package doom-themes)
(setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t) ; if nil, italics is universally disabled
(load-theme 'doom-solarized-light t)

;; Use airline modeline (similar to beloved vim-airline)
(require 'airline-themes)
(load-theme 'airline-base16_solarized_light t) ;; Solarized theme to match main theme

;; Transparent background
(set-frame-parameter (selected-frame) 'alpha '(93 93))
(add-to-list 'default-frame-alist '(alpha 93 93))
;; modify value for opacity (1 = no transparency)

;; Dired
(use-package all-the-icons-dired) ;; Nice icons for files in dired buffer
(use-package peep-dired) ;; Quickly preview images in dired
;; Get file icons in dired
(add-hook 'dired-mode-hook 'all-the-icons-dired-mode)

;; Magit, A Magical Git Tool
(use-package magit)
  
;; Evil Mode >:)
;; Vim's powerful keybindings 
(use-package evil
  :init      ;; tweak evil's configuration before loading it
  (setq evil-want-integration t) ;; This is optional since it's already set to t by default.
  (setq evil-want-keybinding nil)
  (setq evil-vsplit-window-right t)
  (setq evil-split-window-below t)
  (setq evil-want-C-u-scroll t)
  (setq evil-want-C-d-scroll t)
  (evil-mode))
(use-package evil-collection
  :after evil
  :config
  (setq evil-collection-mode-list '(dashboard dired ibuffer))
  (evil-collection-init))

;; General Keybindings
(use-package general
  :config
  (general-evil-setup t))

;; Buffer Keybindings
(nvmap :prefix "SPC"
       "b b"   '(ibuffer :which-key "Ibuffer")
       "b c"   '(clone-indirect-buffer-other-window :which-key "Clone indirect buffer other window")
       "b k"   '(kill-current-buffer :which-key "Kill current buffer")
       "b n"   '(next-buffer :which-key "Next buffer")
       "b p"   '(previous-buffer :which-key "Previous buffer")
       "b B"   '(ibuffer-list-buffers :which-key "Ibuffer list buffers")
       "b K"   '(kill-buffer :which-key "Kill buffer"))
;; Window Movement Keybindings
(winner-mode 1)
(nvmap :prefix "SPC"
       ;; Window splits
       "w c"   '(evil-window-delete :which-key "Close window")
       "w n"   '(evil-window-new :which-key "New window")
       "w s"   '(evil-window-split :which-key "Horizontal split window")
       "w v"   '(evil-window-vsplit :which-key "Vertical split window")
      ;; Window motions
       "w h"   '(evil-window-left :which-key "Window left")
       "w j"   '(evil-window-down :which-key "Window down")
       "w k"   '(evil-window-up :which-key "Window up")
       "w l"   '(evil-window-right :which-key "Window right")
       "w w"   '(evil-window-next :which-key "Goto next window")
       ;; winner mode
       "w <left>"  '(winner-undo :which-key "Winner undo")
       "w <right>" '(winner-redo :which-key "Winner redo"))
;; File Finding Keybindings
(nvmap :states '(normal visual) :keymaps 'override :prefix "SPC"
       "."     '(find-file :which-key "Find file")
       "f f"   '(find-file :which-key "Find file")
       "f s"   '(save-buffer :which-key "Save file")
       "f u"   '(sudo-edit-find-file :which-key "Sudo find file")
       "f y"   '(dt/show-and-copy-buffer-path :which-key "Yank file path")
       "f C"   '(copy-file :which-key "Copy file")
       "f D"   '(delete-file :which-key "Delete file")
       "f R"   '(rename-file :which-key "Rename file")
       "f S"   '(write-file :which-key "Save file as...")
       "f U"   '(sudo-edit :which-key "Sudo edit file"))

;; Org Mode
;; Initial definitions
            (add-to-list 'load-path "~/org-mode/lisp/")
            (require 'org)
            (define-key global-map "\C-cl" 'org-store-link)
            (define-key global-map "\C-ca" 'org-agenda)
            ;;(setq org-log-done)
            (add-hook 'org-mode-hook 'org-indent-mode)
               (setq org-directory "~/org/"
                     org-agenda-files '("~/org/")
                     ;;org-default-notes-file (expand-file-name "notes.org" org-directory)
                     org-ellipsis " ⤵ "
                     org-log-done 'time
                     ;;org-journal-dir "~/org/journal/"
                     ;;org-journal-date-format "%B %d, %Y (%A) "
                     ;;org-journal-file-format "%Y-%m-%d.org"
                     org-hide-emphasis-markers t)
                ;; Set Agenda Files
                (setq org-src-preserve-indentation nil
                      org-edit-src-content-indentation 0)

;; Enabling Org Bullets, nicer bullet points
(use-package org-bullets)
(add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))

;; Org ToDo Keys
 (setq org-todo-keywords        ; This overwrites the default Doom org-todo-keywords
         '((sequence
            "IN-PROGRESS(i)"    ; A task that is underway
            "TODO(t)"           ; A task that is ready to be tackled
            "ASSIGNMENT (a)"    ; Assignment (work or school)
            "SCHEDULE (s)"      ; Item to be scheduled
            "PROJ(p)"           ; A project that contains other tasks
            "DEVELOP (v)"       ; Development plans/activities
            "WAITING(w)"        ; This task will be done later
            "BLOCKED(x)"        ; Something is holding up this task
            "|"                 ; The pipe necessary to separate "active" states and "inactive" states
            "DONE(d)"           ; Task has been completed
	        "ON-HOLD (h)"       ; Item on hold
            "CANCELLED(c)" )))  ; Task has been cancelled

;; Source Code Block Tag Expansion
(use-package org-tempo
  :ensure nil) ;; tell use-package not to try to install org-tempo since it's already there.

;; Source Code Block Syntax Highlighting
(setq org-src-fontify-natively t
    org-src-tab-acts-natively t
    org-confirm-babel-evaluate nil
    org-edit-src-content-indentation 0)

;; Auto-TableofContents(TOC)
(use-package toc-org
  :commands toc-org-enable
  :init (add-hook 'org-mode-hook 'toc-org-enable))

;; ESS + ORG
   (setq make-backup-files nil)
   (setq org-src-tab-acts-natively t)
   (setq org-src-fontify-natively t)
   (require 'org-tempo)
   (org-babel-do-load-languages
   'org-babel-load-languages
   '((R . t)
     (python . t)
     (ipython .t)
     )
   )
(require 'cl-lib)
(require 'ob-ipython)

(setq ess-smart-S-assign-key ";")
;; Remove underscore to <- that happens on some systems in R
;;(ess-toggle-S-assign nil)
;;(ess-toggle-S-assign nil)
;;(ess-toggle-underscore nil)

;; Company Mode
;; Autocompletion mode
(add-hook 'after-init-hook 'global-company-mode)

;; Runtime Performance
;; Make gc pauses faster by decreasing the threshold.
(setq gc-cons-threshold (* 2 1000 1000))

;; Language Support
;;(use-package markdown-mode)

;; Which-key
(use-package which-key
  :init
  (setq which-key-side-window-location 'bottom
        which-key-sort-order #'which-key-key-order-alpha
        which-key-sort-uppercase-first nil
        which-key-add-column-padding 1
        which-key-max-display-columns nil
        which-key-min-display-lines 6
        which-key-side-window-slot -10
        which-key-side-window-max-height 0.25
        which-key-idle-delay 0.8
        which-key-max-description-length 25
        which-key-allow-imprecise-window-fit t
        which-key-separator " → " ))
(which-key-mode)

;; Eshell
(nvmap :prefix "SPC"
       "e h"   '(counsel-esh-history :which-key "Eshell history")
       "e s"   '(eshell :which-key "Eshell"))
(use-package eshell-syntax-highlighting
  :after esh-mode
  :config
  (eshell-syntax-highlighting-global-mode +1))
(setq eshell-rc-script (concat user-emacs-directory "eshell/profile")
      eshell-aliases-file (concat user-emacs-directory "eshell/aliases")
      eshell-history-size 5000
      eshell-buffer-maximum-lines 5000
      eshell-hist-ignoredups t
      eshell-scroll-to-bottom-on-input t
      eshell-destroy-buffer-when-process-dies t
      eshell-visual-commands'("bash" "fish" "htop" "ssh" "top" "zsh"))

;; Ranger Configuration
(nvmap :prefix "SPC"
  "r r" '(ranger :which-key "Load Ranger")
 )
(setq ranger-show-hidden t)
(setq ranger-preview-file t)
(setq ranger-dont-show-binary t)
(setq ranger-show-literal nil)
