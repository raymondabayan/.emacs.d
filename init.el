;; Init File for Emacs

;;Description: Some Plugins may need to be installed by M-x package-install. Should be ;;applicable to text editing, file navigation (Vim-like), and scripting (python, R, matlab). ;;Also has a few GUI modifications to look pretty.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Package repos elpy and melpa
(require 'package)
(add-to-list 'package-archives
             '("elpy" . "http://jorgenschaefer.github.io/packages/")
	     )
(add-to-list 'package-archives
	     '("melpa" . "https://melpa.org/packages/")
	     )
;(package-refresh-contents)
;; Uncomment this line if you want emacs to do this automatically on startup (will slow down start up time and will show an error if not connected to the internet
(package-initialize)

;; Use-package
(unless (package-installed-p 'use-package)
  (package-install 'use-package))
(setq use-package-always-ensure t)
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Startup Performance
;; Garbage collection
(use-package gcmh
  :config
  (gcmh-mode 1)
  )
;; Setting garbage collection threshold
(setq gc-cons-threshold 402653184
      gc-cons-percentage 0.6)

;; Profile emacs startup
(add-hook 'emacs-startup-hook
          (lambda ()
            (message "*** Emacs loaded in %s with %d garbage collections."
                     (format "%.2f seconds"
                             (float-time
                              (time-subtract after-init-time before-init-time)
			      )
			     )
                     gcs-done)
	    )
	  )
(setq comp-async-report-warnings-errors nil) ;; Silence compiler warnings as they can be pretty disruptive
(setq ring-bell-function 'ignore) ;; Remove annoying error bells

;; Native Compil
(if (boundp 'comp-deferred-compilation)
    (setq comp-deferred-compilation nil)
  (setq native-comp-deferred-compilation nil)
  )
;; In noninteractive sessions, prioritize non-byte-compiled source files to
;; prevent the use of stale byte-code. Otherwise, it saves us a little IO time
;; to skip the mtime checks on every *.elc file.
(setq load-prefer-newer noninteractive)

;; package to collect recent files to be reopened quicker later
(use-package recentf
  :config
  (recentf-mode)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Startup Screen Options
;; Window Sizing on startup
(if (display-graphic-p)
   (progn
     (setq initial-frame-alist
	    '(
	      (tool-bar-lines . 0)
	      (width . 200) ; chars
	      (height . 63) ; lines
	      (left . 25)
	      (top . 45)
	      )
	    )
     (setq default-frame-alist
	    '(
	      (tool-bar-lines . 0)
	      (width . 200)
	      (height . 63)
	      (left . 25)
	      (top . 45)
	      )
	    )
     )
 (progn
   (setq initial-frame-alist '( (tool-bar-lines . 0)
				 )
	  )
   (setq default-frame-alist '( (tool-bar-lines . 0)
				 )
	  )
   )
 )
;; (setq inhibit-startup-screen t) ;; Uncomment for a blank startup screen
(use-package all-the-icons) ;; Get a bunch of nice icons to display
;; Dashboard
(use-package dashboard   
  :init 
  (setq dashboard-set-heading-icons t) ;; Enable icons for headings displayed in dashboard
  (setq dashboard-set-file-icons t) ;; Enable icons for files displayed in dashboard
  ;;(setq dashboard-startup-banner 'logo) ;; Uncomment to use standard emacs logo as banner
  (setq dashboard-startup-banner "~/.emacs.d/vimacs.png")  ;; use custom image as banner
  (setq dashboard-banner-logo-title "The evil choose both.") ;; custom text displayed under startup banner
  (setq dashboard-center-content t) ;; t ensures content is displayed in center 
  (setq dashboard-items '(
			  (recents . 7) ;; from recent f (# is # of files shown)
                          (agenda . 7) ;; from org-agenda variable
			  )
	)
  :config
  (dashboard-setup-startup-hook) 
  (dashboard-modify-heading-icons '(
				    (recents . "file-text") ;; Sets format for recents heading display
				    )
				  )
  )
;; Dashboard in emacsclient
(setq initial-buffer-choice (lambda ()
			      (get-buffer "*dashboard*")
			      )
      )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Fonts
(set-face-attribute 'default nil
  :font "Hack Nerd Font Mono"
  :height 135
  :weight 'medium)
(set-face-attribute 'variable-pitch nil
  :font "Hack Nerd Font Mono"
  :height 135
  :weight 'medium)
(set-face-attribute 'fixed-pitch nil
  :font "Hack Nerd Font Mono"
  :height 135
  :weight 'medium)
;; Makes commented text and keywords italics.
;; This is working in emacsclient but not emacs.
;; Your font must have an italic face available.
(set-face-attribute 'font-lock-comment-face nil
  :slant 'italic)
(set-face-attribute 'font-lock-keyword-face nil
  :slant 'italic)
;; Needed if using emacsclient. Otherwise, your fonts will be smaller than expected.
;;(add-to-list 'default-frame-alist '(font . "Hack Nerd Font Mono"))
; changes certain keywords to symbols, such as lamda!
(setq global-prettify-symbols-mode t)

;; Uncomment the following line if line spacing needs adjusting.
(setq-default line-spacing 0.13)

;; Zooming in and out for presentations
(global-set-key (kbd "C-=") 'text-scale-increase) ;; zoom in
(global-set-key (kbd "C--") 'text-scale-decrease) ;; zoom out

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; GUI Tweaks
(menu-bar-mode -1) ;; -1 removes menu bar
(tool-bar-mode -1) ;; -1 removes tool bar
;(scroll-bar-mode -1) ;; Seems to break on non-windows when uncommented 
(global-display-line-numbers-mode) ;; enables line numbers in all buffers
(global-visual-line-mode t) ;; t shows line numbers in all buffers
(setq display-line-numbers-type 'relative) ;; Relative line numbers like vim
(delete-selection-mode t) ;; Delete Selection mode
(setq scroll-step 1) ;; set scrolling span
(setq scroll-margin 10) ;; set scrolling margin from top and bottom (like vim's 'scrolloff')
(setq scroll-conservatively 300) ;; value greater than 100 gets rid of half page jumping
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1))) ;; how many lines at a time
(setq mouse-wheel-progressive-speed nil) ;; accelerate scrolling
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Theme
;; "Bugs are drawn to light"
(set-frame-parameter (selected-frame) 'alpha '(96 96)) ;; Transparent background for frame
(add-to-list 'default-frame-alist '(alpha 96 96)) ;; Transparent background by default
(use-package doom-themes
  :ensure t
  :config
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t) ; if nil, italics is universally disabled
  (load-theme 'doom-gruvbox t)

  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config)
  ;; Enable custom neotree theme (all-the-icons must be installed!)
  (doom-themes-neotree-config)
  ;; or for treemacs users
  (setq doom-themes-treemacs-theme "doom-gruvbox") ; use "doom-colors" for less minimal icon theme
  (doom-themes-treemacs-config)
  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config))

;;(load-theme 'leuven t) ;; light, high contrast theme with good org mode support
;; Use cursor color and type to indicate some modes (read-only, overwrite
;; and normal insert modes).
;; (defun leuven--set-cursor-according-to-mode ()
;;   "Change cursor color according to some minor modes."
;;   (let (
;; 	(color (cond (buffer-read-only "MediumSpringGreen")
;;                      (overwrite-mode   "PaleVioletRed1")
;;                      (t                "MediumOrchid2")
;; 		     )
;; 	       ) ; #21BDFF is less visible.
;;         (type (if (null overwrite-mode)
;;                 'box)
;; 	      )
;; 	)
;;     (set-cursor-color color)
;;     (setq cursor-type type)
;;     )
;;   )
;; (add-hook 'post-command-hook #'leuven--set-cursor-according-to-mode)
(setq-default cursor-type 'box) ;; Cursor to use.
(setq blink-cursor-blinks 0) ;; Cursor blinks forever.
;; (require 'airline-themes) ;; status bar like vim-airline's
;; (load-theme 'airline-base16_gruvbox_dark_hard t) ;; light, high contrast theme for airline

(use-package doom-modeline)
(doom-modeline-mode 1)
(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Dired
;;(use-package dired-single)
(use-package dired
  :ensure nil
  :commands (dired dired-jump)
  :bind (
	 ("C-x C-j" . dired-jump) ;; jumps to dired file location for current file in buffer
	 )
  :config
  (evil-collection-define-key 'normal 'dired-mode-map
    "h" 'dired-up-directory ;; Similar to rangers keybindings
    "l" 'dired-find-file)
  )
(use-package all-the-icons-dired
  :hook (dired-mode . all-the-icons-dired-mode) ;; pretty icons for dired
  )
;; Hide / show dotfiles
(use-package dired-hide-dotfiles
  :hook (dired-mode . dired-hide-dotfiles-mode)
  :config
  (evil-collection-define-key 'normal 'dired-mode-map
			       "H" 'dired-hide-dotfiles-mode)
 ) 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Keybindings
;; Evil Mode
(use-package evil ;;Extensible VI Layer for emacs
  :init      
  (setq evil-want-C-u-scroll t) ;; CTRL up scroll like vim
  (setq evil-want-C-d-scroll t) ;; CTRL down scroll like vim
  (setq evil-want-integration t) ;; This is optional since it's already set to t by default.
  (setq evil-want-keybinding nil)
  (setq evil-vsplit-window-right t) ;; Window Splitting like vim
  (setq evil-split-window-below t)
  (evil-mode)
  )
(evil-global-set-key 'motion "j" 'evil-next-visual-line)
(evil-global-set-key 'motion "k" 'evil-previous-visual-line)

(use-package evil-collection
  :after evil
  :config
  (setq evil-collection-mode-list '(dired dashboard ibuffer)) 
  (evil-collection-init)
  )

;; General Keybindings, helps let us set user-specific keymaps
(use-package general
  :config
  (general-evil-setup t)
  )

(nvmap :prefix "SPC" ;; Neotree bindings
  "n n" '(neotree-toggle :which-key "Toggle Neotree")
  )

(nvmap :prefix "SPC" ;; Buffer Keybindings
  "b b"   '(ibuffer :which-key "Ibuffer")
  "b c"   '(clone-indirect-buffer-other-window :which-key "Clone indirect buffer other window")
  "b k"   '(kill-current-buffer :which-key "Kill current buffer")
  "b n"   '(next-buffer :which-key "Next buffer")
  "b p"   '(previous-buffer :which-key "Previous buffer")
  "b B"   '(ibuffer-list-buffers :which-key "Ibuffer list buffers")
  "b K"   '(kill-buffer :which-key "Kill buffer")
  )

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
  "w <right>" '(winner-redo :which-key "Winner redo")
  )
;; File Finding Keybindings
(nvmap :states '(normal visual) :keymaps 'override :prefix "SPC"
  "."     '(find-file :which-key "Find file")
  "f f"   '(find-file :which-key "Find file")
  "f s"   '(save-buffer :which-key "Save file")
  "f u"   '(sudo-edit-find-file :which-key "Sudo find file")
  "f C"   '(copy-file :which-key "Copy file")
  "f D"   '(delete-file :which-key "Delete file")
  "f R"   '(rename-file :which-key "Rename file")
  "f S"   '(write-file :which-key "Save file as...")
  "f U"   '(sudo-edit :which-key "Sudo edit file")
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Ranger File Explorer Configuration
(nvmap :prefix "SPC"
  "r r" '(ranger :which-key "Load Ranger")
 )
(setq ranger-show-hidden t)
(setq ranger-preview-file t)
(setq ranger-dont-show-binary t)
(setq ranger-show-literal nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Org Mode
;; Initial definitions
(add-to-list 'load-path "~/org-mode/lisp/")
(require 'org)
(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)
(evil-define-key 'normal org-mode-map (kbd "<tab>") #'org-cycle)
;;(setq org-log-done)
(add-hook 'org-mode-hook 'org-indent-mode)
(setq org-directory "~/org-roam")
(setq org-agenda-files (list org-directory))
(setq org-ellipsis "⤵"
      org-log-done 'time
      ;;org-journal-dir "~/org/journal/"
      ;;org-journal-date-format "%B %d, %Y (%A) "
      ;;org-journal-file-format "%Y-%m-%d.org"
      org-hide-emphasis-markers t)
      
;; Set Agenda Files
(setq org-src-preserve-indentation nil
      org-edit-src-content-indentation 0)
(setq org-return-follows-link t)

;; Org-roam
(use-package org-roam
  :ensure t
  :init
  (setq org-roam-v2-ack t)
  :custom
  (org-roam-directory "~/org-roam")
  (org-roam-completion-everywhere t)
  :bind (
	 ("C-c n l" . org-roam-buffer-toggle)
	 ("C-c n f" . org-roam-node-find)
	 ("C-c n i" . org-roam-node-insert)
	 :map org-mode-map
	 ("C-M-i"   . completion-at-point)
	 )
  :config
  (org-roam-setup)
  )
;; Enabling Org Bullets
(use-package org-bullets)
(add-hook 'org-mode-hook (lambda ()
			   (org-bullets-mode 1)
			   )
	  )
;; Fontify the whole line for headings (with a background color).
(setq org-fontify-whole-heading-line t)

;; Org ToDo Keys
(setq org-todo-keywords        ; This overwrites the default Doom org-todo-keywords
      '(
	(sequence
         "IN-PROGRESS(i)"    ; A task that is underway
         "TODO(t)"           ; A task that is ready to be tackled
         "FOLLOW UP(f)"         ; Things to follow up on
         "IDEA(i)"           ; An idea that would be interesting to investigate
         "WAITING(w)"        ; This task will be done later
	 "HANDED OFF(h)"         ; This task was handed off to the appropriate person
         "|"                 ; The pipe necessary to separate "active" states and "inactive" states
         "DONE(d)"           ; Task has been completed
         "BLOCKED(x)"        ; Something is holding up this task
         "CANCELLED(c)"      ; Task has been cancelled
	 )    
	)
      ) 

;; Source Code Block Tag Expansion
(use-package org-tempo
  :ensure nil) ;; tell use-package not to try to install org-tempo since it's already there.
(setq tempo-interactive t)

;; Source Code Block Syntax Highlighting
(setq org-src-fontify-natively t
      org-src-tab-acts-natively t
      org-confirm-babel-evaluate nil
      org-edit-src-content-indentation 0)

;; Auto-TableofContents(TOC)
(use-package toc-org
  :commands toc-org-enable
  :init (add-hook 'org-mode-hook 'toc-org-enable)
  )

;; Setup for LaTex exporting
(require 'ox-latex)
(unless (boundp 'org-latex-classes)
  (setq org-latex-classes nil))
(add-to-list 'org-latex-classes
             '("article"
               "\\documentclass{article}"
               ("\\section{%s}" . "\\section*{%s}")))

(if (eq window-system 'mac)
   (add-to-list 'exec-path "/opt/homebrew/Cellar/texlive/58837_1/bin/tex")
)

;; ESS (Emacs Speaks Statistics) + ORG
(setq make-backup-files nil)
(setq org-src-tab-acts-natively t)
(setq org-src-fontify-natively t)
(require 'org-tempo)
(org-babel-do-load-languages
 'org-babel-load-languages
 '(
   (R . t)
   (python . t)
   )
 )
(require 'cl-lib)
(setq ess-smart-S-assign-key ";")
;;(ess-toggle-S-assign nil)
;;(ess-toggle-S-assign nil)
;;(ess-toggle-underscore nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; LSP Mode
(defun efs/lsp-mode-setup () 
  (setq lsp-headerline-breadcrumb-segments '(path-up-to-project file symbols))
  (lsp-headerline-breadcrumb-mode)
  )
(use-package lsp-mode
             :commands (lsp lsp-deferred)
             :hook (lsp-mode . efs/lsp-mode-setup)
             :init
             (setq lsp-keymap-prefix "C-c l")
             :config
             (lsp-enable-which-key-integration t)
             )
;; lsp-treemacs
;; Provides tree-view for different aspects of code (symbols, references, or diagnostic warnings
(use-package lsp-treemacs
             :after lsp
             )
;; Debugging with dap-mode
(use-package dap-mode
             ;; Uncomment below section if you want all UI panes to be hidden by default
             ;;:custom
             ;;(lsp-enable-dap-auto-configure nil)
             ;;:config
             ;;(dap-ui-mode 1)
             :config
             ;; Setup node debugging
             (require 'dap-node)
             (dap-node-setup) ;; Automatically installs Node debug adapter if needed
             ;; Binding 'C-c l d' to 'dap-hydra' for easy access
             (general-define-key
               :keymaps 'lsp-mode-map
               :prefix lsp-keymap-prefix
               "d" '(dap-hydra t :wk "debugger")
               )
             )
;; Python Development
(use-package python-mode
  :ensure t
  :hook (python-mode . lsp-deferred)
  :custom
  (python-shell-interpreter "/opt/homebrew/bin/python3")
  (python-shell-completion-native-enable nil)
  (dap-python-debugger 'debugpy)
  :config
  (require 'dap-python)
  )
(use-package pyvenv
    :config
    (pyenv-mode 1)
    )
(use-package lsp-pyright
  :ensure t
  :hook (python-mode . (lambda ()
                          (require 'lsp-pyright)
                          (lsp-deferred)
			  )
		     )
  )  

;; Company Mode, Autocompletion 
(use-package company
             :after lsp-mode
             :hook (lsp-mode . company-mode)
             :bind (:map company-active-map
                         ("<tab>" . company-complete-selection)
                         )
                   (:map lsp-mode-map
                         ("<tab>" . company-indent-or-complete-common)
                         )
             :custom
             (company-minimum-prefix-length 1)
             (company-idle-delay 0.0)
             )
(use-package company-box
             :hook (company-mode . company-box-mode)
             )
             
(add-hook 'after-init-hook 'global-company-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Runtime Performance
;; Make gc pauses faster by decreasing the threshold.
(setq gc-cons-threshold (* 2 1000 1000))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
        which-key-separator " → "
	)
  )
(which-key-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Vterm, probably the closest terminal emulator to unix style
(nvmap :prefix "SPC"
  "v v"   '(vterm :which-key "Vterm")
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq yas-snippet-dirs
      '("~/.emacs.d/snippets"                 ;; personal snippets
        ))
(yas-global-mode 1) ;; or M-x yas-reload-all if you've started YASnippet already.
(add-hook 'yas-minor-mode-hook (lambda ()
				 (yas-activate-extra-mode 'fundamental-mode) ;; allows hooks to be shared across file types
				 )
	  )

;; Test Section

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(rainbow-delimiters powerline-evil yasnippet-snippets pyenv pyenv-mode-auto org-roam vterm neotree magit leuven-theme ranger eshell-syntax-highlighting toc-org which-key use-package peep-dired org-bullets general gcmh evil-collection ess doom-themes dashboard company clippy beacon all-the-icons-ibuffer all-the-icons-dired airline-themes))
 '(warning-suppress-types '(((python python-shell-completion-native-turn-on-maybe)))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
