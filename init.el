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

(use-package gcmh
  :config
  (gcmh-mode 1)
  )
;; Setting garbage collection threshold
(setq gc-cons-threshold 100000000
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
(setq gc-cons-threshold (* 2 1000 1000))

(use-package recentf
  :config
  (recentf-mode 1)
  (setq recentf-max-menu-items 9)
  (setq recentf-max-saved-items 9)
  (global-set-key "\C-x\ \C-r" 'recentf-open-files)
  )

(set-face-attribute 'default nil
  :font "Hack Nerd Font"
  :height 150
  :weight 'medium)
(set-face-attribute 'variable-pitch nil
  :font "Hack Nerd Font"
  :height 150
  :weight 'medium)
(set-face-attribute 'fixed-pitch nil
  :font "Hack Nerd Font"
  :height 150
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

(use-package all-the-icons) ;; Get a bunch of nice icons to display

(global-set-key (kbd "C-=") 'text-scale-increase) ;; zoom in
(global-set-key (kbd "C--") 'text-scale-decrease) ;; zoom out

(menu-bar-mode -1) ;; -1 removes menu bar
(tool-bar-mode -1) ;; -1 removes tool bar
(global-hl-line-mode 1) ;; adds a visual line bar like vim 
;(scroll-bar-mode -1) ;; Seems to break on non-windows when uncommented 
(global-display-line-numbers-mode) ;; enables line numbers in all buffers
(global-visual-line-mode t) ;; t shows line numbers in all buffers
;; (setq display-line-numbers-type 'relative) ;; Relative line numbers like vim
(delete-selection-mode t) ;; Delete Selection mode
(setq scroll-step 1) ;; set scrolling span
(setq scroll-margin 10) ;; set scrolling margin from top and bottom (like vim's 'scrolloff')
(setq scroll-conservatively 300) ;; value greater than 100 gets rid of half page jumping
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1))) ;; how many lines at a time
(setq mouse-wheel-progressive-speed nil) ;; accelerate scrolling
(setq inhibit-splash-screen t)
(setq inhibit-startup-message t)

(use-package dashboard   
  :init 
  (setq dashboard-set-heading-icons t) ;; Enable icons for headings displayed in dashboard
  (setq dashboard-set-file-icons t) ;; Enable icons for files displayed in dashboard
  ;; (setq dashboard-startup-banner 'official) ;; Uncomment to use standard emacs logo as banner
  ;; (setq dashboard-startup-banner "~/.emacs.d/gnu-genie.png")  ;; use custom image as banner
  ;; (setq dashboard-banner-logo-title "Go and Make Something GNU") ;; custom text displayed under startup banner
  (setq dashboard-startup-banner "~/.emacs.d/lanturn.png")  ;; use custom image as banner
  (setq dashboard-banner-logo-title "Illuminate The Great Depths.") ;; custom text displayed under startup banner
  ;; (setq dashboard-startup-banner "~/.emacs.d/vimacs.png")  ;; use custom image as banner
  ;; (setq dashboard-banner-logo-title "The evil choose both.") ;; custom text displayed under startup banner
  (setq dashboard-center-content t) ;; t ensures content is displayed in center
  (setq dashboard-items '(
                          (agenda . 5) ;; from org-agenda variable
			      (recents . 5) ;; from recent f (# is # of files shown)
			      (projects . 3) ;; projectile
			   ;; (bookmarks .3)
			  )
	)
  :config
  (dashboard-setup-startup-hook) 
  (dashboard-modify-heading-icons '(
				    (recents . "file-text") ;; Sets format for recents heading display
				    ;; (bookmarks . "book")
				    )
				  )
  )
;; Dashboard in emacsclient
;; (setq initial-buffer-choice (lambda ()
;; 			      (get-buffer "*dashboard*")
;; 			      )
;;       )
(setq dashboard-set-navigator t)
;; Format: "(icon title help action face prefix suffix)"
(setq dashboard-navigator-buttons
      `(;; line1
        ((,(all-the-icons-octicon "mark-github" :height 1.1 :v-adjust 0.0)
         "Homepage"
         "Browse homepage"
         (lambda (&rest _) (browse-url "https://github.com")))
        ("★" "Star" "Show stars" (lambda (&rest _) (show-stars)) warning)
        ("?" "" "?/h" #'show-help nil "<" ">"))
         ;; line 2
        ((,(all-the-icons-faicon "university" :height 1.1 :v-adjust 0.0)
          "Church of Emacs"
          "Browse Documentation"
          (lambda (&rest _) (browse-url "https://www.gnu.org/software/emacs/documentation.html")))
         ("⚑" nil "Show flags" (lambda (&rest _) (message "flag")) error))))
(setq initial-buffer-choice (lambda () (get-buffer-create "*dashboard*")))

;; (use-package modus-themes
 ;;   :ensure t
 ;;   :init
 ;;   (setq modus-themes-italic-constructs t
 ;;         modus-themes-bold-constructs t
 ;;         modus-themes-region '(accented)
 ;; 	    modus-themes-org-blocks '(nil)
 ;;         modus-themes-syntax '(alt-syntax green-strings yellow-comments) 
 ;; 	)

 ;; ;; Load the theme files before enabling a theme
 ;;   (modus-themes-load-themes)
 ;;   :config
 ;; ;; Load the theme of your choice:
 ;;   (modus-themes-load-operandi) ;; OR (modus-themes-load-vivendi)
 ;;   :bind ("<f5>" . modus-themes-toggle)
 ;;   )
 ;; (require 'modus-themes)
 ;; ;; Org-Agenda configuration for modus themes
 ;; (setq modus-themes-org-agenda
 ;;       '((header-block . (variable-pitch 1.5))
 ;;         (header-date . (grayscale workaholic bold-today 1.2))
 ;;         (event . (accented italic varied))
 ;;         (scheduled . uniform)
 ;;         (habit . traffic-light)))

;; Gruvbox theme
(use-package gruvbox-theme
:ensure t
:init
;; (load-theme 'gruvbox-light-soft t)
(load-theme 'gruvbox-dark-medium t)
;; (load-theme 'gruvbox-light-hard t)
)
 (use-package rainbow-delimiters
   :hook (prog-mode . rainbow-delimiters-mode))

(mood-line-mode)

(setq ring-bell-function
     (lambda ()
       (let ((orig-fg (face-foreground 'mode-line)))
         (set-face-foreground 'mode-line "#F2804F")
         (run-with-idle-timer 0.1 nil
                              (lambda (fg) (set-face-foreground 'mode-line fg))
                              orig-fg)))) ;; (doom-modeline-mode 1)

(if (display-graphic-p)
   (progn
     (setq initial-frame-alist
	    '(
	      (tool-bar-lines . 0)
	      (width . 177) ; chars
	      (height . 53) ; lines
	      (left . 25)
	      (top . 43)
	      )
	    )
     (setq default-frame-alist
	    '(
	      (tool-bar-lines . 0)
	      (width . 177)
	      (height . 53)
	      (left . 25)
	      (top . 43)
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
;; (set-frame-parameter (selected-frame) 'alpha '(96 96))  
;; (add-to-list 'default-frame-alist '(alpha 96 96))

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

;; Vim's tpope great plugins for surrounding ' & "
(use-package evil-surround
  :ensure t
  :config
  (global-evil-surround-mode 1))
;; Vim's tpope great plugins for commenting/uncommenting
(evil-commentary-mode)

;; General Keybindings, helps let us set user-specific keymaps
(use-package general
  :config
  (general-evil-setup t)
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

;; Save place in file that you were working on to come back to when reopening the file
(save-place-mode 1)

;; Revert Buffers when files are changed
(global-auto-revert-mode 1)

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
;; Revert Dired and other buffers
(setq global-auto-revert-non-file-buffers t)

(use-package projectile
  :diminish projectile-mode
  :config (projectile-mode)
  :custom ((projectile-completion-system 'ivy))
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :init
  ;; NOTE: Set this to the folder where you keep your Git repos!
  (when (file-directory-p "~/NIPT_Core_Trisomy_Data")
    (setq projectile-project-search-path '("~/NIPT_Core_Trisomy_Data")))
  (setq projectile-switch-project-action #'projectile-dired))

(use-package counsel-projectile
  :config (counsel-projectile-mode))

(defun efs/org-mode-setup ()
  (org-indent-mode)
  (variable-pitch-mode 1)
  (visual-line-mode 1))

(defun efs/org-font-setup ()
  ;; Replace list hyphen with dot
  (font-lock-add-keywords 'org-mode
                          '(
			    ("^ *\\([-]\\) "
                             (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•")
				       )
				)
			     )
			    )
			  )

;; Set faces for heading levels
(dolist (face '((org-level-1 . 1.2)
                (org-level-2 . 1.1)
                (org-level-3 . 1.05)
                (org-level-4 . 1.0)
                (org-level-5 . 1.1)
                (org-level-6 . 1.1)
                (org-level-7 . 1.1)
                (org-level-8 . 1.1)))
  (set-face-attribute (car face) nil :font "Hack Nerd Font" :weight 'regular :height (cdr face)))

;; Ensure that anything that should be fixed-pitch in Org files appears that way
(set-face-attribute 'org-block nil :foreground nil :inherit 'fixed-pitch)
(set-face-attribute 'org-code nil   :inherit '(shadow fixed-pitch))
(set-face-attribute 'org-table nil   :inherit '(shadow fixed-pitch))
(set-face-attribute 'org-verbatim nil :inherit '(shadow fixed-pitch))
(set-face-attribute 'org-special-keyword nil :inherit '(font-lock-comment-face fixed-pitch))
(set-face-attribute 'org-meta-line nil :inherit '(font-lock-comment-face fixed-pitch))
(set-face-attribute 'org-checkbox nil :inherit 'fixed-pitch))
(setq org-startup-folded t)

(add-to-list 'load-path "~/org-mode/lisp/")
(use-package org
  :hook
  (org-mode . efs/org-mode-setup)
  :config
  (setq org-ellipsis " ▾") ;; Try this one if the one below didn't work
  ;; (setq org-ellipsis " ⤵")
  (setq org-agenda-start-with-log-mode t)
  (setq org-log-done 'note)
  (setq org-log-into-drawer t)

(add-hook 'org-mode-hook                                                                      
          (lambda ()                                                                          
        (define-key evil-normal-state-map (kbd "TAB") 'org-cycle))) 
  ;; (setq org-directory "~/org-roam")
  ;; (setq org-agenda-files (list org-directory))
  (setq org-agenda-files
	'("~/Dropbox/org-roam/20220502102008-action_items.org"
	  "~/Dropbox/org-roam/20220517193319-habits.org"
	  "~/Dropbox/org-roam/20220517193404-important_dates.org"
	  "~/Dropbox/org-roam/20220517194835-meetings.org")
	)

  (require 'org-habit)
  (add-to-list 'org-modules 'org-habit)
  (setq org-habit-graph-column 60)

  (setq org-todo-keywords
	'(
	  (sequence "TODO(t)" "NEXT(n)" "|" "DONE(d!)")
	  (sequence "BACKLOG(b)" "PLAN(p)" "READY(r)" "ACTIVE(a)" "REVIEW(v)" "WAIT(w@/!)" "HOLD(h)" "|" "COMPLETED(c)" "CANC(k@)")
	  )
	)

  (setq org-refile-targets
	'(
	  ("~/Dropbox/org-roam/20220517193229-archive.org" :maxlevel . 1)
	  ("~/Dropbox/org-roam/20220502102008-action_items.org" :maxlevel . 1)
	  )
	)

  ;; Save Org buffers after refiling!
  (advice-add 'org-refile :after 'org-save-all-org-buffers)

  (setq org-tag-alist
    '((:startgroup)
       ; Put mutually exclusive tags here
       (:endgroup)
       ("@errand" . ?E)
       ("@home" . ?H)
       ("@school" . ?S)
       ("@work" . ?W)
       ("agenda" . ?a)
       ("planning" . ?p)
       ("track" . ?t)
       ("investigate" . ?I)
       ("note" . ?n)
       ("idea" . ?i)
       )
    )

  ;; Configure custom agenda views
  (setq org-agenda-custom-commands
	'(
	  ("d" "Dashboard"
	   (
	    (agenda "" (
			(org-deadline-warning-days 7)
			)
		    )
      (todo "REVIEW"
            (
	     (org-agenda-overriding-header "Items to Review")
	     )
	    )
      (todo "NEXT"
            (
	     (org-agenda-overriding-header "Next Tasks")
	     )
	    )
      (tags-todo "agenda/ACTIVE" (
				  (org-agenda-overriding-header "Active Projects")
				  )
		 )
      )
	   )

    ("n" "Next Tasks"
     (
      (todo "NEXT"
            (
	     (org-agenda-overriding-header "Next Tasks")
	     )
	    )
      )
     )

    ("W" "Work Tasks" tags-todo "+work-email")

    ;; Low-effort next actions
    ;; ("e" tags-todo "+TODO=\"NEXT\"+Effort<15&+Effort>0"
    ;;  ((org-agenda-overriding-header "Low Effort Tasks")
    ;;   (org-agenda-max-todos 20)
    ;;   (org-agenda-files org-agenda-files)))

    ("w" "Workflow Status"
     (
      (todo "WAIT"
            (
	     (org-agenda-overriding-header "Waiting on External")
             (org-agenda-files org-agenda-files)
	     )
	    )
      (todo "REVIEW"
            (
	     (org-agenda-overriding-header "In Review")
             (org-agenda-files org-agenda-files)
	     )
	    )
      (todo "PLAN"
            (
	     (org-agenda-overriding-header "In Planning")
             (org-agenda-todo-list-sublevels nil)
             (org-agenda-files org-agenda-files)
	     )
	    )
      (todo "BACKLOG"
            (
	     (org-agenda-overriding-header "Project Backlog")
             (org-agenda-todo-list-sublevels nil)
             (org-agenda-files org-agenda-files)
	     )
	    )
      (todo "READY"
            (
	     (org-agenda-overriding-header "Ready for Move")
             (org-agenda-files org-agenda-files)
	     )
	    )
      (todo "ACTIVE"
            (
	     (org-agenda-overriding-header "Active Projects/Tasks")
             (org-agenda-files org-agenda-files)
	     )
	    )
      (todo "COMPLETED"
            (
	     (org-agenda-overriding-header "Completed Projects/Tasks")
             (org-agenda-files org-agenda-files)
	     )
	    )
      (todo "CANC"
            (
	     (org-agenda-overriding-header "Cancelled Projects/Tasks")
             (org-agenda-files org-agenda-files)
	     )
	    )
      )
     )
    )
	)

  (setq org-capture-templates
	`(
	  ("t" "Tasks / Projects")
      ("tt" "Task" entry (file+olp "~/Dropbox/org-roam/20220502102008-action_items.org" "Inbox")
           "* TODO %?\n  %U\n  %a\n  %i" :empty-lines 1)

      ("j" "Journal Entries")
      ("jj" "Journal" entry
           (file+olp+datetree "~/Dropbox/org-roam/20220517193749-journal.org")
           "\n* %<%I:%M %p> - Journal :journal:\n\n%?\n\n"
           ;; ,(dw/read-file-as-string "~/Notes/Templates/Daily.org")
           :clock-in :clock-resume
           :empty-lines 1)
      ("jm" "Meeting" entry
           (file+olp+datetree "~/Dropbox/org-roam/20220517194835-meetings.org")
           "* %<%I:%M %p> - %a :meetings:\n\n%?\n\n"
           :clock-in :clock-resume
           :empty-lines 1)

      ("W" "Workflows")
      ("we" "Checking Email" entry (file+olp+datetree "~/Dropbox/org-roam/20220517193749-journal.org")
           "* Checking Email :email:\n\n%?" :clock-in :clock-resume :empty-lines 1)

      ;; ("m" "Metrics Capture")
      ;; ("mw" "Weight" table-line (file+headline "~/Projects/Code/emacs-from-scratch/OrgFiles/Metrics.org" "Weight")
      ;;  "| %U | %^{Weight} | %^{Notes} |" :kill-buffer t)
      )
	)

  (define-key global-map (kbd "C-c j")
    (lambda () (interactive) (org-capture nil "jj")
      )
    )

  (efs/org-font-setup))

(use-package org-bullets
  :after org
  :hook (org-mode . org-bullets-mode)
  :custom
  (org-bullets-bullet-list '("◉" "○" "●" "○" "●" "○" "●")))

(setq org-fontify-quote-and-verse-blocks t)
(setq org-fontify-whole-heading-line t)
;; (defun efs/org-mode-visual-fill ()
;;   (setq visual-fill-column-width 100
;;         visual-fill-column-center-text t)
;;   (visual-fill-column-mode 1))

;; (use-package visual-fill-column
;;   :hook (org-mode . efs/org-mode-visual-fill))

(require 'org)
(define-key global-map (kbd "C-c c") 'org-capture)
(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)
(evil-define-key 'normal org-mode-map (kbd "<tab>") #'org-cycle)
(setq org-src-preserve-indentation nil
      org-edit-src-content-indentation 0)
(setq org-return-follows-link t)
(use-package evil-org
  :ensure t
  :after org
  :hook (org-mode . (lambda () evil-org-mode))
  :config
  (require 'evil-org-agenda)
  (evil-org-agenda-set-keys))

(use-package org-roam
  :ensure t
  :init
  (setq org-roam-v2-ack t)
  :custom
  (org-roam-directory "~/Dropbox/org-roam")
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

(use-package org-tempo
  :ensure nil) ;; tell use-package not to try to install org-tempo since it's already there.
(setq tempo-interactive t)
(setq org-src-fontify-natively t
      org-src-tab-acts-natively t
      org-confirm-babel-evaluate nil
      org-edit-src-content-indentation 0)
;; ESS (Emacs Speaks Statistics) + ORG
(setq make-backup-files nil)
(require 'org-tempo)
(org-babel-do-load-languages
 'org-babel-load-languages
 '(
   (R . t)
   (python . t)
   (emacs-lisp . t)
   )
 )
(require 'cl-lib)
;; (setq ess-smart-S-assign-key ";")
;;(ess-toggle-S-assign nil)
;;(ess-toggle-S-assign nil)
;;(ess-toggle-underscore nil)
(use-package org-auto-tangle
:defer t
:hook (org-mode . org-auto-tangle-mode)
)

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
             (company-minimum-prefix-length 3)
             (company-idle-delay 0.0)
             )
(use-package company-box
             :hook (company-mode . company-box-mode)
             )
(add-hook 'after-init-hook 'global-company-mode)

(use-package ivy
  :diminish
  :bind (("C-s" . swiper)
         :map ivy-minibuffer-map
         ("TAB" . ivy-alt-done)
         ("C-l" . ivy-alt-done)
         ("C-j" . ivy-next-line)
         ("C-k" . ivy-previous-line)
         :map ivy-switch-buffer-map
         ("C-k" . ivy-previous-line)
         ("C-l" . ivy-done)
         ("C-d" . ivy-switch-buffer-kill)
         :map ivy-reverse-i-search-map
         ("C-k" . ivy-previous-line)
         ("C-d" . ivy-reverse-i-search-kill))
  :config
  (ivy-mode 1))
(use-package ivy-rich
  :init
  (ivy-rich-mode 1))

(use-package counsel
  :bind (("M-x" . counsel-M-x)
         ("C-x b" . counsel-ibuffer)
         ("C-x C-f" . counsel-find-file)
         :map minibuffer-local-map
         ("C-r" . 'counsel-minibuffer-history)))

(use-package which-key
  :init
  (setq which-key-side-window-location 'bottom
        which-key-sort-order #'which-key-key-order-alpha
        which-key-sort-uppercase-first nil
        which-key-add-column-padding 1
        which-key-max-display-columns nil
        which-key-min-display-lines 7
        which-key-side-window-slot -10
        which-key-side-window-max-height 0.25
        which-key-idle-delay 0.8
        which-key-max-description-length 25
        which-key-allow-imprecise-window-fit t
        which-key-separator " → "
	)
  )
(which-key-mode)

(nvmap :prefix "SPC"
  "v v"   '(vterm :which-key "Vterm")
  )

(use-package magit
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("234dbb732ef054b109a9e5ee5b499632c63cc24f7c2383a849815dacc1727cb6" "c4063322b5011829f7fdd7509979b5823e8eea2abf1fe5572ec4b7af1dd78519" "5784d048e5a985627520beb8a101561b502a191b52fa401139f4dd20acb07607" "fc48cc3bb3c90f7761adf65858921ba3aedba1b223755b5924398c666e78af8b" "c414f69a02b719fb9867b41915cb49c853489930be280ce81385ff7b327b4bf6" default))
 '(package-selected-packages
   '(zenburn-theme modus-themes xah-fly-keys org-modern xwwp-follow-link-ivy all-the-icons-ivy-rich visual-fill-column evil-magit evil-surround evil-commentary rainbow-delimiters powerline-evil yasnippet-snippets pyenv pyenv-mode-auto org-roam vterm neotree magit leuven-theme ranger eshell-syntax-highlighting toc-org which-key use-package peep-dired org-bullets general gcmh evil-collection ess doom-themes dashboard company clippy beacon all-the-icons-ibuffer all-the-icons-dired airline-themes))
 '(safe-local-variable-values
   '((org-blank-before-new-entry
      (heading . auto)
      (plain-list-item . auto))
     (org-list-description-max-indent . 5)
     (org-list-two-spaces-after-bullet-regexp)))
 '(warning-suppress-types '(((python python-shell-completion-native-turn-on-maybe)))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
