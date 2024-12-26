					;-*- mode: elisp -*-
;;; Packages handling
(require 'package) 
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("gnu" . "https://elpa.gnu.org/packages/")))
(package-initialize)

(unless (package-installed-p 'use-package) ; Ensure use-package is installed
  (unless package-archive-contents
    (package-refresh-contents))
  (package-install 'use-package))

(eval-when-compile 
  (require 'use-package))  ; Load and configure use-package
(setq use-package-always-ensure t)

;;; Global Builtin Variables/Functions
(setq display-buffer-alist
      '(("\\*Help\\*"
         (display-buffer-same-window))))

(setq inhibit-splash-screen t) ; 0/t == on/off)
(setq-default org-display-custom-times t)

;;; Global 3rd party package variables

;;; Global User Variables
(defvar bh/organization-task-id "default value" "a uuid for the default organization task")
(defvar bh/keep-clock-running nil
        "Non-nil means keep the Org clock running continuously by
        clocking into the next task.")

;;; Global User Functions
  (declare-function org-id-find "org-id" (id &optional markerp))
  (defun bh/clock-in-organization-task-as-default ()
   (interactive)
   (org-with-point-at (org-id-find bh/organization-task-id 'marker)
     (org-clock-in '(16))))

  (defun bh/punch-in (arg)
"Start continuous clocking and set the default task to the
      selected task.  If no task is selected set the Organization task
      as the default task."
        (interactive "p")
        (setq bh/keep-clock-running t)
        (if (equal major-mode 'org-agenda-mode)
            ;;
            ;; We're in the agenda
            ;;
            (let* ((marker (org-get-at-bol 'org-hd-marker))
                   (tags (org-with-point-at marker (org-get-tags))))
              (if (and (eq arg 4) tags)
                  (org-agenda-clock-in '(16))
                (bh/clock-in-organization-task-as-default)))
          ;;
          ;; We are not in the agenda
          ;;
          (save-restriction
            (widen)
            ; Find the tags on the current task
            (if (and (equal major-mode 'org-mode) (not (org-before-first-heading-p)) (eq arg 4))
                  (org-clock-in '(16))
                (bh/clock-in-organization-task-as-default)))))


;;; Global keybindings  
(global-set-key (kbd "C-c h") #'help-for-help)
(global-set-key (kbd "C-c r") #'isearch-backward)
(global-set-key (kbd "<f12>") #'org-agenda)
(global-set-key (kbd "<f5>") #'bh/org-todo)
(global-set-key (kbd "<S-f5>") #'bh/widen)
(global-set-key (kbd "<f7>") #'bh/set-truncate-lines)
(global-set-key (kbd "<f8>") #'org-cycle-agenda-files)
(global-set-key (kbd "<f9> <f9>") #'bh/show-org-agenda)
(global-set-key (kbd "<f9> b") #'bbdb)
(global-set-key (kbd "<f9> c") #'calendar)
(global-set-key (kbd "<f9> f") #'boxquote-insert-file)
(global-set-key (kbd "<f9> g") #'gnus)
(global-set-key (kbd "<f9> h") #'bh/hide-other)
(global-set-key (kbd "<f9> n") #'bh/toggle-next-task-display)
(global-set-key (kbd "<f9> I") #'bh/punch-in)
(global-set-key (kbd "<f9> O") #'bh/punch-out)
(global-set-key (kbd "<f9> o") #'bh/make-org-scratch)
(global-set-key (kbd "<f9> r") #'boxquote-region)
(global-set-key (kbd "<f9> s") #'bh/switch-to-scratch)
(global-set-key (kbd "<f9> t") #'bh/insert-inactive-timestamp)
(global-set-key (kbd "<f9> T") #'bh/toggle-insert-inactive-timestamp)
(global-set-key (kbd "<f9> v") #'visible-mode)
(global-set-key (kbd "<f9> l") #'org-toggle-link-display)
(global-set-key (kbd "<f9> SPC") #'bh/clock-in-last-task)
(global-set-key (kbd "C-<f9>") #'previous-buffer)
(global-set-key (kbd "M-<f9>") #'org-toggle-inline-images)
(global-set-key (kbd "C-x n r") #'narrow-to-region)
(global-set-key (kbd "C-<f10>") #'next-buffer)
(global-set-key (kbd "<f11>") #'org-clock-goto)
(global-set-key (kbd "C-<f11>") #'org-clock-in)
(global-set-key (kbd "C-s-<f12>") #'bh/save-then-publish)
(global-set-key (kbd "C-c c") #'org-capture)
(global-set-key (kbd "C-c RET") #'org-insert-heading)
(global-set-key (kbd "C-c ;") #'smex)
(global-set-key (kbd "M-X") #'smex-major-mode-commands)
(global-set-key (kbd "C-c g") 'hydra-magit/body)
(global-set-key (kbd "C-c r") 'my-repeatable-map)

;;; Native Minor Modes
(transient-mark-mode 1)
(global-visual-line-mode 1)
(global-hl-line-mode 1)
(use-package faces
  :ensure nil
  :defer t 
  :config 
  (set-face-background 'hl-line "magenta")
  (set-face-background 'magit-section-highlight "magenta")
  (set-face-foreground 'hl-line "color-193"))
  ;(set-face-attribute 'cursor nil :foreground "red" :background "black")
  ;(set-cursor-color "red"))

;;; 3rd party Minor Modes 
(use-package evil
  :ensure t
  :defer t
  :init
  (setq evil-want-keybinding nil)
  (evil-mode 1)
  :config
  (evil-define-key 'insert 'global (kbd "C-c n") 'evil-normal-state)) 
(use-package evil-commentary
  :ensure t
  :after evil
  :config
  (evil-commentary-mode 1))

(use-package evil-core
  :ensure nil
  :defer t
  :after evil
  :config
  (when (bound-and-true-p org-capture-mode)
    (evil-define-key '(normal insert visual) org-capture-mode-map "C-c C-c" 'org-capture-finalize)))
(use-package ido-completing-read+
  :ensure t
  :defer t
  :init
  (ido-mode t)
  :config
  (setq ido-everywhere 1
        ido-enable-flex-matching t
        ido-ubiquitous-mode 1
        ido-default-file-method 'selected-window
        ido-default-buffer-method 'selected-window
        ido-max-directory-size 100000))
(use-package no-littering
  :ensure t
  :defer t  
  )
(use-package magit
  :ensure t
  :defer t 
  )
(use-package flycheck
  :ensure t
  :defer t
  )
(use-package json-mode
  :ensure t
  :pin melpa
  :defer t
  )
(use-package evil-collection
  :ensure t
  :defer t
  :after (evil magit)
  :config
  (evil-collection-init 'magit))
(use-package keyfreq
  :ensure t
  :defer t
  :config
  (keyfreq-mode 1)
  (keyfreq-autosave-mode 1))
(use-package hydra
  :ensure t
  :defer t
  :config
  (defhydra hydra-magit (:color blue :hint nil)
    "
^Magit^
---------------------------------
_s_: Status   _c_: Commit
_p_: Push     _P_: Pull
_b_: Branch   _l_: Log
"
    ("s" magit-status)
    ("c" magit-commit)
    ("p" magit-push)
    ("P" magit-pull)
    ("b" magit-branch)
    ("l" magit-log)
    ("q" nil "quit")))
;; (use-package which-key
;;   :ensure t
;;   :config
;;   (setq which-key-idle-delay 0.5
;;         which-key-popup-type 'minibuffer
;;         which-key-sort-order 'which-key-key-order-alpha 
;;         which-key-max-description-length 32
;; 	which-key-show-prefix 'left)
	
;;   (which-key-add-major-mode-key-based-replacements 'magit-mode "C-x" "Magit Command")
;;   ;; (add-hook 'magit-mode-hook
;;   ;;           (lambda () (which-key-mode 1)))
;;   (which-key-mode 1))


;; (use-package mode-icons
;;   :ensure t
;;   :config
;;   (mode-icons-mode 1))
(use-package subr
  :ensure nil
  :config
  (define-prefix-command 'my-repeatable-map))
(use-package keymap
  :ensure nil
  :defer t
  :config
  (keymap-set my-repeatable-map (kbd "}") 'enlarge-window-horizontally))
 
;; End of third party packages --------------------------------


;;; Native Major Modes 
(use-package org
  :ensure nil
  :defer t
  :init 
  (setq org-time-stamp-custom-formats '("%m/%d/%Y %a " . "%m/%d/%Y %a %I:%M %p"))
  (org-clock-persistence-insinuate) ; resume clocking task when emacs is restarted
  (global-flycheck-mode 1)
  
  ;; Org Mode keybindings
  (keymap-unset org-mode-map "C-c ;")
  ;;--------------------------------------------
  :config  
    (setq org-use-sub-superscripts nil
        org-odd-levels-only nil 
        org-enable-priority-commands t ; priorities A-E
        org-default-priority ?E ; Tasks w/out a specific priority are lowest priority E.
        org-lowest-priority ?E
 
        org-emphasis-alist (quote (("*" bold "<b>" "</b>")
                                 ("/" italic "<i>" "</i>")
                                 ("_" underline "<span style=\"text-decoration:underline;\">" "</span>")
                                 ("=" org-code "<code>" "</code>" verbatim)
                                 ("~" org-verbatim "<code>" "</code>" verbatim))) ; removes the strike-through emphasis
 
        org-indirect-buffer-display 'current-window
        org-agenda-text-search-extra-files (quote (agenda-archives))
	org-log-into-drawer t
        org-enforce-todo-dependencies t
	org-insert-heading-respect-content nil
	org-reverse-note-order nil
	org-deadline-warning-days 30
	org-tags-match-list-sublevels t
        org-tag-alist (quote ((:startgroup) ; tags with fast selection keys
                            ("@errand" . ?e)
                            ("@office" . ?o)
                            ("@home" . ?H)
                            ("@farm" . ?f)
                            (:endgroup)
                            ("WAITING" . ?w)
                            ("HOLD" . ?h)
                            ("PERSONAL" . ?P)
                            ("WORK" . ?W)
                            ("FARM" . ?F)
                            ("ORG" . ?O)
                            ("NORANG" . ?N)
                            ("crypt" . ?E)
                            ("NOTE" . ?n)
                            ("CANCELLED" . ?c)
                            ("FLAGGED" . ??)))
        org-directory "~/git/org"
	org-default-notes-file "~/git/org/refile.org"
	org-startup-folded t
	org-read-date-prefer-future 'time))    
    ;; ---------- Org Tag function definitions ------------------------------
    ;; NEXT keywords are for tasks and not projects, any parent tasks marked NEXT automagically change change from NEXT to TODO since they are now projects and not tasks.
    
(use-package org-clock
  :ensure nil
  :defer t
  :after org
  :config
   (setq org-clock-into-drawer t 
   org-clock-out-remove-zero-time-clocks t
   org-clock-out-when-done t
   org-clock-persist t 
   org-clock-persist-query-resume t ; Do not prompt to resume an active clock
   org-clock-auto-clock-resolution t;(quote when-no-clock-is-running) ; Enable automatically finding open clocks
   org-clock-report-include-clocking-task t ; Include current clocking task in clock reports
   org-clock-history-length 23 ; So it's easy to pick items off the C-F11 list
   org-clock-in-resume t 
   ;; (setq org-drawers (quote ("PROPERTIES" "LOGBOOK"))) ; you will have to find another var/Separate drawers for clocking and logs
   org-clock-out-remove-zero-time-clocks t)) ; this removes clocked tasks with 0:00 duration
	   	   
(use-package org-id
 :ensure nil
 :defer t
 :after org
 :config
 (setq org-id-prefix "task"
       org-id-method 'uuid
       bh/organization-task-id "eb155a82-92b2-4f25-a3c6-0304591af2f9" ; make a function that inserts a task-id automatically, maybe it already exists?
       org-id-link-to-org-use-id 'create-if-interactive-and-no-custom-id))

 
(use-package org-agenda
 :ensure nil
 :defer t
 :after org
 :config
 ;; Keybindings 
 (keymap-set org-agenda-mode-map "q" 'bury-buffer)
 ;; ----------------------------------------------
 (setq org-agenda-files '("~/git/org/")
       org-agenda-window-setup 'other-frame
       org-agenda-sticky t
       org-agenda-span 'day
       org-agenda-persistent-filter t
       org-agenda-tags-todo-honor-ignore-options t
       org-agenda-skip-additional-timestamps-same-entry t
       org-agenda-todo-ignore-with-date nil
       org-agenda-todo-ignore-deadlines nil
       org-agenda-todo-ignore-scheduled nil
       org-agenda-todo-ignore-timestamp nil
       org-agenda-skip-deadline-if-done t
       org-agenda-skip-scheduled-if-done t
       org-agenda-skip-timestamp-if-done t
       org-agenda-show-all-dates t
       org-agenda-sorting-strategy
        (quote ((agenda habit-down time-up user-defined-up effort-up category-keep)
              (todo category-up effort-up)
              (tags category-up effort-up)
              (search category-up)))
       org-agenda-start-on-weekday 1
       org-agenda-timegrid-use-ampm t
       org-agenda-time-grid (quote ((daily today require-timed)
                                   (0600 1100 1300 1500 1700 1900 2100 2300)
				   "....." "---------------"))
       org-agenda-tags-column -50 ; Display tags farther right
       org-agenda-cmp-user-defined 'bh/agenda-sort
       org-agenda-custom-commands
      (quote (("N" "Notes" tags "NOTE"
               ((org-agenda-overriding-header "Notes")
                (org-tags-match-list-sublevels t)))
              ("h" "Habits" tags-todo "STYLE=\"habit\""
               ((org-agenda-overriding-header "Habits")
                (org-agenda-sorting-strategy
                 '(todo-state-down effort-up category-keep))))
              (" " "Agenda"
               ((agenda "" nil)
                (tags "REFILE"
                      ((org-agenda-overriding-header "Tasks to Refile")
                       (org-tags-match-list-sublevels nil)))
                (tags-todo "-CANCELLED/!"
                           ((org-agenda-overriding-header "Stuck Projects")
                            (org-agenda-skip-function 'bh/skip-non-stuck-projects)
                            (org-agenda-sorting-strategy
                             '(category-keep))))
                (tags-todo "-HOLD-CANCELLED/!"
                           ((org-agenda-overriding-header "Projects")
                            (org-agenda-skip-function 'bh/skip-non-projects)
                            (org-tags-match-list-sublevels 'indented)
                            (org-agenda-sorting-strategy
                             '(category-keep))))
                (tags-todo "-CANCELLED/!NEXT"
                           ((org-agenda-overriding-header (concat "Project Next Tasks"
                                                                  (if bh/hide-scheduled-and-waiting-next-tasks
                                                                      ""
                                                                    " (including WAITING and SCHEDULED tasks)")))
                            (org-agenda-skip-function 'bh/skip-projects-and-habits-and-single-tasks)
                            (org-tags-match-list-sublevels t)
                            (org-agenda-todo-ignore-scheduled bh/hide-scheduled-and-waiting-next-tasks)
                            (org-agenda-todo-ignore-deadlines bh/hide-scheduled-and-waiting-next-tasks)
                            (org-agenda-todo-ignore-with-date bh/hide-scheduled-and-waiting-next-tasks)
                            (org-agenda-sorting-strategy
                             '(todo-state-down effort-up category-keep))))
                (tags-todo "-REFILE-CANCELLED-WAITING-HOLD/!"
                           ((org-agenda-overriding-header (concat "Project Subtasks"
                                                                  (if bh/hide-scheduled-and-waiting-next-tasks
                                                                      ""
                                                                    " (including WAITING and SCHEDULED tasks)")))
                            (org-agenda-skip-function 'bh/skip-non-project-tasks)
                            (org-agenda-todo-ignore-scheduled bh/hide-scheduled-and-waiting-next-tasks)
                            (org-agenda-todo-ignore-deadlines bh/hide-scheduled-and-waiting-next-tasks)
                            (org-agenda-todo-ignore-with-date bh/hide-scheduled-and-waiting-next-tasks)
                            (org-agenda-sorting-strategy
                             '(category-keep))))
                (tags-todo "-REFILE-CANCELLED-WAITING-HOLD/!"
                           ((org-agenda-overriding-header (concat "Standalone Tasks"
                                                                  (if bh/hide-scheduled-and-waiting-next-tasks
                                                                      ""
                                                                    " (including WAITING and SCHEDULED tasks)")))
                            (org-agenda-skip-function 'bh/skip-project-tasks)
                            (org-agenda-todo-ignore-scheduled bh/hide-scheduled-and-waiting-next-tasks)
                            (org-agenda-todo-ignore-deadlines bh/hide-scheduled-and-waiting-next-tasks)
                            (org-agenda-todo-ignore-with-date bh/hide-scheduled-and-waiting-next-tasks)
                            (org-agenda-sorting-strategy
                             '(category-keep))))
                (tags-todo "-CANCELLED+WAITING|HOLD/!"
                           ((org-agenda-overriding-header (concat "Waiting and Postponed Tasks"
                                                                  (if bh/hide-scheduled-and-waiting-next-tasks
                                                                      ""
                                                                    " (including WAITING and SCHEDULED tasks)")))
                            (org-agenda-skip-function 'bh/skip-non-tasks)
                            (org-tags-match-list-sublevels nil)
                            (org-agenda-todo-ignore-scheduled bh/hide-scheduled-and-waiting-next-tasks)
                            (org-agenda-todo-ignore-deadlines bh/hide-scheduled-and-waiting-next-tasks)))
                (tags "-REFILE/"
                      ((org-agenda-overriding-header "Tasks to Archive")
                       (org-agenda-skip-function 'bh/skip-non-archivable-tasks)
                       (org-tags-match-list-sublevels nil))))
               nil))))

 
       ;; Org Agenda sorting functions
        (defun bh/agenda-sort (a b)
  "Sorting strategy for agenda items.
Late deadlines first, then scheduled, then non-late deadlines"
         (let (result num-a num-b)
           (cond
            ; time specific items are already sorted first by org-agenda-sorting-strategy

            ; non-deadline and non-scheduled items next
            ((bh/agenda-sort-test 'bh/is-not-scheduled-or-deadline a b))

            ; deadlines for today next
            ((bh/agenda-sort-test 'bh/is-due-deadline a b))

            ; late deadlines next
            ((bh/agenda-sort-test-num 'bh/is-late-deadline '> a b))

            ; scheduled items for today next
            ((bh/agenda-sort-test 'bh/is-scheduled-today a b))

            ; late scheduled items next
            ((bh/agenda-sort-test-num 'bh/is-scheduled-late '> a b))

            ; pending deadlines last
            ((bh/agenda-sort-test-num 'bh/is-pending-deadline '< a b))

            ; finally default to unsorted
            (t (setq result nil)))
           result))

            (defmacro bh/agenda-sort-test (fn a b)
              "Test for agenda sort"
              `(cond
                ; if both match leave them unsorted
                ((and (apply ,fn (list ,a))
                      (apply ,fn (list ,b)))
                 (setq result nil))
                ; if a matches put a first
                ((apply ,fn (list ,a))
                 (setq result -1))
                ; otherwise if b matches put b first
                ((apply ,fn (list ,b))
                 (setq result 1))
                ; if none match leave them unsorted
                (t nil)))
            
            (defmacro bh/agenda-sort-test-num (fn compfn a b)
              `(cond
                ((apply ,fn (list ,a))
                 (setq num-a (string-to-number (match-string 1 ,a)))
                 (if (apply ,fn (list ,b))
                     (progn
                       (setq num-b (string-to-number (match-string 1 ,b)))
                       (setq result (if (apply ,compfn (list num-a num-b))
                                        -1
                                      1)))
                   (setq result -1)))
                ((apply ,fn (list ,b))
                 (setq result 1))
                (t nil)))
            
            (defun bh/is-not-scheduled-or-deadline (date-str)
              (and (not (bh/is-deadline date-str))
                   (not (bh/is-scheduled date-str))))
            
            (defun bh/is-due-deadline (date-str)
              (string-match "Deadline:" date-str))
            
            (defun bh/is-late-deadline (date-str)
              (string-match "\\([0-9]*\\) d\. ago:" date-str))
            
            (defun bh/is-pending-deadline (date-str)
              (string-match "In \\([^-]*\\)d\.:" date-str))
            
            (defun bh/is-deadline (date-str)
              (or (bh/is-due-deadline date-str)
                  (bh/is-late-deadline date-str)
                  (bh/is-pending-deadline date-str)))
            
            (defun bh/is-scheduled (date-str)
              (or (bh/is-scheduled-today date-str)
                  (bh/is-scheduled-late date-str)))
            
            (defun bh/is-scheduled-today (date-str)
              (string-match "Scheduled:" date-str))
            
            (defun bh/is-scheduled-late (date-str)
              (string-match "Sched\.\\(.*\\)x:" date-str)))
;; ---------- end of Org Agenda sorting functions --------------
 


	

       
 

(use-package org-refile
 :ensure nil
 :defer t
 :after org
 :config
 (setq org-refile-targets (quote ((nil :maxlevel . 9)
                           (org-agenda-files :maxlevel . 9)))
 org-outline-path-complete-in-steps nil
 org-refile-allow-creating-parent-nodes (quote confirm)
 org-refile-use-outline-path 'file))

(use-package org-indent
  :ensure nil
  :defer t
  :after org
  :config
  (setq org-startup-indented t))

(use-package org-cycle
  :ensure nil
  :defer t
  :after org
  :config
  (setq org-cycle-separator-lines 0
	org-cycle-include-plain-lists t))

(use-package org-fold
  :ensure nil
  :defer t
  :after org
  :config
  (setq org-fold-show-context-detail
	'((default . (ancestors siblings)))
	org-fold-catch-invisible-edits 'error))

(use-package org-keys
  :ensure nil
  :defer t
  :after org
  :config  
  (setq org-return-follows-link t))

(use-package org-list
  :ensure nil
  :defer t
  :after org
  :config
  (setq org-list-demote-modify-bullet (quote (("+" . "-")
                                            ("*" . "-")
                                            ("1." . "-")
                                            ("1)" . "-")
                                            ("A)" . "-")
                                            ("B)" . "-")
                                            ("a)" . "-")
                                            ("b)" . "-")
                                            ("A." . "-")
                                            ("B." . "-")
                                            ("a." . "-")
                                            ("b." . "-")))
	org-list-allow-alphabetical t
	org-clone-delete-id t))

(use-package org-capture
  :ensure nil
  :defer t
  :after org
  :config  
  (setq org-capture-templates
      (quote (("t" "todo" entry (file "~/git/org/refile.org")
               "* TODO %?\n%U\n%a\n" :clock-in t :clock-resume t)
              ("r" "respond" entry (file "~/git/org/refile.org")
               "* NEXT Respond to %:from on %:subject\nSCHEDULED: %t\n%U\n%a\n" :clock-in t :clock-resume t :immediate-finish t)
              ("n" "note" entry (file "~/git/org/refile.org")
               "* %? :NOTE:\n%U\n%a\n" :clock-in t :clock-resume t)
              ("j" "Journal" entry (file+datetree "~/git/org/diary.org")
               "* %?\n%U\n" :clock-in t :clock-resume t)
              ("w" "org-protocol" entry (file "~/git/org/refile.org")
               "* TODO Review %c\n%U\n" :immediate-finish t)
              ("m" "Meeting" entry (file "~/git/org/refile.org")
               "* MEETING with %? :MEETING:\n%U" :clock-in t :clock-resume t)
              ("p" "Phone call" entry (file "~/git/org/refile.org")
               "* PHONE %? :PHONE:\n%U" :clock-in t :clock-resume t)
              ("h" "Habit" entry (file "~/git/org/refile.org")
               "* NEXT %?\n%U\n%a\nSCHEDULED: %(format-time-string \"%<<%Y-%m-%d %a .+1d/3d>>\")\n:PROPERTIES:\n:STYLE: habit\n:REPEAT_TO_STATE: NEXT\n:END:\n")))))
 
(use-package ol
  :ensure nil
  :defer t
  :after org
  :config
  (keymap-set org-mode-map "C-c l" 'org-store-link))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(keyfreq evil-collection evil-core org-refile magit mode-icons use-package treesit-auto treemacs-evil smex pyvenv pyenv-mode lsp-ui lsp-pyright ido-completing-read+ evil-commentary envrc eglot direnv company bbdb)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(put 'erase-buffer 'disabled nil)
