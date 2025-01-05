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

;;; Third party packages
(use-package org-checklist
  :load-path "~/.emacs.d/third-party/contrib/lisp"
  :ensure nil
  :config
  (setq org-checklist-export-params t)
)
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
  (defun bh/punch-out ()
    (interactive)
    (setq bh/keep-clock-running nil)
    (when (org-clock-is-active)
      (org-clock-out))
    (org-agenda-remove-restriction-lock))

(defun bh/clock-in-default-task ()
  (save-excursion
    (org-with-point-at org-clock-default-task
      (org-clock-in))))

(setq bh/keep-clock-running nil)

(defun bh/clock-in-to-next (kw)
  "Switch a task from TODO to NEXT when clocking in.
Skips capture tasks, projects, and subprojects.
Switch projects and subprojects from NEXT back to TODO"
  (when (not (and (boundp 'org-capture-mode) org-capture-mode))
    (cond
     ((and (member (org-get-todo-state) (list "TODO"))
           (bh/is-task-p))
      "NEXT")
     ((and (member (org-get-todo-state) (list "NEXT"))
           (bh/is-project-p))
      "TODO"))))

(defun bh/find-project-task ()
  "Move point to the parent (project) task if any"
  (save-restriction
    (widen)
    (let ((parent-task (save-excursion (org-back-to-heading 'invisible-ok) (point))))
      (while (org-up-heading-safe)
        (when (member (nth 2 (org-heading-components)) org-todo-keywords-1)
          (setq parent-task (point))))
      (goto-char parent-task)
      parent-task)))

(defun bh/clock-in-parent-task ()
  "Move point to the parent (project) task if any and clock in"
  (let ((parent-task))
    (save-excursion
      (save-restriction
        (widen)
        (while (and (not parent-task) (org-up-heading-safe))
          (when (member (nth 2 (org-heading-components)) org-todo-keywords-1)
            (setq parent-task (point))))
        (if parent-task
            (org-with-point-at parent-task
              (org-clock-in))
          (when bh/keep-clock-running
            (bh/clock-in-default-task)))))))


(defun bh/clock-in-last-task (arg)
  "Clock in the interrupted task if there is one
Skip the default task and get the next one.
A prefix arg forces clock in of the default task."
  (interactive "p")
  (let ((clock-in-to-task
         (cond
          ((eq arg 4) org-clock-default-task)
          ((and (org-clock-is-active)
                (equal org-clock-default-task (cadr org-clock-history)))
           (caddr org-clock-history))
          ((org-clock-is-active) (cadr org-clock-history))
          ((equal org-clock-default-task (car org-clock-history)) (cadr org-clock-history))
          (t (car org-clock-history)))))
    (widen)
    (org-with-point-at clock-in-to-task
      (org-clock-in nil))))

(defun bh/clock-out-maybe ()
  (when (and bh/keep-clock-running
             (not org-clock-clocking-in)
             (marker-buffer org-clock-default-task)
             (not org-clock-resolving-clocks-due-to-idleness))
    (bh/clock-in-parent-task)))

(add-hook 'org-clock-out-hook 'bh/clock-out-maybe 'append)

;; Remove empty LOGBOOK drawers on clock out
(defun bh/remove-empty-drawer-on-clock-out ()
  (interactive)
  (save-excursion
    (beginning-of-line 0)
    (org-remove-empty-drawer-at "LOGBOOK" (point))))

(add-hook 'org-clock-out-hook 'bh/remove-empty-drawer-on-clock-out 'append)

(defun bh/skip-non-stuck-projects ()
  "Skip trees that are not stuck projects"
  ;; (bh/list-sublevels-for-projects-indented)
  (save-restriction
    (widen)
    (let ((next-headline (save-excursion (or (outline-next-heading) (point-max)))))
      (if (bh/is-project-p)
          (let* ((subtree-end (save-excursion (org-end-of-subtree t)))
                 (has-next ))
            (save-excursion
              (forward-line 1)
              (while (and (not has-next) (< (point) subtree-end) (re-search-forward "^\\*+ NEXT " subtree-end t))
                (unless (member "WAITING" (org-get-tags-at))
                  (setq has-next t))))
            (if has-next
                next-headline
              nil)) ; a stuck project, has subtasks but no next task
        next-headline))))


;; Helper functions for projects used by the agenda view
(defun bh/is-project-p ()
  "Any task with a todo keyword subtask"
  (save-restriction
    (widen)
    (let ((has-subtask)
          (subtree-end (save-excursion (org-end-of-subtree t)))
          (is-a-task (member (nth 2 (org-heading-components)) org-todo-keywords-1)))
      (save-excursion
        (forward-line 1)
        (while (and (not has-subtask)
                    (< (point) subtree-end)
                    (re-search-forward "^\*+ " subtree-end t))
          (when (member (org-get-todo-state) org-todo-keywords-1)
            (setq has-subtask t))))
      (and is-a-task has-subtask))))

(defun bh/is-project-subtree-p ()
  "Any task with a todo keyword that is in a project subtree.
Callers of this function already widen the buffer view."
  (let ((task (save-excursion (org-back-to-heading 'invisible-ok)
                              (point))))
    (save-excursion
      (bh/find-project-task)
      (if (equal (point) task)
          nil
        t))))

(defun bh/is-task-p ()
  "Any task with a todo keyword and no subtask"
  (save-restriction
    (widen)
    (let ((has-subtask)
          (subtree-end (save-excursion (org-end-of-subtree t)))
          (is-a-task (member (nth 2 (org-heading-components)) org-todo-keywords-1)))
      (save-excursion
        (forward-line 1)
        (while (and (not has-subtask)
                    (< (point) subtree-end)
                    (re-search-forward "^\*+ " subtree-end t))
          (when (member (org-get-todo-state) org-todo-keywords-1)
            (setq has-subtask t))))
      (and is-a-task (not has-subtask)))))

(defun bh/is-subproject-p ()
  "Any task which is a subtask of another project"
  (let ((is-subproject)
        (is-a-task (member (nth 2 (org-heading-components)) org-todo-keywords-1)))
    (save-excursion
      (while (and (not is-subproject) (org-up-heading-safe))
        (when (member (nth 2 (org-heading-components)) org-todo-keywords-1)
          (setq is-subproject t))))
    (and is-a-task is-subproject)))

(defun bh/list-sublevels-for-projects-indented ()
  "Set org-tags-match-list-sublevels so when restricted to a subtree we list all subtasks.
  This is normally used by skipping functions where this variable is already local to the agenda."
  (if (marker-buffer org-agenda-restrict-begin)
      (setq org-tags-match-list-sublevels 'indented)
    (setq org-tags-match-list-sublevels nil))
  nil)

(defun bh/list-sublevels-for-projects ()
  "Set org-tags-match-list-sublevels so when restricted to a subtree we list all subtasks.
  This is normally used by skipping functions where this variable is already local to the agenda."
  (if (marker-buffer org-agenda-restrict-begin)
      (setq org-tags-match-list-sublevels t)
    (setq org-tags-match-list-sublevels nil))
  nil)

(defvar bh/hide-scheduled-and-waiting-next-tasks t)

(defun bh/toggle-next-task-display ()
  (interactive)
  (setq bh/hide-scheduled-and-waiting-next-tasks (not bh/hide-scheduled-and-waiting-next-tasks))
  (when  (equal major-mode 'org-agenda-mode)
    (org-agenda-redo))
  (message "%s WAITING and SCHEDULED NEXT Tasks" (if bh/hide-scheduled-and-waiting-next-tasks "Hide" "Show")))

(defun bh/skip-stuck-projects ()
  "Skip trees that are not stuck projects"
  (save-restriction
    (widen)
    (let ((next-headline (save-excursion (or (outline-next-heading) (point-max)))))
      (if (bh/is-project-p)
          (let* ((subtree-end (save-excursion (org-end-of-subtree t)))
                 (has-next ))
            (save-excursion
              (forward-line 1)
              (while (and (not has-next) (< (point) subtree-end) (re-search-forward "^\\*+ NEXT " subtree-end t))
                (unless (member "WAITING" (org-get-tags-at))
                  (setq has-next t))))
            (if has-next
                nil
              next-headline)) ; a stuck project, has subtasks but no next task
        nil))))

(defun bh/skip-non-stuck-projects ()
  "Skip trees that are not stuck projects"
  ;; (bh/list-sublevels-for-projects-indented)
  (save-restriction
    (widen)
    (let ((next-headline (save-excursion (or (outline-next-heading) (point-max)))))
      (if (bh/is-project-p)
          (let* ((subtree-end (save-excursion (org-end-of-subtree t)))
                 (has-next ))
            (save-excursion
              (forward-line 1)
              (while (and (not has-next) (< (point) subtree-end) (re-search-forward "^\\*+ NEXT " subtree-end t))
                (unless (member "WAITING" (org-get-tags-at))
                  (setq has-next t))))
            (if has-next
                next-headline
              nil)) ; a stuck project, has subtasks but no next task
        next-headline))))

(defun bh/skip-non-projects ()
  "Skip trees that are not projects"
  ;; (bh/list-sublevels-for-projects-indented)
  (if (save-excursion (bh/skip-non-stuck-projects))
      (save-restriction
        (widen)
        (let ((subtree-end (save-excursion (org-end-of-subtree t))))
          (cond
           ((bh/is-project-p)
            nil)
           ((and (bh/is-project-subtree-p) (not (bh/is-task-p)))
            nil)
           (t
            subtree-end))))
    (save-excursion (org-end-of-subtree t))))

(defun bh/skip-non-tasks ()
  "Show non-project tasks.
Skip project and sub-project tasks, habits, and project related tasks."
  (save-restriction
    (widen)
    (let ((next-headline (save-excursion (or (outline-next-heading) (point-max)))))
      (cond
       ((bh/is-task-p)
        nil)
       (t
        next-headline)))))

(defun bh/skip-project-trees-and-habits ()
  "Skip trees that are projects"
  (save-restriction
    (widen)
    (let ((subtree-end (save-excursion (org-end-of-subtree t))))
      (cond
       ((bh/is-project-p)
        subtree-end)
       ((org-is-habit-p)
        subtree-end)
       (t
        nil)))))

(defun bh/skip-projects-and-habits-and-single-tasks ()
  "Skip trees that are projects, tasks that are habits, single non-project tasks"
  (save-restriction
    (widen)
    (let ((next-headline (save-excursion (or (outline-next-heading) (point-max)))))
      (cond
       ((org-is-habit-p)
        next-headline)
       ((and bh/hide-scheduled-and-waiting-next-tasks
             (member "WAITING" (org-get-tags-at)))
        next-headline)
       ((bh/is-project-p)
        next-headline)
       ((and (bh/is-task-p) (not (bh/is-project-subtree-p)))
        next-headline)
       (t
        nil)))))

(defun bh/skip-project-tasks-maybe ()
  "Show tasks related to the current restriction.
When restricted to a project, skip project and sub project tasks, habits, NEXT tasks, and loose tasks.
When not restricted, skip project and sub-project tasks, habits, and project related tasks."
  (save-restriction
    (widen)
    (let* ((subtree-end (save-excursion (org-end-of-subtree t)))
           (next-headline (save-excursion (or (outline-next-heading) (point-max))))
           (limit-to-project (marker-buffer org-agenda-restrict-begin)))
      (cond
       ((bh/is-project-p)
        next-headline)
       ((org-is-habit-p)
        subtree-end)
       ((and (not limit-to-project)
             (bh/is-project-subtree-p))
        subtree-end)
       ((and limit-to-project
             (bh/is-project-subtree-p)
             (member (org-get-todo-state) (list "NEXT")))
        subtree-end)
       (t
        nil)))))

(defun bh/skip-project-tasks ()
  "Show non-project tasks.
Skip project and sub-project tasks, habits, and project related tasks."
  (save-restriction
    (widen)
    (let* ((subtree-end (save-excursion (org-end-of-subtree t))))
      (cond
       ((bh/is-project-p)
        subtree-end)
       ((org-is-habit-p)
        subtree-end)
       ((bh/is-project-subtree-p)
        subtree-end)
       (t
        nil)))))

(defun bh/skip-non-project-tasks ()
  "Show project tasks.
Skip project and sub-project tasks, habits, and loose non-project tasks."
  (save-restriction
    (widen)
    (let* ((subtree-end (save-excursion (org-end-of-subtree t)))
           (next-headline (save-excursion (or (outline-next-heading) (point-max)))))
      (cond
       ((bh/is-project-p)
        next-headline)
       ((org-is-habit-p)
        subtree-end)
       ((and (bh/is-project-subtree-p)
             (member (org-get-todo-state) (list "NEXT")))
        subtree-end)
       ((not (bh/is-project-subtree-p))
        subtree-end)
       (t
        nil)))))

(defun bh/skip-projects-and-habits ()
  "Skip trees that are projects and tasks that are habits"
  (save-restriction
    (widen)
    (let ((subtree-end (save-excursion (org-end-of-subtree t))))
      (cond
       ((bh/is-project-p)
        subtree-end)
       ((org-is-habit-p)
        subtree-end)
       (t
        nil)))))

(defun bh/skip-non-subprojects ()
  "Skip trees that are not projects"
  (let ((next-headline (save-excursion (outline-next-heading))))
    (if (bh/is-subproject-p)
        nil
      next-headline)))

(defun bh/skip-non-archivable-tasks ()
  "Skip trees that are not available for archiving"
  (save-restriction
    (widen)
    ;; Consider only tasks with done todo headings as archivable candidates
    (let ((next-headline (save-excursion (or (outline-next-heading) (point-max))))
          (subtree-end (save-excursion (org-end-of-subtree t))))
      (if (member (org-get-todo-state) org-todo-keywords-1)
          (if (member (org-get-todo-state) org-done-keywords)
              (let* ((daynr (string-to-number (format-time-string "%d" (current-time))))
                     (a-month-ago (* 60 60 24 (+ daynr 1)))
                     (last-month (format-time-string "%Y-%m-" (time-subtract (current-time) (seconds-to-time a-month-ago))))
                     (this-month (format-time-string "%Y-%m-" (current-time)))
                     (subtree-is-current (save-excursion
                                           (forward-line 1)
                                           (and (< (point) subtree-end)
                                                (re-search-forward (concat last-month "\\|" this-month) subtree-end t)))))
                (if subtree-is-current
                    subtree-end ; Has a date in this month or last month, skip it
                  nil))  ; available to archive
            (or subtree-end (point-max)))
        next-headline))))

(defun bh/org-todo (arg)
  (interactive "p")
  (if (equal arg 4)
      (save-restriction
        (bh/narrow-to-org-subtree)
        (org-show-todo-tree nil))
    (bh/narrow-to-org-subtree)
    (org-show-todo-tree nil)))

(defun bh/widen ()
  (interactive)
  (if (equal major-mode 'org-agenda-mode)
      (progn
        (org-agenda-remove-restriction-lock)
        (when org-agenda-sticky
          (org-agenda-redo)))
    (widen)))

(add-hook 'org-agenda-mode-hook
          '(lambda () (org-defkey org-agenda-mode-map "W" (lambda () (interactive) (setq bh/hide-scheduled-and-waiting-next-tasks t) (bh/widen))))
          'append)

(defun bh/restrict-to-file-or-follow (arg)
  "Set agenda restriction to 'file or with argument invoke follow mode.
I don't use follow mode very often but I restrict to file all the time
so change the default 'F' binding in the agenda to allow both"
  (interactive "p")
  (if (equal arg 4)
      (org-agenda-follow-mode)
    (widen)
    (bh/set-agenda-restriction-lock 4)
    (org-agenda-redo)
    (beginning-of-buffer)))

(add-hook 'org-agenda-mode-hook
          '(lambda () (org-defkey org-agenda-mode-map "F" 'bh/restrict-to-file-or-follow))
          'append)

(defun bh/set-agenda-restriction-lock (arg)
  "Set restriction lock to current task subtree or file if prefix is specified"
  (interactive "p")
  (let* ((pom (bh/get-pom-from-agenda-restriction-or-point))
         (tags (org-with-point-at pom (org-get-tags-at))))
    (let ((restriction-type (if (equal arg 4) 'file 'subtree)))
      (save-restriction
        (cond
         ((and (equal major-mode 'org-agenda-mode) pom)
          (org-with-point-at pom
            (org-agenda-set-restriction-lock restriction-type))
          (org-agenda-redo))
         ((and (equal major-mode 'org-mode) (org-before-first-heading-p))
          (org-agenda-set-restriction-lock 'file))
         (pom
          (org-with-point-at pom
            (org-agenda-set-restriction-lock restriction-type))))))))

(defun bh/narrow-to-org-subtree ()
  (widen)
  (org-narrow-to-subtree)
  (save-restriction
    (org-agenda-set-restriction-lock)))

(defun bh/narrow-to-subtree ()
  (interactive)
  (if (equal major-mode 'org-agenda-mode)
      (progn
        (org-with-point-at (org-get-at-bol 'org-hd-marker)
          (bh/narrow-to-org-subtree))
        (when org-agenda-sticky
          (org-agenda-redo)))
    (bh/narrow-to-org-subtree)))

(add-hook 'org-agenda-mode-hook
          '(lambda () (org-defkey org-agenda-mode-map "N" 'bh/narrow-to-subtree))
          'append)

(defun bh/narrow-up-one-org-level ()
  (widen)
  (save-excursion
    (outline-up-heading 1 'invisible-ok)
    (bh/narrow-to-org-subtree)))

(defun bh/get-pom-from-agenda-restriction-or-point ()
  (or (and (marker-position org-agenda-restrict-begin) org-agenda-restrict-begin)
      (org-get-at-bol 'org-hd-marker)
      (and (equal major-mode 'org-mode) (point))
      org-clock-marker))

(defun bh/narrow-up-one-level ()
  (interactive)
  (if (equal major-mode 'org-agenda-mode)
      (progn
        (org-with-point-at (bh/get-pom-from-agenda-restriction-or-point)
          (bh/narrow-up-one-org-level))
        (org-agenda-redo))
    (bh/narrow-up-one-org-level)))

(add-hook 'org-agenda-mode-hook
          '(lambda () (org-defkey org-agenda-mode-map "U" 'bh/narrow-up-one-level))
          'append)

(defun bh/narrow-to-org-project ()
  (widen)
  (save-excursion
    (bh/find-project-task)
    (bh/narrow-to-org-subtree)))

(defun bh/narrow-to-project ()
  (interactive)
  (if (equal major-mode 'org-agenda-mode)
      (progn
        (org-with-point-at (bh/get-pom-from-agenda-restriction-or-point)
          (bh/narrow-to-org-project)
          (save-excursion
            (bh/find-project-task)
            (org-agenda-set-restriction-lock)))
        (org-agenda-redo)
        (beginning-of-buffer))
    (bh/narrow-to-org-project)
    (save-restriction
      (org-agenda-set-restriction-lock))))

(add-hook 'org-agenda-mode-hook
          '(lambda () (org-defkey org-agenda-mode-map "P" 'bh/narrow-to-project))
          'append)

(defvar bh/project-list nil)

(defun bh/view-next-project ()
  (interactive)
  (let (num-project-left current-project)
    (unless (marker-position org-agenda-restrict-begin)
      (goto-char (point-min))
      ; Clear all of the existing markers on the list
      (while bh/project-list
        (set-marker (pop bh/project-list) nil))
      (re-search-forward "Tasks to Refile")
      (forward-visible-line 1))

    ; Build a new project marker list
    (unless bh/project-list
      (while (< (point) (point-max))
        (while (and (< (point) (point-max))
                    (or (not (org-get-at-bol 'org-hd-marker))
                        (org-with-point-at (org-get-at-bol 'org-hd-marker)
                          (or (not (bh/is-project-p))
                              (bh/is-project-subtree-p)))))
          (forward-visible-line 1))
        (when (< (point) (point-max))
          (add-to-list 'bh/project-list (copy-marker (org-get-at-bol 'org-hd-marker)) 'append))
        (forward-visible-line 1)))

    ; Pop off the first marker on the list and display
    (setq current-project (pop bh/project-list))
    (when current-project
      (org-with-point-at current-project
        (setq bh/hide-scheduled-and-waiting-next-tasks nil)
        (bh/narrow-to-project))
      ; Remove the marker
      (setq current-project nil)
      (org-agenda-redo)
      (beginning-of-buffer)
      (setq num-projects-left (length bh/project-list))
      (if (> num-projects-left 0)
          (message "%s projects left to view" num-projects-left)
        (beginning-of-buffer)
        (setq bh/hide-scheduled-and-waiting-next-tasks t)
        (error "All projects viewed.")))))

(add-hook 'org-agenda-mode-hook
          '(lambda () (org-defkey org-agenda-mode-map "V" 'bh/view-next-project))
          'append)
(defun insert-default-columnview-block ()
  (interactive)
  (insert "#+BEGIN: columnview :hlines 1 :id local\n#+END:\n"))


(defun bh/org-auto-exclude-function (tag)
  "Automatic task exclusion in the agenda with / RET"
  (and (cond
        ((string= tag "hold")
         t)
        ((string= tag "farm")
         t))
       (concat "-" tag)))

;; -------- end of user functions 
;;; Global keybindings  
(global-set-key (kbd "C-c h") #'help-for-help)
;(global-set-key (kbd "C-c r") #'isearch-backward)
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

;;; Native Minor Modes
(transient-mark-mode 1)
(global-visual-line-mode 1)
(global-hl-line-mode 1)
(use-package faces
  :ensure nil
  :config 
  (set-face-background 'hl-line "magenta")
  (set-face-background 'magit-section-highlight "magenta")
  (set-face-foreground 'hl-line "color-193")
  (set-face-foreground 'org-hide "black"))

(use-package tab-bar
  :ensure nil
  :init
  (tab-bar-mode t))
  ;(set-face-attribute 'cursor nil :foreground "red" :background "black")
  ;(set-cursor-color "red"))
; ----- end of third party minor modes 
;;; 3rd party Minor Modes 
(use-package evil
  :ensure t
  :init
  (setq evil-want-keybinding nil)
  (evil-mode 1)
  :config
  (evil-define-key 'insert 'global "\\" 'evil-normal-state))
 (use-package evil-commentary
  :ensure t
  :after evil
  :config
  (evil-commentary-mode 1))

;; (use-package evil-core
;;   :ensure nil
;;   :after evil
;;   :config
;;   (when (bound-and-true-p org-capture-mode)
;;     (evil-define-key '(normal insert visual)  org-capture-mode-map "C-c C-c" 'org-capture-finalize)))
(use-package ido-completing-read+
  :ensure t
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
  )
(use-package magit
  :ensure t
  )
(use-package flycheck
  :ensure t
  )
(use-package json-mode
  :ensure t
  :pin melpa
  )
(use-package evil-collection
  :ensure t
  :after (magit)
  :config
  (evil-collection-init 'magit))
(use-package keyfreq
  :ensure t
  :config
  (keyfreq-mode 1)
  (keyfreq-autosave-mode 1))
(use-package hydra
  :ensure t
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

;; End of third party packages --------------------------------


;;; Native Major Modes 
(use-package org
  :ensure nil
  :init 
  (setq org-time-stamp-custom-formats '("%m/%d/%Y %a " . "%m/%d/%Y %a %I:%M %p"))
  (org-clock-persistence-insinuate) ; resume clocking task when emacs is restarted
  (add-to-list 'auto-mode-alist '("\\.\\(org\\|org_archive\\|txt\\)$" . org-mode))
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
	org-use-fast-todo-selection t
	org-insert-heading-respect-content nil
	org-reverse-note-order nil
	org-time-stamp-rounding-minutes (quote (1 1))
	org-deadline-warning-days 30
        org-archive-mark-done nil
        org-archive-location "%s_archive::* Archived Tasks"
        org-tags-match-list-sublevels t
	org-log-done (quote time)
        org-columns-default-format "%80ITEM(Task) %10Effort(Effort){:} %10CLOCKSUM"
	org-global-properties (quote (("Effort_ALL" . "0:15 0:30 0:45 1:00 2:00 3:00 4:00 5:00 6:00 0:00")
                                    ("STYLE_ALL" . "habit")))
	org-log-state-notes-insert-after-drawers nil
	org-todo-keywords
         (quote ((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d)")
              (sequence "WAITING(w@/!)" "HOLD(h@/!)" "|" "CANCELLED(c@/!)" "PHONE" "MEETING")))

        org-tag-alist (quote ((:startgroup) ; tags with fast selection keys
                            ("@errand" . ?e)
                            ("@office" . ?o)
                            ("@home" . ?H)
			    ("@geography" . ?g)
                            (:endgroup)
                            ("WAITING" . ?w)
                            ("HOLD" . ?h)
                            ("PERSONAL" . ?P)
                            ("WORK" . ?W)
                            ("crypt" . ?E)
                            ("NOTE" . ?n)
                            ("CANCELLED" . ?c)
                            ("FLAGGED" . ??)))
	org-todo-state-tags-triggers
         (quote (("CANCELLED" ("CANCELLED" . t))
                 ("WAITING" ("WAITING" . t))
                 ("HOLD" ("WAITING" . t) ("HOLD" . t))
                 (done ("WAITING") ("HOLD"))
                 ("TODO" ("WAITING") ("CANCELLED") ("HOLD"))
                 ("NEXT" ("WAITING") ("CANCELLED") ("HOLD"))
                 ("DONE" ("WAITING") ("CANCELLED") ("HOLD"))))

	
        org-directory "~/git/org"
	org-default-notes-file "~/git/org/refile.org"
	org-startup-folded t
	org-read-date-prefer-future 'time))

    
(use-package org-clock
  :ensure nil
  :after org
  :config
   (setq org-clock-into-drawer t 
   org-clock-out-remove-zero-time-clocks t
   org-clock-out-when-done t
   org-clock-persist t 
   org-clock-persist-query-resume nil ; Do not prompt to resume an active clock
   org-clock-auto-clock-resolution (quote when-no-clock-is-running) ; Enable automatically finding open clocks
   org-clock-report-include-clocking-task t ; Include current clocking task in clock reports
   org-clock-clocktable-default-properties '(:hlines 1 :id local)
   org-clock-history-length 23 ; So it's easy to pick items off the C-F11 list
   org-clock-in-resume t
   org-clock-in-switch-to-state 'bh/clock-in-to-next
   ;; (setq org-drawers (quote ("PROPERTIES" "LOGBOOK"))) ; you will have to find another var/Separate drawers for clocking and logs
   org-clock-out-remove-zero-time-clocks t)) ; this removes clocked tasks with 0:00 duration
	   	   
(use-package org-id
 :ensure nil
 :after org
 :config
 (setq org-id-prefix "task"
       org-id-method 'uuid
       bh/organization-task-id "eb155a82-92b2-4f25-a3c6-0304591af2f9" ; make a function that inserts a task-id automatically, maybe it already exists?
       org-id-link-to-org-use-id 'create-if-interactive-and-no-custom-id))
 
(use-package org-agenda
 :ensure nil
 :after org
 :config
 ;; Keybindings 
 (keymap-set org-agenda-mode-map "q" 'bury-buffer)
 ;; ----------------------------------------------
 (setq org-agenda-files (quote ("~/git/org"))
       org-agenda-window-setup 'current-window
       org-agenda-restore-windows-after-quit t
       org-agenda-restriction-lock-highlight-subtree nil
       org-agenda-sticky t
       org-agenda-log-mode-items (quote (closed state))
       org-agenda-dim-blocked-tasks nil
       org-agenda-compact-blocks t
       org-agenda-span 'day
       org-agenda-persistent-filter t
       org-agenda-clockreport-parameter-plist (quote (:link t :maxlevel 5 :fileskip0 t :compact t :narrow 80))
       org-agenda-auto-exclude-function 'bh/org-auto-exclude-function
       org-agenda-tags-todo-honor-ignore-options t
       org-agenda-skip-additional-timestamps-same-entry t
       org-agenda-todo-ignore-with-date nil
       org-agenda-todo-ignore-deadlines nil
       org-agenda-todo-ignore-scheduled nil
       org-agenda-todo-ignore-timestamp nil
       org-agenda-skip-deadline-if-done t
       org-agenda-skip-scheduled-if-done t
       org-agenda-skip-timestamp-if-done t
       org-agenda-include-diary nil
       org-agenda-diary-file "~/git/org/diary.org"
       org-agenda-show-all-dates t
       org-agenda-show-future-repeats t
       org-stuck-projects (quote ("" nil nil ""))
       org-agenda-sorting-strategy
        (quote ((agenda habit-down time-up user-defined-up effort-up category-keep)
              (todo category-up effort-up)
              (tags category-up effort-up)
              (search category-up)))
       org-agenda-clock-consistency-checks
        (quote (:max-duration "4:00"
              :min-duration 0
              :max-gap 0
              :gap-ok-around ("4:00")))
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
 :after org
 :config
 (setq org-refile-targets (quote ((nil :maxlevel . 9)
                           (org-agenda-files :maxlevel . 9)))
 org-outline-path-complete-in-steps nil
 org-refile-allow-creating-parent-nodes (quote confirm)
 org-refile-use-outline-path 'file))

(use-package org-indent
  :ensure nil
  :after org
  :config
  (setq org-startup-indented t))

(use-package org-cycle
  :ensure nil
  :after org
  :config
  (setq org-cycle-separator-lines 0
	org-cycle-include-plain-lists t))

(use-package org-fold
  :ensure nil
  :after org
  :config
  (setq org-fold-show-context-detail
	'((default))))

(use-package org-keys
  :ensure nil
  :after org
  :config  
  (setq org-return-follows-link t))

(use-package org-list
  :ensure nil
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
               "* NEXT %?\n%U\n%a\nSCHEDULED: %(format-time-string \"%<<%Y-%m-%d %a .+1d/3d>>\")\n:PROPERTIES:\n:STYLE: habit\n:REPEAT_TO_STATE: NEXT\n:END:\n"))))
    (when (bound-and-true-p org-capture-mode)
    (evil-define-key '(normal insert visual)  org-capture-mode-map "C-c C-c" 'org-capture-finalize)))
(use-package org-faces
  :ensure nil
  :after org
  :config
  (setq org-todo-keyword-faces
      (quote (("TODO" :foreground "red" :weight bold)
              ("NEXT" :foreground "blue" :weight bold)
              ("DONE" :foreground "forest green" :weight bold)
              ("WAITING" :foreground "orange" :weight bold)
              ("HOLD" :foreground "magenta" :weight bold)
              ("CANCELLED" :foreground "forest green" :weight bold)
              ("MEETING" :foreground "forest green" :weight bold)
              ("PHONE" :foreground "forest green" :weight bold)))))

(use-package ol
  :ensure nil
  :after org
  :config
  (keymap-set org-mode-map "C-c l" 'org-store-link))

(use-package org-habit
  :ensure nil
  :after org
  :config
  (setq org-habit-graph-column 50)
  (run-at-time "06:00" 86400 '(lambda () (setq org-habit-show-habits t))))


(use-package dired
  :ensure nil
  :config
  (keymap-set dired-mode-map "C-c m" 'dired-mark))
;---- end of native major modes -----
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(org-checklist keyfreq evil-collection evil-core org-refile magit mode-icons use-package treesit-auto treemacs-evil smex pyvenv pyenv-mode lsp-ui lsp-pyright ido-completing-read+ evil-commentary envrc eglot direnv company bbdb)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
  '(org-mode-line-clock ((t (:background "grey75" :foreground "red" :box (:line-width -1 :style released-button)))) t))
(put 'erase-buffer 'disabled nil)
