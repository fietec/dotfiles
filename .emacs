(setq custom-file "~/.emacs.custom.el")
(package-initialize)

(load "~/.emacs.rc/rc.el")

(setq inhibit-startup-message t)
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(load-theme 'gruber-darker t)
(display-line-numbers-mode 1)
(setq-default display-line-numbers 'relative)

(setq dired-listing-switches "-alh")
(setq dired-dwim-target t)

(setq-default tab-width 4)
(indent-tabs-mode . nil)

;;; C-Mode
;;; (setq-default c-basic-offset 4)
(c-add-style "my-c-style"
             '("linux" ; or "k&r" as a base, try both
               (c-basic-offset . 4)      ; Indent width
               (indent-tabs-mode . nil) ; Use spaces
               (c-offsets-alist
                (substatement-open . 0)     ; No extra indent for `{`
                (case-label . +)            ; Indent `case` inside switch
                (statement-case-open . +)   ; Indent opening `{` in case
                (statement-case-intro . +)  ; Indent first line inside case
                )))

(defun my-c-mode-hook ()
  (c-set-style "my-c-style")
  (setq indent-tabs-mode nil)) ; Optional: spaces over tabs

(add-hook 'c-mode-hook 'my-c-mode-hook)
(add-hook 'c++-mode-hook 'my-c-mode-hook)

(global-set-key (kbd "C-M-r") 'revert-buffer)


;;; Move Text
(rc/require 'move-text)
(global-set-key (kbd "M-p") 'move-text-up)
(global-set-key (kbd "M-n") 'move-text-down)

;;; Skip spaces
(global-set-key (kbd "C-M-f") (lambda () (interactive) (forward-whitespace 1)))
(global-set-key (kbd "C-M-b") (lambda () (interactive) (forward-whitespace -1)))

;;; Window Switching
(windmove-default-keybindings)
(use-package ace-window
  :ensure t
  :bind ("M-o" . ace-window))

;;; Line Deletion
(defun my/delete-line (&optional arg)
  (interactive "p")
  (let ((start (point)))
    (if (and (eolp) (not (eq (point) (point-max))))
        (delete-region (point) (1+ (point)))
      (delete-region start (progn (move-end-of-line arg) (point))))))

(global-set-key (kbd "C-c k") 'kill-line)
(global-set-key (kbd "C-k") 'my/delete-line)

;;; Code suggestions
(use-package company
  :ensure t
  :init (global-company-mode))

(use-package company-dabbrev
  :ensure nil ; comes with company
  :after company
  :config
  (setq company-dabbrev-other-buffers t
        company-dabbrev-ignore-case t))

;;; Multiple cursors
(use-package multiple-cursors
  :ensure t
  :bind (("C-S-c C-S-c" . mc/edit-lines)
         ("C->"         . mc/mark-next-like-this)
         ("C-<"         . mc/mark-previous-like-this)
         ("C-c C-<"     . mc/mark-all-like-this)))

;;; Auto saving

(setq auto-save-file-name-transforms
          `((".*" ,(concat user-emacs-directory "auto-save/") t))) 

(setq backup-directory-alist
      `(("." . ,(expand-file-name
                 (concat user-emacs-directory "backups")))))

;;; Compilation
(require 'project)

(defun compile-in-project-root()
 " Run compile, prompting for command, from the project root."
  (interactive)
  (let ((default-directory (project-root (project-current t))))
    (call-interactively 'compile)))
(global-set-key (kbd "C-c c") 'compile)
(global-set-key (kbd "C-c p") 'compile-in-project-root)

(setq display-buffer-alist
      '(("\\*compilation\\*"
         (display-buffer-reuse-window display-buffer-at-bottom)
         (window-height . 0.4))))
(setq compilation-scroll-output t)

(defun my-resize-window-to-percent (direction percent)
  "Resize current window in DIRECTION ('horizontal or 'vertical) to PERCENT of the frame."
  (let* ((frame-size (if (eq direction 'horizontal)
                         (frame-width)
                       (frame-height)))
         (target-size (floor (* frame-size (/ percent 100.0)))))
    (if (eq direction 'horizontal)
        (adjust-window-trailing-edge (selected-window)
                                     (- target-size (window-width))
                                     t)
      (adjust-window-trailing-edge (selected-window)
                                   (- target-size (window-height))
                                   nil))))

;; Resize window
(global-set-key (kbd "C-c w <up>")    (lambda () (interactive) (my-resize-window-to-percent 'vertical 40)))
(global-set-key (kbd "C-c w <down>")  (lambda () (interactive) (my-resize-window-to-percent 'vertical 70)))


(rc/require 'cl-lib)
(rc/require 'magit)
(global-set-key (kbd "C-x g") 'magit-status)

(global-set-key (kbd "<backtab>") (lambda () (interactive) (indent-rigidly (region-beginning) (region-end) -4)))
