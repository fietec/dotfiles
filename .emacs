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

;;; C-Mode
(setq-default c-basic-offset 4)

;;; Move Text
(rc/require 'move-text)
(global-set-key (kbd "M-p") 'move-text-up)
(global-set-key (kbd "M-n") 'move-text-down)

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
         (window-height . 0.3))))
(setq compilation-scroll-output t)

(rc/require 'cl-lib)
(rc/require 'magit)
(global-set-key (kbd "C-x g") 'magit-status)

(global-set-key (kbd "<backtab>") (lambda () (interactive) (indent-rigidly (region-beginning) (region-end) -4)))
