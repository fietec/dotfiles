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

(global-set-key (kbd "C-c p") 'compile-in-project-root)
(global-set-key (kbd "C-c c") 'compile)

(rc/require 'cl-lib)
(rc/require 'magit)
(global-set-key (kbd "C-x g") 'magit-status)
