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

;;; C-Mode
(setq-default c-basic-offset 4)

;;; Move Text
(rc/require 'move-text)
(global-set-key (kbd "M-p") 'move-text-up)
(global-set-key (kbd "M-n") 'move-text-down)


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
