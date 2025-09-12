;;; home-row-expreg-diverted.el --- Return to origin after expansion via diverted  -*- lexical-binding: t; -*-

;; Author: bommbo
;; URL: https://github.com/bommbo/home-row-expreg-diverted
;; Version: 0.1.3
;; Package-Requires: ((emacs "30.1") (diverted "0.1"))
;; Keywords: convenience, region, expreg, diverted

;;; Commentary:

;; After `home-row-expreg-expand-with-letters' finishes, any command
;; listed in `home-row-expreg-diverted-commands' will automatically
;; return point to where the expansion started.
;;
;;   (require 'home-row-expreg-diverted)
;;   (add-to-list 'home-row-expreg-diverted-commands 'meow-save)
;;   (home-row-expreg-diverted-mode 1)

;;; Code:

(require 'diverted)

(defgroup home-row-expreg-diverted nil
  "Diverted-based jump-back for home-row-expreg."
  :group 'convenience
  :prefix "home-row-expreg-diverted-")

(defcustom home-row-expreg-diverted-commands
  nil
  "Commands which, when run right after an expansion, trigger jump-back.
Add symbols like \\='meow-save, \\='save-buffer, \\='indent-for-tab-command, etc."
  :type '(repeat symbol)
  :group 'home-row-expreg-diverted)

;; single-marker stack
(defvar home-row-expreg-diverted--origin-marker nil
  "Marker to original position before expansion.")

(defun home-row-expreg-diverted--push ()
  (setq home-row-expreg-diverted--origin-marker (point-marker)))

(defun home-row-expreg-diverted--pop ()
  (when (marker-position home-row-expreg-diverted--origin-marker)
    (goto-char home-row-expreg-diverted--origin-marker)
    (set-marker home-row-expreg-diverted--origin-marker nil)))

;; around advice for expansion (only push)
(defun home-row-expreg-diverted--around-expansion (orig-fun &rest args)
  (home-row-expreg-diverted--push)
  (apply orig-fun args))

;; around advice for each trigger command (pop this command)
(defun home-row-expreg-diverted--around-command (orig-fun &rest args)
  (prog1 (apply orig-fun args)
    (when (eq last-command 'home-row-expreg-expand-with-letters)
      (home-row-expreg-diverted--pop)
      (message "Returned to pre-expansion point"))))

;;;###autoload
(define-minor-mode home-row-expreg-diverted-mode
  "Use diverted event table to jump back after expansion."
  :global t
  :lighter " hred"
  (if home-row-expreg-diverted-mode
      (progn
        (advice-add 'home-row-expreg-expand-with-letters
                    :around #'home-row-expreg-diverted--around-expansion)
        (dolist (cmd home-row-expreg-diverted-commands)
          (advice-add cmd :around #'home-row-expreg-diverted--around-command)))
    (progn
      (advice-remove 'home-row-expreg-expand-with-letters
                     #'home-row-expreg-diverted--around-expansion)
      (dolist (cmd home-row-expreg-diverted-commands)
        (advice-remove cmd #'home-row-expreg-diverted--around-command)))))

(provide 'home-row-expreg-diverted)
;;; home-row-expreg-diverted.el ends here
