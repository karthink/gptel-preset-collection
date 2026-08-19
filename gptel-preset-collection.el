;;; gptel-preset-collection.el --- Collection of presets for gptel  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Karthik Chikmagalur

;; Author: Karthik Chikmagalur <karthikchikmagalur@gmail.com>
;; Version: 0.1
;; Package-Requires: ((emacs "29.1") (compat "30.1.0.0") (gptel "0.9.9.5"))
;; Keywords: convenience, tools
;; URL: https://github.com/karthink/gptel-preset-collection

;; SPDX-License-Identifier: GPL-3.0-or-later

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;; This file is NOT part of GNU Emacs.

;;; Commentary:

;; This is a 

;;; Code:

(require 'gptel)
(eval-when-compile (require 'rx))

;;;; Preset to turn off streaming
(gptel-make-preset 'nostream :description "No streaming" :stream nil)

;;;; Presets for contextual actions
;;;;; expand: preset to expand shell commands and elisp forms in the prompt
(defun gpc--expand-elisp ()
  (text-property-search-backward 'gptel 'response)
  (while (search-forward "$:(" nil t)
    (let ((from (- (point) 3)) cmd-read)
      (forward-char -1)
      (condition-case nil
          (let* ((print-length nil)
                 (cmd (read (current-buffer)))
                 (res (eval cmd t)))
            (setq cmd-read (prin1-to-string cmd))
            (delete-region from (point))
            (if (stringp res)
                (insert res)
              (insert (prin1-to-string res))))
        (error (warn "Could not read elisp, skipping evaluation"))
        (:success (message "Substituted %s with eval result" cmd-read))))))

(defun gpc--expand-shell ()
  (text-property-search-backward 'gptel 'response)
  (while (search-forward "$:{" nil t)
    (let ((from (- (point) 3)) cmd-read)
      (forward-char -1)
      (condition-case nil
          (let* ((print-length nil)
                 (cmd
                  (buffer-substring-no-properties
                   (1+ (point)) (progn (forward-sexp) (1- (point)))))
                 (res (shell-command-to-string cmd)))
            (setq cmd-read cmd)
            (delete-region from (min (1+ (point)) (point-max)))
            (insert res))
        (error (warn "Could not read shell command, skipping evaluation"))
        (:success (message "Substituted %s with shell result" cmd-read))))))

(gptel-make-preset 'expand
  :description "JIT: expand $:{...} and $:(...) in the prompt"
  :prompt-transform-functions
   (list :append (list #'gpc--expand-shell #'gpc--expand-elisp)))

;;;;; json: preset to force a JSON schema on the response
(gptel-make-preset 'json
  :description "JIT only: use JSON schema following @json cookie"
  :schema '(:eval (buffer-substring-no-properties
                   (point) (point-max)))
  :post (lambda () (delete-region (point) (point-max)))
  :include-reasoning nil)

;;;;; notify: preset to send a notification when a response is complete
(cl-defun gpc--notify (&key title body timeout)
  (ignore-errors (require 'notifications))
  (cond
   ((fboundp 'w32-notification-notify)
    (let ((id (w32-notification-notify
	       :title title
	       :body body
	       :urgency 'low)))
      (run-with-timer
       timeout nil (lambda () (w32-notification-close id)))))
   ((fboundp 'ns-do-applescript)
    (ns-do-applescript
     (format "display notification \"%s\" with title \"%s\""
             (replace-regexp-in-string "\"" "#" body)
             (replace-regexp-in-string "\"" "#" title))))
   ((fboundp 'notifications-notify)
    (notifications-notify :title title :body body :urgency 'normal))
   ((executable-find "notify-send")
    (start-process
     "gptel-notification" nil "notify-send"
     "--urgency=normal" (format "--expire-time=%d" timeout)
     title body))
   (t (message "%s: %s" title body))))

(defun gpc--notify-response (info)
  (pcase-let (((map :position :tracking-marker :buffer) info))
    (when (buffer-live-p (marker-buffer position))
      (let ((resp-short
             (with-current-buffer (marker-buffer position)
               (if tracking-marker
                   (buffer-substring-no-properties
                    position (min tracking-marker
                                  (+ position 340)
                                  (point-max)))
                 "(See buffer for response)"))))
        (gpc--notify
         :title (format "gptel response (%s)" (buffer-name buffer))
         :body resp-short
         :timeout (ceiling (/ (length resp-short) 15.0)))))))
  
(gptel-make-preset 'notify
  :description "HOOK: Notify when the request completes or errors out"
  :prompt-transform-functions
  `(:append (,(lambda (fsm) (push #'gpc--notify-response
                             (plist-get (gptel-fsm-info fsm) :post))))))
  
;;;; Presets to send text from Emacs buffers
;;;;; include: preset to include buffers or files
(defvar gpc--capf-marker (make-marker))
(defvar gpc-capf-identifier-regexp
  (rx (seq "@" (or "include" "gptel-annotate")))
  "Regexp used for preset names that should take file/buffer names.")

(defun gpc-capf ()
  "Completion-at-point function for presets that read file/buffer names.

When point follows an \"@include\" trigger on the current line,
this provides completion over buffer names and file names relative to
`default-directory'.  Buffer/file names are inserted quoted."
  (save-excursion
    (when-let* ((p (point))
                (num (skip-chars-backward "^[:space:]\""))
                (prefix-pos
                 (re-search-backward gpc-capf-identifier-regexp
                                     (line-beginning-position) t)))
      (move-marker gpc--capf-marker (+ p num) (current-buffer))
      (list (+ p num) p
            ;; TODO: Add completion metadata/annotation support
            (completion-table-merge #'internal-complete-buffer
                                    #'read-file-name-internal)
            :exclusive 'no
            :exit-function
            (lambda (str status)
              (when (memq status '(finished))
                ;; We would prefer to use `completion-in-region--data' to
                ;; replace the match, but it's only populated by Emacs' default
                ;; completion system.
                ;; Handle offsets to accommodate existing "quotes"
                (let ((before (if (equal (char-before gpc--capf-marker) ?\")
                                  1 0))
                      (after  (if (equal (char-after) ?\") 1 0)))
                  (delete-region (- gpc--capf-marker before)
                                 (+ (point) after)))
                (move-marker gpc--capf-marker nil)
                ;; Insert quoted buffer/file name
                (insert (format "%S" (substring-no-properties str)))))))))

(defun gpc--parse-line (context)
  "Add file or buffer names after point to CONTEXT."
  (skip-syntax-forward " " (line-end-position))
  (while-let ((_ (not (or (eolp) (eobp))))
              (source (if (eq (char-after) ?\")
                          (read (current-buffer))
                        (prog1 (thing-at-point 'filename)
                          (goto-char (match-end 0))))))
    (skip-syntax-forward " " (line-end-position))
    (cl-callf substring-no-properties source)
    (cond
     ((buffer-live-p (get-buffer source))
      (push (get-buffer source) context))
     ((file-readable-p source)
      (if (or (file-regular-p source)
              (and (file-directory-p source)
                   (or noninteractive
                       (y-or-n-p
                        (format "Include ALL files in directory \"%s\"? "
                                (file-name-as-directory source))))))
          (if-let* (((gptel--file-binary-p source))
                    (mime (mailcap-file-name-to-mime-type source)))
              (push (list source :mime mime) context)
            (push source context))
        (user-error "Query canceled")))
     (t (unless (or noninteractive
                    (y-or-n-p
                     (format "Cannot find source \"%s\", \
continue with gptel request? " source)))
          (user-error "gptel query canceled"))
        (message "Ignoring source \"%s\", file not readable" source))))
  context)

(add-hook 'gptel-mode-hook #'gpc-capf)
(gptel-make-preset 'include
  :description "CONTEXT: Include file or buffer names following @include"
  :context `(:function ,#'gpc--parse-line))

;;;;; visible-text and visible-buffers: presets to send visible text or buffers
(defun gpc--frame-windows ()
  "Return all windows on frame that aren't gptel chat buffers."
  (delq (and-let* ((current-buf (window-buffer (selected-window)))
                   ((buffer-local-value 'gptel-mode current-buf)))
          (selected-window))
        (window-list)))

(defun gpc--add-window (win &optional visible)
  "Return"
  (let ((buf (window-buffer win)))
    (with-current-buffer buf
      (cond
       ((and-let* ((path (buffer-file-name))
                   ((gptel--file-binary-p path)))
          (if-let* ((mime (mailcap-file-name-to-mime-type path)))
              (list path :mime mime)
            (message "Ignoring file %s, mime type unknown or unsupported"
                     path)
            nil)))
       (t `(,buf ,@(and visible `(:bounds ((,(window-start win) . ,(window-end win)))))))))))

(gptel-make-preset 'visible-buffers
  :description "CONTEXT: Include the full text of all buffers visible in the frame."
  :context
  '(:eval (mapcar #'gpc--add-window (gpc--frame-windows))))

(gptel-make-preset 'visible-text
  :description "CONTEXT: Include visible text from all windows in the frame."
  :context
  '(:eval (let ((windows (gpc--frame-windows)))
            (cl-map 'list #'gpc--add-window
                    windows (make-list (length windows) t)))))

(provide 'gptel-preset-collection)
;;; gptel-preset-collection.el ends here

;; Local Variables:
;; read-symbol-shorthands: (("gpc-" . "gptel-preset-collection-"))
;; End:
