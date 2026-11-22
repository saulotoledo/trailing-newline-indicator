;;; trailing-newline-indicator-test.el --- ERT tests -*- lexical-binding: t; -*-

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; ERT tests for the trailing-newline-indicator package.
;;
;; Run locally with:
;;
;;   make test
;;
;; or directly:
;;
;;   emacs --batch -Q -L . \
;;     -l trailing-newline-indicator.el \
;;     -l tests/trailing-newline-indicator-test.el \
;;     -f ert-run-tests-batch-and-exit

;;; Code:

(require 'ert)
(require 'display-line-numbers) ; ensure display-line-numbers is defvar'd as special
(defvar display-line-numbers-offset) ; Emacs 27+; ensure dynamic binding on 26.x
(require 'trailing-newline-indicator)

;;; Helpers

(defmacro tnli-test--with-buffer (content &rest body)
  "Evaluate BODY in a temporary buffer pre-filled with CONTENT.
Always disables the mode in the unwind handler, even on test failure."
  (declare (indent 1))
  `(with-temp-buffer
     (insert ,content)
     (unwind-protect
         (progn ,@body)
       (when (bound-and-true-p trailing-newline-indicator-mode)
         (trailing-newline-indicator-mode -1)))))

(defun tnli-test--before-string ()
  "Return the before-string of the active indicator overlay, or nil."
  (and trailing-newline-indicator--overlay
       (overlay-get trailing-newline-indicator--overlay 'before-string)))

;;; Overlay presence

(ert-deftest tnli/overlay-created-with-trailing-newline ()
  "An overlay is created when the buffer ends with a newline."
  (tnli-test--with-buffer "line1\nline2\n"
    (trailing-newline-indicator-mode 1)
    (should (overlayp trailing-newline-indicator--overlay))))

(ert-deftest tnli/no-overlay-without-trailing-newline ()
  "No overlay is created when the buffer does not end with a newline."
  (tnli-test--with-buffer "line1\nno-trailing-newline"
    (trailing-newline-indicator-mode 1)
    (should-not trailing-newline-indicator--overlay)))

(ert-deftest tnli/no-overlay-in-empty-buffer ()
  "No overlay is created for an empty buffer."
  (tnli-test--with-buffer ""
    (trailing-newline-indicator-mode 1)
    (should-not trailing-newline-indicator--overlay)))

(ert-deftest tnli/overlay-removed-on-mode-disable ()
  "The overlay is deleted when the mode is turned off."
  (tnli-test--with-buffer "line1\n"
    (trailing-newline-indicator-mode 1)
    (should trailing-newline-indicator--overlay)
    (trailing-newline-indicator-mode -1)
    (should-not trailing-newline-indicator--overlay)))

;;; Overlay position

(ert-deftest tnli/overlay-spans-point-max ()
  "The overlay is a zero-width overlay anchored at (point-max)."
  (tnli-test--with-buffer "line1\nline2\n"
    (trailing-newline-indicator-mode 1)
    (should (= (overlay-start trailing-newline-indicator--overlay) (point-max)))
    (should (= (overlay-end   trailing-newline-indicator--overlay) (point-max)))))

(ert-deftest tnli/overlay-advances-on-insert-at-point-max ()
  "Overlay stays zero-width at (point-max) when text is inserted at EOF."
  (tnli-test--with-buffer "line1\n"
    (trailing-newline-indicator-mode 1)
    (let ((inhibit-modification-hooks t)
          (ov trailing-newline-indicator--overlay))
      (goto-char (point-max))
      (insert "line2\n")
      (should (= (overlay-start ov) (point-max)))
      (should (= (overlay-end   ov) (point-max))))))

;;; Ghost-text overlay properties

(ert-deftest tnli/overlay-uses-before-string-not-after-string ()
  "The overlay uses `before-string' (ghost text), never `after-string'."
  (tnli-test--with-buffer "line1\n"
    (trailing-newline-indicator-mode 1)
    (should     (overlay-get trailing-newline-indicator--overlay 'before-string))
    (should-not (overlay-get trailing-newline-indicator--overlay 'after-string))))

(ert-deftest tnli/before-string-first-char-has-cursor-property ()
  "The first character of before-string has `cursor t' for ghost-text placement."
  (tnli-test--with-buffer "line1\n"
    (trailing-newline-indicator-mode 1)
    (let ((str (tnli-test--before-string)))
      (should (and str (> (length str) 0)))
      (should (get-text-property 0 'cursor str)))))

;;; Symbol content

(ert-deftest tnli/before-string-starts-with-default-symbol ()
  "The before-string starts with the default ⏎ symbol."
  (tnli-test--with-buffer "line1\n"
    (let ((trailing-newline-indicator-newline-symbol "⏎")
          (trailing-newline-indicator-show-line-number nil))
      (trailing-newline-indicator-mode 1)
      (should (string-prefix-p "⏎" (substring-no-properties
                                    (tnli-test--before-string)))))))

(ert-deftest tnli/custom-symbol-reflected-in-before-string ()
  "A custom newline symbol is used when changed."
  (tnli-test--with-buffer "line1\n"
    (let ((trailing-newline-indicator-newline-symbol "↵")
          (trailing-newline-indicator-show-line-number nil))
      (trailing-newline-indicator-mode 1)
      (should (string-prefix-p "↵" (substring-no-properties
                                    (tnli-test--before-string)))))))

(ert-deftest tnli/symbol-carries-line-number-face ()
  "The newline symbol is propertized with the `line-number' face."
  (tnli-test--with-buffer "line1\n"
    (let ((trailing-newline-indicator-show-line-number nil))
      (trailing-newline-indicator-mode 1)
      (should (eq 'line-number
                  (get-text-property 0 'face (tnli-test--before-string)))))))

(ert-deftest tnli/no-overlay-when-symbol-empty-and-line-number-hidden ()
  "No overlay and no error when symbol is empty and line numbers are not shown."
  (tnli-test--with-buffer "line1\n"
    (let ((trailing-newline-indicator-newline-symbol "")
          (trailing-newline-indicator-show-line-number nil)
          (display-line-numbers t))
      (trailing-newline-indicator-mode 1)
      (should-not trailing-newline-indicator--overlay)))
  (tnli-test--with-buffer "line1\n"
    (let ((trailing-newline-indicator-newline-symbol "")
          (trailing-newline-indicator-show-line-number t)
          (display-line-numbers nil))
      (trailing-newline-indicator-mode 1)
      (should-not trailing-newline-indicator--overlay))))

(ert-deftest tnli/line-number-only-without-leading-space-when-symbol-empty ()
  "When symbol is empty and line numbers are active, show only the number w/o space."
  (tnli-test--with-buffer "line1\nline2\n"
    (let ((trailing-newline-indicator-newline-symbol "")
          (trailing-newline-indicator-show-line-number t)
          (display-line-numbers t))
      (trailing-newline-indicator-mode 1)
      (let ((str (tnli-test--before-string)))
        (should (equal "3" (substring-no-properties str)))
        (should (eq 'trailing-newline-indicator-small-number
                    (get-text-property 0 'face str)))
        (should (get-text-property 0 'cursor str))))))

;;; Line number content

(ert-deftest tnli/line-number-shown-when-display-line-numbers-active ()
  "A line number is appended when `display-line-numbers' is non-nil."
  (tnli-test--with-buffer "line1\nline2\n"
    (let ((trailing-newline-indicator-show-line-number t)
          (display-line-numbers t))
      (trailing-newline-indicator-mode 1)
      (should (string-match-p "[[:space:]][[:digit:]]+"
                              (substring-no-properties (tnli-test--before-string)))))))

(ert-deftest tnli/no-line-number-when-display-line-numbers-inactive ()
  "No line number is appended when `display-line-numbers' is nil."
  (tnli-test--with-buffer "line1\nline2\n"
    (let ((trailing-newline-indicator-show-line-number t)
          (trailing-newline-indicator-newline-symbol "⏎")
          (display-line-numbers nil))
      (trailing-newline-indicator-mode 1)
      (should (equal "⏎" (substring-no-properties (tnli-test--before-string)))))))

(ert-deftest tnli/no-line-number-when-option-disabled ()
  "No line number when `trailing-newline-indicator-show-line-number' is nil.
This happens regardless of `display-line-numbers'."
  (tnli-test--with-buffer "line1\nline2\n"
    (let ((trailing-newline-indicator-show-line-number nil)
          (trailing-newline-indicator-newline-symbol "⏎")
          (display-line-numbers t))
      (trailing-newline-indicator-mode 1)
      (should (equal "⏎" (substring-no-properties (tnli-test--before-string)))))))

(ert-deftest tnli/skips-line-number-at-pos-when-line-numbers-hidden ()
  "`line-number-at-pos' is not called when line numbers are not shown."
  (tnli-test--with-buffer "line1\nline2\n"
    (let ((called nil)
          (trailing-newline-indicator-show-line-number nil)
          (display-line-numbers t))
      (cl-letf (((symbol-function 'line-number-at-pos)
                 (lambda (&rest _) (setq called t) 1)))
        (trailing-newline-indicator-mode 1)
        (should-not called)))))

(ert-deftest tnli/line-number-value-is-one-plus-last-line ()
  "The displayed line number equals one past the last line."
  (tnli-test--with-buffer "a\nb\nc\n"
    (let ((trailing-newline-indicator-show-line-number t)
          (display-line-numbers t))
      (trailing-newline-indicator-mode 1)
      (let ((expected (1+ (line-number-at-pos (1- (point-max)))))
            (str      (substring-no-properties (tnli-test--before-string))))
        (should (string-match-p (number-to-string expected) str))))))

(ert-deftest tnli/line-number-carries-small-number-face ()
  "The line-number portion uses `trailing-newline-indicator-small-number' face."
  (tnli-test--with-buffer "line1\n"
    (let ((trailing-newline-indicator-show-line-number t)
          (display-line-numbers t))
      (trailing-newline-indicator-mode 1)
      (let* ((str     (tnli-test--before-string))
             ;; The symbol occupies positions 0..(sym-len - 1); line number starts at sym-len.
             (sym-len (length trailing-newline-indicator-newline-symbol)))
        (should (eq 'trailing-newline-indicator-small-number
                    (get-text-property sym-len 'face str)))))))

(ert-deftest tnli/line-number-respects-display-line-numbers-offset ()
  "Absolute line number includes `display-line-numbers-offset' and uses `abs'."
  (tnli-test--with-buffer "a\nb\nc\n"
    (let ((trailing-newline-indicator-newline-symbol "⏎")
          (trailing-newline-indicator-show-line-number t)
          (display-line-numbers t)
          (display-line-numbers-offset 10))
      (trailing-newline-indicator-mode 1)
      (should (equal "⏎ 14" (substring-no-properties (tnli-test--before-string))))
      (setq display-line-numbers-offset -10)
      (trailing-newline-indicator--update-indicator)
      (should (equal "⏎ 6" (substring-no-properties (tnli-test--before-string)))))))

(ert-deftest tnli/relative-line-number-from-earlier-line ()
  "In `relative' mode, line number shows distance from point to trailing line."
  (tnli-test--with-buffer "a\nb\nc\n"
    (let ((trailing-newline-indicator-newline-symbol "⏎")
          (trailing-newline-indicator-show-line-number t)
          (display-line-numbers 'relative))
      (goto-char (point-min)) ; line 1 -> EOF is line 4 (distance 3)
      (trailing-newline-indicator-mode 1)
      (should (equal "⏎ 3" (substring-no-properties (tnli-test--before-string))))
      (forward-line 2) ; line 3 -> distance 1
      (trailing-newline-indicator--update-indicator)
      (should (equal "⏎ 1" (substring-no-properties (tnli-test--before-string)))))))

(ert-deftest tnli/relative-line-number-at-eof-respects-current-absolute ()
  "At EOF in `relative' mode, number respects `current-absolute' and ignores offset."
  (tnli-test--with-buffer "a\nb\nc\n"
    (let ((trailing-newline-indicator-newline-symbol "⏎")
          (trailing-newline-indicator-show-line-number t)
          (display-line-numbers 'relative)
          (display-line-numbers-current-absolute t)
          (display-line-numbers-offset 5))
      (goto-char (point-max))
      (trailing-newline-indicator-mode 1)
      (should (equal "⏎ 4" (substring-no-properties (tnli-test--before-string))))
      (narrow-to-region 5 (point-max)) ; "c\n" -> offset must NOT force widening
      (let ((display-line-numbers-widen nil))
        (trailing-newline-indicator--update-indicator)
        (should (equal "⏎ 2" (substring-no-properties (tnli-test--before-string)))))
      (widen)
      (setq display-line-numbers-current-absolute nil)
      (trailing-newline-indicator--update-indicator)
      (should (equal "⏎ 0" (substring-no-properties (tnli-test--before-string)))))))

(ert-deftest tnli/visual-line-numbers-omits-line-number ()
  "In `visual' mode, no line number is displayed (symbol only)."
  (tnli-test--with-buffer "a\nb\nc\n"
    (let ((trailing-newline-indicator-newline-symbol "⏎")
          (trailing-newline-indicator-show-line-number t)
          (display-line-numbers 'visual))
      (trailing-newline-indicator-mode 1)
      (should (equal "⏎" (substring-no-properties (tnli-test--before-string))))))
  (tnli-test--with-buffer "a\nb\nc\n"
    (let ((trailing-newline-indicator-newline-symbol "")
          (trailing-newline-indicator-show-line-number t)
          (display-line-numbers 'visual))
      (trailing-newline-indicator-mode 1)
      (should-not trailing-newline-indicator--overlay))))

(ert-deftest tnli/offset-forces-widened-line-number-when-narrowed ()
  "Non-zero `display-line-numbers-offset' counts from buffer start even if widen is nil."
  (tnli-test--with-buffer "line1\nline2\nline3\n"
    (narrow-to-region 13 (point-max)) ; "line3\n" (line 3 of full buffer -> EOF is 4)
    (let ((trailing-newline-indicator-newline-symbol "⏎")
          (trailing-newline-indicator-show-line-number t)
          (display-line-numbers t)
          (display-line-numbers-widen nil)
          (display-line-numbers-offset 10))
      (trailing-newline-indicator-mode 1)
      (should (equal "⏎ 14" (substring-no-properties (tnli-test--before-string)))))))

(ert-deftest tnli/non-t-truthy-display-line-numbers-treated-as-absolute ()
  "Any non-nil `display-line-numbers' other than `relative'/`visual' is absolute."
  (tnli-test--with-buffer "a\nb\nc\n"
    (let ((trailing-newline-indicator-newline-symbol "⏎")
          (trailing-newline-indicator-show-line-number t)
          (display-line-numbers 'absolute))
      (trailing-newline-indicator-mode 1)
      (should (equal "⏎ 4" (substring-no-properties (tnli-test--before-string)))))))

(ert-deftest tnli/skips-line-number-at-pos-in-visual-mode ()
  "`line-number-at-pos' is not called in `visual' mode."
  (tnli-test--with-buffer "line1\nline2\n"
    (let ((called nil)
          (trailing-newline-indicator-show-line-number t)
          (display-line-numbers 'visual))
      (cl-letf (((symbol-function 'line-number-at-pos)
                 (lambda (&rest _) (setq called t) 1)))
        (trailing-newline-indicator-mode 1)
        (should-not called)))))

(ert-deftest tnli/update-skips-recalc-and-reuses-overlay-when-unchanged ()
  "Repeated updates with unchanged state skip `line-number-at-pos' and keep overlay."
  (tnli-test--with-buffer "line1\nline2\n"
    (let ((trailing-newline-indicator-show-line-number t)
          (display-line-numbers t))
      (trailing-newline-indicator-mode 1)
      (let ((ov-before trailing-newline-indicator--overlay)
            (called nil))
        (goto-char (point-min))
        (cl-letf (((symbol-function 'line-number-at-pos)
                   (lambda (&rest _) (setq called t) 1)))
          (run-hooks 'post-command-hook)
          (should-not called)
          (should (eq ov-before trailing-newline-indicator--overlay)))))))

(ert-deftest tnli/update-reuses-overlay-in-place-on-buffer-edit ()
  "Editing the buffer moves and updates the existing overlay in place."
  (tnli-test--with-buffer "line1\n"
    (let ((trailing-newline-indicator-newline-symbol "⏎")
          (trailing-newline-indicator-show-line-number t)
          (display-line-numbers t))
      (trailing-newline-indicator-mode 1)
      (let ((ov-before trailing-newline-indicator--overlay))
        (goto-char (point-max))
        (insert "line2\n")
        (should (eq ov-before trailing-newline-indicator--overlay))
        (should (= (overlay-start ov-before) (point-max)))
        (should (equal "⏎ 3" (substring-no-properties (tnli-test--before-string))))))))

(ert-deftest tnli/relative-mode-skips-recalc-on-same-line-move ()
  "In `relative' mode, horizontal move on same line skips `line-number-at-pos'."
  (tnli-test--with-buffer "abcdef\nline2\n"
    (let ((trailing-newline-indicator-newline-symbol "⏎")
          (trailing-newline-indicator-show-line-number t)
          (display-line-numbers 'relative))
      (goto-char (point-min))
      (trailing-newline-indicator-mode 1)
      (should (equal "⏎ 2" (substring-no-properties (tnli-test--before-string))))
      (let ((called nil))
        (forward-char 3) ; still on line 1
        (cl-letf (((symbol-function 'line-number-at-pos)
                   (lambda (&rest _) (setq called t) 1)))
          (run-hooks 'post-command-hook)
          (should-not called)))
      (forward-line 1) ; now on line 2 -> distance 1
      (run-hooks 'post-command-hook)
      (should (equal "⏎ 1" (substring-no-properties (tnli-test--before-string)))))))

(ert-deftest tnli/recovers-after-external-overlay-deletion-or-move ()
  "External deletion or relocation of overlay is repaired even when state matches."
  (tnli-test--with-buffer "line1\n"
    (trailing-newline-indicator-mode 1)
    (let ((ov trailing-newline-indicator--overlay))
      (should (overlayp ov))
      (move-overlay ov 1 1)
      (run-hooks 'post-command-hook)
      (should (= (overlay-start trailing-newline-indicator--overlay) (point-max)))
      (delete-overlay trailing-newline-indicator--overlay)
      (run-hooks 'post-command-hook)
      (should (overlayp trailing-newline-indicator--overlay))
      (should (eq (overlay-buffer trailing-newline-indicator--overlay) (current-buffer))))))

(ert-deftest tnli/ensure-overlay-sweeps-orphan-on-creation ()
  "Creating a new overlay sweeps any orphaned indicator overlay at EOF."
  (tnli-test--with-buffer "line1\n"
    (let ((orphan (make-overlay (point-max) (point-max))))
      (overlay-put orphan 'trailing-newline-indicator t)
      (trailing-newline-indicator-mode 1)
      (should-not (overlay-buffer orphan))
      (should (overlayp trailing-newline-indicator--overlay)))))

(ert-deftest tnli/edit-then-post-command-hook-skips-duplicate-recalc ()
  "Following an edit, `post-command-hook' hits cache and skips `line-number-at-pos'."
  (tnli-test--with-buffer "line1\n"
    (let ((trailing-newline-indicator-show-line-number t)
          (display-line-numbers t))
      (trailing-newline-indicator-mode 1)
      (goto-char (point-max))
      (insert "line2\n") ; triggers after-change-functions
      (let ((called nil))
        (cl-letf (((symbol-function 'line-number-at-pos)
                   (lambda (&rest _) (setq called t) 1)))
          (run-hooks 'post-command-hook)
          (should-not called))))))

(ert-deftest tnli/formatter-command-updates-line-number ()
  "A command replacing buffer content updates the line number accurately.
Covers both normal edits and edits with `inhibit-modification-hooks' non-nil."
  (tnli-test--with-buffer "a\nb\n"
    (let ((trailing-newline-indicator-newline-symbol "⏎")
          (trailing-newline-indicator-show-line-number t)
          (display-line-numbers t))
      (trailing-newline-indicator-mode 1)
      (should (equal "⏎ 3" (substring-no-properties (tnli-test--before-string))))
      ;; Normal formatter replacing buffer with more lines.
      (erase-buffer)
      (insert "1\n2\n3\n4\n5\n")
      (run-hooks 'post-command-hook)
      (should (equal "⏎ 6" (substring-no-properties (tnli-test--before-string))))
      ;; Formatter running with `inhibit-modification-hooks' t (skips after-change).
      (let ((inhibit-modification-hooks t))
        (erase-buffer)
        (insert "only1\nonly2\n"))
      (run-hooks 'post-command-hook)
      (should (equal "⏎ 3" (substring-no-properties (tnli-test--before-string)))))))

(ert-deftest tnli/formatter-removing-lines-updates-line-number-and-position ()
  "A formatter removing lines lowers the line number and keeps overlay at EOF."
  (tnli-test--with-buffer "1\n2\n3\n4\n5\n6\n"
    (let ((trailing-newline-indicator-newline-symbol "⏎")
          (trailing-newline-indicator-show-line-number t)
          (display-line-numbers t))
      (trailing-newline-indicator-mode 1)
      (let ((ov trailing-newline-indicator--overlay))
        (should (equal "⏎ 7" (substring-no-properties (tnli-test--before-string))))
        ;; Remove lines 3..5 ("3\n4\n5\n") -> 3 lines left, EOF line is 4.
        (goto-char (point-min))
        (forward-line 2)
        (delete-region (point) (progn (forward-line 3) (point)))
        (run-hooks 'post-command-hook)
        (should (eq ov trailing-newline-indicator--overlay))
        (should (= (overlay-start ov) (point-max)))
        (should (= (overlay-end   ov) (point-max)))
        (should (equal "⏎ 4" (substring-no-properties (tnli-test--before-string))))))))

(ert-deftest tnli/horizontal-modification-keeps-line-number-and-position ()
  "Inserting/deleting characters on a line keeps line number and zero-width EOF overlay."
  (tnli-test--with-buffer "foo\nbar\n"
    (let ((trailing-newline-indicator-newline-symbol "⏎")
          (trailing-newline-indicator-show-line-number t)
          (display-line-numbers t))
      (trailing-newline-indicator-mode 1)
      (let ((ov trailing-newline-indicator--overlay))
        (should (equal "⏎ 3" (substring-no-properties (tnli-test--before-string))))
        ;; Expand line 1 horizontally.
        (goto-char (point-min))
        (insert "prefix-")
        (run-hooks 'post-command-hook)
        (should (eq ov trailing-newline-indicator--overlay))
        (should (= (overlay-start ov) (point-max)))
        (should (= (overlay-end   ov) (point-max)))
        (should (equal "⏎ 3" (substring-no-properties (tnli-test--before-string))))
        ;; Shrink line 2 horizontally.
        (forward-line 1)
        (delete-char 2)
        (run-hooks 'post-command-hook)
        (should (eq ov trailing-newline-indicator--overlay))
        (should (= (overlay-start ov) (point-max)))
        (should (= (overlay-end   ov) (point-max)))
        (should (equal "⏎ 3" (substring-no-properties (tnli-test--before-string))))))))

(ert-deftest tnli/symbol-text-property-change-invalidates-cache ()
  "Changing text properties on the newline symbol invalidates the state cache."
  (tnli-test--with-buffer "line1\n"
    (let ((trailing-newline-indicator-newline-symbol "⏎")
          (trailing-newline-indicator-show-line-number nil))
      (trailing-newline-indicator-mode 1)
      (should-not (get-text-property 0 'help-echo (tnli-test--before-string)))
      (setq trailing-newline-indicator-newline-symbol
            (propertize "⏎" 'help-echo "EOF newline"))
      (run-hooks 'post-command-hook)
      (should (equal "EOF newline"
                     (get-text-property 0 'help-echo (tnli-test--before-string)))))))

;;; Hooks

(ert-deftest tnli/hooks-installed-on-enable ()
  "All expected buffer-local hooks are added when the mode is enabled."
  (tnli-test--with-buffer "line1\n"
    (trailing-newline-indicator-mode 1)
    (should (memq #'trailing-newline-indicator--update-indicator after-change-functions))
    (should (memq #'trailing-newline-indicator--update-indicator after-save-hook))
    (should (memq #'trailing-newline-indicator--update-indicator after-revert-hook))
    (should (memq #'trailing-newline-indicator--update-indicator post-command-hook))
    (should (memq #'trailing-newline-indicator--on-kill-buffer   kill-buffer-hook))))

(ert-deftest tnli/hooks-removed-on-disable ()
  "All buffer-local hooks are removed when the mode is disabled."
  (tnli-test--with-buffer "line1\n"
    (trailing-newline-indicator-mode 1)
    (trailing-newline-indicator-mode -1)
    (should-not (memq #'trailing-newline-indicator--update-indicator after-change-functions))
    (should-not (memq #'trailing-newline-indicator--update-indicator after-save-hook))
    (should-not (memq #'trailing-newline-indicator--update-indicator after-revert-hook))
    (should-not (memq #'trailing-newline-indicator--update-indicator post-command-hook))
    (should-not (memq #'trailing-newline-indicator--on-kill-buffer   kill-buffer-hook))))

(ert-deftest tnli/hook-list-covers-all-expected-hooks ()
  "The hook list returned by `trailing-newline-indicator--hook-list' is complete."
  (let ((hooks (trailing-newline-indicator--hook-list)))
    (should (memq 'after-change-functions hooks))
    (should (memq 'after-save-hook        hooks))
    (should (memq 'after-revert-hook      hooks))
    (should (memq 'post-command-hook      hooks))))

;;; Active count

(ert-deftest tnli/active-count-increments-on-enable ()
  "The global active-count increases by one when a buffer enables the mode."
  (tnli-test--with-buffer "line1\n"
    (let ((before trailing-newline-indicator--active-count))
      (trailing-newline-indicator-mode 1)
      (should (= (1+ before) trailing-newline-indicator--active-count)))))

(ert-deftest tnli/active-count-decrements-on-disable ()
  "The global active-count decreases by one when a buffer disables the mode."
  (tnli-test--with-buffer "line1\n"
    (trailing-newline-indicator-mode 1)
    (let ((after-enable trailing-newline-indicator--active-count))
      (trailing-newline-indicator-mode -1)
      (should (= (1- after-enable) trailing-newline-indicator--active-count)))))

(ert-deftest tnli/active-count-no-double-increment-without-trailing-newline ()
  "Re-enabling the mode without a trailing newline does not double-increment count."
  (let ((trailing-newline-indicator--active-count 0))
    (tnli-test--with-buffer "no-trailing-newline"
      (trailing-newline-indicator-mode 1)
      (should (= 1 trailing-newline-indicator--active-count))
      (trailing-newline-indicator-mode 1)
      (should (= 1 trailing-newline-indicator--active-count))
      (trailing-newline-indicator-mode -1)
      (should (= 0 trailing-newline-indicator--active-count)))))

(ert-deftest tnli/active-count-no-double-increment-after-newline-deleted ()
  "Deleting trailing newline then re-enabling does not double-increment count."
  (let ((trailing-newline-indicator--active-count 0))
    (tnli-test--with-buffer "line1\n"
      (trailing-newline-indicator-mode 1)
      (should (= 1 trailing-newline-indicator--active-count))
      (goto-char (point-max))
      (delete-char -1)
      (should-not trailing-newline-indicator--overlay)
      (trailing-newline-indicator-mode 1)
      (should (= 1 trailing-newline-indicator--active-count))
      (trailing-newline-indicator-mode -1)
      (should (= 0 trailing-newline-indicator--active-count)))))

(ert-deftest tnli/disable-is-idempotent ()
  "Disabling an already-disabled mode does not decrement active-count again."
  (let ((trailing-newline-indicator--active-count 1))
    (tnli-test--with-buffer "line1\n"
      (trailing-newline-indicator-mode 1)
      (should (= 2 trailing-newline-indicator--active-count))
      (trailing-newline-indicator-mode -1)
      (should (= 1 trailing-newline-indicator--active-count))
      (trailing-newline-indicator-mode -1)
      (should (= 1 trailing-newline-indicator--active-count)))))

(ert-deftest tnli/major-mode-hook-removed-when-active-count-reaches-zero ()
  "Global `after-change-major-mode-hook' entry is removed when count hits zero."
  (let ((trailing-newline-indicator--active-count 0)
        (after-change-major-mode-hook nil))
    (tnli-test--with-buffer "no-trailing-newline"
      (trailing-newline-indicator-mode 1)
      (trailing-newline-indicator-mode 1)
      (should (memq #'trailing-newline-indicator--restore-hooks
                    (default-value 'after-change-major-mode-hook)))
      (trailing-newline-indicator-mode -1)
      (should (= 0 trailing-newline-indicator--active-count))
      (should-not (memq #'trailing-newline-indicator--restore-hooks
                        (default-value 'after-change-major-mode-hook))))))

(ert-deftest tnli/disable-cleans-up-after-hooks-cleared ()
  "Disabling still decrements count and removes overlay if hooks were cleared."
  (let ((trailing-newline-indicator--active-count 0)
        (after-change-major-mode-hook nil))
    (tnli-test--with-buffer "line1\n"
      (trailing-newline-indicator-mode 1)
      (should (= 1 trailing-newline-indicator--active-count))
      (should trailing-newline-indicator--overlay)
      (setq-local kill-buffer-hook nil)
      (trailing-newline-indicator-mode -1)
      (should (= 0 trailing-newline-indicator--active-count))
      (should-not trailing-newline-indicator--overlay)
      (should-not (memq #'trailing-newline-indicator--restore-hooks
                        (default-value 'after-change-major-mode-hook))))))

(ert-deftest tnli/reenable-after-hooks-cleared-does-not-double-increment ()
  "Re-enabling after hooks were cleared restores hooks without double-counting."
  (let ((trailing-newline-indicator--active-count 0))
    (tnli-test--with-buffer "no-trailing-newline"
      (trailing-newline-indicator-mode 1)
      (should (= 1 trailing-newline-indicator--active-count))
      (setq-local kill-buffer-hook nil)
      (trailing-newline-indicator-mode 1)
      (should (= 1 trailing-newline-indicator--active-count))
      (should (memq #'trailing-newline-indicator--on-kill-buffer kill-buffer-hook)))))

;;; Overlay management helpers

(ert-deftest tnli/delete-overlay-nils-variable ()
  "`trailing-newline-indicator--delete-overlay' sets the overlay variable to nil."
  (tnli-test--with-buffer "line1\n"
    (trailing-newline-indicator-mode 1)
    (should trailing-newline-indicator--overlay)
    (trailing-newline-indicator--delete-overlay)
    (should-not trailing-newline-indicator--overlay)))

(ert-deftest tnli/overlay-has-identifying-property ()
  "The indicator overlay carries the `trailing-newline-indicator' property."
  (tnli-test--with-buffer "line1\n"
    (trailing-newline-indicator-mode 1)
    (should (eq t (overlay-get trailing-newline-indicator--overlay
                               'trailing-newline-indicator)))))

(ert-deftest tnli/preserves-other-overlays-at-point-max ()
  "Updating or disabling the mode does not delete foreign overlays at (point-max)."
  (tnli-test--with-buffer "line1\n"
    (let ((foreign-ov (make-overlay (point-max) (point-max))))
      (overlay-put foreign-ov 'foreign-package t)
      (trailing-newline-indicator-mode 1)
      (trailing-newline-indicator--update-indicator)
      (should (overlay-buffer foreign-ov))
      (trailing-newline-indicator--delete-overlay)
      (should (overlay-buffer foreign-ov))
      (trailing-newline-indicator-mode -1)
      (should (overlay-buffer foreign-ov))
      (delete-overlay foreign-ov))))

(ert-deftest tnli/delete-overlay-removes-orphaned-indicator-overlays ()
  "Orphaned overlays tagged `trailing-newline-indicator' at EOF are cleaned up."
  (tnli-test--with-buffer "line1\n"
    (let ((orphan-ov  (make-overlay (point-max) (point-max)))
          (foreign-ov (make-overlay (point-max) (point-max))))
      (overlay-put orphan-ov  'trailing-newline-indicator t)
      (overlay-put foreign-ov 'foreign-package t)
      (setq trailing-newline-indicator--overlay nil)
      (trailing-newline-indicator--delete-overlay)
      (should-not (overlay-buffer orphan-ov))
      (should (overlay-buffer foreign-ov))
      (delete-overlay foreign-ov))))

(ert-deftest tnli/enable-is-idempotent ()
  "Enabling an already-enabled mode does not create a second overlay.
It also does not increment the active-count."
  (tnli-test--with-buffer "line1\n"
    (trailing-newline-indicator-mode 1)
    (let ((count-1st trailing-newline-indicator--active-count)
          (ov-1st    trailing-newline-indicator--overlay))
      (trailing-newline-indicator-mode 1)
      (should (=  count-1st trailing-newline-indicator--active-count))
      (should (eq ov-1st    trailing-newline-indicator--overlay)))))

;;; Live overlay updates

(ert-deftest tnli/overlay-cleared-after-trailing-newline-removed ()
  "The overlay disappears when the trailing newline is deleted from the buffer."
  (tnli-test--with-buffer "line1\n"
    (trailing-newline-indicator-mode 1)
    (should trailing-newline-indicator--overlay)
    (goto-char (point-max))
    (delete-char -1)
    (trailing-newline-indicator--update-indicator)
    (should-not trailing-newline-indicator--overlay)))

(ert-deftest tnli/overlay-appears-after-trailing-newline-inserted ()
  "The overlay is created when a trailing newline is added to the buffer."
  (tnli-test--with-buffer "no-trailing-newline"
    (trailing-newline-indicator-mode 1)
    (should-not trailing-newline-indicator--overlay)
    (goto-char (point-max))
    (insert "\n")
    (trailing-newline-indicator--update-indicator)
    (should (overlayp trailing-newline-indicator--overlay))))

;;; Narrowing

(ert-deftest tnli/no-overlay-when-narrowed-before-eof ()
  "No overlay is shown when buffer is narrowed before the real end of buffer."
  (tnli-test--with-buffer "line1\nline2\nline3\n"
    (narrow-to-region 7 13) ; "line2\n"
    (trailing-newline-indicator-mode 1)
    (should-not trailing-newline-indicator--overlay)
    (widen)
    (trailing-newline-indicator--update-indicator)
    (should (overlayp trailing-newline-indicator--overlay))
    (should (= (overlay-start trailing-newline-indicator--overlay) (point-max)))))

(ert-deftest tnli/narrowed-to-eof-respects-display-line-numbers-widen ()
  "When narrowed to EOF, line number respects `display-line-numbers-widen'."
  (tnli-test--with-buffer "line1\nline2\nline3\n"
    (narrow-to-region 13 (point-max)) ; "line3\n"
    (let ((trailing-newline-indicator-newline-symbol "⏎")
          (trailing-newline-indicator-show-line-number t)
          (display-line-numbers t)
          (display-line-numbers-widen nil))
      (trailing-newline-indicator-mode 1)
      (should (equal "⏎ 2" (substring-no-properties (tnli-test--before-string))))
      (setq display-line-numbers-widen t)
      (trailing-newline-indicator--update-indicator)
      (should (equal "⏎ 4" (substring-no-properties (tnli-test--before-string)))))))

(ert-deftest tnli/delete-overlay-cleans-eof-orphan-while-narrowed ()
  "Orphaned overlay at real EOF is cleaned up even while narrowed mid-file."
  (tnli-test--with-buffer "line1\nline2\nline3\n"
    (let ((orphan-ov (make-overlay (point-max) (point-max))))
      (overlay-put orphan-ov 'trailing-newline-indicator t)
      (narrow-to-region 1 7)
      (trailing-newline-indicator--delete-overlay)
      (should-not (overlay-buffer orphan-ov)))))

(ert-deftest tnli/no-overlay-when-narrowed-to-empty-region-at-eof ()
  "No overlay when narrowed to a zero-width region at EOF.
Ensures `(not (eq (point-min) visible-max))' is preserved so the indicator
is hidden when the preceding newline is outside the narrowed view."
  (tnli-test--with-buffer "line1\nline2\n"
    (narrow-to-region (point-max) (point-max))
    (trailing-newline-indicator-mode 1)
    (should-not trailing-newline-indicator--overlay)))

(ert-deftest tnli/narrow-and-widen-update-via-post-command-hook ()
  "Narrowing and widening update the indicator via `post-command-hook'.
Emacs does not run `after-change-functions' on `narrow-to-region' or `widen'
because buffer text does not change.  This test guards that `post-command-hook'
(or an explicit replacement) keeps the indicator in sync across restriction
changes."
  (tnli-test--with-buffer "line1\nline2\nline3\n"
    (trailing-newline-indicator-mode 1)
    (should (overlayp trailing-newline-indicator--overlay))
    (narrow-to-region 1 13) ; before real EOF; after-change-functions does NOT run
    (run-hooks 'post-command-hook)
    (should-not trailing-newline-indicator--overlay)
    (widen) ; after-change-functions does NOT run
    (run-hooks 'post-command-hook)
    (should (overlayp trailing-newline-indicator--overlay))
    (should (= (overlay-start trailing-newline-indicator--overlay) (point-max)))))

;;; Buffer suitability

(ert-deftest tnli/buffer-without-file-is-not-suitable ()
  "A buffer with no associated file is excluded from the global mode."
  (with-temp-buffer
    (should-not (trailing-newline-indicator--is-buffer-suitable-for-indicator-p))))

(ert-deftest tnli/buffer-with-mode-already-on-is-not-suitable ()
  "A buffer where the mode is already active is not re-enabled by the global mode."
  (tnli-test--with-buffer "line1\n"
    (let ((buffer-file-name "/tmp/tnli-test-file.txt"))
      (should (trailing-newline-indicator--is-buffer-suitable-for-indicator-p))
      (setq-local trailing-newline-indicator-mode t)
      (should-not (trailing-newline-indicator--is-buffer-suitable-for-indicator-p)))))

(provide 'trailing-newline-indicator-test)
;;; trailing-newline-indicator-test.el ends here
