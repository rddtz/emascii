;;; emascii-test.el --- Tests for emascii -*- lexical-binding: t; -*-

;;; Commentary:

;; Run with: make test

;;; Code:

(require 'ert)
(require 'emascii)
(require 'emascii-gallery)

(defmacro emascii-test-with-canvas (text &rest body)
  "Run BODY in a fresh canvas holding TEXT."
  (declare (indent 1))
  `(with-temp-buffer
     (insert ,text)
     (goto-char (point-min))
     (emascii-mode)
     (buffer-enable-undo)
     (switch-to-buffer (current-buffer))
     (transient-mark-mode 1)
     ,@body))

(defun emascii-test-keys (keys)
  "Run KEYS through the command loop."
  (execute-kbd-macro (kbd keys)))

;;;; Grid

(ert-deftest emascii-goto-cell-pads ()
  (emascii-test-with-canvas "ab"
    (emascii--goto-cell 2 4)
    (should (equal (emascii--cell) '(2 . 4)))
    (should (equal (buffer-string) "ab\n\n    "))))

(ert-deftest emascii-goto-cell-nopad ()
  (emascii-test-with-canvas "ab"
    (should-not (emascii--goto-cell 0 5 t))
    (should-not (emascii--goto-cell 3 0 t))
    (should (emascii--goto-cell 0 1 t))
    (should (equal (buffer-string) "ab"))))

(ert-deftest emascii-put-overwrites ()
  (emascii-test-with-canvas "abc\ndef"
    (emascii--put 0 1 ?X)
    (emascii--put 1 6 ?Y)
    (should (equal (buffer-string) "aXc\ndef   Y"))))

(ert-deftest emascii-char-at ()
  (emascii-test-with-canvas "abc"
    (should (eq (emascii--char-at 0 2) ?c))
    (should (eq (emascii--char-at 0 9) ?\s))
    (should (eq (emascii--char-at 5 0) ?\s))))

(ert-deftest emascii-tabs-expanded ()
  (emascii-test-with-canvas "a\tb"
    (should (equal (buffer-string) "a       b"))))

;;;; Typing and motion through the command loop

(ert-deftest emascii-typing-overwrites ()
  (emascii-test-with-canvas "hello world"
    (emascii-test-keys "<right> <right> X Y")
    (should (equal (buffer-string) "heXYo world"))))

(ert-deftest emascii-free-motion ()
  (emascii-test-with-canvas ""
    (emascii-test-keys "<down> <down> <right> <right> <right> A")
    (should (equal (buffer-string) "\n\n   A"))))

(ert-deftest emascii-newline-keeps-start-column ()
  (emascii-test-with-canvas ""
    (emascii-test-keys "<right> <right> a b RET c")
    (should (equal (buffer-string) "  ab\n  c"))))

(ert-deftest emascii-backspace-blanks ()
  (emascii-test-with-canvas "abcd"
    (emascii-test-keys "<right> <right> <right> DEL")
    (should (equal (buffer-string) "ab d"))))

(ert-deftest emascii-shift-backspace-pulls-line ()
  (emascii-test-with-canvas "a--b\ncd"
    (emascii-test-keys "<right> <right> <right>")
    (emascii-delete-backward-shift 1)
    (should (equal (buffer-string) "a-b\ncd"))
    (emascii--goto-cell 0 0)
    (emascii-delete-backward-shift 1)
    (should (equal (buffer-string) "a-b\ncd"))))

(ert-deftest emascii-shift-delete-pulls-line ()
  (emascii-test-with-canvas "a--b"
    (emascii--goto-cell 0 1)
    (emascii-delete-forward-shift 1)
    (should (equal (buffer-string) "a-b"))
    (emascii--goto-cell 0 3)
    (emascii-delete-forward-shift 1)
    (should (equal (buffer-string) "a-b"))))

(ert-deftest emascii-shift-delete-keys-bound ()
  (with-temp-buffer
    (emascii-mode)
    (should (eq (key-binding (kbd "S-DEL")) 'emascii-delete-backward-other))
    (should (eq (key-binding (kbd "S-<backspace>")) 'emascii-delete-backward-other))
    (should (eq (key-binding (kbd "S-<deletechar>")) 'emascii-delete-forward-other))
    (should (eq (key-binding (kbd "<insert>")) 'emascii-toggle-overwrite))))

(ert-deftest emascii-starts-in-overwrite ()
  (emascii-test-with-canvas "abc"
    (should emascii-overwrite)
    (emascii-test-keys "X")
    (should (equal (buffer-string) "Xbc"))))

(ert-deftest emascii-insert-mode-pushes ()
  (emascii-test-with-canvas "ab|cd"
    (emascii-test-keys "<insert> <right> <right> X Y")
    (should-not emascii-overwrite)
    (should (equal (buffer-string) "abXY|cd"))
    (emascii-test-keys "DEL")
    (should (equal (buffer-string) "abX|cd"))
    (emascii-test-keys "S-DEL")
    (should (equal (buffer-string) "ab |cd"))
    (emascii-test-keys "<insert> Z")
    (should emascii-overwrite)
    (should (equal (buffer-string) "abZ|cd"))))

(ert-deftest emascii-insert-mode-past-line-end ()
  (emascii-test-with-canvas "ab"
    (emascii-toggle-overwrite)
    (emascii-test-keys "<down> <right> <right> <right> Q")
    (should (equal (buffer-string) "ab\n   Q"))))

(ert-deftest emascii-moving-away-trims-padding ()
  (emascii-test-with-canvas "ab\ncd"
    (emascii-test-keys "<right> <right> <right> <right> <right> <down>")
    (should (equal (buffer-string) "ab\ncd   "))
    (emascii-test-keys "<left> <left> <left> <left> <left> <up>")
    (should (equal (buffer-string) "ab\ncd"))))

(ert-deftest emascii-undo-merges-padding ()
  (emascii-test-with-canvas "ab"
    (emascii-test-keys "<down> <down> <right> <right> <right> Z")
    (should (equal (buffer-string) "ab\n\n   Z"))
    (undo-boundary)
    (let ((last-command nil)) (undo))
    (should (equal (string-trim-right (buffer-string)) "ab"))))

;;;; Text helpers

(ert-deftest emascii-crop ()
  (should (equal (emascii--crop '("" "   ab  " "    c" ""))
                 '(1 3 ("ab" " c")))))

(ert-deftest emascii-export-pads ()
  (should (equal (emascii-export-string '("" "  +--+" "  |" "  +--+   " ""))
                 "+--+\n|   \n+--+")))

(ert-deftest emascii-normalize ()
  (should (equal (emascii--normalize-text "a\tb\r\nc")
                 '("a       b" "c"))))

;;;; Shapes

(ert-deftest emascii-rect-cells ()
  (should (= (length (emascii--rect-cells '(1 . 1) '(2 . 3))) 6)))

(ert-deftest emascii-ellipse-cells ()
  (let ((cells (emascii--ellipse-cells '(0 . 0) '(4 . 8))))
    (should (member '(2 . 4) cells))
    (should (member '(2 . 0) cells))
    (should-not (member '(0 . 0) cells))
    (should-not (member '(4 . 8) cells))))

(ert-deftest emascii-lasso-cells ()
  (let ((cells (emascii--lasso-cells '((0 . 0) (0 . 4) (4 . 4) (4 . 0)))))
    (should (member '(2 . 2) cells))
    (should (member '(0 . 2) cells))
    (should (= (length cells) 25))))

;;;; Selection, cut and floating

(defun emascii-test-select-rect (a b)
  (emascii--goto-cell (car b) (cdr b))
  (emascii--start-shape a)
  (emascii--goto-cell (car b) (cdr b)))

(ert-deftest emascii-selection-lines ()
  (emascii-test-with-canvas "......\n.ab...\n.cd...\n......"
    (emascii-test-select-rect '(1 . 1) '(2 . 2))
    (should (equal (emascii--selection-lines) '(1 1 ("ab" "cd"))))))

(ert-deftest emascii-toggled-cells ()
  (emascii-test-with-canvas "abc\ndef"
    (emascii--goto-cell 0 0) (emascii-toggle-cell)
    (emascii--goto-cell 1 2) (emascii-toggle-cell)
    (should (equal (emascii--selection-lines) '(0 0 ("a" "  f"))))
    (emascii--goto-cell 1 2) (emascii-toggle-cell)
    (should (equal (emascii--selection-lines) '(0 0 ("a"))))))

(ert-deftest emascii-cut-keeps-layout ()
  (emascii-test-with-canvas "abcd\nefgh"
    (emascii-test-select-rect '(0 . 1) '(1 . 2))
    (let ((kill-ring nil) (interprogram-cut-function nil)
          (emascii-rich-clipboard nil))
      (emascii-cut)
      (should (equal (buffer-string) "a  d\ne  h"))
      (should (equal (car kill-ring) "bc\nfg")))))

(ert-deftest emascii-move-and-drop ()
  (emascii-test-with-canvas "ab..\n...."
    (emascii-test-select-rect '(0 . 0) '(0 . 1))
    (emascii-move)
    (should emascii--float)
    (emascii-float-right) (emascii-float-right) (emascii-float-down)
    (emascii-float-commit)
    (should-not emascii--float)
    (should (equal (buffer-string) "  ..\n..ab"))))

(ert-deftest emascii-move-cancel-restores ()
  (emascii-test-with-canvas "ab..\n...."
    (emascii-test-select-rect '(0 . 0) '(0 . 1))
    (emascii-move)
    (emascii-float-down) (emascii-float-down) (emascii-float-right)
    (emascii-float-cancel)
    (should (equal (buffer-string) "ab..\n...."))))

(ert-deftest emascii-float-transparent ()
  (emascii-test-with-canvas "#####\n#####"
    (emascii-float-start '("o o" " o") 0 1)
    (emascii-float-commit)
    (should (equal (buffer-string) "#o#o#\n##o##"))))

(ert-deftest emascii-float-opaque ()
  (emascii-test-with-canvas "#####"
    (emascii-float-start '("o o") 0 1)
    (emascii-float-toggle-opaque)
    (emascii-float-commit)
    (should (equal (buffer-string) "#o o#"))))

(ert-deftest emascii-float-does-not-damage-background ()
  (emascii-test-with-canvas "abcdef"
    (emascii-float-start '("XY") 0 0)
    (dotimes (_ 3) (emascii-float-right))
    (emascii-float-commit)
    (should (equal (buffer-string) "abcXYf"))))

(ert-deftest emascii-float-undo-is-one-step ()
  (emascii-test-with-canvas "abcdef"
    (undo-boundary)
    (emascii-float-start '("XY") 0 0)
    (dotimes (_ 3) (emascii-float-right))
    (emascii-float-commit)
    (undo-boundary)
    (let ((last-command nil)) (undo))
    (should (equal (string-trim-right (buffer-string)) "abcdef"))))

(ert-deftest emascii-paste-floats-kill ()
  (emascii-test-with-canvas "....\n...."
    (let ((kill-ring '("ab\ncd")) (interprogram-paste-function nil))
      (emascii--goto-cell 0 1)
      (emascii-paste)
      (emascii-float-commit)
      (should (equal (buffer-string) ".ab.\n.cd.")))))

;;;; cua-mode interplay

(ert-deftest emascii-overrides-cua ()
  (let ((was cua-mode))
    (unwind-protect
        (progn
          (cua-mode 1)
          (emascii-test-with-canvas "abc"
            (transient-mark-mode 1)
            (set-mark (point)) (goto-char (point-max))
            (cua--select-keymaps)
            (should (eq (command-remapping 'yank) 'emascii-paste))
            (should (eq (command-remapping 'copy-region-as-kill) 'emascii-copy))
            (should (eq (command-remapping 'kill-region) 'emascii-cut))
            (should (eq (command-remapping 'keyboard-quit) 'emascii-quit))))
      (cua-mode (if was 1 -1)))))

;;;; Export helpers

(ert-deftest emascii-rtf-escapes ()
  (let ((emascii-export-font "Menlo") (emascii-export-font-size 10))
    (should (equal (emascii--rtf "{a}\\\nb")
                   "{\\rtf1\\ansi\\deff0{\\fonttbl{\\f0\\fmodern Menlo;}}\\f0\\fs20 \\{a\\}\\\\\\line\nb}"))))

(ert-deftest emascii-html-escapes ()
  (should (equal (emascii--html-escape "<a & \"b\">") "&lt;a &amp; &quot;b&quot;&gt;")))

;;;; Gallery

(defmacro emascii-test-with-gallery (&rest body)
  (declare (indent 0))
  `(let ((emascii-gallery-directory (make-temp-file "emascii-gallery" t)))
     (unwind-protect (progn ,@body)
       (delete-directory emascii-gallery-directory t))))

(ert-deftest emascii-gallery-roundtrip ()
  (emascii-test-with-gallery
    (emascii-gallery--write "people/stick" '(" o" "/|\\" "/ \\"))
    (emascii-gallery--write "cat" '("=^.^="))
    (should (equal (emascii-gallery--items) '("cat" "people/stick")))
    (should (equal (emascii-gallery--folders) '("people")))
    (should (equal (emascii-gallery--read "people/stick") '(" o" "/|\\" "/ \\")))))

(ert-deftest emascii-gallery-rejects-bad-names ()
  (should-error (emascii-gallery--check-id "../x") :type 'user-error)
  (should-error (emascii-gallery--check-id "a/../../x") :type 'user-error)
  (should-error (emascii-gallery--check-id "") :type 'user-error)
  (should (equal (emascii-gallery--check-id "a/b") "a/b")))

(ert-deftest emascii-gallery-browser-and-search ()
  (emascii-test-with-gallery
    (emascii-gallery--write "people/stick" '(" o" "/|\\"))
    (emascii-gallery--write "animals/cat" '("=^.^="))
    (with-temp-buffer
      (emascii-gallery-mode)
      (emascii-gallery--render)
      (should (string-match-p "=\\^\\.\\^=" (buffer-string)))
      (should (string-match-p "people" (buffer-string)))
      (should (equal (emascii-gallery--id-at-point) "animals/cat"))
      (emascii-gallery-next 1)
      (should (equal (emascii-gallery--id-at-point) "people/stick"))
      (emascii-gallery-search "stick")
      (should-not (string-match-p "=\\^\\.\\^=" (buffer-string))))))

(ert-deftest emascii-gallery-insert-floats ()
  (emascii-test-with-gallery
    (emascii-gallery--write "cat" '("=^.^="))
    (emascii-test-with-canvas "..........\n.........."
      (switch-to-buffer (current-buffer))
      (emascii--goto-cell 1 2)
      (emascii-gallery-insert "cat")
      (emascii-float-commit)
      (should (equal (buffer-string) "..........\n..=^.^=...")))))

(provide 'emascii-test)
;;; emascii-test.el ends here
