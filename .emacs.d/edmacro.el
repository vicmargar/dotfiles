;;; edmacro.el --- keyboard macro editor

;; Copyright (C) 1993, 1994 Free Software Foundation, Inc.

;; Author: Dave Gillespie <daveg@synaptics.com>
;;         Hrvoje Niksic <hniksic@srce.hr>  -- XEmacs port
;; Maintainer: Hrvoje Niksic <hniksic@srce.hr>
;; Version: 3.05
;; Keywords: abbrev

;; This file is part of XEmacs.

;; XEmacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; XEmacs is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with XEmacs; see the file COPYING.  If not, write to the Free
;; Software Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
;; 02111-1307, USA.

;;; Synched up with: FSF 19.34.

;;; Commentary:

;;; Usage:
;;
;; The `C-x C-k' (`edit-kbd-macro') command edits a keyboard macro
;; in a special buffer.  It prompts you to type a key sequence,
;; which should be one of:
;;
;;  * RET or `C-x e' (call-last-kbd-macro), to edit the most 
;;    recently defined keyboard macro.
;;
;;  * `M-x' followed by a command name, to edit a named command
;;    whose definition is a keyboard macro.
;;
;;  * `C-h l' (view-lossage), to edit the 100 most recent keystrokes
;;    and install them as the "current" macro.
;;
;;  * any key sequence whose definition is a keyboard macro.
;;
;; This file includes a version of `insert-kbd-macro' that uses the
;; more readable format defined by these routines.
;;
;; Also, the `read-kbd-macro' command parses the region as
;; a keyboard macro, and installs it as the "current" macro.
;; This and `format-kbd-macro' can also be called directly as
;; Lisp functions.

;; Type `C-h m', or see the documentation for `edmacro-mode' below,
;; for information about the format of written keyboard macros.

;; `edit-kbd-macro' formats the macro with one command per line,
;; including the command names as comments on the right.  If the
;; formatter gets confused about which keymap was used for the
;; characters, the command-name comments will be wrong but that
;; won't hurt anything.

;; With a prefix argument, `edit-kbd-macro' will format the
;; macro in a more concise way that omits the comments.

;; This package requires GNU Emacs 19 or later, and daveg's CL
;; package 2.02 or later.  (CL 2.02 comes standard starting with
;; Emacs 19.18.)  This package does not work with Emacs 18 or
;; Lucid Emacs.

;; But it works with XEmacs.  At least the modified version.  -hniksic

;;; Code:

(eval-when-compile
  (require 'cl))

;;; The user-level commands for editing macros.

;;;###autoload (define-key ctl-x-map "\C-k" 'edit-kbd-macro)

;;;###autoload
(defvar edmacro-eight-bits nil
  "*Non-nil if edit-kbd-macro should leave 8-bit characters intact.
Default nil means to write characters above \\177 in octal notation.")

(defvar edmacro-mode-map nil)
(unless edmacro-mode-map
  (setq edmacro-mode-map (make-sparse-keymap))
  (define-key edmacro-mode-map "\C-c\C-c" 'edmacro-finish-edit)
  (define-key edmacro-mode-map "\C-c\C-q" 'edmacro-insert-key))

(defvar edmacro-store-hook)
(defvar edmacro-finish-hook)
(defvar edmacro-original-buffer)

;;;###autoload
(defun edit-kbd-macro (keys &optional prefix finish-hook store-hook)
  "Edit a keyboard macro.
At the prompt, type any key sequence which is bound to a keyboard macro.
Or, type `C-x e' or RET to edit the last keyboard macro, `C-h l' to edit
the last 100 keystrokes as a keyboard macro, or `M-x' to edit a macro by
its command name.
With a prefix argument, format the macro in a more concise way."
  (interactive "kKeyboard macro to edit (C-x e, M-x, C-h l, or keys): \nP")
  (when keys
    (setq keys (edmacro-events-to-keys keys))
    (let ((cmd (if (arrayp keys) (key-binding keys) keys))
	  (mac nil))
      (cond (store-hook
	     (setq mac keys)
	     (setq cmd nil))
	    ((or (eq cmd 'call-last-kbd-macro)
		 (and (arrayp keys)
		      (= 1 (length keys))
		      (eq ?\r (aref keys 0))))
	     (or last-kbd-macro
		 (y-or-n-p "No keyboard macro defined.  Create one? ")
		 (keyboard-quit))
	     (setq mac (or last-kbd-macro ""))
	     (setq cmd 'last-kbd-macro))
	    ((eq cmd 'execute-extended-command)
	     (setq cmd (read-command "Name of keyboard macro to edit: "))
	     (if (string-equal cmd "")
		 (error "No command name given"))
	     (setq mac (symbol-function cmd)))
	    ((eq cmd 'view-lossage)
	     (setq mac (recent-keys))
	     (setq cmd 'last-kbd-macro))
	    ((null cmd)
	     (error "Key sequence %s is not defined" (key-description keys)))
	    ((symbolp cmd)
	     (setq mac (symbol-function cmd)))
	    (t
	     (setq mac cmd)
	     (setq cmd nil)))
      (unless (arrayp mac)
	(error "Key sequence %s is not a keyboard macro"
	       (key-description keys)))
      (message "Formatting keyboard macro...")
      (let* ((oldbuf (current-buffer))
	     (mmac (edmacro-fix-menu-commands mac))
	     (fmt (edmacro-format-keys mmac 1))
	     (fmtv (edmacro-format-keys mmac (not prefix)))
	     (buf (get-buffer-create "*Edit Macro*")))
	(message "Formatting keyboard macro...done")
	(switch-to-buffer buf)
	(kill-all-local-variables)
	(use-local-map edmacro-mode-map)
	(setq buffer-read-only nil)
	(setq major-mode 'edmacro-mode)
	(setq mode-name "Edit Macro")
	(set (make-local-variable 'edmacro-original-buffer) oldbuf)
	(set (make-local-variable 'edmacro-finish-hook) finish-hook)
	(set (make-local-variable 'edmacro-store-hook) store-hook)
	(erase-buffer)
	(insert ";; Keyboard Macro Editor.  Press C-c C-c to finish; "
		"press C-x k RET to cancel.\n")
	(insert ";; Original keys: " fmt "\n")
	(unless store-hook
	  (insert "\nCommand: " (if cmd (symbol-name cmd) "none") "\n")
	  (let ((keys (where-is-internal (or cmd mac))))
	    (if keys
		(while keys
		  (insert "Key: " (edmacro-format-keys (pop keys) 1) "\n"))
	      (insert "Key: none\n"))))
	(insert "\nMacro:\n\n")
	(save-excursion
	  (insert fmtv "\n"))
	(recenter '(4))
	(when (eq mac mmac)
	  (set-buffer-modified-p nil))
	(run-hooks 'edmacro-format-hook)))))

;;; The next two commands are provided for convenience and backward
;;; compatibility.

;;;###autoload
(defun edit-last-kbd-macro (&optional prefix)
  "Edit the most recently defined keyboard macro."
  (interactive "P")
  (edit-kbd-macro 'call-last-kbd-macro prefix))

;;;###autoload
(defun edit-named-kbd-macro (&optional prefix)
  "Edit a keyboard macro which has been given a name by `name-last-kbd-macro'."
  (interactive "P")
  (edit-kbd-macro 'execute-extended-command prefix))

;;;###autoload
(defun read-kbd-macro (start &optional end)
  "Read the region as a keyboard macro definition.
The region is interpreted as spelled-out keystrokes, e.g., \"M-x abc RET\".
See documentation for `edmacro-mode' for details.
Leading/trailing \"C-x (\" and \"C-x )\" in the text are allowed and ignored.
The resulting macro is installed as the \"current\" keyboard macro.

In Lisp, may also be called with a single STRING argument in which case
the result is returned rather than being installed as the current macro.
The result will be a string if possible, otherwise an event vector.
Second argument NEED-VECTOR means to return an event vector always."
  (interactive "r")
  (if (stringp start)
      (edmacro-parse-keys start end)
    (setq last-kbd-macro (edmacro-parse-keys (buffer-substring start end)))))

;;;###autoload
(defun format-kbd-macro (&optional macro verbose)
  "Return the keyboard macro MACRO as a human-readable string.
This string is suitable for passing to `read-kbd-macro'.
Second argument VERBOSE means to put one command per line with comments.
If VERBOSE is `1', put everything on one line.  If VERBOSE is omitted
or nil, use a compact 80-column format."
  (and macro (symbolp macro) (setq macro (symbol-function macro)))
  (edmacro-format-keys (or macro last-kbd-macro) verbose))

;;; Commands for *Edit Macro* buffer.

(defun edmacro-finish-edit ()
  (interactive)
  (unless (eq major-mode 'edmacro-mode)
    (error
     "This command is valid only in buffers created by `edit-kbd-macro'"))
  (run-hooks 'edmacro-finish-hook)
  (let ((cmd nil) (keys nil) (no-keys nil)
	(top (point-min)))
    (goto-char top)
    (let ((case-fold-search nil))
      (while (cond ((looking-at "[ \t]*\\($\\|;;\\|REM[ \t\n]\\)")
		    t)
		   ((looking-at "Command:[ \t]*\\([^ \t\n]*\\)[ \t]*$")
		    (when edmacro-store-hook
		      (error "\"Command\" line not allowed in this context"))
		    (let ((str (buffer-substring (match-beginning 1)
						 (match-end 1))))
		      (unless (equal str "")
			(setq cmd (and (not (equal str "none"))
				       (intern str)))
			(and (fboundp cmd) (not (arrayp (symbol-function cmd)))
			     (not (y-or-n-p
				   (format "Command %s is already defined; %s"
					   cmd "proceed? ")))
			     (keyboard-quit))))
		    t)
		   ((looking-at "Key:\\(.*\\)$")
		    (when edmacro-store-hook
		      (error "\"Key\" line not allowed in this context"))
		    (let ((key (edmacro-parse-keys
				(buffer-substring (match-beginning 1)
						  (match-end 1)))))
		      (unless (equal key [])
			(if (equal key [?n ?o ?n ?e])
			    (setq no-keys t)
			  (push key keys)
			  (let ((b (key-binding key)))
			    (and b (commandp b) (not (arrayp b))
				 (or (not (fboundp b))
				     (not (arrayp (symbol-function b))))
				 (not (y-or-n-p
				       (format "Key %s is already defined; %s"
					       (edmacro-format-keys key 1)
					       "proceed? ")))
				 (keyboard-quit))))))
		    t)
		   ((looking-at "Macro:[ \t\n]*")
		    (goto-char (match-end 0))
		    nil)
		   ((eobp) nil)
		   (t (error "Expected a `Macro:' line")))
	(forward-line 1))
      (setq top (point)))
    (let* ((buf (current-buffer))
	   (str (buffer-substring top (point-max)))
	   (modp (buffer-modified-p))
	   (obuf edmacro-original-buffer)
	   (store-hook edmacro-store-hook)
	   (finish-hook edmacro-finish-hook))
      (unless (or cmd keys store-hook (equal str ""))
	(error "No command name or keys specified"))
      (when modp
	(when (buffer-name obuf)
	  (set-buffer obuf))
	(message "Compiling keyboard macro...")
	(let ((mac (edmacro-parse-keys str)))
	  (message "Compiling keyboard macro...done")
	  (if store-hook
	      (funcall store-hook mac)
	    (when (eq cmd 'last-kbd-macro)
	      (setq last-kbd-macro (and (> (length mac) 0) mac))
	      (setq cmd nil))
	    (when cmd
	      (if (= (length mac) 0)
		  (fmakunbound cmd)
		(fset cmd mac)))
	    (if no-keys
		(when cmd
		  (loop for key in (where-is-internal cmd) do
			(global-unset-key key)))
	      (when keys
		(if (= (length mac) 0)
		    (loop for key in keys do (global-unset-key key))
		  (loop for key in keys do
			(global-set-key key (or cmd mac)))))))))
      (kill-buffer buf)
      (when (buffer-name obuf)
	(switch-to-buffer obuf))
      (when finish-hook
	(funcall finish-hook)))))

(defun edmacro-insert-key (key)
  "Insert the written name of a key in the buffer."
  (interactive "kKey to insert: ")
  (if (bolp)
      (insert (edmacro-format-keys key t) "\n")
    (insert (edmacro-format-keys key) " ")))

(defun edmacro-mode ()
  "\\<edmacro-mode-map>Keyboard Macro Editing mode.  Press
\\[edmacro-finish-edit] to save and exit.
To abort the edit, just kill this buffer with \\[kill-buffer] RET.

Press \\[edmacro-insert-key] to insert the name of any key by typing the key.

The editing buffer contains a \"Command:\" line and any number of
\"Key:\" lines at the top.  These are followed by a \"Macro:\" line
and the macro itself as spelled-out keystrokes: `C-x C-f foo RET'.

The \"Command:\" line specifies the command name to which the macro
is bound, or \"none\" for no command name.  Write \"last-kbd-macro\"
to refer to the current keyboard macro (as used by \\[call-last-kbd-macro]).

The \"Key:\" lines specify key sequences to which the macro is bound,
or \"none\" for no key bindings.

You can edit these lines to change the places where the new macro
is stored.


Format of keyboard macros during editing:

Text is divided into \"words\" separated by whitespace.  Except for
the words described below, the characters of each word go directly
as characters of the macro.  The whitespace that separates words
is ignored.  Whitespace in the macro must be written explicitly,
as in \"foo SPC bar RET\".

 * The special words RET, SPC, TAB, DEL, LFD, ESC, and NUL represent
   special control characters.  The words must be written in uppercase.

 * A word in angle brackets, e.g., <return>, <down>, or <f1>, represents
   a function key.  (Note that in the standard configuration, the
   function key <return> and the control key RET are synonymous.)
   You can use angle brackets on the words RET, SPC, etc., but they
   are not required there.

 * Keys can be written by their ASCII code, using a backslash followed
   by up to six octal digits.  This is the only way to represent keys
   with codes above \\377.

 * One or more prefixes M- (meta), C- (control), S- (shift), A- (alt),
   H- (hyper), and s- (super) may precede a character or key notation.
   For function keys, the prefixes may go inside or outside of the
   brackets:  C-<down> = <C-down>.  The prefixes may be written in
   any order:  M-C-x = C-M-x.

   Prefixes are not allowed on multi-key words, e.g., C-abc, except
   that the Meta prefix is allowed on a sequence of digits and optional
   minus sign:  M--123 = M-- M-1 M-2 M-3.

 * The `^' notation for control characters also works:  ^M = C-m.

 * Double angle brackets enclose command names:  <<next-line>> is
   shorthand for M-x next-line RET.

 * Finally, REM or ;; causes the rest of the line to be ignored as a
   comment.

Any word may be prefixed by a multiplier in the form of a decimal
number and `*':  3*<right> = <right> <right> <right>, and
10*foo = foofoofoofoofoofoofoofoofoofoo.

Multiple text keys can normally be strung together to form a word,
but you may need to add whitespace if the word would look like one
of the above notations:  `; ; ;' is a keyboard macro with three
semicolons, but `;;;' is a comment.  Likewise, `\\ 1 2 3' is four
keys but `\\123' is a single key written in octal, and `< right >'
is seven keys but `<right>' is a single function key.  When in
doubt, use whitespace."
  (interactive)
  (error "This mode can be enabled only by `edit-kbd-macro'"))
(put 'edmacro-mode 'mode-class 'special)


(defun edmacro-int-char (int)
  (if (fboundp 'char-to-int)
      (char-to-int int)
    int))

;;; Formatting a keyboard macro as human-readable text.

;; Changes for XEmacs -- these two functions re-written from scratch.
;; edmacro-parse-keys always returns a vector.  edmacro-format-keys
;; accepts a vector (but works with a string too).  Vector may contain
;; keypress events.      -hniksic
(defun edmacro-parse-keys (string &optional ignored)
  (let ((pos 0)
	(case-fold-search nil)
	(word-to-sym '(("NUL" . (control space))
		       ("RET" . return)
		       ("LFD" . linefeed)
		       ("TAB" . tab)
		       ("ESC" . escape)
		       ("SPC" . space)
		       ("BS" . backspace)
		       ("DEL" . delete)))
	(char-to-word '((?\0 . "NUL")
			(?\r . "RET")
			(?\n . "LFD")
			(?\t . "TAB")
			(?\e . "ESC")
			(?\  . "SPC")
			(?\C-? . "DEL")))
	;; string-to-symbol-or-char converter
	(conv #'(lambda (arg)
		  (if (= (length arg) 1)
		      (aref arg 0)
		    (if (string-match "^<\\([^>]+\\)>$" arg)
			(setq arg (match-string 1 arg)))
		    (let ((match (assoc arg word-to-sym)))
		      (if match
			  (cdr match)
			(intern arg))))))
	(conv-chars #'(lambda (arg)
			(let ((match (assoc arg char-to-word)))
			  (if match
			      (cdr (assoc (cdr match) word-to-sym))
			    arg))))
	res)
    (while (and (< pos (length string))
		(string-match "[^ \t\n\f]+" string pos))
      (let ((word (substring string (match-beginning 0) (match-end 0)))
	    (times 1)
	    (force-sym nil)
	    (add nil))
	(setq pos (match-end 0))
	(when (string-match "\\([0-9]+\\)\\*." word)
	  (setq times (string-to-int (substring word 0 (match-end 1))))
	  (setq word (substring word (1+ (match-end 1)))))
	(when (string-match "^<\\([^<>]+\\)>$" word)
	  (setq word (match-string 1 word))
	  (setq force-sym t))
	(setq match (assoc word word-to-sym))
	;; Add an element.
	(cond ((string-match "^\\\\[0-7]+" word)
	       ;; Octal value of character.
	       (setq add
		     (list
		      (edmacro-int-char
		       (edmacro-octal-string-to-integer (substring word 1))))))
	      ((string-match "^<<.+>>$" word)
	       ;; Extended command.
	       (setq add
		     (nconc
		      (list
		       (if (eq (key-binding [(meta x)])
			       'execute-extended-command)
			   '(meta x)
			 (or (car (where-is-internal
				   'execute-extended-command))
			     '(meta x))))
		      (mapcar conv-chars (concat (substring word 2 -2) "\r")))
		     ))
	      ((or (equal word "REM") (string-match "^;;" word))
	       ;; Comment.
	       (setq pos (string-match "$" string pos)))
	      (match
	       ;; Convert to symbol.
	       (setq add (list (cdr match))))
	      ((string-match "^\\^" word)
	       ;; ^X == C-x
	       (if (/= (length word) 2)
		   (error "^ must be followed by one character"))
	       (setq add (list 'control (aref word 0))))
	      ((string-match "^[MCSsAH]-" word)
	       ;; Parse C-*
	       (setq
		add
		(list
		 (let ((pos1 0)
		       (r1 nil)
		       follow)
		   (while (string-match "^[MCSsAH]-" (substring word pos1))
		     (setq r1 (nconc
			       r1
			       (list
				(cdr (assq (aref word pos1)
					   '((?C . control)
					     (?M . meta)
					     (?S . shift)
					     (?A . alt)
					     (?H . hyper)
					     (?s . super)))))))
		     (setq pos1 (+ pos1 2)))
		   (setq follow (substring word pos1))
		   (if (equal follow "")
		       (error "%s must precede a string"
			      (substring word 0 pos1)))
		   (nconc r1 (list (funcall conv follow)))))))
	      (force-sym
	       ;; This must be a symbol
	       (setq add (list (intern word))))
	      (t
	       ;; Characters
	       (setq add (mapcar conv-chars word))))
	(let ((new nil))
	  (loop repeat times do (setq new (append new add)))
	  (setq add new))
	(setq res (nconc res add))))
    (mapvector 'identity res)))

(defun edmacro-conv (char-or-sym add-<>)
  (let ((char-to-word '((?\0 . "NUL")
			(?\r . "RET")
			(?\n . "LFD")
			(?\t . "TAB")
			(?\e . "ESC")
			(?\  . "SPC")
			(?\C-? . "DEL")))
	(symbol-to-char '((return . ?\r)
			  (linefeed . ?\n)
			  (space . ?\ )
			  (delete . ?\C-?)
			  (tab . ?\t)
			  (escape . ?\e))))
    (if (symbolp char-or-sym)
	(if (= (length (symbol-name char-or-sym)) 1)
	    (setq char-or-sym (aref (symbol-name char-or-sym) 0))
	  (let ((found (assq char-or-sym symbol-to-char)))
	    (if found
		(setq char-or-sym (cdr found))))))
    ;; Return:
    (cons (symbolp char-or-sym)
	  (if (symbolp char-or-sym)
	      (if add-<>
		  (concat "<" (symbol-name char-or-sym) ">")
		(symbol-name char-or-sym))
	    (let ((found (assq char-or-sym char-to-word)))
	      (cond (found
		     (cdr found))
		    ((< char-or-sym 128)
		     (single-key-description char-or-sym))
		    (t
		     (format "\\%o" (edmacro-int-char char-or-sym)))))))))

(defun edmacro-format-1 (keys command times togetherp)
  (let ((res "")
	(start keys)
	el)
    (while keys
      (unless (or (eq start keys) togetherp)
	(callf concat res " "))
      (if (> times 1)
	  (setq res (concat (format "%d*" times) res)))
      (setq el (car keys))
      (callf concat res
	(cond ((listp el)
	       (let ((my ""))
		 (if (or
		      (let (cnv)
			(while el
			  (let ((found (assq (car el)
					     '((control . "C-")
					       (meta . "M-")
					       (shift . "S-")
					       (alt . "A-")
					       (hyper . "H-")
					       (super . "s-")))))
			    (callf concat my
			      (if found
				  (cdr found)
				(setq cnv (edmacro-conv (car el) nil))
				(cdr cnv))))
			  (setq el (cdr el)))
			(car cnv))
		      (> times 1))
		     (concat "<" my ">")
		   my)))
	      (t
	       (cdr (edmacro-conv el t)))))
      (setq keys (cdr keys)))
    (if command
	(callf concat res
	  (concat
	   (make-string (max (- 3 (/ (length res) tab-width)) 1) ?\t)
	   ";; "
	   (symbol-name command)
	   (if togetherp (format " * %d" (length start))))))
    res))

;; Convert the keypress events in vector x to keys, and return a
;; vector of keys.  If a list element is not a keypress event, ignore
;; it.
(defun edmacro-events-to-keys (x)
  (if (or (not (fboundp 'events-to-keys))
	  (not (arrayp x)))
      x
    (let ((cnt 0)
	  (len (length x))
	  new el)
      (while (< cnt len)
	(setq el (aref x cnt))
	(cond ((eventp el)
	       (if (mouse-event-p el)
		   (setq el nil)
		 (setq el (aref (events-to-keys (vector el)) 0))))
	      (t
	       nil))			; leave it be.
	(if el
	    (setq new (nconc new (list el))))
	(incf cnt))
      (mapvector 'identity new))))

;; Collapse a list of keys into a list of function keys, where
;; applicable.
(defun edmacro-fkeys (keys)
  (let (new k)
    (while keys
      (setq k (nconc k (list (car keys))))
      (setq lookup (lookup-key function-key-map (mapvector 'identity k)))
      (cond ((vectorp lookup)
	     (setq new (nconc new (mapcar 'identity lookup)))
	     (setq k nil))
	    ((keymapp lookup)
	     nil)
	    ((null lookup)
	     (setq new (nconc new k))
	     (setq k nil))
	    (t
	     (setq k nil)))
      (setq keys (cdr keys)))
    (if (keymapp lookup)
	(setq new (nconc new k)))
    new))

(defun edmacro-format-keys (macro &optional verbose)
  ;; XEmacs:
  ;; If we're dealing with events, convert them to symbols first.
  (setq macro (edmacro-events-to-keys macro))
  (if (zerop (length macro))
      ""
    (let ((res ""))
      ;; I'm not sure I understand the original code, but this seems to
      ;; work.
      (and (eq verbose 1)
	   (setq verbose nil))

      ;; Oh come on -- I want a list!  Much easier to process...
      (setq macro (mapcar 'identity macro))
      (setq macro (edmacro-fkeys macro))
      (while macro
	(let (key lookup (times 1) self-insert-p)
	  (loop do
		(setq key (nconc key (list (car macro)))
		      macro (cdr macro)
		      lookup (lookup-key global-map (mapvector 'identity key)))
		while
		(and lookup (not (commandp lookup))))
	  ;; (lookup-key [?\C-x ?e]) seems to return a vector!
	  (if (vectorp lookup)
	      (setq lookup nil))
	  (if (and (eq lookup 'self-insert-command)
		   (= (length key) 1)
		   (not (memq (car key)
			      '(?\  ?\r ?\n space return linefeed tab))))
	      (while (and (< (length key) 23)
			  (eq (lookup-key global-map (car macro))
			      'self-insert-command)
			  (not (memq
				(car macro)
				'(?\  ?\r ?\n space return linefeed tab))))
		(setq key (nconc key (list (car macro)))
		      macro (cdr macro)
		      self-insert-p t))
	    (while (edmacro-seq-equal key macro)
	      (setq macro (nthcdr (length key) macro))
	      (incf times)))
	  (if (or self-insert-p
		  (null (cdr key))
		  (= times 1))
	      (callf concat res (edmacro-format-1 key (if verbose lookup
							nil)
						  times self-insert-p)
		     (and macro (if verbose "\n" " ")))
	    (loop repeat times
		  do
		  (callf concat res
		    (edmacro-format-1 key (if verbose lookup
					    nil)
				      1 self-insert-p)
		    (and macro (if verbose "\n" " ")))))))
      res)))

(defun edmacro-seq-equal (seq1 seq2)
  (while (and seq1 seq2
	      (equal (car seq1) (car seq2)))
    (setq seq1 (cdr seq1)
	  seq2 (cdr seq2)))
  (not seq1))

(defsubst edmacro-oct-char-to-integer (character)
  "Take a char and return its value as if it was a octal digit."
  (if (and (>= character ?0) (<= character ?7))
      (- character ?0)
    (error (format "Invalid octal digit `%c'." character))))

(defun edmacro-octal-string-to-integer (octal-string)
  "Return decimal integer for OCTAL-STRING."
  (interactive "sOctal number: ")
  (let ((oct-num 0))
    (while (not (equal octal-string ""))
      (setq oct-num (+ (* oct-num 8)
		       (edmacro-oct-char-to-integer
			(string-to-char octal-string))))
      (setq octal-string (substring octal-string 1)))
    oct-num))


(defun edmacro-fix-menu-commands (macro)
  (when (vectorp macro)
    (let ((i 0) ev)
      (while (< i (length macro))
	(when (and (consp (setq ev (aref macro i)))
		   (not (memq (car ev)	; ha ha
			      '(hyper super control meta alt control shift))))
	  (cond ((equal (cadadr ev) '(menu-bar))
		 (setq macro (vconcat (edmacro-subseq macro 0 i)
				      (vector 'menu-bar (car ev))
				      (edmacro-subseq macro (1+ i))))
		 (incf i))
		;; It would be nice to do pop-up menus, too, but not enough
		;; info is recorded in macros to make this possible.
		(t
		 (error "Macros with mouse clicks are not %s"
			"supported by this command"))))
	(incf i))))
  macro)

;;; Parsing a human-readable keyboard macro.



;;; The following probably ought to go in macros.el:

;;;###autoload
(defun insert-kbd-macro (macroname &optional keys)
  "Insert in buffer the definition of kbd macro NAME, as Lisp code.
Optional second arg KEYS means also record the keys it is on
\(this is the prefix argument, when calling interactively).

This Lisp code will, when executed, define the kbd macro with the same
definition it has now.  If you say to record the keys, the Lisp code
will also rebind those keys to the macro.  Only global key bindings
are recorded since executing this Lisp code always makes global
bindings.

To save a kbd macro, visit a file of Lisp code such as your `~/.emacs',
use this command, and then save the file."
  (interactive "CInsert kbd macro (name): \nP")
  (let (definition)
    (if (string= (symbol-name macroname) "")
	(progn
	  (setq definition (format-kbd-macro))
	  (insert "(setq last-kbd-macro"))
      (setq definition (format-kbd-macro macroname))
      (insert (format "(defalias '%s" macroname)))
    (if (> (length definition) 50)
	(insert " (read-kbd-macro\n")
      (insert "\n  (read-kbd-macro "))
    (prin1 definition (current-buffer))
    (insert "))\n")
    (if keys
	(let ((keys (where-is-internal macroname)))
	  (while keys
	    (insert (format "(global-set-key %S '%s)\n" (car keys) macroname))
	    (setq keys (cdr keys)))))))

(provide 'edmacro)

;;; edmacro.el ends here
