(load "~/.emacs.d/linum.el")
(require 'linum)
(global-linum-mode 1)

;; Set alt/option key as Meta
(setq mac-option-modifier 'meta)

;; Deactivate ps selection mode so it doesn't conflict with transient-mark-mode
(pc-selection-mode 0)

;; Activate transient-mark-mode
(setq transient-mark-mode t)

;; Activate column-number-mode
(setq column-number-mode t)

(fset 'insertPound
   "#")
(global-set-key (kbd "M-3") 'insertPound)

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;; iswitchb

;; Enable iswitchb-mode, a much better way to switch between visible
;; buffers than the standard C-x b.
(iswitchb-mode t)
;; Bind M-s to use iswitchb-buffer, a great way to switch what buffer
;; you're currently editing.
(global-set-key "\M-s" 'iswitchb-buffer)
;; Make sure the buffer we're switching to appears in the current
;; window, even if it's already being displayed in another window.
(setq iswitchb-default-method 'samewindow)
;; Remove temporary buffers from the iswitch list ... but not quite
;; convenient, maybe we have to combine this with using M-s but
;; setting C-x b to be the default switch-buffer command?
(setq iswitchb-buffer-ignore (quote ("^ " "^\\*.*\\*$")))

(defun iswitchb-local-keys ()
  (mapc (lambda (K)
          (let* ((key (car K)) (fun (cdr K)))
            (define-key iswitchb-mode-map (edmacro-parse-keys key) fun)))
        '(("<right>" . iswitchb-next-match)
          ("<left>"  . iswitchb-prev-match)
          ("<up>"    . ignore             )
          ("<down>"  . ignore             ))))

(add-hook 'iswitchb-define-mode-map-hook 'iswitchb-local-keys)

;; Erlang

;; This is needed for Erlang mode setup
(setq erlang-root-dir "/usr/local/lib/erlang")
(setq load-path (cons "/usr/local/lib/erlang/lib/tools-2.6.6.5/emacs" load-path))

(setq exec-path (cons "/usr/local/lib/erlang/bin" exec-path))
(require 'erlang-start)


;;(add-to-list 'load-path "~/.gem/ruby/1.8/gems/haml-2.0.9/extra/")
;;(require 'haml-mode)
;;(add-to-list 'auto-mode-alist '("\\.haml$" . haml-mode))

(setq-default tab-width 4
              standard-indent 4
              c-indent-level 4
              indent-tabs-mode nil)

(setq-default fill-column 72)

;; Enable ido
(require'ido)
(ido-mode t)
(setq ido-enable-flex-matching t)


(add-hook 'erlang-mode-hook
          (lambda ()
             (modify-syntax-entry ?$ "w")
             (add-hook 'before-save-hook
                       (lambda()
                         (untabify (point-min) (point-max))) nil t)
             (setq indent-tabs-mode nil)))

(setq before-save-hook nil)

(add-hook 'before-save-hook
          (lambda ()
             (delete-trailing-whitespace)))

(setq-default show-trailing-whitespace t)

(show-paren-mode 1)