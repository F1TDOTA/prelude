;;; 自定义变量 
(defgroup prelude nil
  "Emacs Prelude configuration."
  :prefix "prelude-"
  :group 'convenience)

; 非空值 启用自动保存
(defcustom prelude-auto-save t
  "Non-nil values enable Prelude's auto save."
  :type 'boolean
  :group 'prelude)

; 非空值 启用guru-mode
(defcustom prelude-guru t
  "Non-nil values enable `guru-mode'."
  :type 'boolean
  :group 'prelude)

; 非空值 启用空格可见
(defcustom prelude-whitespace t
  "Non-nil values enable Prelude's whitespace visualization."
  :type 'boolean
  :group 'prelude)
 
; 文件保存之前清除空格
; 仅当prelude-whitespace选项启用时才生效
(defcustom prelude-clean-whitespace-on-save t
  "Cleanup whitespace from file before it's saved.
Will only occur if `prelude-whitespace' is also enabled."
  :type 'boolean
  :group 'prelude)

; 非空值 启用flyspell支持
(defcustom prelude-flyspell t
  "Non-nil values enable Prelude's flyspell support."
  :type 'boolean
  :group 'prelude)

; 用户自定义初化目录
(defcustom prelude-user-init-file (expand-file-name "personal/"
                                                    user-emacs-directory)
  "Path to your personal customization file.
Prelude recommends you only put personal customizations in the
personal folder.  This variable allows you to specify a specific
folder as the one that should be visited when running
`prelude-find-user-init-file'.  This can be easily set to the desired buffer
in lisp by putting `(setq prelude-user-init-file load-file-name)'
in the desired elisp file."
  :type 'string
  :group 'prelude)

; 对齐很敏感的模式
(defcustom prelude-indent-sensitive-modes
  '(conf-mode coffee-mode haml-mode python-mode slim-mode yaml-mode)
  "Modes for which auto-indenting is suppressed."
  :type 'list
  :group 'prelude)

; 块选择对齐模式
(defcustom prelude-yank-indent-modes '(LaTeX-mode TeX-mode)
  "Modes in which to indent regions that are yanked (or yank-popped).
Only modes that don't derive from `prog-mode' should be listed here."
  :type 'list
  :group 'prelude)

; 块选择对齐阀值(1000字符)
(defcustom prelude-yank-indent-threshold 1000
  "Threshold (# chars) over which indentation does not automatically occur."
  :type 'number
  :group 'prelude)

; 主题名称
(defcustom prelude-theme 'zenburn
  "The default color theme, change this in your /personal/preload config."
  :type 'symbol
  :group 'prelude)

; 调用shell
(defcustom prelude-shell (getenv "SHELL")
  "The default shell to run with `prelude-visit-term-buffer'"
  :type 'string
  :group 'prelude)

; 包名
(provide 'prelude-custom-2)

;;; prelude-custom-2.el ends here
