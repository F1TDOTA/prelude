;-------------------------------版本检测, 基本设置-----------------------------
(when (version< emacs-version "24.1")
  (error "Prelude requires at least GNU Emacs 24.1, but you're running %s" emacs-version))

;; 始终加载最新字节码
(setq load-prefer-newer t)

; 降低垃圾回收的频率(原来分配0.76MB内存, 现在每50MB内存才开始回收)
(setq gc-cons-threshold 50000000)

; 当打开文件超过100MB时告警
(setq large-file-warning-threshold 100000000)


;-------------------------------路径定义-----------------------------
; 配置根目录
(defvar prelude-dir (file-name-directory load-file-name)
  "The root dir of the Emacs Prelude distribution.")
  
; core 核心目录
(defvar prelude-core-dir (expand-file-name "core" prelude-dir)
  "The home of Prelude's core functionality.")

; modules 模块目录
(defvar prelude-modules-dir (expand-file-name  "modules" prelude-dir)
  "This directory houses all of the built-in Prelude modules.")
  
; personal 个人配置目录
(defvar prelude-personal-dir (expand-file-name "personal" prelude-dir)
  "This directory is for your personal configuration.
   Users of Emacs Prelude are encouraged to keep their personal configuration
   changes in this directory.  All Emacs Lisp files there are loaded automatically
   by Prelude.")

; personal/preload 个人预加载配置目录
(defvar prelude-personal-preload-dir (expand-file-name "preload" prelude-personal-dir)
  "This directory is for your personal configuration, that you want loaded before Prelude.")

; vendor 目录
(defvar prelude-vendor-dir (expand-file-name "vendor" prelude-dir)
  "This directory houses packages that are not yet available in ELPA (or MELPA).")
  
; savefile 目录
(defvar prelude-savefile-dir (expand-file-name "savefile" prelude-dir)
  "This folder stores all the automatically generated save/history-files.")

; 需要加载的模块文件名
(defvar prelude-modules-file (expand-file-name "prelude-modules.el" prelude-dir)
  "This files contains a list of modules that will be loaded by Prelude.")
  

;--------------------------------新建目录,加载路径,加载脚本-----------------------------------
(message "Begin to load basic settings...")

; 新建保存文件目录
(unless (file-exists-p prelude-savefile-dir)
  (make-directory prelude-savefile-dir))

; 定义递归加载子目录到load-path的函数
(defun prelude-add-subfolders-to-load-path (parent-dir)
 "Add all level PARENT-DIR subdirs to the `load-path'."
 (dolist (f (directory-files parent-dir))
   (let ((name (expand-file-name f parent-dir)))
     (when (and (file-directory-p name)
                (not (string-prefix-p "." f)))
       (add-to-list 'load-path name)
       (prelude-add-subfolders-to-load-path name)))))


; 加载核心目录到load-path
(add-to-list 'load-path prelude-core-dir)
(add-to-list 'load-path prelude-modules-dir)
(add-to-list 'load-path prelude-vendor-dir)
(prelude-add-subfolders-to-load-path prelude-vendor-dir)


; 加载个人目录的preload目录脚本
(when (file-exists-p prelude-personal-preload-dir)
  (message "Loading personal configuration files in %s..." prelude-personal-preload-dir)
  (mapc 'load (directory-files prelude-personal-preload-dir 't "^[^#].*el$")))

  
  

;-------------------------------加载核心库-------------------------------------------
; 提示消息
(message "Begin to load core...")

; 加载核心库
(require 'prelude-packages-1)

; custom需要在core, editor and ui前面加载
(require 'prelude-custom-2)
(require 'prelude-ui-3)
(require 'prelude-core-4)
(require 'prelude-mode-5)
(require 'prelude-editor-6)
(require 'prelude-global-keybindings-7)

; macos系统额外库
(when (eq system-type 'darwin)
  (require 'prelude-osx-8))

  
 
;-------------------------------加载核心模块------------------------------------------- 
(message "Begin to load modules...")

; 加载模块配置文件prelude-modules.el
(if (file-exists-p prelude-modules-file)
    (load prelude-modules-file)
  (message "Missing modules file %s" prelude-modules-file)
  (message "You can get started by copying the bundled example file"))

; 自定义UI配置文件
(setq custom-file (expand-file-name "custom.el" prelude-personal-dir))

; 加载个人自定义设置, 包含custom.el
(when (file-exists-p prelude-personal-dir)
  (message "Loading personal configuration files in %s..." prelude-personal-dir)
  (mapc 'load (directory-files prelude-personal-dir 't "^[^#].*el$")))

  

;-------------------------------加载完成,提示------------------------------------------- 
(message "Loading ok...")

