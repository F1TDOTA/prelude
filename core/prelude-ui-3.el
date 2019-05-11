;;; prelude-ui.el --  ui优化和调整
;;

;; 禁用掉工具栏和菜单栏
(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))

(menu-bar-mode -1)

;; 禁用光标闪烁
(blink-cursor-mode -1)

;; 禁用掉启动屏幕
(setq inhibit-startup-screen t)

;; 优化滚动条，还有5行时开始滚动，一次滚动一行
;; 再次滚动回来的时候，光标位于滚动前的那个位置
(setq scroll-margin 5
      scroll-conservatively 100000
      scroll-preserve-screen-position 1)

;; 状态栏显示行号，列号，位置百分比
(line-number-mode t)
(column-number-mode t)
(size-indication-mode t)

;; 以 y/n代表 yes/no
(fset 'yes-or-no-p 'y-or-n-p)

;; 窗口标题,如果是一个文件,显示文件名,如果是一个缓冲区,显示缓冲区名
(setq frame-title-format
      '("" invocation-name " zhoujun - " (:eval (if (buffer-file-name)
                                            (abbreviate-file-name (buffer-file-name))
                                          "%b"))))

;; 加载zenburn作为默认主题
(when prelude-theme
  (load-theme prelude-theme t))

;; 更新sml后,加载主题不需要确认
;; 安装smart-mode-line后的设置
(require 'smart-mode-line)
(setq sml/no-confirm-load-theme t)
(setq sml/theme nil)
(add-hook 'after-init-hook #'sml/setup)

;; beacon插件,当滚动时光标会闪烁一次，告诉你当前位置在哪
(require 'beacon)
(beacon-mode +1)

(provide 'prelude-ui-3)
;;; prelude-ui-3.el ends here
