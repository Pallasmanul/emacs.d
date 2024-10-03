
;;;;;;;;;;;;;;;;;;;;;;;
;;   添加软件源
;;;;;;;;;;;;;;;;;;;;;;

(require 'package)
(setq package-archives '(("gnu"    . "http://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/")
                         ("nongnu" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/nongnu/")
                         ("melpa"  . "http://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/")))

(package-initialize)

(when (not package-archive-contents)
  (package-refresh-contents))

;;;;;;;;;;;;;;;;;;;;;;;;
;;     基础设置
;;;;;;;;;;;;;;;;;;;;;;;;

(setq inhibit-startup-screen t)       ;; 隐藏开始
(setq make-backup-files nil)          ;; 取消自动备份文件
(setq delete-by-moving-to-trash t)    ;; 删除文件到垃圾箱
(setq scroll-margin 3)                ;; 靠近窗口3行时开始滚动
(setq scroll-conservatively 1000)
(setq select-enable-clipboard t)      ;; 允许emacs和外部程序进行粘贴
(setq auto-save-default t)            ;; 打开自动保存
(tool-bar-mode 0)                     ;; 取消工具栏
(scroll-bar-mode 0)                   ;; 取消滚动条
(menu-bar-mode 0)                     ;; 取消菜单栏
(line-number-mode t)                  ;; 显示行号
(save-place-mode 1)                   ;; 记住上次打开文件光标的位置
(display-time-mode 1)                 ;; 启用时间显示设置
(setq display-time-24hr-format t      ;; 使用24小时制
      display-time-day-and-date t     ;; 时间显示包括日期和具体时间
      display-time-interval 10        ;; 时间变化频率
      display-time-format "%A %H:%M"  ;; 显示时间格式
      )
(unless
    (string-match-p "Power N/A" (battery)) ;; 显示笔记本点亮
  (display-battery-mode 1)
  )
(load-theme 'tango-dark)              ;; 加载tango-dark主题
(show-paren-mode t)                   ;; 启用括号匹配



;;;;;;;;;;;;;;;;;;;;;;;;
;;     自定义函数
;;;;;;;;;;;;;;;;;;;;;;;;

;; 打开 .emacs 文件并加载
(defun open-emacs-file()          
  (interactive)
  (if (string-equal (prin1-to-string (current-buffer)) "#<buffer .emacs>") (load-file "~/.emacs") (find-file "~/.emacs")))

;; 打开 pallas-emacs/init.el 文件并加载
(defun open-init-file()
  (interactive)
  (if (string-equal (prin1-to-string (current-buffer)) "#<buffer init.el>") (load-file "~/pallas-emacs/init.el") (find-file "~/pallas-emacs/init.el")))


;;;;;;;;;;;;;;;;;;;;;;;;
;;     美化
;;;;;;;;;;;;;;;;;;;;;;;;

;; 字体 ;;





;;;;;;;;;;;;;;;;;;;;;;;;
;;     实用工具
;;;;;;;;;;;;;;;;;;;;;;;;

(use-package general
  :ensure t
  :config
  (general-auto-unbind-keys)
  )

(add-to-list 'load-path "~/emacs-application-framework/")
(require 'eaf)
(require 'eaf-browser)



;;;;;;;;;;;;;;;;;;;;;;;;
;;     编程语言
;;;;;;;;;;;;;;;;;;;;;;;;




;;;;;;;;;;;;;;;;;;;;;;;;
;;     快捷方式
;;;;;;;;;;;;;;;;;;;;;;;;

(global-set-key (kbd "<f2>") 'open-emacs-file)
(global-set-key (kbd "<f3>") 'open-init-file)
