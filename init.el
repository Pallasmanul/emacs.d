
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
(setq auto-save-silent t)             ;; 停止編輯后立刻保存
(setq auto-save-delete-trailing-whitespace t)  ;;自勭刪除行后空格
;;(global-auto-revert-mode 1)
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
(unless (string-match-p "Power N/A" (battery)) ;; 显示笔记本点亮
  (display-battery-mode 1)
  )
(load-theme 'tango-dark)              ;; 加载tango-dark主题
(show-paren-mode t)                   ;; 启用括号匹配
(paredit-mode t)                      ;; 自勭關閉括號
;;(desktop-save-mode 1)                 ;; 保存佈局

(setq python-indent-offset 4)
(setq python-indent-guess-indent-offset-verbose nil)

;; session 工作區保存和加載 ;;
(load-file "~/pallas-emacs/session.el")
(add-hook 'after-init-hook 'sessions-open-at-start) ;; 打開Emacs后自勭加載sessions
(add-hook 'kill-emacs-hook 'sessions-save)          ;; 關閉Emacs后自勭保存sessions



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

;; 重新加載 pallas-emacs/init.el
(defun reload-init-file()
  (interactive)
  (load-file "~/pallas-emacs/init.el")
  )

;; 執行當前 elisp 文件
(defun load-current-file()
  (interactive)
  (load-file buffer-file-name)
  )

;; 推送倉庫
(defun push-init-file()
  (interactive)
  (async-shell-command "git push origin master")  
  )

;; 拉取並更新倉庫
(defun pull-init-file()
  (interactive)
  (async-shell-command "git pull origin master")  
  )

;;;;;;;;;;;;;;;;;;;;;;;;
;;     美化
;;;;;;;;;;;;;;;;;;;;;;;;

;; 字体 ;;


;; misc ;;

(use-package all-the-icons
  :if (display-graphic-p))

(use-package dashboard
  :ensure t
  :custom
  (dashboard-center-content 1)
  (dashboard-set-heading-icons t)
  (dashboard-set-file-icons t)
  (dashboard-items '((projects . 5)
                     (recents . 5)
                     (agenda . 5)
                     (registers . 5)))
  :init
;;  (dashboard-setup-startup-hook)  ; 啓動emacs時不顯示
;;  (setq initial-buffer-choice (lambda () (get-buffer "*dashboard*")))
  )


(use-package doom-themes
  :ensure t
  :init
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t  ; if nil, italics is universally disabled
	)
  
  (load-theme 'doom-vibrant t)
  (doom-themes-visual-bell-config)
  )


(use-package doom-modeline
  :ensure t
  :init
  (doom-modeline-mode 1)
  )


(use-package centaur-tabs
  :ensure t
  :config
  (centaur-tabs-mode t)
  )


;;;;;;;;;;;;;;;;;;;;;;;;
;;     英文翻譯
;;;;;;;;;;;;;;;;;;;;;;;;
(use-package bing-dict
  :ensure t
)

;;;;;;;;;;;;;;;;;;;;;;;;
;;     实用工具
;;;;;;;;;;;;;;;;;;;;;;;;

;;  输入  ;;
(use-package rime
  :ensure t
  :after posframe
  :custom
  (default-input-method "rime")
  (rime-show-candidate  'posframe)
  )

;; 文本移動 ;;
(use-package move-text
  :ensure t
  :config
  (move-text-default-bindings)
  )

;; 括號自勭補全 ;;


;; 補全系統 ;;
(use-package ivy
  :ensure t
  :init
  (ivy-mode 1)
  :custom
  (ivy-use-virtual-buffers t)
  (enable-recursive-minibuffers t)
)

;; 常用指令 ;;
(use-package counsel
  :ensure t
  :custom
  (counsel-find-file-at-point t)
  :init
  (counsel-mode +1)
)

;; ivy彈出窗口 ;;
(use-package ivy-posframe
  :ensure t
  :custom
  (ivy-posframe-display-functions-alist
   '((swiper          . ivy-posframe-display-at-point)  ;; swiper 紧随光标弹出
     (complete-symbol . ivy-posframe-display-at-point)  ;; 符号补全紧随光标弹出
     (t . ivy-posframe-display)))                       ;; 其他所有都在中心位置弹出
  (ivy-posframe-parameters '((left-fringe . 8)
                             (right-fringe . 8)))       ;; 指示弹出窗口标边缘
  :init
  (ivy-posframe-mode 1))

;; ivy額外信息 ;;
(use-package ivy-rich
  :ensure t
  :init (ivy-rich-mode 1))

;; icons ;;
(use-package all-the-icons-ivy-rich
  :ensure t
  :init (all-the-icons-ivy-rich-mode 1))

;; 跳轉預覽 ;;
(use-package goto-line-preview :ensure t
  :bind (("C-c g" . goto-line-preview)))

;; 快捷鍵提示 ;;
(use-package which-key
  :ensure t
  :init
  (which-key-mode))

;; 標記窗口數字 ;;
(use-package winum
  :ensure t
  :config
  (winum-mode t)
  )

;; 窗口佈局 ;;
(use-package eyebrowse
  :ensure t
  :config
  (eyebrowse-mode t)
  )

;; 寫作模式 ;;
(use-package writeroom-mode
  :ensure t
  :config
  (setq writeroom-width 160)
  )

;;  搜索增強 ;;
(use-package general
  :ensure t
  :config
  (general-auto-unbind-keys)
  :bind
  ("\C-s" . swiper)
  )

(use-package wgrep
  :ensure t
  )

(use-package deadgrep
  :ensure t
  )



;;;;;;;;;;;;;;;;;;;;;;;;
;;     文檔系統
;;;;;;;;;;;;;;;;;;;;;;;;


(add-hook 'org-mode-hook #'turn-on-org-cdlatex)
;;(setq org-format-latex-header "")
;;(setq org-preview-latex-image-directory "/tmp/ltximg/")
;;(setq org-preview-latex-default-process )
(org-babel-do-load-languages 'org-babel-load-languages '((emacs-lisp . t)
							 (python . t)
							 (C . t)
							 ))

(use-package cdlatex
  :ensure t
  )

(use-package org-modern
  :ensure t
  :config
  (add-hook 'org-mode-hook #'org-modern-mode)
  (setq org-modern-star 'replace)
)

(use-package org-latex-impatient
  :ensure t
  :hook (org-mode . org-latex-impatient-mode)
  :init
  (setq org-latex-impatient-tex2svg-bin
        ;; location of tex2svg executable
        "~/node_modules/mathjax-node-cli/bin/tex2svg")
)

(use-package org-fragtog
  :ensure t
  :hook   ((org-mode . org-fragtog-mode))
  )





;;;;;;;;;;;;;;;;;;;;;;;;
;;     版本管理
;;;;;;;;;;;;;;;;;;;;;;;;
(use-package magit
  :ensure t
  )


;;;;;;;;;;;;;;;;;;;;;;;;
;;     虛擬機環境
;;;;;;;;;;;;;;;;;;;;;;;;


;; python 虛擬環境 ;;
(use-package pyvenv
  :ensure t
  :config
  (setq pyvenv-workon "animaker")  ; Default venv
  (add-hook 'python-mode-hook 'pyvenv-mode)
)

;;;;;;;;;;;;;;;;;;;;;;;;
;;     编程语言
;;;;;;;;;;;;;;;;;;;;;;;;

;; 模板  ;;
(use-package yasnippet
  :ensure t
  :config
  (yas-global-mode 1)
  )

;; markdown mode ;;
(use-package markdown-mode
  :ensure t
  )

;; 代碼補全 ;;
(use-package company
  :ensure t
  :init
  ;; Customize company backends.
  (add-hook 'emacs-list-mode-hook #'company-mode)
  (add-hook 'python-mode-hook #'company-mode)
)

(use-package company-posframe 
  :ensure t
  :hook (company-mode . company-box-mode))

;; 代碼檢查 ;;
(use-package flycheck
  :ensure t
  :config
  (setq flycheck-indication-mode nil)
  (setq flycheck-highlighting-style nil)
)

;; python lsp ;;
(use-package lsp-mode
  :ensure t
  :config
  (with-eval-after-load 'lsp-mode
    (add-hook 'lsp-mode-hook #'lsp-enable-which-key-integration)
    (add-hook 'lsp-mode-hook #'flycheck-mode)
    (add-hook 'lsp-mode-hook #'company-mode)
    )
  (setq lsp-headerline-breadcrumb-enable nil)
  (setq lsp-pylsp-plugins-autopep8-enabled t)
  :hook (
         (python-mode . lsp)
         (lsp-mode . lsp-enable-which-key-integration)
	 )
  :commands lsp-mode
  )

;; 添加lsp ;;
(use-package lsp-ui
  :ensure t
  :commands lsp-ui-mode
  :init
  (add-hook 'lsp-mode-hook 'lsp-ui-mode)
  :config
  (setq lsp-ui-peek-enable nil)
  (setq lsp-ui-doc-enable nil)
  (setq lsp-ui-imenu-enable nil)
  (setq lsp-ui-flycheck-enable t)
  (setq lsp-ui-sideline-enable nil)
  (setq lsp-ui-sideline-show-code-actions nil)
  (setq lsp-ui-sideline-show-hover nil)  
  (setq lsp-ui-sideline-ignore-duplicate nil)
)

;; dap-mode 調試器 ;;
(use-package dap-mode
  :ensure t
  :after lsp-mode
  :config
  (require 'dap-python)
  (setq dap-python-debugger 'debugpy)
)

;; quickrun 快速運行程序 ;;
(use-package quickrun
  :ensure t
  :after lsp-mode
  :config
  (setq quickrun-focus-p nil)
  (setq quickrun-timeout-seconds 20)
  (setq quickrun-truncate-lines nil)

)



;;;;;;;;;;;;;;;;;;;;;;;;
;;     工程項目
;;;;;;;;;;;;;;;;;;;;;;;;

;; animaker ;;
(defun animaker_run ()
  (interactive)
  (let ((buf (find-file-noselect "~/animaker/main.py")))
    (with-current-buffer buf
      (quickrun)))
)

(defun animaker_debug ()
  (interactive)
  (dap-mode 'toggle)
  (dap-ui-mode 'toggle)
  (print   (let ((buf (find-file-noselect "~/animaker/main.py")))
    (with-current-buffer buf
      (default-value 'dap-mode))))
)



;; linux ;;

;; embeded ;;


;;;;;;;;;;;;;;;;;;;;;;;;
;;     快捷方式
;;;;;;;;;;;;;;;;;;;;;;;;
;; custom function

(global-set-key (kbd "<f2>") 'open-emacs-file)
(global-set-key (kbd "<f3>") 'open-init-file)
(global-set-key (kbd "<f4>") 'animaker_run)
(global-set-key (kbd "<f5>") 'animaker_debug)



(global-set-key (kbd "M-1") 'winum-select-window-1)
(global-set-key (kbd "M-2") 'winum-select-window-2)
(global-set-key (kbd "M-3") 'winum-select-window-3)
(global-set-key (kbd "M-4") 'winum-select-window-4)
(global-set-key (kbd "M-5") 'winum-select-window-5)


(general-define-key
 :status '(normal visual)
 :keymaps 'override
 :prefix "M-m"

 "e" 'bing-dict-brief

 "l"  '(:wk "layout")
 "lc" 'eyebrowse-create-window-config
 "ld" 'eyebrowse-close-window-config
 "lb" 'eyebrowse-switch-to-window-config
 
 "lo" 'sessions-open
 "ls" 'sessions-save

 ;; swithc layout
 "l0" 'eyebrowse-switch-to-window-config-0
 "l1" 'eyebrowse-switch-to-window-config-1
 "l2" 'eyebrowse-switch-to-window-config-2
 "l3" 'eyebrowse-switch-to-window-config-3
 "l4" 'eyebrowse-switch-to-window-config-4
 "l5" 'eyebrowse-switch-to-window-config-5

 "s"  '(:wk "search")
 "ss"  'deadgrep

 "f"  '(:wk "files")
 "ff" 'find-file
 "fr" 'load-current-file

 "g"  '(:wk "magit")
 "gg" 'magit

 "p"  '(:wk "program")

 "pv" '(:wk "venv")
 "pvc" 'pyvenv-create
 "pva" 'pyvenv-activate
 "pvd" 'pyvenv-deactivate

 "d"  '(:wk "debug")
 "dd" 'dap-debug
 "dr" 'dap-debug-restart
 "dk" 'dap-disconnect

 "db" '(:wk "breakpoint")
 "dba" 'dap-breakpoint-add
 "dbt" 'dap-breakpoint-toggle
 "dbk" 'dap-breakpoint-delete-all

 "dw" '(:wk "debug window")
 "dws" 'dap-ui-show-many-windows
 "dwh" 'dap-ui-hide-many-windows

 "w"  '(:wk "window")
 "ww" 'writeroom-mode

 "T"  '(:wk "theme")
 "Tt"  'load-theme
 
)


;;;;;;;;;;;;;;;;;;;;;;;;
;;     系统环景变量
;;;;;;;;;;;;;;;;;;;;;;;;

;;  WSL2 Ubuntu24.02 ;;

;; 換源
;; Types: deb
;; URIs: https://mirrors.tuna.tsinghua.edu.cn/ubuntu
;; Suites: noble noble-updates noble-backports
;; Components: main restricted universe multiverse
;; Signed-By: /usr/share/keyrings/ubuntu-archive-keyring.gpg

;; dkpg問題
;; cd /var/lib/dpkg
;; sudo mv info info.bak
;; sudo mkdir info
;; sudo apt-get update

;; 中文輸入  sudo apt install librime-dev

;; 安裝字體和icon
;; M-x all-the-icons-install-fonts

;; grep   ripgrep 
;; python lsp   python3 -m pip install python-lsp-server[all]   
;; org latex preview    npm install mathjax-node-cli   apt-get install divpng
;; org export           sudo apt-get install texlive

;; fix PySide6/PyQt6 on wsl2 ubuntu24.02
;; sudo apt install libxkbcommon-x11-0libxcb-icccm4 libxcb-image0 libxcb-keysyms1 libxcb-render-util0 libxcb-xinerama0



(defun prepare_environment()
  (interactive)
  (async-shell-command "sudo apt install librime-dev libxkbcommon-x11-0libxcb-icccm4 libxcb-image0 libxcb-keysyms1 libxcb-render-util0 libxcb-xinerama0")
  )


