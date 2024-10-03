;; 基础设置
(set-face-attribute 'default nil :height 160);;

(setq make-backup-files nil)     ;;
(setq inhibit-startup-screen t)
(load-theme 'tango-dark)
(show-paren-mode t)           ;; 打开括号匹配显示模式

;; Custom Function
(defun open-init-file()
  (interactive)
  (if (string-equal (prin1-to-string (current-buffer)) "#<buffer init.el>") (load-file "~/.emacs.d/init.el") (find-file "~/.emacs.d/init.el")))
(global-set-key (kbd "<f2>") 'open-init-file)
