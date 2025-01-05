;;; init-yj.el --- Insert description here -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; inspired by: https://github.com/RealKai42/liu-yao-divining

(require 'json)
(require 'time-date)

(defvar liu-yao-gua-dict
  '(("阳阳阳" . "乾")
    ("阴阴阴" . "坤") 
    ("阴阳阳" . "兑")
    ("阳阴阳" . "震")
    ("阳阳阴" . "巽")
    ("阴阳阴" . "坎")
    ("阳阴阴" . "艮")
    ("阴阴阳" . "离")))

(defvar liu-yao-number-dict
  '((0 . "初爻")
    (1 . "二爻")
    (2 . "三爻")
    (3 . "四爻")
    (4 . "五爻")
    (5 . "六爻")))

(defvar liu-yao-data-directory
  (expand-file-name "~/.emacs.d")
  "Directory containing liu-yao data files.")

(defun liu-yao-set-random-seed ()
  "Set random seed based on current time"
  (random t))

(defun liu-yao-find-gua-info (gua-key gua-data)
  "Find gua info from gua-data by key"
  (let ((result nil))
    (maphash (lambda (k v)
               (when (string= k gua-key)
                 (setq result v)))
             gua-data)
    result))

(defun liu-yao-load-gua-json ()
  "Load gua.json file"
  (let* ((json-file (expand-file-name "gua.json" liu-yao-data-directory))
         (_ (message "尝试加载文件: %s" json-file)))
    (if (file-exists-p json-file)
        (let* ((json-string (with-temp-buffer
                              (insert-file-contents json-file)
                              (buffer-string)))
               (json-object-type 'hash-table)  ; 使用哈希表
               (json-array-type 'list)
               (data (json-read-from-string json-string)))
          (message "成功加载 JSON 数据，包含 %d 个卦象" (hash-table-count data))
          data)
      (error "找不到 gua.json 文件: %s\n请确保文件存在并设置正确的 liu-yao-data-directory 变量" json-file))))

(defun liu-yao-get-3-coin ()
  "Get 3 random coins (0 or 1)"
  (mapcar (lambda (_) (random 2)) '(1 2 3)))

(defun liu-yao-get-yin-yang (coin-result)
  "Convert coin result to yin/yang"
  (if (> (apply '+ coin-result) 1.5) "阳" "阴"))

(defun liu-yao-format-coin-result (coin-result i)
  "Format the coin toss result"
  (format "%s 为 %s 为 %s"
          (cdr (assoc i liu-yao-number-dict))
          (mapconcat (lambda (x) (if (> x 0.5) "背" "字")) coin-result "")
          (liu-yao-get-yin-yang coin-result)))

(defun liu-yao-divination (question)
  "Do a liu yao divination for the given question"
  (liu-yao-set-random-seed)
  (let* ((gua-data (liu-yao-load-gua-json))
         (_ (message "数据类型: %s" (type-of gua-data)))
         (first-results '())
         (second-results '())
         (output '())
         first-gua
         second-gua)
    
    ;; First three coins
    (dotimes (i 3)
      (let* ((coins (liu-yao-get-3-coin))
             (yin-yang (liu-yao-get-yin-yang coins)))
        (push yin-yang first-results)
        (push (liu-yao-format-coin-result coins i) output)))
    
    ;; 处理首卦
    (let* ((first-gua-key (mapconcat 'identity (reverse first-results) ""))
           (_ (message "首卦键值: %s" first-gua-key))
           (first-gua-lookup (assoc first-gua-key liu-yao-gua-dict))
           (_ (message "首卦查找结果: %s" first-gua-lookup)))
      (setq first-gua (if first-gua-lookup
                          (cdr first-gua-lookup)
                        (progn
                          (message "未找到首卦: %s" first-gua-key)
                          "未知")))
      (push (format "首卦为：%s" first-gua) output))
    
    ;; Second three coins  
    (dotimes (i 3)
      (let* ((coins (liu-yao-get-3-coin))
             (yin-yang (liu-yao-get-yin-yang coins)))
        (push yin-yang second-results)
        (push (liu-yao-format-coin-result coins (+ i 3)) output)))
    
    ;; 处理次卦和结果
    (let* ((second-gua-key (mapconcat 'identity (reverse second-results) ""))
           (_ (message "次卦键值: %s" second-gua-key))
           (second-gua-lookup (assoc second-gua-key liu-yao-gua-dict))
           (_ (message "次卦查找结果: %s" second-gua-lookup)))
      (setq second-gua (if second-gua-lookup
                           (cdr second-gua-lookup)
                         (progn
                           (message "未找到次卦: %s" second-gua-key)
                           "未知")))
      
      (let* ((gua-key (concat first-gua second-gua))
             (_ (message "完整卦象键值: %s" gua-key))
             (_ (message "正在查找卦象: %s" gua-key))
             (gua-info (gethash gua-key gua-data))
             (_ (message "查找结果: %s" gua-info))
             (gua-name (if gua-info
                           (gethash "name" gua-info)
                         (format "未知卦象：%s" gua-key)))
             (_ (message "卦名: %s" gua-name))
             (gua-des (if gua-info
                          (gethash "des" gua-info)
                        "无法解析此卦象"))
             (_ (message "卦象描述: %s" gua-des))
             (gua-sentence (if gua-info
                               (gethash "sentence" gua-info)
                             "请检查卦象数据"))
             (_ (message "卦辞: %s" gua-sentence)))
        
        (push (format "次卦为：%s" second-gua) output)
        (push (format "\n六爻结果: %s\n卦名为：%s\n%s\n卦辞为：%s"
                      gua-key
                      gua-name
                      gua-des
                      gua-sentence)
              output)))
    
    (mapconcat 'identity (reverse output) "\n")))

(defun liu-yao ()
  "Interactive liu yao divination"
  (interactive)
  (let ((question (read-string "请输入你的问题: ")))
    (with-current-buffer (get-buffer-create "*scratch*")
      (goto-char (point-max))
      (insert "\n\n")  ; 添加空行分隔
      (insert (format "问题：%s\n\n" question))
      (insert (liu-yao-divination question)))))

(provide 'init-yj) ;;; init-yj.el ends here
