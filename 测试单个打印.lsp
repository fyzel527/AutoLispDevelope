(defun get-blockreference-center (block)
  (vlax-get block 'InsertionPoint)) ; 获取块参照的插入点

(defun print-blockreference-info (obj center)
  (princ (strcat "\nObject ID: " (vl-princ-to-string (vla-get-ObjectID obj))
                 "\nBlockReference Center - X: " (vl-princ-to-string (car center))
                 ", Y: " (vl-princ-to-string (cadr center))
                 ", Z: " (vl-princ-to-string (caddr center)))))

(defun write-to-csv (filepath data)
  (setq file (open filepath "w"))
  (write-line "Object ID, Center X, Center Y, Center Z" file) ; 写入CSV头
  (foreach item data
    (write-line (strcat (vl-princ-to-string (car item)) "," 
                        (vl-princ-to-string (cadr item)) "," 
                        (vl-princ-to-string (caddr item)) "," 
                        (vl-princ-to-string (cadddr item))) file))
  (close file))

(defun c:printBlockReferenceCenters ()
  (vl-load-com) ; 确保 Visual LISP 函数可用
  (princ "\nType 'printBlockReferenceCenters' to run.\n")
  
  ; 框选当前激活文档中的所有块参照对象
  (setq ss (ssget '((0 . "INSERT")))) ; 只选择块参照对象
  (if ss
    (progn
      (princ "\nSelection set created.\n")
      (setq len (sslength ss))
      (setq i 0)
      (setq all-data '())

      ; 获取所有块参照的插入点并打印数据
      (while (< i len)
        (setq ent (ssname ss i)) ; 获取单个对象
        (setq obj (vlax-ename->vla-object ent)) ; 转换为 COM 对象
        (setq center (get-blockreference-center obj)) ; 获取插入点
        (print-blockreference-info obj center) ; 打印对象ID和位置信息
        (setq all-data (cons (list (vla-get-ObjectID obj) (car center) (cadr center) (caddr center)) all-data)) ; 存储数据
        (setq i (1+ i))) ; 增加索引

      ; 设置CSV文件路径
      (setq filepath (getfiled "Save CSV File" "C:\Users\Fyzel\Documents\output2.csv" "csv" 1))

      ; 写入CSV文件
      (if filepath
        (write-to-csv filepath all-data)
        (princ "\nNo file selected.\n"))
    )
    (princ "\nNo block references selected.\n")
  )
  (princ)
)

(princ "\nType 'printBlockReferenceCenters' to run.\n")
