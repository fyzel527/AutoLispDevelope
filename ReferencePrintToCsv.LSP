(defun get-entity-center (obj)
  (cond
    ((vlax-property-available-p obj 'Center)
     (vlax-get obj 'Center)) ; 圆、圆弧等
    ((vlax-property-available-p obj 'InsertionPoint)
     (vlax-get obj 'InsertionPoint)) ; 块参照、文字等
    ((vlax-method-applicable-p obj 'GetBoundingBox)
     (progn
       (setq minp (vlax-make-safearray vlax-vbDouble '(0 . 2)))
       (setq maxp (vlax-make-safearray vlax-vbDouble '(0 . 2)))
       (vl-catch-all-apply 'vlax-invoke (list obj 'GetBoundingBox minp maxp))
       (setq min (vlax-safearray->list minp))
       (setq max (vlax-safearray->list maxp))
       (mapcar '(lambda (a b) (/ (+ a b) 2.0)) min max))) ; 矩形、多段线等
    (t nil)))

(defun write-to-csv (filepath data)
  (setq file (open filepath "w"))
  (write-line "Entity Type, Center X, Center Y, Center Z" file) ; 写入CSV头
  (foreach item data
    (write-line (strcat (car item) "," (vl-princ-to-string (cadr item)) "," (vl-princ-to-string (caddr item)) "," (vl-princ-to-string (cadddr item))) file))
  (close file))

(defun c:exportCentersToCSV ()
  (vl-load-com) ; 确保 Visual LISP 函数可用
  (princ "\nType 'exportCentersToCSV' to run.\n")
  
  ; 设置CSV文件路径
  (setq filepath (getfiled "Save CSV File" "C:/Users/Fyzel/Documents/output2.csv" "csv" 1))
  
  ; 框选当前激活文档中的所有对象
  (setq ss (ssget "X")) ; 使用 "X" 获取所有对象
  (if ss
    (progn
      (princ "\nSelection set created.\n")
      (setq len (sslength ss))
      (setq all-data '())

      ; 获取所有对象的中心点并存储数据
      (repeat len
        (setq ent (ssname ss (setq i (1- len)))) ; 获取单个对象
        (setq obj (vlax-ename->vla-object ent)) ; 转换为 COM 对象
        (setq center (get-entity-center obj)) ; 获取中心点
        (if center
          (setq all-data (cons (list (vlax-get obj 'ObjectName) (car center) (cadr center) (caddr center)) all-data)) ; 存储数据
          (progn
            (princ (strcat "\nFailed to get center for object: " (vl-princ-to-string ent) "\n"))
            (setq all-data (cons (list (vlax-get obj 'ObjectName) 0.0 0.0 0.0) all-data)))) ; 处理无中心点的情况
      )

      ; 写入CSV文件
      (write-to-csv filepath all-data)
      (princ (strcat "\nData written to " filepath "\n"))
    )
    (princ "\nNo objects selected.\n")
  )
  (princ)
)

(princ "\nType 'exportCentersToCSV' to run.\n")
