(defun print-entity-type (obj)
  (princ (strcat "\nEntity Type: " (vlax-get obj 'ObjectName))))

(defun c:printSelectedEntitiesType ()
  (vl-load-com) ; 确保 Visual LISP 函数可用
  (princ "\nType 'printSelectedEntitiesType' to run.\n")
  
  ; 框选当前激活文档中的所有对象
  (setq ss (ssget)) ; 获取选定的对象
  (if ss
    (progn
      (princ "\nSelection set created.\n")
      (setq len (sslength ss))

      ; 获取所有对象的类型并打印
      (repeat len
        (setq ent (ssname ss (setq i (1- len)))) ; 获取单个对象
        (setq obj (vlax-ename->vla-object ent)) ; 转换为 COM 对象
        (print-entity-type obj)) ; 打印对象类型
    )
    (princ "\nNo objects selected.\n")
  )
  (princ)
)

(princ "\nType 'printSelectedEntitiesType' to run.\n")
