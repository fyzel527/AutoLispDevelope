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

(defun update-progress (progress total)
  (setq percentage (fix (* (/ progress total) 100)))
  (setq bar (repeat percentage (chr 9608))) ; 9608 是 Unicode 的一个满方块字符
  (grtext -1 (strcat "Progress: [" bar "] " (itoa percentage) "%"))
  (grread T 1 0.1)) ; 短暂延迟来刷新显示

(defun c:moveCentersToOrigin ()
  (vl-load-com) ; 确保 Visual LISP 函数可用
  (princ "\nType 'moveCentersToOrigin' to run.\n")

  ; 框选当前激活文档中的所有对象
  (setq ss (ssget "X")) ; 使用 "X" 获取所有对象
  (if ss
    (progn
      (princ "\nSelection set created.\n")
      (setq len (sslength ss))
      (setq all-centers '())

      ; 获取所有对象的中心点并显示进度
      (repeat len
        (setq ent (ssname ss (setq i (1- len)))) ; 获取单个对象
        (setq obj (vlax-ename->vla-object ent)) ; 转换为 COM 对象
        (setq center (get-entity-center obj)) ; 获取中心点
        (if center
          (setq all-centers (cons center all-centers)) ; 存储中心点
        )
        (update-progress (- len i) len) ; 更新进度
      )

      ; 计算所有中心点的几何中心
      (setq sum (apply 'mapcar (cons '+ all-centers)))
      (setq count (length all-centers))
      (setq geo-center (mapcar '(lambda (x) (/ x count)) sum))

      ; 移动所有对象到原点并显示进度
      (repeat len
        (setq ent (ssname ss (setq i (1- len)))) ; 获取单个对象
        (setq obj (vlax-ename->vla-object ent)) ; 转换为 COM 对象
        (vlax-invoke-method obj 'Move (vlax-3d-point geo-center) (vlax-3d-point '(0.0 0.0 0.0))) ; 移动对象
        (princ (strcat "\nObject " (vl-princ-to-string ent) " moved.\n"))
        (update-progress (- len i) len) ; 更新进度
      )
    )
    (princ "\nNo objects selected.\n")
  )
  (grtext -1 "") ; 清除进度条
  (princ)
)

(princ "\nType 'moveCentersToOrigin' to run.\n")
