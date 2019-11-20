#lang racket

(require graphics)
(open-graphics)

;定义框架这一结构体
(struct frame (o x y))

;向量相加与数乘
(define (add p1 p2)
  (make-posn (+ (posn-x p1) (posn-x p2))
             (+ (posn-y p1) (posn-y p2))))
(define (scale s p)
  (make-posn (* s (posn-x p)) (* s (posn-y p))))

;cmap将一个相对坐标p映射到框架f下的实际坐标
(define ((cmap f) p)
  (add (frame-o f)
       (add (scale (posn-x p) (frame-x f))
            (scale (posn-y p) (frame-y f)))))

;transform将相对坐标框架(ox oy ...)转换到框架f下的实际坐标框架，
;再将它传递给画家，lc意即线性组合
(define (((transform ox oy xx xy yx yy) painter) f)
  (let ((o (frame-o f))
        (x (frame-x f))
        (y (frame-y f)))
    (let ((lc (lambda (a b)
                (add (scale a x) (scale b y)))))
      (painter (frame (add o (lc ox oy))
                      (lc xx xy)
                      (lc yx yy))))))

;定义示例画布
(define canvas
  (open-viewport "canvas" 512 512))

;定义示例框架
(define frame0
  (frame (make-posn 0 0)
         (make-posn 512 0)
         (make-posn 0 512)))

;graphics提供了一些绘制图形的接口，如画线
(define line (draw-line canvas))

;lines接受相对坐标的线的表，产生画家
;亦可以使用for-each
(define ((lines lst) f)
  (let ((m (cmap f)))
    (define (iter rest)
      (unless (null? rest)
        (line (m (make-posn (car (car (car rest)))
                            (cadr (car (car rest)))))
              (m (make-posn (car (cadr (car rest)))
                            (cadr (cadr (car rest))))))
        (iter (cdr rest))))
    (iter lst)))

;combine组合起两个画家
(define ((combine painter1 painter2) f)
  (painter1 f) (painter2 f))

;一些示例转换
;mirror将图形左右翻转(就像照镜子)
(define mirror (transform 1 0 -1 0 0 1))
;flip将图形上下颠倒
(define flip (transform 0 1 1 0 0 -1))
;更多例子请见SICP
