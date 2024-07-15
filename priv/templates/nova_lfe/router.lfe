(defmodule {{name}}_router
  (export
   (routes 1)))

(defun routes
  (_)
   '(#M(prefix ""
         security false
         routes
           (#("/hearbeat" funcall (lambda (_)  (tuple 'status' 200)), #M(methods (get))),
            #("/" funcall #'{{name}}_main_controller:index/1' #M(methods (get))))
           )))