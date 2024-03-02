(defmodule {{name}}_router
  (export
   (routes 1)))

(defun routes
  (_)
   '(#M(prefix ""
         security false
         routes
           (#("/" #({{name}}_main_controller index) #M(methods (get))))
           )))