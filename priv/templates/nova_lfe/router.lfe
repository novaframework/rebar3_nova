(defmodule {{name}}_router
  (export
   (routes 1)))

(defun routes
  (_)
   `(#M(prefix ""
         security false
         routes
           (
            #("/hearbeat" ,(lambda (_) #(status 200)) #M(methods (get)))
            #("/" ,(lambda (params) (novatrial_main_controller:index params))
              #M(methods (get)))
            )
           )))
