(defmodule {{name}}_routes
  (export
   (dispatch 0)))

(include-lib "logjam/include/logjam.hrl")

(defun dispatch (
   #m(prefix ""
      security false
      routes (#("/"
               #({{name}}_main_controller index)
               #M(methods (get))
               )
               #("/assets/[...]"
                 "assets")
               )
            )
      )
))