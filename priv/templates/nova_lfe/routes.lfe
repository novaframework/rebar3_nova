(defmodule {{name}}_routing
  (export
   (routes 1)))

(include-lib "logjam/include/logjam.hrl")

(defun routes (({{ name }})
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