(defmodule {{name}}_main_controller
  (export
   (index 1)))

(defun index
  ;;
  ;; GET Handler
  ;;
  (_)
    `#(status 200 #M() "nova is running!"))
