(defmodule {{name}}_main_controller
  (export
   (index 1)))

(defun index
  ;;
  ;; GET Handler
  ;;
  (_)
    `#(ok (#(message "nova is running!"))))