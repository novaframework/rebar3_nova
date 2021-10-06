#{prefix => "",
  security => false,
  routes => [
            {"/", { {{name}}_main_controller, index}, #{methods => [get]}},
            {"/assets/[...]", "assets"}
           ]
}.
