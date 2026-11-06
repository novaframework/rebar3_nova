-module({{name}}_home_live).
-include_lib("arizona/include/arizona_stateful.hrl").

-export([mount/1, render/1, handle_event/3]).

mount(_Bindings) ->
    {#{id => ~"home", title => ~"{{name}}", count => 0}, #{}}.

render(Bindings) ->
    ?html(
        {main, [{id, ?get(id)}, {class, ~"container"}], [
            {h1, [], [~"Welcome to {{name}}"]},
            {p, [{class, ~"subtitle"}], [~"Powered by Nova, Arizona and Kura"]},
            {'div', [{class, ~"counter"}], [
                {p, [{class, ~"count"}], [~"Count: ", {span, [{id, ~"count"}], [?get(count)]}]},
                {'div', [{class, ~"buttons"}], [
                    {button, [{class, ~"btn"}, {az_click, arizona_js:push_event(~"dec")}], [~"-"]},
                    {button, [{class, ~"btn"}, {az_click, arizona_js:push_event(~"inc")}], [~"+"]}
                ]}
            ]}
        ]}
    ).

handle_event(~"inc", _Payload, Bindings) ->
    {Bindings#{count => maps:get(count, Bindings) + 1}, #{}, []};
handle_event(~"dec", _Payload, Bindings) ->
    {Bindings#{count => maps:get(count, Bindings) - 1}, #{}, []}.
