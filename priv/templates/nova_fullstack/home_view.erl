-module({{name}}_home_view).
-behaviour(arizona_view).
-compile({parse_transform, arizona_parse_transform}).

-export([mount/2, layout/1, render/1, handle_event/3]).

mount(_Arg, Req) ->
    Bindings = #{id => ~"view", count => 0},
    Path = arizona_request:get_path(Req),
    Layout = {?MODULE, layout, main_content, #{active_url => Path}},
    arizona_view:new(?MODULE, Bindings, Layout).

layout(Bindings) ->
    arizona_template:from_erl([
        ~"<!DOCTYPE html>",
        {html, [{lang, ~"en"}], [
            {head, [], [
                {meta, [{charset, ~"UTF-8"}], []},
                {meta, [{name, ~"viewport"},
                        {content, ~"width=device-width, initial-scale=1.0"}], []},
                {title, [], ~"{{name}}"},
                {link, [{rel, ~"stylesheet"}, {href, ~"/assets/css/app.css"}], []},
                {script, [{type, ~"module"}], ~"""
                import Arizona from '/assets/js/arizona.min.js';
                globalThis.arizona = new Arizona();
                arizona.connect('/live');
                """}
            ]},
            {body, [], [
                arizona_template:render_slot(maps:get(main_content, Bindings))
            ]}
        ]}
    ]).

render(Bindings) ->
    Count = arizona_template:get_binding(count, Bindings),
    arizona_template:from_erl(
        {'div', [{id, arizona_template:get_binding(id, Bindings)},
                 {class, ~"container"}], [
            {h1, [], ~"Welcome to {{name}}"},
            {p, [{class, ~"subtitle"}], ~"Powered by Nova + Arizona + Kura"},
            {'div', [{class, ~"counter"}], [
                {p, [{class, ~"count"}], [~"Count: ", integer_to_binary(Count)]},
                {'div', [{class, ~"buttons"}], [
                    {button, [{onclick, ~"arizona.pushEvent('decrement')"},
                              {class, ~"btn"}], ~"-"},
                    {button, [{onclick, ~"arizona.pushEvent('increment')"},
                              {class, ~"btn"}], ~"+"}
                ]}
            ]}
        ]}
    ).

handle_event(~"increment", _Params, View) ->
    State = arizona_view:get_state(View),
    Count = arizona_stateful:get_binding(count, State),
    NewState = arizona_stateful:put_binding(count, Count + 1, State),
    {[], arizona_view:update_state(NewState, View)};
handle_event(~"decrement", _Params, View) ->
    State = arizona_view:get_state(View),
    Count = arizona_stateful:get_binding(count, State),
    NewState = arizona_stateful:put_binding(count, Count - 1, State),
    {[], arizona_view:update_state(NewState, View)}.
