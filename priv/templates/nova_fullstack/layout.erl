-module({{name}}_layout).
-include_lib("arizona/include/arizona_stateless.hrl").

-export([render/1]).

render(Bindings) ->
    ?html([
        ~"<!DOCTYPE html>",
        {html, [{lang, ~"en"}], [
            {head, [], [
                {meta, [{charset, ~"utf-8"}], []},
                {meta, [{name, ~"viewport"}, {content, ~"width=device-width, initial-scale=1"}],
                    []},
                {title, [], [?get(title, ~"{{name}}")]},
                {link, [{rel, ~"icon"}, {href, ~"/assets/favicon.ico"}], []},
                {link, [{rel, ~"stylesheet"}, {href, ~"/assets/css/app.css"}], []}
            ]},
            {body, [], [
                ?inner_content,
                {script, [{type, ~"module"}], [
                    ~"""
                    import { connect } from '/assets/arizona/arizona.min.js';
                    connect('/live/ws');
                    """
                ]}
            ]}
        ]}
    ]).
