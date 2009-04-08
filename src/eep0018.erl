% Copyright (c) 2008-2009 Paul J. Davis <paul.joseph.davis@gmail.com>
% Copyright (c) 2008-2009 Enrico Thierbach <eno@open-lab.org>
%
% This file is part of EEP0018, which is released under the MIT
% license.

-module(eep0018).

-export([json_to_term/1, term_to_json/1]).

% Public API

term_to_json(Term) ->
    Bin = term_to_binary(Term),
    case erlang:port_control(drv_port(), 0, Bin) of
        <<"error">> ->
            throw({json_error, {invalid_input, Term}});
        JSON ->
            JSON
    end.

json_to_term(Json) when is_list(Json) ->
    json_to_term(list_to_binary(Json));
json_to_term(Json) when is_binary(Json) ->
    % The null byte is important for bare literals. Without it
    % yajl will throw a fit because it doesn't think it's finished
    % parsing correctly.
    [] = erlang:port_control(drv_port(), 1, <<Json/binary, 0:8>>),
    receive
        {error, Reason} ->
            throw({json_error, Reason});
        Term ->
            Term
    end.

% Implementation

init() ->
    case erl_ddll:load_driver(code:priv_dir(eep0018), eep0018_drv) of
        ok -> ok;
        {error, permanent} -> ok               % Means that the driver is already active
    end,
    Port = open_port({spawn, eep0018_drv}, [binary]),
    erlang:put(eep0018_drv_port, Port),
    Port.

drv_port() ->
    case get(eep0018_drv_port) of
        undefined -> init();
        Port      -> Port
    end.
