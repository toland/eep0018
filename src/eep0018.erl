% Copyright (c) 2008-2009 Paul J. Davis <paul.joseph.davis@gmail.com>
% Copyright (c) 2008-2009 Enrico Thierbach <eno@open-lab.org>
%
% This file is part of EEP0018, which is released under the MIT
% license.

-module(eep0018).

-export([json_to_term/1, term_to_json/1, reformat/1]).

%% Export gen_server that's used for process to ensure the
%% driver code doesn't get unloaded. Hack? I think so.
-behaviour(gen_server).
-export([start_link/0,
         init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% ====================================================================
%% Public API
%% ====================================================================

term_to_json(Term) ->
    [] = erlang:port_control(drv_port(), 0, term_to_binary(Term)),
    term_to_json_loop(drv_port(), <<>>).

json_to_term(Json) when is_list(Json) ->
    json_to_term(list_to_binary(Json));
json_to_term(Json) when is_binary(Json) ->
    %% The null byte is important for bare literals. Without it
    %% yajl will throw a fit because it doesn't think it's finished
    %% parsing correctly.
    [] = erlang:port_control(drv_port(), 1, <<Json/binary, 0:8>>),
    receive
        {json_ok, Term} ->
            Term;
        {json_error, Reason} ->
            throw({json_error, Reason})
    end.

reformat(Json) when is_list(Json) ->
    reformat(list_to_binary(Json));
reformat(Json) when is_binary(Json) ->
    [] = erlang:port_control(drv_port(), 2, <<Json/binary, 0:8>>),
    receive
        {ok, Formatted} ->
            Formatted;
        {error, Reason} ->
            throw({json_error, Reason})
    end.

%% ====================================================================
%% Internal functions
%% ====================================================================

drv_port() ->
    case get(eep0018_drv_port) of
        undefined ->
            ensure_started(),
            Port = open_port({spawn, eep0018_drv}, [binary]),
            erlang:put(eep0018_drv_port, Port),
            Port;
        Port ->
            Port
    end.

ensure_started() ->
    case whereis(eep0018_server) of
        undefined ->
            C = {eep0018_server, {?MODULE, start_link, []}, permanent, 1000,
                 worker, [?MODULE]},
            supervisor:start_child(kernel_safe_sup, C),
            ok;
        _ ->
            ok
    end.


term_to_json_loop(Port, Json) ->
    receive
        {Port, {data, Bin}} when is_binary(Bin) ->
            term_to_json_loop(Port, <<Json/binary, Bin/binary>>);
        term_to_json_complete ->
            Json;
        term_to_json_error ->
            throw({json_error, "Conversion of Erlang term to JSON failed."})
    end.


%% ====================================================================
%% gen_server callbacks/impl
%% ====================================================================

%% @hidden
start_link() ->
    gen_server:start_link({local, eep0018_server}, ?MODULE, [], []).

%% @hidden
init([]) ->
    ok = erl_ddll:load_driver(code:priv_dir(eep0018), eep0018_drv),
    Port = open_port({spawn, eep0018_drv}, [binary]),
    {ok, Port}.

%% @hidden
handle_call(_Msg, _From, State) ->
    {stop, unsupportedOperation, State}.

%% @hidden
handle_cast(_Msg, State) ->
    {stop, unsupportedOperation, State}.

%% @hidden
handle_info(_Info, State) ->
    {stop, unsupportedOperation, State}.    

%% @hidden
terminate(_Reason, Port) ->
    true = port_close(Port),
    ok.

%% @hidden
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
