%%%-------------------------------------------------------------------
%%% @author radek
%%% @copyright (C) 2021, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 14. maj 2021 18:24
%%%-------------------------------------------------------------------
-module(session_manager).
-author("radek").
-behaviour(gen_server).

%% API

-export([
  start_link/0,
  handle_call/3,
  handle_cast/2,
  init/1]).

-export([
  generate_new_session/0,
  check_exists/1,
  refresh_session/1,
  clear_session/1,
  delete_session/1,
  get_all_sessions/0,
  add_session_info/3,
  get_session_info/1,
  check_active/1,
  start/0,
  stop/0
]).

%% MODULE VARIABLES
timeout() -> "20".
sessions_set_key() -> "sessions".
sessions_key(Id) -> sessions_set_key() ++ ":" ++ lists:flatten(io_lib:format("~p", [Id])).
session_is_active_key(Id) -> sessions_set_key() ++ ":" ++ lists:flatten(io_lib:format("~p", [Id])) ++ ":acite".

init(_) -> eredis:start_link().
start_link() -> gen_server:start_link({local,?MODULE},?MODULE,[],[]).

%% API FOR THE CLIENT
stop() -> gen_server:call(?MODULE,stop).
generate_new_session() -> gen_server:call(?MODULE, generate_new_session ).
check_exists(SessionId) -> gen_server:call(?MODULE, {check_exists, SessionId} ).
refresh_session(SessionId) -> gen_server:call(?MODULE, {refresh_session, SessionId} ).
delete_session(SessionId) -> gen_server:call(?MODULE, {delete_session, SessionId} ).
clear_session(SessionId) -> gen_server:call(?MODULE, {clear_session, SessionId} ).
check_active(SessionId) -> gen_server:call(?MODULE, {check_active, SessionId} ).
get_all_sessions() ->  gen_server:call(?MODULE, get_all_sessions ).
add_session_info(SessionId, Key, Value) -> gen_server:call(?MODULE, {add_session_info, SessionId, Key, Value} ).
get_session_info(SessionId) -> gen_server:call(?MODULE, {get_session_info, SessionId} ).


check_exists_inner(Id, Connection) ->
  case eredis:q(Connection, ["SISMEMBER" | [sessions_set_key(), Id]]) of
    {ok, <<"0">>} -> {ok, false};
    {ok, <<"1">>} -> {ok, true};
    {error, Msg} -> {error, Msg}
  end.

check_active_inner(Id, Connection) ->
  case eredis:q(Connection, ["EXISTS" | [session_is_active_key(Id)]]) of
    {ok, <<"0">>} -> {ok, false};
    {ok, <<"1">>} -> {ok, true};
    {error, Msg} -> {error, Msg}
  end.

generate_id(Connection) ->
  Id = rand:uniform(10000000000),
  case eredis:q(Connection, ["SADD" | [sessions_set_key(), Id]]) of
    {ok, <<"0">>} -> generate_id(Connection);
    {ok, <<"1">>} -> {ok, Id};
    {error, Msg} -> {error, Msg }
  end.

%% MESSAGES HANDLING
handle_call(generate_new_session, _From, Connection) ->
  case generate_id(Connection) of
    {ok, Id} ->
      case eredis:q(Connection, ["SET" | [session_is_active_key(Id), "active", "EX", timeout()]]) of
        {ok, _} ->
          io:fwrite("New session generated ~w~n", [Id]),
          {reply, {ok, Id}, Connection};
        {error, Msg} -> {error, Msg }
      end;
    {error, Msg} -> {reply, {error, Msg}, Connection}
  end;

handle_call({check_exists, SessionId}, _From, Connection) ->
  case eredis:q(Connection, ["SISMEMBER" | [sessions_set_key(), SessionId]]) of
    {ok, <<"0">>} -> {reply, {ok, false}, Connection};
    {ok, <<"1">>} -> {reply, {ok, true}, Connection};
    {error, Msg} -> {error, Msg}
  end;

handle_call({check_active, SessionId}, _From, Connection) ->
  case eredis:q(Connection, ["EXISTS" | [session_is_active_key(SessionId)]]) of
    {ok, <<"0">>} -> {reply, {ok, false}, Connection};
    {ok, <<"1">>} -> {reply, {ok, true}, Connection};
    {error, Msg} -> {error, Msg}
  end;

handle_call({add_session_info, SessionId, Key, Value}, _From, Connection) ->
  case check_exists_inner(SessionId, Connection) of
    {ok, true} ->
      case check_active_inner(SessionId, Connection) of
        {ok, true} -> eredis:q(Connection, ["HSET" | [sessions_key(SessionId), Key, Value]]);
        {ok, false} -> {reply, {error, "Session expired"}, Connection};
        {error, Msg} -> {error, Msg}
      end;
    {ok, false} -> {reply, {error, "Session do not exists"}, Connection};
    {error, Msg} -> {error, Msg}
  end;

handle_call({get_session_info, SessionId}, _From, Connection) ->
  case check_exists_inner(SessionId, Connection) of
    {ok, true} ->
      case check_active_inner(SessionId, Connection) of
        {ok, true} ->
          {ok, Objects} = eredis:q(Connection, ["HGETALL" | [sessions_key(SessionId)]]),
          {reply, {ok, Objects}, Connection};
        {ok, false} -> {reply, {error, "Session expired"}, Connection};
        {error, Msg} -> {error, Msg}
      end;
    {ok, false} -> {reply, {error, "Session do not exists"}, Connection};
    {error, Msg} -> {error, Msg}
  end;

handle_call(get_all_sessions, _From, Connection) ->
  case eredis:q(Connection, ["SMEMBERS" | [sessions_set_key()]]) of
    {ok, Members} -> {reply, {ok, Members}, Connection};
    {error, Msg} -> {error, Msg}
  end;

handle_call({clear_session, SessionId}, _From, Connection) ->
  case eredis:q(Connection, ["DEL" | [sessions_key(SessionId)]]) of
    {ok, _} -> {reply, ok, Connection};
    {error, Msg} -> {error, Msg}
  end;

handle_call({refresh_session, SessionId}, _From, Connection) ->
  case check_exists_inner(SessionId, Connection) of
    {ok, true} ->
      case check_active_inner(SessionId, Connection) of
        {ok, true} ->
          case eredis:q(Connection, ["SET" | [session_is_active_key(SessionId), "active", "EX", timeout()]]) of
            {ok, _} ->
              io:fwrite("Session ~w refreshed ~n", [SessionId]),
              {reply, ok, Connection};
            {error, Msg} -> {error, Msg }
          end;
        {ok, false} -> {reply, {error, "Session expired"}, Connection};
        {error, Msg} -> {error, Msg}
      end;
    {ok, false} -> {reply, {error, "Session do not exists"}, Connection};
    {error, Msg} -> {error, Msg}
  end;


handle_call({delete_session, SessionId}, _From, Connection) ->
  case {eredis:q(Connection, ["DEL" | [sessions_key(SessionId)]]), eredis:q(Connection, ["SREM" | [sessions_set_key(), SessionId]])} of
    {{ok, _}, {ok, _}} -> {reply, ok, Connection};
    {{_, Msg1}, {_, Msg2}} -> {error, Msg1 ++ Msg2, Connection}
  end;

handle_call(stop, _From, Connection) -> {stop, normal, ok, Connection}.

handle_cast(_, _) -> {error, "no such command"}.

start() ->
  {ok, C} = eredis:start_link(),
  {ok, <<"OK">>} = eredis:q(C, ["SET", "oss", "bar"]).


