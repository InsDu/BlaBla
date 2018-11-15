%%%-------------------------------------------------------------------
%%% @author xfeiduu
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 14. Dec 2017 1:29 PM
%%%-------------------------------------------------------------------
-module(bla_server).
-author("xfeiduu").

%% API
-export([start/2,
         info/0,
         add_user/1]).
-export([init/1, handle_call/3]).

-record(user, {name,
               status = off % on|off|busy
              }).
-record(state, {name,
                users = [],
                maxLoginUsers = 5}).

start(Name, Max) ->
    gen_server:start({local, ?MODULE}, ?MODULE, [{Name, Max}], []).

info() ->
    gen_server:call({local, ?MODULE}, info).

add_user(Name) ->
    gen_server:call({local, ?MODULE}, {add_user, Name}).
	
find_user(Name) ->
    gen_server:call({local, ?MODULE}, {find_user, Name}).


init({Name, Max}) ->
    #state{name = Name,
           maxLoginUsers = Max}.


handle_call({find_user, UserName}, _From, #state{users=Users} = S) ->
    Result = 
	    case lists:keysearch(UserName, 2, Users) of
            false ->
                not_exist;
            {value, User} ->
                User
        end,
	{reply, Result, S};
handle_call({add_user, UserName}, _From, #state{users=Users} = S) ->
    User = #user{name = UserName,
                 status = off},
    NewUsers = add_user(User, Users),
    NewS = S#state{users = NewUsers},
    {reply, ok, NewS};
handle_call(info, _From, S) ->
    Info = {S#state.name, S#state.users},
    {reply, Info, S};
handle_call(_Request, _From, S) ->
    {reply, not_supported_request, S}.

add_user(#user{name = Name} = User, Users) ->
    case lists:keyfind(Name, 2, Users) of
        false ->
            [User|Users];
        true ->
            Users
    end.