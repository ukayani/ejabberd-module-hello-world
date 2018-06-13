%%%-------------------------------------------------------------------------
%%% Hello World Module
-module(mod_helloworld).

%% All Ejabberd modules need to implement this behaviour
-behaviour(gen_mod).

%% Need for hooking into ejabber events
-include("ejabberd.hrl").
-include("xmpp.hrl").
%% Need for looking up user's roster
-include("mod_roster.hrl").
-include("logger.hrl").

%% This is the public interface for our module
%% modules are required to export start , stop at a minimum.
-export([start/2, stop/1, depends/2, mod_opt_type/1, reload/3, in_subscription/6, get_subscribed/2]).
-define(EMPTY_URL, <<"">>).

%% Called when module is loaded by server
start(Host, Opts) ->

    ?INFO_MSG("Hello, ejabberd world!~n", []),
    %% Retrieve config option for our module 'url' corresponds to name the key in our config
    Url = gen_mod:get_opt(url, Opts, ?EMPTY_URL),
    ?INFO_MSG("Option: Url = ~p", [Url]),
    %% Subscribe our module to presence subscriptions (in)
    %% In other words, subscribe to friend requests and friend request acceptance
    %% runs in_subscription fun when events fire
    ejabberd_hooks:add(roster_in_subscription, Host, ?MODULE, in_subscription, 50),
    ok.

%% Called when module is stopped by server
stop(Host) ->
    ?INFO_MSG("Bye bye, ejabberd world!", []),

    %% clean up, remove our handler for the hook
    ejabberd_hooks:delete(roster_in_subscription, Host, ?MODULE, in_subscription, 50),
    ok.

in_subscription(_, User, Server, JID, Type, Reason) ->
    process_subscription(in, User, Server, JID, Type,
        Reason).

%% Called when a subscription request is received by the server
%% Params
%% User - The user that is receiving the request
%% Server - The server
%% Type - (subscribe, subscribed, unsubscribe, unsubscribed)
%% JID - The id of the user that initiated the request
process_subscription(_, User, Server, JID1,
    subscribe, _) ->

    SUser = binary_to_list(User),
    SJID = format_jid(JID1),

    Items = get_subscribed(User, Server),
    ?INFO_MSG("--------------------------------------------", []),
    ?INFO_MSG("--- Subscription Initiated -----------------", []),
    ?INFO_MSG("~p recieved a subscription request from ~p", [SUser, SJID]),
    ?INFO_MSG("~p", [Items]),
    ?INFO_MSG("--------------------------------------------", []);

%% Called when a subscription request acceptance is received by the server
%% Params
%% User - The user that initiated the original subscription request
%% Server - The server
%% Type - (subscribe, subscribed, unsubscribe, unsubscribed)
%% JID - The id of the user that accepted the request
process_subscription(_, User, _Server, JID1,
    subscribed, _) ->

    SUser = binary_to_list(User),
    SJID = format_jid(JID1),

    ?INFO_MSG("--------------------------------------------", []),
    ?INFO_MSG("--- Subscription Accepted -----------------", []),
    ?INFO_MSG("~p accepted ~p's request", [SJID, SUser]),
    ?INFO_MSG("--------------------------------------------", []);

%% Called when anything other than subscribe/subscribed event is received (ie. everything related to unsubscribe)
process_subscription(_Direction, _User, _Server, _JID1,
    _, _Reason) ->

    ?INFO_MSG("--------------------------------------------", []),
    ?INFO_MSG("Processing unsubscribe(d)", []),
    ?INFO_MSG("--------------------------------------------", []).

%% Retrieves a list of users that the given User has subscribed to (the user's friends list)
get_subscribed(User, Server) ->
  %% Delegating to the mod_roster module to get the roster list
  Items = mod_roster:get_roster(User, Server),

  %% Filter the roster list to only contacts that the user is following or has mutual friendship with
  %% Sub == to is when the User follows the given contact but that contact does not follow them
  %% Sub == to only when the contact accept's the user's follow/subscribe request
  %% Sub == both is when the User and the contact both follow each other
  [{Sub, format_jid(JID)} || #roster{subscription = Sub, jid = JID} <- Items, (Sub == to orelse Sub == both)].

format_jid(JID) ->
    binary_to_list(jid:to_string(JID)).

reload(_Host, _NewOpts, _OldOpts) ->
    ok.

depends(_B, _O) -> [].

%% Tell ejabberd which config options this module accepts and how to transform them into types we accept
%% We only accept one option 'url', see conf/mod_helloworld.conf.example
mod_opt_type(url) -> fun iolist_to_binary/1;
mod_opt_type(_) -> [url].