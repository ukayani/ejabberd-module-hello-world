-module(mod_helloworld).

-behaviour(gen_mod).

%% Required by ?INFO_MSG macros
-include("ejabberd.hrl").
-include("xmpp.hrl").
-include("logger.hrl").

%% gen_mod API callbacks
-export([start/2, stop/1, depends/2, mod_opt_type/1, reload/3, in_subscription/6]).
-define(EMPTY_URL, <<"">>).

start(Host, Opts) ->

    ?INFO_MSG("Hello, ejabberd world!~n", []),
    Url = gen_mod:get_opt(url, Opts, ?EMPTY_URL),
    ?INFO_MSG("Option: Url = ~p", [Url]),
    ejabberd_hooks:add(roster_in_subscription, Host, ?MODULE, in_subscription, 50),
    ok.

stop(Host) ->
    ?INFO_MSG("Bye bye, ejabberd world tests!", []),
    ejabberd_hooks:delete(roster_in_subscription, Host, ?MODULE, in_subscription, 50),
    ok.

in_subscription(_, User, Server, JID, Type, Reason) ->
    process_subscription(in, User, Server, JID, Type,
        Reason).

process_subscription(_, User, _, JID1,
    subscribe, _) ->

    SUser = binary_to_list(User),
    SJID = format_jid(JID1),

    ?INFO_MSG("--------------------------------------------", []),
    ?INFO_MSG("--- Subscription Initiated -----------------", []),
    ?INFO_MSG("~p is trying to subscribe to ~p", [SUser, SJID]),
    ?INFO_MSG("--------------------------------------------", []);

process_subscription(_, User, _Server, JID1,
    subscribed, _) ->

    SUser = binary_to_list(User),
    SJID = format_jid(JID1),

    ?INFO_MSG("--------------------------------------------", []),
    ?INFO_MSG("--- Subscription Accepted -----------------", []),
    ?INFO_MSG("~p is subscribed to ~p", [SUser, SJID]),
    ?INFO_MSG("--------------------------------------------", []);

process_subscription(_Direction, _User, _Server, _JID1,
    _, _Reason) ->

    ?INFO_MSG("--------------------------------------------", []),
    ?INFO_MSG("Processing unsubscribe(d)", []),
    ?INFO_MSG("--------------------------------------------", []).


format_jid(JID) ->
    binary_to_list(jid:to_string(JID)).

reload(_Host, _NewOpts, _OldOpts) ->
    ok.

depends(_B, _O) -> [].

mod_opt_type(url) -> fun iolist_to_binary/1;
mod_opt_type(_) -> [url].