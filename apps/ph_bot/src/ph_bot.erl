%%%-------------------------------------------------------------------
%% @doc ph_bot public API
%% @end
%%%-------------------------------------------------------------------

-module(ph_bot).

-behaviour(application).

-define(SERVER, ?MODULE).
-record(state, {new_url_counter, visited_url_counter}).

%% Application callbacks
-export([start_link/0, start/2, stop/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([worker_new_url/1,
         worker_visited_url/1,
         show_urls/0
         ]).

%%====================================================================
%% API common
%%====================================================================

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

start(_StartType, _StartArgs) ->
    ph_bot_sup:start_link().

init([]) ->
    new_url = ets:new(new_url, [set, protected, named_table]),
    visited_url = ets:new(visited_url, [set, protected, named_table]),
    State = #state{
        new_url_counter = 0,
        visited_url_counter = 0
    },
    {ok, State}.

%%--------------------------------------------------------------------
stop(_State) ->
    ok.

%%====================================================================
%% API user
%%====================================================================

worker_new_url(Url) when is_binary(Url) ->
    gen_server:cast(?MODULE, {add_new_url, Url});
worker_new_url(Url) ->
     BinUrl = binary:list_to_bin(Url),
     gen_server:cast(?MODULE, {add_new_url, BinUrl}).


worker_visited_url(Url) when is_binary(Url) ->
    gen_server:cast(?MODULE, {add_visited_url, Url});
worker_visited_url(Url) ->
     BinUrl = binary:list_to_bin(Url),
     gen_server:cast(?MODULE, {add_visited_url, BinUrl}).


show_urls() ->
    gen_server:call(?MODULE, {show_urls}).

%%====================================================================
%% Internal functions
%%====================================================================

handle_call({show_urls}, _From, State) ->
    Reply = #{
        new_url     => ets:match(new_url, {'$0'}),
        visited_url => ets:match(visited_url, {'$0'}),
        state       => [ {new, State#state.new_url_counter},
                         {visited, State#state.visited_url_counter},
                         {total, State#state.new_url_counter + State#state.visited_url_counter}
                        ]
    },
    {reply, Reply, State};
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast({add_new_url, Url}, State) ->
    case ets:member(visited_url, Url) of
        true  ->
            % io:format("~n", "new url exists!"),
            NewState = State;
        false -> 
            ets:insert(new_url, {Url}),
            ph_bot_link:request_page(Url),
            NewState = State#state{
                new_url_counter = State#state.new_url_counter + 1
            }
    end,
    {noreply, NewState};
handle_cast({add_visited_url, Url}, State) ->
    case ets:member(new_url, Url) of
        true  ->
            ets:delete(new_url, Url),
            ets:insert(visited_url, {Url}),
            NewState = State#state{
                visited_url_counter = State#state.visited_url_counter + 1
            };
        false -> 
            % io:format("~n", "not found this url in url_new!"),
            NewState = State
    end,
    {noreply, NewState};
handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.