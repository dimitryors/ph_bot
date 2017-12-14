%%%-------------------------------------------------------------------
%% @doc ph_bot public API
%% @end
%%%-------------------------------------------------------------------

-module(ph_bot_link).

-behaviour(application).

-define(SERVER, ?MODULE).
-record(state, {name}).

%% Application callbacks
-export([start_link/0, start/2, stop/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([request_page/1,
         request_url/1,
         parse_url/1,
         url_context/1,
         url_domain/1,
         url_depth/1,
         parse_page/1,
         get_tags_attrs/1,
         is_external_link/2,
         trim_subdomains/1,
         fetch_links/2,
         remove_duplicates/1,
         save_crawler_results/2,
         is_robotstxt_exists/1
         ]).

%%====================================================================
%% API
%%====================================================================

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

start(_StartType, _StartArgs) ->
    ph_bot_link_sup:start_link().

init([]) ->
    {ok, #state{}}.

%%--------------------------------------------------------------------
stop(_State) ->
    ok.

%%====================================================================
%% Local API user
%%====================================================================

request_page(Url) ->
    gen_server:cast(?MODULE, {request_page, Url}).

%%====================================================================
%% External API user
%%====================================================================

save_crawler_results(Url, Links) ->
    ph_bot:worker_visited_url(Url),
    lists:map(fun(Link) -> ph_bot:worker_new_url(Link) end, Links).

%%====================================================================
%% Internal functions
%%====================================================================

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast({request_page, Url}, State) ->
    case request_url({Url}) of
        {ok, Page} ->
            Tokens = parse_page({Page}),
            Links  = fetch_links(Tokens, Url),
            save_crawler_results(Url, Links);
        {warning, OtherCode, Url} -> ok;
        {error, Reason, Url}      -> ok;
        _ -> ok
    end,
    {noreply, State};
handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%====================================================================
%% Requesting Block
%%====================================================================

request_url({ Url }) ->
    {Domain,_Root,Folder,File,_Query} = parse_url({Url}),
    NewUrl = Domain ++ Folder ++ File,
    {ok, HttpHeader} = application:get_env(?MODULE, http_header),
    {ok, HttpOptions} = application:get_env(?MODULE, http_options),
    {ok, RequestOptions} = application:get_env(?MODULE, request_options),
    Result = httpc:request(get, {NewUrl, HttpHeader}, HttpOptions, RequestOptions),
    request_url({ Result, Url});
request_url({ { ok, {{_Version, 200, _ReasonPhrase}, _Headers, HtmlPage} }, _Url} ) -> 
    {ok, HtmlPage};
request_url({ { ok, {{_, OtherCode, _}, _, _} }, Url} ) -> 
    {warning, OtherCode, Url};
request_url({ { error, Reason }, Url}) ->
    {error, Reason, Url}.
% {error,socket_closed_remotely}

%%====================================================================
%% Url Parsing Block
%%====================================================================

parse_url({Url}) when is_binary(Url) ->
    UrlStr = binary_to_list(Url),
    Result = http_uri:parse(UrlStr),
    parse_url({Result, UrlStr});
parse_url({Url}) ->
    Result = http_uri:parse(Url),
    parse_url({Result, Url});
parse_url({ {ok, {Protocol,_,Root,Port,Path,Query}}, _Url}) ->
    case Port of
        80 ->
            P = "";
        443 ->
            P = "";
        _ ->
            P = ":" ++ integer_to_list(Port)
    end,
    Domain = atom_to_list(Protocol) ++ "://" ++ Root ++ P,
    {Folder, File} = parse_path(Path),
    TrimSubdomains = trim_subdomains(Root),
    {Domain, TrimSubdomains, Folder, File, Query};
parse_url({ {error, no_scheme}, Url}) ->
    NewUrl = "http://" ++ Url,
    parse_url({NewUrl});
parse_url({ {error, Reason}, Url}) ->
    {error, Reason, Url}.

parse_path(Path) ->
    Separator = string:rstr(Path,"/"),
    {string:sub_string(Path,1, Separator), string:sub_string(Path, Separator + 1)}.

url_context(Url) ->
    {Domain,_Root,Folder,_File,_Query} = parse_url({Url}),
    Domain ++ Folder.

url_depth(Url) ->
    {_Domain,_Root,Folder,_File,_Query} = parse_url({Url}),
    length(string:tokens(Folder, "/")).

url_domain(Url) ->
    {Domain,_,_,_,_} = parse_url({Url}),
    Domain.

remove_duplicates(List) ->
    lists:reverse(lists:foldl(
        fun(Element, Acc) ->
            case lists:member(Element, Acc) of
                true ->
                    Acc;
                false ->
                    [Element] ++ Acc
            end
        end, [], List
    )).

trim_subdomains(Url) when Url =/= [] ->
    List    = re:split(Url, "[.]", [{return,list}]),
    Length  = length(List),
    lists:nth(Length-1, List) ++ "." ++ lists:nth(Length, List);
trim_subdomains(Url) -> [].

is_external_link(Root1, Root2) when Root1 =:= Root2 -> false;
is_external_link(Root1, Root2) -> true.

%%====================================================================
%% Page Parsing Block
%%====================================================================

parse_page({HtmlPage}) ->
    Tokens = mochiweb_html:tokens(HtmlPage).

fetch_links(Tokens, Url) ->
    TagName = <<"a">>, 
    TagAttr = <<"href">>,
    Links = get_tags_attrs({Tokens, TagName, TagAttr, Url}),
    remove_duplicates(Links).


get_tags_attrs({Tokens, TagName}) ->
    lists:foldl(
      fun(Idx, Acc) -> 
	      Token = lists:nth(Idx, Tokens),
	      case Token of 
            {start_tag, TagName, TagAttrs, _} ->
                [ {Idx, TagName, TagAttrs} | Acc ];
            _Other -> Acc
	      end
      end,
      [], 
      lists:seq(1, length(Tokens))
     );
get_tags_attrs({Tokens, TagName, TagAttr, Url}) ->
    {MainUrlDomain,MainUrlRoot,_Folder,_File,_Query} = parse_url({Url}),
    lists:foldl(
      fun(Token, Acc) -> 
	      case Token of 
            {start_tag, TagName, TagAttrs, _} ->
                % Extract TagAttrs
                NewTagAttrs = [ parse_url({Value}) 
                                    || {Tag, Value} <- TagAttrs,
                                    Tag =:= TagAttr 
                                ],
                % Extract Links from TagAttrs
                case NewTagAttrs of 
                    [{_Domain,Root,Folder,_File,_Query}] when Root =:= [] ->
                        [ MainUrlDomain ++ Folder | Acc ];
                    [{Domain,Root,Folder,_File,_Query}] when Root =:= MainUrlRoot ->
                        [ Domain ++ Folder | Acc ];
                    _Other -> Acc
                end;
            _Other -> Acc
	      end
      end,
      [], 
      Tokens
     ).


%%====================================================================
%% Check robots.txt
%%====================================================================

is_robotstxt_exists(Url) ->
    {Domain,_Root,_Folder,_File,_Query} = parse_url({Url}),
    RobotsUrl = Domain ++ "/robots.txt",
    SitemapUrl = Domain ++ "/sitemap.xml",
    lists:foldl(
        fun(Item, Acc) ->
            case Item of
                {robots, NewUrl} ->
                    case request_url({NewUrl}) of
                        {ok, Page}             -> [{robots,  true} | Acc];
                        {warning, 404, _Url}   -> [{robots, false} | Acc];
                        {error, timeout, _Url} -> [{robots, false} | Acc];
                        Error                  -> Error
                    end;
                {sitemap, NewUrl} ->
                    case request_url({NewUrl}) of
                        {ok, Page}             -> [{sitemap,   true} | Acc];
                        {warning, 404, _Url}   -> [{sitemap,  false} | Acc];
                        {error, timeout, _Url} -> [{sitemap,  false} | Acc];
                        Error                  -> Error
                    end
            end
        end,
        [],
        [{robots, RobotsUrl}, {sitemap, SitemapUrl}]
    ).
    

% get_start_tags_data(Tokens, TagName) ->
%     Indexes = get_start_tags_indexes(Tokens, TagName),
%     DeepList = lists:foldl(
% 		 fun(Index, Results) ->
% 			 case lists:nth(Index + 1, Tokens) of
% 			     {data, Data, _Whitespace} ->
% 				 [Data|Results];
% 			     _Else ->
% 				 error_logger:warning_report({?MODULE, ?LINE, {get_start_tags_data, data_token_notFound}}),
% 				 Results
% 			 end
% 		 end,
% 		 [],
% 		 Indexes),
%     lists:flatten(DeepList).