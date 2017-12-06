%%%-------------------------------------------------------------------
%% @doc ph_bot public API
%% @end
%%%-------------------------------------------------------------------

-module(ph_bot_html).

-behaviour(application).

-define(SERVER, ?MODULE).
-record(state, {name}).

%% Application callbacks
-export([start_link/0, start/2, stop/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([request_new_url/1,
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
         test/0,
         time_calc/0
        %  get_start_tags_attributes/3,
        %  get_start_tags_attributes/2
         ]).

%%====================================================================
%% API
%%====================================================================

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

start(_StartType, _StartArgs) ->
    ph_bot_html_sup:start_link().

init([]) ->
    {ok, Options} = application:get_env(?MODULE, web_request_options),
    httpc:set_options(Options),
    {ok, #state{}}.

%%--------------------------------------------------------------------
stop(_State) ->
    ok.

%%====================================================================
%% API user
%%====================================================================

request_new_url(Url) ->
    gen_server:cast(?MODULE, {fetch_new_url, Url}).

%%====================================================================
%% Internal functions
%%====================================================================

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

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
    Result = httpc:request( Domain ++ Folder ++ File ),
    request_url(Result);
request_url( { ok, {{_, 200, _}, _, HtmlPage} } ) -> 
    {ok, HtmlPage};
request_url( { error, Reason } ) ->
    {error, Reason}.

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

trim_subdomains(Url) when Url =:= [] -> [];
trim_subdomains(Url) ->
    List    = re:split(Url, "[.]", [{return,list}]),
    Length  = length(List),
    lists:nth(Length-1, List) ++ "." ++ lists:nth(Length, List).

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
    get_tags_attrs({Tokens, TagName, TagAttr, Url}).


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
                % Extract Link from TagAttrs
                case NewTagAttrs of 
                    [{_Domain,Root,Folder,_File,_Query}] when Root =:= [] ->
                        [ MainUrlDomain ++ Folder | Acc ];
                    _Other -> Acc
                end;
            _Other -> Acc
	      end
      end,
      [], 
      Tokens
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

test() ->
    Url = <<"ogo1.ru">>,
    {ok, Page} = ph_bot_html:request_url({Url}),
    Tokens = ph_bot_html:parse_page({Page}),
    fetch_links(Tokens, Url).
    % fetch_links(Tokens, Url).
    % HrefAttrTags = ph_bot_html:get_tags_attrs({Tokens, <<"a">>, <<"href">>}),
    % lists:flatten([ ph_bot_html:parse_url({TagAttrs}) || {Idx, TagName, [TagAttrs]} <- HrefAttrTags ]).

time_calc() -> 
    Url = <<"ogo1.ru">>,
    {PageM, {ok, Page}} = timer:tc(?MODULE, request_url, [{Url}]),
    SizePage = length(Page),
    {TokensM, Tokens} = timer:tc(?MODULE, parse_page, [{Page}]),
    SizeTokens = length(Tokens),
    {FetchM, Links} = timer:tc(?MODULE, fetch_links, [Tokens, Url]),
    SizeLinks = length(Links),
    % {FetchM, FetchUrls} = timer:tc(?MODULE, fetch_links, [Tokens, Url]),
    % SizeFetchUrls = length(FetchUrls),
    #{
        request_url => PageM/SizePage,
        parse_page  => TokensM/SizeTokens,
        fetch_links => FetchM/SizeLinks,
        total       => PageM/SizePage + TokensM/SizeTokens + FetchM/SizeLinks
    }.
