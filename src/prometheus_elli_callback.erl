-module(prometheus_elli_callback).
-export([handle/2, handle_event/3]).

-include_lib("elli/include/elli.hrl").
-behaviour(elli_handler).

handle(Req, _Args) ->
    %% Delegate to our handler function
    handle(Req#req.method, elli_request:path(Req), Req).

handle('GET', [<<"metrics">>], _Req) ->
    %% Reply with a normal response. `ok' can be used instead of `200'
    %% to signal success.
    {ok, [], prometheus_text_format:format()};
handle(_, _, _) ->
    {404, [], <<"Not Found. Try '/metrics'">>}.

%% @doc Handle request events, like request completed, exception
%% thrown, client timeout, etc. Must return `ok'.
handle_event(_Event, _Data, _Args) ->
    ok.
