%%%-------------------------------------------------------------------
%% @doc mprom top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(mprom_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

%% Child :: #{id => Id, start => {M, F, A}}
%% Optional keys are restart, shutdown, type, modules.
%% Before OTP 18 tuples must be used to specify a child. e.g.
%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}
%%
%% See: https://github.com/elli-lib/elli
init([]) ->
    Port = application:get_env(mprom, port, 4444),
    ElliOpts = [{callback, prometheus_elli_callback}, {port, Port}],
    ElliSpec = {
        prometheus_metrics,
        {elli, start_link, [ElliOpts]},
        permanent,
        5000,
        worker,
        [elli]},
    {ok, { {one_for_one, 5, 10}, [ElliSpec]} }.


%%====================================================================
%% Internal functions
%%====================================================================
