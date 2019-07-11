# mprom

_m is for `micro`_

A _tiny_ rebar3 erlang otp application that serves up prometheus metrics.

## build

	$ make build


## install

Add to an existing application (the common and only use case):

_rebar.config_
```Erlang
{deps,  
	[
		{mprom, {git, "git://github.com/toddg/mprom.git", {tag, "1.0.0"}}}
	]
}.

```

## configuration

* `port` : what port to serve `/metrics` on
* `registry` : namespace the metrics into a separate registry, though `default` is probably fine

These are the defaults:

_sys.config_
```Erlang
  {mprom, [
  		{registry, default},
		{port, 4444}
	]}
```

You only need to add the `mprom` proplist to your `sys.config` if you want to change these defaults.

I'll override the defaults here:

_./examples/foo/config/sys.config_
```Erlang
  {mprom, [
  		{registry, myregistry},
		{port, 9999}
	]},
```


## usage

See `./examples/foo` for how to configure and use this tool. As shown above, update your `rebar.config` and `sys.config` as nec.

### register metrics

_./examples/foo/apps/foo/src/bar_server.erl_
```Erlang
register_metrics() ->
    logger:debug("register_metrics: ~p", [?SERVER]),
    Registry = application:get_env(mprod, registry, default),
    prometheus_counter:new([{registry, Registry}, {name, method_counter},     {help, "count the times each method has been invoked"},     {labels, [method]}]),
    ok.
```

### instrument code


_./examples/foo/apps/foo/src/bar_server.erl_
```Erlang
handle_call(_Request, _From, State) ->
    logger:debug("handle_call: ~p", [?SERVER]),
    prometheus_counter:inc(method_counter, [handle_call], 1),
    {reply, ok, State}.
```

### fire metrics

```Erlang
$ rebar3 shell foo

===> Verifying dependencies...
===> Compiling foo
Erlang/OTP 22 [erts-10.4.2] [source] [64-bit] [smp:8:8] [ds:8:8:10] [async-threads:1] [hipe]

Eshell V10.4.2  (abort with ^G)
1> ===> The rebar3 shell is a development tool; to deploy applications in production, consider using releases (http://www.rebar3.org/docs/releases)
2019-07-11T11:06:50.174372-07:00 info: application: elli, started_at: nonode@nohost
2019-07-11T11:06:50.804174-07:00 info: application: prometheus, started_at: nonode@nohost
2019-07-11T11:06:50.805435-07:00 info: supervisor: {local,mprom_sup}, started: [{pid,<0.224.0>},{id,prometheus_metrics},{mfargs,{elli,start_link,[[{callback,prometheus_elli_callback},{port,9999}]]}},{restart_type,permanent},{shutdown,5000},{child_type,worker}]
2019-07-11T11:06:50.805748-07:00 info: application: mprom, started_at: nonode@nohost
2019-07-11T11:06:50.806117-07:00 debug: start_link: bar_server
2019-07-11T11:06:50.806277-07:00 debug: init: bar_server
2019-07-11T11:06:50.806362-07:00 debug: register_metrics: bar_server
2019-07-11T11:06:50.806494-07:00 info: supervisor: {local,foo_sup}, started: [{pid,<0.249.0>},{id,tag1},{mfargs,{bar_server,start_link,[]}},{restart_type,permanent},{shutdown,10000},{child_type,worker}]
2019-07-11T11:06:50.806804-07:00 info: application: foo, started_at: nonode@nohost
2019-07-11T11:06:50.807209-07:00 info: supervisor: {local,sasl_safe_sup}, started: [{pid,<0.255.0>},{id,alarm_handler},{mfargs,{alarm_handler,start_link,[]}},{restart_type,permanent},{shutdown,2000},{child_type,worker}]
2019-07-11T11:06:50.807542-07:00 info: supervisor: {local,sasl_sup}, started: [{pid,<0.254.0>},{id,sasl_safe_sup},{mfargs,{supervisor,start_link,[{local,sasl_safe_sup},sasl,safe]}},{restart_type,permanent},{shutdown,infinity},{child_type,supervisor}]
2019-07-11T11:06:50.808910-07:00 info: supervisor: {local,sasl_sup}, started: [{pid,<0.256.0>},{id,release_handler},{mfargs,{release_handler,start_link,[]}},{restart_type,permanent},{shutdown,2000},{child_type,worker}]
2019-07-11T11:06:50.809490-07:00 info: application: sasl, started_at: nonode@nohost
===> Booted elli
===> Booted prometheus
===> Booted mprom
===> Booted foo
===> Booted sasl
2019-07-11T11:06:55.281223-07:00 info: supervisor: {local,kernel_safe_sup}, started: [{pid,<0.260.0>},{id,dets_sup},{mfargs,{dets_sup,start_link,[]}},{restart_type,permanent},{shutdown,1000},{child_type,supervisor}]
2019-07-11T11:06:55.281874-07:00 info: supervisor: {local,kernel_safe_sup}, started: [{pid,<0.261.0>},{id,dets},{mfargs,{dets_server,start_link,[]}},{restart_type,permanent},{shutdown,2000},{child_type,worker}] 

1> bar_server ! hello.
2019-07-11T11:07:05.784224-07:00 debug: handle_info: bar_server
hello
2> gen_server:call(bar_server, {sthth}).
2019-07-11T11:09:29.650706-07:00 debug: handle_call: bar_server
ok
```

### metrics output

Here are the resulting metrics that the `foo` app is firing:

```bash
$ curl -s localhost:9999/metrics | grep bar_server

method_counter{method="handle_call",server="bar_server"} 1
method_counter{method="handle_info",server="bar_server"} 1
```

The underlying prometheus library is providing a _ton_ of metrics on the erlang runtime:

```bash
~/.cache/rebar3 $ curl -s localhost:9999/metrics  | egrep -v ^# | head

method_counter{method="handle_call",server="bar_server"} 1
method_counter{method="handle_info",server="bar_server"} 1
erlang_vm_memory_atom_bytes_total{usage="used"} 717956
erlang_vm_memory_atom_bytes_total{usage="free"} 26413
erlang_vm_memory_bytes_total{kind="system"} 27364944
erlang_vm_memory_bytes_total{kind="processes"} 6726328
erlang_vm_memory_dets_tables 0
erlang_vm_memory_ets_tables 48
erlang_vm_memory_processes_bytes_total{usage="used"} 6726208
erlang_vm_memory_processes_bytes_total{usage="free"} 120
erlang_vm_memory_system_bytes_total{usage="atom"} 744369
erlang_vm_memory_system_bytes_total{usage="binary"} 111648
...
```

That's 965 lines of metrics to start with, so there's a lot to play with when building out a 
dashboard.
```bash
 $ curl -s localhost:9999/metrics  | egrep -v ^#  | wc
    965    1928   85891
```

## Links

See my other repos and posts for more details on how to use these metrics and integrate them
with your container infrastructure.

* http://zwrob.com/posts/
* https://github.com/toddg/monitor

### END
