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

You only need to add the `mprom` proplist to your `sys.config` if you want to change these defaults.

I'l override the defaults here:

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

### fire metrics


_./examples/foo/apps/foo/src/bar_server.erl_
```Erlang
handle_call(_Request, _From, State) ->
    logger:debug("handle_call: ~p", [?SERVER]),
    prometheus_counter:inc(method_counter, [handle_call], 1),
    {reply, ok, State}.
```

### metrics output

```Erlang
> bar_server ! {hello}.
> gen_server:call(bar_server, {sthth}).
```


Here are the resulting metrics that the `foo` app is firing:

```bash
$ curl -s localhost:9999/metrics | grep bar_server

method_counter{method="handle_call",server="bar_server"} 1
method_counter{method="handle_info",server="bar_server"} 1
```


The underlying prometheus library is providing a _ton_ of metrics on the erlang runtime:

```bash
$ curl -s localhost:9999/metrics

# TYPE method_counter counter
# HELP method_counter count the times each method has been invoked
method_counter{method="handle_call",server="bar_server"} 1
method_counter{method="handle_info",server="bar_server"} 1
# TYPE erlang_vm_memory_atom_bytes_total gauge
# HELP erlang_vm_memory_atom_bytes_total The total amount of memory currently allocated for atoms. This memory is part of the memory presented as system memory.
erlang_vm_memory_atom_bytes_total{usage="used"} 935794
erlang_vm_memory_atom_bytes_total{usage="free"} 15671
# TYPE erlang_vm_memory_bytes_total gauge
# HELP erlang_vm_memory_bytes_total The total amount of memory currently allocated. This is the same as the sum of the memory size for processes and system.

```

### END
