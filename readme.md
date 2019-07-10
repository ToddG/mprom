# mprom

A rebar3 erlang otp application that serves up prometheus metrics.


## build

	$ make build


## install

Add to an existing application (the common and only use case):

```Erlang

{deps,  
	[
		{mprom, {git, "git://github.com/toddg/micro-prom.git", {tag, "0.0.1"}}}
	]}.
```

## configuration

* `prefix` : is used to prefix emitted metric names and functions as a namespace
* `port` : what port to serve `/metrics` on

_sys.config_
```Erlang
  {mprom, [
  		{prefix, "foo"},
		{port, 4444},
	]}
```

## register metrics



