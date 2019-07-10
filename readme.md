# mprom

A rebar3 erlang otp application that serves up prometheus metrics.

_m is for `micro`_

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

* `port` : what port to serve `/metrics` on
* `registry` : namespace the metrics into a separate registry, though `default` is probably fine

_sys.config_
```Erlang
  {mprom, [
  		{registry, default},
		{port, 4444},
	]}
```

## register metrics

