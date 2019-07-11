APP:=foo
ARGS=""

.PHONY: help
help:
	# -----------------------------------------------------------------------------
	# Targets:
	#
	# ** Local Commands **
	# 	build:          build service locally
	# 	start :		start service locally
	# 	stop :		stop service locally
	# 	package : 	create tar of service locally
	# 	cplane : 	control plane escript
	#
	# ** Container Commands **
	# 	cbuild:         build containerized service via docker compose
	# 	cstart:		start as containerized service via docker compose
	# 	cstop:		stop as containerized service via docker compose
	# 	clog:		tail the containerized service stack via docker compose
	#
	# ** Misc **
	# 	selftest:	run through {build|start|stop} {locally|container}
	#
	# end.
	# -----------------------------------------------------------------------------

# -----------------------------------------------------------------------------
#  LOCAL COMMANDS
# -----------------------------------------------------------------------------
.PHONY: build
build:  
	rebar3 do clean, compile, escriptize, eunit

.PHONY: release
release:  
	rebar3 release 

.PHONY: server
server: 
	./_build/default/rel/${APP}/bin/${APP} ${ARGS}

.PHONY: start
start: release 
	./_build/default/rel/${APP}/bin/${APP} start

.PHONY: stop
stop: 
	-./_build/default/rel/${APP}/bin/${APP} stop

.PHONY: package
package: 
	rebar3 as prod tar

.PHONY: cplane
cplane: build 
	_build/default/bin/cplane ${CPARGS}

# -----------------------------------------------------------------------------
#  CONTAINER COMMANDS
# -----------------------------------------------------------------------------
.PHONY: cstart
cstart: 
	cd monitor && $(MAKE) start

.PHONY: cstop
cstop: 
	cd monitor && $(MAKE) stop

.PHONY: cbuild
cbuild:  
	docker build -t toddg/${APP} .

.PHONY: clog
clog: 
	cd monitor && docker-compose logs -f --tail="all"

# -----------------------------------------------------------------------------
#  SELFTEST
# -----------------------------------------------------------------------------


.PHONY: selftest
selftest: build release start stop cbuild cstart cstop
