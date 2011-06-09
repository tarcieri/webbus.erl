all: compile

deps:
	@./rebar get-deps

compile: deps
	@cd deps/socketio && make # rebar alone horks for some reason
	@./rebar compile

test: force
	@./rebar eunit skip_deps=true

force: 
	@true
	
clean:
	rm -f ebin/webbus.app
	rm -f ebin/*.beam
	rm -rf deps
	rm -f .agner.config