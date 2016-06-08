export PATH:=/opt/entensys/otp/bin:$(PATH)

all:
	./rebar3 as dev compile
dev:
	./rebar3 as dev release
rel:
	./rebar3 as rel release
tar:
	./rebar3 as rel tar
clean:
	./rebar3 clean
	rm -rf ./_build
run: dev
	cd ./_build/dev/rel/planetwars/bin && ./planetwars console
update:
	./rebar3 update
