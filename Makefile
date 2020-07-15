.PHONY: ps erl all test

all: ps erl

ps:
	spago -x test.dhall build
test: ps erl
	erl -pa ebin -noshell -eval '(test_main@ps:main())()' -eval 'init:stop()'

erl:
	mkdir -p ebin
	erlc -o ebin/ output/*/*.erl
