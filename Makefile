.PHONY: ps erl all test

all: ps erl

ps:
	spago build -x test.dhall
test: ps erl
	erl -pa ebin -noshell -eval '(test_main@ps:main())()' -eval 'init:stop()'

erl:
	mkdir -p ebin
	erlc -o ebin/ output/*/*.erl
