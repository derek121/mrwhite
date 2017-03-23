all: create compile

compile:
	@rebar3 compile

create: create_lexer_to_ws create_parser_to_ws create_lexer_from_ws create_parser_from_ws

create_lexer_to_ws:
	erl -s leex file priv/from_text_lexer -s erlang halt
	@echo
	@mv priv/from_text_lexer.erl src/
	
create_parser_to_ws:
	erl -s yecc file priv/from_text_parser -s erlang halt
	@echo
	@mv priv/from_text_parser.erl src/
	
#

create_lexer_from_ws:
	erl -s leex file priv/from_whitespace_lexer -s erlang halt
	@echo
	@mv priv/from_whitespace_lexer.erl src/
	
create_parser_from_ws:
	erl -s yecc file priv/from_whitespace_parser -s erlang halt
	@echo
	@mv priv/from_whitespace_parser.erl src/
	

