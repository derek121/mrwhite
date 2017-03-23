-module(mrwhite_from_text).

%% API
-export([convert/1]).
-export([convert/2]).
-export([run/1]).

-include("mrwhite.hrl").

-ifdef(TEST).
-export([lex_parse/1]).
-include_lib("eunit/include/eunit.hrl").
-endif.

convert(F = {file_in, _Filename}) ->
  convert(F, return).

convert({file_in, Filename}, Out) ->
  {ok, B} = file:read_file(Filename),
  convert(erlang:binary_to_list(B), Out);
convert(S, Out) ->
  Parsed = lex_parse(S),
  Output = mrwhite_from_text_convert:convert(Parsed),

  case Out of
    return -> Output;
    {file_out, Filename} ->
      ok = file:write_file(Filename, Output)
  end.

run({file_in, Filename}) ->
  {ok, B} = file:read_file(Filename),
  run(erlang:binary_to_list(B));
run(S) ->
  Parsed = lex_parse(S),
  mrwhite_general:run(Parsed).

%%%

lex(S) ->
  % E.g., [{stack,1},{push,1},{number,1,"6"},{stack,2},{swap,2}]
  {ok, L, _} = from_text_lexer:string(S),
  %io:format(user, "Lexed: ~p~n", [L]),
  L.

%%%
lex_parse(S) ->
  Lexed = lex(S),

  % E.g., [{stack_push,6},stack_swap]
  {ok, P} = from_text_parser:parse(Lexed),
  %io:format(user, "Parsed: ~p~n", [P]),
  P.


-ifdef(TEST).

lex_parse_test() ->
  S = "stack push 6\nstack duplicate stack copy 7 stack swap stack discard stack slide 2"
      "arith add\narith sub arith mul arith div arith mod "
      "heap store\nheap retrieve "
      "flow mark sstt flow call ttss flow jump tss flow jump_zero ts "
      "flow jump_negative ss flow end_sub flow end_program "
      "io output_char\nio output_num\nio read_char\nio read_num\n",
  
  Expected = [{stack_push, 6}, stack_duplicate, {stack_copy, 7}, stack_swap, stack_discard, {stack_slide, 2},
              arith_add, arith_sub, arith_mul, arith_div, arith_mod,
              heap_store, heap_retrieve,
              {flow_mark, "sstt"}, {flow_call, "ttss"}, {flow_jump, "tss"}, 
              {flow_jump_zero, "ts"}, {flow_jump_negative, "ss"}, flow_end_sub, flow_end_program,
              io_output_char, io_output_num, io_read_char, io_read_num],
  Out = lex_parse(S),
  ?assertEqual(Expected, Out).

convert_test() ->
  S = "stack push 6\nstack duplicate stack copy 7 stack swap stack discard stack slide 2 "
      "arith add\narith sub arith mul arith div arith mod"
      "heap store\nheap retrieve "
      "flow mark sstt flow call ttss flow jump tss flow jump_zero ts flow jump_negative ss flow end_sub flow end_program "
      "io output_char\nio output_num\nio read_char\nio read_num\n",
  
  Expected = ?STACK_PUSH ++ mrwhite_from_text_convert:num_to_ws(6) ++
             ?STACK_DUPLICATE ++
             ?STACK_COPY ++ mrwhite_from_text_convert:num_to_ws(7) ++
             ?STACK_SWAP ++
             ?STACK_DISCARD ++
             ?STACK_SLIDE ++ mrwhite_from_text_convert:num_to_ws(2) ++
             ?ARITH_ADD ++
             ?ARITH_SUB ++
             ?ARITH_MUL ++
             ?ARITH_DIV ++
             ?ARITH_MOD ++
             ?HEAP_STORE ++
             ?HEAP_RETRIEVE ++
             ?FLOW_MARK ++ mrwhite_from_text_convert:label_to_ws("sstt") ++
             ?FLOW_CALL ++ mrwhite_from_text_convert:label_to_ws("ttss") ++
             ?FLOW_JUMP ++ mrwhite_from_text_convert:label_to_ws("tss") ++
             ?FLOW_JUMP_ZERO ++ mrwhite_from_text_convert:label_to_ws("ts") ++
             ?FLOW_JUMP_NEGATIVE ++ mrwhite_from_text_convert:label_to_ws("ss") ++
             ?FLOW_END_SUB ++
             ?FLOW_END_PROGRAM ++
             ?IO_OUTPUT_CHAR ++
             ?IO_OUTPUT_NUM ++
             ?IO_READ_CHAR ++
             ?IO_READ_NUM,
  Out = convert(S, return),
  ?assertEqual(Expected, Out).

output_loop_test() ->
  S =
  "stack push 0 "
  "stack push 33 "
  "stack push 101 "
  "stack push 111 "
  "stack push 74 "
  "stack push 32 "
  "stack push 111 "
  "stack push 108 "
  "stack push 108 "
  "stack push 101 "
  "stack push 72 "

  "flow mark s "
  "io output_char "
  "stack duplicate"
  "flow jump_zero st "
  "flow jump s "

  "flow mark st "
  "stack push 10 "
  "io output_char "
  "flow end_program ",

  run(S).

%lex_parse_stack_test() ->
    %  S = "stack push 6\nstack duplicate stack swap stack discard",
%  Expected = [{stack_push,6},
%              stack_duplicate,
%              stack_swap,
%              stack_discard],
%
%  Out = lex_parse(S),
%  ?assertEqual(Expected, Out).
%
%%%%
%
%lex_parse_arith_test() ->
%  S = "arith add\narith sub arith mul arith div arith mod",
%  Expected = [arith_add,
%              arith_sub,
%              arith_mul,
%              arith_divide,
%              arith_mod],
%
%  Out = lex_parse(S),
%
%  %io:format(user, "Got: ~p~n", [Out]),
%  ?assertEqual(Expected, Out).
%
%lex_parse_heap_test() ->
%  S = "heap store\nheap retrieve",
%  Expected = [heap_store,
%              heap_retrieve],
%
%  Out = lex_parse(S),
%
%  %io:format(user, "Got: ~p~n", [Out]),
%  ?assertEqual(Expected, Out).



-endif.

