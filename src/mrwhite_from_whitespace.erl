-module(mrwhite_from_whitespace).

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
  Output= mrwhite_from_whitespace_convert:convert(Parsed),

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

lex(S) when is_list(S) ->
  % E.g., [{stack_push,1},{ws_number,2," \t\t \n"},{stack_swap,3}]
  {ok, L, _} = from_whitespace_lexer:string(S),
  %io:format(user, "Lexed: ~p~n", [L]),
  L.

lex_parse(S) ->
  Lexed = lex(S),

  % NO E.g., [{stack_push," \t\t \n"},stack_swap]
  % E.g., [{stack_push,6},stack_swap]
  {ok, P} = from_whitespace_parser:parse(Lexed),
  %io:format(user, "Parsed: ~p~n", [P]),
  P.


-ifdef(TEST).

lex_parse_test() ->
  S = ?STACK_PUSH ++ mrwhite_from_text_convert:num_to_ws(6) ++
      ?STACK_DUPLICATE ++
      ?STACK_COPY ++ mrwhite_from_text_convert:num_to_ws(7) ++
      ?STACK_SWAP ++
      ?STACK_DISCARD ++
      ?STACK_SLIDE ++ mrwhite_from_text_convert:num_to_ws(5) ++
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

  Expected = [{stack_push, 6}, stack_duplicate, {stack_copy, 7}, stack_swap, stack_discard, {stack_slide, 5},
              arith_add, arith_sub, arith_mul, arith_div, arith_mod,
              heap_store, heap_retrieve,
              {flow_mark, "sstt"}, {flow_call, "ttss"}, {flow_jump, "tss"},
              {flow_jump_zero, "ts"}, {flow_jump_negative, "ss"}, flow_end_sub, flow_end_program,
              io_output_char, io_output_num, io_read_char, io_read_num],
  Out = lex_parse(S),
  ?assertEqual(Expected, Out).

convert_test() ->
  S = ?STACK_PUSH ++ mrwhite_from_text_convert:num_to_ws(6) ++
      ?STACK_DUPLICATE ++
      ?STACK_COPY ++ mrwhite_from_text_convert:num_to_ws(7) ++
      ?STACK_SWAP ++
      ?STACK_DISCARD ++
      ?STACK_SLIDE ++ mrwhite_from_text_convert:num_to_ws(5) ++
      ?ARITH_ADD ++
      ?ARITH_SUB ++
      ?ARITH_MUL ++
      ?ARITH_DIV ++
      ?ARITH_MOD ++
      ?HEAP_STORE ++
      ?HEAP_RETRIEVE ++
      ?FLOW_MARK ++ mrwhite_from_text_convert:label_to_ws("tsst") ++
      ?FLOW_CALL ++ mrwhite_from_text_convert:label_to_ws("sst") ++
      ?FLOW_JUMP ++ mrwhite_from_text_convert:label_to_ws("ss") ++
      ?FLOW_JUMP_ZERO ++ mrwhite_from_text_convert:label_to_ws("t") ++
      ?FLOW_JUMP_NEGATIVE ++ mrwhite_from_text_convert:label_to_ws("sst") ++
      ?FLOW_END_SUB ++
      ?FLOW_END_PROGRAM ++
      ?IO_OUTPUT_CHAR ++
      ?IO_OUTPUT_NUM ++
      ?IO_READ_CHAR ++
      ?IO_READ_NUM,
  Expected = "stack push 6\nstack duplicate\nstack copy 7\nstack swap\nstack discard\nstack slide 5\n"
             "arith add\narith sub\narith mul\narith div\narith mod\nheap store\nheap retrieve\n"
             "flow mark tsst\nflow call sst\nflow jump ss\nflow jump_zero t\nflow jump_negative sst\n"
             "flow end_sub\nflow end_program\n"
             "io output_char\nio output_num\nio read_char\nio read_num\n",
  Out = convert(S, return),
  ?assertEqual(Expected, Out).

%num_to_ws(N) ->
%  case N >= 0 of
%    true  -> [?S] ++ parse_number_abs(N);
%    false -> [?T] ++ parse_number_abs(erlang:abs(N))
%  end.

%parse_number_abs(N) ->
%  Base2 = erlang:integer_to_list(N, 2),
%  A = re:replace(Base2, "0", [?S], [global, {return, list}]),
%  B = re:replace(A    , "1", [?T], [global, {return, list}]),
%  B ++ [?L].

-endif.


