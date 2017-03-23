-module(mrwhite_from_text_convert).

%% API
-export([convert/1]).

-ifdef(TEST).
-export([num_to_ws/1]).
-export([label_to_ws/1]).
-endif.

-include("mrwhite.hrl").

convert(Parsed) ->
  convert(Parsed, []).

convert([], Acc) ->
  lists:flatten(lists:reverse(Acc));
convert([{stack_push, N} | Rest], Acc) ->
  convert(Rest, [?STACK_PUSH ++ num_to_ws(N) | Acc]);
convert([stack_duplicate | Rest], Acc) ->
  convert(Rest, [?STACK_DUPLICATE | Acc]);
convert([{stack_copy, N} | Rest], Acc) ->
  convert(Rest, [?STACK_COPY ++ num_to_ws(N) | Acc]);
convert([stack_swap | Rest], Acc) ->
  convert(Rest, [?STACK_SWAP | Acc]);
convert([stack_discard | Rest], Acc) ->
  convert(Rest, [?STACK_DISCARD | Acc]);
convert([{stack_slide, N} | Rest], Acc) ->
  convert(Rest, [?STACK_SLIDE ++ num_to_ws(N) | Acc]);

convert([arith_add | Rest], Acc) ->
  convert(Rest, [?ARITH_ADD | Acc]);
convert([arith_sub | Rest], Acc) ->
  convert(Rest, [?ARITH_SUB | Acc]);
convert([arith_mul | Rest], Acc) ->
  convert(Rest, [?ARITH_MUL | Acc]);
convert([arith_div| Rest], Acc) ->
  convert(Rest, [?ARITH_DIV | Acc]);
convert([arith_mod | Rest], Acc) ->
  convert(Rest, [?ARITH_MOD | Acc]);

convert([heap_store | Rest], Acc) ->
  convert(Rest, [?HEAP_STORE | Acc]);
convert([heap_retrieve | Rest], Acc) ->
  convert(Rest, [?HEAP_RETRIEVE | Acc]);

convert([{flow_mark, L} | Rest], Acc) ->
  convert(Rest, [?FLOW_MARK ++ label_to_ws(L) | Acc]);
convert([{flow_call, L} | Rest], Acc) ->
  convert(Rest, [?FLOW_CALL ++ label_to_ws(L) | Acc]);
convert([{flow_jump, L} | Rest], Acc) ->
  convert(Rest, [?FLOW_JUMP ++ label_to_ws(L) | Acc]);
convert([{flow_jump_zero, L} | Rest], Acc) ->
  convert(Rest, [?FLOW_JUMP_ZERO ++ label_to_ws(L) | Acc]);
convert([{flow_jump_negative, L} | Rest], Acc) ->
  convert(Rest, [?FLOW_JUMP_NEGATIVE ++ label_to_ws(L) | Acc]);
convert([flow_end_sub | Rest], Acc) ->
  convert(Rest, [?FLOW_END_SUB | Acc]);
convert([flow_end_program| Rest], Acc) ->
  convert(Rest, [?FLOW_END_PROGRAM | Acc]);

convert([io_output_char | Rest], Acc) ->
  convert(Rest, [?IO_OUTPUT_CHAR | Acc]);
convert([io_output_num | Rest], Acc) ->
  convert(Rest, [?IO_OUTPUT_NUM | Acc]);
convert([io_read_char | Rest], Acc) ->
  convert(Rest, [?IO_READ_CHAR | Acc]);
convert([io_read_num | Rest], Acc) ->
  convert(Rest, [?IO_READ_NUM | Acc]).

num_to_ws(N) ->
  case N >= 0 of
    true  -> [?S] ++ parse_number_abs(N);
    false -> [?T] ++ parse_number_abs(erlang:abs(N))
  end.

parse_number_abs(N) ->
  Base2 = erlang:integer_to_list(N, 2),
  A = re:replace(Base2, "0", [?S], [global, {return, list}]),
  B = re:replace(A    , "1", [?T], [global, {return, list}]),
  B ++ [?L].

label_to_ws(L) ->
  A = re:replace(L, "s", [?S], [global, {return, list}]),
  B = re:replace(A, "t", [?T], [global, {return, list}]),
  B ++ [?L].




