-module(mrwhite_from_whitespace_convert).

%% API
-export([convert/1]).

-include("mrwhite.hrl").


convert(Parsed) ->
  convert(Parsed, []).

convert([], Acc) ->
  lists:flatten(lists:reverse(Acc));
convert([{stack_push, N} | Rest], Acc) ->
  convert(Rest, ["stack push " ++ erlang:integer_to_list(N) ++ "\n" | Acc]);
convert([stack_duplicate | Rest], Acc) ->
  convert(Rest, ["stack duplicate\n" | Acc]);
convert([{stack_copy, N} | Rest], Acc) ->
  convert(Rest, ["stack copy " ++ erlang:integer_to_list(N) ++ "\n" | Acc]);
convert([stack_swap | Rest], Acc) ->
  convert(Rest, ["stack swap\n" | Acc]);
convert([stack_discard | Rest], Acc) ->
  convert(Rest, ["stack discard\n" | Acc]);
convert([{stack_slide, N} | Rest], Acc) ->
  convert(Rest, ["stack slide " ++ erlang:integer_to_list(N) ++ "\n" | Acc]);

convert([arith_add | Rest], Acc) ->
  convert(Rest, ["arith add\n" | Acc]);
convert([arith_sub | Rest], Acc) ->
  convert(Rest, ["arith sub\n" | Acc]);
convert([arith_mul | Rest], Acc) ->
  convert(Rest, ["arith mul\n" | Acc]);
convert([arith_div| Rest], Acc) ->
  convert(Rest, ["arith div\n" | Acc]);
convert([arith_mod | Rest], Acc) ->
  convert(Rest, ["arith mod\n" | Acc]);

convert([heap_store | Rest], Acc) ->
  convert(Rest, ["heap store\n" | Acc]);
convert([heap_retrieve | Rest], Acc) ->
  convert(Rest, ["heap retrieve\n" | Acc]);

convert([{flow_mark, L} | Rest], Acc) ->
  convert(Rest, ["flow mark " ++ L ++ "\n" | Acc]);
convert([{flow_call, L} | Rest], Acc) ->
  convert(Rest, ["flow call " ++ L ++ "\n" | Acc]);
convert([{flow_jump, L} | Rest], Acc) ->
  convert(Rest, ["flow jump " ++ L ++ "\n" | Acc]);
convert([{flow_jump_zero, L} | Rest], Acc) ->
  convert(Rest, ["flow jump_zero " ++ L ++ "\n" | Acc]);
convert([{flow_jump_negative, L} | Rest], Acc) ->
  convert(Rest, ["flow jump_negative " ++ L ++ "\n" | Acc]);
convert([flow_end_sub | Rest], Acc) ->
  convert(Rest, ["flow end_sub\n" | Acc]);
convert([flow_end_program | Rest], Acc) ->
  convert(Rest, ["flow end_program\n" | Acc]);

convert([io_output_char | Rest], Acc) ->
  convert(Rest, ["io output_char\n" | Acc]);
convert([io_output_num | Rest], Acc) ->
  convert(Rest, ["io output_num\n" | Acc]);
convert([io_read_char | Rest], Acc) ->
  convert(Rest, ["io read_char\n" | Acc]);
convert([io_read_num | Rest], Acc) ->
  convert(Rest, ["io read_num\n" | Acc]).


