Nonterminals ops elements op. 

Terminals stack_push stack_duplicate stack_copy stack_swap stack_discard stack_slide
          arith_add arith_sub arith_mul arith_div arith_mod 
          heap_store heap_retrieve
          flow_mark flow_call flow_jump flow_jump_zero flow_jump_negative flow_end_sub flow_end_program
          io_output_char io_output_num io_read_char io_read_num
          ws_number ws_label.

Rootsymbol elements.

elements -> ops : '$1'.

ops -> op : ['$1'].
ops -> op ops : ['$1'] ++ '$2'.

op -> stack_push ws_number        : {stack_push, parse_num(unwrap('$2'))}.
op -> stack_duplicate             : stack_duplicate.
op -> stack_copy ws_number        : {stack_copy, parse_num(unwrap('$2'))}.
op -> stack_swap                  : stack_swap.
op -> stack_discard               : stack_discard.
op -> stack_slide ws_number       : {stack_slide, parse_num(unwrap('$2'))}.

op -> arith_add                   : arith_add.
op -> arith_sub                   : arith_sub.
op -> arith_mul                   : arith_mul.
op -> arith_div                   : arith_div.
op -> arith_mod                   : arith_mod.

op -> heap_store                  : heap_store.
op -> heap_retrieve               : heap_retrieve.

op -> flow_mark ws_label          : {flow_mark, parse_label(unwrap('$2'), [])}.
op -> flow_call ws_label          : {flow_call, parse_label(unwrap('$2'), [])}.
op -> flow_jump ws_label          : {flow_jump, parse_label(unwrap('$2'), [])}.
op -> flow_jump_zero ws_label     : {flow_jump_zero, parse_label(unwrap('$2'), [])}.
op -> flow_jump_negative ws_label : {flow_jump_negative, parse_label(unwrap('$2'), [])}.
op -> flow_end_sub                : flow_end_sub.
op -> flow_end_program            : flow_end_program.

op -> io_output_char              : io_output_char.
op -> io_output_num               : io_output_num.
op -> io_read_char                : io_read_char.
op -> io_read_num                 : io_read_num.


Erlang code.

-define(S,  32).
-define(T,  9).
-define(LF, 10).

unwrap({_,_,V}) -> V.

parse_num([?S | Rest]) ->
  parse_num(1, Rest, []);
parse_num([?T | Rest]) ->
  parse_num(-1, Rest, []).

parse_num(Multiplier, [?LF], Acc) ->
  Rev = lists:reverse(Acc),
  %Num = erlang:integer_to_list(Multiplier * erlang:list_to_integer(Rev, 2)),
  Multiplier * erlang:list_to_integer(Rev, 2);
parse_num(Multiplier, [?S | Rest], Acc) ->
  parse_num(Multiplier, Rest, [$0 | Acc]);
parse_num(Multiplier, [?T | Rest], Acc) ->
  parse_num(Multiplier, Rest, [$1 | Acc]).

parse_label([?LF], Acc) ->
  lists:reverse(Acc);
parse_label([?S | Rest], Acc) ->
  parse_label(Rest, [$s | Acc]);
parse_label([?T | Rest], Acc) ->
  parse_label(Rest, [$t | Acc]).


