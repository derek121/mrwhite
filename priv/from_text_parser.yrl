% In: [{stack,1},{push,1},{number,1,"6"},{stack,2},{swap,2}]

% [{stack_push,1,"6"},{stack_duplicate,1}]

 
Nonterminals ops elements op. 

Terminals stack push duplicate copy swap discard slide
          arith add sub mul divide mod
          heap store retrieve
          flow mark call jump jump_zero jump_negative end_sub end_program
          io output_char output_num read_char read_num
          number label.

Rootsymbol elements.

elements -> ops : '$1'.

ops -> op : ['$1'].
ops -> op ops : ['$1'] ++ '$2'.


% TODO: use "out" for all output, with the ws to output
% TODO: do the ws conversion stuff in the app code?

op -> stack push number        : {stack_push, erlang:list_to_integer(unwrap('$3'))}.
op -> stack duplicate          : stack_duplicate.
op -> stack copy number        : {stack_copy, erlang:list_to_integer(unwrap('$3'))}.
op -> stack swap               : stack_swap.
op -> stack discard            : stack_discard.
op -> stack slide number       : {stack_slide, erlang:list_to_integer(unwrap('$3'))}.

op -> arith add                : arith_add.
op -> arith sub                : arith_sub.
op -> arith mul                : arith_mul.
op -> arith divide             : arith_div.
op -> arith mod                : arith_mod.

op -> heap store               : heap_store.
op -> heap retrieve            : heap_retrieve.

op -> flow mark label          : {flow_mark, unwrap('$3')}.
op -> flow call label          : {flow_call, unwrap('$3')}.
op -> flow jump label          : {flow_jump, unwrap('$3')}.
op -> flow jump_zero label     : {flow_jump_zero, unwrap('$3')}.
op -> flow jump_negative label : {flow_jump_negative, unwrap('$3')}.
op -> flow end_sub             : flow_end_sub.
op -> flow end_program         : flow_end_program.

op -> io output_char           : io_output_char.
op -> io output_num            : io_output_num.
op -> io read_char             : io_read_char.
op -> io read_num              : io_read_num.

Erlang code.

unwrap({_,_,V}) -> V.

