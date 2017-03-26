-module(mrwhite_general).

-export([run/1]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

run(Ops) ->
  {Ops2, Labels} = find_labels(Ops),
  Stack = [],
  Heap = #{},
  Ret = run(Ops2, Stack, Heap, Labels, 0, Ops2),
  handle_return(Ret).

-ifndef(TEST).
handle_return(_Ret) ->
  ok.
-else.
handle_return(Ret) ->
  Ret.
-endif.

find_labels(Ops) ->
  find_labels(Ops, 0, #{}, []).

-spec find_labels([term()], non_neg_integer(), #{string() => integer()} | #{}, [term()]) 
                 -> {[term()], #{string() => integer()}}.
find_labels([], _N, Labels, OpsOut) ->
  {lists:reverse(OpsOut), Labels};

find_labels([{flow_mark, Label} | Rest], N, Labels, OpsOut) ->
  % Add Label at the current line number, and don't add flow_mark to OpsOut, 
  %   and don't increment N, so that the next op in Rest will be at line number N
  find_labels(Rest, N, Labels#{Label => N}, OpsOut);

find_labels([Op | Rest], N, Labels, OpsOut) ->
  find_labels(Rest, N + 1, Labels, [Op | OpsOut]).


-spec run([term()], [integer()], #{integer() => integer()}, #{string() => integer()}, integer(), [term()]) 
         -> {done, [integer()], #{string() => integer()}}.
run([], _Stack, _Heap, _Labels, _NumSubCalls, _AllOps) ->
  exit(unexpected_end_of_program);

run([{stack_push, N} | Rest], Stack, Heap, Labels, NumSubCalls, AllOps) ->
  run(Rest, [N | Stack], Heap, Labels, NumSubCalls, AllOps);

run([stack_duplicate | Rest], Stack = [StackTop | _StackRest], Heap, Labels, NumSubCalls, AllOps)
  when length(Stack) > 0 ->
  run(Rest, [StackTop | Stack], Heap, Labels, NumSubCalls, AllOps);
run([stack_duplicate | _Rest], Stack, _Heap, _Labels, _NumSubCalls, _AllOps) ->
  exit({invalid_stack_length, length(Stack), stack_duplicate});

run([{stack_copy, N} | Rest], Stack, Heap, Labels, NumSubCalls, AllOps)
  when length(Stack) >= N + 1 ->
  %% We consider Whitespace's arg to copy to be 0-based, while lists:nth/2 is 1-based
  Nth = lists:nth(N + 1, Stack),
  run(Rest, [Nth | Stack], Heap, Labels, NumSubCalls, AllOps);
run([{stack_copy, _N} | _Rest], Stack, _Heap, _Labels, _NumSubCalls, _AllOps) ->
  exit({invalid_stack_length, length(Stack), stack_copy});

run([stack_swap | Rest], Stack = [First, Second | StackRest], Heap, Labels, NumSubCalls, AllOps)
  when length(Stack) >= 2 ->
  run(Rest, [Second, First | StackRest], Heap, Labels, NumSubCalls, AllOps);
run([stack_swap | _Rest], Stack, _Heap, _Labels, _NumSubCalls, _AllOps) ->
  exit({invalid_stack_length, length(Stack), stack_swap});

run([stack_discard | Rest], Stack = [_Top | StackRest], Heap, Labels, NumSubCalls, AllOps)
  when length(Stack) > 0 ->
  run(Rest, StackRest, Heap, Labels, NumSubCalls, AllOps);
run([stack_discard | _Rest], Stack, _Heap, _Labels, _NumSubCalls, _AllOps) ->
  exit({invalid_stack_length, length(Stack), stack_discard});

run([{stack_slide, N} | Rest], Stack = [StackTop | StackRest], Heap, Labels, NumSubCalls, AllOps)
  when length(Stack) >= N + 1 ->
  %% From Rest, remove N entries
  Nth = lists:nthtail(N, StackRest),
  run(Rest, [StackTop | Nth], Heap, Labels, NumSubCalls, AllOps);
run([{stack_slide, _N} | _Rest], Stack, _Heap, _Labels, _NumSubCalls, _AllOps) ->
  exit({invalid_stack_length, length(Stack), stack_slide});

run([arith_add | Rest], Stack = [First, Second | StackRest], Heap, Labels, NumSubCalls, AllOps)
  when length(Stack) >= 2 ->
  run(Rest, [Second + First | StackRest], Heap, Labels, NumSubCalls, AllOps);
run([arith_add | _Rest], Stack, _Heap, _Labels, _NumSubCalls, _AllOps) ->
  exit({invalid_stack_length, length(Stack), arith_add});

run([arith_sub | Rest], Stack = [First, Second | StackRest], Heap, Labels, NumSubCalls, AllOps)
  when length(Stack) >= 2->
  run(Rest, [Second - First | StackRest], Heap, Labels, NumSubCalls, AllOps);
run([arith_sub | _Rest], Stack, _Heap, _Labels, _NumSubCalls, _AllOps) ->
  exit({invalid_stack_length, length(Stack), arith_sub});

run([arith_mul | Rest], Stack = [First, Second | StackRest], Heap, Labels, NumSubCalls, AllOps)
  when length(Stack) >= 2 ->
  run(Rest, [Second * First | StackRest], Heap, Labels, NumSubCalls, AllOps);
run([arith_mul | _Rest], Stack, _Heap, _Labels, _NumSubCalls, _AllOps) ->
  exit({invalid_stack_length, length(Stack), arith_mul});

run([arith_div | Rest], Stack = [First, Second | StackRest], Heap, Labels, NumSubCalls, AllOps)
  when length(Stack) >= 2 ->
  run(Rest, [Second div First | StackRest], Heap, Labels, NumSubCalls, AllOps);
run([arith_div | _Rest], Stack, _Heap, _Labels, _NumSubCalls, _AllOps) ->
  exit({invalid_stack_length, length(Stack), arith_div});

run([arith_mod | Rest], Stack = [First, Second | StackRest], Heap, Labels, NumSubCalls, AllOps)
  when length(Stack) >= 2 ->
  run(Rest, [Second rem First | StackRest], Heap, Labels, NumSubCalls, AllOps);
run([arith_mod | _Rest], Stack, _Heap, _Labels, _NumSubCalls, _AllOps) ->
  exit({invalid_stack_length, length(Stack), arith_mod});

run([heap_store | Rest], Stack = [Value, Address | StackRest], Heap, Labels, NumSubCalls, AllOps)
  when length(Stack) >= 2 ->
  run(Rest, StackRest, Heap#{Address => Value}, Labels, NumSubCalls, AllOps);
run([heap_store | _Rest], Stack, _Heap, _Labels, _NumSubCalls, _AllOps) ->
  exit({invalid_stack_length, length(Stack), heap_store});

run([heap_retrieve | Rest], Stack = [Address | StackRest], Heap, Labels, NumSubCalls, AllOps)
  when length(Stack) >= 2 ->
  case maps:get(Address, Heap, undefined) of
    undefined ->
      exit({unknown_heap_address, Address});
    Value ->
      run(Rest, [Value | StackRest], Heap, Labels, NumSubCalls, AllOps)
  end;
run([heap_retrieve | _Rest], Stack, _Heap, _Labels, _NumSubCalls, _AllOps) ->
  exit({invalid_stack_length, length(Stack), heap_retrieve});

run([{flow_call, Label} | Rest], Stack, Heap, Labels, NumSubCalls, AllOps) ->
  case maps:get(Label, Labels, undefined) of
    undefined -> 
      exit({unknown_label_for_call_sub, Label});
    N         ->
      SubOps = lists:nthtail(N, AllOps),
      {sub_done, SubStack, SubHeap, Labels} = run(SubOps, Stack, Heap, Labels, NumSubCalls + 1, AllOps),
      run(Rest, SubStack, SubHeap, Labels, NumSubCalls, AllOps)
  end;

run([{flow_jump, Label} | _Rest], Stack, Heap, Labels, NumSubCalls, AllOps) ->
  case maps:get(Label, Labels, undefined) of
    undefined ->
      exit({unknown_label_for_jump, Label});
    N ->
      %io:format(user, "Jump to ~p: ~p~n", [N, Label]),
      SubOps = lists:nthtail(N, AllOps),
      run(SubOps, Stack, Heap, Labels, NumSubCalls, AllOps)
  end;

run([{flow_jump_zero, Label} | Rest], Stack = [Top | StackRest], Heap, Labels, NumSubCalls, AllOps)
  when length(Stack) >= 0 ->
  %% Fail if Label is unknown even if we woudn't jump (if top of stack is not 0)
  case maps:get(Label, Labels, undefined) of
    undefined ->
      exit({unknown_label_for_jump, Label});
    N ->
      case Top of
        0 ->
          SubOps = lists:nthtail(N, AllOps),
          run(SubOps, StackRest, Heap, Labels, NumSubCalls, AllOps);
        _ ->
          run(Rest, StackRest, Heap, Labels, NumSubCalls, AllOps)
      end
  end;
run([{flow_jump_zero, _Label} | _Rest], Stack, _Heap, _Labels, _NumSubCalls, _AllOps) ->
  exit({invalid_stack_length, length(Stack), flow_jump_zero});

run([{flow_jump_negative, Label} | Rest], [Top | StackRest], Heap, Labels, NumSubCalls, AllOps) ->
  %% Fail if Label is unknown even if we woudn't jump (if top of stack is not negative)
  case maps:get(Label, Labels, undefined) of
    undefined ->
      exit({unknown_label_for_jump, Label});
    N ->
      case Top of
        TopVal when TopVal < 0 ->
          SubOps = lists:nthtail(N, AllOps),
          run(SubOps, StackRest, Heap, Labels, NumSubCalls, AllOps);
        _ ->
          run(Rest, StackRest, Heap, Labels, NumSubCalls, AllOps)
      end
  end;
run([{flow_jump_negative, _Label} | _Rest], Stack, _Heap, _Labels, _NumSubCalls, _AllOps) ->
  exit({invalid_stack_length, length(Stack), flow_jump_negative});

run([flow_end_sub | _Rest], _Stack, _Heap, _Labels, _NumSubCalls = 0, _AllOps) ->
  exit(end_sub_not_in_sub);
run([flow_end_sub | _Rest], Stack, Heap, Labels, _NumSubCalls, _AllOps) ->
  {sub_done, Stack, Heap, Labels};
run([flow_end_program], Stack, Heap, Labels, _NumSubCalls, _AllOps) ->
  {done, Stack, Heap, Labels};
run([flow_end_program | _Rest], Stack, Heap, Labels, _NumSubCalls, _AllOps) ->
  {done, Stack, Heap, Labels};

run([io_output_char | Rest], [Top | StackRest], Heap, Labels, NumSubCalls, AllOps) ->
  io:format(user, "~c", [Top]),
  run(Rest, StackRest, Heap, Labels, NumSubCalls, AllOps);

run([io_output_num | Rest], [Top | StackRest], Heap, Labels, NumSubCalls, AllOps) ->
  %% To see from the eunit test:
  io:format(user, "~w", [Top]),
  run(Rest, StackRest, Heap, Labels, NumSubCalls, AllOps);

run([io_read_char | Rest], [Top | StackRest], Heap, Labels, NumSubCalls, AllOps) ->
  C = hd(io:get_chars("", 1)),
  Heap2 = Heap#{Top => C},
  run(Rest, StackRest, Heap2, Labels, NumSubCalls, AllOps);

run([io_read_num | Rest], [Top | StackRest], Heap, Labels, NumSubCalls, AllOps) ->
  N = erlang:list_to_integer(lists:droplast(io:get_line(""))),
  Heap2 = Heap#{Top => N},
  run(Rest, StackRest, Heap2, Labels, NumSubCalls, AllOps).

%run(A, B, C, D, E, F) ->
%  io:format(user, "A: ~p. B: ~p. C: ~p. D: ~p. E: ~p. F: ~p~n", [A, B, C, D, E, F]).


-ifdef(TEST).

double_convert_test() ->
  S = "stack push 6\nstack duplicate\nstack copy 7\nstack swap\nstack discard\nstack slide 2\n"
      "arith add\narith sub\narith mul\narith div\narith mod\n"
      "heap store\nheap retrieve\n"
      "flow mark stst\nflow call ttss\nflow jump tsts\nflow jump_zero stss\nflow jump_negative ssst\n"
      "flow end_sub\nflow end_program\n"
      "io output_char\nio output_num\nio read_char\nio read_num\n",

  Ws = mrwhite_from_text:convert(S, return),
  S2 = mrwhite_from_whitespace:convert(Ws, return),

  ?assertEqual(S, S2).

lex_parse_test() ->
  S = "stack push 6\nstack duplicate\nstack copy 7\nstack swap\nstack discard\nstack slide 2\n"
      "arith add\narith sub\narith mul\narith div\narith mod\n"
      "heap store\nheap retrieve\n"
      "flow mark stst\nflow call ttss\nflow jump tsts\nflow jump_zero stss\nflow jump_negative ssst\n"
      "flow end_sub\nflow end_program\n"
      "io output_char\nio output_num\nio read_char\nio read_num\n",

  Ws = mrwhite_from_text:convert(S, return),

  TextParsed = mrwhite_from_text:lex_parse(S),
  WsParsed   = mrwhite_from_whitespace:lex_parse(Ws),
  
  ?assertEqual(TextParsed, WsParsed).

find_labels_test() ->
  OpsIn = [
    {flow_mark, "stt"},
    stack_duplicate,
    arith_add,
    {flow_mark, "tss"},
    heap_store],

  {OpsOut, LabelsOut} = find_labels(OpsIn),

  ExpectedOpsOut = [
    stack_duplicate,
    arith_add,
    heap_store],
  ExpectedLabels = #{"stt" => 0, "tss" => 2},

  ?assertEqual(ExpectedOpsOut, OpsOut),
  ?assertEqual(ExpectedLabels, LabelsOut).

run_stack_push_test() ->
  OpsIn = [
    {stack_push, 23},
    {stack_push, 42},
    flow_end_program],

  {done, Stack, _Heap, _Labels} = run(OpsIn),

  ?assertEqual([42, 23], Stack).

run_stack_duplicate_test() ->
  OpsIn = [
    {stack_push, 23},
    stack_duplicate,
    flow_end_program],

  {done, Stack, _Heap, _Labels} = run(OpsIn),

  ?assertEqual([23, 23], Stack).

run_stack_duplicate_fail_test() ->
  OpsIn = [
    stack_duplicate,
    flow_end_program],
  ?assertExit({invalid_stack_length, 0, stack_duplicate}, run(OpsIn)).

run_stack_copy_test() ->
  OpsIn = [
    {stack_push, 23},
    {stack_push, 42},
    {stack_copy, 1},
    flow_end_program],

  {done, Stack, _Heap, _Labels} = run(OpsIn),

  ?assertEqual([23, 42, 23], Stack).

run_stack_copy_fail_test() ->
  OpsIn = [
    {stack_copy, 0},
    flow_end_program],
  ?assertExit({invalid_stack_length, 0, stack_copy}, run(OpsIn)).

run_stack_swap_test() ->
  OpsIn = [
    {stack_push, 23},
    {stack_push, 42},
    stack_swap,
    flow_end_program],

  {done, Stack, _Heap, _Labels} = run(OpsIn),

  ?assertEqual([23, 42], Stack).

run_stack_swap_fail_test() ->
  OpsIn = [
    {stack_push, 21},
    stack_swap,
    flow_end_program],
  ?assertExit({invalid_stack_length, 1, stack_swap}, run(OpsIn)).

run_stack_discard_test() ->
  OpsIn = [
    {stack_push, 23},
    {stack_push, 42},
    {stack_push, 21},
    stack_discard,
    flow_end_program],

  {done, Stack, _Heap, _Labels} = run(OpsIn),

  ?assertEqual([42, 23], Stack).

run_stack_discard_fail_test() ->
  OpsIn = [
    stack_discard,
    flow_end_program],
  ?assertExit({invalid_stack_length, 0, stack_discard}, run(OpsIn)).

run_stack_slide_test() ->
  OpsIn = [
    {stack_push, 25},
    {stack_push, 24},
    {stack_push, 23},
    {stack_push, 22},
    {stack_push, 21},
    {stack_push, 20},
    {stack_slide, 2},
    flow_end_program],

  {done, Stack, _Heap, _Labels} = run(OpsIn),

  ?assertEqual([20, 23, 24, 25], Stack).

run_stack_slide_fail_test() ->
  OpsIn = [
    {stack_push, 21},
    {stack_slide, 1},
    flow_end_program],
  ?assertExit({invalid_stack_length, 1, stack_slide}, run(OpsIn)).

run_arith_add_test() ->
  OpsIn = [
    {stack_push, 23},
    {stack_push, 42},
    {stack_push, 21},
    arith_add,
    flow_end_program],

  {done, Stack, _Heap, _Labels} = run(OpsIn),

  ?assertEqual([63, 23], Stack).

run_arith_add_fail_test() ->
  OpsIn = [
    {stack_push, 21},
    arith_add,
    flow_end_program],
  ?assertExit({invalid_stack_length, 1, arith_add}, run(OpsIn)).

run_arith_sub_test() ->
  OpsIn = [
    {stack_push, 23},
    {stack_push, 9},
    {stack_push, 3},
    arith_sub,
    flow_end_program],

  {done, Stack, _Heap, _Labels} = run(OpsIn),

  ?assertEqual([6, 23], Stack).

run_arith_sub_fail_test() ->
  OpsIn = [
    {stack_push, 21},
    arith_sub,
    flow_end_program],
  ?assertExit({invalid_stack_length, 1, arith_sub}, run(OpsIn)).

run_arith_mul_test() ->
  OpsIn = [
    {stack_push, 23},
    {stack_push, 9},
    {stack_push, 3},
    arith_mul,
    flow_end_program],

  {done, Stack, _Heap, _Labels} = run(OpsIn),

  ?assertEqual([27, 23], Stack).

run_arith_mul_fail_test() ->
  OpsIn = [
    {stack_push, 21},
    arith_mul,
    flow_end_program],
  ?assertExit({invalid_stack_length, 1, arith_mul}, run(OpsIn)).

run_arith_div_test() ->
  OpsIn = [
    {stack_push, 23},
    {stack_push, 12},
    {stack_push, 3},
    arith_div,
    flow_end_program],

  {done, Stack, _Heap, _Labels} = run(OpsIn),

  ?assertEqual([4, 23], Stack).

run_arith_div_fail_test() ->
  OpsIn = [
    {stack_push, 21},
    arith_div,
    flow_end_program],
  ?assertExit({invalid_stack_length, 1, arith_div}, run(OpsIn)).

run_arith_mod_test() ->
  OpsIn = [
    {stack_push, 23},
    {stack_push, 7},
    {stack_push, 4},
    arith_mod,
    flow_end_program],

  {done, Stack, _Heap, _Labels} = run(OpsIn),

  ?assertEqual([3, 23], Stack).

run_arith_mod_fail_test() ->
  OpsIn = [
    {stack_push, 21},
    arith_mod,
    flow_end_program],
  ?assertExit({invalid_stack_length, 1, arith_mod}, run(OpsIn)).

run_heap_store_test() ->
  OpsIn = [
    {stack_push, 23},
    {stack_push, 3},
    {stack_push, 5},
    heap_store,
    flow_end_program],

  {done, Stack, Heap, _Labels} = run(OpsIn),

  ?assertEqual([23], Stack),
  ?assertEqual(#{3 => 5}, Heap).

run_heap_store_fail_test() ->
  OpsIn = [
    {stack_push, 21},
    heap_store,
    flow_end_program],
  ?assertExit({invalid_stack_length, 1, heap_store}, run(OpsIn)).

run_heap_retrieve_test() ->
  OpsIn = [
    {stack_push, 23},
    {stack_push, 3},
    {stack_push, 5},
    heap_store,
    {stack_push, 3},
    heap_retrieve,
    flow_end_program],

  {done, Stack, _Heap, _Labels} = run(OpsIn),

  ?assertEqual([5, 23], Stack).

run_heap_retrieve_fail_test() ->
  OpsIn = [
    {stack_push, 21},
    heap_retrieve,
    flow_end_program],
  ?assertExit({invalid_stack_length, 1, heap_retrieve}, run(OpsIn)).


% TODO: add test for the exit error if run out of ops
% TODO: add tests for unknown label for sub or jump
% TODO: add test for nested jumps

flow_call_test() ->
  OpsIn = [
    {stack_push, 23},
    {flow_call, "stt"},
    {stack_push, 42},
    flow_end_program,

    {flow_mark, "stt"},
    stack_duplicate,
    flow_end_sub],

  {done, Stack, _Heap, _Labels} = run(OpsIn),
  ?assertEqual([42, 23, 23], Stack).

flow_call_fail_test() ->
  BadLabel = "ttttttttt",
  OpsIn = [
    {flow_call, BadLabel},
    flow_end_program,
    
    {flow_mark, "stt"},
    flow_end_sub],

  ?assertExit({unknown_label_for_call_sub, BadLabel}, run(OpsIn)).

flow_call_nested_test() ->
  OpsIn = [
    {stack_push, 23},
    {flow_call, "stt"},
    {stack_push, 42},
    flow_end_program,
    
    {flow_mark, "stt"},
    stack_duplicate,
    {flow_call, "sttt"},
    flow_end_sub,
    
    {flow_mark, "sttt"},
    arith_add,
    flow_end_sub],

  {done, Stack, _Heap, _Labels} = run(OpsIn),
  ?assertEqual([42, 46], Stack).

flow_jump_test() ->
  OpsIn = [
    {stack_push, 23},
    {flow_jump, "tts"},
    {flow_mark, "ttss"},
    {stack_push, 43},
    flow_end_program,

    {flow_mark, "tts"},
    {stack_push, 42},
    {flow_jump, "ttss"}],

  {done, Stack, _Heap, _Labels} = run(OpsIn),
  ?assertEqual([43, 42, 23], Stack).

flow_jump_fail_test() ->
  BadLabel = "ttttttttt",
  OpsIn = [
    {flow_jump, BadLabel},
    flow_end_program,

    {flow_mark, "stt"},
    flow_end_sub],

  ?assertExit({unknown_label_for_jump, BadLabel}, run(OpsIn)).

flow_jump_zero_yes_test() ->
  OpsIn = [
    {stack_push, 0},
    {flow_jump_zero, "tts"},
    {flow_mark, "ttss"},
    {stack_push, 43},
    flow_end_program,

    {flow_mark, "tts"},
    {stack_push, 42},
    {flow_jump, "ttss"}],

  {done, Stack, _Heap, _Labels} = run(OpsIn),
  ?assertEqual([43, 42], Stack).

flow_jump_zero_no_test() ->
  OpsIn = [
    {stack_push, 1},
    {flow_jump_zero, "tts"},
    {flow_mark, "ttss"},
    {stack_push, 43},
    flow_end_program,

    {flow_mark, "tts"},
    {stack_push, 42},
    {flow_jump, "ttss"}],

  {done, Stack, _Heap, _Labels} = run(OpsIn),
  ?assertEqual([43], Stack).

flow_jump_zero_fail_label_test() ->
  BadLabel = "ttttttttt",
  OpsIn = [
    {stack_push, 1},
    {flow_jump_zero, BadLabel},
    flow_end_program,

    {flow_mark, "stt"},
    flow_end_sub],

  ?assertExit({unknown_label_for_jump, BadLabel}, run(OpsIn)).

flow_jump_zero_fail_stack_test() ->
  BadLabel = "ttttttttt",
  OpsIn = [
    {flow_jump_zero, BadLabel},
    flow_end_program,

    {flow_mark, "stt"},
    flow_end_sub],

  ?assertExit({invalid_stack_length, 0, flow_jump_zero}, run(OpsIn)).

flow_jump_negative_yes_test() ->
  OpsIn = [
    {stack_push, -1},
    {flow_jump_negative, "tts"},
    {flow_mark, "ttss"},
    {stack_push, 43},
    flow_end_program,

    {flow_mark, "tts"},
    {stack_push, 42},
    {flow_jump, "ttss"}],

  {done, Stack, _Heap, _Labels} = run(OpsIn),
  ?assertEqual([43, 42], Stack).

flow_jump_negative_no_test() ->
  OpsIn = [
    {stack_push, 1},
    {flow_jump_negative, "tts"},
    {flow_mark, "ttss"},
    {stack_push, 43},
    flow_end_program,

    {flow_mark, "tts"},
    {stack_push, 42},
    {flow_jump, "ttss"}],

  {done, Stack, _Heap, _Labels} = run(OpsIn),
  ?assertEqual([43], Stack).

flow_jump_negative_fail_label_test() ->
  BadLabel = "ttttttttt",
  OpsIn = [
    {stack_push, 1},
    {flow_jump_negative, BadLabel},
    flow_end_program,

    {flow_mark, "stt"},
    flow_end_sub],

  ?assertExit({unknown_label_for_jump, BadLabel}, run(OpsIn)).

flow_jump_negative_fail_stack_test() ->
  BadLabel = "ttttttttt",
  OpsIn = [
    {flow_jump_negative, BadLabel},
    flow_end_program,

    {flow_mark, "stt"},
    flow_end_sub],

  ?assertExit({invalid_stack_length, 0, flow_jump_negative}, run(OpsIn)).

flow_end_sub_not_in_sub_test() ->
  OpsIn = [
    {stack_push, 23},
    flow_end_sub],

  ?assertExit(end_sub_not_in_sub, run(OpsIn)).

run_output_char_test() ->
  % Not actually a test- just to visually inspect...
  OpsIn = [
    {stack_push, $D},
    io_output_char,
    flow_end_program],

  io:format(user, "output_char_test~n", []),
  {done, _Stack, _Heap, _Labels} = run(OpsIn),
  ok.

run_output_num_test() ->
  % Not actually a test- just to visually inspect...
  OpsIn = [
    {stack_push, $D}, % ASCII 68
    io_output_num,
    flow_end_program],

  io:format(user, "read_char_test~n", []),
  {done, _Stack, _Heap, _Labels} = run(OpsIn),
  ok.

% The test gets EOF
% In the shell can check with something like
% mrwhite_from_text:run("stack push 100 io read_char flow end_program").
% When it waits, typing D, for example, results in
% {done,"d",#{100 => 68},#{}}
% That's {done, Stack, Heap, Ops}
% The stack is a list, containing the number 100, which is ASCII d, which the shell formats as a string
% ASCII D is 68, which we see as the value in the heap at address 100, as expected
%run_read_char_test() ->
%  OpsIn = [
%    {stack_push, 100},
%    io_read_char,
%    flow_end_program],
%
%  {done, Stack, Heap, _Labels} = run(OpsIn),
%  io:format(user, "STACK: ~p. HEAP: ~p~n", [Stack, Heap]),
%  ok.

-endif.



