-define(S, $ ).
-define(T, $\t).
-define(L, $\n).

-define(STACK_PUSH,      [?S, ?S]).
-define(STACK_DUPLICATE, [?S, ?L, ?S]).
-define(STACK_COPY,      [?S, ?T, ?S]).
-define(STACK_SWAP,      [?S, ?L, ?T]).
-define(STACK_DISCARD,   [?S, ?L, ?L]).
-define(STACK_SLIDE,     [?S, ?T, ?L]).

-define(ARITH_ADD    , [?T, ?S, ?S, ?S]).
-define(ARITH_SUB    , [?T, ?S, ?S, ?T]).
-define(ARITH_MUL    , [?T, ?S, ?S, ?L]).
-define(ARITH_DIV    , [?T, ?S, ?T, ?S]).
-define(ARITH_MOD    , [?T, ?S, ?T, ?T]).

-define(HEAP_STORE    , [?T, ?T, ?S]).
-define(HEAP_RETRIEVE , [?T, ?T, ?T]).

-define(FLOW_MARK ,         [?L, ?S, ?S]).
-define(FLOW_CALL,          [?L, ?S, ?T]).
-define(FLOW_JUMP,          [?L, ?S, ?L]).
-define(FLOW_JUMP_ZERO,     [?L, ?T, ?S]).
-define(FLOW_JUMP_NEGATIVE, [?L, ?T, ?T]).
-define(FLOW_END_SUB,       [?L, ?T, ?L]).
-define(FLOW_END_PROGRAM,   [?L, ?L, ?L]).

-define(IO_OUTPUT_CHAR,     [?T, ?L, ?S, ?S]).
-define(IO_OUTPUT_NUM,      [?T, ?L, ?S, ?T]).
-define(IO_READ_CHAR,       [?T, ?L, ?T, ?S]).
-define(IO_READ_NUM,        [?T, ?L, ?T, ?T]).

