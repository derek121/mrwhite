Definitions. 

S = \s
T = \t
L = \n

STACK_PUSH         = {S}{S}
STACK_DUP          = {S}{L}{S}
STACK_COPY         = {S}{T}{S}
STACK_SWAP         = {S}{L}{T}
STACK_DISCARD      = {S}{L}{L}
STACK_SLIDE        = {S}{T}{L}

ARITH_ADD          = {T}{S}{S}{S}
ARITH_SUB          = {T}{S}{S}{T}
ARITH_MUL          = {T}{S}{S}{L}
ARITH_DIV          = {T}{S}{T}{S}
ARITH_MOD          = {T}{S}{T}{T}

HEAP_STORE         = {T}{T}{S}
HEAP_RETRIEVE      = {T}{T}{T}

FLOW_MARK          = {L}{S}{S}
FLOW_CALL          = {L}{S}{T}
FLOW_JUMP          = {L}{S}{L}
FLOW_JUMP_ZERO     = {L}{T}{S}
FLOW_JUMP_NEGATIVE = {L}{T}{T}
FLOW_END_SUB       = {L}{T}{L}
FLOW_END           = {L}{L}{L}

IO_OUTPUT_CHAR     = {T}{L}{S}{S}
IO_OUTPUT_NUM      = {T}{L}{S}{T}
IO_READ_CHAR       = {T}{L}{T}{S}
IO_READ_NUM        = {T}{L}{T}{T}

NUM                = [{S}{T}]+{L}
LABEL              = [{S}{T}]+{L}


Rules.

{STACK_PUSH}{NUM}           : {token, {stack_push,         TokenLine}, "N" ++ extract_number_or_label(3, TokenChars)}.
{STACK_DUP}                 : {token, {stack_duplicate,    TokenLine}}.
{STACK_COPY}{NUM}           : {token, {stack_copy,         TokenLine}, "N" ++ extract_number_or_label(4, TokenChars)}.
{STACK_SWAP}                : {token, {stack_swap,         TokenLine}}.
{STACK_DISCARD}             : {token, {stack_discard,      TokenLine}}.
{STACK_SLIDE}{NUM}          : {token, {stack_slide,        TokenLine}, "N" ++ extract_number_or_label(4, TokenChars)}.

{ARITH_ADD}                 : {token, {arith_add,          TokenLine}}.
{ARITH_SUB}                 : {token, {arith_sub,          TokenLine}}.
{ARITH_MUL}                 : {token, {arith_mul,          TokenLine}}.
{ARITH_DIV}                 : {token, {arith_div,          TokenLine}}.
{ARITH_MOD}                 : {token, {arith_mod,          TokenLine}}.
 
{HEAP_STORE}                : {token, {heap_store,         TokenLine}}.
{HEAP_RETRIEVE}             : {token, {heap_retrieve,      TokenLine}}.

{FLOW_MARK}{LABEL}          : {token, {flow_mark,          TokenLine}, "L" ++ extract_number_or_label(4, TokenChars)}.
{FLOW_CALL}{LABEL}          : {token, {flow_call,          TokenLine}, "L" ++ extract_number_or_label(4, TokenChars)}.
{FLOW_JUMP}{LABEL}          : {token, {flow_jump,          TokenLine}, "L" ++ extract_number_or_label(4, TokenChars)}.
{FLOW_JUMP_ZERO}{LABEL}     : {token, {flow_jump_zero,     TokenLine}, "L" ++ extract_number_or_label(4, TokenChars)}.
{FLOW_JUMP_NEGATIVE}{LABEL} : {token, {flow_jump_negative, TokenLine}, "L" ++ extract_number_or_label(4, TokenChars)}.
{FLOW_END_SUB}              : {token, {flow_end_sub,       TokenLine}}.
{FLOW_END}                  : {token, {flow_end_program,   TokenLine}}.

{IO_OUTPUT_CHAR}            : {token, {io_output_char,     TokenLine}}.
{IO_OUTPUT_NUM}             : {token, {io_output_num,      TokenLine}}.
{IO_READ_CHAR}              : {token, {io_read_char,       TokenLine}}.
{IO_READ_NUM}               : {token, {io_read_num,        TokenLine}}.

N[{S}{T}]+{L}               : {token, {ws_number, TokenLine, tl(TokenChars)}}.
L[{S}{T}]+{L}               : {token, {ws_label,  TokenLine, tl(TokenChars)}}.


Erlang code.

extract_number_or_label(StartIdx, TokenChars) ->
  %io:format("XXX StartIdx: ~p. TokenChars: ~p~n", [StartIdx, TokenChars]),
  % The number starts at StartIdx (1-based), but nthtail is 0-based
  lists:nthtail(StartIdx - 1, TokenChars).

