% Example source:
% [{stack_push,6},stack_duplicate]

Definitions. 

WS    = [\s\r\n\t]
NUM   = [0-9]+
LABEL = [st]+

Rules.

{WS}         : skip_token.

stack         : {token, {stack,         TokenLine}}.
push          : {token, {push,          TokenLine}}.
duplicate     : {token, {duplicate,     TokenLine}}.
copy          : {token, {copy,          TokenLine}}.
swap          : {token, {swap,          TokenLine}}.
discard       : {token, {discard,       TokenLine}}.
slide         : {token, {slide,         TokenLine}}.

arith         : {token, {arith,         TokenLine}}.
add           : {token, {add,           TokenLine}}.
sub           : {token, {sub,           TokenLine}}.
mul           : {token, {mul,           TokenLine}}.
div           : {token, {divide,        TokenLine}}. % Can't use "div" since it's a reserved word in Erlang
mod           : {token, {mod,           TokenLine}}.

heap          : {token, {heap,          TokenLine}}.
store         : {token, {store,         TokenLine}}.
retrieve      : {token, {retrieve,      TokenLine}}.

flow          : {token, {flow,          TokenLine}}.
mark          : {token, {mark,          TokenLine}}.
call          : {token, {call,          TokenLine}}.
jump          : {token, {jump,          TokenLine}}.
jump_zero     : {token, {jump_zero,     TokenLine}}.
jump_negative : {token, {jump_negative, TokenLine}}.
end_sub       : {token, {end_sub,       TokenLine}}.
end_program   : {token, {end_program,   TokenLine}}. % Can't use "end" since it's a reserved word in Erlang

io            : {token, {io,            TokenLine}}.
output_char   : {token, {output_char,   TokenLine}}.
output_num    : {token, {output_num,    TokenLine}}.
read_char     : {token, {read_char,     TokenLine}}.
read_num      : {token, {read_num,      TokenLine}}.

{NUM}         : {token, {number,        TokenLine, TokenChars}}.
{LABEL}       : {token, {label,         TokenLine, TokenChars}}.


Erlang code.

