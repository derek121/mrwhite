# Mr. White
## Erlang Interpreter for the Whitespace Language

[Whitespace](http://compsoc.dur.ac.uk/whitespace/tutorial.html) is an imperative, stack-based language whose only significant characters are space, tab, and linefeed. Operations consists of an instruction type followed by the command. The interpreter maintains a stack of integers and a heap.

Besides the execution of Whitespace programs, this application also supports execution of, and conversion between, a textual format that makes it easier to understand what the program is doing.

For example, pushing a value of 5 onto the stack in Whitespace is coded as SSSTSTL, where S, T, and L represent a space, tab, and linefeed, respectively. This can be seen by the following:

```
1> W = mrwhite_from_text:covert("stack push 5", return).
"   \t \t\n"
2> mrwhite_from_whitespace:convert(W, return).
"stack push 5\n"
```

The canonical example at the [Whitespace page](http://compsoc.dur.ac.uk/whitespace/tutorial.html) mentioned above, which can be found in [`priv/sample/canonical.ws`](priv/sample/canonical.ws) and [`priv/sample/canonical.wst`](priv/sample/canonical.wst), illustrates a number of the operations supported by the language, and can be run as follows.

```
3> mrwhite_from_whitespace:run({file_in, "priv/sample/canonical.ws"}).
1
2
3
4
5
6
7
8
9
10
ok
```

## Presentation

Mr. White was presented in a lighning talk at [Erlang & Elixir Factory SF Bay Area 2017](http://www.erlang-factory.com/sfbay2017/) ([slides](https://github.com/derek121/mrwhite/blob/master/priv/mrwhite.pdf))

