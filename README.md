tiny-js
=======

This project aims to be an extremely simple (~2000 line) JavaScript interpreter, meant for
inclusion in applications that require a simple, familiar script language that can be included
with no dependencies other than normal C++ libraries. It currently consists of two source files:
one containing the interpreter, another containing built-in functions such as String.substring.

TinyJS is not designed to be fast or full-featured. However it is great for scripting simple
behaviour, or loading & saving settings.

I make absolutely no guarantees that this is compliant to JavaScript/EcmaScript standard.
In fact I am sure it isn't. However I welcome suggestions for changes that will bring it
closer to compliance without overly complicating the code, or useful test cases to add to
the test suite.

Currently TinyJS supports:

* Variables, Arrays, Structures
* JSON parsing and output
* Functions
* Calling C/C++ code from JavaScript
* Objects with Inheritance (not fully implemented)

Please see [CodeExamples](https://github.com/gfwilliams/tiny-js/blob/wiki/CodeExamples.md) for examples of code that works...

TinyJS is released under an MIT licence.

Internal Structure
------------------------

TinyJS uses a Recursive Descent Parser, so there is no 'Parser Generator' required. It does not
compile to an intermediate code, and instead executes directly from source code. This makes it
quite fast for code that is executed infrequently, and slow for loops.

Variables, Arrays and Objects are stored in a simple linked list tree structure (42tiny-js uses a C++ Map).
This is simple, but relatively slow for large structures or arrays.
