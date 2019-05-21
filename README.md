Vaquero
=======

Vaquero is a Lisp variant with a single-dispatch message passing,
multiple dispatch via generic procedures,
first-class delimited lexical environments,
first-class delimited continuations,
restartable exceptions,
object-capability security on operating system interfaces,
and syntactic abstraction via procedural macros.

## Basic usage

The Vaquero bootstrap interpreter is written in [Chicken Scheme](http://www.call-cc.org/).  
You'll need this to compile and run it.

```bash
cd Vaquero
sh ./bin/get_eggs.sh   # may need to run this part as root
sh ./bin/compile.sh
sh ./bin/run_tests.sh
```

Compilation should take a minute or two. `run_tests.sh` will execute a few hundred tests written in Vaquero. This should give the user a feel for whether the compilation worked.

```bash
home> ./vaquero 

Usage:

vaquero repl
vaquero eval "<code string>"
vaquero run <filename>
vaquero check <filename>
vaquero expand <filename>
vaquero compile <filename>
vaquero clean

vaquero <filename> is shorthand for vaquero run <filename>
```

The Vaquero executable responds to the seven messages above.

* **repl** starts a Read-Eval-Print Loop.
* **eval** parses a string into code and evaluates it.
* **run** takes a filename and expands, links, compiles, caches, and executes it.
* **check** tests the syntax of a file.
* **expand** pretty-prints the named file with all operators expanded
* **compile** expands, links, compiles, and caches a file without executing it.
* **clean** clears the Vaquero cache in ~/.vaquero.


## Hello, world!

```bash
# you don't have to use rlwrap to run the REPL,
# but I can't recommend it enough

home> rlwrap ./vaquero repl
```

```scheme
vaquero> (say "Hello, I'm Johnny Cash.")
Hello, I'm Johnny Cash.
null
vaquero>
```

## More examples

The executable examples below show various facets of the language.

[Hello, world!](/examples/hello_world.vaq)

[Basic syntax](/examples/basics.vaq)

[Traditional single-dispatch OOP](/examples/traditional_oop.vaq)

[An entity system using generic procedures](/examples/entity_system.vaq)

[A simple echo server](/examples/echo_server.vaq)


## Features

### Lisp-1 syntax

Vaquero code is composed of [S-expressions](https://en.wikipedia.org/wiki/S-expression).
Structures in parentheses are assumed to be lists unless they contain a period, in which case they are a pair.
Other structures begin with #(<name> ...).

```scheme
(1 . 2)                 ; pair
(1 2)                   ; list of two elements
#(cell 5)               ; cell containing the number 5
#(vector 2 3 5)         ; vector of three cells
#(object foo 2 bar 3)   ; user-defined type
```

Unquoted lists are evaluated as code.
The head of the list (the first item) should be either an operator, a procedure, or another object that responds to the **apply** message.

### Immutable global environment

The Vaquero global environment is sacred.  Its names cannot be reassigned or shadowed.  This helps eliminate a certain class of symbol capture problems with macros.  So long as your operators are pure - that is, relying only on global operators and procedures or other pure operators - you should have little to fear.  **proc** will always mean **proc**.

### Delimited lexical scope

Vaquero is lexically-scoped by default.  However, the [wall](https://github.com/TurtleKitty/Vaquero/wiki/wall) operator allows one to delimit the extent to which subforms can capture the enclosing environment.

```scheme
(let (x 1 y 2)
   (wall (z (+ x y))
      z))

; 3

(let (x 1 y 2)
   (wall (z (+ x y))
      x))

; (runtime-error (undefined-symbol x "Name not defined."))
```

### First-class environments

The [env](https://github.com/TurtleKitty/Vaquero/wiki/environment) operator grants access to the local environment object.

### First-class delimited continuations

First class sub-continuation capture gives the programmer the ability to build coroutines, generators, backtracking, or any other control structure without the [sorrows](http://okmij.org/ftp/continuations/against-callcc.html) of full continuation capture.

[gate](https://github.com/TurtleKitty/Vaquero/wiki/gate) and [capture](https://github.com/TurtleKitty/Vaquero/wiki/capture) correspond to **reset** and **shift** in the academic literature on delimited continuations.

```scheme
(gate
   (+ 1
      (capture kont
         (+ 7
            (kont (kont (kont 2)))))))

; 12
```

### Restartable exceptions

The [guard](https://github.com/TurtleKitty/Vaquero/wiki/guard) operator adds a handler to the error continuation, which is separate from the user continuation.
When an error is signaled (via [fail](https://github.com/TurtleKitty/Vaquero/wiki/fail) or [error](https://github.com/TurtleKitty/Vaquero/wiki/error)),
a handler can do one of three things: return a default value to the enclosing scope,
retry the computation from the sub-form where it errored by providing another value,
or throw an error itself, at which point the next handler in the error continuation is called.

```scheme
(proc handler (err kontinue)
   (if (= err 'resume)
      (kontinue 69)
      (if (= err 'default)
         42
         (fail 'aww-hell))))

(guard handler
   (+ 3 2))

; 5

(guard handler
   (+ 3 (fail 'default)))

; 42

(guard handler
   (+ 3 (fail 'resume)))

; 72

(guard handler
   (+ 3 (fail 'crap)))

; (runtime-error aww-hell)

; most system procs throw a more sophisticated error than a symbol

(proc show-it (e k)
   (list e.name e.form e.message))

(guard show-it
   (error 'wrong-way '(go left) "Left was a poor choice."))

; (wrong-way (go left) "Left was a poor choice.")
```

### Lexically scoped module import via HTTP

Modules are imported via [use](https://github.com/TurtleKitty/Vaquero/wiki/use).  The pathname can be a local filesystem path or an HTTP[S] uri.

Modules [export](https://github.com/TurtleKitty/Vaquero/wiki/export) a set of names. These names are packaged into a module object in the environment of the use statement.

The [import](https://github.com/TurtleKitty/Vaquero/wiki/import) operator allows the programmer to import names into the current environment from a module previously defined by a use statement.

```scheme
; dk.vaq

(export
   shred
   pentuple)

(proc shred (x)
   (* x x 100))

(op pentuple (x)
   %(list $x $x $x $x $x))
```

The pentuple operator uses [quasiquotation](https://github.com/TurtleKitty/Vaquero/wiki/qq).

```scheme
#!/usr/local/bin/vaquero run

; prog.vaq

(use dk "dk.vaq")

(def n sys.rest.head.to-number)

(say (dk.shred n))
(say (dk.pentuple '(foo bar)))

(import dk shred pentuple)

(say (shred (+ n 2)))
(say (pentuple 'foonballardy))
```

```bash
home> ./prog.vaq 7
4900
((foo bar) (foo bar) (foo bar) (foo bar) (foo bar))
8100
(foonballardy foonballardy foonballardy foonballardy foonballardy)
```

Two modules may have mutually recursive procedures.
If modules import one another, neither can rely on the other to finish evaluation;
procedures may be mutually recursive, but not operators.
Such leads to an infinite loop.

### Object capability security on operating system interfaces

The only global interfaces to the operating system are stdout, stdin, stderr, and the global procedures which use them: read, write, print, say, and log.
All parts of a program, including modules, have access to these.
All other operating system services are contained in the [sys](https://github.com/TurtleKitty/Vaquero/wiki/sys) object, which is available only to the top-level program.
Libraries that wish to read command-line arguments, fork processes, or open files must be passed this capability from the top-level.

### Generic procedures with predicate dispatch

[gen](https://github.com/TurtleKitty/Vaquero/wiki/generic) and [spec](https://github.com/TurtleKitty/Vaquero/wiki/generic) allow the programmer to create generic procedures
that dispatch based on the types of an arbitrary list of arguments.  It's one answer to the [expression problem](https://en.wikipedia.org/wiki/Expression_problem).

### Operators: compile-time procedures

Vaquero [operators](https://github.com/TurtleKitty/Vaquero/wiki/operator) use ordinary Vaquero code to transform source code at compile time.
This allows user-level creation of custom syntax.
An example of this power: the only primitive form of looping is tail recursion; all others (loop, for, while) are defined in terms of recursion via operators.
They can be found in [the prelude](interpreter/prelude.vaq).

### Reader literals for text construction and variable interpolation

Vaquero has a few convenient [reader literals](https://github.com/TurtleKitty/Vaquero/wiki/reader) inspired by Lisp and Perl.

### Reference

The [wiki](https://github.com/TurtleKitty/Vaquero/wiki) contains a more detailed reference of all core data types, operators, procedures, and objects.  

### Bugs

Most of the interpreter was written by Felix Winkelmann and the Chicken Scheme team and they even don't know it.
The bootstrap interpreter snarfs a lot of functionality from Chicken, so implementation details sometimes leak through.

The REPL is a bit fragile.  It doesn't handle reader errors gracefully.

Unix domain sockets are broken at the moment because the egg I used hasn't yet been updated to Chicken 5.

The interpreter works well. The metacircular compiler is a work-in-progress; it ain't ready for prime-time.


