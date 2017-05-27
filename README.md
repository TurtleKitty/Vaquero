Vaquero
=======

Vaquero is a scripting language for cowboy coders.

It's a Lisp variant with an unusual object/message system, delimited lexical environments, delimited continuations, restartable exceptions, and syntactic abstraction.

## Basic usage

The Vaquero bootstrap interpreter is written in [Chicken Scheme](http://www.call-cc.org/).  
You'll need this to compile and run it.

```bash
cd Vaquero
sh ./bin/get_eggs.sh   # may need to run this part as root
sh ./bin/compile.sh
sh ./bin/run_tests.sh
```

Compilation should take a minute or two.  run_tests.sh will execute a few hundred tests written in Vaquero. This should give the user a feel for whether the compilation worked.

```bash
./vaquero 

Usage:

vaquero repl
vaquero eval "<code string>"
vaquero run <filename>
vaquero check <filename>
vaquero expand <filename>
vaquero compile <filename>
vaquero clean
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

## Data types

[null](https://github.com/TurtleKitty/Vaquero/wiki/null) lacks value.  It's not equal to anything but itself.  It answers almost every message with **null**.

[true](https://github.com/TurtleKitty/Vaquero/wiki/bool) and [false](https://github.com/TurtleKitty/Vaquero/wiki/bool) are the boolean values.  null, 0, "", and empty lists, vectors, tables, and objects evaluate to false in a boolean context.  All other values evaluate to true.

Vaquero, like most programming languages, has [numbers](https://github.com/TurtleKitty/Vaquero/wiki/number).

A [symbol](https://github.com/TurtleKitty/Vaquero/wiki/symbol) names a value.  They make the best table keys and object messages.

A string of characters is called a [text](https://github.com/TurtleKitty/Vaquero/wiki/text).  They have built-in messages for dealing with regexen.  They are normally delimited by quotes ("Hi guys!\n"), but there are also reader literals for unescaped quoting #(text ...) and string interpolation #(template ...).

```scheme

"Hello, world!"

; text literals are useful for constructing regexen without needing to escape backslashes.
#(text (\w+)-(\d+))
   ->  "(\\w+)-(\\d+)"

; template literals allow interleaving text and expressions
(def my-variable 7)
#(template foo bar {{ my-variable }})
   -> "foo bar 7"

```

A [box](https://github.com/TurtleKitty/Vaquero/wiki/box) is the simplest container type.  It holds a single value and answers messages **val** and **set!**.

```scheme

(def phantom (box 5))
   -> #(box 5)

phantom.val
   -> 5

(phantom.set! 7)
   -> 7

phantom.val
   -> 7

```

The humble [pair](https://github.com/TurtleKitty/Vaquero/wiki/pair) is the simplest compound data structure.  Pairs form into lists and trees, and Vaquero code is made from these.

```scheme

(pair 'x 1)
   -> (x . 1)

(pair 1 (pair 2 (pair ())))
   -> (1 2)

(list 1 2)
   -> (1 2)

```

A [vector](https://github.com/TurtleKitty/Vaquero/wiki/vector) is a heterogeneous array.

```scheme
(def v (vector 2 3 5))
   -> #(vector 2 3 5)

; vectors answer the apply message
(v 1)
   -> 3

(v.set! 2 42)
   -> #(vector 2 3 42)

```

A [table](https://github.com/TurtleKitty/Vaquero/wiki/table) is an unordered set of pairs (hashtables, under the hood).

```scheme

(table 'x 1 'y (+ 1 1))
   -> #(table x 1 y 2)

; the : operator auto-quotes table keys while evaluating values
(: x 1 y (+ 1 1))
   -> #(table x 1 y 2)

```

Vaquero [lambdas](https://github.com/TurtleKitty/Vaquero/wiki/lambda) are simple functions with one expression.

```scheme
(def foo (lambda (x) (+ x 10))

(foo 3)
   -> 13
```

Vaquero [procedures](https://github.com/TurtleKitty/Vaquero/wiki/procedure) have a sequence of expressions, are variadic, can accept arbitrary optional arguments.

```scheme

(proc foo (x)
   (when (not x)
      (return 'ZERO))
   (say x)
   (list x opt.y opt.z rest))

(foo 1) ; prints 1
   -> (1 null null ())

(foo 2 3 5) ; prints 2
   -> (2 null null (3 5))

(foo 7 y: 11 13 17) ; prints 7
   -> (7 11 null (13 17))

(foo 19 y: 23 z: 29 31 37) ; prints 19
   -> (19 23 29 (31 37))

(foo 0)
   -> 'ZERO

```

Vaquero [operators](https://github.com/TurtleKitty/Vaquero/wiki/operator) are procedures that run at compile time.  They alter source code to create new syntax.

Vaquero [environments](https://github.com/TurtleKitty/Vaquero/wiki/environment) are first-class.  They can be accessed with the **env** operator.

```scheme

(let (x 2 y 3) env)
   -> #(env y 3 x 2)

```

A [stream](https://github.com/TurtleKitty/Vaquero/wiki/stream) is a sequence of characters, possibly from outside the program.
Input streams have a number of useful parsing primitives.
Output streams can write, print, or say things to the outside world.

Vaquero supports both [tcp](https://github.com/TurtleKitty/Vaquero/wiki/tcp-socket) and [unix](https://github.com/TurtleKitty/Vaquero/wiki/fs-socket) sockets.

Programmers can create their own data types with the [object](https://github.com/TurtleKitty/Vaquero/wiki/object) procedure.
There are no classes or prototypes - an object is just a box with slots.
Any procedures it contains will close over the environment of their creation.
The **auto:** option allows the creation of thunks that auto-execute on message reception.
The **resend:** option allows an object to easily delegate messages to other objects.
The **default:** option allows an object to answer arbitrary messages it does not understand.

```scheme

(def simple
   (object 'foo 2 'bar 3))

   -> #(object bar foo)

(def counter (box 0))
   -> #(box 0)

(proc counter-val ()
   counter.val)

(proc counter-incr ()
   (counter.set! counter.val.inc))

(def complex
   (object
      'baz 5
      'incr counter-incr
      'val  counter-val
      resend: (list (list simple 'foo 'bar))
      auto: '(incr val)
      default:
         (lambda (msg)
            (say 'do-not-grok))))

   -> #(object incr baz val bar foo)

simple.foo
   -> 2

simple.bar
   -> 3

simple.baz
   -> ERROR 'message-not-understood  ; the default default: is to toss an error on unknown messages

complex.foo
   -> 2

complex.bar
   -> 3

complex.baz
   -> 5

complex.val
   -> 0

complex.incr
   -> 1

complex.val
   -> 1

complex.foonballardy  ; prints do-not-grok
   -> null

```


## Binding 

Environments are append-only.
**def** inserts a new name and value.
Those who want mutable state will have to use mutable structures such as boxes, vectors, and tables.

```scheme

(def foo 1)
(say foo)
   -> 1

```

## Sequencing

Execute the given expressions in sequence, returning the value of the last one.
Procs and lets have an implied seq, so this form is mostly useful in conditionals.

```scheme
(seq
    (say "Here it comes!")
    (say 42)
    true)
```

## Conditionals

```scheme

(if true 1 2)
   -> 1

(if false 1 2)
   -> 2

(when (= x 0)
   (say "Zero!"))

(cond
   (= x 0) 'zero
   (= x 1) 'one
   default:
      (seq
         (say "Fell through!")
         'infinity))

(case x
   (0) 'zero
   (1 2 3 5 7) 'small-prime-number
   (4 6 8 9) 'small-composite-number
   default: 'way-too-big)

```

## Procedures

```scheme
(def square
   (proc (x)
       (* x x)))

(proc square-2 (x)  ; syntactic sugar for (def square-2 (proc (x) ...))
   (* x x))

(def square-3    ; the _ operator is useful to create quick procedures of one argument
   (_ (* _ _)))
```

## Recursion and loops

Vaquero has tail-call optimization, so a program written in tail-recursive form can recurse forever without blowing the stack or devouring all the RAM.
[loop](https://github.com/TurtleKitty/Vaquero/wiki/loop) is useful for anonymous recursion.
The [while](https://github.com/TurtleKitty/Vaquero/wiki/while) form has **next** and **last** operators.
[for](https://github.com/TurtleKitty/Vaquero/wiki/for) has **next**, **last**, and **redo**.

```scheme
(loop recur (x items.head xs items.tail acc ())
   (if items.tail
      (send (pair x acc) 'reverse)
      (recur xs.head xs.tail (pair x acc))))

(let (i 0 total 0)
   (while (< i 20)
      (set! total (+ total i))
      (set! i i.inc)
      (list i total)))

   -> (20 190)

(for (i 0 total 0) (<= i 20) (set! i i.inc)
   (set! total (+ total i))
   total)

   -> 210

(loop go (counter 1000000)
   (if (= counter 0)
      'done
      (go counter.dec)))

   ; time passes...

   -> done

```

## Quoting

```scheme
(def foo 17)
(def bar (list 1 2 3))

foo
   -> 17

(quote foo)
   -> foo

'foo
   -> foo

bar
   -> (1 2 3)

'bar
   -> bar

(quote (foo bar baz))
   -> (foo bar baz)

'(foo bar baz)
   -> (foo bar baz)

; quasiquotation

(qq (foo bar)) -> (foo bar)
(qq ((unq foo) (unq bar))) -> (17 (1 2 3))
(qq ((unq foo) (unqs bar))) -> (17 1 2 3) 

; syntactic sugar

%($foo $bar) -> (qq ((unq foo) (unq bar))) -> (17 (1 2 3))
%($foo @bar) -> (qq ((unq foo) (unqs bar))) -> (17 1 2 3)
```

## Features

Vaquero has a number of fun features.

### Lisp-1 syntax

Vaquero code is composed of [S-expressions](https://en.wikipedia.org/wiki/S-expression).
Structures in parentheses are assumed to be lists unless they contain a period, in which case they are a pair.
Other structures begin with #(<name> ...).

```scheme

(1 . 2)     ; pair
(1 2)       ; list of two elements
#(box 5)    ; box containing the number 5
#(object foo 2 bar 3) ; user-defined type

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

   -> 3

(let (x 1 y 2)
   (wall (z (+ x y))
      x))

   -> ERROR x is not defined
```

### First-class environments

The [env](https://github.com/TurtleKitty/Vaquero/wiki/environment) operator grants access to the local environment object.

### First-class delimited continuations

First class sub-continuation capture gives the programmer the ability to build coroutines, generators, backtracking, or any other fancy control structure without some of the [sorrows](http://okmij.org/ftp/continuations/against-callcc.html) of full continuation capture.

[gate](https://github.com/TurtleKitty/Vaquero/wiki/gate) and [capture](https://github.com/TurtleKitty/Vaquero/wiki/capture) correspond to **reset** and **shift** in the academic literature on delimited continuations.

```scheme

(gate
   (+ 1
      (capture kont
         (+ 7
            (kont (kont (kont 2)))))))

   -> 12

```

### Restartable exceptions

The [guard](https://github.com/TurtleKitty/Vaquero/wiki/guard) operator adds a handler to the error continuation, which is separate from the user continuation.
When an error is signaled (via [fail](https://github.com/TurtleKitty/Vaquero/wiki/fail) or [error](https://github.com/TurtleKitty/Vaquero/wiki/error)), a handler can do one of three things: return a default value to the enclosing scope,
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
   (+ 2 3))

   -> 6

(guard handler
   (+ 3 (fail 'default)))

   -> 42

(guard handler
   (+ 3 (fail 'resume)))

   -> 72

(guard handler
   (+ 2 (fail 'crap)))

   -> ERROR aww-hell

; most system procs throw a more sophisticated error than a symbol

(proc show-it (e k)
   (list e.name e.form e.message))

(guard show-it
   (error 'wrong-way '(go left) "Left was a poor choice."))

   -> (wrong-way (go left) "Left was a poor choice.")


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
; prog.vaq

#!/usr/local/bin/vaquero run

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
If modules import one another, neither can rely on the other to finish evaluation.
Such leads to an infinite loop.

### Object capability security on operating system interfaces

The only global interfaces to the operating system are stdout, stdin, and stderr (and procedures which use them: read, write, print, say, and log).
All parts of a program, including modules, have access to these.
All other operating system services are contained in the [sys](https://github.com/TurtleKitty/Vaquero/wiki/sys) object, which is available only to the top-level program.
Libraries that wish to read command-line arguments, fork processes, or open files must be passed this capability from the top-level.

### Generic procedures with predicate dispatch

[gen](https://github.com/TurtleKitty/Vaquero/wiki/generic) and [spec](https://github.com/TurtleKitty/Vaquero/wiki/generic) allow the programmer to create generic procedures that dispatch based on arbitrary predicate expressions.  It's one crazy answer to the [expression problem](https://en.wikipedia.org/wiki/Expression_problem).

### Operators: compile-time procedures

Vaquero [operators](https://github.com/TurtleKitty/Vaquero/wiki/operator) use ordinary Vaquero code to transform source code at compile time.
This allows user-level creation of custom syntax.
An example of this power: the only primitive form of looping is tail recursion; all others (loop, for, while) are defined in terms of recursion via operators.
They can be found in interpreter/prelude.vaq.

### Reader literals for text construction and variable interpolation

Vaquero has a few convenient [reader literals](https://github.com/TurtleKitty/Vaquero/wiki/reader) inspired by Lisp and Perl.

### Reference

The [wiki](https://github.com/TurtleKitty/Vaquero/wiki) contains a more detailed reference of all core data types, operators, procedures, and objects.  

### Bugs

**use** and **import** don't work in the REPL.  As a work around, you can pass a stream to env.load.  

Syntax checking sometimes fails in the REPL.  I don't know why.  


