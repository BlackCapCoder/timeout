_A hyper turing complete programming language for regular turing machines!_


# Timeout
Timeout is a hyper turing complete programming language that supports time travel. It is best compiled on a machine in orbit around a black hole; preferably a small one if you're doing it here on earth.

By using this code you agree that I will not be held responsible if you break your universe in a time travel paradox, is swallowed by a black hole, or is annihilated by anti-matter radiation. Please take proper safety precautions if you are made of regular matter, or is a four-dimensional being.

If you want to support timeout development, please deposit a penny with my name on it in some bank, sometime before the end of the universe. Thank you.


# Statements
* `SEND [VAR] TO [Expr] BEFORE/AFTER [[Expr]:][EXPR]` Send a variable to N units of time before/after the Nth time some expression held true. Must be a point in time after the execution- and before the termination of the program, if applicable
* `IN  [VAR]`  Take numeric input
* `OUT [EXPR]` Send the result of the expression to the output
* `SET [VAR] TO [EXPR]` Store the result of the expression in a variable
* `IF [EXPR] THEN [STATEMENT]` Evaluate the statement if the expression does not evaluate to 0
* `LABEL [NAME]` Marks a location in the code
* `GOTO [LABEL]` Jumps to a label
* `HALT` Terminate the program

# Expressions
* `[EXPR] +/-/*///^/% [EXPR]` Arithmetic
* `[EXPR] =/!=/>/</<=/>= [EXPR]` Equality test and comparison, 0 if false
* `[INT]` An integer
* `[VAR]` The value of a variable
* `(/)` Use parentheses for precedence

Parentheses can also be used to group multiple statements, delimited by `;`. All statements can optionally be terminated by `;`.

# Example programs

Print 1 and halt:
```
SET x TO 0;
OUT x
IF x=1 THEN HALT;
SET x TO 1;
SEND x TO 0 AFTER x=0;
```

Print the sum of two numbers, then take them as input:
```
SET a TO 1
OUT z
IN x; IN y
SET z TO x+y
SEND z TO 0 AFTER a=1
```
