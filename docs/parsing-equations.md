# Converting Infix Notation to Postfix Notation

## Introduction

In mathematical notation, expressions can be written in various formats, including infix, postfix, and prefix notations. Infix notation is the most common and intuitive (it's likely what you were taught), where operators are placed between operands (e.g., A + B). Postfix notation (also known as Reverse Polish Notation or RPN) places operators after their operands (e.g., A B +). Converting infix expressions to postfix notation can be useful for evaluation using stack-based algorithms. 

Even though Fortran is a strongly typed language, many applications could find it useful to dynamically set equations. For example, imagine having a single program to solve the heat equation but you can change the initial conditions by defining a function as a character string through the command line. FEQParse aims to provide such dynamic equation support wherein you can define equations in infix notation using the Fortran `character` type and evaluate the equation using floating point input.

FEQParse works by converting an infix expression into a postfix expression and running the evaluation using a stack-based algorithm. This page walks through sketches of the various algorithms that clean, parse & tokenize, convert, and evaluate equations in FEQParse.

## Algorithm overview
The `EquationParser` data type has a constructor routine and evaluation routines that make it easy to create equations that can be evaluated

```
program example

use feqparse
use iso_fortran_env
implicit none

type(EquationParser) :: eq
real(real32) :: xscalar, fscalar
real(real32) :: xarray(1:100), farray(1:100)

eq = EquationParser( 'f=x^2', ['x'] )

! Example evaluating with a scalar
xscalar = 1.0_real32
fscalar = eq % Evaluate(xscalar)

! Example evaluating with an array
xarray(1:100) = 1.0_real32
farray(1:100) = eq % Evaluate(xarray)

end program example
```

There are clearly two main algorithms at play here for (1) construction and (2) evaluation of the parser. Construction involves

1. Cleaning the input equation 
2. Tokenizing the infix equation
3. Conversion from infix notation to postfix notation

After tokenizing and converting, both the infix and postfix expressions are store in a stack of tokens. The postfix expression is used for equation evaluation using a stack-based algorithm, where intermediate results are processed through a stack; the type of data stored in the stack depends on the type of data passed as input for evalation (e.g. scalar float32, 4-d array float 64, etc.)

In case you're not familiar, a stack is a data structure that operates on the Last-In-First-Out (LIFO) principle, where the most recently added element is the first to be removed. It is typically implemented as a class with methods for common stack operations such as push (to add an element to the top), pop (to remove and return the top element), peek or top (to view the top element without removing it), and often additional methods like isEmpty (to check if the stack is empty) and size (to get the number of elements in the stack).

### Cleaning the equation
Before stepping into tokenizing, parsing, and converting to postfix notation for evaluation, an input `CHARACTER` that contains an equation in infix notation 
is preprocessed. We need to clean and format the equation, ensuring it is in a suitable form for subsequent operations.

The algorithm for the FEQParse `EquationParser` `CleanEquation` type-bound procedure is broken down as follows

1. **Identify Dependent Variable Name**:
   - Locate the position of the equal sign (`=`) in the equation.
   - If an equal sign is found, the part of the equation before the equal sign is considered the variable name and stored in `parser%variableName`.

2. **Replace Power Operator**:
   - Replaces occurrences of the exponentiation operator `**` with the caret symbol `^` in the equation using the `ReplaceStr` function. This is done to allow using either `**` or `^` for exponentiation.

3. **Extract Infix Formula**:
   - Extracts the part of the equation to the right of the equal sign, which represents the infix formula.
   - Left adjusts this extracted formula for further processing.

4. **Remove Spaces**:
   - Iterates over the extracted infix formula and removes any spaces, effectively compacting the formula.
   - This is done by copying non-space characters to a new position in the string and adjusting the string length accordingly.

5. **Finalization**:
   - Pads the remaining part of `parser%inFixFormula` with spaces to ensure there are no leftover characters.
   - Sets the `equationCleaned` flag to `.true.` if the cleaning process completes successfully.


### Tokenizing the infix formula
After the equation is cleaned and we are certain we have a valid equation in infix notation, we now "tokenize" the equation. Essentially, this process is used to break down an infix expression into individual tokens and classify each token as a function, variable, number, operator, monad, or parentheses. 

The algorithm for the FEQParse `EquationParser` `Tokenize` type-bound procedure is broken down as follows

1. **Initialization**:
   - The subroutine initializes the `tokenized` flag to `.false.`.
   - Set up a token stack and calculates the maximum length of variable names from our list of independent variables

2. **Tokenization Loop**:
   - The main loop processes the input infix formula one character at a time.
   - For each character (or sequence of characters), the subroutine determines the type of token (function, variable, number, separator, or operator).

3. **Token Identification**: Each token is assigned a type and a string.
   - **Function**: Check if the current substring represents a function. If so, it adds the function token to the stack.
   - **Variable**: Check if the current substring matches any of the independent variables. If it does, it adds the variable token to the stack.
   - **Number**: Check if the current substring is a number. If so, it adds the number token to the stack, handling special cases like the constant `pi`.
   - **Separator**: Identify and process separators (parentheses and operators), adding the appropriate token to the stack.

4. **Error Handling**:
   - The subroutine checks for missing operators or closing parentheses after variables and numbers. If an error is found (such as missing a closing parentheses), print an `errorMsg` and exits.
   - If an invalid token is encountered (we cannot classify it), print an `errorMsg` and exits.

5. **Special Case Handling**:
   - Adjusts the token type for unary operators (monadic tokens), which can appear at the beginning of the expression or immediately after an opening parenthesis. Unary operators, or monadic tokens are typically either `+` or `-` that indicate the sign of a number of variable, e.g. in the infix equation `f = -2*x`, the `-` is a monadic token.

6. **Finalization**:
   - Sets the `tokenized` flag to `.true.` if the tokenization process completes successfully.

### Conversion from infix to postfix

The conversion from infix to postfix notation involves the use of a stack to temporarily hold operators and ensure that the final postfix expression maintains the correct order of operations. The following steps outline the conversion algorithm:

1. **Initialize Structures**:
   - Create an empty stack to hold operators.
   - Create an empty list to output the postfix expression.

2. **Process Each Token**:
   - Read the infix expression from left to right, one token (operand or operator) at a time.
   - Depending on the type of token, perform the following actions:

     **a. Operand**:
        - Directly append operands (numbers, variables) to the output list.

     **b. Left Parenthesis (`(`)**:
        - Push the left parenthesis onto the stack.

     **c. Right Parenthesis (`)`)**:
        - Pop operators from the stack and append them to the output list until a left parenthesis is encountered on the stack.
        - Discard the left parenthesis.

     **d. Operator (e.g., `+`, `-`, `*`, `/`)**:
        - While the stack is not empty and the operator at the top of the stack has higher or equal precedence than the current operator, pop the operator from the stack and append it to the output list.
        - Push the current operator onto the stack.

     **e. Monadic Token**:

     **f. Functions (e.g `sin`, `cos`, `log`, etc.)**:
        - Push the function onto the stack

3. **Empty the Stack**:
   - After processing all tokens, pop any remaining operators from the stack and append them to the output list.

#### Operator Precedence and Associativity

Operators have different levels of precedence which affect the order in which operations are performed:
- Multiplication (`*`), division (`/`), and modulus (`%`) have higher precedence than addition (`+`) and subtraction (`-`).
- Operators of equal precedence are evaluated based on their associativity (left-to-right or right-to-left).

In `FEQParse.F90`, the `Priority` function uses the `token % tokenType` and `token % tokenString` to determin the priority for evaluation.

#### Example Conversion

Consider the infix expression: `A + B * (C - D)`

1. Initialize the stack and output list:
   - Stack: `[]`
   - Output: `[]`

2. Process each token:
   - `A`: Append to output. Output: `[A]`
   - `+`: Push onto stack. Stack: `[+]`
   - `B`: Append to output. Output: `[A, B]`
   - `*`: Push onto stack (higher precedence than `+`). Stack: `[+, *]`
   - `(`: Push onto stack. Stack: `[+, *, (]`
   - `C`: Append to output. Output: `[A, B, C]`
   - `-`: Push onto stack. Stack: `[+, *, (, -]`
   - `D`: Append to output. Output: `[A, B, C, D]`
   - `)`: Pop and append until `(` is found. Stack: `[+, *]`, Output: `[A, B, C, D, -]`
   - Pop `*` from stack (higher precedence than `+`). Stack: `[+]`, Output: `[A, B, C, D, -, *]`
   - Pop `+` from stack. Stack: `[]`, Output: `[A, B, C, D, -, *, +]`

3. Final postfix expression:
   - `A B C D - * +`


### Evaluation

There are multiple functions for evaluation of a parser. All of them follow the same algorithm and they are distinguished only by the type of data used to define the independent variables, which also matches the resulting output. For example, FEQParse provides evaluation routines for scalar `real32`, scalar `real64`, 1-d array `real32`, etc. 

Below is a rough breakdown of the evaluation algorithm:

1. **Initialization**:
   - Construct a stack for evaluation and storing intermediate results

2. **Evaluation Loop**:
   - Iterate through each token in the postfix expression (stored in `parser%postfix%tokens`).
   - Depending on the type of each token (number, variable, operator, function, or monadic operator), perform specific actions using the stack.

3. **Token Processing**:
   - **Number Token**: 
     - Numbers are pushed onto the stack. Special numbers, like `pi` are allowed to be used, so we check for these, also pushing onto the stack.
   - **Variable Token**: 
     - Match the variable name with the independent variables from the input and pushes the corresponding value onto the stack.
   - **Operator Token**: 
     - When we encounter an operator (e.g `+`, `-`, `*`, `/`,`^`), pop the last two operands (last one is `b` and second to last is `a`) from the stack and apply the operation `a op b`.
     - Push the result back onto the stack.
   - **Function Token**: 
     - Pop the operand from the stack, applies the function (like sin, cos, etc.), and push the result back onto the stack.
   - **Monadic Token**: 
     - Handles unary operations (like negation), pop the operand, applies the operation, and pushes the result back onto the stack.

4. **Final Result**:
   - After processing all tokens, the final result is popped from the stack and returned as the function result `f`.
