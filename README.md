clojure-quirk supports two commands:
    lein run < example1.q          - Evaluates the .q file in clojure.
    lein run -pt < example1.q      - Prints the parse tree and evalutes the .q file in clojure

clojure-quirk supports all parts of the quirk grammar and can successfully run
all the examples.

It uses multiple helper functions and global data scopes.
Helper Functions:
    parse-number   - Evaluates the number and returns the appropriate data type (int, float, double)
    CallByLabel    - Evaluates a string and calls the corresponding function along with its arguments
    interpret-quirk- Checks to see that the tokens are correct and runs Program
Helper Global Data Scopes:
    They all use clojure's atom function for getting and setting
    store          - A Global scope used for holding variables and functions
    mapa           - Used to map index value with a variable or function name
    counter        - Counts every time a function is called
    varcounter     - Counts how many variables are declared in a function
    returncounter  - Counts how many return variable are in a function

My interpretation of the program utilizes string evaluations to remedy
a variety of issues from data type mismatch to operations to declaring functions.
I realized that clojure was very specific about data types so an easy solution I figured
was always rewrite everything as a string that matches clojure's syntax,
and at each level simply evaluate the expression to get the correct operation.

For function declaration, it builds out a clojure version of a function after retrieving
the function name, variables, body, and the returns. After running string evaluation,
this function now exists in memory.

For assignments, I used clojure's atom function to map the variable name to the expression in
a global data store. If the variable is not currently assigned or if it is in
a function, it would simply return that value instead. If the variable is assigned and
not under a function, it would return that value by running a get command on the global store.

For multiple function returns, it uses the returncounter, mapa, and the store.
Every time another return is specified, the returncounter increases. This value and
the variable name is then mapped to mapa for indexing. The variable name and the return operation/value
is then mapped to the global store. When retrieving the specific index of a multiple return,
it checks the corresponding value name in mapa with the index requested, then
checks the corresponding return operation/value in the global store. It then uses the
passed in parameters and the returned operation and runs an anonymous function.
This way, the original function declaration still persists.