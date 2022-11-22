Sire
====

This is a quick reference for Sire, a macro-assembler for the Fan
evaluator.  Sire is essentially just the lambda calculus with a macro
system and support for function inlining.


Table of Runes
--------------

### Commands

-   `=` bind

    ```
    x=3                  ;;  Bind a value

    (f x)=x              ;;  Bind a pinned fun

    f=(mkPin (f x ? x))  ;;  Same as the above

    "**"=tar_tar         ;;  Binds the `**` rune to a macro
    ```

-   `<` print entire closure

    ```
    REPL> add
    (add a b)=(exec:3 toNat-a b)

    REPL> <add
    (exec a b c)=(2 b (exec a a-b) c)
    (toNat a)=(2:0:3 a)
    (add a b)=(exec:3 toNat-a b)
    ```


-   `??` assert (expression should evaluate to `1`)

    ```
    ?? (eql 2 (add 1 1))
    ?? eql 2 (add 1 1)
    ```

-   `/+` import a module

    ```
    /+  str   [strCat strTake strToRow]
    /+  json
    ```

-   `^-^` filter environment (all bindings not listed are unbound)

     This mainly is used to control the "export list" of a module.

     ```
     x=3
     y=4

     ^-^ x

     y ;; undefined
     ```

### Expressions

-   Variable references:

    ```
    x     ;; Variable "x"
    $"x"  ;; Variable "x" (useful to refer to runes and other bindings
          ;;               that have non-symbol names)
    ```

-   Nat literals (all of these are the same value)

    ```
    7303014
    %foo
    "foo"
    'foo'
    """foo
    ```

-   Function Application

    ```
    (f x y)    ;;  short-hand for (| f x)
    (| f x y)  ;;  Same as the above
    (- f x y)  ;;  Same as the above, but using the equivalent `-` rune.
    f-x-y      ;;  Same as the above, but written infix.
    (. x y f)  ;;  Same as the above.
    ```

    `.` is mostly useful for loops and similar:

    ```
    . NIL args
    ? (loop acc remain)
    | if (null remain) acc
    | loop
      (CONS (listHead remain) acc)
      (listTail remain)
    ```

-   Function Inlining

    ```
    (! f x y)
    ```

    Here, if `f` is a top-level function binding that takes two arguments,
    then the funcion body will be inlined into the call-site.

-   Lambdas: `&` is anonymous, and `?` is named.

    ```
    (map (x & add 2 x) ~[3 4 5])

    . NIL args
    ? (loop acc remain)
    | if (null remain) acc
    | loop
      (CONS (listHead remain) acc)
      (listTail remain)
    ```

    anonymous functions (`&` rune) will have a `funName` of 0, and named
    functions (`?` rune) will have their name encoded in the underlying
    `funName`.

-   Let binding

    ```
    @ x 3
    @ x (add 2 x)
    @ x (add 4 x)
    x
    ```

-   Recursive let binding.

    ```
    @@ infiniteList (1 infiniteList)
     | eql 1 (car infiniteList)
    ```

-   Where clauses (reversed let binding)

    ```
    ^ eql x (0 9)
    = y 9
    = x (0 y)
    ```

-   Map and set literals (these will soon be implemented as macros and
    removed from the language):

    ```
    %[3 4 5]
    %[3="three" 4="four"]
    ```
