(*Received an extension to resubmit*)
(*A.1*)
(*
   1. - : int = 10
   2. - : float = 10.
   3. - : int = 12
   4. Error: This expression has type float but an expression was expected of type int. 
      This error occured because we are adding floats hence we need to use the +. 
      operator instead of the int operator +
   5. Error: This expression has type int but an expression was expected of type float.
      This error occured because we are adding integers hence we need to use the + 
      operator instead of the float operator +.
   6. Error: This expression has type float but an expression was expected of type int.
      This error occured because we are adding an integer to a float and using the int + 
      which we cannot do as we can only add the same types.
   7. Error: This expression has type int but an expression was expected of type float.
      This error occured because we are adding an integer to a float and using the float +. 
      which we cannot do as we can only add the same types.
   8. - : float = 7.2
   9. - : int = 5
   10. - : int = 7
   11. val a : int = 3
   12. val b : int = 4
   13. - : bool = false
   14. - : bool = true
   15. - : bool = false. This is different because this is checking their physical equality 
      hence their addresses in memory which are not equal in this case.
   16. - : (int * int * int) list = [(1, 2, 3)]
   17. - : (int * int * int) list = [(1, 2, 3)] 
      OCaml has interepreted the commas as meaning a tuple, so it thinks I want a list 
      with a single element which is a 3-tuple. You should use ; when defining lists.
   18. - : int = 4
   19. Error: Syntax error. This is because the correct syntax for and is &&
   20. - : int = 6
   21. - : int = 4 . This is different because here the + 2 is part of the else statement.
      so it is not being added to the result of the conditional.
   22. - : int = 6
   23. Error: This expression has type int but an expression was expected of type unit
       because it is in the result of a conditional with no else branch. This is a type
       error because the else was ommited hence it assumes the returns a type unit which
       conflicts with the if return type which is an int. 
*)

(*A.2*)
let sum_of_squares_of_two_largest a b c =
  if a > c && b > c then a * a + b * b
  else if c > a && b > a then b * b + c * c
  else a * a + c * c

(*A.3*)
(*
  This function takes in two integers,a and b, and decides whether to add b to 
  a or subtract b from a. If b > 0 then it returns the add expression which
  adds a and b else if b <= 0 then it subtracts b from a which essentially
  adds th absolute value of b to a as b is negative and negative times negative is a + hence this
  function as the name implies simply does a + |b|.
*)

(*B.1*)
(*
  In applicative order evaluation, operands are evaluted before the operator is applied
  to them hence for test then the operand zero is evaluated, then p () is evaluted. 
  However, this result will never evalute because there is an infinite recursion here.
  When he runs it with normal order evaluation, it evaluates on a need-to-know basis
  hence non-primative operators are substituted for compound procedures until 
  only primative operators remain before evalitoon occurs meaning that in test it will 
  evaluate 0 = 0 and retunr zero hence not erroring as it never evaluates p ()
  hence avoiding infinite recurssion.
*)
(*B.2*)
(*
  She will encounter an error because application order evalutes operands before
  operators hence it evaluates all operands of new_if first resulting in a 
  recursive loop that will infinitely run (sqrt_iter (improve guess x) x)
  because new_if cannot stop to return guess like if then else would.
*)
(*B.3*)
(*
  1. add_a is a recursive while add_b is an iterative process.
  2. 
  let rec add_a a b =
    if a = 0 
    then b 
    else inc (add_a (dec a) b)
Desugar:
let rec add_a =
    fun a b -> if a = 0 
                  then b 
                  else inc (add_a (dec a) b)
Bind "add_a" name to:
fun a b -> if a = 0 then b else inc (add_a (dec a) b)
Evaluate: add_a 2 5
* evaluate 2 -> 2
* evaluate 5 -> 5
* evaluate add_a -> fun a b -> (...)
apply (fun a b -> (...)) to 2 5 ->
    substitute 2 for a and  5 for b in if a = 0 then b else inc (add_a (dec a) b) ->
        if 2 = 0 then 5 else inc (add_a (dec 2) 5)
    evaluate: if 2 = 0 then 5 else inc (add_a (dec 2) 5)
       evaluate if 2 = 0
            * evaluate 2 -> 2
            * evaluate 0 -> 0
            * evaluate = operator -> (primitive function =)
            apply = operator to 2 0 -> false
        For the false case (the else) evaluate inc (add_a (dec 2) 5)
            * evaluate add_a (dec 2) 5 ->
                * evaluate dec 2 ->
                    * evaluate 2 -> 2
                    * evaluate dec -> (primitive function dec)
                    apply dec func to 2 -> 1
                * evaluate 5 -> 5
                * evaluate add_a -> fun a b -> (...)
                apply (fun a b -> ...) to 1 5
                    substitute 1 for a, 5 for b in if a = 0 then b else inc (add_a (dec a) b) ->
                        if 1 = 0 then 5 else inc (add_a (dec 1) 5)
                    evaluate: if 1 = 0 then 5 else inc (add_a (dec 1) 5)
                        Special form if: evaluate 1 = 0
                            * evaluate 1 -> 1
                            * evaluate 0 -> 0
                            * evaluate = -> [primitive function =]
                            apply = on 1 0 -> false
                        For false case (else),: evaluate inc (add_a (dec 1) 5)
                            * evaluate add_a (dec 1) 5 ->
                                * evaluate dec 1 ->
                                    * evaluate 1 -> 1
                                    * evaluate dec -> primitive function dec
                                    apply dec to 1 -> 0
                                * evaluate 5 -> 5
                                * evaluate add_a -> fun a b -> (...)
                                apply (fun a b -> (...)) on 0 5
                                    substitute 0 for a, 5 for b in the if a = 0 then b else inc (add_a (dec a) b) ->
                                        if 0 = 0 then 5 else inc (add_a (dec 0) 5)
                                    evaluate: if 0 = 0 then 5 else inc (add_a (dec 0) 5)
                                        Special form if: evaluate 0 = 0
                                            * evaluate 0 -> 0
                                            * evaluate 0 -> 0
                                            * evaluate = -> primitive function =
                                            apply = to 0 and 0 -> true
                                        For true case (then), evaluate 5 -> 5
                            * evaluate inc -> primitive function inc
                            apply inc to 5 -> 6      
            * evaluate inc -> primitive function inc
            apply inc to 6 -> 7
Therefore our result: 7

3.
Substitution:

let rec add_b a b =
    if a = 0 then b else add_b (dec a) (inc b)

Desugar this to:

let rec add_b =
    fun a b -> if a = 0 then b else add_b (dec a) (inc b)

Bind the name "add_b" to the value:

fun a b -> if a = 0 then b else add_b (dec a) (inc b)

Evaluate (add_b 2 5)
    >>> evaluate 2 -> 2
    >>> evaluate 5 -> 5
    >>> evaluate add_b -> fun a b -> ...
    apply (fun a b -> if ...) to 2, 5
    substitute 2 for a, 5 for b in (if ...)
        -> if 2 = 0 then 5 else add_b (dec 2) (inc 5)
    evaluate (if 2 = 0 then 5 else add_b (dec 2) (inc 5))
        if is a special form, so evaluate the first operand:
            evaluate (2 = 0)
                >>> evaluate 2 -> 2
                >>> evaluate 0 -> 0
                >>> evaluate = -> primitive function =
                apply = to 2, 0 -> false
        first argument of if is false, so evaluate the third operand:
            evaluate (add_b (dec 2) (inc 5))
                evaluate (dec 2)
                    >>> evaluate 2 -> 2
                    >>> evaluate dec -> primitive function dec
                    apply dec to 2 -> 1
                evaluate (inc 5)
                    >>> evaluate 5 -> 5
                    >>> evaluate inc -> primitive function inc
                    apply inc to 5 -> 6
                >>> evaluate add_b -> fun a b -> ...
                apply (fun a b -> if ...) to 1, 6
                substitute 1 for a, 6 for b in (if ...)
                    -> if 1 = 0 then 6 else add_b (dec 1) (inc 6)
                evaluate (if 1 = 0 then 6 else add_b (dec 1) (inc 6))
                    if is a special form, so evaluate the first operand:
                        evaluate (1 = 0)
                            >>> evaluate 1 -> 1
                            >>> evaluate 0 -> 0
                            >>> evaluate = -> primitive function =
                            apply = to 1, 0 -> false
                    first argument of if is false, so evaluate the third operand:
                        evaluate (add_b (dec 1) (inc 6))
                            evaluate (dec 1)
                                >>> evaluate 1 -> 1
                                >>> evaluate dec -> primitive function dec
                                apply dec to 1 -> 0
                            evaluate (inc 6)
                                >>> evaluate 6 -> 6
                                >>> evaluate inc -> primitive function inc
                                apply inc to 6 -> 7
                            apply (fun a b -> if ...) to 0, 7
                            substitute 0 for a, 7 for b in (if ...)
                                -> if 0 = 0 then 7 else add_b (dec 0) (inc 7)
                            evaluate (if 0 = 0 then 7 else add_b (dec 0) (inc 7))
                                if is a special form, so evaluate the first operand:
                                    evaluate (0 = 0)
                                        >>> evaluate 0 -> 0
                                        >>> evaluate = -> primitive function =
                                        apply = to 0, 0 -> true
                                first argument of if is true, so evaluate the second operand:
                                    >>> evaluate 7 -> 7
                                    result: 7
*)
(*C.1*)
let rec factorial n =
   if n = 0 then 1 else n * factorial (n - 1)

let e_term (n: int) = 
   1. /. float_of_int(factorial n) 

let rec e_approximation (n: int) = 
   if n = 0 then 1. else
      (e_term n) +. e_approximation(n - 1)

(*
   utop # e_approximation 20;;
   - : float = 2.71828182845904553
   utop # exp 1.0
   ;;
   - : float = 2.71828182845904509
*)

(*
   utop # e_approximation 100;;
   - : float = infinity
   This happens as a result of overflowing the the float because the precision of float
   types is limited by the amount of bite space the data type has and once we 
   overflow that then we get infinity added to some number resulting in infinity.
   
*)

(*C.2*)
let rec is_even (n: int) = if n = 0 then true else
   is_odd(n - 1)
   and is_odd n = if n = 0 then false else is_even(n - 1)

(*C.3*)
let rec f_rec (n: int) = if n < 3 then n else
   f_rec(n - 1) + (2 * f_rec(n - 2)) + (3 * f_rec(n - 3))

let f_iter (n: int) = if n < 3 then n else
   let rec iter1 m a  b c = 
      if m != n then iter1 (m + 1) (a + (2 * b) + (3 * c)) a b else
         a + 2 * b + 3 * c
   in iter1 3 2 1 0

(*C.4*)
let rec pascal_coefficient (row: int) (idx: int) = 
   match (row, idx) with
   |(1, _) -> 1
   |(_, 1) -> 1
   |(r, i) when r = i -> 1
   |(r, i) when r < 1 || i < 1 || i > r -> failwith "invalid arguments"
   |(r, i) -> (pascal_coefficient (r - 1) (i-1)) + (pascal_coefficient(r - 1) i)
