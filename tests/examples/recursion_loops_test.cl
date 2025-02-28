(* ComplexExample.cl
   A complex COOL program that demonstrates various computations.
*)

class Calculator {
    (* Compute factorial recursively. *)
    factorial(n: Int): Int {
        if n < 2 then 1 else n * self.factorial(n - 1) fi;
    };

    (* Compute Fibonacci numbers recursively. *)
    fib(n: Int): Int {
        if n < 2 then n else self.fib(n - 1) + self.fib(n - 2) fi;
    };

    (* Compute exponentiation recursively. *)
    power(base: Int, exp: Int): Int {
        if exp = 0 then 1 else base * self.power(base, exp - 1) fi;
    };

    (* Compute greatest common divisor using the subtraction method. *)
    gcd(a: Int, b: Int): Int {
        if a = b then a else
            if a < b then self.gcd(a, b - a)
            else self.gcd(a - b, b) fi
        fi;
    };

    (* Compute the sum of all integers from start to end using a loop. *)
    sum_range(start: Int, end: Int): Int {
        let result: Int <- 0, i: Int <- start in {
            while i <= end loop ({
                result <- result + i;
                i <- i + 1
            }) pool;
            result;
        };
    };

    (* Select an operation based on an integer code using if-else. *)
    selectOperation(op: Int, a: Int, b: Int): Int {
        if op = 0 then a + b 
        else if op = 1 then a - b 
        else if op = 2 then a * b 
        else if op = 3 then (if b = 0 then 0 else a / b fi) 
        else 0  (* Default case if `op` is invalid *)
        fi fi fi fi
    };


    (* Compute a complex mathematical expression. *)
    computeExpression(x: Int, y: Int): Int {
        let a: Int <- self.factorial(x) in {
            let b: Int <- self.fib(y) in {
                let c: Int <- self.power(x, y) in {
                    let d: Int <- self.gcd(a, b + 1) in {
                        a + (b * c) - d
                    }
                }
            }
        }
    };
};




class Main inherits IO {
    main(): SELF_TYPE {
        let calc: Calculator <- new Calculator in {
            out_string("Factorial of 5: ");
            out_int(calc.factorial(5));
            out_string("\n");

            out_string("Fibonacci of 10: ");
            out_int(calc.fib(10));
            out_string("\n");

            out_string("2 raised to 8: ");
            out_int(calc.power(2, 8));
            out_string("\n");

            out_string("GCD of 54 and 24: ");
            out_int(calc.gcd(54, 24));
            out_string("\n");

            out_string("Sum of numbers from 1 to 10: ");
            out_int(calc.sum_range(1, 10));
            out_string("\n");

            out_string("Select operation (addition) on 7 and 3: ");
            out_int(calc.selectOperation(0, 7, 3));  (* 0 indicates addition *)
            out_string("\n");

            out_string("Select operation (substraction) on 7 and 3: ");
            out_int(calc.selectOperation(1, 7, 3));  (* 1 indicates subs *)
            out_string("\n");

            out_string("Complex computation with x = 4 and y = 5: ");
            out_int(calc.computeExpression(4, 5));
            out_string("\n");

            self
        }
    };
};
