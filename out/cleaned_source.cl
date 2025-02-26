

class Calculator {
    
    factorial(n: Int): Int {
        if n < 2 then 1 else n * self.factorial(n - 1) fi;
    };

    
    fib(n: Int): Int {
        if n < 2 then n else self.fib(n - 1) + self.fib(n - 2) fi;
    };

    
    power(base: Int, exp: Int): Int {
        if exp = 0 then 1 else base * self.power(base, exp - 1) fi;
    };

    
    gcd(a: Int, b: Int): Int {
        if a = b then a else
            if a < b then self.gcd(a, b - a)
            else self.gcd(a - b, b) fi
        fi;
    };

    
    sum_range(start: Int, end: Int): Int {
        let result: Int <- 0, i: Int <- start in {
            while i <= end loop ({
                result <- result + i;
                i <- i + 1
            }) pool;
            result;
        };
    };

    
    selectOperation(op: Int, a: Int, b: Int): Int {
        if op = 0 then a + b 
        else if op = 1 then a - b 
        else if op = 2 then a * b 
        else if op = 3 then (if b = 0 then 0 else a / b fi) 
        else 0  
        fi fi fi fi
    };


    
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
            self.out_string("Factorial of 5: ");
            self.out_int(calc.factorial(5));
            self.out_string("\n");

            self.out_string("Fibonacci of 10: ");
            self.out_int(calc.fib(10));
            self.out_string("\n");

            self.out_string("2 raised to 8: ");
            self.out_int(calc.power(2, 8));
            self.out_string("\n");

            self.out_string("GCD of 54 and 24: ");
            self.out_int(calc.gcd(54, 24));
            self.out_string("\n");

            self.out_string("Sum of numbers from 1 to 10: ");
            self.out_int(calc.sum_range(1, 10));
            self.out_string("\n");

            self.out_string("Select operation (addition) on 7 and 3: ");
            self.out_int(calc.selectOperation(0, 7, 3));  
            self.out_string("\n");

            self.out_string("Select operation (substraction) on 7 and 3: ");
            self.out_int(calc.selectOperation(1, 7, 3));  
            self.out_string("\n");

            self.out_string("Complex computation with x = 4 and y = 5: ");
            self.out_int(calc.computeExpression(4, 5));
            self.out_string("\n");

            self
        }
    };
};
