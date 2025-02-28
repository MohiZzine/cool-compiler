-- Missing Parentheses in Static Dispatch

class Main {
  main() : Object {
    (new Main)@Main.foo; 
    self;
  };
};
