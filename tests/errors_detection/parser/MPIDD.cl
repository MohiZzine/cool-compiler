-- Missing Parentheses in Dynamic Dispatch
class Main {
  main() : Object {
    let x : Int <- 5 in
      x.foo;  
    self;
  };
  foo(): Object{
    self;
  }
};
