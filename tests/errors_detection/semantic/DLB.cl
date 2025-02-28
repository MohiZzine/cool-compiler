-- Duplicate Let Binding

class Main {
  main() : Object {
    let x : Int <- 5, x : Int <- 10 in x; 
  };
};
