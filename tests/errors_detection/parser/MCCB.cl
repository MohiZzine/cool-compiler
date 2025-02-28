-- Missing Colon in a Case Branch

class Main {
  main() : Object {
    case 5 of
      x Int => x; 
    esac;
    self;
  };
};
