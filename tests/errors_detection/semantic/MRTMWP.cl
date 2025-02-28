-- Method Return Type Mismatch with Parent

class Parent {
  foo() : Int { 1 };
};

class Child inherits Parent {
  foo() : String { "oops" }; 
  main() : Object { self };
};

class Main{
    main(): Object{
        self;
    }
}
