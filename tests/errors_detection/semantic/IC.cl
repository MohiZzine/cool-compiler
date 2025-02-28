-- Inheritance Cycle

class A inherits B {
  main() : Object { self };
};

class B inherits A {
  main() : Object { self };
};

class Main{
    main(): Object{
        self;
    }
}