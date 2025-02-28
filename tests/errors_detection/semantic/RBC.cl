-- Redefinition of a Basic Class

class Object {
  main() : Object { self };
};

class Main{
    main(): Object{
        self;
    }
}