class Main inherits IO {
    main(): Object {
        let user_input: String <- self.in_string() in{
            self.out_string("You entered: ") ;
            self.out_string(user_input);
            self.apah(1);
        }
    };

    apah(a: Int): SELF_TYPE{
        {
            self.out_int(a);
            self;
        };
    }
};
