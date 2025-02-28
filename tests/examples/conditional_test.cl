class Main inherits IO{
    main(): Object {
        let d: Int <- 5 in
            self.out_int(self.getfive(d));
    };

    getfive(d: Int): Int{
        if d = 5 then 4+4/2 * self.get() else 0 fi;
    }

    get(): Int{
        100;
    }
}