import depA.cl

class apah inherits ktest{

}

class Main inherits IO {
    main(): Object {
        let d1: ktest <- (new ktest).setK(2), d2: ktest <- (new ktest).setK(4) in {
            self.out_int(d1.getK()); -- Should print 2
            self.out_int(d2.getK()); -- Should print 4
            self.out_int(d1.getK()); -- Should print 2
            self.out_int((new apah).setK(5).getK()); 
        }
    };
};

