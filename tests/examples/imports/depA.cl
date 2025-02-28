class ktest inherits IO {
    k: Int <- 5; 
    l: String <- "ou";

    getK(): Int {
        k;
    };

    setK(new_k: Int): SELF_TYPE { 
        {   
            k <- new_k; 
            out_string(self.type_name());
            self;
        }
    };
};