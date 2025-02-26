package semantic

import "cool-compiler/ast"

// addBasicClasses manually registers the 5 basic classes in the global table.
// This ensures they appear in the symbol table *before* user-defined classes.
func AddBasicClasses(globalTable *SymbolTable) {
    // 1) OBJECT
    objectScope := NewSymbolTable(globalTable)
    // Methods: abort() : Object, type_name() : String, copy() : SELF_TYPE
    objectScope.AddEntry("abort", SymbolEntry{
        Type: "Object",
        Method: &ast.Method{
            Name:       "abort",
            ReturnType: "Object",
            // No formal params, empty body or nil
        },
        Scope: NewSymbolTable(objectScope), // method scope
    })
    objectScope.AddEntry("type_name", SymbolEntry{
        Type: "String",
        Method: &ast.Method{
            Name:       "type_name",
            ReturnType: "String",
        },
        Scope: NewSymbolTable(objectScope),
    })
    objectScope.AddEntry("copy", SymbolEntry{
        Type: "SELF_TYPE",
        Method: &ast.Method{
            Name:       "copy",
            ReturnType: "SELF_TYPE",
        },
        Scope: NewSymbolTable(objectScope),
    })
    globalTable.AddEntry("Object", SymbolEntry{
        Type:  "class",
        Scope: objectScope,
    })

    // 2) IO (inherits from Object)
    ioScope := NewSymbolTable(globalTable)
    // We treat parent as "Object" - your code already handles inheritance
    ioEntry := SymbolEntry{
        Type:   "class",
        Scope:  ioScope,
        Parent: "Object",
    }
    // Add the four required methods:
    ioScope.AddEntry("out_string", SymbolEntry{
        Type: "SELF_TYPE",
        Method: &ast.Method{
            Name:       "out_string",
            Parameters: []ast.Param{{Name: "x", Type: "String"}},
            ReturnType: "SELF_TYPE",
        },
        Scope: NewSymbolTable(ioScope),
    })
    ioScope.AddEntry("out_int", SymbolEntry{
        Type: "SELF_TYPE",
        Method: &ast.Method{
            Name:       "out_int",
            Parameters: []ast.Param{{Name: "x", Type: "Int"}},
            ReturnType: "SELF_TYPE",
        },
        Scope: NewSymbolTable(ioScope),
    })
    ioScope.AddEntry("in_string", SymbolEntry{
        Type: "String",
        Method: &ast.Method{
            Name:       "in_string",
            ReturnType: "String",
        },
        Scope: NewSymbolTable(ioScope),
    })
    ioScope.AddEntry("in_int", SymbolEntry{
        Type: "Int",
        Method: &ast.Method{
            Name:       "in_int",
            ReturnType: "Int",
        },
        Scope: NewSymbolTable(ioScope),
    })
    globalTable.AddEntry("IO", ioEntry)

    // 3) Int
    intScope := NewSymbolTable(globalTable)
    globalTable.AddEntry("Int", SymbolEntry{
        Type:  "class",
        Scope: intScope,
        Parent: "Object",
    })

    // 4) String
    strScope := NewSymbolTable(globalTable)
    strScope.AddEntry("length", SymbolEntry{
        Type: "Int",
        Method: &ast.Method{
            Name:       "length",
            ReturnType: "Int",
        },
        Scope: NewSymbolTable(strScope),
    })
    strScope.AddEntry("concat", SymbolEntry{
        Type: "String",
        Method: &ast.Method{
            Name:       "concat",
            Parameters: []ast.Param{{Name: "s", Type: "String"}},
            ReturnType: "String",
        },
        Scope: NewSymbolTable(strScope),
    })
    strScope.AddEntry("substr", SymbolEntry{
        Type: "String",
        Method: &ast.Method{
            Name:       "substr",
            Parameters: []ast.Param{
                {Name: "i", Type: "Int"},
                {Name: "l", Type: "Int"},
            },
            ReturnType: "String",
        },
        Scope: NewSymbolTable(strScope),
    })
    globalTable.AddEntry("String", SymbolEntry{
        Type:   "class",
        Scope:  strScope,
        Parent: "Object",
    })

    // 5) Bool
    boolScope := NewSymbolTable(globalTable)
    globalTable.AddEntry("Bool", SymbolEntry{
        Type:   "class",
        Scope:  boolScope,
        Parent: "Object",
    })
}

