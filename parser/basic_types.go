package parser

import "cool-compiler/ast"

func InjectBasicClasses() []ast.ClassDecl {
    // Object
    objectClass := ast.ClassDecl{
        Name:   "Object",
        Parent: "", // no parent
        Methods: []ast.Method{
            {
                Name:       "abort",
                Parameters: nil,
                ReturnType: "Object",
                Body:       &ast.BlockExpr{}, // empty body
            },
            {
                Name:       "type_name",
                Parameters: nil,
                ReturnType: "String",
                Body:       &ast.BlockExpr{},
            },
            {
                Name:       "copy",
                Parameters: nil,
                ReturnType: "SELF_TYPE",
                Body:       &ast.BlockExpr{},
            },
        },
        Attributes: nil,
    }

    // IO
    ioClass := ast.ClassDecl{
        Name:   "IO",
        Parent: "Object",
        Methods: []ast.Method{
            {
                Name:       "out_string",
                Parameters: []ast.Param{{Name: "x", Type: "String"}},
                ReturnType: "SELF_TYPE",
                Body:       &ast.BlockExpr{}, 
            },
            {
                Name:       "out_int",
                Parameters: []ast.Param{{Name: "x", Type: "Int"}},
                ReturnType: "SELF_TYPE",
                Body:       &ast.BlockExpr{},
            },
            {
                Name:       "in_string",
                Parameters: nil,
                ReturnType: "String",
                Body:       &ast.BlockExpr{},
            },
            {
                Name:       "in_int",
                Parameters: nil,
                ReturnType: "Int",
                Body:       &ast.BlockExpr{},
            },
        },
        Attributes: nil,
    }

    // Int
    intClass := ast.ClassDecl{
        Name:       "Int",
        Parent:     "Object",
        Methods:    nil, // no special methods
        Attributes: nil,
    }

    // String
    stringClass := ast.ClassDecl{
        Name:   "String",
        Parent: "Object",
        Methods: []ast.Method{
            {
                Name:       "length",
                Parameters: nil,
                ReturnType: "Int",
                Body:       &ast.BlockExpr{},
            },
            {
                Name:       "concat",
                Parameters: []ast.Param{{Name: "s", Type: "String"}},
                ReturnType: "String",
                Body:       &ast.BlockExpr{},
            },
            {
                Name: "substr",
                Parameters: []ast.Param{
                    {Name: "i", Type: "Int"},
                    {Name: "l", Type: "Int"},
                },
                ReturnType: "String",
                Body:       &ast.BlockExpr{},
            },
        },
        Attributes: nil,
    }

    // Bool
    boolClass := ast.ClassDecl{
        Name:       "Bool",
        Parent:     "Object",
        Methods:    nil,
        Attributes: nil,
    }

    return []ast.ClassDecl{objectClass, ioClass, intClass, stringClass, boolClass}
}