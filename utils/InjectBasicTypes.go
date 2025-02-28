package utils

import (
	"cool-compiler/structures"
)

// InjectBasicClassesAST returns a slice of basic class declarations for the Cool compiler.
// The returned classes include Object, IO, Int, String, and Bool.
func InjectBasicClassesAST() []structures.ClassDecl {
	objectClass := structures.ClassDecl{
		Name:   "Object",
		Parent: "",
		Methods: []structures.Method{
			{
				Name:       "abort",
				Parameters: nil,
				ReturnType: "Object",
				Body:       &structures.BlockExpr{},
			},
			{
				Name:       "type_name",
				Parameters: nil,
				ReturnType: "String",
				Body:       &structures.BlockExpr{},
			},
			{
				Name:       "copy",
				Parameters: nil,
				ReturnType: "SELF_TYPE",
				Body:       &structures.BlockExpr{},
			},
		},
		Attributes: nil,
	}

	ioClass := structures.ClassDecl{
		Name:   "IO",
		Parent: "Object",
		Methods: []structures.Method{
			{
				Name:       "out_string",
				Parameters: []structures.Param{{Name: "x", Type: "String"}},
				ReturnType: "SELF_TYPE",
				Body:       &structures.BlockExpr{},
			},
			{
				Name:       "out_int",
				Parameters: []structures.Param{{Name: "x", Type: "Int"}},
				ReturnType: "SELF_TYPE",
				Body:       &structures.BlockExpr{},
			},
			{
				Name:       "in_string",
				Parameters: nil,
				ReturnType: "String",
				Body:       &structures.BlockExpr{},
			},
			{
				Name:       "in_int",
				Parameters: nil,
				ReturnType: "Int",
				Body:       &structures.BlockExpr{},
			},
		},
		Attributes: nil,
	}

	intClass := structures.ClassDecl{
		Name:       "Int",
		Parent:     "Object",
		Methods:    nil,
		Attributes: nil,
	}

	stringClass := structures.ClassDecl{
		Name:   "String",
		Parent: "Object",
		Methods: []structures.Method{
			{
				Name:       "length",
				Parameters: nil,
				ReturnType: "Int",
				Body:       &structures.BlockExpr{},
			},
			{
				Name:       "concat",
				Parameters: []structures.Param{{Name: "s", Type: "String"}},
				ReturnType: "String",
				Body:       &structures.BlockExpr{},
			},
			{
				Name: "substr",
				Parameters: []structures.Param{
					{Name: "i", Type: "Int"},
					{Name: "l", Type: "Int"},
				},
				ReturnType: "String",
				Body:       &structures.BlockExpr{},
			},
		},
		Attributes: nil,
	}

	boolClass := structures.ClassDecl{
		Name:       "Bool",
		Parent:     "Object",
		Methods:    nil,
		Attributes: nil,
	}

	return []structures.ClassDecl{objectClass, ioClass, intClass, stringClass, boolClass}
}

// InjectBasicClassesST registers the 5 basic classes in the global symbol table.
// This ensures that the basic classes (Object, IO, Int, String, Bool) are present in the symbol table
// before any user-defined classes are added.
func InjectBasicClassesST(globalTable *structures.SymbolTable) {
	objectScope := structures.NewSymbolTable(globalTable)
	objectScope.AddEntry("abort", structures.SymbolEntry{
		Type: "Object",
		Method: &structures.Method{
			Name:       "abort",
			ReturnType: "Object",
		},
		Scope: structures.NewSymbolTable(objectScope),
	})
	objectScope.AddEntry("type_name", structures.SymbolEntry{
		Type: "String",
		Method: &structures.Method{
			Name:       "type_name",
			ReturnType: "String",
		},
		Scope: structures.NewSymbolTable(objectScope),
	})
	objectScope.AddEntry("copy", structures.SymbolEntry{
		Type: "SELF_TYPE",
		Method: &structures.Method{
			Name:       "copy",
			ReturnType: "SELF_TYPE",
		},
		Scope: structures.NewSymbolTable(objectScope),
	})
	globalTable.AddEntry("Object", structures.SymbolEntry{
		Type:  "class",
		Scope: objectScope,
	})

	ioScope := structures.NewSymbolTable(globalTable)
	ioEntry := structures.SymbolEntry{
		Type:   "class",
		Scope:  ioScope,
		Parent: "Object",
	}
	ioScope.AddEntry("out_string", structures.SymbolEntry{
		Type: "SELF_TYPE",
		Method: &structures.Method{
			Name:       "out_string",
			Parameters: []structures.Param{{Name: "x", Type: "String"}},
			ReturnType: "SELF_TYPE",
		},
		Scope: structures.NewSymbolTable(ioScope),
	})
	ioScope.AddEntry("out_int", structures.SymbolEntry{
		Type: "SELF_TYPE",
		Method: &structures.Method{
			Name:       "out_int",
			Parameters: []structures.Param{{Name: "x", Type: "Int"}},
			ReturnType: "SELF_TYPE",
		},
		Scope: structures.NewSymbolTable(ioScope),
	})
	ioScope.AddEntry("in_string", structures.SymbolEntry{
		Type: "String",
		Method: &structures.Method{
			Name:       "in_string",
			ReturnType: "String",
		},
		Scope: structures.NewSymbolTable(ioScope),
	})
	ioScope.AddEntry("in_int", structures.SymbolEntry{
		Type: "Int",
		Method: &structures.Method{
			Name:       "in_int",
			ReturnType: "Int",
		},
		Scope: structures.NewSymbolTable(ioScope),
	})
	globalTable.AddEntry("IO", ioEntry)

	intScope := structures.NewSymbolTable(globalTable)
	globalTable.AddEntry("Int", structures.SymbolEntry{
		Type:   "class",
		Scope:  intScope,
		Parent: "Object",
	})

	strScope := structures.NewSymbolTable(globalTable)
	strScope.AddEntry("length", structures.SymbolEntry{
		Type: "Int",
		Method: &structures.Method{
			Name:       "length",
			ReturnType: "Int",
		},
		Scope: structures.NewSymbolTable(strScope),
	})
	strScope.AddEntry("concat", structures.SymbolEntry{
		Type: "String",
		Method: &structures.Method{
			Name:       "concat",
			Parameters: []structures.Param{{Name: "s", Type: "String"}},
			ReturnType: "String",
		},
		Scope: structures.NewSymbolTable(strScope),
	})
	strScope.AddEntry("substr", structures.SymbolEntry{
		Type: "String",
		Method: &structures.Method{
			Name:       "substr",
			Parameters: []structures.Param{
				{Name: "i", Type: "Int"},
				{Name: "l", Type: "Int"},
			},
			ReturnType: "String",
		},
		Scope: structures.NewSymbolTable(strScope),
	})
	globalTable.AddEntry("String", structures.SymbolEntry{
		Type:   "class",
		Scope:  strScope,
		Parent: "Object",
	})

	boolScope := structures.NewSymbolTable(globalTable)
	globalTable.AddEntry("Bool", structures.SymbolEntry{
		Type:   "class",
		Scope:  boolScope,
		Parent: "Object",
	})
}

func BasicClassStubs() string {
    return `
    ; Declare external functions
declare i32 @printf(i8*, ...)
declare i32 @scanf(i8*, ...)
declare i8* @malloc(i32)
declare void @exit(i32)
declare i64 @strlen(i8*)
declare i8* @strcpy(i8*, i8*)
declare i8* @strcat(i8*, i8*)
declare i8* @strncpy(i8*, i8*, i32)

; String constants
@.fmt_str = private constant [4 x i8] c"%s\0A\00"
@.fmt_int = private constant [4 x i8] c"%d\0A\00"
@.fmt_scan_str = private constant [3 x i8] c"%s\00"
@.fmt_scan_int = private constant [3 x i8] c"%d\00"
@.empty_str = private constant [1 x i8] c"\00"

; Global buffer for input storage
@.input_buffer = global [256 x i8] zeroinitializer


; Entry point function
define i32 @main() {
  %mainObj = call i8* @Main_new()
  %t0 = call i8* @Main_main(i8* %mainObj)
  ret i32 0
}

; Object methods
define i8* @Object_abort(i8* %self) {
  call void @exit(i32 1)
  ret i8* %self
}

define i8* @Object_type_name(i8* %self) {
  %t13 = bitcast i8* %self to i64*
  %t14 = load i64, i64* %t13, align 8
  %t15 = inttoptr i64 %t14 to i8*
  ret i8* %t15
}



define i8* @Object_copy(i8* %self) {
  ret i8* %self
}

; Output string method
define i8* @IO_out_string(i8* %self, i8* %x) {
  %fmt_ptr = getelementptr inbounds [4 x i8], [4 x i8]* @.fmt_str, i32 0, i32 0
  call i32 (i8*, ...) @printf(i8* %fmt_ptr, i8* %x)
  ret i8* %self
}

; Output integer method
define i8* @IO_out_int(i8* %self, i32 %x) {
  %fmt_ptr = getelementptr inbounds [4 x i8], [4 x i8]* @.fmt_int, i32 0, i32 0
  call i32 (i8*, ...) @printf(i8* %fmt_ptr, i32 %x)
  ret i8* %self
}

; Input string method (reads from user)
define i8* @IO_in_string(i8* %self) {
  %fmt_ptr = getelementptr inbounds [4 x i8], [4 x i8]* @.fmt_scan_str, i32 0, i32 0
  %buf_ptr = getelementptr inbounds [256 x i8], [256 x i8]* @.input_buffer, i32 0, i32 0
  call i32 (i8*, ...) @scanf(i8* %fmt_ptr, i8* %buf_ptr)
  ret i8* %buf_ptr
}

; Input integer method (reads from user)
define i32 @IO_in_int(i8* %self) {
  %fmt_ptr = getelementptr inbounds [3 x i8], [3 x i8]* @.fmt_scan_int, i32 0, i32 0
  %input = alloca i32
  call i32 (i8*, ...) @scanf(i8* %fmt_ptr, i32* %input)
  %result = load i32, i32* %input
  ret i32 %result
}



define i32 @String_length(i8* %self) {
    %len64 = call i64 @strlen(i8* %self)
    %len32 = trunc i64 %len64 to i32
    ret i32 %len32
  }
  define i8* @String_concat(i8* %self, i8* %s) {
    %len_self = call i64 @strlen(i8* %self)
    %len_s = call i64 @strlen(i8* %s)
    %total_len = add i64 %len_self, %len_s
    %total_size = add i64 %total_len, 1        
    %total_size32 = trunc i64 %total_size to i32  
    %ptr = call i8* @malloc(i32 %total_size32)
    %copy = call i8* @strcpy(i8* %ptr, i8* %self)
    %concat = call i8* @strcat(i8* %ptr, i8* %s)
    ret i8* %ptr
  }
  define i8* @String_substr(i8* %self, i32 %i, i32 %l) {
    %len64 = call i64 @strlen(i8* %self)
    %len = trunc i64 %len64 to i32
    %alloc_size = add i32 %l, 1
    %ptr = call i8* @malloc(i32 %alloc_size)
    %src_ptr = getelementptr i8, i8* %self, i32 %i
    %res = call i8* @strncpy(i8* %ptr, i8* %src_ptr, i32 %l)
    %end_ptr = getelementptr i8, i8* %ptr, i32 %l
    store i8 0, i8* %end_ptr
    ret i8* %ptr
  }
    `
}