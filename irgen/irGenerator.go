package irgen

import (
    "fmt"
    "strings"

    "cool-compiler/ast"
)

// ExprResult holds the register, LLVM type, and original COOL type after generating code for an expression.
type ExprResult struct {
    reg     string // e.g. %t1
    llvmTy  string // e.g. i32 or i8*
    coolTy  string // e.g. "Int", "ktest", etc.
}

// IRGenerator generates LLVM IR for COOL.
type IRGenerator struct {
    sb               strings.Builder
    globals          strings.Builder
    tempCount        int
    strings          map[string]string             // maps literal strings to global constant names
    methodSignatures map[string]MethodSig          // maps "Class_Method" to its signature

    // vars maps variable names -> VarInfo. This includes let-bindings, parameters, etc.
    vars         map[string]VarInfo
    currentClass string // current class name for SELF_TYPE resolution
}

// VarInfo tracks the declared storage (ptrReg), plus its LLVM type and COOL type
type VarInfo struct {
    ptrReg   string
    llvmType string
    coolType string
}

type MethodSig struct {
    ReturnType   string
    ParamTypes   []string
    IsReturnSelf bool
}

func NewIRGenerator() *IRGenerator {
    return &IRGenerator{
        strings:          make(map[string]string),
        methodSignatures: make(map[string]MethodSig),
        vars:             make(map[string]VarInfo),
    }
}

// newTemp produces a fresh temporary register name (e.g., %t0, %t1, …).
func (gen *IRGenerator) newTemp() string {
    r := fmt.Sprintf("%%t%d", gen.tempCount)
    gen.tempCount++
    return r
}

func (gen *IRGenerator) emit(line string) {
    gen.sb.WriteString(line + "\n")
}

// methodKey returns a unique key for a method in a given class.
func (gen *IRGenerator) methodKey(className, methodName string) string {
    return className + "_" + methodName
}

// newStringConstant creates (or reuses) a global constant for a string literal.
func (gen *IRGenerator) newStringConstant(val string) string {
    if g, ok := gen.strings[val]; ok {
        return g
    }
    name := fmt.Sprintf("@.str%d", len(gen.strings))
    gen.strings[val] = name
    length := len(val) + 1 // include null terminator
    gen.globals.WriteString(
        fmt.Sprintf("%s = private unnamed_addr constant [%d x i8] c\"%s\\00\"\n",
            name, length, val),
    )
    return name
}

// llvmType maps a COOL type to an LLVM type.
func (gen *IRGenerator) llvmType(t string) string {
    switch t {
    case "Int", "Bool":
        return "i32"
    case "String":
        return "i8*"
    default:
        // For SELF_TYPE and user classes, use i8*.
        return "i8*"
    }
}

func (gen *IRGenerator) Generate(program *ast.Program) string {

    // 1) Collect method signatures.
    gen.collectMethodSignatures(program)

    // 2) Generate IR for user-defined classes (skip built-in classes).
    for _, cls := range program.Classes {
        if cls.Name == "Object" || cls.Name == "IO" ||
            cls.Name == "Int" || cls.Name == "String" || cls.Name == "Bool" {
            continue
        }
        gen.genClass(&cls)
        gen.genNewFunction(&cls)
    }

    // Ensure that the global constant for "Object" is defined.
    _ = gen.newStringConstant("Object")

    var finalIR strings.Builder
    finalIR.WriteString("; ModuleID = 'cool_module'\n")

    // 3) Print global string constants.
    finalIR.WriteString(gen.globals.String())

    // 4) Print user-defined function bodies.
    finalIR.WriteString(gen.sb.String())

    // 5) Append built-in stubs.
    finalIR.WriteString(gen.basicClassStubs())

    return finalIR.String()
}


func (gen *IRGenerator) basicClassStubs() string {
    return `
    ; Declare external functions
declare i32 @printf(i8*, ...)
declare i32 @scanf(i8*, ...)
declare i8* @malloc(i32)
declare void @exit(i32)

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
  ret i8* @.str1
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

; String methods (stub implementations)
define i32 @String_length(i8* %self) {
  ret i32 999
}

define i8* @String_concat(i8* %self, i8* %s) {
  ret i8* %self
}

define i8* @String_substr(i8* %self, i32 %i, i32 %l) {
  ret i8* %self
}
    `
}



// collectMethodSignatures gathers method signature info from every class.
func (gen *IRGenerator) collectMethodSignatures(program *ast.Program) {
    for _, class := range program.Classes {
        for _, method := range class.Methods {
            key := gen.methodKey(class.Name, method.Name)
            isSelf := (method.ReturnType == "SELF_TYPE")
            var retTy string
            if isSelf {
                retTy = "i8*"
            } else {
                retTy = gen.llvmType(method.ReturnType)
            }
            var pTys []string
            pTys = append(pTys, "i8*") // self
            for _, param := range method.Parameters {
                pTys = append(pTys, gen.llvmType(param.Type))
            }
            gen.methodSignatures[key] = MethodSig{
                ReturnType:   retTy,
                ParamTypes:   pTys,
                IsReturnSelf: isSelf,
            }
        }
    }
}

func (gen *IRGenerator) genClass(class *ast.ClassDecl) {
    gen.currentClass = class.Name
    for _, m := range class.Methods {
        gen.genMethod(class.Name, &m)
    }
}

// genNewFunction generates a "Class_new" function that allocates memory for objects using malloc.
// Here we assume each object occupies ((number_of_attributes + 1) * 8) bytes.
func (gen *IRGenerator) genNewFunction(class *ast.ClassDecl) {
    // For built-in classes, assume runtime provides new functions.
    if class.Name == "Object" || class.Name == "IO" ||
        class.Name == "Int" || class.Name == "String" || class.Name == "Bool" {
        return
    }
    numAttrs := len(class.Attributes)
    size := (numAttrs + 1) * 8
    gen.emit(fmt.Sprintf("define i8* @%s_new() {", class.Name))
    gen.emit(fmt.Sprintf("  %%size = add i32 %d, 0", size))
    gen.emit("  %ptr = call i8* @malloc(i32 %size)")
    gen.emit("  ret i8* %ptr")
    gen.emit("}")
    gen.emit("")
}

func (gen *IRGenerator) genMethod(className string, method *ast.Method) {
    // Start fresh var map for each method
    gen.vars = make(map[string]VarInfo)

    key := gen.methodKey(className, method.Name)
    sig, ok := gen.methodSignatures[key]
    if !ok {
        // fallback if no signature found
        sig = MethodSig{ReturnType: "i32", ParamTypes: []string{"i8*"}}
    }
    funcName := "@" + key

    // Build parameter list for IR
    var params []string
    params = append(params, "i8* %self")
    for _, p := range method.Parameters {
        pt := gen.llvmType(p.Type)
        params = append(params, fmt.Sprintf("%s %%%s", pt, p.Name))
    }

    gen.emit(fmt.Sprintf("define %s %s(%s) {", sig.ReturnType, funcName, strings.Join(params, ", ")))

    // Handle 'self'
    {
        tmp := gen.newTemp()
        gen.emit(fmt.Sprintf("  %s = alloca i8*", tmp))
        gen.emit(fmt.Sprintf("  store i8* %%self, i8** %s", tmp))
        // Record that 'self' has coolType = className
        gen.vars["self"] = VarInfo{
            ptrReg:   tmp,
            llvmType: "i8*",
            coolType: className,
        }
    }

    // Handle method parameters
    for _, p := range method.Parameters {
        paramTy := gen.llvmType(p.Type)
        ptr := gen.newTemp()
        gen.emit(fmt.Sprintf("  %s = alloca %s", ptr, paramTy))
        gen.emit(fmt.Sprintf("  store %s %%%s, %s* %s", paramTy, p.Name, paramTy, ptr))
        gen.vars[p.Name] = VarInfo{
            ptrReg:   ptr,
            llvmType: paramTy,
            coolType: p.Type,
        }
    }

    // Generate code for the body
    body := gen.genExprEx(method.Body) // Our new version that returns ExprResult

    // If return type is SELF_TYPE
    if sig.IsReturnSelf {
        tmpLoad := gen.newTemp()
        gen.emit(fmt.Sprintf("  %s = load i8*, i8** %s", tmpLoad, gen.vars["self"].ptrReg))
        gen.emit(fmt.Sprintf("  ret i8* %s", tmpLoad))
        gen.emit("}\n")
        return
    }

    // If function returns i8* but we got i32 => do inttoptr
    if sig.ReturnType == "i8*" && body.llvmTy == "i32" {
        c := gen.newTemp()
        gen.emit(fmt.Sprintf("  %s = inttoptr i32 %s to i8*", c, body.reg))
        body.reg = c
        body.llvmTy = "i8*"
    }

    // Return final
    gen.emit(fmt.Sprintf("  ret %s %s", sig.ReturnType, body.reg))
    gen.emit("}\n")
}

// ----------------------------------------------------------------------------
//    Below is the *NEW* genExprEx that returns (reg, llvmTy, coolTy).
//    Then we define small helpers to cast/unify these results
// ----------------------------------------------------------------------------
func (gen *IRGenerator) genExprEx(expr ast.Expr) ExprResult {
    switch e := expr.(type) {
    case *ast.IntExpr:
        return ExprResult{
            reg:    fmt.Sprintf("%d", e.Value),
            llvmTy: "i32",
            coolTy: "Int",
        }
    case *ast.BoolExpr:
        val := "0"
        if e.Value {
            val = "1"
        }
        return ExprResult{reg: val, llvmTy: "i32", coolTy: "Bool"}
    case *ast.StringExpr:
        gl := gen.newStringConstant(e.Value)
        tmp := gen.newTemp()
        length := len(e.Value) + 1
        gen.emit(fmt.Sprintf("  %s = getelementptr [%d x i8], [%d x i8]* %s, i32 0, i32 0",
            tmp, length, length, gl))
        return ExprResult{reg: tmp, llvmTy: "i8*", coolTy: "String"}
    case *ast.VarExpr:
        if info, ok := gen.vars[e.Name]; ok {
            loadReg := gen.newTemp()
            gen.emit(fmt.Sprintf("  %s = load %s, %s* %s",
                loadReg, info.llvmType, info.llvmType, info.ptrReg))
            return ExprResult{
                reg:    loadReg,
                llvmTy: info.llvmType,
                coolTy: info.coolType,
            }
        }
        // Fallback: allocate a default integer variable
        ptr := gen.newTemp()
        gen.emit(fmt.Sprintf("  %s = alloca i32", ptr))
        gen.emit(fmt.Sprintf("  store i32 0, i32* %s", ptr))
        loadReg := gen.newTemp()
       	gen.emit(fmt.Sprintf("  %s = load i32, i32* %s", loadReg, ptr))
        gen.vars[e.Name] = VarInfo{ptrReg: ptr, llvmType: "i32", coolType: "Int"}
        return ExprResult{reg: loadReg, llvmTy: "i32", coolTy: "Int"}
    case *ast.AssignExpr:
        rhs := gen.genExprEx(e.Value)
       	info, exists := gen.vars[e.Name]
        if !exists {
           	newPtr := gen.newTemp()
           	gen.emit(fmt.Sprintf("  %s = alloca i32", newPtr))
           	info = VarInfo{ptrReg: newPtr, llvmType: "i32", coolType: "Int"}
           	gen.vars[e.Name] = info
        }
        fixed := castResultIfNeeded(rhs, info.llvmType, gen)
       	gen.emit(fmt.Sprintf("  store %s %s, %s* %s",
            info.llvmType, fixed.reg, info.llvmType, info.ptrReg))
        return ExprResult{reg: fixed.reg, llvmTy: info.llvmType, coolTy: info.coolType}
    case *ast.NewExpr:
       	className := e.Type
        if className == "SELF_TYPE" {
            className = gen.currentClass
        }
        newReg := gen.newTemp()
        gen.emit(fmt.Sprintf("  %s = call i8* @%s_new()", newReg, className))
        return ExprResult{reg: newReg, llvmTy: "i8*", coolTy: className}
    case *ast.DispatchExpr:
        return gen.genDispatchEx(e)
    case *ast.StaticDispatchExpr:
        // Evaluate the caller expression
       	caller := gen.genExprEx(e.Caller)
       	fixedCaller := castResultIfNeeded(caller, "i8*", gen)
       	// Use the explicitly specified type for method resolution
       	mk := gen.methodKey(e.Type, e.Method)
       	sig, ok := gen.methodSignatures[mk]
       	if !ok {
            switch e.Method {
            case "out_string":
                sig = MethodSig{ReturnType: "i8*", ParamTypes: []string{"i8*", "i8*"}, IsReturnSelf: true}
            case "out_int":
                sig = MethodSig{ReturnType: "i8*", ParamTypes: []string{"i8*", "i32"}, IsReturnSelf: true}
            case "in_string":
                sig = MethodSig{ReturnType: "i8*", ParamTypes: []string{"i8*"}}
            case "in_int":
                sig = MethodSig{ReturnType: "i32", ParamTypes: []string{"i8*"}}
            default:
                sig = MethodSig{ReturnType: "i32", ParamTypes: []string{"i8*"}}
            }
       	}
       	funcName := "@" + mk
       	var arglist []string
       	arglist = append(arglist, "i8* "+fixedCaller.reg)
       	for i, argE := range e.Arguments {
           	argRes := gen.genExprEx(argE)
           	paramTy := "i32"
           	if i+1 < len(sig.ParamTypes) {
               	paramTy = sig.ParamTypes[i+1]
           	}
           	castedArg := castResultIfNeeded(argRes, paramTy, gen)
           	arglist = append(arglist, fmt.Sprintf("%s %s", paramTy, castedArg.reg))
       	}
       	callReg := gen.newTemp()
       	typeList := strings.Join(sig.ParamTypes, ", ")
       	argsStr := strings.Join(arglist, ", ")
       	gen.emit(fmt.Sprintf("  %s = call %s (%s) %s(%s)",
            callReg, sig.ReturnType, typeList, funcName, argsStr))
       	var retCool string
       	if sig.IsReturnSelf {
           	retCool = caller.coolTy
       	} else {
           	if sig.ReturnType == "i32" {
               	retCool = "Int"
           	} else {
               	retCool = "Object"
           	}
       	}
       	return ExprResult{reg: callReg, llvmTy: sig.ReturnType, coolTy: retCool}
    case *ast.IfExpr:
        cond := gen.genExprEx(e.Condition)
        condCasted := castResultIfNeeded(cond, "i32", gen)
       	condI1 := gen.newTemp()
       	gen.emit(fmt.Sprintf("  %s = icmp ne i32 %s, 0", condI1, condCasted.reg))
       	thenLabel := gen.newTemp()[1:]
       	elseLabel := gen.newTemp()[1:]
       	endLabel := gen.newTemp()[1:]
       	gen.emit(fmt.Sprintf("  br i1 %s, label %%%s, label %%%s", condI1, thenLabel, elseLabel))
       
       	gen.emit(fmt.Sprintf("%s:", thenLabel))
       	tRes := gen.genExprEx(e.ThenBranch)
       	gen.emit(fmt.Sprintf("  br label %%%s", endLabel))
       
       	gen.emit(fmt.Sprintf("%s:", elseLabel))
       	eRes := gen.genExprEx(e.ElseBranch)
       	gen.emit(fmt.Sprintf("  br label %%%s", endLabel))
       
       	gen.emit(fmt.Sprintf("%s:", endLabel))
       	uniTy := unifyLLVMTypes(tRes.llvmTy, eRes.llvmTy)
       	uniCool := unifyCoolTypes(tRes.coolTy, eRes.coolTy)
       
       	tCast := castResultIfNeeded(tRes, uniTy, gen)
       	eCast := castResultIfNeeded(eRes, uniTy, gen)
       
       	phi := gen.newTemp()
       	gen.emit(fmt.Sprintf("  %s = phi %s [%s, %%%s], [%s, %%%s]",
            phi, uniTy, tCast.reg, thenLabel, eCast.reg, elseLabel))
       
       	return ExprResult{reg: phi, llvmTy: uniTy, coolTy: uniCool}
    case *ast.LetExpr:
        oldVars := gen.vars
       	// Create a fresh scope (shallow copy)
       	newVars := make(map[string]VarInfo)
       	for k, v := range oldVars {
           	newVars[k] = v
       	}
       	gen.vars = newVars
       	// Process each let binding
       	for _, binding := range e.Variables {
           	vty := gen.llvmType(binding.Type)
           	ptr := gen.newTemp()
           	gen.emit(fmt.Sprintf("  %s = alloca %s", ptr, vty))
           	var initReg string
           	if binding.InitValue != nil {
               	initRes := gen.genExprEx(binding.InitValue)
               	initReg = castResultIfNeeded(initRes, vty, gen).reg
           	} else {
               	if vty == "i32" {
                   	initReg = "0"
               	} else {
                   	initReg = "null"
               	}
           	}
           	gen.emit(fmt.Sprintf("  store %s %s, %s* %s", vty, initReg, vty, ptr))
           	gen.vars[binding.Name] = VarInfo{ptrReg: ptr, llvmType: vty, coolType: binding.Type}
       	}
       	bodyRes := gen.genExprEx(e.Body)
       	gen.vars = oldVars // restore previous scope
       	return bodyRes
    case *ast.BlockExpr:
        var last ExprResult
       	for _, subExpr := range e.Expressions {
           	last = gen.genExprEx(subExpr)
       	}
       	return last
    case *ast.WhileExpr:
       	loopLabel := gen.newTemp()[1:]
       	bodyLabel := gen.newTemp()[1:]
       	doneLabel := gen.newTemp()[1:]
       	gen.emit(fmt.Sprintf("  br label %%%s", loopLabel))
       	gen.emit(fmt.Sprintf("%s:", loopLabel))
       	condRes := gen.genExprEx(e.Condition)
       	condRes = castResultIfNeeded(condRes, "i32", gen)
       	cmp := gen.newTemp()
       	gen.emit(fmt.Sprintf("  %s = icmp ne i32 %s, 0", cmp, condRes.reg))
       	gen.emit(fmt.Sprintf("  br i1 %s, label %%%s, label %%%s", cmp, bodyLabel, doneLabel))
       	gen.emit(fmt.Sprintf("%s:", bodyLabel))
       	_ = gen.genExprEx(e.Body) // discard body result
       	gen.emit(fmt.Sprintf("  br label %%%s", loopLabel))
       	gen.emit(fmt.Sprintf("%s:", doneLabel))
       	// While returns an integer constant (could represent void)
       	return ExprResult{reg: "0", llvmTy: "i32", coolTy: "Int"}
    case *ast.UnaryExpr:
       	operand := gen.genExprEx(e.Operand)
       	switch e.Operator {
       	case "not":
           	operand = castResultIfNeeded(operand, "i32", gen)
           	cmp := gen.newTemp()
           	gen.emit(fmt.Sprintf("  %s = icmp eq i32 %s, 0", cmp, operand.reg))
           	zext := gen.newTemp()
           	gen.emit(fmt.Sprintf("  %s = zext i1 %s to i32", zext, cmp))
           	return ExprResult{reg: zext, llvmTy: "i32", coolTy: "Bool"}
       	case "isvoid":
           	if operand.llvmTy == "i32" {
               	operand = castResultIfNeeded(operand, "i8*", gen)
           	}
           	cmp := gen.newTemp()
           	gen.emit(fmt.Sprintf("  %s = icmp eq i8* %s, null", cmp, operand.reg))
           	zext := gen.newTemp()
           	gen.emit(fmt.Sprintf("  %s = zext i1 %s to i32", zext, cmp))
           	return ExprResult{reg: zext, llvmTy: "i32", coolTy: "Bool"}
       	case "~":
           	operand = castResultIfNeeded(operand, "i32", gen)
           	sub := gen.newTemp()
           	gen.emit(fmt.Sprintf("  %s = sub i32 0, %s", sub, operand.reg))
           	return ExprResult{reg: sub, llvmTy: "i32", coolTy: "Int"}
       	default:
           	gen.emit("; unhandled unary operator")
           	return ExprResult{"0", "i32", "Int"}
       	}
    case *ast.BinaryExpr:
       	left := gen.genExprEx(e.Left)
       	right := gen.genExprEx(e.Right)
       	left = castResultIfNeeded(left, "i32", gen)
       	right = castResultIfNeeded(right, "i32", gen)
       	switch e.Operator {
       	case "+":
           	tmp := gen.newTemp()
           	gen.emit(fmt.Sprintf("  %s = add i32 %s, %s", tmp, left.reg, right.reg))
           	return ExprResult{reg: tmp, llvmTy: "i32", coolTy: "Int"}
       	case "-":
           	tmp := gen.newTemp()
           	gen.emit(fmt.Sprintf("  %s = sub i32 %s, %s", tmp, left.reg, right.reg))
           	return ExprResult{reg: tmp, llvmTy: "i32", coolTy: "Int"}
       	case "*":
           	tmp := gen.newTemp()
           	gen.emit(fmt.Sprintf("  %s = mul i32 %s, %s", tmp, left.reg, right.reg))
           	return ExprResult{reg: tmp, llvmTy: "i32", coolTy: "Int"}
       	case "/":
           	tmp := gen.newTemp()
           	gen.emit(fmt.Sprintf("  %s = sdiv i32 %s, %s", tmp, left.reg, right.reg))
           	return ExprResult{reg: tmp, llvmTy: "i32", coolTy: "Int"}
       	case "<":
           	cmp := gen.newTemp()
           	gen.emit(fmt.Sprintf("  %s = icmp slt i32 %s, %s", cmp, left.reg, right.reg))
           	zext := gen.newTemp()
           	gen.emit(fmt.Sprintf("  %s = zext i1 %s to i32", zext, cmp))
           	return ExprResult{reg: zext, llvmTy: "i32", coolTy: "Bool"}
       	case "<=":
           	cmp := gen.newTemp()
           	gen.emit(fmt.Sprintf("  %s = icmp sle i32 %s, %s", cmp, left.reg, right.reg))
           	zext := gen.newTemp()
           	gen.emit(fmt.Sprintf("  %s = zext i1 %s to i32", zext, cmp))
           	return ExprResult{reg: zext, llvmTy: "i32", coolTy: "Bool"}
       	case "=":
           	cmp := gen.newTemp()
           	gen.emit(fmt.Sprintf("  %s = icmp eq i32 %s, %s", cmp, left.reg, right.reg))
           	zext := gen.newTemp()
           	gen.emit(fmt.Sprintf("  %s = zext i1 %s to i32", zext, cmp))
           	return ExprResult{reg: zext, llvmTy: "i32", coolTy: "Bool"}
       	default:
           	gen.emit("; unknown binary operator")
           	return ExprResult{"0", "i32", "Int"}
       	}
    case *ast.CaseExpr:
       	gen.emit("; case expression stub: selecting first branch")
       	_ = gen.genExprEx(e.Expr)
       	if len(e.Cases) > 0 {
           	return gen.genExprEx(e.Cases[0].Body)
       	}
       	return ExprResult{"0", "i32", "Int"}
    default:
       	gen.emit("; unknown expr node")
       	return ExprResult{"0", "i32", "Int"}
    }
}


// -----------------------------------------------------------------------------
// The new dynamic dispatch that returns an ExprResult
// -----------------------------------------------------------------------------
func (gen *IRGenerator) genDispatchEx(e *ast.DispatchExpr) ExprResult {
    caller := gen.genExprEx(e.Caller)
    // cast to i8*
    fixedCaller := castResultIfNeeded(caller, "i8*", gen)
    // figure out actual class name
    var className string
    // If the caller is 'self' and the method is an IO function, force lookup in IO.
    if v, ok := e.Caller.(*ast.VarExpr); ok && v.Name == "self" {
        if e.Method == "out_string" || e.Method == "out_int" || e.Method == "in_string" || e.Method == "in_int" {
            className = "IO"
        } else {
            className = gen.currentClass
        }
    } else {
        className = caller.coolTy
        if className == "" {
            className = "Object"
        }
    }
    // fallback if caller is literally self, etc. (like your old logic), or if we detect "out_int"
    // But let's do a simpler approach:
    mk := gen.methodKey(className, e.Method)
    sig, ok := gen.methodSignatures[mk]
    if !ok {
        // fallback or builtins
        switch e.Method {
        case "out_string":
            sig = MethodSig{ReturnType: "i8*", ParamTypes: []string{"i8*", "i8*"}, IsReturnSelf: true}
        case "out_int":
            sig = MethodSig{ReturnType: "i8*", ParamTypes: []string{"i8*", "i32"}, IsReturnSelf: true}
        case "in_string":
            sig = MethodSig{ReturnType: "i8*", ParamTypes: []string{"i8*"}}
        case "in_int":
            sig = MethodSig{ReturnType: "i32", ParamTypes: []string{"i8*"}}
        default:
            sig = MethodSig{ReturnType: "i32", ParamTypes: []string{"i8*"}}
        }
    }

    funcName := "@" + mk
    var arglist []string
    // first argument: the caller
    arglist = append(arglist, "i8* "+fixedCaller.reg)

    // then the method args
    for i, argE := range e.Arguments {
        argRes := gen.genExprEx(argE)
        paramTy := "i32"
        if i+1 < len(sig.ParamTypes) {
            paramTy = sig.ParamTypes[i+1]
        }
        castedArg := castResultIfNeeded(argRes, paramTy, gen)
        arglist = append(arglist, fmt.Sprintf("%s %s", paramTy, castedArg.reg))
    }

    callReg := gen.newTemp()
    typeList := strings.Join(sig.ParamTypes, ", ")
    argsStr := strings.Join(arglist, ", ")
    gen.emit(fmt.Sprintf("  %s = call %s (%s) %s(%s)",
        callReg, sig.ReturnType, typeList, funcName, argsStr))

    // If the method signature says IsReturnSelf => the COOL type is the same as caller's type
    // else if the method declared a known return type, we can guess that, but we only stored the llvm type
    var retCool string
    if sig.IsReturnSelf {
        retCool = caller.coolTy
    } else {
        // We can't know for sure; fallback to "Int" if ReturnType==i32, else the class name
        // This is a bit hacky:
        if sig.ReturnType == "i32" {
            retCool = "Int"
        } else {
            // no easy guess; maybe "Object" or something
            retCool = "Object"
        }
    }

    return ExprResult{reg: callReg, llvmTy: sig.ReturnType, coolTy: retCool}
}

// -----------------------------------------------------------------------------
// Helper to unify the LLVM types of two ExprResults
// -----------------------------------------------------------------------------
func unifyLLVMTypes(a, b string) string {
    if a == b {
        return a
    }
    if a == "i8*" || b == "i8*" {
        return "i8*"
    }
    return "i32"
}
func unifyCoolTypes(a, b string) string {
    if a == b {
        return a
    }
    // naive fallback
    return "Object"
}

// -----------------------------------------------------------------------------
// Cast an ExprResult to a new LLVM type if needed
// -----------------------------------------------------------------------------
func castResultIfNeeded(r ExprResult, toTy string, gen *IRGenerator) ExprResult {
    if r.llvmTy == toTy {
        return r
    }
    // from i32 => i8*
    if r.llvmTy == "i32" && toTy == "i8*" {
        tmp := gen.newTemp()
        gen.emit(fmt.Sprintf("  %s = inttoptr i32 %s to i8*", tmp, r.reg))
        return ExprResult{reg: tmp, llvmTy: "i8*", coolTy: r.coolTy}
    }
    // from i8* => i32
    if r.llvmTy == "i8*" && toTy == "i32" {
        tmp := gen.newTemp()
        gen.emit(fmt.Sprintf("  %s = ptrtoint i8* %s to i32", tmp, r.reg))
        return ExprResult{reg: tmp, llvmTy: "i32", coolTy: r.coolTy}
    }
    // fallback => no change
    return r
}

// -----------------------------------------------------------------------------
// If you want to keep the old genExpr, you can just call genExprEx internally.
// For example, define a small wrapper:
// -----------------------------------------------------------------------------

func (gen *IRGenerator) genExpr(expr ast.Expr) (string, string) {
    r := gen.genExprEx(expr)
    return r.reg, r.llvmTy
}
