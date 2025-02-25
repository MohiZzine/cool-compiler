package irgen

import (
    "fmt"
    "strings"

    "cool-compiler/ast"
)

// IRGenerator codegen: uses (regName,type), converting i32 <-> i8* with inttoptr/ptrtoint.
type IRGenerator struct {
    sb        strings.Builder
    globals   strings.Builder
    tempCount int

    strings          map[string]string
    methodSignatures map[string]MethodSig
    vars             map[string]VarInfo
}

type VarInfo struct {
    ptrReg   string
    llvmType string
}

type MethodSig struct {
    ReturnType string
    ParamTypes []string
}

// NewIRGenerator => create an instance
func NewIRGenerator() *IRGenerator {
    return &IRGenerator{
        strings:          make(map[string]string),
        methodSignatures: make(map[string]MethodSig),
        vars:             make(map[string]VarInfo),
    }
}

func (gen *IRGenerator) newTemp() string {
    r := fmt.Sprintf("%%t%d", gen.tempCount)
    gen.tempCount++
    return r
}

// isAllDigits => check if literal is purely integer (with optional - sign)
func isAllDigits(s string) bool {
    if s == "" {
        return false
    }
    i := 0
    if s[0] == '-' {
        i = 1
        if len(s) == 1 {
            return false
        }
    }
    for ; i < len(s); i++ {
        c := s[i]
        if c < '0' || c > '9' {
            return false
        }
    }
    return true
}

// methodKey => \"Class_method\"
func (gen *IRGenerator) methodKey(className, methodName string) string {
    return className + "_" + methodName
}

// newStringConstant => define a global c-string
func (gen *IRGenerator) newStringConstant(val string) string {
    if g, ok := gen.strings[val]; ok {
        return g
    }
    name := fmt.Sprintf("@.str%d", len(gen.strings))
    gen.strings[val] = name
    length := len(val) + 1
    gen.globals.WriteString(
        fmt.Sprintf("%s = private unnamed_addr constant [%d x i8] c\"%s\\00\"\n",
            name, length, val),
    )
    return name
}

func (gen *IRGenerator) emit(line string) {
    gen.sb.WriteString(line + "\n")
}

// llvmType => map COOL type to i32 or i8*
func (gen *IRGenerator) llvmType(t string) string {
    switch t {
    case "Int", "Bool":
        return "i32"
    case "String":
        return "i8*"
    default:
        return "i8*"
    }
}

// Generate => produce final IR
func (gen *IRGenerator) Generate(program *ast.Program) string {
    gen.collectMethodSignatures(program)

    var finalIR strings.Builder
    finalIR.WriteString("; ModuleID = 'cool_module'\n")
    finalIR.WriteString("declare i32 @printf(i8*, ...)\n")

    // 1) Write the global string constants FIRST
    finalIR.WriteString(gen.globals.String())

    // 2) Then the function bodies (which reference those strings)
    finalIR.WriteString(gen.sb.String())

    return finalIR.String()
}

func (gen *IRGenerator) collectMethodSignatures(program *ast.Program) {
    for _, class := range program.Classes {
        for _, method := range class.Methods {
            key := gen.methodKey(class.Name, method.Name)
            retTy := gen.llvmType(method.ReturnType)
            var pTys []string
            pTys = append(pTys, "i8*") // self
            for _, param := range method.Parameters {
                pTys = append(pTys, gen.llvmType(param.Type))
            }
            gen.methodSignatures[key] = MethodSig{
                ReturnType: retTy,
                ParamTypes: pTys,
            }
        }
    }
}

func (gen *IRGenerator) genClass(class *ast.ClassDecl) {
    for _, m := range class.Methods {
        gen.genMethod(class.Name, &m)
    }
}

func (gen *IRGenerator) genMethod(className string, method *ast.Method) {
    gen.vars = make(map[string]VarInfo)

    key := gen.methodKey(className, method.Name)
    sig, ok := gen.methodSignatures[key]
    if !ok {
        sig = MethodSig{ReturnType: "i32", ParamTypes: []string{"i8*"}}
    }

    funcName := "@" + key
    var params []string
    // param0 => i8* %self
    params = append(params, "i8* %self")
    for _, p := range method.Parameters {
        pt := gen.llvmType(p.Type)
        params = append(params, fmt.Sprintf("%s %%%s", pt, p.Name))
    }

    gen.emit(fmt.Sprintf("define %s %s(%s) {",
        sig.ReturnType, funcName, strings.Join(params, ", ")))
    // store self
    {
        tmp := gen.newTemp()
        gen.emit(fmt.Sprintf("  %s = alloca i8*", tmp))
        gen.emit(fmt.Sprintf("  store i8* %%self, i8** %s", tmp))
        gen.vars["self"] = VarInfo{ptrReg: tmp, llvmType: "i8*"}
    }

    // store user params
    // we skip the param0 b/c it's self
    idx := 1
    for _, p := range method.Parameters {
        paramTy := gen.llvmType(p.Type)
        ptr := gen.newTemp()
        gen.emit(fmt.Sprintf("  %s = alloca %s", ptr, paramTy))
        gen.emit(fmt.Sprintf("  store %s %%%s, %s* %s", paramTy, p.Name, paramTy, ptr))
        gen.vars[p.Name] = VarInfo{ptrReg: ptr, llvmType: paramTy}
        idx++
    }

    reg, rty := gen.genExpr(method.Body)

    // if returns i8* but we got i32 => inttoptr
    if sig.ReturnType == "i8*" && rty == "i32" {
        c := gen.newTemp()
        gen.emit(fmt.Sprintf("  %s = inttoptr i32 %s to i8*", c, reg))
        reg = c
        rty = "i8*"
    }

    gen.emit(fmt.Sprintf("  ret %s %s", sig.ReturnType, reg))
    gen.emit("}\n")
}

// genExpr => produce (registerName, llvmType). We do safe casts using inttoptr or ptrtoint if needed
func (gen *IRGenerator) genExpr(expr ast.Expr) (string, string) {
    switch e := expr.(type) {
    case *ast.IntExpr:
        return fmt.Sprintf("%d", e.Value), "i32"

    case *ast.BoolExpr:
        if e.Value {
            return "1", "i32"
        }
        return "0", "i32"

    case *ast.StringExpr:
        gl := gen.newStringConstant(e.Value)
        tmp := gen.newTemp()
        length := len(e.Value) + 1
        gen.emit(fmt.Sprintf("  %s = getelementptr [%d x i8], [%d x i8]* %s, i32 0, i32 0",
            tmp, length, length, gl))
        return tmp, "i8*"

    case *ast.VarExpr:
        if info, ok := gen.vars[e.Name]; ok {
            loadReg := gen.newTemp()
            gen.emit(fmt.Sprintf("  %s = load %s, %s* %s",
                loadReg, info.llvmType, info.llvmType, info.ptrReg))
            return loadReg, info.llvmType
        }
        // fallback => treat as i8*
        return "%" + e.Name, "i8*"

    case *ast.AssignExpr:
        rhsReg, rhsTy := gen.genExpr(e.Value)
        info, exists := gen.vars[e.Name]
        if !exists {
            // create i32
            newPtr := gen.newTemp()
            gen.emit(fmt.Sprintf("  %s = alloca i32", newPtr))
            info = VarInfo{ptrReg: newPtr, llvmType: "i32"}
            gen.vars[e.Name] = info
        }
        // unify
        finalReg := castIfNeeded(rhsReg, rhsTy, info.llvmType, gen)
        gen.emit(fmt.Sprintf("  store %s %s, %s* %s",
            info.llvmType, finalReg, info.llvmType, info.ptrReg))
        return finalReg, info.llvmType

    case *ast.LetExpr:
        // shadow
        oldVars := gen.vars
        newMap := make(map[string]VarInfo)
        for k, v := range oldVars {
            newMap[k] = v
        }
        gen.vars = newMap

        for _, b := range e.Variables {
            vty := gen.llvmType(b.Type)
            ptr := gen.newTemp()
            gen.emit(fmt.Sprintf("  %s = alloca %s", ptr, vty))
            r := ""
            rty := vty
            if b.InitValue != nil {
                r, rty = gen.genExpr(b.InitValue)
                r = castIfNeeded(r, rty, vty, gen)
            } else {
                if vty == "i32" {
                    r = "0"
                } else {
                    r = "null"
                }
            }
            gen.emit(fmt.Sprintf("  store %s %s, %s* %s",
                vty, r, vty, ptr))
            gen.vars[b.Name] = VarInfo{ptrReg: ptr, llvmType: vty}
        }
        reg, ty := gen.genExpr(e.Body)
        gen.vars = oldVars
        return reg, ty

    case *ast.BlockExpr:
        var lastReg, lastTy string
        for _, sub := range e.Expressions {
            lastReg, lastTy = gen.genExpr(sub)
        }
        return lastReg, lastTy

    case *ast.IfExpr:
        condReg, condTy := gen.genExpr(e.Condition)
        condReg = castIfNeeded(condReg, condTy, "i32", gen)

        condI1 := gen.newTemp()
        gen.emit(fmt.Sprintf("  %s = icmp ne i32 %s, 0", condI1, condReg))

        thenLabel := gen.newTemp()[1:]
        elseLabel := gen.newTemp()[1:]
        endLabel := gen.newTemp()[1:]

        gen.emit(fmt.Sprintf("  br i1 %s, label %%%s, label %%%s", condI1, thenLabel, elseLabel))

        // then
        gen.emit(fmt.Sprintf("%s:", thenLabel))
        tReg, tTy := gen.genExpr(e.ThenBranch)
        gen.emit(fmt.Sprintf("  br label %%%s", endLabel))

        // else
        gen.emit(fmt.Sprintf("%s:", elseLabel))
        eReg, eTy := gen.genExpr(e.ElseBranch)
        gen.emit(fmt.Sprintf("  br label %%%s", endLabel))

        // unify
        unifyTy := unifyTypes(tTy, eTy, gen)

        tReg = castIfNeeded(tReg, tTy, unifyTy, gen)
        eReg = castIfNeeded(eReg, eTy, unifyTy, gen)

        gen.emit(fmt.Sprintf("%s:", endLabel))
        phi := gen.newTemp()
        gen.emit(fmt.Sprintf("  %s = phi %s [%s, %%%s], [%s, %%%s]",
            phi, unifyTy, tReg, thenLabel, eReg, elseLabel))
        return phi, unifyTy

    case *ast.WhileExpr:
        loopLabel := gen.newTemp()[1:]
        bodyLabel := gen.newTemp()[1:]
        doneLabel := gen.newTemp()[1:]

        gen.emit(fmt.Sprintf("  br label %%%s", loopLabel))
        gen.emit(fmt.Sprintf("%s:", loopLabel))

        cReg, cTy := gen.genExpr(e.Condition)
        cReg = castIfNeeded(cReg, cTy, "i32", gen)

        cmp := gen.newTemp()
        gen.emit(fmt.Sprintf("  %s = icmp ne i32 %s, 0", cmp, cReg))
        gen.emit(fmt.Sprintf("  br i1 %s, label %%%s, label %%%s", cmp, bodyLabel, doneLabel))

        gen.emit(fmt.Sprintf("%s:", bodyLabel))
        gen.genExpr(e.Body)
        gen.emit(fmt.Sprintf("  br label %%%s", loopLabel))

        gen.emit(fmt.Sprintf("%s:", doneLabel))
        // while => i32 0
        return "0", "i32"

    case *ast.BinaryExpr:
        lReg, lTy := gen.genExpr(e.Left)
        rReg, rTy := gen.genExpr(e.Right)
        // unify both to i32 for these ops
        lReg = castIfNeeded(lReg, lTy, "i32", gen)
        rReg = castIfNeeded(rReg, rTy, "i32", gen)

        switch e.Operator {
        case "+":
            tmp := gen.newTemp()
            gen.emit(fmt.Sprintf("  %s = add i32 %s, %s", tmp, lReg, rReg))
            return tmp, "i32"
        case "-":
            tmp := gen.newTemp()
            gen.emit(fmt.Sprintf("  %s = sub i32 %s, %s", tmp, lReg, rReg))
            return tmp, "i32"
        case "*":
            tmp := gen.newTemp()
            gen.emit(fmt.Sprintf("  %s = mul i32 %s, %s", tmp, lReg, rReg))
            return tmp, "i32"
        case "/":
            tmp := gen.newTemp()
            gen.emit(fmt.Sprintf("  %s = sdiv i32 %s, %s", tmp, lReg, rReg))
            return tmp, "i32"
        case "<":
            c := gen.newTemp()
            gen.emit(fmt.Sprintf("  %s = icmp slt i32 %s, %s", c, lReg, rReg))
            z := gen.newTemp()
            gen.emit(fmt.Sprintf("  %s = zext i1 %s to i32", z, c))
            return z, "i32"
        case "<=":
            c := gen.newTemp()
            gen.emit(fmt.Sprintf("  %s = icmp sle i32 %s, %s", c, lReg, rReg))
            z := gen.newTemp()
            gen.emit(fmt.Sprintf("  %s = zext i1 %s to i32", z, c))
            return z, "i32"
        case "=":
            c := gen.newTemp()
            gen.emit(fmt.Sprintf("  %s = icmp eq i32 %s, %s", c, lReg, rReg))
            z := gen.newTemp()
            gen.emit(fmt.Sprintf("  %s = zext i1 %s to i32", z, c))
            return z, "i32"
        default:
            gen.emit("; unrecognized op " + e.Operator)
            return "0", "i32"
        }

    case *ast.UnaryExpr:
        reg, ty := gen.genExpr(e.Operand)
        switch e.Operator {
        case "not":
            // unify => i32
            reg = castIfNeeded(reg, ty, "i32", gen)
            c := gen.newTemp()
            gen.emit(fmt.Sprintf("  %s = icmp eq i32 %s, 0", c, reg))
            z := gen.newTemp()
            gen.emit(fmt.Sprintf("  %s = zext i1 %s to i32", z, c))
            return z, "i32"
        case "isvoid":
            // unify => i8*
            if ty == "i32" {
                cast := gen.newTemp()
                gen.emit(fmt.Sprintf("  %s = inttoptr i32 %s to i8*", cast, reg))
                reg = cast
                ty = "i8*"
            }
            c := gen.newTemp()
            gen.emit(fmt.Sprintf("  %s = icmp eq i8* %s, null", c, reg))
            z := gen.newTemp()
            gen.emit(fmt.Sprintf("  %s = zext i1 %s to i32", z, c))
            return z, "i32"
        case "~":
            // unify => i32
            reg = castIfNeeded(reg, ty, "i32", gen)
            tmp := gen.newTemp()
            gen.emit(fmt.Sprintf("  %s = sub i32 0, %s", tmp, reg))
            return tmp, "i32"
        default:
            gen.emit("; unrecognized unary op " + e.Operator)
            return "0", "i32"
        }

    case *ast.NewExpr:
        // return 'null' as i8*
        return "null", "i8*"

    case *ast.DispatchExpr:
        return gen.genDispatch(e)

    case *ast.StaticDispatchExpr:
        gen.emit("; static dispatch not implemented")
        return "0", "i32"

    case *ast.CaseExpr:
        gen.emit("; case stub => pick 1st branch")
        gen.genExpr(e.Expr)
        if len(e.Cases) > 0 {
            return gen.genExpr(e.Cases[0].Body)
        }
        return "0", "i32"

    default:
        gen.emit("; unknown expr node")
        return "0", "i32"
    }
}

// genDispatch => produce (reg,ty). We'll unify param types as needed
func (gen *IRGenerator) genDispatch(e *ast.DispatchExpr) (string, string) {
    cReg, cTy := gen.genExpr(e.Caller)
    // we unify caller => i8*
    cReg = castIfNeeded(cReg, cTy, "i8*", gen)

    className := "Object" // fallback
    mk := gen.methodKey(className, e.Method)
    sig, ok := gen.methodSignatures[mk]
    if !ok {
        sig = MethodSig{ReturnType: "i32", ParamTypes: []string{"i8*"}}
    }
    funcName := "@" + mk

    var arglist []string
    // self => i8*
    arglist = append(arglist, "i8* "+cReg)
    for i, argE := range e.Arguments {
        aReg, aTy := gen.genExpr(argE)
        paramTy := "i32"
        if i+1 < len(sig.ParamTypes) {
            paramTy = sig.ParamTypes[i+1]
        }
        fixed := castIfNeeded(aReg, aTy, paramTy, gen)
        arglist = append(arglist, fmt.Sprintf("%s %s", paramTy, fixed))
    }

    callReg := gen.newTemp()
    typeList := strings.Join(sig.ParamTypes, ", ")
    argsStr := strings.Join(arglist, ", ")

    gen.emit(fmt.Sprintf("  %s = call %s (%s) %s(%s)",
        callReg, sig.ReturnType, typeList, funcName, argsStr))
    return callReg, sig.ReturnType
}

// unifyTypes => if one is i8* and the other is i8*, return i8*. If both i32, i32. Else default i8*.
func unifyTypes(t1, t2 string, gen *IRGenerator) string {
    if t1 == t2 {
        return t1
    }
    if t1 == "i8*" || t2 == "i8*" {
        return "i8*"
    }
    return "i32"
}

// castIfNeeded => if fromTy != toTy, produce either inttoptr or ptrtoint
func castIfNeeded(reg, fromTy, toTy string, gen *IRGenerator) string {
    if fromTy == toTy {
        return reg
    }
    // i32 => i8* => inttoptr
    if fromTy == "i32" && toTy == "i8*" {
        tmp := gen.newTemp()
        gen.emit(fmt.Sprintf("  %s = inttoptr i32 %s to i8*", tmp, reg))
        return tmp
    }
    // i8* => i32 => ptrtoint
    if fromTy == "i8*" && toTy == "i32" {
        tmp := gen.newTemp()
        gen.emit(fmt.Sprintf("  %s = ptrtoint i8* %s to i32", tmp, reg))
        return tmp
    }
    return reg
}
