package main

import (
	"encoding/json"
	"flag"
	"fmt"
	"io"
	"io/ioutil"
	"log"
	"os"

	"cool-compiler/irgen"
	"cool-compiler/lexer"
	"cool-compiler/parser"
	"cool-compiler/semantic"
)

// stringReader returns an io.Reader from a string.
func stringReader(s string) io.Reader {
	return &stringReaderWrapper{s: s}
}

type stringReaderWrapper struct {
	s string
	i int64
}

func (r *stringReaderWrapper) Read(p []byte) (n int, err error) {
	if r.i >= int64(len(r.s)) {
		return 0, io.EOF
	}
	n = copy(p, r.s[r.i:])
	r.i += int64(n)
	return n, nil
}

func main() {
	// Define an input flag for the Cool source file.
	inputFile := flag.String("i", "", "Path to the input Cool source file")
	flag.Parse()

	if *inputFile == "" {
		log.Fatal("Please provide a Cool source file with -i flag")
	}

	// Read the file contents.
	sourceData, err := ioutil.ReadFile(*inputFile)
	if err != nil {
		log.Fatalf("Error reading file: %v", err)
	}

	// Remove comments from the source using your RemoveComments function.
	cleanSource, err := lexer.RemoveComments(stringReader(string(sourceData)))
	if err != nil {
		log.Fatalf("Error removing comments: %v", err)
	}

	// Create a new lexer using the cleaned source.
	lex := lexer.NewLexer(stringReader(cleanSource))

	// Create a parser using the lexer.
	p := parser.NewParser(lex)
	program := p.ParseProgram()

	// marshal program and save it to out/ast.json
	
	progfile, err := os.Create("out/ast.json")
	if err != nil {
		log.Fatalf("Error creating file: %v", err)
	}
	defer progfile.Close()

	jsonProg, err := json.MarshalIndent(program, "", "  ")
	if err != nil {
		log.Fatalf("Error marshalling program: %v", err)
	}

	_, err = progfile.Write(jsonProg)



	// Build the symbol table from the parsed AST.
	symbolTable := semantic.BuildSymbolTable(program)

	// Run semantic analysis (type checking, inheritance graph checks, etc.).
	semAnalyzer := semantic.NewSemanticAnalyzer(symbolTable)
	err = semAnalyzer.Analyze(program)
	if err != nil {
		fmt.Fprintf(os.Stderr, "Semantic analysis failed: %v\n", err)
		os.Exit(1)
	}
	fmt.Println("Semantic analysis succeeded. No errors found!")

	// Generate LLVM IR from the AST.
	irGen := irgen.NewIRGenerator()
	mod, err := irGen.GenerateModule(program)
	if err != nil {
		log.Fatalf("IR generation failed: %v", err)
	}

	// Write the generated LLVM IR to out/out.txt.
	outPath := "out/out.txt"
	outFile, err := os.Create(outPath)
	if err != nil {
		log.Fatalf("Failed to create output file: %v", err)
	}
	defer outFile.Close()

	_, err = outFile.WriteString(mod.String())
	if err != nil {
		log.Fatalf("Failed to write LLVM IR to output file: %v", err)
	}

	fmt.Printf("LLVM IR successfully generated and written to %s\n", outPath)
}
