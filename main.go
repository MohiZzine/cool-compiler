package main

import (
	"encoding/json"
	"flag"
	"fmt"
	"log"
	"io"
	"os"
	"path/filepath"

	"cool-compiler/irgen"
	"cool-compiler/lexer"
	"cool-compiler/parser"
	"cool-compiler/semantic"
)

// stringReader returns an io.Reader from a string.
func stringReader(s string) *stringReaderWrapper {
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
	sourceData, err := os.ReadFile(*inputFile)
	if err != nil {
		log.Fatalf("Error reading file: %v", err)
	}

	// Remove comments from the source using RemoveComments function.
	cleanSource, err := lexer.RemoveComments(stringReader(string(sourceData))) // FIXED
	if err != nil {
		log.Fatalf("Error processing comments: %v", err)
	}

	// Save the cleaned source to out/cleaned_source.cl.
	cleanedSourcePath := filepath.Join("out", "cleaned_source.cl")
	cleanedSourceFile, err := os.Create(cleanedSourcePath)
	if err != nil {
		log.Fatalf("Error creating file: %v", err)
	}
	defer cleanedSourceFile.Close()

	_, err = cleanedSourceFile.WriteString(cleanSource)

	// Create a new lexer using the cleaned source.
	lex := lexer.NewLexer(stringReader(cleanSource))

	// Create a parser using the lexer.
	p := parser.NewParser(lex)
	program := p.ParseProgram()

	// Ensure the "out" directory exists before writing output files.
	outDir := "out"
	if err := os.MkdirAll(outDir, os.ModePerm); err != nil {
		log.Fatalf("Failed to create output directory: %v", err)
	}

	// Save AST to JSON file
	astFilePath := filepath.Join(outDir, "ast.json")
	progfile, err := os.Create(astFilePath)
	if err != nil {
		log.Fatalf("Error creating file: %v", err)
	}
	defer progfile.Close()

	jsonProg, err := json.MarshalIndent(program, "", "  ")
	if err != nil {
		log.Fatalf("Error marshalling program: %v", err)
	}

	_, err = progfile.Write(jsonProg)
	if err != nil {
		log.Fatalf("Error writing AST JSON file: %v", err)
	}

	// Build the symbol table from the parsed AST.
	symbolTable := semantic.BuildSymbolTable(program)

	// Run semantic analysis (type checking, inheritance graph checks, etc.).
	semAnalyzer := semantic.NewSemanticAnalyzer(symbolTable)
	err = semAnalyzer.Analyze(program)
	if err != nil {
		log.Fatalf("Semantic analysis failed: %v", err)
	}
	fmt.Println("Semantic analysis succeeded. No errors found!")

	// Generate LLVM IR from the AST.
	irGen := irgen.NewIRGenerator()
	mod := irGen.Generate(program)
	if err != nil {
		log.Fatalf("IR generation failed: %v", err)
	}

	// Write the generated LLVM IR to out/out.txt.
	irPath := filepath.Join(outDir, "out.txt")
	outFile, err := os.Create(irPath)
	if err != nil {
		log.Fatalf("Failed to create output file: %v", err)
	}
	defer outFile.Close()

	_, err = outFile.WriteString(mod)
	if err != nil {
		log.Fatalf("Failed to write LLVM IR to output file: %v", err)
	}

	fmt.Printf("LLVM IR successfully generated and written to %s\n", irPath)
}
