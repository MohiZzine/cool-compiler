package utils

import (
	"fmt"
	"io/ioutil"
	"os"
	"path/filepath"
)

type Error struct {
	filePath string
}

func NewErrorLogger() *Error {
	dir, err := os.Getwd()
	if err != nil {
		dir = "."
	}
	filePath := filepath.Join(dir, ".temp_error_logger")
	return &Error{filePath: filePath}
}

func (l *Error) LogLine(line string) error {
	f, err := os.OpenFile(l.filePath, os.O_CREATE|os.O_WRONLY|os.O_APPEND, 0644)
	if err != nil {
		return fmt.Errorf("could not open logger file: %w", err)
	}
	if _, err := f.WriteString(line + "\n"); err != nil {
		f.Close()
		return fmt.Errorf("could not write to logger file: %w", err)
	}
	f.Close()
	
	//panic to stop the program
	return nil
}

func (l *Error) PrintErrors() error {
	content, err := ioutil.ReadFile(l.filePath)
	if err != nil {
		if os.IsNotExist(err) {
			fmt.Println("Success: no errors encountered")
			return nil
		}
		return fmt.Errorf("could not read logger file: %w", err)
	}
	if len(content) == 0 {
		fmt.Println("Success: no errors encountered")
	} else {
		fmt.Print(string(content))
	}
	return nil
}

func (l *Error) ErrorExists() (bool, error) {
	content, err := ioutil.ReadFile(l.filePath)
	if err != nil {
		if os.IsNotExist(err) {
			return false, nil
		}
		return false, fmt.Errorf("could not read logger file: %w", err)
	}
	return len(content) > 0, nil
}

func (l *Error) DeleteFile() error {
	if err := os.Remove(l.filePath); err != nil {
		if os.IsNotExist(err) {
			return nil
		}
		return fmt.Errorf("could not delete logger file: %w", err)
	}
	return nil
}
