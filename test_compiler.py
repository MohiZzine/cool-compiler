#!/usr/bin/env python3

import os
import subprocess

with open("test_results.log", "w") as log_file:
    log_file.write("")

def main():
    base_path = "./tests/errors_detection"  

    for item in os.listdir(base_path):
        folder_path = os.path.join(base_path, item)
        if os.path.isdir(folder_path):
            pload = f"""

==========================
Testing {item}
==========================

            """
            with open("test_results.log", "a") as log_file:
                log_file.write(pload)
            print(pload)
            for filename in os.listdir(folder_path):
                file_path = os.path.join(folder_path, filename)
                if os.path.isfile(file_path):
                    with open(file_path, "r", encoding="utf-8", errors="ignore") as f:
                        first_line = f.readline().rstrip("\n")
                        if first_line:

                            result = subprocess.run(["go", "run", "main.go", "-i", file_path], check=True, capture_output=True, text=True)
                            print(first_line)
                            print(result.stdout)
                            with open("test_results.log", "a") as log_file:
                                log_file.write(first_line + "\n")
                                log_file.write(result.stdout + "\n")


if __name__ == "__main__":
    main()
