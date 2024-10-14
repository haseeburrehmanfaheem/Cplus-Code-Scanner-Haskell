# Cplus-Code-Scanner-Haskell

# C++ Code Scanner in Haskell

## Overview

This project is a simple C++ code scanner implemented in Haskell. The scanner reads a C++ source code file and produces a sequence of tokens and lexemes. These tokens are generated based on the syntax rules provided, including keywords, identifiers, operators, and more.

## Features

- Reads C++ code from a file provided by the user.
- Generates an output file in the format `<token, lexeme>` for each line of the input C++ code.
- Handles C++ code elements such as keywords, identifiers, operators, numbers, strings, and comments.
- Identifies errors in numbers not formatted in the base-7 system.

## How It Works

1. The user inputs the name of a text file containing C++ code.
2. The scanner reads the C++ code and parses it to extract tokens like keywords, identifiers, operators, etc.
3. The tokens are output in a text file named after the user's roll number in the format `<token, lexeme>`.
4. The project supports comments (both single and multi-line), and string literals enclosed in quotes.

## Example

### Input:
```cpp
int xY;
xY = 43;
```

### Output:
```cpp
<keyword, int>, <identifier, xY>, <operator, ;>
<identifier, xY>, <operator, =>, <intConstant, 43>
```
