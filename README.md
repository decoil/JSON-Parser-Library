# F# Parser Combinator Library

A lightweight, functional parser combinator library written in F# with a complete JSON parser implementation. This library provides a clean, composable way to build parsers using functional programming principles.

## Features

- **Parser Combinators**: Build complex parsers by combining simple ones
- **Position Tracking**: Detailed error reporting with line and column information
- **Monadic Interface**: Supports `>>=`, `<*>`, `<|>` and other functional operators
- **Complete JSON Parser**: Full JSON specification support including Unicode escapes
- **Extensive Primitives**: Built-in parsers for common patterns (strings, numbers, whitespace, etc.)

## Quick Start

### Basic Usage

```fsharp
#load "ParserLib.fsx"
open ParserLib

// Simple character parser
let parseA = pchar 'a'
let result = run parseA "abc"
// Success ('a', remaining input)

// Combine parsers
let parseAB = pchar 'a' .>>. pchar 'b'
let result2 = run parseAB "abc"
// Success (('a', 'b'), remaining input)
```

### JSON Parsing

```fsharp
#load "JSONparser.fsx"
open JSONparser

let jsonText = """
{
  "name": "John Doe",
  "age": 30,
  "isActive": true,
  "address": {
    "street": "123 Main St",
    "city": "Anytown"
  },
  "hobbies": ["reading", "swimming"]
}
"""

let result = run jValue jsonText
match result with
| Success (jsonValue, _) -> 
    printfn "Parsed successfully: %A" jsonValue
| Failure (label, error, pos) -> 
    printfn "Parse error at line %d, column %d: %s" pos.line pos.column error
```

## Core Types

### Parser Type
```fsharp
type Parser<'a> = {
    parseFn : (Input -> ParseResult<'a * Input>)
    label : ParserLabel
}
```

### Parse Results
```fsharp
type ParseResult<'a> =
    | Success of 'a
    | Failure of ParserLabel * ParserError * ParserPosition
```

### JSON Types
```fsharp
type JValue =
    | JString of string
    | JNumber of float
    | JBool of bool
    | JNull
    | JObject of Map<string, JValue>
    | JArray of JValue list
```

## Parser Combinators

### Basic Combinators

| Combinator | Description | Example |
|------------|-------------|---------|
| `pchar` | Parse a specific character | `pchar 'a'` |
| `pstring` | Parse a specific string | `pstring "hello"` |
| `satisfy` | Parse character matching predicate | `satisfy Char.IsDigit "digit"` |
| `anyOf` | Parse any character from list | `anyOf ['a'; 'b'; 'c']` |

### Combining Parsers

| Operator | Description | Example |
|----------|-------------|---------|
| `.>>.` | Parse both, return both | `pchar 'a' .>>. pchar 'b'` |
| `.>>` | Parse both, return first | `pchar 'a' .>> pchar 'b'` |
| `>>.` | Parse both, return second | `pchar 'a' >>. pchar 'b'` |
| `<\|>` | Try first, if fails try second | `pchar 'a' <\|> pchar 'b'` |
| `<*>` | Apply parser to function parser | `returnP (+) <*> pint <*> pint` |

### Repetition

| Function | Description | Example |
|----------|-------------|---------|
| `many` | Zero or more occurrences | `many (pchar 'a')` |
| `many1` | One or more occurrences | `many1 digitChar` |
| `sepBy` | Separated by delimiter | `sepBy pint (pchar ',')` |
| `opt` | Optional parser | `opt (pchar '-')` |

### Utility Functions

| Function | Description |
|----------|-------------|
| `between` | Parse between delimiters |
| `choice` | Choose from list of parsers |
| `sequence` | Parse sequence of parsers |
| `spaces` | Parse zero or more whitespace |
| `spaces1` | Parse one or more whitespace |

## Built-in Parsers

### Numbers
- `pint` - Parse integers (with optional minus sign)
- `pfloat` - Parse floating-point numbers
- `digitChar` - Parse single digit

### Strings
- `quotedString` - Parse quoted strings with escape sequences
- `manyChars` - Convert character parser to string parser
- `whitespaceChar` - Parse single whitespace character

## JSON Parser Features

The JSON parser supports the complete JSON specification:

- **Strings**: With full Unicode escape sequence support (`\uXXXX`)
- **Numbers**: Integer and floating-point with scientific notation
- **Booleans**: `true` and `false` literals
- **Null**: `null` literal
- **Arrays**: Nested arrays with proper comma separation
- **Objects**: Key-value pairs with string keys
- **Whitespace**: Flexible whitespace handling

### Escape Sequences Supported
- `\"` - Quote
- `\\` - Backslash  
- `\/` - Forward slash
- `\b` - Backspace
- `\f` - Form feed
- `\n` - Newline
- `\r` - Carriage return
- `\t` - Tab
- `\uXXXX` - Unicode code point

## Error Handling

The library provides detailed error information including:
- **Line and column numbers** for error location
- **Parser labels** for context about what was expected
- **Error messages** describing what went wrong
- **Visual error display** with caret pointing to error location

```fsharp
let result = run jValue "{ invalid json }"
printResult result
// Output:
// Line:0 Col:2 Error parsing object
// { invalid json }
//   ^Unexpected 'i'
```

## Examples

### Parse a List of Numbers
```fsharp
let numberList = 
    pchar '[' >>. 
    sepBy pint (pchar ',' .>> spaces) .>> 
    pchar ']'

let result = run numberList "[1, 2, 3, 4]"
```

### Parse Key-Value Pairs
```fsharp
let keyValue = 
    manyChars1 (satisfy Char.IsLetter "letter") .>> 
    pchar '=' .>>. 
    pint

let result = run keyValue "count=42"
```

### Custom Data Types
```fsharp
type Person = { Name: string; Age: int }

let personParser =
    pipe2 
        (quotedString .>> pchar ',')
        pint
        (fun name age -> { Name = name; Age = age })
```

## Installation

1. Copy `ParserLib.fsx` to your F# project
2. Copy `JSONparser.fsx` for JSON parsing functionality
3. Load the files using `#load` directive:

```fsharp
#load "ParserLib.fsx"
#load "JSONparser.fsx"  // Optional, for JSON parsing
```

## Contributing

Contributions are welcome! Please feel free to submit issues and enhancement requests.

## License

This project is open source. Please add your preferred license here.

## Architecture Notes

The library is built around these core concepts:

- **Input State Tracking**: Maintains position information for error reporting
- **Parser Composition**: Small parsers combine to create complex ones
- **Monadic Design**: Supports functional composition patterns
- **Forward References**: Handles recursive grammar rules (like JSON objects containing objects)

The JSON parser demonstrates advanced usage including forward references for recursive data structures and comprehensive string parsing with Unicode support.
