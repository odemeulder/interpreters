# A study in interpreters
Based on this wonderful series of articles:

https://ruslanspivak.com/lsbasi-part1/

Two implementations:
1. One in Python, following Ruslan's example
2. One in Rust (because I want to learn how to build a compiler and learn rust at the same time.)

To run the rust compiler:

```bash 
cargo run program27.pas
```

To debug, change the log levels in `parser.rs` or in `node_visitor.rs`.

## Explanation

* lexer: reads the file and turns the text into tokens
* parser: contains the definitions of the AstNodes (Abstract Syntax Tree Nodes)
* node_visitor: contains the implementations to walk the various AstNodes
* interpreter: initiates the tree walking to execute/evaluate the program
* datum: defitions of return types, result of evaluation nodes
* call_stack: definition of the call stack + functions to access, push, pop and insert
* symbols: definitions of symbols (for semantic analysis)
* scope: call stack for semantic analysis
* semantic_analyzer: walks the program tree to perform semantic analysis (i.e. not syntax errors, but semantic errors)
* main: entry point
* lib: program
