# JSON-Compiler
This project presents a complete implementation of a JASON compiler in Python. The JASON language, which is case-insensitive, incorporates keywords, operators, and various statements to facilitate programming tasks. The project encompasses two phases: building a scanner (lexical analysis) and writing a parser (syntax analysis) for the language.

*Phase 1: Scanner for the JASON Programming Language*

The scanner is implemented using Python and follows the specified rules for JASON. It reads the source code and tokenizes it into meaningful units called **tokens**. The scanner handles case-insensitivity, recognizes reserved keywords in uppercase, identifies operators,and handles optional parentheses in CALL statements. It generates a sequence of tokens, each belonging to a specific token class, which will serve as input for the parser in the next phase.

*Phase 2: Parser for the JASON Programming Language*

In this phase, the grammar rules for the JASON language are defined using Python's syntax. The parser takes the tokens generated by the scanner and constructs a parse tree according to the grammar rules. The starting symbol, Program, is defined as the entry point of the parse tree. Other grammar rules, such as:
- Declare statement
- Assign statement
- Expression
- IF_Else Condition
- While_loop
- Function
- Read statement
- Write statement

These statements are implemented as separate parsing functions. These functions recursively build the parse tree by analyzing the token sequence and checking if it conforms to the grammar rules.The parser, implemented from scratch in Python, verifies the syntax and structure of the JASON source code. It checks for correct statement sequencing, variable declarations, assignment operations, control flow structures, and function calls. If the code passes the parser's scrutiny, it is considered syntactically valid, allowing subsequent stages like semantic analysis, optimization, and code generation.

The project's codebase serves as a valuable reference for those interested in language design, compiler construction, and programming language theory.
