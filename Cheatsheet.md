# COBOL Cheat Sheet

**Running COBOL on a Mac using the GnuCOBOL compiler and Visual Studio Code (VSCode) is a straightforward process. Let's dive in step-by-step:**

### Step 1: Install Homebrew (if not installed)

1. Open Terminal.
2. Enter the following command and follow the on-screen instructions:

```bash
/bin/bash -c "$(curl -fsSL <https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh>)"

```

### Step 2: Install GnuCOBOL

With Homebrew installed, you can easily get GnuCOBOL:

```bash
brew install gnu-cobol

```

Once the installation completes, you can check the installed version:

```bash
cobc --version

```

### Step 3: Install Visual Studio Code

If you haven't already installed VSCode, you can download it from the [official website](https://code.visualstudio.com/).

Alternatively, using Homebrew:

```bash
brew install --cask visual-studio-code

```

### Step 4: Install COBOL Extensions for VSCode

1. Open Visual Studio Code.
2. Go to Extensions (you can use the shortcut `Cmd+Shift+X`).
3. Search for "COBOL" in the marketplace.
4. Install an extension that seems suitable. One of the popular extensions is "COBOL Language Support" by Bitlang.

### Step 5: Write and Compile a COBOL Program

1. In VSCode, create a new file with a `.cob` or `.cbl` extension, say `hello.cob`.
2. Write a simple COBOL program:

```
       IDENTIFICATION DIVISION.
       PROGRAM-ID. HelloWorld.
       PROCEDURE DIVISION.
           DISPLAY 'Hello, World!'.
           STOP RUN.

```

1. Save the file.
2. Open Terminal in the directory containing your COBOL file.
3. Compile the COBOL program:

```bash
cobc -x -free hello.cob

```

Here:

- `x` makes the output executable.
- `free` indicates the use of free-format code, allowing for more flexibility in how you write your COBOL. If you're using fixed format, omit this option.
1. Run the compiled program:

```bash
./hello

```

You should see the output `Hello, World!`.

### Step 6: Optional - Configure VSCode for Easier Build & Run

If you're going to be working with COBOL frequently, consider configuring tasks in VSCode to compile and run COBOL with just a shortcut:

1. In VSCode, go to Terminal > Configure Tasks > Create `tasks.json` file from template.
2. Add a custom task for compiling COBOL:

```json
{
    "label": "Compile COBOL",
    "type": "shell",
    "command": "cobc",
    "args": ["-x", "-free", "${file}"],
    "group": {
        "kind": "build",
        "isDefault": true
    }
}

```

1. Save the file.
2. Now you can press `Cmd+Shift+B` in VSCode to compile your COBOL program directly.

### Conclusion

Now you're set up with a smooth COBOL development environment on your Mac! Remember that while writing and compiling might be easy, mastering COBOL requires understanding its unique paradigms and idiosyncrasies. Happy coding!

## **COBOL Syntax Overview Lecture**

COBOL, which stands for Common Business-Oriented Language, is a high-level programming language primarily used for business, finance, and administrative systems.

### **Basic Structure**:

A COBOL program has a hierarchical structure and is divided into four divisions:

1. **IDENTIFICATION DIVISION**: Contains basic program metadata.
2. **ENVIRONMENT DIVISION**: Specifies the program’s input, output, and other environmental dependencies.
3. **DATA DIVISION**: Defines all data items, records, and files used in the program.
4. **PROCEDURE DIVISION**: Contains the logic and procedures that operate on the data.

### Key Elements:

1. **Program Identification**:
    
    ```
    IDENTIFICATION DIVISION.
    PROGRAM-ID. YourProgramName.
    AUTHOR. YourName.
    
    ```
    
2. **Environment Setup**:
    - Specifies files and their attributes.
    
    ```
    ENVIRONMENT DIVISION.
    INPUT-OUTPUT SECTION.
    FILE-CONTROL.
        SELECT INPUT-FILE ASSIGN TO 'your-input-file.txt'.
        SELECT OUTPUT-FILE ASSIGN TO 'your-output-file.txt'.
    
    ```
    
3. **Data Definitions**:
    - `WORKING-STORAGE SECTION`: For defining temporary storage areas.
    - `FILE SECTION`: For defining file records.
    - Use of `PIC` (or `PICTURE`): Describes the structure of a data item.
        
        ```
        DATA DIVISION.
        WORKING-STORAGE SECTION.
        01 WS-AGE             PIC 9(2).     -- defines a two-digit number
        01 WS-NAME           PIC X(30).    -- defines a 30-character alphanumeric field
        
        ```
        
4. **Procedural Logic**:
    - COBOL statements are grouped into paragraphs, which can then be grouped into sections.
    - Basic operations: `MOVE`, `DISPLAY`, `ADD`, `SUBTRACT`, `MULTIPLY`, `DIVIDE`, etc.
        
        ```
        PROCEDURE DIVISION.
        A100-BEGIN.
            DISPLAY 'Hello, World!'.
            MOVE 'Jane' TO WS-NAME.
            DISPLAY 'Name is: ' WS-NAME.
        
        ```
        
5. **File Operations**:
    - Reading from and writing to files:
        
        ```
        OPEN INPUT INPUT-FILE.
        OPEN OUTPUT OUTPUT-FILE.
        READ INPUT-FILE INTO WS-RECORD AT END MOVE 'Y' TO WS-END-OF-FILE.
        WRITE WS-OUTPUT-RECORD TO OUTPUT-FILE.
        CLOSE INPUT-FILE, OUTPUT-FILE.
        
        ```
        
6. **Conditional Statements**:
    - `IF ... ELSE`, `EVALUATE` (similar to `switch` in other languages).
        
        ```
        IF WS-AGE > 18
            DISPLAY 'Adult'
        ELSE
            DISPLAY 'Minor'
        END-IF.
        
        ```
        
7. **Loops**:
    - `PERFORM` is used for loops.
        
        ```
        PERFORM VARYING WS-COUNTER FROM 1 BY 1 UNTIL WS-COUNTER > 10
            DISPLAY 'Counter: ' WS-COUNTER
        END-PERFORM.
        
        ```
        
8. **Comments**:
    - Any line can have a comment that starts in column 7 with an asterisk (``).
        
        ```
        * This is a comment
        
        ```
        
9. **Program Termination**:
    - `STOP RUN` or `EXIT PROGRAM` are used to terminate a program.
        
        ```
        STOP RUN.
        
        ```
        
10. **Calling Other Programs**:
    - COBOL supports modular programming. You can `CALL` another program and even pass data to it.
        
        ```
        CALL 'OTHERPROGRAM' USING WS-DATA-ITEM.
        
        ```
        

### Points to Note:

- COBOL is not case sensitive.
- COBOL syntax requires that certain statements start in specific columns, especially in older versions. This is less stringent in newer versions and depends on the compiler.
- Data definitions in the `DATA DIVISION` don't immediately allocate storage or initialize data. They merely define the format and layout of data.

### 1. Data Types

In COBOL, data types are represented by `PICTURE` or `PIC` clauses. The `PICTURE` clause specifies the exact format of a data item:

- **Alphabetic (A)**: Represents letters (A-Z, a-z).
- **Numeric (9)**: Represents numbers (0-9).
- **Alphanumeric (X)**: Represents numbers, letters, and special characters.
- **Decimal (V)**: Implied decimal point.
- **Sign (S)**: Indicates a signed number.

**Examples**:

```
01 ALPHA-ITEM                PIC A(5).        -- Holds 5 alphabetic characters.
01 NUMERIC-ITEM              PIC 9(3).        -- Holds a 3-digit number (e.g., 123).
01 ALPHANUM-ITEM             PIC X(10).       -- Holds 10 characters, be it letter, number, or special character.
01 DECIMAL-ITEM              PIC 9(3)V9(2).   -- Holds a number with 3 digits before the decimal and 2 after (e.g., 123.45).
01 SIGNED-ITEM               PIC S9(4).       -- Holds a signed 4-digit number (e.g., +1234 or -1234).

```

### 2. Variables

Variables are data items that can hold values. They are declared in the `DATA DIVISION`:

**Examples**:

```
DATA DIVISION.
WORKING-STORAGE SECTION.
01 EMPLOYEE-NAME             PIC X(30).
01 EMPLOYEE-ID               PIC 9(5).
01 EMPLOYEE-SALARY           PIC S9(5)V99.  -- e.g., -1234.56

```

**Setting and Displaying Variables**:

```
PROCEDURE DIVISION.
    MOVE 'John Doe' TO EMPLOYEE-NAME.
    MOVE '12345' TO EMPLOYEE-ID.
    MOVE 5000.50 TO EMPLOYEE-SALARY.

    DISPLAY 'Employee Name: ' EMPLOYEE-NAME.
    DISPLAY 'Employee ID: ' EMPLOYEE-ID.
    DISPLAY 'Employee Salary: $' EMPLOYEE-SALARY.

```

**Output**:

```
Employee Name: John Doe
Employee ID: 12345
Employee Salary: $5000.50

```

### 3. Constants

Constants in COBOL are values that don't change during the execution of the program. They can be numeric, alphanumeric, or symbolic. Symbolic constants are defined in the `WORKING-STORAGE SECTION` using the `VALUE` clause:

**Examples**:

```
DATA DIVISION.
WORKING-STORAGE SECTION.
01 MAX-EMPLOYEES             PIC 9(3)  VALUE 100.
01 COMPANY-NAME              PIC X(20) VALUE 'Tech Corp'.

```

You can then use these constants in your program, ensuring that the values don't change:

```
PROCEDURE DIVISION.
    DISPLAY 'Maximum number of employees: ' MAX-EMPLOYEES.
    DISPLAY 'Company: ' COMPANY-NAME.

```

**Output**:

```
Maximum number of employees: 100
Company: Tech Corp

```

**Note**: The constants defined with the `VALUE` clause in the `WORKING-STORAGE SECTION` should not be modified during the program's execution. If you try to `MOVE` another value to them, it'll overwrite the constant. While COBOL allows this, it's not best practice. If a data item's value needs to change, it should not be initialized with a `VALUE` clause and considered a variable instead.

## COBOL Arithmetic Operators:

COBOL supports various arithmetic operations using a different approach than many modern languages, making it unique. Here's an overview:

### 1. Basic Arithmetic Operations:

- **ADD**: Used to add numbers.
    
    ```
    ADD A TO B GIVING C.
    
    ```
    
    Alternatively:
    
    ```
    ADD A, B GIVING C.
    
    ```
    
    It adds the value of `A` to `B` and stores the result in `C`. If `GIVING` is not used, `B` would be replaced by the sum of `A` and `B`.
    
- **SUBTRACT**: Used to subtract numbers.
    
    ```
    SUBTRACT A FROM B GIVING C.
    
    ```
    
    It subtracts `A` from `B` and stores the result in `C`. If `GIVING` is not used, `B` would be replaced by the result of `B - A`.
    
- **MULTIPLY**: For multiplication.
    
    ```
    MULTIPLY A BY B GIVING C.
    
    ```
    
    Multiplies `A` and `B` and stores the result in `C`.
    
- **DIVIDE**: For division.
    
    ```
    DIVIDE A INTO B GIVING C REMAINDER D.
    
    ```
    
    Divides `A` by `B`, the quotient is stored in `C` and the remainder in `D`.
    

### 2. Compound Arithmetic:

COBOL also allows you to perform arithmetic operations on multiple operands at once. For example:

```
ADD A B C TO D E F.

```

This adds the values of `A`, `B`, and `C` to the corresponding values of `D`, `E`, and `F`, replacing the values of `D`, `E`, and `F` with the results.

### 3. ROUNDED phrase:

The `ROUNDED` phrase rounds off the intermediate or final result to the nearest whole number.

```
DIVIDE A BY B GIVING C ROUNDED.

```

This will round the result in `C` to the nearest whole number.

### 4. COMPUTE Statement:

The `COMPUTE` statement is more versatile and allows more conventional arithmetic expressions:

```
COMPUTE C = A + B * (D - E) / F.

```

The above statement calculates the value of the expression and assigns it to `C`.

### 5. On Size Error:

When working with arithmetic operations, it's possible for an overflow or other size-related errors to occur. COBOL provides the `ON SIZE ERROR` clause to handle such scenarios:

```
MULTIPLY A BY B GIVING C
    ON SIZE ERROR
        DISPLAY 'Error: Overflow occurred'.

```

If the result of the multiplication is too large for the receiving field (`C`), the `ON SIZE ERROR` clause is executed.

### Notes:

- The efficiency of arithmetic operations can sometimes depend on how data is defined in the `DATA DIVISION`, especially when considering computational (binary representation) vs. display formats.
- Always ensure that the variables involved in arithmetic operations are defined with appropriate sizes to prevent overflow or truncation issues.

### Conclusion:

Arithmetic operations in COBOL, while verbose in comparison to some other languages, provide a clear and detailed manner to perform calculations, making the logic easily understandable. As with any aspect of COBOL, regular practice and real-world application will help solidify these concepts.

**EXAMPLE**:  Here's a simple COBOL program that demonstrates the basic arithmetic operations:

```
       IDENTIFICATION DIVISION.
       PROGRAM-ID. ArithmeticOperations.
       AUTHOR. ChatGPT.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 NUM1               PIC 9(5) VALUE ZEROS.
       01 NUM2               PIC 9(5) VALUE ZEROS.
       01 RESULT-ADD         PIC 9(5) VALUE ZEROS.
       01 RESULT-SUBTRACT    PIC 9(5) VALUE ZEROS.
       01 RESULT-MULTIPLY    PIC 9(6) VALUE ZEROS.
       01 RESULT-DIVIDE      PIC 9(5) VALUE ZEROS.
       01 REMAINDER          PIC 9(5) VALUE ZEROS.

       PROCEDURE DIVISION.
       A000-MAIN-LOGIC.
           DISPLAY "COBOL Arithmetic Operations".

           MOVE 25 TO NUM1.
           MOVE 5 TO NUM2.

           DISPLAY "NUM1 = " NUM1.
           DISPLAY "NUM2 = " NUM2.

           ADD NUM1 TO NUM2 GIVING RESULT-ADD.
           DISPLAY "Addition Result: " RESULT-ADD.

           SUBTRACT NUM2 FROM NUM1 GIVING RESULT-SUBTRACT.
           DISPLAY "Subtraction Result: " RESULT-SUBTRACT.

           MULTIPLY NUM1 BY NUM2 GIVING RESULT-MULTIPLY.
           DISPLAY "Multiplication Result: " RESULT-MULTIPLY.

           DIVIDE NUM1 BY NUM2 GIVING RESULT-DIVIDE REMAINDER REMAINDER.
           DISPLAY "Division Result: " RESULT-DIVIDE.
           DISPLAY "Remainder: " REMAINDER.

           STOP RUN.

```

Explanation:

1. We declare the program's metadata in the `IDENTIFICATION DIVISION`.
2. In the `DATA DIVISION`, we declare our variables.
3. In the `PROCEDURE DIVISION`, we implement the logic. This program takes two numbers (25 and 5) and performs addition, subtraction, multiplication, and division on them.

Remember, while COBOL syntax may look long-winded, it was designed with the intent to be self-documenting and easy for non-programmers to read.

### COBOL Logical Operators

In COBOL, logical (or relational) operators are used to compare data items. Based on the results of these comparisons, a program can decide which set of instructions to execute next.

### 1. Basic Logical Operators:

- **EQUAL TO (`=`)**: Tests for equality between two items.
    
    ```
    IF A = B
        DISPLAY 'A is equal to B'
    END-IF.
    
    ```
    
- **GREATER THAN (`>`) and LESS THAN (`<`)**: These operators check if one item is greater than or less than another.
    
    ```
    IF SALARY > 50000
        DISPLAY 'High Salary'
    ELSE IF SALARY < 20000
        DISPLAY 'Low Salary'
    END-IF.
    
    ```
    
- **NOT EQUAL TO (`<>` or `NOT =`)**: Determines if two items are not equal.
    
    ```
    IF A <> B
        DISPLAY 'A is not equal to B'
    END-IF.
    
    ```
    
- **GREATER THAN OR EQUAL TO (`>=`) and LESS THAN OR EQUAL TO (`<=`)**:
    
    ```
    IF AGE >= 18
        DISPLAY 'Adult'
    END-IF.
    
    ```
    

### 2. Combining Conditions:

You can combine multiple conditions using the `AND` and `OR` operators:

- **AND**: All conditions combined using `AND` must be true for the overall condition to be true.
    
    ```
    IF AGE >= 18 AND GENDER = 'F'
        DISPLAY 'Adult Female'
    END-IF.
    
    ```
    
- **OR**: Only one of the conditions combined using `OR` needs to be true for the overall condition to be true.
    
    ```
    IF AGE < 18 OR GENDER = 'F'
        DISPLAY 'Either a minor or a female'
    END-IF.
    
    ```
    

### 3. Negation:

- **NOT**: This is used to negate or reverse a condition.
    
    ```
    IF NOT (AGE >= 18)
        DISPLAY 'Minor'
    END-IF.
    
    ```
    

### 4. Special Operators:

- **IS NUMERIC**: Checks if a data item contains only numeric characters.
    
    ```
    IF DATA-ITEM IS NUMERIC
        DISPLAY 'Data item contains only numbers'
    END-IF.
    
    ```
    
- **IS ALPHABETIC**: Checks if a data item contains only alphabetic characters (A-Z, a-z).
    
    ```
    IF DATA-ITEM IS ALPHABETIC
        DISPLAY 'Data item contains only letters'
    END-IF.
    
    ```
    

### 5. Class Tests:

You can also define custom classes for data in the `DATA DIVISION` and then use them in logical tests:

```
DATA DIVISION.
WORKING-STORAGE SECTION.
01 DATA-ITEM    PIC X(5) VALUE 'A123B'.
01 LETTERS      CLASS A THROUGH Z.
...

PROCEDURE DIVISION.
IF DATA-ITEM (1:1) IS LETTER
    DISPLAY 'The first character is a letter'
END-IF.

```

### Notes:

- Always be cautious of the data types and lengths you're comparing. For example, comparing a numeric stored as a character string to a pure numeric item can yield unexpected results.
- Logical operations often go hand in hand with conditional statements (`IF ... ELSE`).
- In COBOL, unlike some other languages, parentheses `()` are not mandatory but are recommended for clarity, especially when combining multiple conditions.

### Conclusion:

Logical operators in COBOL provide a rich set of tools for making decisions based on data. They are foundational for any business logic and are instrumental in guiding the flow of program execution. Understanding these operators deeply is crucial for anyone looking to master COBOL programming.

## COBOL Control Structures

### 1. Conditional Structures:

- **IF ... ELSE ... END-IF**:
    
    The `IF` statement is used to test a condition. If the condition is true, the statements following it are executed. If it's false, the program jumps to the `ELSE` part if present.
    
    ```
    IF AGE > 18
       DISPLAY 'Adult'
    ELSE
       DISPLAY 'Minor'
    END-IF.
    
    ```
    
    Note: The `ELSE` part is optional.
    
- **EVALUATE ... END-EVALUATE**:
    
    Equivalent to the `switch` or `case` statement in other languages. It's used for multi-way branching.
    
    ```
    EVALUATE TRUE
       WHEN AGE <= 12
          DISPLAY 'Child'
       WHEN AGE > 12 AND AGE <= 19
          DISPLAY 'Teenager'
       WHEN OTHER
          DISPLAY 'Adult'
    END-EVALUATE.
    
    ```
    

### 2. Iterative Structures:

- **PERFORM ... TIMES**:
    
    This repeats a set of statements a specified number of times.
    
    ```
    PERFORM VARYING I FROM 1 BY 1 UNTIL I > 5
       DISPLAY 'Iteration number: ' I
    END-PERFORM.
    
    ```
    
- **PERFORM ... UNTIL**:
    
    This repeats a block of code until a certain condition becomes true.
    
    ```
    PERFORM VARYING I FROM 1 BY 1 UNTIL I = 5
       DISPLAY 'Iteration number: ' I
    END-PERFORM.
    
    ```
    
- **PERFORM ... THRU**:
    
    This is used to execute a series of paragraphs or sections. It's a way to modularize code, much like calling functions in other languages.
    
    ```
    PERFORM PARAGRAPH-1 THRU PARAGRAPH-3.
    
    ```
    
    Where `PARAGRAPH-1`, `PARAGRAPH-2`, and `PARAGRAPH-3` are defined paragraphs in the `PROCEDURE DIVISION`.
    

### 3. Transfer of Control:

- **GO TO**:
    
    Allows for an unconditional jump to a specified paragraph or section. Though it's available, the use of `GO TO` is often discouraged as it can make the flow of the program hard to follow.
    
    ```
    IF ERROR-FLAG = 'Y'
       GO TO ERROR-HANDLER.
    
    ```
    
    Note: Excessive use of `GO TO` can lead to "spaghetti code", which is difficult to understand and maintain.
    
- **EXIT**:
    
    Used to leave a paragraph or section. Often used in conjunction with `PERFORM ... THRU`.
    
    ```
    PARAGRAPH-1.
       ...some code...
       EXIT.
    
    ```
    

### 4. Conditional Compilation:

- **COPY** and **REPLACING**:
    
    These are not strictly control structures, but they're worth mentioning. They allow for the inclusion of source code from other files and can be used to change certain parts of the copied code. This is particularly useful for handling different compilation scenarios or versions of a program.
    
    ```
    COPY 'SOMEFILE' REPLACING ==placeholder== BY ==actual-value==.
    
    ```
    

### Conclusion:

COBOL's control structures provide robust tools for guiding program execution, making decisions based on data, and iterating over sets of operations. While some of these constructs might seem verbose when compared to modern languages, they were designed to be clear and readable. As with all aspects of COBOL, understanding the language's control structures is crucial for writing efficient and maintainable code.

### EXAMPLE:

Here's a COBOL program that demonstrates most of the essential control structures:

```
       IDENTIFICATION DIVISION.
       PROGRAM-ID. ControlStructuresDemo.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 AGE                        PIC 99 VALUE 0.
       01 REPEAT-COUNTER             PIC 9(3) VALUE 0.
       01 NAME                       PIC X(10) VALUE SPACES.
       01 ERROR-FLAG                 PIC X VALUE 'N'.
       01 NUM                        PIC 9 VALUE 5.

       PROCEDURE DIVISION.
       BEGIN.

           DISPLAY 'COBOL Control Structures Demonstration'.

           MOVE 25 TO AGE.
           MOVE 'John' TO NAME.

           * IF structure
           IF AGE < 18
               DISPLAY 'Hello young ' NAME '!'
           ELSE
               DISPLAY 'Hello Mr./Ms. ' NAME '!'
           END-IF.

           * PERFORM...UNTIL structure
           PERFORM VARYING REPEAT-COUNTER FROM 1 BY 1 UNTIL REPEAT-COUNTER > 3
               DISPLAY 'This is loop iteration number: ' REPEAT-COUNTER
           END-PERFORM.

           * EVALUATE structure
           EVALUATE TRUE
               WHEN AGE < 20
                   DISPLAY NAME ' is a teenager.'
               WHEN AGE BETWEEN 20 AND 60
                   DISPLAY NAME ' is an adult.'
               WHEN OTHER
                   DISPLAY NAME ' is a senior citizen.'
           END-EVALUATE.

           * Using GO TO (Note: excessive use is discouraged)
           MOVE 'Y' TO ERROR-FLAG.
           IF ERROR-FLAG = 'Y'
               GO TO ERROR-HANDLER
           END-IF.

           PERFORM TEST-PARAGRAPH THRU TEST-PARAGRAPH-END.

           DISPLAY 'End of demonstration.'.
           STOP RUN.

       ERROR-HANDLER.
           DISPLAY 'An error has occurred!'.

       TEST-PARAGRAPH.
           DISPLAY 'This is a test paragraph!'.
       TEST-PARAGRAPH-END.
           DISPLAY 'End of test paragraph!'.

```

Explanation:

1. We declare several variables, including `AGE`, `NAME`, and `ERROR-FLAG`.
2. We then use an `IF...ELSE` structure to check `AGE` and display a different greeting based on the value.
3. The `PERFORM...UNTIL` structure is used to run a loop three times, displaying the iteration number.
4. An `EVALUATE` structure checks the age range and provides a suitable message.
5. We have a simple `GO TO` structure that jumps to the error handler when the `ERROR-FLAG` is set to `'Y'`.
6. Finally, we demonstrate the `PERFORM...THRU` structure by executing the `TEST-PARAGRAPH` and its related end paragraph.

This is a basic demonstration, but it gives you a clear idea of how these control structures function in COBOL.

## COBOL Tables (Arrays) Lecture:

### 1. **Introduction to Tables**:

- A table in COBOL is a collection of data items, usually of the same type, that can be accessed by an index.
- Tables allow for systematic organization and retrieval of a set of related data items.

### 2. **Defining Tables**:

- In the `DATA DIVISION`, tables are defined in the `WORKING-STORAGE SECTION` or `LOCAL-STORAGE SECTION`.
- Each table consists of a table name and a series of entries.
    
    ```
    01 STUDENT-TABLE.
       05 STUDENT-ENTRY   OCCURS 10 TIMES.
          10 STUDENT-NAME   PIC X(20).
          10 STUDENT-AGE    PIC 99.
    
    ```
    
    Here, `STUDENT-TABLE` contains 10 occurrences of `STUDENT-ENTRY`.
    

### 3. **Accessing Table Elements**:

- Individual elements of a table are accessed using their index, referred to as the table's "subscript".
    
    ```
    MOVE 'John' TO STUDENT-NAME(1).
    MOVE 23 TO STUDENT-AGE(1).
    
    ```
    
- COBOL does not provide bounds checking, meaning that accessing `STUDENT-NAME(11)` when your table size is 10 would not result in an error but might lead to unpredictable results.

### 4. **Variable-Length Tables (OCCURS DEPENDING ON)**:

- COBOL tables can be variable in length, using the `OCCURS...DEPENDING ON` clause.
    
    ```
    01 NUMBER-OF-STUDENTS   PIC 9(3) VALUE 0.
    01 STUDENT-TABLE.
       05 STUDENT-ENTRY   OCCURS 1 TO 100 TIMES DEPENDING ON NUMBER-OF-STUDENTS.
          10 STUDENT-NAME   PIC X(20).
          10 STUDENT-AGE    PIC 99.
    
    ```
    
    Here, the number of occurrences in `STUDENT-TABLE` will vary depending on the value in `NUMBER-OF-STUDENTS`.
    

### 5. **Multi-dimensional Tables**:

- COBOL supports multi-dimensional tables.
    
    ```
    01 GRADEBOOK-TABLE.
       05 STUDENT-ROW   OCCURS 10 TIMES.
          10 GRADE-ENTRY   OCCURS 5 TIMES.
              15 GRADE-VALUE   PIC 99.
    
    ```
    
    To assign a grade to the second student's third grade entry:
    
    ```
    MOVE 85 TO GRADE-VALUE(2,3).
    
    ```
    

### 6. **SEARCH & SEARCH ALL**:

- **SEARCH**: Used to search an element in a table linearly.
    
    ```
    SEARCH STUDENT-ENTRY
       AT END DISPLAY 'Student not found.'
       WHEN STUDENT-NAME(CURRENT) = 'Alice'
       DISPLAY 'Found Alice at position ' CURRENT '.'
    
    ```
    
    `CURRENT` is an implicit index used in `SEARCH`.
    
- **SEARCH ALL**: Used for binary search on a table (the table should be sorted).
    
    ```
    SEARCH ALL STUDENT-ENTRY
       AT END DISPLAY 'Student not found.'
       WHEN STUDENT-NAME(CURRENT) = 'Alice'
       DISPLAY 'Found Alice.'
    
    ```
    

### 7. **Points to Remember**:

- Always initialize tables before use.
- While COBOL does not have array bounds checking, some compilers provide extensions or options for this.
- For large tables, consider using files for storage and retrieval.

### Conclusion:

Tables in COBOL are essential when dealing with a collection of similar data items. Proper understanding and use of tables ensure efficient data handling, especially in business applications where vast amounts of structured data are the norm. Whether it's storing grades in a gradebook, items in an inventory, or transactions in a ledger, COBOL's table handling capabilities are up to the task.

### EXAMPLE:

Below is a COBOL program that demonstrates the key concepts of arrays (tables) in COBOL:

```
       IDENTIFICATION DIVISION.
       PROGRAM-ID. ArraysDemo.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 STUDENT-TABLE.
          05 STUDENT-ENTRY   OCCURS 5 TIMES.
             10 STUDENT-NAME   PIC X(20) VALUE SPACES.
             10 STUDENT-AGE    PIC 99 VALUE ZERO.
       01 INDEX-VAL            PIC 9(3) VALUE 0.
       01 STUDENT-COUNT       PIC 99 VALUE 5.
       01 SEARCHED-NAME       PIC X(20) VALUE 'Lucas'.

       PROCEDURE DIVISION.
       BEGIN.

           DISPLAY 'COBOL Arrays (Tables) Demonstration'.

           * Populate the array (table)
           MOVE 'John' TO STUDENT-NAME(1).
           MOVE 21 TO STUDENT-AGE(1).

           MOVE 'Alice' TO STUDENT-NAME(2).
           MOVE 23 TO STUDENT-AGE(2).

           MOVE 'Lucas' TO STUDENT-NAME(3).
           MOVE 20 TO STUDENT-AGE(3).

           MOVE 'Anna' TO STUDENT-NAME(4).
           MOVE 22 TO STUDENT-AGE(4).

           MOVE 'Bob' TO STUDENT-NAME(5).
           MOVE 24 TO STUDENT-AGE(5).

           * Display the table contents using a loop
           DISPLAY 'Student List:'.
           PERFORM VARYING INDEX-VAL FROM 1 BY 1 UNTIL INDEX-VAL > STUDENT-COUNT
               DISPLAY 'Student Name: ' STUDENT-NAME(INDEX-VAL) ', Age: ' STUDENT-AGE(INDEX-VAL)
           END-PERFORM.

           * Searching for a student named 'Lucas' in the table using SEARCH
           DISPLAY 'Searching for Lucas...'.
           SET INDEX-VAL TO 1.
           SEARCH STUDENT-ENTRY
               AT END DISPLAY 'Lucas not found.'
               WHEN STUDENT-NAME(INDEX-VAL) = SEARCHED-NAME
               DISPLAY 'Lucas found at position: ' INDEX-VAL
           END-SEARCH.

           STOP RUN.

```

Explanation:

1. **Table Definition**: `STUDENT-TABLE` is our table (array) consisting of `STUDENT-ENTRY` that occurs 5 times. Each entry has a `STUDENT-NAME` and `STUDENT-AGE`.
2. **Populating the Table**: We manually populate the table with student names and ages. In real-world scenarios, this data could be read from a file or another source.
3. **Displaying Table Contents**: We use a `PERFORM...VARYING` loop to iterate over the table and display its contents.
4. **Searching the Table**: We demonstrate the `SEARCH` verb to find a student named 'Lucas' in the table.

This program gives a basic introduction to tables in COBOL, including defining, populating, displaying, and searching tables.

## COBOL Procedures and Paragraphs Lecture:

### 1. **Introduction**:

- In COBOL, the `PROCEDURE DIVISION` is where the program's logic resides. Within this division, the code is organized into paragraphs and sections.
- A paragraph is a block of code that performs a specific function, while a section is a collection of related paragraphs.

### 2. **Paragraphs**:

- **Definition**: A paragraph is defined by giving it a name followed by a period and then the associated COBOL statements.
    
    ```
    READ-DATA.
        ACCEPT STUDENT-NAME.
        ACCEPT STUDENT-AGE.
    
    ```
    
- **Invocation**: To call a paragraph, you use its name as a verb in the `PROCEDURE DIVISION`.
    
    ```
    PERFORM READ-DATA.
    
    ```
    

### 3. **Sections**:

- **Definition**: A section is a group of related paragraphs. It is defined by giving it a name followed by the `SECTION` keyword and a period.
    
    ```
    STUDENT-INPUT SECTION.
    READ-DATA.
        ACCEPT STUDENT-NAME.
        ACCEPT STUDENT-AGE.
    VALIDATE-DATA.
        IF STUDENT-AGE > 100
           DISPLAY 'Error: Invalid age'.
    
    ```
    
- **Invocation**: To call a section, you can use the `PERFORM` verb:
    
    ```
    PERFORM STUDENT-INPUT.
    
    ```
    
    This would execute all the paragraphs within the `STUDENT-INPUT` section sequentially.
    

### 4. **PERFORM Variants**:

- **Inline PERFORM**: Repeats the code within a paragraph or section immediately.
    
    ```
    PERFORM VALIDATE-DATA 3 TIMES.
    
    ```
    
- **PERFORM THRU**: Used to perform a range of paragraphs from a start to an end.
    
    ```
    PERFORM READ-DATA THRU VALIDATE-DATA.
    
    ```
    

### 5. **EXIT**:

- The `EXIT` statement can be used to signify the end of a paragraph or section, especially when it's necessary to clarify the flow or when using `PERFORM...THRU`.
    
    ```
    READ-DATA.
        ACCEPT STUDENT-NAME.
        ACCEPT STUDENT-AGE.
        EXIT.
    
    ```
    
    Note: The `EXIT` statement in COBOL is a no-operation statement, meaning it doesn't perform any actual function other than serving as a place marker or point of reference.
    

### 6. **Points to Remember**:

- Use meaningful names for paragraphs and sections to enhance code readability.
- Use sections and paragraphs to modularize code and promote reuse.
- Ensure that the logic within a paragraph or section is closely related, ensuring each component has a singular, clear purpose.

### Conclusion:

Procedures in the form of paragraphs and sections are the backbone of COBOL programming, allowing for organized, modular, and maintainable code. Proper utilization of these constructs ensures that the code remains readable and easy to debug, especially in the extensive programs often found in the business and finance sectors.

By designing programs around the principles of modularity and functionality encapsulation, COBOL developers can ensure longevity and ease of maintenance for their applications.

## COBOL File Handling Lecture:

### 1. **Introduction**:

COBOL has built-in support for handling various types of files, including Sequential, Indexed, and Relative files. These files serve as a medium to store, retrieve, and update data.

### 2. **File Description**:

Before using a file, you must describe its structure in the `FILE SECTION` of the `DATA DIVISION`.

Example:

```
FILE SECTION.
FD  STUDENT-FILE.
01  STUDENT-RECORD.
    05 STUDENT-ID    PIC 9(5).
    05 STUDENT-NAME  PIC X(30).

```

Here:

- `FD` describes the file, named `STUDENT-FILE`.
- The record structure of the file is `STUDENT-RECORD`, which has two fields, `STUDENT-ID` and `STUDENT-NAME`.

### 3. **File Control**:

In the `FILE-CONTROL` paragraph of the `ENVIRONMENT DIVISION`, you'll specify the actual physical location of the file.

```
FILE-CONTROL.
SELECT STUDENT-FILE ASSIGN TO "STUDENT.DAT".

```

### 4. **File Operations**:

COBOL offers various verbs for file operations:

- **OPEN**: Opens a file for processing.
    
    ```
    OPEN INPUT STUDENT-FILE.
    
    ```
    
- **CLOSE**: Closes a file post-processing.
    
    ```
    CLOSE STUDENT-FILE.
    
    ```
    
- **READ**: Reads a record from a file.
    
    ```
    READ STUDENT-FILE INTO STUDENT-RECORD.
    
    ```
    
- **WRITE**: Writes a record to a file.
    
    ```
    WRITE STUDENT-RECORD.
    
    ```
    
- **REWRITE**: Modifies an existing record.
    
    ```
    REWRITE STUDENT-RECORD.
    
    ```
    
- **DELETE**: Removes a record from the file.
    
    ```
    DELETE STUDENT-RECORD.
    
    ```
    

### 5. **File Types**:

- **Sequential Files**: Data is stored in a sequence. To access a particular record, you'd need to traverse from the beginning.
- **Indexed Files**: These have an index to quickly locate records. COBOL uses the `ISAM` (Indexed Sequential Access Method) system for this.
- **Relative Files**: Records are accessed based on their relative position, allowing for direct access.

### 6. **Error Handling**:

During file operations, errors might occur. COBOL provides `AT END`, `INVALID KEY`, and `FILE STATUS` to handle such situations.

```
READ STUDENT-FILE INTO STUDENT-RECORD
  AT END DISPLAY 'End of File Reached'
  NOT AT END DISPLAY 'Record Retrieved Successfully'
END-READ.

```

### 7. **File Status**:

To check the status of file operations, COBOL provides a `FILE STATUS` clause.

```
FD STUDENT-FILE.
   FILE STATUS IS WS-FILE-STATUS.

WORKING-STORAGE SECTION.
01 WS-FILE-STATUS PIC XX.

```

After every file operation, you can check the value of `WS-FILE-STATUS`. For example, '00' means successful operation, while '10' signifies an end-of-file condition.

### Conclusion:

File handling in COBOL is vital due to the nature of business applications. It allows for systematic storage, retrieval, and update operations. By leveraging the file handling capabilities in COBOL, developers can create efficient and robust data processing systems.

To master file handling, you should practice with real-world scenarios, simulating business processes, and handling various data volumes and structures.

### EXAMPLES:

Here's a simple example of reading from a sequential file:

**1. Create a sample data file (`sample.dat`) with the following contents:**

```
001 John        5000
002 Alice       6000
003 Bob         5500

```

Each line represents an employee record with the following format:

- First 3 characters: Employee ID
- Next 10 characters: Employee Name
- Last 4 characters: Employee Salary

**2. Use the following COBOL program to read and display the data:**

```
       IDENTIFICATION DIVISION.
       PROGRAM-ID. FileHandling.
       AUTHOR. ChatGPT.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT EMPLOYEE-FILE ASSIGN TO 'sample.dat'
           ORGANIZATION IS SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD EMPLOYEE-FILE.
       01 EMPLOYEE-RECORD.
           05 EMP-ID            PIC 9(3).
           05 EMP-NAME          PIC X(10).
           05 EMP-SALARY        PIC 9(4).

       WORKING-STORAGE SECTION.
       01 END-OF-FILE          PIC X(1) VALUE 'N'.

       PROCEDURE DIVISION.
       A000-MAIN-LOGIC.
           OPEN INPUT EMPLOYEE-FILE.

           READ EMPLOYEE-FILE
               AT END MOVE 'Y' TO END-OF-FILE
           END-READ.

           PERFORM UNTIL END-OF-FILE = 'Y'
               DISPLAY 'Employee ID: ' EMP-ID
               DISPLAY 'Employee Name: ' EMP-NAME
               DISPLAY 'Employee Salary: $' EMP-SALARY
               DISPLAY '-----------------------------'

               READ EMPLOYEE-FILE
                   AT END MOVE 'Y' TO END-OF-FILE
               END-READ
           END-PERFORM.

           CLOSE EMPLOYEE-FILE.
           STOP RUN.

```

This program:

1. Defines the file EMPLOYEE-FILE that it will read from.
2. Defines the structure of the records in the file.
3. Reads and displays each record until it reaches the end of the file.

## 1. Relative Files

Relative files allow records to be accessed either sequentially or directly based on a relative record number. Each record has a unique relative record number that starts from 1. This number increases by 1 for each subsequent record.

**Example**:

Let's write a program to create a relative file and write some records into it:

```
IDENTIFICATION DIVISION.
PROGRAM-ID. RelativeFileExample.
ENVIRONMENT DIVISION.
INPUT-OUTPUT SECTION.
FILE-CONTROL.
    SELECT STUDENT-FILE ASSIGN TO 'STUDENTS.DAT'
           ORGANIZATION IS RELATIVE
           ACCESS MODE IS RANDOM
           RELATIVE KEY IS STUDENT-ID.

DATA DIVISION.
FILE SECTION.
FD STUDENT-FILE.
01 STUDENT-RECORD.
    05 STUDENT-ID           PIC 9(4).
    05 STUDENT-NAME         PIC X(20).
    05 STUDENT-GRADE        PIC X(1).

WORKING-STORAGE SECTION.
01 WS-END-OF-FILE             PIC X(1)   VALUE 'N'.
01 WS-RESPONSE-CODE           PIC 99     VALUE ZERO.

PROCEDURE DIVISION.
A000-START.
    OPEN OUTPUT STUDENT-FILE.

    MOVE 1 TO STUDENT-ID.
    MOVE 'John Doe' TO STUDENT-NAME.
    MOVE 'A' TO STUDENT-GRADE.
    WRITE STUDENT-RECORD INVALID KEY DISPLAY 'WRITE ERROR FOR JOHN'.

    MOVE 2 TO STUDENT-ID.
    MOVE 'Jane Smith' TO STUDENT-NAME.
    MOVE 'B' TO STUDENT-GRADE.
    WRITE STUDENT-RECORD INVALID KEY DISPLAY 'WRITE ERROR FOR JANE'.

    CLOSE STUDENT-FILE.

    STOP RUN.

```

## 2. Writing to Files

Writing to files in COBOL is done using the `WRITE` statement. You've already seen it in action in the relative file example above.

**Example**:

The below snippet writes a record to an OUTPUT file:

```
WRITE RECORD-NAME INVALID KEY DISPLAY 'WRITE ERROR'.

```

In the `INVALID KEY` clause, if there's an issue (for instance, writing a duplicate key in indexed files), the specified procedure or statements will be executed.

## 3. Handling Multiple Files Simultaneously

COBOL allows operations on multiple files simultaneously. This is useful, for example, when you have to merge two sorted files.

**Example**:

Let's consider we have two files: `FILEA` and `FILEB`, both containing sorted student names. We want to merge them into a third file, `FILEC`.

```
IDENTIFICATION DIVISION.
PROGRAM-ID. MultiFileHandling.

ENVIRONMENT DIVISION.
INPUT-OUTPUT SECTION.
FILE-CONTROL.
    SELECT FILEA ASSIGN TO 'FILEA.DAT'.
    SELECT FILEB ASSIGN TO 'FILEB.DAT'.
    SELECT FILEC ASSIGN TO 'FILEC.DAT'.

DATA DIVISION.
FILE SECTION.
FD FILEA.
01 REC-A                      PIC X(20).
FD FILEB.
01 REC-B                      PIC X(20).
FD FILEC.
01 REC-C                      PIC X(20).

WORKING-STORAGE SECTION.
01 END-OF-FILEA               PIC X(1) VALUE 'N'.
01 END-OF-FILEB               PIC X(1) VALUE 'N'.

PROCEDURE DIVISION.
A000-START.

    OPEN INPUT FILEA, FILEB.
    OPEN OUTPUT FILEC.

    READ FILEA
        AT END MOVE 'Y' TO END-OF-FILEA
    END-READ.
    READ FILEB
        AT END MOVE 'Y' TO END-OF-FILEB
    END-READ.

    PERFORM UNTIL END-OF-FILEA = 'Y' AND END-OF-FILEB = 'Y'
        IF REC-A <= REC-B OR END-OF-FILEB = 'Y'
            WRITE REC-C FROM REC-A
            READ FILEA
                AT END MOVE 'Y' TO END-OF-FILEA
            END-READ
        ELSE
            WRITE REC-C FROM REC-B
            READ FILEB
                AT END MOVE 'Y' TO END-OF-FILEB
            END-READ
        END-IF
    END-PERFORM.

    CLOSE FILEA, FILEB, FILEC.
    STOP RUN.

```

This program:

- Opens `FILEA` and `FILEB` for reading and `FILEC` for writing.
- Reads records from `FILEA` and `FILEB` and writes the lower value record to `FILEC`.
- Continues until both `FILEA` and `FILEB` have been fully read.
- Outputs a merged sorted file as `FILEC`.

**Note**: These examples are simplified for clarity. In a real-world application, you would likely need to add more error handling and additional logic.

## COBOL Debugging Techniques Lecture:

### 1. **Introduction**:

- Debugging is the process of identifying and fixing errors or abnormalities in a program.
- Given the often large and complex nature of COBOL programs, especially those in legacy systems, robust debugging techniques are crucial.

### 2. **Syntax and Compilation Errors**:

- **Syntax Errors**: These errors are the result of violations in the COBOL language structure. The COBOL compiler can easily catch them.
- **Compilation Errors**: Can arise due to incorrect compiler settings, misconfigurations, or missing files.
    
    **Solution**: Always carefully read the compiler output and logs to understand and rectify these errors.
    

### 3. **Runtime Errors**:

These are trickier as they occur while the program is running. Examples include accessing unavailable files, arithmetic overflows, or invalid data operations.

### 4. **COBOL Debugger Tools**:

Modern COBOL environments, like Micro Focus or IBM's COBOL for z/OS, come with integrated debugging tools that allow:

- Setting breakpoints
- Step-by-step execution
- Variable inspections
- Condition-based execution

**Tip**: Familiarize yourself with the debugger tool associated with your COBOL environment.

### 5. **Using DISPLAY**:

A classic, yet effective technique. The `DISPLAY` verb can be used to print variable values, execution steps, or any other information.

```
DISPLAY 'Value of STUDENT-ID: ' STUDENT-ID.

```

While this is a simple technique, overuse can clutter the program, so remember to remove unnecessary `DISPLAY` statements after debugging.

### 6. **Test Data and Test Cases**:

- **Always** have a set of test data and test cases.
- Test for boundary values, null values, and other edge cases.
- Automated testing frameworks, if available in your environment, can be incredibly beneficial.

### 7. **Logs and Dumps**:

- **Logs**: Many COBOL environments provide detailed logs of program executions. These logs can be invaluable in understanding the sequence of operations and failures.
- **Memory Dumps**: In critical failures, the system might produce a memory dump, which is a snapshot of the program's memory at the time of the crash. Analyzing dumps requires more expertise but can provide deep insights into issues.

### 8. **Static Code Analysis**:

Some modern tools can analyze COBOL code without executing it to identify potential issues, bad practices, or areas of improvement.

### 9. **Review and Collaborate**:

Two pairs of eyes are better than one. Code reviews by peers can identify potential bugs or areas of improvement that the original developer might overlook.

### 10. **Documentation**:

Always refer to the available documentation, be it COBOL's official documentation or your system's specific documentation. Many 'bugs' arise due to a lack of understanding of a feature or function.

### Conclusion:

Debugging in COBOL, like in other languages, is a mix of systematic approaches, intuition, and experience. A good COBOL programmer is not just someone who can write code but someone who can efficiently debug and optimize it. Given the critical nature of many COBOL applications, effective debugging is not just a skill—it's a necessity.

## String Handling Lecture

String manipulation is essential in many COBOL programs, given the language's deep roots in business applications where textual data processing is frequent. COBOL offers several built-in features and verbs to handle strings efficiently.

### **1. STRING:**

The `STRING` verb is used to concatenate two or more data items or literals.

**Syntax**:

```
STRING source-1 [DELIMITED BY delimiter-1]
       source-2 [DELIMITED BY delimiter-2] ...
       INTO destination-string
       [WITH POINTER integer-pointer]
       [ON OVERFLOW imperative-statement-1]
       [NOT ON OVERFLOW imperative-statement-2]
       END-STRING.

```

**Example**:

```
DATA DIVISION.
WORKING-STORAGE SECTION.
01 FIRST-NAME         PIC X(10) VALUE 'John'.
01 LAST-NAME          PIC X(10) VALUE 'Doe'.
01 FULL-NAME          PIC X(25) VALUE SPACES.

PROCEDURE DIVISION.
STRING FIRST-NAME ' ' LAST-NAME INTO FULL-NAME.
DISPLAY FULL-NAME.
STOP RUN.

```

**Outcome**: `John Doe`

### **2. UNSTRING:**

The `UNSTRING` verb is used to split a string into multiple substrings based on specified delimiters.

**Syntax**:

```
UNSTRING source-string
        DELIMITED BY delimiter-1 [OR delimiter-2 ...]
        INTO destination-1 [COUNT IN integer-pointer-1]
             destination-2 [COUNT IN integer-pointer-2] ...
        [WITH POINTER integer-pointer]
        [ON OVERFLOW imperative-statement-1]
        [NOT ON OVERFLOW imperative-statement-2]
        END-UNSTRING.

```

**Example**:

```
DATA DIVISION.
WORKING-STORAGE SECTION.
01 FULL-NAME          PIC X(25) VALUE 'John, Doe'.
01 FIRST-NAME         PIC X(10) VALUE SPACES.
01 LAST-NAME          PIC X(10) VALUE SPACES.

PROCEDURE DIVISION.
UNSTRING FULL-NAME DELIMITED BY ','
        INTO FIRST-NAME, LAST-NAME.
DISPLAY FIRST-NAME LAST-NAME.
STOP RUN.

```

**Outcome**: `John Doe`

### **3. INSPECT:**

The `INSPECT` verb is used to count, replace, or convert characters in a string.

- **TALLYING**: Count occurrences of a character or substring.
- **REPLACING**: Replace occurrences of a character or substring.
- **CONVERTING**: Convert one set of characters to another.

**Example**:

```
DATA DIVISION.
WORKING-STORAGE SECTION.
01 MESSAGE            PIC X(25) VALUE 'Hello, COBOL! How are you?'.
01 TALLY-COUNT        PIC 9(3)  VALUE ZERO.

PROCEDURE DIVISION.
INSPECT MESSAGE TALLYING TALLY-COUNT FOR ALL 'O'.
DISPLAY 'Number of Os: ' TALLY-COUNT.

INSPECT MESSAGE REPLACING ALL 'O' BY 'A'.
DISPLAY MESSAGE.

STOP RUN.

```

**Outcome**:

```
Number of Os: 5
Hella, CABAL! Haw are yau?

```

### **Other String Functions**:

- **FUNCTION LENGTH (string)**: Returns the length of the string.
- **FUNCTION UPPER-CASE (string)**: Converts string to uppercase.
- **FUNCTION LOWER-CASE (string)**: Converts string to lowercase.

These functions and verbs make COBOL quite capable of handling and processing strings for various applications. 

## Tables and Multi-dimensional Array Lecture

Tables (arrays) in COBOL are a crucial part of the language, especially when dealing with batch processing of large datasets, which is common in many business applications. Let's delve into this topic.

### **1. Tables (Arrays) in COBOL**:

In COBOL, we use the term 'table' to refer to what many modern languages call an 'array'. A table allows you to define a series of items that all have the same data type.

**Example of a Simple Table**:

```
DATA DIVISION.
WORKING-STORAGE SECTION.
01 MONTH-NAMES.
   05 MONTH-NAME    PIC X(10) OCCURS 12 TIMES.

```

### **2. Initializing and Accessing Table Elements**:

You can set values for each item and access them using their index.

```
MOVE 'January'   TO MONTH-NAME(1).
MOVE 'February'  TO MONTH-NAME(2).
... and so on ...

DISPLAY MONTH-NAME(3).

```

### **3. Multi-dimensional Arrays**:

COBOL supports multi-dimensional tables.

**Example of a Two-dimensional Table**:

```
01 SALES-TABLE.
   05 MONTHLY-SALES   OCCURS 12 TIMES.
      10 DAILY-SALES  PIC 9(5) OCCURS 31 TIMES.

```

You can access elements with:

```
DISPLAY DAILY-SALES(5, 10).

```

This would display the sales on the 10th day of May.

### **4. SEARCH**:

The `SEARCH` verb is used to search an element in a one-dimensional table. It's similar to a linear search, where elements are searched one by one.

**Example**:

```
01 NUM-TABLE.
   05 NUMS       PIC 9 OCCURS 10 TIMES VALUE 1 2 3 4 5 6 7 8 9 10.
01 IDX           PIC 9 VALUE 1.
01 NUM-TO-FIND   PIC 9 VALUE 6.

PROCEDURE DIVISION.
SEARCH NUM-TABLE
   AT END DISPLAY 'Number not found'
   WHEN NUMS(IDX) = NUM-TO-FIND
      DISPLAY 'Number found at position: ' IDX
END-SEARCH.

```

### **5. SEARCH ALL**:

The `SEARCH ALL` verb is used for binary searching in a sorted table. For this to work, the table should be in ascending or descending order.

**Example**:

```
01 SORTED-TABLE.
   05 SORTED-NUMS   PIC 9(5) OCCURS 10 TIMES
       ASCENDING KEY IS SORTED-NUMS
       VALUE 10 20 30 40 50 60 70 80 90 100.
01 NUM-TO-FIND   PIC 9(5) VALUE 60.

PROCEDURE DIVISION.
SEARCH ALL SORTED-TABLE
   AT END DISPLAY 'Number not found'
   WHEN SORTED-NUMS = NUM-TO-FIND
      DISPLAY 'Number found'
END-SEARCH.

```

**Note**: Always ensure your table is sorted before using `SEARCH ALL`.

### **6. VARYING clause with SEARCH**:

This allows you to iterate over a table's elements.

```
SEARCH NUM-TABLE VARYING IDX
   AT END DISPLAY 'Number not found'
   WHEN NUMS(IDX) = NUM-TO-FIND
      DISPLAY 'Number found at position: ' IDX
END-SEARCH.

```

### **Conclusion**:

Tables in COBOL, combined with verbs like `SEARCH` and `SEARCH ALL`, allow for efficient storage and retrieval of data, especially in business applications where one might need to process large datasets. Understanding how to use these constructs effectively will enhance your capabilities as a COBOL developer.

## Report Generation Lecture:

Generating reports is a fundamental task in many COBOL applications, especially given the language's historical importance in business and finance operations. COBOL's **Report Writer** is a tool to automate many of the tasks associated with report generation.

### **1. Components of Report Writer**:

The main components of the Report Writer are:

- **Report Section**: This is where the report layouts are defined.
- **RD (Report Description) entry**: Defines characteristics for the report.
- **FD (File Description) entry**: Describes the input data file.
- **Report Writer Control System (RWCS)**: Interacts with the Data Division and the Procedure Division to produce reports.

### **2. Report Description (RD) Entry**:

The RD entry describes the report's general characteristics, like its layout, control breaks, footing lines, and so on.

**Example**:

```
RD REPORT-1
   PAGE LIMITS 60
   HEADING 1
   FOOTING 2
   FIRST DETAIL 3
   LAST DETAIL 58
   CONTROL IS STUDENT-LEVEL.

```

### **3. Report Groups**:

Report groups are used to define the layout of the report. They can be of type **DETAILED**, **CONTROL**, **HEADING**, or **FOOTING**.

**Example**:

```
01 TYPE-1-HEADING.
   03 LINE   1.
   03 COL    1 VALUE 'Student Report'.

01 STUDENT-LINE.
   03 LINE   +1.
   03 COL    1  PIC X(20)    SOURCE STUDENT-NAME.
   03 COL   22  PIC 9(3)     SOURCE STUDENT-SCORE.

01 TYPE-1-FOOTING.
   03 LINE   +1.
   03 COL    1  VALUE 'End of Report'.

```

### **4. Generate Report**:

After defining the RD entry and the report groups, you can generate the report using the `GENERATE` verb.

**Example**:

```
PROCEDURE DIVISION.
...
   OPEN INPUT STUDENT-FILE
   OPEN OUTPUT REPORT-1

   READ STUDENT-FILE
      AT END SET END-OF-FILE TO TRUE
   END-READ

   PERFORM UNTIL END-OF-FILE
      GENERATE STUDENT-LINE
      READ STUDENT-FILE
         AT END SET END-OF-FILE TO TRUE
      END-READ
   END-PERFORM

   CLOSE STUDENT-FILE, REPORT-1.

```

### **5. Control Breaks**:

You can use control breaks to change the output format when a control field's value changes. Control fields are specified in the RD entry using the `CONTROL IS` clause.

In the example above, if `STUDENT-LEVEL` changes, you can use the `SUPPRESS` or `PRINT` statements to manage report line output.

**Example**:

```
IF STUDENT-LEVEL NOT = LAST-STUDENT-LEVEL
   SUPPRESS PRINTING OF STUDENT-LINE
   GENERATE TYPE-1-FOOTING
   GENERATE TYPE-1-HEADING
END-IF

```

### **Conclusion**:

COBOL's Report Writer provides a structured and high-level approach to report generation. By defining the report layout and characteristics in the `DATA DIVISION` and using verbs like `GENERATE`, `SUPPRESS`, and `PRINT` in the `PROCEDURE DIVISION`, you can automate and customize the generation of detailed business reports.

It's also worth noting that, while Report Writer is powerful and integral to COBOL, there are other tools and technologies available in modern contexts for more intricate reporting needs, which might be used in tandem with COBOL in contemporary environments.

## Calling External Subprograms Lecture:

The concept of external subprograms (or subroutines) is a powerful way to modularize COBOL applications. By breaking a large program into smaller, more manageable subprograms, you can improve maintainability, promote code reuse, and simplify testing.

### **1. Basics of External Subprograms**:

In COBOL, a subprogram is a self-contained piece of code that can be invoked by a main program or another subprogram. The calling program is often referred to as the "calling program," while the invoked program is the "called program" or "subprogram."

### **2. The `CALL` Verb**:

To invoke an external subprogram, you use the `CALL` verb followed by the program's name or a data item containing the program's name.

**Syntax**:

```
CALL 'subprogram-name' USING parameter-1 parameter-2 ... parameter-n.

```

### **3. The `LINKAGE SECTION`**:

This section of the `DATA DIVISION` is crucial when working with subprograms. The `LINKAGE SECTION` defines data items that a subprogram will receive from or return to the calling program. These data items are passed via the `USING` phrase of the `CALL` statement.

For instance, if you have:

```
LINKAGE SECTION.
01 LS-PARAM-1  PIC X(10).
01 LS-PARAM-2  PIC 9(5).

```

You can access these parameters within the subprogram if they are passed using the `CALL` statement's `USING` phrase.

### **4. Example**:

**Main Program**:

```
IDENTIFICATION DIVISION.
PROGRAM-ID. MainProg.
DATA DIVISION.
WORKING-STORAGE SECTION.
01 WS-VAR1  PIC X(10) VALUE 'Hello'.
01 WS-VAR2  PIC 9(5)  VALUE 100.
...
PROCEDURE DIVISION.
    CALL 'SubProg' USING WS-VAR1 WS-VAR2.
    STOP RUN.

```

**Subprogram**:

```
IDENTIFICATION DIVISION.
PROGRAM-ID. SubProg.
DATA DIVISION.
LINKAGE SECTION.
01 LS-VAR1  PIC X(10).
01 LS-VAR2  PIC 9(5).
...
PROCEDURE DIVISION USING LS-VAR1 LS-VAR2.
    DISPLAY LS-VAR1.
    DISPLAY LS-VAR2.
    EXIT PROGRAM.

```

### **5. Return Values**:

Subprograms can also return values to the calling program. For this, you'd use the `RETURNING` clause.

**Syntax**:

```
CALL 'subprogram-name' USING parameter-1 RETURNING result-parameter.

```

### **6. Dynamic Calls vs. Static Calls**:

- **Static Calls**: The called program is bound to the calling program during the compile time. This means the called program becomes part of the load module or executable.
- **Dynamic Calls**: The called program is not bound to the calling program during compile time. Instead, it's loaded at runtime. This allows for more flexibility, as you can replace or modify the called program without recompiling the calling program.

### **7. Tips**:

- Always ensure that the parameters in the `CALL` statement and the `LINKAGE SECTION` of the subprogram match in order, number, and data type.
- Dynamic calls can be more flexible, but they might introduce slight overheads because of the runtime binding.

### **Conclusion**:

Calling external subprograms allows for modularized COBOL applications, facilitating better code organization, maintenance, and reusability. By understanding the `CALL` verb, the `LINKAGE SECTION`, and the nuances between static and dynamic calls, you can create efficient and organized COBOL applications.

## Using Intrinsic Functions Lecture:

COBOL's intrinsic functions provide built-in functionality that simplifies common operations. They're similar to functions in other programming languages and can be used within COBOL statements for various tasks.

### **1. Basics of Intrinsic Functions**:

Intrinsic functions in COBOL return values based on input arguments, and they can be invoked without the need for a separate procedure call. Their results can be used directly within arithmetic or conditional expressions.

### **2. Categories**:

COBOL intrinsic functions are categorized based on their functionality:

- **Mathematical Functions**: Deal with arithmetic operations.
- **String Handling Functions**: Handle string manipulations.
- **Date, Time, and Interval Functions**: For operations related to date and time.
- **Statistical Functions**: Provide statistical calculations.
- **Miscellaneous Functions**: Functions that don't fit neatly into the above categories.

### **3. Key Intrinsic Functions**:

### **Mathematical Functions**:

- **`ABS`**: Returns the absolute value of a numeric argument.
    
    ```
    COMPUTE ABS_VALUE = FUNCTION ABS(-5)  --> ABS_VALUE becomes 5.
    
    ```
    
- **`SQRT`**: Returns the square root of a numeric argument.
    
    ```
    COMPUTE ROOT_VALUE = FUNCTION SQRT(9)  --> ROOT_VALUE becomes 3.
    
    ```
    

### **String Handling Functions**:

- **`UPPER-CASE` and `LOWER-CASE`**: Converts a string to upper or lower case.
    
    ```
    COMPUTE UC_TEXT = FUNCTION UPPER-CASE('cobol')  --> UC_TEXT becomes 'COBOL'.
    
    ```
    
- **`REVERSE`**: Reverses a string.
    
    ```
    COMPUTE REV_TEXT = FUNCTION REVERSE('cobol')  --> REV_TEXT becomes 'loboc'.
    
    ```
    
- **`TRIM`**: Removes trailing spaces.
    
    ```
    COMPUTE TRIM_TEXT = FUNCTION TRIM('cobol   ')  --> TRIM_TEXT becomes 'cobol'.
    
    ```
    

### **Date, Time, and Interval Functions**:

- **`CURRENT-DATE`**: Returns the current system date and time.
    
    ```
    DISPLAY FUNCTION CURRENT-DATE.
    
    ```
    
- **`DATE-OF-INTEGER`**: Converts an integer to a date.
    
    ```
    COMPUTE MY_DATE = FUNCTION DATE-OF-INTEGER(2000).
    
    ```
    
- **`INTEGER-OF-DATE`**: Converts a date to an integer.
    
    ```
    COMPUTE INT_DATE = FUNCTION INTEGER-OF-DATE(MY_DATE).
    
    ```
    

### **Statistical Functions**:

- **`MAX` and `MIN`**: Returns the maximum or minimum value from a list of arguments.
    
    ```
    COMPUTE MAX_VALUE = FUNCTION MAX(5, 10, 15)  --> MAX_VALUE becomes 15.
    
    ```
    

### **Miscellaneous Functions**:

- **`LENGTH`**: Returns the length of a string or table item.
    
    ```
    COMPUTE STR_LEN = FUNCTION LENGTH('cobol')  --> STR_LEN becomes 5.
    
    ```
    
- **`NUMVAL` and `NUMVAL-C`**: Convert alphanumeric strings to numeric values.
    
    ```
    COMPUTE NUM = FUNCTION NUMVAL('123.45')  --> NUM becomes 123.45.
    
    ```
    

### **4. Function Options**:

Some functions provide additional options. For instance, the `TRIM` function can be directed to trim leading spaces instead:

```
COMPUTE TRIM_TEXT = FUNCTION TRIM('   cobol' LEADING)  --> TRIM_TEXT becomes 'cobol'.

```

### **Conclusion**:

Intrinsic functions in COBOL simplify many tasks that would otherwise require additional code to achieve. These functions help in streamlining the code, making it more readable, and reducing the potential for errors. It's beneficial to familiarize oneself with these functions, as they can significantly enhance productivity and code clarity.

## Handling Dynamic Memory Lecture:

Handling dynamic memory is a crucial topic in modern programming. While early versions of COBOL didn't support dynamic memory allocation, modern implementations have incorporated these capabilities, allowing for more flexible and efficient memory usage.

### **1. Basics of Dynamic Memory in COBOL**:

Dynamic memory allocation refers to the process of reserving memory space during runtime. This contrasts with static memory allocation, where the memory size is fixed at compile time. By using dynamic memory, programs can adapt to varying data sizes and only use memory when needed, thus conserving resources.

### **2. The `ALLOCATE` Verb**:

The `ALLOCATE` verb is used to allocate memory during runtime.

**Syntax**:

```
ALLOCATE memory-pointer

```

Where `memory-pointer` is a pointer data type that will reference the allocated memory.

For instance:

```
01 PTR USAGE POINTER.
...
ALLOCATE PTR.

```

To allocate an area of a specific size or type:

```
01 MY-DATA.
   05 DATA-LEN   PIC 9(4) COMP.
   05 DATA-VAL   PIC X(100).
...
ALLOCATE MY-DATA LENGTH OF DATA-VAL
       RETURNING PTR.

```

### **3. The `FREE` Verb**:

To release memory that was dynamically allocated, you use the `FREE` verb.

**Syntax**:

```
FREE memory-pointer

```

Example:

```
FREE PTR.

```

### **4. Working with Allocated Memory**:

Once memory is allocated, you can use the SET verb to associate a data structure with the allocated memory.

```
01 DATA-AREA.
   05 NAME      PIC X(30).
   05 AGE       PIC 9(2).

01 PTR USAGE POINTER.

ALLOCATE DATA-AREA RETURNING PTR.
SET ADDRESS OF DATA-AREA TO PTR.

```

Now, the `DATA-AREA` group item is associated with the dynamically allocated memory, and you can use it as you would with statically allocated memory.

### **5. Error Handling**:

Errors can occur during memory allocation. It's crucial to handle these cases to prevent unexpected behaviors.

```
01 MY-DATA.
   05 DATA-LEN   PIC 9(4) COMP VALUE 100.
   05 DATA-VAL   PIC X(100).
01 PTR USAGE POINTER.

ALLOCATE MY-DATA LENGTH OF DATA-VAL
       RETURNING PTR
       ON SIZE ERROR
          DISPLAY 'Memory allocation failed!'
       NOT ON SIZE ERROR
          DISPLAY 'Memory allocated successfully'.

```

### **6. Tips**:

- Always check for errors when allocating memory.
- Ensure that you release memory using `FREE` once you're done with it to avoid memory leaks.
- Dynamic memory allocation can be useful when dealing with unpredictable data sizes, such as reading files of unknown sizes.
- Avoid excessive fragmentation. Allocating and freeing small amounts of memory repeatedly can cause memory fragmentation.

### **Conclusion**:

Dynamic memory allocation in COBOL provides flexibility and efficiency. The ability to allocate memory based on runtime conditions makes applications more adaptable and can improve performance by conserving resources. However, with great power comes great responsibility. Always ensure you handle errors appropriately and manage memory wisely to prevent memory leaks and other issues.

## Object-Oriented COBOL Lecture:

Object-Oriented COBOL brings the power and flexibility of object-oriented programming (OOP) to the long-standing, traditional world of COBOL. It's an essential topic for anyone wanting to bridge the old with the new in the COBOL environment.

### **1. Basics of Object-Oriented Programming (OOP)**:

Object-Oriented Programming is a paradigm that uses "objects" – which can contain both data (in the form of fields, often known as attributes) and code (in the form of procedures, known as methods) – to design applications and computer programs.

The core concepts of OOP include:

- **Classes**: Templates for creating objects.
- **Objects**: Instances of classes.
- **Encapsulation**: Hiding the internal state and requiring all interaction to be performed through well-defined interfaces.
- **Inheritance**: Creating a new class from an existing class.
- **Polymorphism**: Allowing a single interface to represent different data types.

### **2. Object-Oriented COBOL: Classes and Objects**:

In Object-Oriented COBOL, classes are defined much like a traditional data structure, but with added capabilities:

```
CLASS-ID. Person PUBLIC.
DATA DIVISION.
WORKING-STORAGE SECTION.
01  Name       PIC X(30).
01  Age        PIC 9(3).
PROCEDURE DIVISION.
METHOD-ID. SetName.
ARGUMENTS NameValue PIC X(30).
MOVE NameValue TO Name.
END METHOD SetName.
...
END CLASS Person.

```

To instantiate an object from a class:

```
01 MyPerson OBJECT REFERENCE CLASS Person.
CREATE OBJECT MyPerson.

```

### **3. Encapsulation**:

Encapsulation is naturally handled in COBOL through the usual separation of the `DATA DIVISION` and `PROCEDURE DIVISION`. Data is typically private unless explicitly made public, and methods are used to manipulate that data.

### **4. Inheritance**:

Object-Oriented COBOL supports inheritance, allowing for the creation of subclasses that inherit properties and methods from a parent class:

```
CLASS-ID. Employee INHERITS FROM Person PUBLIC.
DATA DIVISION.
WORKING-STORAGE SECTION.
01  EmployeeNumber  PIC 9(5).
...
END CLASS Employee.

```

In the above, `Employee` is a subclass of `Person` and inherits its data and methods. You can also override methods in the subclass.

### **5. Polymorphism**:

Polymorphism in COBOL allows objects of different classes to be handled as objects of a common superclass:

```
CLASS-ID. Manager INHERITS FROM Employee.
...
END CLASS Manager.

01 AnEmployee OBJECT REFERENCE CLASS Employee.
01 AManager OBJECT REFERENCE CLASS Manager.

CREATE OBJECT AManager.
SET AnEmployee TO AManager.

```

Here, an object of the `Manager` class is treated as an object of the `Employee` class, demonstrating polymorphism.

### **6. Interface**:

Interfaces define a contract that classes can implement. They can contain method signatures but no implementation:

```
INTERFACE-ID. IPrintable.
METHOD-ID. "Print" ABSTRACT.
END METHOD "Print".
END INTERFACE IPrintable.

```

A class that implements the interface must provide an implementation for all its methods:

```
CLASS-ID. Document IMPLEMENTS IPrintable.
...
METHOD-ID. "Print".
...
END METHOD "Print".
...
END CLASS Document.

```

### **7. Tips**:

- While OOP in COBOL provides powerful capabilities, it's essential to use it judiciously. Not all problems are best solved using OOP.
- COBOL's OOP syntax might be verbose compared to languages like Java or C#, but it brings modern paradigms to a historically procedural language.
- Object-oriented design requires a different mindset than procedural programming. It's crucial to understand the concepts deeply to leverage them effectively.

### **Conclusion**:

Object-Oriented COBOL is a testament to COBOL's adaptability and continued evolution. It allows COBOL developers to use modern software design principles, making the language relevant in contemporary application development. Combining the power of OOP with COBOL's established strengths, developers can tackle complex problems in a modular, organized manner.

## Using Databases with COBOL Lecture:

Interacting with databases is one of the critical tasks many COBOL applications are designed for. COBOL's ability to process large volumes of data efficiently makes it a natural choice for database interactions.

### **1. Overview:**

When COBOL was first developed, databases as we know them today didn't exist. COBOL programs primarily processed flat files. As the technology landscape evolved, databases emerged, and COBOL had to evolve to interact with them.

### **2. Embedded SQL in COBOL**:

This is the most common method by which COBOL programs interact with relational databases. SQL statements are directly embedded in the COBOL program and are processed by a precompiler before the COBOL compiler sees the code. This precompiler transforms SQL into COBOL code.

Here's a basic structure:

```
EXEC SQL
   SQL command
END-EXEC.

```

### **2.1 Connecting to a Database:**

Before any SQL operations, you need to connect to a database:

```
EXEC SQL
   CONNECT TO :database-name
END-EXEC.

```

### **2.2 Fetching Data:**

```
WORKING-STORAGE SECTION.
01  EMPLOYEE-NAME        PIC X(30).
01  EMPLOYEE-ID          PIC 9(5).

...

EXEC SQL
   SELECT EMP_NAME, EMP_ID INTO :EMPLOYEE-NAME, :EMPLOYEE-ID
   FROM EMPLOYEES
   WHERE EMP_ID = '12345'
END-EXEC.

```

The colon (:) before a variable name indicates a host variable – a variable from the COBOL program being used in the SQL statement.

### **2.3 Inserting Data:**

```
EXEC SQL
   INSERT INTO EMPLOYEES (EMP_NAME, EMP_ID)
   VALUES (:EMPLOYEE-NAME, :EMPLOYEE-ID)
END-EXEC.

```

### **3. Cursors:**

When you need to retrieve multiple rows, you use a cursor. It's like an iterator in modern programming languages:

```
EXEC SQL
   DECLARE CURSOR_EMP CURSOR FOR
   SELECT EMP_NAME, EMP_ID
   FROM EMPLOYEES
END-EXEC.

EXEC SQL
   OPEN CURSOR_EMP
END-EXEC.

PERFORM UNTIL SQLCODE NOT = 0
   EXEC SQL
      FETCH CURSOR_EMP INTO :EMPLOYEE-NAME, :EMPLOYEE-ID
   END-EXEC

   DISPLAY EMPLOYEE-NAME, EMPLOYEE-ID
END-PERFORM.

EXEC SQL
   CLOSE CURSOR_EMP
END-EXEC.

```

### **4. Handling SQL Errors:**

Every SQL statement sets a special variable called `SQLCODE`. A value of 0 means the SQL operation was successful. A negative value indicates an error.

```
IF SQLCODE NOT = 0
   DISPLAY "Error encountered: ", SQLCODE
END-IF.

```

### **5. Middleware and COBOL**:

Apart from embedded SQL, COBOL can interact with databases using middleware solutions like CICS (Customer Information Control System) or IMS (Information Management System). These systems allow COBOL to interact with databases without embedding SQL directly.

### **6. Tips**:

- Always check `SQLCODE` after each SQL operation to ensure data integrity.
- Regularly COMMIT your operations to save changes and maintain database lock sanity.
- Use prepared statements if executing the same SQL command multiple times to optimize performance.
- When dealing with large datasets, be wary of resource consumption, and try to optimize SQL statements to fetch only the necessary data.

### **Conclusion**:

Database interactions are a cornerstone of many COBOL applications. Whether through embedded SQL or middleware solutions, COBOL's powerful data processing capabilities combined with database systems can handle vast amounts of information efficiently. As always, while the syntax and methodology might be unique, the underlying principles of data integrity, optimization, and error handling remain universal.

## COBOL in Modern Environments Lecture:

COBOL, despite its age, is still widely used in various industries, especially in sectors like banking, insurance, and other major transactional systems. Even in the modern era, where technologies change rapidly, COBOL remains a steady and reliable tool. However, the ways we interact with COBOL have evolved.

### **COBOL in Modern Environments**

### **1. Integration with Modern Languages:**

Modern applications often require integration between COBOL and newer languages like Java, C#, and Python. Middleware tools, such as Micro Focus Enterprise Server or IBM's CICS, can be used to facilitate this.

For example, Java can call COBOL code by employing JNI (Java Native Interface). Similarly, in the .NET ecosystem, COBOL can be compiled to the Intermediate Language, enabling COBOL routines to be called from languages like C#.

### **2. Web Services and COBOL:**

COBOL isn't limited to mainframes; it can also be used in web environments. With tools such as the Micro Focus Visual COBOL, you can create COBOL RESTful web services. This allows COBOL programs to be accessed over the web, making COBOL-backed services accessible to any system or platform that can make HTTP requests.

### **3. COBOL and Databases:**

Modern COBOL applications often interact with relational databases, like Oracle or SQL Server. This is done through Embedded SQL, where SQL queries are written directly in COBOL programs, or via middleware products.

### **4. COBOL and .NET:**

Micro Focus Visual COBOL for Visual Studio offers the ability to write COBOL programs that integrate seamlessly with the .NET framework. Here's a basic example:

```
       IDENTIFICATION DIVISION.
       PROGRAM-ID. HelloDotNet.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WELCOME-MESSAGE   PIC X(30) VALUE 'Hello from COBOL in .NET!'.

       PROCEDURE DIVISION.
           DISPLAY WELCOME-MESSAGE
           .
       STOP RUN.

```

This can be compiled and run in a .NET environment, and you can also reference other .NET libraries and use them in your COBOL code, making it easier to integrate with other .NET applications or services.

### **5. Modern Development Environments:**

Developers aren't limited to old terminals or outdated IDEs. Modern COBOL IDEs, like those provided by Micro Focus or IBM, offer features like code highlighting, auto-completion, and debuggers, much like any modern programming environment.

### **6. Version Control and COBOL:**

COBOL codebases can be and are often managed under version control systems like Git. This allows for modern CI/CD practices to be applied, ensuring efficient code lifecycle management.

### **Conclusion:**

While COBOL originated in a different era of computing, it has shown an impressive ability to adapt and stay relevant. Modern tools and methodologies allow COBOL to integrate seamlessly with contemporary systems, making it an enduring and indispensable asset in the IT landscape. Whether you're looking at web services, integrated databases, or integration with modern platforms like .NET, COBOL continues to serve its purpose reliably.

## Migration and Modernization Lecture:

Migration and modernization of legacy COBOL systems is a significant undertaking for many enterprises. The key goal is to preserve the core business logic while moving to more modern platforms, or even updating the way that business logic is implemented. Let's dive into this:

### **Migration and Modernization of COBOL Systems**

### **1. Reasons for Migration/Modernization:**

- **Technology Obsolescence**: Hardware and software on which COBOL systems run might become obsolete.
- **Cost Savings**: New platforms can offer cost efficiencies compared to mainframe operations.
- **Improved Agility**: Modern systems can be more adaptable to changing business requirements.
- **Integration with Modern Systems**: It might be easier to integrate with modern technologies post migration.

### **2. Types of Migration/Modernization:**

- **Platform Migration**: Moving COBOL applications from mainframes to distributed platforms (like UNIX, Linux, or Windows).
- **COBOL Version Upgrade**: Migrating from older versions of COBOL to newer ones.
- **Language Conversion**: Translating COBOL code into another high-level language, like Java or C#.
- **Database Migration**: Moving from legacy databases like IMS or VSAM to RDBMS like Oracle or SQL Server.

### **3. Considerations During Migration:**

- **Data Integrity**: Ensure that data is migrated accurately.
- **Business Logic**: Ensure that the business logic in the COBOL program remains intact after migration.
- **Performance**: The migrated system should ideally perform at par, if not better than, the legacy system.
- **User Experience**: If there are any UI components, they should be as usable as, or better than, the original.

### **4. Techniques and Tools:**

Several tools exist in the market to assist with COBOL migrations. For example:

- **Micro Focus Enterprise Developer**: Aids in migrating mainframe COBOL to distributed COBOL.
- **IBM Migration Toolkit**: Helps in transitioning from one database system to another.

### **Example: Migrating to a Newer Version of COBOL**

For simplicity, let’s consider a hypothetical COBOL program that we want to migrate from an old version of COBOL to a modern version.

```
       IDENTIFICATION DIVISION.
       PROGRAM-ID. OldVersionProg.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 OLD-VAR                PIC X(10) VALUE 'OLDVERSION'.
       PROCEDURE DIVISION.
       DISPLAY-MSG.
           DISPLAY OLD-VAR
           .
       STOP RUN.

```

Assuming newer versions of COBOL support more modern features or have different reserved words, this program might need changes during migration. A possible "modern" version might look like this:

```
       IDENTIFICATION DIVISION.
       PROGRAM-ID. NewVersionProg.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 NEW-VAR                PIC X(15) VALUE 'NEW VERSION V2'.
       PROCEDURE DIVISION.
       DISPLAY-MSG.
           DISPLAY NEW-VAR
           .
       STOP RUN.

```

This is an oversimplified example. In reality, migrations might involve addressing deprecated functions, changing file handling techniques, or adapting to new standards.

### **Conclusion:**

Migration and modernization are essential to keep legacy COBOL systems relevant and efficient in today's fast-evolving technological landscape. It's imperative to have a well-planned strategy, use the right set of tools, and ensure that the core business logic remains unchanged during this process.

## ****Error Handling and Exception Management + Performance Tuning lecture:****

### **Error Handling and Exception Management**

In COBOL, error handling and exception management ensure that your program can cope with unexpected situations gracefully. Proper error handling can mean the difference between a minor inconvenience and a significant system failure.

### **1. Typical Errors:**

- **I/O Errors**: Occur during file operations, like reading from or writing to a file.
- **Arithmetic Errors**: Such as division by zero.
- **Data Errors**: Like invalid data format or type mismatches.

### **2. COBOL's Approach:**

COBOL uses a combination of `FILE STATUS` and declaratives (`USE AFTER ERROR` or `USE FOR DEBUGGING`) to handle errors.

### **Example:**

```
       IDENTIFICATION DIVISION.
       PROGRAM-ID. ErrorHandler.
       DATA DIVISION.
       FILE SECTION.
       FD  SAMPLE-FILE.
       01  SAMPLE-RECORD       PIC X(50).
       WORKING-STORAGE SECTION.
       01  FILE-STATUS         PIC 99.

       PROCEDURE DIVISION.
       OPEN INPUT SAMPLE-FILE
           INVALID KEY
           DISPLAY "ERROR OPENING FILE. STATUS: " FILE-STATUS
           STOP RUN.

       READ SAMPLE-FILE
           AT END DISPLAY "END OF FILE REACHED"
           NOT AT END DISPLAY SAMPLE-RECORD
           INVALID KEY
           DISPLAY "ERROR READING FILE. STATUS: " FILE-STATUS
           STOP RUN.
       CLOSE SAMPLE-FILE.
       STOP RUN.

```

In the above code, file operations are paired with `INVALID KEY` clauses to detect errors. If an error occurs, the relevant message is displayed using the `DISPLAY` verb.

### **Performance Tuning**

Efficient and performant COBOL programs are crucial, especially when dealing with extensive data processing tasks typical in enterprise environments.

### **1. Common Performance Pitfalls:**

- **Inefficient Loops**: Nested loops, especially with file I/O, can be resource-intensive.
- **Unoptimized File Access**: Sequential access when direct or indexed access is possible can slow down operations.
- **Excessive I/O**: Too many individual reads/writes as opposed to block operations.

### **2. Tips and Techniques:**

- **Use Indexed Files for Frequent Access**: Indexed files provide faster access compared to sequential files.
- **Minimize I/O Operations**: Whenever possible, read/write data in blocks rather than one record at a time.
- **Optimize Arithmetic Operations**: Avoid computationally intensive operations inside tight loops.

### **Example:**

Consider a program that processes transactions on an account. If every transaction is processed by reading the account details from a file, updating it, and then writing it back, it's inefficient. Instead, it's better to read the account details once, process all transactions in memory, and then write back the updated account details.

```
       IDENTIFICATION DIVISION.
       PROGRAM-ID. PerfTuning.
       DATA DIVISION.
       FILE SECTION.
       FD  ACCOUNT-FILE.
       01  ACCOUNT-RECORD     PIC X(100).
       WORKING-STORAGE SECTION.
       01  TOTAL-TRANSACTIONS  PIC 9(5)  VALUE 0.
       01  TRANSACTION-VALUE   PIC 9(7)V99.

       PROCEDURE DIVISION.
       OPEN INPUT ACCOUNT-FILE.
       READ ACCOUNT-FILE INTO ACCOUNT-RECORD
           AT END DISPLAY "ALL ACCOUNTS PROCESSED".

       PERFORM VARYING TRANSACTION-VALUE FROM 1 BY 1 UNTIL TRANSACTION-VALUE > 1000
           ADD TRANSACTION-VALUE TO TOTAL-TRANSACTIONS
       END-PERFORM.

       CLOSE ACCOUNT-FILE.
       STOP RUN.

```

In this oversimplified example, rather than updating the `ACCOUNT-FILE` with every transaction, we accumulate the total and can update the file once at the end.

### **Conclusion:**

Both error handling and performance tuning are vital aspects of COBOL programming. Proper error management ensures resilience and user trust, while performance tuning ensures optimal resource usage and speed. Both aspects, when done right, ensure a smooth and efficient system.

## EXAMPLE:

A comprehensive COBOL program that encapsulates many of the topics.

The program will manage a basic library system where:

- Users can borrow and return books.
- Users and books have unique IDs.
- The system logs all borrow and return actions.

Please note: This is a basic implementation meant for illustrative purposes.

```
       IDENTIFICATION DIVISION.
       PROGRAM-ID. LibraryManagement.
       DATA DIVISION.
       FILE SECTION.

       FD  USER-FILE.
       01  USER-RECORD.
           05 USER-ID         PIC 9(5).
           05 USER-NAME       PIC X(20).

       FD  BOOK-FILE.
       01  BOOK-RECORD.
           05 BOOK-ID         PIC 9(5).
           05 BOOK-TITLE      PIC X(30).
           05 BOOK-AUTHOR     PIC X(20).

       FD  LOG-FILE.
       01  LOG-RECORD.
           05 LOG-USER-ID     PIC 9(5).
           05 LOG-BOOK-ID     PIC 9(5).
           05 LOG-ACTION      PIC X(10) VALUE SPACES.
           05 LOG-DATE        PIC 9(8).

       WORKING-STORAGE SECTION.
       01  WS-DATE             PIC 9(8).
       01  WS-ACTION-CODE      PIC 9 VALUE 0.
           88 BORROW           VALUE 1.
           88 RETURN           VALUE 2.

       PROCEDURE DIVISION.

       100-MAIN.
           DISPLAY "WELCOME TO LIBRARY MANAGEMENT SYSTEM".
           DISPLAY "1. BORROW BOOK".
           DISPLAY "2. RETURN BOOK".
           ACCEPT WS-ACTION-CODE.
           PERFORM 200-ACTION-PROCESS THRU 200-ACTION-EXIT.
           DISPLAY "THANK YOU FOR USING THE SYSTEM".
           STOP RUN.

       200-ACTION-PROCESS.
           DISPLAY "ENTER USER ID:".
           ACCEPT USER-ID FROM CONSOLE.
           DISPLAY "ENTER BOOK ID:".
           ACCEPT BOOK-ID FROM CONSOLE.
           PERFORM 300-VALIDATE-USER.
           PERFORM 400-VALIDATE-BOOK.
           MOVE FUNCTION CURRENT-DATE(1:8) TO WS-DATE.

           IF BORROW
               MOVE 'BORROW' TO LOG-ACTION
           ELSE IF RETURN
               MOVE 'RETURN' TO LOG-ACTION
           END-IF.

           WRITE LOG-RECORD.

       200-ACTION-EXIT.
           EXIT.

       300-VALIDATE-USER.
           READ USER-FILE KEY IS USER-ID
               INVALID KEY
               DISPLAY "INVALID USER!".
           NOT INVALID KEY
               DISPLAY "HELLO ", USER-NAME.
           END-READ.

       400-VALIDATE-BOOK.
           READ BOOK-FILE KEY IS BOOK-ID
               INVALID KEY
               DISPLAY "BOOK NOT FOUND!".
           NOT INVALID KEY
               DISPLAY "YOU SELECTED: ", BOOK-TITLE, " BY ", BOOK-AUTHOR.
           END-READ.

```

Here's a rundown of the features:

- **Files Used**:
    - **USER-FILE** holds information about users.
    - **BOOK-FILE** has details about books.
    - **LOG-FILE** captures borrow and return actions.
- **Error Handling**: The program validates user and book IDs. If an invalid ID is entered, it shows a relevant message.
- **COBOL Concepts**: This program integrates:
    - File Handling: Reading from and writing to files.
    - Conditional Logic: Using `IF` and `88` level conditions.
    - Procedural Logic: Making use of `PERFORM` to call paragraphs.
    - Data Manipulation: Moving values, using functions like `CURRENT-DATE`.

It's important to understand that in a real-world scenario, the program would be far more complex, encompassing more features, validations, and conditions. This example is designed to provide a foundational understanding.

## Mainframe

COBOL development, especially in its earlier days, was primarily done on mainframes. Thus, many COBOL developers often had to (and still have to) familiarize themselves with mainframe concepts. Here's a guide on the essential mainframe concepts:

### 1. **Mainframe Basics**:

- **Mainframe**: It's a high-performance computer used for large-scale processing. It supports numerous simultaneous tasks and handles vast volumes of data.
- **Big Iron**: A colloquial term for mainframes.
- **Batch Processing**: Refers to executing a series of non-interactive tasks (or jobs). These jobs are processed without manual intervention, often scheduled during off-peak hours.

### 2. **Mainframe Architecture**:

- **Central Processor Complex (CPC)**: Contains the main processor and memory.
- **Logical Partition (LPAR)**: A subset of a mainframe's resources, defined and managed as a logical unit. Allows multiple systems to run on a single machine.
- **Channel**: A connection between the mainframe and its peripherals (like storage). It handles I/O operations.

### 3. **Operating Systems**:

- **z/OS**: The primary OS for mainframes. Designed for enterprise applications.
- **z/VSE**: Another OS, suitable for smaller mainframes.
- **z/TPF**: Used mainly for real-time transaction processing.
- **z/VM**: Allows running multiple instances of z/OS, z/VSE, or Linux on a single mainframe.

### 4. **Job Control Language (JCL)**:

- Mainframe batch processing often requires a script, written in JCL, to specify steps for the job.
- Essential JCL statements:
    - **JOB**: Indicates the start of a job.
    - **EXEC**: Specifies a program or procedure to execute.
    - **DD**: Defines input and output sources.

### 5. **VSAM (Virtual Storage Access Method)**:

- It's a method for organizing, storing, and accessing data in mainframe datasets.
- Types:
    - **KSDS (Key Sequenced Data Set)**: Records are stored/accessed using a unique key.
    - **ESDS (Entry Sequenced Data Set)**: Records are appended sequentially.
    - **RRDS (Relative Record Data Set)**: Records are identified by their relative position.

### 6. **TSO/E (Time Sharing Option/Extensions)**:

- An interactive text-based interface to the mainframe.
- Allows users to issue commands, edit data, and submit JCL.

### 7. **ISPF (Interactive System Productivity Facility)**:

- Provides a graphical interface to manage mainframe datasets, submit JCL jobs, and more.

### 8. **CICS (Customer Information Control System)**:

- A transaction server that runs on mainframes. It manages online transaction processing.

### 9. **DB2**:

- A relational database system for mainframes. COBOL programs often interface with DB2 for CRUD (Create, Read, Update, Delete) operations.

### 10. **Mainframe Storage**:

- **DASD (Direct Access Storage Device)**: The primary storage medium for mainframes, like a hard drive in PCs.

### 11. **Mainframe Security**:

- **RACF (Resource Access Control Facility)**: Provides security features like user authentication and access control.

### 12. **SDLC on Mainframe**:

- Development often involves coding, compiling, and testing on the mainframe environment. Tools like **Endevor** help manage the software lifecycle on mainframes.

### 13. **Middleware**:

- Mainframes often interface with other platforms. Middleware like **MQSeries** allows for message-driven processing between different systems.

### 14. **Modern Mainframes**:

- Many legacy mainframes are being replaced or integrated with newer technologies. Understanding concepts like **WebSphere** (for integrating mainframe applications with web technologies) is essential.

### 15. **Mainframe in the Cloud Era**:

- With the advent of cloud computing, many companies are exploring hybrid models, combining the power of mainframes with the flexibility of cloud. Understanding how mainframes can be integrated into such environments is crucial.

Remember, while mainframes might seem old-fashioned in the era of cloud computing, they remain foundational in many industries. Their reliability, processing power, and extensive history mean they'll likely continue to be significant for some time. As a COBOL developer, understanding these mainframe concepts will enhance your ability to develop, debug, and optimize COBOL programs in mainframe environments.

## GitHub and Visual Studio Code for mainframe development

Certainly! GitHub and Visual Studio Code (VSCode) have transformed the way many developers work, even in mainframe environments. Integrating these modern tools with mainframe development offers a bridge between traditional development practices and contemporary DevOps culture. Here's how to use GitHub and Visual Studio Code for mainframe development:

### 1. **Understanding the Need**:

Many enterprises are striving to bring their mainframe operations into the fold of their DevOps initiatives. The aim is to speed up development cycles, improve code quality, and foster collaboration. GitHub offers version control, while VSCode provides a powerful and extensible IDE - both can play roles in modern mainframe development.

### 2. **Setting up Visual Studio Code**:

1. **Download and Install**:
    - Download VSCode from the official website.
    - Install the application.
2. **Extensions**:
    - VSCode's true power lies in its extensions. For mainframe development, you'd be interested in the "Zowe Explorer" extension. This connects VSCode to mainframe datasets, USS files, and jobs.
    - Install the Zowe Explorer from the Extensions pane.

### 3. **Setting up GitHub**:

1. **Create a Repository**:
    - If you don’t have a GitHub account, sign up.
    - Create a new repository for your COBOL projects.
2. **Clone the Repository**:
    - In your local environment, clone the repository using the command `git clone [repository_url]`.

### 4. **Integrate VSCode with GitHub**:

1. **Initialize Git**:
    - Open the cloned directory with VSCode.
    - Initialize a Git repository if not done already using the command `git init`.
2. **Commit and Push**:
    - After making changes to your COBOL code in VSCode, stage and commit those changes.
    - Push your changes to the GitHub repository using `git push`.

### 5. **Mainframe Integration with Zowe Explorer**:

1. **Configure Zowe Profile**:
    - Zowe is an open-source framework for mainframe integration. The Zowe Explorer extension in VSCode will allow you to interact with the mainframe datasets directly.
    - You need to create a Zowe CLI profile. This profile contains details about the mainframe connection.
2. **Access Mainframe Data**:
    - Once the profile is set up, you can access datasets, USS files, and jobs right from VSCode. You can also edit datasets and submit JCLs.
3. **Download and Upload Code**:
    - With the Zowe Explorer, you can download mainframe datasets, edit them in VSCode, and then upload the changes back to the mainframe.

### 6. **Workflow Example**:

1. **Edit Code in VSCode**:
    - Edit a COBOL program in VSCode.
2. **Commit Changes**:
    - Stage and commit the changes using Git commands within VSCode.
3. **Push to GitHub**:
    - Push the committed changes to your GitHub repository.
4. **Upload to Mainframe**:
    - Using Zowe Explorer, upload the edited COBOL program back to the mainframe dataset.
5. **Test on Mainframe**:
    - If necessary, you can then run the program on the mainframe, either through VSCode (with Zowe) or using your mainframe's native tools.

### 7. **Considerations and Best Practices**:

1. **Keep Sensitive Data Out of GitHub**:
    - Never store sensitive data, like mainframe credentials, in your GitHub repository. Use environment variables or config files that are in your .gitignore.
2. **Commit Frequently**:
    - Regular, smaller commits are better than occasional, larger ones. They're easier to manage, review, and rollback if necessary.
3. **Collaboration**:
    - Utilize features like pull requests for code reviews. This can foster collaboration and improve code quality.
4. **Backup**:
    - While GitHub is a form of backup, always ensure you have separate backups of your mainframe code and data.

Remember, while GitHub and VSCode offer modern tools for development, mainframes often have critical, legacy applications. Any changes should be made judiciously, with thorough testing before deployment.
