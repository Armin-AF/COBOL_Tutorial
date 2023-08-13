       IDENTIFICATION DIVISION.
       PROGRAM-ID. ArithmeticOperations.
       AUTHOR. Armin.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 NUM1               PIC 9(5) VALUE ZEROS.
       01 NUM2               PIC 9(5) VALUE ZEROS.
       01 RESULT-ADD         PIC 9(5) VALUE ZEROS.
       01 RESULT-SUBTRACT    PIC 9(5) VALUE ZEROS.
       01 RESULT-MULTIPLY    PIC 9(6) VALUE ZEROS.
       01 RESULT-DIVIDE      PIC 9(5) VALUE ZEROS.
       01 REMAINDER-VAL          PIC 9(5) VALUE ZEROS.


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
           
           DIVIDE NUM1 BY NUM2 
           GIVING RESULT-DIVIDE 
           REMAINDER REMAINDER-VAL.
           DISPLAY "Division Result: " RESULT-DIVIDE.
           DISPLAY "Remainder: " REMAINDER-VAL.


           STOP RUN.

