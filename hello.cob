       IDENTIFICATION DIVISION.
       PROGRAM-ID. hello.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-COUNTER PIC 9(2).
       01 EMPLOYEE-NAME PIC X(30).
       01 EMPLOYEE-AGE PIC 9(3).
       01 A PIC 9(3).
       01 B PIC 9(3).
       01 C PIC S9(3).
       01 D PIC 9(3)V9(2).
       PROCEDURE DIVISION.
           DISPLAY "Hello, world!".
           MOVE 0 TO WS-COUNTER.
           PERFORM VARYING WS-COUNTER FROM 1 BY 1 UNTIL WS-COUNTER > 10
           DISPLAY "Hello, world!" WS-COUNTER " times"
           ACCEPT EMPLOYEE-NAME FROM CONSOLE
           DISPLAY "Hello, " EMPLOYEE-NAME
           MOVE 25 TO EMPLOYEE-AGE
           DISPLAY "Hello, " EMPLOYEE-NAME " you are " EMPLOYEE-AGE " years old"
           DISPLAY "Enter first number"
           ACCEPT A FROM CONSOLE
           DISPLAY "Enter second number"
           ACCEPT B FROM CONSOLE
           COMPUTE C = A + B
           DISPLAY "The sum of " A " and " B " is " C
           SUBTRACT A FROM B GIVING C
           DISPLAY "The difference of " A " and " B " is " C
           COMPUTE C = A * B
           DISPLAY "The product of " A " and " B " is " C
           DIVIDE A INTO B GIVING C REMAINDER D
           DISPLAY "The quotient of " A " and " B " is " C " with a remainder of " D

           END-PERFORM.
           STOP RUN.