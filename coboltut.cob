       IDENTIFICATION DIVISION.
       *> In this section, you identify the program and the author.
       PROGRAM-ID.  coboltut.
       AUTHOR. Armin.
       DATE-WRITTEN. April 24 2023
       ENVIRONMENT DIVISION.
         *> In this section, you specify the configuration of your system.
       CONFIGURATION SECTION.
       *> In this section, you specify the special names that you will use in yo
       SPECIAL-NAMES. 
           CLASS PassingScore IS "A" THRU "C".

       DATA DIVISION.
         *> In this section, you define the data that you will use in your progr
       FILE SECTION. 
         *> In this section, you define the files that you will use in your prog
       WORKING-STORAGE SECTION. 
       *> In this section, you define the variables that you will use in your pr
       01 UserName PIC X(20) VALUE "Armin".
       01 Num1 PIC 9 VALUE ZEROS.
       01 Num2 PIC 9 VALUE ZEROS.
       01 Total PIC 99 VALUE 0.
       01 SSNum.
           02 SSArea PIC 999.
           02 SSGroup PIC 99.
           02 SSSerial PIC 9999.
       01 ans PIC S99V99 VALUE 0. *> S99V99 is a signed numeric with 2 decimal p
       01 Rem PIC 9V99 VALUE 0.

       01 Age PIC 99 VALUE 0.
       01 Grade PIC 99 VALUE 0.
       01 Score PIC X(1) VALUE "B".
       01 CanVoteFlag PIC 9 VALUE 0.
           88 CanVote VALUE 1.
           88 CannotVote VALUE 0.
       01 TestNumber PIC X.
           88 IsPrime VALUE "1", "2", "3", "5", "7".
           88 IsOdd VALUE "1", "3", "5", "7", "9".
           88 IsEven VALUE "0", "2", "4", "6", "8".
           88 LessThan5 VALUE "0", "1", "2", "3", "4".
           88 ANumber VALUE "0" THRU "9".


       PROCEDURE DIVISION.
         *> In this section, you write the statements that you will use in your 
           DISPLAY "What is your name?".
           ACCEPT UserName.
           DISPLAY "Hello " UserName.

           MOVE ZERO TO UserName.
           DISPLAY UserName.
           DISPLAY "Enter two numbers to add".
           ACCEPT Num1.
           ACCEPT Num2.
           COMPUTE Total = Num1 + Num2.
           DISPLAY Num1 " + " Num2 " = " Total.
           DISPLAY "Enter your social security number".
           ACCEPT SSNum.
           DISPLAY "Your Area Number is " SSArea.

           ADD Num1 TO Num2 GIVING ans.
           DISPLAY "The sum of " Num1 " and " Num2 " is " ans.
           SUBTRACT Num1 FROM Num2 GIVING ans.
           DISPLAY "The difference of " Num1 " and " Num2 " is " ans.
           MULTIPLY Num1 BY Num2 GIVING ans.
           DISPLAY "The product of " Num1 " and " Num2 " is " ans.
           DIVIDE Num1 BY Num2 GIVING ans.
           DISPLAY "The quotient of " Num1 " and " Num2 " is " ans.
           DIVIDE Num1 BY Num2 GIVING ans REMAINDER Rem.
           DISPLAY "The remainder of " Num1 " and " Num2 " is " Rem.

           COMPUTE ans = Num1 + Num2.
           DISPLAY "The sum of " Num1 " and " Num2 " is " ans.
           COMPUTE ans = Num1 - Num2.
           DISPLAY "The difference of " Num1 " and " Num2 " is " ans.
           COMPUTE ans = Num1 * Num2.
           DISPLAY "The product of " Num1 " and " Num2 " is " ans.
           COMPUTE ans = Num1 / Num2.
           DISPLAY "The quotient of " Num1 " and " Num2 " is " ans.
           COMPUTE ans = Num1 ** 2.
           DISPLAY "The square of " Num1 " is " ans.
           COMPUTE ans ROUNDED = 3.0 + 2.005.
           DISPLAY "The rounded sum of 3.0 and 2.005 is " ans.
           COMPUTE ans = FUNCTION RANDOM(100).
           DISPLAY "A random number between 0 and 100 is " ans.
           COMPUTE ans = FUNCTION ABS(-100).
           DISPLAY "The absolute value of -100 is " ans.
           COMPUTE ans = FUNCTION SQRT(100).
           DISPLAY "The square root of 100 is " ans.
           COMPUTE ans = FUNCTION LENGTH("Hello").
           DISPLAY "The length of the string 'Hello' is " ans.
           COMPUTE ans = FUNCTION NUMVAL("123").
           DISPLAY "The numeric value of the string '123' is " ans.
           COMPUTE ans = FUNCTION NUMVAL-C("123").
           DISPLAY "The numeric value of the string '123' is " ans.
           COMPUTE ans = FUNCTION ORD("A").
           DISPLAY "The ordinal value of the character 'A' is " ans.
           COMPUTE ans = FUNCTION DATE-OF-INTEGER(20230424).
           DISPLAY "The date of the integer 20230424 is " ans.

           DISPLAY "Enter your age: " WITH NO ADVANCING.
           ACCEPT Age.
           IF Age >= 18 THEN
               DISPLAY "You are old enough to vote."
           ELSE
               DISPLAY "You are not old enough to vote."
           END-IF

           DISPLAY "Enter your grade: " WITH NO ADVANCING.
           ACCEPT Grade.
           EVALUATE TRUE
             WHEN Grade < 60
                  DISPLAY "You failed."
             WHEN Grade < 70
                  DISPLAY "You got a D."
             WHEN Grade < 80
                  DISPLAY "You got a C."
             WHEN Grade < 90
                  DISPLAY "You got a B."
             WHEN Grade < 100
                  DISPLAY "You got an A."
             WHEN OTHER
                  DISPLAY "You got a perfect score."
           END-EVALUATE
           
           *> List of relational operators
           *> < LESS THAN
           *> > GREATER THAN
           *> <= LESS THAN OR EQUAL TO
           *> >= GREATER THAN OR EQUAL TO
           *> <> NOT EQUAL TO
           *> = EQUAL TO
           *> == EQUAL TO
           *> != NOT EQUAL TO
           *> AND
           *> OR

           *> Examples of Logical/Conditional Operators

           IF Age LESS THAN 5 THEN
               DISPLAY "You are a baby."
           ELSE IF Age LESS THAN 18 THEN
               DISPLAY "You are a child."
           ELSE IF Age LESS THAN 65 THEN
               DISPLAY "You are an adult."
           ELSE
               DISPLAY "You are a senior citizen."
           END-IF

           IF Age >= 18 AND Age < 65 THEN
               DISPLAY "You are an adult."
           ELSE
               DISPLAY "You are not an adult."
           END-IF

           IF Age < 18 OR Age >= 65 THEN
             DISPLAY "You are not an adult."
           ELSE
             DISPLAY "You are an adult."
           END-IF

           IF Score IS PassingScore THEN
               DISPLAY "You passed."
           ELSE
               DISPLAY "You failed."
           END-IF

           IF Score IS NOT PassingScore THEN
             DISPLAY "You failed."
           ELSE
             DISPLAY "You passed."
           END-IF

           *> NUMERIC ALPHABETIC ALPHABETIC-UPPER ALPHABETIC-LOWER ALPHANUMERIC
           IF Score IS NOT NUMERIC THEN
             DISPLAY "Score is not numeric."
           ELSE
             DISPLAY "Score is numeric."
           END-IF

           IF Age > 18 THEN
                SET CanVote TO TRUE
           ELSE
                SET CannotVote TO TRUE
           END-IF

           *> Evaluate statement
           DISPLAY "Enter single number or X to exit: "
           ACCEPT TestNumber
           PERFORM UNTIL NOT ANumber 
                EVALUATE TRUE
                WHEN IsPrime
                        DISPLAY "The number is prime."
                WHEN IsOdd
                        DISPLAY "The number is odd."
                WHEN IsEven
                        DISPLAY "The number is even."
                WHEN LessThan5
                        DISPLAY "The number is less than 5."
                WHEN OTHER
                        DISPLAY "The number is greater than 5."
                END-EVALUATE
                DISPLAY "Enter single number or X to exit: "
                ACCEPT TestNumber
            END-PERFORM




           STOP RUN.



