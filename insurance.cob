       IDENTIFICATION DIVISION.
       PROGRAM-ID. insurance.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 AGE PIC 9(3).
       01 INSURANCE-TYPE PIC X(10).
       01 BASE-PREMIUM PIC 9(5) VALUE ZEROS.
       01 TOTAL-PREMIUM PIC 9(5) VALUE ZEROS.


       PROCEDURE DIVISION.
       BEGIN-insurance.

           DISPLAY "ENTER YOUR AGE: " WITH NO ADVANCING.
           ACCEPT AGE.

           DISPLAY "ENTER YOUR INSURANCE TYPE (LIFE/HEALTH): " 
           WITH NO ADVANCING.
           ACCEPT INSURANCE-TYPE.

           MOVE FUNCTION UPPER-CASE (INSURANCE-TYPE) TO INSURANCE-TYPE.

           IF INSURANCE-TYPE = 'LIFE' THEN
               MOVE 5000 TO BASE-PREMIUM
           ELSE IF INSURANCE-TYPE = 'HEALTH' THEN
               MOVE 3000 TO BASE-PREMIUM
           ELSE
               DISPLAY "INVALID INSURANCE TYPE"

           COMPUTE TOTAL-PREMIUM = BASE-PREMIUM + (AGE * 10).

           DISPLAY "YOUR TOTAL PREMIUM IS: " TOTAL-PREMIUM.

           STOP RUN.