       IDENTIFICATION DIVISION.
       PROGRAM-ID. FileHandling.
       AUTHOR. Armin A.

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
