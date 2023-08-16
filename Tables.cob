       IDENTIFICATION DIVISION.
       PROGRAM-ID.  Tables.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 MONTH-NAMES.
         05 MONTH-NAME OCCURS 12 TIMES PIC X(10).
       01 SALES-TABLE.
         05 MONTHLY-SALES OCCURS 12 TIMES.
            10 DAILY-SALES OCCURS 31 TIMES PIC 9(5).
       01 NUM-TABLE.
         05 NUMS PIC 9 OCCURS 9 TIMES INDEXED BY NUMS-IDX.
       01 IDX PIC 9 VALUE 1.
       01 NUM-TO-FIND PIC 9 VALUE 6.
      

       PROCEDURE DIVISION.
           MOVE 'JANUARY' TO MONTH-NAME(1).
           MOVE 'FEBRUARY' TO MONTH-NAME(2).

           MOVE 1000 TO DAILY-SALES (1, 1).
           MOVE 2000 TO DAILY-SALES (1, 2).

           DISPLAY MONTH-NAME(1) ' ' MONTH-NAME(2).
           DISPLAY MONTH-NAME(1) '/' 1 '=' DAILY-SALES (1, 1).
           DISPLAY MONTH-NAME(2) '/' 1 '=' DAILY-SALES (1, 2).
           MOVE 1 TO NUMS (1).
           MOVE 2 TO NUMS (2).
           MOVE 3 TO NUMS (3).
           MOVE 4 TO NUMS (4).
           MOVE 5 TO NUMS (5).
           MOVE 6 TO NUMS (6).
           MOVE 7 TO NUMS (7).
           MOVE 8 TO NUMS (8).
           MOVE 9 TO NUMS (9).

           SET NUMS-IDX TO 1.

           SEARCH NUMS
             AT END DISPLAY 'NOT FOUND'
             WHEN NUMS (NUMS-IDX) = NUM-TO-FIND
               DISPLAY 'FOUND AT POSITION ' NUMS-IDX 
           END-SEARCH.


           STOP RUN.

       END PROGRAM Tables.


