       IDENTIFICATION DIVISION.
       
       PROGRAM-ID. GETSUM.

       DATA DIVISION.

       LINKAGE SECTION.

       *> Subroutines/ Linkage Section

          01  LNum1 PIC 9 VALUE 5.
          01  LNum2 PIC 9 VALUE 4.
          01  LSum1  PIC 99.



       PROCEDURE DIVISION USING LNum1, LNum2, LSum1.
           COMPUTE LSum1 = LNum1 + LNum2.


           EXIT PROGRAM.
           
       
          
       
