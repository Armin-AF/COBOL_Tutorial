       IDENTIFICATION DIVISION.
       PROGRAM-ID. BinarySearch.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       
       01 TABLE-VALUES.
           05 VALUE-TABLE  OCCURS 10 ASCENDING KEY IS TABLE-ITEM.
               10 TABLE-ITEM      PIC 9(3) VALUE ZEROS.

       01 TABLE-POINTERS.
           05 LOW-POINTER        PIC 9(3) VALUE 1.
           05 HIGH-POINTER      PIC 9(3) VALUE 10.
           05 MID-POINTER       PIC 9(3) VALUE ZEROS.
           
       01 SEARCH-VALUE          PIC 9(3) VALUE ZEROS.
       01 FOUND-FLAG            PIC X(1) VALUE 'N'.
       01 TABLE-COUNTER        PIC 9(3) VALUE 1.

       
       PROCEDURE DIVISION.
       INITIALIZE-TABLE.
           PERFORM VARYING TABLE-COUNTER FROM 1 BY 1
           UNTIL TABLE-COUNTER > 10
               COMPUTE TABLE-ITEM(TABLE-COUNTER) = TABLE-COUNTER * 100
           END-PERFORM.


           
       GET-SEARCH-VALUE.
           DISPLAY 'Enter a value to search: ' WITH NO ADVANCING.
           ACCEPT SEARCH-VALUE.
           
       BINARY-SEARCH.
           PERFORM UNTIL LOW-POINTER > HIGH-POINTER
               COMPUTE MID-POINTER = (LOW-POINTER + HIGH-POINTER) / 2
               IF SEARCH-VALUE = VALUE-TABLE(MID-POINTER)
                   MOVE 'Y' TO FOUND-FLAG
                   EXIT PERFORM
               END-IF
               IF SEARCH-VALUE < VALUE-TABLE(MID-POINTER)
                   MOVE MID-POINTER TO HIGH-POINTER
               ELSE
                   MOVE MID-POINTER TO LOW-POINTER
               END-IF
           END-PERFORM.
           
       DISPLAY-RESULT.
           IF FOUND-FLAG = 'Y'
               DISPLAY 'Value found at position: ' MID-POINTER
           ELSE
               DISPLAY 'Value not found.'
           END-IF.
           
           STOP RUN.
