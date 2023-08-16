       IDENTIFICATION DIVISION.
       PROGRAM-ID. StringLecture.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 first-name PIC X(10) VALUE "Armin".
       01 last-name PIC X(10) VALUE "Afa".
       01 full-name PIC X(20) VALUE SPACES.

       PROCEDURE DIVISION.
       STRING first-name ' ' last-name INTO full-name.
       DISPLAY full-name.
       UNSTRING full-name DELIMITED BY ' ' INTO first-name last-name.
       DISPLAY first-name.  

       DISPLAY FUNCTION LENGTH(first-name).
       DISPLAY FUNCTION UPPER-CASE(first-name).
       DISPLAY FUNCTION LOWER-CASE(first-name).

       STOP RUN.
            