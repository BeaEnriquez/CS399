      ******************************************************************
      * Author:
      * Date:
      * Purpose:
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. ENRIQUEZ.
       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.
       01  WS-TYPE-OF-FRAME    PIC X(10).
       01  WS-LENGTH-AND-WIDTH    PIC 9(3).
       01  WS-FRAME-COLOR       PIC X(10).
       01  WS-NUMBER-OF-CROWNS     PIC 9(3).

       01  WS-AMOUNT-OF-FRAMES     PIC 999V99.
       01  WS-AMOUNT-OF-CROWNS     PIC 999V99.
       01  WS-AMOUNT-OF-FRAME-COLOR    PIC 999V99.
       01  WS-AMOUNT-OF-CB         PIC 999V99.
       01  WS-AMOUNT-OF-G         PIC 999V99.

       01  WS-TOTAL-PURCHASED-AMOUNT   PIC 999V99.

       01  WS-CHOICE-CB PIC X(1).
       01  WS-CHOICE-G PIC X(1).
       PROCEDURE DIVISION.
       MAIN-PROCEDURE.

            DISPLAY "The length and width, in inches, of the picture:"
            ACCEPT WS-LENGTH-AND-WIDTH
            DISPLAY "The type of the frame:"
            ACCEPT WS-TYPE-OF-FRAME
            DISPLAY "Choice of color to color the frame:"
            ACCEPT WS-FRAME-COLOR
            DISPLAY "Enter the number of crowns:"
            ACCEPT WS-NUMBER-OF-CROWNS

            DISPLAY "Want to put a carboard behind?"
            ACCEPT WS-CHOICE-CB
            DISPLAY "Want to put a glass on top?"
            ACCEPT WS-CHOICE-G

            IF WS-TYPE-OF-FRAME = 'REGULAR' OR 'regular' THEN
                ADD 15 TO WS-AMOUNT-OF-FRAMES
            ELSE IF WS-TYPE-OF-FRAME = 'FANCY' OR 'fancy' THEN
                ADD 35 TO WS-AMOUNT-OF-FRAMES
            END-IF.

            IF WS-FRAME-COLOR NOT = 'WHITE' OR 'white' THEN
                MULTIPLY 10 BY WS-LENGTH-AND-WIDTH
                GIVING WS-AMOUNT-OF-FRAME-COLOR
            ELSE
                ADD 0 TO WS-AMOUNT-OF-FRAME-COLOR
            END-IF.

            IF WS-NUMBER-OF-CROWNS NOT = 0 THEN
                MULTIPLY 35 BY WS-NUMBER-OF-CROWNS
                GIVING WS-AMOUNT-OF-CROWNS
            ELSE
                ADD 0 TO WS-AMOUNT-OF-CROWNS
            END-IF.

            IF WS-CHOICE-CB = 'Y' OR 'y' THEN
                MULTIPLY 2 BY WS-LENGTH-AND-WIDTH
                GIVING WS-AMOUNT-OF-CB
            ELSE
                ADD 0 TO WS-AMOUNT-OF-CB
            END-IF.

            IF WS-CHOICE-G = 'Y' OR 'y' THEN
                MULTIPLY 7 BY WS-LENGTH-AND-WIDTH
                GIVING WS-AMOUNT-OF-G
            ELSE
                ADD 0 TO WS-AMOUNT-OF-G
            END-IF.

       DONE-PROCEDURE.
            DISPLAY "-----------------------------------------"
            DISPLAY "TOTAL AMOUNT PURCHASED:"
             ADD WS-AMOUNT-OF-FRAMES, WS-AMOUNT-OF-FRAME-COLOR,
             WS-AMOUNT-OF-CROWNS, WS-AMOUNT-OF-CB, WS-AMOUNT-OF-G
             TO WS-TOTAL-PURCHASED-AMOUNT.
            DISPLAY WS-TOTAL-PURCHASED-AMOUNT.
            STOP RUN.
       END PROGRAM ENRIQUEZ.
