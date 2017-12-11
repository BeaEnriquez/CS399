      ******************************************************************
      * Author:
      * Date:
      * Purpose:
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. BDE.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       SELECT OUTPUT-FILE
           ASSIGN TO "C:\COBOL_Files\CAO.TXT".
       DATA DIVISION.
       FILE SECTION.
       FD  OUTPUT-FILE
           DATA RECORD IS OUTPUT-REC.
       01  OUTPUT-REC.
           05 LOT-NUM-OUT              PIC 9(9).
           05 OWNER-NAME-OUT           PIC A(26).
           05 ASSESSED-EVALUATION-OUT  PIC 99999999V99.
       WORKING-STORAGE SECTION.
       01  RESPONSES.
           05  RESPONSE-IN-WS  PIC 9     VALUE 5.
           05  OTHER-RESP      PIC X     VALUE SPACES.
           05  UPDATE-RESP     PIC 9     VALUE 6.
       01  CREATING.
           05  LOT-NUM                 PIC 9(9).
           05  OWNER-NAME              PIC A(26).
           05  ASSESSED-EVALUATION     PIC 99999999V99.
       SCREEN SECTION.
       01  DATA-ENTRY-SCREEN.
           05  VALUE "MENU"   BLANK SCREEN     LINE 1 COL 10.
           05  VALUE "[1] CREATE MASTER FILE"  LINE 3 COL 10.
           05  VALUE "[2] UPDATE MASTER FILE"  LINE 5 COL 10.
           05  VALUE "[3] PRINT MASTER FILE"   LINE 7 COL 10.
           05  VALUE "[4] QUIT"                LINE 9 COL 10.

           05  VALUE "ENTER RESPONSE:"  LINE 12 COL 10.
           05  RESPONSE-INPUT           LINE 12 COL 25
                           PIC X TO RESPONSE-IN-WS.

       01  OTHER-RESP-INFO.
           05 VALUE "C - TO CONTINUE"  LINE 9 COL 10.
           05 VALUE "Q - TO QUIT"      LINE 12 COL 10.
           05 VALUE "ENTER CHOICE:"    LINE 14 COL 10.
           05 RESPONSE-SCR             LINE 14 COL 25
                           PIC X TO OTHER-RESP.

       01  CREATING-FILE.
           05 VALUE "MASTER FILE"   BLANK SCREEN     LINE 1 COL 10.
           05 VALUE "LOT NUMBER:"           LINE 3 COL 10.
           05 VALUE "OWNER NAME:"           LINE 5 COL 10.
           05 VALUE "ASSESSED EVALUATION:"  LINE 7 COL 10.

       01  UPDATE-FILE.
           05 VALUE "UPDATING FILES" BLANK SCREEN        LINE 1 COL 10.
           05 VALUE "[1] CHANGE NAME"                    LINE 3 COL 10.
           05 VALUE "[2] CHANGE VALUATION"               LINE 5 COL 10.
           05 VALUE "[3] CHANGE NAME AND VALUATION"      LINE 7 COL 10.
           05 VALUE "[4] ADD TO TAX ROLLS"               LINE 9 COL 10.
           05 VALUE "[5] REMOVE FROM TAX ROLLS"          LINE 12 COL 10.
           05 VALUE "[6] QUIT"                          LINE 14  COL 10.
           05  VALUE "ENTER CHOICE:"                   LINE 15 COL 10.
           05  RESPONSE                                LINE 15 COL 25
                           PIC 9 TO UPDATE-RESP.

       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
            PERFORM A-100-INITIALIZE.
            PERFORM A-100-MENU.


            EVALUATE TRUE
            WHEN RESPONSE-IN-WS = 1
               PERFORM B-100-CREATING-FILE UNTIL OTHER-RESP = "Q"

            WHEN RESPONSE-IN-WS = 2
               PERFORM B-200-UPDATE-FILE
                   IF UPDATE-RESP = 1 THEN
                       PERFORM B-200-UPDATE-NAME UNTIL OTHER-RESP = "Q"
                       PERFORM B-200-UPDATE-FILE
                   ELSE IF UPDATE-RESP = 2 THEN
                       PERFORM B-200-UPDATE-VALUATION
                           UNTIL OTHER-RESP = "Q"
                   END-IF
            END-EVALUATE.

            PERFORM B-150.


       A-100-INITIALIZE.
            OPEN OUTPUT OUTPUT-FILE.

       A-100-MENU.
            DISPLAY DATA-ENTRY-SCREEN.
            ACCEPT DATA-ENTRY-SCREEN.

       B-100-CREATING-FILE.
            DISPLAY CREATING-FILE.
            ACCEPT LOT-NUM LINE 3 COL 25.
            ACCEPT OWNER-NAME  LINE 5 COL 25.
            ACCEPT ASSESSED-EVALUATION LINE 7 COL 30.
            MOVE LOT-NUM TO LOT-NUM-OUT.
            MOVE OWNER-NAME TO OWNER-NAME-OUT.
            MOVE ASSESSED-EVALUATION TO ASSESSED-EVALUATION-OUT.
             WRITE OUTPUT-REC.
            DISPLAY OTHER-RESP-INFO.
            ACCEPT OTHER-RESP-INFO.

       B-150.
            CLOSE OUTPUT-FILE.

       B-200-UPDATE-FILE.
           DISPLAY UPDATE-FILE.
           ACCEPT UPDATE-FILE.

       B-200-UPDATE-NAME.
            DISPLAY "UPDATE NAME: "   BLANK SCREEN LINE 3 COL 10.
            ACCEPT OWNER-NAME  LINE 3 COL 25.
            MOVE OWNER-NAME TO OWNER-NAME-OUT.

            DISPLAY OTHER-RESP-INFO.
            ACCEPT OTHER-RESP-INFO.

       B-200-UPDATE-VALUATION.
            DISPLAY "UPDATE ASSESSED-VALUATION"
               BLANK SCREEN LINE 3 COL 10.
            ACCEPT ASSESSED-EVALUATION LINE 3 COL 40.
            MOVE ASSESSED-EVALUATION TO ASSESSED-EVALUATION-OUT.

            DISPLAY OTHER-RESP-INFO.
            ACCEPT OTHER-RESP-INFO.
       C-100-LOOP.
           DISPLAY DATA-ENTRY-SCREEN.
           ACCEPT DATA-ENTRY-SCREEN.
            STOP RUN.
       END PROGRAM BDE.
