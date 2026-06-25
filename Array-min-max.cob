       IDENTIFICATION DIVISION.
       PROGRAM-ID. TEST1.

       DATA DIVISION.
       WORKING-STORAGE SECTION.

       01  WS-ARAY.
           05 WS-ELEM OCCURS 5 TIMES PIC 9(3).
       01  WS-I       PIC 9(2) VALUE 1.
       01  WS-MAX     PIC 9(3) VALUE 0.
       01  WS-MIN     PIC 9(3) VALUE 999.

       PROCEDURE DIVISION.
       FUNC.
           MOVE 5     TO WS-ELEM(1)
           MOVE 84    TO WS-ELEM(2)
           MOVE 65    TO WS-ELEM(3)
           MOVE 54    TO WS-ELEM(4)
           MOVE 47    TO WS-ELEM(5)

           PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > 5
               IF WS-ELEM(WS-I) > WS-MAX
                      MOVE  WS-ELEM(WS-I) TO WS-MAX
               END-IF
               IF WS-ELEM(WS-I) < WS-MIN
                      MOVE WS-ELEM(WS-I) TO WS-MIN
               END-IF
           END-PERFORM

           DISPLAY "MIN -> " WS-MIN
           DISPLAY "MAX -> " WS-MAX
           STOP RUN.
