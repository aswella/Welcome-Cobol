       IDENTIFICATION DIVISION.
       PROGRAM-ID. MAXTRIX2D.

       DATA DIVISION.
       WORKING-STORAGE SECTION.

       01  WS-MATRIX.
           05    WS-ROW OCCURS 3 TIMES.
               10    WS-COL OCCURS 3 TIMES PIC 9(3).

       01  WS-I    PIC 9 VALUE 1.
       01  WS-J    PIC 9 VALUE 1.
       01  WS-MAX  PIC 9(3) VALUE 0.
       01  WS-MIN  PIC 9(3) VALUE 999.

       PROCEDURE DIVISION.
       FUNC.  
           MOVE 1 TO WS-COL(1,1)
           MOVE 2 TO WS-COL(1,2)
           MOVE 3 TO WS-COL(1,3)
           MOVE 4 TO WS-COL(2,1)
           MOVE 5 TO WS-COL(2,2)
           MOVE 6 TO WS-COL(2,3)
           MOVE 7 TO WS-COL(3,1)
           MOVE 8 TO WS-COL(3,2)
           MOVE 9 TO WS-COL(3,3)

           PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > 3
               PERFORM VARYING WS-J FROM 1 BY 1 UNTIL WS-J > 3
                   IF WS-COL(WS-I, WS-J) > WS-MAX
                       MOVE WS-COL(WS-I, WS-J) TO WS-MAX
                   END-IF
                   IF WS-COL(WS-I, WS-J) < WS-MIN
                       MOVE WS-COL(WS-I, WS-J) TO WS-MIN
                   END-IF
               END-PERFORM
           END-PERFORM

           DISPLAY "MAX: " WS-MAX
           DISPLAY "MIN: " WS-MIN
           STOP RUN.
