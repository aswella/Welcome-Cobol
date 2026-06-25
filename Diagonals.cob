       IDENTIFICATION DIVISION.
       PROGRAM-ID. DIOGONALS.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       
       01  WS-MATRIX.
           05    WS-ROW OCCURS 3 TIMES.
               10    WS-COL    OCCURS 3 TIMES PIC 9(3).

       01  WS-I         PIC 9 VALUE 1.
       01  WS-J         PIC 9 VALUE 1.
       01  WS-N         PIC 9 VALUE 3.
       01  WS-SUM-MAIN  PIC 9(4) VALUE 0.
       01  WS-SUM-ANTI  PIC 9(4) VALUE 0.

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

      *ROWS = COLLUM
           PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > 3
               ADD WS-COL(WS-I, WS-I) TO WS-SUM-MAIN
           END-PERFORM

      *J = N + 1 - I
           PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > 3
               COMPUTE WS-J = WS-N + 1 - WS-I
               ADD WS-COL(WS-I, WS-J) TO WS-SUM-ANTI
           END-PERFORM

           DISPLAY "SUM MAIN: " WS-SUM-MAIN
           DISPLAY "SUM ANTI: " WS-SUM-ANTI


           STOP RUN.
