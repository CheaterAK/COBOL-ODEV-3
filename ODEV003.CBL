       IDENTIFICATION DIVISION.
       PROGRAM-ID.    ODEV003
       AUTHOR.        AHMET KOCABAS aka CheaterAK
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT IDX-FILE ASSIGN to IDXFILE ORGANIZATION IS INDEXED
                           ACCESS MODE IS RANDOM
                           RECORD KEY is IDX-KEY
                           FILE STATUS is STATUS-IDX.
           SELECT OUT-FILE ASSIGN TO OUTFILE
                           FILE STATUS IS STATUS-OUT.
           SELECT ACC-FILE    ASSIGN TO ACCFILE
                           FILE STATUS IS STATUS-ACC.
       DATA DIVISION.
       FILE SECTION.
       FD  IDX-FILE.
       01  IDX-REC.
           05    IDX-KEY.
              07    IDX-ID               PIC S9(5) COMP-3.
              07    CURRENCY-CURRENCY     PIC S9(3) COMP.
           05    FIRST-NAME              PIC X(15).
           05    LAST-NAME               PIC X(15).
           05    LAST-ORDER-DATE         PIC S9(7) COMP-3.
           05    BALANCE                 PIC S9(15) COMP-3.
       66  FULL-NAME   RENAMES FIRST-NAME THROUGH LAST-NAME.
       FD  OUT-FILE RECORDING MODE F.
       01  OUT-REC.
           05    IDX-ID-O                PIC 9(5).
           05    FILLER                  PIC X(1) VALUE SPACE.
           05    IDX-CURR-O              PIC X(3).
           05    FILLER                  PIC X(1) VALUE SPACE.
           05    FIRST-NAME-O            PIC X(15).
           05    FILLER                  PIC X(1) VALUE SPACE.
           05    LAST-NAME-O             PIC X(15).
           05    FILLER                  PIC X(1) VALUE SPACE.
           05    LAST-ORDER-O            PIC 9(8).
           05    FILLER                  PIC X(1) VALUE SPACE.
           05    BALANCE-O               PIC 9(15).
       66  FULL-NAME-O RENAMES FIRST-NAME-O THRU LAST-NAME-O.
       FD  ACC-FILE RECORDING MODE F.
       01  ACC-REC.
           05    ACC-ID                  PIC X(5).
           05    CURRENCY-KEY             PIC X(3).
           05    RESERVED                PIC X(42).
       WORKING-STORAGE SECTION.
       01  HEADER.
           05 FILLER                     PIC X(5) VALUE 'ACCNT'.
           05 FILLER                     PIC X(1) VALUE SPACE.
           05 FILLER                     PIC X(3) VALUE 'CUR'.
           05 FILLER                     PIC X(1) VALUE SPACE.
           05 FILLER                     PIC X(15) VALUE 'FIRST-NAME'.
           05 FILLER                     PIC X(1) VALUE SPACE.
           05 FILLER                     PIC X(15) VALUE 'LAST-NAME'.
           05 FILLER                     PIC X(1) VALUE SPACE.
           05 FILLER                     PIC X(8) VALUE 'ORDER DT'.
           05 FILLER                     PIC X(1) VALUE SPACE.
           05 FILLER                     PIC X(15) VALUE 'BALANCE'.
       01  ERR.
           05 FILLER                     PIC X(5) VALUE '-----'.
           05 FILLER                     PIC X(1) VALUE SPACE.
           05 FILLER                     PIC X(3) VALUE '---'.
           05 FILLER                     PIC X(1) VALUE SPACE.
           05 FILLER                     PIC X(31)
                             VALUE "ERROR. INVALID INPUT CHK SYSOUT".
           05 FILLER                     PIC X(1) VALUE SPACE.
           05 FILLER                     PIC X(8) VALUE '--------'.
           05 FILLER                     PIC X(1) VALUE SPACE.
           05 FILLER                     PIC X(15)
                                   VALUE '---------------'.
       01  WS-FLAGS.
           05 STATUS-IDX                 PIC 9(2).
              88 IDX-SUCC                         VALUE 00 97.
              88 IDX-EOF                          VALUE 10.
              88 IDX-INVALID-KEY                  VALUE 23.
           05 STATUS-OUT                 PIC 99.
              88 OUT-SUCC                         VALUE 00 97.
           05 STATUS-ACC                 PIC 99.
              88 ACC-SUCC                         VALUE 00 97.
              88 ACC-EOF                          VALUE 10.
       77  READ-CNT                      PIC 9(3) VALUE 0.
       77  VALID-READ-CNT                PIC 9(3) VALUE 0.
       77  INVALID-READ-CNT              PIC 9(3) VALUE 0.
       77  VALID-KEY                     PIC 9(1) VALUE 1.
       77  VALID-IDX                     PIC 9(1) VALUE 1.
       77  TOTAL-BALANCE                 PIC 9(18) VALUE 0.
       77  TAKVIMLER-GREGORYEN           PIC 9(8).
       77  RIP-BETA                      PIC 9(8).
       PROCEDURE DIVISION.
       0000-MAIN.
           PERFORM 0100-OPEN-FILES.
           PERFORM 0110-FILE-CONTROL.
           PERFORM 0200-PROCESS-FILE.
           PERFORM 0800-CLOSE-FILES.
           PERFORM 9999-EXIT.

       0100-OPEN-FILES.
           OPEN INPUT ACC-FILE.
           OPEN INPUT IDX-FILE.
           OPEN OUTPUT OUT-FILE.

       0110-FILE-CONTROL.
           IF NOT IDX-SUCC
              DISPLAY "IDX FILE NOT FOUND. EXITING..."
              STOP RUN
           END-IF.
           IF NOT OUT-SUCC
              DISPLAY "OUT FILE NOT FOUND. EXITING..."
              STOP RUN
           END-IF.
           IF NOT ACC-SUCC
              DISPLAY "ACC FILE NOT FOUND. EXITING..."
              STOP RUN
           END-IF.

       0200-PROCESS-FILE.
           PERFORM P100-HEADER-PRINT.
           PERFORM P200-READ-KEY.
           PERFORM UNTIL ACC-EOF
              IF VALID-KEY = 1
                 PERFORM P300-READ-IDX
              END-IF
              PERFORM P400-WRITE-AND-HANDLE
              PERFORM P200-READ-KEY
           END-PERFORM.
           PERFORM P999-REPORT.
       P100-HEADER-PRINT.
           MOVE HEADER TO OUT-REC.
           WRITE OUT-REC.
           MOVE SPACES TO OUT-REC.
           WRITE OUT-REC.
       P200-READ-KEY.
           READ ACC-FILE
           NOT AT END
              ADD 1 TO READ-CNT 
           END-READ.
           PERFORM P210-KEY-CONTROL.
           IF VALID-KEY = 1
                 MOVE ACC-ID TO IDX-ID
                 MOVE CURRENCY-KEY TO CURRENCY-CURRENCY
           END-IF.
       P210-KEY-CONTROL.
           IF ACC-ID NOT NUMERIC
              DISPLAY ACC-ID 
              " << ACC-ID MUST BE NUMERIC AND 5 CHARACTER"
              MOVE 0 TO VALID-KEY
           END-IF.
           IF CURRENCY-KEY NOT NUMERIC
              DISPLAY CURRENCY-KEY 
              " << CURRENCY-KEY MUST BE NUMERIC AND 3 CHARACTER"
              MOVE 0 TO VALID-KEY
           END-IF.
       P300-READ-IDX.
           READ IDX-FILE KEY IDX-KEY.
           IF IDX-INVALID-KEY
              MOVE 0 TO VALID-IDX
           END-IF.
       P400-WRITE-AND-HANDLE.
           INITIALIZE OUT-REC.
           MOVE SPACES TO OUT-REC.
           IF VALID-IDX = 0 OR VALID-KEY = 0
                PERFORM U100-ERR-PRINT
           ELSE 
                 MOVE IDX-ID TO IDX-ID-O
                 MOVE CURRENCY-CURRENCY TO IDX-CURR-O
                 MOVE FIRST-NAME TO FIRST-NAME-O
                 MOVE LAST-NAME TO LAST-NAME-O
                 PERFORM P410-CALCULATE-DATE
                 MOVE TAKVIMLER-GREGORYEN TO LAST-ORDER-O
                 PERFORM P500-UPBALANCE
                 MOVE BALANCE TO BALANCE-O
                 ADD BALANCE-O TO TOTAL-BALANCE
                 ADD 1 TO VALID-READ-CNT
           END-IF.
           WRITE OUT-REC.
           MOVE 1 TO VALID-IDX.
           MOVE 1 TO VALID-KEY.
       P410-CALCULATE-DATE.
           COMPUTE RIP-BETA =
           FUNCTION INTEGER-OF-DAY(LAST-ORDER-DATE).
           COMPUTE TAKVIMLER-GREGORYEN = 
           FUNCTION DATE-OF-INTEGER(RIP-BETA).
       P500-UPBALANCE.
           ADD IDX-ID TO BALANCE.
       P999-REPORT.
           MOVE SPACES TO OUT-REC.
           WRITE OUT-REC.
           MOVE 'TOTAL' TO OUT-REC.
           WRITE OUT-REC.
           MOVE SPACES TO OUT-REC.
           MOVE 'RD. VD. IVD         TOTAL VALID BALANCE' TO OUT-REC.
           WRITE OUT-REC.
           MOVE SPACES TO OUT-REC.
           MOVE READ-CNT TO OUT-REC(1:3).
           MOVE VALID-READ-CNT TO OUT-REC(5:3).
           COMPUTE INVALID-READ-CNT = READ-CNT - VALID-READ-CNT.
           MOVE INVALID-READ-CNT TO OUT-REC(9:3).
           MOVE TOTAL-BALANCE TO OUT-REC(21:19)
           WRITE  OUT-REC.            
       U100-ERR-PRINT.
           MOVE ERR TO OUT-REC.
           ADD 1 TO INVALID-READ-CNT.
       0800-CLOSE-FILES.
           CLOSE ACC-FILE.
           CLOSE IDX-FILE.
           CLOSE OUT-FILE.
       9999-EXIT.
           STOP RUN.
