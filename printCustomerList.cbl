      ******************************************************************
      * Author:
      * Date:
      * Purpose:
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. PRINT-CUSTOMER-LIST.
       AUTHOR. RICK.
      *
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT CUSTOMER-FILE
               ASSIGN TO "D:\S-SYSIN".
           SELECT PRINT-FILE
               ASSIGN TO "D:\S-SYSOUT".
      *
       DATA DIVISION.
       FILE SECTION.
       FD  CUSTOMER-FILE
           RECORDING MODE IS F.
       01  CUST-RECORD.
               05 ACCOUNT-NO       PIC 9(10).
               05 CLINENT-NAME     PIC X(20).
               05 PRI-FINANCE.
                   10 F-SALARY     PIC 9(5).
                   10 F-STOCK      PIC 9(7).
                   10 F-FUND       PIC 9(7).
                   10 F-FOREX      PIC 9(7).
       FD PRINT-FILE
       RECORDING MODE IS F
       LABEL RECORDS ARE OMITTED
       RECORD CONTAINS 132 CHARACTERS
       DATA RECORD IS PRINT-LINE.
       01  PRINT-LINE              PIC X(132).
       WORKING-STORAGE SECTION.
       01  HEADING-LINE.
               05 FILLER       PIC X(10)
                               VALUE   SPACES.
               05 FILLER       PIC X(20)
                               VALUE   'CLIENT NAME LIST'.
               05 FILLER       PIC X(102)
                               VALUE   SPACES.
       01  DETAIL-LINE.
               05 FILLER       PIC X(12)
                               VALUE SPACES.
               05 PRT-NAME     PIC X(20).
               05 FILLER       PIC X(100)
                               VALUE SPACES.
       01  EOF-FLAG            PIC X VALUE 'N'.
      *
       PROCEDURE DIVISION.
       000-PREPARE-CUSTOMER-REPORT.
           OPEN INPUT      CUSTOMER-FILE
                OUTPUT     PRINT-FILE.
       100-WRITE-HEADING-LINE.
           MOVE    HEADING-LINE TO PRINT-LINE.
           WRITE   PRINT-LINE.
       200-PROCESS-RECORDS.
           PERFORM UNTIL EOF-FLAG = 'Y'
           READ CUSTOMER-FILE
               AT END MOVE 'Y' TO EOF-FLAG
           END-READ
           MOVE CLINENT-NAME TO PRT-NAME
           MOVE DETAIL-LINE  TO PRINT-LINE
           WRITE PRINT-LINE
       END-PERFORM.
       300-CLOSE-CUSTOMER-FILE.
           CLOSE CUSTOMER-FILE
           PRINT-FILE.
       STOP RUN.
       END PROGRAM PRINT-CUSTOMER-LIST.
