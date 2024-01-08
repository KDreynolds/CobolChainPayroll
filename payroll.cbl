       IDENTIFICATION DIVISION.
       PROGRAM-ID. PAYROLL.
       AUTHOR. YOUR NAME.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT EMPLOYEE-FILE ASSIGN TO "employee_record.cpy".
           SELECT PAYROLL-FILE ASSIGN TO "payroll_record.cpy".

       DATA DIVISION.
       FILE SECTION.
       FD EMPLOYEE-FILE.
       01 EMPLOYEE-RECORD.
           COPY "employee_record.cpy".

       FD PAYROLL-FILE.
       01 PAYROLL-RECORD.
           COPY "payroll_record.cpy".

       WORKING-STORAGE SECTION.
       01 WS-END-OF-FILE                PIC X(3)       VALUE "NO".
       01 WS-EMPLOYEE-ID                PIC X(10).
       01 WS-PAYROLL-AMOUNT             PIC 9(5)V99.
       01 WS-EMPLOYEE-NAME              PIC X(30).
       01 WS-EMPLOYEE-DEPARTMENT        PIC X(20).
       01 WS-EMPLOYEE-SALARY            PIC 9(5)V99.
       01 WS-EMPLOYEE-BANK-ACCOUNT      PIC X(20).

       PROCEDURE DIVISION.
       100-MAIN.
           OPEN INPUT EMPLOYEE-FILE
           OPEN OUTPUT PAYROLL-FILE
           PERFORM UNTIL WS-END-OF-FILE = "YES"
               READ EMPLOYEE-FILE
               AT END
                   MOVE "YES" TO WS-END-OF-FILE
               NOT AT END
                   PERFORM 200-PROCESS-PAYROLL
               END-READ
           END-PERFORM
           CLOSE EMPLOYEE-FILE
           CLOSE PAYROLL-FILE
           STOP RUN.

       200-PROCESS-PAYROLL.
           MOVE EMPLOYEE-ID TO WS-EMPLOYEE-ID
           MOVE EMPLOYEE-NAME TO WS-EMPLOYEE-NAME
           MOVE DEPARTMENT TO WS-EMPLOYEE-DEPARTMENT
           MOVE SALARY TO WS-EMPLOYEE-SALARY
           MOVE BANK-ACCOUNT TO WS-EMPLOYEE-BANK-ACCOUNT
           COMPUTE WS-PAYROLL-AMOUNT = WS-EMPLOYEE-SALARY.
           PERFORM 300-RECORD-TRANSACTION
           WRITE PAYROLL-RECORD FROM WS-PAYROLL-AMOUNT.

       300-RECORD-TRANSACTION.
           CALL "blockchain_interface" USING WS-EMPLOYEE-ID
                                             WS-EMPLOYEE-NAME
                                             WS-EMPLOYEE-DEPARTMENT
                                             WS-PAYROLL-AMOUNT
           DISPLAY "Transaction recorded for employee: " WS-EMPLOYEE-ID.

       END PROGRAM PAYROLL.
