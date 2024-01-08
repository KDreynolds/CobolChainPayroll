       IDENTIFICATION DIVISION.
       PROGRAM-ID. INTEGRATION.
       AUTHOR. YOUR NAME.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT BLOCKCHAIN-INTERFACE ASSIGN TO "blockchain_interface.py".

       DATA DIVISION.
       FILE SECTION.

       WORKING-STORAGE SECTION.
       01 WS-BLOCKCHAIN-ENABLED       PIC X(3).
       01 WS-BLOCKCHAIN-URL           PIC X(100).
       01 WS-BLOCKCHAIN-API-KEY       PIC X(32).
       01 WS-EMPLOYEE-ID              PIC X(10).
       01 WS-EMPLOYEE-NAME            PIC X(30).
       01 WS-EMPLOYEE-DEPARTMENT      PIC X(20).
       01 WS-PAYROLL-AMOUNT           PIC 9(5)V99.
       01 WS-RESPONSE                 PIC X(200).
       01 WS-STATUS-CODE              PIC 9(3).
       01 WS-TRANSACTION              PIC X(256).
       01 WS-JSON-STRING              PIC X(1024).

       LINKAGE SECTION.
       01 LS-EMPLOYEE-ID              PIC X(10).
       01 LS-EMPLOYEE-NAME            PIC X(30).
       01 LS-EMPLOYEE-DEPARTMENT      PIC X(20).
       01 LS-PAYROLL-AMOUNT           PIC 9(5)V99.

       PROCEDURE DIVISION USING LS-EMPLOYEE-ID
                                  LS-EMPLOYEE-NAME
                                  LS-EMPLOYEE-DEPARTMENT
                                  LS-PAYROLL-AMOUNT.
       MAIN-LOGIC.
           MOVE LS-EMPLOYEE-ID TO WS-EMPLOYEE-ID
           MOVE LS-EMPLOYEE-NAME TO WS-EMPLOYEE-NAME
           MOVE LS-EMPLOYEE-DEPARTMENT TO WS-EMPLOYEE-DEPARTMENT
           MOVE LS-PAYROLL-AMOUNT TO WS-PAYROLL-AMOUNT

           CALL "blockchain_config" USING WS-BLOCKCHAIN-ENABLED
                                           WS-BLOCKCHAIN-URL
                                           WS-BLOCKCHAIN-API-KEY

           IF WS-BLOCKCHAIN-ENABLED = "YES"
               PERFORM INVOKE-BLOCKCHAIN-INTERFACE
           ELSE
               DISPLAY "Blockchain integration is disabled."
           END-IF
           GOBACK.

       INVOKE-BLOCKCHAIN-INTERFACE.
           STRING WS-EMPLOYEE-ID DELIMITED BY SIZE
                  WS-EMPLOYEE-NAME DELIMITED BY SIZE
                  WS-EMPLOYEE-DEPARTMENT DELIMITED BY SIZE
                  WS-PAYROLL-AMOUNT DELIMITED BY SIZE
                  INTO WS-TRANSACTION
           END-STRING

           STRING '{ "sender": "payroll", "recipient": "' DELIMITED BY SIZE
                  WS-EMPLOYEE-ID DELIMITED BY SIZE
                  '", "amount": ' DELIMITED BY SIZE
                  WS-PAYROLL-AMOUNT DELIMITED BY SIZE
                  ' }' DELIMITED BY SIZE
                  INTO WS-JSON-STRING
           END-STRING

           CALL "blockchain_interface" USING WS-JSON-STRING
                                             WS-BLOCKCHAIN-URL
                                             WS-BLOCKCHAIN-API-KEY
                                             WS-RESPONSE
                                             WS-STATUS-CODE
           DISPLAY "Blockchain response: " WS-RESPONSE
           DISPLAY "Status code: " WS-STATUS-CODE.

       END PROGRAM INTEGRATION.
