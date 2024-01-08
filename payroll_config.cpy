       01 PAYROLL-CONFIG.
           05 PC-PAYROLL-PROCESSING-DATE.
               10 PC-YEAR         PIC 9(4).
               10 PC-MONTH        PIC 9(2).
               10 PC-DAY          PIC 9(2).
           05 PC-TAX-RATE                 PIC V99 VALUE 0.25.
           05 PC-BONUS-RATE               PIC V99 VALUE 0.10.
           05 PC-RETIREMENT-FUND-RATE     PIC V99 VALUE 0.05.
           05 PC-HEALTH-INSURANCE-AMOUNT  PIC 9(5)V99 VALUE 500.00.
           05 PC-BLOCKCHAIN-ENABLED       PIC X(3) VALUE "YES".
           05 PC-BLOCKCHAIN-URL           PIC X(100) VALUE "http://localhost:5000".
           05 PC-BLOCKCHAIN-API-KEY       PIC X(32) VALUE SPACES.
