       IDENTIFICATION DIVISION.
         PROGRAM-ID. COBREPT
         AUTHOR. Wesley Salesberry
         DATE-WRITTEN. FEB 1 2024 
       ENVIRONMENT DIVISION.
         INPUT-OUTPUT SECTION.
         FILE-CONTROL. 
           SELECT IN-ICTO-PARM        ASSIGN TO ICTOPARM 
                                      FILE STATUS IS IN-FILE-STATUS.
                                     
           SELECT OT-ICTO-REPORT      ASSIGN TO ICTOREPT 
                                      FILE STATUS IS OT-FILE-STATUS.

       DATA DIVISION.
         FILE SECTION.
         FD IN-ICTO-PARM
           LABEL RECORDS ARE STANDARD
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS
           DATA RECORD IS ICTO-FILE-RECORD.

       01 ICTO-FILE-RECORD           PIC X(15).

       FD OT-ICTO-REPORT
           LABEL RECORDS ARE STANDARD
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS
           DATA RECORD IS ICTO-FILE-REPORT.

       01 ICTO-FILE-REPORT           PIC X(200).


         WORKING-STORAGE SECTION. 
           EXEC SQL
           INCLUDE SQLCA
           END-EXEC.

       01 IN-FILE-STATUS             PIC XX.
       01 OT-FILE-STATUS             PIC XX.
       
       01 WS-ICTO                    PIC X(15).
       01 WS-CURRENT-ICTO            PIC X(15).

       01 IN-END-OF-FILE             PIC A(1).

       01 CSR-ENTITY                 PIC X(08).
       01 CSR-DESCRIPTION            PIC X(30).
       01 CSR-SUBROUTINE             PIC X(01).
       01 CSR-LANG                   PIC X(04).
       01 CSR-DB2-USAGE              PIC X(04).
       01 CSR-PROJECT                PIC X(04).
       01 CSR-PROJ-ID                PIC X(04).

       01 PROJECT-FOUND-MATCH        PIC A(01)  VALUE 'N'.
       01 PROJECT-FOUND              PIC A(01)  VALUE 'N'.
       01 WS-WILDCARD-TRUNC          PIC X(08).
       01 WS-WILDCARD-TRUNC-LEN      PIC 9(01).

       01 NHM-PROGRAMS.
          05 NHM-PROG-TABLE OCCURS 20000 TIMES
                ASCENDING KEY IS NHMPROG-NAME
                INDEXED BY NHMPROG-IDX.
             10 NHMPROG-NAME         PIC X(08).
             10 NHMPROG-DESCRIPTION  PIC X(30).
             10 NHMPROG-SUBROUTINE   PIC X(01).
             10 NHMPROG-LANG         PIC X(04).
             10 NHMPROG-DB2-USAGE    PIC X(04).
             10 NHMPROG-PROJID       PIC X(04).
             10 NHMPROG-PROJID-ASSN  PIC X(04)  VALUE SPACES.

       01 NHM-ENT-PROJ-WC.
          05 NHMENT-PROJ-TABLE-WC OCCURS 2000 TIMES
                ASCENDING KEY IS NHMENT-NAME-WC
                INDEXED BY NHMENT-PROJ-WC-IDX.
             10 NHMENT-NAME-WC       PIC X(30).
             10 NHMENT-PROJID-WC     PIC X(04).

       01 NHM-ENT-PROJ-FN.
          05 NHMENT-PROJ-TABLE-FN OCCURS 2000 TIMES
                ASCENDING KEY IS NHMENT-NAME-FN
                INDEXED BY NHMENT-PROJ-FN-IDX.
             10 NHMENT-NAME-FN       PIC X(30).
             10 NHMENT-PROJID-FN     PIC X(04).

       01 END-OF-PROGRAMS-CSR        PIC A(01)  VALUE 'N'.
       01 END-OF-NHMENT-PROG-CSR     PIC A(01)  VALUE 'N'.
       01 END-OF-PROGECT-CSR         PIC A(01)  VALUE 'N'.
          
       77 SAVE-NHMPROG-IDX USAGE IS INDEX.
       77 SAVE-NHMENT-PROJ-WC-IDX USAGE IS INDEX.
       77 SAVE-NHMENT-PROJ-FN-IDX USAGE IS INDEX.

       01 SQL-ERROR-MESSAGE.
          05 SQL-ERROR-LEN           PIC S9(4) COMP-5
                                                VALUE +1320.
          05 SQL-ERROR-TEXT          PIC X(132) OCCURS 10 TIMES
                INDEXED BY ERROR-INDEX.
       01 SQL-MSG                    PIC X(20).

       77 SQL-ERROR-TEXT-LENGTH      PIC S9(9) COMP-5
                                                VALUE +132.

       01 REGION-CODE                PIC A(1).

       01 FILE-ERROR.
          05 WS-ERROR-MSG            PIC X(20).
          05 WS-ERROR-LOC            PIC X(10).
          05 WS-FILE-STATUS          PIC X(02). 

         LINKAGE SECTION.
       01 PARM-INPUT. 
          05 PARM-LENGTH             PIC S9(4) COMP.
          05 PARM-NAME.
             10 PARM-REGION          PIC X(1).

       PROCEDURE DIVISION.
      * Begin Main Program

           PERFORM 000-GET-INPUT-PARMS.
           PERFORM 001-OPEN-FILES.

           PERFORM 100-LOAD-PROGRAMS-TABLE.
           PERFORM 200-LOAD-NHMENT-PROJ-WC-TABLE.
           PERFORM 300-LOAD-NHMENT-PROJ-FN-TABLE.
           PERFORM 400-GET-NHMPROG-PROGRAMS.

           PERFORM 600-READ-FILE.

           PERFORM 700-GET-ICTO-ENTITES UNTIL IN-END-OF-FILE IS EQUAL
              TO 'Y'.
                                              
      * End Main Program 

       000-GET-INPUT-PARMS.
           IF PARM-LENGTH IS NOT GREATER THAN ZERO THEN
              DISPLAY 'Parm input is required'
              MOVE 8 TO RETURN-CODE
              STOP RUN 
           END-IF.

           MOVE PARM-REGION TO REGION-CODE.

       001-OPEN-FILES.
           OPEN INPUT IN-ICTO-PARM.

           IF IN-FILE-STATUS IS NOT EQUAL TO '00' THEN 
              MOVE 'Input File Error' TO WS-ERROR-MSG
              MOVE 'Error In 000-OPEN-FILES' TO WS-ERROR-LOC
              MOVE IN-FILE-STATUS TO WS-FILE-STATUS
              PERFORM 900-FILE-ERROR
           END-IF. 

           OPEN INPUT OT-ICTO-REPORT.

           IF OT-FILE-STATUS IS NOT EQUAL TO '00' THEN 
              MOVE 'Output File Error' TO WS-ERROR-MSG
              MOVE 'Error In 000-OPEN-FILES' TO WS-ERROR-LOC
              MOVE IN-FILE-STATUS TO WS-FILE-STATUS
              PERFORM 900-FILE-ERROR
           END-IF. 
       
       100-LOAD-PROGRAMS-TABLE.         
           EXEC SQL
                DECLARE PROGRAMS-CSR CURSOR FOR
                SELECT PROGRAM_ID,
                DESCRIPTION,
                SUBROUTINE,
                LANG,
                DB2_USAGE,
                PROJECT
                FROM E_PROGRAMS
                ORDER BY PROGRAM_ID  
                END-EXEC.

           EXEC SQL
                OPEN PROGRAMS-CSR
                END-EXEC.

           MOVE 'Programs Cursor Open Error' TO SQL-MSG.
           PERFORM 910-CHECK-SQL-CODE.

           PERFORM UNTIL END-OF-PROGRAMS-CSR EQUAL 'Y'
                   MOVE CSR-ENTITY TO NHMPROG-NAME
                   MOVE CSR-DESCRIPTION TO NHMPROG-DESCRIPTION
                   MOVE CSR-SUBROUTINE TO NHMPROG-SUBROUTINE
                   MOVE CSR-LANG TO NHMPROG-LANG 
                   MOVE CSR-DB2-USAGE TO NHMPROG-DB2-USAGE
                   MOVE CSR-PROJECT TO NHMPROG-PROJID
                   MOVE SPACES TO NHMPROG-PROJID-ASSN

                   PERFORM 120-FETCH-PROGRAMS

                   IF END-OF-PROGRAMS-CSR IS EQUAL TO 'N' THEN
                      SET NHMPROG-IDX UP BY 1       
                   END-IF
           END-PERFORM.

           SET SAVE-NHMPROG-IDX TO NHMPROG-IDX

           EXEC SQL
                CLOSE PROGRAMS-CSR
                END-EXEC.
          
           MOVE 'Programs Cursor Close Error' TO SQL-MSG.
           PERFORM 910-CHECK-SQL-CODE.

       120-FETCH-PROGRAMS.
           EXEC SQL
                FETCH FROM PROGRAMS-CSR
                INTO
                :CSR-ENTITY,
                :CSR-DESCRIPTION,
                :CSR-SUBROUTINE,
                :CSR-LANG,
                :CSR-DB2-USAGE,
                :CSR-PROJECT
                END-EXEC.
           
           MOVE 'Programs Cursor FETCH Error' TO SQL-MSG.
           PERFORM 910-CHECK-SQL-CODE.
           
           IF SQLCODE IS EQUAL TO 100 THEN 
              MOVE 'Y' TO END-OF-PROGRAMS-CSR
           END-IF.

       200-LOAD-NHMENT-PROJ-WC-TABLE.
           IF REGION-CODE IS EQUAL TO 'L' THEN 
              EXEC SQL
                   DECLARE LON-NHM-ENT-PROJ-WC CURSOR FOR
                   SELECT ENTITY_NAME,
                   PROJECT_ID
                   FROM LON.ENT_PROJ
                   WHERE PROG_TYPE = 'NHMPROG'
                   AND PROJ_NAME LIKE '%*%'
                   AND ACTIVE_ID = 'Y'
                   ORDER BY SEQ_ID
                   END-EXEC

              EXEC SQL
                   OPEN LON-NHM-ENT-PROJ-WC
                   END-EXEC

              MOVE 'Lon Wildcard Program Cursor OPEN Error' TO SQL-MSG
              PERFORM 910-CHECK-SQL-CODE
           ELSE 
              EXEC SQL
                   DECLARE NYC-NHM-ENT-PROJ-WC CURSOR FOR
                   SELECT ENTITY_NAME,
                   PROJECT_ID
                   FROM NYC.ENT_PROJ
                   WHERE PROG_TYPE = 'NHMPROG'
                   AND PROJ_NAME LIKE '%*%'
                   AND ACTIVE_ID = 'Y'
                   ORDER BY SEQ_ID
                   END-EXEC
                   
              EXEC SQL
                   OPEN NYC-NHM-ENT-PROJ-WC
                   END-EXEC

              MOVE 'NYC Wildcard Programs Cursor OPEN Error' TO SQL-MSG
              PERFORM 910-CHECK-SQL-CODE
           END-IF.

           MOVE 'N' TO END-OF-NHMENT-PROG-CSR.

           IF REGION-CODE IS EQUAL TO 'L' THEN 
              PERFORM 220-FETCH-LON-NHMENT-PROJ-WC
           ELSE
              PERFORM 230-FETCH-NYC-NHMENT-PROJ-WC
           END-IF.
           
           SET NHMENT-PROJ-WC-IDX TO 1.

           PERFORM UNTIL END-OF-NHMENT-PROG-CSR EQUAL 'Y'
                   MOVE CSR-ENTITY TO NHMENT-NAME-WC
                      (NHMENT-PROJ-WC-IDX)
                   MOVE CSR-PROJ-ID TO NHMENT-PROJID-WC
                      (NHMENT-PROJ-WC-IDX)
             
                   IF REGION-CODE IS EQUAL TO 'L' THEN 
                      PERFORM 220-FETCH-LON-NHMENT-PROJ-WC
                   ELSE
                      PERFORM 230-FETCH-NYC-NHMENT-PROJ-WC
                   END-IF

                   IF END-OF-NHMENT-PROG-CSR IS EQUAL TO 'N' THEN
                      SET NHMENT-PROJ-WC-IDX UP BY 1
                   END-IF
           END-PERFORM.

           SET SAVE-NHMENT-PROJ-WC-IDX TO NHMENT-PROJ-WC-IDX.

           IF REGION-CODE IS EQUAL TO 'L' THEN 
              EXEC SQL
                   CLOSE LON-NHM-ENT-PROJ-WC
                   END-EXEC

              MOVE 'Lon Wildcard Program Cursor CLOSE Error' TO SQL-MSG
              PERFORM 910-CHECK-SQL-CODE
           ELSE
              EXEC SQL
                   CLOSE NYC-NHM-ENT-PROJ-WC
                   END-EXEC

              MOVE 'NYC Wildcard Program Cursor CLOSE Error' TO SQL-MSG
              PERFORM 910-CHECK-SQL-CODE
           END-IF.
        
       220-FETCH-LON-NHMENT-PROJ-WC.
           EXEC SQL
                FETCH LON-NHM-ENT-PROJ-WC
                INTO CSR-ENTITY,
                CSR-PROJ-ID
                END-EXEC.

           MOVE 'LON Wildcard Program Cursor FETCH Error' TO SQL-MSG.
           PERFORM 910-CHECK-SQL-CODE.

           IF SQLCODE IS EQUAL TO 100 THEN 
              MOVE 'Y' TO END-OF-NHMENT-PROG-CSR
           END-IF.

       230-FETCH-NYC-NHMENT-PROJ-WC.   
           EXEC SQL
                FETCH NYC-NHM-ENT-PROJ-WC
                INTO CSR-ENTITY,
                CSR-PROJ-ID
                END-EXEC.
           
           MOVE 'NYC Wildcard Program Cursor FETCH Error' TO SQL-MSG.
           PERFORM 910-CHECK-SQL-CODE.

           IF SQLCODE IS EQUAL TO 100 THEN 
              MOVE 'Y' TO END-OF-NHMENT-PROG-CSR
           END-IF.

       300-LOAD-NHMENT-PROJ-FN-TABLE.
           IF REGION-CODE IS EQUAL TO 'L' THEN 
              EXEC SQL
                   DECLARE LON-NHM-ENT-PROJ-FN CURSOR FOR
                   SELECT ENTITY_NAME,
                   PROJECT_ID
                   FROM LON.ENT_PROJ
                   WHERE PROG_TYPE = 'NHMPROG'
                   AND PROJ_NAME NOT LIKE '%*%'
                   AND ACTIVE_ID = 'Y'
                   ORDER BY SEQ_ID
                   END-EXEC

              EXEC SQL
                   OPEN LON-NHM-ENT-PROJ-FN
                   END-EXEC

              MOVE 'Lon Full Name Program Cursor OPEN Error' TO SQL-MSG
              PERFORM 910-CHECK-SQL-CODE
           ELSE 
              EXEC SQL
                   DECLARE NYC-NHM-ENT-PROJ-FN CURSOR FOR
                   SELECT ENTITY_NAME,
                   PROJECT_ID
                   FROM NYC.ENT_PROJ
                   WHERE PROG_TYPE = 'NHMPROG'
                   AND PROJ_NAME NOT LIKE '%*%'
                   AND ACTIVE_ID = 'Y'
                   ORDER BY SEQ_ID
                   END-EXEC
                   
              EXEC SQL
                   OPEN NYC-NHM-ENT-PROJ-FN
                   END-EXEC

              MOVE 'NYC Full Name Programs Cursor OPEN Error' TO SQL-MSG
              PERFORM 910-CHECK-SQL-CODE
           END-IF.

           MOVE 'N' TO END-OF-NHMENT-PROG-CSR.

           IF REGION-CODE IS EQUAL TO 'L' THEN 
              PERFORM 320-FETCH-LON-NHMENT-PROJ-FN
           ELSE
              PERFORM 330-FETCH-NYC-NHMENT-PROJ-FN
           END-IF.
           
           SET NHMENT-PROJ-FN-IDX TO 1.

           PERFORM UNTIL END-OF-NHMENT-PROG-CSR EQUAL 'Y'
                   MOVE CSR-ENTITY TO NHMENT-NAME-WC
                      (NHMENT-PROJ-WC-IDX)
                   MOVE CSR-PROJ-ID TO NHMENT-PROJID-WC
                      (NHMENT-PROJ-WC-IDX)
             
                   IF REGION-CODE IS EQUAL TO 'L' THEN 
                      PERFORM 320-FETCH-LON-NHMENT-PROJ-FN
                   ELSE
                      PERFORM 330-FETCH-NYC-NHMENT-PROJ-FN
                   END-IF

                   IF END-OF-NHMENT-PROG-CSR IS EQUAL TO 'N' THEN
                      SET NHMENT-PROJ-FN-IDX UP BY 1
                   END-IF
           END-PERFORM.

           SET SAVE-NHMENT-PROJ-FN-IDX TO NHMENT-PROJ-FN-IDX.

           IF REGION-CODE IS EQUAL TO 'L' THEN 
              EXEC SQL
                   CLOSE LON-NHM-ENT-PROJ-FN
                   END-EXEC

              MOVE 'Lon FullName Program Cursor CLOSE Error' TO SQL-MSG
              PERFORM 910-CHECK-SQL-CODE
           ELSE
              EXEC SQL
                   CLOSE NYC-NHM-ENT-PROJ-FN
                   END-EXEC

              MOVE 'NYC FullName Program Cursor CLOSE Error' TO SQL-MSG
              PERFORM 910-CHECK-SQL-CODE
           END-IF.
        
       320-FETCH-LON-NHMENT-PROJ-FN.
           EXEC SQL
                FETCH LON-NHM-ENT-PROJ-FN
                INTO CSR-ENTITY,
                CSR-PROJ-ID
                END-EXEC.

           MOVE 'LON Full Name Program Cursor FETCH Error' TO SQL-MSG.
           PERFORM 910-CHECK-SQL-CODE.

           IF SQLCODE IS EQUAL TO 100 THEN 
              MOVE 'Y' TO END-OF-NHMENT-PROG-CSR
           END-IF.

       330-FETCH-NYC-NHMENT-PROJ-FN.   
           EXEC SQL
                FETCH NYC-NHM-ENT-PROJ-FN
                INTO CSR-ENTITY,
                CSR-PROJ-ID
                END-EXEC.

           MOVE 'NYC Full Name Program Cursor FETCH Error' TO SQL-MSG.
           PERFORM 910-CHECK-SQL-CODE.

           IF SQLCODE IS EQUAL TO 100 THEN 
              MOVE 'Y' TO END-OF-NHMENT-PROG-CSR
           END-IF.

       400-GET-NHMPROG-PROGRAMS.
           SET NHMPROG-IDX UP BY 1.

           PERFORM UNTIL NHMPROG-IDX IS GREATER THAN SAVE-NHMPROG-IDX
                   MOVE 'N' TO PROJECT-FOUND-MATCH

                   PERFORM 420-NHM-ENT-NAME-MATCH

                   IF PROJECT-FOUND-MATCH IS EQUAL TO 'Y' THEN
                      MOVE NHMENT-PROJID-FN(NHMENT-PROJ-FN-IDX) TO
                         NHMPROG-PROJID-ASSN(NHMPROG-IDX)
                   ELSE
                      PERFORM 440-NHM-WILDCARD-MATCH
                   END-IF

                   SET NHMPROG-IDX UP BY 1
           END-PERFORM. 
       
       420-NHM-ENT-NAME-MATCH.
           SET NHMENT-PROJ-FN-IDX UP BY 1.
           SEARCH NHMENT-PROJ-TABLE-FN
           AT END
              MOVE 'N' TO PROJECT-FOUND-MATCH
           WHEN NHMENT-PROJ-TABLE-FN(NHMENT-PROJ-FN-IDX) IS EQUAL TO
              NHMPROG-NAME(NHMPROG-IDX)
                MOVE 'Y' TO PROJECT-FOUND-MATCH
           END-SEARCH.

       440-NHM-WILDCARD-MATCH.
           SET NHMENT-PROJ-WC-IDX TO 1.
           PERFORM UNTIL NHMENT-PROJ-WC-IDX > SAVE-NHMENT-PROJ-WC-IDX
                   MOVE NHMENT-NAME-WC(NHMENT-PROJ-WC-IDX) TO
                      WS-WILDCARD-TRUNC
                   PERFORM 445-CHECK-WILDCARD-NAME-MATCH

                   IF PROJECT-FOUND-MATCH IS EQUAL TO 'Y' THEN
                      MOVE NHMENT-PROJID-WC(NHMENT-PROJ-WC-IDX) TO
                         NHMPROG-PROJID-ASSN(NHMPROG-IDX) 
                      EXIT PERFORM
                   END-IF
                   SET NHMENT-PROJ-WC-IDX UP BY 1
           END-PERFORM.

       445-CHECK-WILDCARD-NAME-MATCH.
           INSPECT WS-WILDCARD-TRUNC CONVERTING '*' TO SPACES.
           COMPUTE WS-WILDCARD-TRUNC-LEN = FUNCTION LENGTH(FUNCTION
              TRIM(WS-WILDCARD-TRUNC)).

           IF NHMPROG-NAME(NHMPROG-IDX)(1:WS-WILDCARD-TRUNC-LEN)
              IS EQUAL TO WS-WILDCARD-TRUNC THEN 
              MOVE 'Y' TO PROJECT-FOUND-MATCH
           END-IF.

       600-READ-FILE.
           READ IN-ICTO-PARM INTO WS-ICTO
           AT END
              MOVE 'Y' TO IN-END-OF-FILE
           NOT AT END
               MOVE WS-ICTO TO WS-CURRENT-ICTO
           END-READ.

       700-GET-ICTO-ENTITES.
           EXEC SQL
                DECLARE LON-ICTO-PROJECT-CSR CURSOR FOR
                SELECT PROJECT_ID
                FROM LON.PROJECT_ICTO
                WHERE PROJECT_ICTO = :WS-CURRENT-ICTO
                ORDER BY PROJECT_ID
                END-EXEC.

           EXEC SQL
                DECLARE NYC-ICTO-PROJECT-CSR CURSOR FOR
                SELECT PROJECT_ID
                FROM NYC.PROJECT_ICTO
                WHERE PROJECT_ICTO = :WS-CURRENT-ICTO
                ORDER BY PROJECT_ID
                END-EXEC.

           IF REGION-CODE IS EQUAL TO 'L' THEN
              EXEC SQL
                   OPEN LON-ICTO-PROJECT-CSR
                   END-EXEC
              MOVE 'ICTO Lon SQL Open Error' TO SQL-MSG
           ELSE
              EXEC SQL
                   OPEN NYC-ICTO-PROJECT-CSR
                   END-EXEC
              MOVE 'ICTO NYC SQL Open Error' TO SQL-MSG
           END-IF.

           PERFORM 910-CHECK-SQL-CODE.

           MOVE 'N' TO END-OF-PROGECT-CSR.
           PERFORM 705-FETCH-ICTO-PROJECTS.

           PERFORM UNTIL END-OF-PROGECT-CSR IS EQUAL TO 'Y'
      *          Create the rest of the routines 
                   PERFORM 720-GET-NHMPROG    
                   PERFORM 705-FETCH-ICTO-PROJECTS
           END-PERFORM.

           IF REGION-CODE IS EQUAL TO 'L' THEN
              EXEC SQL
                   CLOSE LON-ICTO-PROJECT-CSR
                   END-EXEC
              MOVE 'ICTO Lon SQL Close Error' TO SQL-MSG
           ELSE
              EXEC SQL
                   CLOSE NYC-ICTO-PROJECT-CSR
                   END-EXEC
              MOVE 'ICTO NYC SQL Close Error' TO SQL-MSG
           END-IF.

           PERFORM 910-CHECK-SQL-CODE.
           PERFORM 600-READ-FILE. 

       705-FETCH-ICTO-PROJECTS.
           IF REGION-CODE IS EQUAL TO 'L' THEN
              EXEC SQL
                   FETCH LON-ICTO-PROJECT-CSR
                   END-EXEC
              MOVE 'ICTO Lon SQL Fetch Error' TO SQL-MSG
           ELSE
              EXEC SQL
                   FETCH NYC-ICTO-PROJECT-CSR
                   END-EXEC
              MOVE 'ICTO NYC SQL Fetch Error' TO SQL-MSG
           END-IF.

           PERFORM 910-CHECK-SQL-CODE.

           IF SQLCODE IS EQUAL TO 100 THEN
              MOVE 'Y' TO END-OF-PROGECT-CSR
           END-IF.

       720-GET-NHMPROG.
           SET NHMPROG-IDX TO 1.

           PERFORM UNTIL NHMPROG-IDX IS GREATER THAN SAVE-NHMPROG-IDX
                   IF NHMPROG-PROJID-ASSN(NHMPROG-IDX) IS EQUAL TO
                      CSR-PROJECT THEN
      *               TODO: Write projects to report  
                      DISPLAY 'Project Exist'  
                   END-IF
                   SET NHMPROG-IDX UP BY 1
           END-PERFORM.

       800-CLOSE-FILE.
           CLOSE IN-ICTO-PARM
                 OT-ICTO-REPORT.
          
       900-FILE-ERROR.
           DISPLAY '***** FILE ERROR *****'.
           DISPLAY WS-ERROR-MSG.
           DISPLAY WS-ERROR-LOC.
           DISPLAY 'File Status ' WS-FILE-STATUS.
           MOVE 8 TO RETURN-CODE.
           PERFORM 800-CLOSE-FILE. 
           STOP RUN.
       
       910-CHECK-SQL-CODE.
           IF SQLCODE IS NOT EQUAL TO ZEROS THEN 
              IF SQLCODE IS NOT EQUAL TO 100 THEN 
                 CALL 'DSNTIAR' USING SQLCA
                                      SQL-ERROR-MESSAGE
                                      SQL-ERROR-TEXT-LENGTH
                 DISPLAY '***** SQL ERROR *****'
                 DISPLAY SQL-MSG
                 DISPLAY SQL-ERROR-TEXT(1)
                 MOVE 8 TO RETURN-CODE
                 PERFORM 800-CLOSE-FILE
                 STOP RUN
              END-IF
           END-IF.