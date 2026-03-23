     H DATFMT(*ISO)                                                             
     H*****************************************************************         
     H* FastBreed Technologies, Inc. © 2000                           *         
     H*****************************************************************         
     H*****************************************************************         
     H**                                                             **         
     H** OBJECT ID: IS923R2                                          **         
     H**                                                             **         
     H** OBJECT DESCRIPTION: PRINT EXPENSE REPORT                    **         
     H**                                                             **         
     H** MOD#   DATE  PGMR DESCRIPTION                               **         
     H** ----  ------ ---- ----------------------------------------- **         
     H**                                                             **         
     H*****************************************************************         
     F*                                                                         
     FIS925P1   IF   E           K DISK                                         
     FIS926P1   IF   E           K DISK                                         
     FIS927L1   IF   E           K DISK    PREFIX(PR_)                          
     FIS928L2   IF   E           K DISK                                         
     FIS929P1   IF   E           K DISK                                         
     FIS929L2   IF   E           K DISK    RENAME(R929P1:REXP) PREFIX(EX_)      
     FSCDATEF   IF   E           K DISK                                         
     FQSYSPRT   O    F  132        PRINTER OFLIND(*INOF)                        
     D*                                                                         
     D UNLN            S             62    DIM(1) CTDATA PERRCD(1)              
     D UNLX            S             40    DIM(1) CTDATA PERRCD(1)              
     D*                                                                         
     D** Date fields                                                            
     D DATISO          S               D   DATFMT(*ISO)                         
     D DATMDY          S               D   DATFMT(*MDY)                         
     D*                                                                         
     D PROJECTS        S              5  0 DIM(9999)                            
     D*                                                                         
*0002D                 DS                                                       
*0002D  CMPDT3                 1     70                                         
*0002D    WEEKSTART            1      3                                         
     D*                                                                         
     D** Program Status data structure                                          
     D PSDS           SDS                                                       
     D  PGMNAM           *PROC                                                  
     D  USERID               254    263                                         
     D*                                                                         
     C*****************************************************************         
     C*  MAINLINE                                                     *         
     C*****************************************************************         
     C*                                                                         
     C     *LOVAL        SETLL     R929P1                                       
     C*                                                                         
     C** Determine report date range                                            
     C     MAIN01        TAG                                                    
     C                   EXSR      S000                                         
     C     *IN96         CABEQ     *ON           MAIN99                         
     C*                                                                         
     C** Print line - company name                                              
     C                   EXSR      S001                                         
     C*                                                                         
     C** Print line - company address                                           
     C                   EXSR      S002                                         
     C*                                                                         
     C** Print line - employee name                                             
     C                   EXSR      S003                                         
     C*                                                                         
     C** Print line - organization, dept                                        
     C                   EXSR      S004                                         
     C*                                                                         
     C** Print line - day headings                                              
     C                   EXSR      S005                                         
     C*                                                                         
     C** Print line - dates                                                     
     C                   EXSR      S006                                         
     C*                                                                         
     C** Print line - from location                                             
     C                   EXSR      S007                                         
     C*                                                                         
     C** Print line - to location                                               
     C                   EXSR      S008                                         
     C*                                                                         
     C** Print line - project                                                   
     C                   EXSR      S009                                         
     C*                                                                         
     C** Print line - expense codes                                             
     C                   EXSR      S010                                         
     C*                                                                         
     C** Print line - daily total (subtotal)                                    
     C                   EXSR      S011                                         
     C*                                                                         
     C** Print line - advances                                                  
     C                   EXSR      S012                                         
     C*                                                                         
     C** Print line - due company                                               
     C                   EXSR      S013                                         
     C*                                                                         
     C** Print line - due employee                                              
     C                   EXSR      S014                                         
     C*                                                                         
     C** Print line - expense details                                           
     C                   EXSR      S015                                         
     C*                                                                         
     C** Print line - project definitions                                       
     C                   EXSR      S016                                         
     C*                                                                         
     C** Check for more reports requested                                       
     C                   IF        NUMDATE7 < DFENDX                            
     C                   MOVE      NUMDATE7      DATISO                         
     C                   ADDDUR    1:*DAYS       DATISO                         
     C                   MOVE      DATISO        DFBEGX                         
     C                   GOTO      MAIN01                                       
     C                   ENDIF                                                  
     C*                                                                         
     C** End program                                                            
     C     MAIN99        TAG                                                    
     C                   EXSR      S999                                         
     C*                                                                         
     C*****************************************************************         
     C*  S000: DETERMINE REPORT DATE RANGE                            *         
     C*****************************************************************         
     C*                                                                         
     C     S000          BEGSR                                                  
     C*                                                                         
     C** Initialize overflow flag                                               
     C                   MOVE      'Y'           HDG2              1            
     C*                                                                         
     C** Initialize date fields                                                 
     C                   Z-ADD     0             DSPDATE1          6 0          
     C                   Z-ADD     0             DSPDATE2          6 0          
     C                   Z-ADD     0             DSPDATE3          6 0          
     C                   Z-ADD     0             DSPDATE4          6 0          
     C                   Z-ADD     0             DSPDATE5          6 0          
     C                   Z-ADD     0             DSPDATE6          6 0          
     C                   Z-ADD     0             DSPDATE7          6 0          
     C*                                                                         
     C                   Z-ADD     0             NUMDATE1          8 0          
     C                   Z-ADD     0             NUMDATE2          8 0          
     C                   Z-ADD     0             NUMDATE3          8 0          
     C                   Z-ADD     0             NUMDATE4          8 0          
     C                   Z-ADD     0             NUMDATE5          8 0          
     C                   Z-ADD     0             NUMDATE6          8 0          
     C                   Z-ADD     0             NUMDATE7          8 0          
     C*                                                                         
     C                   MOVE      *BLANKS       SHTDATE1          3            
     C                   MOVE      *BLANKS       SHTDATE2          3            
     C                   MOVE      *BLANKS       SHTDATE3          3            
     C                   MOVE      *BLANKS       SHTDATE4          3            
     C                   MOVE      *BLANKS       SHTDATE5          3            
     C                   MOVE      *BLANKS       SHTDATE6          3            
     C                   MOVE      *BLANKS       SHTDATE7          3            
     C*                                                                         
     C                   MOVE      *BLANKS       ALPHDATE1         9            
     C                   MOVE      *BLANKS       ALPHDATE2         9            
     C                   MOVE      *BLANKS       ALPHDATE3         9            
     C                   MOVE      *BLANKS       ALPHDATE4         9            
     C                   MOVE      *BLANKS       ALPHDATE5         9            
     C                   MOVE      *BLANKS       ALPHDATE6         9            
     C                   MOVE      *BLANKS       ALPHDATE7         9            
     C                   MOVE      *BLANKS       ALPHEND           3            
     C*                                                                         
     C                   MOVE      'N'           ACTIVE1           1            
     C                   MOVE      'N'           ACTIVE2           1            
     C                   MOVE      'N'           ACTIVE3           1            
     C                   MOVE      'N'           ACTIVE4           1            
     C                   MOVE      'N'           ACTIVE5           1            
     C                   MOVE      'N'           ACTIVE6           1            
     C                   MOVE      'N'           ACTIVE7           1            
     C*                                                                         
     C** Get day of week work week begins on                                    
     C     DFCMPX        CHAIN     R925P1                             96        
     C     *IN96         CABEQ     *ON           E000                           
     C*                                                                         
     C** Get day of week date range begins on                                   
     C     DFBEGX        CHAIN     DATFMT                             96        
     C     *IN96         CABEQ     *ON           E000                           
     C*                                                                         
     C** Get 1st day of week                                                    
     C     WEEKSTART     DOWNE     DAYSHT                                       
     C     *IN95         ANDEQ     *OFF                                         
     C                   READP     DATFMT                                 95    
     C                   ENDDO                                                  
     C     *IN96         CABEQ     *ON           E000                           
     C                   Z-ADD     MDYDEC        DSPDATE1                       
     C                   Z-ADD     CYMDDT        NUMDATE1                       
     C                   MOVE      DAYSHT        SHTDATE1                       
     C                   MOVE      DAY           ALPHDATE1                      
     C*                                                                         
     C** Get 2nd day of week                                                    
     C                   READ      DATFMT                                 95    
     C     *IN96         CABEQ     *ON           E000                           
     C                   Z-ADD     MDYDEC        DSPDATE2                       
     C                   Z-ADD     CYMDDT        NUMDATE2                       
     C                   MOVE      DAYSHT        SHTDATE2                       
     C                   MOVE      DAY           ALPHDATE2                      
     C*                                                                         
     C** Get 3rd day of week                                                    
     C                   READ      DATFMT                                 95    
     C     *IN96         CABEQ     *ON           E000                           
     C                   Z-ADD     MDYDEC        DSPDATE3                       
     C                   Z-ADD     CYMDDT        NUMDATE3                       
     C                   MOVE      DAYSHT        SHTDATE3                       
     C                   MOVE      DAY           ALPHDATE3                      
     C*                                                                         
     C** Get 4th day of week                                                    
     C                   READ      DATFMT                                 95    
     C     *IN96         CABEQ     *ON           E000                           
     C                   Z-ADD     MDYDEC        DSPDATE4                       
     C                   Z-ADD     CYMDDT        NUMDATE4                       
     C                   MOVE      DAYSHT        SHTDATE4                       
     C                   MOVE      DAY           ALPHDATE4                      
     C*                                                                         
     C** Get 5th day of week                                                    
     C                   READ      DATFMT                                 95    
     C     *IN96         CABEQ     *ON           E000                           
     C                   Z-ADD     MDYDEC        DSPDATE5                       
     C                   Z-ADD     CYMDDT        NUMDATE5                       
     C                   MOVE      DAYSHT        SHTDATE5                       
     C                   MOVE      DAY           ALPHDATE5                      
     C*                                                                         
     C** Get 6th day of week                                                    
     C                   READ      DATFMT                                 95    
     C     *IN96         CABEQ     *ON           E000                           
     C                   Z-ADD     MDYDEC        DSPDATE6                       
     C                   Z-ADD     CYMDDT        NUMDATE6                       
     C                   MOVE      DAYSHT        SHTDATE6                       
     C                   MOVE      DAY           ALPHDATE6                      
     C*                                                                         
     C** Get 7th day of week                                                    
     C                   READ      DATFMT                                 95    
     C     *IN96         CABEQ     *ON           E000                           
     C                   Z-ADD     MDYDEC        DSPDATE7                       
     C                   Z-ADD     CYMDDT        NUMDATE7                       
     C                   MOVE      DAYSHT        SHTDATE7                       
     C                   MOVE      DAY           ALPHDATE7                      
     C                   MOVEL     DAYSHT        ALPHEND                        
     C*                                                                         
     C** Set active flags for dates within range                                
     C                   IF        NUMDATE1 >= DFBEGX AND NUMDATE1 <= DFENDX    
     C                   MOVE      'Y'           ACTIVE1                        
     C                   ENDIF                                                  
     C*                                                                         
     C                   IF        NUMDATE2 >= DFBEGX AND NUMDATE2 <= DFENDX    
     C                   MOVE      'Y'           ACTIVE2                        
     C                   ENDIF                                                  
     C*                                                                         
     C                   IF        NUMDATE3 >= DFBEGX AND NUMDATE3 <= DFENDX    
     C                   MOVE      'Y'           ACTIVE3                        
     C                   ENDIF                                                  
     C*                                                                         
     C                   IF        NUMDATE4 >= DFBEGX AND NUMDATE4 <= DFENDX    
     C                   MOVE      'Y'           ACTIVE4                        
     C                   ENDIF                                                  
     C*                                                                         
     C                   IF        NUMDATE5 >= DFBEGX AND NUMDATE5 <= DFENDX    
     C                   MOVE      'Y'           ACTIVE5                        
     C                   ENDIF                                                  
     C*                                                                         
     C                   IF        NUMDATE6 >= DFBEGX AND NUMDATE6 <= DFENDX    
     C                   MOVE      'Y'           ACTIVE6                        
     C                   ENDIF                                                  
     C*                                                                         
     C                   IF        NUMDATE7 >= DFBEGX AND NUMDATE7 <= DFENDX    
     C                   MOVE      'Y'           ACTIVE7                        
     C                   ENDIF                                                  
     C*                                                                         
     C     E000          ENDSR                                                  
     C*                                                                         
     C*****************************************************************         
     C*  S001: PRINT LINE - COMPANY NAME                              *         
     C*****************************************************************         
     C*                                                                         
     C     S001          BEGSR                                                  
     C*                                                                         
     C     DFCMPX        CHAIN     R925P1                             95        
     C                   IF        *IN95 = *OFF                                 
     C                   EXCEPT    LINE1                                        
     C                   ENDIF                                                  
     C*                                                                         
     C     E001          ENDSR                                                  
     C*                                                                         
     C*****************************************************************         
     C*  S002: PRINT LINE - COMPANY ADDRESS                           *         
     C*****************************************************************         
     C*                                                                         
     C     S002          BEGSR                                                  
     C*                                                                         
     C     DFCMPX        CHAIN     R925P1                             95        
     C                   IF        *IN95 = *OFF                                 
     C                   EXCEPT    LINE2                                        
     C                   ENDIF                                                  
     C*                                                                         
     C     E002          ENDSR                                                  
     C*                                                                         
     C*****************************************************************         
     C*  S003: PRINT LINE - EMPLOYEE NAME                             *         
     C*****************************************************************         
     C*                                                                         
     C     S003          BEGSR                                                  
     C*                                                                         
     C     EMKY01        CHAIN     R928P1                             95        
     C                   IF        *IN95 = *OFF                                 
     C                   EXCEPT    LINE3                                        
     C                   ENDIF                                                  
     C*                                                                         
     C     E003          ENDSR                                                  
     C*                                                                         
     C*****************************************************************         
     C*  S004: PRINT LINE - ORGANIZATION, DEPARTMENT                  *         
     C*****************************************************************         
     C*                                                                         
     C     S004          BEGSR                                                  
     C*                                                                         
     C     EMKY01        CHAIN     R928P1                             95        
     C                   IF        *IN95 = *OFF                                 
     C                   EXCEPT    LINE4                                        
     C                   ENDIF                                                  
     C*                                                                         
     C     E004          ENDSR                                                  
     C*                                                                         
     C*****************************************************************         
     C*  S005: PRINT LINE - DAY HEADINGS                              *         
     C*****************************************************************         
     C*                                                                         
     C     S005          BEGSR                                                  
     C*                                                                         
     C                   EXCEPT    LINE5                                        
     C*                                                                         
     C     E005          ENDSR                                                  
     C*                                                                         
     C*****************************************************************         
     C*  S006: PRINT LINES - DATES                                    *         
     C*****************************************************************         
     C*                                                                         
     C     S006          BEGSR                                                  
     C*                                                                         
     C                   EXCEPT    LINE6                                        
     C*                                                                         
     C     E006          ENDSR                                                  
     C*                                                                         
     C*****************************************************************         
     C*  S007: PRINT LINES - FROM LOCATION                            *         
     C*****************************************************************         
     C*                                                                         
     C     S007          BEGSR                                                  
     C*                                                                         
     C** Initialize location and project fields                                 
     C                   MOVE      *BLANKS       FROMLOC1          9            
     C                   MOVE      *BLANKS       FROMLOC2          9            
     C                   MOVE      *BLANKS       FROMLOC3          9            
     C                   MOVE      *BLANKS       FROMLOC4          9            
     C                   MOVE      *BLANKS       FROMLOC5          9            
     C                   MOVE      *BLANKS       FROMLOC6          9            
     C                   MOVE      *BLANKS       FROMLOC7          9            
     C*                                                                         
     C                   MOVE      *BLANKS       TOLOC1            9            
     C                   MOVE      *BLANKS       TOLOC2            9            
     C                   MOVE      *BLANKS       TOLOC3            9            
     C                   MOVE      *BLANKS       TOLOC4            9            
     C                   MOVE      *BLANKS       TOLOC5            9            
     C                   MOVE      *BLANKS       TOLOC6            9            
     C                   MOVE      *BLANKS       TOLOC7            9            
     C*                                                                         
     C                   MOVE      *ZEROS        PRJUNQ1           5 0          
     C                   MOVE      *ZEROS        PRJUNQ2           5 0          
     C                   MOVE      *ZEROS        PRJUNQ3           5 0          
     C                   MOVE      *ZEROS        PRJUNQ4           5 0          
     C                   MOVE      *ZEROS        PRJUNQ5           5 0          
     C                   MOVE      *ZEROS        PRJUNQ6           5 0          
     C                   MOVE      *ZEROS        PRJUNQ7           5 0          
     C*                                                                         
     C** Get from location, to location, and project numbers                    
     C                   EXSR      S100                                         
     C*                                                                         
     C** Print line                                                             
     C                   EXCEPT    LINE7                                        
     C*                                                                         
     C     E007          ENDSR                                                  
     C*                                                                         
     C*****************************************************************         
     C*  S008: PRINT LINE - TO LOCATION                               *         
     C*****************************************************************         
     C*                                                                         
     C     S008          BEGSR                                                  
     C*                                                                         
     C                   EXCEPT    LINE8                                        
     C*                                                                         
     C     E008          ENDSR                                                  
     C*                                                                         
     C*****************************************************************         
     C*  S009: PRINT LINE - PROJECTS                                  *         
     C*****************************************************************         
     C*                                                                         
     C     S009          BEGSR                                                  
     C*                                                                         
     C                   EXCEPT    LINE9                                        
     C*                                                                         
     C     E009          ENDSR                                                  
     C*                                                                         
     C*****************************************************************         
     C*  S010: PRINT LINES - EXPENSE CODES                            *         
     C*****************************************************************         
     C*                                                                         
     C     S010          BEGSR                                                  
     C*                                                                         
     C** Initialize daily total fields                                          
     C                   MOVE      *ZEROS        DAILYAMT1         7 2          
     C                   MOVE      *ZEROS        DAILYAMT2         7 2          
     C                   MOVE      *ZEROS        DAILYAMT3         7 2          
     C                   MOVE      *ZEROS        DAILYAMT4         7 2          
     C                   MOVE      *ZEROS        DAILYAMT5         7 2          
     C                   MOVE      *ZEROS        DAILYAMT6         7 2          
     C                   MOVE      *ZEROS        DAILYAMT7         7 2          
     C                   MOVE      *ZEROS        DAILYTOT          7 2          
     C*                                                                         
     C     EXKY01        SETLL     R926P1                                       
     C     DFCMPX        READE     R926P1                                 96    
     C     *IN96         DOWEQ     *OFF                                         
     C*                                                                         
     C** Codes above 900 fall after daily totals                                
     C                   IF        EXPCDE >= 900                                
     C                   LEAVE                                                  
     C                   ENDIF                                                  
     C*                                                                         
     C** Only show active expense codes                                         
     C                   IF        EXPDSP = 'Y'                                 
     C*                                                                         
     C** Abbreviate expense description                                         
     C                   MOVEL     EXPTYP        EXPTYPABR        17            
     C                   MOVEL     EXPCAT        EXPCATABR         6            
     C*                                                                         
     C** Get expenses for this code on all transaction dates                    
     C                   EXSR      S200                                         
     C*                                                                         
     C** Print expense line                                                     
     C                   EXCEPT    LINE10                                       
     C*                                                                         
     C                   ENDIF                                                  
     C*                                                                         
     C     DFCMPX        READE     R926P1                                 96    
     C                   ENDDO                                                  
     C*                                                                         
     C     E010          ENDSR                                                  
     C*                                                                         
     C*****************************************************************         
     C*  S011: PRINT LINES - DAILY TOTALS                             *         
     C*****************************************************************         
     C*                                                                         
     C     S011          BEGSR                                                  
     C*                                                                         
     C                   EXCEPT    LINE11                                       
     C*                                                                         
     C     E011          ENDSR                                                  
     C*                                                                         
     C*****************************************************************         
     C*  S012: PRINT LINE - ADVANCES                                  *         
     C*****************************************************************         
     C*                                                                         
     C     S012          BEGSR                                                  
     C*                                                                         
     C     EXKY01        SETLL     R926P1                                       
     C     DFCMPX        READE     R926P1                                 96    
     C     *IN96         DOWEQ     *OFF                                         
     C*                                                                         
     C** Only show active expense codes                                         
     C                   IF        EXPDSP = 'Y' AND EXPADV = 'Y'                
     C*                                                                         
     C** Get advances for this code on all transaction dates                    
     C                   EXSR      S300                                         
     C*                                                                         
     C** Print advance line                                                     
     C                   EXCEPT    LINE12                                       
     C*                                                                         
     C                   ENDIF                                                  
     C*                                                                         
     C     DFCMPX        READE     R926P1                                 96    
     C                   ENDDO                                                  
     C*                                                                         
     C     E012          ENDSR                                                  
     C*                                                                         
     C*****************************************************************         
     C*  S013: PRINT LINE - AMOUNT DUE COMPANY                        *         
     C*****************************************************************         
     C*                                                                         
     C     S013          BEGSR                                                  
     C*                                                                         
     C     ADVTOT        SUB       DAILYTOT      CMPDUE            7 2          
     C                   IF        CMPDUE < 0                                   
     C                   Z-ADD     0             CMPDUE                         
     C                   ENDIF                                                  
     C                   EXCEPT    LINE13                                       
     C*                                                                         
     C     E013          ENDSR                                                  
     C*                                                                         
     C*****************************************************************         
     C*  S014: PRINT LINE - AMOUNT DUE EMPLOYEE                       *         
     C*****************************************************************         
     C*                                                                         
     C     S014          BEGSR                                                  
     C*                                                                         
     C     DAILYTOT      SUB       ADVTOT        EMPDUE            7 2          
     C                   IF        EMPDUE < 0                                   
     C                   Z-ADD     0             EMPDUE                         
     C                   ENDIF                                                  
     C                   EXCEPT    LINE14                                       
     C*                                                                         
     C     E014          ENDSR                                                  
     C*                                                                         
     C*****************************************************************         
     C*  S015: PRINT LINE - EXPENSE DETAILS                           *         
     C*****************************************************************         
     C*                                                                         
     C     S015          BEGSR                                                  
     C*                                                                         
     C                   MOVE      'Y'           PRTDETHDG         1            
     C*                                                                         
     C** Day 1                                                                  
     C                   IF        ACTIVE1 = 'Y'                                
     C                   Z-ADD     NUMDATE1      WRKDATE           8 0          
     C                   EXSR      S401                                         
     C                   ENDIF                                                  
     C*                                                                         
     C** Day 2                                                                  
     C                   IF        ACTIVE2 = 'Y'                                
     C                   Z-ADD     NUMDATE2      WRKDATE           8 0          
     C                   EXSR      S401                                         
     C                   ENDIF                                                  
     C*                                                                         
     C** Day 3                                                                  
     C                   IF        ACTIVE3 = 'Y'                                
     C                   Z-ADD     NUMDATE3      WRKDATE           8 0          
     C                   EXSR      S401                                         
     C                   ENDIF                                                  
     C*                                                                         
     C** Day 4                                                                  
     C                   IF        ACTIVE4 = 'Y'                                
     C                   Z-ADD     NUMDATE4      WRKDATE           8 0          
     C                   EXSR      S401                                         
     C                   ENDIF                                                  
     C*                                                                         
     C** Day 5                                                                  
     C                   IF        ACTIVE5 = 'Y'                                
     C                   Z-ADD     NUMDATE5      WRKDATE           8 0          
     C                   EXSR      S401                                         
     C                   ENDIF                                                  
     C*                                                                         
     C** Day 6                                                                  
     C                   IF        ACTIVE6 = 'Y'                                
     C                   Z-ADD     NUMDATE6      WRKDATE           8 0          
     C                   EXSR      S401                                         
     C                   ENDIF                                                  
     C*                                                                         
     C** Day 7                                                                  
     C                   IF        ACTIVE7 = 'Y'                                
     C                   Z-ADD     NUMDATE7      WRKDATE           8 0          
     C                   EXSR      S401                                         
     C                   ENDIF                                                  
     C*                                                                         
     C     E015          ENDSR                                                  
     C*                                                                         
     C*****************************************************************         
     C*  S016: PRINT LINE - PROJECT DEFINTIONS                        *         
     C*****************************************************************         
     C*                                                                         
     C     S016          BEGSR                                                  
     C*                                                                         
     C                   MOVE      'Y'           PRTPRJHDG         1            
     C*                                                                         
     C** Get projects                                                           
     C                   MOVE      *ZEROS        PROJECTS                       
     C*                                                                         
     C** Day 1                                                                  
     C                   IF        ACTIVE1 = 'Y'                                
     C                   Z-ADD     NUMDATE1      WRKDATE           8 0          
     C                   EXSR      S402                                         
     C                   ENDIF                                                  
     C*                                                                         
     C** Day 2                                                                  
     C                   IF        ACTIVE2 = 'Y'                                
     C                   Z-ADD     NUMDATE2      WRKDATE           8 0          
     C                   EXSR      S402                                         
     C                   ENDIF                                                  
     C*                                                                         
     C** Day 3                                                                  
     C                   IF        ACTIVE3 = 'Y'                                
     C                   Z-ADD     NUMDATE3      WRKDATE           8 0          
     C                   EXSR      S402                                         
     C                   ENDIF                                                  
     C*                                                                         
     C** Day 4                                                                  
     C                   IF        ACTIVE4 = 'Y'                                
     C                   Z-ADD     NUMDATE4      WRKDATE           8 0          
     C                   EXSR      S402                                         
     C                   ENDIF                                                  
     C*                                                                         
     C** Day 5                                                                  
     C                   IF        ACTIVE5 = 'Y'                                
     C                   Z-ADD     NUMDATE5      WRKDATE           8 0          
     C                   EXSR      S402                                         
     C                   ENDIF                                                  
     C*                                                                         
     C** Day 6                                                                  
     C                   IF        ACTIVE6 = 'Y'                                
     C                   Z-ADD     NUMDATE6      WRKDATE           8 0          
     C                   EXSR      S402                                         
     C                   ENDIF                                                  
     C*                                                                         
     C** Day 7                                                                  
     C                   IF        ACTIVE7 = 'Y'                                
     C                   Z-ADD     NUMDATE7      WRKDATE           8 0          
     C                   EXSR      S402                                         
     C                   ENDIF                                                  
     C*                                                                         
     C** Print projects                                                         
     C                   MOVE      'N'           PRTDETHDG                      
     C                   IF        PROJECTS(1) <> 0                             
     C     1             DO        9999          X1                             
     C                   IF        PROJECTS(X1) <> 0                            
     C                   EXSR      S500                                         
     C                   MOVE      *BLANKS       PR_PRJNME                      
     C                   MOVE      PROJECTS(X1)  PRJUNQ                         
     C     PRKY01        CHAIN     R927P1                             95        
     C                   EXCEPT    LINE16B                                      
     C                   ENDIF                                                  
     C                   ENDDO                                                  
     C                   ENDIF                                                  
     C*                                                                         
     C     E016          ENDSR                                                  
     C*                                                                         
     C*****************************************************************         
     C*  S100: GET FROM LOCATION, TO LOCATION, AND PROJECT NUMBER     *         
     C*****************************************************************         
     C*                                                                         
     C     S100          BEGSR                                                  
     C*                                                                         
     C** Day 1                                                                  
     C                   IF        ACTIVE1 = 'Y'                                
     C                   Z-ADD     NUMDATE1      WRKDATE           8 0          
     C                   EXSR      S101                                         
     C                   MOVEL     WRKFROM       FROMLOC1                       
     C                   MOVEL     WRKTO         TOLOC1                         
     C                   Z-ADD     WRKPRJ        PRJUNQ1                        
     C                   ENDIF                                                  
     C*                                                                         
     C** Day 2                                                                  
     C                   IF        ACTIVE2 = 'Y'                                
     C                   Z-ADD     NUMDATE2      WRKDATE           8 0          
     C                   EXSR      S101                                         
     C                   MOVEL     WRKFROM       FROMLOC2                       
     C                   MOVEL     WRKTO         TOLOC2                         
     C                   Z-ADD     WRKPRJ        PRJUNQ2                        
     C                   ENDIF                                                  
     C*                                                                         
     C** Day 3                                                                  
     C                   IF        ACTIVE3 = 'Y'                                
     C                   Z-ADD     NUMDATE3      WRKDATE           8 0          
     C                   EXSR      S101                                         
     C                   MOVEL     WRKFROM       FROMLOC3                       
     C                   MOVEL     WRKTO         TOLOC3                         
     C                   Z-ADD     WRKPRJ        PRJUNQ3                        
     C                   ENDIF                                                  
     C*                                                                         
     C** Day 4                                                                  
     C                   IF        ACTIVE4 = 'Y'                                
     C                   Z-ADD     NUMDATE4      WRKDATE           8 0          
     C                   EXSR      S101                                         
     C                   MOVEL     WRKFROM       FROMLOC4                       
     C                   MOVEL     WRKTO         TOLOC4                         
     C                   Z-ADD     WRKPRJ        PRJUNQ4                        
     C                   ENDIF                                                  
     C*                                                                         
     C** Day 5                                                                  
     C                   IF        ACTIVE5 = 'Y'                                
     C                   Z-ADD     NUMDATE5      WRKDATE           8 0          
     C                   EXSR      S101                                         
     C                   MOVEL     WRKFROM       FROMLOC5                       
     C                   MOVEL     WRKTO         TOLOC5                         
     C                   Z-ADD     WRKPRJ        PRJUNQ5                        
     C                   ENDIF                                                  
     C*                                                                         
     C** Day 6                                                                  
     C                   IF        ACTIVE6 = 'Y'                                
     C                   Z-ADD     NUMDATE6      WRKDATE           8 0          
     C                   EXSR      S101                                         
     C                   MOVEL     WRKFROM       FROMLOC6                       
     C                   MOVEL     WRKTO         TOLOC6                         
     C                   Z-ADD     WRKPRJ        PRJUNQ6                        
     C                   ENDIF                                                  
     C*                                                                         
     C** Day 7                                                                  
     C                   IF        ACTIVE7 = 'Y'                                
     C                   Z-ADD     NUMDATE7      WRKDATE           8 0          
     C                   EXSR      S101                                         
     C                   MOVEL     WRKFROM       FROMLOC7                       
     C                   MOVEL     WRKTO         TOLOC7                         
     C                   Z-ADD     WRKPRJ        PRJUNQ7                        
     C                   ENDIF                                                  
     C*                                                                         
     C     E100          ENDSR                                                  
     C*                                                                         
     C*****************************************************************         
     C*  S101: GET FROM LOCATION, TO LOCATION, AND PROJECT NUMBER     *         
     C*****************************************************************         
     C*                                                                         
     C     S101          BEGSR                                                  
     C*                                                                         
     C                   MOVE      *BLANKS       WRKFROM           9            
     C                   MOVE      *BLANKS       WRKTO             9            
     C                   MOVE      *ZEROS        WRKPRJ            5 0          
     C*                                                                         
     C     EMKY02        SETLL     R929P1                                       
     C     EMKY03        READE     R929P1                                 94    
     C     *IN94         DOWEQ     *OFF                                         
     C                   IF        WRKFROM = *BLANKS                            
     C                   MOVEL     TRNLC1        WRKFROM                        
     C                   ENDIF                                                  
     C                   IF        WRKTO   = *BLANKS                            
     C                   MOVEL     TRNLC2        WRKTO                          
     C                   ENDIF                                                  
     C                   IF        WRKPRJ  = *ZEROS                             
     C                   Z-ADD     PRJUNQ        WRKPRJ                         
     C                   ENDIF                                                  
     C     EMKY03        READE     R929P1                                 94    
     C                   ENDDO                                                  
     C*                                                                         
     C     E101          ENDSR                                                  
     C*                                                                         
     C*****************************************************************         
     C*  S200: GET EXPENSES FOR THIS CODE ON ALL TRANSACTION DATES    *         
     C*****************************************************************         
     C*                                                                         
     C     S200          BEGSR                                                  
     C*                                                                         
     C** Initialize line totals                                                 
     C                   MOVE      *ZEROS        LINEAMT1          7 2          
     C                   MOVE      *ZEROS        LINEAMT2          7 2          
     C                   MOVE      *ZEROS        LINEAMT3          7 2          
     C                   MOVE      *ZEROS        LINEAMT4          7 2          
     C                   MOVE      *ZEROS        LINEAMT5          7 2          
     C                   MOVE      *ZEROS        LINEAMT6          7 2          
     C                   MOVE      *ZEROS        LINEAMT7          7 2          
     C                   MOVE      *ZEROS        LINETOT           7 2          
     C*                                                                         
     C** Day 1                                                                  
     C                   IF        ACTIVE1 = 'Y'                                
     C                   Z-ADD     NUMDATE1      WRKDATE           8 0          
     C                   EXSR      S201                                         
     C                   Z-ADD     WRKAMT        LINEAMT1                       
     C                   ADD       WRKAMT        DAILYAMT1                      
     C                   ENDIF                                                  
     C*                                                                         
     C** Day 2                                                                  
     C                   IF        ACTIVE2 = 'Y'                                
     C                   Z-ADD     NUMDATE2      WRKDATE           8 0          
     C                   EXSR      S201                                         
     C                   Z-ADD     WRKAMT        LINEAMT2                       
     C                   ADD       WRKAMT        DAILYAMT2                      
     C                   ENDIF                                                  
     C*                                                                         
     C** Day 3                                                                  
     C                   IF        ACTIVE3 = 'Y'                                
     C                   Z-ADD     NUMDATE3      WRKDATE           8 0          
     C                   EXSR      S201                                         
     C                   Z-ADD     WRKAMT        LINEAMT3                       
     C                   ADD       WRKAMT        DAILYAMT3                      
     C                   ENDIF                                                  
     C*                                                                         
     C** Day 4                                                                  
     C                   IF        ACTIVE4 = 'Y'                                
     C                   Z-ADD     NUMDATE4      WRKDATE           8 0          
     C                   EXSR      S201                                         
     C                   Z-ADD     WRKAMT        LINEAMT4                       
     C                   ADD       WRKAMT        DAILYAMT4                      
     C                   ENDIF                                                  
     C*                                                                         
     C** Day 5                                                                  
     C                   IF        ACTIVE5 = 'Y'                                
     C                   Z-ADD     NUMDATE5      WRKDATE           8 0          
     C                   EXSR      S201                                         
     C                   Z-ADD     WRKAMT        LINEAMT5                       
     C                   ADD       WRKAMT        DAILYAMT5                      
     C                   ENDIF                                                  
     C*                                                                         
     C** Day 6                                                                  
     C                   IF        ACTIVE6 = 'Y'                                
     C                   Z-ADD     NUMDATE6      WRKDATE           8 0          
     C                   EXSR      S201                                         
     C                   Z-ADD     WRKAMT        LINEAMT6                       
     C                   ADD       WRKAMT        DAILYAMT6                      
     C                   ENDIF                                                  
     C*                                                                         
     C** Day 7                                                                  
     C                   IF        ACTIVE7 = 'Y'                                
     C                   Z-ADD     NUMDATE7      WRKDATE           8 0          
     C                   EXSR      S201                                         
     C                   Z-ADD     WRKAMT        LINEAMT7                       
     C                   ADD       WRKAMT        DAILYAMT7                      
     C                   ENDIF                                                  
     C*                                                                         
     C     E200          ENDSR                                                  
     C*                                                                         
     C*****************************************************************         
     C*  S201: GET EXPENSES FOR DAY                                   *         
     C*****************************************************************         
     C*                                                                         
     C     S201          BEGSR                                                  
     C*                                                                         
     C                   MOVE      *ZEROS        WRKAMT            7 2          
     C                   MOVE      *ZEROS        EXTAMT            7 2          
     C*                                                                         
     C     EMKY04        SETLL     REXP                                         
     C     EMKY05        READE     REXP                                   94    
     C     *IN94         DOWEQ     *OFF                                         
     C     EX_TRNAMT     MULT      EX_EXPMLT     EXTAMT                         
     C                   ADD       EXTAMT        WRKAMT                         
     C     EMKY05        READE     REXP                                   94    
     C                   ENDDO                                                  
     C*                                                                         
     C** Add amount to line total                                               
     C                   IF        EXPCDE = EXPLIN                              
     C                   ADD       WRKAMT        LINETOT                        
     C                   ADD       SUBAMT        LINETOT                        
     C                   Z-ADD     0             SUBAMT                         
     C                   ELSE                                                   
     C                   ADD       WRKAMT        SUBAMT            7 2          
     C                   ENDIF                                                  
     C*                                                                         
     C** Add amount to daily total                                              
     C                   ADD       WRKAMT        DAILYTOT                       
     C*                                                                         
     C     E201          ENDSR                                                  
     C*                                                                         
     C*****************************************************************         
     C*  S300: GET ADVANCES FOR THIS CODE ON ALL TRANSACTION DATES    *         
     C*****************************************************************         
     C*                                                                         
     C     S300          BEGSR                                                  
     C*                                                                         
     C** Initialize advance total                                               
     C                   MOVE      *ZEROS        ADVTOT            7 2          
     C*                                                                         
     C** Day 1                                                                  
     C                   IF        ACTIVE1 = 'Y'                                
     C                   Z-ADD     NUMDATE1      WRKDATE           8 0          
     C                   EXSR      S301                                         
     C                   ADD       WRKAMT        ADVTOT                         
     C                   ENDIF                                                  
     C*                                                                         
     C** Day 2                                                                  
     C                   IF        ACTIVE2 = 'Y'                                
     C                   Z-ADD     NUMDATE2      WRKDATE           8 0          
     C                   EXSR      S301                                         
     C                   ADD       WRKAMT        ADVTOT                         
     C                   ENDIF                                                  
     C*                                                                         
     C** Day 3                                                                  
     C                   IF        ACTIVE3 = 'Y'                                
     C                   Z-ADD     NUMDATE3      WRKDATE           8 0          
     C                   EXSR      S301                                         
     C                   ADD       WRKAMT        ADVTOT                         
     C                   ENDIF                                                  
     C*                                                                         
     C** Day 4                                                                  
     C                   IF        ACTIVE4 = 'Y'                                
     C                   Z-ADD     NUMDATE4      WRKDATE           8 0          
     C                   EXSR      S301                                         
     C                   ADD       WRKAMT        ADVTOT                         
     C                   ENDIF                                                  
     C*                                                                         
     C** Day 5                                                                  
     C                   IF        ACTIVE5 = 'Y'                                
     C                   Z-ADD     NUMDATE5      WRKDATE           8 0          
     C                   EXSR      S301                                         
     C                   ADD       WRKAMT        ADVTOT                         
     C                   ENDIF                                                  
     C*                                                                         
     C** Day 6                                                                  
     C                   IF        ACTIVE6 = 'Y'                                
     C                   Z-ADD     NUMDATE6      WRKDATE           8 0          
     C                   EXSR      S301                                         
     C                   ADD       WRKAMT        ADVTOT                         
     C                   ENDIF                                                  
     C*                                                                         
     C** Day 7                                                                  
     C                   IF        ACTIVE7 = 'Y'                                
     C                   Z-ADD     NUMDATE7      WRKDATE           8 0          
     C                   EXSR      S301                                         
     C                   ADD       WRKAMT        ADVTOT                         
     C                   ENDIF                                                  
     C*                                                                         
     C     E300          ENDSR                                                  
     C*                                                                         
     C*****************************************************************         
     C*  S301: GET ADVANCES FOR DAY                                   *         
     C*****************************************************************         
     C*                                                                         
     C     S301          BEGSR                                                  
     C*                                                                         
     C                   MOVE      *ZEROS        WRKAMT            7 2          
     C*                                                                         
     C     EMKY04        SETLL     REXP                                         
     C     EMKY05        READE     REXP                                   94    
     C     *IN94         DOWEQ     *OFF                                         
     C     EX_TRNAMT     MULT      EX_EXPMLT     EXTAMT                         
     C                   ADD       EXTAMT        WRKAMT                         
     C     EMKY05        READE     REXP                                   94    
     C                   ENDDO                                                  
     C*                                                                         
     C     E301          ENDSR                                                  
     C*                                                                         
     C*****************************************************************         
herexC*  S401: GET DETAILS FOR DAY                                    *         
     C*****************************************************************         
     C*                                                                         
     C     S401          BEGSR                                                  
     C*                                                                         
     C     EMKY06        SETLL     REXP                                         
     C     EMKY07        READE     REXP                                   94    
     C     *IN94         DOWEQ     *OFF                                         
     C                   IF        EX_TRNDET <> *BLANKS                         
     C                   EXSR      S500                                         
     C                   MOVE      EX_TRNDTE     DATISO                         
     C                   MOVE      DATISO        DATMDY                         
     C                   MOVE      DATMDY        OUDATE            6 0          
     C                   EXCEPT    LINE15B                                      
     C                   ENDIF                                                  
     C     EMKY06        READE     REXP                                   94    
     C                   ENDDO                                                  
     C*                                                                         
     C     E401          ENDSR                                                  
     C*                                                                         
     C*****************************************************************         
herexC*  S402: GET PROJECTS FOR DAY                                   *         
     C*****************************************************************         
     C*                                                                         
     C     S402          BEGSR                                                  
     C*                                                                         
     C                   Z-ADD     0             LOWVAL            5 0          
     C*                                                                         
     C     EMKY06        SETLL     REXP                                         
     C     EMKY07        READE     REXP                                   94    
     C     *IN94         DOWEQ     *OFF                                         
     C                   IF        EX_PRJUNQ <> *ZEROS                          
     C*                                                                         
     C                   Z-ADD     1             X1                5 0          
     C     EX_PRJUNQ     LOOKUP    PROJECTS(X1)                           95    
     C     *IN95         IFEQ      '0'                                          
     C                   Z-ADD     1             X1                5 0          
     C     LOWVAL        LOOKUP    PROJECTS(X1)                           95    
     C     *IN95         IFEQ      '1'                                          
     C                   MOVE      EX_PRJUNQ     PROJECTS(X1)                   
     C                   ENDIF                                                  
     C                   ENDIF                                                  
     C*                                                                         
     C                   ENDIF                                                  
     C     EMKY06        READE     REXP                                   94    
     C                   ENDDO                                                  
     C*                                                                         
     C     E402          ENDSR                                                  
     C*                                                                         
     C*****************************************************************         
     C*  S500: PRINT HEADER IF REQUIRED                               *         
     C*****************************************************************         
     C*                                                                         
     C     S500          BEGSR                                                  
     C*                                                                         
     C                   IF        HDG2 = 'Y'                                   
     C                   EXCEPT    LINE1                                        
     C                   EXCEPT    LINE2                                        
     C                   EXCEPT    LINE3                                        
     C                   EXCEPT    LINE4X                                       
     C                   MOVE      'N'           HDG2                           
     C                   ENDIF                                                  
     C*                                                                         
     C                   IF        PRTDETHDG = 'Y'                              
     C                   EXCEPT    LINE15A                                      
     C                   MOVE      'N'           PRTDETHDG                      
     C                   ENDIF                                                  
     C*                                                                         
     C                   IF        PRTPRJHDG = 'Y'                              
     C                   EXCEPT    LINE16A                                      
     C                   MOVE      'N'           PRTPRJHDG                      
     C                   ENDIF                                                  
     C*                                                                         
     C     E500          ENDSR                                                  
     C*                                                                         
     C*****************************************************************         
     C*  S999: END PROGRAM                                            *         
     C*****************************************************************         
     C*                                                                         
     C     S999          BEGSR                                                  
     C*                                                                         
     C                   MOVE      *ON           *INLR                          
     C                   RETURN                                                 
     C*                                                                         
     C     E999          ENDSR                                                  
     C*                                                                         
     C*****************************************************************         
     C*  INZSR: INITIALIZATION SUBROUTINE                             *         
     C*****************************************************************         
     C*                                                                         
     C     *INZSR        BEGSR                                                  
     C*                                                                         
     C** Entry parms                                                            
     C     *ENTRY        PLIST                                                  
     C                   PARM                    WKCMPX            5            
     C                   PARM                    WKEMPX            5            
     C                   PARM                    WKBEGX            8            
     C                   PARM                    WKENDX            8            
     C*                                                                         
     C** Klists                                                                 
     C     EMKY01        KLIST                                                  
     C                   KFLD                    DFCMPX                         
     C                   KFLD                    DFEMPX                         
     C*                                                                         
     C     EMKY02        KLIST                                                  
     C                   KFLD                    DFCMPX                         
     C                   KFLD                    DFEMPX                         
     C                   KFLD                    WRKDATE                        
     C                   KFLD                    LOWUNQ                         
     C*                                                                         
     C     EMKY03        KLIST                                                  
     C                   KFLD                    DFCMPX                         
     C                   KFLD                    DFEMPX                         
     C                   KFLD                    WRKDATE                        
     C*                                                                         
     C     EMKY04        KLIST                                                  
     C                   KFLD                    DFCMPX                         
     C                   KFLD                    DFEMPX                         
     C                   KFLD                    WRKDATE                        
     C                   KFLD                    EXPCDE                         
     C                   KFLD                    LOWUNQ                         
     C*                                                                         
     C     EMKY05        KLIST                                                  
     C                   KFLD                    DFCMPX                         
     C                   KFLD                    DFEMPX                         
     C                   KFLD                    WRKDATE                        
     C                   KFLD                    EXPCDE                         
     C*                                                                         
     C     EMKY06        KLIST                                                  
     C                   KFLD                    DFCMPX                         
     C                   KFLD                    DFEMPX                         
     C                   KFLD                    WRKDATE                        
     C                   KFLD                    LOWCDE                         
     C*                                                                         
     C     EMKY07        KLIST                                                  
     C                   KFLD                    DFCMPX                         
     C                   KFLD                    DFEMPX                         
     C                   KFLD                    WRKDATE                        
     C*                                                                         
     C     EXKY01        KLIST                                                  
     C                   KFLD                    DFCMPX                         
     C                   KFLD                    LOWCDE                         
     C*                                                                         
     C     PRKY01        KLIST                                                  
     C                   KFLD                    DFCMPX                         
     C                   KFLD                    PRJUNQ                         
     C*                                                                         
     C** Initialize fields                                                      
     C                   MOVE      WKCMPX        DFCMPX            5 0          
     C                   MOVE      WKEMPX        DFEMPX            5 0          
     C                   MOVE      WKBEGX        DFBEGX            8 0          
     C                   MOVE      WKENDX        DFENDX            8 0          
     C                   MOVE      *ZEROS        LOWUNQ            5 0          
     C                   MOVE      *ZEROS        LOWCDE            3 0          
     C                   MOVE      'N'           WKDONE            1            
     C*                                                                         
     C                   ENDSR                                                  
     C*                                                                         
     C*****************************************************************         
     O*                                                                         
     OQSYSPRT   E            LINE1            04                                
     O                       CMPNME              41                             
     O                                           78 'EXPENSE REPORT'            
     O          E            LINE1       0                                      
     O                                           78 'EXPENSE REPORT'            
     O*                                                                         
     O          E            LINE2       1                                      
     O                       CMPAD1              26                             
     O                       CMPCTY              47                             
     O                       CMPSTE              50                             
     O                       CMPZP5        Z     56                             
     O                       CMPZP4        Z     61                             
     O*                                                                         
     O          E            LINE2       2                                      
     O                       UNLN                63                             
     O                       UNLN               119                             
     O*                                                                         
     O          E            LINE3       1                                      
     O                                            1 '|'                         
     O                                           16 'Employee Name:'            
     O                                           63 '|'                         
     O                                           83 'Employee Signature:'       
     O                                          120 '|'                         
     O*                                                                         
     O          E            LINE3       1                                      
     O                                            1 '|'                         
     O                       EMPNME              42                             
     O                                           63 '|'                         
     O                                          120 '|'                         
     O*                                                                         
     O          E            LINE3       1                                      
     O                                            1 '|'                         
     O                       UNLN                63                             
     O                       UNLN               119                             
     O                                          120 '|'                         
     O*                                                                         
     O          E            LINE4       1                                      
     O                                            1 '|'                         
     O                                            6 'Org:'                      
     O                                           13 '|'                         
     O                                           19 'Dept:'                     
     O                                           24 '|'                         
     O                                           32 'Clock#:'                   
     O                                           35 '|'                         
     O                                           48 'Week Ending:'              
     O                       ALPHEND             52                             
     O                                           63 '|'                         
     O                                           77 'Mgr Approval:'             
     O                                          120 '|'                         
     O*                                                                         
     O          E            LINE4       1                                      
     O                                            1 '|'                         
     O                       DFCMPX               7 '    0'                     
     O                                           13 '|'                         
     O                       EMPDPT              20 '     0'                    
     O                                           24 '|'                         
     O                       EMPNBR              33 '       0'                  
     O                                           35 '|'                         
     O                       DSPDATE7            44 '  /  / 0'                  
     O                                           63 '|'                         
     O                                          120 '|'                         
     O*                                                                         
     O          E            LINE4       1                                      
     O                                            1 '|'                         
     O                       UNLN                63                             
     O                       UNLN               119                             
     O                                          120 '|'                         
     O*                                                                         
     O          E            LINE4X      1                                      
     O                                            1 '|'                         
     O                                            6 'Org:'                      
     O                                           13 '|'                         
     O                                           19 'Dept:'                     
     O                                           24 '|'                         
     O                                           32 'Clock#:'                   
     O                                           35 '|'                         
     O                                           48 'Week Ending:'              
     O                       ALPHEND             52                             
     O                                           63 '|'                         
     O                                           77 'Mgr Approval:'             
     O                                          120 '|'                         
     O*                                                                         
     O          E            LINE4X      1                                      
     O                                            1 '|'                         
     O                       DFCMPX               7 '    0'                     
     O                                           13 '|'                         
     O                       EMPDPT              20 '     0'                    
     O                                           24 '|'                         
     O                       EMPNBR              33 '       0'                  
     O                                           35 '|'                         
     O                       DSPDATE7            44 '  /  / 0'                  
     O                                           63 '|'                         
     O                                          120 '|'                         
     O*                                                                         
     O          E            LINE4X      1                                      
     O                       UNLN                63                             
     O                       UNLN               119                             
     O*                                                                         
     O          E            LINE5       1                                      
     O                                            1 '|'                         
     O                                           24 '|'                         
     O                       SHTDATE1            31                             
     O                                           35 '|'                         
     O                       SHTDATE2            42                             
     O                                           46 '|'                         
     O                       SHTDATE3            53                             
     O                                           57 '|'                         
     O                       SHTDATE4            64                             
     O                                           68 '|'                         
     O                       SHTDATE5            75                             
     O                                           79 '|'                         
     O                       SHTDATE6            86                             
     O                                           90 '|'                         
     O                       SHTDATE7            97                             
     O                                          101 '|'                         
     O                                          109 'TOTALS'                    
     O                                          111 '|'                         
     O                                          118 'GL REF'                    
     O                                          120 '|'                         
     O*                                                                         
     O          E            LINE5       1                                      
     O                                            1 '|'                         
     O                                           24 '|'                         
     O                                           35 '|'                         
     O                                           46 '|'                         
     O                                           57 '|'                         
     O                                           68 '|'                         
     O                                           79 '|'                         
     O                                           90 '|'                         
     O                                          101 '|'                         
     O                                          111 '|'                         
     O                                          120 '|'                         
     O*                                                                         
     O          E            LINE5       0                                      
     O                       UNLN                63                             
     O                       UNLN               119                             
     O*                                                                         
     O          E            LINE6       1                                      
     O                                            1 '|'                         
     O                                           13 '|'                         
     O                                           20 'DATES:'                    
     O                                           24 '|'                         
     O                       DSPDATE1            33 '  /  / 0'                  
     O                                           35 '|'                         
     O                       DSPDATE2            44 '  /  / 0'                  
     O                                           46 '|'                         
     O                       DSPDATE3            55 '  /  / 0'                  
     O                                           57 '|'                         
     O                       DSPDATE4            66 '  /  / 0'                  
     O                                           68 '|'                         
     O                       DSPDATE5            77 '  /  / 0'                  
     O                                           79 '|'                         
     O                       DSPDATE6            88 '  /  / 0'                  
     O                                           90 '|'                         
     O                       DSPDATE7            99 '  /  / 0'                  
     O                                          101 '|'                         
     O                                          111 '|'                         
     O                                          120 '|'                         
     O*                                                                         
     O          E            LINE6       1                                      
     O                                            1 '|'                         
     O                                           24 '|'                         
     O                                           35 '|'                         
     O                                           46 '|'                         
     O                                           57 '|'                         
     O                                           68 '|'                         
     O                                           79 '|'                         
     O                                           90 '|'                         
     O                                          101 '|'                         
     O                                          111 '|'                         
     O                                          120 '|'                         
     O*                                                                         
     O          E            LINE6       0                                      
     O                       UNLN                63                             
     O                       UNLN               119                             
     O*                                                                         
     O          E            LINE7       1                                      
     O                                            1 '|'                         
     O                                           11 'ITINERARY:'                
     O                                           13 '|'                         
     O                                           19 'FROM:'                     
     O                                           24 '|'                         
     O                       FROMLOC1            34                             
     O                                           35 '|'                         
     O                       FROMLOC2            45                             
     O                                           46 '|'                         
     O                       FROMLOC3            56                             
     O                                           57 '|'                         
     O                       FROMLOC4            67                             
     O                                           68 '|'                         
     O                       FROMLOC5            78                             
     O                                           79 '|'                         
     O                       FROMLOC6            89                             
     O                                           90 '|'                         
     O                       FROMLOC7           100                             
     O                                          101 '|'                         
     O                                          111 '|'                         
     O                                          120 '|'                         
     O*                                                                         
     O          E            LINE7       1                                      
     O                                            1 '|'                         
     O                                           24 '|'                         
     O                                           35 '|'                         
     O                                           46 '|'                         
     O                                           57 '|'                         
     O                                           68 '|'                         
     O                                           79 '|'                         
     O                                           90 '|'                         
     O                                          101 '|'                         
     O                                          111 '|'                         
     O                                          120 '|'                         
     O*                                                                         
     O          E            LINE7       0                                      
     O                       UNLN                63                             
     O                       UNLN               119                             
     O*                                                                         
     O          E            LINE8       1                                      
     O                                            1 '|'                         
     O                                           13 '|'                         
     O                                           17 'TO:'                       
     O                                           24 '|'                         
     O                       TOLOC1              34                             
     O                                           35 '|'                         
     O                       TOLOC2              45                             
     O                                           46 '|'                         
     O                       TOLOC3              56                             
     O                                           57 '|'                         
     O                       TOLOC4              67                             
     O                                           68 '|'                         
     O                       TOLOC5              78                             
     O                                           79 '|'                         
     O                       TOLOC6              89                             
     O                                           90 '|'                         
     O                       TOLOC7             100                             
     O                                          101 '|'                         
     O                                          111 '|'                         
     O                                          120 '|'                         
     O*                                                                         
     O          E            LINE8       1                                      
     O                                            1 '|'                         
     O                                           24 '|'                         
     O                                           35 '|'                         
     O                                           46 '|'                         
     O                                           57 '|'                         
     O                                           68 '|'                         
     O                                           79 '|'                         
     O                                           90 '|'                         
     O                                          101 '|'                         
     O                                          111 '|'                         
     O                                          120 '|'                         
     O*                                                                         
     O          E            LINE8       0                                      
     O                       UNLN                63                             
     O                       UNLN               119                             
     O*                                                                         
     O          E            LINE9       1                                      
     O                                            1 '|'                         
     O                                            9 'PURPOSE:'                  
     O                                           24 '|'                         
     O                       PRJUNQ1             33 '    0'                     
     O                                           35 '|'                         
     O                       PRJUNQ2             44 '    0'                     
     O                                           46 '|'                         
     O                       PRJUNQ3             55 '    0'                     
     O                                           57 '|'                         
     O                       PRJUNQ4             66 '    0'                     
     O                                           68 '|'                         
     O                       PRJUNQ5             77 '    0'                     
     O                                           79 '|'                         
     O                       PRJUNQ6             88 '    0'                     
     O                                           90 '|'                         
     O                       PRJUNQ7             99 '    0'                     
     O                                          101 '|'                         
     O                                          111 '|'                         
     O                                          120 '|'                         
     O*                                                                         
     O          E            LINE9       1                                      
     O                                            1 '|'                         
     O                                           24 '|'                         
     O                                           35 '|'                         
     O                                           46 '|'                         
     O                                           57 '|'                         
     O                                           68 '|'                         
     O                                           79 '|'                         
     O                                           90 '|'                         
     O                                          101 '|'                         
     O                                          111 '|'                         
     O                                          120 '|'                         
     O*                                                                         
     O          E            LINE9       0                                      
     O                       UNLN                63                             
     O                       UNLN               119                             
     O*                                                                         
     O          E            LINE10      1                                      
     O                                            1 '|'                         
     O                       EXPCDE               5 '  0'                       
     O                       EXPTYPABR           23                             
     O                                           24 '|'                         
     O                       LINEAMT1            33 '     . 0'                  
     O                                           35 '|'                         
     O                       LINEAMT2            44 '     . 0'                  
     O                                           46 '|'                         
     O                       LINEAMT3            55 '     . 0'                  
     O                                           57 '|'                         
     O                       LINEAMT4            66 '     . 0'                  
     O                                           68 '|'                         
     O                       LINEAMT5            77 '     . 0'                  
     O                                           79 '|'                         
     O                       LINEAMT6            88 '     . 0'                  
     O                                           90 '|'                         
     O                       LINEAMT7            99 '     . 0'                  
     O                                          101 '|'                         
     O                       LINETOT            109 '     . 0'                  
     O                                          111 '|'                         
     O                       EXPCATABR          118                             
     O                                          120 '|'                         
     O*                                                                         
     O          E            LINE10      1                                      
     O                                            1 '|'                         
     O                                           24 '|'                         
     O                                           35 '|'                         
     O                                           46 '|'                         
     O                                           57 '|'                         
     O                                           68 '|'                         
     O                                           79 '|'                         
     O                                           90 '|'                         
     O                                          101 '|'                         
     O                                          111 '|'                         
     O                                          120 '|'                         
     O*                                                                         
     O          E            LINE10      0                                      
     O                       UNLN                63                             
     O                       UNLN               119                             
     O*                                                                         
     O          E            LINE11      1                                      
     O                                            1 '|'                         
     O                                           14 'DAILY TOTALS:'             
     O                                           24 '|'                         
     O                       DAILYAMT1           33 '     . 0'                  
     O                                           35 '|'                         
     O                       DAILYAMT2           44 '     . 0'                  
     O                                           46 '|'                         
     O                       DAILYAMT3           55 '     . 0'                  
     O                                           57 '|'                         
     O                       DAILYAMT4           66 '     . 0'                  
     O                                           68 '|'                         
     O                       DAILYAMT5           77 '     . 0'                  
     O                                           79 '|'                         
     O                       DAILYAMT6           88 '     . 0'                  
     O                                           90 '|'                         
     O                       DAILYAMT7           99 '     . 0'                  
     O                                          101 '|'                         
     O                       DAILYTOT           109 '     . 0'                  
     O                                          111 '|'                         
     O                                          120 '|'                         
     O*                                                                         
     O          E            LINE11      1                                      
     O                                          120 '|'                         
     O*                                                                         
     O          E            LINE11      0                                      
     O                       UNLN                63                             
     O                       UNLN               119                             
     O*                                                                         
     O          E            LINE12      1                                      
     O                                           79 '|'                         
     O                       EXPTYP             100                             
     O                                          101 '|'                         
     O                       ADVTOT             109 '     . 0'                  
     O                                          111 '|'                         
     O                                          120 '|'                         
     O*                                                                         
     O          E            LINE12      0                                      
     O                       EXPTYP             100                             
     O*                                                                         
     O          E            LINE12      1                                      
     O                                           79 '|'                         
     O                                          101 '|'                         
     O                                          111 '|'                         
     O                                          120 '|'                         
     O*                                                                         
     O          E            LINE12      0                                      
     O                       UNLX               119                             
     O*                                                                         
     O          E            LINE13      1                                      
     O                                           79 '|'                         
     O                                           92 'COMPANY DUE:'              
     O                                          101 '|'                         
     O                       CMPDUE             109 '     . 0'                  
     O                                          111 '|'                         
     O                                          120 '|'                         
     O*                                                                         
     O          E            LINE13      0                                      
     O                                           92 'COMPANY DUE:'              
     O*                                                                         
     O          E            LINE13      1                                      
     O                                           79 '|'                         
     O                                          101 '|'                         
     O                                          111 '|'                         
     O                                          120 '|'                         
     O*                                                                         
     O          E            LINE13      0                                      
     O                       UNLX               119                             
     O*                                                                         
     O          E            LINE14      1                                      
     O                                           79 '|'                         
     O                                           93 'EMPLOYEE DUE:'             
     O                                          101 '|'                         
     O                       EMPDUE             109 '     . 0'                  
     O                                          111 '|'                         
     O                                          120 '|'                         
     O*                                                                         
     O          E            LINE14      0                                      
     O                                           93 'EMPLOYEE DUE:'             
     O*                                                                         
     O          E            LINE14      1                                      
     O                       UNLX               119                             
     O*                                                                         
     O          E            LINE14      5                                      
     O                                           19 'EMPLOYEE SIGNATURE:'       
     O                                           46 '__________________________'
     O*                                                                         
     O          E            LINE15A     4                                      
     O                                            4 'Code'                      
     O                                           12 'Date'                      
     O                                           31 'Expense Details'           
     O*                                                                         
     O          E            LINE15A     1                                      
     O                                            4 '----'                      
     O                                           14 '------'                    
     O                                           36 '--------------------'      
     O                                           56 '--------------------'      
     O                                           76 '--------------------'      
     O*                                                                         
     O          E            LINE15B     1                                      
     O                       EX_EXPCDE            3 '  0'                       
     O                       OUDATE              14 '  /  /0 '                  
     O                       EX_TRNDET           76                             
     O*                                                                         
     O          E            LINE16A     4                                      
     O                                           14 'Project Number'            
     O                                           35 'Project Description'       
     O*                                                                         
     O          E            LINE16A     1                                      
     O                                           14 '--------------'            
     O                                           36 '--------------------'      
     O                                           56 '--------------------'      
     O*                                                                         
     O          E            LINE16B     1                                      
     O                       PROJECTS(X1)         8 '    0'                     
     O                       PR_PRJNME           56                             
     C*                                                                         
     C*****************************************************************         
     O*                                                                         
** CTDATA UNLN                                                                  
-------------------------------------------------------------------             
** CTDATA UNLX                                                                  
----------------------------------------                                        
