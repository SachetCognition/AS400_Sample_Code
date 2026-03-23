     H DATFMT(*ISO)                                                             
     H*****************************************************************         
     H* FastBreed Technologies, Inc. © 2000                           *         
     H*****************************************************************         
     H*****************************************************************         
     H**                                                             **         
     H** OBJECT ID: IS923R98                                         **         
     H**                                                             **         
     H** OBJECT DESCRIPTION: EXPENSE SYSTEM REPORTS BY EXPENSE CODE  **         
     H**                                                             **         
     H** MOD#   DATE  PGMR DESCRIPTION                               **         
     H** ----  ------ ---- ----------------------------------------- **         
     H**                                                             **         
     H*****************************************************************         
     F*                                                                         
     FIS929L9   IF   E           K DISK                                         
     FIS925P1   IF   E           K DISK                                         
     FIS926P1   IF   E           K DISK    PREFIX(EX_)                          
     FQSYSPRT   O    F   80        PRINTER OFLIND(*INOF)                        
     D*                                                                         
     D ARYN            S              1    DIM(40)                              
     D EXPC            S              3  0 DIM(9999)                            
     D EXPA            S             10  2 DIM(9999)                            
     D*                                                                         
     D DATISO          S               D   DATFMT(*ISO)                         
     D DATMDY          S               D   DATFMT(*MDY)                         
     D*                                                                         
     D PSDS           SDS                                                       
     D  PGMNAM           *PROC                                                  
     D  USERID               254    263                                         
     I*                                                                         
     C*****************************************************************         
     C*  MAINLINE                                                     *         
     C*****************************************************************         
     C*                                                                         
     C** Collect amounts                                                        
     C                   EXSR      S001                                         
     C*                                                                         
     C** Print expense code amounts                                             
     C                   EXSR      S003                                         
     C*                                                                         
     C** End program                                                            
     C                   MOVE      '1'           *INLR                          
     C                   RETURN                                                 
     C*                                                                         
     C*****************************************************************         
     C*   S001: COLLECT AMOUNTS                                       *         
     C*****************************************************************         
     C*                                                                         
     C     S001          BEGSR                                                  
     C*                                                                         
     C** Initialize arrays                                                      
     C                   MOVEA     *ZEROS        EXPC                           
     C                   MOVEA     *ZEROS        EXPA                           
     C*                                                                         
     C** Read work file                                                         
     C     CMPUNQ        SETLL     R929P1                                       
     C     CMPUNQ        READE     R929P1                                 90    
     C     *IN90         DOWEQ     *OFF                                         
     C*                                                                         
     C** Skip if not for selected project                                       
     C                   IF        DFPUNQ <> 0 AND DFPUNQ <> PRJUNQ             
     C     CMPUNQ        READE     R929P1                                 90    
     C                   ITER                                                   
     C                   ENDIF                                                  
     C*                                                                         
     C** Skip if not for selected cost center                                   
     C                   IF        DFCUNQ <> 0 AND DFCUNQ <> CSTUNQ             
     C     CMPUNQ        READE     R929P1                                 90    
     C                   ITER                                                   
     C                   ENDIF                                                  
     C*                                                                         
     C** Skip if not for selected employee                                      
     C                   IF        DFNBRX <> 0 AND DFNBRX <> EMPNBR             
     C     CMPUNQ        READE     R929P1                                 90    
     C                   ITER                                                   
     C                   ENDIF                                                  
     C*                                                                         
     C** Skip if not for selected department                                    
     C                   IF        DFDPTX <> 0 AND DFDPTX <> EMPDPT             
     C     CMPUNQ        READE     R929P1                                 90    
     C                   ITER                                                   
     C                   ENDIF                                                  
     C*                                                                         
     C** Skip if not for selected expense code                                  
     C                   IF        DFCDEX <> 0 AND DFCDEX <> EXPCDE             
     C     CMPUNQ        READE     R929P1                                 90    
     C                   ITER                                                   
     C                   ENDIF                                                  
     C*                                                                         
     C**                 IF        EXPCDE >= 900 OR EXPCDE <> EXPLIN            
     C                   IF        EXPCDE >= 900                                
     C                   READ      R929P1                                 90    
     C                   ITER                                                   
     C                   ENDIF                                                  
     C*                                                                         
     C** Skip if not for selected date range                                    
     C                   IF        TRNDTE < BEGDTE OR TRNDTE > ENDDTE           
     C     CMPUNQ        READE     R929P1                                 90    
     C                   ITER                                                   
     C                   ENDIF                                                  
     C*                                                                         
     C** Save amounts to arrays                                                 
     C                   EXSR      S002                                         
     C*                                                                         
     C     CMPUNQ        READE     R929P1                                 90    
     C                   ENDDO                                                  
     C*                                                                         
     C     E001          ENDSR                                                  
     C*                                                                         
     C*****************************************************************         
     C*   S002: SAVE AMOUNTS TO ARRAYS                                *         
     C*****************************************************************         
     C*                                                                         
     C     S002          BEGSR                                                  
     C*                                                                         
     C                   Z-ADD     1             I                 5 0          
     C     EXPCDE        LOOKUP    EXPC(I)                                62    
     C*                                                                         
     C                   IF        *IN62 = *ON                                  
     C                   ADD       TRNAMT        EXPA(I)                        
     C                   GOTO      E002                                         
     C                   ENDIF                                                  
     C*                                                                         
     C                   IF        *IN62 = *OFF                                 
     C                   Z-ADD     1             I                 5 0          
     C                   MOVE      *ZEROS        WKFLD3            3 0          
     C     WKFLD3        LOOKUP    EXPC(I)                                62    
     C                   IF        *IN62 = *ON                                  
     C                   MOVE      EXPCDE        EXPC(I)                        
     C                   ADD       TRNAMT        EXPA(I)                        
     C                   ENDIF                                                  
     C                   ENDIF                                                  
     C*                                                                         
     C     E002          ENDSR                                                  
     C*                                                                         
     C*****************************************************************         
     C*   S003: PRINT EXPENSE CODE AMOUNTS                            *         
     C*****************************************************************         
     C*                                                                         
     C     S003          BEGSR                                                  
     C*                                                                         
     C                   Z-ADD     0             SUBAMT           10 2          
     C                   Z-ADD     0             TOTAMT           10 2          
     C                   MOVE      *ON           *IN51                          
     C     1             DO        9999          I                              
     C                   IF        EXPC(I) <> 0                                 
     C*                                                                         
     C                   MOVE      EXPC(I)       OUEXPCDE          3 0          
     C                   Z-ADD     EXPA(I)       OUEXPAMT         10 2          
     C                   Z-ADD     OUEXPAMT      SUBAMT                         
     C                   ADD       OUEXPAMT      TOTAMT                         
     C                   MOVE      *BLANKS       OUEXPTYP         20            
     C     EXKY01        CHAIN     R926P1                             95        
     C                   IF        *IN95 = *OFF                                 
     C                   MOVEL     EX_EXPTYP     OUEXPTYP                       
     C                   ENDIF                                                  
     C*                                                                         
     C                   EXSR      CHKOVF                                       
     C                   EXCEPT    SUB1                                         
     C                   MOVE      *OFF          *IN51                          
     C*                                                                         
     C                   ENDIF                                                  
     C                   ENDDO                                                  
     C*                                                                         
     C                   EXSR      CHKOVF                                       
     C                   EXCEPT    TOT1                                         
     C*                                                                         
     C     E003          ENDSR                                                  
     C*                                                                         
     C*****************************************************************         
     C*   S800: CENTER COMPANY NAME                                   *         
     C*****************************************************************         
     C*                                                                         
     C     S800          BEGSR                                                  
     C*                                                                         
     C                   MOVEA     *BLANKS       ARYN                           
     C                   MOVE      *ZEROS        CENTER            3 0          
     C                   EVAL      CENTER = (%SIZE(CMPNME)/2)+1                 
     C                   MOVE      *ZEROS        LSTPOS            3 0          
     C     ' '           CHECKR    CMPNME        LSTPOS                   95    
     C*                                                                         
     C                   IF        *IN95 = *ON AND LSTPOS > 0                   
     C     LSTPOS        DIV       2             LSTPOS                         
     C     CENTER        SUB       LSTPOS        I                 5 0          
     C                   MOVEA     CMPNME        ARYN(I)                        
     C                   MOVE      *BLANKS       CMPNME                         
     C                   MOVEA     ARYN          CMPNME                         
     C                   ENDIF                                                  
     C*                                                                         
     C     E800          ENDSR                                                  
     C*                                                                         
     C*****************************************************************         
     C* CHKOVF SUBROUTINE - CHECK FOR OVERFLOW AND REPRINT HEADER               
     C*****************************************************************         
     C*                                                                         
     C     CHKOVF        BEGSR                                                  
     C*                                                                         
     C     *INOF         IFEQ      '1'                                          
     C     HDGFLG        OREQ      'Y'                                          
     C                   EXCEPT    HDG1                                         
     C                   MOVE      '0'           *INOF                          
     C                   MOVE      'N'           HDGFLG                         
     C                   END                                                    
     C*                                                                         
     C     CHKO99        ENDSR                                                  
     C*                                                                         
     C*************************************************************             
     C*  INZSR: INITIALIZATION SUBROUTINE                                       
     C*************************************************************             
     C*                                                                         
     C     *INZSR        BEGSR                                                  
     C*                                                                         
     C     *ENTRY        PLIST                                                  
     C                   PARM                    WKCMP             5            
     C                   PARM                    WKPUNQ            5            
     C                   PARM                    WKCUNQ            5            
     C                   PARM                    WKDPTX            6            
     C                   PARM                    WKNBRX            8            
     C                   PARM                    WKCDEX            3            
     C                   PARM                    WKDTE1            8            
     C                   PARM                    WKDTE2            8            
     C*                                                                         
     C     EXKY01        KLIST                                                  
     C                   KFLD                    CMPUNQ                         
     C                   KFLD                    OUEXPCDE                       
     C*                                                                         
     C                   MOVE      WKCMP         DFCMPX            5 0          
     C     DFCMPX        CHAIN     R925P1                             95        
     C                   IF        *IN95 = *OFF                                 
     C                   EXSR      S800                                         
     C                   MOVEL     CMPNME        OUCMPN           40            
     C                   ELSE                                                   
     C                   MOVE      *BLANKS       OUCMPN                         
     C                   ENDIF                                                  
     C*                                                                         
     C                   MOVE      WKPUNQ        DFPUNQ            5 0          
     C                   MOVE      WKCUNQ        DFCUNQ            5 0          
     C                   MOVE      WKDPTX        DFDPTX            6 0          
     C                   MOVE      WKNBRX        DFNBRX            8 0          
     C                   MOVE      WKCDEX        DFCDEX            3 0          
     C*                                                                         
     C                   MOVE      WKDTE1        WKDTEX            8 0          
     C                   MOVE      WKDTEX        DATISO                         
     C                   MOVE      DATISO        DATMDY                         
     C                   MOVE      DATISO        BEGDTE            8 0          
     C                   MOVE      DATMDY        OUDTE1            6 0          
     C*                                                                         
     C                   MOVE      WKDTE2        WKDTEX                         
     C                   MOVE      WKDTEX        DATISO                         
     C                   MOVE      DATISO        DATMDY                         
     C                   MOVE      DATISO        ENDDTE            8 0          
     C                   MOVE      DATMDY        OUDTE2            6 0          
     C*                                                                         
     C                   MOVE      'Y'           HDGFLG            1            
     C*                                                                         
     C                   TIME                    RTIME             6 0          
     C*                                                                         
     C                   EXCEPT    COVER                                        
     C*                                                                         
     C                   ENDSR                                                  
     C*                                                                         
     C*****************************************************************         
     O*                                                                         
     OQSYSPRT   E            COVER            02                                
     O                                            5 'Date:'                     
     O                       UDATE         Y     14                             
     O                       OUCMPN              60                             
     O                                           69 'Page:'                     
     O                       PAGE1         Z     74                             
     O*                                                                         
     O          E            COVER       1  1                                   
     O                                            5 'Time:'                     
     O                       RTIME               14 '  :  :  '                  
     O                                           52 'Summary by Expense Codes'  
     O                                           69 'User:'                     
     O                       USERID              80                             
     O*                                                                         
     O          E            COVER       1                                      
     O*                                                                         
     O                                           18 'Selection Criteria'        
     O*                                                                         
     O          E            COVER       1                                      
     O                                           18 '------------------'        
     O*                                                                         
     O          E            COVER       1                                      
     O                                           22 'Company Number . . . .'    
     O                       DFCMPX              31 '   0 '                     
     O*                                                                         
     O          E            COVER       1                                      
     O                                           22 'Project Number . . . .'    
     O                       DFPUNQ              31 '   0 '                     
     O*                                                                         
     O          E            COVER       1                                      
     O                                           22 'Cost Center Number . .'    
     O                       DFCUNQ              31 '   0 '                     
     O*                                                                         
     O          E            COVER       1                                      
     O                                           22 'Employee Department. .'    
     O                       DFDPTX              31 '    0 '                    
     O*                                                                         
     O          E            COVER       1                                      
     O                                           22 'Employee Number. . . .'    
     O                       DFNBRX              31 '      0 '                  
     O*                                                                         
     O          E            COVER       1                                      
     O                                           22 'Expense Code . . . . .'    
     O                       DFCDEX              31 ' 0 '                       
     O*                                                                         
     O          E            COVER       1                                      
     O                                           22 'From Date. . . . . . .'    
     O                       OUDTE1              31 '  /  /0 '                  
     O*                                                                         
     O          E            COVER       1                                      
     O                                           22 'Thru Date. . . . . . .'    
     O                       OUDTE2              31 '  /  /0 '                  
     O*                                                                         
     OQSYSPRT   E            HDG1             02                                
     O                                            5 'Date:'                     
     O                       UDATE         Y     14                             
     O                       OUCMPN              60                             
     O                                           69 'Page:'                     
     O                       PAGE2         Z     74                             
     O*                                                                         
     O          E            HDG1        1  1                                   
     O                                            5 'Time:'                     
     O                       RTIME               14 '  :  :  '                  
     O                                           52 'Summary by Expense Codes'  
     O                                           69 'User:'                     
     O                       USERID              80                             
     O*                                                                         
     O          E            HDG1        1                                      
     O                                           19 'Expense Description'       
     O                                           42 'Amount'                    
     O*                                                                         
     O          E            HDG1        1                                      
     O                                           20 '--------------------'      
     O                                           24 '----'                      
     O                                           42 '------'                    
     O*                                                                         
     O          E            SUB1        1                                      
     O                       OUEXPCDE             3 '  0'                       
     O                       OUEXPTYP            24                             
     O                       SUBAMT              43 '  ,   , 0 .  -'            
     O*                                                                         
     O          E            TOT1        1                                      
     O                                           42 '----------'                
     O*                                                                         
     O          E            TOT1        1                                      
     O                                           28 'Grand Total:'              
     O                       TOTAMT              43 '  ,   , 0 .  -'            
     O*                                                                         
