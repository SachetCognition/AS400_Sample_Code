     H DATFMT(*ISO)                                                             
     H*****************************************************************         
     H* FastBreed Technologies, Inc. © 2000                           *         
     H*****************************************************************         
     H*****************************************************************         
     H**                                                             **         
     H** OBJECT ID: IS923R6                                          **         
     H**                                                             **         
     H** OBJECT DESCRIPTION: EXPENSE SYSTEM REPORTS -                **         
     H**                     PRINT ALL TRANSACTIONS BY DATE          **         
     H**                                                             **         
     H** MOD#   DATE  PGMR DESCRIPTION                               **         
     H** ----  ------ ---- ----------------------------------------- **         
     H**                                                             **         
     H*****************************************************************         
     F*                                                                         
     FIS925P1   IF   E           K DISK                                         
     FIS929L3   IF   E           K DISK                                         
     FQSYSPRT   O    F  132        PRINTER OFLIND(*INOF)                        
     D*                                                                         
     D ARYN            S              1    DIM(40)                              
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
     C** Print detail                                                           
     C                   EXSR      S001                                         
     C*                                                                         
     C** Print totals                                                           
     C                   EXSR      CHKOVF                                       
     C                   EXCEPT    TOT1                                         
     C*                                                                         
     C** End program                                                            
     C                   MOVE      '1'           *INLR                          
     C                   RETURN                                                 
     C*                                                                         
     C*****************************************************************         
     C*   S001: PRINT DETAIL                                          *         
     C*****************************************************************         
     C*                                                                         
     C     S001          BEGSR                                                  
     C*                                                                         
     C** Read work file                                                         
     C     *LOVAL        SETLL     R929P1                                       
     C                   READ      R929P1                                 90    
     C     *IN90         DOWEQ     *OFF                                         
     C*                                                                         
     C** Exit if beyond date range                                              
     C                   IF        TRNDTE > ENDDTE                              
     C                   LEAVE                                                  
     C                   ENDIF                                                  
     C*                                                                         
     C** Skip if not for selected company                                       
     C                   IF        DFCMPX <> 0 AND DFCMPX <> CMPUNQ             
     C                   READ      R929P1                                 90    
     C                   ITER                                                   
     C                   ENDIF                                                  
     C*                                                                         
     C** Skip if not for selected project                                       
     C                   IF        DFPUNQ <> 0 AND DFPUNQ <> PRJUNQ             
     C                   READ      R929P1                                 90    
     C                   ITER                                                   
     C                   ENDIF                                                  
     C*                                                                         
     C** Skip if not for selected cost center                                   
     C                   IF        DFCUNQ <> 0 AND DFCUNQ <> CSTUNQ             
     C                   READ      R929P1                                 90    
     C                   ITER                                                   
     C                   ENDIF                                                  
     C*                                                                         
     C** Skip if not for selected employee                                      
     C                   IF        DFNBRX <> 0 AND DFNBRX <> EMPNBR             
     C                   READ      R929P1                                 90    
     C                   ITER                                                   
     C                   ENDIF                                                  
     C*                                                                         
     C** Skip if not for selected department                                    
     C                   IF        DFDPTX <> 0 AND DFDPTX <> EMPDPT             
     C                   READ      R929P1                                 90    
     C                   ITER                                                   
     C                   ENDIF                                                  
     C*                                                                         
     C** Skip if not for selected expense code                                  
     C                   IF        DFCDEX <> 0 AND DFCDEX <> EXPCDE             
     C                   READ      R929P1                                 90    
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
     C                   READ      R929P1                                 90    
     C                   ITER                                                   
     C                   ENDIF                                                  
     C*                                                                         
     C** Convert fields to output format                                        
     C                   IF        TRNDTE <> 0                                  
     C                   MOVE      TRNDTE        DATISO                         
     C                   MOVE      DATISO        DATMDY                         
     C                   MOVE      DATMDY        OUDTE3            6 0          
     C                   ENDIF                                                  
     C*                                                                         
     C                   Z-ADD     EXPCDE        OUCDEX            3 0          
     C                   MOVEL     EXPTYP        OUTYPX           20            
     C                   Z-ADD     EMPNBR        OUNBRX            8 0          
     C                   MOVEL     EMPNME        OUNMEX           40            
     C                   Z-ADD     EMPDPT        OUDPTX            6 0          
     C                   Z-ADD     PRJUNQ        OUPUNQ            5 0          
     C                   Z-ADD     CSTUNQ        OUCUNQ            5 0          
     C*                                                                         
     C                   Z-ADD     TRNAMT        OUAMTX            8 2          
     C                   ADD       TRNAMT        TOTAMT            9 2          
     C*                                                                         
     C** Print detail                                                           
     C                   EXSR      CHKOVF                                       
     C                   EXCEPT    DET1                                         
     C*                                                                         
     C                   READ      R929P1                                 90    
     C                   ENDDO                                                  
     C*                                                                         
     C     E001          ENDSR                                                  
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
     C     CENTER        SUB       LSTPOS        I                 3 0          
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
     C     EMKY01        KLIST                                                  
     C                   KFLD                    CMPUNQ                         
     C                   KFLD                    EMPUNQ                         
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
     OQSYSPRT   E            COVER            01                                
     O                                            5 'Date:'                     
     O                       UDATE         Y     14                             
     O                       OUCMPN              86                             
     O                                          121 'Page:'                     
     O                       PAGE1         Z    126                             
     O*                                                                         
     O          E            COVER       1  1                                   
     O                                            5 'Time:'                     
     O                       RTIME               14 '  :  :  '                  
     O                                           78 'All Transactions by Date'  
     O                                          121 'User:'                     
     O                       USERID             132                             
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
     OQSYSPRT   E            HDG1             01                                
     O                                            5 'Date:'                     
     O                       UDATE         Y     14                             
     O                       OUCMPN              86                             
     O                                          121 'Page:'                     
     O                       PAGE2         Z    126                             
     O*                                                                         
     O          E            HDG1        1  1                                   
     O                                            5 'Time:'                     
     O                       RTIME               14 '  :  :  '                  
     O                                           78 'All Transactions by Date'  
     O                                          121 'User:'                     
     O                       USERID             132                             
     O*                                                                         
     O          E            HDG1        1                                      
     O                                            6 'Date'                      
     O                                           14 'Code'                      
     O                                           31 'Exp Description'           
     O                                           46 'Emp Nbr'                   
     O                                           61 'Employee Name'             
     O                                          100 'Amount'                    
     O                                          110 'Project'                   
     O                                          123 'Cost Center'               
     O                                          132 'Dept'                      
     O*                                                                         
     O          E            HDG1        1                                      
     O                                            8 '--------'                  
     O                                           14 '----'                      
     O                                           26 '----------'                
     O                                           36 '----------'                
     O                                           46 '-------'                   
     O                                           88 '--------------------'      
     O                                           68 '--------------------'      
     O                                          100 '------'                    
     O                                          110 '-------'                   
     O                                          123 '-----------'               
     O                                          132 '----'                      
     O*                                                                         
     O          E            DET1        1                                      
     O                       OUDTE3               8 '  /  /0 '                  
     O                       OUCDEX              14 '  0'                       
     O                       OUTYPX              36                             
     O                       OUNBRX              46 '       0'                  
     O                       OUNMEX              88                             
     O                       OUAMTX             101 '   , 0 .  -'               
     O                       OUPUNQ             109 '    0'                     
     O                       OUCUNQ             120 '    0'                     
     O                       OUDPTX             132 '     0'                    
     O*                                                                         
     O          E            TOT1        1                                      
     O                                          100 '----------'                
     O*                                                                         
     O          E            TOT1        1                                      
     O                       TOTAMT             101 ' ,   , 0 .  -'             
     O*                                                                         
