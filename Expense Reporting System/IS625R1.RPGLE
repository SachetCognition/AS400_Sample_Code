     H*****************************************************************         
     H* FastBreed Technologies, Inc. © 2000                           *         
     H*****************************************************************         
     H*****************************************************************         
     H**                                                             **         
     H** OBJECT ID: IS625R1                                          **         
     H**                                                             **         
     H** OBJECT DESCRIPTION: MAINTAIN COMPANY MASTER                 **         
     H**                                                             **         
     H** MOD#    DATE      DESCRIPTION                               **         
     H** ----  --------    ----------------------------------------- **         
     H**                                                             **         
     H*****************************************************************         
     F*                                                                         
     FIS921P1   IF   E           K DISK                                         
     FIS922P1   IF   E           K DISK                                         
     FIS625L1   IF   E           K DISK                                         
     FIS625P1   UF   E           K DISK    RENAME(R625P1:UF1)                   
     FIS626P1   UF   E           K DISK                                         
     FIS626P2   UF   E           K DISK                                         
     FIS626P3   UF   E           K DISK                                         
     FIS627P1   UF   E           K DISK                                         
     FIS627P2   UF   E           K DISK                                         
     FIS625D1   CF   E             WORKSTN INFDS(INFDS)                         
     F                                     SFILE(WDW006S:CRRN)                  
     D*                                                                         
     D COUNT           S              2  0                                      
     D CRRN            S              5  0                                      
     D CRRNSV          S              5  0                                      
     D FLDNME          S              8                                         
     D MNUNME          S             10                                         
     D MNUOP#          S              2  0                                      
     D MNUOPT          S              2                                         
     D MSGD            S             50                                         
     D MSGF            S             10                                         
     D MSGID           S              7                                         
     D SVCOL           S              3  0                                      
     D SVROW           S              3  0                                      
     D DFNMEX          S             20                                         
     D SVNMEX          S             20                                         
     D*                                                                         
     D INFDS           DS                                                       
     D KEY                   369    369                                         
     D PAGRRN                378    379B 0                                      
     D*                                                                         
     D #DMSG           C                   CONST('* Record deleted')            
     D*                                                                         
     D F01             C                   CONST(X'31')                         
     D F03             C                   CONST(X'33')                         
     D F06             C                   CONST(X'36')                         
     D F12             C                   CONST(X'3C')                         
     D ENTER           C                   CONST(X'F1')                         
     D PAGEDN          C                   CONST(X'F5')                         
     D*                                                                         
     D PSDS           SDS                                                       
     D VDUSID                254    263                                         
     D*                                                                         
     D** Hi Intensity - Reverse Image                                           
     D HIREV1          C                   CONST(X'23')                         
     I*                                                                         
     C*****************************************************************         
     C*  MAINLINE                                                     *         
     C*****************************************************************         
     C*                                                                         
     C     *LOVAL        SETLL     R625P1                                       
     C*                                                                         
HEREXC** Mainline logic.                                                        
     C                   DOU       KEY = F03 OR KEY = F12                       
     C*                                                                         
     C** Fill subfile.                                                          
     C                   EXSR      S001                                         
     C*                                                                         
     C** Display subfile.                                                       
     C                   DOU       KEY = F03 OR KEY = F12                       
     C*                                                                         
     C     MAIN01        TAG                                                    
     C                   WRITE     CMD                                          
     C*                                                                         
     C** Check for existing subfile records to condition SFLDSP.                
     C                   IF        CRRN > 0                                     
     C                   MOVE      *ON           *IN42                          
     C                   ELSE                                                   
     C                   MOVE      *OFF          *IN42                          
     C                   ENDIF                                                  
     C*                                                                         
     C                   MOVE      *ON           *IN41                          
     C                   EXFMT     WDW006C                                      
     C                   MOVE      *OFF          *IN41                          
     C                   MOVE      *OFF          *IN45                          
0001 C*                                                                         
 !   C** Init SFLRCDNBR value with PAGRRN value from I/O feedback area - the    
 !   C** lowest subfile RRN on the current subfile display.  Ensures that       
 !   C** proper page is redisplayed.                                            
0001 C                   Z-ADD     PAGRRN        RCDNBR                         
     C*                                                                         
     C** Remove any error messages                                              
     C                   CALL      'RMVMSGX'                                    
     C                   WRITE     SFLCTLM                                      
     C*                                                                         
     C** Reload defaults                                                        
     C                   MOVE      *ON           *IN28                          
     C*                                                                         
     C** F3 Pressed = Exit                                                      
     C                   IF        KEY = F03 OR KEY = F12                       
     C                   LEAVE                                                  
     C                   ENDIF                                                  
     C*                                                                         
     C** F1 Pressed = Help                                                      
     C                   IF        *IN01 = *ON                                  
     C                   MOVEL     '01'          MNUOPT                         
     C                   EXSR      S050                                         
     C                   GOTO      MAIN01                                       
     C                   ENDIF                                                  
     C*                                                                         
     C** Add request.                                                           
     C                   IF        KEY = F06                                    
     C                   EXSR      S003                                         
     C                   EXSR      S002                                         
     C                   LEAVE                                                  
     C                   ENDIF                                                  
     C*                                                                         
     C** Re-Position Subfile                                                    
     C                   IF        DFNMEX <> SVNMEX                             
     C                   MOVEL     DFNMEX        SVNMEX                         
     C                   EXSR      S002                                         
     C                   ENDIF                                                  
     C*                                                                         
     C** Page down for more records request.                                    
     C     KEY           IFEQ      PAGEDN                                       
     C                   LEAVE                                                  
     C                   ENDIF                                                  
     C*                                                                         
     C** Enter with no subfile records displayed.                               
     C                   IF        KEY = ENTER                                  
     C                             AND CRRN  = 0                                
     C                   EXSR      S002                                         
     C                   LEAVE                                                  
     C                   ENDIF                                                  
     C*                                                                         
     C                   MOVE      *OFF          *IN98                          
     C*                                                                         
     C                   READC     WDW006S                                98    
     C                   DOW       *IN98 = *OFF                                 
     C*                                                                         
     C** Change Company Master                                                  
     C                   IF        SELIO = 1                                    
     C                   EXSR      S004                                         
     C                   ENDIF                                                  
     C*                                                                         
     C** Change Company Number                                                  
     C                   IF        SELIO = 2                                    
     C                   EXSR      S005                                         
     C                   ENDIF                                                  
     C*                                                                         
     C** Delete Company Master                                                  
     C                   IF        SELIO = 4                                    
     C                   EXSR      S006                                         
     C                   ENDIF                                                  
     C*                                                                         
     C                   READC     WDW006S                                98    
     C                   ENDDO                                                  
     C*                                                                         
     C                   Z-ADD     SVCRRN        CRRN                           
     C                   GOTO      MAIN01                                       
     C*                                                                         
     C                   ENDDO                                                  
     C                   ENDDO                                                  
     C*                                                                         
     C                   MOVE      *ON           *INLR                          
     C                   RETURN                                                 
     C*                                                                         
     C****************************************************************          
     C*   S001: LOAD SUBFILE RECORDS                                 *          
     C****************************************************************          
     C*                                                                         
     C     S001          BEGSR                                                  
0001 C*                                                                         
 !   C** Reset last subfile relative record number                              
 !   C                   Z-ADD     CRRNSV        CRRN                           
 !   C*                                                                         
 !   C** To ensure new subfile page is displayed RCDNBR=SFLRCDNBR and           
 !   C** cursor is placed in first record on subfile.                           
0001 C     CRRN          ADD       1             RCDNBR                         
     C*                                                                         
     C                   EVAL      COUNT = 0                                    
     C                   EVAL      CRRNSV = 0                                   
     C*                                                                         
     C                   DOU       COUNT = 14                                   
     C*                                                                         
     C                   READ      R625P1                                 99    
     C*                                                                         
     C                   IF        *IN99 = *ON                                  
0001 C     RCDNBR        IFGT      CRRN                                         
 !   C                   Z-ADD     CRRN          RCDNBR                         
0001 C                   ENDIF                                                  
0001 C                   GOTO      T002                                         
     C                   ENDIF                                                  
     C*                                                                         
     C                   CLEAR                   SELIO                          
     C                   ADD       1             CRRN                           
     C                   ADD       1             COUNT                          
     C                   MOVE      CMPUNQ        VDUNQ1                         
     C                   MOVEL     CMPNME        VDNME1                         
     C*                                                                         
     C                   IF        CRRNSV = 0                                   
     C                   Z-ADD     CRRN          CRRNSV            5 0          
     C                   Z-ADD     CRRN          RCDNBR                         
     C                   ENDIF                                                  
     C*                                                                         
     C                   WRITE     WDW006S                                      
     C                   ENDDO                                                  
     C*                                                                         
     C     T002          TAG                                                    
     C*                                                                         
     C                   Z-ADD     CRRN          SVCRRN            5 0          
0001 C*                                                                         
 !   C** Save last subfile record number.                                       
0001 C                   Z-ADD     CRRN          CRRNSV                         
     C*                                                                         
     C     E001          ENDSR                                                  
     C*                                                                         
     C****************************************************************          
     C*   S002: REPOSITION SUBFILE                                   *          
     C****************************************************************          
     C*                                                                         
     C     S002          BEGSR                                                  
     C*                                                                         
     C                   MOVE      *ON           *IN40                          
     C                   WRITE     WDW006C                                      
     C                   MOVE      *OFF          *IN40                          
     C*                                                                         
     C                   RESET                   CRRN                           
     C                   RESET                   CRRNSV                         
     C*                                                                         
     C                   MOVEL     DFNMEX        WKKEY1           40            
     C     WKKEY1        SETLL     R625P1                                       
     C*                                                                         
     C     E002          ENDSR                                                  
     C*                                                                         
     C****************************************************************          
     C*   S003: ADD COMPANY MASTER                                   *          
     C****************************************************************          
     C*                                                                         
     C     S003          BEGSR                                                  
     C*                                                                         
     C                   IF        INQ25 <> '0'                                 
     C                   MOVE      'MSG0001'     MSGID                          
     C                   EXSR      SNDMSGX                                      
     C                   WRITE     SFLCTLM                                      
     C                   GOTO      E003                                         
     C                   ENDIF                                                  
     C*                                                                         
     C                   MOVE      '00000'       OUUNQX            5            
     C                   MOVE      *BLANKS       OUNMEX           40            
     C*                                                                         
     C                   CALL      'IS625R2'                                    
     C                   PARM                    OUUNQX                         
     C                   PARM                    OUNMEX                         
     C                   PARM                    WKF03                          
     C*                                                                         
     C                   IF        WKF03 = 'Y'                                  
     C                   MOVE      *ON           *INLR                          
     C                   RETURN                                                 
     C                   ENDIF                                                  
     C*                                                                         
     C                   IF        OUUNQX <> '00000'                            
     C                   MOVEL     OUNMEX        DFNMEX                         
     C                   MOVEL     OUNMEX        SVNMEX                         
     C                   ENDIF                                                  
     C*                                                                         
     C     E003          ENDSR                                                  
     C*                                                                         
     C****************************************************************          
     C*   S004: CHANGE COMPANY MASTER                                *          
     C****************************************************************          
     C*                                                                         
     C     S004          BEGSR                                                  
     C*                                                                         
     C                   MOVE      VDUNQ1        OUUNQX            5            
     C                   MOVEL     VDNME1        OUNMEX           40            
     C*                                                                         
     C                   CALL      'IS625R3'                                    
     C                   PARM                    INQ25                          
     C                   PARM                    OUUNQX                         
     C                   PARM                    OUNMEX                         
     C                   PARM                    WKF03                          
     C*                                                                         
     C                   IF        WKF03 = 'Y'                                  
     C                   MOVE      *ON           *INLR                          
     C                   RETURN                                                 
     C                   ENDIF                                                  
     C*                                                                         
     C** Update subfile.                                                        
     C     CRRN          CHAIN     WDW006S                            22        
     C                   CLEAR                   SELIO                          
     C                   MOVEL     OUNMEX        VDNME1                         
     C                   UPDATE    WDW006S                                      
     C*                                                                         
     C     E004          ENDSR                                                  
     C*                                                                         
     C****************************************************************          
     C*   S005: CHANGE COMPANY NUMBER                                *          
     C****************************************************************          
     C*                                                                         
     C     S005          BEGSR                                                  
     C*                                                                         
     C** Validate authority.                                                    
     C                   IF        INQ25 <> '0'                                 
     C                   MOVE      'MSG0006'     MSGID                          
     C                   EXSR      SNDMSGX                                      
     C                   WRITE     SFLCTLM                                      
     C                   GOTO      E005                                         
     C                   ENDIF                                                  
     C*                                                                         
     C                   Z-ADD     6             VDROW                          
     C                   Z-ADD     14            VDCOL                          
     C                   MOVE      *BLANKS       XXED01                         
     C                   MOVE      VDUNQ1        DFNBR1                         
     C                   MOVE      *ZEROS        DFNBR2                         
     C*                                                                         
     C** Mainline logic.                                                        
     C                   DOU       KEY = F03 OR KEY = F12                       
     C*                                                                         
     C                   EXFMT     DSP01                                        
     C*                                                                         
     C** Remove any error messages                                              
     C                   CALL      'RMVMSGX'                                    
     C                   WRITE     SFLCTLM                                      
     C                   MOVE      *ON           *IN28                          
     C                   MOVE      *BLANKS       XXED01                         
     C                   MOVE      *BLANKS       OUCNME                         
     C                   Z-ADD     6             VDROW                          
     C                   Z-ADD     14            VDCOL                          
     C*                                                                         
     C** F3 Pressed = Exit                                                      
     C                   IF        KEY = F03                                    
     C                   EVAL      VDROW = SVROW                                
     C                   EVAL      VDCOL = SVCOL                                
     C                   MOVE      *ON           *INLR                          
     C                   RETURN                                                 
     C                   ENDIF                                                  
     C*                                                                         
     C** F12 Pressed = Return                                                   
     C                   IF        KEY = F12                                    
     C                   EVAL      VDROW = SVROW                                
     C                   EVAL      VDCOL = SVCOL                                
     C                   LEAVE                                                  
     C                   ENDIF                                                  
     C*                                                                         
     C** F1 Pressed = Help                                                      
     C                   IF        *IN01 = *ON                                  
     C                   MOVEL     '02'          MNUOPT                         
     C                   EXSR      S050                                         
     C                   ITER                                                   
     C                   ENDIF                                                  
     C*                                                                         
     C** Validate new number                                                    
     C                   EXSR      S010                                         
     C                   IF        PASSEDIT = 'N'                               
     C                   ITER                                                   
     C                   ENDIF                                                  
     C*                                                                         
     C** Change company number                                                  
     C                   EXSR      S011                                         
     C                   LEAVE                                                  
     C*                                                                         
     C                   ENDDO                                                  
     C*                                                                         
     C** Update subfile.                                                        
     C     CRRN          CHAIN     WDW006S                            22        
     C                   CLEAR                   SELIO                          
     C                   Z-ADD     DFNBR2        VDUNQ1                         
     C                   UPDATE    WDW006S                                      
     C*                                                                         
     C                   Z-ADD     4             VDROW                          
     C                   Z-ADD     23            VDCOL                          
     C*                                                                         
     C     E005          ENDSR                                                  
     C*                                                                         
     C****************************************************************          
     C*   S006: DELETE COMPANY MASTER                                *          
     C****************************************************************          
     C*                                                                         
     C     S006          BEGSR                                                  
     C*                                                                         
     C** Validate authority.                                                    
     C                   IF        INQ25 <> '0'                                 
     C                   MOVE      'MSG0006'     MSGID                          
     C                   EXSR      SNDMSGX                                      
     C                   WRITE     SFLCTLM                                      
     C                   GOTO      E006                                         
     C                   ENDIF                                                  
     C*                                                                         
     C** Delete header record.                                                  
     C     VDUNQ1        DELETE    UF1                                22        
     C*                                                                         
     C** Update subfile.                                                        
     C     CRRN          CHAIN     WDW006S                            22        
     C                   CLEAR                   SELIO                          
     C                   EVAL      VDUNQ1 = *ZEROS                              
     C                   EVAL      VDNME1 = *BLANKS                             
     C                   MOVEL     #DMSG         VDNME1                         
     C                   UPDATE    WDW006S                                      
     C*                                                                         
     C     E006          ENDSR                                                  
     C*                                                                         
     C****************************************************************          
     C*   S010: VALIDATE NEW COMPANY NUMBER                          *          
     C****************************************************************          
     C*                                                                         
     C     S010          BEGSR                                                  
     C*                                                                         
     C                   MOVE      'Y'           PASSEDIT          1            
     C*                                                                         
     C** New company number can't be zero                                       
     C                   IF        DFNBR2 <= 0                                  
     C                   Z-ADD     6             VDROW                          
     C                   Z-ADD     14            VDCOL                          
     C                   MOVE      HIREV1        XXED01                         
     C                   MOVE      'N'           PASSEDIT                       
     C                   MOVE      'MSG0007'     MSGID                          
     C                   EXSR      SNDMSGX                                      
     C                   WRITE     SFLCTLM                                      
     C                   GOTO      E010                                         
     C                   ENDIF                                                  
     C*                                                                         
     C** New company number can't already be being used                         
     C     DFNBR2        CHAIN(N)  UF1                                95        
     C                   IF        *IN95 = *OFF                                 
     C                   MOVEL     CMPNME        OUCNME                         
     C                   Z-ADD     6             VDROW                          
     C                   Z-ADD     14            VDCOL                          
     C                   MOVE      HIREV1        XXED01                         
     C                   MOVE      'N'           PASSEDIT                       
     C                   MOVE      'MSG0036'     MSGID                          
     C                   EXSR      SNDMSGX                                      
     C                   WRITE     SFLCTLM                                      
     C                   GOTO      E010                                         
     C                   ENDIF                                                  
     C*                                                                         
     C     E010          ENDSR                                                  
     C*                                                                         
     C****************************************************************          
     C*   S011: CHANGE COMPANY NUMBER                                *          
     C****************************************************************          
     C*                                                                         
     C     S011          BEGSR                                                  
     C*                                                                         
     C** Save current time                                                      
     C                   TIME                    SVTIME            6 0          
     C                   IF        SVTIME < 120000                              
     C                   MOVE      'A'           SVAMPM            1            
     C                   ELSE                                                   
     C                   MOVE      'P'           SVAMPM                         
     C                   ENDIF                                                  
     C*                                                                         
     C** Change company master                                                  
     C     DFNBR1        CHAIN     UF1                                95        
     C                   IF        *IN95 = *OFF                                 
     C                   Z-ADD     DFNBR2        CMPUNQ                         
     C                   MOVEL     VDUSID        CMPUBY                         
     C                   MOVE      TODAY         CMPUDT                         
     C                   MOVE      SVTIME        CMPUTM                         
     C                   MOVE      SVAMPM        CMPUAM                         
     C                   UPDATE    UF1                                          
     C                   ENDIF                                                  
     C*                                                                         
     C** Update project files                                                   
     C                   MOVE      *OFF          *IN91                          
     C     ISKY01        SETLL     R626P1                                       
     C     DFNBR1        READE     R626P1                                 91    
     C     *IN91         DOWEQ     *OFF                                         
     C                   Z-ADD     DFNBR2        CMPUNQ                         
     C                   MOVEL     VDUSID        PRJUBY                         
     C                   MOVE      TODAY         PRJUDT                         
     C                   MOVE      SVTIME        PRJUTM                         
     C                   MOVE      SVAMPM        PRJUAM                         
     C                   UPDATE    R626P1                                       
     C     DFNBR1        READE     R626P1                                 91    
     C                   ENDDO                                                  
     C*                                                                         
     C     DFNBR1        CHAIN     R626P2                             95        
     C                   IF        *IN95 = *OFF                                 
     C                   Z-ADD     DFNBR2        CMPUNQ                         
     C                   UPDATE    R626P2                                       
     C                   ENDIF                                                  
     C*                                                                         
     C                   MOVE      *OFF          *IN91                          
     C     ISKY02        SETLL     R626P3                                       
     C     DFNBR1        READE     R626P3                                 91    
     C     *IN91         DOWEQ     *OFF                                         
     C                   Z-ADD     DFNBR2        CMPUNQ                         
     C                   MOVEL     VDUSID        PROUBY                         
     C                   MOVE      TODAY         PROUDT                         
     C                   MOVE      SVTIME        PROUTM                         
     C                   MOVE      SVAMPM        PROUAM                         
     C                   UPDATE    R626P3                                       
     C     DFNBR1        READE     R626P3                                 91    
     C                   ENDDO                                                  
     C*                                                                         
     C** Update cost centers                                                    
     C                   MOVE      *OFF          *IN91                          
     C     ISKY01        SETLL     R627P1                                       
     C     DFNBR1        READE     R627P1                                 91    
     C     *IN91         DOWEQ     *OFF                                         
     C                   Z-ADD     DFNBR2        CMPUNQ                         
     C                   MOVEL     VDUSID        CSTUBY                         
     C                   MOVE      TODAY         CSTUDT                         
     C                   MOVE      SVTIME        CSTUTM                         
     C                   MOVE      SVAMPM        CSTUAM                         
     C                   UPDATE    R627P1                                       
     C     DFNBR1        READE     R627P1                                 91    
     C                   ENDDO                                                  
     C*                                                                         
     C     DFNBR1        CHAIN     R627P2                             95        
     C                   IF        *IN95 = *OFF                                 
     C                   Z-ADD     DFNBR2        CMPUNQ                         
     C                   UPDATE    R627P2                                       
     C                   ENDIF                                                  
     C*                                                                         
     C     E011          ENDSR                                                  
     C*                                                                         
     C*****************************************************************         
     C*  S050: DISPLAY HELP TEXT                                                
     C*****************************************************************         
     C*                                                                         
     C     S050          BEGSR                                                  
     C*                                                                         
     C                   EVAL      SVROW = ROW                                  
     C                   EVAL      SVCOL = COL                                  
     C*                                                                         
     C                   MOVEL     'IS625D1'     MNUNME                         
     C                   EVAL      FLDNME = *BLANKS                             
     C                   MOVEL     FLD           FLDNME                         
     C                   MOVE      MNUOPT        MNUOP#                         
     C*                                                                         
     C** Check user access to help text                                         
     C                   EVAL      TBNAME = *BLANKS                             
     C                   EVAL      TBKEY = *BLANKS                              
     C                   EVAL      TBVALU = *BLANKS                             
     C                   MOVEL     'SECHELP'     TBNAME                         
     C                   MOVEL     'SECHELP'     TBKEY                          
     C                   MOVEL     VDUSID        TBVALU                         
     C     HPKY01        CHAIN     R922P                              89        
     C                   IF        *IN89 = *OFF                                 
     C                   CALL      'IS921R9'     PLIST1                         
     C                   ELSE                                                   
     C                   CALL      'IS921R5'     PLIST1                         
     C     HPKY02        CHAIN     R921P1                             89        
     C                   IF        *IN89 = *ON                                  
     C                   MOVE      'MSG0002'     MSGID                          
     C                   EXSR      SNDMSGX                                      
     C                   WRITE     SFLCTLM                                      
     C                   ENDIF                                                  
     C                   ENDIF                                                  
     C*                                                                         
     C                   EVAL      VDROW = SVROW                                
     C                   EVAL      VDCOL = SVCOL                                
     C*                                                                         
     C     E050          ENDSR                                                  
     C*                                                                         
     C*****************************************************************         
     C*  SNDMSGX: WRITE ERROR MESSAGE SUBROUTINE                                
     C*****************************************************************         
     C*                                                                         
     C     SNDMSGX       BEGSR                                                  
     C*                                                                         
     C                   MOVE      *OFF          *IN28                          
     C*                                                                         
     C                   CALL      'SNDMSGX'                                    
     C                   PARM                    MSGID                          
     C                   PARM      'IS920M'      MSGF                           
     C                   PARM      *BLANKS       MSGD                           
     C*                                                                         
     C                   ENDSR                                                  
     C*                                                                         
     C****************************************************************          
     C*   INZSR: INITIALIZATION SUBROUTINE                           *          
     C****************************************************************          
     C*                                                                         
     C     *INZSR        BEGSR                                                  
     C*                                                                         
     C     *ENTRY        PLIST                                                  
     C                   PARM                    INQ25             1            
     C*                                                                         
     C     PLIST1        PLIST                                                  
     C                   PARM                    MNUNME                         
     C                   PARM                    MNUOPT                         
     C                   PARM                    FLDNME                         
     C*                                                                         
     C     HPKY01        KLIST                                                  
     C                   KFLD                    TBNAME                         
     C                   KFLD                    TBKEY                          
     C                   KFLD                    TBVALU                         
     C*                                                                         
     C     HPKY02        KLIST                                                  
     C                   KFLD                    MNUNME                         
     C                   KFLD                    MNUOP#                         
     C                   KFLD                    FLDNME                         
     C*                                                                         
     C     ISKY01        KLIST                                                  
     C                   KFLD                    DFNBR1                         
     C                   KFLD                    LWFL40                         
     C*                                                                         
     C     ISKY02        KLIST                                                  
     C                   KFLD                    DFNBR1                         
     C                   KFLD                    LWUNQX                         
     C*                                                                         
     C** Error message preparation                                              
     C                   MOVEL     '*'           MSGQ                           
     C*                                                                         
     C** Remove any error messages                                              
     C                   CALL      'RMVMSGX'                                    
     C                   WRITE     SFLCTLM                                      
     C*                                                                         
     C** Send copyright message                                                 
     C                   MOVE      'MSG0050'     MSGID                          
     C                   EXSR      SNDMSGX                                      
     C                   WRITE     SFLCTLM                                      
     C*                                                                         
     C                   MOVE      *BLANKS       DFNMEX                         
     C                   MOVE      'N'           WKF03             1            
     C                   MOVE      *ZEROS        LWUNQX            5 0          
     C                   MOVE      *BLANKS       LWFL40           40            
     C     *DATE         MULT      10000.0001    TODAY             8 0          
     C*                                                                         
     C                   EVAL      CRRN = 0                                     
0001 C                   Z-ADD     0             CRRNSV            5 0          
     C*                                                                         
     C                   ENDSR                                                  
     C*                                                                         
     C****************************************************************          
     C****************************************************************          
