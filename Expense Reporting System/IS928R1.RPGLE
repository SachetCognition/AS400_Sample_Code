     H*****************************************************************         
     H* FastBreed Technologies, Inc. © 2000                           *         
     H*****************************************************************         
     H*****************************************************************         
     H**                                                             **         
     H** OBJECT ID: IS928R1                                          **         
     H**                                                             **         
     H** OBJECT DESCRIPTION: MAINTAIN EMPLOYEE MASTER                **         
     H**                                                             **         
     H** MOD#    DATE      DESCRIPTION                               **         
     H** ----  --------    ----------------------------------------- **         
     H**                                                             **         
     H*****************************************************************         
     F*                                                                         
     FIS925P1   IF   E           K DISK                                         
     FIS928L1   IF   E           K DISK                                         
     FIS928P1   UF   E           K DISK    RENAME(R928P1:UF1)                   
     FIS922P1   IF   E           K DISK                                         
     FIS921P1   IF   E           K DISK                                         
     FIS928D1   CF   E             WORKSTN INFDS(INFDS)                         
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
     D DFCMPX          S              5  0                                      
     D SVCMPX          S              5  0                                      
     D DFNMEX          S             40                                         
     D SVNMEX          S             40                                         
     D DFACTX          S              1                                         
     D SVACTX          S              1                                         
     D*                                                                         
     D INFDS           DS                                                       
     D KEY                   369    369                                         
     D PAGRRN                378    379B 0                                      
     D*                                                                         
     D #DMSG           C                   CONST('* Record deleted')            
     D*                                                                         
     D F01             C                   CONST(X'31')                         
     D F03             C                   CONST(X'33')                         
     D F05             C                   CONST(X'35')                         
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
     C     EMKY01        SETLL     R928P1                                       
     C*                                                                         
     C** Mainline logic.                                                        
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
     C*                                                                         
     C** Remove any error messages                                              
     C                   CALL      'RMVMSGX'                                    
     C                   WRITE     SFLCTLM                                      
     C*                                                                         
     C                   MOVE      'N'           REORG             1            
     C                   MOVE      *ON           *IN28                          
     C                   MOVE      *BLANKS       XXED01                         
     C*                                                                         
     C** F3 Pressed = Exit                                                      
     C                   IF        KEY = F03 OR KEY = F12                       
     C                   LEAVE                                                  
     C                   ENDIF                                                  
     C*                                                                         
     C** F1 Pressed = Help                                                      
     C                   IF        *IN01 = *ON                                  
     C                   EXSR      S050                                         
     C                   GOTO      MAIN01                                       
     C                   ENDIF                                                  
     C*                                                                         
     C** Validate company number.                                               
     C     DFCMPX        CHAIN     R925P1                             95        
     C                   IF        *IN95 = *ON                                  
     C                   Z-ADD     4             VDROW                          
     C                   Z-ADD     19            VDCOL                          
     C                   MOVE      HIREV1        XXED01                         
     C                   MOVE      'MSG0007'     MSGID                          
     C                   EXSR      SNDMSGX                                      
     C                   WRITE     SFLCTLM                                      
     C                   GOTO      MAIN01                                       
     C                   ENDIF                                                  
     C*                                                                         
     C** Load/refresh file request                                              
     C                   IF        KEY = F05                                    
     C                   MOVE      DFCMPX        WKCMPX            5            
     C                   CALL      'IS928R4'                                    
     C                   PARM                    WKCMPX                         
     C                   EXSR      S002                                         
     C                   LEAVE                                                  
     C                   ENDIF                                                  
     C*                                                                         
     C** Add request                                                            
     C                   IF        KEY = F06                                    
     C                   EXSR      S003                                         
     C                   EXSR      S002                                         
     C                   LEAVE                                                  
     C                   ENDIF                                                  
     C*                                                                         
     C** Re-Position Subfile                                                    
     C                   IF        DFCMPX <> SVCMPX OR DFNMEX <> SVNMEX OR      
     C                             DFACTX <> SVACTX                             
     C                   MOVE      DFCMPX        SVCMPX                         
     C                   MOVE      DFNMEX        SVNMEX                         
     C                   MOVE      DFACTX        SVACTX                         
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
     C** Change Employee Master                                                 
     C                   IF        SELIO = 1                                    
     C                   EXSR      S004                                         
     C                   ENDIF                                                  
     C*                                                                         
     C** Delete Employee Master                                                 
     C                   IF        SELIO = 4                                    
     C                   EXSR      S005                                         
     C                   ENDIF                                                  
     C*                                                                         
     C                   READC     WDW006S                                98    
     C                   ENDDO                                                  
     C*                                                                         
     C** Reorganize display if number(s) changed                                
     C                   IF        REORG = 'Y'                                  
     C                   EXSR      S002                                         
     C                   EXSR      S001                                         
     C                   ENDIF                                                  
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
     C*                                                                         
     C                   EVAL      COUNT = 0                                    
     C                   EVAL      CRRNSV = 0                                   
     C*                                                                         
     C                   DOU       COUNT = 14                                   
     C*                                                                         
     C     T001          TAG                                                    
     C*                                                                         
     C     DFCMPX        READE     R928P1                                 99    
     C*                                                                         
     C                   IF        *IN99 = *ON                                  
     C                   LEAVE                                                  
     C                   ENDIF                                                  
     C*                                                                         
     C                   IF        DFACTX = 'Y' AND EMPDSP = 'N'                
     C                   GOTO      T001                                         
     C                   ENDIF                                                  
     C*                                                                         
     C                   CLEAR                   SELIO                          
     C                   ADD       1             CRRN                           
     C                   ADD       1             COUNT                          
     C                   MOVE      EMPUNQ        VDUNQ1                         
     C                   MOVE      EMPNBR        VDNBR1                         
     C                   MOVEL     EMPNME        VDNME1                         
     C                   MOVE      EMPDPT        VDDPT1                         
     C*                                                                         
     C                   IF        CRRNSV = 0                                   
     C                   Z-ADD     CRRN          CRRNSV            5 0          
     C                   Z-ADD     CRRN          RCDNBR                         
     C                   ENDIF                                                  
     C*                                                                         
     C                   WRITE     WDW006S                                      
     C                   ENDDO                                                  
     C*                                                                         
     C                   Z-ADD     CRRN          SVCRRN            5 0          
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
     C     EMKY03        SETLL     R928P1                                       
     C*                                                                         
     C     E002          ENDSR                                                  
     C*                                                                         
     C****************************************************************          
     C*   S003: ADD EMPLOYEE MASTER                                  *          
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
     C                   MOVE      DFCMPX        OUCMPX            5            
     C                   MOVE      *BLANKS       OUNMEX           40            
     C*                                                                         
     C                   CALL      'IS928R2'                                    
     C                   PARM                    OUCMPX                         
     C                   PARM                    OUNBRX                         
     C                   PARM                    WKF03                          
     C*                                                                         
     C                   IF        WKF03 = 'Y'                                  
     C                   MOVE      *ON           *INLR                          
     C                   RETURN                                                 
     C                   ENDIF                                                  
     C*                                                                         
     C                   IF        OUNMEX <> *BLANKS                            
     C                   MOVEL     OUCMPX        DFCMPX                         
     C                   MOVEL     OUCMPX        SVCMPX                         
     C                   MOVEL     OUNMEX        DFNMEX                         
     C                   MOVEL     OUNMEX        SVNMEX                         
     C                   ENDIF                                                  
     C*                                                                         
     C     E003          ENDSR                                                  
     C*                                                                         
     C****************************************************************          
     C*   S004: CHANGE EMPLOYEE MASTER                               *          
     C****************************************************************          
     C*                                                                         
     C     S004          BEGSR                                                  
     C*                                                                         
     C                   MOVE      DFCMPX        OUCMPX            5            
     C                   MOVE      VDUNQ1        OUUNQX            5            
     C                   MOVE      VDNBR1        OUNBRX            8            
     C                   MOVE      VDNBR1        HDNBRX            8            
     C                   MOVE      VDNME1        OUNMEX           40            
     C                   MOVE      VDDPT1        OUDPTX            6            
     C*                                                                         
     C                   CALL      'IS928R3'                                    
     C                   PARM                    INQ25                          
     C                   PARM                    OUCMPX                         
     C                   PARM                    OUUNQX                         
     C                   PARM                    OUNBRX                         
     C                   PARM                    OUNMEX                         
     C                   PARM                    OUDPTX                         
     C                   PARM                    WKF03                          
     C*                                                                         
     C                   IF        WKF03 = 'Y'                                  
     C                   MOVE      *ON           *INLR                          
     C                   RETURN                                                 
     C                   ENDIF                                                  
     C*                                                                         
     C** Reorganize display if number(s) changed                                
     C                   IF        HDNBRX <> OUNBRX                             
     C                   MOVE      'Y'           REORG                          
     C                   ENDIF                                                  
     C*                                                                         
     C** Update subfile.                                                        
     C     CRRN          CHAIN     WDW006S                            22        
     C                   CLEAR                   SELIO                          
     C                   MOVE      OUNBRX        VDNBR1                         
     C                   MOVEL     OUNMEX        VDNME1                         
     C                   MOVE      OUDPTX        VDDPT1                         
     C                   UPDATE    WDW006S                                      
     C*                                                                         
     C     E004          ENDSR                                                  
     C*                                                                         
     C****************************************************************          
     C*   S004: DELETE EMPLOYEE MASTER                               *          
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
     C** Mark expense record deleted.                                           
     C     EMKY02        CHAIN     UF1                                91        
     C                   IF        *IN91 = *OFF                                 
     C                   MOVE      'N'           EMPDSP                         
     C                   UPDATE    UF1                                          
     C                   ENDIF                                                  
     C*                                                                         
     C** Update subfile.                                                        
     C                   IF        *IN91 = *OFF                                 
     C     CRRN          CHAIN     WDW006S                            22        
     C                   CLEAR                   SELIO                          
     C                   MOVE      *ZEROS        VDNBR1                         
     C                   MOVE      *BLANKS       VDNME1                         
     C                   MOVEL     #DMSG         VDNME1                         
     C                   UPDATE    WDW006S                                      
     C                   ENDIF                                                  
     C*                                                                         
     C     E005          ENDSR                                                  
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
     C                   MOVEL     'IS928D1'     MNUNME                         
     C                   MOVEL     '01'          MNUOPT                         
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
     C     EMKY01        KLIST                                                  
     C                   KFLD                    DFCMPX                         
     C                   KFLD                    LWNMEX                         
     C*                                                                         
     C     EMKY02        KLIST                                                  
     C                   KFLD                    DFCMPX                         
     C                   KFLD                    VDNBR1                         
     C*                                                                         
     C     EMKY03        KLIST                                                  
     C                   KFLD                    DFCMPX                         
     C                   KFLD                    DFNMEX                         
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
     C                   MOVE      *ZEROS        DFCMPX                         
     C                   MOVE      *BLANKS       DFNMEX                         
     C                   MOVE      *BLANKS       LWNMEX           40            
     C                   MOVE      'N'           WKF03             1            
     C                   MOVE      'Y'           DFACTX            1            
     C*                                                                         
     C                   EVAL      CRRN = 0                                     
     C*                                                                         
     C                   ENDSR                                                  
     C*                                                                         
     C****************************************************************          
     C****************************************************************          
