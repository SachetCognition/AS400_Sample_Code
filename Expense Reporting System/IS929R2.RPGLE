     H*****************************************************************         
     H* FastBreed Technologies, Inc. © 2000                           *         
     H*****************************************************************         
     H*****************************************************************         
     H**                                                             **         
     H** OBJECT ID: IS929R2                                          **         
     H**                                                             **         
     H** OBJECT DESCRIPTION: CHANGE EXPENSE TRANSACTIONS             **         
     H**                                                             **         
     H** MOD#    DATE      DESCRIPTION                               **         
     H** ----  --------    ----------------------------------------- **         
     H**                                                             **         
     H*****************************************************************         
     F*                                                                         
     FIS925P1   IF   E           K DISK                                         
     FIS926P1   IF   E           K DISK                                         
     FIS929L1   IF   E           K DISK                                         
     FIS929P1   UF   E           K DISK    RENAME(R929P1:UF1)                   
     FIS922P1   IF   E           K DISK                                         
     FIS921P1   IF   E           K DISK                                         
     FIS929D2   CF   E             WORKSTN INFDS(INFDS)                         
     F                                     SFILE(WDW006S:CRRN)                  
     D*                                                                         
     D  DATISO         S               D   DATFMT(*ISO)                         
     D  DATMDY         S               D   DATFMT(*MDY)                         
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
     D DFDTE1          S              6  0                                      
     D SVDTE1          S              6  0                                      
     D DFDTE2          S              6  0                                      
     D SVDTE2          S              6  0                                      
     D DFCDEX          S              3  0                                      
     D SVCDEX          S              3  0                                      
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
     C     TRKY01        SETLL     R929P1                                       
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
     C                   Z-ADD     6             VDROW                          
     C                   Z-ADD     14            VDCOL                          
     C                   MOVE      *ON           *IN28                          
     C                   MOVE      *BLANKS       XXED01                         
     C                   MOVE      00010101      BEGDTE                         
     C                   MOVE      20391231      ENDDTE                         
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
     C** Validate dates                                                         
     C                   IF        DFDTE1 <> 0                                  
     C     *MDY          TEST(D)                 DFDTE1                 95      
     C     *IN95         IFEQ      '1'                                          
     C                   Z-ADD     6             VDROW                          
     C                   Z-ADD     14            VDCOL                          
     C                   MOVE      HIREV1        XXED01                         
     C                   MOVE      'MSG0021'     MSGID                          
     C                   EXSR      SNDMSGX                                      
     C                   WRITE     SFLCTLM                                      
     C                   GOTO      MAIN01                                       
     C                   ENDIF                                                  
     C                   MOVE      DFDTE1        DATMDY                         
     C                   MOVE      DATMDY        DATISO                         
     C                   MOVE      DATISO        BEGDTE                         
     C                   ENDIF                                                  
     C*                                                                         
     C                   IF        DFDTE2 <> 0                                  
     C     *MDY          TEST(D)                 DFDTE2                 95      
     C     *IN95         IFEQ      '1'                                          
     C                   Z-ADD     6             VDROW                          
     C                   Z-ADD     14            VDCOL                          
     C                   MOVE      HIREV1        XXED02                         
     C                   MOVE      'MSG0021'     MSGID                          
     C                   EXSR      SNDMSGX                                      
     C                   WRITE     SFLCTLM                                      
     C                   GOTO      MAIN01                                       
     C                   ENDIF                                                  
     C                   MOVE      DFDTE2        DATMDY                         
     C                   MOVE      DATMDY        DATISO                         
     C                   MOVE      DATISO        ENDDTE                         
     C                   ENDIF                                                  
     C*                                                                         
     C** Validate expense code filter                                           
     C                   IF        DFCDEX <> 0                                  
     C     EXKY01        CHAIN     R926P1                             95        
     C                   IF        *IN95 = *ON OR (*IN95=*OFF AND EXPDSP='N')   
     C                   Z-ADD     6             VDROW                          
     C                   Z-ADD     43            VDCOL                          
     C                   MOVE      HIREV1        XXED03                         
     C                   MOVE      'MSG0022'     MSGID                          
     C                   EXSR      SNDMSGX                                      
     C                   WRITE     SFLCTLM                                      
     C                   GOTO      MAIN01                                       
     C                   ENDIF                                                  
     C                   ENDIF                                                  
     C*                                                                         
     C** Re-Position Subfile                                                    
     C                   IF        DFDTE1 <> SVDTE1 OR DFDTE2 <> SVDTE2 OR      
     C                             DFCDEX <> SVCDEX                             
     C                   MOVE      DFDTE1        SVDTE1                         
     C                   MOVE      DFDTE2        SVDTE2                         
     C                   MOVE      DFCDEX        SVCDEX                         
     C                   EXSR      S002                                         
     C                   LEAVE                                                  
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
     C** Change Transaction                                                     
     C                   IF        SELIO = 1                                    
     C                   EXSR      S004                                         
     C                   ENDIF                                                  
     C*                                                                         
     C** Delete Transaction                                                     
     C                   IF        SELIO = 4                                    
     C                   EXSR      S005                                         
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
     C*                                                                         
     C                   EVAL      COUNT = 0                                    
     C                   EVAL      CRRNSV = 0                                   
     C*                                                                         
     C                   DOU       COUNT = 13                                   
     C*                                                                         
     C     T001          TAG                                                    
     C*                                                                         
     C     TRKY02        READE     R929P1                                 99    
     C*                                                                         
     C     *IN99         CABEQ     *ON           T002                           
     C     TRNDTE        CABLT     BEGDTE        T001                           
     C     TRNDTE        CABGT     ENDDTE        T002                           
     C*                                                                         
     C                   IF        DFCDEX <> 0 AND DFCDEX <> EXPCDE             
     C                   GOTO      T001                                         
     C                   ENDIF                                                  
     C*                                                                         
     C                   CLEAR                   SELIO                          
     C                   ADD       1             CRRN                           
     C                   ADD       1             COUNT                          
     C                   MOVE      TRNDTE        HDDTE1                         
     C                   MOVE      TRNUNQ        HDUNQ1                         
     C                   MOVE      EXPCDE        VDCDE1                         
     C                   MOVEL     EXPTYP        VDTYP1                         
     C                   MOVE      TRNAMT        VDAMT1                         
     C                   Z-ADD     PRJUNQ        VDPUNQ                         
     C                   Z-ADD     CSTUNQ        VDCUNQ                         
     C*                                                                         
     C                   IF        TRNDTE <> 0                                  
     C                   MOVE      TRNDTE        DATISO                         
     C                   MOVE      DATISO        DATMDY                         
     C                   MOVE      DATMDY        VDDTE1                         
     C                   ELSE                                                   
     C                   MOVE      *ZEROS        VDDTE1                         
     C                   ENDIF                                                  
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
     C     TRKY04        SETLL     R929P1                                       
     C*                                                                         
     C     E002          ENDSR                                                  
     C*                                                                         
     C****************************************************************          
     C*   S004: CHANGE TRANSACTION                                   *          
     C****************************************************************          
     C*                                                                         
     C     S004          BEGSR                                                  
     C*                                                                         
     C                   MOVE      DFCMPX        OUCMPX            5            
     C                   MOVE      WKEMPX        OUEMPX            5            
     C                   MOVE      HDDTE1        OUDTEX            8            
     C                   MOVE      HDUNQ1        OUUNQX            5            
     C                   MOVE      VDDTE1        OUDTE1            6            
     C                   MOVE      VDCDE1        OUCDE1            3            
     C                   MOVEL     VDTYP1        OUTYP1           20            
     C                   MOVE      VDAMT1        OUAMT1            8            
     C                   MOVE      VDPUNQ        OUPUNQ            5            
     C                   MOVE      VDCUNQ        OUCUNQ            5            
     C*                                                                         
     C                   CALL      'IS929R3'                                    
     C                   PARM                    INQ25                          
     C                   PARM                    OUCMPX                         
     C                   PARM                    OUEMPX                         
     C                   PARM                    OUDTEX                         
     C                   PARM                    OUUNQX                         
     C                   PARM                    OUDTE1                         
     C                   PARM                    OUCDE1                         
     C                   PARM                    OUTYP1                         
     C                   PARM                    OUAMT1                         
     C                   PARM                    OUPUNQ                         
     C                   PARM                    OUCUNQ                         
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
     C                   MOVE      OUDTEX        HDDTE1                         
     C                   MOVE      OUUNQX        HDUNQ1                         
     C                   MOVE      OUDTE1        VDDTE1                         
     C                   MOVE      OUCDE1        VDCDE1                         
     C                   MOVEL     OUTYP1        VDTYP1                         
     C                   MOVE      OUAMT1        VDAMT1                         
     C                   MOVE      OUPUNQ        VDPUNQ                         
     C                   MOVE      OUCUNQ        VDCUNQ                         
     C                   UPDATE    WDW006S                                      
     C*                                                                         
     C     E004          ENDSR                                                  
     C*                                                                         
     C****************************************************************          
     C*   S004: DELETE TRANSACTION                                   *          
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
     C** Delete transaction record                                              
     C     TRKY03        CHAIN     UF1                                95        
     C                   IF        *IN95 = *OFF                                 
     C                   DELETE    UF1                                          
     C                   ENDIF                                                  
     C*                                                                         
     C** Update subfile.                                                        
     C                   IF        *IN95 = *OFF                                 
     C     CRRN          CHAIN     WDW006S                            22        
     C                   CLEAR                   SELIO                          
     C                   MOVE      *ZEROS        VDDTE1                         
     C                   MOVE      *ZEROS        VDCDE1                         
     C                   MOVE      *BLANKS       VDTYP1                         
     C                   MOVEL     #DMSG         VDTYP1                         
     C                   MOVE      *ZEROS        VDAMT1                         
     C                   MOVE      *ZEROS        VDPUNQ                         
     C                   MOVE      *ZEROS        VDCUNQ                         
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
     C                   MOVEL     'IS929D2'     MNUNME                         
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
     C                   PARM                    WKCMPX            5            
     C                   PARM                    WKNBRX            8            
     C                   PARM                    WKEMPX            5            
     C                   PARM                    WKNMEX           40            
     C                   PARM                    WKF03             1            
     C*                                                                         
     C     PLIST1        PLIST                                                  
     C                   PARM                    MNUNME                         
     C                   PARM                    MNUOPT                         
     C                   PARM                    FLDNME                         
     C*                                                                         
     C     EXKY01        KLIST                                                  
     C                   KFLD                    DFCMPX                         
     C                   KFLD                    DFCDEX                         
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
     C     TRKY01        KLIST                                                  
     C                   KFLD                    DFCMPX                         
     C                   KFLD                    DFNBRX                         
     C                   KFLD                    LOWDTE                         
     C*                                                                         
     C     TRKY02        KLIST                                                  
     C                   KFLD                    DFCMPX                         
     C                   KFLD                    DFNBRX                         
     C*                                                                         
     C     TRKY03        KLIST                                                  
     C                   KFLD                    DFCMPX                         
     C                   KFLD                    DFEMPX                         
     C                   KFLD                    HDDTE1                         
     C                   KFLD                    HDUNQ1                         
     C*                                                                         
     C     TRKY04        KLIST                                                  
     C                   KFLD                    DFCMPX                         
     C                   KFLD                    DFNBRX                         
     C                   KFLD                    BEGDTE                         
     C                   KFLD                    LOWUNQ                         
     C*                                                                         
     C** Error message preparation                                              
     C                   MOVEL     '*'           MSGQ                           
     C*                                                                         
     C** Remove any error messages                                              
     C                   CALL      'RMVMSGX'                                    
     C                   WRITE     SFLCTLM                                      
     C*                                                                         
     C                   MOVE      WKCMPX        DFCMPX                         
     C                   MOVE      WKNBRX        DFNBRX                         
     C                   MOVE      WKEMPX        DFEMPX            5 0          
     C                   MOVEL     WKNMEX        DFNMEX                         
     C                   MOVE      *ZEROS        LOWDTE            8 0          
     C                   MOVE      *ZEROS        LOWUNQ            5 0          
     C                   MOVE      00010101      BEGDTE            8 0          
     C                   MOVE      00010101      ENDDTE            8 0          
     C                   MOVE      'N'           WKF03             1            
     C*                                                                         
     C                   EVAL      CRRN = 0                                     
     C*                                                                         
     C                   ENDSR                                                  
     C*                                                                         
     C****************************************************************          
     C****************************************************************          
