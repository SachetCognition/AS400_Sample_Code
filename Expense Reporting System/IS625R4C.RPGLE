     H*****************************************************************         
     H* FastBreed Technologies, Inc. © 2000                           *         
     H*****************************************************************         
     H*****************************************************************         
     H**                                                             **         
     H** OBJECT ID: IS625R3C                                         **         
     H**                                                             **         
     H** OBJECT DESCRIPTION: DEFINE USER ACCESS TO COMPANY - CHG MODE**         
     H**                                                             **         
     H** MOD#    DATE      DESCRIPTION                               **         
     H** ----  --------    ----------------------------------------- **         
     H**                                                             **         
     H*****************************************************************         
     F*                                                                         
     FEXPENSEP  IF   E           K DISK                                         
     FIS922P1   IF   E           K DISK                                         
     FIS921P1   IF   E           K DISK                                         
     FIS625P4   UF A E           K DISK                                         
     FIS625D3C  CF   E             WORKSTN INFDS(INFDS)                         
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
     D*                                                                         
     D INFDS           DS                                                       
     D KEY                   369    369                                         
     D PAGRRN                378    379B 0                                      
     D*                                                                         
     D #DMSG           C                   CONST('* Deleted')                   
     D #STS1           C                   CONST('Authorized')                  
     D #STS2           C                   CONST('Not Authorized')              
     D*                                                                         
     D F01             C                   CONST(X'31')                         
     D F03             C                   CONST(X'33')                         
     D F12             C                   CONST(X'3C')                         
     D ENTER           C                   CONST(X'F1')                         
     D PAGEDN          C                   CONST(X'F5')                         
     D*                                                                         
     D PSDS           SDS                                                       
     D  PGMNAM           *PROC                                                  
     D  VDUSID               254    263                                         
     D*                                                                         
     D** Hi Intensity - Reverse Image                                           
     D HIREV1          C                   CONST(X'23')                         
     I*                                                                         
     C*****************************************************************         
     C*  MAINLINE                                                     *         
     C*****************************************************************         
     C*                                                                         
herexC     *LOVAL        SETLL     IMENU                                        
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
     C                   Z-ADD     7             VDROW                          
     C                   Z-ADD     3             VDCOL                          
     C                   ELSE                                                   
     C                   MOVE      *OFF          *IN42                          
     C                   Z-ADD     7             VDROW                          
     C                   Z-ADD     3             VDCOL                          
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
     C** Add User Access to Menu                                                
     C                   IF        SELIO = 'A'                                  
     C                   EXSR      S004                                         
     C                   ENDIF                                                  
     C*                                                                         
     C** Remove User Access to Menu                                             
     C                   IF        SELIO = 'U'                                  
     C                   EXSR      S005                                         
     C                   ENDIF                                                  
     C*                                                                         
     C                   READC     WDW006S                                98    
     C                   ENDDO                                                  
     C*                                                                         
     C** Display record changed essage                                          
     C                   MOVE      'MSG0152'     MSGID                          
     C                   EXSR      SNDMSGX                                      
     C                   WRITE     SFLCTLM                                      
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
     C                   READ      IMENU                                  99    
     C*                                                                         
     C                   IF        *IN99 = *ON                                  
     C                   LEAVE                                                  
     C                   ENDIF                                                  
     C*                                                                         
     C** Skip System Setup option                                               
     C                   IF        MMOPTP = 'IS625R0'                           
     C                   ITER                                                   
     C                   ENDIF                                                  
     C*                                                                         
     C                   CLEAR                   SELIO                          
     C                   ADD       1             CRRN                           
     C                   ADD       1             COUNT                          
     C                   MOVEL     MMMENU        HDMENU                         
     C                   Z-ADD     MMOPTN        HDOPTN                         
     C                   MOVEL     MMOPTD        HDOPTD                         
     C                   MOVEL     MMOPTP        HDOPTP                         
     C                   MOVEL     MMSTRT        HDSTRT                         
     C                   MOVEL     MMOPTD        VDDES1                         
     C*                                                                         
     C** Check for existing menu option access record                           
     C                   MOVE      *BLANKS       VDSTS1                         
     C     MMKY01        CHAIN     R625P4                             95        
     C     *IN95         IFEQ      *OFF                                         
     C                   MOVEL     #STS1         VDSTS1                         
     C                   ELSE                                                   
     C                   MOVEL     #STS2         VDSTS1                         
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
     C     *LOVAL        SETLL     IMENU                                        
     C*                                                                         
     C     E002          ENDSR                                                  
herexC*                                                                         
     C****************************************************************          
     C*   S004: ADD USER ACCESS TO MENU                              *          
     C****************************************************************          
     C*                                                                         
     C     S004          BEGSR                                                  
     C*                                                                         
     C** Make sure user doesn't already have access to menu option              
     C     MMKY01        CHAIN     R625P4                             95        
     C     *IN95         CABEQ     *OFF          E004                           
     C*                                                                         
     C** Write menu access record                                               
     C                   MOVEL     DFUSRX        ACCUSR                         
     C                   MOVEL     HDMENU        MMMENU                         
     C                   MOVEL     HDOPTP        MMOPTP                         
     C*                                                                         
     C                   MOVEL     VDUSID        ACCCBY                         
     C     *DATE         MULT      10000.0001    ACCCDT                         
     C                   TIME                    ACCCTM                         
     C                   IF        ACCCTM < 120000                              
     C                   MOVE      'A'           ACCCAM                         
     C                   ELSE                                                   
     C                   MOVE      'P'           ACCCAM                         
     C                   ENDIF                                                  
     C*                                                                         
     C                   WRITE     R625P4                                       
     C*                                                                         
     C** Update subfile.                                                        
     C     CRRN          CHAIN     WDW006S                            22        
     C                   CLEAR                   SELIO                          
     C                   MOVE      *BLANKS       VDSTS1                         
     C                   MOVEL     #STS1         VDSTS1                         
     C                   UPDATE    WDW006S                                      
     C*                                                                         
     C     E004          ENDSR                                                  
herexC*                                                                         
     C****************************************************************          
     C*   S005: REMOVE USER ACCESS TO MENU                           *          
     C****************************************************************          
     C*                                                                         
     C     S005          BEGSR                                                  
     C*                                                                         
     C** Delete user authorization record                                       
     C     MMKY01        CHAIN     R625P4                             95        
     C     *IN95         CABEQ     *ON           E005                           
     C                   DELETE    R625P4                                       
     C*                                                                         
     C** Update subfile.                                                        
     C     CRRN          CHAIN     WDW006S                            22        
     C                   CLEAR                   SELIO                          
     C                   MOVE      *BLANKS       VDSTS1                         
     C                   MOVEL     #STS2         VDSTS1                         
     C                   UPDATE    WDW006S                                      
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
     C                   MOVEL     'IS625D3C'    MNUNME                         
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
     C                   PARM                    OUUSRX           10            
     C                   PARM                    WKF03             1            
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
     C     MMKY01        KLIST                                                  
     C                   KFLD                    DFUSRX                         
     C                   KFLD                    HDMENU                         
     C                   KFLD                    HDOPTP                         
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
     C                   MOVEL     OUUSRX        DFUSRX                         
     C                   MOVE      'N'           WKF03                          
     C*                                                                         
     C     *DATE         MULT      10000.0001    TODAY             8 0          
     C*                                                                         
     C                   EVAL      CRRN = 0                                     
     C*                                                                         
     C                   ENDSR                                                  
     C*                                                                         
     C****************************************************************          
     C****************************************************************          
