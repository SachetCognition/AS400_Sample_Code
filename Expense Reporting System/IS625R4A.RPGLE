     H*****************************************************************         
     H* FastBreed Technologies, Inc. © 2000                           *         
     H*****************************************************************         
     H*****************************************************************         
     H**                                                             **         
     H** OBJECT ID: IS625R4A                                         **         
     H**                                                             **         
     H** OBJECT DESCRIPTION: DEFINE USER ACCESS TO COMPANIES         **         
     H**                                                             **         
     H** MOD#    DATE      DESCRIPTION                               **         
     H** ----  --------    ----------------------------------------- **         
     H**                                                             **         
     H*****************************************************************         
     F*                                                                         
     FIS922P1   IF   E           K DISK                                         
     FIS921P1   IF   E           K DISK                                         
     FIS625P1   IF   E           K DISK    PREFIX(CM_)                          
     FIS625L3   IF   E           K DISK                                         
     FIS625P3   UF   E           K DISK    PREFIX(US_) RENAME(R625P3:UF1)       
     FIS625D4A  CF   E             WORKSTN INFDS(INFDS)                         
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
     D DFUSRX          S             10                                         
     D SVUSRX          S             10                                         
     D*                                                                         
     D INFDS           DS                                                       
     D KEY                   369    369                                         
     D PAGRRN                378    379B 0                                      
     D*                                                                         
     D #RMSG           C                   CONST('* Access Removed')            
     D*                                                                         
     D F01             C                   CONST(X'31')                         
     D F03             C                   CONST(X'33')                         
     D F06             C                   CONST(X'36')                         
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
     C     *LOVAL        SETLL     R625P3                                       
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
     C                   IF        DFUSRX <> SVUSRX                             
     C                   MOVEL     DFUSRX        SVUSRX                         
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
     C** Remove User Access to Menu                                             
     C                   IF        SELIO = 'R'                                  
     C                   EXSR      S006                                         
     C                   ENDIF                                                  
     C*                                                                         
     C                   READC     WDW006S                                98    
     C                   ENDDO                                                  
     C*                                                                         
     C                   EXSR      S002                                         
     C                   EXSR      S001                                         
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
     C                   READ      R625P3                                 99    
     C*                                                                         
     C                   IF        *IN99 = *ON                                  
     C                   LEAVE                                                  
     C                   ENDIF                                                  
     C*                                                                         
     C** Only show companies user has access to                                 
     C     VDUSID        IFNE      'CMPINZUSR'                                  
     C     CMKY02        CHAIN(N)  UF1                                95        
     C     *IN95         IFEQ      *ON                                          
     C                   ITER                                                   
     C                   ENDIF                                                  
     C                   ENDIF                                                  
     C*                                                                         
     C                   CLEAR                   SELIO                          
     C                   ADD       1             CRRN                           
     C                   ADD       1             COUNT                          
     C                   MOVEL     ACCUSR        VDUSR1                         
     C                   Z-ADD     CMPUNQ        VDUNQ1                         
     C*                                                                         
     C** Get user description                                                   
     C                   MOVE      *BLANKS       VDUSR2                         
     C                   MOVE      *BLANKS       WKDESX           50            
     C                   CALL      'IS625C4A'                                   
     C                   PARM                    VDUSR1                         
     C                   PARM                    WKDESX                         
     C                   MOVEL     WKDESX        VDUSR2                         
     C*                                                                         
     C** Get company description                                                
     C                   MOVE      *BLANKS       VDUNQ2                         
     C     CMPUNQ        CHAIN     R625P1                                       
     C     *IN95         IFEQ      *OFF                                         
     C                   MOVEL     CM_CMPNME     VDUNQ2                         
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
     C                   MOVEL     DFUSRX        WKKEY1           10            
     C                   MOVE      *ZEROS        WKKEY2            5 0          
     C     WKKY01        SETLL     R625P3                                       
     C*                                                                         
     C     E002          ENDSR                                                  
     C*                                                                         
     C****************************************************************          
     C*   S003: ADD USER ACCESS TO MENU                              *          
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
     C                   MOVE      *BLANKS       OUUSRX           10            
     C*                                                                         
     C                   CALL      'IS625R4B'                                   
     C                   PARM                    OUUSRX                         
     C                   PARM                    WKF03                          
     C*                                                                         
     C                   IF        WKF03 = 'Y'                                  
     C                   MOVE      *ON           *INLR                          
     C                   RETURN                                                 
     C                   ENDIF                                                  
     C*                                                                         
     C                   IF        OUUSRX <> '          '                       
     C                   MOVEL     OUUSRX        DFUSRX                         
     C                   MOVEL     OUUSRX        SVUSRX                         
     C                   ENDIF                                                  
     C*                                                                         
     C     E003          ENDSR                                                  
     C*                                                                         
     C****************************************************************          
     C*   S006: REMOVE USER ACCESS TO MENU                           *          
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
     C** Remove user access to company (only companies remover has access       
     C** to are displayed, so they must belong to the same company as the       
     C** user they're removing access for)                                      
     C     CMKY01        CHAIN     UF1                                22        
     C     *IN22         IFEQ      *OFF                                         
     C                   DELETE    UF1                                          
     C                   ENDIF                                                  
     C*                                                                         
     C** Update subfile.                                                        
     C     CRRN          CHAIN     WDW006S                            22        
     C                   CLEAR                   SELIO                          
     C                   EVAL      VDUSR1 = *BLANKS                             
     C                   EVAL      VDUSR2 = *BLANKS                             
     C                   EVAL      VDUNQ1 = *ZEROS                              
     C                   EVAL      VDUNQ2 = *BLANKS                             
     C                   MOVEL     #RMSG         VDUSR2                         
     C                   UPDATE    WDW006S                                      
     C*                                                                         
     C     E006          ENDSR                                                  
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
     C                   MOVEL     'IS625D4A'    MNUNME                         
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
     C     CMKY01        KLIST                                                  
     C                   KFLD                    VDUNQ1                         
     C                   KFLD                    VDUSR1                         
     C*                                                                         
     C     CMKY02        KLIST                                                  
     C                   KFLD                    CMPUNQ                         
     C                   KFLD                    VDUSID                         
     C*                                                                         
     C     WKKY01        KLIST                                                  
     C                   KFLD                    WKKEY1                         
     C                   KFLD                    WKKEY2                         
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
     C                   MOVE      *BLANKS       DFUSRX                         
     C                   MOVE      *BLANKS       SVUSRX                         
     C                   MOVE      *BLANKS       WKKEY1                         
     C                   MOVE      *ZEROS        WKKEY2                         
     C                   MOVE      'N'           WKF03             1            
     C                   MOVE      *BLANKS       LWMENU           10            
     C*                                                                         
     C     *DATE         MULT      10000.0001    TODAY             8 0          
     C*                                                                         
     C                   EVAL      CRRN = 0                                     
     C*                                                                         
     C                   ENDSR                                                  
     C*                                                                         
     C****************************************************************          
     C****************************************************************          
