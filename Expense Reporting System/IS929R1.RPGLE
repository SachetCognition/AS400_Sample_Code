     H*****************************************************************         
     H* FastBreed Technologies, Inc. © 2000                           *         
     H*****************************************************************         
     H*****************************************************************         
     H**                                                             **         
     H** OBJECT ID: IS929R1                                          **         
     H**                                                             **         
     H** OBJECT DESCRIPTION: ENTER EXPENSES                          **         
     H**                                                             **         
     H** MOD#    DATE      DESCRIPTION                               **         
     H** ----  --------    ----------------------------------------- **         
     H**                                                             **         
     H*****************************************************************         
     F*                                                                         
     FIS925P1   IF   E           K DISK                                         
     FIS926P1   IF   E           K DISK                                         
     FIS927L1   IF   E           K DISK                                         
     FIS927L4   IF   E           K DISK                                         
     FIS928P1   IF   E           K DISK                                         
     FIS929P1   O  A E           K DISK                                         
     FIS929P2   UF A E           K DISK                                         
     FIS922P1   IF   E           K DISK                                         
     FIS921P1   IF   E           K DISK                                         
     FIS929D1   CF   E             WORKSTN INFDS(INFDS)                         
     F                                     SFILE(WDW006S:CRRN)                  
     D*                                                                         
     D  DATISO         S               D   DATFMT(*ISO)                         
     D  DATMDY         S               D   DATFMT(*MDY)                         
     D  BEGDTE         S               D   DATFMT(*ISO)                         
     D  ENDDTE         S               D   DATFMT(*ISO)                         
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
     D*                                                                         
     D                 DS                                                       
     D XXED00                  1     10    DIM(10)                              
     D  XXED01                 1      1                                         
     D  XXED02                 2      2                                         
     D  XXED03                 3      3                                         
     D  XXED04                 4      4                                         
     D  XXED05                 5      5                                         
     D  XXED06                 6      6                                         
     D  XXED07                 7      7                                         
     D  XXED08                 8      8                                         
     D  XXED09                 9      9                                         
     D  XXED10                10     10                                         
     D*                                                                         
     D INFDS           DS                                                       
     D KEY                   369    369                                         
     D PAGRRN                378    379B 0                                      
     D*                                                                         
     D #DMSG           C                   CONST('* Record deleted')            
     D*                                                                         
     D F01             C                   CONST(X'31')                         
     D F02             C                   CONST(X'32')                         
     D F03             C                   CONST(X'33')                         
     D F07             C                   CONST(X'37')                         
     D F08             C                   CONST(X'38')                         
     D F09             C                   CONST(X'39')                         
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
     C     EXKY01        SETLL     R926P1                                       
     C*                                                                         
     C** Mainline logic.                                                        
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
     C                   MOVE      *ON           *IN28                          
     C                   MOVEA     *BLANKS       XXED00                         
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
     C** F2 Pressed = Search                                                    
     C                   IF        *IN02 = *ON                                  
     C                   EXSR      S051                                         
     C                   GOTO      MAIN01                                       
     C                   ENDIF                                                  
     C*                                                                         
     C** Build subfile                                                          
     C                   IF        DFCMPX <> SVCMPX OR (ROW=4 AND COL=15)       
     C                   EXSR      S002                                         
     C                   EXSR      S001                                         
     C                   Z-ADD     4             VDROW                          
     C                   Z-ADD     27            VDCOL                          
     C                   MOVE      DFCMPX        SVCMPX                         
     C                   GOTO      MAIN01                                       
     C                   ENDIF                                                  
     C*                                                                         
     C** Initialize fields and values                                           
     C                   Z-ADD     6             VDROW                          
     C                   Z-ADD     13            VDCOL                          
     C*                                                                         
     C** If pagedown                                                            
     C                   IF        KEY = PAGEDN                                 
     C                   GOTO      MAIN01                                       
     C                   ENDIF                                                  
     C*                                                                         
     C** Validate company and employee numbers                                  
     C                   EXSR      S003                                         
     C     PASSEDIT      CABEQ     'N'           MAIN01                         
     C*                                                                         
     C** Change expenses                                                        
     C                   IF        KEY = F07                                    
     C                   EXSR      S007                                         
     C                   GOTO      MAIN01                                       
     C                   ENDIF                                                  
     C*                                                                         
     C** Add Projects                                                           
     C                   IF        KEY = F08                                    
     C                   EXSR      S008                                         
     C                   GOTO      MAIN01                                       
     C                   ENDIF                                                  
     C*                                                                         
     C** Add Cost Centers                                                       
     C                   IF        KEY = F09                                    
     C                   EXSR      S009                                         
     C                   GOTO      MAIN01                                       
     C                   ENDIF                                                  
     C*                                                                         
     C** Validate rest of fields                                                
     C                   EXSR      S004                                         
     C     PASSEDIT      CABEQ     'N'           MAIN01                         
     C*                                                                         
     C** Load output fields                                                     
herexC                   EXSR      S005                                         
     C*                                                                         
     C** Write transactions                                                     
     C                   WRITE     R929P1                                       
     C*                                                                         
     C                   Z-ADD     *ZEROS        DFCDEX                         
     C                   Z-ADD     *ZEROS        DFAMTX                         
     C                   MOVE      *BLANKS       DFDETX                         
     C                   GOTO      MAIN01                                       
     C*                                                                         
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
     C                   EVAL      CRRNSV = 0                                   
     C*                                                                         
     C     T001          TAG                                                    
     C     DFCMPX        READE     R926P1                                 99    
     C     *IN99         CABEQ     *ON           E001                           
     C     EXPDSP        CABEQ     'N'           T001                           
     C*                                                                         
     C                   ADD       1             CRRN                           
     C                   MOVE      EXPUNQ        VDUNQ1                         
     C                   MOVE      EXPCDE        VDCDE1                         
     C                   MOVEL     EXPTYP        VDTYP1                         
     C                   MOVEL     EXPDES        VDDES1                         
     C                   MOVE      EXPMLT        VDMLT1                         
     C*                                                                         
     C                   IF        CRRNSV = 0                                   
     C                   Z-ADD     CRRN          CRRNSV            5 0          
     C                   Z-ADD     CRRN          RCDNBR                         
     C                   ENDIF                                                  
     C*                                                                         
     C                   WRITE     WDW006S                                      
     C                   GOTO      T001                                         
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
     C                   Z-ADD     *ZEROS        DFNBRX                         
     C                   Z-ADD     *ZEROS        DFCDEX                         
     C                   Z-ADD     *ZEROS        DFAMTX                         
     C                   Z-ADD     *ZEROS        DFDTEX                         
     C                   Z-ADD     *ZEROS        DFPRJX                         
     C                   Z-ADD     *ZEROS        DFCSTX                         
     C                   MOVE      *BLANKS       DFDETX                         
     C                   MOVE      *BLANKS       DFLOC1                         
     C                   MOVE      *BLANKS       DFLOC2                         
     C*                                                                         
     C                   RESET                   CRRN                           
     C                   RESET                   CRRNSV                         
     C*                                                                         
     C     EXKY01        SETLL     R926P1                                       
     C*                                                                         
     C     E002          ENDSR                                                  
     C*                                                                         
     C****************************************************************          
     C*   S003: VALIDATE COMPANY AND EMPLOYEE NUMBERS                *          
     C****************************************************************          
     C*                                                                         
     C     S003          BEGSR                                                  
     C*                                                                         
     C                   MOVE      'Y'           PASSEDIT          1            
     C*                                                                         
     C** Validate company number                                                
     C     DFCMPX        CHAIN     R925P1                             95        
     C                   IF        *IN95 = *ON                                  
     C                   Z-ADD     4             VDROW                          
     C                   Z-ADD     11            VDCOL                          
     C                   MOVE      HIREV1        XXED01                         
     C                   MOVE      'N'           PASSEDIT                       
     C                   MOVE      'MSG0007'     MSGID                          
     C                   EXSR      SNDMSGX                                      
     C                   WRITE     SFLCTLM                                      
     C                   GOTO      E003                                         
     C                   ENDIF                                                  
     C*                                                                         
     C** Validate employee number                                               
     C     EMKY01        CHAIN     R928P1                             95        
     C                   IF        *IN95 = *ON                                  
     C                   Z-ADD     4             VDROW                          
     C                   Z-ADD     27            VDCOL                          
     C                   MOVE      HIREV1        XXED02                         
     C                   MOVE      'N'           PASSEDIT                       
     C                   MOVE      'MSG0013'     MSGID                          
     C                   EXSR      SNDMSGX                                      
     C                   WRITE     SFLCTLM                                      
     C                   GOTO      E003                                         
     C                   ENDIF                                                  
     C*                                                                         
     C     E003          ENDSR                                                  
     C*                                                                         
     C****************************************************************          
     C*   S004: VALIDATE REST OF FIELDS                              *          
     C****************************************************************          
     C*                                                                         
     C     S004          BEGSR                                                  
     C*                                                                         
     C                   MOVE      'Y'           PASSEDIT          1            
     C*                                                                         
     C** Validate project number                                                
     C                   IF        DFPRJX <> 0                                  
     C     PRKY01        CHAIN     R927P1                             95        
     C                   IF        *IN95 = *ON OR (*IN95=*OFF AND PRJDSP='N')   
     C                   Z-ADD     4             VDROW                          
     C                   Z-ADD     43            VDCOL                          
     C                   MOVE      HIREV1        XXED06                         
     C                   MOVE      'N'           PASSEDIT                       
     C                   MOVE      'MSG0014'     MSGID                          
     C                   EXSR      SNDMSGX                                      
     C                   WRITE     SFLCTLM                                      
     C                   GOTO      E004                                         
     C                   ENDIF                                                  
     C                   ENDIF                                                  
     C*                                                                         
     C                   IF        DFPRJX = 0 AND CMPRQ1 = 'Y'                  
     C                   Z-ADD     4             VDROW                          
     C                   Z-ADD     43            VDCOL                          
     C                   MOVE      HIREV1        XXED06                         
     C                   MOVE      'N'           PASSEDIT                       
     C                   MOVE      'MSG0015'     MSGID                          
     C                   EXSR      SNDMSGX                                      
     C                   WRITE     SFLCTLM                                      
     C                   GOTO      E004                                         
     C                   ENDIF                                                  
     C*                                                                         
     C** Validate cost center                                                   
     C                   IF        DFCSTX <> 0                                  
     C     CSKY01        CHAIN     R927P4                             95        
     C                   IF        *IN95 = *ON OR (*IN95=*OFF AND CSTDSP='N')   
     C                   Z-ADD     4             VDROW                          
     C                   Z-ADD     61            VDCOL                          
     C                   MOVE      HIREV1        XXED07                         
     C                   MOVE      'N'           PASSEDIT                       
     C                   MOVE      'MSG0016'     MSGID                          
     C                   EXSR      SNDMSGX                                      
     C                   WRITE     SFLCTLM                                      
     C                   GOTO      E004                                         
     C                   ENDIF                                                  
     C                   ENDIF                                                  
     C*                                                                         
     C                   IF        DFCSTX = 0 AND CMPRQ2 = 'Y'                  
     C                   Z-ADD     4             VDROW                          
     C                   Z-ADD     61            VDCOL                          
     C                   MOVE      HIREV1        XXED07                         
     C                   MOVE      'N'           PASSEDIT                       
     C                   MOVE      'MSG0017'     MSGID                          
     C                   EXSR      SNDMSGX                                      
     C                   WRITE     SFLCTLM                                      
     C                   GOTO      E004                                         
     C                   ENDIF                                                  
     C*                                                                         
     C** Validate expense code                                                  
     C                   IF        DFCDEX <= 0                                  
     C                   Z-ADD     6             VDROW                          
     C                   Z-ADD     13            VDCOL                          
     C                   MOVE      HIREV1        XXED03                         
     C                   MOVE      'N'           PASSEDIT                       
     C                   MOVE      'MSG0018'     MSGID                          
     C                   EXSR      SNDMSGX                                      
     C                   WRITE     SFLCTLM                                      
     C                   GOTO      E004                                         
     C                   ENDIF                                                  
     C*                                                                         
     C     EXKY02        CHAIN     R926P1                             95        
     C                   IF        *IN95 = *ON OR (*IN95=*OFF AND EXPDSP='N')   
     C                   Z-ADD     6             VDROW                          
     C                   Z-ADD     13            VDCOL                          
     C                   MOVE      HIREV1        XXED03                         
     C                   MOVE      'N'           PASSEDIT                       
     C                   MOVE      'MSG0022'     MSGID                          
     C                   EXSR      SNDMSGX                                      
     C                   WRITE     SFLCTLM                                      
     C                   GOTO      E004                                         
     C                   ENDIF                                                  
     C*                                                                         
     C** Validate amount                                                        
     C                   IF        DFAMTX <= 0                                  
     C                   Z-ADD     6             VDROW                          
     C                   Z-ADD     26            VDCOL                          
     C                   MOVE      HIREV1        XXED04                         
     C                   MOVE      'N'           PASSEDIT                       
     C                   MOVE      'MSG0019'     MSGID                          
     C                   EXSR      SNDMSGX                                      
     C                   WRITE     SFLCTLM                                      
     C                   GOTO      E004                                         
     C                   ENDIF                                                  
     C*                                                                         
     C** Validate date                                                          
     C                   IF        DFDTEX = 0                                   
     C                   Z-ADD     6             VDROW                          
     C                   Z-ADD     43            VDCOL                          
     C                   MOVE      HIREV1        XXED05                         
     C                   MOVE      'N'           PASSEDIT                       
     C                   MOVE      'MSG0020'     MSGID                          
     C                   EXSR      SNDMSGX                                      
     C                   WRITE     SFLCTLM                                      
     C                   GOTO      E004                                         
     C                   ENDIF                                                  
     C*                                                                         
     C                   IF        DFDTEX <> 0                                  
     C     *MDY          TEST(D)                 DFDTEX                 95      
     C     *IN95         IFEQ      '1'                                          
     C                   Z-ADD     6             VDROW                          
     C                   Z-ADD     43            VDCOL                          
     C                   MOVE      HIREV1        XXED05                         
     C                   MOVE      'N'           PASSEDIT                       
     C                   MOVE      'MSG0021'     MSGID                          
     C                   EXSR      SNDMSGX                                      
     C                   WRITE     SFLCTLM                                      
     C                   GOTO      E004                                         
     C                   ENDIF                                                  
     C                   ENDIF                                                  
     C*                                                                         
     C** Validate details                                                       
     C                   IF        DFDETX =*BLANKS  AND EXPDET = 'Y'            
     C                   Z-ADD     8             VDROW                          
     C                   Z-ADD     11            VDCOL                          
     C                   MOVE      HIREV1        XXED08                         
     C                   MOVE      'N'           PASSEDIT                       
     C                   MOVE      'MSG0023'     MSGID                          
     C                   EXSR      SNDMSGX                                      
     C                   WRITE     SFLCTLM                                      
     C                   GOTO      E004                                         
     C                   ENDIF                                                  
     C*                                                                         
     C     E004          ENDSR                                                  
     C*                                                                         
     C****************************************************************          
     C*   S005: LOAD OUTPUT FIELDS                                   *          
     C****************************************************************          
     C*                                                                         
     C     S005          BEGSR                                                  
     C*                                                                         
     C** Initialize fields                                                      
     C                   MOVE      *BLANKS       CMPNME                         
     C                   MOVE      *ZEROS        PRJUNQ                         
     C                   MOVE      *BLANKS       PRJNME                         
     C                   MOVE      *ZEROS        CSTUNQ                         
     C                   MOVE      *BLANKS       CSTNME                         
     C                   MOVE      *ZEROS        EMPNBR                         
     C                   MOVE      *BLANKS       EMPNME                         
     C                   MOVE      *ZEROS        EMPDPT                         
     C                   MOVE      *ZEROS        EXPUNQ                         
     C                   MOVE      *ZEROS        EXPCDE                         
     C                   MOVE      *BLANKS       EXPTYP                         
     C                   MOVE      *ZEROS        EXPMLT                         
     C                   MOVE      *BLANKS       EXPADV                         
     C                   MOVE      *ZEROS        EXPLIN                         
     C*                                                                         
     C** Company-related fields                                                 
     C     DFCMPX        CHAIN     R925P1                             95        
     C*                                                                         
     C** Employee-related fields                                                
     C     EMKY01        CHAIN     R928P1                             95        
     C*                                                                         
     C** Project-related fields                                                 
     C     PRKY01        CHAIN     R927P1                             95        
     C*                                                                         
     C** Cost Center-related fields                                             
     C     CSKY01        CHAIN     R927P4                             95        
     C*                                                                         
     C** Expense-related fields                                                 
     C     EXKY02        CHAIN     R926P1                             95        
     C*                                                                         
     C** Transaction-related fields                                             
     C     CMPUNQ        CHAIN     R929P2                             95        
     C                   IF        *IN95 = *OFF                                 
     C                   ADD       1             NXTUNQ                         
     C                   UPDATE    R929P2                                       
     C                   ELSE                                                   
     C                   Z-ADD     1             NXTUNQ                         
     C                   WRITE     R929P2                                       
     C                   ENDIF                                                  
     C                   Z-ADD     NXTUNQ        TRNUNQ                         
     C                   MOVE      DFDTEX        DATMDY                         
     C                   MOVE      DATMDY        DATISO                         
     C                   MOVE      DATISO        TRNDTE                         
     C                   Z-ADD     DFAMTX        TRNAMT                         
     C                   MOVEL     DFLOC1        TRNLC1                         
     C                   MOVEL     DFLOC2        TRNLC2                         
     C                   MOVEL     DFDETX        TRNDET                         
     C*                                                                         
     C     E005          ENDSR                                                  
     C*                                                                         
     C****************************************************************          
     C*   S007: CHANGE EXPENSES                                      *          
     C****************************************************************          
     C*                                                                         
     C     S007          BEGSR                                                  
     C*                                                                         
     C                   MOVE      DFCMPX        OUCMPX            5            
     C                   MOVE      DFNBRX        OUNBRX            8            
     C                   MOVE      EMPUNQ        OUEMPX            5            
     C                   MOVEL     EMPNME        OUNMEX           40            
     C*                                                                         
     C                   CALL      'IS929R2'                                    
     C                   PARM                    INQ25                          
     C                   PARM                    OUCMPX                         
     C                   PARM                    OUNBRX                         
     C                   PARM                    OUEMPX                         
     C                   PARM                    OUNMEX                         
     C                   PARM                    WKF03                          
     C*                                                                         
     C                   IF        WKF03 = 'Y'                                  
     C                   MOVE      *ON           *INLR                          
     C                   RETURN                                                 
     C                   ENDIF                                                  
     C*                                                                         
     C     E007          ENDSR                                                  
     C*                                                                         
     C****************************************************************          
     C*   S008: ADD PROJECTS                                         *          
     C****************************************************************          
     C*                                                                         
     C     S008          BEGSR                                                  
     C*                                                                         
     C                   IF        INQ25 <> '0'                                 
     C                   MOVE      'MSG0001'     MSGID                          
     C                   EXSR      SNDMSGX                                      
     C                   WRITE     SFLCTLM                                      
     C                   GOTO      E008                                         
     C                   ENDIF                                                  
     C*                                                                         
     C                   MOVE      DFCMPX        OUCMPX            5            
     C                   MOVE      *ZEROS        OUUNQX            5            
     C                   MOVE      'N'           WKF03             1            
     C*                                                                         
     C                   CALL      'IS927R2'                                    
     C                   PARM                    OUCMPX                         
     C                   PARM                    OUUNQX                         
     C                   PARM                    WKF03                          
     C*                                                                         
     C                   IF        WKF03 = 'Y'                                  
     C                   MOVE      *ON           *INLR                          
     C                   RETURN                                                 
     C                   ENDIF                                                  
     C*                                                                         
     C     E008          ENDSR                                                  
     C*                                                                         
     C****************************************************************          
     C*   S009: ADD COST CENTERS                                     *          
     C****************************************************************          
     C*                                                                         
     C     S009          BEGSR                                                  
     C*                                                                         
     C                   IF        INQ25 <> '0'                                 
     C                   MOVE      'MSG0001'     MSGID                          
     C                   EXSR      SNDMSGX                                      
     C                   WRITE     SFLCTLM                                      
     C                   GOTO      E009                                         
     C                   ENDIF                                                  
     C*                                                                         
     C                   MOVE      DFCMPX        OUCMPX            5            
     C                   MOVE      *ZEROS        OUUNQX            5            
     C                   MOVE      'N'           WKF03             1            
     C*                                                                         
     C                   CALL      'IS927R5'                                    
     C                   PARM                    OUCMPX                         
     C                   PARM                    OUUNQX                         
     C                   PARM                    WKF03                          
     C*                                                                         
     C                   IF        WKF03 = 'Y'                                  
     C                   MOVE      *ON           *INLR                          
     C                   RETURN                                                 
     C                   ENDIF                                                  
     C*                                                                         
     C     E009          ENDSR                                                  
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
     C                   MOVEL     'IS929D1'     MNUNME                         
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
     C*  S051: SEARCH FOR VALID VALUES                                          
     C*****************************************************************         
     C*                                                                         
     C     S051          BEGSR                                                  
     C*                                                                         
     C                   MOVE      'N'           FOUND             1            
     C*                                                                         
     C                   Z-ADD     ROW           SVROW             3 0          
     C                   Z-ADD     COL           SVCOL             3 0          
     C*                                                                         
     C** Project search                                                         
     C     FLD           IFEQ      'DFPRJX'                                     
     C                   MOVE      'Y'           FOUND             1            
     C                   MOVE      DFCMPX        INCMPX            5            
     C                   MOVE      *BLANKS       INPRJX            5            
     C                   MOVE      *BLANKS       INNMEX           40            
     C                   CALL      'IS924R1'                                    
     C                   PARM                    INCMPX                         
     C                   PARM                    INPRJX                         
     C                   PARM                    INNMEX                         
     C     INPRJX        IFNE      *BLANKS                                      
     C                   MOVE      INPRJX        DFPRJX                         
     C                   ENDIF                                                  
     C                   GOTO      T051                                         
     C                   ENDIF                                                  
     C*                                                                         
     C** Cost Center search                                                     
     C     FLD           IFEQ      'DFCSTX'                                     
     C                   MOVE      'Y'           FOUND             1            
     C                   MOVE      DFCMPX        INCMPX            5            
     C                   MOVE      *BLANKS       INCSTX            5            
     C                   MOVE      *BLANKS       INNMEX           40            
     C                   CALL      'IS924R2'                                    
     C                   PARM                    INCMPX                         
     C                   PARM                    INCSTX                         
     C                   PARM                    INNMEX                         
     C     INCSTX        IFNE      *BLANKS                                      
     C                   MOVE      INCSTX        DFCSTX                         
     C                   ENDIF                                                  
     C                   GOTO      T051                                         
     C                   ENDIF                                                  
     C*                                                                         
     C** Employee search                                                        
     C     FLD           IFEQ      'DFNBRX'                                     
     C                   MOVE      'Y'           FOUND             1            
     C                   MOVE      DFCMPX        INCMPX            5            
     C                   MOVE      *BLANKS       INEMPX            5            
     C                   MOVE      *BLANKS       INNBRX            8            
     C                   MOVE      *BLANKS       INNMEX           40            
     C                   CALL      'IS924R3'                                    
     C                   PARM                    INCMPX                         
     C                   PARM                    INEMPX                         
     C                   PARM                    INNBRX                         
     C                   PARM                    INNMEX                         
     C     INNBRX        IFNE      *BLANKS                                      
     C                   MOVE      INNBRX        DFNBRX                         
     C                   ENDIF                                                  
     C                   GOTO      T051                                         
     C                   ENDIF                                                  
     C*                                                                         
     C     T051          TAG                                                    
     C                   Z-ADD     SVROW         VDROW                          
     C                   Z-ADD     SVCOL         VDCOL                          
     C*                                                                         
     C     FOUND         IFEQ      'N'                                          
     C                   MOVE      'MSG0030'     MSGID                          
     C                   EXSR      SNDMSGX                                      
     C                   WRITE     SFLCTLM                                      
     C                   ENDIF                                                  
     C*                                                                         
     C     E051          ENDSR                                                  
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
     C     CSKY01        KLIST                                                  
     C                   KFLD                    DFCMPX                         
     C                   KFLD                    DFCSTX                         
     C*                                                                         
     C     EMKY01        KLIST                                                  
     C                   KFLD                    DFCMPX                         
     C                   KFLD                    DFNBRX                         
     C*                                                                         
     C     EXKY01        KLIST                                                  
     C                   KFLD                    DFCMPX                         
     C                   KFLD                    LWCDEX                         
     C*                                                                         
     C     EXKY02        KLIST                                                  
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
     C     PRKY01        KLIST                                                  
     C                   KFLD                    DFCMPX                         
     C                   KFLD                    DFPRJX                         
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
     C                   MOVE      *ZEROS        DFCDEX                         
     C                   MOVE      *ZEROS        LWCDEX            3 0          
     C                   MOVE      *ZEROS        WKEMPX            5 0          
     C                   MOVE      'N'           WKF03             1            
     C*                                                                         
     C                   EVAL      CRRN = 0                                     
     C*                                                                         
     C                   ENDSR                                                  
     C*                                                                         
     C****************************************************************          
     C****************************************************************          
