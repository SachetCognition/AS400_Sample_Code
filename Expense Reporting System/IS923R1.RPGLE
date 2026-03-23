     H*****************************************************************         
     H* FastBreed Technologies, Inc. © 2000                           *         
     H*****************************************************************         
     H*****************************************************************         
     H**                                                             **         
     H** OBJECT ID: IS923R1                                          **         
     H**                                                             **         
     H** OBJECT DESCRIPTION: PRINT EXPENSE REPORT                    **         
     H**                                                             **         
     H** MOD#    DATE      DESCRIPTION                               **         
     H** ----  --------    ----------------------------------------- **         
     H**                                                             **         
     H*****************************************************************         
     FIS925P1   IF   E           K DISK                                         
     FIS928P1   IF   E           K DISK                                         
     FIS921P1   IF   E           K DISK                                         
     FIS922P1   IF   E           K DISK                                         
     FIS923D1   CF   E             WORKSTN INFDS(INFDS)                         
     D*                                                                         
     D  DATISO         S               D   DATFMT(*ISO)                         
     D  DATMDY         S               D   DATFMT(*MDY)                         
     D*                                                                         
     D INFDS           DS                                                       
     D KEY                   369    369                                         
     D PAGRRN                378    379B 0                                      
     D*                                                                         
     D PSDS           SDS                                                       
     D VDUSID                254    263                                         
     D*                                                                         
     D F01             C                   CONST(X'31')                         
     D F03             C                   CONST(X'33')                         
     D F12             C                   CONST(X'3C')                         
     D*                                                                         
     D** Hi Intensity - Reverse Image                                           
     D HIREV1          C                   CONST(X'23')                         
     D*                                                                         
     D DFCODE          S              4  0                                      
     D DFDESC          S             60                                         
     D FLDNME          S              8                                         
     D MNUNME          S             10                                         
     D MNUOP#          S              2  0                                      
     D MNUOPT          S              2                                         
     D MSGD            S             50                                         
     D MSGF            S             10                                         
     D MSGID           S              7                                         
     D SVCOL           S              3  0                                      
     D SVROW           S              3  0                                      
     D WKCDEX          S              4                                         
     D WKSTEX          S              2                                         
     D*                                                                         
     D                 DS                                                       
     D XXED00                  1      4    DIM(4)                               
     D  XXED01                 1      1                                         
     D  XXED02                 2      2                                         
     D  XXED03                 3      3                                         
     D  XXED04                 4      4                                         
     I*                                                                         
     C*****************************************************************         
     C*  MAINLINE                                                     *         
     C*****************************************************************         
     C*                                                                         
     C                   DOU       *IN03 = *ON  OR *IN12 = *ON                  
     C*                                                                         
     C     MAIN01        TAG                                                    
     C                   EXFMT     SCRN01                                       
     C*                                                                         
     C** Remove any error messages                                              
     C                   CALL      'RMVMSGX'                                    
     C                   WRITE     SFLCTLM                                      
     C*                                                                         
     C                   MOVEA     *BLANKS       XXED00                         
     C*                                                                         
     C** F23 Pressed = Mouse Button                                             
     C     *IN23         IFEQ      '1'                                          
     C                   EXSR      S500                                         
     C     GOODMOUSE     CABEQ     'N'           MAIN01                         
     C                   ENDIF                                                  
     C*                                                                         
     C** F3 Pressed = Exit                                                      
     C                   IF        *IN03 = *ON                                  
     C                   MOVE      *ON           *INLR                          
     C                   RETURN                                                 
     C                   ENDIF                                                  
     C*                                                                         
     C** F12 Pressed = Return                                                   
     C                   IF        *IN12 = *ON                                  
     C                   MOVE      *ON           *INLR                          
     C                   RETURN                                                 
     C                   ENDIF                                                  
     C*                                                                         
     C** F1 Pressed = Help                                                      
     C                   IF        *IN01 = *ON                                  
     C                   EXSR      S050                                         
     C                   GOTO      MAIN01                                       
     C                   ENDIF                                                  
     C*                                                                         
     C** Validate fields                                                        
     C                   EXSR      S001                                         
     C     PASSEDIT      CABEQ     'N'           MAIN01                         
     C*                                                                         
     C** Submit job                                                             
     C                   EXSR      S002                                         
     C*                                                                         
     C                   ENDDO                                                  
     C*                                                                         
     C                   MOVE      *ON           *INLR                          
     C                   RETURN                                                 
     C*                                                                         
     C*****************************************************************         
     C*  S001: VALDIDATE FIELDS                                                 
     C*****************************************************************         
     C*                                                                         
     C     S001          BEGSR                                                  
     C*                                                                         
     C                   MOVE      'Y'           PASSEDIT          1            
     C*                                                                         
     C** Validate company number                                                
     C     DFCMPX        CHAIN     R925P1                             95        
     C                   IF        *IN95 = *ON                                  
     C                   Z-ADD     4             VDROW                          
     C                   Z-ADD     18            VDCOL                          
     C                   MOVE      HIREV1        XXED01                         
     C                   MOVE      'N'           PASSEDIT                       
     C                   MOVE      'MSG0007'     MSGID                          
     C                   EXSR      SNDMSGX                                      
     C                   WRITE     SFLCTLM                                      
     C                   GOTO      E001                                         
     C                   ENDIF                                                  
     C*                                                                         
     C** Validate employee number                                               
     C                   MOVE      *BLANKS       DFNMEX                         
     C     EMKY01        CHAIN     R928P1                             95        
     C                   IF        *IN95 = *ON                                  
     C                   Z-ADD     6             VDROW                          
     C                   Z-ADD     18            VDCOL                          
     C                   MOVE      HIREV1        XXED02                         
     C                   MOVE      'N'           PASSEDIT                       
     C                   MOVE      'MSG0013'     MSGID                          
     C                   EXSR      SNDMSGX                                      
     C                   WRITE     SFLCTLM                                      
     C                   GOTO      E001                                         
     C                   ENDIF                                                  
     C                   MOVEL     EMPUNQ        WKEMPX            5 0          
     C                   MOVEL     EMPNME        DFNMEX                         
     C*                                                                         
     C** Validate from date                                                     
     C                   IF        DFBEGX = 0                                   
     C                   Z-ADD     8             VDROW                          
     C                   Z-ADD     18            VDCOL                          
     C                   MOVE      HIREV1        XXED03                         
     C                   MOVE      'MSG0020'     MSGID                          
     C                   EXSR      SNDMSGX                                      
     C                   WRITE     SFLCTLM                                      
     C                   GOTO      MAIN01                                       
     C                   ENDIF                                                  
     C*                                                                         
     C                   IF        DFBEGX <> 0                                  
     C     *MDY          TEST(D)                 DFBEGX                 95      
     C     *IN95         IFEQ      '1'                                          
     C                   Z-ADD     8             VDROW                          
     C                   Z-ADD     18            VDCOL                          
     C                   MOVE      HIREV1        XXED03                         
     C                   MOVE      'MSG0021'     MSGID                          
     C                   EXSR      SNDMSGX                                      
     C                   WRITE     SFLCTLM                                      
     C                   GOTO      MAIN01                                       
     C                   ENDIF                                                  
     C                   MOVE      DFBEGX        DATMDY                         
     C                   MOVE      DATMDY        DATISO                         
     C                   MOVE      DATISO        BEGDTE            8 0          
     C                   ENDIF                                                  
     C*                                                                         
     C** Validate thru date                                                     
     C                   IF        DFENDX = 0                                   
     C                   Z-ADD     10            VDROW                          
     C                   Z-ADD     18            VDCOL                          
     C                   MOVE      HIREV1        XXED04                         
     C                   MOVE      'MSG0020'     MSGID                          
     C                   EXSR      SNDMSGX                                      
     C                   WRITE     SFLCTLM                                      
     C                   GOTO      MAIN01                                       
     C                   ENDIF                                                  
     C*                                                                         
     C                   IF        DFENDX <> 0                                  
     C     *MDY          TEST(D)                 DFENDX                 95      
     C     *IN95         IFEQ      '1'                                          
     C                   Z-ADD     10            VDROW                          
     C                   Z-ADD     18            VDCOL                          
     C                   MOVE      HIREV1        XXED04                         
     C                   MOVE      'MSG0021'     MSGID                          
     C                   EXSR      SNDMSGX                                      
     C                   WRITE     SFLCTLM                                      
     C                   GOTO      MAIN01                                       
     C                   ENDIF                                                  
     C                   MOVE      DFENDX        DATMDY                         
     C                   MOVE      DATMDY        DATISO                         
     C                   MOVE      DATISO        ENDDTE            8 0          
     C                   ENDIF                                                  
     C*                                                                         
     C     E001          ENDSR                                                  
     C*                                                                         
     C*****************************************************************         
     C*  S002: SUBMIT JOB                                                       
     C*****************************************************************         
     C*                                                                         
     C     S002          BEGSR                                                  
     C*                                                                         
     C                   MOVE      DFCMPX        OUCMPX            5            
     C                   MOVE      WKEMPX        OUEMPX            5            
     C                   MOVE      BEGDTE        OUBEGX            8            
     C                   MOVE      ENDDTE        OUENDX            8            
     C*                                                                         
     C                   CALL      'IS923C1'                                    
     C                   PARM                    OUCMPX                         
     C                   PARM                    OUEMPX                         
     C                   PARM                    OUBEGX                         
     C                   PARM                    OUENDX                         
     C*                                                                         
     C                   MOVE      'MSG0024'     MSGID                          
     C                   EXSR      SNDMSGX                                      
     C                   WRITE     SFLCTLM                                      
     C*                                                                         
     C     E002          ENDSR                                                  
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
     C                   MOVEL     'IS923D1'     MNUNME                         
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
     C*   S500: INTERPRET MOUSE BUTTON                               *          
     C*****************************************************************         
     C*                                                                         
     C     S500          BEGSR                                                  
     C*                                                                         
     C                   MOVE      'Y'           GOODMOUSE         1            
     C*                                                                         
     C** Command key                                                            
     C     ROW           IFEQ      23                                           
     C     COL           IFGE      2                                            
     C     COL           ANDLE     8                                            
     C                   EVAL      KEY = F01                                    
     C                   EVAL      *IN01 = '1'                                  
     C                   GOTO      E500                                         
     C                   ENDIF                                                  
     C     COL           IFGE      11                                           
     C     COL           ANDLE     17                                           
     C                   EVAL      KEY = F03                                    
     C                   EVAL      *IN03 = '1'                                  
     C                   GOTO      E500                                         
     C                   ENDIF                                                  
     C                   ENDIF                                                  
     C*                                                                         
     C                   MOVE      'N'           GOODMOUSE                      
     C*                                                                         
     C     E500          ENDSR                                                  
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
     C*****************************************************************         
     C* INIT *     INITIALIZATION SUBROUTINE                          *         
     C*****************************************************************         
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
     C                   KFLD                    DFNBRX                         
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
     C                   Z-ADD     0             DFCMPX                         
     C                   Z-ADD     0             DFNBRX                         
     C                   Z-ADD     0             DFBEGX                         
     C                   Z-ADD     0             DFENDX                         
     C*                                                                         
     C     INIT99        ENDSR                                                  
     C*                                                                         
     C*****************************************************************         
