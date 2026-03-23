     H*****************************************************************         
     H* FastBreed Technologies, Inc. © 2000                           *         
     H*****************************************************************         
     H*****************************************************************         
     H**                                                             **         
     H** OBJECT ID: IS625R3                                          **         
     H**                                                             **         
     H** OBJECT DESCRIPTION: MAINTAIN COMPANY MASTER - CHG MODE      **         
     H**                                                             **         
     H** MOD#    DATE      DESCRIPTION                               **         
     H** ----  --------    ----------------------------------------- **         
     H**                                                             **         
     H*****************************************************************         
     FIS625P1   UF   E           K DISK                                         
     FIS625L1   IF   E           K DISK    RENAME(R625P1:LF1) PREFIX(L1_)       
     FIS921P1   IF   E           K DISK                                         
     FIS922P1   IF   E           K DISK                                         
     FIS625D3   CF   E             WORKSTN INFDS(INFDS)                         
     D*                                                                         
*0002D                 DS                                                       
*0002D  DSDT3X                 1     70                                         
*0002D    DFSTRX               1      3                                         
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
     D** Protect field                                                          
     D NOACCS          C                   CONST(X'A0')                         
     D*                                                                         
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
     D                 DS                                                       
     D XXED00                  1     22    DIM(22)                              
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
     D  XXED11                11     11                                         
     D  XXED12                12     12                                         
     D  XXED13                13     13                                         
     D  XXED14                14     14                                         
     D  XXED15                15     15                                         
     D  XXED16                16     16                                         
     D  XXED17                17     17                                         
     D  XXED18                18     18                                         
     D  XXED19                19     19                                         
     D  XXED20                20     20                                         
     D  XXED21                21     21                                         
     D  XXED22                22     22                                         
     I*                                                                         
     C*****************************************************************         
     C*  MAINLINE                                                     *         
     C*****************************************************************         
     C*                                                                         
     C                   DOU       *IN03 = *ON  OR *IN12 = *ON                  
     C*                                                                         
     C     MAIN01        TAG                                                    
     C                   EXFMT     DSP01                                        
     C*                                                                         
     C** Remove any error messages                                              
     C                   CALL      'RMVMSGX'                                    
     C                   WRITE     SFLCTLM                                      
     C*                                                                         
     C                   MOVEA     *BLANKS       XXED00                         
     C                   Z-ADD     4             VDROW                          
     C                   Z-ADD     16            VDCOL                          
     C*                                                                         
     C** F23 Pressed = Mouse Button                                             
     C     *IN23         IFEQ      '1'                                          
     C                   EXSR      S500                                         
     C     GOODMOUSE     CABEQ     'N'           MAIN01                         
     C                   ENDIF                                                  
     C*                                                                         
     C** F3 Pressed = Exit                                                      
     C                   IF        *IN03 = *ON                                  
     C                   MOVE      'Y'           WKF03                          
     C                   MOVE      *ON           *INLR                          
     C                   RETURN                                                 
     C                   ENDIF                                                  
     C*                                                                         
     C** F12 Pressed = Return                                                   
     C                   IF        *IN12 = *ON                                  
     C                   MOVE      'N'           WKF03                          
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
     C** Update record                                                          
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
     C** Validate company name                                                  
     C                   IF        DFNMEX = *BLANKS                             
     C                   Z-ADD     4             VDROW                          
     C                   Z-ADD     16            VDCOL                          
     C                   MOVE      HIREV1        XXED01                         
     C                   MOVE      'N'           PASSEDIT                       
     C                   MOVE      'MSG0003'     MSGID                          
     C                   EXSR      SNDMSGX                                      
     C                   WRITE     SFLCTLM                                      
     C                   GOTO      E001                                         
     C                   ENDIF                                                  
     C*                                                                         
     C     DFNMEX        CHAIN     LF1                                95        
     C                   IF        *IN95 = *OFF AND DFUNQX <> L1_CMPUNQ         
     C                   Z-ADD     4             VDROW                          
     C                   Z-ADD     10            VDCOL                          
     C                   MOVE      HIREV1        XXED01                         
     C                   MOVE      'N'           PASSEDIT                       
     C                   MOVE      'MSG0004'     MSGID                          
     C                   EXSR      SNDMSGX                                      
     C                   WRITE     SFLCTLM                                      
     C                   GOTO      E001                                         
     C                   ENDIF                                                  
     C*                                                                         
     C** Validate per diem firm flag                                            
     C                   IF        DFFRMX <> 'Y' AND DFFRMX <> 'N'              
     C                   Z-ADD     18            VDROW                          
     C                   Z-ADD     65            VDCOL                          
     C                   MOVE      HIREV1        XXED02                         
     C                   MOVE      'N'           PASSEDIT                       
     C                   MOVE      'MSG0005'     MSGID                          
     C                   EXSR      SNDMSGX                                      
     C                   WRITE     SFLCTLM                                      
     C                   GOTO      E001                                         
     C                   ENDIF                                                  
     C*                                                                         
     C** Validate project numbers required flag                                 
     C                   IF        DFRQ1X <> 'Y' AND DFRQ1X <> 'N'              
     C                   Z-ADD     19            VDROW                          
     C                   Z-ADD     34            VDCOL                          
     C                   MOVE      HIREV1        XXED03                         
     C                   MOVE      'N'           PASSEDIT                       
     C                   MOVE      'MSG0005'     MSGID                          
     C                   EXSR      SNDMSGX                                      
     C                   WRITE     SFLCTLM                                      
     C                   GOTO      E001                                         
     C                   ENDIF                                                  
     C*                                                                         
     C** Validate cost centers required flag                                    
     C                   IF        DFRQ2X <> 'Y' AND DFRQ2X <> 'N'              
     C                   Z-ADD     19            VDROW                          
     C                   Z-ADD     65            VDCOL                          
     C                   MOVE      HIREV1        XXED04                         
     C                   MOVE      'N'           PASSEDIT                       
     C                   MOVE      'MSG0005'     MSGID                          
     C                   EXSR      SNDMSGX                                      
     C                   WRITE     SFLCTLM                                      
     C                   GOTO      E001                                         
     C                   ENDIF                                                  
     C*                                                                         
     C** Validate week start day field                                          
     C                   IF        DFSTRX <> 'SUN' AND                          
     C                             DFSTRX <> 'MON' AND                          
     C                             DFSTRX <> 'TUE' AND                          
     C                             DFSTRX <> 'WED' AND                          
     C                             DFSTRX <> 'THU' AND                          
     C                             DFSTRX <> 'FRI' AND                          
     C                             DFSTRX <> 'SAT'                              
     C                   Z-ADD     19            VDROW                          
     C                   Z-ADD     78            VDCOL                          
     C                   MOVE      HIREV1        XXED22                         
     C                   MOVE      'N'           PASSEDIT                       
     C                   MOVE      'MSG0025'     MSGID                          
     C                   EXSR      SNDMSGX                                      
     C                   WRITE     SFLCTLM                                      
     C                   GOTO      E001                                         
     C                   ENDIF                                                  
     C*                                                                         
     C     E001          ENDSR                                                  
     C*                                                                         
     C*****************************************************************         
     C*  S002: UPDATE RECORDS                                                   
     C*****************************************************************         
     C*                                                                         
     C     S002          BEGSR                                                  
     C*                                                                         
     C** Save current time                                                      
     C                   TIME                    SVTIME            6 0          
     C                   IF        SVTIME < 120000                              
     C                   MOVE      'A'           SVAMPM            1            
     C                   ELSE                                                   
     C                   MOVE      'P'           SVAMPM                         
     C                   ENDIF                                                  
     C*                                                                         
     C     DFUNQX        CHAIN     R625P1                             99        
     C     *IN99         IFEQ      '0'                                          
     C                   MOVEL     DFNMEX        WKNMEX                         
     C                   MOVEL     DFNMEX        CMPNME                         
     C                   MOVEL     DFAD1X        CMPAD1                         
     C                   MOVEL     DFAD2X        CMPAD2                         
     C                   MOVEL     DFCTYX        CMPCTY                         
     C                   MOVEL     DFSTEX        CMPSTE                         
     C                   MOVE      DFZP5X        CMPZP5                         
     C                   MOVE      DFZP4X        CMPZP4                         
     C                   MOVEL     DFCN1X        CMPCN1                         
     C                   MOVEL     DFPH1X        CMPPH1                         
     C                   MOVEL     DFFX1X        CMPFX1                         
     C                   MOVEL     DFPG1X        CMPPG1                         
     C                   MOVEL     DFCN2X        CMPCN2                         
     C                   MOVEL     DFPH2X        CMPPH2                         
     C                   MOVEL     DFFX2X        CMPFX2                         
     C                   MOVEL     DFPG2X        CMPPG2                         
     C                   MOVEL     DFDT1X        CMPDT1                         
     C                   MOVEL     DFDT2X        CMPDT2                         
     C                   MOVEL     DSDT3X        CMPDT3                         
     C                   MOVE      DFDEMX        CMPDEM                         
     C                   MOVE      DFFRMX        CMPFRM                         
     C                   MOVE      DFRQ1X        CMPRQ1                         
     C                   MOVE      DFRQ2X        CMPRQ2                         
     C                   MOVEL     VDUSID        CMPUBY                         
     C                   MOVE      TODAY         CMPUDT                         
     C                   MOVE      SVTIME        CMPUTM                         
     C                   MOVE      SVAMPM        CMPUAM                         
     C                   UPDATE    R625P1                                       
     C                   ENDIF                                                  
     C*                                                                         
     C     E002          ENDSR                                                  
     C*                                                                         
     C*****************************************************************         
     C*  S050: DETERMINE HELP FIELD NAME                                        
     C*****************************************************************         
     C*                                                                         
     C     S050          BEGSR                                                  
     C*                                                                         
     C                   EVAL      SVROW = ROW                                  
     C                   EVAL      SVCOL = COL                                  
     C*                                                                         
     C                   MOVEL     'IS625D3'     MNUNME                         
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
     C     COL           IFGE      20                                           
     C     COL           ANDLE     29                                           
     C                   EVAL      KEY = F12                                    
     C                   EVAL      *IN12 = '1'                                  
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
     C                   PARM                    WKUNQX            5            
     C                   PARM                    WKNMEX           40            
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
     C** Error message preparation                                              
     C                   MOVEL     '*'           MSGQ                           
     C*                                                                         
     C** Remove any error messages                                              
     C                   CALL      'RMVMSGX'                                    
     C                   WRITE     SFLCTLM                                      
     C*                                                                         
     C                   MOVE      WKUNQX        DFUNQX                         
     C     DFUNQX        CHAIN     R625P1                             99        
     C     *IN99         IFEQ      '1'                                          
     C                   MOVE      '1'           *INLR                          
     C                   RETURN                                                 
     C                   ENDIF                                                  
     C*                                                                         
     C                   MOVEL     CMPNME        DFNMEX                         
     C                   MOVEL     CMPAD1        DFAD1X                         
     C                   MOVEL     CMPAD2        DFAD2X                         
     C                   MOVEL     CMPCTY        DFCTYX                         
     C                   MOVEL     CMPSTE        DFSTEX                         
     C                   MOVE      CMPZP5        DFZP5X                         
     C                   MOVE      CMPZP4        DFZP4X                         
     C                   MOVEL     CMPCN1        DFCN1X                         
     C                   MOVEL     CMPPH1        DFPH1X                         
     C                   MOVEL     CMPFX1        DFFX1X                         
     C                   MOVEL     CMPPG1        DFPG1X                         
     C                   MOVEL     CMPCN2        DFCN2X                         
     C                   MOVEL     CMPPH2        DFPH2X                         
     C                   MOVEL     CMPFX2        DFFX2X                         
     C                   MOVEL     CMPPG2        DFPG2X                         
     C                   MOVEL     CMPDT1        DFDT1X                         
     C                   MOVEL     CMPDT2        DFDT2X                         
     C                   MOVEL     CMPDT3        DSDT3X                         
     C                   MOVE      CMPDEM        DFDEMX                         
     C                   MOVE      CMPFRM        DFFRMX                         
     C                   MOVE      CMPRQ1        DFRQ1X                         
     C                   MOVE      CMPRQ2        DFRQ2X                         
     C*                                                                         
     C** Block access to fields if in inquiry mode                              
     C     INQ25         IFEQ      '1'                                          
     C                   MOVE      NOACCS        XXED01                         
     C                   MOVE      NOACCS        XXED02                         
     C                   MOVE      NOACCS        XXED03                         
     C                   MOVE      NOACCS        XXED04                         
     C                   MOVE      NOACCS        XXED05                         
     C                   MOVE      NOACCS        XXED06                         
     C                   MOVE      NOACCS        XXED07                         
     C                   MOVE      NOACCS        XXED08                         
     C                   MOVE      NOACCS        XXED09                         
     C                   MOVE      NOACCS        XXED10                         
     C                   MOVE      NOACCS        XXED11                         
     C                   MOVE      NOACCS        XXED12                         
     C                   MOVE      NOACCS        XXED13                         
     C                   MOVE      NOACCS        XXED14                         
     C                   MOVE      NOACCS        XXED15                         
     C                   MOVE      NOACCS        XXED16                         
     C                   MOVE      NOACCS        XXED17                         
     C                   MOVE      NOACCS        XXED18                         
     C                   MOVE      NOACCS        XXED19                         
     C                   MOVE      NOACCS        XXED20                         
     C                   MOVE      NOACCS        XXED21                         
     C                   MOVE      NOACCS        XXED22                         
     C                   ELSE                                                   
     C                   MOVE      *BLANKS       XXED00                         
     C                   ENDIF                                                  
     C*                                                                         
     C     *DATE         MULT      10000.0001    TODAY             8 0          
     C*                                                                         
     C     INIT99        ENDSR                                                  
     C*                                                                         
