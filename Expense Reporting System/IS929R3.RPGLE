     H*****************************************************************         
     H* FastBreed Technologies, Inc. © 2000                           *         
     H*****************************************************************         
     H*****************************************************************         
     H**                                                             **         
     H** OBJECT ID: IS929R3                                          **         
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
     FIS927L1   IF   E           K DISK                                         
     FIS927L4   IF   E           K DISK                                         
     FIS928L2   IF   E           K DISK                                         
     FIS929P1   UF   E           K DISK                                         
     FIS921P1   IF   E           K DISK                                         
     FIS922P1   IF   E           K DISK                                         
     FIS929D3   CF   E             WORKSTN INFDS(INFDS)                         
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
     D XXED00                  1      6    DIM(6)                               
     D  XXED01                 1      1                                         
     D  XXED02                 2      2                                         
     D  XXED03                 3      3                                         
     D  XXED04                 4      4                                         
     D  XXED05                 5      5                                         
     D  XXED06                 6      6                                         
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
     C                   Z-ADD     8             VDROW                          
     C                   Z-ADD     17            VDCOL                          
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
     C                   LEAVE                                                  
     C                   ENDIF                                                  
     C*                                                                         
     C** F12 Pressed = Return                                                   
     C                   IF        *IN12 = *ON                                  
     C                   MOVE      'N'           WKF03                          
     C                   LEAVE                                                  
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
     C** Pass changed values back                                               
     C                   MOVE      HDDTEX        WKDTEX                         
     C                   MOVE      DFDTEX        WKDTE1                         
     C                   MOVE      DFCDEX        WKCDE1                         
     C                   MOVEL     DFTYPX        WKTYP1                         
     C                   MOVE      DFAMTX        WKAMT1                         
     C                   MOVE      DFPUNQ        WKPUNQ                         
     C                   MOVE      DFCUNQ        WKCUNQ                         
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
     C** Validate project number                                                
     C                   IF        DFPUNQ <> 0                                  
     C     PRKY01        CHAIN     R927P1                             95        
     C                   IF        *IN95 = *ON OR (*IN95=*OFF AND PRJDSP='N')   
     C                   Z-ADD     8             VDROW                          
     C                   Z-ADD     17            VDCOL                          
     C                   MOVE      HIREV1        XXED01                         
     C                   MOVE      'N'           PASSEDIT                       
     C                   MOVE      'MSG0014'     MSGID                          
     C                   EXSR      SNDMSGX                                      
     C                   WRITE     SFLCTLM                                      
     C                   GOTO      E001                                         
     C                   ENDIF                                                  
     C                   MOVEL     PRJNME        DFPNME                         
     C                   ENDIF                                                  
     C*                                                                         
     C                   IF        DFPUNQ <> 0                                  
     C                   MOVE      *BLANKS       DFPNME                         
     C                   ENDIF                                                  
     C*                                                                         
     C                   IF        DFPUNQ = 0 AND CMPRQ1 = 'Y'                  
     C                   Z-ADD     8             VDROW                          
     C                   Z-ADD     17            VDCOL                          
     C                   MOVE      HIREV1        XXED01                         
     C                   MOVE      'N'           PASSEDIT                       
     C                   MOVE      'MSG0015'     MSGID                          
     C                   EXSR      SNDMSGX                                      
     C                   WRITE     SFLCTLM                                      
     C                   GOTO      E001                                         
     C                   ENDIF                                                  
     C*                                                                         
     C** Validate cost center                                                   
     C                   IF        DFCUNQ <> 0                                  
     C     CSKY01        CHAIN     R927P4                             95        
     C                   IF        *IN95 = *ON OR (*IN95=*OFF AND CSTDSP='N')   
     C                   Z-ADD     9             VDROW                          
     C                   Z-ADD     17            VDCOL                          
     C                   MOVE      HIREV1        XXED02                         
     C                   MOVE      'N'           PASSEDIT                       
     C                   MOVE      'MSG0016'     MSGID                          
     C                   EXSR      SNDMSGX                                      
     C                   WRITE     SFLCTLM                                      
     C                   GOTO      E001                                         
     C                   ENDIF                                                  
     C                   MOVEL     CSTNME        DFCNME                         
     C                   ENDIF                                                  
     C*                                                                         
     C                   IF        DFCUNQ <> 0                                  
     C                   MOVE      *BLANKS       DFCNME                         
     C                   ENDIF                                                  
     C*                                                                         
     C                   IF        DFCUNQ = 0 AND CMPRQ2 = 'Y'                  
     C                   Z-ADD     9             VDROW                          
     C                   Z-ADD     17            VDCOL                          
     C                   MOVE      HIREV1        XXED02                         
     C                   MOVE      'N'           PASSEDIT                       
     C                   MOVE      'MSG0017'     MSGID                          
     C                   EXSR      SNDMSGX                                      
     C                   WRITE     SFLCTLM                                      
     C                   GOTO      E001                                         
     C                   ENDIF                                                  
     C*                                                                         
     C** Validate expense code                                                  
     C                   IF        DFCDEX <= 0                                  
     C                   Z-ADD     10            VDROW                          
     C                   Z-ADD     17            VDCOL                          
     C                   MOVE      HIREV1        XXED03                         
     C                   MOVE      'N'           PASSEDIT                       
     C                   MOVE      'MSG0018'     MSGID                          
     C                   EXSR      SNDMSGX                                      
     C                   WRITE     SFLCTLM                                      
     C                   GOTO      E001                                         
     C                   ENDIF                                                  
     C*                                                                         
     C     EXKY02        CHAIN     R926P1                             95        
     C                   IF        *IN95 = *ON OR (*IN95=*OFF AND EXPDSP='N')   
     C                   Z-ADD     10            VDROW                          
     C                   Z-ADD     17            VDCOL                          
     C                   MOVE      HIREV1        XXED03                         
     C                   MOVE      'N'           PASSEDIT                       
     C                   MOVE      'MSG0022'     MSGID                          
     C                   EXSR      SNDMSGX                                      
     C                   WRITE     SFLCTLM                                      
     C                   GOTO      E001                                         
     C                   ENDIF                                                  
     C                   MOVEL     EXPTYP        DFTYPX                         
     C                   MOVE      EXPUNQ        SVEXPUNQ          5 0          
     C                   MOVE      EXPMLT        SVEXPMLT          4 2          
     C                   MOVE      EXPADV        SVEXPADV          1            
     C*                                                                         
     C** Validate date                                                          
     C                   IF        DFDTEX = 0                                   
     C                   Z-ADD     13            VDROW                          
     C                   Z-ADD     17            VDCOL                          
     C                   MOVE      HIREV1        XXED04                         
     C                   MOVE      'N'           PASSEDIT                       
     C                   MOVE      'MSG0020'     MSGID                          
     C                   EXSR      SNDMSGX                                      
     C                   WRITE     SFLCTLM                                      
     C                   GOTO      E001                                         
     C                   ENDIF                                                  
     C*                                                                         
     C                   IF        DFDTEX <> 0                                  
     C     *MDY          TEST(D)                 DFDTEX                 95      
     C     *IN95         IFEQ      '1'                                          
     C                   Z-ADD     13            VDROW                          
     C                   Z-ADD     17            VDCOL                          
     C                   MOVE      HIREV1        XXED04                         
     C                   MOVE      'N'           PASSEDIT                       
     C                   MOVE      'MSG0021'     MSGID                          
     C                   EXSR      SNDMSGX                                      
     C                   WRITE     SFLCTLM                                      
     C                   GOTO      E001                                         
     C                   ENDIF                                                  
     C                   ENDIF                                                  
     C*                                                                         
     C** Validate amount                                                        
     C                   IF        DFAMTX <= 0                                  
     C                   Z-ADD     14            VDROW                          
     C                   Z-ADD     17            VDCOL                          
     C                   MOVE      HIREV1        XXED05                         
     C                   MOVE      'N'           PASSEDIT                       
     C                   MOVE      'MSG0019'     MSGID                          
     C                   EXSR      SNDMSGX                                      
     C                   WRITE     SFLCTLM                                      
     C                   GOTO      E001                                         
     C                   ENDIF                                                  
     C*                                                                         
     C** Validate details                                                       
     C                   IF        DFDETX =*BLANKS  AND EXPDET = 'Y'            
     C                   Z-ADD     17            VDROW                          
     C                   Z-ADD     17            VDCOL                          
     C                   MOVE      HIREV1        XXED06                         
     C                   MOVE      'N'           PASSEDIT                       
     C                   MOVE      'MSG0023'     MSGID                          
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
     C     TRKY02        CHAIN     R929P1                             99        
     C     *IN99         IFEQ      '0'                                          
     C                   Z-ADD     DFPUNQ        PRJUNQ                         
     C                   MOVEL     DFPNME        PRJNME                         
     C                   Z-ADD     DFCUNQ        CSTUNQ                         
     C                   MOVEL     DFCNME        CSTNME                         
     C                   Z-ADD     DFCDEX        EXPCDE                         
     C                   MOVEL     DFTYPX        EXPTYP                         
     C                   Z-ADD     SVEXPUNQ      EXPUNQ                         
     C                   Z-ADD     SVEXPMLT      EXPMLT                         
     C                   MOVE      SVEXPADV      EXPADV                         
     C                   MOVE      DFDTEX        DATMDY                         
     C                   MOVE      DATMDY        DATISO                         
     C                   MOVE      DATISO        TRNDTE                         
     C                   MOVE      DATISO        HDDTEX                         
     C                   Z-ADD     DFAMTX        TRNAMT                         
     C                   MOVEL     DFLOC1        TRNLC1                         
     C                   MOVEL     DFLOC2        TRNLC2                         
     C                   MOVEL     DFDETX        TRNDET                         
     C                   UPDATE    R929P1                                       
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
     C                   MOVEL     'IS929D3'     MNUNME                         
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
     C                   PARM                    WKCMPX            5            
     C                   PARM                    WKEMPX            5            
     C                   PARM                    WKDTEX            8            
     C                   PARM                    WKUNQX            5            
     C                   PARM                    WKDTE1            6            
     C                   PARM                    WKCDE1            3            
     C                   PARM                    WKTYP1           20            
     C                   PARM                    WKAMT1            8            
     C                   PARM                    WKPUNQ            5            
     C                   PARM                    WKCUNQ            5            
     C                   PARM                    WKF03             1            
     C*                                                                         
     C     PLIST1        PLIST                                                  
     C                   PARM                    MNUNME                         
     C                   PARM                    MNUOPT                         
     C                   PARM                    FLDNME                         
     C*                                                                         
     C     CSKY01        KLIST                                                  
     C                   KFLD                    DFCMPX                         
     C                   KFLD                    DFCUNQ                         
     C*                                                                         
     C     EMKY01        KLIST                                                  
     C                   KFLD                    DFCMPX                         
     C                   KFLD                    DFEMPX                         
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
     C                   KFLD                    DFPUNQ                         
     C*                                                                         
     C     TRKY01        KLIST                                                  
     C                   KFLD                    DFCMPX                         
     C                   KFLD                    DFEMPX                         
     C                   KFLD                    TRNDTE                         
     C                   KFLD                    TRNUNQ                         
     C*                                                                         
     C     TRKY02        KLIST                                                  
     C                   KFLD                    DFCMPX                         
     C                   KFLD                    DFEMPX                         
     C                   KFLD                    HDDTEX                         
     C                   KFLD                    TRNUNQ                         
     C*                                                                         
     C** Error message preparation                                              
     C                   MOVEL     '*'           MSGQ                           
     C*                                                                         
     C** Remove any error messages                                              
     C                   CALL      'RMVMSGX'                                    
     C                   WRITE     SFLCTLM                                      
     C*                                                                         
     C                   MOVE      WKCMPX        DFCMPX                         
     C                   MOVE      WKEMPX        DFEMPX            5 0          
     C                   MOVE      WKDTEX        TRNDTE                         
     C                   MOVE      WKUNQX        TRNUNQ                         
     C*                                                                         
     C** Get company name                                                       
     C     DFCMPX        CHAIN     R925P1                             95        
     C     *IN95         IFEQ      '0'                                          
     C                   MOVEL     CMPNME        DFCMPN                         
     C                   ENDIF                                                  
     C*                                                                         
     C** Get employee record                                                    
     C     EMKY01        CHAIN     R928P1                             95        
     C     *IN95         IFEQ      '0'                                          
     C                   MOVE      EMPNBR        DFNBRX                         
     C                   MOVEL     EMPNME        DFENME                         
     C                   ENDIF                                                  
     C*                                                                         
     C** Get expense record                                                     
     C     TRKY01        CHAIN     R929P1                             95        
     C     *IN95         IFEQ      '1'                                          
     C                   MOVE      '1'           *INLR                          
     C                   RETURN                                                 
     C                   ENDIF                                                  
     C*                                                                         
     C                   MOVE      PRJUNQ        DFPUNQ                         
     C                   MOVEL     PRJNME        DFPNME                         
     C                   MOVE      CSTUNQ        DFCUNQ                         
     C                   MOVEL     CSTNME        DFCNME                         
     C                   MOVE      EXPCDE        DFCDEX                         
     C                   MOVEL     EXPTYP        DFTYPX                         
     C                   MOVE      WKDTE1        DFDTEX                         
     C                   MOVE      TRNDTE        HDDTEX                         
     C                   MOVE      TRNAMT        DFAMTX                         
     C                   MOVEL     TRNLC1        DFLOC1                         
     C                   MOVEL     TRNLC2        DFLOC2                         
     C                   MOVEL     TRNDET        DFDETX                         
     C*                                                                         
     C** Block access to fields if in inquiry mode                              
     C     INQ25         IFEQ      '1'                                          
     C                   MOVE      NOACCS        XXED01                         
     C                   MOVE      NOACCS        XXED02                         
     C                   MOVE      NOACCS        XXED03                         
     C                   MOVE      NOACCS        XXED04                         
     C                   MOVE      NOACCS        XXED05                         
     C                   MOVE      NOACCS        XXED06                         
     C                   ELSE                                                   
     C                   MOVE      *BLANKS       XXED00                         
     C                   ENDIF                                                  
     C*                                                                         
     C     INIT99        ENDSR                                                  
     C*                                                                         
