     H*****************************************************************         
     H* FastBreed Technologies, Inc. © 2000                           *         
     H*****************************************************************         
     H*****************************************************************         
     H**                                                             **         
     H** OBJECT ID: IS926R3                                          **         
     H**                                                             **         
     H** OBJECT DESCRIPTION: MAINTAIN EXPENSE MASTER - CHG MODE      **         
     H**                                                             **         
     H** MOD#    DATE      DESCRIPTION                               **         
     H** ----  --------    ----------------------------------------- **         
     H**                                                             **         
     H*****************************************************************         
     F*                                                                         
     FIS925P1   IF   E           K DISK                                         
     FIS921P1   IF   E           K DISK                                         
     FIS922P1   IF   E           K DISK                                         
     FIS926D3   CF   E             WORKSTN INFDS(INFDS)                         
     FIS926L1   UF   E           K DISK                                         
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
     D XXED00                  1     12    DIM(12)                              
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
     C                   Z-ADD     7             VDROW                          
     C                   Z-ADD     23            VDCOL                          
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
     C                   MOVE      DFCDEX        WKCDEX                         
     C                   MOVEL     DFTYPX        WKTYPX                         
     C                   MOVEL     DFDESX        WKDESX                         
     C                   MOVE      DFMLTX        WKMLTX                         
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
     C** Validate expense code                                                  
     C                   IF        DFCDEX = *ZEROS                              
     C                   Z-ADD     7             VDROW                          
     C                   Z-ADD     23            VDCOL                          
     C                   MOVE      HIREV1        XXED01                         
     C                   MOVE      'N'           PASSEDIT                       
     C                   MOVE      'MSG0008'     MSGID                          
     C                   EXSR      SNDMSGX                                      
     C                   WRITE     SFLCTLM                                      
     C                   GOTO      E001                                         
     C                   ENDIF                                                  
     C*                                                                         
     C** Validate expense type                                                  
     C                   IF        DFTYPX = *BLANKS                             
     C                   Z-ADD     8             VDROW                          
     C                   Z-ADD     23            VDCOL                          
     C                   MOVE      HIREV1        XXED02                         
     C                   MOVE      'N'           PASSEDIT                       
     C                   MOVE      'MSG0009'     MSGID                          
     C                   EXSR      SNDMSGX                                      
     C                   WRITE     SFLCTLM                                      
     C                   GOTO      E001                                         
     C                   ENDIF                                                  
     C*                                                                         
     C** Validate expense multiplier                                            
     C                   IF        DFMLTX <= 0                                  
     C                   Z-ADD     9             VDROW                          
     C                   Z-ADD     23            VDCOL                          
     C                   MOVE      HIREV1        XXED03                         
     C                   MOVE      'N'           PASSEDIT                       
     C                   MOVE      'MSG0010'     MSGID                          
     C                   EXSR      SNDMSGX                                      
     C                   WRITE     SFLCTLM                                      
     C                   GOTO      E001                                         
     C                   ENDIF                                                  
     C*                                                                         
     C** Validate extended to code                                              
     C                   IF        DFLINX = 0                                   
     C                   Z-ADD     DFCDEX        DFLINX                         
     C                   ENDIF                                                  
     C*                                                                         
     C** Validate subtotal field                                                
     C                   Z-ADD     DFLINX        DFSUBX                         
     C                   IF        DFSUBX = 0                                   
     C                   Z-ADD     11            VDROW                          
     C                   Z-ADD     23            VDCOL                          
     C                   MOVE      HIREV1        XXED05                         
     C                   MOVE      'N'           PASSEDIT                       
     C                   MOVE      'MSG0008'     MSGID                          
     C                   EXSR      SNDMSGX                                      
     C                   WRITE     SFLCTLM                                      
     C                   GOTO      E001                                         
     C                   ENDIF                                                  
     C*                                                                         
     C** Validate description field                                             
     C                   IF        DFDESX = *BLANKS                             
     C                   MOVEL     DFTYPX        DFDESX                         
     C                   ENDIF                                                  
     C*                                                                         
     C** Validate details required flag                                         
     C                   IF        DFDETX <> 'Y' AND DFDETX <> 'N'              
     C                   Z-ADD     16            VDROW                          
     C                   Z-ADD     23            VDCOL                          
     C                   MOVE      HIREV1        XXED08                         
     C                   MOVE      'N'           PASSEDIT                       
     C                   MOVE      'MSG0005'     MSGID                          
     C                   EXSR      SNDMSGX                                      
     C                   WRITE     SFLCTLM                                      
     C                   GOTO      E001                                         
     C                   ENDIF                                                  
     C*                                                                         
     C** Validate per diem firm flag                                            
     C                   IF        DFFRMX <> 'Y' AND DFFRMX <> 'N'              
     C                   Z-ADD     17            VDROW                          
     C                   Z-ADD     23            VDCOL                          
     C                   MOVE      HIREV1        XXED09                         
     C                   MOVE      'N'           PASSEDIT                       
     C                   MOVE      'MSG0005'     MSGID                          
     C                   EXSR      SNDMSGX                                      
     C                   WRITE     SFLCTLM                                      
     C                   GOTO      E001                                         
     C                   ENDIF                                                  
     C*                                                                         
     C** Validate advance flag                                                  
     C                   IF        DFADVX <> 'Y' AND DFADVX <> 'N'              
     C                   Z-ADD     19            VDROW                          
     C                   Z-ADD     23            VDCOL                          
     C                   MOVE      HIREV1        XXED11                         
     C                   MOVE      'N'           PASSEDIT                       
     C                   MOVE      'MSG0005'     MSGID                          
     C                   EXSR      SNDMSGX                                      
     C                   WRITE     SFLCTLM                                      
     C                   GOTO      E001                                         
     C                   ENDIF                                                  
     C*                                                                         
     C** Validate active flag                                                   
     C                   IF        DFDSPX <> 'Y' AND DFDSPX <> 'N'              
     C                   Z-ADD     20            VDROW                          
     C                   Z-ADD     23            VDCOL                          
     C                   MOVE      HIREV1        XXED12                         
     C                   MOVE      'N'           PASSEDIT                       
     C                   MOVE      'MSG0005'     MSGID                          
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
     C     EXKY01        CHAIN     R926P1                             99        
     C     *IN99         IFEQ      '0'                                          
     C                   MOVE      DFCDEX        EXPCDE                         
     C                   MOVEL     DFTYPX        EXPTYP                         
     C                   MOVE      DFMLTX        EXPMLT                         
     C                   MOVE      DFLINX        EXPLIN                         
     C                   MOVE      DFSUBX        EXPSUB                         
     C                   MOVEL     DFDESX        EXPDES                         
     C                   MOVEL     DFCATX        EXPCAT                         
     C                   MOVE      DFDETX        EXPDET                         
     C                   MOVE      DFDEMX        EXPDEM                         
     C                   MOVE      DFFRMX        EXPFRM                         
     C                   MOVE      DFADVX        EXPADV                         
     C                   MOVE      DFDSPX        EXPDSP                         
     C                   UPDATE    R926P1                                       
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
     C                   MOVEL     'IS926D3'     MNUNME                         
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
     C                   PARM                    WKUNQX            5            
     C                   PARM                    WKCDEX            3            
     C                   PARM                    WKTYPX           20            
     C                   PARM                    WKDESX           40            
     C                   PARM                    WKMLTX            4            
     C                   PARM                    WKF03             1            
     C*                                                                         
     C     PLIST1        PLIST                                                  
     C                   PARM                    MNUNME                         
     C                   PARM                    MNUOPT                         
     C                   PARM                    FLDNME                         
     C*                                                                         
     C     EXKY01        KLIST                                                  
     C                   KFLD                    DFCMPX                         
     C                   KFLD                    DFUNQX                         
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
     C** Get company name                                                       
     C                   MOVE      WKCMPX        DFCMPX                         
     C     DFCMPX        CHAIN     R925P1                             95        
     C                   IF        *IN95 = *OFF                                 
     C                   MOVEL     CMPNME        DFNMEX                         
     C                   ENDIF                                                  
     C*                                                                         
     C** Get expense type record                                                
     C                   MOVE      WKUNQX        DFUNQX            5 0          
     C     EXKY01        CHAIN     R926P1                             99        
     C     *IN99         IFEQ      '1'                                          
     C                   MOVE      '1'           *INLR                          
     C                   RETURN                                                 
     C                   ENDIF                                                  
     C*                                                                         
     C                   MOVE      EXPCDE        DFCDEX                         
     C                   MOVEL     EXPTYP        DFTYPX                         
     C                   MOVE      EXPMLT        DFMLTX                         
     C                   MOVE      EXPLIN        DFLINX                         
     C                   MOVE      EXPSUB        DFSUBX                         
     C                   MOVEL     EXPDES        DFDESX                         
     C                   MOVEL     EXPCAT        DFCATX                         
     C                   MOVE      EXPDET        DFDETX                         
     C                   MOVE      EXPDEM        DFDEMX                         
     C                   MOVEL     EXPFRM        DFFRMX                         
     C                   MOVE      EXPADV        DFADVX                         
     C                   MOVEL     EXPDSP        DFDSPX                         
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
     C                   ELSE                                                   
     C                   MOVE      *BLANKS       XXED00                         
     C                   ENDIF                                                  
     C*                                                                         
     C     INIT99        ENDSR                                                  
     C*                                                                         
