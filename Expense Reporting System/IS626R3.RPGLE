     H*****************************************************************         
     H* FastBreed Technologies, Inc. © 2000                           *         
     H*****************************************************************         
     H*****************************************************************         
     H**                                                             **         
     H** OBJECT ID: IS626R3                                          **         
     H**                                                             **         
     H** OBJECT DESCRIPTION: MAINTAIN PROJECT MASTER - CHG MODE      **         
     H**                                                             **         
     H** MOD#    DATE      DESCRIPTION                               **         
     H** ----  --------    ----------------------------------------- **         
     H**                                                             **         
     H*****************************************************************         
     F*                                                                         
     FIS921P1   IF   E           K DISK                                         
     FIS922P1   IF   E           K DISK                                         
     FIS625P1   IF   E           K DISK                                         
     FIS626L1   UF   E           K DISK                                         
     FIS626P1   IF   E           K DISK    RENAME(R626P1:CKFILE) PREFIX(CK_)    
     FIS626D3   CF   E             WORKSTN INFDS(INFDS)                         
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
     C                   EXFMT     DSP01                                        
     C*                                                                         
     C** Remove any error messages                                              
     C                   CALL      'RMVMSGX'                                    
     C                   WRITE     SFLCTLM                                      
     C*                                                                         
     C                   MOVEA     *BLANKS       XXED00                         
     C                   Z-ADD     8             VDROW                          
     C                   Z-ADD     28            VDCOL                          
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
     C                   MOVE      DFUNQX        WKUNQX                         
     C                   MOVEL     DFNMEX        WKNMEX                         
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
     C** Validate project name                                                  
     C                   IF        DFNMEX = *BLANKS                             
     C                   Z-ADD     8             VDROW                          
     C                   Z-ADD     28            VDCOL                          
     C                   MOVE      HIREV1        XXED01                         
     C                   MOVE      'N'           PASSEDIT                       
     C                   MOVE      'MSG0026'     MSGID                          
     C                   EXSR      SNDMSGX                                      
     C                   WRITE     SFLCTLM                                      
     C                   GOTO      E001                                         
     C                   ENDIF                                                  
     C*                                                                         
     C     PRKY02        CHAIN     CKFILE                             95        
     C     *IN95         IFEQ      *OFF                                         
     C                   Z-ADD     8             VDROW                          
     C                   Z-ADD     28            VDCOL                          
     C                   MOVE      HIREV1        XXED01                         
     C                   MOVE      'N'           PASSEDIT                       
     C                   MOVE      'MSG0028'     MSGID                          
     C                   EXSR      SNDMSGX                                      
     C                   WRITE     SFLCTLM                                      
     C                   GOTO      E001                                         
     C                   ENDIF                                                  
     C*                                                                         
     C** Validate active flag                                                   
     C                   IF        DFDSPX <> 'Y' AND DFDSPX <> 'N'              
     C                   Z-ADD     14            VDROW                          
     C                   Z-ADD     28            VDCOL                          
     C                   MOVE      HIREV1        XXED04                         
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
     C** Save current time                                                      
     C                   TIME                    SVTIME            6 0          
     C                   IF        SVTIME < 120000                              
     C                   MOVE      'A'           SVAMPM            1            
     C                   ELSE                                                   
     C                   MOVE      'P'           SVAMPM                         
     C                   ENDIF                                                  
     C*                                                                         
     C     PRKY01        CHAIN     R626P1                             99        
     C     *IN99         IFEQ      '0'                                          
     C                   MOVEL     DFNMEX        PRJNME                         
     C                   MOVE      DFDEMX        PRJDEM                         
     C                   MOVE      DFFRMX        PRJFRM                         
     C                   MOVE      DFDSPX        PRJDSP                         
     C                   MOVEL     VDUSID        PRJUBY                         
     C                   MOVE      TODAY         PRJUDT                         
     C                   MOVE      SVTIME        PRJUTM                         
     C                   MOVE      SVAMPM        PRJUAM                         
     C                   UPDATE    R626P1                                       
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
     C                   MOVEL     'IS626D3'     MNUNME                         
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
     C                   PARM                    WKNMEX           40            
     C                   PARM                    WKF03             1            
     C*                                                                         
     C     PLIST1        PLIST                                                  
     C                   PARM                    MNUNME                         
     C                   PARM                    MNUOPT                         
     C                   PARM                    FLDNME                         
     C*                                                                         
     C     PRKY01        KLIST                                                  
     C                   KFLD                    DFCMPX                         
     C                   KFLD                    DFUNQX                         
     C*                                                                         
     C     PRKY02        KLIST                                                  
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
     C** Get company name                                                       
     C                   MOVE      WKCMPX        DFCMPX                         
     C     DFCMPX        CHAIN     R625P1                             95        
     C                   IF        *IN95 = *OFF                                 
     C                   MOVEL     CMPNME        DFCMPN                         
     C                   ENDIF                                                  
     C*                                                                         
     C** Get project master record                                              
     C                   MOVE      WKUNQX        DFUNQX            5 0          
     C     PRKY01        CHAIN     R626P1                             99        
     C     *IN99         IFEQ      '1'                                          
     C                   MOVE      '1'           *INLR                          
     C                   RETURN                                                 
     C                   ENDIF                                                  
     C*                                                                         
     C                   MOVEL     PRJNME        DFNMEX                         
     C                   MOVE      PRJDEM        DFDEMX                         
     C                   MOVE      PRJFRM        DFFRMX                         
     C                   MOVE      PRJDSP        DFDSPX                         
     C*                                                                         
     C** Block access to fields if in inquiry mode                              
     C     INQ25         IFEQ      '1'                                          
     C                   MOVE      NOACCS        XXED01                         
     C                   MOVE      NOACCS        XXED02                         
     C                   MOVE      NOACCS        XXED03                         
     C                   MOVE      NOACCS        XXED04                         
     C                   ELSE                                                   
     C                   MOVE      *BLANKS       XXED00                         
     C                   ENDIF                                                  
     C*                                                                         
     C     *DATE         MULT      10000.0001    TODAY             8 0          
     C*                                                                         
     C     INIT99        ENDSR                                                  
     C*                                                                         
