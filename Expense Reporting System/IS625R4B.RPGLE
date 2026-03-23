     H*****************************************************************         
     H* FastBreed Technologies, Inc. © 2000                           *         
     H*****************************************************************         
     H*****************************************************************         
     H**                                                             **         
     H** OBJECT ID: IS625R4B                                         **         
     H**                                                             **         
     H** OBJECT DESCRIPTION: DEFINE USER ACCESS TO COMPANIES - ADD   **         
     H**                                                             **         
     H** MOD#    DATE      DESCRIPTION                               **         
     H** ----  --------    ----------------------------------------- **         
     H**                                                             **         
     H*****************************************************************         
     FIS921P1   IF   E           K DISK                                         
     FIS922P1   IF   E           K DISK                                         
     FIS625P1   IF   E           K DISK                                         
     FIS625P3   IF A E           K DISK                                         
     FIS625D4B  CF   E             WORKSTN INFDS(INFDS)                         
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
     D XXED00                  1      2    DIM(2)                               
     D  XXED01                 1      1                                         
     D  XXED02                 2      2                                         
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
     C*                                                                         
     C** Position cursor                                                        
     C     DFUNQX        IFEQ      *ZEROS                                       
     C                   Z-ADD     4             VDROW                          
     C                   Z-ADD     19            VDCOL                          
     C                   ELSE                                                   
     C                   Z-ADD     6             VDROW                          
     C                   Z-ADD     19            VDCOL                          
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
     C** Write record                                                           
     C                   EXSR      S002                                         
     C*                                                                         
     C** Initialize fields                                                      
     C                   MOVE      *BLANKS       DFUSRX                         
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
     C                   IF        DFUNQX = *ZEROS                              
     C                   Z-ADD     4             VDROW                          
     C                   Z-ADD     19            VDCOL                          
     C                   MOVE      HIREV1        XXED01                         
     C                   MOVE      'N'           PASSEDIT                       
     C                   MOVE      'MSG0061'     MSGID                          
     C                   EXSR      SNDMSGX                                      
     C                   WRITE     SFLCTLM                                      
     C                   GOTO      E001                                         
     C                   ENDIF                                                  
     C*                                                                         
     C     DFUNQX        CHAIN     R625P1                             95        
     C                   IF        *IN95 = *ON                                  
     C                   Z-ADD     4             VDROW                          
     C                   Z-ADD     19            VDCOL                          
     C                   MOVE      HIREV1        XXED01                         
     C                   MOVE      'N'           PASSEDIT                       
     C                   MOVE      'MSG0007'     MSGID                          
     C                   EXSR      SNDMSGX                                      
     C                   WRITE     SFLCTLM                                      
     C                   GOTO      E001                                         
     C                   ENDIF                                                  
     C*                                                                         
     C** Make sure person doing the adding has authority to the company         
     C** they're adding users to (unless user is CMPINZUSR)                     
     C                   IF        VDUSID <> 'CMPINZUSR'                        
     C     CMKY01        CHAIN(N)  R625P3                             95        
     C     *IN95         IFEQ      *ON                                          
     C                   Z-ADD     4             VDROW                          
     C                   Z-ADD     19            VDCOL                          
     C                   MOVE      HIREV1        XXED01                         
     C                   MOVE      'N'           PASSEDIT                       
     C                   MOVE      'MSG0220'     MSGID                          
     C                   EXSR      SNDMSGX                                      
     C                   WRITE     SFLCTLM                                      
     C                   GOTO      E001                                         
     C                   ENDIF                                                  
     C                   ENDIF                                                  
     C*                                                                         
     C** Validate user profile                                                  
     C                   IF        DFUSRX = '          '                        
     C                   Z-ADD     6             VDROW                          
     C                   Z-ADD     19            VDCOL                          
     C                   MOVE      HIREV1        XXED02                         
     C                   MOVE      'N'           PASSEDIT                       
     C                   MOVE      'MSG0216'     MSGID                          
     C                   EXSR      SNDMSGX                                      
     C                   WRITE     SFLCTLM                                      
     C                   GOTO      E001                                         
     C                   ENDIF                                                  
     C*                                                                         
     C** Make sure user profile is a valid one                                  
     C                   MOVE      'N'           OUERRX            1            
     C                   CALL      'IS946C2'                                    
     C                   PARM                    DFUSRX                         
     C                   PARM                    OUERRX                         
     C                   IF        OUERRX = 'Y'                                 
     C                   Z-ADD     6             VDROW                          
     C                   Z-ADD     19            VDCOL                          
     C                   MOVE      HIREV1        XXED02                         
     C                   MOVE      'N'           PASSEDIT                       
     C                   MOVE      'MSG0217'     MSGID                          
     C                   EXSR      SNDMSGX                                      
     C                   WRITE     SFLCTLM                                      
     C                   GOTO      E001                                         
     C                   ENDIF                                                  
     C*                                                                         
     C** Make sure user doesn't already have access to company                  
     C     CMKY02        CHAIN     R625P3                             95        
     C     *IN95         IFEQ      *OFF                                         
     C                   Z-ADD     4             VDROW                          
     C                   Z-ADD     19            VDCOL                          
     C                   MOVE      HIREV1        XXED01                         
     C                   MOVE      HIREV1        XXED02                         
     C                   MOVE      'N'           PASSEDIT                       
     C                   MOVE      'MSG0219'     MSGID                          
     C                   EXSR      SNDMSGX                                      
     C                   WRITE     SFLCTLM                                      
     C                   GOTO      E001                                         
     C                   ENDIF                                                  
     C*                                                                         
     C     E001          ENDSR                                                  
     C*                                                                         
     C*****************************************************************         
     C*  S002: WRITE RECORDS                                                    
     C*****************************************************************         
     C*                                                                         
     C     S002          BEGSR                                                  
     C*                                                                         
     C                   Z-ADD     DFUNQX        CMPUNQ                         
     C                   MOVEL     DFUSRX        ACCUSR                         
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
     C                   WRITE     R625P3                                       
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
     C                   MOVEL     'IS625D4B'    MNUNME                         
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
     C                   PARM                    WKUSRX           10            
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
     C     CMKY01        KLIST                                                  
     C                   KFLD                    DFUNQX                         
     C                   KFLD                    VDUSID                         
     C*                                                                         
     C     CMKY02        KLIST                                                  
     C                   KFLD                    DFUNQX                         
     C                   KFLD                    DFUSRX                         
     C*                                                                         
     C** Error message preparation                                              
     C                   MOVEL     '*'           MSGQ                           
     C*                                                                         
     C** Remove any error messages                                              
     C                   CALL      'RMVMSGX'                                    
     C                   WRITE     SFLCTLM                                      
     C*                                                                         
     C** Initialize fields                                                      
     C                   MOVE      *ZEROS        DFUNQX                         
     C                   MOVE      *BLANKS       DFUSRX                         
     C                   Z-ADD     4             VDROW                          
     C                   Z-ADD     19            VDCOL                          
     C*                                                                         
     C     INIT99        ENDSR                                                  
     C*                                                                         
