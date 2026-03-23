     H*****************************************************************         
     H* FastBreed Technologies, Inc. © 2000                           *         
     H*****************************************************************         
     H*****************************************************************         
     H**                                                             **         
     H** OBJECT ID: IS927R5                                          **         
     H**                                                             **         
     H** OBJECT DESCRIPTION: MAINTAIN COST CENTERS - ADD MODE        **         
     H**                                                             **         
     H** MOD#    DATE      DESCRIPTION                               **         
     H** ----  --------    ----------------------------------------- **         
     H**                                                             **         
     H*****************************************************************         
     FIS921P1   IF   E           K DISK                                         
     FIS922P1   IF   E           K DISK                                         
     FIS925P1   IF   E           K DISK                                         
     FIS927P5   UF A E           K DISK                                         
     FIS927P4   IF A E           K DISK                                         
     FIS927L4   IF   E           K DISK    RENAME(R927P4:CKCST) PREFIX(CS_)     
     FIS927D5   CF   E             WORKSTN INFDS(INFDS)                         
     D*                                                                         
     D DFCODE          S              4  0                                      
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
     D XXED00                  1      3    DIM(3)                               
     D  XXED01                 1      1                                         
     D  XXED02                 2      2                                         
     D  XXED03                 3      3                                         
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
     C                   Z-ADD     6             VDROW                          
     C                   Z-ADD     28            VDCOL                          
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
     C** Get next unique identifier                                             
     C     DFUNQX        IFEQ      *ZEROS                                       
     C     DFCMPX        CHAIN     R927P5                             95        
     C                   IF        *IN95 = *OFF                                 
     C                   Z-ADD     NXTUNQ        DFUNQX                         
     C                   EXSR      S004                                         
     C                   UPDATE    R927P5                                       
     C                   ELSE                                                   
     C                   Z-ADD     1             DFUNQX                         
     C                   EXSR      S004                                         
     C                   WRITE     R927P5                                       
     C                   ENDIF                                                  
     C                   ENDIF                                                  
     C*                                                                         
     C** Write record                                                           
     C                   EXSR      S002                                         
     C*                                                                         
     C** Initialize fields                                                      
     C                   EXSR      S003                                         
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
     C** Validate cost center number                                            
     C                   IF        DFUNQX <> 0                                  
     C     CSKY02        CHAIN     CKCST                              95        
     C     *IN95         IFEQ      *OFF                                         
     C                   Z-ADD     6             VDROW                          
     C                   Z-ADD     28            VDCOL                          
     C                   MOVE      HIREV1        XXED03                         
     C                   MOVE      'N'           PASSEDIT                       
     C                   MOVE      'MSG0029'     MSGID                          
     C                   EXSR      SNDMSGX                                      
     C                   WRITE     SFLCTLM                                      
     C                   GOTO      E001                                         
     C                   ENDIF                                                  
     C                   ENDIF                                                  
     C*                                                                         
     C** Validate cost center name                                              
     C                   IF        DFNMEX = *BLANKS                             
     C                   Z-ADD     8             VDROW                          
     C                   Z-ADD     28            VDCOL                          
     C                   MOVE      HIREV1        XXED01                         
     C                   MOVE      'N'           PASSEDIT                       
     C                   MOVE      'MSG0027'     MSGID                          
     C                   EXSR      SNDMSGX                                      
     C                   WRITE     SFLCTLM                                      
     C                   GOTO      E001                                         
     C                   ENDIF                                                  
     C*                                                                         
     C     CSKY01        CHAIN     R927P4                             95        
     C     *IN95         IFEQ      *OFF                                         
     C                   Z-ADD     8             VDROW                          
     C                   Z-ADD     28            VDCOL                          
     C                   MOVE      HIREV1        XXED01                         
     C                   MOVE      'N'           PASSEDIT                       
     C                   MOVE      'MSG0029'     MSGID                          
     C                   EXSR      SNDMSGX                                      
     C                   WRITE     SFLCTLM                                      
     C                   GOTO      E001                                         
     C                   ENDIF                                                  
     C*                                                                         
     C** Validate active flag                                                   
     C                   IF        DFDSPX <> 'Y' AND DFDSPX <> 'N'              
     C                   Z-ADD     8             VDROW                          
     C                   Z-ADD     28            VDCOL                          
     C                   MOVE      HIREV1        XXED02                         
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
     C*  S002: WRITE RECORDS                                                    
     C*****************************************************************         
     C*                                                                         
     C     S002          BEGSR                                                  
     C*                                                                         
     C                   MOVEL     DFCMPX        CMPUNQ                         
     C                   MOVE      DFUNQX        CSTUNQ                         
     C                   MOVEL     DFNMEX        CSTNME                         
     C                   MOVE      DFDSPX        CSTDSP                         
     C*                                                                         
     C                   WRITE     R927P4                                       
     C*                                                                         
     C     E002          ENDSR                                                  
     C*                                                                         
     C*****************************************************************         
     C*  S003: INITIALIZE FIELDS                                                
     C*****************************************************************         
     C*                                                                         
     C     S003          BEGSR                                                  
     C*                                                                         
     C                   Z-ADD     0             CSTUNQ                         
     C                   MOVE      *ZEROS        DFUNQX                         
     C                   MOVE      *BLANKS       DFNMEX                         
     C                   MOVE      'Y'           DFDSPX                         
     C*                                                                         
     C     E003          ENDSR                                                  
     C*                                                                         
     C*****************************************************************         
     C*  S004: GET NEXT AVAILABLE COST CENTER NUMBER                            
     C*****************************************************************         
     C*                                                                         
     C     S004          BEGSR                                                  
     C*                                                                         
     C     CSKY02        SETLL     CKCST                                        
     C     CSKY02        READE     CKCST                                  99    
     C     *IN99         DOWEQ     *OFF                                         
     C                   ADD       1             DFUNQX                         
     C     CSKY02        READE     CKCST                                  99    
     C                   ENDDO                                                  
     C*                                                                         
     C                   Z-ADD     DFUNQX        NXTUNQ                         
     C*                                                                         
     C     E004          ENDSR                                                  
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
     C                   MOVEL     'IS927D5'     MNUNME                         
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
     C                   PARM                    WKCMPX            5            
     C                   PARM                    WKUNQX            5            
     C                   PARM                    WKF03             1            
     C*                                                                         
     C     PLIST1        PLIST                                                  
     C                   PARM                    MNUNME                         
     C                   PARM                    MNUOPT                         
     C                   PARM                    FLDNME                         
     C*                                                                         
     C     CSKY01        KLIST                                                  
     C                   KFLD                    DFCMPX                         
     C                   KFLD                    DFNMEX                         
     C*                                                                         
     C     CSKY02        KLIST                                                  
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
     C                   Z-ADD     1             WKKEY1            1 0          
     C*                                                                         
     C                   MOVE      'Y'           DFDSPX                         
     C*                                                                         
     C** Get company name                                                       
     C                   MOVE      WKCMPX        DFCMPX                         
     C     DFCMPX        CHAIN     R925P1                             95        
     C                   IF        *IN95 = *OFF                                 
     C                   MOVEL     CMPNME        DFCMPN                         
     C                   ENDIF                                                  
     C*                                                                         
     C     INIT99        ENDSR                                                  
     C*                                                                         
