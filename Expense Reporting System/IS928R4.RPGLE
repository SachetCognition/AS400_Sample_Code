     H*****************************************************************         
     H* FastBreed Technologies, Inc. © 2000                           *         
     H*****************************************************************         
     H*****************************************************************         
     H**                                                             **         
     H** OBJECT ID: IS928R2                                          **         
     H**                                                             **         
     H** OBJECT DESCRIPTION: MAINTAIN EMPLOYEE MASTER - LOAD FILE    **         
     H**                                                             **         
     H** MOD#    DATE      DESCRIPTION                               **         
     H** ----  --------    ----------------------------------------- **         
     H**                                                             **         
     H*****************************************************************         
     FIS921P1   IF   E           K DISK                                         
     FIS922P1   IF   E           K DISK                                         
     FIS925P1   IF   E           K DISK                                         
     FIS928P3   UF A E           K DISK                                         
     FIS928D4   CF   E             WORKSTN INFDS(INFDS)                         
     D*                                                                         
     D DATISO          S               D   DATFMT(*ISO)                         
     D DATMDY          S               D   DATFMT(*MDY)                         
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
     D WKGOOD          S              1                                         
     D*                                                                         
     D                 DS                                                       
     D XXED00                  1      6    DIM(6)                               
     D  XXED01                 1      1                                         
     D  XXED02                 2      2                                         
     D  XXED03                 3      3                                         
     D  XXED04                 4      4                                         
     D  XXED05                 5      5                                         
     D  XXED06                 6      6                                         
     D*                                                                         
     D F01             C                   CONST(X'31')                         
     D F03             C                   CONST(X'33')                         
     D F12             C                   CONST(X'3C')                         
     D*                                                                         
     D** Hi Intensity - Reverse Image                                           
     D HIREV1          C                   CONST(X'23')                         
     D*                                                                         
     D INFDS           DS                                                       
     D KEY                   369    369                                         
     D PAGRRN                378    379B 0                                      
     D*                                                                         
     D PSDS           SDS                                                       
     D  PGMNAM           *PROC                                                  
     D  VDUSID               254    263                                         
     I*                                                                         
     C*****************************************************************         
     C*  MAINLINE                                                     *         
     C*****************************************************************         
     C*                                                                         
     C** Display prompt screen                                                  
     C     MAIN10        TAG                                                    
     C                   EXFMT     DSP01                                        
     C*                                                                         
     C** Remove any error messages                                              
     C                   CALL      'RMVMSGX'                                    
     C                   WRITE     SFLCTLM                                      
     C*                                                                         
     C                   MOVEA     *BLANKS       XXED00                         
     C                   Z-ADD     4             VDROW                          
     C                   Z-ADD     35            VDCOL                          
     C*                                                                         
     C** F3 Pressed = Exit                                                      
     C                   IF        *IN03 = *ON                                  
     C                   MOVE      *ON           *INLR                          
     C                   RETURN                                                 
     C                   ENDIF                                                  
     C*                                                                         
     C** F1 Pressed = Help                                                      
     C                   IF        *IN01 = *ON                                  
     C                   EXSR      S050                                         
     C                   GOTO      MAIN10                                       
     C                   ENDIF                                                  
     C*                                                                         
     C** Load existing record if it exists                                      
     C                   IF        ROW = 4 AND COL = 39                         
     C     DFCMPX        CHAIN     R928P3                             95        
     C                   IF        *IN95 = *OFF                                 
     C                   MOVEL     CNVFIL        DFFILX                         
     C                   MOVEL     CNVMBR        DFMBRX                         
     C                   MOVEL     CNVNBR        DFNBRX                         
     C                   MOVEL     CNVNME        DFNMEX                         
     C                   MOVEL     CNVNML        DFNMEL                         
     C                   MOVEL     CNVNMF        DFNMEF                         
     C                   MOVEL     CNVDPT        DFDPTX                         
     C                   MOVEL     CNVPOS        DFPOSX                         
     C                   MOVEL     CNVACT        DFACTX                         
     C                   MOVEL     CNVVAL        DFVALX                         
     C                   Z-ADD     6             VDROW                          
     C                   Z-ADD     35            VDCOL                          
     C                   GOTO      MAIN10                                       
     C                   ENDIF                                                  
     C                   ENDIF                                                  
     C*                                                                         
     C** Validate fields                                                        
     C                   EXSR      S001                                         
     C     PASSEDIT      CABEQ     'N'           MAIN10                         
     C*                                                                         
     C** Write file definition record                                           
     C                   EXSR      S002                                         
     C*                                                                         
     C** Load file                                                              
     C                   MOVE      DFCMPX        WKCMPX            5            
     C                   CALL      'IS928C5'                                    
     C                   PARM                    WKCMPX                         
     C                   PARM                    DFFILX                         
     C                   PARM                    DFMBRX                         
     C*                                                                         
     C** End Program                                                            
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
     C                   IF        DFCMPX = *ZEROS                              
     C                   Z-ADD     4             VDROW                          
     C                   Z-ADD     35            VDCOL                          
     C                   MOVE      HIREV1        XXED01                         
     C                   MOVE      'N'           PASSEDIT                       
     C                   MOVE      'MSG0007'     MSGID                          
     C                   EXSR      SNDMSGX                                      
     C                   WRITE     SFLCTLM                                      
     C                   GOTO      E001                                         
     C                   ENDIF                                                  
     C*                                                                         
     C     DFCMPX        CHAIN     R925P1                             95        
     C                   IF        *IN95 = *ON                                  
     C                   Z-ADD     4             VDROW                          
     C                   Z-ADD     35            VDCOL                          
     C                   MOVE      HIREV1        XXED01                         
     C                   MOVE      'N'           PASSEDIT                       
     C                   MOVE      'MSG0007'     MSGID                          
     C                   EXSR      SNDMSGX                                      
     C                   WRITE     SFLCTLM                                      
     C                   GOTO      E001                                         
     C                   ENDIF                                                  
     C*                                                                         
     C** Validate file name                                                     
     C                   IF        DFFILX = *BLANKS                             
     C                   Z-ADD     6             VDROW                          
     C                   Z-ADD     35            VDCOL                          
     C                   MOVE      HIREV1        XXED02                         
     C                   MOVE      'N'           PASSEDIT                       
     C                   MOVE      'MSG0034'     MSGID                          
     C                   EXSR      SNDMSGX                                      
     C                   WRITE     SFLCTLM                                      
     C                   GOTO      E001                                         
     C                   ENDIF                                                  
     C*                                                                         
     C                   IF        DFMBRX = *BLANKS                             
     C                   MOVEL     '*FIRST'      DFMBRX                         
     C                   ENDIF                                                  
     C*                                                                         
     C                   MOVE      'Y'           WKGOOD                         
     C                   CALL      'IS928C4'                                    
     C                   PARM                    DFFILX                         
     C                   PARM                    DFMBRX                         
     C                   PARM                    WKGOOD                         
     C*                                                                         
     C                   IF        WKGOOD = 'N'                                 
     C                   Z-ADD     6             VDROW                          
     C                   Z-ADD     35            VDCOL                          
     C                   MOVE      HIREV1        XXED02                         
     C                   MOVE      'N'           PASSEDIT                       
     C                   MOVE      'MSG0036'     MSGID                          
     C                   EXSR      SNDMSGX                                      
     C                   WRITE     SFLCTLM                                      
     C                   GOTO      E001                                         
     C                   ENDIF                                                  
     C*                                                                         
     C** Validate field name - employee number                                  
     C                   IF        DFNBRX = *BLANKS                             
     C                   Z-ADD     8             VDROW                          
     C                   Z-ADD     35            VDCOL                          
     C                   MOVE      HIREV1        XXED03                         
     C                   MOVE      'N'           PASSEDIT                       
     C                   MOVE      'MSG0034'     MSGID                          
     C                   EXSR      SNDMSGX                                      
     C                   WRITE     SFLCTLM                                      
     C                   GOTO      E001                                         
     C                   ENDIF                                                  
     C*                                                                         
     C** Validate field name - employee name                                    
     C                   IF        DFNMEX = *BLANKS AND                         
     C                             DFNMEL = *BLANKS                             
     C                   Z-ADD     10            VDROW                          
     C                   Z-ADD     35            VDCOL                          
     C                   MOVE      HIREV1        XXED04                         
     C                   MOVE      'N'           PASSEDIT                       
     C                   MOVE      'MSG0034'     MSGID                          
     C                   EXSR      SNDMSGX                                      
     C                   WRITE     SFLCTLM                                      
     C                   GOTO      E001                                         
     C                   ENDIF                                                  
     C*                                                                         
     C** Validate field name - active flag                                      
     C                   IF        DFACTX = *BLANKS AND DFVALX <> *BLANKS       
     C                   Z-ADD     16            VDROW                          
     C                   Z-ADD     35            VDCOL                          
     C                   MOVE      HIREV1        XXED05                         
     C                   MOVE      'N'           PASSEDIT                       
     C                   MOVE      'MSG0034'     MSGID                          
     C                   EXSR      SNDMSGX                                      
     C                   WRITE     SFLCTLM                                      
     C                   GOTO      E001                                         
     C                   ENDIF                                                  
     C*                                                                         
     C** Validate field value - active flag                                     
     C                   IF        DFACTX <> *BLANKS AND DFVALX = *BLANKS       
     C                   Z-ADD     18            VDROW                          
     C                   Z-ADD     35            VDCOL                          
     C                   MOVE      HIREV1        XXED06                         
     C                   MOVE      'N'           PASSEDIT                       
     C                   MOVE      'MSG0034'     MSGID                          
     C                   EXSR      SNDMSGX                                      
     C                   WRITE     SFLCTLM                                      
     C                   GOTO      E001                                         
     C                   ENDIF                                                  
     C*                                                                         
     C     E001          ENDSR                                                  
     C*                                                                         
     C*****************************************************************         
     C*  S002: WRITE CONVERSION RECORD                                          
     C*****************************************************************         
     C*                                                                         
     C     S002          BEGSR                                                  
     C*                                                                         
     C     DFCMPX        CHAIN     R928P3                             95        
     C*                                                                         
     C                   Z-ADD     DFCMPX        CMPUNQ                         
     C                   MOVEL     DFFILX        CNVFIL                         
     C                   MOVEL     DFMBRX        CNVMBR                         
     C                   MOVEL     DFNBRX        CNVNBR                         
     C                   MOVEL     DFNMEX        CNVNME                         
     C                   MOVEL     DFNMEL        CNVNML                         
     C                   MOVEL     DFNMEF        CNVNMF                         
     C                   MOVEL     DFDPTX        CNVDPT                         
     C                   MOVEL     DFPOSX        CNVPOS                         
     C                   MOVEL     DFACTX        CNVACT                         
     C                   MOVEL     DFVALX        CNVVAL                         
     C*                                                                         
     C                   IF        *IN95 = *OFF                                 
     C                   UPDATE    R928P3                                       
     C                   ELSE                                                   
     C                   WRITE     R928P3                                       
     C                   ENDIF                                                  
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
     C                   MOVEL     'IS928D4'     MNUNME                         
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
     C*************************************************************             
     C*  INZSR: INITIALIZATION SUBROUTINE                                       
     C*************************************************************             
     C*                                                                         
     C     *INZSR        BEGSR                                                  
     C*                                                                         
     C     *ENTRY        PLIST                                                  
     C                   PARM                    WKCMPX            5            
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
     C                   MOVE      WKCMPX        DFCMPX            5 0          
     C                   MOVEL     '*FIRST'      DFMBRX                         
     C*                                                                         
     C** Load existing record if it exists                                      
     C     DFCMPX        CHAIN     R928P3                             95        
     C                   IF        *IN95 = *OFF                                 
     C                   MOVEL     CNVFIL        DFFILX                         
     C                   MOVEL     CNVMBR        DFMBRX                         
     C                   MOVEL     CNVNBR        DFNBRX                         
     C                   MOVEL     CNVNME        DFNMEX                         
     C                   MOVEL     CNVNML        DFNMEL                         
     C                   MOVEL     CNVNMF        DFNMEF                         
     C                   MOVEL     CNVDPT        DFDPTX                         
     C                   MOVEL     CNVPOS        DFPOSX                         
     C                   MOVEL     CNVACT        DFACTX                         
     C                   MOVEL     CNVVAL        DFVALX                         
     C                   ENDIF                                                  
     C*                                                                         
     C                   ENDSR                                                  
     C*                                                                         
     C*****************************************************************         
     C*****************************************************************         
