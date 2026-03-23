     H*****************************************************************         
     H* FastBreed Technologies, Inc. © 2000                           *         
     H*****************************************************************         
     H*****************************************************************         
     H**                                                             **         
     H** OBJECT ID: IS625R0                                          **         
     H**                                                             **         
     H** OBJECT DESCRIPTION: SYSTEM SETUP                            **         
     H**                                                             **         
     H** MOD#    DATE      DESCRIPTION                               **         
     H** ----  --------    ----------------------------------------- **         
     H**                                                             **         
     H*****************************************************************         
     F*                                                                         
     FIS922P1   IF   E           K DISK                                         
     FIS921P1   IF   E           K DISK                                         
     FIS625D0   CF   E             WORKSTN INFDS(INFDS)                         
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
     D*                                                                         
     D INFDS           DS                                                       
     D KEY                   369    369                                         
     D PAGRRN                378    379B 0                                      
     D*                                                                         
     D #DMSG           C                   CONST('* Record deleted')            
     D*                                                                         
     D F01             C                   CONST(X'31')                         
     D F03             C                   CONST(X'33')                         
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
HEREXC** Mainline logic.                                                        
     C                   DOU       KEY = F03 OR KEY = F12                       
     C*                                                                         
     C                   EXFMT     DSP01                                        
     C*                                                                         
     C** Remove any error messages                                              
     C                   CALL      'RMVMSGX'                                    
     C                   WRITE     SFLCTLM                                      
     C*                                                                         
     C** Initialize fields and indicators                                       
     C                   MOVE      *ON           *IN28                          
     C                   MOVE      *BLANKS       XXED01                         
     C                   MOVE      *BLANKS       XXED02                         
     C                   MOVE      *BLANKS       XXED03                         
     C                   MOVE      *BLANKS       XXED04                         
     C                   MOVE      *BLANKS       XXED05                         
     C                   MOVE      *BLANKS       XXED06                         
     C                   MOVE      *BLANKS       XXED07                         
     C                   MOVE      *BLANKS       XXED08                         
     C                   Z-ADD     4             VDROW                          
     C                   Z-ADD     38            VDCOL                          
     C*                                                                         
     C** F3 Pressed = Exit                                                      
     C                   IF        KEY = F03                                    
     C                   LEAVE                                                  
     C                   ENDIF                                                  
     C*                                                                         
     C** F12 Pressed = Return                                                   
     C                   IF        KEY = F12                                    
     C                   LEAVE                                                  
     C                   ENDIF                                                  
     C*                                                                         
     C** F1 Pressed = Help                                                      
     C                   IF        *IN01 = *ON                                  
     C                   MOVEL     '01'          MNUOPT                         
     C                   EXSR      S050                                         
     C                   ITER                                                   
     C                   ENDIF                                                  
     C*                                                                         
     C** Validate Selections                                                    
     C                   EXSR      S010                                         
     C                   IF        PASSEDIT = 'N'                               
     C                   ITER                                                   
     C                   ENDIF                                                  
     C*                                                                         
     C** Process Selections                                                     
     C                   EXSR      S011                                         
     C*                                                                         
     C** Reset Selection Flags                                                  
     C                   MOVE      'N'           DFSEL1                         
     C                   MOVE      'N'           DFSEL2                         
     C                   MOVE      'N'           DFSEL3                         
     C                   MOVE      'N'           DFSEL4                         
     C                   MOVE      'N'           DFSEL5                         
     C                   MOVE      'N'           DFSEL6                         
     C                   MOVE      'N'           DFSEL7                         
     C                   MOVE      'N'           DFSEL8                         
     C*                                                                         
     C                   ENDDO                                                  
     C*                                                                         
     C                   MOVE      *ON           *INLR                          
     C                   RETURN                                                 
     C*                                                                         
     C****************************************************************          
     C*   S010: VALIDATE SELECTION FLAGS                             *          
     C****************************************************************          
     C*                                                                         
     C     S010          BEGSR                                                  
     C*                                                                         
     C                   MOVE      'Y'           PASSEDIT          1            
     C*                                                                         
     C** Validate Define User Access to Menu                                    
     C                   IF        DFSEL1 <> 'Y' AND DFSEL1 <> 'N'              
     C                   Z-ADD     4             VDROW                          
     C                   Z-ADD     38            VDCOL                          
     C                   MOVE      HIREV1        XXED01                         
     C                   MOVE      'N'           PASSEDIT                       
     C                   MOVE      'MSG0005'     MSGID                          
     C                   EXSR      SNDMSGX                                      
     C                   WRITE     SFLCTLM                                      
     C                   GOTO      E010                                         
     C                   ENDIF                                                  
     C*                                                                         
     C** Validate Define Companies Flag                                         
     C                   IF        DFSEL2 <> 'Y' AND DFSEL2 <> 'N'              
     C                   Z-ADD     6             VDROW                          
     C                   Z-ADD     38            VDCOL                          
     C                   MOVE      HIREV1        XXED02                         
     C                   MOVE      'N'           PASSEDIT                       
     C                   MOVE      'MSG0005'     MSGID                          
     C                   EXSR      SNDMSGX                                      
     C                   WRITE     SFLCTLM                                      
     C                   GOTO      E010                                         
     C                   ENDIF                                                  
     C*                                                                         
     C** Validate Define User Access to Companies                               
     C                   IF        DFSEL3 <> 'Y' AND DFSEL3 <> 'N'              
     C                   Z-ADD     7             VDROW                          
     C                   Z-ADD     38            VDCOL                          
     C                   MOVE      HIREV1        XXED03                         
     C                   MOVE      'N'           PASSEDIT                       
     C                   MOVE      'MSG0005'     MSGID                          
     C                   EXSR      SNDMSGX                                      
     C                   WRITE     SFLCTLM                                      
     C                   GOTO      E010                                         
     C                   ENDIF                                                  
     C*                                                                         
     C** Validate Define Cost Centers Flag                                      
     C                   IF        DFSEL4 <> 'Y' AND DFSEL4 <> 'N'              
     C                   Z-ADD     9             VDROW                          
     C                   Z-ADD     38            VDCOL                          
     C                   MOVE      HIREV1        XXED04                         
     C                   MOVE      'N'           PASSEDIT                       
     C                   MOVE      'MSG0005'     MSGID                          
     C                   EXSR      SNDMSGX                                      
     C                   WRITE     SFLCTLM                                      
     C                   GOTO      E010                                         
     C                   ENDIF                                                  
     C*                                                                         
     C** Validate Define User Access to Cost Centers                            
     C                   IF        DFSEL5 <> 'Y' AND DFSEL5 <> 'N'              
     C                   Z-ADD     10            VDROW                          
     C                   Z-ADD     38            VDCOL                          
     C                   MOVE      HIREV1        XXED05                         
     C                   MOVE      'N'           PASSEDIT                       
     C                   MOVE      'MSG0005'     MSGID                          
     C                   EXSR      SNDMSGX                                      
     C                   WRITE     SFLCTLM                                      
     C                   GOTO      E010                                         
     C                   ENDIF                                                  
     C*                                                                         
     C** Validate Define Projects Flag                                          
     C                   IF        DFSEL6 <> 'Y' AND DFSEL6 <> 'N'              
     C                   Z-ADD     12            VDROW                          
     C                   Z-ADD     38            VDCOL                          
     C                   MOVE      HIREV1        XXED06                         
     C                   MOVE      'N'           PASSEDIT                       
     C                   MOVE      'MSG0005'     MSGID                          
     C                   EXSR      SNDMSGX                                      
     C                   WRITE     SFLCTLM                                      
     C                   GOTO      E010                                         
     C                   ENDIF                                                  
     C*                                                                         
     C** Validate Define User Access to Projects                                
     C                   IF        DFSEL7 <> 'Y' AND DFSEL7 <> 'N'              
     C                   Z-ADD     13            VDROW                          
     C                   Z-ADD     38            VDCOL                          
     C                   MOVE      HIREV1        XXED07                         
     C                   MOVE      'N'           PASSEDIT                       
     C                   MOVE      'MSG0005'     MSGID                          
     C                   EXSR      SNDMSGX                                      
     C                   WRITE     SFLCTLM                                      
     C                   GOTO      E010                                         
     C                   ENDIF                                                  
     C*                                                                         
     C** Validate Define Expense Codes Flag                                     
     C                   IF        DFSEL8 <> 'Y' AND DFSEL8 <> 'N'              
     C                   Z-ADD     15            VDROW                          
     C                   Z-ADD     38            VDCOL                          
     C                   MOVE      HIREV1        XXED08                         
     C                   MOVE      'N'           PASSEDIT                       
     C                   MOVE      'MSG0005'     MSGID                          
     C                   EXSR      SNDMSGX                                      
     C                   WRITE     SFLCTLM                                      
     C                   GOTO      E010                                         
     C                   ENDIF                                                  
     C*                                                                         
     C     E010          ENDSR                                                  
     C*                                                                         
     C****************************************************************          
     C*   S011: PROCESS SELECTIONS                                   *          
     C****************************************************************          
     C*                                                                         
     C     S011          BEGSR                                                  
     C*                                                                         
     C                   MOVE      'N'           WKF03                          
     C*                                                                         
     C** Define User Access to Menu                                             
     C                   IF        DFSEL1 = 'Y'                                 
     C                   CALL      'IS625R3A'                                   
     C                   PARM                    INQ25                          
     C                   ENDIF                                                  
     C*                                                                         
     C** Define Companies                                                       
     C                   IF        DFSEL2 = 'Y'                                 
     C                   CALL      'IS625R1'                                    
     C                   PARM                    INQ25                          
     C                   ENDIF                                                  
     C*                                                                         
     C** Define User Access to Companies                                        
     C                   IF        DFSEL3 = 'Y'                                 
     C                   CALL      'IS625R4A'                                   
     C                   PARM                    INQ25                          
     C                   ENDIF                                                  
     C*                                                                         
     C** Define Cost Centers                                                    
     C                   IF        DFSEL4 = 'Y'                                 
     C                   CALL      'IS627R1'                                    
     C                   PARM                    INQ25                          
     C                   ENDIF                                                  
     C*                                                                         
     C** Define Projects                                                        
     C                   IF        DFSEL6 = 'Y'                                 
     C                   CALL      'IS626R1'                                    
     C                   PARM                    INQ25                          
     C                   ENDIF                                                  
     C*                                                                         
     C     E011          ENDSR                                                  
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
     C                   MOVEL     'IS625D0'     MNUNME                         
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
     C                   MOVE      'N'           WKF03             1            
     C                   MOVE      'N'           DFSEL1                         
     C                   MOVE      'N'           DFSEL2                         
     C                   MOVE      'N'           DFSEL3                         
     C                   MOVE      'N'           DFSEL4                         
     C                   MOVE      'N'           DFSEL5                         
     C                   MOVE      'N'           DFSEL6                         
     C                   MOVE      'N'           DFSEL7                         
     C                   MOVE      'N'           DFSEL8                         
     C*                                                                         
     C     *DATE         MULT      10000.0001    TODAY             8 0          
     C*                                                                         
     C                   ENDSR                                                  
     C*                                                                         
     C****************************************************************          
     C****************************************************************          
