     H*****************************************************************         
     H* FastBreed Technologies, Inc. © 2000                           *         
     H*****************************************************************         
     H*****************************************************************         
     H**                                                             **         
     H** OBJECT ID: EXPENSER                                         **         
     H**                                                             **         
     H** OBJECT DESCRIPTION: EXPENSE REPORT SYSTEM                   **         
     H**                                                             **         
     H*****************************************************************         
     F*                                                                         
     FIS625P4   IF   E           K DISK                                         
     FEXPENSEP  IF   E           K DISK                                         
     FEXPENSED  CF   E             WORKSTN INFDS(INFDS)                         
     F                                     SFILE(EXP000S:CRRN)                  
     D MDN             S              1    DIM(30)                              
     D SCN1            S              1    DIM(10)                              
     D*                                                                         
     D INFDS           DS                                                       
     D  KEY                  369    369                                         
     D  PAGRRN               378    379B 0                                      
     D*                                                                         
     D PSDS           SDS                                                       
     D  PGMNAM           *PROC                                                  
     D  USERID               254    263                                         
     D*                                                                         
     D F03             C                   CONST(X'33')                         
     D ENTER           C                   CONST(X'F1')                         
     D PAGEDN          C                   CONST(X'F5')                         
     I*                                                                         
     C*****************************************************************         
     C*  MAINLINE                                                     *         
     C*****************************************************************         
     C*                                                                         
     C** Position file at beginning for initial read.                           
     C     WKLST1        SETLL     IMENU                                        
     C*                                                                         
     C** Mainline logic.                                                        
     C     KEY           DOUEQ     F03                                          
     C*                                                                         
     C** Fill subfile.                                                          
     C                   EXSR      S001                                         
     C*                                                                         
     C** Display subfile.                                                       
     C     KEY           DOUEQ     F03                                          
     C                   WRITE     CSFLCMD                                      
     C*                                                                         
     C** Check for existing subfile records to condition SFLDSP.                
     C     CRRN          IFGT      0                                            
     C                   MOVE      *ON           *IN42                          
     C                   ELSE                                                   
     C                   MOVE      *OFF          *IN42                          
     C                   END                                                    
     C*                                                                         
     C                   MOVE      *ON           *IN41                          
     C                   EXFMT     EXP000C                                      
     C                   MOVE      *OFF          *IN41                          
     C*                                                                         
     C** Remove any error messages                                              
     C                   CALL      'RMVMSGX'                                    
     C                   WRITE     SFLCTLM                                      
     C                   MOVE      *ON           *IN28                          
     C*                                                                         
     C** F3 Pressed = Exit                                                      
     C     KEY           IFEQ      F03                                          
     C                   LEAVE                                                  
     C                   ENDIF                                                  
     C*                                                                         
     C** Option number entered                                                  
     C     DFOPTX        IFNE      0                                            
     C*                                                                         
     C** Skip option if user is not authorized to it                            
     C     MMKY02        CHAIN     R625P4                             95        
     C     *IN95         IFEQ      *ON                                          
     C                   EXSR      S002                                         
     C                   LEAVE                                                  
     C                   ENDIF                                                  
     C*                                                                         
     C     DFOPTX        CHAIN     EXP000S                            95        
     C     *IN95         IFEQ      *OFF                                         
     C                   EXSR      S003                                         
     C                   ENDIF                                                  
     C                   EXSR      S002                                         
     C                   LEAVE                                                  
     C                   ENDIF                                                  
     C*                                                                         
     C** Init SFLRCDNBR value with PAGRRN value from I/O feedback area - the    
     C** lowest subfile RRN on the current subfile display.  Ensures that       
     C** proper page is redisplayed.                                            
     C                   Z-ADD     PAGRRN        RCDNBR                         
     C*                                                                         
     C** Page down for more records request.                                    
     C     KEY           IFEQ      PAGEDN                                       
     C                   LEAVE                                                  
     C                   ENDIF                                                  
     C*                                                                         
     C** Enter with no subfile records displayed.                               
     C     KEY           IFEQ      ENTER                                        
     C     COUNT         ANDEQ     0                                            
     C                   EXSR      S002                                         
     C                   LEAVE                                                  
     C                   ENDIF                                                  
     C*                                                                         
     C                   READC     EXP000S                                98    
     C     *IN98         DOWEQ     *OFF                                         
     C     SELIO         IFNE      ' '                                          
     C                   EXSR      S003                                         
     C                   END                                                    
     C                   READC     EXP000S                                98    
     C                   ENDDO                                                  
     C*                                                                         
     C                   EXSR      S002                                         
     C                   EXSR      S001                                         
     C*                                                                         
     C                   ENDDO                                                  
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
     C                   Z-ADD     0             COUNT             2 0          
     C     COUNT         DOUGT     1400                                         
     C     T001          TAG                                                    
     C                   READ      IMENU                                  99    
     C*                                                                         
     C     *IN99         IFEQ      *ON                                          
     C                   LEAVE                                                  
     C                   ENDIF                                                  
     C*                                                                         
     C** Skip options the user is not authorized to                             
     C     MMKY01        CHAIN     R625P4                             95        
     C     *IN95         CABEQ     *ON           T001                           
     C*                                                                         
     C                   ADD       1             CRRN                           
     C                   CLEAR                   SELIO                          
     C                   Z-ADD     CRRN          VDOPTN                         
     C                   MOVEL     MMOPTD        VDOPTD                         
     C                   MOVEL     MMOPTP        VDOPTP                         
     C                   WRITE     EXP000S                                      
     C                   ADD       1             COUNT                          
     C                   ENDDO                                                  
     C*                                                                         
     C                   Z-ADD     1             RCDNBR                         
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
     C                   WRITE     EXP000C                                      
     C                   MOVE      *OFF          *IN40                          
     C*                                                                         
     C                   RESET                   CRRN                           
     C*                                                                         
     C     WKLST1        SETLL     IMENU                                        
     C*                                                                         
     C     E002          ENDSR                                                  
     C*                                                                         
     C****************************************************************          
     C*   S003: CALL PROGRAMS                                        *          
     C****************************************************************          
     C*                                                                         
     C     S003          BEGSR                                                  
     C*                                                                         
     C                   MOVE      '0'           INQ25             1            
     C*                                                                         
     C** System Setup                                                           
     C     VDOPTP        IFEQ      'IS625R0'                                    
     C                   CALL      'IS625R0'                                    
     C                   PARM                    INQ25                          
     C                   GOTO      E003                                         
     C                   ENDIF                                                  
     C*                                                                         
     C** Company Master Maintenance                                             
     C     VDOPTP        IFEQ      'IS625R1'                                    
     C                   CALL      'IS625R1'                                    
     C                   PARM                    INQ25                          
     C                   GOTO      E003                                         
     C                   ENDIF                                                  
     C*                                                                         
     C** Expense Master Maintenance                                             
     C     VDOPTP        IFEQ      'IS626R1'                                    
     C                   CALL      'IS626R1'                                    
     C                   PARM                    INQ25                          
     C                   GOTO      E003                                         
     C                   ENDIF                                                  
     C*                                                                         
     C** Employee Master Maintenance                                            
     C     VDOPTP        IFEQ      'IS628R1'                                    
     C                   CALL      'IS628R1'                                    
     C                   PARM                    INQ25                          
     C                   GOTO      E003                                         
     C                   ENDIF                                                  
     C*                                                                         
     C** Project Master Maintenance                                             
     C     VDOPTP        IFEQ      'IS627R1'                                    
     C                   CALL      'IS627R1'                                    
     C                   PARM                    INQ25                          
     C                   GOTO      E003                                         
     C                   ENDIF                                                  
     C*                                                                         
     C** Cost Center Master Maintenance                                         
     C     VDOPTP        IFEQ      'IS627R4'                                    
     C                   CALL      'IS627R4'                                    
     C                   PARM                    INQ25                          
     C                   GOTO      E003                                         
     C                   ENDIF                                                  
     C*                                                                         
     C** Enter Transactions                                                     
     C     VDOPTP        IFEQ      'IS629R1'                                    
     C                   CALL      'IS629R1'                                    
     C                   PARM                    INQ25                          
     C                   GOTO      E003                                         
     C                   ENDIF                                                  
     C*                                                                         
     C** Change Transacitons                                                    
     C     VDOPTP        IFEQ      'IS629R4'                                    
     C                   CALL      'IS629R4'                                    
     C                   PARM                    INQ25                          
     C                   GOTO      E003                                         
     C                   ENDIF                                                  
     C*                                                                         
     C** Print Transacitons                                                     
     C     VDOPTP        IFEQ      'IS623R1'                                    
     C                   CALL      'IS623R1'                                    
     C                   PARM                    INQ25                          
     C                   GOTO      E003                                         
     C                   ENDIF                                                  
     C*                                                                         
     C** Expense System Reports                                                 
     C     VDOPTP        IFEQ      'IS623R5'                                    
     C                   CALL      'IS623R5'                                    
     C                   PARM                    INQ25                          
     C                   GOTO      E003                                         
     C                   ENDIF                                                  
     C*                                                                         
     C** All other programs                                                     
     C                   CALL      VDOPTP                                       
     C                   PARM                    INQ25                          
     C*                                                                         
     C     E003          ENDSR                                                  
     C*                                                                         
     C*****************************************************************         
     C* PSSR *     ERROR HANDLING FOR LOCKED PATIENT MASTER           *         
     C*****************************************************************         
     C*                                                                         
     C     *PSSR         BEGSR                                                  
     C*                                                                         
     C                   MOVE      '1'           *INLR                          
     C                   RETURN                                                 
     C*                                                                         
     C                   ENDSR                                                  
     C*                                                                         
     C*****************************************************************         
     C*  SNDMSG: WRITE ERROR MESSAGE SUBROUTINE                                 
     C*****************************************************************         
     C*                                                                         
     C     SNDMSG        BEGSR                                                  
     C*                                                                         
     C                   MOVE      *OFF          *IN28                          
     C*                                                                         
     C                   CALL      'SNDMSGX'                                    
     C                   PARM                    MSGID             7            
     C                   PARM      'IS920M'      MSGF             10            
     C                   PARM      *BLANKS       MSGD             50            
     C*                                                                         
     C                   ENDSR                                                  
     C*                                                                         
     C****************************************************************          
     C*   INZSR: INITIALIZATION SUBROUTINE                           *          
     C****************************************************************          
     C*                                                                         
     C     *INZSR        BEGSR                                                  
     C*                                                                         
     C     WKLST1        KLIST                                                  
     C                   KFLD                    WKMENU                         
     C                   KFLD                    LWOPTN                         
     C*                                                                         
     C     MMKY01        KLIST                                                  
     C                   KFLD                    USERID                         
     C                   KFLD                    WKMENU                         
     C                   KFLD                    MMOPTP                         
     C*                                                                         
     C     MMKY02        KLIST                                                  
     C                   KFLD                    USERID                         
     C                   KFLD                    WKMENU                         
     C                   KFLD                    VDOPTP                         
     C*                                                                         
     C                   Z-ADD     0             LWOPTN            2 0          
     C                   Z-ADD     0             CRRN              5 0          
     C                   MOVEL     'EXPENSE'     WKMENU           10            
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
     C                   EXSR      SNDMSG                                       
     C                   WRITE     SFLCTLM                                      
     C*                                                                         
     C                   ENDSR                                                  
     C*                                                                         
     C****************************************************************          
     C****************************************************************          
