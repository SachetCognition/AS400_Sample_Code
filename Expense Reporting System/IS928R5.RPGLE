     H*****************************************************************         
     H* FastBreed Technologies, Inc. © 2000                           *         
     H*****************************************************************         
     H*****************************************************************         
     H**                                                             **         
     H** OBJECT ID: IS928R5                                          **         
     H**                                                             **         
     H** OBJECT DESCRIPTION: MAINTAIN EMPLOYEE MASTER - LOAD FILE    **         
     H**                                                             **         
     H** MOD#    DATE      DESCRIPTION                               **         
     H** ----  --------    ----------------------------------------- **         
     H**                                                             **         
     H*****************************************************************         
0001 FMFEMP100  IP   E           K DISK                                         
     FIS928P1   UF A E           K DISK                                         
     FIS928P2   UF A E           K DISK                                         
     FIS928L1   IF   E           K DISK    RENAME(R928P1:R928) PREFIX(EM_)      
     FQSYSPRT   O    F  132        PRINTER OFLIND(*INOF)                        
     D*                                                                         
     D EMPLOYEES       S              8  0 DIM(9999)                            
     D EMPNAME         S             40    DIM(9999)                            
     D CMB             S             48    DIM(9999)                            
     D*                                                                         
     D                 DS                                                       
     D DSCOMB                  1     48                                         
     D  DSCMB1                 1     40                                         
     D  DSCMB2                41     48  0                                      
     D*                                                                         
     D PSDS           SDS                                                       
     D PGMNAM            *PROC                                                  
     D USERID                254    263                                         
     D*                                                                         
     C*****************************************************************         
     C*  MAINLINE                                                     *         
     C*****************************************************************         
     C*                                                                         
     C                   Z-ADD     DFCMPX        CMPUNQ                         
0009 C                   MOVE      EMPNO         EMPNBR                         
     C     EMKY01        CHAIN     R928P1                             96        
     C*                                                                         
     C                   MOVEL     EMPNME        OLDNAME          40            
     C                   MOVEL     EMPDSP        OLDDSP            1            
     C                   MOVE      EMPDPT        OLDDPT            6 0          
     C                   MOVEL     EMPPOS        OLDPOS           30            
     C*                                                                         
0011 C                   IF        DFNMEX     <> *BLANKS                        
0012 C                   MOVEL     DFNMEX        EMPNME                         
     C                   ELSE                                                   
0014 C                   EVAL      EMPNME = %TRIM(EMPFN     ) + ' ' +           
0015 C                                      %TRIM(EMPNM     )                   
     C                   ENDIF                                                  
     C*                                                                         
0018 C                   MOVE      DFDPTX        WKDPTX           20            
     C                   IF        WKDPTX      = *BLANKS OR                     
     C                             WKDPTX      = *ZEROS                         
     C                   MOVE      *ZEROS        EMPDPT                         
     C                   ELSE                                                   
0019 C                   MOVE      DFDPTX        EMPDPT                         
     C                   ENDIF                                                  
     C*                                                                         
0024 C                   IF        DFPOSX     <> *BLANKS                        
0025 C                   MOVEL     DFPOSX        EMPPOS                         
     C                   ELSE                                                   
     C                   MOVE      *BLANKS       EMPPOS                         
     C                   ENDIF                                                  
     C*                                                                         
0030 C                   IF        ACTIV       = *BLANKS                        
     C                   MOVE      'Y'           EMPDSP                         
     C                   ENDIF                                                  
     C*                                                                         
0034 C                   IF        ACTIV      <> *BLANKS                        
0035 C                   IF        ACTIV         ='1'                           
     C                   MOVE      'Y'           EMPDSP                         
     C                   ELSE                                                   
     C                   MOVE      'N'           EMPDSP                         
     C                   ENDIF                                                  
     C                   ENDIF                                                  
     C*                                                                         
     C                   MOVEL     DFMBRX        EMPMBR                         
     C*                                                                         
     C** Add record                                                             
     C                   IF        *IN96 = *ON                                  
     C     DFCMPX        CHAIN     R928P2                             95        
     C                   IF        *IN95 = *OFF                                 
     C                   ADD       1             NXTUNQ                         
     C                   Z-ADD     NXTUNQ        EMPUNQ                         
     C                   UPDATE    R928P2                                       
     C                   ELSE                                                   
     C                   Z-ADD     DFCMPX        CMPUNQ                         
     C                   Z-ADD     1             NXTUNQ                         
     C                   Z-ADD     NXTUNQ        EMPUNQ                         
     C                   WRITE     R928P2                                       
     C                   ENDIF                                                  
     C                   WRITE     R928P1                                       
     C                   EXSR      CHKOVF                                       
     C                   EXCEPT    DET01                                        
     C                   ENDIF                                                  
     C*                                                                         
     C** Update record                                                          
     C                   IF        *IN96 = *OFF                                 
     C                   UPDATE    R928P1                                       
     C                   IF        EMPNME <> OLDNAME                            
     C                   EXSR      CHKOVF                                       
     C                   EXCEPT    DET02                                        
     C                   ENDIF                                                  
     C                   IF        EMPDPT <> OLDDPT                             
     C                   EXSR      CHKOVF                                       
     C                   EXCEPT    DET03                                        
     C                   ENDIF                                                  
     C                   IF        EMPPOS <> OLDPOS                             
     C                   EXSR      CHKOVF                                       
     C                   EXCEPT    DET06                                        
     C                   ENDIF                                                  
     C                   IF        EMPDSP <> OLDDSP                             
     C                   EXSR      CHKOVF                                       
     C                   EXCEPT    DET04                                        
     C                   ENDIF                                                  
     C                   ENDIF                                                  
     C*                                                                         
     C** Add record to summary array                                            
     C                   Z-ADD     1             X                 5 0          
     C     EMPNBR        LOOKUP    EMPLOYEES(X)                           98    
     C                   IF        *IN98 = *OFF                                 
     C                   Z-ADD     1             X                              
     C     EMPDFT        LOOKUP    EMPLOYEES(X)                           98    
     C                   IF        *IN98 = *ON                                  
     C                   MOVE      EMPNBR        EMPLOYEES(X)                   
     C                   ENDIF                                                  
     C                   ENDIF                                                  
     C*                                                                         
     C** De-activate missing employees                                          
     CLR                 EXSR      S100                                         
     C*                                                                         
     C** Check for possible duplicates                                          
     CLR                 EXSR      S200                                         
     C*                                                                         
     C*****************************************************************         
     C*   S100: DE-ACTIVATE MISSING EMPLOYEES                                   
     C*****************************************************************         
     C*                                                                         
     C     S100          BEGSR                                                  
     C*                                                                         
     C     EMKY02        SETLL     R928P1                                       
     C     DFCMPX        READE     R928P1                                 94    
     C     *IN94         DOWEQ     *OFF                                         
     C*                                                                         
     C                   Z-ADD     1             X                              
     C     EMPNBR        LOOKUP    EMPLOYEES(X)                           98    
     C                   IF        *IN98 = *OFF AND DFMBRX = EMPMBR AND         
     C                             EMPDSP <> 'N'                                
     C                   EXSR      CHKOVF                                       
     C                   EXCEPT    DET04                                        
     C                   MOVE      'N'           EMPDSP                         
     C                   UPDATE    R928P1                                       
     C                   ENDIF                                                  
     C*                                                                         
     C     DFCMPX        READE     R928P1                                 94    
     C                   ENDDO                                                  
     C*                                                                         
     C     E100          ENDSR                                                  
     C*                                                                         
     C*****************************************************************         
     C*   S200: CHECK FOR POSSIBLE DUPLICATES                                   
     C*****************************************************************         
     C*                                                                         
     C     S200          BEGSR                                                  
     C*                                                                         
     C                   MOVEA     *ZEROS        EMPLOYEES                      
     C                   MOVEA     *BLANKS       EMPNAME                        
     C*                                                                         
     C     EMKY03        SETLL     R928                                         
     C     DFCMPX        READE     R928                                   96    
     C     *IN96         DOWEQ     *OFF                                         
     C                   IF        EM_EMPNME = SVNAME AND SVNAME <> *BLANKS     
     C                   Z-ADD     1             X                              
     C     EMPDFT        LOOKUP    EMPLOYEES(X)                           98    
     C                   IF        *IN98 = *ON                                  
     C                   MOVE      SVENBR        EMPLOYEES(X)                   
     C                   MOVEL     SVNAME        EMPNAME(X)                     
     C                   ENDIF                                                  
     C                   Z-ADD     1             X                              
     C     EMPDFT        LOOKUP    EMPLOYEES(X)                           98    
     C                   IF        *IN98 = *ON                                  
     C                   MOVE      EM_EMPNBR     EMPLOYEES(X)                   
     C                   MOVEL     EM_EMPNME     EMPNAME(X)                     
     C                   ENDIF                                                  
     C                   ENDIF                                                  
     C                   IF        DFMBRX = EM_EMPMBR AND EM_EMPDSP = 'Y'       
     C                   MOVEL     EM_EMPNME     SVNAME           40            
     C                   MOVE      EM_EMPNBR     SVENBR            8 0          
     C                   ENDIF                                                  
     C     DFCMPX        READE     R928                                   96    
     C                   ENDDO                                                  
     C*                                                                         
     C** Combine arrays.                                                        
     C     1             DO        9999          X                              
     C                   MOVEL     EMPNAME(X)    DSCMB1                         
     C                   Z-ADD     EMPLOYEES(X)  DSCMB2                         
     C                   MOVE      DSCOMB        CMB(X)                         
     C                   ENDDO                                                  
     C*                                                                         
     C** Sort combined arrays.                                                  
     C                   SORTA     CMB                                          
     C*                                                                         
     C** Reload sorted arrays.                                                  
     C                   MOVEA     *BLANKS       EMPNAME                        
     C                   MOVEA     *ZEROS        EMPLOYEES                      
     C     1             DO        9999          X                              
     C                   MOVEL     CMB(X)        DSCOMB                         
     C                   MOVEL     DSCMB1        EMPNAME(X)                     
     C                   Z-ADD     DSCMB2        EMPLOYEES(X)                   
     C                   ENDDO                                                  
     C*                                                                         
     C     1             DO        9999          X                              
     C                   Z-ADD     EMPLOYEES(X)  EMPNBR                         
     C     EMPNBR        IFNE      0                                            
     C     EMKY01        CHAIN     R928P1                             95        
     C     *IN95         IFEQ      *OFF                                         
     C                   EXSR      CHKOVF                                       
     C                   EXCEPT    DET05                                        
     C                   ENDIF                                                  
     C                   ENDIF                                                  
     C                   ENDDO                                                  
     C*                                                                         
     C     E200          ENDSR                                                  
     C*                                                                         
     C*****************************************************************         
     C* CHKOVF SUBROUTINE - CHECK FOR OVERFLOW AND REPRINT HEADER               
     C*****************************************************************         
     C*                                                                         
     C     CHKOVF        BEGSR                                                  
     C*                                                                         
     C     *INOF         IFEQ      '1'                                          
     C     HDGFLG        OREQ      'Y'                                          
     C                   MOVE      '0'           *INOF                          
     C                   MOVE      'N'           HDGFLG                         
     C                   EXCEPT    HDG1                                         
     C                   END                                                    
     C*                                                                         
     C     CHKO99        ENDSR                                                  
     C*                                                                         
     C*************************************************************             
     C*  INZSR: INITIALIZATION SUBROUTINE                                       
     C*************************************************************             
     C*                                                                         
     C     *INZSR        BEGSR                                                  
     C*                                                                         
     C     *ENTRY        PLIST                                                  
     C                   PARM                    WKCMPX            5            
     C                   PARM                    WKMBRX           10            
     C*                                                                         
     C     EMKY01        KLIST                                                  
     C                   KFLD                    DFCMPX                         
     C                   KFLD                    EMPNBR                         
     C*                                                                         
     C     EMKY02        KLIST                                                  
     C                   KFLD                    DFCMPX                         
     C                   KFLD                    EMPDFT                         
     C*                                                                         
     C     EMKY03        KLIST                                                  
     C                   KFLD                    DFCMPX                         
     C                   KFLD                    NMEDFT                         
     C*                                                                         
     C                   MOVE      WKCMPX        DFCMPX            5 0          
     C                   MOVEL     WKMBRX        DFMBRX           10            
     C                   MOVE      *BLANKS       DFNMEX           10            
     C                   MOVE      *BLANKS       DFNMEL           10            
     C                   MOVE      *BLANKS       DFNMEF           10            
     C                   MOVE      *BLANKS       DFPOSX           10            
     C                   MOVE      *BLANKS       DFDPTX           10            
     C                   MOVE      *BLANKS       DFACTX           10            
     C                   MOVE      *BLANKS       DFVALX           10            
     C                   MOVEA     *ZEROS        EMPLOYEES                      
     C                   Z-ADD     0             EMPDFT            8 0          
     C                   MOVE      *BLANKS       NMEDFT           40            
     C*                                                                         
     C                   MOVE      'Y'           HDGFLG            1            
     C*                                                                         
     C                   TIME                    TIMEX             6 0          
     C*                                                                         
     C                   ENDSR                                                  
     C*                                                                         
     C*****************************************************************         
     C*****************************************************************         
     OQSYSPRT   E            HDG1             02                                
     O                       PGMNAM              10                             
     O                                          123 'Page'                      
     O                       PAGE          Z    128                             
     O*                                                                         
     O          E            HDG1        1  1                                   
     O                                            5 'Date:'                     
     O                       UDATE         Y     14                             
     O                                           67 'Employee Master'           
     O                                           80 'Load/Refresh'              
     O                       USERID             129                             
     O*                                                                         
     O          E            HDG1        1  1                                   
     O                                            8 'Emp Nbr'                   
     O                                           18 'Emp Name'                  
     O                                           60 'Dept'                      
     O                                           77 'Action Taken'              
     O*                                                                         
     O          E            DET01       2                                      
     O                       EMPNBR               8 '       0'                  
     O                       EMPNME              50                             
     O                       EMPDPT              60 '     0'                    
     O                                           77 'Record added'              
     O*                                                                         
     O          E            DET02       2                                      
     O                       EMPNBR               8 '       0'                  
     O                       EMPNME              50                             
     O                       EMPDPT              60 '     0'                    
     O                                           77 'Name changed'              
     O*                                                                         
     O          E            DET03       2                                      
     O                       EMPNBR               8 '       0'                  
     O                       EMPNME              50                             
     O                       EMPDPT              60 '     0'                    
     O                                           77 'Dept changed'              
     O*                                                                         
     O          E            DET04       2                                      
     O                       EMPNBR               8 '       0'                  
     O                       EMPNME              50                             
     O                       EMPDPT              60 '     0'                    
     O                                           86 'Employee de-activated'     
     O*                                                                         
     O          E            DET05       2                                      
     O                       EMPNBR               8 '       0'                  
     O                       EMPNME              50                             
     O                       EMPDPT              60 '     0'                    
     O                                           84 'Possible duplicate.'       
     O                                          100 'Please review.'            
     O*                                                                         
     O          E            DET06       2                                      
     O                       EMPNBR               8 '       0'                  
     O                       EMPNME              50                             
     O                       EMPDPT              60 '     0'                    
     O                                           81 'Position changed'          
     O*                                                                         
