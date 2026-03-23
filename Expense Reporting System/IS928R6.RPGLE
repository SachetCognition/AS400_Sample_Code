     H*****************************************************************         
     H* FastBreed Technologies, Inc. © 2000                           *         
     H*****************************************************************         
     H*****************************************************************         
     H**                                                             **         
     H** OBJECT ID: IS928R6                                          **         
     H**                                                             **         
     H** OBJECT DESCRIPTION: MAINTAIN EMPLOYEE MASTER - LOAD FILE    **         
     H**                                                             **         
     H** MOD#    DATE      DESCRIPTION                               **         
     H** ----  --------    ----------------------------------------- **         
     H**                                                             **         
     H*****************************************************************         
     F*                                                                         
     FEXPSRC    UPE  F   92        DISK                                         
     FIS928P3   IF   E           K DISK                                         
     D*                                                                         
     D                 DS                                                       
     D  DSLINE                 1     92                                         
     D  LINNBR                13     16                                         
     D  DFFILX                19     28                                         
     D  DFNBRX                48     57                                         
     D  DFNMEX                48     57                                         
     D  DFNMEF                63     72                                         
     D  DFNMEL                63     72                                         
     D  DFDPTX                48     57                                         
     D  DFPOSX                48     57                                         
     D  DFACTX                48     57                                         
     D  DFVALX                63     72                                         
     I*                                                                         
     IEXPSRC    AA  01                                                          
     I                                  1   92  DSTEXT                          
     I*                                                                         
     C*****************************************************************         
     C*  MAINLINE                                                     *         
     C*****************************************************************         
     C*                                                                         
     C     *IN01         IFEQ      *ON                                          
     C*                                                                         
     C                   MOVE      DSTEXT        DSLINE                         
     C*                                                                         
     C                   IF        LINNBR = '0001'                              
     C                   MOVEL     CNVFIL        DFFILX                         
     C                   ENDIF                                                  
     C*                                                                         
     C                   IF        LINNBR = '0009'                              
     C                   MOVEL     CNVNBR        DFNBRX                         
     C                   ENDIF                                                  
     C*                                                                         
     C                   IF        LINNBR = '0011' AND CNVNME <> *BLANKS        
     C                   MOVEL     CNVNME        DFNMEX                         
     C                   ENDIF                                                  
     C*                                                                         
     C                   IF        LINNBR = '0012' AND CNVNME <> *BLANKS        
     C                   MOVEL     CNVNME        DFNMEX                         
     C                   ENDIF                                                  
     C*                                                                         
     C                   IF        LINNBR = '0014' AND CNVNMF <> *BLANKS        
     C                   MOVEL     CNVNMF        DFNMEF                         
     C                   ENDIF                                                  
     C*                                                                         
     C                   IF        LINNBR = '0015' AND CNVNML <> *BLANKS        
     C                   MOVEL     CNVNML        DFNMEL                         
     C                   ENDIF                                                  
     C*                                                                         
     C                   IF        LINNBR = '0018' AND CNVDPT <> *BLANKS        
     C                   MOVEL     CNVDPT        DFDPTX                         
     C                   ENDIF                                                  
     C*                                                                         
     C                   IF        LINNBR = '0019' AND CNVDPT <> *BLANKS        
     C                   MOVEL     CNVDPT        DFDPTX                         
     C                   ENDIF                                                  
     C*                                                                         
     C                   IF        LINNBR = '0024' AND CNVPOS <> *BLANKS        
     C                   MOVEL     CNVPOS        DFPOSX                         
     C                   ENDIF                                                  
     C*                                                                         
     C                   IF        LINNBR = '0025' AND CNVPOS <> *BLANKS        
     C                   MOVEL     CNVPOS        DFPOSX                         
     C                   ENDIF                                                  
     C*                                                                         
     C                   IF        LINNBR = '0030' AND CNVACT <> *BLANKS        
     C                   MOVEL     CNVACT        DFACTX                         
     C                   ENDIF                                                  
     C*                                                                         
     C                   IF        LINNBR = '0034' AND CNVACT <> *BLANKS        
     C                   MOVEL     CNVACT        DFACTX                         
     C                   ENDIF                                                  
     C*                                                                         
     C                   IF        LINNBR = '0035' AND CNVACT <> *BLANKS        
     C                   MOVEL     CNVACT        DFACTX                         
     C                   MOVEL     CNVVAL        DFVALX                         
     C                   ENDIF                                                  
     C*                                                                         
     C                   MOVEL     DSLINE        DSTEXT                         
     C                   EXCEPT    DET1                                         
     C*                                                                         
     C                   ENDIF                                                  
     C*                                                                         
     C*****************************************************************         
     C*  INZSR: INITIALIZATION SUBROUTINE                                       
     C*****************************************************************         
     C*                                                                         
     C     *INZSR        BEGSR                                                  
     C*                                                                         
     C     *ENTRY        PLIST                                                  
     C                   PARM                    WKCMPX            5            
     C*                                                                         
     C                   MOVE      WKCMPX        DFCMPX            5 0          
     C     DFCMPX        CHAIN     R928P3                             95        
     C     *IN95         IFEQ      *ON                                          
     C                   MOVE      *ON           *INLR                          
     C                   RETURN                                                 
     C                   ENDIF                                                  
     C*                                                                         
     C                   ENDSR                                                  
     C*                                                                         
     C*****************************************************************         
     OEXPSRC    E            DET1                                               
     O                       DSTEXT                                             
     C*                                                                         
