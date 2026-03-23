     H TIMFMT(*HMS)                                                             
     H*****************************************************************         
     H**                                                             **         
     H** OBJECT ID: DATER01                                          **         
     H**                                                             **         
     H** OBJECT DESCRIPTION: LOAD SCDATEF                            **         
     H**                                                             **         
     H*****************************************************************         
     FSCDATEF   O  A E           K DISK                                         
     D*                                                                         
     D DATISO          S               D   DATFMT(*ISO)                         
     D*                                                                         
     D                 DS                  INZ                                  
     DWRKDATE1                 1      6  0                                      
     D  YYDEC                  1      2  0                                      
     D  MMDEC                  3      4  0                                      
     D  DDDEC                  5      6  0                                      
     D*                                                                         
     D                 DS                  INZ                                  
     DWRKDATE2                 1      6  0                                      
     D  MMDEC2                 1      2  0                                      
     D  DDDEC2                 3      4  0                                      
     D  YYDEC2                 5      6  0                                      
     D*                                                                         
     D                 DS                  INZ                                  
     DWRKDATE3                 1      6  0                                      
     D  DDDEC3                 1      2  0                                      
     D  MMDEC3                 3      4  0                                      
     D  YYDEC3                 5      6  0                                      
     D*                                                                         
     D                 DS                  INZ                                  
     DBEGDATE                  1      8  0                                      
     D  DSYY                   1      4  0                                      
     D  DSMM                   5      6  0                                      
     D  DSDD                   7      8  0                                      
     D*                                                                         
     D PSDS           SDS                                                       
     D  PGMNAM           *PROC                                                  
     D  USERID               254    263                                         
     I*                                                                         
     C*****************************************************************         
     C*  MAINLINE                                                     *         
     C*****************************************************************         
     C*                                                                         
     C                   Z-ADD     19931231      BEGDATE           8 0          
     C                   Z-ADD     20381230      ENDDATE           8 0          
     C                   Z-ADD     33968         JUL1                           
     C                   Z-ADD     0             JUL2                           
     C                   Z-ADD     DSYY          SAVEYY            4 0          
     C                   MOVEL     'FRI'         DAYSHT                         
     C                   MOVEL     'FRIDAY'      DAY                            
     C*                                                                         
     C** Load dates                                                             
     C                   DOW       BEGDATE < ENDDATE                            
     C*                                                                         
     C                   MOVE      BEGDATE       DATISO                         
     C                   ADDDUR    1:*DAYS       DATISO                         
     C                   MOVE      DATISO        BEGDATE                        
     C*                                                                         
     C                   MOVE      BEGDATE       WRKDATE1                       
     C                   MOVE      WRKDATE1      YMDDEC                         
     C     WRKDATE1      MULT      100.0001      WRKDATE2                       
     C                   MOVE      WRKDATE2      MDYDEC                         
     C                   MOVE      MMDEC2        MMDEC3                         
     C                   MOVE      DDDEC2        DDDEC3                         
     C                   MOVE      YYDEC2        YYDEC3                         
     C                   MOVE      WRKDATE3      DMYDEC                         
     C*                                                                         
     C                   IF        DSYY < 2000                                  
     C                   Z-ADD     0             CDEC                           
     C                   ELSE                                                   
     C                   Z-ADD     1             CDEC                           
     C                   ENDIF                                                  
     C*                                                                         
     C                   MOVE      MMDEC         MMCHR                          
     C                   MOVE      DDDEC         DDCHR                          
     C                   MOVE      YYDEC         YYCHR                          
     C                   MOVE      CDEC          CCHR                           
     C                   MOVE      DSYY          FULLYRX           4            
     C*                                                                         
     C                   MOVE      MDYDEC        MDYCHR                         
     C                   MOVE      DMYDEC        DMYCHR                         
     C                   MOVE      WRKDATE1      YMDCHR                         
     C*                                                                         
     C                   IF        DSMM = 1                                     
     C                   MOVEL     'JANUARY  '   MONTH                          
     C                   MOVEL     'JAN'         MTHSHT                         
     C                   EVAL      LNGDTU = 'January'+' '+DDCHR+', '+FULLYRX    
     C                   EVAL      LNGDTE = DDCHR+' January'+' '+ FULLYRX       
     C                   ENDIF                                                  
     C*                                                                         
     C                   IF        DSMM = 2                                     
     C                   MOVEL     'FEBRUARY '   MONTH                          
     C                   MOVEL     'FEB'         MTHSHT                         
     C                   EVAL      LNGDTU = 'February'+' '+DDCHR+', '+FULLYRX   
     C                   EVAL      LNGDTE = DDCHR+' February'+' '+ FULLYRX      
     C                   ENDIF                                                  
     C*                                                                         
     C                   IF        DSMM = 3                                     
     C                   MOVEL     'MARCH    '   MONTH                          
     C                   MOVEL     'MAR'         MTHSHT                         
     C                   EVAL      LNGDTU = 'March'+' '+DDCHR+', '+FULLYRX      
     C                   EVAL      LNGDTE = DDCHR+' March'+' '+ FULLYRX         
     C                   ENDIF                                                  
     C*                                                                         
     C                   IF        DSMM = 4                                     
     C                   MOVEL     'APRIL    '   MONTH                          
     C                   MOVEL     'APR'         MTHSHT                         
     C                   EVAL      LNGDTU = 'April'+' '+DDCHR+', '+FULLYRX      
     C                   EVAL      LNGDTE = DDCHR+' April'+' '+ FULLYRX         
     C                   ENDIF                                                  
     C*                                                                         
     C                   IF        DSMM = 5                                     
     C                   MOVEL     'MAY      '   MONTH                          
     C                   MOVEL     'MAY'         MTHSHT                         
     C                   EVAL      LNGDTU = 'May'+' '+DDCHR+', '+FULLYRX        
     C                   EVAL      LNGDTE = DDCHR+' May'+' '+ FULLYRX           
     C                   ENDIF                                                  
     C*                                                                         
     C                   IF        DSMM = 6                                     
     C                   MOVEL     'JUNE     '   MONTH                          
     C                   MOVEL     'JUN'         MTHSHT                         
     C                   EVAL      LNGDTU = 'June'+' '+DDCHR+', '+FULLYRX       
     C                   EVAL      LNGDTE = DDCHR+' June'+' '+ FULLYRX          
     C                   ENDIF                                                  
     C*                                                                         
     C                   IF        DSMM = 7                                     
     C                   MOVEL     'JULY     '   MONTH                          
     C                   MOVEL     'JUL'         MTHSHT                         
     C                   EVAL      LNGDTU = 'July'+' '+DDCHR+', '+FULLYRX       
     C                   EVAL      LNGDTE = DDCHR+' July'+' '+ FULLYRX          
     C                   ENDIF                                                  
     C*                                                                         
     C                   IF        DSMM = 8                                     
     C                   MOVEL     'AUGUST   '   MONTH                          
     C                   MOVEL     'AUG'         MTHSHT                         
     C                   EVAL      LNGDTU = 'August'+' '+DDCHR+', '+FULLYRX     
     C                   EVAL      LNGDTE = DDCHR+' August'+' '+ FULLYRX        
     C                   ENDIF                                                  
     C*                                                                         
     C                   IF        DSMM = 9                                     
     C                   MOVEL     'SEPTEMBER'   MONTH                          
     C                   MOVEL     'SEP'         MTHSHT                         
     C                   EVAL      LNGDTU = 'September'+' '+DDCHR+', '+FULLYRX  
     C                   EVAL      LNGDTE = DDCHR+' September'+' '+ FULLYRX     
     C                   ENDIF                                                  
     C*                                                                         
     C                   IF        DSMM = 10                                    
     C                   MOVEL     'OCTOBER  '   MONTH                          
     C                   MOVEL     'OCT'         MTHSHT                         
     C                   EVAL      LNGDTU = 'October  '+' '+DDCHR+', '+FULLYRX  
     C                   EVAL      LNGDTE = DDCHR+' October  '+' '+ FULLYRX     
     C                   ENDIF                                                  
     C*                                                                         
     C                   IF        DSMM = 11                                    
     C                   MOVEL     'NOVEMBER '   MONTH                          
     C                   MOVEL     'NOV'         MTHSHT                         
     C                   EVAL      LNGDTU = 'November '+' '+DDCHR+', '+FULLYRX  
     C                   EVAL      LNGDTE = DDCHR+' November '+' '+ FULLYRX     
     C                   ENDIF                                                  
     C*                                                                         
     C                   IF        DSMM = 12                                    
     C                   MOVEL     'DECEMBER '   MONTH                          
     C                   MOVEL     'DEC'         MTHSHT                         
     C                   EVAL      LNGDTU = 'December '+' '+DDCHR+', '+FULLYRX  
     C                   EVAL      LNGDTE = DDCHR+' December '+' '+ FULLYRX     
     C                   ENDIF                                                  
     C*                                                                         
     C                   MOVE      DSYY          FULLYR                         
     C                   MOVE      BEGDATE       CYMDDT                         
     C     BEGDATE       MULT      10000.0001    MDCYDT                         
     C*                                                                         
     C                   ADD       1             JUL1                           
     C*                                                                         
     C                   ADD       1             JUL2                           
     C                   IF        DSYY <> SAVEYY                               
     C                   Z-ADD     1             JUL2                           
     C                   Z-ADD     DSYY          SAVEYY                         
     C                   ENDIF                                                  
     C     DSYY          MULT      1000          CYJUL1                         
     C                   ADD       JUL2          CYJUL1                         
     C*                                                                         
     C                   IF        DAYSHT = 'FRI'                               
     C                   MOVEL     'SAT'         DAYSHT                         
     C                   MOVEL     'SATURDAY  '  DAY                            
     C                   GOTO      T001                                         
     C                   ENDIF                                                  
     C*                                                                         
     C                   IF        DAYSHT = 'SAT'                               
     C                   MOVEL     'SUN'         DAYSHT                         
     C                   MOVEL     'SUNDAY    '  DAY                            
     C                   GOTO      T001                                         
     C                   ENDIF                                                  
     C*                                                                         
     C                   IF        DAYSHT = 'SUN'                               
     C                   MOVEL     'MON'         DAYSHT                         
     C                   MOVEL     'MONDAY    '  DAY                            
     C                   GOTO      T001                                         
     C                   ENDIF                                                  
     C*                                                                         
     C                   IF        DAYSHT = 'MON'                               
     C                   MOVEL     'TUE'         DAYSHT                         
     C                   MOVEL     'TUESDAY   '  DAY                            
     C                   GOTO      T001                                         
     C                   ENDIF                                                  
     C*                                                                         
     C                   IF        DAYSHT = 'TUE'                               
     C                   MOVEL     'WED'         DAYSHT                         
     C                   MOVEL     'WEDNESDAY '  DAY                            
     C                   GOTO      T001                                         
     C                   ENDIF                                                  
     C*                                                                         
     C                   IF        DAYSHT = 'WED'                               
     C                   MOVEL     'THU'         DAYSHT                         
     C                   MOVEL     'THURSDAY  '  DAY                            
     C                   GOTO      T001                                         
     C                   ENDIF                                                  
     C*                                                                         
     C                   IF        DAYSHT = 'THU'                               
     C                   MOVEL     'FRI'         DAYSHT                         
     C                   MOVEL     'FRIDAY    '  DAY                            
     C                   GOTO      T001                                         
     C                   ENDIF                                                  
     C*                                                                         
     C     T001          TAG                                                    
     C                   EVAL      YYJULD = (YYDEC3 * 1000) + JUL2              
     C                   MOVE      YYJULD        YYJULC                         
     C*                                                                         
     C                   WRITE     DATFMT                                       
     C*                                                                         
     C                   ENDDO                                                  
     C*                                                                         
     C                   MOVE      *ON           *INLR                          
     C                   RETURN                                                 
     C*                                                                         
     C*****************************************************************         
