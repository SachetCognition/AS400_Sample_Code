/*********************************************************************/         
/* FastBreed Technologies, Inc. © 2000                               */         
/*********************************************************************/         
/*********************************************************************/         
/**                                                                 **/         
/** OBJECT ID: IS923C2                                              **/         
/**                                                                 **/         
/** OBJECT DESCRIPTION: PRINT EXPENSE REPORT                        **/         
/**                                                                 **/         
/**  MOD#   DATE   PGMR  DESCRIPTION                                **/         
/**  ----  ------  ----  -----------------------------------------  **/         
/**                                                                 **/         
/*********************************************************************/         
                                                                                
             PGM        PARM(&WKCMPX &WKEMPX &WKBEGX &WKENDX)                   
                                                                                
             DCL        VAR(&WKCMPX)  TYPE(*CHAR)   LEN(5)                      
             DCL        VAR(&WKEMPX)  TYPE(*CHAR)   LEN(5)                      
             DCL        VAR(&WKBEGX)  TYPE(*CHAR)   LEN(8)                      
             DCL        VAR(&WKENDX)  TYPE(*CHAR)   LEN(8)                      
             DCL        VAR(&WKDONE)  TYPE(*CHAR)   LEN(1)    VALUE('N')        
                                                                                
                                                                                
             /* CREATE EXPENSE REPORT FOR WEEK            */                    
             OVRPRTF    FILE(QSYSPRT) PAGESIZE(88 120) LPI(8) +                 
                          CPI(15) OVRFLW(84)                                    
             CALL       PGM(IS923R2) PARM(&WKCMPX &WKEMPX &WKBEGX +             
                          &WKENDX)                                              
                                                                                
                                                                                
ENDCLPGM:    ENDPGM                                                             
