/*********************************************************************/         
/* FastBreed Technologies, Inc. © 2000                               */         
/*********************************************************************/         
/*********************************************************************/         
/**                                                                 **/         
/** OBJECT ID: IS923C1                                              **/         
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
                                                                                
             SBMJOB     CMD(CALL PGM(IS923C2) PARM(&WKCMPX &WKEMPX +            
                          &WKBEGX &WKENDX)) JOB(EXPRPT)                         
                                                                                
             ENDPGM                                                             
