/*********************************************************************/         
/* FastBreed Technologies, Inc. © 2000                               */         
/*********************************************************************/         
/*********************************************************************/         
/**                                                                 **/         
/** OBJECT ID: IS923C5                                              **/         
/**                                                                 **/         
/** OBJECT DESCRIPTION: EXPENSE SYSTEM REPORTS                      **/         
/**                                                                 **/         
/**  MOD#   DATE   PGMR  DESCRIPTION                                **/         
/**  ----  ------  ----  -----------------------------------------  **/         
/**                                                                 **/         
/*********************************************************************/         
                                                                                
             PGM        PARM(&WKCMPX &WKPUNQ &WKCUNQ &WKDPTX &WKNBRX +          
                          &WKCDEX &WKBEGX &WKENDX &WKSEL1 &WKSEL2 +             
                          &WKSEL3 &WKSEL4 &WKSEL5 &WKSEL6 &WKSUB1 +             
                          &WKSUB2 &WKSUB3 &WKSUB4 &WKSEL7 &WKSUB7)              
                                                                                
             DCL        VAR(&WKCMPX)  TYPE(*CHAR)   LEN(5)                      
             DCL        VAR(&WKPUNQ)  TYPE(*CHAR)   LEN(5)                      
             DCL        VAR(&WKCUNQ)  TYPE(*CHAR)   LEN(5)                      
             DCL        VAR(&WKDPTX)  TYPE(*CHAR)   LEN(6)                      
             DCL        VAR(&WKNBRX)  TYPE(*CHAR)   LEN(8)                      
             DCL        VAR(&WKCDEX)  TYPE(*CHAR)   LEN(3)                      
             DCL        VAR(&WKBEGX)  TYPE(*CHAR)   LEN(8)                      
             DCL        VAR(&WKENDX)  TYPE(*CHAR)   LEN(8)                      
             DCL        VAR(&WKSEL1)  TYPE(*CHAR)   LEN(1)                      
             DCL        VAR(&WKSEL2)  TYPE(*CHAR)   LEN(1)                      
             DCL        VAR(&WKSEL3)  TYPE(*CHAR)   LEN(1)                      
             DCL        VAR(&WKSEL4)  TYPE(*CHAR)   LEN(1)                      
             DCL        VAR(&WKSEL5)  TYPE(*CHAR)   LEN(1)                      
             DCL        VAR(&WKSEL6)  TYPE(*CHAR)   LEN(1)                      
             DCL        VAR(&WKSEL7)  TYPE(*CHAR)   LEN(1)                      
             DCL        VAR(&WKSUB1)  TYPE(*CHAR)   LEN(1)                      
             DCL        VAR(&WKSUB2)  TYPE(*CHAR)   LEN(1)                      
             DCL        VAR(&WKSUB3)  TYPE(*CHAR)   LEN(1)                      
             DCL        VAR(&WKSUB4)  TYPE(*CHAR)   LEN(1)                      
             DCL        VAR(&WKSUB7)  TYPE(*CHAR)   LEN(1)                      
                                                                                
             SBMJOB     CMD(CALL PGM(IS923C6) PARM(&WKCMPX &WKPUNQ +            
                          &WKCUNQ &WKDPTX &WKNBRX &WKCDEX &WKBEGX +             
                          &WKENDX &WKSEL1 &WKSEL2 &WKSEL3 &WKSEL4 +             
                          &WKSEL5 &WKSEL6 &WKSUB1 &WKSUB2 &WKSUB3 +             
                          &WKSUB4 &WKSEL7 &WKSUB7)) JOB(EXPSYSRPT)              
                                                                                
             ENDPGM                                                             
