/*********************************************************************/         
/* FastBreed Technologies, Inc. © 2000                               */         
/*********************************************************************/         
/*********************************************************************/         
/**                                                                 **/         
/** OBJECT ID: IS923C6                                              **/         
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
                                                                                
                                                                                
             /* PRINT SUMMARY BY PROJECT                  */                    
             IF         COND(&WKSEL1 *EQ 'Y') THEN(DO)                          
             IF         COND(&WKSUB1 *NE 'Y') THEN(DO)                          
             OVRPRTF    FILE(QSYSPRT) PAGESIZE(*N 80) CPI(10)                   
             CALL       PGM(IS923R7) PARM(&WKCMPX &WKPUNQ &WKCUNQ +             
                          &WKDPTX &WKNBRX &WKCDEX &WKBEGX &WKENDX)              
             DLTOVR     FILE(QSYSPRT)                                           
             ENDDO                                                              
             IF         COND(&WKSUB1 *EQ 'Y') THEN(DO)                          
             OVRPRTF    FILE(QSYSPRT) PAGESIZE(*N 132) CPI(10)                  
             CALL       PGM(IS923R8) PARM(&WKCMPX &WKPUNQ &WKCUNQ +             
                          &WKDPTX &WKNBRX &WKCDEX &WKBEGX &WKENDX)              
             DLTOVR     FILE(QSYSPRT)                                           
             ENDDO                                                              
             ENDDO                                                              
                                                                                
                                                                                
             /* PRINT SUMMARY BY COST CENTER              */                    
             IF         COND(&WKSEL2 *EQ 'Y') THEN(DO)                          
             IF         COND(&WKSUB2 *NE 'Y') THEN(DO)                          
             OVRPRTF    FILE(QSYSPRT) PAGESIZE(*N 80) CPI(10)                   
             CALL       PGM(IS923R9) PARM(&WKCMPX &WKPUNQ &WKCUNQ +             
                          &WKDPTX &WKNBRX &WKCDEX &WKBEGX &WKENDX)              
             DLTOVR     FILE(QSYSPRT)                                           
             ENDDO                                                              
             IF         COND(&WKSUB2 *EQ 'Y') THEN(DO)                          
             OVRPRTF    FILE(QSYSPRT) PAGESIZE(*N 132) CPI(10)                  
             CALL       PGM(IS923R91) PARM(&WKCMPX &WKPUNQ &WKCUNQ +            
                          &WKDPTX &WKNBRX &WKCDEX &WKBEGX &WKENDX)              
             DLTOVR     FILE(QSYSPRT)                                           
             ENDDO                                                              
             ENDDO                                                              
                                                                                
                                                                                
             /* PRINT SUMMARY BY DEPARTMENT               */                    
             IF         COND(&WKSEL3 *EQ 'Y') THEN(DO)                          
             IF         COND(&WKSUB3 *NE 'Y') THEN(DO)                          
             OVRPRTF    FILE(QSYSPRT) PAGESIZE(*N 80) CPI(10)                   
             CALL       PGM(IS923R92) PARM(&WKCMPX &WKPUNQ &WKCUNQ +            
                          &WKDPTX &WKNBRX &WKCDEX &WKBEGX &WKENDX)              
             DLTOVR     FILE(QSYSPRT)                                           
             ENDDO                                                              
             IF         COND(&WKSUB3 *EQ 'Y') THEN(DO)                          
             OVRPRTF    FILE(QSYSPRT) PAGESIZE(*N 132) CPI(10)                  
             CALL       PGM(IS923R93) PARM(&WKCMPX &WKPUNQ &WKCUNQ +            
                          &WKDPTX &WKNBRX &WKCDEX &WKBEGX &WKENDX)              
             DLTOVR     FILE(QSYSPRT)                                           
             ENDDO                                                              
             ENDDO                                                              
                                                                                
                                                                                
             /* PRINT SUMMARY BY EMPLOYEE NUMBER          */                    
             IF         COND(&WKSEL4 *EQ 'Y') THEN(DO)                          
             IF         COND(&WKSUB4 *NE 'Y') THEN(DO)                          
             OVRPRTF    FILE(QSYSPRT) PAGESIZE(*N 80) CPI(10)                   
             CALL       PGM(IS923R94) PARM(&WKCMPX &WKPUNQ &WKCUNQ +            
                          &WKDPTX &WKNBRX &WKCDEX &WKBEGX &WKENDX)              
             DLTOVR     FILE(QSYSPRT)                                           
             ENDDO                                                              
             IF         COND(&WKSUB4 *EQ 'Y') THEN(DO)                          
             OVRPRTF    FILE(QSYSPRT) PAGESIZE(*N 132) CPI(10)                  
             CALL       PGM(IS923R95) PARM(&WKCMPX &WKPUNQ &WKCUNQ +            
                          &WKDPTX &WKNBRX &WKCDEX &WKBEGX &WKENDX)              
             DLTOVR     FILE(QSYSPRT)                                           
             ENDDO                                                              
             ENDDO                                                              
                                                                                
                                                                                
             /* PRINT SUMMARY BY EMPLOYEE NAME            */                    
             IF         COND(&WKSEL7 *EQ 'Y') THEN(DO)                          
             IF         COND(&WKSUB7 *NE 'Y') THEN(DO)                          
             OVRPRTF    FILE(QSYSPRT) PAGESIZE(*N 80) CPI(10)                   
             CALL       PGM(IS923R96) PARM(&WKCMPX &WKPUNQ &WKCUNQ +            
                          &WKDPTX &WKNBRX &WKCDEX &WKBEGX &WKENDX)              
             DLTOVR     FILE(QSYSPRT)                                           
             ENDDO                                                              
             IF         COND(&WKSUB7 *EQ 'Y') THEN(DO)                          
             OVRPRTF    FILE(QSYSPRT) PAGESIZE(*N 132) CPI(10)                  
             CALL       PGM(IS923R97) PARM(&WKCMPX &WKPUNQ &WKCUNQ +            
                          &WKDPTX &WKNBRX &WKCDEX &WKBEGX &WKENDX)              
             DLTOVR     FILE(QSYSPRT)                                           
             ENDDO                                                              
             ENDDO                                                              
                                                                                
                                                                                
             /* PRINT SUMMARY BY EXPENSE CODE             */                    
             IF         COND(&WKSEL5 *EQ 'Y') THEN(DO)                          
             OVRPRTF    FILE(QSYSPRT) PAGESIZE(*N 80) CPI(10)                   
             CALL       PGM(IS923R98) PARM(&WKCMPX &WKPUNQ &WKCUNQ +            
                          &WKDPTX &WKNBRX &WKCDEX &WKBEGX &WKENDX)              
             DLTOVR     FILE(QSYSPRT)                                           
             ENDDO                                                              
                                                                                
                                                                                
             /* PRINT TRANSACTIONS BY DATE                */                    
             IF         COND(&WKSEL6 *EQ 'Y') THEN(DO)                          
             OVRPRTF    FILE(QSYSPRT) PAGESIZE(*N 132) CPI(10)                  
             CALL       PGM(IS923R6) PARM(&WKCMPX &WKPUNQ &WKCUNQ +             
                          &WKDPTX &WKNBRX &WKCDEX &WKBEGX &WKENDX)              
             DLTOVR     FILE(QSYSPRT)                                           
             ENDDO                                                              
                                                                                
                                                                                
ENDCLPGM:    ENDPGM                                                             
