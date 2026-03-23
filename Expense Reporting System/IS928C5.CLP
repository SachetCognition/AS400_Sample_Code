/*********************************************************************/         
/** FastBreed Technologies, Inc. © 2000                              */         
/*********************************************************************/         
/*********************************************************************/         
/**                                                                 **/         
/** OBJECT ID: IS928C5                                              **/         
/**                                                                 **/         
/** OBJECT DESCRIPTION: EMPLOYEE MASTER MAINTENANCE - LOAD FILE     **/         
/**                                                                 **/         
/** MOD#   DATE  PGMR DESCRIPTION                                   **/         
/** ----  ------ ---- --------------------------------------------  **/         
/**                                                                 **/         
/*********************************************************************/         
             PGM        PARM(&WKCMPX &WKFILX &WKMBRX)                           
             DCL        VAR(&WKCMPX) TYPE(*CHAR) LEN(5)                         
             DCL        VAR(&WKFILX) TYPE(*CHAR) LEN(10)                        
             DCL        VAR(&WKMBRX) TYPE(*CHAR) LEN(10)                        
                                                                                
                                                                                
             CPYSRCF    FROMFILE(EXPSRCX) TOFILE(EXPSRCX) +                     
                          FROMMBR(IS928R5X) TOMBR(IS928R5)                      
             MONMSG     MSGID(CPF0000)                                          
                                                                                
             OVRDBF     FILE(EXPSRC) TOFILE(EXPSRCX) MBR(IS928R5)               
             CALL       PGM(IS928R6) PARM(&WKCMPX)                              
             DLTOVR     FILE(*ALL)                                              
                                                                                
             CRTBNDRPG  PGM(FASTBREED/IS928R5) SRCFILE(EXPSRCX) +               
                          DBGVIEW(*LIST)                                        
             MONMSG     MSGID(CPF0000)                                          
                                                                                
             CHKOBJ     OBJ(FASTBREED/IS928R5) OBJTYPE(*PGM)                    
             MONMSG     MSGID(CPF9801) EXEC(GOTO CMDLBL(ENDCLPGM))              
                                                                                
             OVRDBF     FILE(&WKFILX) TOFILE(*LIBL/&WKFILX) MBR(&WKMBRX)        
             CALL       PGM(IS928R5) PARM(&WKCMPX &WKMBRX)                      
             DLTOVR     FILE(*ALL)                                              
                                                                                
ENDCLPGM:    ENDPGM                                                             
