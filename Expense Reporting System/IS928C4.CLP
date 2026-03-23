/*********************************************************************/         
/** FastBreed Technologies, Inc. © 2000                              */         
/*********************************************************************/         
/*********************************************************************/         
/**                                                                 **/         
/** OBJECT ID: IS928C4                                              **/         
/**                                                                 **/         
/** OBJECT DESCRIPTION: EMPLOYEE MASTER MAINTENANCE - LOAD FILE     **/         
/**                                                                 **/         
/** MOD#   DATE  PGMR DESCRIPTION                                   **/         
/** ----  ------ ---- --------------------------------------------  **/         
/**                                                                 **/         
/*********************************************************************/         
             PGM        PARM(&WKFILX &WKMBRX &WKGOOD)                           
             DCL        VAR(&WKFILX) TYPE(*CHAR) LEN(10)                        
             DCL        VAR(&WKMBRX) TYPE(*CHAR) LEN(10)                        
             DCL        VAR(&WKGOOD) TYPE(*CHAR) LEN(1) VALUE('Y')              
                                                                                
                                                                                
             /* VALIDATE CURRENT EMPLOYEE FILE */                               
             CHKOBJ     OBJ(&WKFILX) OBJTYPE(*FILE) MBR(&WKMBRX)                
             MONMSG     MSGID(CPF9801) EXEC(DO)                                 
                        CHGVAR     VAR(&WKGOOD) VALUE('N')                      
                        GOTO       ENDCLPGM                                     
                        ENDDO                                                   
                                                                                
                                                                                
ENDCLPGM:    ENDPGM                                                             
