/*********************************************************************/         
/* FastBreed Technologies, Inc. © 2000                               */         
/*********************************************************************/         
/*********************************************************************/         
/**                                                                 **/         
/** OBJECT ID: IS625C4A                                             **/         
/**                                                                 **/         
/** OBJECT DESCRIPTION: GET USER PROFILE DESCRIPTION                **/         
/**                                                                 **/         
/**  MOD#   DATE   PGMR  DESCRIPTION                                **/         
/**  ----  ------  ----  -----------------------------------------  **/         
/**                                                                 **/         
/*********************************************************************/         
                                                                                
             PGM        PARM(&WKUSRX &WKDESX)                                   
                                                                                
             DCL        VAR(&WKUSRX)  TYPE(*CHAR)   LEN(10)                     
             DCL        VAR(&WKDESX)  TYPE(*CHAR)   LEN(50)                     
                                                                                
             RTVUSRPRF  USRPRF(&WKUSRX) TEXT(&WKDESX)                           
             MONMSG     MSGID(CPF0000)                                          
                                                                                
             ENDPGM                                                             
