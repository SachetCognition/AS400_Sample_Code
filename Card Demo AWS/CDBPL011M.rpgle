      *******************************************************************
      *  Program      : CDBPL011R                                       *
      *  Author       : Subu                                            *
      *  Date         : 05/31/2024                                      *
      *  Description  : Batch Authorization                             *
      *******************************************************************

     H*DftActGrp(*No) ActGrp('QILE')
     H Option(*NoDebugIO:*SrcStmt)

     FCDTRNR02  UF   E           K Disk

     FCDCCMR01  IF   E           K Disk    Prefix(CCM_)

     FCDACTR04  UF   E           K Disk    Prefix(ACT_)

     FCDAUTU01  O    E           K Disk    Prefix(AUT_)
          **********************************************************************
          * EXPANDED FROM: COPYBPLAUT
          **********************************************************************
          **********************************************************************
          * EXPANDED FROM: COPYPSDS
          **********************************************************************

     DMYPSDS          SDS
     D PROC_NAME         *PROC
     D PGM_STATUS        *STATUS
     D PRV_STATUS             16     20S 0
     D LINE_NUM               21     28
     D ROUTINE           *ROUTINE
     D PARMS             *PARMS
     D EXCP_TYPE              40     42
     D EXCP_NUM               43     46
     D PGM_LIB                81     90
     D EXCP_DATA              91    170
     D EXCP_ID               171    174
     D DATE                  191    198
     D YEAR                  199    200S 0
     D LAST_FILE             201    208
     D FILE_INFO             209    243
     D JOB_NAME              244    253
     D USER                  254    263
     D JOB_NUM               264    269S 0
     D JOB_DATE              270    275S 0
     D RUN_DATE              276    281S 0
     D RUN_TIME              282    287S 0
     D CRT_DATE              288    293
     D CRT_TIME              294    299
     D CPL_LEVEL             300    303
     D SRC_FILE              304    313
     D SRC_LIB               314    323
     D SRC_MBR               324    333
     D PROC_PGM              334    343
     D PROC_MOD              344    353
     D SRC_ID_1              354    355B 0
     D SRC_ID_2              356    357B 0
     D CUR_USER              358    367
     D EXT_ERR               368    371I 0
     D XML_JSON              372    379I 0
     D INT_JOBID             380    395
     D SYS_NAME              396    403


          **********************************************************************
          * END OF COPYBOOK
          **********************************************************************
          **********************************************************************
          * EXPANDED FROM: COPYQCMDEX
          **********************************************************************

     ** Prototype for calling QCMDEXC
     D QCmdPrcs        PR                  ExtPgm('QCMDEXC')
     D  Cmd                        3000    Options (*VarSize) Const
     D  CmdLen                       15P 5 Const
     D  Filler                        3    Options (*NoPass) Const


          **********************************************************************
          * END OF COPYBOOK
          **********************************************************************

     D GenAUT#         PR            18  0
     D  Generated#                   18  0


          **********************************************************************
          * END OF COPYBOOK
          **********************************************************************

     D $               PR                  EXTPGM('CDBPL011R')
     D $EngTrnSts                     1

     D                 PI
     D $EngTrnSts                     1

     D SubmitSts       C                   's'
     D RejectedSts     C                   'R'
     D ApprovedSts     C                   'A'
     D SubmitRevSts    C                   'a'
     D ReversedSts     C                   'X'
     D*InactRevSts     C                   'I'
     D ErrorSts        C                   '$'
     D Delay           S              2    Inz('01')
     D Dly             S           3000    Varying
     D GeneratedAuth   S                   Like(AUT_AUTHID)
     D @CT             S             30    Dim(7) CTData PerRcd(1)

      ************************************************************************
      * Main Line                                                            *
      ************************************************************************

       Setll ($EngTrnSts) CDTRNR02;

       DoW Not %ShtDn;
         ReadE(E) ($EngTrnSts) CDTRNR02;
         If %Error();
           // Rcd Lck
         ElseIf %Eof();
           Sleep();
           Setll ($EngTrnSts) CDTRNR02;
         ElseIf %Found();
           TRNCHGDT = %Dec(%Date():*CYMD);
           TRNCHGTM = %Dec(%Time());
           TRNCHGUSR = User;
           TRNCHGPGM = Proc_Name;
           ExSR Process_TRN;
         EndIf;
       EndDo;

       Return;

      ************************************************************************
      * Subroutines and Procs                                                *
      ************************************************************************

       BegSR *InzSr;

         If $EngTrnSts <> SubmitSts and $EngTrnSts <> SubmitRevSts;
           Return;
         EndIf;

         AUT_AUTHUSER = User;
         AUT_AUTHPGM = Proc_Name;

       EndSR;

       BegSR Process_TRN;

         AUT_FRAUDFLAG = *Blanks;
         AUT_RSPREASON = *Blanks;
         TRNSTATUS = RejectedSts;
         GenAUT#(GeneratedAuth);

         Chain (CARDNUM) CDCCMR01;
         If Not %Found();
           AUT_RSPREASON = @CT(1);
           ExSR WrtAUT_UpdTRN_UpdACT;
           LeaveSR;
         EndIf;

         If $EngTrnSts = SubmitSts;
           If CCM_STATUS <> 'A';
             If CCM_STATUS = 'H';
               AUT_FRAUDFLAG = 'Y';
             EndIf;
             AUT_RSPREASON = @CT(2);
             ExSR WrtAUT_UpdTRN_UpdACT;
             LeaveSR;
           Else;
             If (CCM_EXPIRYYR < %SubDt(ORIGDATE:*Years)) or
                ((CCM_EXPIRYYR = %SubDt(ORIGDATE:*Years)) and
                (CCM_EXPIRYMO < %SubDt(ORIGDATE:*Months)));
                  AUT_RSPREASON = @CT(3);
                  ExSR WrtAUT_UpdTRN_UpdACT;
                  LeaveSR;
             EndIf;
             Chain(NE) ('A':CCM_ACCTNUM) CDACTR04;
             If Not %Error() and %Found() and ACT_EXPIRYDATE >= PROCDATE;
               // continue
             Else;
               AUT_RSPREASON = @CT(4);
               ExSR WrtAUT_UpdTRN_UpdACT;
               LeaveSR;
             EndIf;
           EndIf;
         TRNSTATUS = ApprovedSts;
         Else;                                // SubmitRevSts
           TRNSTATUS = ReversedSts;
         EndIf;

         CalcUpd$Bal();
         ExSR WrtAUT_UpdTRN_UpdACT;

       EndSR;

       BegSR WrtAUT_UpdTRN_UpdACT;

         AUT_AUTHID = GeneratedAuth;
         AUT_TRNID = TRNID;
         AUT_CARDNUM = CARDNUM;
         //AUT_RSPREASON = CN(1);     //move up
         AUT_AUTHDATE = %Date();
         AUT_AUTHTIME = %Time();

         If TRNSTATUS = ErrorSts;
           AUT_RESPONSE = 'F';
         ElseIf TRNSTATUS = RejectedSts;
           AUT_RESPONSE = 'P';
         Else;
           AUT_RESPONSE = 'P';
           Update CDACTPR;
         EndIf;

         Write CDAUTPR;

         Update CDTRNPR;

       EndSR;

       BegSR RcdLck_TRN;

         *IN60 = *On;
         DoW *IN60 = *On;
           Setll ($EngTrnSts) CDTRNR02;
           ReadE(E) ($EngTrnSts) CDTRNR02;
           If Not %Error();
             *IN60 = *Off;
           EndIf;
         EndDo;

       EndSR;

       Dcl-Proc Sleep;

         Dly = 'DLYJOB DLY(' +%CHAR (DELAY) + ')';
         QCmdPrcs (Dly:%Len(Dly));

       End-Proc;

       Dcl-Proc CalcUpd$Bal;

         Dcl-Pi *n End-Pi;

         Dcl-S wkAmount Packed(20:5);
         Dcl-S wkCurDebit Like(ACT_CURDEBIT);
         Dcl-S wkCurCredit Like(ACT_CURCREDIT);

         *IN60 = *On;
         DoW *IN60 = *On;
           Chain(E) ('A':CCM_ACCTNUM) CDACTR04;
           If Not %Error() and %Found();
             *IN60 = *Off;
           EndIf;
         EndDo;

         If TRNSTATUS = ApprovedSts;
           wkAmount = ACT_BALANCE + AMOUNT;
           If wkAmount > ACT_CREDLIMIT;
            Unlock CDACTR04;
            AUT_RSPREASON = @CT(5);
            TRNSTATUS = RejectedSts;
            Return;
           EndIf;
           Monitor;
           wkCurDebit = ACT_CURDEBIT + AMOUNT;
           On-Error;
              Unlock CDACTR04;
              AUT_RSPREASON = @CT(6);
              TRNSTATUS = ErrorSts;
              Return;
           EndMon;
           ACT_CURDEBIT = wkCurDebit;
           ACT_BALANCE = ACT_CURDEBIT - ACT_CURCREDIT;
         Else;
           Monitor;
           wkCurCredit = ACT_CURCREDIT + AMOUNT;
           On-Error;
              Unlock CDACTR04;
              AUT_RSPREASON = @CT(7);
              TRNSTATUS = ErrorSts;
              Return;
           EndMon;
           ACT_CURCREDIT = wkCurCredit;
           ACT_BALANCE = ACT_CURDEBIT - ACT_CURCREDIT;
         EndIf;

       End-Proc;

** @CT
CREDIT CARD NOT FOUND
CREDIT CARD NOT ACTIVE
CREDIT CARD EXPIRED
ACCOUNT EXPIRED OR INACTIVE
CREDIT LIMIT EXCEEDED
IT ERROR - CURDEBIT OVERFLOW
IT ERROR - CURCREDIT OVERFLOW
