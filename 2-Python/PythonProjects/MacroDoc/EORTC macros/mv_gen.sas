**********************************************************************************************************;
** MACRO MV_GEN                                                                                         **
** written by Jérôme RAPION                                                                             **
**                                                                                                      **
** Creation of global macro variables for each LAB test and its relatives                               **
**                                                                                                      **
** This macro is supposed to be run after INPUT_UNIT and RECOGNITION macros.                            **
** From all the variables identified during RECOGNITION and INPUT_UNIT macros run,                      **
** creation of GLOBAL MACRO VARIABLES for the lab test variables, lln variables, uln variables, etc..   **
**                                                                                                      **
** A first macro variable called NB_TEST will take the value “N” corresponding to the number of rows    **
** into INPUT_VAR dataset (The size of this INPUT_VAR dataset corresponds to the number                 ** 
** of LAB tests variables).                                                                             **
** 1)	A list of macro variables from &var1 to &varN will take the value of all observed value lab     **
** test variables listed into INPUT_VAR dataset.                                                        **
** 2)	A list of macro variables from &lln1 to &llnN will take the value of all LLN variables listed   **
** into INPUT_VAR dataset                                                                               **
** 3)	A list of macro variables from &uln1 to &ulnN will take the value of all ULN variables listed   ** 
** into INPUT_VAR dataset                                                                               **
**                                                                                                      **
** If applicable:                                                                                       **
**                                                                                                      **
** 4)	A list of macro variables from &rvar1 to &rvarN will take the value of all relationship         ** 
** variables listed into INPUT_VAR dataset.                                                             **
** 5)	A list of macro variables from &svar1 to &svarN will take the value of all seriousness          **
** variables listed into INPUT_VAR dataset.                                                             **
** 6)	A list of macro variables from &avar1 to &avarN will take the value of all “action taken”       **
** variables listed into INPUT_VAR dataset.                                                             **
** 7)	A list of macro variables from &root1 to &rootN will take the value of all the root             **
** of all LAB tests listed into INPUT_VAR dataset.                                                      **
** 8)	A list of macro variables from &unit1 to &unitN will take the value of all the root of          **
** all LAB tests listed into INPUT_VAR dataset.                                                         **
**                                                                                                      **
**                                                                                                      **
** Creation date  : 17-11-2010                                                                          **
** Version date   : 23-09-2011                                                                          **
** Software       : SAS version 9.2                                                                     **
** Original author: Jérôme RAPION                                                                       **
** Modified by    : Jérôme RAPION                                                                       **
** macro version : 1.0                                                                                  **
**********************************************************************************************************

PARAMETERS

    DATA              :  Name of the input dataset containing the list of all LAB exams coming 
                         from INPUT_UNIT macro (Required parameter)
**********************************************************************************************************;


%MACRO MV_GEN (data=);

** JRA, 17NOV2010;
** By convention, if UNIT, LLN, ULN, REL, SAE or AT is missing, it takes "_NA" value (not applicable);

data _data;
set &data;
if lln="" then lln="_NA";
if uln="" then uln="_NA";
if REL="" then REL="_NA";
if SAE="" then SAE="_NA";
if AT="" then AT="_NA";
run;

** Creation of NB_TEST global macro variable containing the number of LAB tests (nb of rows into &data);
%global NB_TEST;
data _null_;
set _data end=eof;
N+1;
IF eof THEN CALL SYMPUT('NB_TEST',COMPRESS(N));
run;

** Declaration of global macro variables;
%do i=1 %to &NB_TEST;
	%global var&i lln&i uln&i rvar&i svar&i avar&i root&i unit&i;
%end;

** Creation of VAR&i LLN&i ULN&i RVAR&i SVAR&i AVAR&i ROOT&i UNIT&i global macro variables;
data _null_;
set _data end=eof;
N+1;
	CALL SYMPUT('VAR'||COMPRESS(N),COMPRESS(NAME));
	CALL SYMPUT('LLN'||COMPRESS(N),COMPRESS(LLN));
	CALL SYMPUT('ULN'||COMPRESS(N),COMPRESS(ULN));
	CALL SYMPUT('RVAR'||COMPRESS(N),COMPRESS(REL));
	CALL SYMPUT('SVAR'||COMPRESS(N),COMPRESS(SAE));
	CALL SYMPUT('AVAR'||COMPRESS(N),COMPRESS(AT));
	CALL SYMPUT('ROOT'||COMPRESS(N),COMPRESS(ROOT));
	CALL SYMPUT('UNIT'||COMPRESS(N),COMPRESS(UNIT));
run;


%do i=1 %to &NB_TEST;
    %put root&i: &&root&i ;
    %put lln&i: &&lln&i ;
    %put uln&i: &&uln&i ;
    %put UNIT&i: &&UNIT&i ;
%end;

	proc datasets nolist;
		delete   
		 _DATA 
		  ;
	run;
	quit;


%MEND;

