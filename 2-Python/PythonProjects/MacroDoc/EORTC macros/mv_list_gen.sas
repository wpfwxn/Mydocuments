**********************************************************************************************************;
** MACRO MV_LIST_GEN                                                                                    **
** written by Jérôme RAPION                                                                             **
**                                                                                                      **
** Creation of global macro variables lists which will contain the name of all worst grade variable     **
** per lab exam and per period                                                                          **
**                                                                                                      **
** The convention name of the macro variable list is LABLIST_<value of the period>                      **
**                                                                                                      **
** For instance, for baseline, the macro variable LABLIST_B is created and it will contain the name of  **
** all worst grade variable at baseline:  ANC_B WBC_B PLT_B ALT_B etc... inside.                        **
** For on-study period, the macro variable LABLIST_O is created and it will contain the name of all     **
** worst grade variable durinf on-study period:  ANC_O WBC_O PLT_O ALT_O etc... inside.                 **
** For cycle1 period, the macro variable LABLIST_C1 is created and it will contain the name of all      **
** worst grade variable during cycle 1 period:  ANC_C1 WBC_C1 PLT_C1 ALT_C1 etc... inside.              **
**                                                                                                      **
** These macro variable lists could then be used into PROC FREQ or %TABLES2 statement, avoiding to list **                                                                                                      **
** all variables for each LAB test per period (see NOTES)                                               **
**                                                                                                      **
** Creation date  : 12-01-2011                                                                          **
** Version date   : 09-12-2011                                                                          **
** Software       : SAS version 9.2                                                                     **
** Original author: Jérôme RAPION                                                                       **
** Modified by    : Jérôme RAPION                                                                       **
** macro version : 1.0                                                                                  **
**********************************************************************************************************

PARAMETERS

    DATA              :  Name of the input dataset containing Hematology and Biochemistry data coming 
                         from COMPUTE_CTC_GRADE macro (Required parameter)

    PERIOD            :  Variable containing the period. The values of this variable will for instance
                         distinguish several period, such as ’Baseline’, ‘On study’ etc. (Required parameter). 

**********************************************************************************************************
NOTES
**********************************************************************************************************

 EXAMPLES:

	After running this macro, it is possible to use the macro variable lists into PROC FREQ or %TABLES2 statement:

	For instance:

	proc freq data=patient
	tables (&lablist_B)*trt1/missing
	tables (&lablist_C1)*trt1/missing
	tables (&lablist_O)*trt1/missing
	run

 	or 

	%Tables2(data=patient,var=&lablist_B,by=trt1,total=Y,ODS=Y) 
	
**********************************************************************************************************;


%MACRO MV_LIST_GEN (data=,period=);

proc sort data=&data out=list_test(keep=labtest &period) nodupkey;
by labtest &period.;
run;

data list_test_ALL;
set list_test;
format &period.;
TEST=compress(LABTEST)!!compress(&period.);
run;


%let _per=%upcase(&period.);

proc contents data=&data noprint out=_var;
run;

data _null_;
	set _var;
	format=upcase(format);
	name=upcase(name);

	format=TRANWRD(format,"$","");

	if name="&_per.";
	CALL SYMPUT('f_per',COMPRESS(format));
run;

proc format cntlout=_format;
run;

data _format;
set _format;
if FMTNAME="&f_per.";
run;


data _null_;
set _format end=eof;
N+1;
	CALL SYMPUT('PER'||COMPRESS(N),COMPRESS(START));
	IF eof THEN CALL SYMPUT('NB_PER',COMPRESS(N));

run;

data _format;
set _format;
IND=_n_;
&period.=START;
run;
%sort(data=_format,var=&period.);
%sort(data=list_test_ALL,var=&period. labtest);

data list_test_ALL;
merge list_test_ALL _format(keep=&period. ind);
by &period.;
run;


%global NB_EXAMS;
%let NB_EXAMS=;


proc sort data=&data out=list_test(keep=labtest) nodupkey;
by labtest;
run;


data _null_;
set list_test end=eof;
N+1;
IF eof THEN CALL SYMPUT('NB_EXAMS',COMPRESS(N));
run;

%do k=1 %to &NB_PER;
	%do i=1 %to &NB_EXAMS;

	%local TEST&K._&i.;
	%let TEST&K._&i. =;
	%end;
%end;

%do l=1 %to &NB_PER;

	data _null_;
	set list_test_ALL end=eof;
	if ind=&L.;
	N+1;
		CALL SYMPUT("TEST&L"||'_'||COMPRESS(N),COMPRESS(TEST));
	run;

%end;

%do k=1 %to &NB_PER;
	%global lablist&&PER&k.;
	%let lablist&&PER&k.=.;

	%let a=;
	%do i=1 %to &NB_EXAMS;
	      %let a=%trim(&a.) &&test&k._&i;
	%end;

	%let lablist&&PER&k.=&a.;

%end;

%do k=1 %to &NB_PER;
%put LIST &K.:;
%put lablist&&PER&k.: &&&&lablist&&PER&k. ;%put; %put;
%end;

	proc datasets nolist;
		delete  
		LIST_TEST
		LIST_TEST_ALL
		_FORMAT
		_VAR
;
	run;
	quit;


%MEND;

