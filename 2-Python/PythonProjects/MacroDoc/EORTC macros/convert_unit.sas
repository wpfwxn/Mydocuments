
**********************************************************************************************************;
** MACRO CONVERT_UNIT                                                                                   **
** written by Jérôme RAPION                                                                             **
**                                                                                                      **
** Conversion of all the lab test values in a single unit                                               **
**                                                                                                      **
** The purpose of this macro is to convert all lab test values and LLN/ULN values in a single unit      **
** which is SI unit                                                                                     **
** A conversion dictionnary is used to give all conversion factors between usual units and SI units for **
** all lab test exams.                                                                                  ** 
**                                                                                                      **
** The input dataset and the output dataset will be LABLIST dataset                                     **                                                                                                     **
**                                                                                                      **
**                                                                                                      **
** Creation date  : 18-11-2010                                                                          **
** Version date   : 25-10-2011                                                                          **
** Software       : SAS version 9.2                                                                     **
** Original author: Jérôme RAPION                                                                       **
** Modified by    : Jérôme RAPION                                                                       **
** macro version : 1.0                                                                                  **
**********************************************************************************************************

PARAMETERS

    DATA              :  Name of the input dataset containing Hematology and Biochemistry data (Required parameter)

    PERIOD            :  Variable containing the period. The values of this variable will for instance
                         distinguish several period, such as ’Baseline’, ‘On study’ etc. (Required parameter). 

    DATEXAM           :  Variable containing the date of assessment of the laboratory examination (Required parameter)

	VARLIST           :  A vector which contains the ‘ROOT’ names of LAB tests which do not belong to the standard LAB 
						 tests listed in the dictionanary (see appendix 1). (Optional parameter)
						 For instance, TSH variable could be added in the list for a specific analysis but does not 
                         belong to the Dictionanary

**********************************************************************************************************;

%MACRO CONVERT_UNIT (data=,period=,datexam=,varlist=);

%LET NVARLIST=0;
%IF &VARLIST ne "" %then %do;
	** Recuparation of all variables names from VARLIST vector;
	%local j;
	%LET j=1;
	%DO %WHILE(%SCAN(&varlist.,&j.) ne %STR());
	   %LOCAL v&j. ;
		   %LET v&j.=%upcase(%SCAN(&varlist.,&j.));
	   %LET j=%EVAL(&j.+1);

	%END;
	%LET Nvarlist=%EVAL(&j.-1);
%END;

%put &NVARLIST;


%if %sysfunc (fileexist(%STR(K:\SAS\EORTC macros\LABTOX\Dictionnary\CONVERT.sas7bdat))) > 0 %then %do;
libname dico "K:\SAS\EORTC macros\LABTOX\Dictionnary";
%end;

%if %sysfunc (fileexist(%STR(K:\SAS\EORTC macros\LABTOX\Dictionnary\CONVERT.sas7bdat))) = 0 %then %do;
libname dico "C:\SAS\EORTC macros\LABTOX\Dictionnary";
%end;

** JRA, 18NOV2010;
** Importation of UNIT Conversion dictionnary;

data CONVERT;
	set dico.CONVERT;
	ROOT=TEST;
	drop test;
run;

data &data.;
	set &data.;
	root=labtest;
run;

%local ancpct lympct eospct;
data _null_;
	set &data.;
	if LABTEST="ANC" and UNIT="PCT" then do;
	CALL SYMPUT('ancpct',COMPRESS(labtest));
	end;
	if LABTEST="LYM" and UNIT="PCT" then do;
	CALL SYMPUT('lympct',COMPRESS(labtest));
	end;
	if LABTEST="EOS" and UNIT="PCT" then do;
	CALL SYMPUT('eospct',COMPRESS(labtest));
	end;
run;

%put ancpct &ancpct lympct &lympct. eospct &eospct.;

%if &ancpct =ANC %then %do;

	** SPECIFIC case with PCT unit (for ANC) **;
	data _ancpct;
	set &data.;
		if LABTEST="ANC" and UNIT="PCT";

		if &datexam. > .Z;

		keep LABTEST labvalue lln uln unit patid &datexam. &period.;
	run;

	proc sort data=_ancpct nodupkey ;
		by patid &datexam. &period.;
	run;

	data _wbc;
	set &data.;
		if LABTEST="WBC";
		REF_WBC=LABVALUE;
		if &datexam. > .Z;

		keep REF_WBC patid &datexam. &period.;
	run;

	proc sort data=_wbc nodupkey;
		by patid &datexam. &period.;
	run;

	data _ancpct;
		merge _ancpct(in=a) _wbc;
		by patid &datexam. &period.;
		if a;

		oldlabvalue=labvalue;
		oldlln=lln;
		olduln=uln;
		labvalue=REF_WBC*oldlabvalue/100;
		lln=REF_WBC*oldlln/100;
		uln=REF_WBC*olduln/100;

	run;



%sort(data=&data,var=patid labtest unit &datexam. &period. );
%sort(data=_ancpct,var=patid labtest unit &datexam. &period. );

data &data.;
	merge &data.(in=a) _ancpct(in=b);
	by patid labtest unit &datexam. &period. ;
	if a;

	if b then do;
		oldunit=unit;
		UNIT='109L';
	end;

run;

%end;

%if &lympct =LYM %then %do;

	** SPECIFIC case with PCT unit (for ANC) **;
	data _lympct;
	set &data.;
		if LABTEST="LYM" and UNIT="PCT";

		if &datexam. > .Z;

		keep LABTEST labvalue unit lln uln patid &datexam. &period.;
	run;

	proc sort data=_lympct nodupkey;
		by patid &datexam. &period. ;
	run;

	data _wbc;
	set &data.;
		if LABTEST="WBC";
		REF_WBC=LABVALUE;
		if &datexam. > .Z;

		keep REF_WBC patid &datexam. &period. ;
	run;

	proc sort data=_wbc nodupkey;
		by patid &datexam. &period. ;
	run;

	data _lympct;
		merge _lympct(in=a) _wbc;
		by patid &datexam. &period. ;
		if a;

		oldlabvalue=labvalue;
		labvalue=REF_WBC*oldlabvalue/100;
		oldlln=lln;
		olduln=uln;
		lln=REF_WBC*oldlln/100;
		uln=REF_WBC*olduln/100;

	run;



%sort(data=&data,var=patid labtest unit &datexam. &period. );
%sort(data=_lympct,var=patid labtest unit &datexam. &period. );

data &data.;
	merge &data.(in=a) _lympct(in=b);
	by patid labtest unit &datexam. &period. ;
	if a;

	if b then do;
		oldunit=unit;
		UNIT='109L';
	end;

run;

%end;

%if &eospct =EOS %then %do;

	** SPECIFIC case with PCT unit (for EOS) **;
	data _eospct;
	set &data.;
		if LABTEST="EOS" and UNIT="PCT";

		if &datexam. > .Z;

		keep LABTEST labvalue lln uln unit patid &datexam. &period.;
	run;

	proc sort data=_eospct nodupkey ;
		by patid &datexam. &period.;
	run;

	data _wbc;
	set &data.;
		if LABTEST="WBC";
		REF_WBC=LABVALUE;
		if &datexam. > .Z;

		keep REF_WBC patid &datexam. &period.;
	run;

	proc sort data=_wbc nodupkey;
		by patid &datexam. &period.;
	run;

	data _eospct;
		merge _eospct(in=a) _wbc;
		by patid &datexam. &period.;
		if a;

		oldlabvalue=labvalue;
		oldlln=lln;
		olduln=uln;
		labvalue=REF_WBC*oldlabvalue/100;
		lln=REF_WBC*oldlln/100;
		uln=REF_WBC*olduln/100;

	run;



%sort(data=&data,var=patid labtest unit &datexam. &period. );
%sort(data=_eospct,var=patid labtest unit &datexam. &period. );

data &data.;
	merge &data.(in=a) _eospct(in=b);
	by patid labtest unit &datexam. &period. ;
	if a;

	if b then do;
		oldunit=unit;
		UNIT='109L';
	end;

run;

%end;

data &data._varlist;
set &data;
flag=.;
%do i=1 %to &NVARLIST;
COD_&i="&&v&i";

COD_&i=tranwrd(COD_&i, trim(UNIT), "");

if LABTEST =COD_&i then flag=1;

%end;

if flag=1;

run;

data _wbc2;
	set &data.;
		if LABTEST="WBC";
		REF_WBC=LABVALUE;
		if &datexam. > .Z;

		keep REF_WBC patid &datexam. ;
run;

proc sort data=_wbc2 nodupkey;
	by patid &datexam. ;
run;

%sort(data=&data._varlist,var=patid &datexam.);

data _exams;
set &data._varlist;
keep patid &datexam. labtest;
run;

data _exams;
merge _exams(in=a) _wbc2;
		by patid &datexam. ;
		if a;
run;


data &data._varlist;
		merge &data._varlist(in=a) _exams;
		by patid &datexam. ;
		if a;

		length si_unit $5.;

 if unit ='PCT' then do;
		oldlabvalue=labvalue;
		oldlln=lln;
		olduln=uln;
		labvalue=REF_WBC*oldlabvalue/100;
		lln=REF_WBC*oldlln/100;
		uln=REF_WBC*olduln/100;

		si_unit='109L';
		factor=1;
		unit='109/L';

 end;
 else do;
 		oldlabvalue=labvalue;
		oldlln=lln;
		olduln=uln;
		oldunit=unit;
		factor=1;

		labvalue=oldlabvalue*factor;
		lln=oldlln*factor;
		uln=olduln*factor;
 end;

run;



data &data._novarlist;
set &data;
%do i=1 %to &NVARLIST;
COD_&i="&&v&i";

COD_&i=tranwrd(COD_&i, trim(UNIT), "");

if LABTEST =COD_&i then flag=1;

%end;

if flag ne 1;

run;



%sort(data=convert,var=root unit);
%sort(data=&data._novarlist,var=root unit &period. &datexam.);

data &data._novarlist;
	merge &data._novarlist(in=a) convert;
	by root unit;
	if a;

	_unit=UNIT;

	length oldunit $10. ;

	if NOT((labtest="ANC" and oldunit='PCT') OR (labtest="LYM" and oldunit='PCT')
	OR  (labtest="EOS" and oldunit='PCT')) then do;

		oldlabvalue=labvalue;
		oldlln=lln;
		olduln=uln;
		oldunit=lab_unit;
		unit=lab_si_unit;

		labvalue=oldlabvalue*factor;
		lln=oldlln*factor;
		uln=olduln*factor;

	end;

	if (labtest="ANC" and oldunit='PCT') OR (labtest="LYM" and oldunit='PCT') 
		OR (labtest="EOS" and oldunit='PCT') then do;
		si_unit='109L';
		factor=1;
		unit='109/L';
	end;

	drop root lab_si_unit lab_unit;

	format unit oldunit $10.;

run;

data _out _no_unit;
set &data._novarlist;
if UNIT ne "" then output _out;
if UNIT="" then output _no_unit;
run;

data _no_unit;
set _no_unit;
_VARLIST=0;
%do i=1 %to &Nvarlist.;
if LABTEST="&&v&i" then _VARLIST=1;
%end;
labvalue=oldlabvalue;
run;

%sort(data=_no_unit,var=labtest patid);

data _no_unit;
set _no_unit;
by labtest patid;
if first.labtest then ind=1;
else ind+1;
run;

data &data;
set &data._varlist _out ;
drop %IF &NVARLIST ne 0 %THEN COD_: ; flag root;
run;


%sort(data=&data,var=patid labtest unit &period. &datexam.);

	proc datasets nolist;
		delete   
		 _ancpct _lympct _eospct _out _wbc convert &data._varlist &data._novarlist _exams  _wbc2
		  ;
	run;
	quit;



%MEND;

