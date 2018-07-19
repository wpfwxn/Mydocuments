**********************************************************************************************************;
** MACRO RECOGNITION                                                                                    **
** written by Jérôme RAPION                                                                             **
**                                                                                                      **
** Scanning of all variables names and checking                                                         **
**                                                                                                      **
** This macro scans all the variables entered into DATA parameter                                       **
** and compares these variables names to the list of all possible LAB test variables following          **
** the DM standard naming (see LAB dictionnary excel file)                                              **
**                                                                                                      **
** This macro compares all variable names to the list of variables included                             **
** into VAR_LIST and identify the variables which do not fit                                            **
** This check concerns LAB test variables and also LLN and ULN LAB variables,                           **
** and if applicable relationship, seriousness and action taken.                                        **
**                                                                                                      **
** A first listing will be produced with the first check and a second one with the second check.        **                                                          **
**                                                                                                      **
** A dataset called INPUT_VAR will be created with :                                                    **
** - a column for the name of observed value lab test variable (according to DM standard naming): NAME, **
** - a column for the label of the observed value lab test variable: LABEL,                             **
** - a column for LLN variable: LLN,                                                                    **
** - a column for the ULN variable: ULN,  and if applicable:                                            **
** - a column for relationship: REL,                                                                    **
** - a column for seriousness: SAE ,                                                                    **
** - a column for action taken according: AT.                                                           **
**                                                                                                      **
** All these variables will be identified thanks to the suffix of these variables from                  **
** the standard naming conventions and put in the right column: LLN, ULN, SAE, etc…                     **
** Another variable containing the root of all LAB tests (taken from LAB dictionnary excel file)        **
** is also added to INPUT_VAR dataset (for instance ANC, WBC, PLT, AST, ALT, ALP, etc...).              **
**                                                                                                      **
** Creation date  : 10-11-2010                                                                          **
** Version date   : 09-09-2011                                                                          **
** Software       : SAS version 9.2                                                                     **
** Original author: Jérôme RAPION                                                                       **
** Modified by    : Jérôme RAPION                                                                       **
** macro version : 1.0                                                                                  **
**********************************************************************************************************

PARAMETERS

    DATA              :  Name of the input dataset containing Hematology and Biochemistry data (Required parameter)
    VARLIST           :  A vector which contains the ‘ROOT’ names of LAB tests which do not belong to the standard LAB 
						 tests listed in the dictionanary (see appendix 1). (Optional parameter)
						 For instance, TSH variable could be added in the list for a specific analysis but does not 
                         belong to the Dictionanary

    KEEPLIST          :  A vector which lists all the variable names the user would like to add in the OUTPUT dataset. 
						 It could be identification variables or flag that could be useful for the analysis that the 
						 statistician would like to keep in the OUTPUT dataset 
					     (For instance, date of assessment, cycle number, treatment arm, flags for ITT, PP analysis, etc.)
                         (Optional parameter). Example: KEEPLIST=CYCLE TRT1 ITT
 	
    PERIOD            :  Name of the variable containing the period. The values of this variable will for instance
                         distinguish several period, such as ’Baseline’, ‘On study’ etc. (Required parameter). 

**********************************************************************************************************;


%MACRO RECOGNITION (data=,varlist=,keeplist=,period=);

	* Importation of LAB dictionnary;
	** If the user works with a laptop import from C:\SAS. Otherwise from K:\**;
	%if %sysfunc (fileexist(%STR(K:\SAS\EORTC macros\LABTOX\Dictionnary\lab_dictionnary.sas7bdat))) > 0 %then %do;
	libname dico "K:\SAS\EORTC macros\LABTOX\Dictionnary";
	%end;
	
	%if %sysfunc (fileexist(%STR(K:\SAS\EORTC macros\LABTOX\Dictionnary\lab_dictionnary.sas7bdat))) = 0 %then %do;
	libname dico "C:\SAS\EORTC macros\LABTOX\Dictionnary";
	%end;

	data lab_dictionnary;
		set dico.lab_dictionnary;
	run;

	* Macro used to seperate words in several macro variables (&KEEPLIST) ;
	* from a SUITE of words (in SEP) seperated by a seperator SEP ;
	* and to count the number of words (in N) - JRA, Oct2006*;

	%MACRO RCHV(SUITE=,COMPTE=,SEP= ,N=);
		%LOCAL NUMERO MOT;
		%LET NUMERO=1;
		%LET MOT=%NRBQUOTE(%SCAN(%STR(&SUITE),&NUMERO,%STR(&SEP)));

		%DO %WHILE(&MOT ^=);
		%GLOBAL PV_&NUMERO &COMPTE&NUMERO;
		%LET &COMPTE&NUMERO=%TRIM(&MOT);
		%LET PV_&NUMERO=%SUBSTR(&MOT,1,1);
		%LET NUMERO=%EVAL(&NUMERO+1);
		%LET MOT=%NRBQUOTE(%QSCAN(%STR(&SUITE),&NUMERO,%STR(&SEP)));

		%PUT MOT : &MOT;


		%END;
		%GLOBAL &N;
		%LET &N=%EVAL(&NUMERO-1);
	%MEND RCHV;

	** -> creation of n macro variables called KEEPx containing the content of &KEEPLIST vector;
	%RCHV(SUITE=&KEEPLIST.,COMPTE=KEEP,SEP=" ",N=NKEEP);


	** Recuparation of all variables names from VARLIST vector;
	%put &VARLIST.;
	%IF &VARLIST ne "" %then %do;
		** Recuparation of all variables names from VARLIST vector;
		%local j;
		%LET j=1;
		%DO %WHILE(%SCAN(&varlist.,&j.) ne %STR());
		   %LOCAL w&j. ;

		   %LET w&j.=%upcase(%SCAN(&varlist.,&j.));
		   %LET j=%EVAL(&j.+1);

		%END;
		%LET Nvarlistb=%EVAL(&j.-1);
	%END;


	** Recuperation of all variable names and labels from INPUT dataset;
	proc contents data=&data out=_contentlab(drop= flags) noprint;
	run;

	data _contentlab;
		set _contentlab;

		name=upcase(name);
		if name='PATID' then delete;
		if name=:'FORMDATE_' then delete;
		if name=:'TXLAB' then delete;
		if name=:'LAB' then delete;
		if name="&PERIOD." then delete;

		%do i=1 %to &NKEEP;
			if name=upcase("&&KEEP&I") then delete;
		%end;
		
		length=length(name);
		l=length-2;
		ll=length-3;

		* Handling of suffixes;
		m=length-1;
		if m > 0 then do;
			suffix2= substr(name,m,2);
		end;

		mm=length-2;
		if mm > 0 then do;
			suffix3= substr(name,mm,3);
		end;

		suffix1= substr(name,length,1);

		if suffix1 ='R' then flag_r=1;
		else flag_r=0;

		if UPCASE(name) = 'PTINR' then flag_r=0;
		if UPCASE(name) = 'GFR' then flag_r=0;

		if suffix1 ='S' then flag_s=1;
		else flag_s=0;
		if UPCASE(name) = 'EOS' then flag_s=0;

		if suffix2 ='UL' then flag_ul=1;
		else flag_ul=0;

		if suffix2 ='LL' then flag_ll=1;
		else flag_ll=0;

		if suffix2 ='AT' then flag_at=1;
		else flag_at=0;

		if suffix3 in ('UML' 'MDL' 'MML' 'UML' 'GDL' 'PCT' 'UKL' 'MEL') then flag_unit=1;
		else flag_unit=0;

		flag=sum(of flag:);

		%do i=1 %to &Nvarlistb;
			if NAME = "&&w&i" then flag=0;
		%end;

		if flag_ul=1 then do;
			_name=substr(name,1,l);
		end;
		if flag_ll=1 then do;
			_name=substr(name,1,l);
		end;
		if flag_at=1 then do;
			_name=substr(name,1,l);
		end;
		if flag_r=1 then do;
			_name=substr(name,1,m);
		end;
		if flag_s=1 then do;
			_name=substr(name,1,m);
		end;
		if flag_unit=1 then do;
			_name=substr(name,1,ll);
		end;

		keep name label format suffix: flag: length _name;
		;
	run;

	data _NAME ;
		set _contentlab;
		where flag=0;
		ORIG=NAME;
		keep name label ORIG; 
	run;
	%sort(data=_name,var=name);

	data _NAME2 ;
		set _contentlab;
		ORIG=NAME;
		where flag_unit=1;
		keep name label _name suffix3 ORIG;
	run;
	%sort(data=_name2,var=name);

	data _LL ;
		set _contentlab;
		where flag_ll=1;
		ORIG=NAME;
		LLN=name;
		name=_name;

		%do i=1 %to &Nvarlistb;
			if NAME = "&&w&i" then flag_ll=0;
		%end;
		keep lln name ORIG;
	run;
	%sort(data=_ll,var=name);

	data _UL ;
		set _contentlab;
		where flag_ul=1;
		ORIG=NAME;
		ULN=name;
		name=_name;

		%do i=1 %to &Nvarlistb;
			if NAME = "&&w&i" then flag_ul=0;
		%end;
		keep uln name ORIG;
	run;
	%sort(data=_ul,var=name);

	data _AT ;
		set _contentlab;
		where flag_at=1;
		ORIG=NAME;
		AT=name;
		name=_name;
		keep at name ORIG;
	run;
	%sort(data=_at,var=name);

	data _REL ;
		set _contentlab;
		where flag_r=1;
		ORIG=NAME;
		REL=name;
		name=_name;

		out=0;
		%do i=1 %to &Nvarlistb;
			if ORIG = "&&w&i" then out=1;
		%end;

		keep rel name ORIG out;
	run;
	%sort(data=_rel,var=name);

	data _SAE ;
		set _contentlab;
		where flag_s=1;
		ORIG=NAME;
		SAE=name;
		name=_name;

		out=0;
		%do i=1 %to &Nvarlistb;
			if ORIG = "&&w&i" then out=1;
		%end;


		keep sae name ORIG out;
	run;
	%sort(data=_sae,var=name);

	* Merge of all variable names (NAME LLN ULN REL SAE AT) into an INPUT_VAR dataset;
	* This dataset will contain all useful information to analyze LAB data;
	data INPUT_VAR;
		merge _ll _ul _rel _sae _at _name _name2 ;
		by name;
		unit_ext=suffix3;
		NAME_UNIT=name;
		LLN_UNIT=lln;
		ULN_UNIT=uln;
		REL_UNIT=rel;
		SAE_UNIT=sae;
		AT_UNIT=at;

		drop suffix3;
		if unit_ext ne "" then	do;
			name=_name;

			if lln ne "" then do;
				lln_unit=lln;
 				lln=compress(tranwrd(lln, compress(unit_ext), ""));
			end;
			if uln ne "" then do;
				uln_unit=uln;
 				uln=compress(tranwrd(uln, compress(unit_ext), ""));
			end;
			if rel ne "" then do;
				rel_unit=rel;
 				rel=compress(tranwrd(rel, compress(unit_ext), ""));
			end;
			if sae ne "" then do;
				sae_unit=sae;
 				sae=compress(tranwrd(sae, compress(unit_ext), ""));
			end;
			if at ne "" then do;
				at_unit=at;
 				at=compress(tranwrd(at, compress(unit_ext), ""));
			end;
		end;

		if out=1 then delete;

		drop _name unit_ext out;
	run;

	** Creation of several macro variables (CODEi, LLNi, ULNi, SAEi, RELi, ATi) from the LAB dictionnary;
	data _null_;
		set Lab_dictionnary END=FINAL;
		N+1;
		CALL SYMPUT('CODE'||COMPRESS(N),COMPRESS(code));
		CALL SYMPUT('LLN'||COMPRESS(N),COMPRESS(derived1));
		CALL SYMPUT('ULN'||COMPRESS(N),COMPRESS(derived2));
		CALL SYMPUT('SAE'||COMPRESS(N),COMPRESS(derived3));
		CALL SYMPUT('REL'||COMPRESS(N),COMPRESS(derived4));
		CALL SYMPUT('AT'||COMPRESS(N),COMPRESS(derived5));
		IF FINAL THEN CALL SYMPUT('NB',COMPRESS(N));
	run;


	DATA check;
	length _sugg $50.;
		set input_var;

		* Check if the name of the observed value lab test variable from INPUT_VAR exists in the LAB dictionnary;
		_c=1;
		%do i=1 %to &nb;
			if name = "&&code&i" then _c=0;
		%end;

		* Check if the name of the LLN variable from INPUT_VAR exists in the LAB dictionnary;
		if lln ne "" then do;
		_l=1;
		%do i=1 %to &nb;
			if lln = "&&lln&i" then _l=0;
		%end;
		end;

		* Check if the name of the ULN variable from INPUT_VAR exists in the LAB dictionnary;
		if uln ne "" then do;
		_u=1;
		%do i=1 %to &nb;
			if uln = "&&uln&i" then _u=0;
		%end;
		end;

		* Check if the name of the variable for relationship from INPUT_VAR exists in the LAB dictionnary;
		if rel ne "" then do;
		_r=1;
		%do i=1 %to &nb;
			if rel = "&&rel&i" then _r=0;
		%end;
		end;

		* Check if the name of the variable for seriousness from INPUT_VAR exists in the LAB dictionnary;
		if sae ne "" then do;
		_s=1;
		%do i=1 %to &nb;
			if sae = "&&sae&i" then _s=0;
		%end;
		end;

		* Check if the name of the variable for action taken from INPUT_VAR exists in the LAB dictionnary;
		if at ne "" then do;
		_a=1;
		%do i=1 %to &nb;
			if at = "&&at&i" then _a=0;
		%end;
		end;

		** if checkdico >0 then it means that one of all variable names from
		NAME LLN ULN REL SAE AT does not exist in the dictionnary;

		checkdico=sum(of _:);
	    checklist1=0;
	    checklist2=0;
		checklist=0;

		lengthorig=length(orig);

		%do i=1 %to &nb;
		** Suggested names Based on beginning of... **;
			if NAME =: "&&code&i" then _sugg="&&code&i";
		** Suggested units **;
		%end;

		if lengthorig>1 then twolast=SUBSTR(ORIG,lengthorig-1);

		if twolast in ('LL' 'UL') then _sll=compress(twolast);
		else _sll='';

		if index(ORIG,"UML")>0 then _sunit="UML";
		if index(ORIG,"GL")>0 then _sunit="GL";
		if index(ORIG,"GDL")>0 then _sunit="GDL";
		if index(ORIG,"MML")>0 then _sunit="MML";
		if index(ORIG,"MDL")>0 then _sunit="MDL";
		if index(ORIG,"PCT")>0 then _sunit="PCT";

		label _suggunit="Name suggested";
		label ORIG="Original name";

		** Suggested names Based on labels **;
		if index(upcase(LABEL),"ALBUMIN")>0 then _sugg="ALB";
		if index(upcase(LABEL),"ALKALIN")>0 then _sugg="ALK";
		if index(upcase(LABEL),"AMYLASE")>0 then _sugg="AMY";
		if index(upcase(LABEL),"BICARBONATE")>0 then _sugg="BICA";
		if index(upcase(LABEL),"CALCIUM")>0 then _sugg="CA";
		if index(upcase(LABEL),"CTNT")>0 then _sugg="CTNT";
		if index(upcase(LABEL),"CD4")>0 then _sugg="CD4";
		if index(upcase(LABEL),"CHLORIDE")>0 then _sugg="CHL";
		if index(upcase(LABEL),"CPK")>0 then _sugg="CPK";
		if index(upcase(LABEL),"PHOSPHOKINASE")>0 then _sugg="CPK";
		if index(upcase(LABEL),"EOSINOPHILS")>0 then _sugg="EOS";
		if index(upcase(LABEL),"FIB")>0 then _sugg="FIBRI";
		if index(upcase(LABEL),"GLOM")>0 then _sugg="GFR";
		if index(upcase(LABEL),"GFR")>0 then _sugg="GFR";
		if index(upcase(LABEL),"GGT")>0 then _sugg="GGT";
		if index(upcase(LABEL),"GLUCOSE")>0 then _sugg="GLU";
		if index(upcase(LABEL),"HEMATOCRIT")>0 then _sugg="HCT";
		if index(upcase(LABEL),"HEMOGLOBIN")>0 then _sugg="HB";
		if index(upcase(LABEL),"HB")>0 then _sugg="HB";
		if index(upcase(LABEL),"INR")>0 then _sugg="INR";
		if index(upcase(LABEL),"LDH")>0 then _sugg="LDH";
		if index(upcase(LABEL),"LIPASE")>0 then _sugg="LIP";
		if index(upcase(LABEL),"LYMPHO")>0 then _sugg="LYM";
		if index(upcase(LABEL),"MAGNESIUM")>0 then _sugg="MG";
		if index(upcase(LABEL),"NEUTRO")>0 then _sugg="ANC";
		if upcase(LABEL)="ANC" then _sugg="ANC";
		if index(upcase(LABEL),"PLAT")>0 then _sugg="PLT";
		if index(upcase(LABEL),"POTASSIUM")>0 then _sugg="K";
		if upcase(LABEL)="K" then _sugg="K";
		if index(upcase(LABEL),"K+")>0 then _sugg="K";
		if index(upcase(LABEL),"QTC")>0 then _sugg="QTC";
		if index(upcase(LABEL),"PROTHROMBIN")>0 then _sugg="PT";
		if index(upcase(LABEL),"PHOSPHATE")>0 then _sugg="PO";
		if upcase(LABEL)="PT" then _sugg="PT";
		if upcase(LABEL)="PTT" then _sugg="PTT";
		if index(upcase(LABEL),"THROMBOPLASTIN")>0 then _sugg="PTT";
		if index(upcase(LABEL),"RBC")>0 then _sugg="RBC";
		if index(upcase(LABEL),"RED")>0 then _sugg="RBC";
		if index(upcase(LABEL),"CREA")>0 then _sugg="CRE";
		if index(upcase(LABEL),"ASAT")>0 then _sugg="AST";
		if upcase(LABEL)="AST" then _sugg="NA";
		if index(upcase(LABEL),"SGOT")>0 then _sugg="AST";
		if index(upcase(LABEL),"ALAT")>0 then _sugg="ALT";
		if upcase(LABEL)="ALT" then _sugg="NA";
		if index(upcase(LABEL),"SGPT")>0 then _sugg="ALT";
		if index(upcase(LABEL),"SODIUM")>0 then _sugg="NA";
		if upcase(LABEL)="NA" then _sugg="NA";
		if index(upcase(LABEL),"BILI")>0 then _sugg="BILI";
		if index(upcase(LABEL),"PROTE")>0 then _sugg="PROT";
		if index(upcase(LABEL),"UPC")>0 then _sugg="UPC";
		if index(upcase(LABEL),"TRIG")>0 then _sugg="TRI";
		if index(upcase(LABEL),"BUN")>0 then _sugg="BUN";
		if index(upcase(LABEL),"UREA")>0 then _sugg="BUN";
		if index(upcase(LABEL),"URIC")>0 then _sugg="UAC";
		if upcase(LABEL)="UAC" then _sugg="UAC";
		if upcase(LABEL)="UPC" then _sugg="UPC";
		if index(upcase(LABEL),"WHITE")>0 then _sugg="WBC";
		if upcase(LABEL)="WBC" then _sugg="WBC";

		_suggunit=trim(left(_sugg))||trim(left(_sunit))||trim(left(_sll));
		_suggunit=compress(_suggunit);

		if compress(_suggunit) in ("UML" "GL" "GDL" "MML" "MDL" "PCT") then _suggunit='does not exist in the dictionary'; 

		if _suggunit='' then _suggunit='does not exist in the dictionary';

	run;


	DATA _null_;
	set check(where=(checkdico = 0)) end=eof;
	N+1;	
		CALL SYMPUT('DICONAME'||COMPRESS(N),COMPRESS(NAME_UNIT));
	if eof then CALL SYMPUT('NBDICO',COMPRESS(N));
	keep name;
	run;

	%let VDICONAME=;
	%do i=1 %to &NBDICO.;
	      %let VDICONAME=%trim(&VDICONAME.) &&DICONAME&i;
	%end;

	%put &VDICONAME.;

	%LET CONTENTINITVARLIST=&VARLIST.;
	%let INITVARLIST=%length(&VARLIST.);
	%let VARLIST=&VARLIST. &VDICONAME.;

	%put &VARLIST.;

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

   %global check checklist;

	%IF &INITVARLIST. NE 0 %THEN %DO;

		data CHECK;
			set CHECK;

			* Check if the name of the observed value lab test variable from INPUT_VAR exists in the VARLIST vector;

			%IF %length(&VARLIST.) ne 0 %then %do;
			checklist1=1;
			checklist2=1;

				%do j=1 %to &Nvarlist;
					if ORIG = "&&v&j" then checklist1=0;
					if NAME_UNIT = "&&v&j" then checklist2=0;
				%end;

			%END;

		** if checklist1 =1 then it means that one of all variable names from
		ORIG does not exist in the VARLIST;
		** if checklist2 =1 then it means that one of all variable names from
		NAME_UNIT does not exist in the VARLIST;

		checklist=sum(of checklist1-checklist2);

		run;

	%END;

	data CHECK2;
		set CHECK;
		if checklist1=0 and checklist2=1;
		** List of NAMES that are in the VARLIST;
	run;

	data CHECK;
		set CHECK;
			** Keep the lines only if the variables from INPUT_VAR do not fit with VARLIST vector or with the DM standard 
			naming from the LAB dictionary;

			** if VARLIST parameter is not used (checklist=0) and  if one of the variables name
			does not fit with VARLIST vector or with the DM standard naming from the LAB dictionary (checkdico>0);
			** OR;
			** if the variables from INPUT_VAR do not fit with VARLIST vector(checklist=2)
			and  if one of the variables name does not fit with VARLIST vector or with the DM standard naming from the LAB dictionary (checkdico>0);

			if (checklist=0 and checkdico>0)
			OR (checklist=2  and checkdico>0);


			label name_unit="NAME";
			label lln_unit="LLN" ;
			label uln_unit="ULN";
			label sae_unit="SAE" ;
			label rel_unit="REL";
			label at_unit="AT";

	RUN;


	data _null_;
	set _contentlab end=eof;
	N+1;	
	CALL SYMPUT('CONT_NAME'||COMPRESS(N),COMPRESS(NAME));
	if eof then CALL SYMPUT('NBCONT',COMPRESS(N));
	run;

	data check;
	set check;
		zname1=0;
		zname2=0;
		zname3=0;
		zname4=0;
		zname5=0;
		zname6=0;

		if name = "" then zname1=.;
		if lln = "" then zname2=.;
		if uln = "" then zname3=.;
		if sae = "" then zname4=.;
		if rel = "" then zname5=.;
		if at = "" then zname6=.;

		%do j=1 %to &NBCONT;
			if name = "&&CONT_NAME&j" then zname1=1;
			if lln = "&&CONT_NAME&j" then zname2=1;
			if uln = "&&CONT_NAME&j" then zname3=1;
			if sae = "&&CONT_NAME&j" then zname4=1;
			if rel = "&&CONT_NAME&j" then zname5=1;
			if at = "&&CONT_NAME&j" then zname6=1;
		%end;

		if zname1=0 then do ; NAME=""; NAME_UNIT=""; end;
		if zname2=0 then do; LLN=""; LLN_UNIT="";  end;
		if zname3=0 then do; ULN=""; ULN_UNIT=""; end;
		if zname4=0 then do; SAE=""; SAE_UNIT=""; end;
		if zname5=0 then do; REL=""; REL_UNIT=""; end;
		if zname6=0 then do; AT=""; AT_UNIT=""; end;

	run;

   %let check=0;
   data _null_;
		set check END=FINAL;
		N+1;
		IF FINAL THEN CALL SYMPUT('check',COMPRESS(N));
   run;

   proc means data =check noprint;	
      var checklist ;
	  output out=checklist sum=sum;
   run;

   data _null_;
		set checklist;
		CALL SYMPUT('checklist',COMPRESS(SUM));
   run;

   %if &check >0 %then %do;

   %put ERROR !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!;
   %put CHECK: THERE ARE SOME PROBLEMS ABOUT VARIABLE NAMES CONTAINED INTO &data. DATASET. ;
   %put SEE LISTINGS IN OUTPUT WINDOW ;
   %put !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!;

	** Produce checking listing; 
	options nodate;

	%put &INITVARLIST.;
	%if &INITVARLIST. ne 0 and &checklist =0 and &check >=1 %then %do;
	
	footnote1 " &CONTENTINITVARLIST. lab exam(s) is (are) already in the VARLIST parameter.";
	footnote2 " LABTOX macro is not stopped and can run, it is not an ERROR then.";

	%end;

	title "Check of input variables";
	title2 "Comparison with lab dictionnary";
	title4 "This (these) variable(s) is (are) not in the LAB dictionnary";
	proc print data=check noobs label ;
	var ORIG  label lln_unit uln_unit sae_unit rel_unit at_unit _suggunit  ;
	where checkdico>0;
	format label $30.;
	run;

	title "Check of input variables";
	title2 "Comparison with VARLIST vector";
	title4 "This (these) variable(s) is (are) not in the VARLIST vector";
	proc print data=check noobs label;
	var ORIG  label lln_unit uln_unit sae_unit rel_unit at_unit _suggunit  ;
	where checklist>0;
	format label $30.;
	run;

   %end;

   %else %do;

   %put NO ERROR !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!;
   %put CHECK: THERE IS NO PROBLEM AT ALL ABOUT VARIABLE NAMES CONTAINED INTO &data. DATASET.;
   %put !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!;

   %end;

	title;

	%sort(data=INPUT_VAR,var=NAME);
	%sort(data=check2,var=NAME);

	data INPUT_VAR;
		merge INPUT_VAR(in=a) check2(keep=NAME checklist ORIG);
		by NAME;
		if a;

		NAME=name_unit;
		LLN=lln_unit;
		ULN=uln_unit;
		SAE=sae_unit;
		REL=rel_unit;
		AT=at_unit;

		** if the variables from INPUT_VAR fit with VARLIST vector(checklist=1)
		then replace NAME by ORIG (original name from database);

		if checklist=1 then do;
			NAME=ORIG;
			LLN="";
			ULN="";
			SAE="";
			REL="";
			AT="";
		end;

		drop name_unit lln_unit uln_unit sae_unit rel_unit at_unit checklist ORIG;
	run;

	%put &CHECK. &CHECKLIST.;

	footnote;

	proc datasets nolist;
		delete  _AT 
		 _CONTENTLAB 
		 LAB_DICTIONNARY
		 CHECK  CHECK2 CHECKLIST
		 _LL 
		 _NAME  _NAME2 _allnames
		 _REL 
		 _SAE 
		 _UL ;
	run;
	quit;


%MEND;

