**********************************************************************************************************;
** MACRO DETECT_PROBLEMS                                                                                **
** written by Jérôme RAPION                                                                             **
**                                                                                                      **
** Production of Listing to detect errors                                                               **
**                                                                                                      **
** The purpose of this macro is to produce several listings to check the accuracy of laboratory data    **
** and to detect ouliers.                                                                               **
**                                                                                                      **
** A first listing is produced to summarize all variable names coming from the LAB database.  It is a   **
** print out of INPUT_VAR dataset in fact. (dataset created by RECOGNITION and INPUT_UNIT macros)       **
** This listing could warn the statistician that some wrong names have been given to variables that are **
** not compliant with DM standard variable naming.                                                      **
**                                                                                                      **
** A second listing is produced to detect ouliers, listing the x highest and lowest values for one or   **
** several lab exams. The user can choose how many values of lowest or highest he would like to print   **
** and also for which lab exams he would like to get this outliers listing.                             **
**                                                                                                      **
** A third listing is produced to identify conversion problems (when a unit in LABLIST dataset is not   **
** recognized by CONVERT dictionnary                                                                    **
**                                                                                                      **
** A fourth listing is produced to lab exams for which the CTC grade computation failed, so when the    **
** value of CTC grade is empty.                                                                         **
**                                                                                                      **
** Creation date  : 18-01-2011                                                                          **
** Version date   : 17-02-2012                                                                          **
** Software       : SAS version 9.2                                                                     **
** Original author: Jérôme RAPION                                                                       **
** Modified by    : Jérôme RAPION                                                                       **
** macro version : 1.1                                                                                  **
**********************************************************************************************************
 
PARAMETERS

    DATA              :  Name of the input dataset containing Hematology and Biochemistry data (Required parameter)
 	
    KEEPLIST          :  A vector which lists all the variable names the user would like to add in the OUTPUT dataset. 
						 It could be identification variables or flag that could be useful for the analysis that the 
						 statistician would like to keep in the OUTPUT dataset 
					     (For instance, date of assessment, cycle number, treatment arm, flags for ITT, PP analysis, etc.)
                         (Optional parameter). Example: KEEPLIST=CYCLE TRT1 ITT

    OUTFILE           :  name of an outfile to contain all the listings to detect errors (Required parameter). 
						 This name will contain the whole address with the file name with RTF extension. 
						 For instance, OUTFILE = c:\temp\errors.rtf

    DATEXAM           :  Variable containing the date of assessment of the laboratory examination (Required parameter)

    PERIOD            :  Name of the variable containing the period. The values of this variable will for instance
                         distinguish several period, such as ’Baseline’, ‘On study’ etc. (Required parameter). 

    PRINTOUTLIER      :  n
						 if n>0, indicates to SAS if you want to print outliers in your outfile and the number of 
						 line of outliers per LAB TESTS you want to print
						 WARNING: These listings could be very huge.
						 0, if you don’t want to print outliers in your outfile 
						 (Optional parameter) Default: PRINTOUTLIER = 20 

    LISTOUTLIER       :  List the LAB TEST you want to be print in your outliers listing 
                         For instance, if LISTOUTLIER= ANC WBC, it means that the outliers listing will concern only 
						 ANC and WBC lab test. 
						 (Optional parameter) Default: LISTOUTLIER = ALL

    CTC               :  Variable containing the version of the CTC grading (v. 3 or 4). (Required parameter)

************************************************************************************;
		
	

%MACRO DETECT_PROBLEMS (data=,keeplist=,OUTFILE=,datexam=,period=,PRINTOUTLIER=20,LISTOUTLIER=,CTC=);

%if %length(&PRINTOUTLIER)=0 %then %let PRINTOUTLIER=20;
%if %length(&LISTOUTLIER)=0 %then %let LISTOUTLIER=ALL;

%let PRINTOUTLIER=%UPCASE(&PRINTOUTLIER);

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

data _data;
set &data;
run;

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


%IF &LISTOUTLIER ne ALL %THEN %DO;

	%RCHV(SUITE=&LISTOUTLIER.,COMPTE=LISTOUT,SEP=" ",N=NLISTOUTLIER);

	
	data _LISTOULIER;
	length LABLIST $15.;
	%do i=1 %to &NLISTOUTLIER;
	LABLIST="&&LISTOUT&I.";
	output;
	%end;
	run;



%END;
%ELSE %DO;

proc sort data=input_var out=_input_var nodupkey;
by root;
run;

data _input_var;
set _input_var;
LABLIST=root;
keep LABLIST;
run;

data _LISTOULIER;
	set _input_var;
run;



%END;

	data _LISTOULIER;
		set _LISTOULIER;
		if LABLIST ="CA"  then do;
		LABLIST="CA_HYPO";
		end;
		else if LABLIST ="K"  then do;
		LABLIST="K_HYPER";
		end;
		else if LABLIST ="MG"  then do;
		LABLIST="MG_HYPO";
		end;
		else if LABLIST ="NA"  then do;
		LABLIST="NA_HYPER";
		end;
		else if LABLIST="GLU" then do;
		LABLIST="GLUC_HYPO";
		end;

	run;

	%sort(data=_LISTOULIER,var= LABLIST);

	data _null_;
	set _LISTOULIER end=eof;
	N+1;
	call symput("LISTOUT"||COMPRESS(N),LABLIST);
	if eof then call symput('NLISTOUTLIER',N);
	run;


proc template;
define style MyEORTCStyle1 ;
   parent=styles.default;
     replace fonts /
           'TitleFont2' = ("Arial",12pt,Bold Italic)
           'TitleFont' = ("Arial",11pt,Bold Italic)
           'StrongFont' = ("Times",10pt,Bold)
           'FixedEmphasisFont' = ("Courier New, Courier",9pt,Italic)
           'FixedStrongFont' = ("Courier New, Courier",9pt,Bold)
           'FixedHeadingFont' = ("Courier New, Courier",9pt,Bold)
           'BatchFixedFont' = ("SAS Monospace, Courier New, Courier",6.7pt)
           'FixedFont' = ("Courier New, Courier",9pt)
           'headingEmphasisFont' = ("Arial",12pt,Bold Italic)
           'EmphasisFont' = ("Arial",7pt)
           'headingFont' = ("Arial",8pt,Bold)
           'docFont' = ("Arial",8pt)
;***JRA, put headingFont and docFont to Arial 8pt;

    replace color_list                                                      
         "Colors used in the default style" /                                 
         'link' = blue                                                        
         'bgH' = grayBB                                                       
         'fg' = black                                                         
         'bg' = _undef_
         'bgA'=cxE0E0E0;***JRA, put background color for left colomns to grey;

      replace colors                                                          
         "Abstract colors used in the default style" /                        
         'headerfgemph' = color_list('fg')                                    
         'headerbgemph' = color_list('bgH')                                   
         'headerfgstrong' = color_list('fg')                                  
         'headerbgstrong' = color_list('bgH')                                 
         'headerfg' = color_list('fg')                                        
         'headerbg' = color_list('bgA')                                       
         'datafgemph' = color_list('fg')                                      
         'databgemph' = color_list('bg')                                      
         'datafgstrong' = color_list('fg')                                    
         'databgstrong' = color_list('bg')                                    
         'datafg' = color_list('fg')                                          
         'databg' = color_list('bg')                                          
         'batchbg' = color_list('bg')                                         
         'batchfg' = color_list('fg')                                         
         'tableborder' = color_list('fg')                                     
         'tablebg' = color_list('bg')                                         
         'notefg' = color_list('fg')                                          
         'notebg' = color_list('bg')                                          
         'bylinefg' = color_list('fg')                                        
         'bylinebg' = color_list('bg') 
         'captionfg' = color_list('fg')                                       
         'captionbg' = color_list('bg')                                       
         'proctitlefg' = color_list('fg')                                     
         'proctitlebg' = color_list('bg')                                     
         'titlefg' = color_list('fg')                                         
         'titlebg' = color_list('bg')                                         
         'systitlefg' = color_list('fg')                                      
         'systitlebg' = color_list('bg')                                      
         'Conentryfg' = color_list('fg')                                      
         'Confolderfg' = color_list('fg')                                     
         'Contitlefg' = color_list('fg')                                      
         'link2' = color_list('link')                                         
         'link1' = color_list('link')                                         
         'contentfg' = color_list('fg')                                       
         'contentbg' = color_list('bg')                                       
         'docfg' = color_list('fg')                                           
         'docbg' = color_list('bg');  ***JRA, put bylinebg color to white instead of black;
	style Data from Data/
	 Just=C;
	style Header from Header/
	 Just=C;
    style Table from Output/
         rules = GROUPS
         cellpadding=2pt
         cellspacing=0pt
         borderwidth=0.25pt
; 
    style body from body /
      leftmargin=3.17cm
      rightmargin=3.17cm
      topmargin=2.54cm
      bottommargin=2.54cm
;
end;


run;

ODS RTF FILE="&OUTFILE." STYLE=MyEORTCStyle1 bodytitle startpage=yes;

ods listing close;
********************;
*** PRINT INPUT_VAR ;
********************;
title "Print out of INPUT_VAR dataset to detect wrong variable names";

title3 "The table gives the overview of what the macro believes it has found. Please review for contents and completeness";
proc print data= INPUT_VAR noobs label;
var root unit LAB_UNIT name label lln uln rel sae at; 
run;
title;

%let NB_PB=0;

proc sort data=_no_unit out=_lab_no_unit nodupkey;
by labtest;
run;

data _null_ ;
set _lab_no_unit end=eof;
N+1;
	CALL SYMPUT('LABPB'||COMPRESS(N),COMPRESS(LABTEST));
	IF eof THEN CALL SYMPUT('NB_PB',COMPRESS(N));
run;


*****************************************************;
*** PB OF MERGE BETWEEN LABLIST and CONVERT datasets ;
*****************************************************;

title1 "DETECT UNIT PROBLEMS";
title2 "Print out of CONVERT table with LABTEST which met UNIT problems";

%do i=1 %to &NB_PB;

proc print data=input_var label noobs ;
var root unit LAB_UNIT LABEL;
by root;
where root="&&LABPB&i.";
run;

%end;


title;

*****************************;
*** Detect CTC EMPTY ;
*****************************;

data _llnempty;
set _data;
if ctc<.Z;
run;

%sort(data=_llnempty,var=LABTEST patid &datexam.);

title1 "CTC Grade EMPTY";
proc print data= _llnempty label noobs;
var patid &keeplist &period. LABTEST UNIT LABVALUE LLN ULN CTC;
by LABTEST;
run;
title;

*************************************;
*** Detect LLN or ULN outside ranges ;
*************************************;

data _llnulnoutside;
set _data;
	%if &CTC.=3 %then %do;

			select (LABTEST);

			**** within CTC grading book;

			  when ('ALB') do;** in g/L;
			   if lln gt .Z then do;
				   if lln lt 30 then flag_lln=1;
			   	end;
			   end;

			  when ('BICA') do; ** in mmol/L;
			   if lln gt .Z then do;
			   	   if lln lt 16 then flag_lln=1;
				end;
			   end;

			  when ('CA_HYPER') do;
			    if uln gt .Z then do;
     			  if uln gt 2.9 then flag_uln=1;
			    end;
			   end;
			  when ('CA_HYPO') do;
			    if lln gt .Z then do;
			      if lln lt 2 then flag_lln=1;
			    end;
			   end;

			  when ('CD4') do; ** in 109/L;
			   if lln gt .Z then do;
				   if lln lt 0.5 then flag_lln=1;
			   	end;
			   end;

			  when ('CHOL') do; ** in mmol/L;
			   if uln gt .Z then do;
     			  if uln gt 7.75 then flag_uln=1;
			    end;
			   end;

			  when ('GLUC_HYPER') do;** in mmol/L;
			   if uln gt .Z then do;
				  if uln gt 8.9 then flag_uln=1;
			    end;
			  end;
			  when ('GLUC_HYPO') do;** in mmol/L;
     		   if lln gt .Z then do;
				  if lln lt 3 then flag_lln=1;
			   end;
			  end;

			  when ('K_HYPER') do;
			   if uln gt .Z then do;
				  if uln gt 5.5 then flag_uln=1;
			    end;
			   end;
			  when ('K_HYPO') do;
			   if lln gt .Z then do;
				  if lln lt 3 then flag_lln=1;
			    end;
			   end;

			  when ('MG_HYPER') do;
			   if uln gt .Z then do;
				  if uln gt 1.23 then flag_uln=1;
			    end;
			   end;
			  when ('MG_HYPO') do;
			   if lln gt .Z then do;
				  if lln lt 0.5 then flag_lln=1;
			    end;
			   end;

			  when ('NA_HYPER') do;** in mmol/L;
				if uln gt .Z then do;
				  if uln gt 150 then flag_uln=1;
			    end;
			   end;
			  when ('NA_HYPO') do;** in mmol/L;
			   if lln gt .Z then do;
				  if lln lt 130 then flag_lln=1;
			    end;
			   end;

			  when ('PO') do;** in mmol/L;
			   if lln gt .Z then do;
				  if lln lt 0.8 then flag_lln=1;
			    end;
			   end;

			  *** HEMATOLOGICAL EXAMS **;

			  when ('ANC') do; ** in 109/L;
			   if lln gt .Z then do;
				  if lln lt 1.5 then flag_lln=1;
			    end;
			   end;

			  when ('HB') do; ** in mmol/L;
			   if lln gt .Z then do;
				  if lln lt 6.2 then flag_lln=1;
			    end;
			   end;

			  when ('LYM') do; ** in 109/L;
			   if lln gt .Z then do;
				  if lln lt 0.8 then flag_lln=1;
			    end;
			   end;

			  when ('PLT') do; ** in 109/L;
			   if lln gt .Z then do;
				  if lln lt 75 then flag_lln=1;
			    end;
			   end;

			  when ("WBC") do; ** in 109/L;
			   if lln gt .Z then do;
				  if lln lt 3 then flag_lln=1;
			    end;
			   end;


 			otherwise;

			end; 
	%end;

	%if &CTC.=4 %then %do;

			select (LABTEST);

			**** within CTC grading book;

			  when ('ALB') do;** in g/L;
			   if lln gt .Z then do;
				   if lln lt 30 then flag_lln=1;
			   	end;
			   end;

			  when ('CA_HYPER') do;
			    if uln gt .Z then do;
     			  if uln gt 2.9 then flag_uln=1;
			    end;
			   end;
			  when ('CA_HYPO') do;
			    if lln gt .Z then do;
			      if lln lt 2 then flag_lln=1;
			    end;
			   end;

			  when ('CD4') do; ** in 109/L;
			   if lln gt .Z then do;
				   if lln lt 0.5 then flag_lln=1;
			   	end;
			   end;

			  when ('CHOL') do; ** in mmol/L;
			   if uln gt .Z then do;
     			  if uln gt 7.75 then flag_uln=1;
			    end;
			   end;

			  when ('GLUC_HYPER') do;** in mmol/L;
			   if uln gt .Z then do;
				  if uln gt 8.9 then flag_uln=1;
			    end;
			  end;
			  when ('GLUC_HYPO') do;** in mmol/L;
     		   if lln gt .Z then do;
				  if lln lt 3 then flag_lln=1;
			   end;
			  end;

			  when ('K_HYPER') do;
			   if uln gt .Z then do;
				  if uln gt 5.5 then flag_uln=1;
			    end;
			   end;
			  when ('K_HYPO') do;
			   if lln gt .Z then do;
				  if lln lt 3 then flag_lln=1;
			    end;
			   end;

			  when ('MG_HYPER') do;
			   if uln gt .Z then do;
				  if uln gt 1.23 then flag_uln=1;
			    end;
			   end;
			  when ('MG_HYPO') do;
			   if lln gt .Z then do;
				  if lln lt 0.5 then flag_lln=1;
			    end;
			   end;

			  when ('NA_HYPER') do;** in mmol/L;
				if uln gt .Z then do;
				  if uln gt 150 then flag_uln=1;
			    end;
			   end;
			  when ('NA_HYPO') do;** in mmol/L;
			   if lln gt .Z then do;
				  if lln lt 130 then flag_lln=1;
			    end;
			   end;

			  when ('PO') do;** in mmol/L;
			   if lln gt .Z then do;
				  if lln lt 0.8 then flag_lln=1;
			    end;
			   end;

			  *** HEMATOLOGICAL EXAMS **;

			  when ('ANC') do; ** in 109/L;
			   if lln gt .Z then do;
				  if lln lt 1.5 then flag_lln=1;
			    end;
			   end;

			  when ('HB') do; ** in mmol/L;
			   if lln gt .Z then do;
				  if lln lt 6.2 then flag_lln=1;
			    end;
			   end;

			  when ('LYM') do; ** in 109/L;
			   if lln gt .Z then do;
				  if lln lt 0.8 then flag_lln=1;
			    end;
			   end;

			  when ('PLT') do; ** in 109/L;
			   if lln gt .Z then do;
				  if lln lt 75 then flag_lln=1;
			    end;
			   end;

			  when ("WBC") do; ** in 109/L;
			   if lln gt .Z then do;
				  if lln lt 3 then flag_lln=1;
			    end;
			   end;

 			otherwise;

			end; 

	%end;

	if flag_lln ne . or flag_uln ne .;

run;

data CTC_grade_scaling;
	set dico.CTC_grade_scaling;
	if &ctc=3 then do;
		if LABTEST="HB4" then delete;
		if LABTEST="HB3" then LABTEST="HB";
	end;
	if &ctc=4 then do;
		if LABTEST="HB3" then delete;
		if LABTEST="HB4" then LABTEST="HB";
		if LABTEST="BICA" then delete;
	end;
run;

%sort(data=CTC_grade_scaling,var=LABTEST);
%sort(data=_llnulnoutside,var=LABTEST);

data _llnulnoutside;
merge _llnulnoutside(in=a) CTC_grade_scaling;
by LABTEST;
if a;
run;

proc sort data=_llnulnoutside out=list_llnulnoutside(keep=labtest CTC__grade_scaling) nodupkey;
by LABTEST;
run;

%let NBscale=0;

data _null_;
set list_llnulnoutside end=final;
	m+1;
	call SYMPUT('scale_test'||compress(m),compress(LABTEST));
	call SYMPUT('scale'||compress(m),trim(left(CTC__grade_scaling)));
	if final then call symput('NBscale',compress(m));
run;

%if &NBscale >0 %then %do;

	title1 "LLN < maximum limit for CTC grade computation according to the booklet.";
	title2 "or ULN > minimum limit for CTC grade computation according to the booklet.";

	%sort(data=_llnulnoutside,var=patid &datexam. LABTEST);

	%do i=1 %to &NBscale.;

		footnote height=10pt "CTC v.&ctc. grade scale";
			footnote%eval(&i+1) height=8pt "&&scale_test&i: &&scale&i.";

		proc print data=_llnulnoutside label noobs ;
		var patid &datexam. LABTEST UNIT LABVALUE LLN ULN ;
		where LABTEST="&&scale_test&i";
		run;
	%end;

	title;
	footnote;

%end;

*****************************;
*** Detect DATEXM missing ;
*****************************;

** JRA, 27MAR2012, Test if _datexam_missing is existing or not;
%if %sysfunc(exist(_datexam_missing)) ne 0  %then %DO;

%sort(data=_datexam_missing,var= patid LABTEST &datexam.);

title1 "Listing of patients and lab exams with &datexam. missing";
proc print data=_datexam_missing label noobs ;
var patid &datexam. LABTEST UNIT LABVALUE LLN ULN ;
run;
%end;


%IF &PRINTOUTLIER >0 %THEN %DO;

********************;
*** Detect Outliers ;
********************;

** &PRINTOUTLIER lowest;

data _lowoutliers;
set _data;
where not missing(ctc);

	if LABTEST in (%DO i=1 %to &NLISTOUTLIER; "&&LISTOUT&I." %END;);;

run;

proc sort data=_lowoutliers nodupkey;
by patid labtest labvalue &datexam.;
run;

%sort(data=_lowoutliers,var=labtest labvalue);

data _lowoutliers;
set _lowoutliers;
by labtest labvalue;
if first.labtest then _i=1;
else _i+1;

if _i <=&PRINTOUTLIER;

run;

%sort(data=_lowoutliers,var=labtest labvalue);

** &PRINTOUTLIER highest;
data _highoutliers;
set _data;
where not missing(ctc);

	if LABTEST in (%DO i=1 %to &NLISTOUTLIER; "&&LISTOUT&I." %END;);;

run;

proc sort data=_highoutliers nodupkey;
by patid labtest labvalue &datexam.;
run;

%sort(data=_highoutliers,var=labtest descending labvalue);

data _highoutliers;
set _highoutliers;
by labtest descending labvalue;
if first.labtest then _i=1;
else _i+1;

if _i <=&PRINTOUTLIER;

run;

%sort(data=_highoutliers,var=labtest descending labvalue);

data _data;
set _data;

if LABTEST in (%DO i=1 %to &NLISTOUTLIER; "&&LISTOUT&I." %END;);;

length IDdate $16.;
IDdate=put(patid,3.)||" ("||put(&datexam.,ddmmyy8.)||")";
run;

%sort(data=_data,var=&period iddate);

proc sort data=_data out=_names(keep=labtest unit) nodupkey;
by labtest;
run;

data _names;
set _names;
length LABTEST2 $15.;
	if LABTEST ="CA_HYPO"  then do;
	LABTEST2="CA";
	end;
	else if LABTEST ="CA_HYPER"  then do;
	delete;
	end;
	else if LABTEST ="MG_HYPO"  then do;
	LABTEST2="MG";
	end;
	else if LABTEST ="MG_HYPER"  then do;
	delete;
	end;
	else if LABTEST="GLUC_HYPO" then do;
	LABTEST2="GLU";
	end;
	else if LABTEST="GLUC_HYPER" then do;
	delete;
	end;
	else if LABTEST ="K_HYPER"  then do;
	LABTEST2="K";
	end;
	else if LABTEST ="K_HYPO"  then do;
	delete;
	end;
	else if LABTEST ="NA_HYPER"  then do;
	LABTEST2="NA";
	end;
	else if LABTEST ="NA_HYPO"  then do;
	delete;
	end;
	else LABTEST2=LABTEST;
run;


data _null_;
set _names end=eof;
N+1;
	CALL SYMPUT('NAMEB'||COMPRESS(N),COMPRESS(LABTEST2));
	CALL SYMPUT('NAME'||COMPRESS(N),COMPRESS(LABTEST));
	CALL SYMPUT('CONV_UNIT'||COMPRESS(N),COMPRESS(UNIT));
IF eof THEN CALL SYMPUT('NBLAB',COMPRESS(N));
run;


title1 "DETECT OUTLIERS";

%do i=1 %to &NBLAB;

title2 "The &PRINTOUTLIER lowest values in the database for &&NAMEB&i";
title4 h=8pt "All values have been converted to the SI units";

proc print data= _lowoutliers label noobs;
var patid &keeplist LABTEST UNIT LABVALUE LLN ULN CTC;
  where LABTEST="&&NAME&i";
  label ctc="CTC grade";
run;

title2 "The &PRINTOUTLIER highest in the database values for &&NAMEB&i";
title4 h=8pt "All values have been converted to the SI units";

proc print data= _highoutliers label noobs;
var patid &keeplist LABTEST UNIT LABVALUE LLN ULN CTC;
  where LABTEST="&&NAME&i";
  label ctc="CTC grade";
run;
title;


%end;


%END;

ODS RTF CLOSE;


	proc datasets nolist;
		delete _data: convert
		_highoutliers  _names
		 _lowoutliers  
		_lab_no_unit
		_llnempty
		_LISTOULIER:
		_datexam_missing
		_llnulnoutside
		_no_unit
		list_llnulnoutside CTC_grade_scaling
		_input_var;
	run;
	quit;



ods listing;

%MEND;

