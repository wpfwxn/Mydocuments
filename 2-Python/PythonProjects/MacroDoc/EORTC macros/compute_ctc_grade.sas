**********************************************************************************************************;
** MACRO COMPUTE_CTC_GRADE                                                                              **
** written by Jérôme RAPION                                                                             **
**                                                                                                      **
** Computation of CTC grades according to the CTC version (3 or 4)                                      **
**                                                                                                      **
** The purpose of this macro is to compute CTC grades for all lab EXAMS according to CTCAE booklet      **
** from the US National Cancer Institute (NCI) and also finding the worst grade per lab EXAMS           **
** and per period. The CTC grade according to CTCAE booklet is coded beween 0 and 5.                    **
**                                                                                                      **
** For some LAB TEST, to distinguish a grade between grade 0 or grade 1, we need LLN or ULN value.      **
** In this case, if LLN (or ULN) is missing but we know it is grade 0 or 1, because of the level of the **
** lab value, the CTC grade is declared as Grade 0/1 (coded 0.5)                                        **
**                                                                                                      **
** If the LAB exam was not present in CTCAE booklet, the grade is computed following these rules (1):   **
**  ->  if LLN and ULN are not missing:                                                                 **
**     - below LLN  (coded 8)                                                                           **
**     - Within Normal Limits (coded 7)                                                                 **
**     - above ULN  (coded 10)                                                                           **
**  ->  if LLN is not missing but ULN is missing:                                                       **
**     - below LLN  (coded 8)                                                                           **
**     - above LLN  (coded 6)                                                                          **
**  ->  if ULN is not missing but LLN is missing:                                                       **
**     - below ULN  (coded 9)                                                                           **
**     - above ULN  (coded 10)                                                                           **
**                                                                                                      **
**  In &DATA dataset, the variable CTC is created to contain this grade.                                **
**  New variables for worst grades are created following this naming convention :                       **
**  <name of the LAB EXAM>_<lab_period>                                                                 **
**  All these new variables containing the worst grade are merged with &OUTDATA dataset by PATID        **
**  A new dataset called CTC_PER_PATIENT is created and contains onmy these new variables containing    **
**  the worst grade (one line per PATID)                                                                **
**                                                                                                      **
**                                                                                                      **
** Creation date  : 17-11-2010                                                                          **
** Version date   : 21-03-2012                                                                          **
** Software       : SAS version 9.2                                                                     **
** Original author: Jérôme RAPION                                                                       **
** Modified by    : Jérôme RAPION                                                                       **
** macro version : 1.1                                                                                  **
**********************************************************************************************************

PARAMETERS

    DATA              :  Name of the input dataset containing Hematology and Biochemistry data (Required parameter)

 	
    PERIOD            :  Variable containing the period. The values of this variable will for instance
                         distinguish several period, such as ’Baseline’, ‘On study’ etc. (Required parameter). 

    CTC               :  Variable containing the version of the CTC grading (v. 3 or 4). (Required parameter)

    OUTDATA           :  name of a SAS dataset which contains one observation per PATID in which
						 all new outcome variables will be merged (Optional parameter). 
                         Default: OUTDATA = PATIENT. 

	VARLIST           :  A vector which contains the ‘ROOT’ names of LAB tests which do not belong to the standard LAB 
						 tests listed in the dictionanary (see appendix 1). (Optional parameter)
						 For instance, TSH variable could be added in the list for a specific analysis but does not 
                         belong to the Dictionanary (see notes)

************************************************************************************
			
NOTES

 If VARLIST parameter is used, in this case, the CTC grade is computed according to LLN and ULN variables linked
 to the exam identified into VARLIST parameter following rules described in (1).

********************;

%MACRO COMPUTE_CTC_GRADE (data=,period=,ctc=,outdata=,varlist=);

** Recuparation of all variables names from VARLIST vector;
%put &VARLIST.;
%IF &VARLIST ne "" %then %do;

	%local j;
	%LET j=1;
	%DO %WHILE(%SCAN(&varlist.,&j.) ne %STR());
	   %LOCAL w&j. ;
		   %LET w&j.=%upcase(%SCAN(&varlist.,&j.));
	   %LET j=%EVAL(&j.+1);
		%END;
	%LET Nvarlist=%EVAL(&j.-1);
%END;


%if %length(&outdata.)=0 %then %let outdata=PATIENT;

proc format;
value ctc 
		  0.5="Grade 0/1"
		  0='Grade 0'
          1='Grade 1'
		  2='Grade 2'
		  3='Grade 3'
		  4='Grade 4'
		  7='Within Normal Limits'
		  8='Below LLN'
		  9='Below ULN'
		  10='Above ULN'
		  6='Above LLN'
		  .='Not reported'
		  .U='ULN not reported'
		  .L='LLN not reported'
		  .V='ULN or LLN not reported';
run;


proc contents data= &data out=_contents noprint;
run;

data _contents;
set _contents;
if UPCASE(NAME) =%UPCASE("&PERIOD");
call SYMPUT('FORMAT_PER',compress(FORMAT));
run;

%put FORMAT_PER &FORMAT_PER  ;

** JRA, 21MAR2012, keep all lab exams in ROOT dataset;
proc sort data=INPUT_VAR(keep=root) out=root nodupkey;
by root;
run;

data root;
length labtest $10.;
set root;
labtest=root;
drop root;
run;

** JRA, 21MAR2012, keep all lab period in PERIODS dataset;
proc sort data=&data. out=periods(keep=&period.) nodupkey;
by &period.;
run;

** JRA, 21MAR2012, Creation of ALL_LABS_PERIODS dataset which contains all lab exams and all lab periods
which should be present in theory;
proc sql;
create table all_labs_periods as select * from root,periods;
quit;

** JRA, 21MAR2012, Merge of ALL_LABS_PERIODS with &data.;
** If a sepcif lab exam for a specific period was not present into &data. dataset
** we add it artificially (patid=.M : fake patient) and flag it with flag_test=1;

%sort(data=all_labs_periods,var=labtest &period);
%sort(data=&data.,var=labtest &period patid);
data &data.;
	merge &data.(in=a) all_labs_periods(in=b);
	by labtest &period;
	if b and not a then do;
	flag_test=1;
	patid=.M;
	end;
run;


data &data;
	set &data;

	***icicicicic;

	%if &CTC.=3 %then %do;

			select (LABTEST);

			**** within CTC grading book;

			  when ('ALB') do;** in g/L;
			   if lln gt .Z and labvalue gt .Z then do;
				   if labvalue lt 20 then ctc=3;
				   else if labvalue lt 30 then ctc=2;
				   else if labvalue lt lln then ctc=1;
				   else ctc=0;
			  	 end;
			   if lln le .Z and labvalue gt .Z then do;
				   if labvalue lt 20 then ctc=3;
				   else if labvalue lt 30 then ctc=2;
				   else ctc=0.5;
			   	end;
			   end;

			  when ('ALP') do; ** U/L;
			   if uln gt .Z and labvalue gt .Z then do;
				   if labvalue gt (20*uln) then ctc=4;
				   else if labvalue gt (5*uln) then ctc=3;
				   else if labvalue gt (2.5*uln) then ctc=2;
				   else if labvalue gt uln then ctc=1;
				   else ctc=0;
				end;
			   if uln le .Z then ctc=.U;
			   end;

			  when ('ALT') do; ** U/L;
			   if uln gt .Z and labvalue gt .Z then do;
				   if labvalue gt (20*uln) then ctc=4;
				   else if labvalue gt (5*uln) then ctc=3;
				   else if labvalue gt (2.5*uln) then ctc=2;
				   else if labvalue gt uln then ctc=1;
				   else ctc=0;
				end;
			   if uln le .Z then ctc=.U;
			   end;

			  when ('AST') do; ** U/L;
			   if uln gt .Z and labvalue gt .Z then do;
				   if labvalue gt (20*uln) then ctc=4;
				   else if labvalue gt (5*uln) then ctc=3;
				   else if labvalue gt (2.5*uln) then ctc=2;
				   else if labvalue gt uln then ctc=1;
				   else ctc=0;
				end;
			   if uln le .Z then ctc=.U;
			   end;


			  when ('AMY') do; ** U/L;
			   if uln gt .Z and labvalue gt .Z then do;
				   if labvalue gt (5*uln) then ctc=4;
				   else if labvalue gt (2*uln) then ctc=3;
				   else if labvalue gt (1.5*uln) then ctc=2;
				   else if labvalue gt uln then ctc=1;
				   else ctc=0;
				end;
			   if uln le .Z then ctc=.U;
			   end;


			  when ('BICA') do; ** in mmol/L;
			   if lln gt .Z and labvalue gt .Z then do;
				   if labvalue lt 8 then ctc=4;
				   else if labvalue lt 11 then ctc=3;
				   else if labvalue lt 16 then ctc=2;
				   else if labvalue lt lln then ctc=1;
				   else ctc=0;
				end;

			   if lln le .Z and labvalue gt .Z then do;
				   if labvalue lt 8 then ctc=4;
				   else if labvalue lt 11 then ctc=3;
				   else if labvalue lt 16 then ctc=2;
				   else ctc=0.5;
			   end;
			   end;

			  when ('BILI') do; ** in umol/L;
			   if uln gt .Z and labvalue gt .Z then do;
				   if labvalue gt (10*uln) then ctc=4;
				   else if labvalue gt (3*uln) then ctc=3;
				   else if labvalue gt (1.5*uln) then ctc=2;
				   else if labvalue gt uln then ctc=1;
				   else ctc=0;
			   end;
			   if uln le .Z then ctc=.U;
			   end;

			  when ('CA') do;
			    labtest='CA_HYPER ';** in mmol/L;
			    if uln gt .Z and labvalue gt .Z then do;
					   if labvalue gt 3.4 then ctc=4;
					   else if labvalue gt 3.1 then ctc=3;
					   else if labvalue gt 2.9 then ctc=2;
					   else if labvalue gt uln then ctc=1;
					   else ctc=0;
				end;
			    if uln le .Z and labvalue gt .Z then do;
					   if labvalue gt 3.4 then ctc=4;
					   else if labvalue gt 3.1 then ctc=3;
					   else if labvalue gt 2.9 then ctc=2;
					   else ctc=0.5;
			     end;
				 output;
				 labtest='CA_HYPO ';** in mmol/L;
			     if lln gt .Z and labvalue gt .Z then do;
  					   if labvalue lt 1.5 then ctc=4;
					   else if labvalue lt 1.75 then ctc=3;
					   else if labvalue lt 2 then ctc=2;
					   else if labvalue lt lln then ctc=1;
					   else ctc=0;
			   	 end;
			     if lln le .Z and labvalue gt .Z then do;
  					   if labvalue lt 1.5 then ctc=4;
					   else if labvalue lt 1.75 then ctc=3;
					   else if labvalue lt 2 then ctc=2;
					   else ctc=0.5;
				 end;
			     end;

			  when ('CTNT') do; ** in ng/mL;
			  if labvalue gt .Z then do;
				   if labvalue ge 0.2 then ctc=4;
				   else if labvalue ge 0.1 then ctc=3;
				   else if labvalue ge 0.05 then ctc=2;
				   else if labvalue ge 0.03 then ctc=1;
				   else ctc=0; 
			   end;
			   end;

			  when ('CD4') do; ** in 109/L;
			   if lln gt .Z and labvalue gt .Z then do;
				   if labvalue lt 0.05 then ctc=4;
				   else if labvalue lt 0.2 then ctc=3;
				   else if labvalue lt 0.5 then ctc=2;
				   else if labvalue lt lln then ctc=1;
				   else ctc=0;
			   end;
			   if lln le .Z and labvalue gt .Z then do;
				   if labvalue lt 0.05 then ctc=4;
				   else if labvalue lt 0.2 then ctc=3;
				   else if labvalue lt 0.5 then ctc=2;
				   else ctc=0.5;

			     end;
			   end;


			  when ('CPK') do; ** U/L;
			   if uln gt .Z and labvalue gt .Z then do;
				   if labvalue gt (10*uln) then ctc=4;
				   else if labvalue gt (5*uln) then ctc=3;
				   else if labvalue gt (2.5*uln) then ctc=2;
				   else if labvalue gt uln then ctc=1;
				   else ctc=0;
				end;
			   if uln le .Z then ctc=.U;
			   end;


			  when ('CHOL') do; ** in mmol/L;
			   if uln gt .Z and labvalue gt .Z then do;
				   if labvalue gt 12.92 then ctc=4;
				   else if labvalue gt 10.34 then ctc=3;
				   else if labvalue gt 7.75 then ctc=2;
				   else if labvalue gt uln then ctc=1;
				   else ctc=0;
				   end;
			   if uln le .Z and labvalue gt .Z then do;
				   if labvalue gt 12.92 then ctc=4;
				   else if labvalue gt 10.34 then ctc=3;
				   else if labvalue gt 7.75 then ctc=2;
				   else ctc=0.5;

			      end;
			   end;

			  when ('CRE') do; ** in umol/L;
			   if uln gt .Z and labvalue gt .Z then do;
				   if labvalue gt (6*uln) then ctc=4;
				   else if labvalue gt (3*uln) then ctc=3;
				   else if labvalue gt (1.5*uln) then ctc=2;
				   else if labvalue gt uln then ctc=1;
				   else ctc=0;
				   end;
			   if uln le .Z then ctc=.U;
			   end;

			  when ('GGT') do; ** U/L;
			   if uln gt .Z and labvalue gt .Z then do;
				   if labvalue gt (20*uln) then ctc=4;
				   else if labvalue gt (5*uln) then ctc=3;
				   else if labvalue gt (2.5*uln) then ctc=2;
				   else if labvalue gt uln then ctc=1;
				   else ctc=0;
				   end;
			   if uln le .Z then ctc=.U;
			   end;

			  when ('GLU') do;** in mmol/L;
			   labtest='GLUC_HYPER';
			   if uln gt .Z and labvalue gt .Z then do;
					if labvalue gt 27.8 then ctc=4;
					else if labvalue gt 13.9 then ctc=3;
					else if labvalue gt 8.9 then ctc=2;
					else if labvalue gt uln then ctc=1;
					else ctc=0;
				end;
			    if uln le .Z and labvalue gt .Z then do;
					if labvalue gt 27.8 then ctc=4;
					else if labvalue gt 13.9 then ctc=3;
					else if labvalue gt 8.9 then ctc=2;
				    else ctc=0.5;
				end;
				output;
				labtest='GLUC_HYPO';** in mmol/L;
			    if lln gt .Z and labvalue gt .Z then do;
					if labvalue lt 1.7 then ctc=4;
					else if labvalue lt 2.2 then ctc=3;
					else if labvalue lt 3 then ctc=2;
					else if labvalue lt lln then ctc=1;
					else ctc=0;
			        end;
			    if lln le .Z and labvalue gt .Z then do;
					if labvalue lt 1.7 then ctc=4;
					else if labvalue lt 2.2 then ctc=3;
					else if labvalue lt 3 then ctc=2;
				    else ctc=0.5;

				end;
				end;

			  when ('GFR') do; ** mL/min;
			   if lln gt .Z and labvalue gt .Z then do;
				   if labvalue lt (0.25*lln) then ctc=3; *no grade 4;
				   else if labvalue lt (0.5*lln) then ctc=2;
				   else if labvalue lt (0.75*lln) then ctc=1;
				   else ctc=0;
			   end;
			   if lln le .Z then ctc=.L;
			   end;


			  when ('K') do;
			   labtest='K_HYPER';** in mmol/L;
			   if uln gt .Z and labvalue gt .Z then do;
				   if labvalue gt 7 then ctc=4;
				   else if labvalue gt 6 then ctc=3;
				   else if labvalue gt 5.5 then ctc=2;
				   else if labvalue gt uln then ctc=1;
				   else ctc=0;
			       end;
			   if uln le .Z and labvalue gt .Z then do;
				   if labvalue gt 7 then ctc=4;
				   else if labvalue gt 6 then ctc=3;
				   else if labvalue gt 5.5 then ctc=2;
				   else ctc=0.5;

			   end;
			   output;
			   labtest='K_HYPO';** in mmol/L;
			   if lln gt .Z and labvalue gt .Z then do;
				   if labvalue lt 2.5 then ctc=4;
				   else if labvalue lt 3 then ctc=3;
				    ** no grade 2 **;
				   else if labvalue lt lln then ctc=1;
				   else ctc=0;
				   end;
			   if lln le .Z then ctc=.L;
			   end;

			  when ('LIP') do;** in U/L;
			   if uln gt .Z and labvalue gt .Z then do;
				   if labvalue gt 5*uln then ctc=4;
				   else if labvalue gt 2*uln then ctc=3;
				   else if labvalue gt 1.5*uln then ctc=2;
				   else if labvalue gt uln then ctc=1;
				   else ctc=0;
				   end;
			   if uln le .Z then ctc=.U;
			   end;


			  when ('MG') do;
			   labtest='MG_HYPER ';** in mmol/L;
			   if uln gt .Z and labvalue gt .Z then do;
				   if labvalue gt 3.3 then ctc=4;
				   else if labvalue gt 1.23 then ctc=3;
				    ** no grade 2 **;
				   else if labvalue gt uln then ctc=1;
				   else ctc=0;
			   end;
			   if uln le .Z then ctc=.U;
			   output;
			   labtest='MG_HYPO ';** in mmol/L;
			   if lln gt .Z and labvalue gt .Z then do;
				   if labvalue lt 0.3 then ctc=4;
				   else if labvalue lt 0.4 then ctc=3;
				   else if labvalue lt 0.5 then ctc=2;
				   else if labvalue lt lln then ctc=1;
				   else ctc=0;
				   end;
			   if lln le .Z and labvalue gt .Z then do;
				   if labvalue lt 0.3 then ctc=4;
				   else if labvalue lt 0.4 then ctc=3;
				   else if labvalue lt 0.5 then ctc=2;
				   else ctc=0.5;

			   end;
			   end;

			  when ('NA') do;** in mmol/L;
			   labtest='NA_HYPER';
			   if uln gt .Z and labvalue gt .Z then do;
				   if labvalue gt 160 then ctc=4;
				   else if labvalue gt 155 then ctc=3;
				   else if labvalue gt 150 then ctc=2;
				   else if labvalue gt uln then ctc=1;
				   else ctc=0;
			   end;
			   if uln le .Z and labvalue gt .Z then do;
				   if labvalue gt 160 then ctc=4;
				   else if labvalue gt 155 then ctc=3;
				   else if labvalue gt 150 then ctc=2;
				   else ctc=0.5;

			   end;
			   output;
			   labtest='NA_HYPO';** in mmol/L;
			   if lln gt .Z and labvalue gt .Z then do;
				   if labvalue lt 120 then ctc=4;
				   else if labvalue lt 130 then ctc=3;
				    ** no grade 2 **;
				   else if labvalue lt lln then ctc=1;
				   else ctc=0;
			   end;
			   if lln le .Z then ctc=.L;
			   end;


			  when ('PO') do;** in mmol/L;
			   if lln gt .Z and labvalue gt .Z then do;
				   if labvalue lt 0.3 then ctc=4;
				   else if labvalue lt 0.6 then ctc=3;
				   else if labvalue lt 0.8 then ctc=2;
				   else if labvalue lt lln then ctc=1;
				   else ctc=0;
				end;
			   if lln le .Z and labvalue gt .Z then do;
				   if labvalue lt 0.3 then ctc=4;
				   else if labvalue lt 0.6 then ctc=3;
				   else if labvalue lt 0.8 then ctc=2;
				   else ctc=0.5;
			     end;
			  end;

			  when ('PTT') do;** in sec;
			   if uln gt .Z and labvalue gt .Z then do;
				   if labvalue gt 2*uln then ctc=3;
				   else if labvalue gt 1.5*uln then ctc=2;
				   else if labvalue gt uln then ctc=1;
				   else ctc=0;
				end;
			   if uln le .Z then ctc=.U;
			   end;

			  when ("TRI") do; 
			   if uln gt .Z and labvalue gt .Z then do;
			   	   if labvalue gt 10*uln then ctc=4;
				   else if labvalue gt 5*uln then ctc=3;
				   else if labvalue gt 2.5*uln then ctc=2;
				   else if labvalue gt uln then ctc=1;
				   else ctc=0;
				end;
			   if uln le .Z then ctc=.U;
			   end;

			  when ('PTINR') do;
			   if uln gt .Z and labvalue gt .Z then do;
				   if labvalue gt 2*uln then ctc=3;
				   else if labvalue gt 1.5*uln then ctc=2;
				   else if labvalue gt uln then ctc=1;
				   else ctc=0;
				end;
			   if uln le .Z then ctc=.U;
			   end;

			  when ('QTC') do;
			  	if labvalue gt .Z then do;
				   if labvalue gt 0.5 then ctc=3;
				   else if labvalue gt 0.47 then ctc=2;
				   else if labvalue gt 0.45 then ctc=1;
				   else ctc=0;
				end;
			   end;

			  *** HEMATOLOGICAL EXAMS **;

			  when ('ANC') do; ** in 109/L;
			   if lln gt .Z and labvalue gt .Z then do;
				   if labvalue lt 0.5 then ctc=4;
				   else if labvalue lt 1 then ctc=3;
				   else if labvalue lt 1.5 then ctc=2;
				   else if labvalue lt lln then ctc=1;
				   else ctc=0;
				end;

			   if lln le .Z  and labvalue gt .Z then do;
				   if labvalue lt 0.5 then ctc=4;
				   else if labvalue lt 1 then ctc=3;
				   else if labvalue lt 1.5 then ctc=2;
				   else ctc=0.5;
			    end;
			   end;

			  when ('HB') do; ** in mmol/L;

			   if lln gt .Z  and labvalue gt .Z then do;
				   if labvalue lt 4 then ctc=4;
				   else if labvalue lt 4.9 then ctc=3;
				   else if labvalue lt 6.2 then ctc=2;
				   else if labvalue lt lln then ctc=1;
				   else ctc=0;
				end;

			   if lln le .Z and labvalue gt .Z then do;
				   if labvalue lt 4 then ctc=4;
				   else if labvalue lt 4.9 then ctc=3;
				   else if labvalue lt 6.2 then ctc=2;
				   else ctc=0.5;
			    end;

			  end;

			  when ('LYM') do; ** in 109/L;

			   if lln gt .Z and labvalue gt .Z then do;
				   if labvalue lt 0.2 then ctc=4;
				   else if labvalue lt 0.5 then ctc=3;
				   else if labvalue lt 0.8 then ctc=2;
				   else if labvalue lt lln then ctc=1;
				   else ctc=0;
				end;

			   if lln le .Z and labvalue gt .Z  then do;
				   if labvalue lt 0.2 then ctc=4;
				   else if labvalue lt 0.5 then ctc=3;
				   else if labvalue lt 0.8 then ctc=2;
				   else ctc=0.5;
			    end;

			   end;

			  when ('PLT') do; ** in 109/L;

			   if lln gt .Z and labvalue gt .Z  then do;
	 			   if labvalue lt 25 then ctc=4;
				   else if labvalue lt 50 then ctc=3;
				   else if labvalue lt 75 then ctc=2;
				   else if labvalue lt lln then ctc=1;
				   else ctc=0;
				end;

			   if lln le .Z and labvalue gt .Z  then do;
	 			   if labvalue lt 25 then ctc=4;
				   else if labvalue lt 50 then ctc=3;
				   else if labvalue lt 75 then ctc=2;
				   else ctc=0.5;
			    end;

			   end;

			  when ("WBC") do; ** in 109/L;

			   if lln gt .Z  and labvalue gt .Z then do;
				   if labvalue lt 1 then ctc=4;
				   else if labvalue lt 2 then ctc=3;
				   else if labvalue lt 3 then ctc=2;
				   else if labvalue lt lln then ctc=1;
				   else ctc=0;
				end;

			   if lln le .Z and labvalue gt .Z then do;
				   if labvalue lt 1 then ctc=4;
				   else if labvalue lt 2 then ctc=3;
				   else if labvalue lt 3 then ctc=2;
				   else ctc=0.5;
			   end;

  		      end;

	**** not in CTC grading book;
	**** no CTC: only below LLN, within normal limits, above ULN ;

			  when ('BUN') do; ** mmol/L;
			   if uln gt .Z and lln gt .Z  and labvalue gt .Z then do;
				   if labvalue lt lln then ctc=8;
				   else if labvalue gt uln then ctc=10;
				   else ctc=7;
				end;
			   if uln = . and lln ne . and labvalue gt .Z then do;
					if labvalue lt lln then ctc=8;
					else ctc=6;
			   end;
			   if uln ne . and lln = . and labvalue gt .Z then do;
					if labvalue lt uln then ctc=9;
					else ctc=10;
			   end;
			   if uln = . and lln = . then ctc=.V;

			   end;

			  when ('CHL') do;
			   if uln gt .Z and lln gt .Z and labvalue gt .Z then do;
				   if labvalue lt lln then ctc=8;
				   else if labvalue gt uln then ctc=10;
				   else ctc=7;
				end;
			   if uln = . and lln ne . and labvalue gt .Z then do;
					if labvalue lt lln then ctc=8;
					else ctc=6;
			   end;
			   if uln ne . and lln = . and labvalue gt .Z then do;
					if labvalue lt uln then ctc=9;
					else ctc=10;
			   end;
			   if uln = . and lln = . then ctc=.V;

			   end;

			  when ('EOS') do; ** in 109/L;
			   if uln gt .Z and lln gt .Z and labvalue gt .Z then do;
				   if labvalue lt lln then ctc=8;
				   else if labvalue gt uln then ctc=10;
				   else ctc=7;
				end;
			   if uln = . and lln ne . and labvalue gt .Z then do;
					if labvalue lt lln then ctc=8;
					else ctc=6;
			   end;
			   if uln ne . and lln = . and labvalue gt .Z then do;
					if labvalue lt uln then ctc=9;
					else ctc=10;
			   end;
			   if uln = . and lln = . then ctc=.V;

			   end;

			  when ('HCT') do; 
			   if uln gt .Z and lln gt .Z and labvalue gt .Z then do;
				   if labvalue lt lln then ctc=8;
				   else if labvalue gt uln then ctc=10;
				   else ctc=7;
				end;
			   if uln = . and lln ne . and labvalue gt .Z then do;
					if labvalue lt lln then ctc=8;
					else ctc=6;
			   end;
			   if uln ne . and lln = . and labvalue gt .Z then do;
					if labvalue lt uln then ctc=9;
					else ctc=10;
			   end;
			   if uln = . and lln = . then ctc=.V;

			   end;

			  when ('LDH') do;
			   if uln gt .Z and lln gt .Z and labvalue gt .Z then do;
				   if labvalue lt lln then ctc=8;
				   else if labvalue gt uln then ctc=10;
				   else ctc=7;
				end;
			   if uln = . and lln ne . and labvalue gt .Z then do;
					if labvalue lt lln then ctc=8;
					else ctc=6;
			   end;
			   if uln ne . and lln = . and labvalue gt .Z then do;
					if labvalue lt uln then ctc=9;
					else ctc=10;
			   end;
			   if uln = . and lln = . then ctc=.V;

			   end;

			  when ('PROT') do; ** g/L;
			   if uln gt .Z and lln gt .Z and labvalue gt .Z then do;
				   if labvalue lt lln then ctc=8;
				   else if labvalue gt uln then ctc=10;
				   else ctc=7;
				end;
			   if uln = . and lln ne . and labvalue gt .Z then do;
					if labvalue lt lln then ctc=8;
					else ctc=6;
			   end;
			   if uln ne . and lln = . and labvalue gt .Z then do;
					if labvalue lt uln then ctc=9;
					else ctc=10;
			   end;
			   if uln = . and lln = . then ctc=.V;

			   end;

			  when ('PT') do; ** in sec;
			   if uln gt .Z and lln gt .Z and labvalue gt .Z then do;
				   if labvalue lt lln then ctc=8;
				   else if labvalue gt uln then ctc=10;
				   else ctc=7;
				end;
			   if uln = . and lln ne . and labvalue gt .Z then do;
					if labvalue lt lln then ctc=8;
					else ctc=6;
			   end;
			   if uln ne . and lln = . and labvalue gt .Z then do;
					if labvalue lt uln then ctc=9;
					else ctc=10;
			   end;
			   if uln = . and lln = . then ctc=.V;

			   end;

			  when ('UAC') do; ** in mmol/L;
			   if uln gt .Z and lln gt .Z and labvalue gt .Z then do;
				   if labvalue lt lln then ctc=8;
				   else if labvalue gt uln then ctc=10;
				   else ctc=7;
				end;
			   if uln = . and lln ne . and labvalue gt .Z then do;
					if labvalue lt lln then ctc=8;
					else ctc=6;
			   end;
			   if uln ne . and lln = . and labvalue gt .Z then do;
					if labvalue lt uln then ctc=9;
					else ctc=10;
			   end;
			   if uln = . and lln = . then ctc=.V;

			   end;

			  when ('UPC') do; 
			   if uln gt .Z and lln gt .Z and labvalue gt .Z then do;
				   if labvalue lt lln then ctc=8;
				   else if labvalue gt uln then ctc=10;
				   else ctc=7;
				end;
			   if uln = . and lln ne . and labvalue gt .Z then do;
					if labvalue lt lln then ctc=8;
					else ctc=6;
			   end;
			   if uln ne . and lln = . and labvalue gt .Z then do;
					if labvalue lt uln then ctc=9;
					else ctc=10;
			   end;
			   if uln = . and lln = . then ctc=.V;

			   end;

			  when ('RBC') do; ** in 1012/L;
			   if uln gt .Z and lln gt .Z and labvalue gt .Z then do;
				   if labvalue lt lln then ctc=8;
				   else if labvalue gt uln then ctc=10;
				   else ctc=7;
				end;
			   if uln = . and lln ne . and labvalue gt .Z then do;
					if labvalue lt lln then ctc=8;
					else ctc=6;
			   end;
			   if uln ne . and lln = . and labvalue gt .Z then do;
					if labvalue lt uln then ctc=9;
					else ctc=10;
			   end;
			   if uln = . and lln = . then ctc=.V;

			   end;

			  when ('FIBRI') do;
			   if uln gt .Z and lln gt .Z and labvalue gt .Z then do;
				   if labvalue lt lln then ctc=8;
				   else if labvalue gt uln then ctc=10;
				   else ctc=7;
				end;
			   if uln = . and lln ne . and labvalue gt .Z then do;
					if labvalue lt lln then ctc=8;
					else ctc=6;
			   end;
			   if uln ne . and lln = . and labvalue gt .Z then do;
					if labvalue lt uln then ctc=9;
					else ctc=10;
			   end;
			   if uln = . and lln = . then ctc=.V;

			   end;

%IF %LENGTH(&VARLIST)>0 %THEN %DO;

		%DO I = 1 %TO &Nvarlist;
			  when ("&&w&I") do; 
			   if uln gt .Z and lln gt .Z and labvalue gt .Z then do;
				   if labvalue lt lln then ctc=8;
				   else if labvalue gt uln then ctc=10;
				   else ctc=7;
				end;
			   if uln = . and lln ne . and labvalue gt .Z then do;
					if labvalue lt lln then ctc=8;
					else ctc=6;
			   end;
			   if uln ne . and lln = . and labvalue gt .Z then do;
					if labvalue lt uln then ctc=9;
					else ctc=10;
			   end;
			   if uln = . and lln = . then ctc=.V;

			   end;

		%END;
%END;
			   otherwise;

			end; 

	output;

	%end; 

	%if &CTC.=4 %then %do;

			select (LABTEST);

			  when ('ALB') do;** in g/L;
			   if lln gt .Z and labvalue gt .Z then do;
				   if labvalue lt 20 then ctc=3;
				   else if labvalue lt 30 then ctc=2;
				   else if labvalue lt lln then ctc=1;
				   else ctc=0;
			  	 end;
			   if lln le .Z and labvalue gt .Z then do;
				   if labvalue lt 20 then ctc=3;
				   else if labvalue lt 30 then ctc=2;
				   else ctc=0.5;

			   	end;
			   end;

			  when ('ALP') do; ** U/L;
			   if uln gt .Z and labvalue gt .Z  then do;
				   if labvalue gt (20*uln) then ctc=4;
				   else if labvalue gt (5*uln) then ctc=3;
				   else if labvalue gt (2.5*uln) then ctc=2;
				   else if labvalue gt uln then ctc=1;
				   else ctc=0;
				end;
			   if uln le .Z then ctc=.U;
			   end;

			  when ('ALT') do; ** U/L;
			   if uln gt .Z and labvalue gt .Z  then do;
				   if labvalue gt (20*uln) then ctc=4;
				   else if labvalue gt (5*uln) then ctc=3;
				   else if labvalue gt (3*uln) then ctc=2;
				   else if labvalue gt uln then ctc=1;
				   else ctc=0;
				end;
			   if uln le .Z then ctc=.U;
			   end;

			  when ('AST') do; ** U/L;
			   if uln gt .Z and labvalue gt .Z  then do;
				   if labvalue gt (20*uln) then ctc=4;
				   else if labvalue gt (5*uln) then ctc=3;
				   else if labvalue gt (3*uln) then ctc=2;
				   else if labvalue gt uln then ctc=1;
				   else ctc=0;
				end;
			   if uln le .Z then ctc=.U;
			   end;

			  when ('AMY') do; ** U/L;
			   if uln gt .Z and labvalue gt .Z  then do;
				   if labvalue gt (5*uln) then ctc=4;
				   else if labvalue gt (2*uln) then ctc=3;
				   else if labvalue gt (1.5*uln) then ctc=2;
				   else if labvalue gt uln then ctc=1;
				   else ctc=0;
				end;
			   if uln le .Z then ctc=.U;
			   end;

			  when ('BILI') do; ** in umol/L;
			   if uln gt .Z and labvalue gt .Z  then do;
				   if labvalue gt (10*uln) then ctc=4;
				   else if labvalue gt (3*uln) then ctc=3;
				   else if labvalue gt (1.5*uln) then ctc=2;
				   else if labvalue gt uln then ctc=1;
				   else ctc=0;
			   end;
			   if uln le .Z then ctc=.U;
			   end;

			  when ('CA') do;
			    labtest='CA_HYPER ';** in mmol/L;
			    if uln gt .Z and labvalue gt .Z  then do;
					   if labvalue gt 3.4 then ctc=4;
					   else if labvalue gt 3.1 then ctc=3;
					   else if labvalue gt 2.9 then ctc=2;
					   else if labvalue gt uln then ctc=1;
					   else ctc=0;
				end;
			    if uln le .Z and labvalue gt .Z  then do;
					   if labvalue gt 3.4 then ctc=4;
					   else if labvalue gt 3.1 then ctc=3;
					   else if labvalue gt 2.9 then ctc=2;
					   else ctc=0.5;
			     end;
				 output;
				 labtest='CA_HYPO ';** in mmol/L;
			     if lln gt .Z and labvalue gt .Z  then do;
  					   if labvalue lt 1.5 then ctc=4;
					   else if labvalue lt 1.75 then ctc=3;
					   else if labvalue lt 2 then ctc=2;
					   else if labvalue lt lln then ctc=1;
					   else ctc=0;
			   	  end;
			     if lln le .Z and labvalue gt .Z  then do;
  					   if labvalue lt 1.5 then ctc=4;
					   else if labvalue lt 1.75 then ctc=3;
					   else if labvalue lt 2 then ctc=2;
					   else ctc=0.5;
					 end;
			     end;

			  when ('CHOL') do; ** in mmol/L;
			   if uln gt .Z and labvalue gt .Z  then do;
				   if labvalue gt 12.92 then ctc=4;
				   else if labvalue gt 10.34 then ctc=3;
				   else if labvalue gt 7.75 then ctc=2;
				   else if labvalue gt uln then ctc=1;
				   else ctc=0;
				end;
			   if uln le .Z and labvalue gt .Z  then do;
				   if labvalue gt 12.92 then ctc=4;
				   else if labvalue gt 10.34 then ctc=3;
				   else if labvalue gt 7.75 then ctc=2;
				   else ctc=0.5;
			    end;
			   end;

			  when ('CRE') do; ** in umol/L;
			   if uln gt .Z and labvalue gt .Z  then do;
				   if labvalue gt (6*uln) then ctc=4;
				   else if labvalue gt (3*uln) then ctc=3;
				   else if labvalue gt (1.5*uln) then ctc=2;
				   else if labvalue gt uln then ctc=1;
				   else ctc=0;
				end;
			   if uln le .Z then ctc=.U;
			   end;

			  when ('CD4') do; ** in 109/L;
			   if lln gt .Z and labvalue gt .Z  then do;
				   if labvalue lt 0.05 then ctc=4;
				   else if labvalue lt 0.2 then ctc=3;
				   else if labvalue lt 0.5 then ctc=2;
				   else if labvalue lt lln then ctc=1;
				   else ctc=0;
			   end;
			   if lln le .Z and labvalue gt .Z  then do;
				   if labvalue lt 0.05 then ctc=4;
				   else if labvalue lt 0.2 then ctc=3;
				   else if labvalue lt 0.5 then ctc=2;
				   else ctc=0.5;
			   end;
			   end;

			  when ('CPK') do; ** U/L;
			   if uln gt .Z and labvalue gt .Z  then do;
				   if labvalue gt (10*uln) then ctc=4;
				   else if labvalue gt (5*uln) then ctc=3;
				   else if labvalue gt (2.5*uln) then ctc=2;
				   else if labvalue gt uln then ctc=1;
				   else ctc=0;
				end;
			   if uln le .Z then ctc=.U;
			   end;

			  when ('GGT') do; ** U/L;
			   if uln gt .Z and labvalue gt .Z  then do;
				   if labvalue gt (20*uln) then ctc=4;
				   else if labvalue gt (5*uln) then ctc=3;
				   else if labvalue gt (2.5*uln) then ctc=2;
				   else if labvalue gt uln then ctc=1;
				   else ctc=0;
				end;
			   if uln le .Z then ctc=.U;
			   end;

			  when ('GLU') do;** in mmol/L;
			   labtest='GLUC_HYPER';
			   if uln gt .Z and labvalue gt .Z  then do;
					if labvalue gt 27.8 then ctc=4;
					else if labvalue gt 13.9 then ctc=3;
					else if labvalue gt 8.9 then ctc=2;
					else if labvalue gt uln then ctc=1;
					else ctc=0;
				end;
			    if uln le .Z and labvalue gt .Z  then do;
					if labvalue gt 27.8 then ctc=4;
					else if labvalue gt 13.9 then ctc=3;
					else if labvalue gt 8.9 then ctc=2;
				    else ctc=0.5;
				end;
				output;
				labtest='GLUC_HYPO';** in mmol/L;
			    if lln gt .Z and labvalue gt .Z  then do;
					if labvalue lt 1.7 then ctc=4;
					else if labvalue lt 2.2 then ctc=3;
					else if labvalue lt 3 then ctc=2;
					else if labvalue lt lln then ctc=1;
					else ctc=0;
			    end;
			    if lln le .Z and labvalue gt .Z  then do;
					if labvalue lt 1.7 then ctc=4;
					else if labvalue lt 2.2 then ctc=3;
					else if labvalue lt 3 then ctc=2;
				    else ctc=0.5;
				  end;
				end;

			  when ('K') do;
			   labtest='K_HYPER';** in mmol/L;
			   if uln gt .Z and labvalue gt .Z  then do;
				   if labvalue gt 7 then ctc=4;
				   else if labvalue gt 6 then ctc=3;
				   else if labvalue gt 5.5 then ctc=2;
				   else if labvalue gt uln then ctc=1;
				   else ctc=0;
			   end;
			   if uln le .Z and labvalue gt .Z  then do;
				   if labvalue gt 7 then ctc=4;
				   else if labvalue gt 6 then ctc=3;
				   else if labvalue gt 5.5 then ctc=2;
				   else ctc=0.5;
			   end;
			   output;
			   labtest='K_HYPO';** in mmol/L;
			   if lln gt .Z and labvalue gt .Z  then do;
				   if labvalue lt 2.5 then ctc=4;
				   else if labvalue lt 3 then ctc=3;
				    ** no grade 2 **;
				   else if labvalue lt lln then ctc=1;
				   else ctc=0;
			   end;
			     if lln le .Z then ctc=.L;
			   end;


			  when ('LIP') do;** in U/L;
			   if uln gt .Z and labvalue gt .Z  then do;
				   if labvalue gt 5*uln then ctc=4;
				   else if labvalue gt 2*uln then ctc=3;
				   else if labvalue gt 1.5*uln then ctc=2;
				   else if labvalue gt uln then ctc=1;
				   else ctc=0;
				   end;
			   if uln = . then do;
				   if labvalue gt 5*uln then ctc=4;
				   else if labvalue gt 2*uln then ctc=3;
				   else if labvalue gt 1.5*uln then ctc=2;
				   else ctc=0.5;
			   end;
			   end;

			  when ('MG') do;
			   labtest='MG_HYPER ';** in mmol/L;
			   if uln gt .Z and labvalue gt .Z  then do;
				   if labvalue gt 3.3 then ctc=4;
				   else if labvalue gt 1.23 then ctc=3;
				    ** no grade 2 **;
				   else if labvalue gt uln then ctc=1;
				   else ctc=0;
			   end;
			   if uln le .Z then ctc=.U;
			   output;
			   labtest='MG_HYPO ';** in mmol/L;
			   if lln gt .Z and labvalue gt .Z  then do;
				   if labvalue lt 0.3 then ctc=4;
				   else if labvalue lt 0.4 then ctc=3;
				   else if labvalue lt 0.5 then ctc=2;
				   else if labvalue lt lln then ctc=1;
				   else ctc=0;
				   end;
			   if lln le .Z and labvalue gt .Z then do;
				   if labvalue lt 0.3 then ctc=4;
				   else if labvalue lt 0.4 then ctc=3;
				   else if labvalue lt 0.5 then ctc=2;
				   else ctc=0.5;

			   end;
			   end;

			  when ('NA') do;** in mmol/L;
			   labtest='NA_HYPER';
			   if uln gt .Z and labvalue gt .Z  then do;
				   if labvalue gt 160 then ctc=4;
				   else if labvalue gt 155 then ctc=3;
				   else if labvalue gt 150 then ctc=2;
				   else if labvalue gt uln then ctc=1;
				   else ctc=0;
			   end;
			   if uln le .Z and labvalue gt .Z  then do;
				   if labvalue gt 160 then ctc=4;
				   else if labvalue gt 155 then ctc=3;
				   else if labvalue gt 150 then ctc=2;
				   else ctc=0.5;

			   end;
			   output;
			   labtest='NA_HYPO';** in mmol/L;
			   if lln gt .Z and labvalue gt .Z  then do;
				   if labvalue lt 120 then ctc=4;
				   else if labvalue lt 130 then ctc=3;
				    ** no grade 2 **;
				   else if labvalue lt lln then ctc=1;
				   else ctc=0;
				   end;
			   if lln le .Z then ctc=.L;
			   end;

			  when ('PO') do;** in mmol/L;
			   if lln gt .Z and labvalue gt .Z  then do;
				   if labvalue lt 0.3 then ctc=4;
				   else if labvalue lt 0.6 then ctc=3;
				   else if labvalue lt 0.8 then ctc=2;
				   else if labvalue lt lln then ctc=1;
				   else ctc=0;
				   end;
			   if lln le .Z and labvalue gt .Z  then do;
				   if labvalue lt 0.3 then ctc=4;
				   else if labvalue lt 0.6 then ctc=3;
				   else if labvalue lt 0.8 then ctc=2;
				   else ctc=0.5;

			  	 end;
			  end;

			  when ('PTT') do;** in sec;
			   if uln gt .Z and labvalue gt .Z  then do;
				   if labvalue gt 2.5*uln then ctc=3;
				   else if labvalue gt 1.5*uln then ctc=2;
				   else if labvalue gt uln then ctc=1;
				   else ctc=0;
				   end;
			   if uln le .Z then ctc=.U;
			   end;

			  when ('PTINR') do;
			   if uln gt .Z and labvalue gt .Z  then do;
				   if labvalue gt 2.5*uln then ctc=3;
				   else if labvalue gt 1.5*uln then ctc=2;
				   else if labvalue gt uln then ctc=1;
				   else ctc=0;
				   end;
			   if uln le .Z then ctc=.U;
			   end;

			  when ('QTC') do;
			  	if labvalue gt .Z then do;
				   if labvalue gt 0.501 then ctc=3;
				   else if labvalue gt 0.481 then ctc=2;
				   else if labvalue gt 0.450 then ctc=1;
				   else ctc=0;
				end;
			   end;

			  when ("TRI") do; ** in mmol/L;
			  	if labvalue gt .Z then do;
				   if labvalue gt 11.4 then ctc=4;
				   else if labvalue gt 5.7 then ctc=3;
				   else if labvalue gt 3.42 then ctc=2;
				   else if labvalue gt 1.71 then ctc=1;
				   else ctc=0;
				end;
			   end;

			  *** HEMATOLOGICAL EXAMS **;

			  when ('ANC') do; ** in 109/L;
			   if lln gt .Z and labvalue gt .Z then do;
				   if labvalue lt 0.5 then ctc=4;
				   else if labvalue lt 1 then ctc=3;
				   else if labvalue lt 1.5 then ctc=2;
				   else if labvalue lt lln then ctc=1;
				   else ctc=0;
				end;

			   if lln le .Z and labvalue gt .Z  then do;
				   if labvalue lt 0.5 then ctc=4;
				   else if labvalue lt 1 then ctc=3;
				   else if labvalue lt 1.5 then ctc=2;
				   else ctc=0.5;
			    end;
			   end;

			  when ('HB') do; ** in mmol/L;

			   if lln gt .Z and labvalue gt .Z then do;
				    ** no grade 4 **;
				   if labvalue lt 4.9 then ctc=3;
				   else if labvalue lt 6.2 then ctc=2;
				   else if labvalue lt lln then ctc=1;
				   else ctc=0;
				end;

			   if lln le .Z and labvalue gt .Z then do;
				    ** no grade 4 **;
				   if labvalue lt 4.9 then ctc=3;
				   else if labvalue lt 6.2 then ctc=2;
				   else ctc=0.5;
			    end;

			  end;

			  when ('LYM') do; ** in 109/L;

			   if lln gt .Z and labvalue gt .Z then do;
				   if labvalue lt 0.2 then ctc=4;
				   else if labvalue lt 0.5 then ctc=3;
				   else if labvalue lt 0.8 then ctc=2;
				   else if labvalue lt lln then ctc=1;
				   else ctc=0;
				end;

			   if lln le .Z and labvalue gt .Z then do;
				   if labvalue lt 0.2 then ctc=4;
				   else if labvalue lt 0.5 then ctc=3;
				   else if labvalue lt 0.8 then ctc=2;
				   else ctc=0.5;
			    end;

			   end;

			  when ('PLT') do; ** in 109/L;

			   if lln gt .Z and labvalue gt .Z then do;
	 			   if labvalue lt 25 then ctc=4;
				   else if labvalue lt 50 then ctc=3;
				   else if labvalue lt 75 then ctc=2;
				   else if labvalue lt lln then ctc=1;
				   else ctc=0;
				end;

			   if lln le .Z and labvalue gt .Z then do;
	 			   if labvalue lt 25 then ctc=4;
				   else if labvalue lt 50 then ctc=3;
				   else if labvalue lt 75 then ctc=2;
				   else ctc=0.5;
			    end;

			   end;

			  when ("WBC") do; ** in 109/L;

			   if lln gt .Z and labvalue gt .Z then do;
				   if labvalue lt 1 then ctc=4;
				   else if labvalue lt 2 then ctc=3;
				   else if labvalue lt 3 then ctc=2;
				   else if labvalue lt lln then ctc=1;
				   else ctc=0;
				end;

			   if lln le .Z and labvalue gt .Z then do;
				   if labvalue lt 1 then ctc=4;
				   else if labvalue lt 2 then ctc=3;
				   else if labvalue lt 3 then ctc=2;
				   else ctc=0.5;
			   end;

  		      end;


	**** not in CTC grading book;
	**** no CTC: only below LLN, within normal limits, above ULN ;

			  when ('BICA') do; ** in mmol/L;
			   if uln gt .Z and lln gt .Z and labvalue gt .Z then do;
				   if labvalue lt lln then ctc=8;
				   else if labvalue gt uln then ctc=10;
				   else ctc=7;
				end;
			   if uln = . and lln ne . and labvalue gt .Z then do;
					if labvalue lt lln then ctc=8;
					else ctc=6;
			   end;
			   if uln ne . and lln = . and labvalue gt .Z then do;
					if labvalue lt uln then ctc=9;
					else ctc=10;
			   end;
			   if uln = . and lln = . then ctc=.V;

			   end;

			  when ('BUN') do; ** mmol/L;
			   if uln gt .Z and lln gt .Z and labvalue gt .Z then do;
				   if labvalue lt lln then ctc=8;
				   else if labvalue gt uln then ctc=10;
				   else ctc=7;
				end;
			   if uln = . and lln ne . and labvalue gt .Z then do;
					if labvalue lt lln then ctc=8;
					else ctc=6;
			   end;
			   if uln ne . and lln = . and labvalue gt .Z then do;
					if labvalue lt uln then ctc=9;
					else ctc=10;
			   end;
			   if uln = . and lln = . then ctc=.V;

				end;


			  when ('CTNT') do; 
			   if uln gt .Z and lln gt .Z and labvalue gt .Z then do;
				   if labvalue lt lln then ctc=8;
				   else if labvalue gt uln then ctc=10;
				   else ctc=7;
				end;
			   if uln = . and lln ne . and labvalue gt .Z then do;
					if labvalue lt lln then ctc=8;
					else ctc=6;
			   end;
			   if uln ne . and lln = . and labvalue gt .Z then do;
					if labvalue lt uln then ctc=9;
					else ctc=10;
			   end;
			   if uln = . and lln = . then ctc=.V;

				end;


			  when ('CHL') do;
			   if uln gt .Z and lln gt .Z and labvalue gt .Z then do;
				   if labvalue lt lln then ctc=8;
				   else if labvalue gt uln then ctc=10;
				   else ctc=7;
				end;
			   if uln = . and lln ne . and labvalue gt .Z then do;
					if labvalue lt lln then ctc=8;
					else ctc=6;
			   end;
			   if uln ne . and lln = . and labvalue gt .Z then do;
					if labvalue lt uln then ctc=9;
					else ctc=10;
			   end;
			   if uln = . and lln = . then ctc=.V;
			   end;

			  when ('EOS') do; ** in 109/L;
			   if uln gt .Z and lln gt .Z and labvalue gt .Z then do;
				   if labvalue lt lln then ctc=8;
				   else if labvalue gt uln then ctc=10;
				   else ctc=7;
				end;
			   if uln = . and lln ne . and labvalue gt .Z then do;
					if labvalue lt lln then ctc=8;
					else ctc=6;
			   end;
			   if uln ne . and lln = . and labvalue gt .Z then do;
					if labvalue lt uln then ctc=9;
					else ctc=10;
			   end;
			   if uln = . and lln = . then ctc=.V;
			   end;

			  when ('HCT') do; 
			   if uln gt .Z and lln gt .Z and labvalue gt .Z then do;
				   if labvalue lt lln then ctc=8;
				   else if labvalue gt uln then ctc=10;
				   else ctc=7;
				end;
			   if uln = . and lln ne . and labvalue gt .Z then do;
					if labvalue lt lln then ctc=8;
					else ctc=6;
			   end;
			   if uln ne . and lln = . and labvalue gt .Z then do;
					if labvalue lt uln then ctc=9;
					else ctc=10;
			   end;
			   if uln = . and lln = . then ctc=.V;
			   end;

			  when ('LDH') do;
			   if uln gt .Z and lln gt .Z and labvalue gt .Z then do;
				   if labvalue lt lln then ctc=8;
				   else if labvalue gt uln then ctc=10;
				   else ctc=7;
				end;
			   if uln = . and lln ne . and labvalue gt .Z then do;
					if labvalue lt lln then ctc=8;
					else ctc=6;
			   end;
			   if uln ne . and lln = . and labvalue gt .Z then do;
					if labvalue lt uln then ctc=9;
					else ctc=10;
			   end;
			   if uln = . and lln = . then ctc=.V;
			   end;

			  when ('PROT') do; ** g/L;
			   if uln gt .Z and lln gt .Z and labvalue gt .Z  then do;
				   if labvalue lt lln then ctc=8;
				   else if labvalue gt uln then ctc=10;
				   else ctc=7;
				end;
			   if uln = . and lln ne . and labvalue gt .Z  then do;
					if labvalue lt lln then ctc=8;
					else ctc=6;
			   end;
			   if uln ne . and lln = . and labvalue gt .Z  then do;
					if labvalue lt uln then ctc=9;
					else ctc=10;
			   end;
			   if uln = . and lln = . then ctc=.V;
			   end;

			  when ('PT') do; ** in sec;
			   if uln gt .Z and lln gt .Z and labvalue gt .Z  then do;
				   if labvalue lt lln then ctc=8;
				   else if labvalue gt uln then ctc=10;
				   else ctc=7;
				end;
			   if uln = . and lln ne . and labvalue gt .Z  then do;
					if labvalue lt lln then ctc=8;
					else ctc=6;
			   end;
			   if uln ne . and lln = . and labvalue gt .Z  then do;
					if labvalue lt uln then ctc=9;
					else ctc=10;
			   end;
			   if uln = . and lln = . then ctc=.V;
			   end;

			  when ('RBC') do; ** in 1012/L;
			   if uln gt .Z and lln gt .Z and labvalue gt .Z  then do;
				   if labvalue lt lln then ctc=8;
				   else if labvalue gt uln then ctc=10;
				   else ctc=7;
				end;
			   if uln = . and lln ne . and labvalue gt .Z  then do;
					if labvalue lt lln then ctc=8;
					else ctc=6;
			   end;
			   if uln ne . and lln = . and labvalue gt .Z  then do;
					if labvalue lt uln then ctc=9;
					else ctc=10;
			   end;
			   if uln = . and lln = . then ctc=.V;
			   end;

			  when ('UAC') do; ** in mmol/L;
			   if uln gt .Z and lln gt .Z and labvalue gt .Z  then do;
				   if labvalue lt lln then ctc=8;
				   else if labvalue gt uln then ctc=10;
				   else ctc=7;
				end;
			   if uln = . and lln ne . and labvalue gt .Z  then do;
					if labvalue lt lln then ctc=8;
					else ctc=6;
			   end;
			   if uln ne . and lln = . and labvalue gt .Z  then do;
					if labvalue lt uln then ctc=9;
					else ctc=10;
			   end;
			   if uln = . and lln = . then ctc=.V;
			   end;

			  when ('UPC') do; 
			   if uln gt .Z and lln gt .Z and labvalue gt .Z  then do;
				   if labvalue lt lln then ctc=8;
				   else if labvalue gt uln then ctc=10;
				   else ctc=7;
				end;
			   if uln = . and lln ne . and labvalue gt .Z  then do;
					if labvalue lt lln then ctc=8;
					else ctc=6;
			   end;
			   if uln ne . and lln = . and labvalue gt .Z  then do;
					if labvalue lt uln then ctc=9;
					else ctc=10;
			   end;
			   if uln = . and lln = . then ctc=.V;
			   end;

			  when ('FIBRI') do;
			   if uln gt .Z and lln gt .Z and labvalue gt .Z  then do;
				   if labvalue lt lln then ctc=8;
				   else if labvalue gt uln then ctc=10;
				   else ctc=7;
				end;
			   if uln = . and lln ne . and labvalue gt .Z  then do;
					if labvalue lt lln then ctc=8;
					else ctc=6;
			   end;
			   if uln ne . and lln = . and labvalue gt .Z  then do;
					if labvalue lt uln then ctc=9;
					else ctc=10;
			   end;
			   if uln = . and lln = . then ctc=.V;

			   end;

			  when ('GFR') do;
			   if uln gt .Z and lln gt .Z and labvalue gt .Z  then do;
				   if labvalue lt lln then ctc=8;
				   else if labvalue gt uln then ctc=10;
				   else ctc=7;
				end;
			   if uln = . and lln ne . and labvalue gt .Z  then do;
					if labvalue lt lln then ctc=8;
					else ctc=6;
			   end;
			   if uln ne . and lln = . and labvalue gt .Z  then do;
					if labvalue lt uln then ctc=9;
					else ctc=10;
			   end;
			   if uln = . and lln = . then ctc=.V;

			   end;

%IF %LENGTH(&VARLIST)>0 %THEN %DO;

		%DO I = 1 %TO &Nvarlist;
			  when ("&&w&I") do; 
			   if uln gt .Z and lln gt .Z and labvalue gt .Z  then do;
				   if labvalue lt lln then ctc=8;
				   else if labvalue gt uln then ctc=10;
				   else ctc=7;
				end;
			   if uln = . and lln ne . and labvalue gt .Z  then do;
					if labvalue lt lln then ctc=8;
					else ctc=6;
			   end;
			   if uln ne . and lln = . and labvalue gt .Z  then do;
					if labvalue lt uln then ctc=9;
					else ctc=10;
			   end;
			   if uln = . and lln = . then ctc=.V;
			   end;
		%END;
%END;

			   otherwise;

			end; 

	output;

	%end; 

** JRA, 21MAR2012, if flag_test=1 then CTC is missing;
if flag_test = 1 then ctc=.;

format ctc ctc.;

run;

%sort(data=&data,var=patid &period labtest ctc);

data worstlist;
set &data;
by patid &period labtest ctc;
length labid $15. labid_label $100.;
if last.labtest;
labid=left(trim(labtest))!!&period; 
select (labtest);
 when ("ALB") labid_label="Hypoalbuminemia "!!"("!!trim(left(put(&period.,&FORMAT_PER..)))!!")";
 when ("ALP") labid_label="Alkaline phosphatase "!!"("!!trim(left(put(&period.,&FORMAT_PER..)))!!")";
 when ("ALT") labid_label="SGPT "!!"("!!trim(left(put(&period.,&FORMAT_PER..)))!!")";
 when ("ANC") labid_label="Neutropenia "!!"("!!trim(left(put(&period.,&FORMAT_PER..)))!!")";
 when ("AST") labid_label="SGOT "!!"("!!trim(left(put(&period.,&FORMAT_PER..)))!!")";
 when ("BICA") labid_label="Bicarbonate "!!"("!!trim(left(put(&period.,&FORMAT_PER..)))!!")";
 when ("BILI") labid_label="Hyperbilirubinemia "!!"("!!trim(left(put(&period.,&FORMAT_PER..)))!!")";
 when ("BUN") labid_label="BUN abnormality "!!"("!!trim(left(put(&period.,&FORMAT_PER..)))!!")";
 when ("CA_HYPER") labid_label="Hypercalcemia "!!"("!!trim(left(put(&period.,&FORMAT_PER..)))!!")";
 when ("CA_HYPO") labid_label="Hypocalcemia "!!"("!!trim(left(put(&period.,&FORMAT_PER..)))!!")";
 when ("CHL") labid_label="Chloride abnormality "!!"("!!trim(left(put(&period.,&FORMAT_PER..)))!!")";
 when ("CHOL") labid_label="Hypercholesteremia "!!"("!!trim(left(put(&period.,&FORMAT_PER..)))!!")";
 when ("CRE") labid_label="Serum creatinine "!!"("!!trim(left(put(&period.,&FORMAT_PER..)))!!")";
 when ("EOS") labid_label="Eosinopenia "!!"("!!trim(left(put(&period.,&FORMAT_PER..)))!!")";
 when ("GGT") labid_label="GGT "!!"("!!trim(left(put(&period.,&FORMAT_PER..)))!!")";
 when ("GLUC_HYPER") labid_label="Hyperglycemia "!!"("!!trim(left(put(&period.,&FORMAT_PER..)))!!")";
 when ("GLUC_HYPO") labid_label="Hypoglycemia "!!"("!!trim(left(put(&period.,&FORMAT_PER..)))!!")";
 when ("HB") labid_label="Anemia "!!"("!!trim(left(put(&period.,&FORMAT_PER..)))!!")";
 when ("HCT") labid_label="Hematocrit "!!"("!!trim(left(put(&period.,&FORMAT_PER..)))!!")";
 when ("K_HYPER") labid_label="Hyperkalemia "!!"("!!trim(left(put(&period.,&FORMAT_PER..)))!!")";
 when ("K_HYPO") labid_label="Hypokalemia "!!"("!!trim(left(put(&period.,&FORMAT_PER..)))!!")";
 when ("LDH") labid_label="LDH abnormality "!!"("!!trim(left(put(&period.,&FORMAT_PER..)))!!")";
 when ("LYM") labid_label="Lymphopenia "!!"("!!trim(left(put(&period.,&FORMAT_PER..)))!!")";
 when ("MG_HYPER") labid_label="Hypermagnesemia "!!"("!!trim(left(put(&period.,&FORMAT_PER..)))!!")";
 when ("MG_HYPO") labid_label="Hypomagnesemia "!!"("!!trim(left(put(&period.,&FORMAT_PER..)))!!")";
 when ("NA_HYPER") labid_label="Hypernatremia "!!"("!!trim(left(put(&period.,&FORMAT_PER..)))!!")";
 when ("NA_HYPO") labid_label="Hyponatremia "!!"("!!trim(left(put(&period.,&FORMAT_PER..)))!!")";
 when ("PLT") labid_label="Thrombocytopenia "!!"("!!trim(left(put(&period.,&FORMAT_PER..)))!!")";
 when ("PO") labid_label="Hypophosphatemia "!!"("!!trim(left(put(&period.,&FORMAT_PER..)))!!")";
 when ("PROT") labid_label="Total proteins "!!"("!!trim(left(put(&period.,&FORMAT_PER..)))!!")";
 when ("PT") labid_label="PT (Prothrombin Time) "!!"("!!trim(left(put(&period.,&FORMAT_PER..)))!!")";
 when ("PTT") labid_label="PTT (Partial Thromboplastin Time) "!!"("!!trim(left(put(&period.,&FORMAT_PER..)))!!")";
 when ("PTINR") labid_label="INR (International Normalized Ratio) "!!"("!!trim(left(put(&period.,&FORMAT_PER..)))!!")";
 when ("RBC") labid_label="Erythropenia "!!"("!!trim(left(put(&period.,&FORMAT_PER..)))!!")";
 when ("TRI") labid_label="Hypertriglyceridemia "!!"("!!trim(left(put(&period.,&FORMAT_PER..)))!!")";
 when ("UAC") labid_label="Uric acid "!!"("!!trim(left(put(&period.,&FORMAT_PER..)))!!")";
 when ("UPC") labid_label="Urine protein creatinine ratio "!!"("!!trim(left(put(&period.,&FORMAT_PER..)))!!")";
 when ("WBC") labid_label="WBC count "!!"("!!trim(left(put(&period.,&FORMAT_PER..)))!!")";
 when ("QTC") labid_label="Prolonged QTc interval "!!"("!!trim(left(put(&period.,&FORMAT_PER..)))!!")";
 when ("LIP") labid_label="Lipase increased "!!"("!!trim(left(put(&period.,&FORMAT_PER..)))!!")";
 when ("GFR") labid_label="Glomerular Filtration Rate "!!"("!!trim(left(put(&period.,&FORMAT_PER..)))!!")";
 when ("CPK") labid_label="CPK increased "!!"("!!trim(left(put(&period.,&FORMAT_PER..)))!!")";
 when ("CD4") labid_label="CD4 lymphocytes decreased "!!"("!!trim(left(put(&period.,&FORMAT_PER..)))!!")";
 when ("CTNT") labid_label="Cardiac troponin T "!!"("!!trim(left(put(&period.,&FORMAT_PER..)))!!")";
 when ("AMY") labid_label="Amylase "!!"("!!trim(left(put(&period.,&FORMAT_PER..)))!!")";

 %IF %LENGTH(&VARLIST)>0 %THEN %DO;

	%do i=1 %to &Nvarlist;
	 when ("&&w&i") labid_label="&&w&i "!!"("!!trim(left(put(&period.,&FORMAT_PER..)))!!")";
	%end;

 %END;

otherwise;
end;

** JRA, 21MAR2012, if flag_test=1 then CTC is missing;
if flag_test = 1 then ctc=.;

run;


proc transpose data=worstlist out=ctc_per_patient;
by patid;
var ctc;
id labid;
idlabel labid_label;
run;


data ctc_per_patient;
set ctc_per_patient;
** JRA, 21MAR2012, if it is a fake patient (patient +.M) then the record is deleted;
if patid = .M then delete;

drop _name_;
run;

data &data;
set &data;
** JRA, 21MAR2012, if flag_test=1 then the record is deleted;
if flag_test = 1 then delete;
if  labvalue lt .Z  then delete;
drop flag_test;
run;

%sort(data=ctc_per_patient,var=patid);
%sort(data=&outdata.,var=patid);
data &outdata.;
	merge &outdata.(in=a) ctc_per_patient;
	by patid;
	if a;
run;

	proc datasets nolist;
		delete worstlist root periods _contents all_labs_periods;
	run;
	quit;



%MEND;

