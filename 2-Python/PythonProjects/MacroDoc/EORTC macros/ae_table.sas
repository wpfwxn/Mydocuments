
/******************************************************************************************************* 
 * 
 * AE_TABLE.SAS
 * -------------
 *
 * This program is a macro to compute a tables presenting AE, relatives AE and SAE
 * 				and appropriate graphs (3 graphs)
 * 
 * Version date: 19/09/2016
 * Version 1.1
 * Software: SAS 9.4
 * Original author: Gaëlle Isaac 
 ******************************************************************************************************
 * 
 * HISTORY
 * 
 * Based on:
 * 		Programs for TSR_AE
 * 		by Jérôme Rapion
 * 
 * REVISIONS:
 *------------
 * June 2016	Each existing programs for TSR_AE was study specific 
 * 				==> To harmonize and automatized the reporting of AEs for medical review,TSR and stat reports
 * September 2016: Fixing bug with trt2 parameter which had not been replaced by it's macro variable version (SLI)
 *
 ********************************************************************************************************
 * PARAMETERS
 * 
 * DATA     = Name of the input dataset. (Required parameter)
 *
 * POP      = Indicator for the patients being part of the population of interest for the reporting of AEs 
 *				(safety population) =1 for patient of interest | =0 otherwise (Required parameter)
 *
 * TRT       = Name of the variable containing treatment information. Default: TRT = TRT1
 *     	       (Required parameter)
 *
 * PERIOD	= Period of interest for the reporting of AEs (baseline AEs, on-study,after surgery,...) 
 *             By default, period=(period=1).
 *
 * WHERE    = Logical condition to select records from the input dataset if not all records are to be taken into account. 
 *            (Optional parameter) Default: WHERE = 1, i.e. select all records.
 *
 * COLUMN	= Parameter used for the layout of the table. (Optional parameter)
 *		      Values:
 *                    -	ALL	= if you want to present all grades separately for each treatment arm (Default: COLUMN=ALL)
 *                    -	SEVERE = if you want to present only severe AE (grade 3, grade 4, grade 5, grade>=3, grade>=1)
 *                    -	POOLED = if you want to present grades 1&2 pooled, grades 3&4 pooled,
 *                      grade 5 alone, grade>=3, grade>=1
 *                    -	SUMMARY = if you want to present only grade>=3 and grades >=1
 *	
 * TRESHOLD  = Filter for the TABLE only. Presents AEs reaching a certain percentage of incidence 
 *             (default: THRESHOLD=0.0, i.e. select all records). This parameter has a numeric format (1 digit after the coma) and can take any value between 0.0 and 100.0.
 * 		       (Optional parameter)
 *
 * SAE 	     = Y if only serious adverse events are required (Optional parameter).
 *             Default: SAE = N, i.e. Serious adverse events are not excluded. Impacts the graphs.
 *
 * RELATED	 = Y to present only AEs related to treatment (Optional parameter).
 *		       Default: RELATED=N, i.e. AEs related to treatment are not excluded. Impacts the graphs.
 *
 * RECOVERED = Y to select AEs that are "recovered" or "recovered with sequelae". (Optional parameter) 
 *             Default: RECOVERED=N, i.e. AEs that are "recovered" or "recovered with sequelae" are not excluded. 
 *             Impacts the graph s.
 *
 * DETAILS	 = Y to create three additional listings and checks. (Optional parameter)
 *                -	Number of patients having started treatment who did not observe any AE
 *                -	Listing of Others AE
 *                -	Listing of terms from AEMODIFY variable which do not fit with CTCAE_v4_0_Term
 *            (Default: DETAILS=Y)
 *
 * ODS       = Y if ODS output is required (Optional parameter)
 *	           Default: ODS = Y, i.e. the results are presented in an RTF file.
 *
 * ODSFile   = Name of the ‘rtf’ file which is to contain the ODS output table (see note below) (Optional parameter)
 *
 * GRAPH     = To display the graphs (Optional parameter, see comments below). To be left empty if no graph is 
 *             required. Default: Graph=(), i.e. No graphs.
 *	 	       Value:
 *                -	ALL	= display all grades in the graphs representing at least 10% incidence (All grades)
 *                -	SEVERE = display only severe AEs (Grade =3) in the graphs representing at least 5% incidence
 *            NOTE: If GRPHCUTOFF= is precised, the percentages of incidence are modified 
 *            accordingly in all graphs.
 *
 * GRPHCUTOFF = Filter for the GRAPHS only (Optional parameter). Presents AEs reaching a certain percentage of incidence
 *              (default=10% for when GRAPH=ALL ; 5% when GRAPH=SEVERE). 
 *               This parameter can take any value between 0.0 and 100.0.
 *               The user must pay attention to the visibility of axis when chosing a low cut-off.
 *
 * SOCGRPH	  = Name of the output file for the graph presenting ‘Frequency of patients with clinical Advserse Events, by SOC’(Optional parameter). 
 *		        Default: SOCGRPH =(), i.e. no graph is displayed.
 *
 * AXISORDER  = Parameter linked to the SOCGRPH graph (Optional parameter). Value:
 *                    -	AXISORDER=SOC: Sort the graph by SOC (Alphabetical order) 
 *                    -	AXISORDER=freq: Sort the graph by descending frequencies.
 *              Default: AXISORDER=freq 
 *
 * PYRAMID   = Name of the output file for the ‘Pyramid Plot (Frequency of clinical adverse events, by treatment’)
 *             (Optional parameter). Only available for studies with 2 treatment arms.
 *	           Default: PYRAMID=(), i.e. no graph is displayed.
 *
 * PCT_PLOT  = Name of the output file for the scatter plot presenting 'Frequency of clinical adverse events by Treatment’ (Optional parameter)
 *	           Default: PCT_PLOT=(), i.e. no graph is displayed.
 *
 * PCT_FOREST= Name of the output file to display the scatter plot presenting frequency of clinical adverse events by Treatment AND a forest plot presenting the
 *		        Relative Risks and their confidence intervals (Optional parameter)
 *		        Default: PCT_FOREST=(), i.e. no graph is displayed.
 *
 * ALPHA     =   Alpha for CI around Relative Risks (in range 0.0001-0.999). This parameter is linked to the Forest plot (previous parameter).
 *			     Default: ALPHA = 0.05. (Optional parameter)
 *
 * DISPLAY	 =   TIFF, PNG or PDF. (Optional parameter)
 *				 Default:PNG for graphs
 *
 *EDITGRPH	 =	Y if you want to create a file .sge (SAS Graph Editor) for further manual 
 *                modifications. (Optional parameter) Default: EDITGRPH=N
 *
 * COLOUR  = 	Set to Y to obtain graph in colour, N for black and white. 
 *              Default: COLOUR=Y. (Optional parameter)
 *
 *
 ************************************************************************************************************************
 * 
 * USAGE
 * 
 * Examples of macro calls:
 * 
 * %AE_table (data=formae, pop=safetypop, Related=Y, ODSFile=MyTable_REL.rtf, SOCGrph=SOCGrph_REL.png,   PCT_Plot=PCT_REL.png, 
 *			  Pct_Forest=Forest_REL.png, Pyramid=Pyramid_REL.png);
 *
 *
 * %table_AE (data=formae, pop=safetypop,trt=trt2, period=(period=2), where=, column=summary,Treshold=, SAE=, related=Y, recovered=, details=, ODS=, 
 *		      ODSFile=C:\Temp\MyTable.rtf, graph=all, 
 *			  Pct_plot= C:\Temp\Percentage.pdf,
 *			  Pct_Forest=C:\Temp\Forest_pct.pdf, alpha=0.10, display=PDF, colour=N);
 *
 *************************************************************************************************************************
 *
 * NOTES
 * 
 * --- PARAMETERS ----
 *
 * DATA=, POP=, and PERIOD= must be prepared by the user before running the macro
 *
 * 		PERIOD=:  This parameter implies a notion of time and allows the user to select the advsere events of
 *                interest in the AE dataset (baseline AE, on-study AE). In that way, it’s different from the
 *                WHERE= statement which can filter the population of interest (for example for a country,
 *                gender,...) in the patient dataset.
 *
 * SAE: Must be a binary variable called AESER (1 or 0). To be recoded by the user if it is not already coded like that in the CRF
 * RECOVERED: The outcome variable must be called AEOUT and need to be coded as follow:
					1= Recovered  and 4= recovered with sequelae. 
			  Otherwise, must be recoded by the user before running the macro.
 * RELATED: Must be a binary variable (0=No reasonnable possibility | 1= Reasonnables possibility) called AEREL.
 *          Otherwise, must be recoded by the user before running the macro.
 * 
 *
 *---- GRAPHS ----
 * By default all the graphs will present AEs with: 
 *      - 5 percent incidence in at least one treatment arm when GRAPH=SEVERE 
 *      - 10 percent incidence in at least one treatment arm  when GRAPH=ALL
 * This is done in order to keep an acceptable and readable size for the graphs 
 * 
 * If the user wants to change this default parameter, use the parameter GRPHCUTOFF=.
 * Example: if you want severe with 10%, set GRPHCUTOFF=10.0 and GRAPH=SEVERE. 
 * 
 * IMPORTANT: If SAE=Y the parameter &GRAPH will automatically be set to &GRAPH=SEVERE.
 * 
 *
 *
 * --- ODS OPTIONS ----
 *
 *    Use ODS=Y if you wish to send the output to a Word document in rich text format (rtf).
 *    The ODSFile parameter should contain the name of the rtf file to which you wish to send the
 *    output.  However, you can choose to take control yourself of opening and closing ODS
 *    destination(s) by leaving the ODSFile parameter blank.  (This could be useful if you wish to
 *    call the macro several times and send all the results to the same file.)
 *
 *    For Example:
 *
 *    To put several tables in the same file:
 *
 *           ODS RTF FILE='C:\TEMP\TABLE.RTF' STYLE=EORTCStyle STARTPAGE=NO;
 *           %AE_table (data=formae, pop=safetypop, trt=trt2, column=all, ODSFile=, period=(period=2));

 *
 *          %AE_table (data=formae, pop=safetypop, trt=trt2, ODSFile=, period=(period=2), where=country='Netherlands',
                       column=pooled );

 *           ODS RTF CLOSE;
 *
 *    To put each table in a separate file:
 *
 *           %table_AE (data=formae, pop=safetypop,trt=trt2, period=(period=2), where=, column=pooled,Treshold=, SAE=, related=, recovered=, details=, ODS=, 
		      ODSFile=C:\Temp\MyTable.rtf );
 *
 *           %table_AE (data=formae, pop=safetypop,trt=trt2, period=(period=2), where=, column=pooled,Treshold=, SAE=Y, related=, recovered=, details=, ODS=, 
		      ODSFile=C:\Temp\MyTable_SAE.rtf);
 *
 *
 *    If you do choose to open and close the ODS destinations yourself, please note:
 * 
 * 			- You cannont use a STYLE= option
 * 			- You need to precise option=landscape to get all arms on one page.
 * 			
 *
 *************************************************************************************************************************/


%macro AE_table (data=, pop=,trt=, period=, where=, column=,Treshold=, SAE=, related=, recovered=, details=, ODS=, ODSFile=, graph=,GrphCutoff=, SOCgrph=,AxisOrder=, Pyramid=, Pct_plot=,Pct_Forest=,alpha=, display=, EditGrph=,colour=);

**Setup parameters by default**;
%if (%length(&where)=0) %then %let where=1 ;
%if (%length(&period)=0) %then %let period=1 ; 
%if (%length(&column)=0) %then %let column=ALL ; 
%if (%length(&trt)=0) %then %let trt=TRT1 ; 
%if (%length(&related)=0) %then %let related=N ;
%if (%length(&details)=0) %then %let details=Y ;
%if (%length(&SAE)=0) %then %let SAE=N ;
%if (%length(&colour)=0) %then %let colour=Y ;
%if (%length(&recovered)=0) %then %let recovered=N ;
%if (%length(&ODS)=0) %then %let ODS=Y ;
%if %UPCASE(&graph)=N %then %let graph= ;
%if (%length(&alpha)=0) %then %let alpha=0.05 ;
%if (%length(&AxisOrder)=0) %then %let AxisOrder=FREQ ;
%if (%length(&EditGrph)=0) %then %let EditGrph=N ;
%if (%length(&Display)=0) %then %let Display=PNG ;

*Upcase all Y/N parameters*;
%let related=%UPCASE(&related);
%let details=%UPCASE(&details);
%let SAE=%UPCASE(&SAE);
%let colour=%UPCASE(&colour);
%let recovered=%UPCASE(&recovered);
%let ODS=%UPCASE(&ODS);
%let EditGrph=%UPCASE(&EditGrph);
%let Display=%UPCASE(&Display);

* Present AE =>grade3 when SAE=Y*;
%if &SAE.=Y %then %let Graph=SEVERE;

/*Filter patient with where condition*/
proc sort data=&data. out=__patient2(where=(&where.)); by patid; run;


/* Import CTCAE dictionnary*/

**Import from C:\SAS in case of laptop and on the K:\ in case of fixed computer**;

%if %sysfunc (fileexist(%STR(K:\SAS\EORTC macros\CTCAEv4.xls))) > 0 %then %do;
PROC IMPORT OUT=CTCAE 
            DATAFILE="K:\SAS\EORTC macros\CTCAEv4.xls"
            DBMS=  XLS   REPLACE;
            SHEET='CTCAE'; 
RUN;
%end;

%if %sysfunc (fileexist(%STR(K:\SAS\EORTC macros\CTCAEv4.xls))) = 0 %then %do;
PROC IMPORT OUT=CTCAE 
            DATAFILE="C:\SAS\EORTC macros\CTCAEv4.xls"
            DBMS=  XLS   REPLACE;
            SHEET='CTCAE'; 
RUN;
%end;


data CTCAE;
set CTCAE;

rename SOC_Name=SOC2;
rename Term=MEDDRATERM;

SOC_Name=upcase(SOC_Name);
Term=upcase(Term);
run;

proc sort data=CTCAE out=ALLSOCv4(keep=SOC2) nodupkey;
by SOC2;
run;

data ALLSOCv4;
length threeletters $7.;
set ALLSOCv4;
threeletters=substr(upcase(SOC2),1,3);
run;

%sort(data=ALLSOCv4,var=threeletters);

**Keep the original form in case of filter on the treatment ar. Otherwise, the macro variable totarm will not be correct**;
data Keep&data.;
set &data.;
run;

*VISTA*;
data ___data2;
length MEDDRATERM $200.;
set &data.;
if &where.;
MEDDRATERM=upcase(AEMODIFY);
run;

%if &details.=Y and (%length(&ODSFile)>0) %then %do;
ods rtf file="Patient_who_start_trt.rtf" style=journal;
%end;
%if &details.=Y %then %do;
title "Number of patients having started treatment who did not observe any AE";
proc print data=___data2 noobs label;
var patid &trt. AESTDTC ;
where AEMODIFY="" and AETERM=""  and &pop.=1;
label patid="Patient ID"
	  &trt.="Treatment"
	  AESTDTC="Date of onset of AE";
run;
%end;
%if &details.=Y and (%length(&ODSFile)>0) %then %do;
ods rtf close;
%end;

title;

** Total number of patients for the calculation of percentages and title of the tables**;
*Safety population*;
proc sql;
create table __patient3 as
select *,count(distinct(patid)) as totpat
from __patient2
where &pop.=1
group by &trt.;
quit;

**Count the number of treatment arms**;
proc sql noprint;
select count(distinct(&trt.)) into :totarm
%IF %INDEX(&where.,%str(&trt.))>0 %THEN from Keep&data.;
%IF %INDEX(&where.,%str(&trt.))=0 %THEN from __patient2;
;
quit;
%put totarm &totarm.;

/*FORMAT*/
proc contents data=___data2 noprint out=__contents (where=(NAME="&trt.")) ; run; 

data __contents;
set __contents;
fmtname=cats(FORMAT,'.');
run;

data _NULL_;
set __contents;
call symput('trtfrm',format);
call symput('trtformat',fmtname);
run;
%put trtformat &trtformat. trtfrm &trtfrm.;


%sort(data=___data2,var=MEDDRATERM);
%sort(data=CTCAE,var=MEDDRATERM);

 * recodage VISTA-MEDRA CTC ;
data __formAEmerge ;
merge ___data2(in=a) CTCAE;
by MEDDRATERM;
if a;
run;

** check, if there are some withmedraV12 code missgn but SOC not OTh they are not considered ;
data __AE_OTHERS; 
length threeletters $7.;
set __formAEmerge(where= (MedDRA_Code=.));
if substr(upcase(SOC),5,3) ="OTH" ;
threeletters=substr(upcase(SOC),1,3);
drop SOC2;
run;

%sort(data=__AE_OTHERS,var=threeletters MEDDRATERM);

data __AE_OTHERS;
merge __AE_OTHERS(in=a) ALLSOCv4;
by threeletters;
if a;
run;

%sort(data=__AE_OTHERS,var=patid AESTDTC );

%if &details.=Y and (%length(&ODSFile)>0) %then %do;
ods rtf file="Other_AE.rtf" style=journal;
%end;
%if &details.=Y %then %do;
title "check : Listing of Others AE (For &period.)";
proc print data=__AE_OTHERS noobs label;
var patid SOC AETERM AEMODIFY AETOXGR AESTDTC AESER AEREL ;
where &period. and &pop.=1;
label patid="Patient ID"
	  SOC="System Organ Class"
	  AETERM="Adverse event (detailed)"
	  AEMODIFY= "Adverse event"
	  AETOXGR= "Grade"
	  AESER="Serious AE (Y/N)"
	  AEREL="AE related to treatment"
	  AESTDTC="Date of onset of AE";
run;
%end;
%if &details.=Y and (%length(&ODSFile)>0) %then %do;
ods rtf close;
%end;

data __nomatchCRF_DICTIONNARY;
set __formAEmerge;
where MedDRA_Code=.;
run;

%sort(data=__nomatchCRF_DICTIONNARY,var=patid AEMODIFY);
%sort(data=__AE_OTHERS,var=patid AEMODIFY);

** remove the others from the listing of terms from AEMODIFY variable which do not fit with CTCAE_v4_0_Term;
data __listing_ae_nonmatch;
merge __nomatchCRF_DICTIONNARY __AE_OTHERS(in=a);
by patid AEMODIFY ;
if not a;
run;

%sort(data=__listing_ae_nonmatch,var=patid AESTDTC MEDDRATERM);

%if &details.=Y and (%length(&ODSFile)>0) %then %do;
ods rtf file="Listing_not_CTC.rtf" style=journal;
%end;
%if &details.=Y %then %do;
title "check Listing of terms from AEMODIFY variable which do not fit with CTCAE_v4_0_Term";
title2 "Missing SOC must be filled in. 'Adverse event' need recoding." ;
proc print data=__listing_ae_nonmatch label noobs;
var patid &trt. SOC AETERM AEMODIFY AETOXGR AESTDTC ;
where &period. and &pop.=1;
label patid="Patient ID"
      &trt.= "Treatment"
	  SOC="System Organ Class"
	  AETERM="Adverse event (detailed)"
	  AEMODIFY= "Adverse event"
	  AETOXGR= "Grade"
	  AESTDTC="Date of onset of AE";

run;
%end;
%if &details.=Y and (%length(&ODSFile)>0) %then %do;
ods rtf close;
%END;

title;


**Before putting the Other_AE together with the AE, pooled them all by SOC to avoid all 'Specify' thus very small categories in the tables**;
data __AE_OTHERS;
set __AE_OTHERS;
MEDDRATERM= Propcase(TRIM(LEFT(SOC2))||COMPRESS(": "|| "Other"));
run;

** do not present for medical review the AE that need recoding by the DM ;
data __formAEclean; 
set __formAEmerge(where=(MedDRA_Code ne .)) __AE_OTHERS; * add of SOC OTHERS to non OThers terms;
run;

proc sort data=__formAEclean out=list_of_meddraterm_in_this_study(keep=MEDDRATERM) nodupkey;
	by MEDDRATERM;
run;

proc sort data=__formAEclean out=list_of_SOC_in_this_study(keep=SOC2) nodupkey;
	by SOC2;
run;

data list_of_meddraterm_in_this_study;
set list_of_meddraterm_in_this_study;
indTERM=_n_;
run;

data list_of_SOC_in_this_study;
length threeletters $7.;
set list_of_SOC_in_this_study (where =(SOC2 ne ''));
indSOC=_n_;
threeletters=substr(upcase(SOC2),1,3);
run;

/*ADD Medraterm*/
%sort(data=__formAEclean,var=MEDDRATERM);
%sort(data=list_of_meddraterm_in_this_study,var=MEDDRATERM);

data __formAEclean;
merge __formAEclean list_of_meddraterm_in_this_study;
by MEDDRATERM;
run;

/*ADD SOC*/
%sort(data=__formAEclean,var=SOC2);
%sort(data=list_of_SOC_in_this_study,var=SOC2);

data __formAEclean;
merge __formAEclean(in=a) list_of_SOC_in_this_study;
by SOC2;
if a;
run;


/*MAXIMUM GRADE*/

%sort(data=__formAEclean,var=MEDDRATERM);

data _null_;
set list_of_meddraterm_in_this_study;
call symput('NTERM',compress(_n_));
run;
data _null_;
set list_of_SOC_in_this_study;
call symput('NSOC',compress(_n_));
run;
%put NSOC &NSOC NTERM &NTERM;


data _null_;
set list_of_meddraterm_in_this_study;
call symput("labelTERM"||compress(_n_) ,trim(left(MEDDRATERM)));
run;


proc freq data=__formAEclean noprint;
tables SOC2*MEDDRATERM*PATID/ out=__code_SOC_PT;
where &period. 	%if (&related.=Y and &SAE.=N and &recovered.=N) %then and AEREL =1; 
				%if (&related.=N and &SAE.=Y and &recovered.=N) %then and AESER=1;
				%if (&related.=N and &SAE.=N and &recovered.=Y) %then and AEOUT in (1,4);

				%if (&related.=Y and &SAE.=Y and &recovered.=N) %then and AEREL=1 and AESER=1;
				%if (&related.=Y and &SAE.=N and &recovered.=Y) %then and AEREL=1 and AEOUT in (1,4); 
				%if (&related.=N and &SAE.=Y and &recovered.=Y) %then and AESER=1 and AEOUT in (1,4); 

				%if (&related.=Y and &SAE.=Y and &recovered.=Y) %then and AEREL=1 and AESER=1 and AEOUT in (1,4); 
;
run;

*Select period**;
proc freq data=__formAEclean noprint;
tables SOC2*PATID/ out=__code_SOC;
where  &period. %if (&related.=Y and &SAE.=N and &recovered.=N) %then and AEREL =1; 
				%if (&related.=N and &SAE.=Y and &recovered.=N) %then and AESER=1;
				%if (&related.=N and &SAE.=N and &recovered.=Y) %then and AEOUT in (1,4);

				%if (&related.=Y and &SAE.=Y and &recovered.=N) %then and AEREL=1 and AESER=1;
				%if (&related.=Y and &SAE.=N and &recovered.=Y) %then and AEREL=1 and AEOUT in (1,4); 
				%if (&related.=N and &SAE.=Y and &recovered.=Y) %then and AESER=1 and AEOUT in (1,4); 

				%if (&related.=Y and &SAE.=Y and &recovered.=Y) %then and AEREL=1 and AESER=1 and AEOUT in (1,4); 
;
run;

*Create generale term*;
data __code_SOC;
set __code_SOC;
MEDDRATERM="*GENERAL TERM";
run;

data ___code_SOC_PT;
set __code_SOC_PT __code_SOC;
run;

%sort(data=___code_SOC_PT,var=patid) ;

proc sort data=___code_SOC_PT nodupkey;
by patid SOC2 MEDDRATERM;
run;

%sort(data=___code_SOC_PT,var=PATID SOC2 MEDDRATERM );


*Compute worst grade for all differents compbination of parameters*;
%IF &related.=N and &SAE.=N and &recovered.=N %THEN %DO;
%GMAX (DATA=__formAEclean, VAR=AETOXGR, BYVAR=patid MEDDRATERM, OUTDATA=__WORST_PT,OUTVAR=WORST_GR_PT, sel=( &period.)); 
%GMAX (DATA=__formAEclean, VAR=AETOXGR, BYVAR=patid SOC2, OUTDATA=__WORST_SOC,OUTVAR=WORST_GR_SOC, sel=( &period.));
%END;
%IF &related.=N and &SAE.=Y and &recovered.=N %THEN %DO;
%GMAX (DATA=__formAEclean, SEL=((&period.) and (AESER =1)), VAR=AETOXGR, BYVAR=patid MEDDRATERM, OUTDATA=__WORST_PT,OUTVAR=WORST_GR_PT); 
%GMAX (DATA=__formAEclean, SEL=(( &period.) and (AESER =1)), VAR=AETOXGR, BYVAR=patid SOC2, OUTDATA=__WORST_SOC,OUTVAR=WORST_GR_SOC);
%END;
%IF &related.=Y and &SAE.=N and &recovered.=N %THEN %DO;
%GMAX (DATA=__formAEclean,SEL=(( &period.) and (AEREL =1)), VAR=AETOXGR, BYVAR=patid MEDDRATERM, OUTDATA=__WORST_PT,OUTVAR=WORST_GR_PT); 
%GMAX (DATA=__formAEclean,SEL=(( &period.) and (AEREL =1)), VAR=AETOXGR, BYVAR=patid SOC2, OUTDATA=__WORST_SOC,OUTVAR=WORST_GR_SOC);
%END;
%IF &related.=N and &SAE.=N and &recovered.=Y %THEN %DO;
%GMAX (DATA=__formAEclean, SEL=(( &period.) and (AEOUT in (1,4))), VAR=AETOXGR, BYVAR=patid MEDDRATERM, OUTDATA=__WORST_PT,OUTVAR=WORST_GR_PT); 
%GMAX (DATA=__formAEclean, SEL=(( &period.) and (AEOUT in (1,4))), VAR=AETOXGR, BYVAR=patid SOC2, OUTDATA=__WORST_SOC,OUTVAR=WORST_GR_SOC);
%END;
%IF &related.=N and &SAE.=Y and &recovered.=Y %THEN %DO;
%GMAX (DATA=__formAEclean, SEL=(( &period.) and (AEOUT in (1,4)) and (AESER =1)), VAR=AETOXGR, BYVAR=patid MEDDRATERM, OUTDATA=__WORST_PT,OUTVAR=WORST_GR_PT); 
%GMAX (DATA=__formAEclean, SEL=(( &period.) and (AEOUT in (1,4)) and (AESER =1)), VAR=AETOXGR, BYVAR=patid SOC2, OUTDATA=__WORST_SOC,OUTVAR=WORST_GR_SOC);
%END;
%IF &related.=Y and &SAE.=N and &recovered.=Y %THEN %DO;
%GMAX (DATA=__formAEclean, SEL=(( &period.) and (AEOUT in (1,4)) and (AEREL =1)), VAR=AETOXGR, BYVAR=patid MEDDRATERM, OUTDATA=__WORST_PT,OUTVAR=WORST_GR_PT); 
%GMAX (DATA=__formAEclean, SEL=(( &period.) and (AEOUT in (1,4)) and (AEREL =1)), VAR=AETOXGR, BYVAR=patid SOC2, OUTDATA=__WORST_SOC,OUTVAR=WORST_GR_SOC);
%END;
%IF &related.=Y and &SAE.=Y and &recovered.=N %THEN %DO;
%GMAX (DATA=__formAEclean, SEL=(( &period.) and (AESER =1) and (AEREL =1)), VAR=AETOXGR, BYVAR=patid MEDDRATERM, OUTDATA=__WORST_PT,OUTVAR=WORST_GR_PT); 
%GMAX (DATA=__formAEclean, SEL=(( &period.) and (AESER =1) and (AEREL =1)), VAR=AETOXGR, BYVAR=patid SOC2, OUTDATA=__WORST_SOC,OUTVAR=WORST_GR_SOC);
%END;
%IF &related.=Y and &SAE.=Y and &recovered.=Y %THEN %DO;
%GMAX (DATA=__formAEclean,SEL=(( &period.) and (AEREL =1) and (AESER =1) and (AEOUT in (1,4))), VAR=AETOXGR, BYVAR=patid MEDDRATERM, OUTDATA=__WORST_PT,OUTVAR=WORST_GR_PT); 
%GMAX (DATA=__formAEclean,SEL=(( &period.) and (AEREL =1) and (AESER =1) and (AEOUT in (1,4))), VAR=AETOXGR, BYVAR=patid SOC2, OUTDATA=__WORST_SOC,OUTVAR=WORST_GR_SOC);
%END;



data __WORST_SOC;
set __WORST_SOC;
MEDDRATERM="*GENERAL TERM";
run;

data ___AE;
merge ___code_SOC_PT(in=a) __WORST_SOC ;
by PATID SOC2 MEDDRATERM;
if a;
run;


%sort(data=___AE,var=PATID MEDDRATERM );

data ___AE;
merge ___AE(in=a) __WORST_PT ;
by PATID MEDDRATERM;
if a;
run;

%sort(data=___AE,var=patid);
proc sort data=__patient3 nodupkey out=___pat (keep=patid totpat &trt.) ;by patid; run;

data ___AEv2;
merge ___AE(in=a) ___pat; 
by patid;
if a;
if WORST_GR_SOC=1 then COUNT_GR_SOC1=1;
if WORST_GR_PT=1 then COUNT_GR_PT1=1;

if WORST_GR_SOC=2 then COUNT_GR_SOC2=1;
if WORST_GR_PT=2 then COUNT_GR_PT2=1;

if WORST_GR_SOC=3 then COUNT_GR_SOC3=1;
if WORST_GR_PT=3 then COUNT_GR_PT3=1;

if WORST_GR_SOC=4 then COUNT_GR_SOC4=1;
if WORST_GR_PT=4 then COUNT_GR_PT4=1;

if WORST_GR_SOC=5 then COUNT_GR_SOC5=1;
if WORST_GR_PT=5 then COUNT_GR_PT5=1;


if WORST_GR_SOC >2 then COUNT_GR_SOC345=1;
if WORST_GR_SOC >. then COUNT_GR_SOC_ALL=1;

if WORST_GR_PT in (1,2) then COUNT_GR_PT12=1;
if WORST_GR_PT in (3,4) then COUNT_GR_PT34=1;
if WORST_GR_PT >2 then COUNT_GR_PT345=1;
if WORST_GR_PT >. then COUNT_GR_PT_ALL=1;

if WORST_GR_SOC=. and WORST_GR_PT=. then delete;
label COUNT_GR_PT1="Grade 1";
label COUNT_GR_PT2="Grade 2";
label COUNT_GR_PT3="Grade 3";
label COUNT_GR_PT4="Grade 4";
label COUNT_GR_PT5="Grade 5";
label COUNT_GR_PT12="Grade 1/2";
label COUNT_GR_PT34="Grade 3/4";
label COUNT_GR_PT345="Grade => 3";
label COUNT_GR_PT_ALL= "Grade => 1";
run;


/*Count the number of patients by treatment arm and grades. To be displayed in the final table*/

proc sql;
create table __CountGrade as
select patid, MEDDRATERM, SOC2, WORST_GR_PT, &trt., totpat, max (WORST_GR_PT) as pts_maxgrade
from ___aev2
group by patid;
quit;

data __GR1 __GR2 __GR3 __GR4 __GR5 __GR12 __GR34 __GR345 __GRAll;
set __CountGrade;
by patid;
	if pts_maxgrade=1 then output __GR1;
	if pts_maxgrade=2 then output __GR2;
	if pts_maxgrade=3 then output __GR3;
	if pts_maxgrade=4 then output __GR4;
	if pts_maxgrade=5 then output __GR5;
	if pts_maxgrade in (1,2) then output __GR12;
	if pts_maxgrade in (3,4) then output __GR34;
	if pts_maxgrade in (3,4,5) then output __GR345;
	if pts_maxgrade ne . then output __GRAll;
run;

%macro totalPatient (dataset=, dataset2=, dataset3=,dataset4=, dataset5=, dataset6=, datasetFinal=, N=, pct=, Npct=, 
					 Npcttrt1=, Npcttrt2=, Npcttrt3=, Npcttrt4=);
proc sql;
create table &dataset2. as
	select*, count(distinct(patid)) as &N.,
			 (calculated &N./totpat)*100 as &Pct. format=8.1 ,
			 compress(put(calculated &N.,3.))||" ("||  compress(put(calculated &Pct.,8.1))||")" as &Npct.			
	from &dataset.
	group by &trt.;
quit; 

proc sort data=&dataset2. nodupkey; by &trt.; run;

data &dataset3. (rename=(&Npct.=&Npcttrt1.)) &dataset4. (rename=(&Npct.=&Npcttrt2.))
	 &dataset5. (rename=(&Npct.=&Npcttrt3.)) &dataset6. (rename=(&Npct.=&Npcttrt4.));
set &dataset2.;
Concatenation_="NUMBER OF PATIENTS WITH AE's";
	if &trt.=1 then output &dataset3.;
	if &trt.=2 then output &dataset4.;
	if &trt.=3 then output &dataset5.;
	if &trt.=4 then output &dataset6.;
run;

data &datasetFinal.;
length Concatenation_ $270. &Npcttrt1. &Npcttrt2. &Npcttrt3. &Npcttrt4. $27.;
merge &dataset3. &dataset4. &dataset5. &dataset6.;
by Concatenation_; 
run;

%mend;
**Grade 1**;
%totalPatient (dataset=__GR1, dataset2=__Grade1, dataset3=__Grade11,dataset4=__Grade21, dataset5=__Grade31, dataset6=__Grade41, datasetFinal=__Grade1all, N=N1, pct=Pct1, Npct=Npct1, 
					 Npcttrt1=Npct11, Npcttrt2=Npct21, Npcttrt3=Npct31, Npcttrt4=Npct41);
**Grade 2**;
%totalPatient (dataset=__GR2, dataset2=__Grade2, dataset3=__Grade12,dataset4=__Grade22, dataset5=__Grade32, dataset6=__Grade42, datasetFinal=__Grade2all, N=N2, pct=Pct2, Npct=Npct2, 
					 Npcttrt1=Npct12, Npcttrt2=Npct22, Npcttrt3=Npct32, Npcttrt4=Npct42);
**Grade 3**;
%totalPatient (dataset=__GR3, dataset2=__Grade3, dataset3=__Grade13,dataset4=__Grade23, dataset5=__Grade33, dataset6=__Grade43, datasetFinal=__Grade3all, N=N3, pct=Pct3, Npct=Npct3, 
					 Npcttrt1=Npct13, Npcttrt2=Npct23, Npcttrt3=Npct33, Npcttrt4=Npct43);
**Grade 4**;
%totalPatient (dataset=__GR4, dataset2=__Grade4, dataset3=__Grade14,dataset4=__Grade24, dataset5=__Grade34, dataset6=__Grade44, datasetFinal=__Grade4all, N=N4, pct=Pct4, Npct=Npct4, 
					 Npcttrt1=Npct14, Npcttrt2=Npct24, Npcttrt3=Npct34, Npcttrt4=Npct44);
**Grade 5**;
%totalPatient (dataset=__GR5, dataset2=__Grade5, dataset3=__Grade15,dataset4=__Grade25, dataset5=__Grade35, dataset6=__Grade45, datasetFinal=__Grade5all, N=N5, pct=Pct5, Npct=Npct5, 
					 Npcttrt1=Npct15, Npcttrt2=Npct25, Npcttrt3=Npct35, Npcttrt4=Npct45);
**Grades 1/2**;
%totalPatient (dataset=__GR12, dataset2=__Grade12, dataset3=__Grade112,dataset4=__Grade212, dataset5=__Grade312, dataset6=__Grade412, datasetFinal=__Grade12all, N=N12, pct=Pct12, Npct=Npct12, 
					 Npcttrt1=Npct112, Npcttrt2=Npct212, Npcttrt3=Npct312, Npcttrt4=Npct412);
**Grades 3/4**;
%totalPatient (dataset=__GR34, dataset2=__Grade34, dataset3=__Grade134,dataset4=__Grade234, dataset5=__Grade334, dataset6=__Grade434, datasetFinal=__Grade34all, N=N34, pct=Pct34, Npct=Npct34, 
					 Npcttrt1=Npct134, Npcttrt2=Npct234, Npcttrt3=Npct334, Npcttrt4=Npct434);
**Grades 3/4/5**;
%totalPatient (dataset=__GR345, dataset2=__Grade345, dataset3=__Grade1345,dataset4=__Grade2345, dataset5=__Grade3345, dataset6=__Grade4345, datasetFinal=__Grade345all, N=N345, pct=Pct345, Npct=Npct345, 
					 Npcttrt1=Npct1345, Npcttrt2=Npct2345, Npcttrt3=Npct3345, Npcttrt4=Npct4345);
**Grades all**;
%totalPatient (dataset=__GRall, dataset2=__GradeAll, dataset3=__Grade1all_,dataset4=__Grade2all_, dataset5=__Grade3all_, dataset6=__Grade4all_, datasetFinal=__ALLGrade, N=NALL, pct=PctAll, Npct=NpctAll, 
					 Npcttrt1=Npct1ALL, Npcttrt2=Npct2All, Npcttrt3=Npct3All, Npcttrt4=Npct4All);

data __totalpatient;
merge __Grade1all __Grade2all __Grade3all __Grade4all __Grade5all __Grade12all __Grade34all __Grade345all __ALLGrade;
by Concatenation_; 
drop N1-N5 N12 N34 N345 Nall pct1-pct5 pct12 pct34 pct345 pctAll;
run;


/*SUM OF ALL GRADES*/
%sort(data=___AEv2,var=patid) ;

proc sql;
create table __sumall as
select * , sum(COUNT_GR_PT1) as tot_GR_1,
		   sum(COUNT_GR_PT2) as tot_GR_2,
		   sum(COUNT_GR_PT3) as tot_GR_3,
		   sum(COUNT_GR_PT4) as tot_GR_4,
		   sum(COUNT_GR_PT5) as tot_GR_5,
		   sum (COUNT_GR_PT_ALL) as tot_ALL_GR,
 		   sum(COUNT_GR_PT345) as tot_GR_345,
 		   sum(COUNT_GR_PT12) as tot_GR_12,
		   sum(COUNT_GR_PT34) as tot_GR_34
from ___AEv2
group by &trt., SOC2,MEDDRATERM;
quit;

/*PERCENTAGE OF EACH GRADES + CREATION OF &ANYGR5 TO CHECK IF GRADE 5 NEED TO BE DISPLAYED*/
data __sumall; 
set __sumall;
totall=sum (of tot_GR:);
PCT_GR_1=ROUND((tot_GR_1/totpat)*100, 0.1);
PCT_GR_2=ROUND((tot_GR_2/totpat)*100, 0.1);
PCT_GR_3=ROUND((tot_GR_3/totpat)*100, 0.1);
PCT_GR_4=ROUND((tot_GR_4/totpat)*100, 0.1);
PCT_GR_5=ROUND((tot_GR_5/totpat)*100, 0.1);
PCT_GR_345=ROUND((tot_GR_345/totpat)*100, 0.1);
PCT_GR_ALL=ROUND((tot_ALL_GR/totpat)*100, 0.1);
PCT_GR_12=ROUND((tot_GR_12/totpat)*100, 0.1);
PCT_GR_34=ROUND((tot_GR_34/totpat)*100, 0.1);
*if totall=. then delete;
drop totall;
run;

proc sql noprint;
select case
		when sum (tot_GR_5) >0.0 then 'Y'
		else 'N'
		end into :AnyGR5
	from __sumall;
quit;

%put number_dead &AnyGR5.;


/*Treshold parameter: select AE representing more than x % incidence in at least one of the treatment arms
  By default, no treshold. Otherwise, use the one defined by the user*/

data __summall_1 ;
set __sumall;
if MEDDRATERM = "*GENERAL TERM" then keepAll=1;
	%IF (%LENGTH(&Treshold.))=0 %THEN %DO; 
		keepAll=1;
	%END;

	%IF (%LENGTH(&Treshold.))>0 %THEN %DO; 
			if &trt.=1 then do;
				if PCT_GR_ALL>&Treshold. then keepAll=1;
			end;
		%IF &totarm.>1 %THEN %DO;
			if &trt.=2 then do;
				if PCT_GR_ALL>&Treshold. then keepAll=1;
			end;
		%END;
		%IF &totarm.=3 %THEN %DO;
			if &trt.=3 then do;
				if PCT_GR_ALL>&Treshold. then keepAll=1;
			end;
		%END;
		%IF &totarm.=4 %THEN %DO;
			if &trt.=3 then do;
				if PCT_GR_ALL>&Treshold. then keepAll=1;
			end;
			if &trt.=4 then do;
				if PCT_GR_ALL>&Treshold. then keepAll=1;
			end;
		%END;
	%END;
run;

proc sort data=__summall_1 (where=(keepAll=1)) out=__MoreTreshold(keep=MEDDRATERM ) nodupkey ; by MEDDRATERM; run;

%sort(data=__summall_1,var=MEDDRATERM) ;

data __summall_1;
merge __summall_1 __MoreTreshold (in=a);
by MEDDRATERM;
if a;
run;

proc sort data=__summall_1 out=__table_ae nodupkey; by SOC2 MEDDRATERM &trt.; run;

data __table_ae;
set __table_ae;
	Npct1=compress(tot_GR_1)||" ("|| compress(PCT_GR_1)||")";
	Npct2=compress(tot_GR_2)||" ("|| compress(PCT_GR_2)||")";
	Npct3=compress(tot_GR_3)||" ("|| compress(PCT_GR_3)||")";
	Npct4=compress(tot_GR_4)||" ("|| compress(PCT_GR_4)||")";
	Npct5=compress(tot_GR_5)||" ("|| compress(PCT_GR_5)||")";
	Npct345=compress(tot_GR_345)||" ("|| compress(PCT_GR_345)||")";
	NpctALL=compress(tot_ALL_GR)||" ("|| compress(PCT_GR_ALL)||")";
	Npct12=compress(tot_GR_12)||" ("|| compress(PCT_GR_12)||")";
	Npct34=compress(tot_GR_34)||" ("|| compress(PCT_GR_34)||")";

	if MEDDRATERM="*GENERAL TERM" then do;
		Npct1=""; Npct2=""; Npct3=""; Npct4=""; Npct5=""; Npct345=""; NpctALL=""; Npct12=""; Npct34="";
	    MEDDRATERM="";
	end;

	if Npct1 =". (.)"  then Npct1="";
	if Npct2 =". (.)"  then Npct2="";
	if Npct3 =". (.)"  then Npct3="";
	if Npct4 =". (.)"  then Npct4="";
	if Npct5 =". (.)"  then Npct5="";
	if Npct345 =". (.)"  then Npct345="";
	if NpctALL =". (.)"  then NpctALL="";
	if Npct12 =". (.)"  then Npct12="";
	if Npct34 =". (.)"  then Npct34="";
run;

/*Split table by treatment*/

data __arm1 %if &totarm.>1 %then __arm2; 
		  %if &totarm.=3 %then __arm3;
		  %if &totarm.=4 %then __arm3 __arm4;
;
set __table_ae;
if &trt.=1 then output __arm1;
%if &totarm.>1 %then if &trt.=2 then output __arm2;;
%if &totarm.=3 %then if &trt.=3 then output __arm3;;
%if &totarm.=4 %then %do;
if &trt.=3 then output __arm3; 
if &trt.=4 then output __arm4;
%end;
run;

data __arm1;
set __arm1;
rename Npct1=Npct11;
rename Npct2=Npct12;
rename Npct3=Npct13;
rename Npct4=Npct14;
rename Npct5=Npct15;
rename NpctALL=Npct1ALL;
rename Npct345=Npct1345;
rename Npct12=Npct112;
rename Npct34=Npct134;
run;

%if &totarm.>1 %then %do;
data __arm2;
set __arm2;
rename Npct1=Npct21;
rename Npct2=Npct22;
rename Npct3=Npct23;
rename Npct4=Npct24;
rename Npct5=Npct25;
rename NpctALL=Npct2ALL;
rename Npct345=Npct2345;
rename Npct12=Npct212;
rename Npct34=Npct234;
run;
%end;

%if &totarm.=3 %then %do;
data __arm3;
set __arm3;
rename Npct1=Npct31;
rename Npct2=Npct32;
rename Npct3=Npct33;
rename Npct4=Npct34;
rename Npct5=Npct35;
rename NpctALL=Npct3ALL;
rename Npct345=Npct3345;
rename Npct12=Npct312;
rename Npct34=Npct334;
run;
%end;

%if &totarm.=4 %then %do;

data __arm3;
set __arm3;
rename Npct1=Npct31;
rename Npct2=Npct32;
rename Npct3=Npct33;
rename Npct4=Npct34;
rename Npct5=Npct35;
rename NpctALL=Npct3ALL;
rename Npct345=Npct3345;
rename Npct12=Npct312;
rename Npct34=Npct334;
run;

data __arm4;
set __arm4;
rename Npct1=Npct41;
rename Npct2=Npct42;
rename Npct3=Npct43;
rename Npct4=Npct44;
rename Npct5=Npct45;
rename NpctALL=Npct4ALL;
rename Npct345=Npct4345;
rename Npct12=Npct412;
rename Npct34=Npct434;
run;
%end;

%sort(data=__arm1,var=SOC2 Meddraterm &trt.) ;
%if &totarm.>1 %then %do;
%sort(data=__arm2,var=SOC2 Meddraterm &trt.) ;
%end;
%if &totarm.=3 %then %do;
%sort(data=__arm3,var=SOC2 Meddraterm &trt.) ;
%end;
%if &totarm.=4 %then %do;
%sort(data=__arm3,var=SOC2 Meddraterm &trt.) ;
%sort(data=__arm4,var=SOC2 Meddraterm &trt.) ;
%end;

%IF &totarm.=1 %THEN %DO;
data __table_ae;
set __arm1;
by SOC2 meddraterm %if %INDEX(&where.,%STR(&trt.))>0 %then &trt.;;
run;
%END;

%IF &totarm.>1 %THEN %DO;
data __table_ae;
merge __arm1 __arm2   %if &totarm.=3 %then __arm3;
		          %if &totarm.=4 %then __arm3 __arm4;
;
by SOC2 meddraterm; 
run;
%END;

/*CREATE TABLE*/

proc format cntlout=__formats(where=(FMTNAME="&trtfrm."));
run;

data __formats;
set __formats;
value=start*1;
if value>.;
run;

data _null_;
set __formats;
call symput("TRT"||compress(value),trim(left(LABEL)));
run;


/*NOTE:Macro variables not defined if no adverse event in 1 of the two groups*/
proc sql noprint;
 select count(distinct(patid)) into :NPAT1 from __patient3
 where &trt.=1 and &period.;
 quit;

%if &totarm.>1 %then %do;
proc sql noprint;
 select count(distinct(patid)) into :NPAT2 from __patient3
 where &trt.=2 and &period.;
 quit;
%end;

%if &totarm.=3 %then %do;
proc sql noprint;
 select count(distinct(patid)) into :NPAT3 from __patient3
 where &trt.=3 and &period.;
 quit;
%end;

%if &totarm.=4 %then %do;
proc sql noprint;
 select count(distinct(patid)) into :NPAT3 from __patient3
 where &trt.=3 and &period.;
 quit;

proc sql noprint;
 select count (distinct(patid)) into :NPAT4 from __patient3
 where &trt.=4 and &period.;
quit;
%end;


%if (&NPAT1>0) %then %do;
data _null_;
set __patient3;
if &trt.=1;
call symput("NTOT"||compress(&trt.),compress(totpat));
run;
%end;
%else %do; %let NTOT1=0;
%end;

%IF &totarm.>1 %THEN %DO;
%if (&NPAT2 >0) %then %do;
data _null_;
set __patient3;
if &trt.=2;
call symput("NTOT"||compress(&trt.),compress(totpat));
run;
%end;
%else %do; %let NTOT2=0;
%end;
%end;

%IF &totarm.=3 %THEN %DO;
	%if (&NPAT3>0) %then %do;
	data _null_;
	set __patient3;
	if &trt.=3;
	call symput("NTOT"||compress(&trt.),compress(totpat));
	run;
	%end;
	%else %do; %let NTOT3=0;
	%end;
%END;

%IF &totarm.=4 %THEN %DO;
	%if (&NPAT3>0) %then %do;
		data _null_;
		set __patient3;
		if &trt.=3;
		call symput("NTOT"||compress(&trt.),compress(totpat));
		run;
	%end;
	%else %do; %let NTOT3=0;
	%end;

	%if (&NPAT4>0) %then %do;
		data _null_;
		set __patient3;
		if &trt.=4;
		call symput("NTOT"||compress(&trt.),compress(totpat));
		run;
	%end;
	%else %do; %let NTOT4=0;
	%end;
%END;


/*CREATE TABLE*/

**Delete if a SOC or MEDDRATERM not represented in the AE**;
%sort(data=__table_ae,var=SOC2 meddraterm ) ;

%IF %UPCASE(&column.)=SEVERE %THEN %DO;
data __table_ae; 
set __table_ae;
	if MEDDRATERM ne "" then do; 
			%if &totarm.=1 %then %do; if (Npct1345="") then delete; %end;
			%if &totarm.=2 %then %do; if (Npct1345="" and Npct2345="") then delete; %end;
			%if &totarm.=3 %then %do; if (Npct1345="" and Npct2345="" and Npct3345="") then delete; %end;
			%if &totarm.=4 %then %do; if (Npct1345="" and Npct2345="" and Npct3345="" and Npct4345="") then delete; %end;
	end;
run;
%END;


data __table_ae;
set __table_ae;
by SOC2 meddraterm ;
retain SOC_alone;
if first.SOC2 then SOC_alone=0;
if first.MEDDRATERM then SOC_alone=SOC_alone+1;
run;

**Put the other category at the end for the table*;
data __table_ae;
set __table_ae;
if INDEX(UPCASE(MEDDRATERM),'OTHER,')>0 or INDEX(UPCASE(MEDDRATERM),' OTHER ')>0 or INDEX(UPCASE(MEDDRATERM),': OTHER')>0 
	or INDEX(UPCASE(MEDDRATERM),':OTHER')>0 then MEDDRATERM="XXX";
run;

proc sort data=__table_ae; by SOC2 MEDDRATERM; run;

**Delete when no AE **;
data __table_final;
set __table_ae;
	if MEDDRATERM ne "" then do; 
		%IF %UPCASE(&column.)=SEVERE %THEN %DO;
			%if &totarm.=1 %then %do; if (Npct1345="") then delete; %end;
			%if &totarm.=2 %then %do; if (Npct1345="" and Npct2345="") then delete; %end;
			%if &totarm.=3 %then %do; if (Npct1345="" and Npct2345="" and Npct3345="") then delete; %end;
			%if &totarm.=4 %then %do; if (Npct1345="" and Npct2345="" and Npct3345="" and Npct4345="") then delete; %end;
		%END;
		%ELSE %DO;
			%if &totarm.=1 %then %do; if (Npct1ALL="") then delete; %end;
			%if &totarm.=2 %then %do; if (Npct1ALL="" and Npct2ALL="") then delete; %end;
			%if &totarm.=3 %then %do; if (Npct1ALL="" and Npct2ALL="" and Npct3ALL="") then delete; %end;
			%if &totarm.=4 %then %do; if (Npct1ALL="" and Npct2ALL="" and Npct3ALL="" and Npct4ALL="") then delete; %end;
		%END;
	end;
run;

%GMAX (DATA=__table_final, SEL=, VAR=SOC_alone, BYVAR=SOC2, OUTVAR=max_SOC, OUTDATA=, MERGE=__table_final);

data __table_final;
set __table_final;
if max_SOC=1 then delete; 
run;


/*Prepare the column with SOC and MEDDRATERM with an indent*/

data __table_final;
set __table_final;
if MEDDRATERM ne "" then SOC2="";
MEDDRATERM=PROPCASE(MEDDRATERM);
if INDEX(UPCASE(MEDDRATERM),'XXX')>0 then MEDDRATERM="Other AE";
Concatenation_=LEFT(SOC2 ||"   "|| MEDDRATERM);
run;

data __table_final;
set __table_final;
if SOC2 = "" then Concatenation_="    "||Concatenation_;
run;

/*Add the total of patient by treatment arm and grade*/
data __table_final;
set __totalpatient __table_final;
run;


/*=============================================================================================
 ================================	CREATE TABLE    ===========================================
==============================================================================================*/

/*********************************************************************************************************
*********************************************************************************************************
In case of filter on the treatment arm with the were= condition some additional paramater need to be created to 
 avoid mistakes in the table.*/

**All cases for two arms**;
%IF &totarm.=2 %THEN %DO;
	%IF %INDEX(&where.,%str(&trt.=1))>0 or %INDEX(&where.,%str(&trt.ne 2))>0 or %INDEX(&where.,%str(&trt.^= 2))>0 %THEN %LET show1=Y;%ELSE %LET show1=N;
	%IF %INDEX(&where.,%str(&trt.=2))>0 or %INDEX(&where.,%str(&trt.ne 1))>0 or %INDEX(&where.,%str(&trt.^= 1))>0 %THEN %LET show2=Y;%ELSE %LET show2=N;
%END;
		%LET show3=N;
		%LET show4=N;

**All cases for three arms**;
%IF &totarm.>2 %THEN %DO;
	%IF %INDEX(&where.,%str(&trt.=1))>0  or %INDEX(&where.,%str(&trt. ne 2))>0 or %INDEX(&where.,%str(&trt. ^= 2))>0 or
	    %INDEX(&where.,%str(&trt. ne 3))>0 or %INDEX(&where.,%str(&trt. ^= 3))>0 or 
	    %INDEX(&where.,%str(&trt. not in (2,3)))>0 or %INDEX(&where.,%str(&trt. in (1,3)))>0 or 
		%INDEX(&where.,%str(&trt. in (1,2)))>0 or %INDEX(&where.,%str(&trt. in (1,4)))>0
	%THEN %LET show1=Y; %ELSE %LET show1=N;

	%IF %INDEX(&where.,%str(&trt.=2))>0 or %INDEX(&where.,%str(&trt. ne 1))>0 or %INDEX(&where.,%str(&trt. ^= 1))>0 or
	    %INDEX(&where.,%str(&trt. ne 3))>0 or %INDEX(&where.,%str(&trt. ^= 3))>0 or 
	    %INDEX(&where.,%str(&trt. not in (1,3)))>0 or %INDEX(&where.,%str(&trt. in (1,2)))>0 or 
		%INDEX(&where.,%str(&trt. in (2,3)))>0 or %INDEX(&where.,%str(&trt. in (2,4)))>0
	%THEN %LET show2=Y; %ELSE %LET show2=N;

	%IF %INDEX(&where.,%str(&trt.=3))>0 or %INDEX(&where.,%str(&trt. ne 1))>0 or %INDEX(&where.,%str(&trt. ^= 1))>0 or
	    %INDEX(&where.,%str(&trt. ne 2))>0 or %INDEX(&where.,%str(&trt. ^= 2))>0 or 
	    %INDEX(&where.,%str(&trt. not in (1,2)))>0 or %INDEX(&where.,%str(&trt. in (1,3)))>0 or 
		%INDEX(&where.,%str(&trt. in (2,3)))>0 or %INDEX(&where.,%str(&trt. in (3,4)))>0
	%THEN %LET show3=Y; %ELSE %LET show3=N;

		%LET show4=N;
%END;

**In case of four arms**;
%IF &totarm.=4 %THEN %DO;
	%IF %INDEX(&where.,%str(&trt.=4))>0 or %INDEX(&where.,%str(&trt. ne 1))>0 or %INDEX(&where.,%str(&trt. ^= 1))>0 or
	    %INDEX(&where.,%str(&trt. ne 2))>0 or %INDEX(&where.,%str(&trt. ^= 2))>0 or
	    %INDEX(&where.,%str(&trt. ne 3))>0 or %INDEX(&where.,%str(&trt. ^= 3))>0 or 
	    %INDEX(&where.,%str(&trt. not in (2,3)))>0 or %INDEX(&where.,%str(&trt. not in (1,2)))>0 or
	    %INDEX(&where.,%str(&trt. not in (1,3)))>0 or %INDEX(&where.,%str(&trt. not in (1,2,3)))>0 or
		%INDEX(&where.,%str(&trt. in (1,4)))>0 or %INDEX(&where.,%str(&trt. in (2,4)))>0 or
	    %INDEX(&where.,%str(&trt. in (3,4)))>0 or %INDEX(&where.,%str(&trt. in (2,3,4)))>0 or 
		%INDEX(&where.,%str(&trt. in (1,3,4)))>0 or %INDEX(&where.,%str(&trt. in (1,2,4)))>0  
	%THEN %LET show4=Y; %ELSE %LET show4=N;
%END;

/*************************************************************************************************************
/*************************************************************************************************************/

**Due to the PDF settings in the code for graphs (see below), the table is set in portrait when PDF is seclected 
  for DISPLAY. ==> Change this setting to make sure that the table is in landscape orientation.**;
%if %UPCASE(&Display.)=PDF %THEN %DO;
	option orientation=landscape;
%END;

%IF %UPCASE(&ODS.)=Y %THEN %DO;

	%IF (%length(&ODSFile)>0) %THEN %DO;
		options ls=180 ps=150 orientation=landscape nodate nonumber papersize=A4;
		ods rtf file= "&ODSFile.";
	%END;

ODS escapechar='^';

%let gradewidth=1.4;
%let socwidth=7.0;
%let cellheight=0.5;
%let fontsize=9;

%if &totarm.=4 %then %let gradewidth=1.3;
%if %UPCASE(&column.)=SUMMARY %then %let gradewidth=2.5; 
%if &anyGR5.=Y and %UPCASE(&column.)=ALL %then %let socwidth=4.5;
%if &anyGR5.=Y and %UPCASE(&column.)=ALL %then %let fontsize=8;
%if &anyGR5.=Y and %UPCASE(&column.)=ALL %then %let gradewidth=1.2;
%if &totarm.=2 and %UPCASE(&column.)=SUMMARY %then %let socwidth=12.0;
%if &totarm.=4 and %UPCASE(&column.)^=SUMMARY %then %let fontsize=8;
%if &totarm.>3 and %UPCASE(&column.)=SUMMARY %then %let gradewidth=2.0;

 
	Title "All adverse events, by treatment." height=14pt bold italic;
	%IF (%LENGTH(&Treshold.))>0 %THEN %DO ; Title2 "Adverse events representing more than &Treshold. % incidence in at least one arm"; %END;
	%IF (&related.=Y and &SAE.=N and &recovered.=N) %THEN %DO ; Title "Related Adverse events, by treatment.";%END;
	%IF (&related.=N and &SAE.=Y and &recovered.=N) %THEN %DO ; Title "Serious Adverse events, by treatment.";%END;
	%IF (&related.=N and &SAE.=N and &recovered.=Y) %THEN %DO ; Title "All Adverse events, by treatment.";
																Title2 "AE's recovered or recovered with sequelae";%END;
	%IF (&related.=Y and &SAE.=Y and &recovered.=N) %THEN %DO ; Title "Serious Adverse events related to treatment, by treatment.";%END;
	%IF (&related.=Y and &SAE.=N and &recovered.=Y) %THEN %DO ; Title "Related Adverse events, by treatment.";
																Title2 "AE's recovered or recovered with sequelae";%END;
	%IF (&related.=N and &SAE.=Y and &recovered.=Y) %THEN %DO ; Title "Serious Adverse events, by treatment.";
																Title2 "AE's recovered or recovered with sequelae";%END;
	%IF (&related.=Y and &SAE.=Y and &recovered.=Y) %THEN %DO ; Title "Serious Adverse events related to treatment, by treatment.";
																Title2 "AE's recovered or recovered with sequelae";%END;
	%IF (%length(&where.)>1) %THEN %DO; Title3 "Where &where.";%END;

proc report nowd data=__Table_final 
style(report)=[rules=none frame=void 
			   borderrightcolor=black borderrightwidth=0.5 
			   borderleftcolor=black borderleftwidth=0.5 
			   bordertopcolor=black bordertopwidth=0.5  
			   borderbottomcolor=black borderbottomwidth=0.5 ] split="*" center
style (header)=[ borderrightcolor=black borderrightwidth=0.5 
				 %if &colour.=Y %then bordertopcolor=cxE0E0E0 background=cxE0E0E0;
				%if &colour.=N %then bordertopcolor=black background=white borderbottomcolor=white borderbottomwidth=0.5 ;
				bordertopwidth=0.5 font_face=calibri fontsize=10pt] 
style(column)=[cellheight=&cellheight. cm];

/*Determine the column to be displayed*/ 
%IF %INDEX(&where.,%str(&trt.))>0 %THEN %DO;
COLUMN Concatenation_
%IF &show1.=Y %THEN %DO; 
	 ("&trt1*(Safety pop:N=&Ntot1)" %if %UPCASE(&column.)=ALL %then Npct11-Npct15 Npct1345 Npct1ALL) ;
														 %if %UPCASE(&column.)=POOLED %then Npct112 Npct134 Npct15 Npct1345 Npct1ALL) ;
														 %if %UPCASE(&column.)=SEVERE %then Npct13-Npct15 Npct1345) ;
														 %if %UPCASE(&column.)=SUMMARY %then Npct1345 Npct1ALL) ;
		%if &show2.=N and &show3.=N and &show4.=N %then %do;;%end;
%END;
%IF &show2.=Y %THEN %DO; 
  	("&trt2*(Safety pop:N=&Ntot2)" %if %UPCASE(&column.)=ALL %then Npct21-Npct25 Npct2345 Npct2ALL);
														 %if %UPCASE(&column.)=POOLED %then Npct212 Npct234	Npct25 Npct2345 Npct2ALL);
														 %if %UPCASE(&column.)=SEVERE %then Npct23-Npct25 Npct2345);
														 %if %UPCASE(&column.)=SUMMARY %then Npct2345 Npct2ALL) ;
		%if &show3.=N and &show4.=N %then %do;;%end;

%END;
%IF &show3.=Y %THEN %DO; 
	("&trt3*(Safety pop:N=&Ntot3)" %if %UPCASE(&column.)=ALL %then Npct31-Npct35 Npct3345 Npct3ALL) ;
														 %if %UPCASE(&column.)=POOLED %then Npct312 Npct334	Npct35 Npct3345 Npct3ALL) ;
														 %if %UPCASE(&column.)=SEVERE %then Npct33-Npct35 Npct3345) ;
														 %if %UPCASE(&column.)=SUMMARY %then Npct3345 Npct3ALL) ;
		%if &show4.=N %then %do;;%end;
%END;
%IF &show4.=Y %THEN %DO; 
  	("&trt4*(Safety pop:N=&Ntot4)" %if %UPCASE(&column.)=ALL %then Npct41-Npct45 Npct4345 Npct4ALL);
														 %if %UPCASE(&column.)=POOLED %then Npct412 Npct434	Npct45 Npct4345 Npct4ALL);
														 %if %UPCASE(&column.)=SEVERE %then Npct43-Npct45 Npct4345);
														 %if %UPCASE(&column.)=SUMMARY %then Npct4345 Npct4ALL) ;
														;
%END;
%END;
%IF %INDEX(&where.,%str(&trt.))=0 %THEN %DO;
	%if &totarm.=1 %then %do;													 
	COLUMN Concatenation_ ("&trt1*(Safety pop:N=&Ntot1)" %if %UPCASE(&column.)=ALL %then Npct11-Npct15 Npct1345 Npct1ALL) ;
														 %if %UPCASE(&column.)=POOLED %then Npct112 Npct134 Npct15 Npct1345 Npct1ALL) ;
														 %if %UPCASE(&column.)=SEVERE %then Npct13-Npct15 Npct1345) ;
														 %if %UPCASE(&column.)=SUMMARY %then Npct1345 Npct1ALL) ;
 														 ;
	%end;
 
	%if &totarm.=2 %then %do;													 
	COLUMN Concatenation_ ("&trt1*(Safety pop:N=&Ntot1)" %if %UPCASE(&column.)=ALL %then Npct11-Npct15 Npct1345 Npct1ALL) ;
														 %if %UPCASE(&column.)=POOLED %then Npct112 Npct134 Npct15 Npct1345 Npct1ALL) ;
														 %if %UPCASE(&column.)=SEVERE %then Npct13-Npct15 Npct1345) ;
														 %if %UPCASE(&column.)=SUMMARY %then Npct1345 Npct1ALL) ;
 
             			  ("&trt2*(Safety pop:N=&Ntot2)" %if %UPCASE(&column.)=ALL %then Npct21-Npct25 Npct2345 Npct2ALL);
														 %if %UPCASE(&column.)=POOLED %then Npct212 Npct234 Npct25 Npct2345 Npct2ALL);
														 %if %UPCASE(&column.)=SEVERE %then Npct23-Npct25 Npct2345);
														 %if %UPCASE(&column.)=SUMMARY %then Npct2345 Npct2ALL) ;
														 ;
	%end;
	%if &totarm.=3 %then %do;													 
	COLUMN Concatenation_ ("&trt1*(Safety pop:N=&Ntot1)" %if %UPCASE(&column.)=ALL %then Npct11-Npct15 Npct1345 Npct1ALL) ;
														 %if %UPCASE(&column.)=POOLED %then Npct112 Npct134 Npct15 Npct1345 Npct1ALL) ;
														 %if %UPCASE(&column.)=SEVERE %then Npct13-Npct15 Npct1345) ;
														 %if %UPCASE(&column.)=SUMMARY %then Npct1345 Npct1ALL) ;
																							 
             			  ("&trt2*(Safety pop:N=&Ntot2)" %if %UPCASE(&column.)=ALL %then Npct21-Npct25 Npct2345 Npct2ALL);
														 %if %UPCASE(&column.)=POOLED %then Npct212 Npct234 Npct25 Npct2345 Npct2ALL);
														 %if %UPCASE(&column.)=SEVERE %then Npct23-Npct25 Npct2345);
														 %if %UPCASE(&column.)=SUMMARY %then Npct2345 Npct2ALL) ;

						   ("&trt3*(Safety pop:N=&Ntot3)" %if %UPCASE(&column.)=ALL %then Npct31-Npct35 Npct3345 Npct3ALL);
														 %if %UPCASE(&column.)=POOLED %then Npct312 Npct334 Npct35 Npct3345 Npct3ALL);
														 %if %UPCASE(&column.)=SEVERE %then Npct33-Npct35 Npct3345);
														 %if %UPCASE(&column.)=SUMMARY %then Npct3345 Npct3ALL) ;
														 ;
	%end;
	%if &totarm.=4 %then %do;	
	COLUMN Concatenation_ ("&trt1*(Safety pop:N=&Ntot1)" %if %UPCASE(&column.)=ALL %then Npct11-Npct15 Npct1345 Npct1ALL) ;
														 %if %UPCASE(&column.)=POOLED %then Npct112 Npct134 Npct15 Npct1ALL) ;
														 %if %UPCASE(&column.)=SEVERE %then Npct13-Npct15 Npct1345) ;
														 %if %UPCASE(&column.)=SUMMARY %then Npct1345 Npct1ALL) ;
																							 
             			  ("&trt2*(Safety pop:N=&Ntot2)" %if %UPCASE(&column.)=ALL %then Npct21-Npct25 Npct2345 Npct2ALL);
														 %if %UPCASE(&column.)=POOLED %then Npct212 Npct234 Npct25 Npct2ALL);
														 %if %UPCASE(&column.)=SEVERE %then Npct23-Npct25 Npct2345);
														 %if %UPCASE(&column.)=SUMMARY %then Npct2345 Npct2ALL) ;

						  ("&trt3*(Safety pop:N=&Ntot3)" %if %UPCASE(&column.)=ALL %then Npct31-Npct35 Npct3345 Npct3ALL);
						   							     %if %UPCASE(&column.)=POOLED %then Npct312 Npct334 Npct35 Npct3ALL);
														 %if %UPCASE(&column.)=SEVERE %then Npct33-Npct35 Npct3345);
														 %if %UPCASE(&column.)=SUMMARY %then Npct3345 Npct3ALL) ;
 
             			  ("&trt4*(Safety pop:N=&Ntot4)" %if %UPCASE(&column.)=ALL %then Npct41-Npct45 Npct4345 Npct4ALL);
														 %if %UPCASE(&column.)=POOLED %then Npct412 Npct434 Npct45 Npct4ALL);
														 %if %UPCASE(&column.)=SEVERE %then Npct43-Npct45 Npct4345);
														 %if %UPCASE(&column.)=SUMMARY %then Npct4345 Npct4ALL) ;
													 ;
	%end;
%END;

/*Define variables*/
define Concatenation_ / id 'CTC + MedDRA Term'  
								  style(column)={font_face=calibri fontsize=9pt cellspacing=0.2 asis=on borderrightcolor=black borderrightwidth=0.5 
									 			borderbottomstyle=dotted borderbottomcolor=black borderbottomwidth=0.5 cellwidth=&socwidth.cm}
									style (header)={font_face=calibri fontsize=10pt borderrightcolor=black borderrightwidth=0.5 
													%if &colour.=N %then borderbottomcolor=black borderbottomwidth=0.5;} width=5;

%IF &totarm.=1 %THEN %DO;

%IF %UPCASE(&column.)=ALL %THEN %DO;
define Npct11 / "Grade     1     N (%)" style (column)={just=center borderbottomstyle=dotted borderbottomcolor=black borderbottomwidth=0.5 cellwidth=&gradewidth.cm font_face=calibri fontsize=&fontsize.pt}
						  				style (header)={%if &colour.=Y %then borderrightcolor=cxE0E0E0;
														%if &colour.=N %then borderrightcolor=white borderbottomcolor=black borderbottomwidth=0.5; borderrightwidth=0.5};
define Npct12 / "Grade     2     N (%)" style (column)={just=center borderbottomstyle=dotted borderbottomcolor=black borderbottomwidth=0.5 cellwidth=&gradewidth.cm font_face=calibri fontsize=&fontsize.pt}
						  				style (header)={%if &colour.=Y %then borderrightcolor=cxE0E0E0;
														%if &colour.=N %then borderrightcolor=white borderbottomcolor=black borderbottomwidth=0.5;
														borderrightwidth=0.5};
define Npct13 / "Grade    3     N (%)" style (column)={just=center borderbottomstyle=dotted borderbottomcolor=black borderbottomwidth=0.5 cellwidth=&gradewidth.cm font_face=calibri fontsize=&fontsize.pt}
						  			   style (header)={%if &colour.=Y %then borderrightcolor=cxE0E0E0;
														%if &colour.=N %then borderrightcolor=white borderbottomcolor=black borderbottomwidth=0.5;
														borderrightwidth=0.5};
define Npct14 / "Grade     4     N (%)" style (column)={just=center borderbottomstyle=dotted borderbottomcolor=black borderbottomwidth=0.5 cellwidth=&gradewidth.cm font_face=calibri fontsize=&fontsize.pt}
						  				style (header)={%if &colour.=Y %then borderrightcolor=cxE0E0E0;
														%if &colour.=N %then borderrightcolor=white borderbottomcolor=black borderbottomwidth=0.5;
														borderrightwidth=0.5};
%if &anyGR5.=Y %then %do; 
define Npct15 / "Grade     5     N (%)" style (column)={just=center borderbottomstyle=dotted borderbottomcolor=black borderbottomwidth=0.5 cellwidth=&gradewidth.cm font_face=calibri fontsize=&fontsize.pt}
  									    style (header)={%if &colour.=Y %then borderrightcolor=cxE0E0E0;
										 				%if &colour.=N %then borderrightcolor=white borderbottomcolor=black borderbottomwidth=0.5;
																			 borderrightwidth=0.5};
%end;
%if &anyGR5.=N %then define Npct15 /noprint;;

define Npct1345 / "Grade ^{unicode '2265'x}3     N (%)" style(column)={just=center cellwidth=&gradewidth.cm 
														borderbottomstyle=dotted borderbottomcolor=black borderbottomwidth=0.5
														%if &colour.=Y %then borderrightcolor=whitesmoke background=whitesmoke;
														%if &colour.=N %then borderrightcolor=white background=white;
														borderrightwidth=0.5 cellwidth=&gradewidth.cm font_face=calibri fontsize=&fontsize.pt}
							 							style (header)={%if &colour.=Y %then borderrightcolor=cxE0E0E0; 
																		%if &colour.=N %then borderrightcolor=white borderbottomcolor=black borderbottomwidth=0.5;
																		borderrightwidth=0.5};
define Npct1ALL / "Grade ^{unicode '2265'x}1     N (%)"  style(column)={just=center cellwidth=&gradewidth.cm 
																		%if &colour.=Y %then background=whitesmoke;
																		%if &colour.=N %then background=white;	 
											 							borderrightcolor=black borderrightwidth=0.5 font_face=calibri fontsize=&fontsize.pt
											 							borderbottomstyle=dotted borderbottomcolor=black borderbottomwidth=0.5}
							  							style (header)={just=center borderrightcolor=black borderrightwidth=0.5 cellwidth=&gradewidth.cm
																		%if &colour.=N %then borderbottomcolor=black borderbottomwidth=0.5;};
%END;

%IF %UPCASE(&column.)=SEVERE %THEN %DO;
define Npct13 / "Grade   3        N (%)" style (column)={just=center borderbottomstyle=dotted borderbottomcolor=black borderbottomwidth=0.5 cellwidth=&gradewidth.cm font_face=calibri fontsize=&fontsize.pt}
						  				 style (header)={%if &colour.=Y %then borderrightcolor=cxE0E0E0;
										 				 %if &colour.=N %then borderrightcolor=white borderbottomcolor=black borderbottomwidth=0.5;
										 				 borderrightwidth=0.5};
define Npct14 / "Grade   4        N (%)" style (column)={just=center borderbottomstyle=dotted borderbottomcolor=black borderbottomwidth=0.5 cellwidth=&gradewidth.cm font_face=calibri fontsize=&fontsize.pt}
						  				 style (header)={%if &colour.=Y %then borderrightcolor=cxE0E0E0;
										 				 %if &colour.=N %then borderrightcolor=white borderbottomcolor=black borderbottomwidth=0.5;
										 				 borderrightwidth=0.5};
%if &anyGR5.=Y %then %do;
define Npct15 / "Grade   5        N (%)" style (column)={just=center borderbottomstyle=dotted borderbottomcolor=black borderbottomwidth=0.5 cellwidth=&gradewidth.cm font_face=calibri fontsize=&fontsize.pt}
										  style (header)={%if &colour.=Y %then borderrightcolor=cxE0E0E0;
										  				  %if &colour.=N %then borderrightcolor=white borderbottomcolor=black borderbottomwidth=0.5; 
										  				  borderrightwidth=0.5};
%end;
%if &anyGR5.=N %then define Npct15 /noprint;;

define Npct1345 / "Grade ^{unicode '2265'x}3     N (%)" style(column)={just=center cellwidth=&gradewidth.cm %if &colour.=Y %then background=whitesmoke;
																		%if &colour.=N %then background=white;	
													   					borderbottomstyle=dotted borderbottomcolor=black borderbottomwidth=0.5
													   					borderrightcolor=black borderrightwidth=0.5 cellwidth=&gradewidth.cm 
													   					font_face=calibri fontsize=&fontsize.pt}
													   style (header)={borderrightcolor=black borderrightwidth=0.5 
																	   %if &colour.=N %then borderbottomcolor=black borderbottomwidth=0.5;};
%END;

%IF %UPCASE(&column.)=POOLED %THEN %DO;
define Npct112 / "Grade 1/2   N (%)" style (column)={just=center borderbottomstyle=dotted borderbottomcolor=black borderbottomwidth=0.5 cellwidth=&gradewidth.cm font_face=calibri fontsize=&fontsize.pt}
						  			 style (header)={%if &colour.=Y %then borderrightcolor=cxE0E0E0;
													 %if &colour.=N %then borderrightcolor=white borderbottomcolor=black borderbottomwidth=0.5;
												     borderrightwidth=0.5};
define Npct134 / "Grade 3/4   N (%)" style (column)={just=center borderbottomstyle=dotted borderbottomcolor=black borderbottomwidth=0.5 cellwidth=&gradewidth.cm font_face=calibri fontsize=&fontsize.pt}
						  			 style (header)={%if &colour.=Y %then borderrightcolor=cxE0E0E0;
													 %if &colour.=N %then borderrightcolor=white borderbottomcolor=black borderbottomwidth=0.5;
													borderrightwidth=0.5};
%if &anyGR5.=Y %then %do;
define Npct15 / "Grade   5        N (%)" style (column)={just=center borderbottomstyle=dotted borderbottomcolor=black borderbottomwidth=0.5 cellwidth=&gradewidth.cm font_face=calibri fontsize=&fontsize.pt}
 									     style (header)={%if &colour.=Y %then borderrightcolor=cxE0E0E0;
														 %if &colour.=N %then borderrightcolor=white borderbottomcolor=black borderbottomwidth=0.5;
														  borderrightwidth=0.5};
%end;
%if &anyGR5.=N %then define Npct15 /noprint;;

define Npct1345 / "Grade ^{unicode '2265'x}3     N (%)" style(column)={just=center cellwidth=&gradewidth.cm 
																		%if &colour.=Y %then background=whitesmoke borderrightcolor=whitesmoke;
																		%if &colour.=N %then background=white borderrightcolor=white;
													    				borderbottomstyle=dotted borderbottomcolor=black borderbottomwidth=0.5
													    				borderrightwidth=0.5 cellwidth=&gradewidth.cm font_face=calibri fontsize=&fontsize.pt}
							 							style (header)={%if &colour.=Y %then borderrightcolor=cxE0E0E0;
																		%if &colour.=N %then borderrightcolor=white borderbottomcolor=black borderbottomwidth=0.5;
														borderrightwidth=0.5};
define Npct1ALL / "Grade ^{unicode '2265'x}1     N (%)"  style(column)={just=center cellwidth=&gradewidth.cm 
																		%if &colour.=Y %then background=whitesmoke;
 																		%if &colour.=N %then background=white;
																		borderrightcolor=black borderrightwidth=0.5 font_face=calibri fontsize=&fontsize.pt
											 							borderbottomstyle=dotted borderbottomcolor=black borderbottomwidth=0.5}
							 							style (header)={just=center borderrightcolor=black borderrightwidth=0.5 cellwidth=&gradewidth.cm
																		%if &colour.=N %then borderbottomcolor=black borderbottomwidth=0.5;};
%END;

%IF %UPCASE(&column.)=SUMMARY %THEN %DO;
define Npct1345 / "Grade ^{unicode '2265'x}3     N (%)" style(column)={just=center cellwidth=&gradewidth.cm 
																		borderbottomstyle=dotted borderbottomcolor=black borderbottomwidth=0.5
																		borderrightcolor=white borderrightwidth=0.5 cellwidth=&gradewidth.cm
																		font_face=calibri fontsize=&fontsize.pt}
							 							style (header)={%if &colour.=Y %then borderrightcolor=cxE0E0E0;
										     							%if &colour.=N %then borderrightcolor=white borderbottomcolor=black borderbottomwidth=0.5;
																		borderrightwidth=0.5};
define Npct1ALL / "Grade ^{unicode '2265'x}1     N (%)"  style(column)={just=center cellwidth=&gradewidth.cm  
																		borderrightcolor=black borderrightwidth=0.5 font_face=calibri fontsize=&fontsize.pt
											 							borderbottomstyle=dotted borderbottomcolor=black borderbottomwidth=0.5}
							  							style (header)={just=center borderrightcolor=black borderrightwidth=0.5 cellwidth=&gradewidth.cm
																		%if &colour.=N %then borderbottomcolor=black borderbottomwidth=0.5;};
%END;
%END;

/*If more than 1 arm*/
%IF &totarm.>1 %THEN %DO;

/*ARM1*/
%IF %INDEX(&where.,%str(&trt.))=0 or &show1.=Y %THEN %DO;

%IF %UPCASE(&column.)=ALL %THEN %DO;
define Npct11 / "Grade     1     N (%)" style (column)={just=center borderbottomstyle=dotted borderbottomcolor=black borderbottomwidth=0.5 cellwidth=&gradewidth.cm font_face=calibri fontsize=&fontsize.pt}
						  				style (header)={%if &colour.=Y %then borderrightcolor=cxE0E0E0;
														%if &colour.=N %then borderrightcolor=white borderbottomcolor=black borderbottomwidth=0.5; borderrightwidth=0.5 };
define Npct12 / "Grade     2     N (%)" style (column)={just=center borderbottomstyle=dotted borderbottomcolor=black borderbottomwidth=0.5 cellwidth=&gradewidth.cm font_face=calibri fontsize=&fontsize.pt}
						  				style (header)={%if &colour.=Y %then borderrightcolor=cxE0E0E0;
														%if &colour.=N %then borderrightcolor=white borderbottomcolor=black borderbottomwidth=0.5 ;
														borderrightwidth=0.5};
define Npct13 / "Grade    3     N (%)" style (column)={just=center borderbottomstyle=dotted borderbottomcolor=black borderbottomwidth=0.5 cellwidth=&gradewidth.cm font_face=calibri fontsize=&fontsize.pt}
						  			   style (header)={%if &colour.=Y %then borderrightcolor=cxE0E0E0;
														%if &colour.=N %then borderrightcolor=white borderbottomcolor=black borderbottomwidth=0.5;
														borderrightwidth=0.5};
define Npct14 / "Grade     4     N (%)" style (column)={just=center borderbottomstyle=dotted borderbottomcolor=black borderbottomwidth=0.5 cellwidth=&gradewidth.cm font_face=calibri fontsize=&fontsize.pt}
						  				style (header)={%if &colour.=Y %then borderrightcolor=cxE0E0E0;
														%if &colour.=N %then borderrightcolor=white borderbottomcolor=black borderbottomwidth=0.5;
														borderrightwidth=0.5};
%if &anyGR5.=Y %then %do;
define Npct15 / "Grade     5     N (%)" style (column)={just=center borderbottomstyle=dotted borderbottomcolor=black borderbottomwidth=0.5 cellwidth=&gradewidth.cm font_face=calibri fontsize=&fontsize.pt}
										 style (header)={%if &colour.=Y %then borderrightcolor=cxE0E0E0;
										 				 %if &colour.=N %then borderrightcolor=white borderbottomcolor=black borderbottomwidth=0.5;
										 				 borderrightwidth=0.5};
%end;
%if &anyGR5.=N %then define Npct15 /noprint;;

define Npct1345 / "Grade ^{unicode '2265'x}3     N (%)" style(column)={just=center cellwidth=&gradewidth.cm 
																		borderbottomstyle=dotted borderbottomcolor=black borderbottomwidth=0.5
																		%if &colour.=Y %then borderrightcolor=whitesmoke background=whitesmoke;
																		%if &colour.=N %then borderrightcolor=white background=white;
																		borderrightwidth=0.5 cellwidth=&gradewidth.cm font_face=calibri fontsize=&fontsize.pt}
							 							style (header)={%if &colour.=Y %then borderrightcolor=cxE0E0E0; 
																		%if &colour.=N %then borderrightcolor=white borderbottomcolor=black borderbottomwidth=0.5;
																		borderrightwidth=0.5};
define Npct1ALL / "Grade ^{unicode '2265'x}1     N (%)"  style(column)={just=center cellwidth=&gradewidth.cm 
																		%if &colour.=Y %then background=whitesmoke ;
																		%if &colour.=N %then background=white ;
											 							borderrightcolor=black borderrightwidth=0.5 font_face=calibri fontsize=&fontsize.pt
											 							borderbottomstyle=dotted borderbottomcolor=black borderbottomwidth=0.5}
							  							 style (header)={just=center borderrightcolor=black borderrightwidth=0.5 cellwidth=&gradewidth.cm
																		%if &colour.=N %then borderbottomcolor=black borderbottomwidth=0.5;};
%END;

%IF %UPCASE(&column.)=SEVERE %THEN %DO;
define Npct13 / "Grade   3        N (%)" style (column)={just=center borderbottomstyle=dotted borderbottomcolor=black borderbottomwidth=0.5 cellwidth=&gradewidth.cm font_face=calibri fontsize=&fontsize.pt}
						  				 style (header)={%if &colour.=Y %then borderrightcolor=cxE0E0E0;
										 				 %if &colour.=N %then borderrightcolor=white borderbottomcolor=black borderbottomwidth=0.5;
														 borderrightwidth=0.5};
define Npct14 / "Grade   4        N (%)" style (column)={just=center borderbottomstyle=dotted borderbottomcolor=black borderbottomwidth=0.5 cellwidth=&gradewidth.cm font_face=calibri fontsize=&fontsize.pt}
						  				 style (header)={%if &colour.=Y %then borderrightcolor=cxE0E0E0;
										 				 %if &colour.=N %then borderrightcolor=white borderbottomcolor=black borderbottomwidth=0.5;
										 				 borderrightwidth=0.5};
%if &anyGR5.=Y %then %do;
define Npct15 / "Grade   5        N (%)" style (column)={just=center borderbottomstyle=dotted borderbottomcolor=black borderbottomwidth=0.5 cellwidth=&gradewidth.cm font_face=calibri fontsize=&fontsize.pt}
										  style (header)={%if &colour.=Y %then borderrightcolor=cxE0E0E0;
										  				  %if &colour.=N %then borderrightcolor=white borderbottomcolor=black borderbottomwidth=0.5; 
										  				  borderrightwidth=0.5};
%end;
%if &anyGR5.=N %then define Npct15 /noprint;;

define Npct1345 / "Grade ^{unicode '2265'x}3     N (%)" style(column)={just=center cellwidth=&gradewidth.cm %if &colour.=Y %then background=whitesmoke;
																		%if &colour.=N %then background=white;	
													   					borderbottomstyle=dotted borderbottomcolor=black borderbottomwidth=0.5
													   					borderrightcolor=black borderrightwidth=0.5 cellwidth=&gradewidth.cm 
													   					font_face=calibri fontsize=&fontsize.pt}
													   style (header)={borderrightcolor=black borderrightwidth=0.5
																		%if &colour.=N %then borderbottomcolor=black borderbottomwidth=0.5;};
%END;

%IF %UPCASE(&column.)=POOLED %THEN %DO;
define Npct112 / "Grade 1/2   N (%)" style (column)={just=center borderbottomstyle=dotted borderbottomcolor=black borderbottomwidth=0.5 cellwidth=&gradewidth.cm font_face=calibri fontsize=&fontsize.pt}
						  			 style (header)={%if &colour.=Y %then borderrightcolor=cxE0E0E0;
													 %if &colour.=N %then borderrightcolor=white borderbottomcolor=black borderbottomwidth=0.5;
												     borderrightwidth=0.5};
define Npct134 / "Grade 3/4   N (%)" style (column)={just=center borderbottomstyle=dotted borderbottomcolor=black borderbottomwidth=0.5 cellwidth=&gradewidth.cm font_face=calibri fontsize=&fontsize.pt}
						  			 style (header)={%if &colour.=Y %then borderrightcolor=cxE0E0E0;
													 %if &colour.=N %then borderrightcolor=white borderbottomcolor=black borderbottomwidth=0.5;
													borderrightwidth=0.5};
%if &anyGR5.=Y %then %do;
define Npct15 / "Grade   5        N (%)" style (column)={just=center borderbottomstyle=dotted borderbottomcolor=black borderbottomwidth=0.5 cellwidth=&gradewidth.cm font_face=calibri fontsize=&fontsize.pt}
									      style (header)={%if &colour.=Y %then borderrightcolor=cxE0E0E0;
														  %if &colour.=N %then borderrightcolor=white borderbottomcolor=black borderbottomwidth=0.5;
														  borderrightwidth=0.5};
%end;
%if &anyGR5.=N %then define Npct15 /noprint;;

define Npct1345 / "Grade ^{unicode '2265'x}3     N (%)" style(column)={just=center cellwidth=&gradewidth.cm 
																		%if &colour.=Y %then background=whitesmoke borderrightcolor=whitesmoke;
																		%if &colour.=N %then background=white borderrightcolor=white;
																	    borderbottomstyle=dotted borderbottomcolor=black borderbottomwidth=0.5
																	    borderrightwidth=0.5 cellwidth=&gradewidth.cm font_face=calibri fontsize=&fontsize.pt}
							 							style (header)={%if &colour.=Y %then borderrightcolor=cxE0E0E0;
																		%if &colour.=N %then borderrightcolor=white borderbottomcolor=black borderbottomwidth=0.5;
																		borderrightwidth=0.5};
define Npct1ALL / "Grade ^{unicode '2265'x}1     N (%)"  style(column)={just=center cellwidth=&gradewidth.cm 
																		%if &colour.=Y %then background=whitesmoke;
				 														%if &colour.=N %then background=white;
																		borderrightcolor=black borderrightwidth=0.5 font_face=calibri fontsize=&fontsize.pt
															 			borderbottomstyle=dotted borderbottomcolor=black borderbottomwidth=0.5}
							 							style (header)={just=center borderrightcolor=black borderrightwidth=0.5 cellwidth=&gradewidth.cm
																		%if &colour.=N %then borderbottomcolor=black borderbottomwidth=0.5;};
%END;

%IF %UPCASE(&column.)=SUMMARY %THEN %DO;
define Npct1345 / "Grade ^{unicode '2265'x}3     N (%)" style(column)={just=center cellwidth=&gradewidth.cm 
																		borderbottomstyle=dotted borderbottomcolor=black borderbottomwidth=0.5
																		borderrightcolor=white borderrightwidth=0.5 cellwidth=&gradewidth.cm
																		font_face=calibri fontsize=&fontsize.pt}
										 				style (header)={%if &colour.=Y %then borderrightcolor=cxE0E0E0;
																	    %if &colour.=N %then borderrightcolor=white borderbottomcolor=black borderbottomwidth=0.5;
																		borderrightwidth=0.5};
define Npct1ALL / "Grade ^{unicode '2265'x}1     N (%)"  style(column)={just=center cellwidth=&gradewidth.cm  
																		 borderrightcolor=black borderrightwidth=0.5 font_face=calibri fontsize=&fontsize.pt
																		 borderbottomstyle=dotted borderbottomcolor=black borderbottomwidth=0.5}
														 style (header)={%if &colour.=N %then borderbottomcolor=black borderbottomwidth=0.5;
																		 just=center borderrightcolor=black borderrightwidth=0.5 cellwidth=&gradewidth.cm};
%END;
%END;

/*ARM2*/
%IF %INDEX(&where.,%str(&trt.))=0 or &show2.=Y %THEN %DO;

%IF %UPCASE(&column.)=ALL %THEN %DO;
define Npct21 / "Grade     1     N (%)" style (column)={just=center borderbottomstyle=dotted borderbottomcolor=black borderbottomwidth=0.5 cellwidth=&gradewidth.cm font_face=calibri fontsize=&fontsize.pt}
						  			 	style (header)={%if &colour.=Y %then borderrightcolor=cxE0E0E0;
														%if &colour.=N %then borderrightcolor=white borderbottomcolor=black borderbottomwidth=0.5; borderrightwidth=0.5 };
define Npct22 / "Grade     2     N (%)" style (column)={just=center borderbottomstyle=dotted borderbottomcolor=black borderbottomwidth=0.5 cellwidth=&gradewidth.cm font_face=calibri fontsize=&fontsize.pt}
						  				style (header)={%if &colour.=Y %then borderrightcolor=cxE0E0E0;
														%if &colour.=N %then borderrightcolor=white borderbottomcolor=black borderbottomwidth=0.5; borderrightwidth=0.5 };
define Npct23 / "Grade     3     N (%)" style (column)={just=center borderbottomstyle=dotted borderbottomcolor=black borderbottomwidth=0.5 cellwidth=&gradewidth.cm font_face=calibri fontsize=&fontsize.pt}
						  				style (header)={%if &colour.=Y %then borderrightcolor=cxE0E0E0;
														%if &colour.=N %then borderrightcolor=white borderbottomcolor=black borderbottomwidth=0.5; borderrightwidth=0.5 };
define Npct24 / "Grade     4     N (%)" style (column)={just=center borderbottomstyle=dotted borderbottomcolor=black borderbottomwidth=0.5 cellwidth=&gradewidth.cm font_face=calibri fontsize=&fontsize.pt}
						  				style (header)={%if &colour.=Y %then borderrightcolor=cxE0E0E0;
														%if &colour.=N %then borderrightcolor=white borderbottomcolor=black borderbottomwidth=0.5; borderrightwidth=0.5 };
%if &anyGR5.=Y %then  %do;
define Npct25 / "Grade     5     N (%)" style (column)={just=center borderbottomstyle=dotted borderbottomcolor=black borderbottomwidth=0.5 cellwidth=&gradewidth.cm font_face=calibri fontsize=&fontsize.pt}
 									    style (header)={%if &colour.=Y %then borderrightcolor=cxE0E0E0;
													   %if &colour.=N %then borderrightcolor=white borderbottomcolor=black borderbottomwidth=0.5; 
														borderrightwidth=0.5};
%end;
%if &anyGR5.=N %then define Npct25 /noprint;;

define Npct2345 / "Grade ^{unicode '2265'x}3     N (%)" style(column)={just=center cellwidth=&gradewidth.cm 
																	    %if &colour.=Y %then background=whitesmoke borderrightcolor=whitesmoke;
																		%if &colour.=N %then background=white borderrightcolor=white;
																		borderbottomstyle=dotted borderbottomcolor=black borderbottomwidth=0.5
																		borderrightwidth=0.5 cellwidth=&gradewidth.cm
																		font_face=calibri fontsize=&fontsize.pt}
							 							style (header)={%if &colour.=Y %then borderrightcolor=cxE0E0E0;
																		%if &colour.=N %then borderrightcolor=white borderbottomcolor=black borderbottomwidth=0.5; borderrightwidth=0.5} ;
define Npct2ALL / "Grade ^{unicode '2265'x}1     N (%)" style(column)={just=center cellwidth=&gradewidth.cm 
																		%if &colour.=Y %then background=whitesmoke;
																		%if &colour.=N %then background=white;
																		font_face=calibri fontsize=&fontsize.pt
															 			borderrightcolor=black borderrightwidth=0.5 cellwidth=&gradewidth.cm
															 			borderbottomstyle=dotted borderbottomcolor=black borderbottomwidth=0.5}
														%if &colour.=N %then style (header)={borderbottomcolor=black borderbottomwidth=0.5};;
%END;

%IF %UPCASE(&column.)=SEVERE %THEN %DO;
define Npct23 / "Grade   3        N (%)" style (column)={just=center borderbottomstyle=dotted borderbottomcolor=black borderbottomwidth=0.5 cellwidth=&gradewidth.cm font_face=calibri fontsize=&fontsize.pt}
						  				 style (header)={%if &colour.=Y %then borderrightcolor=cxE0E0E0;
														 %if &colour.=N %then borderrightcolor=white borderbottomcolor=black borderbottomwidth=0.5; borderrightwidth=0.5 };
define Npct24 / "Grade   4        N (%)" style (column)={just=center borderbottomstyle=dotted borderbottomcolor=black borderbottomwidth=0.5 cellwidth=&gradewidth.cm font_face=calibri fontsize=&fontsize.pt}
						 				 style (header)={%if &colour.=Y %then borderrightcolor=cxE0E0E0;
														 %if &colour.=N %then borderrightcolor=white borderbottomcolor=black borderbottomwidth=0.5; borderrightwidth=0.5 };
%if &anyGR5.=Y %then  %do;
define Npct25 / "Grade     5     N (%)" style (column)={just=center borderbottomstyle=dotted borderbottomcolor=black borderbottomwidth=0.5 cellwidth=&gradewidth.cm font_face=calibri fontsize=&fontsize.pt}
 									    style (header)={%if &colour.=Y %then borderrightcolor=cxE0E0E0;
													   %if &colour.=N %then borderrightcolor=white borderbottomcolor=black borderbottomwidth=0.5; 
														borderrightwidth=0.5};
%end;
%if &anyGR5.=N %then define Npct25 /noprint;;

define Npct2345 / "Grade ^{unicode '2265'x}3     N (%)" style(column)={just=center cellwidth=&gradewidth.cm 
																	   %if &colour.=Y %then background=whitesmoke; 
																	   %if &colour.=N %then background=white;
																	   font_face=calibri fontsize=&fontsize.pt
											 						   borderrightcolor=black borderrightwidth=0.5
																	   borderbottomstyle=dotted borderbottomcolor=black borderbottomwidth=0.5}
														%if &colour.=N %then style (header)={borderbottomcolor=black borderbottomwidth=0.5};;
%END;

%IF %UPCASE(&column.)=POOLED %THEN %DO;
define Npct212 / "Grade 1/2   N (%)" style (column)={just=center borderbottomstyle=dotted borderbottomcolor=black borderbottomwidth=0.5 cellwidth=&gradewidth.cm font_face=calibri fontsize=&fontsize.pt}
						  			 style (header)={%if &colour.=Y %then borderrightcolor=cxE0E0E0;
													 %if &colour.=N %then borderrightcolor=white borderbottomcolor=black borderbottomwidth=0.5; borderrightwidth=0.5 };
define Npct234 / "Grade 3/4   N (%)" style (column)={just=center borderbottomstyle=dotted borderbottomcolor=black borderbottomwidth=0.5 cellwidth=&gradewidth.cm font_face=calibri fontsize=&fontsize.pt}
						  			 style (header)={%if &colour.=Y %then borderrightcolor=cxE0E0E0;
													 %if &colour.=N %then borderrightcolor=white borderbottomcolor=black borderbottomwidth=0.5; borderrightwidth=0.5 };
%if &anyGR5.=Y %then %do;
define Npct25 / "Grade   5        N (%)" style (column)={just=center borderbottomstyle=dotted borderbottomcolor=black borderbottomwidth=0.5 cellwidth=&gradewidth.cm font_face=calibri fontsize=&fontsize.pt}
									     style (header)={%if &colour.=Y %then borderrightcolor=cxE0E0E0;
													     %if &colour.=N %then borderrightcolor=white borderbottomcolor=black borderbottomwidth=0.5; 
														 borderrightwidth=0.5};
%end;
%if &anyGR5.=N %then define Npct25 /noprint;;

define Npct2345 / "Grade ^{unicode '2265'x}3     N (%)" style(column)={just=center cellwidth=&gradewidth.cm 
																	   %if &colour.=Y %then background=whitesmoke borderrightcolor=whitesmoke;
																	   %if &colour.=N %then background=white borderrightcolor=white;
																	   borderbottomstyle=dotted borderbottomcolor=black borderbottomwidth=0.5
																	   borderrightwidth=0.5 font_face=calibri fontsize=&fontsize.pt}
							 						   style (header)={%if &colour.=Y %then borderrightcolor=cxE0E0E0;
																	   %if &colour.=N %then borderrightcolor=white borderbottomcolor=black borderbottomwidth=0.5; borderrightwidth=0.5 };
define Npct2ALL / "Grade ^{unicode '2265'x}1     N (%)" style(column)={just=center cellwidth=&gradewidth.cm 
																	   %if &colour.=Y %then background=whitesmoke;
																	   %if &colour.=N %then background=white;
																	   font_face=calibri fontsize=&fontsize.pt
																	   borderrightcolor=black borderrightwidth=0.5 
																	   borderbottomstyle=dotted borderbottomcolor=black borderbottomwidth=0.5}
														%if &colour.=N %then style (header)={borderbottomcolor=black borderbottomwidth=0.5};;
%END;

%IF %UPCASE(&column.)=SUMMARY %THEN %DO;
define Npct2345 / "Grade ^{unicode '2265'x}3     N (%)" style(column)={just=center cellwidth=&gradewidth.cm 
																		borderbottomstyle=dotted borderbottomcolor=black borderbottomwidth=0.5
																		borderrightcolor=white borderrightwidth=0.5 cellwidth=&gradewidth.cm
																		font_face=calibri fontsize=&fontsize.pt}
							 							style (header)={%if &colour.=Y %then borderrightcolor=cxE0E0E0;
																		%if &colour.=N %then borderrightcolor=white borderbottomcolor=black borderbottomwidth=0.5; borderrightwidth=0.5 };
define Npct2ALL / "Grade ^{unicode '2265'x}1     N (%)"  style(column)={just=center cellwidth=&gradewidth.cm  
																		 borderrightcolor=black borderrightwidth=0.5 font_face=calibri fontsize=&fontsize.pt
																		 borderbottomstyle=dotted borderbottomcolor=black borderbottomwidth=0.5}
							  							 style (header)={just=center borderrightcolor=black borderrightwidth=0.5 cellwidth=&gradewidth.cm
																		%if &colour.=N %then borderbottomcolor=black borderbottomwidth=0.5;};
%END;
%END;
%END;

/*ARM3*/
%IF &totarm.=3 %THEN %DO;
%IF %INDEX(&where.,%str(&trt.))=0 or &show3.=Y %THEN %DO;

	%IF %UPCASE(&column.)=ALL %THEN %DO;
define Npct31 / "Grade     1     N (%)" style (column)={just=center borderbottomstyle=dotted borderbottomcolor=black borderbottomwidth=0.5 cellwidth=&gradewidth.cm font_face=calibri fontsize=&fontsize.pt}
						  				style (header)={%if &colour.=Y %then borderrightcolor=cxE0E0E0;
														%if &colour.=N %then borderrightcolor=white borderbottomcolor=black borderbottomwidth=0.5; borderrightwidth=0.5 };
define Npct32 / "Grade     2     N (%)" style (column)={just=center borderbottomstyle=dotted borderbottomcolor=black borderbottomwidth=0.5 cellwidth=&gradewidth.cm font_face=calibri fontsize=&fontsize.pt}
						  				style (header)={%if &colour.=Y %then borderrightcolor=cxE0E0E0;
														%if &colour.=N %then borderrightcolor=white borderbottomcolor=black borderbottomwidth=0.5; borderrightwidth=0.5 };
define Npct33 / "Grade     3     N (%)" style (column)={just=center borderbottomstyle=dotted borderbottomcolor=black borderbottomwidth=0.5 cellwidth=&gradewidth.cm font_face=calibri fontsize=&fontsize.pt}
						  				style (header)={%if &colour.=Y %then borderrightcolor=cxE0E0E0;
														%if &colour.=N %then borderrightcolor=white borderbottomcolor=black borderbottomwidth=0.5; borderrightwidth=0.5 };
define Npct34 / "Grade     4     N (%)" style (column)={just=center borderbottomstyle=dotted borderbottomcolor=black borderbottomwidth=0.5 cellwidth=&gradewidth.cm font_face=calibri fontsize=&fontsize.pt}
						  				style (header)={%if &colour.=Y %then borderrightcolor=cxE0E0E0;
														%if &colour.=N %then borderrightcolor=white borderbottomcolor=black borderbottomwidth=0.5; borderrightwidth=0.5 };
%if &anyGR5.=Y %then  %do;
	define Npct35 / "Grade     5     N (%)" style (column)={just=center borderbottomstyle=dotted borderbottomcolor=black borderbottomwidth=0.5 cellwidth=&gradewidth.cm font_face=calibri fontsize=&fontsize.pt}
											style (header)={%if &colour.=Y %then borderrightcolor=cxE0E0E0;
														    %if &colour.=N %then borderrightcolor=white borderbottomcolor=black borderbottomwidth=0.5; 
															borderrightwidth=0.5 };
%end;
%if &anyGR5.=N %then define Npct35 /noprint;;

define Npct3345 / "Grade ^{unicode '2265'x}3     N (%)" style(column)={just=center cellwidth=&gradewidth.cm 
																	   %if &colour.=Y %then background=whitesmoke borderrightcolor=whitesmoke;
																	   %if &colour.=N %then background=white borderrightcolor=white;
																		borderbottomstyle=dotted borderbottomcolor=black borderbottomwidth=0.5
																		borderrightwidth=0.5 cellwidth=&gradewidth.cm
																		font_face=calibri fontsize=&fontsize.pt}
							 							style (header)={%if &colour.=Y %then borderrightcolor=cxE0E0E0;
																		%if &colour.=N %then borderrightcolor=white borderbottomcolor=black borderbottomwidth=0.5; borderrightwidth=0.5 };
define Npct3ALL / "Grade ^{unicode '2265'x}1     N (%)" style(column)={just=center cellwidth=&gradewidth.cm 
																		%if &colour.=Y %then background=whitesmoke;
																		%if &colour.=N %then background=white; font_face=calibri fontsize=&fontsize.pt
																	   borderrightcolor=black borderrightwidth=0.5 cellwidth=&gradewidth.cm
																	   borderbottomstyle=dotted borderbottomcolor=black borderbottomwidth=0.5}
														%if &colour.=N %then style (header)={borderbottomcolor=black borderbottomwidth=0.5};;
	%END;

	%IF %UPCASE(&column.)=SEVERE %THEN %DO;
define Npct33 / "Grade   3        N (%)" style (column)={just=center borderbottomstyle=dotted borderbottomcolor=black borderbottomwidth=0.5 cellwidth=&gradewidth.cm font_face=calibri fontsize=&fontsize.pt}
						  				 style (header)={%if &colour.=Y %then borderrightcolor=cxE0E0E0;
														 %if &colour.=N %then borderrightcolor=white borderbottomcolor=black borderbottomwidth=0.5; borderrightwidth=0.5};
define Npct34 / "Grade   4        N (%)" style (column)={just=center borderbottomstyle=dotted borderbottomcolor=black borderbottomwidth=0.5 cellwidth=&gradewidth.cm font_face=calibri fontsize=&fontsize.pt}
						  				 style (header)={%if &colour.=Y %then borderrightcolor=cxE0E0E0;
														 %if &colour.=N %then borderrightcolor=white borderbottomcolor=black borderbottomwidth=0.5; borderrightwidth=0.5};
%if &anyGR5.=Y %then  %do;
define Npct35 / "Grade   5        N (%)" style (column)={just=center borderbottomstyle=dotted borderbottomcolor=black borderbottomwidth=0.5 cellwidth=&gradewidth.cm font_face=calibri fontsize=&fontsize.pt}
									     style (header)={%if &colour.=Y %then borderrightcolor=cxE0E0E0;
											 		     %if &colour.=N %then borderrightcolor=white borderbottomcolor=black borderbottomwidth=0.5; 
														borderrightwidth=0.5};
%end;
%if &anyGR5.=N %then define Npct35 /noprint;;

define Npct3345 / "Grade ^{unicode '2265'x}3     N (%)" style(column)={just=center cellwidth=&gradewidth.cm 
																	   %if &colour.=Y %then background=whitesmoke;
																	   %if &colour.=N %then background=white;
																	   font_face=calibri fontsize=&fontsize.pt
															 		   borderrightcolor=black borderrightwidth=0.5
																	   borderbottomstyle=dotted borderbottomcolor=black borderbottomwidth=0.5}
														%if &colour.=N %then style (header)={borderbottomcolor=black borderbottomwidth=0.5};;
%END;

	%IF %UPCASE(&column.)=POOLED %THEN %DO;
define Npct312 / "Grade 1/2   N (%)" style (column)={just=center borderbottomstyle=dotted borderbottomcolor=black borderbottomwidth=0.5 cellwidth=&gradewidth.cm font_face=calibri fontsize=&fontsize.pt}
						  			 style (header)={%if &colour.=Y %then borderrightcolor=cxE0E0E0;
													 %if &colour.=N %then borderrightcolor=white borderbottomcolor=black borderbottomwidth=0.5; borderrightwidth=0.5};
define Npct334 / "Grade 3/4   N (%)" style (column)={just=center borderbottomstyle=dotted borderbottomcolor=black borderbottomwidth=0.5 cellwidth=&gradewidth.cm font_face=calibri fontsize=&fontsize.pt}
						  			 style (header)={%if &colour.=Y %then borderrightcolor=cxE0E0E0;
													 %if &colour.=N %then borderrightcolor=white borderbottomcolor=black borderbottomwidth=0.5; borderrightwidth=0.5};
%if &anyGR5.=Y %then %do;
define Npct35 / "Grade   5        N (%)" style (column)={just=center borderbottomstyle=dotted borderbottomcolor=black borderbottomwidth=0.5 cellwidth=&gradewidth.cm font_face=calibri fontsize=&fontsize.pt}
	 								     style (header)={%if &colour.=Y %then borderrightcolor=cxE0E0E0;
													     %if &colour.=N %then borderrightcolor=white borderbottomcolor=black borderbottomwidth=0.5;
														  borderrightwidth=0.5};
%end;
%if &anyGR5.=N %then define Npct35 /noprint;;

define Npct3345 / "Grade ^{unicode '2265'x}3     N (%)" style(column)={just=center cellwidth=&gradewidth.cm 
													   				   %if &colour.=Y %then background=whitesmoke borderrightcolor=whitesmoke;
																	   %if &colour.=N %then background=white borderrightcolor=white;
																		borderbottomstyle=dotted borderbottomcolor=black borderbottomwidth=0.5
																		borderrightwidth=0.5 font_face=calibri fontsize=&fontsize.pt}
							 							style (header)={%if &colour.=Y %then borderrightcolor=cxE0E0E0;
																		%if &colour.=N %then borderrightcolor=white borderbottomcolor=black borderbottomwidth=0.5; borderrightwidth=0.5};
define Npct3ALL / "Grade ^{unicode '2265'x}1     N (%)" style(column)={just=center cellwidth=&gradewidth.cm 
																		%if &colour.=Y %then background=whitesmoke;
																		%if &colour.=N %then background=white;
																		font_face=calibri fontsize=&fontsize.pt
																		borderrightcolor=black borderrightwidth=0.5 
																		borderbottomstyle=dotted borderbottomcolor=black borderbottomwidth=0.5}
														%if &colour.=N %then style (header)={borderbottomcolor=black borderbottomwidth=0.5};;
%END;

	%IF %UPCASE(&column.)=SUMMARY %THEN %DO;
define Npct3345 / "Grade ^{unicode '2265'x}3     N (%)" style(column)={just=center cellwidth=&gradewidth.cm 
																		borderbottomstyle=dotted borderbottomcolor=black borderbottomwidth=0.5
																		borderrightcolor=white borderrightwidth=0.5 cellwidth=&gradewidth.cm
																		font_face=calibri fontsize=&fontsize.pt}
							 							style (header)={%if &colour.=Y %then borderrightcolor=cxE0E0E0;
																		%if &colour.=N %then borderrightcolor=white borderbottomcolor=black borderbottomwidth=0.5; borderrightwidth=0.5};
define Npct3ALL / "Grade ^{unicode '2265'x}1     N (%)"  style(column)={just=center cellwidth=&gradewidth.cm  
											 							borderrightcolor=black borderrightwidth=0.5 font_face=calibri fontsize=&fontsize.pt
											 							borderbottomstyle=dotted borderbottomcolor=black borderbottomwidth=0.5}
							  							style (header)={just=center borderrightcolor=black borderrightwidth=0.5 cellwidth=&gradewidth.cm
																		%if &colour.=N %then borderbottomcolor=black borderbottomwidth=0.5;};
	%END;
%END;
%END;

/*ARM4*/
%IF &totarm.=4 %THEN %DO;
%IF %INDEX(&where.,%str(&trt.))=0 or &show4.=Y %THEN %DO;

	%IF %UPCASE(&column.)=ALL %THEN %DO;
define Npct31 / "Grade     1     N (%)" style (column)={just=center borderbottomstyle=dotted borderbottomcolor=black borderbottomwidth=0.5 cellwidth=&gradewidth.cm font_face=calibri fontsize=&fontsize.pt}
						  				style (header)={%if &colour.=Y %then borderrightcolor=cxE0E0E0;
														%if &colour.=N %then borderrightcolor=white borderbottomcolor=black borderbottomwidth=0.5; borderrightwidth=0.5};
define Npct32 / "Grade     2     N (%)" style (column)={just=center borderbottomstyle=dotted borderbottomcolor=black borderbottomwidth=0.5 cellwidth=&gradewidth.cm font_face=calibri fontsize=&fontsize.pt}
						  				style (header)={%if &colour.=Y %then borderrightcolor=cxE0E0E0;
														%if &colour.=N %then borderrightcolor=white borderbottomcolor=black borderbottomwidth=0.5; borderrightwidth=0.5};
define Npct33 / "Grade     3     N (%)" style (column)={just=center borderbottomstyle=dotted borderbottomcolor=black borderbottomwidth=0.5 cellwidth=&gradewidth.cm font_face=calibri fontsize=&fontsize.pt}
						  				style (header)={%if &colour.=Y %then borderrightcolor=cxE0E0E0;
														%if &colour.=N %then borderrightcolor=white borderbottomcolor=black borderbottomwidth=0.5; borderrightwidth=0.5};
define Npct34 / "Grade     4     N (%)" style (column)={just=center borderbottomstyle=dotted borderbottomcolor=black borderbottomwidth=0.5 cellwidth=&gradewidth.cm font_face=calibri fontsize=&fontsize.pt}
						  				style (header)={%if &colour.=Y %then borderrightcolor=cxE0E0E0;
														%if &colour.=N %then borderrightcolor=white borderbottomcolor=black borderbottomwidth=0.5; borderrightwidth=0.5};
%if &anyGR5.=Y %then %do;
define Npct35 / "Grade     5     N (%)" style (column)={just=center borderbottomstyle=dotted borderbottomcolor=black borderbottomwidth=0.5 cellwidth=&gradewidth.cm font_face=calibri fontsize=&fontsize.pt}
  									    style (header)={%if &colour.=Y %then borderrightcolor=cxE0E0E0;
													    %if &colour.=N %then borderrightcolor=white borderbottomcolor=black borderbottomwidth=0.5;
														borderrightwidth=0.5};
%end;
%if &anyGR5.=N %then define Npct35 /noprint;;

define Npct3345 / "Grade ^{unicode '2265'x}3     N (%)" style(column)={just=center cellwidth=&gradewidth.cm 
													    			   %if &colour.=Y %then background=whitesmoke borderrightcolor=whitesmoke ;
													    			   %if &colour.=N %then background=white borderrightcolor=white ;
																		borderbottomstyle=dotted borderbottomcolor=black borderbottomwidth=0.5
																		borderrightwidth=0.5 cellwidth=&gradewidth.cm
																		font_face=calibri fontsize=&fontsize.pt}
							 							style (header)={ %if &colour.=Y %then borderrightcolor=cxE0E0E0;
																		 %if &colour.=N %then borderrightcolor=white borderbottomcolor=black borderbottomwidth=0.5; borderrightwidth=0.5};
define Npct3ALL / "Grade ^{unicode '2265'x}1     N (%)" style(column)={just=center cellwidth=&gradewidth.cm 
																	   %if &colour.=Y %then background=whitesmoke;
																	   %if &colour.=N %then background=white;
																	   font_face=calibri fontsize=&fontsize.pt
											 						   borderrightcolor=black borderrightwidth=0.5 cellwidth=&gradewidth.cm
											 						   borderbottomstyle=dotted borderbottomcolor=black borderbottomwidth=0.5}
														%if &colour.=N %then style (header)={borderbottomcolor=black borderbottomwidth=0.5};;



*Arm4*;
define Npct41 / "Grade     1     N (%)" style (column)={just=center borderbottomstyle=dotted borderbottomcolor=black borderbottomwidth=0.5 cellwidth=&gradewidth.cm font_face=calibri fontsize=&fontsize.pt}
						  				style (header)={%if &colour.=Y %then borderrightcolor=cxE0E0E0;
														%if &colour.=N %then borderrightcolor=white borderbottomcolor=black borderbottomwidth=0.5; borderrightwidth=0.5};
define Npct42 / "Grade     2     N (%)" style (column)={just=center borderbottomstyle=dotted borderbottomcolor=black borderbottomwidth=0.5 cellwidth=&gradewidth.cm font_face=calibri fontsize=&fontsize.pt}
						  				style (header)={%if &colour.=Y %then borderrightcolor=cxE0E0E0;
														%if &colour.=N %then borderrightcolor=white borderbottomcolor=black borderbottomwidth=0.5; borderrightwidth=0.5};
define Npct43 / "Grade     3     N (%)" style (column)={just=center borderbottomstyle=dotted borderbottomcolor=black borderbottomwidth=0.5 cellwidth=&gradewidth.cm font_face=calibri fontsize=&fontsize.pt}
						  				style (header)={%if &colour.=Y %then borderrightcolor=cxE0E0E0;
														%if &colour.=N %then borderrightcolor=white borderbottomcolor=black borderbottomwidth=0.5; borderrightwidth=0.5};
define Npct44 / "Grade     4     N (%)" style (column)={just=center borderbottomstyle=dotted borderbottomcolor=black borderbottomwidth=0.5 cellwidth=&gradewidth.cm font_face=calibri fontsize=&fontsize.pt}
						  				style (header)={%if &colour.=Y %then borderrightcolor=cxE0E0E0;
														%if &colour.=N %then borderrightcolor=white borderbottomcolor=black borderbottomwidth=0.5; borderrightwidth=0.5};
%if &anyGR5.=Y %then %do;
define Npct45 / "Grade     5     N (%)" style (column)={just=center borderbottomstyle=dotted borderbottomcolor=black borderbottomwidth=0.5 cellwidth=&gradewidth.cm font_face=calibri fontsize=&fontsize.pt}
    									style (header)={%if &colour.=Y %then borderrightcolor=cxE0E0E0;
													    %if &colour.=N %then borderrightcolor=white borderbottomcolor=black borderbottomwidth=0.5; 
														borderrightwidth=0.5};
%end;
%if &anyGR5.=N %then define Npct45 /noprint;;

define Npct4345 / "Grade ^{unicode '2265'x}3     N (%)" style(column)={just=center cellwidth=&gradewidth.cm 
																	   %if &colour.=Y %then background=whitesmoke borderrightcolor=whitesmoke;
																	   %if &colour.=N %then background=white borderrightcolor=white;
																		borderbottomstyle=dotted borderbottomcolor=black borderbottomwidth=0.5
																		borderrightwidth=0.5 cellwidth=&gradewidth.cm
																		font_face=calibri fontsize=&fontsize.pt}
							 							style (header)={%if &colour.=Y %then borderrightcolor=cxE0E0E0;
																		%if &colour.=N %then borderrightcolor=white borderbottomcolor=black borderbottomwidth=0.5; borderrightwidth=0.5};
define Npct4ALL / "Grade ^{unicode '2265'x}1     N (%)" style(column)={just=center cellwidth=&gradewidth.cm 
																		%if &colour.=Y %then background=whitesmoke;
																		%if &colour.=N %then background=white; 
																		 font_face=calibri fontsize=&fontsize.pt
																		 borderrightcolor=black borderrightwidth=0.5 cellwidth=&gradewidth.cm
																		 borderbottomstyle=dotted borderbottomcolor=black borderbottomwidth=0.5}
														%if &colour.=N %then style (header)={borderbottomcolor=black borderbottomwidth=0.5};;

%END;

	%IF %UPCASE(&column.)=SEVERE %THEN %DO;
define Npct33 / "Grade   3        N (%)" style (column)={just=center borderbottomstyle=dotted borderbottomcolor=black borderbottomwidth=0.5 cellwidth=&gradewidth.cm font_face=calibri fontsize=&fontsize.pt}
						  				style (header)={%if &colour.=Y %then borderrightcolor=cxE0E0E0;
														%if &colour.=N %then borderrightcolor=white borderbottomcolor=black borderbottomwidth=0.5; borderrightwidth=0.5};
define Npct34 / "Grade   4        N (%)" style (column)={just=center borderbottomstyle=dotted borderbottomcolor=black borderbottomwidth=0.5 cellwidth=&gradewidth.cm font_face=calibri fontsize=&fontsize.pt}
						  				style (header)={%if &colour.=Y %then borderrightcolor=cxE0E0E0;
														%if &colour.=N %then borderrightcolor=white borderbottomcolor=black borderbottomwidth=0.5; borderrightwidth=0.5};
%if &anyGR5.=Y %then %do;
define Npct35 / "Grade   5        N (%)" style (column)={just=center borderbottomstyle=dotted borderbottomcolor=black borderbottomwidth=0.5 cellwidth=&gradewidth.cm font_face=calibri fontsize=&fontsize.pt}
  									     style (header)={%if &colour.=Y %then borderrightcolor=cxE0E0E0;
														 %if &colour.=N %then borderrightcolor=white borderbottomcolor=black borderbottomwidth=0.5;
														 borderrightwidth=0.5};
%end;
%if &anyGR5.=N %then define Npct35 /noprint;;

define Npct3345 / "Grade ^{unicode '2265'x}3     N (%)" style(column)={just=center cellwidth=&gradewidth.cm 
																	   %if &colour.=Y %then background=whitesmoke;
																	   %if &colour.=N %then background=white; 
																		font_face=calibri fontsize=&fontsize.pt
															 			borderrightcolor=black borderrightwidth=0.5
																		borderbottomstyle=dotted borderbottomcolor=black borderbottomwidth=0.5}
														%if &colour.=N %then style (header)={borderbottomcolor=black borderbottomwidth=0.5};;

define Npct43 / "Grade   3        N (%)" style (column)={just=center borderbottomstyle=dotted borderbottomcolor=black borderbottomwidth=0.5 cellwidth=&gradewidth.cm font_face=calibri fontsize=&fontsize.pt}
						  				 style (header)={%if &colour.=Y %then borderrightcolor=cxE0E0E0;
														 %if &colour.=N %then borderrightcolor=white borderbottomcolor=black borderbottomwidth=0.5; borderrightwidth=0.5};
define Npct44 / "Grade   4        N (%)" style (column)={just=center borderbottomstyle=dotted borderbottomcolor=black borderbottomwidth=0.5 cellwidth=&gradewidth.cm font_face=calibri fontsize=&fontsize.pt}
						  				style (header)={%if &colour.=Y %then borderrightcolor=cxE0E0E0;
														%if &colour.=N %then borderrightcolor=white borderbottomcolor=black borderbottomwidth=0.5; borderrightwidth=0.5};
%if &anyGR5.=Y %then %do;
define Npct45 / "Grade   5        N (%)" style (column)={just=center borderbottomstyle=dotted borderbottomcolor=black borderbottomwidth=0.5 cellwidth=&gradewidth.cm font_face=calibri fontsize=&fontsize.pt}
						  				 style (header)={%if &colour.=Y %then borderrightcolor=cxE0E0E0;
														 %if &colour.=N %then borderrightcolor=white borderbottomcolor=black borderbottomwidth=0.5; 
														 borderrightwidth=0.5};
%end;
%if &anyGR5.=N %then define Npct45 /noprint;;

define Npct4345 / "Grade ^{unicode '2265'x}3     N (%)" style(column)={just=center cellwidth=&gradewidth.cm 
																	   %if &colour.=Y %then	background=whitesmoke;
																	   %if &colour.=N %then	background=white; 
																	   font_face=calibri fontsize=&fontsize.pt
															 			borderrightcolor=black borderrightwidth=0.5
																		borderbottomstyle=dotted borderbottomcolor=black borderbottomwidth=0.5}
														%if &colour.=N %then style (header)={borderbottomcolor=black borderbottomwidth=0.5};;
%END;

	%IF %UPCASE(&column.)=POOLED %THEN %DO;
define Npct312 / "Grade 1/2   N (%)" style (column)={just=center borderbottomstyle=dotted borderbottomcolor=black borderbottomwidth=0.5 cellwidth=&gradewidth.cm font_face=calibri fontsize=&fontsize.pt}
						  				style (header)={%if &colour.=Y %then borderrightcolor=cxE0E0E0;
														%if &colour.=N %then borderrightcolor=white borderbottomcolor=black borderbottomwidth=0.5; borderrightwidth=0.5};
define Npct334 / "Grade 3/4   N (%)" style (column)={just=center borderbottomstyle=dotted borderbottomcolor=black borderbottomwidth=0.5 cellwidth=&gradewidth.cm font_face=calibri fontsize=&fontsize.pt}
						  				style (header)={%if &colour.=Y %then borderrightcolor=cxE0E0E0;
														%if &colour.=N %then borderrightcolor=white borderbottomcolor=black borderbottomwidth=0.5; borderrightwidth=0.5};
%if &anyGR5.=Y %then %do;
define Npct35 / "Grade   5        N (%)" style (column)={just=center borderbottomstyle=dotted borderbottomcolor=black borderbottomwidth=0.5 cellwidth=&gradewidth.cm font_face=calibri fontsize=&fontsize.pt}
						  				 style (header)={%if &colour.=Y %then borderrightcolor=cxE0E0E0;
														 %if &colour.=N %then borderrightcolor=white borderbottomcolor=black borderbottomwidth=0.5;
														 borderrightwidth=0.5};
%end;
%if &anyGR5.=N %then define Npct35 /noprint;;

define Npct3345 / "Grade ^{unicode '2265'x}3     N (%)" style(column)={just=center cellwidth=&gradewidth.cm 
																	   %if &colour.=Y %then background=whitesmoke borderrightcolor=whitesmoke;
																	   %if &colour.=N %then background=white borderrightcolor=white;
																		borderbottomstyle=dotted borderbottomcolor=black borderbottomwidth=0.5
																		borderrightwidth=0.5 font_face=calibri fontsize=&fontsize.pt}
						  								style (header)={%if &colour.=Y %then borderrightcolor=cxE0E0E0;
																		%if &colour.=N %then borderrightcolor=white borderbottomcolor=black borderbottomwidth=0.5; borderrightwidth=0.5};
define Npct3ALL / "Grade ^{unicode '2265'x}1     N (%)" style(column)={just=center cellwidth=&gradewidth.cm 
																		%if &colour.=Y %then background=whitesmoke;
																		%if &colour.=N %then background=white;
																		font_face=calibri fontsize=&fontsize.pt
																		 borderrightcolor=black borderrightwidth=0.5 
																		 borderbottomstyle=dotted borderbottomcolor=black borderbottomwidth=0.5}
														%if &colour.=N %then style (header)={borderbottomcolor=black borderbottomwidth=0.5};;

define Npct412 / "Grade 1/2   N (%)" style (column)={just=center borderbottomstyle=dotted borderbottomcolor=black borderbottomwidth=0.5 cellwidth=&gradewidth.cm font_face=calibri fontsize=&fontsize.pt}
						  				style (header)={%if &colour.=Y %then borderrightcolor=cxE0E0E0;
														%if &colour.=N %then borderrightcolor=white borderbottomcolor=black borderbottomwidth=0.5; borderrightwidth=0.5};
define Npct434 / "Grade 3/4   N (%)" style (column)={just=center borderbottomstyle=dotted borderbottomcolor=black borderbottomwidth=0.5 cellwidth=&gradewidth.cm font_face=calibri fontsize=&fontsize.pt}
						  				style (header)={%if &colour.=Y %then borderrightcolor=cxE0E0E0;
														%if &colour.=N %then borderrightcolor=white borderbottomcolor=black borderbottomwidth=0.5; borderrightwidth=0.5};
%if &anyGR5.=Y %then %do;
define Npct45 / "Grade   5        N (%)" style (column)={just=center borderbottomstyle=dotted borderbottomcolor=black borderbottomwidth=0.5 cellwidth=&gradewidth.cm font_face=calibri fontsize=&fontsize.pt}
						  				 style (header)={%if &colour.=Y %then borderrightcolor=cxE0E0E0;
														 %if &colour.=N %then borderrightcolor=white borderbottomcolor=black borderbottomwidth=0.5;
														 borderrightwidth=0.5};
%end;
%if &anyGR5.=N %then define Npct45 /noprint;;

define Npct4345 / "Grade ^{unicode '2265'x}3     N (%)" style(column)={just=center cellwidth=&gradewidth.cm 
																	  %if &colour.=Y %then background=whitesmoke borderrightcolor=whitesmoke;
																	  %if &colour.=N %then background=white borderrightcolor=white;
																		borderbottomstyle=dotted borderbottomcolor=black borderbottomwidth=0.5
																		borderrightwidth=0.5 font_face=calibri fontsize=&fontsize.pt}
							 							style (header)={%if &colour.=Y %then borderrightcolor=cxE0E0E0;
																		%if &colour.=N %then borderrightcolor=white borderbottomcolor=black borderbottomwidth=0.5; borderrightwidth=0.5};
define Npct4ALL / "Grade ^{unicode '2265'x}1     N (%)" style(column)={just=center cellwidth=&gradewidth.cm 
																	   %if &colour.=Y %then background=whitesmoke;
																	   %if &colour.=N %then background=white;
																	   font_face=calibri fontsize=&fontsize.pt
																	   borderrightcolor=black borderrightwidth=0.5 
																	   borderbottomstyle=dotted borderbottomcolor=black borderbottomwidth=0.5}
														%if &colour.=N %then style (header)={borderbottomcolor=black borderbottomwidth=0.5};;

	%END;

	%IF %UPCASE(&column.)=SUMMARY %THEN %DO;
define Npct3345 / "Grade ^{unicode '2265'x}3     N (%)" style(column)={just=center cellwidth=&gradewidth.cm 
																		borderbottomstyle=dotted borderbottomcolor=black borderbottomwidth=0.5
																		borderrightcolor=white borderrightwidth=0.5 cellwidth=&gradewidth.cm
																		font_face=calibri fontsize=&fontsize.pt}
						  								style (header)={%if &colour.=Y %then borderrightcolor=cxE0E0E0;
																		%if &colour.=N %then borderrightcolor=white borderbottomcolor=black borderbottomwidth=0.5; borderrightwidth=0.5};
define Npct3ALL / "Grade ^{unicode '2265'x}1     N (%)"  style(column)={just=center cellwidth=&gradewidth.cm 
																		 borderrightcolor=black borderrightwidth=0.5 font_face=calibri fontsize=&fontsize.pt
																		 borderbottomstyle=dotted borderbottomcolor=black borderbottomwidth=0.5}
							  							style (header)={just=center borderrightcolor=black borderrightwidth=0.5 cellwidth=&gradewidth.cm
																		%if &colour.=N %then borderbottomcolor=black borderbottomwidth=0.5;};
define Npct4345 / "Grade ^{unicode '2265'x}3     N (%)" style(column)={just=center cellwidth=&gradewidth.cm
																		borderbottomstyle=dotted borderbottomcolor=black borderbottomwidth=0.5
																		borderrightcolor=white borderrightwidth=0.5 cellwidth=&gradewidth.cm
																		font_face=calibri fontsize=&fontsize.pt}
						  								style (header)={%if &colour.=Y %then borderrightcolor=cxE0E0E0;
																		%if &colour.=N %then borderrightcolor=white borderbottomcolor=black borderbottomwidth=0.5; borderrightwidth=0.5};
define Npct4ALL / "Grade ^{unicode '2265'x}1     N (%)"  style(column)={just=center cellwidth=&gradewidth.cm  
																		 borderrightcolor=black borderrightwidth=0.5 font_face=calibri fontsize=&fontsize.pt
																		 borderbottomstyle=dotted borderbottomcolor=black borderbottomwidth=0.5}
							  							style (header)={just=center borderrightcolor=black borderrightwidth=0.5 cellwidth=&gradewidth.cm
																		%if &colour.=N %then borderbottomcolor=black borderbottomwidth=0.5;};

	%END;
%END;
%end;
run;
title;


%IF (%length(&ODSFile)>0) %THEN %DO;
		%STR(ods rtf close;);
%END;
%END;


/*=============================================================================================
 ================================	CREATE GRAPHS   ===========================================
==============================================================================================*/

%IF (%length(&graph.)>0) %THEN %DO;

**Reset ODS option for PDF*;
%if %UPCASE(&Display.)=PDF %THEN %DO;
	option orientation=portrait;
%END;

******************************************************************************************************* 
Define local variables for options which vary according to type of output required (value of DISPLAY)
*******************************************************************************************************;

%LOCAL imagefmt imagename imagefmt ;

%IF %UPCASE(&DISPLAY.)=SASEMF  %THEN %LET DISPLAY=WORD;

%IF %UPCASE(&DISPLAY.)=WORD OR %UPCASE(&DISPLAY.)=POWERPOINT %THEN %DO;
   %LET OutFile2 = %CMPRES(%scan(%str(&SOCgrph.),1,.).wmf );
   %LET OutFile3 = %CMPRES(%scan(%str(&pct_forest.),1,.).wmf );
   %LET OutFile4 = %CMPRES(%scan(%str(&pct_plot.),1,.).wmf );
   %LET OutFile5 = %CMPRES(%scan(%str(&pyramid.),1,.).wmf );

      **Add option for ods graphics for Forest_pct**;
		%LET maxlegendarea=40;
		%LET imagefmt=wmf ;
		%LET imagename2=%scan(%str(&outfile3.),-1,"\");
		%LET imagename3=%scan(&imagename2.,1,.);
		%LET gpath2=%SYSFUNC(TRANWRD(%str(&outfile3.),&imagename2.,));

      **Add option for ods graphics for pct_plot**;
		%LET imagefmt=wmf ;
		%LET imagename4=%scan(%str(&outfile4.),-1,"\");
		%LET imagename5=%scan(&imagename4.,1,.);
		%LET gpath3=%SYSFUNC(TRANWRD(%str(&outfile4.),&imagename4.,));

		**Add option for ods graphics for SOCgrph**;
		%LET imagefmt=wmf ;
		%LET imagename6=%scan(%str(&outfile2.),-1,"\");
		%LET imagename7=%scan(&imagename6.,1,.);
		%LET gpath4=%SYSFUNC(TRANWRD(%str(&outfile2.),&imagename6.,));

		**Add option for ods graphics for Pyramid**;
		%LET imagefmt=wmf ;
		%LET imagename8=%scan(%str(&outfile5.),-1,"\");
		%LET imagename9=%scan(&imagename8.,1,.);
		%LET gpath5=%SYSFUNC(TRANWRD(%str(&outfile5.),&imagename8.,));
%END;

%ELSE %IF %UPCASE(&DISPLAY.)=TIFF %THEN %DO;
   %LET OutFile2 = %CMPRES(%scan(%str(&SOCgrph.),1,.).tif);
   %LET OutFile3 = %CMPRES(%scan(%str(&pct_forest.),1,.).tif);
   %LET OutFile4 = %CMPRES(%scan(%str(&pct_plot.),1,.).tif);
   %LET OutFile5 = %CMPRES(%scan(%str(&pyramid.),1,.).tif);

      **Add option for ods graphics for Forest_pct**;
		%LET maxlegendarea=40;
		%LET imagefmt=TIFF;
		%LET imagename2=%scan(%str(&outfile3.),-1,"\");
		%LET imagename3=%scan(&imagename2.,1,.);
		%LET gpath2=%SYSFUNC(TRANWRD(%str(&outfile3.),&imagename2.,));

      **Add option for ods graphics for pct_plot**;
		%LET imagefmt=TIFF;
		%LET imagename4=%scan(%str(&outfile4.),-1,"\");
		%LET imagename5=%scan(&imagename4.,1,.);
		%LET gpath3=%SYSFUNC(TRANWRD(%str(&outfile4.),&imagename4.,));

		**Add option for ods graphics for SOCgrph**;
		%LET imagefmt=TIFF;
		%LET imagename6=%scan(%str(&outfile2.),-1,"\");
		%LET imagename7=%scan(&imagename6.,1,.);
		%LET gpath4=%SYSFUNC(TRANWRD(%str(&outfile2.),&imagename6.,));

		**Add option for ods graphics for Pyramid**;
		%LET imagefmt=TIFF;
		%LET imagename8=%scan(%str(&outfile5.),-1,"\");
		%LET imagename9=%scan(&imagename8.,1,.);
		%LET gpath5=%SYSFUNC(TRANWRD(%str(&outfile5.),&imagename8.,));

%END;

%ELSE %IF %UPCASE(&DISPLAY.)=PNG %THEN %DO;
   %LET OutFile2 = %CMPRES(%scan(%str(&SOCgrph.),1,.).png);
   %LET OutFile3 = %CMPRES(%scan(%str(&pct_forest.),1,.).png);
   %LET OutFile4 = %CMPRES(%scan(%str(&pct_plot.),1,.).png);
   %LET OutFile5 = %CMPRES(%scan(%str(&pyramid.),1,.).png);

   	    **Add option for ods graphics for Forest_pct**;
		%LET maxlegendarea=40;
		%LET imagefmt=PNG;
		%LET imagename2=%scan(%str(&outfile3.),-1,"\");
		%LET imagename3=%scan(&imagename2.,1,.);
		%LET gpath2=%SYSFUNC(TRANWRD(%str(&outfile3.),&imagename2.,));

		**Add option for ods graphics for pct_plot**;
		%LET imagefmt=PNG;
		%LET imagename4=%scan(%str(&outfile4.),-1,"\");
		%LET imagename5=%scan(&imagename4.,1,.);
		%LET gpath3=%SYSFUNC(TRANWRD(%str(&outfile4.),&imagename4.,));

		**Add option for ods graphics for SOCgrph**;
		%LET imagefmt=PNG;
		%LET imagename6=%scan(%str(&outfile2.),-1,"\");
		%LET imagename7=%scan(&imagename6.,1,.);
		%LET gpath4=%SYSFUNC(TRANWRD(%str(&outfile2.),&imagename6.,));

		**Add option for ods graphics for Pyramid**;
		%LET imagefmt=PNG;
		%LET imagename8=%scan(%str(&outfile5.),-1,"\");
		%LET imagename9=%scan(&imagename8.,1,.);
		%LET gpath5=%SYSFUNC(TRANWRD(%str(&outfile5.),&imagename8.,));
%END;

%ELSE %IF %UPCASE(&DISPLAY.)=PDF %THEN %DO;
   %LET OutFile2 = %CMPRES(%scan(%str(&SOCgrph.),1,.).pdf);
   %LET OutFile3 = %CMPRES(%scan(%str(&pct_forest.),1,.).pdf);
   %LET OutFile4 = %CMPRES(%scan(%str(&pct_plot.),1,.).pdf);
   %LET OutFile5 = %CMPRES(%scan(%str(&pyramid.),1,.).pdf);

        **Add option for ods graphics for Forest_pct**;
		%LET maxlegendarea=40;
		%LET imagefmt=PDF;
		%LET imagename2=%scan(%str(&outfile3.),-1,"\");
		%LET imagename3=%scan(&imagename2.,1,.);
		%LET gpath2=%SYSFUNC(TRANWRD(%str(&outfile3.),&imagename2.,));

		**Add option for ods graphics for pct_plot**;
		%LET imagefmt=PDF;
		%LET imagename4=%scan(%str(&outfile4.),-1,"\");
		%LET imagename5=%scan(&imagename4.,1,.);
		%LET gpath3=%SYSFUNC(TRANWRD(%str(&outfile4.),&imagename4.,));

		**Add option for ods graphics for SOCgrph**;
		%LET imagefmt=PDF;
		%LET imagename6=%scan(%str(&outfile2.),-1,"\");
		%LET imagename7=%scan(&imagename6.,1,.);
		%LET gpath4=%SYSFUNC(TRANWRD(%str(&outfile2.),&imagename6.,));

		**Add option for ods graphics for Pyramid**;
		%LET imagefmt=PDF;
		%LET imagename8=%scan(%str(&outfile5.),-1,"\");
		%LET imagename9=%scan(&imagename8.,1,.);
		%LET gpath5=%SYSFUNC(TRANWRD(%str(&outfile5.),&imagename8.,));

%END;


* Define macro variables for font colours, depending on whether graph is required 
  in colour or black and white;
%LOCAL reference darkblue mediumblue blue lightblue;
%IF &Colour.=Y %THEN %DO;
	%let reference=CXF78E1E;
	%let darkblue=CX0054A4;
	%let mediumblue=CX60B4FF;
	%let blue=CX98ECFF;
	%let lightblue=CXE0FFFF;
%END;

%ELSE %DO;
	%let refForest=black;
	%let reference=black;
		%IF &totarm.=1 %THEN %LET reference=gray;
	%let darkblue=gray;
	%let mediumblue=lightgrey;
	%let blue=whitesmoke;
%END;


**Change colors for the bars**;
proc template;
	define style mytemplate;
	parent=styles.listing;
	style GraphBar from GraphComponent /
		displayopts = "color inside";
	style GraphData1 from GraphData1 /  color = &reference. ;
	%if &totarm.>1 %then %do; style GraphData2 from GraphData2 /  color = &darkblue. ;%end;
	%if &totarm.=3 %then %do; style GraphData3 from GraphData3 /  color = &mediumblue.;%end;
	%if &totarm.=4 %then %do;
		style GraphData3 from GraphData3 /  color = &mediumblue.;
		style GraphData4 from GraphData4 /  color = &blue. ;
	%end;					 
end;
run;
/************************************************************************************
 NOTE: Except if another treashold is provided, All the graphs will present AEs with 
		5 percent incidence when &graph.=severe and
		10 percent incidence when &graph.=all
  This is done in order to keep an acceptable and readable size for the graphs
************************************************************************************/

/*********************************************
 ***** Graph Number 1: CumGrph (cancelled)  **
**********************************************/	

/*********************************************
 ***** Graph Number 2: SOCgrph          ******
**********************************************/

%IF (%length(&SOCgrph.)>0) %THEN %DO;

/* data preparation steps: SUM OF ALL GRADES*/

proc sql;
create table __sumall_SOC as
select *,  sum(COUNT_GR_SOC1) as tot_SOC_1,
		   sum(COUNT_GR_SOC2) as tot_SOC_2,
		   sum(COUNT_GR_SOC3) as tot_SOC_3,
		   sum(COUNT_GR_SOC4) as tot_SOC_4,
		   sum(COUNT_GR_SOC5) as tot_SOC_5,
		   sum (COUNT_GR_SOC_ALL) as tot_SOC_ALL_GR,
 		   sum(COUNT_GR_SOC345) as tot_SOC_GR345
from ___AEv2
group by &trt., SOC2;
quit;

/*PERCENTAGE OF EACH GRADES + CREATION OF &ANYGR5 TO CHECK IF GRADE 5 NEED TO BE DISPLAYED*/
data __sumall_SOC;
length SOC_short $3. ; 
set __sumall_SOC;
		if &trt.=1 then do;
			PCT_SOC_345=ROUND((tot_SOC_GR345/&Ntot1.)*100, 0.1);
			PCT_SOC_ALL=ROUND((tot_SOC_ALL_GR/&Ntot1.)*100, 0.1);
		end;
	%IF &totarm.>1 %THEN %DO;
		if &trt.=2 then do;
			PCT_SOC_345=ROUND((tot_SOC_GR345/&Ntot2.)*100, 0.1);
			PCT_SOC_ALL=ROUND((tot_SOC_ALL_GR/&Ntot2.)*100, 0.1);
		end;
	%END;
	%IF &totarm.=3 %THEN %DO;
		if &trt.=3 then do;
			PCT_SOC_345=ROUND((tot_SOC_GR345/&Ntot3.)*100, 0.1);
			PCT_SOC_ALL=ROUND((tot_SOC_ALL_GR/&Ntot3.)*100, 0.1);
		end;
	%END;
	%IF &totarm.=4 %THEN %DO;
		if &trt.=3 then do;
			PCT_SOC_345=ROUND((tot_SOC_GR345/&Ntot3.)*100, 0.1);
			PCT_SOC_ALL=ROUND((tot_SOC_ALL_GR/&Ntot3.)*100, 0.1);
		end;
		
		if &trt.=4 then do;
			PCT_SOC_345=ROUND((tot_SOC_GR345/&Ntot4.)*100, 0.1);
			PCT_SOC_ALL=ROUND((tot_SOC_ALL_GR/&Ntot4.)*100, 0.1);
		end;
		
	%END;
	SOC_short=substr(SOC2,1,3);
run;

proc sort data=__sumall_SOC out=__plot3 nodupkey; by SOC2 &trt. ;run;


*Max total for the calculation of the maximum for YAXIS*;
proc sql;
create table __Plot3_1 as
	select*, %if %UPCASE(&graph.)=SEVERE %then max(PCT_SOC_345) as maxpct; 
			 %if %UPCASE(&graph.)=ALL %then max(PCT_SOC_ALL) as maxpct; 
	from __plot3;
quit;

data __Plot3_1;
set __Plot3_1;
maxaxisUpper=ROUND (maxpct, 10.0)+10;
run;

data _NULL_;
set __Plot3_1;
call symput ("Upper1",maxaxisUpper);
run;
%put &Upper1. ;

**sum incidence by SOC to be able to sort in descending order**;
proc sql;
create table __Plot3_2 as
select *, %if %UPCASE(&graph.)=ALL %then sum(PCT_SOC_ALL) as TOT_INC;
	      %if %UPCASE(&graph.)=SEVERE %then sum (PCT_SOC_345) as TOT_INC;
from __Plot3_1
group by SOC_short;
quit;

data __Plot3_2;
set __Plot3_2;
%if %UPCASE(&graph.)=ALL %then %do;
	if PCT_SOC_ALL=. then delete;
%end;
%if %UPCASE(&graph.)=SEVERE %then %do;
	if PCT_SOC_345=. then delete;
%end;
run;

%IF %UPCASE(&AxisOrder.)=SOC %THEN %DO;
	proc sort data=__Plot3_2; by SOC_short; run;
%END;

%IF %UPCASE(&AxisOrder.)=FREQ %THEN %DO;
	proc sort data=__Plot3_2; by descending TOT_INC; run;
%END;



/*Draw graph*/
ODS _ALL_ close;
ods listing;
%let width=30;
%let height=18;  


%IF %UPCASE(&DISPLAY.)=PDF %THEN %DO ;
option orientation=landscape nodate;
ods pdf file="&SOCgrph." style=mytemplate %if &editGrph.=Y %then sge=on;;
%END;

	ods graphics on/ reset reset=index border=off width=&width. cm height=&height. cm outputfmt=&imagefmt. imagename="&imagename7.";

%IF %UPCASE(&DISPLAY.)^=PDF %THEN %DO ;
ods listing gpath="&gpath4." style=mytemplate %if &editGrph.=Y %then sge=on;;
%END;


/*Adapt the title to the situation*/
	%if %UPCASE(&graph.)=SEVERE %then %do; title j=c h=14pt "Frequency of patients with clinical Adverse Events (Grades 3-5), by SOC"; %end;
	%if %UPCASE(&graph.)=ALL %then %do; title j=c h=14pt "Frequency of patients with clinical Adverse Events (all grades), by SOC"; %end;

	%IF (&related.=Y and &SAE.=N and &recovered.=N) %THEN %DO ; Title2 "AE's related to treatment.";%END;
	%IF (&related.=N and &SAE.=Y and &recovered.=N) %THEN %DO ; Title "Frequency of patients with Serious Adverse events, by SOC.";%END;
	%IF (&related.=N and &SAE.=N and &recovered.=Y) %THEN %DO ; Title2 "AE's recovered or recovered with sequelae";%END;
	%IF (&related.=Y and &SAE.=Y and &recovered.=N) %THEN %DO ; Title "Frequency of patients with Serious Adverse events related to treatment, by SOC.";%END;
	%IF (&related.=Y and &SAE.=N and &recovered.=Y) %THEN %DO ; Title2 "AE's related to treatment";
																Title3 "AE's recovered or recovered with sequelae";%END;
	%IF (&related.=N and &SAE.=Y and &recovered.=Y) %THEN %DO ; Title "Frequency of patients with Serious Adverse events, by SOC.";
																Title2 "AE's recovered or recovered with sequelae";%END;
	%IF (&related.=Y and &SAE.=Y and &recovered.=Y) %THEN %DO ; Title "Frequency of patients with Serious Adverse events related to treatment, by SOC.";
																Title2 "AE's recovered or recovered with sequelae";%END;
	%IF (%length(&where.)>1) %THEN %DO; Title4 "Where &where.";%END;


proc sgplot data=__Plot3_2;
  vbar SOC_short / %if %UPCASE(&graph.)=ALL %then response=PCT_SOC_ALL;
				   %if %UPCASE(&graph.)=SEVERE %then response=PCT_SOC_345;
				   group=&trt. nostatlabel
         groupdisplay=cluster  dataskin=pressed ;
  yaxis label="Percent" grid VALUES=( 0 to &Upper1. by 10);
  xaxis discreteorder=data label="System Organ Class (SOC)";
  run;

title;


%IF %UPCASE(&DISPLAY.)^=PDF %THEN %DO ;
ods listing gpath=none;
ods graphics off;
%END;

%IF %UPCASE(&DISPLAY.)=PDF %THEN %DO ;
ods pdf close;
%END;
ods listing;
%END;

/********************************************************
 ***** Graph Number 3: Percentages and forest plot ******
*********************************************************/

/*NOTE: To calculate the RR, one contingency table (treatment in column | AE+ or AE- in row) 
        is created for each adverse event. All those tables including the RR and IC are then set together in one 
        database to create the forest plots*/


%IF (%length(&PCT_FOREST)>0) OR (%length(&PCT_PLOT.)>0) %THEN %DO;

proc sort data=__sumall (where=(meddraterm ne "*GENERAL TERM"))out=__graph (keep=meddraterm SOC2 &trt. tot: pct:) ; by meddraterm &trt.; run;

data __graph_arm1 __graph_arm2 %if &totarm.=3 %then __graph_arm3;
						   %if &totarm.=4 %then __graph_arm3 __graph_arm4;
;
set __graph;
if &trt.=1 then output __graph_arm1;
if &trt.=2 then output __graph_arm2;
%IF &totarm.=3 %THEN if &trt.=3 then output __graph_arm3;;
%IF &totarm.=4 %THEN %DO;
if &trt.=3 then output __graph_arm3;
if &trt.=4 then output __graph_arm4;
%END;
run;

proc sort data=__graph_arm1 nodupkey; by meddraterm &trt.; run;
proc sort data=__graph_arm2 nodupkey; by meddraterm &trt.; run;

%IF &totarm.=3 %then %do;
proc sort data=__graph_arm3 nodupkey; by meddraterm &trt.; run;
%end;

%IF &totarm.=4 %then %do;
proc sort data=__graph_arm3 nodupkey; by meddraterm &trt.; run;
proc sort data=__graph_arm4 nodupkey; by meddraterm &trt.; run;
%end;


data __graph_flag (keep=MEDDRATERM SOC2 flag:);
merge __graph_arm1 (in=InArm1) __graph_arm2 (in=InArm2) %if &totarm.=3 %then __graph_arm3 (in=InArm3);
													%if &totarm.=4 %then __graph_arm3 (in=InArm3) __graph_arm4 (in=InArm4);
;
by meddraterm ;
if InArm1 then flag_arm1=1;
if InArm2 then flag_arm2=1;
%if &totarm.=3 %then if InArm3 then flag_arm3=1;;
%if &totarm.=4 %then %do;
if InArm3 then flag_arm3=1;
if InArm4 then flag_arm4=1;
%end;
run;

/*NOTE: if no AE for one treatment arm, need to add a line with 0.0 fot pct and count. 
        Otherwise the contingency table cannot be created.*/

%IF &totarm.>1 %THEN %DO;
data __create_arm1(keep=MEDDRATERM SOC2) __create_arm2 (keep=MEDDRATERM SOC2) 
	 %if &totarm.=3 %then __create_arm3 (keep=MEDDRATERM SOC2);
	 %if &totarm.=4 %then __create_arm3 (keep=MEDDRATERM SOC2) __create_arm4 (keep=MEDDRATERM SOC2);
;
set __graph_flag;
if flag_arm1=. then output __create_arm1;
if flag_arm2=. then output __create_arm2;
%if &totarm.=3 %then if flag_arm3=. then output __create_arm3;;
%if &totarm.=4 %then %do;
if flag_arm3=. then output __create_arm3;
if flag_arm4=. then output __create_arm4;
%end;
run;

/*Create missing arm*/
data __create_arm1;
set __create_arm1;
&trt.=1;totpat=&Ntot1.; 
run;

%IF &totarm.>1 %then %do;
data __create_arm2;
set __create_arm2;
&trt.=2;totpat=&Ntot2.; 
run;
%end;

%IF &totarm.=3 %THEN %DO;
data __create_arm3;
set __create_arm3;
&trt.=3;totpat=&Ntot3.; 
run;
%end;

%IF &totarm.=4 %THEN %DO;
		data __create_arm3;
		set __create_arm3;
		&trt.=3;totpat=&Ntot3.; 
		run;
	
		data __create_arm4;
		set __create_arm4;
		&trt.=4;totpat=&Ntot4.; 
		run;
%END;


data __graph_all;
set __create_arm1 __create_arm2 %if &totarm.=3 %then __create_arm3;
							%if &totarm.=4 %then __create_arm3 __create_arm4;
;
TOT_GR_345=0.0;
TOT_ALL_GR=0.0;
PCT_GR_345=0.0;
PCT_GR_ALL=0.0;
run;
%END;

%IF &totarm.=1 %THEN %DO;
data __graph_all;
set __graph_arm1 ;
run;
%end;

proc sort data= __sumall (where=(meddraterm ne "*GENERAL TERM"))out=__plot2 (keep=meddraterm SOC2 &trt. tot: pct:) nodupkey; by meddraterm &trt.; run;

%sort(data=__plot2,var=meddraterm &trt.) ;
%sort(data=__graph_all,var=meddraterm &trt.) ;

data __plot2_2;
merge __graph_all (in=a) __plot2 ;
by meddraterm &trt.;
	if TOT_GR_1=. then TOT_GR_1=0;
	if TOT_GR_2=. then TOT_GR_2=0;
	if TOT_GR_3=. then TOT_GR_3=0;
	if TOT_GR_4=. then TOT_GR_4=0;
	if TOT_GR_12=. then TOT_GR_12=0;
	if TOT_GR_34=. then TOT_GR_34=0;
	if TOT_GR_345=. then TOT_GR_345=0;
	if TOT_ALL_GR=. then TOT_ALL_GR=0;

	if PCT_GR_1=. then PCT_GR_1=0.0;
	if PCT_GR_2=. then PCT_GR_2=0.0;
	if PCT_GR_3=. then PCT_GR_3=0.0;
	if PCT_GR_4=. then PCT_GR_4=0.0;
	if PCT_GR_12=. then PCT_GR_12=0.0;
	if PCT_GR_34=. then PCT_GR_34=0.0;
	if PCT_GR_345=. then PCT_GR_345=0.0;
	if PCT_GR_ALL=. then PCT_GR_ALL=0.0;

%if &anyGR5.=Y %then %do;
if TOT_GR_5=. then TOT_GR_5=0;
if PCT_GR_5=. then PCT_GR_5=0.0;
%end;
MEDDRATERM=Propcase(MEDDRATERM);
run;


proc sort data=__plot2_2  out=__plot2_3(keep=meddraterm) nodupkey; by meddraterm;run;

data __plot2_3;
set __plot2_3;
indTerm=_n_;
MEDDRATERM=TRANWRD(MEDDRATERM,"'"," ");
run;

data _null_;
set __plot2_3;
call symput("labelTERM"||compress(_n_) ,trim(left(MEDDRATERM)));
call symput('NTERM',compress(indTerm));
run;
%put &labelTERM10;

%IF (%length(&PCT_FOREST)>0) and &totarm.>1 %THEN %DO;

%macro relrisk;
%do i=1 %to &NTERM.;

	data __data&i._YES;
	set __plot2_2;
	present=1;
		if meddraterm="&&labelTERM&i." then output __data&i._YES;
	run;

	data __data&i._NO;
	set __plot2_2 ;
	present=0;
		if meddraterm="&&labelTERM&i." then output __data&i._NO;
	run;

	data __data&i._ALL;
	set __data&i._YES (in=a) __data&i._NO (in=b);
		%if %UPCASE(&graph.)=ALL %then %do;
			if a then count=TOT_ALL_GR;
			if b then count=totpat-TOT_ALL_GR;
		%end;

		%if %UPCASE(&graph.)=SEVERE %then %do;
			if a then count=TOT_GR_345;
			if b then count=totpat-TOT_GR_345;
		%end;
	run;

	/*Here to avoid error messages, I need to avoir division by zero =>when present=1, 
	  TOT_ALL_GR or TOT_GR_345 cannot be equal to zero*/
    /*Do RR calculation only when event observed*/

		proc sort data=__data&i._ALL; by descending &trt.; run;

		data __data&i._trt2 __data&i._trt3 __data&i._trt4;
		set __data&i._ALL;
		if &trt.=2 then output __data&i._trt2;
		if &trt.=3 then output __data&i._trt3;
		if &trt.=4 then output __data&i._trt4;
		run;

		**Skip process when count=0 for experimental arms*;
		proc sql noprint;
			select case
			when (present=1 and count=0) then 'Y'
			else 'N'
			end into :skip2
			from __data&i._trt2;
		quit;

		proc sql noprint;
			select case
			when (present=1 and count=0) then 'Y'
			else 'N'
			end into :skip3
			from __data&i._trt3;
		quit;

		proc sql noprint;
			select case
			when (present=1 and count=0) then 'Y'
			else 'N'
			end into :skip4
			from __data&i._trt4;
		quit;


%IF &skip2.=N %THEN %DO;
		proc freq data=__data&i._ALL order=data;
			weight count;
			tables &trt.*present / relrisk cl alpha=&alpha.;
			where &trt.=1 or &trt.=2;
			output out=__test&i._arm12 relrisk ;
		run;

		%if %sysfunc(exist(__test&i._arm12)) and _N_>0 %then %do; 
		data __test&i._arm12 ;
		set __test&i._arm12 (keep= _RRC1_ L_RRC1 U_RRC1); 
		meddraterm="&&labelTERM&i.";
		&trt.=2;
		run;
		%end;
%END;


	
	%IF &totarm.>2 %THEN %DO;
	
	%IF &skip3.=N %THEN %DO;
		proc freq data=__data&i._ALL order=data;
			weight count;
			tables &trt.*present / relrisk cl alpha=&alpha.;
			where &trt.=1 or &trt.=3;
			output out=__test&i._arm13 relrisk ;
		run;

		%if %sysfunc(exist(__test&i._arm13)) and _N_>0 %then %do; 
		data __test&i._arm13 ;							 
		set __test&i._arm13 (keep= _RRC1_ L_RRC1 U_RRC1); 
		meddraterm="&&labelTERM&i.";
		&trt.=3;
		run;
		%end;
	%END;
	%END;

	%IF &totarm.=4 %THEN %DO;

	%IF &skip4.=N %THEN %DO;	
		proc freq data=__data&i._ALL order=data;
			weight count;
			tables &trt.*present / relrisk cl alpha=&alpha.;
			where &trt.=1 or &trt.=4;
			output out=__test&i._arm14 relrisk ;
		run;
		%if %sysfunc(exist(__test&i._arm14)) and _N_>0 %then %do; 
		data __test&i._arm14 ;
		set __test&i._arm14 (keep= _RRC1_ L_RRC1 U_RRC1); 
		meddraterm="&&labelTERM&i.";
		&trt.=4;
		run;
		%end;
	%END;
	%END;

		proc sort data=__plot2_2; by meddraterm; run;

		data __plot2_2;
		merge __plot2_2 
			  %if %sysfunc(exist(__test&i._arm12)) and _N_>0 %then  __test&i._arm12; 
			  %if %sysfunc(exist(__test&i._arm13)) and _N_>0 %then  __test&i._arm13; 
			  %if %sysfunc(exist(__test&i._arm14)) and _N_>0 %then  __test&i._arm14; 
		;
		by meddraterm &trt.; 
		run;

	

		/**Delete unwanted datasets**/
		proc datasets library=work nolist;
		delete __test: __data:;
		quit;
		run;
%end;
%mend;
%relrisk;
%END;

proc sort data=__plot2_2; by MEDDRATERM; run;

/*By default, Draw graph if more than 5% incidence for severe and 10% incidence for all (because size of graph)
 Otherwise, use the treshold defined by the user*/
data __plot2_4;
set __plot2_2;
by meddraterm;
**If the user wants to present SAE, the treshold by default to display the graph
 (10% for ALL and 5% for severe) is too high. We need to set the treshold at 0 to reduce the change to have a empty graph**;
%IF %UPCASE(&SAE.)=Y %THEN %LET GrphCutoff=0;

%IF (%LENGTH(&GrphCutoff.))=0 %THEN %DO ;
	%IF %UPCASE(&Graph.)=ALL %THEN %DO;
		if &trt.=1 and PCT_GR_ALL>10.0 then keep=1; 
		if &trt.=2 and PCT_GR_ALL>10.0 then keep=1; 
		%if &totarm.=3 %then if &trt.=3 and PCT_GR_ALL>10.0 then keep=1; ;
		%if &totarm.=4 %then %do;
			if &trt.=3 and PCT_GR_ALL>10.0 then keep=1;
			if &trt.=4 and PCT_GR_ALL>10.0 then keep=1; 
		%end;
	%END;
	%IF %UPCASE(&Graph.)=SEVERE %THEN %DO;
		if &trt.=1 and PCT_GR_345>5.0 then keep=1; 
		if &trt.=2 and PCT_GR_345>5.0 then keep=1; 
		%if &totarm.=3 %then if &trt.=3 and PCT_GR_345>5.0 then keep=1; ;
		%if &totarm.=4 %then %do;
			if &trt.=3 and PCT_GR_345>5.0 then keep=1;
			if &trt.=4 and PCT_GR_345>5.0 then keep=1; 
		%end;
	%END;
%END;

%IF (%LENGTH(&GrphCutoff.))>0 %THEN %DO ;
	%IF %UPCASE(&Graph.)=ALL %THEN %DO;
		if &trt.=1 and PCT_GR_ALL>&GrphCutoff. then keep=1; 
		if &trt.=2 and PCT_GR_ALL>&GrphCutoff. then keep=1; 
		%if &totarm.=3 %then if &trt.=3 and PCT_GR_ALL>&GrphCutoff. then keep=1; ;
		%if &totarm.=4 %then %do;
			if &trt.=3 and PCT_GR_ALL>&GrphCutoff. then keep=1;
			if &trt.=4 and PCT_GR_ALL>&GrphCutoff. then keep=1; 
		%end;
	%END;
	%IF %UPCASE(&Graph.)=SEVERE %THEN %DO;
		if &trt.=1 and PCT_GR_345>&GrphCutoff. then keep=1; 
		if &trt.=2 and PCT_GR_345>&GrphCutoff. then keep=1; 
		%if &totarm.=3 %then if &trt.=3 and PCT_GR_345>&GrphCutoff. then keep=1; ;
		%if &totarm.=4 %then %do;
			if &trt.=3 and PCT_GR_345>&GrphCutoff. then keep=1;
			if &trt.=4 and PCT_GR_345>&GrphCutoff. then keep=1; 
		%end;
	%END;
%END;
run;

proc sort data=__plot2_4 (where=(keep=1)) out=__More5P_plot2(keep=MEDDRATERM ) ; by MEDDRATERM; run;
%sort(data=__plot2_4,var=MEDDRATERM) ;

data __plot2_4;
merge __plot2_4 __More5P_plot2 (in=a);
by MEDDRATERM;
if a;
drop keep;
run;

%IF (%length(&PCT_FOREST)>0) and &totarm.>1 %THEN %DO;
	data __plot2_4;
	set __plot2_4;
			if &trt.=1 then do; RR1=_RRC1_; L_RR1=L_RRC1; U_RR1=U_RRC1; end;
			if &trt.=2 then do; RR2=_RRC1_; L_RR2=L_RRC1; U_RR2=U_RRC1; end;
		%IF &totarm.=3 %THEN %DO;
			if &trt.=3 then do; RR3=_RRC1_; L_RR3=L_RRC1; U_RR3=U_RRC1; end;
		%END;
		%IF &totarm.=4 %THEN %DO;
			if &trt.=3 then do; RR3=_RRC1_; L_RR3=L_RRC1; U_RR3=U_RRC1; end;
			if &trt.=4 then do; RR4=_RRC1_; L_RR4=L_RRC1; U_RR4=U_RRC1; end;
		%end;
		run;
%END;

/*PDF maximum width does not allow to present clearly data when MEDDRATERM is too long => reduce max lenght for meddraterm*/

data __plot2_5;
length Short_first $3. Short_second $4. Short_medra $200.;
set __plot2_4;
SOC_short=substr(SOC2,1,3);
if INDEX(MEDDRATERM,"other")>0 then MEDDRATERM=COMPRESS(LEFT(SOC_short))||" - "||"Other";
Length_medra=length(MEDDRATERM);
Short_medra=MEDDRATERM;
if Length_medra>30 then do;
	Short_first=(COMPRESS(LEFT(substr(scan(MEDDRATERM,1),1,3))));
	short_first2=Short_first||".";

	Short_second=(COMPRESS(LEFT(substr(scan(MEDDRATERM,2),1,4))));
	short_second2=Short_second||".";

	Short_medra1=TRANWRD(MEDDRATERM,(scan(MEDDRATERM,1)),short_first2);
	Short_Medra=TRANWRD(Short_medra1,(scan(MEDDRATERM,2)),short_second2);
end;
run;


proc sort data=__plot2_5;
by descending SOC2;
run;

**Create the max for XAXIS**;
proc sql noprint;
	select %if %UPCASE(&graph.)=SEVERE %then  max(PCT_GR_345) into :Upper2;
              %if %UPCASE(&graph.)=ALL %then max (PCT_GR_ALL) into :Upper2;
	from __plot2_5;
quit;
%put &Upper2;

%LET UpperAxis=%SYSEVALF(&Upper2+10);
%put &UpperAxis.;

/*DEFINE A TEMPLATE FOR APPEARANCE OF THE DOT IN THE SCATTER PLOT*/
%IF &colour.=N %THEN %DO;
	%LET mediumblue=grey;
	%LET blue=grey;
%END;

proc template;
  define style trt; 
    parent=styles.listing;
    style GraphData1 /
      ContrastColor=&reference.
      Color=&reference.
      MarkerSymbol="CircleFilled"
      Linestyle=1;
    style GraphData2 /
      ContrastColor=&darkblue.
      Color=&darkblue.
      MarkerSymbol="TriangleFilled"
      Linestyle=1;
	style GraphData3 /
      ContrastColor=&mediumblue.
      Color=&mediumblue.
      MarkerSymbol="SquareFilled"
      Linestyle=1;
    style GraphData4 /
      ContrastColor=&blue.
      Color=&blue.
      MarkerSymbol="DiamondFilled"
      Linestyle=1;
   end;
run;

/*In case of filter on treatment arm, do not display arm with 0 pct */
%IF %INDEX(&where.,%str(&trt.))>0 %then %Do;
data __plot2_5;
set __plot2_5;
	%IF &show1.=N %then %do; if &trt.=1 then delete;%end;
	%IF &show2.=N %then %do; if &trt.=2 then delete;%end;
	%IF &show3.=N %then %do; if &trt.=3 then delete;%end;
	%IF &show4.=N %then %do; if &trt.=4 then delete;%end;
run;
%END;

proc sql noprint;
select count(distinct(&trt.)) into: num_trt
from __plot2_5;
quit;

*Change size of symbol according to the display*;
%IF %UPCASE(&DISPLAY.)=PDF %THEN %LET size=4pt;;
%IF %UPCASE(&DISPLAY.)^=PDF %THEN %LET size=6pt;;


/*If the user only want to present the percentages of AEs without the forest: parmater=pct_plot*/
%IF (%length(&pct_plot)>0) %THEN %DO;

	/*Adapt the Titles to the situation*/
	%IF &totarm.>1 %THEN %DO;
	%IF (%LENGTH(&GrphCutoff.))=0 %THEN %DO ;
	%if %UPCASE(&graph.)=ALL %then title "Frequency of clinical adverse events (All Grades. AE representing 10% incidence in at least one arm)";;
	%if %UPCASE(&graph.)=SEVERE %then title "Frequency of clinical adverse events (Grade ^{unicode '2265'x}3. AE representing 5% incidence in at least one arm)";;
	%END;
	%END;

	%IF &totarm.=1 %THEN %DO;
	%IF (%LENGTH(&GrphCutoff.))=0 %THEN %DO ;
	%if %UPCASE(&graph.)=ALL %then title "Frequency of clinical adverse events (All Grades. AE representing 10% incidence)";;
	%if %UPCASE(&graph.)=SEVERE %then title "Frequency of clinical adverse events (Grade ^{unicode '2265'x}3. AE representing 5% incidence)";;
	%END;
	%END;

	%IF (%LENGTH(&GrphCutoff.))>0 %THEN %DO ;
		%IF &GrphCutoff. ^= 0 %THEN %DO;
		%if %UPCASE(&graph.)=ALL %then title "Frequency of clinical adverse events (All Grades. AE representing &GrphCutoff.% incidence in at least one arm)";;
		%if %UPCASE(&graph.)=SEVERE %then title "Frequency of clinical adverse events (Grade ^{unicode '2265'x}3. AE representing &GrphCutoff.% incidence in at least one arm)";;
		%END;
	%END;
	%IF (%LENGTH(&GrphCutoff.))>0 %THEN %DO ;
		%IF &GrphCutoff.= 0 %THEN %DO;
		%if %UPCASE(&graph.)=ALL %then title "Frequency of clinical adverse events, by treatment (All Grades.)";;
		%if %UPCASE(&graph.)=SEVERE %then title "Frequency of clinical adverse events, by treatment (Grade ^{unicode '2265'x}3.)";;
		%END;
	%END;


	%IF (&related.=Y and &SAE.=N and &recovered.=N) %THEN %DO ; title2 "AE's related to treatment.";%END;
	%IF (&related.=N and &SAE.=Y and &recovered.=N) %THEN %DO ; title "Frequency of Serious Adverse Events, by treatment.";%END;
	%IF (&related.=N and &SAE.=N and &recovered.=Y) %THEN %DO ; title2 "AE's recovered or recovered with sequelae";%END;
	%IF (&related.=Y and &SAE.=Y and &recovered.=N) %THEN %DO ; title "Frequency of Serious Adverse Events related to treatment, by treatment.";%END;
	%IF (&related.=Y and &SAE.=N and &recovered.=Y) %THEN %DO ; title2 "AE's related to treatment";
																title3 "AE's recovered or recovered with sequelae";%END;
	%IF (&related.=N and &SAE.=Y and &recovered.=Y) %THEN %DO ; title "Frequency of Serious Adverse Events, by treatment.";
																title2 "SAE's recovered or recovered with sequelae";%END;
	%IF (&related.=Y and &SAE.=Y and &recovered.=Y) %THEN %DO ; title "Frequency of Serious Adverse Events related to treatment, by treatment.";
																title2 "SAE's recovered or recovered with sequelae";%END;
	%IF (%length(&where.)>1) %THEN %DO; Title4 "Where &where.";%END;

%let height=26;
%let width=20;
ods _ALL_ close; 

%IF %UPCASE(&DISPLAY.)=PDF %THEN %DO ;
%let height=32;
option orientation=portrait;
ods pdf file="&pct_plot." style=trt %if &editGrph.=Y %then sge=on;;
%END;

	ods graphics on/ reset reset=index border=off width=&width. cm height=&height. cm outputfmt=&imagefmt. imagename="&imagename5.";

%IF %UPCASE(&DISPLAY.)^=PDF %THEN %DO ;
ods listing gpath="&gpath3." style=trt %if &editGrph.=Y %then sge=on;;
%END;


%if %UPCASE(&graph.)=ALL %then %do;
proc sgplot data=__plot2_5;
Scatter X=PCT_GR_ALL Y=Short_medra / markerattrs=(size=&size.) Group=&trt. ;
xaxis label="Percent" VALUES=( 0 to &UpperAxis. by 5.0);
yaxis label="Adverse event" grid;
format &trt. &trtformat.;
run;
%end;
%if %UPCASE(&graph.)=SEVERE %then %do;
proc sgplot data=__plot2_5;
Scatter X=PCT_GR_345 Y=MEDDRATERM / markerattrs=(size=&size.) Group=&trt. ;
xaxis label="Percent" VALUES=( 0 to &UpperAxis. by 5.0);
yaxis label="Adverse event" grid;
format &trt. &trtformat.;
run;
%end;

%IF %UPCASE(&DISPLAY.)^=PDF %THEN %DO ;
ods listing gpath=none;
ods graphics off;
%END;
%IF %UPCASE(&DISPLAY.)=PDF %THEN %DO ;
ods pdf close;
ods listing;
%END;

%END;
title;

**Before plotting the Forest plot, make sure that all XAXIS will have the same range to be comparable**;

%IF (%length(&pct_forest.)>0) and &totarm.>1 %THEN %DO;
proc sql noprint;
select case
		when max(U_RRC1) >= 30.0 then 'Y'
		else 'N'
		end into :axisUpper
	from __plot2_5;
quit;

%put &axisUpper.;

/**If the user want to present the Forest plot as well ==> Put both graphs in one picture**/;

*Calculate IC for label of axis*;
%LET ALPHA=&alpha.;
%LET IC=%SYSEVALF(100-(&alpha.*100));
%put &IC;

proc template;
define statgraph aeplot;
begingraph;
/*Adapt the Titles to the situation*/
%IF (%LENGTH(&GrphCutoff.))=0 %THEN %DO ;
%if %UPCASE(&graph.)=ALL %then entrytitle "Frequency of clinical adverse events and relative risk plot (All Grades. AE representing 10% incidence in at least one arm)";;
%if %UPCASE(&graph.)=SEVERE %then entrytitle "Frequency of clinical adverse events and relative risk plot (Grade ^{unicode '2265'x}3. AE representing 5% incidence in at least one arm)";;
%END;
%IF (%LENGTH(&GrphCutoff.))>0 %THEN %DO ;
	%IF &GrphCutoff. ^= 0.0 %THEN %DO;
	%if %UPCASE(&graph.)=ALL %then entrytitle "Frequency of clinical adverse events and relative risk plot (All Grades. AE representing at least &GrphCutoff.% incidence)";;
	%if %UPCASE(&graph.)=SEVERE %then entrytitle "Frequency of clinical adverse events and relative risk plot (Grade ^{unicode '2265'x}3. AE representing at least &GrphCutoff. % incidence)";;
	%END;
%END;
%IF (%LENGTH(&GrphCutoff.))>0 %THEN %DO ;
	%IF &GrphCutoff.= 0.0 %THEN %DO;
	%if %UPCASE(&graph.)=ALL %then entrytitle "Frequency of clinical adverse events and relative risk plot (All Grades.)";;
	%if %UPCASE(&graph.)=SEVERE %then entrytitle "Frequency of clinical adverse events and relative risk plot (Grade ^{unicode '2265'x}3.)";;
	%END;
%END;


%IF (&related.=Y and &SAE.=N and &recovered.=N) %THEN %DO ; entrytitle "AE's related to treatment.";%END;
%IF (&related.=N and &SAE.=Y and &recovered.=N) %THEN %DO ; entrytitle "Serious Adverse Events.";%END;
%IF (&related.=N and &SAE.=N and &recovered.=Y) %THEN %DO ; entrytitle "AE's recovered or recovered with sequelae";%END;
%IF (&related.=Y and &SAE.=Y and &recovered.=N) %THEN %DO ; entrytitle "Serious Adverse Events related to treatment.";%END;
%IF (&related.=Y and &SAE.=N and &recovered.=Y) %THEN %DO ; entrytitle "AE's related to treatment";
															entrytitle "AE's recovered or recovered with sequelae";%END;
%IF (&related.=N and &SAE.=Y and &recovered.=Y) %THEN %DO ; entrytitle "Serious Adverse Events.";
															entrytitle "SAE's recovered or recovered with sequelae";%END;
%IF (&related.=Y and &SAE.=Y and &recovered.=Y) %THEN %DO ; entrytitle "Serious Adverse Events related to treatment.";
															entrytitle "SAE's recovered or recovered with sequelae";%END;
%IF (%length(&where.)>1) %THEN %DO; entrytitle "Where &where.";%END;


**Define number of columns according the number of treatment arms**;
%IF %INDEX(&where.,%str(&trt.))=0 %THEN %DO;
%if &totarm.=2 %then layout lattice / columns=2 rows=1 columnweights=(.65 .35);;
%if &totarm.=3 %then layout lattice / columns=3 rows=1 columnweights=(.56 .22 .22 );;
%if &totarm.=4 %then layout lattice / columns=4 rows=1 columnweights=(.46 .18 .18 .18);;
%END;

%IF %INDEX(&where.,%str(&trt.))>0 %THEN %DO;
%if &totarm.=3 %then %do;
	%if (&show1.=Y and &show2.=Y and &show3.=N) or (&show1.=Y and &show3.=Y and &show2.=N) or
		(&show2.=Y and &show3.=Y and &show1.=N) %then layout lattice / columns=2 rows=1 columnweights=(.65 .35);;
%end;
%if &totarm.=4 %then %do;
	%if (&show1.=Y and &show2.=Y and &show3.=N and &show4.=N) or (&show1.=Y and &show3.=Y and &show2.=N and &show4.=N) or
		(&show1.=Y and &show4.=Y and &show2.=N and &show3.=N) or (&show2.=Y and &show3.=Y and &show1.=N and &show4.=N) or 
		(&show2.=Y and &show4.=Y and &show3.=N and &show1.=N) or (&show2.=N and &show4.=Y and &show3.=Y and &show1.=N) 
	%then layout lattice / columns=2 rows=1 columnweights=(.65 .35);;

	%if (&show1.=Y and &show2.=Y and &show3.=Y and &show4.=N) or (&show1.=Y and &show3.=N and &show2.=Y and &show4.=Y) or
		(&show1.=Y and &show4.=Y and &show2.=N and &show3.=Y) or (&show2.=Y and &show3.=Y and &show1.=N and &show4.=Y) 
	%then layout lattice / columns=3 rows=1 columnweights=(.56 .22 .22 );;
%end;
%END;

*** COLUMN 1: Presenting the percentages***;
layout overlay / xaxisopts=(label="Percent" type=linear) yaxisopts=(griddisplay=on display=(ticks tickvalues line) type=discrete);
%if %UPCASE(&graph.)=ALL %then %do;
ScatterPlot X=PCT_GR_ALL Y=Short_medra / markerattrs=(size=&size. transparency=0.4) Group=&trt. NAME="SCATTER1";
%end;
%if %UPCASE(&graph.)=SEVERE %then %do;
ScatterPlot X=PCT_GR_345 Y=MEDDRATERM / markerattrs=(size=&size. transparency=0.4) Group=&trt. NAME="SCATTER1";
%end;
endlayout;

*** COLUMN 2: Presenting the RR for control arm verus arm2***;
%IF %INDEX(&where.,%str(&trt.))=0 or &show2.=Y %THEN %DO;
layout overlay / xaxisopts=(%if &totarm.<4 %then Label="RR (&IC.% CI)"; %else Label="  "; type=log 
							%if  &axisUpper.=Y %then logopts=(TICKVALUEPRIORITY=TRUE tickvaluelist=(.1 1 10 100));)
				 yaxisopts=( griddisplay=on display=none);
ScatterPlot X=RR2 Y=MEDDRATERM / XErrorUpper=U_RR2 XErrorLower=L_RR2 
								 errorbarattrs=(color=black thickness=1.9 ) markerattrs=(symbol=TriangleFilled color=&darkblue. size=9)
								 NAME="SCATTER2";
referenceLine x=1/ lineattrs=(color=&reference.) clip=true ;
endlayout;
%END;

%IF &totarm.>2 %THEN %DO;
%IF %INDEX(&where.,%str(&trt.))=0 or &show3.=Y %THEN %DO;

*** COLUMN 3: Presenting the RR for control arm verus arm3***;
layout overlay / xaxisopts=(%if &totarm.=4 %then Label="RR (&IC% CI)"; %else label="  "; type=log
							%if  &axisUpper.=Y %then logopts=(TICKVALUEPRIORITY=TRUE tickvaluelist=(.1 1 10 100));)
				 yaxisopts=( griddisplay=on display=none);
ScatterPlot X=RR3 Y=MEDDRATERM /  primary=true XErrorUpper=U_RR3 XErrorLower=L_RR3 primary=true
								 errorbarattrs=(color=black thickness=1.9 ) markerattrs=(%if &num_trt.>2 %then symbol=SquareFilled color=&mediumblue.;
														 								 %if &totarm.>2 and &num_trt.=2 %then symbol=TriangleFilled color=&darkblue.;
																						 %if &totarm.=4 and &num_trt.=3 %then symbol=TriangleFilled color=&darkblue.;
																						  size=9)
								 NAME="SCATTER3";
referenceLine x=1/ lineattrs=(color=&reference.) clip=true ;
endlayout;
%END;
%END;

%IF &totarm.=4 %THEN %DO;
%IF %INDEX(&where.,%str(&trt.))=0 or &show4.=Y %THEN %DO;
** COLUMN 4: Presenting the RR for control arm verus arm4***;
layout overlay / xaxisopts=(Label="  " type=log %if &axisUpper.=Y %then logopts=(TICKVALUEPRIORITY=TRUE tickvaluelist=(.1 1 10 100));)
				 yaxisopts=( griddisplay=on display=none);
ScatterPlot X=RR4 Y=MEDDRATERM / XErrorUpper=U_RR4 XErrorLower=L_RR4 
								 errorbarattrs=(color=black thickness=1.9) markerattrs=(%if &num_trt.=4 %then symbol=DiamondFilled color=&blue.;
																						%if &totarm.>2 and &num_trt.=2 %then symbol=TriangleFilled color=&darkblue.;
																						%if &totarm.=4 and &num_trt.=3 %then symbol=SquareFilled color=&mediumblue.;
																						 size=9)
								NAME="SCATTER4";
referenceLine x=1/ lineattrs=(color=&reference.) clip=true ;
endlayout;
%END;
%END;

endlayout; **lattice;

layout globalLegend / type=row title="Treatment";
        discretelegend "SCATTER1" / ;
      endLayout;

endgraph;
end;
run;


*** sort the input dataset and submit PROC SGRENDER to create the graph;
proc sort data=__plot2_5;
by descending SOC2;
run;

%let height=26;
%let width=26;
ods _ALL_ close; 

%IF %UPCASE(&DISPLAY.)=PDF %THEN %DO ;
	%LET height=32;
	%let width=19.72733; *Max for pdf*;
	%IF (%LENGTH(&GrphCutoff.)>0) and %UPCASE(&Graph.)=ALL %then %let height=40;;
	option orientation=portrait nodate;
	ods pdf file="&pct_forest." style=trt %if &editGrph.=Y %then sge=on;;
%END;

	ods graphics on/ reset reset=index border=off maxlegendarea=&maxlegendarea. width=&width. cm height=&height. cm 
					  outputfmt=&imagefmt. imagename="&imagename3.";

%IF %UPCASE(&DISPLAY.)^=PDF %THEN %DO ;
ods listing gpath="&gpath2." style=trt %if &editGrph.=Y %then sge=on;;
%END;


proc sgrender data=__plot2_5 template=aeplot ;
format &trt. &trtformat.;
run;
quit;

%IF %UPCASE(&DISPLAY.)^=PDF %THEN %DO ;
ods listing gpath=none;
ods graphics off;
%END;
%IF %UPCASE(&DISPLAY.)=PDF %THEN %DO ;
ods pdf close;
ods listing;
%END;

%END;
%END;
%END;
title;

/************************************
 ***** Graph Number 5: Pyramid ******
*************************************/
%IF (%length(&pyramid.)>0) and &totarm.=2 %THEN %DO;

data __Plot5 (keep=&trt. MEDDRATERM SOC2 PCT_GR_345 PCT_GR_all keep:) ;
set __sumall;
if MEDDRATERM ne "*GENERAL TERM";
%IF (%LENGTH(&GrphCutoff.))=0 %THEN %DO; **By default, present in the graph only AE with more than 5percent incidence*;
			if &trt.=1 then do;
			if PCT_GR_345>5.0 then keepSevere=1;
			if PCT_GR_ALL>10.0 then keepAll=1;
		end;
		if &trt.=2 then do;
			if PCT_GR_345>5.0 then keepSevere=1;
			if PCT_GR_ALL>10.0 then keepAll=1;
		end;
%END;

%IF (%LENGTH(&GrphCutoff.))>0 %THEN %DO; **Else, use the same treshold than the one defined by the user**;
			if &trt.=1 then do;
			if PCT_GR_345>&GrphCutoff. then keepSevere=1;
			if PCT_GR_ALL>&GrphCutoff. then keepAll=1;
		end;
		if &trt.=2 then do;
			if PCT_GR_345>&GrphCutoff. then keepSevere=1;
			if PCT_GR_ALL>&GrphCutoff. then keepAll=1;
		end;
%END;
run;

%IF %UPCASE(&graph.)=SEVERE %then %DO;
proc sort data=__Plot5 (where=(keepSevere=1)) out=__More5P(keep=MEDDRATERM ) nodupkey ; by MEDDRATERM; run;
%END;
%IF %UPCASE(&graph.)=ALL %then %DO;
proc sort data=__Plot5 (where=(keepAll=1)) out=__More5P(keep=MEDDRATERM ) nodupkey ; by MEDDRATERM; run;
%END;

%sort(data=__Plot5,var=MEDDRATERM) ;

data __plot5;
merge __Plot5 __More5P (in=a);
by MEDDRATERM;
if a;
run;


proc sql;
create table __Plot5_1 as
	select*, %if %UPCASE(&graph.)=SEVERE %then max(PCT_GR_345) as maxpct; 
			 %if %UPCASE(&graph.)=ALL %then max(PCT_GR_all) as maxpct; 
	from __Plot5;
quit;


proc sort data=__Plot5_1 nodup; by &trt.; run;

data __Plot5_1;
length Short_first $3. Short_second $4.;
set __Plot5_1;
%if %UPCASE(&graph.)=SEVERE %then %do; 
	if &trt.=1 then PCT_GR_345=PCT_GR_345 * -1; 
	if &trt.=1 then PCTSeveretrt1=PCT_GR_345;
	if &trt.=2 then PCTSeveretrt2=PCT_GR_345;
%end;

%if %UPCASE(&graph.)=ALL %then %do; 
	if &trt.=1 then PCT_GR_ALL=PCT_GR_ALL * -1; 
	if &trt.=1 then PCTAlltrt1=PCT_GR_ALL;
	if &trt.=2 then PCTAlltrt2=PCT_GR_ALL;
%end;
maxaxisUpper=ROUND (maxpct,10.0)+10;
maxaxislower=maxaxisUpper * -1;
MEDDRATERM=propcase(MEDDRATERM);
if INDEX(UPCASE(MEDDRATERM),'OTHER')>0 then MEDDRATERM="Other";
Length_medra=length(MEDDRATERM);
Short_medra=MEDDRATERM;
if Length_medra>30 then do;
	Short_first=(COMPRESS(LEFT(substr(scan(MEDDRATERM,1),1,3))));
	short_first2=Short_first||".";

	Short_second=(COMPRESS(LEFT(substr(scan(MEDDRATERM,2),1,4))));
	short_second2=Short_second||".";

	Short_medra1=TRANWRD(MEDDRATERM,(scan(MEDDRATERM,1)),short_first2);
	Short_Medra=TRANWRD(Short_medra1,(scan(MEDDRATERM,2)),short_second2);
end;
SOC_short=substr(SOC2,1,3);
axis=trim(left(SOC_short))||' - '||trim(left(Short_Medra));
format &trt. &trtformat.;
run;

data _NULL_;
set __Plot5_1;
call symput ("Upper",maxaxisUpper);
call symput ("lower",maxaxislower);
run;
%put &Upper. &lower.;


proc format;
   picture positive 
     low-<0='000'
     0<-high='000';
run;

proc sort data=__Plot5_1; by axis; run;

**Make the graph**;

%if %UPCASE(&graph.)=ALL %then %do;
	%let width=20;
	%let height=26;  
%end;

%if %UPCASE(&graph.)=SEVERE %then %do;
	%let width=20;
	%let height=24;  
%end;

ODS _ALL_ CLOSE;
ods listing;

%IF %UPCASE(&DISPLAY.)=PDF %THEN %DO ;
option orientation=portrait nodate nonumber ;
ods pdf file="&pyramid." %if &editGrph.=Y %then sge=on;;
%END;

	ods graphics on/ reset reset=index border=off width=&width. cm height=&height. cm outputfmt=&imagefmt. imagename="&imagename9.";

%IF %UPCASE(&DISPLAY.)^=PDF %THEN %DO ;
ods listing gpath="&gpath5." %if &editGrph.=Y %then sge=on;;
%END;

title 'Frequency of clinical adverse events, by treatment';
	%IF (%LENGTH(&GrphCutoff.))=0 %THEN %DO ;
		%if %UPCASE(&graph.)=SEVERE %then %do; title2 "Representing 5% incidence in at least one arm (Grades 3-5)"; %end;
		%if %UPCASE(&graph.)=ALL %then %do; title2 "Representing 10% incidence in at least one arm (All grades)"; %end;
	%END;

	%IF (%LENGTH(&GrphCutoff.))>0 %THEN %DO ;
		%IF &GrphCutoff. ^=0 %THEN %DO;
			%if %UPCASE(&graph.)=SEVERE %then %do; title2 "Representing &GrphCutoff.% incidence in at least one arm (Grades 3-5)"; %end;
			%if %UPCASE(&graph.)=ALL %then %do; title2 "Representing &GrphCutoff.% incidence in at least one arm (All grades)"; %end;
		%END;
	%END;

	%IF (&related.=Y and &SAE.=N and &recovered.=N) %THEN %DO ; Title3 "Related Adverse events.";%END;
	%IF (&related.=N and &SAE.=Y and &recovered.=N) %THEN %DO ; Title "Frequency of clinical Serious Adverse Events (SAE), by treatment";%END;
	%IF (&related.=N and &SAE.=N and &recovered.=Y) %THEN %DO ; Title3 "AE's recovered or recovered with sequelae";%END;
	%IF (&related.=Y and &SAE.=Y and &recovered.=N) %THEN %DO ; Title "Frequency of clinical Serious Adverse Events (SAE) related to treatment.";%END;
	%IF (&related.=Y and &SAE.=N and &recovered.=Y) %THEN %DO ; Title3 "Related Adverse events.";
																Title4 "AE's recovered or recovered with sequelae";%END;
	%IF (&related.=N and &SAE.=Y and &recovered.=Y) %THEN %DO ; Title "Frequency of clinical Serious Adverse Events (SAE).";
																Title3 "SAE's recovered or recovered with sequelae";%END;
	%IF (&related.=Y and &SAE.=Y and &recovered.=Y) %THEN %DO ; Title "Frequency of clinical Serious Adverse Events (SAE) related to treatment.";
																Title3 "SAE's recovered or recovered with sequelae";%END;
	%IF (%length(&where.)>1) %THEN %DO; Title5 "Where &where.";%END;

proc sgplot data=__Plot5_1;
   %if %UPCASE(&graph.)=SEVERE %then format PCTSeveretrt1 PCTSeveretrt2 positive. &trt. &trtformat;
   %if %UPCASE(&graph.)=ALL %then format PCTalltrt1 PCTalltrt2 positive. &trt. &trtformat;
;
   hbar axis / %if %UPCASE(&graph.)=SEVERE %then response=PCTSeveretrt1;
   			   %if %UPCASE(&graph.)=ALL %then response=PCTalltrt1;
   			fillattrs=graphdata1 (color=&reference.) dataskin=pressed
        	legendlabel="&trt1" name="&trt1";

 	hbar axis / %if %UPCASE(&graph.)=SEVERE %then response=PCTSeveretrt2;
				%if %UPCASE(&graph.)=ALL %then response=PCTalltrt2;
       			fillattrs=graphdata2 (color=&darkblue.) dataskin=pressed
		        legendlabel="&trt2" name="&trt2";
 
 xaxis label="Percent" grid VALUES=( &lower. to &Upper. by 10);
 yaxis label="SOC and Adverse event" discreteorder=data 
  %if %UPCASE(&graph.)=ALL and (%LENGTH(&GrphCutoff.))>0 %then valueattrs=(size=8pt); 
 ;
run;
title;

%IF %UPCASE(&DISPLAY.)^=PDF %THEN %DO ;
ods listing gpath=none;
ods graphics off;
%END;

%IF %UPCASE(&DISPLAY.)=PDF %THEN %DO ;
ods pdf close;
ods listing;
%END;

%END;

/**removing unwanted datasets**/
proc datasets library=work nolist;
	delete __patient2 ALLSOCv4 __patient3 __contents __formAEmerge __AE_OTHERS __nomatchCRF_DICTIONNARY
			__formAEclean list_of_meddraterm_in_this_study list_of_SOC_in_this_study __code_SOC_PT __code_SOC
			___code_SOC_PT ___code_SOC_PT __WORST_PT __WORST_SOC ___AE ___pat ___AEv2 __GR1 __GR2 __GR3 __GR4 __GR5
			__GR12 __GR34 __GR345 __GRAll __Grade1 __Grade11 __Grade21 __Grade31 __Grade41 __Grade1all __Grade2
		 	__Grade12 __Grade22 __Grade32 __Grade42 __Grade2all __Grade3 __Grade13 __Grade23 __Grade33 __Grade43
			__Grade3all __Grade4 __Grade14 __Grade24 __Grade34 __Grade44 __Grade4all __Grade5 __Grade15 __Grade25
			__Grade35 __Grade45 __Grade5all __Grade12 __Grade112 __Grade212 __Grade312 __Grade412 __Grade12all
			__Grade34 __Grade134 __Grade234 __Grade334 __Grade434 __Grade34all __Grade345 __Grade1345 __Grade2345
			__Grade3345 __Grade4345 __Grade345all __GradeAll __Grade1all_ __Grade2all_ __Grade3all_ __Grade4all_
			__ALLGrade __totalpatient __sumall __summall_1 __MoreTreshold __table_ae __arm1 __arm2 __arm3 __arm4
			__formats __table_final __countPat __plot1_1 __plot1_2 __plot1_3 __plot1_4 /*__sumall_SOC*/ __plot3
			__Plot3_1 __Plot3_2 __graph __graph_arm1 __graph_arm2 __graph_arm3 __graph_arm4 __graph_flag
			__create_arm1 __create_arm2 __create_arm3 __create_arm4 __graph_all __plot2 __plot2_2 __plot2_3
			__plot2_4 __More5P_plot2 __plot2_5 __Plot5 __More5P __Plot5_1 __data2;
quit;
run;





%mend AE_table;


