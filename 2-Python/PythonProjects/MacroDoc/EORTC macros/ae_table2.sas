
/******************************************************************************************************* 
 * 
 * AE_TABLE.SAS
 * -------------
 *
 * This program is a macro to compute a tables presenting AE, relatives AE and SAE
 * 				and appropriate graphs (3 graphs)
 * 
 * Version date: 
 *       version 1: 6/9/2016
 *       version 2: 10/08/2017
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
 * JAN 2018,GIS:
 * 		1-  Add a defensive programming (%validator_v2) to check that the treatment variable is coded from 1 to 4.
 * 			The variable &trt. Should not start by 0.
 * 		2- Add some options to the table setting in order to get all arms on one page in case of COLUMN=ALL and with grade 5 in both arms
 * 
 * REVISIONS:
 *------------
 * June 2016	Each existing programs for TSR_AE was study specific 
 * 				==> To harmonize and automatized the reporting of AEs for medical review,TSR and stat reports
 * July 2017:
 *    - Add the possibility for the user to chose the title for the tables and the graphs
 *    - Replacement of a sql join by a SAS base merge to increase the rapidity of the code
 *    - Implementation of the macro %validator to ensure the correct use of the parameters and avoid non-sense results
 *    - Replacement of the pyramid graph by another pyramid graph presenting AEs by grade in each arms.
 *    - Add the possibility to present adverse events according to CTC3 classification
 *    - Merge the CTC dictionnary and the form_ae by SOC_short and not by meddraterm anymore to increase the recognized items.
 *    - Add the parameter EUDRACT= to create automatically the excel file for EUDRACT reporting.
 *    - Replace the POP= parameter into POPVAR= and POPVAL= to facilitate the use of this parameter.
 *    - Give the possibility to use the macro outside EORTC by using the K:\ or the C:\ directories
 *      depending on whether the user is connected to the EORTC server or not.
 *    - Changing the label of the first row of the table from "NUMBER OF PATIENTS WITH AE" into "MAXIMUM GRADE BY PATIENT".
 *    - Change the label of the first column of the table from "CTC+MeDdraTerm" into "System Organ Class + Preferred Term".
 * 
 * 15 December 2017: Change %include "K:\SAS\Extra EORTC Macros\validator_v2.sas"; by
 * 		     %include "C:\SAS\Extra EORTC Macros\validator_v2.sas"; to use the macro in remote
 ********************************************************************************************************
 * 1.0 REQUIRED PARAMETERS
 * 
 * DATAPATIENT    = Name of the PATIENT dataset. (Required parameter if EUDRACT=Y)
 *
 * DATA_AE		  =	Name of the dataset containing all AEs, the treatment variables, 
 *      			the flag for safety population and the period of interest.
 *
 * POPVAR      	  = Safety population variable 
 *
 * POPVAL	  	  = Value of parameter POPVAR for a patient included in the safety population.
 *
 * PERIODVAR	  = variable for Period of interest for the reporting of AEs 
 *
 * PERIODVAL	  = Value of parameter PERIODVAR. Add quotes if PERIODVAR is a character variable
 *					Example: PERIODVAL=2 (numeric) or PERIODVAL="Baseline" (character)
 *             
 * 2.0 OPTIONAL PARAMETERS
 * 		2.1 GLOBAL OPTIONS
 *
 * CTC		=	Version of CTC for the classification of Adverse Events (default=4)

 * TRT      = Treatment variable. Default: TRT = TRT1  (Required parameter)
 *
 * WHERE    = Logical condition to select records from the input dataset if not all records are to be taken into account. 
 *            (Optional parameter) Default: WHERE = 1, i.e. select all records.
 *
 * SAE 	     = Y if only serious adverse events are required (Optional parameter).
 *             Default: SAE = N, i.e. Serious adverse events are not excluded. Impacts the graphs.
 *
 * RELATED	 = Y to present only AEs related to treatment (Optional parameter).
 *		       Default: RELATED=N, i.e. AEs related to treatment are not excluded. Impacts the graphs.
 *
 * RECOVERED = Y to select AEs that are "recovered" or "recovered with sequelae". (Optional parameter) 
 *             Default: RECOVERED=N, i.e. AEs that are "recovered" or "recovered with sequelae" are not excluded. 
 *             Impacts the graphs.
 *
 *			2.1.1 EUDRACT RELATED OPTIONS
 * 
 * EUDRACT	= Y to create an excel file that can be used for EUDRACT reporting. (Optional parameter)
 * 			  Default=N
 * 
 * PVUFILE	= path and name of the Excel file sent by Pharmaco-vigilance unit (PVU) containing all the Serious Adverse Events (SAE)
 * 
 * SS	= Variable of survival status (0=Alive, 1=Dead)
 *
 * OUTPUTEUDRACT= path and name of the Excel file newly created for the reporting of AE.
 *
 *
 * 		2.2 TABLE OPTIONS
 *
 * TRESHOLD  = Filter for the TABLE only. Presents AEs reaching a certain percentage of incidence 
 *             (default: THRESHOLD=0.0, i.e. select all records). This parameter has a numeric format (1 digit after the coma) and can take any value between 0.0 and 100.0.
 * 		       (Optional parameter)
 *
 * COLUMN	= Parameter used for the layout of the table. (Optional parameter)
 *		      Values:
 *                    -	ALL	= if you want to present all grades separately for each treatment arm (Default: COLUMN=ALL)
 *                    -	SEVERE = if you want to present only severe AE (grade 3, grade 4, grade 5, grade>=3, grade>=1)
 *                    -	POOLED = if you want to present grades 1&2 pooled, grades 3&4 pooled,
 *                      grade 5 alone, grade>=3, grade>=1
 *                    -	SUMMARY = if you want to present only grade>=3 and grades >=1
 * DETAILS	 = Y to create three additional listings and checks. (Optional parameter)
 *                -	Number of patients having started treatment who did not observe any AE
 *                -	Listing of Others AE
 *                -	Listing of terms from AEMODIFY variable which do not fit with CTCAE_v4_0_Term
 *            (Default: DETAILS=Y)
 *
 * TITLETABLE	= User-defined title for the table (optional parameter). 
 * 				  Default value depends on other parameters (RELATED=, SAE=, RECOVERED, WHERE=, TRESHOLD, PERIOD=).
 *
 * ODS       = Y if ODS output is required (Optional parameter)
 *	           Default: ODS = Y, i.e. the results are presented in an RTF file.
 *
 * ODSFile   = Name of the ‘rtf’ file which is to contain the ODS output table (see note below) (Optional parameter)
 * 
 *
 *		2.3 PLOT OPTIONS
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
 * 			2.3.1  BAR CHART OPTIONS
 * SOCGRPH	  = Name of the output file for the graph presenting ‘Frequency of patients with clinical Advserse Events, by SOC’(Optional parameter). 
 *		        Default: SOCGRPH =(), i.e. no graph is displayed.
 * 
 * TITLESOCGRPH = User-defined title for the Graph by SOC (Frequency of AE by SOC, by treatment) (optional parameter). 
 * 				  Default value depends on other parameters (GRAPH=, RELATED=, SAE=, RECOVERED, WHERE=, ..).
 *
 * AXISORDER  = Parameter linked to the SOCGRPH graph (Optional parameter). Value:
 *                    -	AXISORDER=SOC: Sort the graph by SOC (Alphabetical order) 
 *                    -	AXISORDER=freq: Sort the graph by descending frequencies.
 *              Default: AXISORDER=freq 
 *
 *			2.3.2 PYRAMID PLOT OPTIONS
 * PYRAMID   = Name of the output file for the ‘Pyramid Plot (Frequency of clinical adverse events, by treatment’)
 *             (Optional parameter). Only available for studies with 2 treatment arms.
 *	           Default: PYRAMID=(), i.e. no graph is displayed.
 *
 * TITLEPYRAMID	= User-defined title for the ‘Pyramid Plot'. (optional parameter). 
 * 				  	 Default value depends on other parameters (GRAPH=, RELATED=, SAE=, RECOVERED, WHERE=, ..).
 * 
 *			2.3.3 SCATTER PLOT OPTIONS
 * PCT_PLOT  = Name of the output file for the scatter plot presenting 'Frequency of clinical adverse events by Treatment’ (Optional parameter)
 *	           Default: PCT_PLOT=(), i.e. no graph is displayed.
 * 
 * TITLEPCTPLOT	= User-defined title for the Scatter plot. (optional parameter). 
 * 				  Default value depends on other parameters (GRAPH=, RELATED=, SAE=, RECOVERED, WHERE=, ..).
 *
 *			2.3.4 FOREST PLOT OPTIONS
 * PCT_FOREST= Name of the output file to display the scatter plot presenting frequency of clinical adverse events by Treatment AND a forest plot presenting the
 *		        Relative Risks and their confidence intervals (Optional parameter)
 *		        Default: PCT_FOREST=(), i.e. no graph is displayed.
 * 
 * ALPHA     =   Alpha for CI around Relative Risks (in range 0.0001-0.999). This parameter is linked to the Forest plot (previous parameter).
 *			     Default: ALPHA = 0.05. (Optional parameter)
 *
 * TITLEFOREST	= User-defined title for the Forest plot of relative risk. (optional parameter). 
 * 				  Default value depends on other parameters (GRAPH=, RELATED=, SAE=, RECOVERED, WHERE=, ..).
 *
 *
 *		2.4 IMAGE CONTROLLING OPTIONS
 *
 * DISPLAY	 =   SASEMF, WORD, POWERPOINT, TIF, PNG or PDF. (Optional parameter)
 *				 Default:PNG for graphs
 *
 * EDITGRPH	 =	Y if you want to create a file .sge (SAS Graph Editor) for further manual 
 *                modifications. (Optional parameter) Default: EDITGRPH=N
 *
 * COLOUR  = 	Set to Y to obtain graph in colour, N for black and white. 
 *              Default: COLOUR=Y. (Optional parameter)
 *
 *
 * 
 ************************************************************************************************************************
 * 
 * USAGE
 * 
 * Examples of macro calls:
 * 
 * %AE_table (data_ae=formae, popvar=safetypop, popval=1, Related=Y, ODSFile=MyTable_REL.rtf, SOCGrph=SOCGrph_REL.png,   PCT_Plot=PCT_REL.png, 
 *			  Pct_Forest=Forest_REL.png, Pyramid=Pyramid_REL.png);
 *
 *
 * %table_AE (datapatient=patient, data_ae=formae, popvar=safetypop, popval="treated",trt=trt2, period=(period=2), 
 *			  where=, column=summary,Treshold=, SAE=, related=Y, recovered=, details=, ODS=, 
 *		      ODSFile=C:\Temp\MyTable.rtf, graph=all, TitleTable=Adverse events (safety population)
 *			  Pct_plot= C:\Temp\Percentage.pdf, TitlePctPlot=Scatter plot of AEs by treatment arm
 *			  Pct_Forest=C:\Temp\Forest_pct.pdf, alpha=0.10, display=PDF, colour=N);
 *
 *************************************************************************************************************************
 *
 * NOTES
 * 
 * --- PARAMETERS ----
 *
 * DATAPATIENT=, SS=, DATA_AE=, POPVAR=, and PERIOD= must be prepared by the user before running the macro
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
 *           ODS RTF FILE='C:\TEMP\TABLE.RTF' STARTPAGE=NO;
 *           %AE_table (data_ae=formae, popvar=safetypop, popval=1, Related=Y, ODSFile=MyTable_REL.rtf, SOCGrph=SOCGrph_REL.png,   PCT_Plot=PCT_REL.png, 
 *			  Pct_Forest=Forest_REL.png, Pyramid=Pyramid_REL.png);
 *
 *
 *          %table_AE (datapatient=patient, data_ae=formae, popvar=safetypop, popval="treated",trt=trt2, period=(period=2), 
 *			  where=, column=summary,Treshold=, SAE=, related=Y, recovered=, details=, ODS=, 
 *		      ODSFile=C:\Temp\MyTable.rtf, graph=all, TitleTable=Adverse events (safety population)
 *			  Pct_plot= C:\Temp\Percentage.pdf, TitlePctPlot=Scatter plot of AEs by treatment arm
 *			  Pct_Forest=C:\Temp\Forest_pct.pdf, alpha=0.10, display=PDF, colour=N);

 *           ODS RTF CLOSE;
 *
 *    To put each table in a separate file:
 *
 *           %AE_table (data_ae=formae, popvar=safetypop, popval=1, Related=Y, ODSFile=MyTable_REL.rtf, SOCGrph=SOCGrph_REL.png,   PCT_Plot=PCT_REL.png, 
 *			  Pct_Forest=Forest_REL.png, Pyramid=Pyramid_REL.png);
 *
 *
 *          %table_AE (datapatient=patient, data_ae=formae, popvar=safetypop, popval="treated",trt=trt2, period=(period=2), 
 *			  where=, column=summary,Treshold=, SAE=, related=Y, recovered=, details=, ODS=, 
 *		      ODSFile=C:\Temp\MyTable.rtf, graph=all, TitleTable=Adverse events (safety population)
 *			  Pct_plot= C:\Temp\Percentage.pdf, TitlePctPlot=Scatter plot of AEs by treatment arm
 *			  Pct_Forest=C:\Temp\Forest_pct.pdf, alpha=0.10, display=PDF, colour=N);
 *
 *
 *    If you do choose to open and close the ODS destinations yourself, please note:
 * 
 * 			- You cannont use a STYLE= option
 * 			- You need to precise option=landscape to get all arms on one page.
 * 			
 *
 *************************************************************************************************************************/


%macro AE_table2 (dataPatient=, data_AE=, CTC= , popvar=, popval=, trt=, periodvar=, periodval=,where=, SAE=, related=, recovered=,details=,
				 column=,Treshold=, TitleTable=, ODS=, ODSFile=, 
				 graph=,GrphCutoff=, 
				 SOCgrph=, TitleSOCgrph=, AxisOrder=, 
				 Pyramid=, TitlePyramid=, 
				 Pct_plot=, TitlePctPlot=,
				 Pct_Forest=,TitleForest=, alpha=, 
				 display=, EditGrph=,colour=, 
				 EUDRACT=, PVUFile=, ss=,outputEUDRACT=);

option nonotes;
**Setup parameters by default**;
%if (%length(&trt)=0) %then %let trt=trt1 ;
%if (%length(&where)=0) %then %let where=1 ;
%if (%length(&column)=0) %then %let column=ALL ; 
%if (%length(&SAE)=0) %then %let SAE=N ;
%if (%length(&related)=0) %then %let related=N ;
%if (%length(&recovered)=0) %then %let recovered=N ;
%if (%length(&details)=0) %then %let details=Y ;
%if (%length(&ODS)=0) %then %let ODS=Y ;
%if (%length(&AxisOrder)=0) %then %let AxisOrder=FREQ ;
%if (%length(&alpha)=0) %then %let alpha=0.05 ;
%if (%length(&EditGrph)=0) %then %let EditGrph=N ;
%if (%length(&Display)=0) %then %let Display=PNG ;
%if (%length(&colour)=0) %then %let colour=Y ;
%if (%length(&graph)=0) %then %let graph=N ;
%if (%length(&Treshold)=0) %then %let Treshold=0 ;
%if (%length(&EUDRACT)=0) %then %let EUDRACT=N;
%if (%length(&CTC)=0) %then %let CTC=4;

*Upcase all parameters*;
%let related=%UPCASE(&related);
%let details=%UPCASE(&details);
%let SAE=%UPCASE(&SAE);
%let colour=%UPCASE(&colour);
%let recovered=%UPCASE(&recovered);
%let ODS=%UPCASE(&ODS);
%let EditGrph=%UPCASE(&EditGrph);
%let display=%UPCASE(&display);
%let graph=%UPCASE(&graph);
%let AxisOrder=%UPCASE(&AxisOrder);
%let column=%UPCASE(&column);

* Present AE =>grade3 when SAE=Y*;
%if &SAE.=Y %then %let Graph=SEVERE;


/*==========================================================================
  ------------------------------------------------------------------------
   FOOL PROOF TESTING OF ALL PARAMETERS
  --------------------------------------------------------------------------
=============================================================================*/

%include "C:\SAS\Extra EORTC Macros\validator_v2.sas";

/*------------------------
  REQUIRED PARAMETERS
-------------------------*/

**DATA=**;
%validator (type=VALUE, parm=&data_AE, parm_name=data_AE, acceptable=, data_validator=, var=, required=Y, YN=);
	%if &_STOPMACRO=Y %then %goto stop_exec;
**POPVAR**;
%validator (type=VALUE, parm=&popvar, parm_name=popvar, acceptable=, data_validator=, var=, required=Y, YN=);
	%if &_STOPMACRO=Y %then %goto stop_exec;
**POPVAL**;
%validator (type=VALUE, parm=&popval, parm_name=popval, acceptable=, data_validator=, var=, required=Y, YN=);
	%if &_STOPMACRO=Y %then %goto stop_exec;
**DATAPATIENT=**;
%IF &EUDRACT=Y %then %do;
%validator (type=VALUE, parm=&datapatient, parm_name=datapatient, acceptable=, data_validator=, var=, required=Y, YN=);
	%if &_STOPMACRO=Y %then %goto stop_exec;
%END;	
/***PERIODVAR***/
%validator (type=VALUE, parm=&periodvar, parm_name=PERIODVAR, acceptable=, data_validator=, var=, required=Y, YN=);
	%if &_STOPMACRO=Y %then %goto stop_exec;
/***PERIODVAL***/
%validator (type=VALUE, parm=&periodval, parm_name=PERIODVAL, acceptable=, data_validator=, var=, required=Y, YN=);
	%if &_STOPMACRO=Y %then %goto stop_exec;

/*-------------------------------
   YES/NO PARAMETERS
--------------------------------*/

**SAE**;
%validator (type=VALUE, parm=&sae, parm_name=sae, acceptable=, data_validator=, var=, required=, YN=Y);
	%if &_STOPMACRO=Y %then %goto stop_exec;
**Related**;
%validator (type=VALUE, parm=&related, parm_name=related, acceptable=, data_validator=, var=, required=, YN=Y);
	%if &_STOPMACRO=Y %then %goto stop_exec;
**Recovered**;
%validator (type=VALUE, parm=&recovered, parm_name=recovered, acceptable=, data_validator=, var=, required=, YN=Y);
	%if &_STOPMACRO=Y %then %goto stop_exec;
**Details**;
%validator (type=VALUE, parm=&details, parm_name=details, acceptable=, data_validator=, var=, required=, YN=Y);
	%if &_STOPMACRO=Y %then %goto stop_exec;
**ODS**;
%validator (type=VALUE, parm=&ODS, parm_name=ODS, acceptable=, data_validator=, var=, required=, YN=Y);
	%if &_STOPMACRO=Y %then %goto stop_exec;
**EdithGrph**;
%validator (type=VALUE, parm=&EditGrph, parm_name=EditGrph, acceptable=, data_validator=, var=, required=, YN=Y);
	%if &_STOPMACRO=Y %then %goto stop_exec;
**Colour**;
%validator (type=VALUE, parm=&colour, parm_name=colour, acceptable=, data_validator=, var=, required=, YN=Y);
	%if &_STOPMACRO=Y %then %goto stop_exec;
**EUDRACT**;
%validator (type=VALUE, parm=&EUDRACT, parm_name=Eudract, acceptable=, data_validator=, var=, required=, YN=Y);
	%if &_STOPMACRO=Y %then %goto stop_exec;

/*----------------------------------------------
	PARAMETERS REFERING TO A DATASET
------------------------------------------------*/

**Valid SAS name**;
%validator (type=DATA, parm=&data_AE, parm_name=&data_AE, acceptable=, data_validator=&data_AE, var=, required=, YN=N);
	%if &_STOPMACRO=Y %then %goto stop_exec;
** SAS dataset exist**;
%validator (type=DATA, parm=&data_AE, parm_name=&data_AE, acceptable=, data_validator=&data_AE, var=, required=, YN=N);
	%if &_STOPMACRO=Y %then %goto stop_exec;
**SAS dataset is empty**;
%validator (type=DATA, parm=&data_AE, parm_name=&data_AE, acceptable=, data_validator=&data_AE, var=, required=, YN=N);
	%if &_STOPMACRO=Y %then %goto stop_exec;


/*-----------------------------------------------
	PARAMETERS REFERING TO A VARIABLE
---------------------------------------------------*/

**popvar**;
%validator (type=VAR, parm=&popvar, parm_name=POPVAR, acceptable=, data_validator=&data_AE, var=&popvar, required=, YN=N);
	%if &_STOPMACRO=Y %then %goto stop_exec;
**periodvar**;
%validator (type=VAR, parm=&periodvar, parm_name=PERIODVAR, acceptable=, data_validator=&data_AE, var=&periodvar, required=, YN=N);
	%if &_STOPMACRO=Y %then %goto stop_exec;
**trt**;
%validator (type=VAR, parm=&trt, parm_name=TRT, acceptable=, data_validator=&data_AE, var=&trt, required=, YN=N);
	%if &_STOPMACRO=Y %then %goto stop_exec;
**format for trt**;
%validator (type=VAR, parm=&trt, parm_name=TRT, acceptable=, data_validator=&data_AE, var=&trt, required=, YN=N, format=Y);
	%if &_STOPMACRO=Y %then %goto stop_exec;
**trt from 1 to 4**; * Add by GIS in JAN 2018;
%validator (type=VAR, parm=&trt, parm_name=TRT, acceptable=1-4, data_validator=&data_AE, var=&trt, required=, YN=N, format=Y);
	%if &_STOPMACRO=Y %then %goto stop_exec;

	%IF &EUDRACT=Y %then %do;
%validator (type=VAR, parm=&trt, parm_name=TRT, acceptable=1-4, data_validator=&dataPatient, var=&trt, required=, YN=N, format=Y);
	%if &_STOPMACRO=Y %then %goto stop_exec;
	%END;

**SS= 0-1 (0=alive, 1=dead)*;
	%IF (%LENGTH(&ss)>0) %THEN %DO;
%validator (type=VAR, parm=&ss, parm_name=SS, acceptable=0-1, data_validator=&dataPatient, var=ss, required=, YN=N, format=);
	%put WARNING: THE SURVIVAL STATUS MUST BE CODED 1=dead and 0=alive;
	%if &_STOPMACRO=Y %then %goto stop_exec;
	%END;
/***POPVAR=POPVAL**/;
proc sql noprint;
	select case
			when count(distinct(patid))>0 then 'Y'
			else 'N'
			end into :Nopopval
		from &data_ae.
		where &POPVAR.=&POPVAL.;
quit;
%IF &Nopopval=N %then %do;
	%put ERROR: ----------------------------------------------------;
	%put ERROR: &POPVAR does not contain any value equal to &POPVAL ;
	%put ERROR: ----------------------------------------------------;
	%goto stop_exec;
%end;

/***PERIODVAR=PERIODVAL**/;
proc sql noprint;
	select case
			when count(distinct(patid))>0 then 'Y'
			else 'N'
			end into :Noperiod
		from &data_ae.
		where &PERIODVAR.=&PERIODVAL.;
quit;
%IF &Noperiod=N %then %do;
	%put ERROR: ----------------------------------------------------------;
	%put ERROR: &PERIODVAR does not contain any value equal to &PERIODVAL ;
	%put ERROR: -----------------------------------------------------------;
	%goto stop_exec;
%end;

/*-----------------------------------------------
	PRESPECIFIED OR PREDEFINED PARAMETERS VALUES
--------------------------------------------------*/

**COLUMN= ALL, POOLED, SUMMARY or SEVERE**;
%validator (type=VALUE, parm=&column, parm_name=COLUMN, acceptable=ALL POOLED SUMMARY SEVERE, data_validator=, var=, required=, YN=N, format=);
	%if &_STOPMACRO=Y %then %goto stop_exec;
**GRAPH= ALL or SEVERE**;
%validator (type=VALUE, parm=&graph, parm_name=GRAPH, acceptable=N ALL SEVERE, data_validator=, var=, required=, YN=N, format=);
	%if &_STOPMACRO=Y %then %goto stop_exec;
**AXISORDER= FREQ or SOC**;
%validator (type=VALUE, parm=&AxisOrder, parm_name=AxisOrder, acceptable=SOC FREQ, data_validator=, var=, required=, YN=N, format=);
	%if &_STOPMACRO=Y %then %goto stop_exec;
**DISPLAY= TIF, PNG or PDF**;
%validator (type=VALUE, parm=&display, parm_name=DISPLAY, acceptable=TIF PNG PDF SASEMF WORD POWERPOINT, data_validator=, var=, required=, YN=N, format=);
	%if &_STOPMACRO=Y %then %goto stop_exec;
**TRESHOLD= 0-100*;
%validator (type=VALUE, parm=&Treshold, parm_name=TRESHOLD, acceptable=0-100, data_validator=&data_AE, var=, required=, YN=N, format=);
	%if &_STOPMACRO=Y %then %goto stop_exec;



/*--------------------------------------------------------------------------------
	CHECK THAT VARIABLES USED FOR CALCULATION ARE PRESENT IN THE INPUT DATASET
----------------------------------------------------------------------------------*/
**set missing value at zero for patients without AE form for coding purposes. 
	Otherwise the macro %validator does not recognize the range 0-1 for those variables**;
data &data_AE;
set &data_AE;
	if AESER=. then AESER=0;
	if AEREL=. or AEREL=.U then AEREL=0;
run;

**AETERM**;
%validator (type=VAR, parm=AETERM, parm_name=AETERM, acceptable=, data_validator=&data_AE, var=AETERM, required=, YN=);
	%if &_STOPMACRO=Y %then %goto stop_exec;
**AEMODIFY**;
%validator (type=VAR, parm=AEMODIFY, parm_name=AEMODIFY, acceptable=, data_validator=&data_AE, var=AEMODIFY, required=, YN=);
	%if &_STOPMACRO=Y %then %goto stop_exec;
**AETOXGR**;
%validator (type=VAR, parm=AETOXGR, parm_name=AETOXGR, acceptable=, data_validator=&data_AE, var=AETOXGR, required=, YN=);
	%if &_STOPMACRO=Y %then %goto stop_exec;
**SOC**;
%validator (type=VAR, parm=SOC, parm_name=SOC, acceptable=, data_validator=&data_AE, var=SOC, required=, YN=);
	%if &_STOPMACRO=Y %then %goto stop_exec;
** AEREL=0 or 1**;
%validator (type=VAR, parm=AEREL, parm_name=AEREL, acceptable=0-1, data_validator=&data_AE, var=AEREL, required=, YN=N, format=);
	%if &_STOPMACRO=Y %then %goto stop_exec;
** AESER=0 or 1**;
%validator (type=VAR, parm=AESER, parm_name=AESER, acceptable=0-1, data_validator=&data_AE, var=AESER, required=, YN=N, format=);
	%if &_STOPMACRO=Y %then %goto stop_exec;

**Check that recovered or recovered with sequela in AEOUT in coded 1 and 4**;
proc format cntlout=__formaeformats;
run;


data __formaeformats;
set __formaeformats;
	if FMTNAME="AEOUT_" then do;
		if LABEL="Recovered/resolved" and start ne 1 then do;
			put "ERROR: Recovered/resolved in AEOUT variable must be coded 1"; 
			%let _STOPMACRO=Y ;
		end;
		if LABEL="Recovered/resolved with sequelae" and start ne 4 then do;
			put "ERROR: Recovered/resolved with sequelae in AEOUT variable must be coded 4"; 
			%let _STOPMACRO=Y ;
		end;
		else do; 
		%let _STOPMACRO=N ;
		end;
	end;
run;

%if &_STOPMACRO=Y %then %goto stop_exec;



						/*================= STOP CHECKING PARAMETERS ===========================*/


/*--------------------------------
   STEP 1: DATA PREPARATION STEP
---------------------------------*/

/*Filter patient with where condition and remove if grade lower than 1*/

proc sort data=&data_AE. out=__patient2(where=(&where.)); by patid; run;


/* Import from C:\SAS in case of laptop and on the K:\ in case of fixed computer*/

%if %sysfunc (fileexist(%STR(K:\SAS\EORTC macros\CTCAEv&CTC..xls))) > 0 %then %do;
	**Import CTCAE dictionnary**;
	PROC IMPORT OUT=CTCAE 
	            DATAFILE="K:\SAS\EORTC macros\CTCAEv&CTC..xls"
	            DBMS=  EXCELCS   REPLACE;
	            SHEET='CTCAE'; 
	RUN;
%end;

%if %sysfunc (fileexist(%STR(K:\SAS\EORTC macros\CTCAEv&CTC..xls))) = 0 %then %do;
	**Import CTCAE dictionnary**;
	PROC IMPORT OUT=CTCAE 
	            DATAFILE="C:\SAS\EORTC macros\CTCAEv&CTC..xls"
	            DBMS=  EXCELCS   REPLACE;
	            SHEET='CTCAE'; 
	RUN;
%end;


data CTCAE; 
set CTCAE;
	SOC_Name=upcase(SOC_Name);
	Term=upcase(Term);
	rename SOC_Name=SOC2;
	rename Term=MEDDRATERM;
	
run;

proc sort data=CTCAE out=ALLSOCv&CTC.(keep=SOC2 SOC_short) nodupkey;
by SOC_short;
run;

%sort(data=ALLSOCv&CTC.,var=SOC_short);

**Keep the original form in case of filter on the treatment arm. Otherwise, the macro variable totarm will not be correct**;
data __Keep&data_AE.;
set &data_AE.;
run;

*VISTA*;
data ___data2;
length MEDDRATERM $200. SOC_short $22.;
set &data_AE.;
	if &where.;
	if AETOXGR >=1; *GIS, 2-11-2017: Remove of Grade=0 or 0.5;
	if AEMODIFY="" and AETERM ne "" then AEMODIFY=AETERM;
	MEDDRATERM=upcase(AEMODIFY);
	SOC_short=SOC;
run;

*CHECK GIS: if dataset empty it means that the where condition is too strict => ERROR;
%validator (type=DATA, parm=___data2, parm_name=___data2, acceptable=, data_validator=___data2, var=, required=, YN=N);
	%if &_STOPMACRO=Y %then %do;
		%put ERROR: --------------------------------------------------------------;
		%put ERROR: The &where condition is too strict. ;
		%put ERROR: Dataset &data_AE. is empty after where= condition is applied.;
		%put ERROR: --------------------------------------------------------------;
		%goto stop_exec;
	%end;


%if &details.=Y and (%length(&ODSFile)>0) %then %do;
ods rtf file="Patient_who_start_trt.rtf" style=journal;
%end;
%if &details.=Y %then %do;
title "Number of patients having started treatment who did not observe any AE";
proc print data=___data2 noobs label;
	var patid &trt.  ;
	where AEMODIFY="" and AETERM="" and (&popvar.=&popval.);
	label patid="Patient ID"
		  &trt.="Treatment";
run;
%end;
%if &details.=Y and (%length(&ODSFile)>0) %then %do;
ods rtf close;
%end;

title;

** Total number of patients for the calculation of percentages and title of the tables**;
*Safety population*;
proc sql noprint;
	create table __patient3 as
	select *,count(distinct(patid)) as totpat
	from __patient2
	where &popvar.=&popval.
	group by &trt.;
quit;
*NOTE GIS: If SAS dataset empty it means that the value of popval parameter is not in popvar parameter =>ERROR;
%validator (type=DATA, parm=__patient3, parm_name=__patient3, acceptable=, data_validator=__patient3, var=, required=, YN=N);
	%if &_STOPMACRO=Y %then %do;
			%put ERROR: ----------------------------------------------------------------------;
			%put ERROR: The value of &popval parameter is not contained in &popvar parameter ;
			%put ERROR: ----------------------------------------------------------------------;
			%goto stop_exec;
	%end;

**Count the number of treatment arms**;
proc sql noprint;
	select count(distinct(&trt.)) into :totarm
		%IF %INDEX(&where.,%str(&trt.))>0 %THEN from __Keep&data_AE.;
		%IF %INDEX(&where.,%str(&trt.))=0 %THEN from __patient2;
	;
quit;
*ERROR if &totarm=0;
%If &totarm.=0  or &totarm.>4 %then %do;
	%put ERROR: The number of treatment arms cannot be 0 nor higher than 4. ;
	%goto stop_exec;
%end;

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


%sort(data=___data2,var= MEDDRATERM);
%sort(data=CTCAE,var= MEDDRATERM);

 * recodage VISTA-MEDRA CTC ;
data __formAEmerge ;
merge ___data2(in=a) CTCAE;
by  MEDDRATERM;
if a;
run;

/*Run the macro EUDRACT if needed*/
%if %sysfunc (fileexist(%STR(K:\SAS\EORTC macros\EUDRACT_reporting.sas))) > 0 %then %do;
	*Include EUDRACT macro if needed**;
	%if &EUDRACT.=Y %then %do;
		%include "K:\SAS\EORTC macros\EUDRACT_reporting.sas";
		%eudract; 
	%end;
%end;

%if %sysfunc (fileexist(%STR(K:\SAS\EORTC macros\EUDRACT_reporting.sas))) = 0 %then %do;
	*Include EUDRACT macro if needed**;
	%if &EUDRACT.=Y %then %do;
		%include "C:\SAS\EORTC macros\EUDRACT_reporting.sas";
		%eudract;
	%end;
%end;

** Other AE reported with medraterm not reccognized by CTCAE will not be displayed ;
/*
The below data-step takes into account 2 filters:
1. the MedDRA_Code is missing, that indicates there is no match between the AE and the CTCAE;
2. there is OTH or OTHER in the SOC_short or MedDRATerm; */

data __AE_OTHERS; 
set __formAEmerge(where= (MedDRA_Code=.));
if INDEX(SOC_short,"OTH")>1 or (INDEX(UPCASE(MEDDRATERM),'OTHER,')>0 or INDEX(UPCASE(MEDDRATERM),' OTHER ')>0 
								or INDEX(UPCASE(MEDDRATERM),': OTHER')>0 or INDEX(UPCASE(MEDDRATERM),':OTHER')>0);
if SOC_short=" " then SOC_short=substr(MEDDRATERM, 1,3)||" "||"OTH";
drop SOC2;
run;

*CHECK GIS: if __AE_OTHERS is empty make sure that it is truly missing and not a wrong dataset;
%nobs (dataset=__AE_OTHERS);	
%if &_anyobs=N %then %do;
	%put WARNING: -------------------------------------------------------------------------------------------------;
	%put WARNING: The variable SOC and/or AEMODIFY never contain the word 'OTH' referring to OTHER adverse events;
	%put WARNING: It is possible but please check.;
	%put WARNING: -------------------------------------------------------------------------------------------------;
%end;

%sort(data=__AE_OTHERS,var=SOC_short MEDDRATERM);

data __AE_OTHERS;
merge __AE_OTHERS(in=a) ALLSOCv&CTC.;
by SOC_short;
if a;
run;

%sort(data=__AE_OTHERS,var=patid);

%if &details.=Y and (%length(&ODSFile)>0) %then %do;
ods rtf file="Other_AE.rtf" style=journal;
%end;
%if &details.=Y %then %do;
title "check : Listing of Others AE (&periodvar.=&periodval.)";
proc print data=__AE_OTHERS noobs label;
	var patid SOC AETERM AEMODIFY AETOXGR AESER AEREL ;
	where &periodvar.=&periodval. and &popvar.=&popval.
		 %if &SAE=Y %then and AESER=1;
		 %if &Related=Y %then and AEREL=1;
		 %if &Recovered=Y %then and AEOUT in (1,4);
	; 

	label patid="Patient ID"
		  SOC="System Organ Class"
		  AETERM="Adverse event (detailed)"
		  AEMODIFY= "Adverse event"
		  AETOXGR= "Grade"
		  AESER="Serious AE (Y/N)"
		  AEREL="AE related to treatment";
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

** Identify all terms which did not match with dictionary without being a so called OTHER AE**;
data __listing_ae_nonmatch;
merge __nomatchCRF_DICTIONNARY __AE_OTHERS(in=a);
by patid AEMODIFY ;
if not a;
run;

%sort(data=__listing_ae_nonmatch,var=patid MEDDRATERM);

%if &details.=Y and (%length(&ODSFile)>0) %then %do;
ods rtf file="Listing_not_CTC.rtf" style=journal;
%end;
%if &details.=Y %then %do;
title "check Listing of terms from AEMODIFY variable which do not fit with CTCAE_v&CTC. Term";
title2 "Missing SOC must be filled in if missing. 'Adverse event' need recoding." ;
title3 "Selected period for AEs analysis: safety population in &periodvar.=&periodval.";
proc print data=__listing_ae_nonmatch label noobs;
	var patid &trt. SOC AETERM AEMODIFY AETOXGR ;
	where &periodvar.=&periodval. and &popvar.=&popval. 
		 %if &SAE=Y %then and AESER=1;
		 %if &Related=Y %then and AEREL=1;
		 %if &Recovered=Y %then and AEOUT in (1,4);
	; 
	label patid="Patient ID"
	      &trt.= "Treatment"
		  SOC="System Organ Class"
		  AETERM="Adverse event (detailed)"
		  AEMODIFY= "Adverse event"
		  AETOXGR= "Grade";
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
set __formAEmerge(where=(MedDRA_Code ne .)) __AE_OTHERS; 
run;

proc sort data=__formAEclean out=list_of_meddraterm_in_this_study(keep=MEDDRATERM) nodupkey;
	by MEDDRATERM;
run;

proc sort data=__formAEclean out=list_of_SOC_in_this_study(keep=SOC2 SOC_short) nodupkey;
	by SOC2;
run;

data list_of_meddraterm_in_this_study;
set list_of_meddraterm_in_this_study;
indTERM=_n_;
run;

data list_of_SOC_in_this_study;
set list_of_SOC_in_this_study (where =(SOC2 ne ''));
indSOC=_n_;
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

/*-----------------------------------------
   STEP 2: COMPUTATION OF MAXIMUM GRADES
------------------------------------------*/

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
	where &periodvar.=&periodval.
					%if (&related.=Y and &SAE.=N and &recovered.=N) %then and AEREL =1; 
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
	where  &periodvar.=&periodval.
					%if (&related.=Y and &SAE.=N and &recovered.=N) %then and AEREL =1; 
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
	%GMAX (DATA=__formAEclean, VAR=AETOXGR, BYVAR=patid MEDDRATERM, OUTDATA=__WORST_PT,OUTVAR=WORST_GR_PT, sel=( &periodvar.=&periodval.)); 
	%GMAX (DATA=__formAEclean, VAR=AETOXGR, BYVAR=patid SOC2, OUTDATA=__WORST_SOC,OUTVAR=WORST_GR_SOC, sel=( &periodvar.=&periodval.));
%END;

%IF &related.=N and &SAE.=Y and &recovered.=N %THEN %DO;
	%GMAX (DATA=__formAEclean, SEL=((&periodvar.=&periodval.) and (AESER =1)), VAR=AETOXGR, BYVAR=patid MEDDRATERM, OUTDATA=__WORST_PT,OUTVAR=WORST_GR_PT); 
	%GMAX (DATA=__formAEclean, SEL=((&periodvar.=&periodval.) and (AESER =1)), VAR=AETOXGR, BYVAR=patid SOC2, OUTDATA=__WORST_SOC,OUTVAR=WORST_GR_SOC);
%END;

%IF &related.=Y and &SAE.=N and &recovered.=N %THEN %DO;
	%GMAX (DATA=__formAEclean,SEL=(( &periodvar.=&periodval.) and (AEREL =1)), VAR=AETOXGR, BYVAR=patid MEDDRATERM, OUTDATA=__WORST_PT,OUTVAR=WORST_GR_PT); 
	%GMAX (DATA=__formAEclean,SEL=(( &periodvar.=&periodval.) and (AEREL =1)), VAR=AETOXGR, BYVAR=patid SOC2, OUTDATA=__WORST_SOC,OUTVAR=WORST_GR_SOC);
%END;

%IF &related.=N and &SAE.=N and &recovered.=Y %THEN %DO;
	%GMAX (DATA=__formAEclean, SEL=(( &periodvar.=&periodval.) and (AEOUT in (1,4))), VAR=AETOXGR, BYVAR=patid MEDDRATERM, OUTDATA=__WORST_PT,OUTVAR=WORST_GR_PT); 
	%GMAX (DATA=__formAEclean, SEL=(( &periodvar.=&periodval.) and (AEOUT in (1,4))), VAR=AETOXGR, BYVAR=patid SOC2, OUTDATA=__WORST_SOC,OUTVAR=WORST_GR_SOC);
%END;

%IF &related.=N and &SAE.=Y and &recovered.=Y %THEN %DO;
	%GMAX (DATA=__formAEclean, SEL=(( &periodvar.=&periodval.) and (AEOUT in (1,4)) and (AESER =1)), VAR=AETOXGR, BYVAR=patid MEDDRATERM, OUTDATA=__WORST_PT,OUTVAR=WORST_GR_PT); 
	%GMAX (DATA=__formAEclean, SEL=(( &periodvar.=&periodval.) and (AEOUT in (1,4)) and (AESER =1)), VAR=AETOXGR, BYVAR=patid SOC2, OUTDATA=__WORST_SOC,OUTVAR=WORST_GR_SOC);
%END;

%IF &related.=Y and &SAE.=N and &recovered.=Y %THEN %DO;
	%GMAX (DATA=__formAEclean, SEL=((&periodvar.=&periodval.) and (AEOUT in (1,4)) and (AEREL =1)), VAR=AETOXGR, BYVAR=patid MEDDRATERM, OUTDATA=__WORST_PT,OUTVAR=WORST_GR_PT); 
	%GMAX (DATA=__formAEclean, SEL=(( &periodvar.=&periodval.) and (AEOUT in (1,4)) and (AEREL =1)), VAR=AETOXGR, BYVAR=patid SOC2, OUTDATA=__WORST_SOC,OUTVAR=WORST_GR_SOC);
%END;

%IF &related.=Y and &SAE.=Y and &recovered.=N %THEN %DO;
	%GMAX (DATA=__formAEclean, SEL=(( &periodvar.=&periodval.) and (AESER =1) and (AEREL =1)), VAR=AETOXGR, BYVAR=patid MEDDRATERM, OUTDATA=__WORST_PT,OUTVAR=WORST_GR_PT); 
	%GMAX (DATA=__formAEclean, SEL=((&periodvar.=&periodval.) and (AESER =1) and (AEREL =1)), VAR=AETOXGR, BYVAR=patid SOC2, OUTDATA=__WORST_SOC,OUTVAR=WORST_GR_SOC);
%END;

%IF &related.=Y and &SAE.=Y and &recovered.=Y %THEN %DO;
	%GMAX (DATA=__formAEclean,SEL=(( &periodvar.=&periodval.) and (AEREL =1) and (AESER =1) and (AEOUT in (1,4))), VAR=AETOXGR, BYVAR=patid MEDDRATERM, OUTDATA=__WORST_PT,OUTVAR=WORST_GR_PT); 
	%GMAX (DATA=__formAEclean,SEL=(( &periodvar.=&periodval.) and (AEREL =1) and (AESER =1) and (AEOUT in (1,4))), VAR=AETOXGR, BYVAR=patid SOC2, OUTDATA=__WORST_SOC,OUTVAR=WORST_GR_SOC);
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


/*----------------------------------------------------------------------------------------
   STEP 3: COUNT THE NUMBER OF PATIENT IN EACH CATEGORY OF GRADE (MAX GRADE BY PATIENT)
-----------------------------------------------------------------------------------------*/

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
	select patid, MEDDRATERM, SOC2, WORST_GR_PT, &trt, totpat, max (WORST_GR_PT) as pts_maxgrade
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


/*=== Creation of an internal macro to count the total number of patient having at least one AE by grade === */

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
	Concatenation_="PATIENTS' WORST GRADE";
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



/*-------------------------------------------
   STEP 4: PREPARATION OF THE SUMMARY TABLE
----------------------------------------------*/

/*Treshold parameter: select AE representing more than x % incidence in at least one of the treatment arms
  By default, no treshold. Otherwise, use the one defined by the user*/

data __summall_1 ;
set __sumall;
if MEDDRATERM = "*GENERAL TERM" then keepAll=1;
	%IF &Treshold.=0 %THEN %DO; 
		keepAll=1;
	%END;

	%IF &Treshold.>0 %THEN %DO; 
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
 where &trt.=1 and &periodvar.=&periodval.;
 quit;

%if &totarm.>1 %then %do;
proc sql noprint;
 select count(distinct(patid)) into :NPAT2 from __patient3
 where &trt.=2 and &periodvar.=&periodval.;
 quit;
%end;

%if &totarm.=3 %then %do;
proc sql noprint;
 select count(distinct(patid)) into :NPAT3 from __patient3
 where &trt.=3 and &periodvar.=&periodval.;
 quit;
%end;

%if &totarm.=4 %then %do;
proc sql noprint;
 select count(distinct(patid)) into :NPAT3 from __patient3
 where &trt.=3 and &periodvar.=&periodval.;
 quit;

proc sql noprint;
 select count (distinct(patid)) into :NPAT4 from __patient3
 where &trt.=4 and &periodvar.=&periodval.;
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
	if INDEX(SOC_short,"OTH")>1 or (INDEX(UPCASE(MEDDRATERM),'OTHER,')>0 or INDEX(UPCASE(MEDDRATERM),' OTHER ')>0 
		or INDEX(UPCASE(MEDDRATERM),': OTHER')>0 or INDEX(UPCASE(MEDDRATERM),':OTHER')>0)
	then MEDDRATERM="XXX";
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


/*-------------------------------------------
   STEP 5 : DISPLAY THE SUMMARY TABLE
---------------------------------------------*/

/******************************************************************************************************************
  In case of filter on the treatment arm with the were= condition some additional paramater need to be created to 
   avoid mistakes in the table.
******************************************************************************************************************/

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

/********************************  STOP ADDITIONAL MACRO PARAMETERS  ******************************************
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
%if &totarm.=2 and %UPCASE(&column.)=SUMMARY %then %let socwidth=12.0;
%if &totarm.=4 and %UPCASE(&column.)^=SUMMARY %then %let fontsize=8;
%if &totarm.>3 and %UPCASE(&column.)=SUMMARY %then %let gradewidth=2.0;
*Added by GIS in JAN 2018;
%if &anyGR5.=Y and %UPCASE(&column.)=ALL %then %let socwidth=4.5;
%if &anyGR5.=Y and %UPCASE(&column.)=ALL %then %let fontsize=8;
%if &anyGR5.=Y and %UPCASE(&column.)=ALL %then %let gradewidth=1.2;


*Set title by default*;
%IF (%LENGTH(&TitleTable.)>0) %THEN %DO; Title "&TitleTable." height=14pt bold italic; %END;
%IF (%LENGTH(&TitleTable.)=0) %THEN %DO;
 	Title "All adverse events, by treatment." height=14pt bold italic;
	%IF &Treshold.>0 %THEN %DO ; Title2 "Adverse events representing more than &Treshold. % incidence in at least one arm"; %END;
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
%END;

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
define Concatenation_ / id 'System Organ Class + Preferred term'  
								  style(column)={font_face=calibri fontsize=9pt cellspacing=0.2 asis=on borderrightcolor=black borderrightwidth=0.5 
									 			borderbottomstyle=dotted borderbottomcolor=black borderbottomwidth=0.5 cellwidth=&socwidth.cm}
									style (header)={font_face=calibri fontsize=10pt borderrightcolor=black borderrightwidth=0.5 
													%if &colour.=N %then borderbottomcolor=black borderbottomwidth=0.5;} width=5;

%IF &totarm.=1 %THEN %DO;

%IF %UPCASE(&column.)=ALL %THEN %DO;
define Npct11 / "Grade" "^-2n1" "^-2nN (%)" style (column)={just=center borderbottomstyle=dotted borderbottomcolor=black borderbottomwidth=0.5 cellwidth=&gradewidth.cm font_face=calibri fontsize=&fontsize.pt}
						  				style (header)={%if &colour.=Y %then borderrightcolor=cxE0E0E0;
														%if &colour.=N %then borderrightcolor=white borderbottomcolor=black borderbottomwidth=0.5; borderrightwidth=0.5};
define Npct12 / "Grade" "^-2n2" "^-2nN (%)" style (column)={just=center borderbottomstyle=dotted borderbottomcolor=black borderbottomwidth=0.5 cellwidth=&gradewidth.cm font_face=calibri fontsize=&fontsize.pt}
						  				style (header)={%if &colour.=Y %then borderrightcolor=cxE0E0E0;
														%if &colour.=N %then borderrightcolor=white borderbottomcolor=black borderbottomwidth=0.5;
														borderrightwidth=0.5};
define Npct13 / "Grade" "^-2n3" "^-2nN (%)" style (column)={just=center borderbottomstyle=dotted borderbottomcolor=black borderbottomwidth=0.5 cellwidth=&gradewidth.cm font_face=calibri fontsize=&fontsize.pt}
						  			   style (header)={%if &colour.=Y %then borderrightcolor=cxE0E0E0;
														%if &colour.=N %then borderrightcolor=white borderbottomcolor=black borderbottomwidth=0.5;
														borderrightwidth=0.5};
define Npct14 / "Grade" "^-2n4" "^-2nN (%)" style (column)={just=center borderbottomstyle=dotted borderbottomcolor=black borderbottomwidth=0.5 cellwidth=&gradewidth.cm font_face=calibri fontsize=&fontsize.pt}
						  				style (header)={%if &colour.=Y %then borderrightcolor=cxE0E0E0;
														%if &colour.=N %then borderrightcolor=white borderbottomcolor=black borderbottomwidth=0.5;
														borderrightwidth=0.5};
%if &anyGR5.=Y %then %do; 
define Npct15 / "Grade" "^-2n5" "^-2nN (%)" style (column)={just=center borderbottomstyle=dotted borderbottomcolor=black borderbottomwidth=0.5 cellwidth=&gradewidth.cm font_face=calibri fontsize=&fontsize.pt}
  									    style (header)={%if &colour.=Y %then borderrightcolor=cxE0E0E0;
										 				%if &colour.=N %then borderrightcolor=white borderbottomcolor=black borderbottomwidth=0.5;
																			 borderrightwidth=0.5};
%end;
%if &anyGR5.=N %then define Npct15 /noprint;;

define Npct1345 / "Grade" "^-2n ^{unicode '2265'x}3" "^-2nN (%)" style(column)={just=center cellwidth=&gradewidth.cm 
														borderbottomstyle=dotted borderbottomcolor=black borderbottomwidth=0.5
														%if &colour.=Y %then borderrightcolor=whitesmoke background=whitesmoke;
														%if &colour.=N %then borderrightcolor=white background=white;
														borderrightwidth=0.5 cellwidth=&gradewidth.cm font_face=calibri fontsize=&fontsize.pt}
							 							style (header)={%if &colour.=Y %then borderrightcolor=cxE0E0E0; 
																		%if &colour.=N %then borderrightcolor=white borderbottomcolor=black borderbottomwidth=0.5;
																		borderrightwidth=0.5};
define Npct1ALL / "Grade" "^-2n ^{unicode '2265'x}1" "^-2nN (%)"  style(column)={just=center cellwidth=&gradewidth.cm 
																		%if &colour.=Y %then background=whitesmoke;
																		%if &colour.=N %then background=white;	 
											 							borderrightcolor=black borderrightwidth=0.5 font_face=calibri fontsize=&fontsize.pt
											 							borderbottomstyle=dotted borderbottomcolor=black borderbottomwidth=0.5}
							  							style (header)={just=center borderrightcolor=black borderrightwidth=0.5 cellwidth=&gradewidth.cm
																		%if &colour.=N %then borderbottomcolor=black borderbottomwidth=0.5;};
%END;

%IF %UPCASE(&column.)=SEVERE %THEN %DO;
define Npct13 / "Grade" "^-2n3" "^-2nN (%)" style (column)={just=center borderbottomstyle=dotted borderbottomcolor=black borderbottomwidth=0.5 cellwidth=&gradewidth.cm font_face=calibri fontsize=&fontsize.pt}
						  				 style (header)={%if &colour.=Y %then borderrightcolor=cxE0E0E0;
										 				 %if &colour.=N %then borderrightcolor=white borderbottomcolor=black borderbottomwidth=0.5;
										 				 borderrightwidth=0.5};
define Npct14 / "Grade" "^-2n4" "^-2nN (%)" style (column)={just=center borderbottomstyle=dotted borderbottomcolor=black borderbottomwidth=0.5 cellwidth=&gradewidth.cm font_face=calibri fontsize=&fontsize.pt}
						  				 style (header)={%if &colour.=Y %then borderrightcolor=cxE0E0E0;
										 				 %if &colour.=N %then borderrightcolor=white borderbottomcolor=black borderbottomwidth=0.5;
										 				 borderrightwidth=0.5};
%if &anyGR5.=Y %then %do;
define Npct15 / "Grade" "^-2n5" "^-2nN (%)" style (column)={just=center borderbottomstyle=dotted borderbottomcolor=black borderbottomwidth=0.5 cellwidth=&gradewidth.cm font_face=calibri fontsize=&fontsize.pt}
										  style (header)={%if &colour.=Y %then borderrightcolor=cxE0E0E0;
										  				  %if &colour.=N %then borderrightcolor=white borderbottomcolor=black borderbottomwidth=0.5; 
										  				  borderrightwidth=0.5};
%end;
%if &anyGR5.=N %then define Npct15 /noprint;;

define Npct1345 / "Grade" "^-2n ^{unicode '2265'x}3" "^-2nN (%)" style(column)={just=center cellwidth=&gradewidth.cm %if &colour.=Y %then background=whitesmoke;
																		%if &colour.=N %then background=white;	
													   					borderbottomstyle=dotted borderbottomcolor=black borderbottomwidth=0.5
													   					borderrightcolor=black borderrightwidth=0.5 cellwidth=&gradewidth.cm 
													   					font_face=calibri fontsize=&fontsize.pt}
													   style (header)={borderrightcolor=black borderrightwidth=0.5 
																	   %if &colour.=N %then borderbottomcolor=black borderbottomwidth=0.5;};
%END;

%IF %UPCASE(&column.)=POOLED %THEN %DO;
define Npct112 / "Grade" "^-2n1/2" "^-2nN (%)" style (column)={just=center borderbottomstyle=dotted borderbottomcolor=black borderbottomwidth=0.5 cellwidth=&gradewidth.cm font_face=calibri fontsize=&fontsize.pt}
						  			 style (header)={%if &colour.=Y %then borderrightcolor=cxE0E0E0;
													 %if &colour.=N %then borderrightcolor=white borderbottomcolor=black borderbottomwidth=0.5;
												     borderrightwidth=0.5};
define Npct134 / "Grade" "^-2n3/4" "^-2nN (%)" style (column)={just=center borderbottomstyle=dotted borderbottomcolor=black borderbottomwidth=0.5 cellwidth=&gradewidth.cm font_face=calibri fontsize=&fontsize.pt}
						  			 style (header)={%if &colour.=Y %then borderrightcolor=cxE0E0E0;
													 %if &colour.=N %then borderrightcolor=white borderbottomcolor=black borderbottomwidth=0.5;
													borderrightwidth=0.5};
%if &anyGR5.=Y %then %do;
define Npct15 / "Grade" "^-2n5" "^-2nN (%)" style (column)={just=center borderbottomstyle=dotted borderbottomcolor=black borderbottomwidth=0.5 cellwidth=&gradewidth.cm font_face=calibri fontsize=&fontsize.pt}
 									     style (header)={%if &colour.=Y %then borderrightcolor=cxE0E0E0;
														 %if &colour.=N %then borderrightcolor=white borderbottomcolor=black borderbottomwidth=0.5;
														  borderrightwidth=0.5};
%end;
%if &anyGR5.=N %then define Npct15 /noprint;;

define Npct1345 / "Grade" "^-2n ^{unicode '2265'x}3" "^-2nN (%)" style(column)={just=center cellwidth=&gradewidth.cm 
																		%if &colour.=Y %then background=whitesmoke borderrightcolor=whitesmoke;
																		%if &colour.=N %then background=white borderrightcolor=white;
													    				borderbottomstyle=dotted borderbottomcolor=black borderbottomwidth=0.5
													    				borderrightwidth=0.5 cellwidth=&gradewidth.cm font_face=calibri fontsize=&fontsize.pt}
							 							style (header)={%if &colour.=Y %then borderrightcolor=cxE0E0E0;
																		%if &colour.=N %then borderrightcolor=white borderbottomcolor=black borderbottomwidth=0.5;
														borderrightwidth=0.5};
define Npct1ALL / "Grade" "^-2n ^{unicode '2265'x}1" "^-2nN (%)"  style(column)={just=center cellwidth=&gradewidth.cm 
																		%if &colour.=Y %then background=whitesmoke;
 																		%if &colour.=N %then background=white;
																		borderrightcolor=black borderrightwidth=0.5 font_face=calibri fontsize=&fontsize.pt
											 							borderbottomstyle=dotted borderbottomcolor=black borderbottomwidth=0.5}
							 							style (header)={just=center borderrightcolor=black borderrightwidth=0.5 cellwidth=&gradewidth.cm
																		%if &colour.=N %then borderbottomcolor=black borderbottomwidth=0.5;};
%END;

%IF %UPCASE(&column.)=SUMMARY %THEN %DO;
define Npct1345 / "Grade" "^-2n ^{unicode '2265'x}3" "^-2nN (%)" style(column)={just=center cellwidth=&gradewidth.cm 
																		borderbottomstyle=dotted borderbottomcolor=black borderbottomwidth=0.5
																		borderrightcolor=white borderrightwidth=0.5 cellwidth=&gradewidth.cm
																		font_face=calibri fontsize=&fontsize.pt}
							 							style (header)={%if &colour.=Y %then borderrightcolor=cxE0E0E0;
										     							%if &colour.=N %then borderrightcolor=white borderbottomcolor=black borderbottomwidth=0.5;
																		borderrightwidth=0.5};
define Npct1ALL / "Grade" "^-2n ^{unicode '2265'x}1" "^-2nN (%)"  style(column)={just=center cellwidth=&gradewidth.cm  
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
define Npct11 / "Grade" "^-2n1" "^-2nN (%)" style (column)={just=center borderbottomstyle=dotted borderbottomcolor=black borderbottomwidth=0.5 cellwidth=&gradewidth.cm font_face=calibri fontsize=&fontsize.pt}
						  				style (header)={%if &colour.=Y %then borderrightcolor=cxE0E0E0;
														%if &colour.=N %then borderrightcolor=white borderbottomcolor=black borderbottomwidth=0.5; borderrightwidth=0.5 };
define Npct12 / "Grade" "^-2n2" "^-2nN (%)" style (column)={just=center borderbottomstyle=dotted borderbottomcolor=black borderbottomwidth=0.5 cellwidth=&gradewidth.cm font_face=calibri fontsize=&fontsize.pt}
						  				style (header)={%if &colour.=Y %then borderrightcolor=cxE0E0E0;
														%if &colour.=N %then borderrightcolor=white borderbottomcolor=black borderbottomwidth=0.5 ;
														borderrightwidth=0.5};
define Npct13 / "Grade" "^-2n3" "^-2nN (%)" style (column)={just=center borderbottomstyle=dotted borderbottomcolor=black borderbottomwidth=0.5 cellwidth=&gradewidth.cm font_face=calibri fontsize=&fontsize.pt}
						  			   style (header)={%if &colour.=Y %then borderrightcolor=cxE0E0E0;
														%if &colour.=N %then borderrightcolor=white borderbottomcolor=black borderbottomwidth=0.5;
														borderrightwidth=0.5};
define Npct14 / "Grade" "^-2n4" "^-2nN (%)" style (column)={just=center borderbottomstyle=dotted borderbottomcolor=black borderbottomwidth=0.5 cellwidth=&gradewidth.cm font_face=calibri fontsize=&fontsize.pt}
						  				style (header)={%if &colour.=Y %then borderrightcolor=cxE0E0E0;
														%if &colour.=N %then borderrightcolor=white borderbottomcolor=black borderbottomwidth=0.5;
														borderrightwidth=0.5};
%if &anyGR5.=Y %then %do;
define Npct15 / "Grade" "^-2n5" "^-2nN (%)" style (column)={just=center borderbottomstyle=dotted borderbottomcolor=black borderbottomwidth=0.5 cellwidth=&gradewidth.cm font_face=calibri fontsize=&fontsize.pt}
										 style (header)={%if &colour.=Y %then borderrightcolor=cxE0E0E0;
										 				 %if &colour.=N %then borderrightcolor=white borderbottomcolor=black borderbottomwidth=0.5;
										 				 borderrightwidth=0.5};
%end;
%if &anyGR5.=N %then define Npct15 /noprint;;

define Npct1345 / "Grade" "^-2n ^{unicode '2265'x}3" "^-2nN (%)" style(column)={just=center cellwidth=&gradewidth.cm 
																		borderbottomstyle=dotted borderbottomcolor=black borderbottomwidth=0.5
																		%if &colour.=Y %then borderrightcolor=whitesmoke background=whitesmoke;
																		%if &colour.=N %then borderrightcolor=white background=white;
																		borderrightwidth=0.5 cellwidth=&gradewidth.cm font_face=calibri fontsize=&fontsize.pt}
							 							style (header)={%if &colour.=Y %then borderrightcolor=cxE0E0E0; 
																		%if &colour.=N %then borderrightcolor=white borderbottomcolor=black borderbottomwidth=0.5;
																		borderrightwidth=0.5};
define Npct1ALL / "Grade" "^-2n ^{unicode '2265'x}1" "^-2nN (%)"  style(column)={just=center cellwidth=&gradewidth.cm 
																		%if &colour.=Y %then background=whitesmoke ;
																		%if &colour.=N %then background=white ;
											 							borderrightcolor=black borderrightwidth=0.5 font_face=calibri fontsize=&fontsize.pt
											 							borderbottomstyle=dotted borderbottomcolor=black borderbottomwidth=0.5}
							  							 style (header)={just=center borderrightcolor=black borderrightwidth=0.5 cellwidth=&gradewidth.cm
																		%if &colour.=N %then borderbottomcolor=black borderbottomwidth=0.5;};
%END;

%IF %UPCASE(&column.)=SEVERE %THEN %DO;
define Npct13 / "Grade" "^-2n3" "^-2nN (%)" style (column)={just=center borderbottomstyle=dotted borderbottomcolor=black borderbottomwidth=0.5 cellwidth=&gradewidth.cm font_face=calibri fontsize=&fontsize.pt}
						  				 style (header)={%if &colour.=Y %then borderrightcolor=cxE0E0E0;
										 				 %if &colour.=N %then borderrightcolor=white borderbottomcolor=black borderbottomwidth=0.5;
														 borderrightwidth=0.5};
define Npct14 / "Grade" "^-2n4" "^-2nN (%)" style (column)={just=center borderbottomstyle=dotted borderbottomcolor=black borderbottomwidth=0.5 cellwidth=&gradewidth.cm font_face=calibri fontsize=&fontsize.pt}
						  				 style (header)={%if &colour.=Y %then borderrightcolor=cxE0E0E0;
										 				 %if &colour.=N %then borderrightcolor=white borderbottomcolor=black borderbottomwidth=0.5;
										 				 borderrightwidth=0.5};
%if &anyGR5.=Y %then %do;
define Npct15 / "Grade" "^-2n5" "^-2nN (%)" style (column)={just=center borderbottomstyle=dotted borderbottomcolor=black borderbottomwidth=0.5 cellwidth=&gradewidth.cm font_face=calibri fontsize=&fontsize.pt}
										  style (header)={%if &colour.=Y %then borderrightcolor=cxE0E0E0;
										  				  %if &colour.=N %then borderrightcolor=white borderbottomcolor=black borderbottomwidth=0.5; 
										  				  borderrightwidth=0.5};
%end;
%if &anyGR5.=N %then define Npct15 /noprint;;

define Npct1345 / "Grade" "^-2n ^{unicode '2265'x}3" "^-2nN (%)" style(column)={just=center cellwidth=&gradewidth.cm %if &colour.=Y %then background=whitesmoke;
																		%if &colour.=N %then background=white;	
													   					borderbottomstyle=dotted borderbottomcolor=black borderbottomwidth=0.5
													   					borderrightcolor=black borderrightwidth=0.5 cellwidth=&gradewidth.cm 
													   					font_face=calibri fontsize=&fontsize.pt}
													   style (header)={borderrightcolor=black borderrightwidth=0.5
																		%if &colour.=N %then borderbottomcolor=black borderbottomwidth=0.5;};
%END;

%IF %UPCASE(&column.)=POOLED %THEN %DO;
define Npct112 / "Grade" "^-2n1/2" "^-2nN (%)" style (column)={just=center borderbottomstyle=dotted borderbottomcolor=black borderbottomwidth=0.5 cellwidth=&gradewidth.cm font_face=calibri fontsize=&fontsize.pt}
						  			 style (header)={%if &colour.=Y %then borderrightcolor=cxE0E0E0;
													 %if &colour.=N %then borderrightcolor=white borderbottomcolor=black borderbottomwidth=0.5;
												     borderrightwidth=0.5};
define Npct134 / "Grade" "^-2n3/4" "^-2nN (%)" style (column)={just=center borderbottomstyle=dotted borderbottomcolor=black borderbottomwidth=0.5 cellwidth=&gradewidth.cm font_face=calibri fontsize=&fontsize.pt}
						  			 style (header)={%if &colour.=Y %then borderrightcolor=cxE0E0E0;
													 %if &colour.=N %then borderrightcolor=white borderbottomcolor=black borderbottomwidth=0.5;
													borderrightwidth=0.5};
%if &anyGR5.=Y %then %do;
define Npct15 / "Grade" "^-2n5" "^-2nN (%)" style (column)={just=center borderbottomstyle=dotted borderbottomcolor=black borderbottomwidth=0.5 cellwidth=&gradewidth.cm font_face=calibri fontsize=&fontsize.pt}
									      style (header)={%if &colour.=Y %then borderrightcolor=cxE0E0E0;
														  %if &colour.=N %then borderrightcolor=white borderbottomcolor=black borderbottomwidth=0.5;
														  borderrightwidth=0.5};
%end;
%if &anyGR5.=N %then define Npct15 /noprint;;

define Npct1345 / "Grade" "^-2n ^{unicode '2265'x}3" "^-2nN (%)" style(column)={just=center cellwidth=&gradewidth.cm 
																		%if &colour.=Y %then background=whitesmoke borderrightcolor=whitesmoke;
																		%if &colour.=N %then background=white borderrightcolor=white;
																	    borderbottomstyle=dotted borderbottomcolor=black borderbottomwidth=0.5
																	    borderrightwidth=0.5 cellwidth=&gradewidth.cm font_face=calibri fontsize=&fontsize.pt}
							 							style (header)={%if &colour.=Y %then borderrightcolor=cxE0E0E0;
																		%if &colour.=N %then borderrightcolor=white borderbottomcolor=black borderbottomwidth=0.5;
																		borderrightwidth=0.5};
define Npct1ALL / "Grade" "^-2n ^{unicode '2265'x}1" "^-2nN (%)"  style(column)={just=center cellwidth=&gradewidth.cm 
																		%if &colour.=Y %then background=whitesmoke;
				 														%if &colour.=N %then background=white;
																		borderrightcolor=black borderrightwidth=0.5 font_face=calibri fontsize=&fontsize.pt
															 			borderbottomstyle=dotted borderbottomcolor=black borderbottomwidth=0.5}
							 							style (header)={just=center borderrightcolor=black borderrightwidth=0.5 cellwidth=&gradewidth.cm
																		%if &colour.=N %then borderbottomcolor=black borderbottomwidth=0.5;};
%END;

%IF %UPCASE(&column.)=SUMMARY %THEN %DO;
define Npct1345 / "Grade" "^-2n ^{unicode '2265'x}3" "^-2nN (%)" style(column)={just=center cellwidth=&gradewidth.cm 
																		borderbottomstyle=dotted borderbottomcolor=black borderbottomwidth=0.5
																		borderrightcolor=white borderrightwidth=0.5 cellwidth=&gradewidth.cm
																		font_face=calibri fontsize=&fontsize.pt}
										 				style (header)={%if &colour.=Y %then borderrightcolor=cxE0E0E0;
																	    %if &colour.=N %then borderrightcolor=white borderbottomcolor=black borderbottomwidth=0.5;
																		borderrightwidth=0.5};
define Npct1ALL / "Grade" "^-2n ^{unicode '2265'x}1" "^-2nN (%)"  style(column)={just=center cellwidth=&gradewidth.cm  
																		 borderrightcolor=black borderrightwidth=0.5 font_face=calibri fontsize=&fontsize.pt
																		 borderbottomstyle=dotted borderbottomcolor=black borderbottomwidth=0.5}
														 style (header)={%if &colour.=N %then borderbottomcolor=black borderbottomwidth=0.5;
																		 just=center borderrightcolor=black borderrightwidth=0.5 cellwidth=&gradewidth.cm};
%END;
%END;

/*ARM2*/
%IF %INDEX(&where.,%str(&trt.))=0 or &show2.=Y %THEN %DO;

%IF %UPCASE(&column.)=ALL %THEN %DO;
define Npct21 / "Grade" "^-2n1" "^-2nN (%)" style (column)={just=center borderbottomstyle=dotted borderbottomcolor=black borderbottomwidth=0.5 cellwidth=&gradewidth.cm font_face=calibri fontsize=&fontsize.pt}
						  			 	style (header)={%if &colour.=Y %then borderrightcolor=cxE0E0E0;
														%if &colour.=N %then borderrightcolor=white borderbottomcolor=black borderbottomwidth=0.5; borderrightwidth=0.5 };
define Npct22 / "Grade" "^-2n2" "^-2nN (%)" style (column)={just=center borderbottomstyle=dotted borderbottomcolor=black borderbottomwidth=0.5 cellwidth=&gradewidth.cm font_face=calibri fontsize=&fontsize.pt}
						  				style (header)={%if &colour.=Y %then borderrightcolor=cxE0E0E0;
														%if &colour.=N %then borderrightcolor=white borderbottomcolor=black borderbottomwidth=0.5; borderrightwidth=0.5 };
define Npct23 / "Grade" "^-2n3" "^-2nN (%)" style (column)={just=center borderbottomstyle=dotted borderbottomcolor=black borderbottomwidth=0.5 cellwidth=&gradewidth.cm font_face=calibri fontsize=&fontsize.pt}
						  				style (header)={%if &colour.=Y %then borderrightcolor=cxE0E0E0;
														%if &colour.=N %then borderrightcolor=white borderbottomcolor=black borderbottomwidth=0.5; borderrightwidth=0.5 };
define Npct24 / "Grade" "^-2n4" "^-2nN (%)" style (column)={just=center borderbottomstyle=dotted borderbottomcolor=black borderbottomwidth=0.5 cellwidth=&gradewidth.cm font_face=calibri fontsize=&fontsize.pt}
						  				style (header)={%if &colour.=Y %then borderrightcolor=cxE0E0E0;
														%if &colour.=N %then borderrightcolor=white borderbottomcolor=black borderbottomwidth=0.5; borderrightwidth=0.5 };
%if &anyGR5.=Y %then  %do;
define Npct25 / "Grade" "^-2n5" "^-2nN (%)" style (column)={just=center borderbottomstyle=dotted borderbottomcolor=black borderbottomwidth=0.5 cellwidth=&gradewidth.cm font_face=calibri fontsize=&fontsize.pt}
 									    style (header)={%if &colour.=Y %then borderrightcolor=cxE0E0E0;
													   %if &colour.=N %then borderrightcolor=white borderbottomcolor=black borderbottomwidth=0.5; 
														borderrightwidth=0.5};
%end;
%if &anyGR5.=N %then define Npct25 /noprint;;

define Npct2345 / "Grade" "^-2n ^{unicode '2265'x}3" "^-2nN (%)" style(column)={just=center cellwidth=&gradewidth.cm 
																	    %if &colour.=Y %then background=whitesmoke borderrightcolor=whitesmoke;
																		%if &colour.=N %then background=white borderrightcolor=white;
																		borderbottomstyle=dotted borderbottomcolor=black borderbottomwidth=0.5
																		borderrightwidth=0.5 cellwidth=&gradewidth.cm
																		font_face=calibri fontsize=&fontsize.pt}
							 							style (header)={%if &colour.=Y %then borderrightcolor=cxE0E0E0;
																		%if &colour.=N %then borderrightcolor=white borderbottomcolor=black borderbottomwidth=0.5; borderrightwidth=0.5} ;
define Npct2ALL / "Grade" "^-2n ^{unicode '2265'x}1" "^-2nN (%)" style(column)={just=center cellwidth=&gradewidth.cm 
																		%if &colour.=Y %then background=whitesmoke;
																		%if &colour.=N %then background=white;
																		font_face=calibri fontsize=&fontsize.pt
															 			borderrightcolor=black borderrightwidth=0.5 cellwidth=&gradewidth.cm
															 			borderbottomstyle=dotted borderbottomcolor=black borderbottomwidth=0.5}
														%if &colour.=N %then style (header)={borderbottomcolor=black borderbottomwidth=0.5};;
%END;

%IF %UPCASE(&column.)=SEVERE %THEN %DO;
define Npct23 / "Grade" "^-2n3" "^-2nN (%)" style (column)={just=center borderbottomstyle=dotted borderbottomcolor=black borderbottomwidth=0.5 cellwidth=&gradewidth.cm font_face=calibri fontsize=&fontsize.pt}
						  				 style (header)={%if &colour.=Y %then borderrightcolor=cxE0E0E0;
														 %if &colour.=N %then borderrightcolor=white borderbottomcolor=black borderbottomwidth=0.5; borderrightwidth=0.5 };
define Npct24 / "Grade" "^-2n4" "^-2nN (%)" style (column)={just=center borderbottomstyle=dotted borderbottomcolor=black borderbottomwidth=0.5 cellwidth=&gradewidth.cm font_face=calibri fontsize=&fontsize.pt}
						 				 style (header)={%if &colour.=Y %then borderrightcolor=cxE0E0E0;
														 %if &colour.=N %then borderrightcolor=white borderbottomcolor=black borderbottomwidth=0.5; borderrightwidth=0.5 };
%if &anyGR5.=Y %then  %do;
define Npct25 / "Grade" "^-2n5" "^-2nN (%)" style (column)={just=center borderbottomstyle=dotted borderbottomcolor=black borderbottomwidth=0.5 cellwidth=&gradewidth.cm font_face=calibri fontsize=&fontsize.pt}
 									    style (header)={%if &colour.=Y %then borderrightcolor=cxE0E0E0;
													   %if &colour.=N %then borderrightcolor=white borderbottomcolor=black borderbottomwidth=0.5; 
														borderrightwidth=0.5};
%end;
%if &anyGR5.=N %then define Npct25 /noprint;;

define Npct2345 / "Grade" "^-2n ^{unicode '2265'x}3" "^-2nN (%)" style(column)={just=center cellwidth=&gradewidth.cm 
																	   %if &colour.=Y %then background=whitesmoke; 
																	   %if &colour.=N %then background=white;
																	   font_face=calibri fontsize=&fontsize.pt
											 						   borderrightcolor=black borderrightwidth=0.5
																	   borderbottomstyle=dotted borderbottomcolor=black borderbottomwidth=0.5}
														%if &colour.=N %then style (header)={borderbottomcolor=black borderbottomwidth=0.5};;
%END;

%IF %UPCASE(&column.)=POOLED %THEN %DO;
define Npct212 / "Grade" "^-2n1/2" "^-2nN (%)" style (column)={just=center borderbottomstyle=dotted borderbottomcolor=black borderbottomwidth=0.5 cellwidth=&gradewidth.cm font_face=calibri fontsize=&fontsize.pt}
						  			 style (header)={%if &colour.=Y %then borderrightcolor=cxE0E0E0;
													 %if &colour.=N %then borderrightcolor=white borderbottomcolor=black borderbottomwidth=0.5; borderrightwidth=0.5 };
define Npct234 / "Grade" "^-2n3/4" "^-2nN (%)" style (column)={just=center borderbottomstyle=dotted borderbottomcolor=black borderbottomwidth=0.5 cellwidth=&gradewidth.cm font_face=calibri fontsize=&fontsize.pt}
						  			 style (header)={%if &colour.=Y %then borderrightcolor=cxE0E0E0;
													 %if &colour.=N %then borderrightcolor=white borderbottomcolor=black borderbottomwidth=0.5; borderrightwidth=0.5 };
%if &anyGR5.=Y %then %do;
define Npct25 / "Grade" "^-2n5" "^-2nN (%)" style (column)={just=center borderbottomstyle=dotted borderbottomcolor=black borderbottomwidth=0.5 cellwidth=&gradewidth.cm font_face=calibri fontsize=&fontsize.pt}
									     style (header)={%if &colour.=Y %then borderrightcolor=cxE0E0E0;
													     %if &colour.=N %then borderrightcolor=white borderbottomcolor=black borderbottomwidth=0.5; 
														 borderrightwidth=0.5};
%end;
%if &anyGR5.=N %then define Npct25 /noprint;;

define Npct2345 / "Grade" "^-2n ^{unicode '2265'x}3" "^-2nN (%)" style(column)={just=center cellwidth=&gradewidth.cm 
																	   %if &colour.=Y %then background=whitesmoke borderrightcolor=whitesmoke;
																	   %if &colour.=N %then background=white borderrightcolor=white;
																	   borderbottomstyle=dotted borderbottomcolor=black borderbottomwidth=0.5
																	   borderrightwidth=0.5 font_face=calibri fontsize=&fontsize.pt}
							 						   style (header)={%if &colour.=Y %then borderrightcolor=cxE0E0E0;
																	   %if &colour.=N %then borderrightcolor=white borderbottomcolor=black borderbottomwidth=0.5; borderrightwidth=0.5 };
define Npct2ALL / "Grade" "^-2n ^{unicode '2265'x}1" "^-2nN (%)" style(column)={just=center cellwidth=&gradewidth.cm 
																	   %if &colour.=Y %then background=whitesmoke;
																	   %if &colour.=N %then background=white;
																	   font_face=calibri fontsize=&fontsize.pt
																	   borderrightcolor=black borderrightwidth=0.5 
																	   borderbottomstyle=dotted borderbottomcolor=black borderbottomwidth=0.5}
														%if &colour.=N %then style (header)={borderbottomcolor=black borderbottomwidth=0.5};;
%END;

%IF %UPCASE(&column.)=SUMMARY %THEN %DO;
define Npct2345 / "Grade" "^-2n ^{unicode '2265'x}3" "^-2nN (%)" style(column)={just=center cellwidth=&gradewidth.cm 
																		borderbottomstyle=dotted borderbottomcolor=black borderbottomwidth=0.5
																		borderrightcolor=white borderrightwidth=0.5 cellwidth=&gradewidth.cm
																		font_face=calibri fontsize=&fontsize.pt}
							 							style (header)={%if &colour.=Y %then borderrightcolor=cxE0E0E0;
																		%if &colour.=N %then borderrightcolor=white borderbottomcolor=black borderbottomwidth=0.5; borderrightwidth=0.5 };
define Npct2ALL / "Grade" "^-2n ^{unicode '2265'x}1" "^-2nN (%)"  style(column)={just=center cellwidth=&gradewidth.cm  
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
define Npct31 / "Grade" "^-2n1" "^-2nN (%)" style (column)={just=center borderbottomstyle=dotted borderbottomcolor=black borderbottomwidth=0.5 cellwidth=&gradewidth.cm font_face=calibri fontsize=&fontsize.pt}
						  				style (header)={%if &colour.=Y %then borderrightcolor=cxE0E0E0;
														%if &colour.=N %then borderrightcolor=white borderbottomcolor=black borderbottomwidth=0.5; borderrightwidth=0.5 };
define Npct32 / "Grade" "^-2n2" "^-2nN (%)" style (column)={just=center borderbottomstyle=dotted borderbottomcolor=black borderbottomwidth=0.5 cellwidth=&gradewidth.cm font_face=calibri fontsize=&fontsize.pt}
						  				style (header)={%if &colour.=Y %then borderrightcolor=cxE0E0E0;
														%if &colour.=N %then borderrightcolor=white borderbottomcolor=black borderbottomwidth=0.5; borderrightwidth=0.5 };
define Npct33 / "Grade" "^-2n3" "^-2nN (%)" style (column)={just=center borderbottomstyle=dotted borderbottomcolor=black borderbottomwidth=0.5 cellwidth=&gradewidth.cm font_face=calibri fontsize=&fontsize.pt}
						  				style (header)={%if &colour.=Y %then borderrightcolor=cxE0E0E0;
														%if &colour.=N %then borderrightcolor=white borderbottomcolor=black borderbottomwidth=0.5; borderrightwidth=0.5 };
define Npct34 / "Grade" "^-2n4" "^-2nN (%)" style (column)={just=center borderbottomstyle=dotted borderbottomcolor=black borderbottomwidth=0.5 cellwidth=&gradewidth.cm font_face=calibri fontsize=&fontsize.pt}
						  				style (header)={%if &colour.=Y %then borderrightcolor=cxE0E0E0;
														%if &colour.=N %then borderrightcolor=white borderbottomcolor=black borderbottomwidth=0.5; borderrightwidth=0.5 };
%if &anyGR5.=Y %then  %do;
	define Npct35 / "Grade" "^-2n5" "^-2nN (%)" style (column)={just=center borderbottomstyle=dotted borderbottomcolor=black borderbottomwidth=0.5 cellwidth=&gradewidth.cm font_face=calibri fontsize=&fontsize.pt}
											style (header)={%if &colour.=Y %then borderrightcolor=cxE0E0E0;
														    %if &colour.=N %then borderrightcolor=white borderbottomcolor=black borderbottomwidth=0.5; 
															borderrightwidth=0.5 };
%end;
%if &anyGR5.=N %then define Npct35 /noprint;;

define Npct3345 / "Grade" "^-2n ^{unicode '2265'x}3" "^-2nN (%)" style(column)={just=center cellwidth=&gradewidth.cm 
																	   %if &colour.=Y %then background=whitesmoke borderrightcolor=whitesmoke;
																	   %if &colour.=N %then background=white borderrightcolor=white;
																		borderbottomstyle=dotted borderbottomcolor=black borderbottomwidth=0.5
																		borderrightwidth=0.5 cellwidth=&gradewidth.cm
																		font_face=calibri fontsize=&fontsize.pt}
							 							style (header)={%if &colour.=Y %then borderrightcolor=cxE0E0E0;
																		%if &colour.=N %then borderrightcolor=white borderbottomcolor=black borderbottomwidth=0.5; borderrightwidth=0.5 };
define Npct3ALL / "Grade" "^-2n ^{unicode '2265'x}1" "^-2nN (%)" style(column)={just=center cellwidth=&gradewidth.cm 
																		%if &colour.=Y %then background=whitesmoke;
																		%if &colour.=N %then background=white; font_face=calibri fontsize=&fontsize.pt
																	   borderrightcolor=black borderrightwidth=0.5 cellwidth=&gradewidth.cm
																	   borderbottomstyle=dotted borderbottomcolor=black borderbottomwidth=0.5}
														%if &colour.=N %then style (header)={borderbottomcolor=black borderbottomwidth=0.5};;
	%END;

	%IF %UPCASE(&column.)=SEVERE %THEN %DO;
define Npct33 / "Grade" "^-2n3" "^-2nN (%)" style (column)={just=center borderbottomstyle=dotted borderbottomcolor=black borderbottomwidth=0.5 cellwidth=&gradewidth.cm font_face=calibri fontsize=&fontsize.pt}
						  				 style (header)={%if &colour.=Y %then borderrightcolor=cxE0E0E0;
														 %if &colour.=N %then borderrightcolor=white borderbottomcolor=black borderbottomwidth=0.5; borderrightwidth=0.5};
define Npct34 / "Grade" "^-2n4" "^-2nN (%)" style (column)={just=center borderbottomstyle=dotted borderbottomcolor=black borderbottomwidth=0.5 cellwidth=&gradewidth.cm font_face=calibri fontsize=&fontsize.pt}
						  				 style (header)={%if &colour.=Y %then borderrightcolor=cxE0E0E0;
														 %if &colour.=N %then borderrightcolor=white borderbottomcolor=black borderbottomwidth=0.5; borderrightwidth=0.5};
%if &anyGR5.=Y %then  %do;
define Npct35 / "Grade" "^-2n5" "^-2nN (%)" style (column)={just=center borderbottomstyle=dotted borderbottomcolor=black borderbottomwidth=0.5 cellwidth=&gradewidth.cm font_face=calibri fontsize=&fontsize.pt}
									     style (header)={%if &colour.=Y %then borderrightcolor=cxE0E0E0;
											 		     %if &colour.=N %then borderrightcolor=white borderbottomcolor=black borderbottomwidth=0.5; 
														borderrightwidth=0.5};
%end;
%if &anyGR5.=N %then define Npct35 /noprint;;

define Npct3345 / "Grade" "^-2n ^{unicode '2265'x}3" "^-2nN (%)" style(column)={just=center cellwidth=&gradewidth.cm 
																	   %if &colour.=Y %then background=whitesmoke;
																	   %if &colour.=N %then background=white;
																	   font_face=calibri fontsize=&fontsize.pt
															 		   borderrightcolor=black borderrightwidth=0.5
																	   borderbottomstyle=dotted borderbottomcolor=black borderbottomwidth=0.5}
														%if &colour.=N %then style (header)={borderbottomcolor=black borderbottomwidth=0.5};;
%END;

	%IF %UPCASE(&column.)=POOLED %THEN %DO;
define Npct312 / "Grade" "^-2n1/2" "^-2nN (%)" style (column)={just=center borderbottomstyle=dotted borderbottomcolor=black borderbottomwidth=0.5 cellwidth=&gradewidth.cm font_face=calibri fontsize=&fontsize.pt}
						  			 style (header)={%if &colour.=Y %then borderrightcolor=cxE0E0E0;
													 %if &colour.=N %then borderrightcolor=white borderbottomcolor=black borderbottomwidth=0.5; borderrightwidth=0.5};
define Npct334 / "Grade" "^-2n3/4" "^-2nN (%)" style (column)={just=center borderbottomstyle=dotted borderbottomcolor=black borderbottomwidth=0.5 cellwidth=&gradewidth.cm font_face=calibri fontsize=&fontsize.pt}
						  			 style (header)={%if &colour.=Y %then borderrightcolor=cxE0E0E0;
													 %if &colour.=N %then borderrightcolor=white borderbottomcolor=black borderbottomwidth=0.5; borderrightwidth=0.5};
%if &anyGR5.=Y %then %do;
define Npct35 / "Grade" "^-2n5" "^-2nN (%)" style (column)={just=center borderbottomstyle=dotted borderbottomcolor=black borderbottomwidth=0.5 cellwidth=&gradewidth.cm font_face=calibri fontsize=&fontsize.pt}
	 								     style (header)={%if &colour.=Y %then borderrightcolor=cxE0E0E0;
													     %if &colour.=N %then borderrightcolor=white borderbottomcolor=black borderbottomwidth=0.5;
														  borderrightwidth=0.5};
%end;
%if &anyGR5.=N %then define Npct35 /noprint;;

define Npct3345 / "Grade" "^-2n ^{unicode '2265'x}3" "^-2nN (%)" style(column)={just=center cellwidth=&gradewidth.cm 
													   				   %if &colour.=Y %then background=whitesmoke borderrightcolor=whitesmoke;
																	   %if &colour.=N %then background=white borderrightcolor=white;
																		borderbottomstyle=dotted borderbottomcolor=black borderbottomwidth=0.5
																		borderrightwidth=0.5 font_face=calibri fontsize=&fontsize.pt}
							 							style (header)={%if &colour.=Y %then borderrightcolor=cxE0E0E0;
																		%if &colour.=N %then borderrightcolor=white borderbottomcolor=black borderbottomwidth=0.5; borderrightwidth=0.5};
define Npct3ALL / "Grade" "^-2n ^{unicode '2265'x}1" "^-2nN (%)" style(column)={just=center cellwidth=&gradewidth.cm 
																		%if &colour.=Y %then background=whitesmoke;
																		%if &colour.=N %then background=white;
																		font_face=calibri fontsize=&fontsize.pt
																		borderrightcolor=black borderrightwidth=0.5 
																		borderbottomstyle=dotted borderbottomcolor=black borderbottomwidth=0.5}
														%if &colour.=N %then style (header)={borderbottomcolor=black borderbottomwidth=0.5};;
%END;

	%IF %UPCASE(&column.)=SUMMARY %THEN %DO;
define Npct3345 / "Grade" "^-2n ^{unicode '2265'x}3" "^-2nN (%)" style(column)={just=center cellwidth=&gradewidth.cm 
																		borderbottomstyle=dotted borderbottomcolor=black borderbottomwidth=0.5
																		borderrightcolor=white borderrightwidth=0.5 cellwidth=&gradewidth.cm
																		font_face=calibri fontsize=&fontsize.pt}
							 							style (header)={%if &colour.=Y %then borderrightcolor=cxE0E0E0;
																		%if &colour.=N %then borderrightcolor=white borderbottomcolor=black borderbottomwidth=0.5; borderrightwidth=0.5};
define Npct3ALL / "Grade" "^-2n ^{unicode '2265'x}1" "^-2nN (%)"  style(column)={just=center cellwidth=&gradewidth.cm  
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
define Npct31 / "Grade" "^-2n1" "^-2nN (%)" style (column)={just=center borderbottomstyle=dotted borderbottomcolor=black borderbottomwidth=0.5 cellwidth=&gradewidth.cm font_face=calibri fontsize=&fontsize.pt}
						  				style (header)={%if &colour.=Y %then borderrightcolor=cxE0E0E0;
														%if &colour.=N %then borderrightcolor=white borderbottomcolor=black borderbottomwidth=0.5; borderrightwidth=0.5};
define Npct32 / "Grade" "^-2n2" "^-2nN (%)" style (column)={just=center borderbottomstyle=dotted borderbottomcolor=black borderbottomwidth=0.5 cellwidth=&gradewidth.cm font_face=calibri fontsize=&fontsize.pt}
						  				style (header)={%if &colour.=Y %then borderrightcolor=cxE0E0E0;
														%if &colour.=N %then borderrightcolor=white borderbottomcolor=black borderbottomwidth=0.5; borderrightwidth=0.5};
define Npct33 / "Grade" "^-2n3" "^-2nN (%)" style (column)={just=center borderbottomstyle=dotted borderbottomcolor=black borderbottomwidth=0.5 cellwidth=&gradewidth.cm font_face=calibri fontsize=&fontsize.pt}
						  				style (header)={%if &colour.=Y %then borderrightcolor=cxE0E0E0;
														%if &colour.=N %then borderrightcolor=white borderbottomcolor=black borderbottomwidth=0.5; borderrightwidth=0.5};
define Npct34 / "Grade" "^-2n4" "^-2nN (%)" style (column)={just=center borderbottomstyle=dotted borderbottomcolor=black borderbottomwidth=0.5 cellwidth=&gradewidth.cm font_face=calibri fontsize=&fontsize.pt}
						  				style (header)={%if &colour.=Y %then borderrightcolor=cxE0E0E0;
														%if &colour.=N %then borderrightcolor=white borderbottomcolor=black borderbottomwidth=0.5; borderrightwidth=0.5};
%if &anyGR5.=Y %then %do;
define Npct35 / "Grade" "^-2n5" "^-2nN (%)" style (column)={just=center borderbottomstyle=dotted borderbottomcolor=black borderbottomwidth=0.5 cellwidth=&gradewidth.cm font_face=calibri fontsize=&fontsize.pt}
  									    style (header)={%if &colour.=Y %then borderrightcolor=cxE0E0E0;
													    %if &colour.=N %then borderrightcolor=white borderbottomcolor=black borderbottomwidth=0.5;
														borderrightwidth=0.5};
%end;
%if &anyGR5.=N %then define Npct35 /noprint;;

define Npct3345 / "Grade" "^-2n ^{unicode '2265'x}3" "^-2nN (%)" style(column)={just=center cellwidth=&gradewidth.cm 
													    			   %if &colour.=Y %then background=whitesmoke borderrightcolor=whitesmoke ;
													    			   %if &colour.=N %then background=white borderrightcolor=white ;
																		borderbottomstyle=dotted borderbottomcolor=black borderbottomwidth=0.5
																		borderrightwidth=0.5 cellwidth=&gradewidth.cm
																		font_face=calibri fontsize=&fontsize.pt}
							 							style (header)={ %if &colour.=Y %then borderrightcolor=cxE0E0E0;
																		 %if &colour.=N %then borderrightcolor=white borderbottomcolor=black borderbottomwidth=0.5; borderrightwidth=0.5};
define Npct3ALL / "Grade" "^-2n ^{unicode '2265'x}1" "^-2nN (%)" style(column)={just=center cellwidth=&gradewidth.cm 
																	   %if &colour.=Y %then background=whitesmoke;
																	   %if &colour.=N %then background=white;
																	   font_face=calibri fontsize=&fontsize.pt
											 						   borderrightcolor=black borderrightwidth=0.5 cellwidth=&gradewidth.cm
											 						   borderbottomstyle=dotted borderbottomcolor=black borderbottomwidth=0.5}
														%if &colour.=N %then style (header)={borderbottomcolor=black borderbottomwidth=0.5};;



*Arm4*;
define Npct41 / "Grade" "^-2n1" "^-2nN (%)" style (column)={just=center borderbottomstyle=dotted borderbottomcolor=black borderbottomwidth=0.5 cellwidth=&gradewidth.cm font_face=calibri fontsize=&fontsize.pt}
						  				style (header)={%if &colour.=Y %then borderrightcolor=cxE0E0E0;
														%if &colour.=N %then borderrightcolor=white borderbottomcolor=black borderbottomwidth=0.5; borderrightwidth=0.5};
define Npct42 / "Grade" "^-2n2" "^-2nN (%)" style (column)={just=center borderbottomstyle=dotted borderbottomcolor=black borderbottomwidth=0.5 cellwidth=&gradewidth.cm font_face=calibri fontsize=&fontsize.pt}
						  				style (header)={%if &colour.=Y %then borderrightcolor=cxE0E0E0;
														%if &colour.=N %then borderrightcolor=white borderbottomcolor=black borderbottomwidth=0.5; borderrightwidth=0.5};
define Npct43 / "Grade" "^-2n3" "^-2nN (%)" style (column)={just=center borderbottomstyle=dotted borderbottomcolor=black borderbottomwidth=0.5 cellwidth=&gradewidth.cm font_face=calibri fontsize=&fontsize.pt}
						  				style (header)={%if &colour.=Y %then borderrightcolor=cxE0E0E0;
														%if &colour.=N %then borderrightcolor=white borderbottomcolor=black borderbottomwidth=0.5; borderrightwidth=0.5};
define Npct44 / "Grade" "^-2n4" "^-2nN (%)" style (column)={just=center borderbottomstyle=dotted borderbottomcolor=black borderbottomwidth=0.5 cellwidth=&gradewidth.cm font_face=calibri fontsize=&fontsize.pt}
						  				style (header)={%if &colour.=Y %then borderrightcolor=cxE0E0E0;
														%if &colour.=N %then borderrightcolor=white borderbottomcolor=black borderbottomwidth=0.5; borderrightwidth=0.5};
%if &anyGR5.=Y %then %do;
define Npct45 / "Grade" "^-2n5" "^-2nN (%)" style (column)={just=center borderbottomstyle=dotted borderbottomcolor=black borderbottomwidth=0.5 cellwidth=&gradewidth.cm font_face=calibri fontsize=&fontsize.pt}
    									style (header)={%if &colour.=Y %then borderrightcolor=cxE0E0E0;
													    %if &colour.=N %then borderrightcolor=white borderbottomcolor=black borderbottomwidth=0.5; 
														borderrightwidth=0.5};
%end;
%if &anyGR5.=N %then define Npct45 /noprint;;

define Npct4345 / "Grade" "^-2n ^{unicode '2265'x}3" "^-2nN (%)" style(column)={just=center cellwidth=&gradewidth.cm 
																	   %if &colour.=Y %then background=whitesmoke borderrightcolor=whitesmoke;
																	   %if &colour.=N %then background=white borderrightcolor=white;
																		borderbottomstyle=dotted borderbottomcolor=black borderbottomwidth=0.5
																		borderrightwidth=0.5 cellwidth=&gradewidth.cm
																		font_face=calibri fontsize=&fontsize.pt}
							 							style (header)={%if &colour.=Y %then borderrightcolor=cxE0E0E0;
																		%if &colour.=N %then borderrightcolor=white borderbottomcolor=black borderbottomwidth=0.5; borderrightwidth=0.5};
define Npct4ALL / "Grade" "^-2n ^{unicode '2265'x}1" "^-2nN (%)" style(column)={just=center cellwidth=&gradewidth.cm 
																		%if &colour.=Y %then background=whitesmoke;
																		%if &colour.=N %then background=white; 
																		 font_face=calibri fontsize=&fontsize.pt
																		 borderrightcolor=black borderrightwidth=0.5 cellwidth=&gradewidth.cm
																		 borderbottomstyle=dotted borderbottomcolor=black borderbottomwidth=0.5}
														%if &colour.=N %then style (header)={borderbottomcolor=black borderbottomwidth=0.5};;

%END;

	%IF %UPCASE(&column.)=SEVERE %THEN %DO;
define Npct33 / "Grade" "^-2n3" "^-2nN (%)" style (column)={just=center borderbottomstyle=dotted borderbottomcolor=black borderbottomwidth=0.5 cellwidth=&gradewidth.cm font_face=calibri fontsize=&fontsize.pt}
						  				style (header)={%if &colour.=Y %then borderrightcolor=cxE0E0E0;
														%if &colour.=N %then borderrightcolor=white borderbottomcolor=black borderbottomwidth=0.5; borderrightwidth=0.5};
define Npct34 / "Grade" "^-2n4" "^-2nN (%)" style (column)={just=center borderbottomstyle=dotted borderbottomcolor=black borderbottomwidth=0.5 cellwidth=&gradewidth.cm font_face=calibri fontsize=&fontsize.pt}
						  				style (header)={%if &colour.=Y %then borderrightcolor=cxE0E0E0;
														%if &colour.=N %then borderrightcolor=white borderbottomcolor=black borderbottomwidth=0.5; borderrightwidth=0.5};
%if &anyGR5.=Y %then %do;
define Npct35 / "Grade" "^-2n5" "^-2nN (%)" style (column)={just=center borderbottomstyle=dotted borderbottomcolor=black borderbottomwidth=0.5 cellwidth=&gradewidth.cm font_face=calibri fontsize=&fontsize.pt}
  									     style (header)={%if &colour.=Y %then borderrightcolor=cxE0E0E0;
														 %if &colour.=N %then borderrightcolor=white borderbottomcolor=black borderbottomwidth=0.5;
														 borderrightwidth=0.5};
%end;
%if &anyGR5.=N %then define Npct35 /noprint;;

define Npct3345 / "Grade" "^-2n ^{unicode '2265'x}3" "^-2nN (%)" style(column)={just=center cellwidth=&gradewidth.cm 
																	   %if &colour.=Y %then background=whitesmoke;
																	   %if &colour.=N %then background=white; 
																		font_face=calibri fontsize=&fontsize.pt
															 			borderrightcolor=black borderrightwidth=0.5
																		borderbottomstyle=dotted borderbottomcolor=black borderbottomwidth=0.5}
														%if &colour.=N %then style (header)={borderbottomcolor=black borderbottomwidth=0.5};;

define Npct43 / "Grade" "^-2n3" "^-2nN (%)" style (column)={just=center borderbottomstyle=dotted borderbottomcolor=black borderbottomwidth=0.5 cellwidth=&gradewidth.cm font_face=calibri fontsize=&fontsize.pt}
						  				 style (header)={%if &colour.=Y %then borderrightcolor=cxE0E0E0;
														 %if &colour.=N %then borderrightcolor=white borderbottomcolor=black borderbottomwidth=0.5; borderrightwidth=0.5};
define Npct44 / "Grade" "^-2n4" "^-2nN (%)" style (column)={just=center borderbottomstyle=dotted borderbottomcolor=black borderbottomwidth=0.5 cellwidth=&gradewidth.cm font_face=calibri fontsize=&fontsize.pt}
						  				style (header)={%if &colour.=Y %then borderrightcolor=cxE0E0E0;
														%if &colour.=N %then borderrightcolor=white borderbottomcolor=black borderbottomwidth=0.5; borderrightwidth=0.5};
%if &anyGR5.=Y %then %do;
define Npct45 / "Grade" "^-2n5" "^-2nN (%)" style (column)={just=center borderbottomstyle=dotted borderbottomcolor=black borderbottomwidth=0.5 cellwidth=&gradewidth.cm font_face=calibri fontsize=&fontsize.pt}
						  				 style (header)={%if &colour.=Y %then borderrightcolor=cxE0E0E0;
														 %if &colour.=N %then borderrightcolor=white borderbottomcolor=black borderbottomwidth=0.5; 
														 borderrightwidth=0.5};
%end;
%if &anyGR5.=N %then define Npct45 /noprint;;

define Npct4345 / "Grade" "^-2n ^{unicode '2265'x}3" "^-2nN (%)" style(column)={just=center cellwidth=&gradewidth.cm 
																	   %if &colour.=Y %then	background=whitesmoke;
																	   %if &colour.=N %then	background=white; 
																	   font_face=calibri fontsize=&fontsize.pt
															 			borderrightcolor=black borderrightwidth=0.5
																		borderbottomstyle=dotted borderbottomcolor=black borderbottomwidth=0.5}
														%if &colour.=N %then style (header)={borderbottomcolor=black borderbottomwidth=0.5};;
%END;

	%IF %UPCASE(&column.)=POOLED %THEN %DO;
define Npct312 / "Grade" "^-2n1/2" "^-2nN (%)" style (column)={just=center borderbottomstyle=dotted borderbottomcolor=black borderbottomwidth=0.5 cellwidth=&gradewidth.cm font_face=calibri fontsize=&fontsize.pt}
						  				style (header)={%if &colour.=Y %then borderrightcolor=cxE0E0E0;
														%if &colour.=N %then borderrightcolor=white borderbottomcolor=black borderbottomwidth=0.5; borderrightwidth=0.5};
define Npct334 / "Grade" "^-2n3/4" "^-2nN (%)" style (column)={just=center borderbottomstyle=dotted borderbottomcolor=black borderbottomwidth=0.5 cellwidth=&gradewidth.cm font_face=calibri fontsize=&fontsize.pt}
						  				style (header)={%if &colour.=Y %then borderrightcolor=cxE0E0E0;
														%if &colour.=N %then borderrightcolor=white borderbottomcolor=black borderbottomwidth=0.5; borderrightwidth=0.5};
%if &anyGR5.=Y %then %do;
define Npct35 / "Grade" "^-2n5" "^-2nN (%)" style (column)={just=center borderbottomstyle=dotted borderbottomcolor=black borderbottomwidth=0.5 cellwidth=&gradewidth.cm font_face=calibri fontsize=&fontsize.pt}
						  				 style (header)={%if &colour.=Y %then borderrightcolor=cxE0E0E0;
														 %if &colour.=N %then borderrightcolor=white borderbottomcolor=black borderbottomwidth=0.5;
														 borderrightwidth=0.5};
%end;
%if &anyGR5.=N %then define Npct35 /noprint;;

define Npct3345 / "Grade" "^-2n ^{unicode '2265'x}3" "^-2nN (%)" style(column)={just=center cellwidth=&gradewidth.cm 
																	   %if &colour.=Y %then background=whitesmoke borderrightcolor=whitesmoke;
																	   %if &colour.=N %then background=white borderrightcolor=white;
																		borderbottomstyle=dotted borderbottomcolor=black borderbottomwidth=0.5
																		borderrightwidth=0.5 font_face=calibri fontsize=&fontsize.pt}
						  								style (header)={%if &colour.=Y %then borderrightcolor=cxE0E0E0;
																		%if &colour.=N %then borderrightcolor=white borderbottomcolor=black borderbottomwidth=0.5; borderrightwidth=0.5};
define Npct3ALL / "Grade" "^-2n ^{unicode '2265'x}1" "^-2nN (%)" style(column)={just=center cellwidth=&gradewidth.cm 
																		%if &colour.=Y %then background=whitesmoke;
																		%if &colour.=N %then background=white;
																		font_face=calibri fontsize=&fontsize.pt
																		 borderrightcolor=black borderrightwidth=0.5 
																		 borderbottomstyle=dotted borderbottomcolor=black borderbottomwidth=0.5}
														%if &colour.=N %then style (header)={borderbottomcolor=black borderbottomwidth=0.5};;

define Npct412 / "Grade" "^-2n1/2" "^-2nN (%)" style (column)={just=center borderbottomstyle=dotted borderbottomcolor=black borderbottomwidth=0.5 cellwidth=&gradewidth.cm font_face=calibri fontsize=&fontsize.pt}
						  				style (header)={%if &colour.=Y %then borderrightcolor=cxE0E0E0;
														%if &colour.=N %then borderrightcolor=white borderbottomcolor=black borderbottomwidth=0.5; borderrightwidth=0.5};
define Npct434 / "Grade" "^-2n3/4" "^-2nN (%)" style (column)={just=center borderbottomstyle=dotted borderbottomcolor=black borderbottomwidth=0.5 cellwidth=&gradewidth.cm font_face=calibri fontsize=&fontsize.pt}
						  				style (header)={%if &colour.=Y %then borderrightcolor=cxE0E0E0;
														%if &colour.=N %then borderrightcolor=white borderbottomcolor=black borderbottomwidth=0.5; borderrightwidth=0.5};
%if &anyGR5.=Y %then %do;
define Npct45 / "Grade" "^-2n5" "^-2nN (%)" style (column)={just=center borderbottomstyle=dotted borderbottomcolor=black borderbottomwidth=0.5 cellwidth=&gradewidth.cm font_face=calibri fontsize=&fontsize.pt}
						  				 style (header)={%if &colour.=Y %then borderrightcolor=cxE0E0E0;
														 %if &colour.=N %then borderrightcolor=white borderbottomcolor=black borderbottomwidth=0.5;
														 borderrightwidth=0.5};
%end;
%if &anyGR5.=N %then define Npct45 /noprint;;

define Npct4345 / "Grade" "^-2n ^{unicode '2265'x}3" "^-2nN (%)" style(column)={just=center cellwidth=&gradewidth.cm 
																	  %if &colour.=Y %then background=whitesmoke borderrightcolor=whitesmoke;
																	  %if &colour.=N %then background=white borderrightcolor=white;
																		borderbottomstyle=dotted borderbottomcolor=black borderbottomwidth=0.5
																		borderrightwidth=0.5 font_face=calibri fontsize=&fontsize.pt}
							 							style (header)={%if &colour.=Y %then borderrightcolor=cxE0E0E0;
																		%if &colour.=N %then borderrightcolor=white borderbottomcolor=black borderbottomwidth=0.5; borderrightwidth=0.5};
define Npct4ALL / "Grade" "^-2n ^{unicode '2265'x}1" "^-2nN (%)" style(column)={just=center cellwidth=&gradewidth.cm 
																	   %if &colour.=Y %then background=whitesmoke;
																	   %if &colour.=N %then background=white;
																	   font_face=calibri fontsize=&fontsize.pt
																	   borderrightcolor=black borderrightwidth=0.5 
																	   borderbottomstyle=dotted borderbottomcolor=black borderbottomwidth=0.5}
														%if &colour.=N %then style (header)={borderbottomcolor=black borderbottomwidth=0.5};;

	%END;

	%IF %UPCASE(&column.)=SUMMARY %THEN %DO;
define Npct3345 / "Grade" "^-2n ^{unicode '2265'x}3" "^-2nN (%)" style(column)={just=center cellwidth=&gradewidth.cm 
																		borderbottomstyle=dotted borderbottomcolor=black borderbottomwidth=0.5
																		borderrightcolor=white borderrightwidth=0.5 cellwidth=&gradewidth.cm
																		font_face=calibri fontsize=&fontsize.pt}
						  								style (header)={%if &colour.=Y %then borderrightcolor=cxE0E0E0;
																		%if &colour.=N %then borderrightcolor=white borderbottomcolor=black borderbottomwidth=0.5; borderrightwidth=0.5};
define Npct3ALL / "Grade" "^-2n ^{unicode '2265'x}1" "^-2nN (%)"  style(column)={just=center cellwidth=&gradewidth.cm 
																		 borderrightcolor=black borderrightwidth=0.5 font_face=calibri fontsize=&fontsize.pt
																		 borderbottomstyle=dotted borderbottomcolor=black borderbottomwidth=0.5}
							  							style (header)={just=center borderrightcolor=black borderrightwidth=0.5 cellwidth=&gradewidth.cm
																		%if &colour.=N %then borderbottomcolor=black borderbottomwidth=0.5;};
define Npct4345 / "Grade" "^-2n ^{unicode '2265'x}3" "^-2nN (%)" style(column)={just=center cellwidth=&gradewidth.cm
																		borderbottomstyle=dotted borderbottomcolor=black borderbottomwidth=0.5
																		borderrightcolor=white borderrightwidth=0.5 cellwidth=&gradewidth.cm
																		font_face=calibri fontsize=&fontsize.pt}
						  								style (header)={%if &colour.=Y %then borderrightcolor=cxE0E0E0;
																		%if &colour.=N %then borderrightcolor=white borderbottomcolor=black borderbottomwidth=0.5; borderrightwidth=0.5};
define Npct4ALL / "Grade" "^-2n ^{unicode '2265'x}1" "^-2nN (%)"  style(column)={just=center cellwidth=&gradewidth.cm  
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



/*----------------------------------------------------
   STEP 6 : CREATE ASSOCIATED GRAPHS
------------------------------------------------------*/

%IF (&graph.^= "N") %THEN %DO;

**Reset ODS option for PDF*;
%if %UPCASE(&Display.)=PDF %THEN %DO;
	option orientation=portrait;
%END;

******************************************************************************************************* 
Define local variables for options which vary according to type of output required (value of DISPLAY)
*******************************************************************************************************;

%LOCAL imagefmt imagename imagefmt ;

%IF %UPCASE(&DISPLAY.)=SASEMF OR %UPCASE(&DISPLAY.)=WORD OR %UPCASE(&DISPLAY.)=POWERPOINT %THEN %LET DISPLAY=wmf;
    

	/*Use the extension mentionned in display parameter if it is not mentioned in the outpu path of the graph.*/

	*SOC GRAPH
	-----------;
	%IF %length(&SOCgrph.)>0 %then %do;
		%if %index(%substr(&SOCgrph.,%sysfunc(find(&SOCgrph.,\,-%length(&SOCgrph))) + 1),.) = 0 %then %do;
				%LET OutFile2 = %CMPRES(%str(&SOCgrph.).&DISPLAY.);
		%end;
		%else %do;
			%if %eval(%length(%substr(&SOCgrph.,%sysfunc(find(&SOCgrph.,\,-%length(&SOCgrph))) + 1)) - 
			%index(%substr(&SOCgrph.,%sysfunc(find(&SOCgrph.,\,-%length(&SOCgrph))) + 1),.)) = 3 %then %do;
					%let OutFile2 = %CMPRES(%substr(&SOCgrph.,1,%sysfunc(find(&SOCgrph.,.,-%length(&SOCgrph))) - 1).&DISPLAY.);
			%end;
			%else %do;
				%put ERROR: The output file name for SOC Graph includes extra dot (.) besides the one for extension;
				%goto stop_exec;
			%end;
		%end; 

		**Add option for ods graphics for SOCgrph**;
		%LET imagename6=%scan(%str(&outfile2.),-1,"\");
		%LET imagename7=%scan(&imagename6.,1,.);
		%LET gpath4=%SYSFUNC(TRANWRD(%str(&outfile2.),&imagename6.,));

	%END;

	*FOREST PLOT
	------------;
	%IF %length(&pct_forest.)>0 %then %do;
		%if %index(%substr(&pct_forest.,%sysfunc(find(&pct_forest.,\,-%length(&pct_forest))) + 1),.) = 0 %then %do;
			%LET OutFile3 = %CMPRES(%str(&pct_forest.).&DISPLAY.);
		%end;
		%else %do;
			%if %eval(%length(%substr(&pct_forest.,%sysfunc(find(&pct_forest.,\,-%length(&pct_forest))) + 1)) - 
			%index(%substr(&pct_forest.,%sysfunc(find(&pct_forest.,\,-%length(&pct_forest))) + 1),.)) = 3 %then %do;
				%let OutFile3 = %CMPRES(%substr(&pct_forest.,1,%sysfunc(find(&pct_forest.,.,-%length(&pct_forest))) - 1).&DISPLAY.);
			%end;
			%else %do;
				%put ERROR: The output file name for PCT_forest includes extra dot (.) besides the one for extension;
				%goto stop_exec;
			%end;
		%end; 

		**Add option for ods graphics for FOREST**;
		%LET imagename2=%scan(%str(&outfile3.),-1,"\");
		%LET imagename3=%scan(&imagename2.,1,.);
		%LET gpath2=%SYSFUNC(TRANWRD(%str(&outfile3.),&imagename2.,));
	%END;

	*PCT_PLOT
	-----------;
	%IF %length(&pct_plot.)>0 %then %do;
		%if %index(%substr(&pct_plot.,%sysfunc(find(&pct_plot.,\,-%length(&pct_plot))) + 1),.) = 0 %then %do;
			%LET OutFile4 = %CMPRES(%str(&pct_plot.).&DISPLAY.);
		%end;
		%else %do;
			%if %eval(%length(%substr(&pct_plot.,%sysfunc(find(&pct_plot.,\,-%length(&pct_plot))) + 1)) - 
			%index(%substr(&pct_plot.,%sysfunc(find(&pct_plot.,\,-%length(&pct_plot))) + 1),.)) = 3 %then %do;
				%let OutFile4 = %CMPRES(%substr(&pct_plot.,1,%sysfunc(find(&pct_plot.,.,-%length(&pct_plot))) - 1).&DISPLAY.);
			%end;
			%else %do;
				%put ERROR: The output file name for pct_plot includes extra dot (.) besides the one for extension;
				%goto stop_exec;
			%end;
		%end; 

		**Add option for ods graphics for pct_plot**;
		%LET imagename4=%scan(%str(&outfile4.),-1,"\");
		%LET imagename5=%scan(&imagename4.,1,.);
		%LET gpath3=%SYSFUNC(TRANWRD(%str(&outfile4.),&imagename4.,));

	%END;

	*PYRAMID
	-----------;
	%IF %length(&pyramid.)>0 %then %do;
		%if %index(%substr(&pyramid.,%sysfunc(find(&pyramid.,\,-%length(&pyramid))) + 1),.) = 0 %then %do;
			%LET OutFile5 = %CMPRES(%str(&pyramid.).&DISPLAY.);
		%end;
		%else %do;
			%if %eval(%length(%substr(&pyramid.,%sysfunc(find(&pyramid.,\,-%length(&pyramid))) + 1)) - 
			%index(%substr(&pyramid.,%sysfunc(find(&pyramid.,\,-%length(&pyramid))) + 1),.)) = 3 %then %do;
				%let OutFile5 = %CMPRES(%substr(&pyramid.,1,%sysfunc(find(&pyramid.,.,-%length(&pyramid))) - 1).&DISPLAY.);
			%end;
			%else %do;
				%put ERROR: The output file name for pyramid includes extra dot (.) besides the one for extension;
				%goto stop_exec;
			%end;
		%end; 

		**Add option for ods graphics for Pyramid**;
		%LET imagename8=%scan(%str(&outfile5.),-1,"\");
		%LET imagename9=%scan(&imagename8.,1,.);
		%LET gpath5=%SYSFUNC(TRANWRD(%str(&outfile5.),&imagename8.,));

	%END;
	
      **Add option for ods graphics **;
		%IF &DISPLAY ^= TIF %THEN %LET imagefmt=&display.;
		%if &DISPLAY=TIF %THEN %LET imagefmt=TIFF;


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
 ***** Graph Number 1: SOCgrph          ******
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
ODS listing close;
ODS pdf close;
ods listing;
%let width=26;
%let height=18;  


%IF %UPCASE(&DISPLAY.)=PDF %THEN %DO ;
 	option orientation=landscape nodate;
	ods pdf file="&Outfile2." style=mytemplate %if &editGrph.=Y %then sge=on;;
%END;

	ods graphics on/ reset reset=index border=off width=&width. cm height=&height. cm outputfmt=&imagefmt. imagename="&imagename7.";

%IF %UPCASE(&DISPLAY.)^=PDF %THEN %DO ;
ods listing gpath="&gpath4." style=mytemplate %if &editGrph.=Y %then sge=on;;
%END;

%IF (%LENGTH(&TitleSOCgrph.)>0) %THEN %DO ; Title j=c h=14pt "&TitleSOCgrph" ; %END;
%IF (%LENGTH(&TitleSOCgrph.)=0) %THEN %DO; 
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
%END;

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

*Check that the &graphcutoff parameter is not strict creating an empty dataset;
%nobs (dataset=__More5P_plot2);	
%if &_anyobs=N %then %do;
	%put ERROR: ------------------------------------------------------------------------------;
	%put ERROR: The GRPHCUTOFF parameter is set to high.                                      ;
	%put ERROR: Reminder: if GRAPH=ALL the default value of GRPHCUTOFF is set to 10%		  ;
	%put ERROR: Reminder: if GRAPH=SEVERE the default value of GRPHCUTOFF is set to 5%	      ;
	%put ERROR: No graph can be displayed. Macro ae_table stopped processing.				  ;
	%put ERROR: -------------------------------------------------------------------------------;
	%goto stop_exec;
%end;

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
		if (INDEX(UPCASE(MEDDRATERM),'OTHER,')>0 or INDEX(UPCASE(MEDDRATERM),' OTHER ')>0 
		    or INDEX(UPCASE(MEDDRATERM),': OTHER')>0 or INDEX(UPCASE(MEDDRATERM),':OTHER')>0)
	 	then MEDDRATERM=COMPRESS(LEFT(SOC_short))||" - "||"Other";
Length_medra=length(MEDDRATERM);
Short_medra=MEDDRATERM;
*For very long preferred term, the label can take the all place in the graph
=> create abbreviation: the first 3 letters of the first word and the first 4 letters of the second word.
We choose the first four letters of the second word because sometimes the first three of the second word can be the same;
if Length_medra>30 then do;
	*First word;
	if length(scan(MEDDRATERM,1)) >= 3 then do;
		Short_first=(COMPRESS(LEFT(substr(scan(MEDDRATERM,1),1,3))));
		short_first2=Short_first||".";
	end;
	else do;
		Short_first=(COMPRESS(LEFT(scan(MEDDRATERM,1))));
		short_first2=Short_first;
	end;
	*second word;
	if length(scan(MEDDRATERM,2)) >= 4 then do;
		Short_second=(COMPRESS(LEFT(substr(scan(MEDDRATERM,2),1,4))));
		short_second2=Short_second||".";
	end;
	else do;
		Short_second=(COMPRESS(LEFT(scan(MEDDRATERM,2))));
		short_second2=Short_second;
	end;
	*Create label;
	Short_Medra=trim(left(short_first2))||' '||trim(left(short_second2));
end;
%if &totarm.<3 %then %do;
	axis=trim(left(substr(SOC2,1,3)))||' - '||trim(left(Short_Medra));
		*If meddraterm contains OTHER then do not take two time the SOC short in the label;
		if (INDEX(UPCASE(MEDDRATERM),'OTHER')>0) then axis=MEDDRATERM;
%end;
%else %do;
	axis=Short_Medra;
%end;
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

proc sort data=__plot2_5;
by descending SOC_short descending MEDDRATERM;
run;



/*-------------------------------
   DISPLAY PCT_PLOT
---------------------------------*/

*Change size of symbol according to the display*;
%IF %UPCASE(&DISPLAY.)=PDF %THEN %LET size=4pt;;
%IF %UPCASE(&DISPLAY.)^=PDF %THEN %LET size=6pt;;

/*If the user only want to present the percentages of AEs without the forest: parmater=pct_plot*/
%IF (%length(&pct_plot)>0) %THEN %DO;
	%IF (%LENGTH(&TitlePctPlot.)>0) %THEN %DO; title "&TitlePctPlot."; %END;
	%IF (%LENGTH(&TitlePctPlot.)=0) %THEN %DO;
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
	%END;

%let height=26;
%let width=30;
ODS listing close;
ODS pdf close;
ODS listing;

%IF %UPCASE(&DISPLAY.)=PDF %THEN %DO ;
	%let height=32;
	%let width=26;
	option orientation=portrait;
	ods pdf file="&Outfile4." style=trt %if &editGrph.=Y %then sge=on;;
%END;

	ods graphics on/ reset reset=index border=off width=&width. cm height=&height. cm outputfmt=&imagefmt. imagename="&imagename5.";

%IF %UPCASE(&DISPLAY.)^=PDF %THEN %DO ;
ods listing gpath="&gpath3." style=trt %if &editGrph.=Y %then sge=on;;
%END;


%if %UPCASE(&graph.)=ALL %then %do;
proc sgplot data=__plot2_5;
Scatter X=PCT_GR_ALL Y=axis / markerattrs=(size=&size.) Group=&trt.;
xaxis label="Percent" VALUES=( 0 to &UpperAxis. by 5.0);
yaxis label="Adverse event" grid discreteorder=data;
format &trt. &trtformat.;
run;
%end;
%if %UPCASE(&graph.)=SEVERE %then %do;
proc sgplot data=__plot2_5;
Scatter X=PCT_GR_345 Y=axis / markerattrs=(size=&size.) Group=&trt. ;
xaxis label="Percent" VALUES=( 0 to &UpperAxis. by 5.0);
yaxis label="Adverse event" grid discreteorder=data;
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

/*-------------------------------
   DISPLAY FOREST_PLOT
---------------------------------*/
 %let maxlegendarea=40;
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
%IF (%LENGTH(&TitleForest.)>0) %THEN %DO; entrytitle "&TitleForest."; %END;
%IF (%LENGTH(&TitleForest.)=0) %THEN %DO;
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
%END;

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
ScatterPlot X=PCT_GR_ALL Y=axis / markerattrs=(size=&size. transparency=0.4) Group=&trt. NAME="SCATTER1";
%end;
%if %UPCASE(&graph.)=SEVERE %then %do;
ScatterPlot X=PCT_GR_345 Y=axis / markerattrs=(size=&size. transparency=0.4) Group=&trt. NAME="SCATTER1";
%end;
endlayout;

*** COLUMN 2: Presenting the RR for control arm verus arm2***;
%IF %INDEX(&where.,%str(&trt.))=0 or &show2.=Y %THEN %DO;
layout overlay / xaxisopts=(%if &totarm.<4 %then Label="RR (&IC.% CI)"; %else Label="  "; type=log 
							%if  &axisUpper.=Y %then logopts=(TICKVALUEPRIORITY=TRUE tickvaluelist=(.1 1 10 100));)
				 yaxisopts=( griddisplay=on display=none);
ScatterPlot X=RR2 Y=axis / XErrorUpper=U_RR2 XErrorLower=L_RR2 
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
ScatterPlot X=RR3 Y=axis /  primary=true XErrorUpper=U_RR3 XErrorLower=L_RR3 primary=true
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
%IF %INDEX(&where.,%str(&trt.)=0) or &show4.=Y %THEN %DO;
** COLUMN 4: Presenting the RR for control arm verus arm4***;
layout overlay / xaxisopts=(Label="  " type=log %if &axisUpper.=Y %then logopts=(TICKVALUEPRIORITY=TRUE tickvaluelist=(.1 1 10 100));)
				 yaxisopts=( griddisplay=on display=none);
ScatterPlot X=RR4 Y=axis / XErrorUpper=U_RR4 XErrorLower=L_RR4 
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
ODS listing close;
ODS pdf close;
ODS listing;

%IF %UPCASE(&DISPLAY.)=PDF %THEN %DO ;
	%LET height=32;
	%let width=19.72733; *Max for pdf*;
	%IF (%LENGTH(&GrphCutoff.)>0) and %UPCASE(&Graph.)=ALL %then %let height=40;;
	option orientation=portrait nodate;
	ods pdf file="&Outfile3." style=trt %if &editGrph.=Y %then sge=on;;
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

proc format;
	value $Grade
		PCT_GR_1= "Grade 1"
		PCT_GR_2= "Grade 2"
		PCT_GR_3= "Grade 3"
		PCT_GR_4= "Grade 4"
		PCT_GR_5= "Grade 5"
		PCT_GR_345="Grade 3/5"
		PCT_GR_ALL="Grade 1/5";
run;

** write a comment in the log in case of wrong macro call**;
%if (%length(&pyramid.)>0) and &totarm. ne 2 %THEN %DO;
	%put ERROR: The pyramid graph is only available for studies with two treatment arms.;
	%goto stop_exec;
%END;

%IF (%length(&pyramid.)>0) and &totarm.=2 %THEN %DO;

*Take into account the graphcutoff parameter;
data __Plot5 (keep=&trt. MEDDRATERM SOC2 PCT_GR: keep:) ;
set __sumall;
	if MEDDRATERM ne "*GENERAL TERM";
	%IF (%LENGTH(&GrphCutoff.)=0) %THEN %DO; **By default, present in the graph only AE with more than 5percent incidence*;
				if &trt.=1 then do;
				if PCT_GR_345>5.0 then keepSevere=1;
				if PCT_GR_ALL>10.0 then keepAll=1;
			end;
			if &trt.=2 then do;
				if PCT_GR_345>5.0 then keepSevere=1;
				if PCT_GR_ALL>10.0 then keepAll=1;
			end;
	%END;

	%IF (%LENGTH(&GrphCutoff.)>0) %THEN %DO; **Else, use the same treshold than the one defined by the user**;
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

*Check that the &graphcutoff parameter is not strict creating an empty dataset;
%nobs (dataset=__More5P);	
%if &_anyobs=N %then %do;
	%put ERROR: ------------------------------------------------------------------------------;
	%put ERROR: The GRPHCUTOFF parameter is set to high.                                      ;
	%put ERROR: Reminder: if GRAPH=ALL the default value of GRPHCUTOFF is set to 10%		  ;
	%put ERROR: Reminder: if GRAPH=SEVERE the default value of GRPHCUTOFF is set to 5%		  ;
	%put ERROR: No graph can be displayed. Macro ae_table stopped processing.				  ;
	%put ERROR: -------------------------------------------------------------------------------;
	%goto stop_exec;
%end;

%sort(data=__Plot5,var=MEDDRATERM) ;

data __Plot5_1;
merge __Plot5 __More5P (in=a);
by MEDDRATERM;
if a;
rename PCT_GR_1=Grade1
	   PCT_GR_2=Grade2
	   PCT_GR_3=Grade3
	   PCT_GR_4=Grade4
	   PCT_GR_5=Grade5;
run;


proc sort data=__Plot5_1 nodup; by &trt.; run;

data __Plot5_1;
length Short_first $3. Short_second $4.;
set __Plot5_1;
	MEDDRATERM=propcase(MEDDRATERM);
	if INDEX(UPCASE(MEDDRATERM),'OTHER')>0 then MEDDRATERM="Other";
	Short_medra=MEDDRATERM;
	*For very long preferred term, the label can take the all place in the graph
=> create abbreviation: the first 3 letters of the first word and the first 4 letters of the second word.
We choose the first four letters of the second word because sometimes the first three of the second word can be the same;
if Length(MEDDRATERM)>30 then do;
	*First word;
	if length(scan(MEDDRATERM,1)) >= 3 then do;
		Short_first=(COMPRESS(LEFT(substr(scan(MEDDRATERM,1),1,3))));
		short_first2=Short_first||".";
	end;
	else do;
		Short_first=(COMPRESS(LEFT(scan(MEDDRATERM,1))));
		short_first2=Short_first;
	end;
	*second word;
	if length(scan(MEDDRATERM,2)) >= 4 then do;
		Short_second=(COMPRESS(LEFT(substr(scan(MEDDRATERM,2),1,4))));
		short_second2=Short_second||".";
	end;
	else do;
		Short_second=(COMPRESS(LEFT(scan(MEDDRATERM,2))));
		short_second2=Short_second;
	end;
	*Create label;
	Short_Medra=trim(left(short_first2))||' '||trim(left(short_second2));
end;
	axis=trim(left(substr(SOC2,1,3)))||' - '||trim(left(Short_Medra));
	treatment=&trt.;
	format &trt. &trtformat.;
run;


%sort(data=__Plot5_1,var=axis) ;

/*Transpose dataset*/
proc transpose data=__Plot5_1 
				out=__tpyramid (rename=(_1=treatment1 _2=treatment2))
				name=GRADE;
by axis;
id treatment;
var Grade1 Grade2 Grade3 Grade4 Grade5 ;
run;

data __tpyramid;
set __tpyramid;
treatment1=treatment1*-1;
label grade=" ";
run;

proc format;
   picture positive 
     low-<0='000'
     0<-high='000';
run;

proc format;
	value rev_gr
		5="Grade 1"
		4="Grade 2"
		3="Grade 3"
		2="Grade 4"
		1="Grade 5";
run;

data __tpyramid;
set __tpyramid;
	if UPCASE(GRADE)="GRADE1" then reverse_gr=5;
	if UPCASE(GRADE)="GRADE2" then reverse_gr=4;
	if UPCASE(GRADE)="GRADE3" then reverse_gr=3;
	if UPCASE(GRADE)="GRADE4" then reverse_gr=2;
	if UPCASE(GRADE)="GRADE5" then reverse_gr=1;
format reverse_gr rev_gr.;
run;

*GIS, 2-11-2017: Remove grade 1 and 2 if GRAPH=SEVERE;
%if %UPCASE(&graph.)=SEVERE %THEN %DO;
data __tpyramid;
set __tpyramid;
	if UPCASE(GRADE)="GRADE1" or UPCASE(GRADE)="GRADE2" then delete;
run;
%END;

*Macro variable to get the maximum for X axis;
proc sql noprint;
create table __pyramid_final as
select*, sum (treatment1*-1) as tottrt1,
		 sum (treatment2) as tottrt2
from __tpyramid
group by axis;
quit;

proc sql noprint;
select  ROUND( max(max (tottrt1) , max (tottrt2)),10.0) into: upper
from __pyramid_final;
quit;
%put &upper.;		

** DISPLAY THE GRAPH
***********************;
**NOTE GIS: GraphData1= color for grade5. GraphData5= color for grade1**;
%if %UPCASE(&graph.)=ALL %then %do;
proc template;
  define style _grade2; 
    parent=styles.listing;
    style GraphData1 /
      ContrastColor=CXD9576E
      Color=CXD9576E; *cx00af00 CX66CC66;
    style GraphData2 /
      ContrastColor=CX336666
      Color=CX336666; *cxcfaf00 CXFFFF66;
   style GraphData3 /
      ContrastColor=CX93C6C6
      Color=CX93C6C6; *CXCC6600 CXFF9933;
   style GraphData4 /
      ContrastColor=CXB3E6E6
      Color=CXB3E6E6; *cxaf0000 CXFF0000;
  style GraphData5 /
      ContrastColor=CXDBFFFF
      Color=CXDBFFFF;

style GraphData6 /
      ContrastColor=CXD9576E
      Color=CXD9576E;
    style GraphData7 /
      ContrastColor=CX336666
      Color=CX336666;
   style GraphData8 /
      ContrastColor=CX93C6C6
      Color=CX93C6C6;
   style GraphData9 /
      ContrastColor=CXB3E6E6
      Color=CXB3E6E6;
  style GraphData10 /
      ContrastColor=CXDBFFFF
      Color=CXDBFFFF;
   end;
run;
%end;
%if %UPCASE(&graph.)=SEVERE %then %do;
proc template;
  define style _grade2; 
    parent=styles.listing;
    style GraphData1 /
      ContrastColor=CXD9576E
      Color=CXD9576E; *cx00af00 CX66CC66;
    style GraphData2 /
      ContrastColor=CX336666
      Color=CX336666; *cxcfaf00 CXFFFF66;
   style GraphData3 /
      ContrastColor=CX93C6C6
      Color=CX93C6C6; *CXCC6600 CXFF9933;

style GraphData4 /
      ContrastColor=CXD9576E
      Color=CXD9576E;
    style GraphData5 /
      ContrastColor=CX336666
      Color=CX336666;
   style GraphData6 /
      ContrastColor=CX93C6C6
      Color=CX93C6C6;
   end;
run;
%end;


%if %UPCASE(&graph.)=ALL %then %do;
	%let width=20;
	%let height=26; 
	%let y1=10.5; 
%end;

%if %UPCASE(&graph.)=SEVERE %then %do;
	%let width=20;
	%let height=24; 
	%let y1=9.0; 
%end;

data __anno;
length function $10.;
function='text'; x1space='wallpercent'; y1space='graphpercent';*92;
textsize=9; textweight='bold'; width=50;y1=&y1.; x1=25 ;label='&trt1.'; 
output;
function='text'; x1space='wallpercent'; y1space='graphpercent';
textsize=9; textweight='bold'; width=50;y1=&y1.; x1=75; label='&trt2.'; 
output;
run;



ODS listing close;
ODS pdf close;
ODS listing;


%IF %UPCASE(&DISPLAY.)=PDF %THEN %DO ;
	option orientation=portrait nodate nonumber ;
	ods pdf file="&Outfile5." style=_grade2 %if &editGrph.=Y %then sge=on;;
%END;

	ods graphics on/ reset reset=index border=off width=&width. cm height=&height. cm outputfmt=&imagefmt. imagename="&imagename9.";

%IF %UPCASE(&DISPLAY.)^=PDF %THEN %DO ;
ods listing gpath="&gpath5." style=_grade2 %if &editGrph.=Y %then sge=on;;
goptions gunit=pct htitle=3.5 ftitle="albany amt/bold" htext=1.5 ftext="albany amt";
%END;

%IF (%LENGTH(&Titlepyramid.)>0) %THEN %DO; Title "&Titlepyramid.."; %END;
%IF (%LENGTH(&Titlepyramid.)=0) %THEN %DO;
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
%END;


proc sgplot data=__tpyramid sganno=__anno;
format treatment1 treatment2 positive.;
   hbar axis / 	response=treatment1 group=reverse_gr  
				     stat=sum  datalabel
        			name="&trt1.";

 	hbar axis / response=treatment2 group=reverse_gr
       			 	 stat=sum  datalabel
		        	name="&trt2.";

 xaxis label="Percent" grid VALUES=(%eval((&Upper+10)*-1) to %eval(&Upper+10) by 10);
 yaxis label="SOC and Adverse event" discreteorder=data  ;
 keylegend '&trt1.' / title= " ";
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
	delete __:;
quit;
run;
options notes nomprint nomlogic nosymbolgen;

%stop_exec: 
run;
quit;

/**removing unwanted datasets**/
proc datasets library=work nolist;
	delete __: List_of: CTCAE Allsocv&CTC. ;
quit;
run;

options notes nomprint nomlogic nosymbolgen;
%mend AE_table2;


