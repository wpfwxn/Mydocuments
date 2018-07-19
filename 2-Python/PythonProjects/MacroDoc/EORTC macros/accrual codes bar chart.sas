
********************************************************************************************************;
* Code: This is the code to be used to create the graph of accrual that will be put in the newsletters
         This code need to be adapted for each study
* Author: Saskia Litière & Lei Ding
* Date: August 2016
*  
*********************************************************************************************************;


/******************************************************************************************************** 
This code is composed of several steps, which are:        
Step 1: Input and Output Preparation
 ** The name of an output rtf file is determined; if the name-macro, outputFileName, is not provided,
	the default name is used, which is: 'output_accrual_EORTC_****', **** denotes the study number.
 ** Load the regulatory database information from an Excel file provided by PM.
 ** Determine the data type of the variable, 'Authorisation_list', because the data type can be number
	or text, which will need different methods to read the date.
 ** If the date_planned is not provided in the Excel file, the date which is 1 day before first patient
	in will be used as the value of date_planned.

Step 2: time window and basic datasets
 ** The first date is recorded by 3 macro variables (year, month and day) based on the first patient in.
 ** The last date is recorded by 3 macro variables (YY, MM and DD) based on the end day of the month
	when the data is extracted.
 ** 4 basic datasets are obtained by transposing the dataset, 'groups', to keep the information of site
	activation/openning date, site closing date, actual number of accrual, site availability date.

Step 3: collect the data about original prediction
 ** Duplicate the basic datasets at everyday from the first patient in to the end of the month, when the
	data is extracted; 
 ** Calculate the number of activated sites and the number of planned patients

Step 4: collect the data about actual observation
 ** Merge and create the dataset with patients' actual information;
 ** Calculate the observed number of registered or randomized patients.

Step 5: take into account the study hold-on
 ** Subset (predicted number of patients) of the dataset, rates, has constant value during the hold-on period;
 ** Merge the subset with the original dataset, rates, by the date and update the predicted number of patients.

Step 6: Final Graph
 ** Calculate when the recruitment is complete based on the data in the previous # months
 ** Create a graph

********************************************************************************************************/

/*--------------------------------------------------------------------------------------*/
/*                       Step 1: Input and Output Preparation                           */
/*--------------------------------------------------------------------------------------*/

/*  Check the date style and change it if necessary. This is for the authorisation_list,          */
/*	which could be any format. Because most of the studies take place in Europe, the DMY          */
/*	datestyle is chosen.                                                                          */
/*proc options option=(DATESTYLE LOCALE) value; run;*/
options DATESTYLE=DMY;

/*-------------------------- Provide the name of the output file ------------------------------------------*/
data _null_;
if "&outputFileName" = "" then call symput ('fileName', "output_accrual_EORTC_&study");
else call symput('fileName',"&outputFileName");
run;

/*----------------------------------- Load regulatory database information --------------------------------*/
PROC IMPORT OUT= WORK.groups 
            DATAFILE= "&dataaddress.\&study.\&ExcelName" 
            DBMS=XLSX REPLACE;
RUN;
/*------------- Determine the data type of the variable: Authorisation_list -------------------------------*/
proc contents data = groups noprint out = table_groups;
run;
proc sql;
	select type into :typeOfAuthorisationList from table_groups where upcase(name) = "AUTHORISATION_LIST";
	select type into :typeOfDatePlanned from table_groups where upcase(name) = "DATE_PLANNED";
	select type into :typeOfRemovalDate from table_groups where upcase(name) = "REMOVED_FROM_AUTHORISATION_LIST";
quit;

/*-------------- Use the date which is 1 day before the first-patient-in as the "date_planned". -----------*/
proc sort data = patient;
by &ddor;
run;
data patient2;
	set patient;
/* The if statement below is used to keep the valid patients based on the value by &ddor */
	if &ddor ^=.;
run;
data _null_;
set patient2;
if _N_ = 1 then call symput ('date_planned', put(&ddor - 1, ddmmyy10.));
run;

/* Provide the dates when the site is openned, and when the site will be closed. */
/* Provide the site number/ID and planned annual recruitment number.             */
data groups (rename = (plannedDate = date_planned));
	set groups;
	format _temp date_added date_closed plannedDate date7.;
/* If date_planned, does not exsit in groups, the date_planned equals the macro variable, &date_planned */
    if "&typeOfDatePlanned" = 1 then plannedDate = date_planned;
	else plannedDate = input(date_planned,anydtdte12.);
	if plannedDate = . then
/*		date_planned = input(&date_planned., ddmmyy10.); */
		plannedDate = input("&date_planned", ddmmyy10.); 
/* date_added is the actual date when the site is open for this study */
/* the format of authorisation_list can be different because it is manually input by site */
	if "&typeOfAuthorisationList" = 1 then date_added = Authorisation_list;
	else date_added = input(Authorisation_list,anydtdte12.);
/* _temp and removed_from_authorisation_list are the dates when the sites are removed from the current study */
	if "&typeOfRemovalDate" = 1 then _temp = Removed_from_authorisation_list;
    else _temp = input(Removed_from_authorisation_list,DDMMYY10.);
/* the date_closed when all the sites are normally closed equals &dt_cut plus 100 days */
/*	date_closed = input(&dt_cut., ddmmyy10.) + 100;*/
	date_closed = &dt_cut. + 100;
	/* if the site is closed in the middle of the study, that date is its closing date. */
    if _temp > 0 then date_closed = _temp;
/* hospno is the id for each site, in case the inst is a text, this calculation makes it a number */
	hospno = inst*1;
	keep hospno plannedDate date_added date_closed  ; *recruit is removed;
run;

ODS RTF FILE="&dataaddress.\&study.\&fileName..rtf  " STYLE=EORTCStyle1 bodytitle startpage=no;

/*--------------------------------------------------------------------------------------*/
/*                       Step 2: Prepare the time window and basic datasets             */
/*--------------------------------------------------------------------------------------*/

/*------------------------------------ Only keep the necessary datasets ----------------*/
/*GIS FEB2018: Add a sysfunc (exist) to check whether the formats exist or not before saving 
			  (example: study 1527 from Vista trial do not have a format catalogue 
			   ==> ERROR in the log window.
*/
%macro save_data;
proc datasets library=work nolist;
	SAVE patient patient2 groups %if %sysfunc (exist(formats)) > 0 %then formats;
	;
run;
quit;
%mend;
%save_data;


/*------------------------- Provide the time window --------------------------------------------------*/
/* The year, month and day of the first patient randomization/registration, firsty, firstm, firstd;   */
/* The date of the first patient randomization/registration, dt_start;                                */
/* The end Y, M and D are defined as the end of the month when the data extraction happens, lasty...  */
proc sort data = patient2;
by &ddor;
run;
data patient2;
	set patient2;
	year_r = year(&ddor);
	month_r = month(&ddor);
	day_r = day(&ddor);
    * for data manipulation later;
	if _N_=1 then do;
		call symput('firsty',strip(year_r));
		call symput('firstm',strip(month_r));
		call symput('firstd',strip(day_r));
		call symput('dt_start',strip(put(&ddor,ddmmyy10.)));
	end;
	call symput('lastd',strip(day(intnx('month', &dt_cut, 0, 'end'))));
	call symput('lasty',strip(year(intnx('month', &dt_cut, 0, 'end'))));
	call symput('lastm',strip(month(intnx('month', &dt_cut, 0, 'end'))));

	label year_r = "Year";
	label month_r = "Month";
	label day_r = "Day";
run;

/*--------------------- Provide the number of registered or randomized patients for each site -------*/

/* dataset 'group_site': the information about the hospno and corresponding number of patients*/
proc freq data=patient2  noprint;
	table hospno / out=group_site;
run;

proc sort data=groups; by hospno; run;
/* The final dataset, groups, includes the number of registered or randomized patients */
data groups;
	merge groups group_site (keep=hospno count);
	by hospno;
run;
/* Print the dataset, groups */
title "EORTC &study.: Summary of accrual by site";
proc print data=groups noobs label;
	label date_planned = "Planned date of activation"
		  date_added = "Date of activation"
	      date_closed = "Date of closing";
run; title;

/*------------------ Calculate several new variables ----------------------------------------*/
/* the number of distinct dates; 
   the number of sites;
   the total registered or randomized patients in the previous # months;
   the average number of registered or randomized patients per day in the previous # months;
   the total registered or randomized patients.
*/
/* The default value for the #month for prediction is 3 */
data _null_;
if "&monthForPrediction" = "" then call symput('monthForPrediction','3');
run;

proc sql noprint;
 select count(distinct(&ddor.)) into :DORCOUNT from patient2;  * nr of distinct randomization dates!!;
 select count(*) into :SITECOUNT from groups;
 select count(*)/(&dt_cut - intnx('month',&dt_cut,&monthForPrediction.*(-1),'same')) into :averagePatientPerDay 
		from patient2 where &ddor >= intnx('month',&dt_cut,&monthForPrediction.*(-1),'same');
 select count(*) into :patientLast&monthForPrediction.Month 
		from patient2 where &ddor >= intnx('month',&dt_cut,&monthForPrediction.*(-1),'same');
 select count(*) into :actualRecruitment from patient2;
quit; 
%put 'No. of distinct randomization dates is: ' &dorcount;
%put 'No. of sites is: ' &sitecount;
%put 'No. of patients recruited in the previous ' &monthForPrediction. ' months is: ' &&patientLast&monthForPrediction.Month;
%put 'Average number of patients registered/randomized per day is: ' &averagePatientPerDay;
%put 'Actual number of patients registered/randomized in total is: ' &actualRecruitment;

/*---------------- Transpose the dataset, groups, to get 4 new datasets --------------------------*/
/* create a dataset with site activation date */
proc transpose data=groups (keep=date_added) 
out=groupstx prefix=hospno_;
run;
/* create a dataset with site closure date: if the date is not provided, then it is &dt_cut + 100 */
proc transpose data=groups (keep=date_closed)
out=groupstx2 prefix=hospno2_;
run;
/* create a dataset with the number of registered or randomized patients in each site */
proc transpose data=groups (keep=count)
out=groupstx4 prefix=actual_;
run;
/* create a dataset with the planned availability date for each site */
proc transpose data=groups (keep=date_planned)
out=groupstx5 prefix=planned_;
run;

/*--------------------------------------------------------------------------------------*/
/*                       Step 3: Original Prediction                                    */
/*--------------------------------------------------------------------------------------*/
/*------------------------ Pool and calculate expected number of sites and patients --------------*/
%macro accrual1;
/* The data step below creates a dataset 'test', which includes the openning date, closing date,
	planned availability date and actual recruitment number for each site                         */
data test;
	merge groupstx groupstx2 groupstx4 groupstx5;
	format studyDate ddmmyy10.;
	retain studyDate;
	i=1;
	year_r = &firsty.;
	month_r = &firstm.;
	day_r = &firstd.;
	studyDate = mdy(month_r, day_r, year_r);
	output;
/* the do-while loop below is used to duplicate the row with increasing date */
	do while (studyDate < mdy(&lastm., &lastd., &lasty.));
		studyDate = studyDate + 1;
		year_r = year(studyDate);
		month_r = month(studyDate);
		day_r = day(studyDate);
		output;
	end;
	drop i _NAME_ _LABEL_;
run;

data test;
	set test;
    /* randdate: last day of each corresponding month, intnx function is used for this purpose */
	randdate = intnx('month',studyDate, 0, 'end');

	* calculations on the number of planned and actual sites to be open(ed);
	nr_hospno_planned = (studyDate >= planned_1)  %do j = 2 %to &sitecount; + (studyDate >= planned_&j.) %end;; 
	nr_hospno = (.< hospno_1 <= studyDate <=hospno2_1)  %do j = 2 %to &sitecount; + (.< hospno_&j. <= studyDate <=hospno2_&j.) %end;; 

	/*------ predictions on how sample size should have been -----*/
	/* original expected based on exponential: as in accruals.xls */
	/* formula available in confluence pages  */

	time = (studyDate - input("&dt_start",ddmmyy10.))/365.25;
	cum_freq_pred = (time<=1)*(((EXP(LOG(2)*time)-1)/2) + (time**2)/2)*(&pred_accr/2)
					    + (time>1)* ((((EXP(LOG(2)*1)-1)/2) + (1**2)/2)*(&pred_accr/2) + (time-1)*&pred_accr);
	cum_freq_pred = min(&target, cum_freq_pred);  * cap at planned number of patients;

	format randdate ddmmyy10.;
	keep year_r month_r day_r studyDate randdate nr_hospno nr_hospno_planned cum_freq_pred time;
run;
%mend; 
%accrual1;

/*--------------------------------------------------------------------------------------*/
/*                       Step 4: Actual Observation                                     */
/*--------------------------------------------------------------------------------------*/

/* merge expectations with observed number of patients */
proc sort data=patient2; by year_r month_r; run;
proc freq data=patient2 noprint;
	tables day_r / out=day_reg;
	by year_r month_r;
run;

/* The data step below creates cumulated number of randomized patients and gives a sequence number for each month */
data rates;
	merge day_reg (keep= month_r year_r day_r count) test;
	by year_r month_r day_r;
	if count=. then count=0;  * fill in empty months of accrual;
	retain cum_freq nr_day;
	if _N_=1 then do;
		cum_freq = count;
		nr_day=1;
	end; 
	else do;
		cum_freq = cum_freq + count;
		nr_day = nr_day + 1;
	end;
	randdate=mdy(month_r,15,year_r);
  	format randdate MONYY7.;
	label nr_hospno = "Activated sites"
		  nr_hospno_planned = "Planned sites"
		  cum_freq = "Observed accrual"
		  cum_freq_pred = "Original prediction";
run;

/*--------------------------------------------------------------------------------------*/
/*                       Step 5: Take into account the study Hold-On                    */
/*--------------------------------------------------------------------------------------*/

/**********************************************************************/
/* In order to consider some new sites could be added into the study, */
/* I need to make changes on the variable, nr_hospno_planned.         */
/* In order to consider the hold on of the study, I need to work on   */
/* the variable randdate. In this case, the values for planned sites, */
/* activated sites, cum_freq_pred and cum_freq should be constant for */
/* n months, where n is the number months when the study is hold-on.  */
/* In order to predict the trend of the patient recruitment, */
/**********************************************************************/

%macro holdOn(withHoldOn = , holdOnEndDate = , holdOnStartDate = );
%if "&withHoldOn" = "Y" %then %do;

data _null_;
	call symput('numberOfHoldOnDay', &holdOnEndDate - &holdOnStartDate);
run;

data temp (keep = cum_freq_pred studyDateNew rename = (studyDateNew = studyDate cum_freq_pred = cum_freq_pred_new));
set rates;
format studyDateNew ddmmyy10.;
retain prediction 0;
retain studyDateNew;
prediction = cum_freq_pred;
if _N_ = 1 then studyDateNew = mdy(&firstm,&firstd,&firsty);
else studyDateNew = studyDateNew + 1;
output;
if studyDate = &holdOnStartDate then do;
	do i = 1 to &numberOfHoldOnDay;
		studyDateNew = studyDateNew + 1;
		cum_freq_pred = prediction;
		output;
	end;
end;
run;

proc sort data = rates;
by studyDate;
run;
proc sort data = temp;
by studyDate;
run;

data rates (drop = cum_freq_pred rename = (cum_freq_pred_new = cum_freq_pred));
merge rates (in = a) temp;
by studyDate;
if a;
run;
%end;
%mend holdOn;

/* Multiple hold-on is possible by using the macro: executeHoldOn*/
%macro executeHoldOn ;
	%if "&numberOfHoldOn" ^= "0" %then %do;
		%do i = 1 %to &numberOfHoldOn;
			%holdOn(withHoldOn = &&withHoldOn&i, holdOnEndDate = &&holdOnEndDate&i, holdOnStartDate = &&holdOnStartDate&i);
		%end;
	%end;
%mend executeHoldOn;
%executeHoldOn;

/*--------------------------------------------------------------------------------------*/
/*                               Step 6: Final Graph                                    */
/*--------------------------------------------------------------------------------------*/

/*------------------------------- Predict the end date ---------------------------------*/
data _null_;
format targetDate ddmmyy10.;
targetDate = &dt_cut + (&target - &actualRecruitment)/&averagePatientPerDay;
call symput('targetDate',put(targetDate, MONYY7.));
call symput('extractDate',put(&dt_cut,worddate12.));
run;
%put 'The date to have all the required number of patients to be registered/randomized is: ' &targetDate;
/*---------------------- Only the keep the data at the end of each month ---------------*/
proc sort data = rates;
by year_r month_r;
run;
data ratesByMonth;
set rates;
by year_r month_r;
if last.month_r;
run;

/*----------------------- Draw graph: set the comments below the graph -----------------------------*/
data anno;
function = "text";
x1space = "wallpercent";
y1space = "graphpercent";
x1 = 50;
y1 = 8;
width = 100;
label = "The accrual of the planned &target patients is expected in &targetDate. based on the accrual observed in the last &monthForPrediction. months.";
run;
/*----------------------- The default value for showExpectedDate is 'Y' ----------------------------*/
data _null_;
if "&showExpectedDate" = "" then call symput('showExpectedDate',"Y");
run;

%let maxsite = %eval(&sitecount.*3);
%put &maxsite;
%macro Graph;
title "EORTC &study. - Accrual: observed versus expected";
*footnote j=left height=8pt "Database extracted on the &date_extraction." ;
proc sgplot data=ratesByMonth %if "&showExpectedDate" = "Y" %then %do; sganno = anno pad = (bottom = 15pct) %end;;
	yaxis label = "Number of sites" max = &maxsite;
	y2axis label = "Number of patients" integer;
	xaxis label = "Time" fitpolicy=thin; *values = ("&firstDate"d to &dt_cut by month);
	vbar randdate / response=nr_hospno_planned  legendlabel="Planned sites" FILLATTRS=(color = lightblue) barwidth=0.3 transparency=0.5;
	vbar randdate / response=nr_hospno legendlabel="Activated sites" FILLATTRS=(color = darkblue) barwidth=0.3 transparency=0.5 nooutline;
	vline randdate / response=cum_freq y2axis legendlabel="Number of patients observed"  LINEATTRS=(color=darkred pattern=1 thickness=2);
	vline randdate / response=cum_freq_pred y2axis legendlabel="Original prediction (&pred_accr. per year)"  LINEATTRS=(color=green pattern=1 thickness=2);
/*	vbar studyDate / response=nr_hospno_planned  legendlabel="Planned sites" FILLATTRS=(color = lightblue) barwidth=0.3 transparency=0.5;*/
/*	vbar studyDate / response=nr_hospno legendlabel="Activated sites" FILLATTRS=(color = darkblue) barwidth=0.3 transparency=0.5 nooutline;*/
/*	vline studyDate / response=cum_freq y2axis legendlabel="Number of patients observed"  LINEATTRS=(color=darkred pattern=1 thickness=2);*/
/*	vline studyDate / response=cum_freq_pred y2axis legendlabel="Original prediction (&pred_accr. per year)"  LINEATTRS=(color=green pattern=1 thickness=2);*/
	inset "Data extracted on &extractDate" / position = topleft BORDER;
run;
title;
footnote;
ods rtf close;
%mend;

ods graphics on/OUTPUTFMT=jpg border = off imagename="&fileName._" reset=index width=564px  ;
ods listing gpath="&dataaddress.\&study.";
title;

%graph;

/* Delete all the dataset listed below */
/*proc datasets library=work nolist;*/
/*	*/
/*run;*/
/*quit;*/

