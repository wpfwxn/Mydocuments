/************************************************************************************
*
* QOL_COMP_V.SAS
* ***********
*
* Macro to compute compliance tables for QoL with FIXED timepoints or non fixed timepoints
*
* This version has an option to produce output as a Word document using ODS.
*
* Version date    : 19 January 2007
* Software        : SAS version 9.1
* Original author : Murielle MAUER
* Based on QOL_COMP_F v. 1.2 macro
* Modified by : Jérôme RAPION
* Modification date : 10 Fev 2009
*************************************************************************************
*
* HISTORY
*
*  Date: 19JAN2007 - version 0 (Murielle Mauer -> basis version)                                
*        13DEC2007 - version 1.0: to let the choice between ODS RTF and output (JRA)
*                    The creation of an ODS RTF file is not mandatory anymore (see notes)
*                    The calculation of dev (indicator to remove duplicates) 
*                    has been moved inside the WINDOW condition;
*                    if a patient had an EVENT, we assign no WINDOW for this patient (JRA)
*                    Change in the handling of DSTART parameter (JRA) : see notes
*        10FEV2009 - Deletion of DSTART parameter (JRA)
*                    Replacement of DESCR parameter by DETAILS parameter 
*                    (to be homogenous with other EORTC macros).
*                    Add of a condition to avoid counting expected forms, in case target time  point is missing (JRA)
*                    Deletion of the reason of discard Baseline after trt start as it is not possible anymore now
*                    as DSTART parameter has been deleted (JRA)
*
*        11OCT2011 - Cleaning of SAS code (drop of some useless variables) (JRA)
*
* Current version: 1.2
*
*************************************************************************************
*
* PARAMETERS
*
*    STUDY     = Study number (used for printing on report) (optional parameter)
*    CLINDAT   = Name of dataset containing clinical variables, 
*                default=PATIENT 
*    WINREF    = Y if the date of randomization is used as reference date (day=0) 
*		         in WINDAT for the definition of the target days and time windows for each 
*			     assessment, =N otherwise (see notes);
*                default=N (optional parameter) 
*    WINVAR    = variables in CLINDAT other than the date of randomisation (dor) used to define the windows in WINDAT 
*                default=dor (optional parameter, see notes) 
*    WINDAT    = Name of dataset containing time windows, (see notes)
*                default=TIMEWIN 
*    QLdat     = Name of (WORK) dataset containing QL data in the study, 
*                default=FORM930 
*    Qdate     = name of variable in QLform that contains date of filling the QL form, 
*                default=Q31
*    EXTRACT   = date of extracting the data from VISTA: should be %str(mm,dd,yyyy), 
*                This date should be equal to the date of database LOCK according to our new SOP, JRA, 10FEV2009.
*                default=TODAY (optional parameter)
*    TRT       = name of the treatment variable, default=TRT1 (optional parameter)
*    TIMEVAR   = name of time variable containing the time during which the patient
*                is at risk of QL evaluation (in days), default=DSUR (optional parameter) 
*    CENSVAR   = censoring indicator, default=SS (optional parameter) 
*    CENSVAL   = indicator for censoring, default=1 (optional parameter) 
*                Ql is stopped when the event occurs ;
*                if QL until death: timevar=duration of surv, censvar=surv status ;
*                if QL until prog: time var=prog free surv, censvar=pfs indicator ;
*    BCOMP     = Y if patients without baseline are included in the tables, 
*                N otherwise, default=Y (optional parameter) 
*    ALLOW     = allowance for delayed Ql in months, default=3 (optional parameter)  
*    (ie: do not request it if target point is not passed by at least ALLOW months)(months) ;
*    Note: for analysis allow is set to 0 ;
*    DETAILS   = Y if description of data is to be printed, N otherwise, default=Y 
*                (optional parameter) 
*    ODSDir    = N (no ODS rtf file will be provided)
*              = the directory where the ODS output should be located: default=C:\sas
*                (optional parameter, see notes) 
*
*************************************************************************************
* NOTES
*
* This macro produces a dataset named _scqol that contains the &clindat data merged with 
* the &qldat dataset, cleaned of unusable forms, with a step indicator and an indicator
* 'withbase' that takes value 1 if baseline is OK for the patient 
* It also produces a dataset named _discardall containing all excluded forms - except 
* those invalid due to missing date.
* NB: in case of duplicates, the one closest to the target is kept ;
*     subjects for which no QoL forms were received are not kept in the dataset _scqol ;
*
***********************
* WINVAR Parameter: 
***********************
* if QoL_comp_V is used with fixed timepoints, WINVAR is not necessary: in this case, it is an optional parameter 
* and by default, WINVAR=dor 
* if QoL_comp_V is used with non fixed timepoints, WINVAR is necessary: in this case, it is strongly recommended 
* to fill in this parameter. Be carreful, if in this case, WINVAR is not filled in, WINVAR will be equal to dor.
* But it will not be appropriate. Results will be correct but listing of checks may not be complete
* (it may miss some variable)
* Example of use of WINVAR: winvar=%str(dasurg dasra daera)
*
***********************
* WINDAT Parameter: 
***********************
* QoL assessments taken on the lower day of a window are not included
* QoL assessments taken on the upper day of a time window are included;
*
***	Structure of the dataset WINDAT
*	step= timepoint ordering
*   assesd= target day of the QoL assessment (any SAS expression to compute it) 
*	lassesd= lower bound of the time window or distance from the target day to the lower bound
*			 (any SAS expression to compute it)
*	!!! If lassesd starts with - or +, it is taken by the macro as the distance from the target
*       day to the lower bound otherwise it is taken as the lower bound of the time window 
*	uassessd= upper bound of the time window or distance from the target day to the upper bound
*			  (any SAS expression to compute it) 
*   !!! If uassesd starts with - or +, it is taken by the macro as the distance from the target
*		day to the upper bound otherwise it is taken as the upper bound of the time window
*	stepname= format for the variable step 
*
*** Time unit: use D, W, M or Y as multiplicative factors for days, weeks, months or years 
*	           (D=1, W=7, M=365.25/12, Y=365.25); 
*
*   Example with fixed timepoints
*	Macro variables:
*	winref=Y ,	*it indicates that day=0 on the date of randomization,
*	winvar=	,
*    windat=windat defined as 
*
*	data windat;
*    input step assesd $4. lassesd $5. uassesd $5. stepname $10.;
*    cards;
*    0 0   -2*W +2*W baseline  
*    1 3*M -2*W +2*W 3months   
*    2 6*M -2*W +2*W 6months   
*    3 1*Y -1*W +1*W 1year     
*    4 2*Y -3*W +3*W 2years    
*    ;
*   run;
*
*	Example with non fixed timepoints 
*	First timepoint: at randomization 
*	Second timepoint: at end of radiotherapy
*	Third timepoint: at end of radiotherapy + 2 months
*	Fourth timepoint: at end of radiotherapy + 6 months
*	Fifth timepoint: at end of radiotherapy + 1 year 
*	Variables in CLINDAT: dasurg=date of surgery (SAS date)
*				  		  dor=date of randomization (SAS date) 
*				  		  dasra=date of start of radiotherapy (SAS date)
*				  		  daera=date of end of radiotherapy (SAS date)
*	Macro variables:
*	winref=N ,
*	winvar=%str(dasurg dasra daera),
*	windat=windat defined as 
*
*	data windat;
*	input step assesd $11. lassesd $22. uassesd $19. stepname $20.;
*	cards;
*	0 dor 		 max(dasurg+7,dor-42)  min(dasra,dor+42)  baseline 			
*	1 daera 	 max(dasra,daera-15)   +1*M	 			  end radio 			
*	2 daera+2*M  -1*M 				   +2*M 			  end radio+2 months	 		
*	3 daera+6*M	 -2*M				   +3*M				  end radio+6 months																	
*	4 daera+1*Y  -3*M				   +3*M			      end of radio+1 year	
*	;
*	run;
*
***********************
* WINREF Parameter: 
***********************
* if QoL_comp_V is used with non fixed timepoints, WINVAR is not necessary: in this case, it is an optional parameter 
* and by default, WINREF=N. In this case, the target days are calculated according to DoR.
* It means that all target points are centered according to the date of randomization inside the macro pgm.
* Example:  target=trim(left(assesd))||"-dor";
*           lower=trim(left(lassesd))||"-dor";
*           upper=trim(left(uassesd))||"-dor";
* if QoL_comp_V is used with fixed timepoints, WINREF is necessary: in this case, it is strongly recommended 
* to fill in this parameter: WINREF=Y. In this case, doR is used as reference date (day=0)
* so, in this case, the target day is really equal to what is indicated in WINDAT parameter.
*
***********************
* ODSDIR Parameter: 
***********************
* The default name of the rtf file produced is : QOLStudynumber_date(today).rtf ;
* In this case, the macro %clean_table will be runned.
* If ODSDir=N, no ODS rtf file will be provided (only results in the OUTPUT window.) 
*
**************************************************************************************/

%MACRO Qol_comp_V(study,clindat,windat,winref,winvar,QLdat,Qdate,extract,trt,
                  timevar,censvar,censval,bcomp,allow,details,ODSDir) ;

*** clearing all datasets ;

proc datasets nolist library = work ;
delete _clindat _windat _wincheck _reverse _overlap _scqol _discard _discard2 _discardall
_temp _base _withbase _withql _tmp _hosps _comply
_data1 _ndata1 _ndata2 _stemp _fmt _chisq
_duplicate;
run ;
quit ;


options nodate nonumber center linesize=80 ;


*** assigning default values ;

%let ODSdir=%UPCASE(&ODSdir.);
%if %length(&windat)=0 %then %let windat=timewin ;
%if %length(&clindat)=0 %then %let clindat=patient ;
%if %length(&qldat)=0 %then %let qldat=form930 ;
%if %length(&qdate)=0 %then %let Qdate=q31 ;
%if %length(&winvar)=0 %then %let winvar=dor ; *JRA, Dec 2007;
%if %length(&winref)=0 %then %let winref=N ; *JRA, Oct 2008;

data _null_ ;
           d=day(today());
           m=month(today()) ;
           y=year(today()) ;
		   %if %length(&extract)=0 %then %do ;
           call symput('extract',compress(m)||','||compress(d)||','||compress(y)) ;
		   call symput('fextract',compress(d)||'/'||compress(m)||'/'||compress(y));
		   %end ;
           %if %length(&extract)>0 %then %do ;
		   d2=day(mdy(&extract)) ;
		   m2=month(mdy(&extract)) ;
		   y2=year(mdy(&extract)) ;
           call symput('fextract',compress(d2)||'/'||compress(m2)||'/'||compress(y2));
		   %end ;
		   call symput('todays',compress(d)||'_'||compress(m)||'_'||compress(y)) ;
		   
		   run ;
%put fextract &fextract todays &todays;

%if %length(&trt)=0 %then %let trt=trt1;

%if %length(&timevar)=0 %then %let timevar=dsur ;
%if %length(&bcomp)=0 %then %let bcomp=Y ;
%if %length(&allow)=0 %then %let allow=3 ;
%let allowd=%eval(&allow*30) ; * back to days ;
%if %length(&censvar)=0 %then %let censvar=ss ;
%if %length(&censval)=0 %then %let censval=1 ;
%if %length(&DETAILS)=0 %then %let DETAILS=Y ;
%if %length(&ODSdir)=0 %then      %let ODSdir=%str(c:\sas) ; *JRA, Dec 2007;


proc format ;
	value _unit_ 1 = 'days'    7 = 'weeks'  30 = 'months'   365 = 'years' ; * just for memory ;
      value _withql OTHER='No QL at all' 1='At least 1 form' ;
    value _withbase 0='No baseline' 1='Baseline OK' ;
    value _qoldone 0='Missing' 1='Received' ;
      value _casev 0='Before first window'
                   1='Baseline after trt start'
                   2='After last assessment time'
                   3='After progression/last visit'
				   4='In between time windows or missing target dates'
                   5='Duplicate within windows'
                   6='Missing date on QL form'
			       7="More &allow months before today's date";
run;
quit;

****** 1. CHECKING, CLEANING ******;

*** CHECK for start of trt before random or missing date of start of trt ;

title1 "Trial &study" ;

title3 j=c "Trial &study - Extraction date &fextract";

*** CHECK for missing date on QLform ;
title2 'Missing dates on quality of life forms';

proc print data=&QLdat noobs split='*';
        where &qdate=.;
        run;
title2 ;


****** 2. READING THE TIME WINDOWS DATA SET  *****;

** CHECK for missing dates in the variables WINVAR;

%let varlist=%trim(%left(&winvar));
%let start=1;
%let length=%length(&varlist);

%do %until (&length=0);

%let varlist=%trim(%left(%substr(&varlist,&start,&length)));
%let var=%scan(&varlist,1,%str( ));
%put &var;

proc sort data=&clindat;
  by &trt patid;
run;

proc print data=&clindat;
  title2 "Missing dates for &var.";
  where &var=.;
  var patid &trt dor &winvar.;
run;

%let start=%eval(%length(&var)+1);
%let length=%eval(%length(&varlist)-%length(&var));

%end;

** Define the multiplicative factors D,W,M,Y 	;

proc sort data=&clindat;
  by patid;
run;

data _clindat;
  set &clindat;
  D=1;
  W=7;
  M=365.25/12;
  Y=365.25;
run;

** Compute the lower and upper bounds if not done	;

proc sort data=&windat;
  by step;
  run;

data _windat;
  set &windat;
  by step;

  if substr(trim(left(lassesd)),1,1)='-' then 
    lower=trim(left(assesd))||trim(left(lassesd));
	else lower=lassesd;

  if substr(trim(left(uassesd)),1,1)='+' then 
    upper=trim(left(assesd))||trim(left(uassesd));
    else upper=uassesd;
  run;

data _windat;
  set _windat (drop=lassesd uassesd);
  rename lower=lassesd upper=uassesd;
  run;

** Count the target day, the lower and upper bounds starting at the date of randomization 
    if not done ;

data _windat;
  set _windat;
  by step;

  %if "%trim(%left(&winref))"="N" %then %do;
    target=trim(left(assesd))||"-dor";
	lower=trim(left(lassesd))||"-dor";
	upper=trim(left(uassesd))||"-dor";
    %end;
  %if "%trim(%left(&winref))"="Y" %then %do;
    target=assesd;
	lower=lassesd;
	upper=uassesd;
    %end;		
  run;

data _windat;
  set _windat (drop=assesd lassesd uassesd);
  rename target=assesd lower=lassesd upper=uassesd;
  run;


  proc print data=_windat;
  Title2 'Test Dataset WINDAT';
  run;

** Per assessement, creates macro variables with the assessment time, lower and upper limits, name of the assessment and time unit  ;

        data _null_ _fmt (keep=start label fmtname);
                set _windat end=lastobs ;
                by step;
                retain ns 0 ;
                ns+1;
                        if lastobs then call symput('NS1',compress(ns));
                call symput('A'||trim(left(step)),assesd); * target assessment time ;
                call symput('L'||trim(left(step)),lassesd); * lower end of window ;
                call symput('U'||trim(left(step)),uassesd); * upper end of window ;
                call symput('T'||trim(left(step)),stepname);

                        *** to create the format for step ;
                        start=step ;
                label=stepname ;
                fmtname='_Step' ;
                run;

                proc format cntlin=_fmt ;
                run ;

** Check that the windows are not missing	;
data _wincheck;
  set _clindat;
run;

data _missing ;
  set _wincheck;
 
  %do i=0 %to %eval(&NS1-1);

  		if (&&A&i=.) then do;
		    mis_n="&i";
			mis_a=&&A&i;
			mis_l=&&L&i;
            mis_u=&&U&i;
			output _missing;
		    end;

  %end;
run;

proc print data=_missing label;
  title2 'Check of windows with missing target dates';
  label mis_n='Step' mis_a='Target date' mis_l='Lower limit' mis_u='Upper limit';
  var patid dor &winvar mis_n mis_a mis_l mis_u;
  where (mis_a=. or mis_l=. or mis_u=.) and (mis_a ne . or mis_l ne . or mis_u ne .); 
  run;
	
** Check that the windows are not reversed	;
data _wincheck;
  set _clindat;
run;
		
data _reverse ;
  set _wincheck;

  %do i=0 %to %eval(&NS1-1);

  		if ( &&L&i ne . and &&U&i ne . and &&L&i >= &&U&i) then do;
		    rev_n="&i";
			rev_l=&&L&i;
			rev_u=&&U&i;
			output _reverse;
		    end;
  %end;
run;

proc print data=_reverse label;
  title2 'Check for upper bound before lower bound in windows';
  label rev_n='Step' rev_l='Lower limit' rev_u='Upper limit';
  var patid dor &winvar rev_n rev_l rev_u; 
  run;

** Check that the windows are not overlapping	;
data _wincheck;
  set _clindat;
run;
		
data _overlap ;
  set _wincheck;

  %do i=0 %to %eval(&NS1-2);

		%let next=%eval(&i+1);
        if (&&L&next ne . and &&U&i ne . and &&L&next < &&U&i ) then do;
		    over_n="&i"||"/"||"&next";
            over_u=&&U&i;
			over_l=&&L&next;
            output _overlap;
			end;

  %end;
 run;

 proc print data=_overlap label;
  title2 'Check for overlapping windows';
  label over_n='Step' over_u='Upper limit' over_l='Lower limit';
  var patid dor 
	%if %upcase(&winvar) ne DOR %then %do; &winvar %end;  over_n over_u over_l  ;*, JRA Dec 2007;
 ; 
  run;

****** 3. MERGING QL DATA AND CLINICAL DATA (computing time at risk of QL) AND ASSIGNING QL FORMS TO WINDOWS *****;

* note: excluded forms (unassignable) are put in dataset _discard ;


** COMPUTING TONSTUD ;

    data _clindat (keep=patid hospno instname country dob dor &trt &censvar &timevar 
                        tonstud &winvar D W M Y );
         set _clindat;

      ** TONSTUD = time at risk for QoL ;
 	   uptonow=(mdy(&extract)-dor)-&allowd ;
	   tonstud=uptonow;*JRA, dec 2007;
       if (&censvar^=&censval) then tonstud=min(&timevar,uptonow) ;

    run ;

DATA _scqol _discard; ** merge of clinical dataset and QL dataset ;

        merge _clindat &qldat(in=Q); *JRA, dec 2007;

        by patid;

		delay=(&qdate-dor); * time to QL form in days ;
        step=-1; * initialise variable step indicating assessment identification ;

        * assigns baseline assessement *;

		  if &A0 ne . and delay > &L0 and delay <=&U0 then step=0;									
		 
        l0=&L0; u0=&U0; a0=&A0;

        * assigns to the fup windows  *;

        %do i=1 %to %eval(&NS1-1);

            l&i=&&L&i;   * l=lower bound ;
            u&i=&&U&i;   * u=upper bound ;
            a&i=&&A&i;   * a=target point ;

            l=&&L&i;   * l=lower bound ;
            u=&&U&i;   * u=upper bound ;
            a=&&A&i;   * a=target point ;
            
           * if > L and <= U, then assign to step i *;

           if a&i ne . and delay>l&i and delay <=u&i  then step=&i;
           dev&i=abs(delay-a&i) ; * JRA, 24-09-2008, calculate deviation step by step;
		   if step=&i then dev=dev&i;* this is later for keeping best of duplicates ;

        %end;

	    * remove the forms after tonstud *;

			if delay>tonstud then step=-1;
 	   		if delay>&timevar and (&censvar ne &censval) then step=-1; 

			if Q then do;

	            if (delay=. or step=-1) then output _discard; * removes unassigned forms or forms without date ;
            	else output _scqol ; ** outputs valid obs to data _scqol ;

			end;

	drop dev1-dev%eval(&NS1-1) a1-a%eval(&NS1-1) l1-l%eval(&NS1-1) u1-u%eval(&NS1-1);

run;


****** 4. REMOVING DUPLICATES WITHIN WINDOWS KEEPING THE ONE CLOSEST TO TARGET TIME  *****;

proc sort data=_scqol;
   by patid step dev;
   run;

data _scqol _discard2; * duplicates are sent to _discard2;
        set _scqol;
        by patid step dev;
        if first.step then output _scqol ;
        else output _discard2 ;
run;


data _discardall ;
set _discard(in=aa) _discard2 (in=bb) ;
%let n=%eval(&NS1-1);
l_up=&&U&n;
l_low0=&L0;
l_up0=&U0;

case=.;
if (aa and delay=.) then case=6; * date missing ;
if (case=. and  aa and delay>. and delay<=l_low0) then case=0 ; * before 1st window;
if (case=. and aa and l_up ne . and delay>l_up) then case=2; * after end of last window ;
*if (case=. and aa and delay>l_low0 and delay<=l_up0 and &dstart ne . and &Qdate-&dstart>0) then case=1 ;*baseline after start of trt ;
*JRA, 11FEV2009, as dstart parameter is now deleted, this reason is not possible anymore;
if (case=. and aa and delay>=&timevar and &censvar^=&censval) then case=3 ; * after prog ;
if (case=. and aa and delay>l_low0 and (l_up=. or (l_up ne . and delay<=l_up))) then case=4; * in between windows or missing target dates;

if bb then case=5; * duplicates ;
if (case=. and aa and delay>=&timevar and &censvar=&censval) then case=7 ; * more than &allow months before today s date ;
label case='Reason why _discarded';
*format _dstart ddmmyy10.;
*drop a: l: u: D W M Y;
run;

data _scqol;
set _scqol; 
drop D W M Y;
run;



***** 5. CREATING A COMPLETE EMPTY GRID OF ASSESSMENT POINTS BY PATIENT *****;


data _temp (keep=patid step tonstud &trt hospno dor &winvar D W M Y);
        set _clindat;

        step=-1;
        ns=symget('ns'||trim(left(1)));

        do i=0 to ns-1;
              step=i;
              output;
        end;
run;

proc sort data=_temp;
     by patid step;
     run ;

proc sort data=_scqol;
     by patid step;
     run ;



** merge the grid and the available assessments ;


data _stemp (keep=hospno patid step &trt rec tonstud dob dor &winvar D W M Y);
        merge _temp _scqol(in=in_scqol);
        by patid step;
            rec=in_scqol;
        if (rec=.) then rec=0 ;
run;


data _base (keep=patid withbase &winvar );
 set _scqol ;
 by patid step ;
 * creates an indicator whether baseline is OK or not ;
 if first.patid and step=0  then do ;
    withbase=1 ;
      output ;
      end ;
run ;


data _stemp ;
 merge _stemp _base ;
 by patid ;
 if withbase=. then withbase=0 ;
 label withbase='With baseline Qol' ;
 run ;

data _scqol ;
 merge _scqol _base ;
 by patid ;
 if withbase=. then withbase=0 ;
 label withbase='With baseline QoL' ;
 run;


proc means data=_stemp noprint;
var rec ;
by patid ;
id hospno dor &trt ;
output out=_withql max=withql ;
run ;

proc sort data=_withql ;
 by patid ;
 run ;

data _withbase ;
 merge _clindat _base(in=inbase) _withql;
 by patid ;
 withbase=inbase;
 if inbase=. then withbase=0 ;
 label withbase='With baseline QoL' ;
 run ;



***** 6. DETERMINING WHETHER A FORM IS OK, MISSING or UNDUE *****;


proc transpose data=_stemp out=_data1;
        by patid;* &trt tonstud;
        var rec;
   %if %upcase(&Bcomp)=N %then %do ;
        where withbase=1 ; ** keep only those with baseline ;
   %end ;
    copy hospno dor &trt tonstud withbase dob ;
run;

data _tmp (keep= patid hospno &trt withbase instname country &winvar D W M Y);
       merge _clindat _base ;
       by patid ;
   %if %upcase(&Bcomp)=N %then %do ; * keep only those with baseline ;
        if withbase=1 then
   %end ;
       output ;
run;


data _data1;
   merge _data1 _tmp;
   by patid;
   if first.patid ;
   run;

data _data1 (drop= _name_ );
        set _data1;
        by patid;
        array col {&NS1};
        %do i=0 %to &NS1-1; * i is equal to step, there is a shift of one betw numbering of col and i ;
        a=&&A&i;
        if (col[&i+1]=.) then col[&i+1]=0 ;
        if ((a=. or tonstud<a) and col[&i+1]^=1) then col[&i+1]=.; * if missing but not due, then OK ;
		** JRA, 9FEV2008 : add of 'if a=.';
        ** --> it means if the target timpepoint or target date is empty then it is not counted anymore;
        %end;
run;

proc sort data=_data1; by patid; run;


**** 7. COMPUTING THE COMPLIANCE TABLE BY ASSESSMENT TIME  *****;



proc means data=_data1 noprint;
var col1-col&NS1 ;
id &trt hospno dor ;
output out=_ndata1 sum=r1-r&NS1 n=e1-e&NS1 ;
run;


data _ndata2;* (keep=stepname);
 set _ndata1;
 array received {&ns1} r1-r&NS1;
 array expected {&ns1} e1-e&NS1;
 do j=1 to &ns1;
    rec=received[j];
    expec=expected[j];
    comp=round(100*rec/expec,.1) ;
    stepname=symget('T'||trim(left(j-1))) ;
    output;
    end ;
 run;

***** 8. COMPUTING THE COMPLIANCE TABLE BY INSTIT  *****;

data _data1;
        set _data1;
        by patid;
        nba=n(of col1); * nb of baseline ;
        num=n(of col2-col&NS1); * nb of fup forms due ;
        sm=sum(of col2-col&NS1); * nb of fup forms there ;
run;

proc sort data=_data1 ;
  by hospno ;
  run;

proc means data=_data1 noprint;
var num sm nba col1 ;
by hospno ;
id instname country;
output out=_hosps sum=tnum tsum tnba tsba ; ** summary by institution ;
run;

data _hosps;
        set _hosps;
        format pab 3.;
        format pba 3.;
        pab=0;
        pba=0;
        if (tnum^=0) then pab=tsum*100/tnum;
        if (tnba^=0) then pba=tsba*100/tnba;
        if (tnum=0) then pab=.;
        if (tnba=0) then pba=.;
run;



***** 9. CHECK FOR DIFFERENCES IN COMPLIANCE BETWEEN GROUPS  *****;


proc sort data=_data1;
by patid;
run;


data _comply (drop= col1-col&NS1);
        set _data1 ;
            array col {&NS1} ;
        by patid;
        label qoldone = 'QoL Compliance' ;
        label step='Timing of Assessments';
        do j=1 to &NS1 ;
        Qoldone=col[j] ;
        step=j-1 ;
       output ;
     end ;
run;

*options linesize=80 ;



***** 10. Check for forms assigned by the macro to different windows *****;

proc sort data=_scqol out=_duplicate;
  by patid &Qdate;
run;

data _duplicate (keep=patid &Qdate);
  set _duplicate;
  by patid &Qdate;
  if not first.&Qdate;
run;

data _duplicate (keep=patid &Qdate step);
  merge _scqol _duplicate(in=ina);
  by patid &Qdate;
  if ina;
run;

proc print data=_duplicate;
  title2 'Warning: same QoL form assigned to more than one time point';
  var patid &Qdate step;
run;

title ;
options ;

**** 11. PRINTING OF THE RESULTS ;

%IF &ODSDIR ne N %THEN %DO;
ODS rtf file="&ODSDIR.\QOL&study._&todays..rtf" STYLE=EORTCStyle1  BODYTITLE;
%END;

*****    DESCRIPTIONS (1st part) ******************** *;

%if %upcase(&DETAILS)=Y %then %do ;

footnote1 j=c "Trial &study - Extraction date &fextract";

title1 "Trial &study" ;
title2 'Describing the data - Part I' ;
title3 'Number of patients for which no valid QOL form has been received';

 proc freq data=_withql ;
 table withql / missing ;
 table withql*&trt / missing norow nopercent ;
 format withql _withql.;
 label withql='With any QoL' ;
 run ;

proc sort data=_withql ;
 by hospno patid ;
 run;

title3 'List of patients for which no valid QOL form has been received';
proc print data=_withql noobs;
     var hospno patid dor &trt ;
     where withql^=1;
     run;
title3 ;


Title3 'Number of valid qol forms per arm';
proc freq data=_scqol;
     table &trt;
     run;
title3 ;

proc freq data=_withbase ;
title3 'Nb of patients with baseline QL form' ;
 table withbase / missing ;
 table withbase * withql / missing ;
 table withbase * &trt / missing norow nopercent ;
 table &trt*withbase*withql / missing ;
 format withql _withql.;
 format withbase _withbase.;
 label withql='With any QoL' ;
run ;


title2;

%end;


proc print data=_ndata2 noobs split="*" ;
 var stepname rec expec comp ;
 label stepname="Assessment*time" rec="Nb forms*Received" expec="Nb forms*Expected" comp="% compliance" ;
 title1 "Trial &study: Overall Compliance to QOL as of &fextract" ;
 title2 "(allowing for &allow months delay)" ;
 title3  "Only patients on study for at least &allow months are displayed" ;
 run;

proc print data=_hosps split="*" noobs ;
var hospno instname tnba pba pab;
label hospno="INSTITUTION" instname="NAME" tnba="Nb patients" pba="Baseline*%" pab="Follow-up*%" ;
title1 "Trial &study: Compliance to QOL by Institution as of &fextract";
title2 "(allowing for &allow months delay)";
title3 "Only patients on study for at least &allow months are displayed" ;
run ;
title ;

*****    DESCRIPTIONS (2nd part) ******************** *;

%if %upcase(&DETAILS)=Y %then %do ;
title2 'Describing the data - Part II' ;
%end ;

proc freq data=_discardall ;
title3 'Reasons why forms were discarded';
 table case ;
 table case*&trt / norow nopercent ;
 format case _casev.;
run ;

%if %upcase(&DETAILS)=Y %then %do ;
proc sort data=_discardall ;
by hospno patid ;
run ;

proc print data=_discardall noobs split='*';
var hospno patid &qdate case ;
label case='Reason why discarded';
format case _casev.;
label &qdate='Date on QoL form' ;

title2;

title2 'Describing the data - Part III' ;

options linesize=120 ;* nocenter ;
proc freq data=_comply;
title3 'Compliance to QoL assessments per arm ';
tables &trt*qoldone*step / norow nopercent  ;
format qoldone _qoldone. step _step. ;
run;
options linesize=80  ;

proc freq data=_comply noprint;
tables step*&trt*qoldone / nopercent nocol chisq ;
output out=_chisq pchi ;
run ;

proc print data=_chisq noobs split='*' ;
format step _step. ;
label _PCHI_="Chi-square*Statistic" DF_PCHI="DF" P_Pchi="P-value" ;
title2 'Chi-square tests for comparing compliance at each step' ;
run ;
title2 ;
%end ;

title ; 
options ;
footnote;

%IF &ODSDIR NE N %THEN %DO ;
ods rtf close ;

%clean_Table(path=&ODSDIR.\QOL&study._&todays..rtf);
%END;


%mend ;
