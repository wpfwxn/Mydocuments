
/***************************************************************************************************

CINDEX2.SAS
*************

Calculates Harrell's C-index for cennsored time to event data.

Also allows to output the associated Somers'D rank statistic.

Version date   : March 2007
Software       : SAS version 9
Original author: based on macro by Mithat Gonen (gonenm@Mskcc.org)

************************************************************************************

REVISIONS

03APRIL2007,CCO 	Adapt macro to fit in with standard EORTC formatting. Allow handling of ties and stratification.
					Allow input of either Cox model parameters or predictors directly.

28APRIL2009,SCO 	Give all kind of pairs (concordant, discordant,ties in prediction, ties in outcome, uncertain).

					Give "C-index" as defined by Harrel: (C+T/2)/(C+D+T).
					Give C-index for Ties=0: (C)/(C+D+T) and for Ties=1: (C+T)/(C+D+T).
					Give C-index as defined by Yan&Greene named "Ties out": (C)/(C+D).

					Give the optimal Cindex according to the number of observation and number of categories of the variable tested.

29APRIL2010,JRA     Add of ODSFile parameter to let the user save the ODS rtf output file in a pre-specified location.

07MAR2011,JRA       Remove of StratVar parameter

************************************************************************************

PARAMETERS

    Data       :  Name of the data file (Required parameter)
    TestVar    :  Test variable - either list of one or more variables to put into a Cox PH model 
					 OR single variable representing the predictions (Required parameter)
    TimeVar    :  Time variable (Required parameter)
    CensVar    :  Censoring variable (Required parameter)
    CensVal    :  Value of CensVar for a censored observation. (optional) [default=l]
    ODSFile    :  The name of the 'rtf' file which is to contain the ODS rtf output
                     (optional) [default=C:\Temp\c-index.rtf], see example.
    Where      :  Selection condition (optional) [default=l, i.e., select all records]
    Somers     :  If Y then also Somers'D statistic is produced (optional) [default=N]
    Order      :     If COX then Cox PH model is run on variables in TestVar. Survival predictions are taken from the x'b values.
					 If INC then TestVar is a single variable containing a prognostic index where higher values represent longer survival.
 					 If DEC then TestVar is a single variable containing a prognostic index where higher values represent shorter survival.
                     (optional) [default=COX]

NOTES

	Observations where any of the TestVar, Timevar or CensVar  variables are missing cannot be used in calculating
	 the c-index. The actual used number of observations is given in the title of the output.
	The output also gives the proportion of usable (ie. where the order could be assessed) pairs due to censoring. 

	The c-index is bounded between 0 and 1. A value of 0.5 indicates no order association betwwen the predicted and observed
	 survival times. A value of 1 indicates perfect association. A value of 0 indicates a perfect reverse association.
	 The Somers'D index is merely a linear rescaling of the C-index onto a [-1,1] range with 0 indicating no order association,
	 1 is perfect association and -1 is perfect reverse association.

EXAMPLE

   ODSFile: %cindex2(Data=analyseb,TestVar=cutoff4,TimeVar=t_fib2b,CensVar=fib_2b,ODSFile=H:\My Documents\EORTC\STUDIES\10995\output\cindex.rtf,Order=inc);
  
REFERENCES

	Harrell FE, Lee KL, Mark DB. Multivariable prognostic models–issues in developing models, evaluating assumptions and adequacy,
	and measuring and reducing errors. Stat Med. 1996;15:361-387. 


***************************************************************************************************/

%macro cindex2(Data=,TestVar=,TimeVar=,CensVar=,CensVal=1,Where=,Somers=N,Order=COX,ODSFile=);

ods listing close;

%let ODSFile=%UPCASE(&ODSFile) ;
%let Somers=%UPCASE(&Somers) ;
%let Order=%UPCASE(&Order) ;

%put &ODSFILE;

%let i=1;
%let word=%qscan(&TestVar,&i,%str( ));
%let VarList=&word.;
%do %while(&word ne);
	%let i=%eval(&i+1) ;
   %let word=%qscan(&TestVar,&i,%str( ));
	%let VarList=&VarList.,&word;
%end;
%let nrvar=%eval(&i+1);
%let VarList=&VarList. &TimeVar.,&CensVar.;


* JRA, 11OCT2010, Initialization of Where parameter when it is empty;
%if %length(&WHERE)=0 %then %let WHERE=1;

data __Input;
   set &Data.;
	if &where.;
   if &CensVar.=&CensVal. then Censored=1;
   else Censored=0;
   keep &TestVar. Censored &TimeVar. &Censvar.;
run;

data __Input;
   set __Input;
   where nmiss(&VarList.)=0;
   call SYMPUT('Nrobs',trim(left(put(_n_,8.))));
run;


%IF &Order=COX %THEN %DO;

	proc phreg data=__Input noprint;
	      model &TimeVar.*Censored(1)= &TestVar. ;
		  output out=_outpred1 xbeta=predxb;
	run;
	data _outpred1;
	   set _outpred1;
	   nobs=_N_;
	   predxb=-predxb;
	run;
%END; %IF &Order=INC %THEN %DO;
	data _outpred1;
	   set __Input;
	   predxb=&TestVar.;
	   nobs=_N_;
	run;
%END; %IF &Order=DEC %THEN %DO;
	data _outpred1;
	   set __Input;
	   predxb=-&TestVar.;
	   nobs=_N_;
	run;

%END;


	/* Number of patients in the dataset*/
	proc sql noprint;
		select count(&TimeVar.) into :nbpat from &Data.;
	quit;
	/* Number of categories of TestVar*/
	proc sql noprint;
		select count(distinct predxb) into :nbcat from _outpred1;
	quit;
	/* Sort of the dataset to compute the optimal C-index */
	proc sort data=__Input;
		by &TimeVar. Censored;
	run;
	proc sort data=_outpred1;
		by &TimeVar. Censored;
	run;

   /* create the optimal variable with the same number of categories as TestVar */
	data __Input;
	   set __Input;
	   spatid=_N_;
	   byvar=&nbpat./&nbcat.;
	   do i=0 to (&nbcat.-1);
			if spatid>=(i*byvar) and spatid<=((i+1)*byvar) then optvar=i+1;
       end;
	run;

	data _outpred;
	   merge _outpred1 __Input;
	   by &TimeVar. Censored;
	   predxbopt=optvar;
	   nobs=_N_;
	run;



/* Matrice of all pairs of patients */
proc sql;
  create table _pair as
  select a.nobs as obs1, a.predxb as preds1, a.predxbopt as preds1opt, a.&TimeVar as surv1, a.Censored as censor1,
         b.nobs as obs2, b.predxb as preds2, b.predxbopt as preds2opt, b.&TimeVar as surv2, b.Censored as censor2
  from _outpred a, _outpred b 
  where b.nobs>a.nobs;
quit;

data _pair;
set _pair;
	To=0;
	U=0;
	T=0;
	C=0;
	D=0;
	Topt=0;
	Copt=0;
	Dopt=0;

	if censor1=0 and censor2=0 and surv1=surv2 then To=1;
	if censor1=1 and censor2=0 and surv1<surv2 then U=1;
	if censor1=0 and censor2=1 and surv1>surv2 then U=1;
	if censor1=1 and censor2=1 then U=1;

	if To=1 or U=1 then usable=0; else usable=1;

	if usable=1 then do;
		/*Number of Condordant, Discordant and Ties pairs for TestVar */
		if preds1=preds2 then T=1; 

		if censor1=0 and censor2=0 and surv1>surv2 and preds1>preds2 then C=1;
		if censor1=0 and censor2=0 and surv1<surv2 and preds1<preds2 then C=1;
		if censor1=0 and censor2=0 and surv1>surv2 and preds1<preds2 then D=1;
		if censor1=0 and censor2=0 and surv1<surv2 and preds1>preds2 then D=1;

		if censor1=1 and censor2=0 and surv1>=surv2 and preds1>preds2 then C=1;
		if censor1=1 and censor2=0 and surv1>=surv2 and preds1<preds2 then D=1;

		if censor1=0 and censor2=1 and surv1<=surv2 and preds1<preds2 then C=1;
		if censor1=0 and censor2=1 and surv1<=surv2 and preds1>preds2 then D=1;


		/*Number of Condordant, Discordant and Ties pairs for Optimal variable */
		if preds1opt=preds2opt then Topt=1; 

		if censor1=0 and censor2=0 and surv1>surv2 and preds1opt>preds2opt then Copt=1;
		if censor1=0 and censor2=0 and surv1<surv2 and preds1opt<preds2opt then Copt=1;
		if censor1=0 and censor2=0 and surv1>surv2 and preds1opt<preds2opt then Dopt=1;
		if censor1=0 and censor2=0 and surv1<surv2 and preds1opt>preds2opt then Dopt=1;

		if censor1=1 and censor2=0 and surv1>=surv2 and preds1opt>preds2opt then Copt=1;
		if censor1=1 and censor2=0 and surv1>=surv2 and preds1opt<preds2opt then Dopt=1;

		if censor1=0 and censor2=1 and surv1<=surv2 and preds1opt<preds2opt then Copt=1;
		if censor1=0 and censor2=1 and surv1<=surv2 and preds1opt>preds2opt then Dopt=1;

	end;
run;

proc sql noprint;
  	select sum(usable) into :nused2 from _pair;
quit;

%let nbcatb=%sysevalf(&nbcat,integer);
%let nall=%eval(((&Nrobs*&Nrobs)-&Nrobs)/2);
%let nprop2=%sysevalf((&nused2/&nall)*100,ceil);

** 29APR2010, JRA, Add of an external path to save the ods rtf file in a pre-specified location;

 %IF &ODSFile=%STR() %THEN %DO;
 		%let ODSFile=C:\Temp\c-index.rtf;
 %END;

ODS RTF FILE="&ODSFile." STYLE=EORTCStyle1 bodytitle startpage=no;

goptions reset=all;

 title1 "C-index for the variable '&Testvar' (&nbcatb categories)";
%IF &Order=COX %THEN %DO;
 title2 "Cox model";
%END;
%IF &Order=INC %THEN title2 "Increasing survival for increasing '&TestVar'"; ;
%IF &Order=DEC %THEN title2 "Decreasing survival for increasing '&TestVar'"; ;
title3 "&Nrobs observations";


proc sql;
  select sum(usable) label='Usable pairs', 
		 sum(To) label='Ties in outcome',
		 sum(U) label='Uncertain pairs',
		 n(usable) as total label='Total pairs',
		 sum(usable)/n(usable) as percent label='"%" of usable pairs'
  from _pair;
  select sum(C) label='Concordant (C)', 
		 sum(D) label='Discordant (D)', 
		 sum(T) label='Ties in prediction (T)',
		 sum(usable) label='Total usable pairs', 
		 sum(C)/sum(usable) as percentC label='"%" of C pairs',
		 sum(D)/sum(usable) as percentD label='"%" of D pairs',
		 sum(T)/sum(usable) as percentT label='"%" of T pairs'
  from _pair;
  select ((sum(C))/sum(usable)) as cindex2 label='Weight = 0 for Ties',
		 ((sum(C)+(sum(T)/2))/sum(usable)) as cindex1 label='C-index by Harrell', 
		 ((sum(C)+(sum(T)))/sum(usable)) as cindex3 label='Weight = 1 for Ties',
		 ((sum(C))/(sum(C)+sum(D))) as cindex4 label='C-index with Ties out',
		 ((sum(Copt)+(sum(Topt)/2))/sum(usable)) as cindex11 label='C-index optimal' 
		   %IF &Somers=Y %THEN , 2*(((sum(C)+(sum(T)/2))/sum(usable))-0.5) as somersd label='Somers D';
  from _pair;
  /* Could be added if you want the number of pairs C, D, Ties in the Optimal case;
  /*
  select sum(Copt) label='Concordant (C)', 
		 sum(Dopt) label='Discordant (D)', 
		 sum(Topt) label='Ties in prediction (T)',
		 sum(usable) label='Total usable pairs'
  from _pair;
  */
footnote1 j=left color=black height=8pt "C-index by Harrell = (C+T/2)/(C+D+T)";
footnote2 j=left color=black height=8pt "Weight = 0 for Ties  -> C-index = (C)/(C+D+T)";
footnote3 j=left color=black height=8pt "Weight = 1 for Ties  -> C-index = (C+T)/(C+D+T)";
footnote4 j=left color=black height=8pt "C-index with Ties out = (C)/(C+D)";
footnote5 j=left color=black height=8pt "C-index optimal = (C+T/2)/(C+D+T) in a dataset ordered by survival time";
quit;

ODS RTF CLOSE;

%clean_table(path=&ODSFile.);


proc datasets nolist;
   delete  __Input _pair _outpred _outpred1 _outpred2;
run;
quit;



ODS LISTING ;


%mend;


