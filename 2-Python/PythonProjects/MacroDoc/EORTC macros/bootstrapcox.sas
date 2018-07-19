/************************************************************************************
*
* BOOTSTRAPCOX.SAS
* ****************
*
* Macro to perform Bootstrap Model Averaging.
*  This macro can be used to check the replication stability of a prognostic model via
       a bootstrap re-sampling procedure as proposed by Sauerbrei. This technique generates
       a number of samples (each the same size as the original data set), by randomly
       selecting patients and replacing them before selecting the next patient
       (ie. bootstrap resampling). The frequency of inclusion of the component variables
       in the Cox PH regression models, including all the selected covariates and stratified
       for treatment, fitted to each of these data sets using automatic forward or backward
       stepwise selection, can be considered to be indicative for the importance of the factors.
       Alternatively, these inclusion frequencies can be seen as estimates of the posterior
       probabilities that the regression coefficients are different from zero.
       The model selection probabilities, based on how many times a permissible model
       was selected in the bootstrap samples, are then used as weights to obtain weighted
       averaged parameters (e.g., Augustin et al 2002, ).
* 
* Version date    : 12 January 2005
* Software        : SAS version 9.1
* Original author : Corneel Coens & Kristel Van Steen
*
*************************************************************************************
*
* PARAMETERS
*
*    Data       :  Name of the data file
*    TimeVar    :  Time variable [default=time]
*    CensVar    :  Censoring variable [default=censor]
*    CensVal    :  Value of CensVar for a censored observation [default=l]
*    Strat      :  Stratification variable (optional) [empty by default, i.e., no stratification]
*    Where      :  Selection condition
*    Varnms     :  List of prognostic variables (seperated by blank space)
*    Fixed      :  List of variables to remain fixed in the model
*    Nsamp      :  Number of bootstrap samples to be generated
*    Seed       :  Seed used by the random generator for creating the bootstrap sample.
*                   [default=-1, ie. random seed] 
*    Select     :  Selection option to be used for the Cox model building procedure. 
*                     F/FORWARD = forward stepwise selection [default]
*                     B/BACKWARD = backward stepwise selection
*    Alpha      :  Alpha level used for the entry/exit criterion of the model building
*                   [default = 0.05]
*    File       :  The directory name where the output files are created by the ODS statements.
*                  One text file is made (logboot.txt) containing the log of the macro. Three rtf
*                  files are made. "best10m.rtf" contains the top 10 most selected models.
*                  "inclusion.rtf" contains the variable selection frequencies and the resulting parameters.
*                  "maresults.rtf" contains the model averaging results. [default = c:\temp]
*    logwin     :  Y = log output shown as normal in logwindow. N = Log output suppressed [default=N]
*    logtxt     :  Y = log file logboot.txt created. N = logboot.txt not created [default=N]
*    mavg       :  Y = model averaging part is run. N = model averaging part is not run [default=N]
*
*
*************************************************************************************/


%macro bootstrapcox(data,timevar,censvar,censval,where,strat,varnms,fixed,nsamp,seed,select,alpha,file,logwin,logtxt,mavg);

*** DEFAULT PARAMETERS ;

options orientation=landscape;

* Assign default value if missing ;
%if %length(&censval)=0 %then   %let censval=1 ;
%if %length(&timevar)=0 %then   %let timevar=time ;
%if %length(&censvar)=0 %then   %let censvar=censor ;
%if %length(&where)=0 %then     %let where=1 ;
%if %length(&nsamp)=0 %then     %let nsamp=10 ;
%if %length(&seed)=0 %then      %let seed=-1 ;
%if %length(&select)=0 %then      %let select=f ;
%if %length(&alpha)=0 %then      %let alpha=0.05 ;
%if %length(&file)=0 %then      %let file=%str(c:\temp) ;
%if %length(&logwin)=0 %then      %let logwin=N ;
%if %length(&logtxt)=0 %then      %let logtxt=N ;
%if %length(&mavg)=0 %then      %let mavg=N ;



%let select=%upcase(&select) ;
%if (&select=F or &select=FORWARD) %then %let siglev=sle;
%if (&select=B or &select=BACKWARD) %then %let siglev=sls;

* incorporate fixed vars and count number of vars;
%let i=1;
%let renamelist=;
%let freqlist=;
%let word=%qscan(&fixed,&i,%str( ));
%do %while(&word ne);
	%let renamelist=&renamelist col&i=&word;
	%if %length(&freqlist)=0 %then %let freqlist=&word; %else %let freqlist=&freqlist*&word;
	%let i=%eval(&i+1) ;
	%let word=%qscan(&fixed,&i,%str( ));
%END;
%let nrfixed=%eval(&i-1); /* nr of fixed vars */
%let i=1;
%let word=%qscan(&varnms,&i,%str( ));
%do %while(&word ne);
    %let j=%eval(&i+&nrfixed) ;
	%let renamelist=&renamelist col&j=&word;
	%if %length(&freqlist)=0 %then %let freqlist=&word; %else %let freqlist=&freqlist*&word;
	%let i=%eval(&i+1) ;
	%let word=%qscan(&varnms,&i,%str( ));
%END;
%let nrvar=%eval(&i-1); /* nr of non-fixed vars */
%let totnrvar=%eval(&nrvar.+&nrfixed); /* total nr of vars */
%let totvarnms=&fixed &varnms; /* combined list: fixed + non-fixed */

%if &logwin=N %then %do; 
FILENAME SASOUTP "C:\TEMP\SASOUTP.LOG";
PROC PRINTTO LOG=REGRESS; RUN;
%end;


*** GET LABELS;


data _null_;
   set &data (obs=1);
   %let i=1;
   %let word=%qscan(&totvarnms,&i,%str( ));
   %do %while(&word ne);
    call symput("label&i",trim(left(vlabel(&word.))));
    %let i=%eval(&i+1) ;
    %let word=%qscan(&totvarnms,&i,%str( ));
   %END;
run;

*** CLEAN DATA;

proc sort data=&data out=_work;
 by patid;
run;

data _work;
 set _work;
 where ( (&where.) and &timevar>.Z and &timevar>=0 and &censvar>.Z);
 if (&censvar=&censval) then _censor=1 ; else _censor=0 ;
 length title $45.;
 call label(&timevar,title ) ;
 nql=n(of &totvarnms);
 call symput("sampsize",trim(left(_N_)));
run;

data _aid1; array new{&totnrvar} &totvarnms; run;
data _empty; set _aid1; delete; run;
proc transpose data=_aid1 out=_aid2(rename=(col1=count)); run;
data _aid2; set _aid2 _aid1; run;
data _aid2; set _aid2; where _NAME_^=""; run;
data _bparms; set _empty; run;
data _vparms; set _empty; run;

data _stemp (keep=patid &strat &timevar _censor &totvarnms);
        set _work;
        where nql=&totnrvar; /* restricts the dataset !!!!! */
run;

%if &logtxt=Y %then %do; 
proc printto log="&file.\logboot.txt"; run;
%end;

* get sample size, copy data set;
data _null_;
   set _stemp end=eof;
   n+1;
   if (eof) then call symput('n',n);
   drop n;
run;

%do isamp=1 %to &nsamp;
   * get a bootstrap sample;
   data _bsamp;
      seed=&seed;
      do cow=1 to &n;
         call ranuni(seed,y);
         j=floor(y*&n+1);
         j2=j;
         set _stemp point=j;
         output;
      end;
      call symput('seed',seed);
      stop;
   run;

   /* do the model selection */
	proc phreg data=_bsamp OUTEST=_osas covout noprint;
 		model &timevar*_censor(1)= &totvarnms / selection=&select &siglev=&alpha include=&nrfixed;
        %if %length(&strat)>0 %then %do; strata &strat; %end;
	run ;

title 'Convergence status on sample data';
data _problem; set _osas; iternr=&isamp; run;
proc print data=_problem; where _STATUS_^=:'0 Converged'; run;

data _null1(drop=_TIES_ _TYPE_ _STATUS_ _NAME_ _LNLIKE_ _STATUS_) _null2(drop=_TIES_ _TYPE_ _STATUS_ _NAME_ _LNLIKE_ _STATUS_);
set _osas;
by _TIES_;
if first._TIES_ then output _null1;
else output _null2;
run;

proc append base=_bparms data=_null1;
run;

data _null2 (keep= mnew1-mnew&totnrvar);
set _null2;
        array mnew {&totnrvar} mnew1-mnew&totnrvar;
        array orig {&totnrvar} &totvarnms;
		do teller=1 to &totnrvar;
			if _N_=teller then do; count=teller; mnew[teller]=sqrt(orig[teller]); end;
		end;
run;
title;
data _null2 (keep=se); set _null2;
se = sum(of mnew1-mnew&totnrvar);
proc transpose data=_null2 out=_null2; run;
data _null2 (drop=_NAME_); set _null2;
rename &renamelist;
run;

proc append base=_vparms data=_null2;
run;

%end;

proc printto;
run;

%if &logwin=N %then %do; 
FILENAME SASOUTP "C:\TEMP\SASOUTP.LOG";
PROC PRINTTO LOG=REGRESS; RUN;
%end;


proc datasets library=work nolist;
 delete _osas _null1 _null2 _bsamp _problem;
quit;


data _null1;
        set _bparms;
        array new {&totnrvar} &totvarnms;
        do i=1 to &totnrvar;
		  if new[i]^=. then new[i]=1;
		  else new[i]=.;
        end;
		drop i;
run;
proc transpose data=_null1 out=_worknull; run;
data _test; set _worknull;
m_incl=sum(of col1-col&nsamp);
p_incl=100*(m_incl/&nsamp);
if m_incl=. then m_incl=0;
if p_incl=. then p_incl=0;
nn=_N_;
keep _NAME_ m_incl p_incl nn;
run;
data _null1;
        set _bparms;
        array new {&totnrvar} &totvarnms;
        do i=1 to &totnrvar;
		  if new[i]>0 and new[i]^=. then new[i]=1;
		  else new[i]=.;
        end;
		drop i;
run;
proc transpose data=_null1 out=_worknull; run;
data _worknull; set _worknull; nn=_N_; run;
/* */
data _test; merge _test _worknull; by nn;
m_pos=sum(of col1-col&nsamp);
p_pos=100*(m_pos/&nsamp);
if m_pos=. then m_pos=0;
if p_pos=. then p_pos=0;
keep _NAME_ m_incl p_incl m_pos p_pos nn;
run;
data _null1;
        set _bparms;
        array new {&totnrvar} &totvarnms;
        do i=1 to &totnrvar;
		  if new[i]<0 and new[i]^=. then new[i]=1;
		  else new[i]=.;
        end;
		drop i;
run;
proc transpose data=_null1 out=_worknull; run;
data _worknull; set _worknull; nn=_N_; run;
/* */
data _test; merge _test _worknull; by nn;
length _variable $30.;
m_neg=sum(of col1-col&nsamp);
p_neg=100*(m_neg/&nsamp);
if m_neg=. then m_neg=0;
if p_neg=. then p_neg=0;
label m_incl='Inclusion (#)';
label p_incl='Inclusion (%)';
label m_pos='Pos. incl. (#)';
label p_pos='Pos. incl. (%)';
label m_neg='Neg. incl. (#)';
label p_neg='Neg. incl. (%)';
format p_incl 6.1;
format p_pos 6.1;
format p_neg 6.1;
%do ii=1 %to &totnrvar;
 if _N_=&ii then _variable="&&label&ii";
%end;
label _variable='Variable';
keep _variable m_incl p_incl m_pos p_pos m_neg p_neg;
run;

/* mean of beta parameters */

proc means data=_bparms noprint;
 var &totvarnms;
 output out=_null1;
run;
data _null1(keep=stat &totvarnms); set _null1;
stat='Beta: without missing';
if _STAT_='MEAN';
run;

data _bparms0;
 set _bparms;
 array estims {&totnrvar} &totvarnms;
 do i=1 to &totnrvar;
          if estims[i]=. then estims[i]=0;
 end; drop i;
run;

proc means data=_bparms0 noprint;
 var &totvarnms;
 output out=_null2;
run;
data _null2(keep=stat &totvarnms); set _null2;
stat='Beta: missing set to 0';
if _STAT_='MEAN';
run;

data _outb; set _null1 _null2; run;

data _outb; set _outb;
%let i=1;
   %let word=%qscan(&totvarnms,&i,%str( ));
   %do %while(&word ne);
    label &word.="&&label&i";
    %let i=%eval(&i+1) ;
    %let word=%qscan(&totvarnms,&i,%str( ));
   %END;
run;


/* mean of st.dev parameters */

proc means data=_vparms noprint;
 var &totvarnms;
 output out=_null1;
run;
data _null1(keep=stat &totvarnms); set _null1;
stat='StDev: without missing';
if _STAT_='MEAN';
run;

data _vparms0;
 set _vparms;
 array estims {&totnrvar} &totvarnms;
 do i=1 to &totnrvar;
          if estims[i]=. then estims[i]=0;
 end; drop i;
run;

proc means data=_vparms0 noprint;
 var &totvarnms;
 output out=_null2;
run;
data _null2(keep=stat &totvarnms); set _null2;
stat='StDev: missing set to 0';
if _STAT_='MEAN';
run;

/* bootstrap variance */

proc means data=_vparms noprint;
 var &totvarnms;
 output out=_null3;
run;
data _null3(keep=stat &totvarnms); set _null3;
stat='StDev: Bootstrap';
if _STAT_='STD';
run;

data _outv; set _null1 _null2 _null3; run;

data _outv; set _outv;
%let i=1;
   %let word=%qscan(&totvarnms,&i,%str( ));
   %do %while(&word ne);
    label &word.="&&label&i";
    %let i=%eval(&i+1) ;
    %let word=%qscan(&totvarnms,&i,%str( ));
   %END;
run;

data _test2;
set _outb _outv;
run;

proc transpose data=_test2 out=_outall; run;
data _outall; set _outall;
length _variable $30.;
label col1='Beta: without missing';
label col2='Beta: missing set to 0';
label col3='SD: without missing';
label col4='SD: missing set to 0';
label col5='SD: Bootstrap';
col6=exp(col1);
col7=exp(col2);
label col6='HR: without missing';
label col7='HR: missing set to 1';
%do ii=1 %to &totnrvar;
 if _N_=&ii then _variable="&&label&ii";
%end;
label _variable='Variable';
keep _variable col1-col7;
run;

ODS RTF FILE="&file.\inclusion.rtf" STARTPAGE=NO bodytitle;
title1 'Inclusion frequencies';
proc print data=_test label; var _variable m_incl p_incl m_pos p_pos m_neg p_neg; run;
title1 'Averages of the obtained coefficient estimates';
proc print data=_outb noobs label; var stat &totvarnms; run;
title1 'Averages of the obtained standard deviation estimates';
proc print data=_outv noobs label; var stat &totvarnms; run;
title1 'Summary table';
proc print data=_outall label; var _variable col1-col7; run;
ODS RTF CLOSE;

data _bootvar; set _outv; where stat='StDev: Bootstrap'; 
_NAME_='bootvar'; keep _NAME_ &totvarnms; run;

/**********************************
*****     model averaging *********
***********************************/



/**
**/
data _mnew;
        set _bparms;
        array new {&totnrvar} &totvarnms;
        array mnew {&totnrvar} mnew1-mnew&totnrvar;
        do i=1 to &totnrvar;
          if new[i]>0 then mnew[i]=1;
          if new[i]<0 then mnew[i]=-1;
          if new[i]=. then mnew[i]=0;
          if new[i]^=. then new[i]=1;
          if new[i]=. then new[i]=0;
        end;
run;


/* to know which models are selected */
/**
**/
title 'Selected models';
proc freq data=_mnew;
        table &freqlist / list sparse noprint out=_selmod;
run;
data _selmod ;
set _selmod;
where count>0;
run;

data _best;
set _selmod;
run;
proc sort data=_best; by descending count; run;
data _best10; 
set _best ; l+1; output; run;
data _best10; set _best10;
%let i=1;
   %let word=%qscan(&totvarnms,&i,%str( ));
   %do %while(&word ne);
    label &word.="&&label&i";
    %let i=%eval(&i+1);
    %let word=%qscan(&totvarnms,&i,%str( ));
   %END;
label l="Model ranking";
format PERCENT 6.1;
run;

ODS RTF FILE="&file.\best10m.rtf" BODYTITLE STARTPAGE=NO;
title1 'Ten highest model frequencies';
title2 "based on &nsamp bootstrap samples";
proc print label noobs; where l<11;run;
ODS RTF CLOSE;

%if &mavg=Y %then %do; 


data _selmod (drop=count percent);
set _selmod;
wt=count/&nsamp;
run;
/*title 'Weights vector';
proc print data=_selmod;
var wt;
run;*/

/* fitting the selected models and averaging the parameters */
data _NULL_;
set _selmod end=last0;
if last0 then call symput('selmod',_n_);
run;

%let selmod=%left(&selmod);

proc transpose data=_selmod out=_temp2;
data _temp2b; set _temp2; 
if _name_="wt" then delete;
run;

data _temp4; set _empty; run;
data _temp4b; set _empty; run;
data _params0; set _empty; run;
data _covars0; set _empty; run; 


%do j=1 %to &selmod;

	data _temp3;
	set _temp2b end=last;
	if COL&j=1.0 then call symput('v'||left(put(_n_,3.)),_name_);
	else call symput('v'||left(put(_n_,3.)),' '); 
	if last then call symput('max',put(_n_,3.));
	run;

	%let string= %str();
	%do i=1 %to &max;
		%let string = %str(&string &&v&i);
	%end;
	
	proc phreg data=_stemp OUTEST=_osas1 covout noprint;
 	 model &timevar*_censor(1) = &string / risklimits;
	 %if %length(&strat)>0 %then %do; strata &strat; %end;
      run ;
	
 	proc sort data=_osas1; by _TIES_ _TYPE_; run;
	data _osas2 (drop=_TIES_ _TYPE_ _NAME_ _LNLIKE_ _STATUS_);
	set _osas1;
	by _TIES_;
	if last._TIES_;
	run;
/* */
	data _osas2;
	set _empty _osas2;
	run;

data _params&j; set _osas2; run;
data _params0; set _params0 _params&j; run; 

	proc append base=_temp4 data=_osas2;
	run;

	data _osas3;
	set _osas1;
	by _TIES_;
	if last._TIES_ then delete;
	run;

    proc sort data=_aid2; by _NAME_; run;
	proc sort data=_osas3; by _NAME_; run;
	data _osas3; merge _aid2 _osas3 ; by _NAME_; run;
	proc sort data=_osas3; by count; run; 

data _covars&j (drop=count _NAME_ _TIES_ _TYPE_ _STATUS_ _LNLIKE_); set _osas3; run;
data _covars0; set _covars0 _covars&j; run;



data _osas4 (keep= mnew1-mnew&totnrvar count);
	set _osas3 (drop=count);
    array mnew {&totnrvar} mnew1-mnew&totnrvar;
    array orig {&totnrvar} &totvarnms;
	do teller=1 to &totnrvar;
			if _N_=teller then do; count=teller; mnew[teller]=sqrt(orig[teller]); end;
	end;
run;


	data _osas5 (keep=se count);
	set _osas4;
	se = sum(of mnew1-mnew&totnrvar);
	run;

	proc sort data=_osas5; by count; run;

	proc transpose data=_osas5(where=(count^=.)) out=_osas6; var se; run;

	data _osas3 (drop=_NAME_);
	set _osas6;
	rename &renamelist;
	run;
	
	proc append base=_temp4b data=_osas3;
	run;

%end;

title;

proc transpose data=_temp4 out=_temp5;
data _temp2c;
set _temp2;
where _NAME_='wt';
run;
data _temp5;
set _temp5 _temp2c;
run;
proc transpose data=_temp5 out=_temp6;
/**
**/
%let nrvarplus=%eval(&totnrvar+1);

data _temp6pr (drop=i wt);
set _temp6;
array avpar {&nrvarplus} &totvarnms wt;
do i=1 to &totnrvar;
if avpar[i]=. then avpar[i]=0; else avpar[i]=avpar[i]*avpar[&nrvarplus]; end;
run;

proc transpose data=_temp4b out=_temp5b;
*proc print data=_temp5b; run;
data _temp2c;
set _temp2;
where _NAME_='wt';
run;
data _temp5b;
set _temp5b _temp2c;
run;
proc transpose data=_temp5b out=_temp6b;
/**
**/
data _temp6b (drop=i wt);
set _temp6b;
array avpar {&nrvarplus} &totvarnms wt;
do i=1 to &totnrvar;
if avpar[i]=. then avpar[i]=0; else avpar[i]=avpar[i]*avpar[i]*avpar[&nrvarplus]*avpar[&nrvarplus]; end;
run;

/**
**/
proc transpose data=_temp6pr out=_final1;run;
data _final1 (keep=mabeta);
set _final1;
mabeta = sum(of COL1-COL&selmod);
run;
proc transpose data=_final1 out=_final1; run;
data _final1;
set _final1;
rename &renamelist;
run;
proc transpose data=_temp6b out=_final2; run;
data _final2 (keep=mavar);
set _final2;
mavar = sum(of COL1-COL&selmod);
run;
proc transpose data=_final2 out=_final2;run;
data _final2;
set _final2;
rename &renamelist;
run;

title 'MA parameter estimates and variances';
data _final;
set _final1 _final2 _bootvar;
proc print; run;

proc transpose data=_final out=_results; run;
proc print data=_results; run;

data _results;
set _results;
div=mabeta/bootvar;
pval=2*(1-probnorm(abs(div)));
HR=exp(mabeta);
label mabeta='MA Beta';
label mavar='MA Var';
label bootvar='Bootstrap Var';
label pval='P-value';
label hr='MA HR';
%do ii=1 %to &totnrvar;
 if _N_=&ii then _NAME_="&&label&ii";
%end;
label _NAME_='Variable';
keep _NAME_ mabeta mavar bootvar pval hr;
run;

ODS RTF FILE="&file.\maresults.rtf" STARTPAGE=NO bodytitle;
title1 'Model averaging results';
title2 'Reported estimates are weighted averages according to the observed frequencies per model in the bootstrap run';
title3 "Estimates calculated on original dataset: effective sample size = &sampsize";
footnote1 'Note: reported variances are underestimations of the true values';
footnote2 'Note: p-values calculated on base of bootstrap variance';
proc print data=_results noobs label; run;
ODS RTF CLOSE;

proc export data=_bparms outfile="&file/bparams.xls" replace;

run;
quit;

proc datasets library=work nolist;
 delete _params1-_params&selmod. _covars1-_covars&selmod. _bootvar _temp2c _null3 ;
quit;


%end;

title; title1; title2; title3; footnote1; footnote2;

%if &logwin=N %then %do; 
PROC PRINTTO; RUN;
%end;

proc datasets library=work nolist;
 delete _work _aid1 _aid2 _empty _bparms _vparms _stemp _osas _osas1 _osas2 _osas3 _osas4 _osas5 _osas6
 _problem _null1 _null2 _best _best10 _bsamp _test _worknull _bparms0 _vparms0 _outb _outv _outall _test2
 _mnew _selmod _temp _temp2 _temp2b _temp3 _temp4 _temp4b _temp5 _temp5b _temp6 _temp6b _temp6pr _params0 _covars0
 _final _final1 _final2 _results;
quit;

%mend;

