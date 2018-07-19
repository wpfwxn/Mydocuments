/*************************************************************************************

testF2.SAS

* MACRO testF2 IS A MODULE OF THE NEW MA SOFTWARE ;
* It was created on the basis of the previous software MINISURV ;
* by Guido Hoctin-Boes and Luc Bijnens ;

**************
Creation date  : 09/2006
Software       : SAS version 9.1
Original author: Jérôme RAPION 
Based on summary macro written by Luc Bijnens (24/08/95) 
et Laurence Colette (20/11/95) 

version n°2 of testF macro ;

************************************************************************************

PARAMETERS

    SUMFILE           :  Name of the summary file created by module 1 (summary2 or summaryB3 macro)
    Outtest           :  Name for the output data set containing tests (SAS name, default= work.tests) 
    Outtotal          :  Name for the output data set containing totals (SAS name, default= work.totals) 
    Group             :  Group indicator (in case of subgroup analysis)
    Total             :  Indicator of the grouping for the subtotals
    Tren              :  Parameter containing the value of the category "unknown" of the group variable
                         see notes below;
    Trendindic        :  Parameter indicates to the macro if the Trend test should be done or not;
                         1=Trend test should be done, 0=not. (by default Trenindic=0)
                         see notes below;
    Round             :  Set to Y for rounded p-values, N otherwise  (default= N);

NOTES

    TREN parameter: The category "unknown" is printed on the forest plot but not taken into account in the 
    computation of the trend test ;

    TRENDINDIC parameter: if GROUP contains one variable, Trendindic takes only one value,
    If GROUP contains several variables, TRENDINDIC should take several values, one per variable contained in GROUP.
    If there are less values in TRENDINDIC parameter than in GROUP parameter, this parameter will be completed with 0 
    so as to have the same number of values in TRENDINDIC and GROUP paramter

    The heterogeneity and interaction test of Peto is computed  following Berlin, laird, Sacks and Chalmers
    Stats in MED vol 8: 141-151 (1989)

    The macro for formatting the p-values are used (%pvalrnd and %pval macros);

EXAMPLES

    %testF2(SUMFILE=summary);

    %testF2(SUMFILE=summary,OUTTEST=TEST1, OUTTOTAL=TOTAL1,GROUP=SEX);

    %testF2(SUMFILE=summary,OUTTEST=TEST1, OUTTOTAL=TOTAL1,TOTAL=SEX,ROUND=Y);

    %testF2(SUMFILE=summary,GROUP=AGECAT,TOTAL=SEX,ROUND=Y);

    %testF2(SUMFILE=summary,OUTTEST=TEST1, OUTTOTAL=TOTAL1,GROUP=SEX AGECAT,TRENDINDIC=0 1);

************************************************************************************

HISTORY

* LB  24/08/95   : Creation of testF.sas
* LC  20/11/95   : Minor change of testF    
* LC  18/09/96   : Minor change of testF                                 
* LC  20/03/2001 : change LCL&conf2 to LCL_&conf2 
* JRA 25/09/2006 : Replacement of testF macro by testF2 macro.
				     Deletion of the call of %benefits macro and deletion of all parameters
				     depending on %benefits macro call (deletion of benefits, ipdfile, var,
				     timevar, censvar, censval parameters)
				     Add of a condition with the use of input dataset &summfile :
				     %if (%upcase(&subtotal)=Y) %then (where=(&total. ne .))

* JRA 05/03/2006  Correction of MERGE between _summ and _summ1 datasets with a BY.
* JRA 09/02/2009  Add the possibilty to have several variables in GROUP parameter
                  Add of Trendindic parameter to indicates if the TREND test should be done or not
* JRA 20/05/2010  Add of TREND variable into TEST dataset in case of parameter GROUP countains 1 variable or less.
* JRA 13/02/2015  Add of I2 computation and its confidence intervals, base on Borenstein book, 2009
				 (Introduction to Meta-Analysis:  Chapter 16 Identifying and Quantifying Heterogeneity);

************************************************************************************/

%macro testF2(summfile,outtest,outtotal, group, total , tren, round,TRENDINDIC);

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

%GLOBAL NB NBTR;
%LET NB=0;
%LET NBTR=0;

** JRA, January 2009, decompositon of vector &GROUP with each element created into &&A&i variable;
** Number of elements of vector &group put into &nb variable;
%if %length(&group.)>0 %then %RCHV(SUITE=&group,COMPTE=A,SEP=" ",N=NB);

%if (%length(%cmpres(&TRENDINDIC))=0) %then %let TRENDINDIC=0 ;
%if %length(&TRENDINDIC.)>0 and &TRENDINDIC. ne 0 %then %RCHV(SUITE=&TRENDINDIC,COMPTE=TR,SEP=" ",N=NBTR);

%if &NBTR lt &NB %then %do;

	%let NDIF=%EVAL(&NB-&NBTR);
	%put &NDIF;

	%DO K = %EVAL(&NBTR+1) %TO &NB;
		%LET TR&k=0;
		%PUT &&TR&k;
	%END;

%end;

data z;
alpha=&conf1;
z=abs(probit((100-alpha)/200));
call symput('z',compress(z));
run;
%put z &z.;

%if (%length(%cmpres(&total))>0) %then %let subtotal=Y ;
%if (%length(%cmpres(&total))=0) %then %let subtotal=N ;
%if (%length(%cmpres(&group))>0) %then %let subgroup=Y ;
%if (%length(%cmpres(&group))=0) %then %let subgroup=N ;
%if (%length(%cmpres(&tren))=0) %then %let tren=. ;

%if not(&subtotal=Y and &subgroup=N) %then %do;
	%if &NBTR gt &NB %then %do;

	%PUT ;%PUT WARNING !! ; %PUT ;
	%PUT IT IS NOT POSSIBLE TO HAVE MORE VALUES IN TRENDINDIC THAN IN GROUP PARAMETER ;
	%PUT YOU SHOULD HAVE THE SAME NUMBER OF VALUES IN BOTH PARAMTERS;
	%PUT OR LESS VALUES IN TRENDINDIC THAN IN GROUP PARAMETER (in this case, 0 is filling the blanks);
	%PUT;

	%goto fin ;

	%end;
%end;

%if (%LENGTH(&total.)>0) and (&NB >1)  %then %do;

	%PUT ;%PUT WARNING !! ; %PUT ;
	%PUT IT IS NOT POSSIBLE TO HAVE A VARIABLE IN TOTAL PARAMETER ;
	%PUT AND IN THE SAME TIME SEVERAL VARIABLES IN GROUP PARAMETER;
	%PUT YOU SHOULD HAVE ONLY ONE VARIABLE IN GROUP PARAMETER IF YOU WANT TO USE TOTAL PARAMETER;
	%PUT OR NOTHING IN TOTAL PARAMETER IF YOU WANT TO HAVE SEVERAL VARIABLES IN GROUP PARAMETER;
	%PUT;

%goto fin ;

%end;

%if %length(%scan(&outtest,2))^=0
                      %then %do ;
                      %let libtest=%scan(&outtest, 1);
                      %let nametest=%scan(&outtest,2);
                      %end ;
%else %do ;
                 %let libtest=WORK ;
                 %if %length(%scan(&outtest,1))^=0  %then  %let nametest=%scan(&outtest,1) ;
                 %else %let nametest=TESTS ;
          %end ;

%if %length(%scan(&outtotal,2))^=0
                      %then %do ;
                      %let libtot=%scan(&outtotal, 1);
                      %let nametot=%scan(&outtotal,2);
                      %end ;
%else %do ;
                 %let libtot=WORK ;
                 %if %length(%scan(&outtotal,1))^=0  %then  %let nametot=%scan(&outtotal,1) ;
                 %else %let nametot=TOTALS ;
          %end ;

proc datasets library=&libtest nolist ;
 delete &nametest ;
 run ;

proc datasets library=&libtot nolist ;
 delete &nametot;
 run ;

%let LRCHI=;
%let HTCHI=;
%let ITCHI=;
%let TTCHI=;
%let TTP=;
%let LRP=;
%let HTP=;
%let ITP=;
%let TTZ=;
%let LRZ=;
%let HTDF=;
%let ITDF=;

title ;

* COMPUTES THE HETEROGENEITY TEST (both subgroup=Y or subgroup=N) ;

*** CASE IF GROUP PARAMATER CONTAINS MORE THAN 1 VARIABLE ;
%IF &NB > 1 %THEN %DO;
	data __summfile;
		set &summfile;
		%do i=1 %to &NB;
		if &&a&i ne . then flag=&i;
		%end;
		if flag=. then flag=99;
	run;	
	%sort(data=__summfile,var=flag);
%END;

data _peto;
%IF &NB > 1 %THEN %DO;
   set __summfile(where=(flag ne 99));
%END;
%IF &NB <=1 %THEN %DO;
   set &summfile %if (%upcase(&subtotal)=Y) %then (where=(&total. ne .));;
%END;
   sqOE=(O_E)**2;
   part1=sqOE/varO_E;
run;

proc means data=_peto noprint;
     var part1 O_E VarO_E;

	 %IF &NB > 1 %THEN %DO;
		 by flag;
	 %END;

	 %IF &NB <=1 %THEN %DO;
	     %if (%upcase(&subtotal)=Y) %then %do ;
	     class &total ;
	     %end ;
	 %END;
     output out=_summ n=n sum=smprt1 smOE smvarOE;
run;


data _summ (drop=htpeto smprt1 smOE smvarOE);
    set _summ;
    HTdf=n-1;
    HTpeto=smprt1-((smOE**2)/smvarOE);
    p=1-probchi(Htpeto,HTdf,0.0);
    length HTchi $ 5;
    HTchi=left(trim(round(htpeto,.01)));

    if (_TYPE_=0) then do ;

	 %IF &NB <=1 %THEN %DO;
	    call symput('HTCHI',HTchi);
	    call symput('HTDF', compress(HTdf));
	 %END;


	 %IF &NB > 1 %THEN %DO;
	    call symput('HTCHI'||compress(flag),HTchi);
	    call symput('HTDF'||compress(flag), compress(HTdf));
	 %END;

    end ;

    label HTpeto='Heterogeneity*Chi-square' htdf='df' p='P-value' ;
run;


%if (%upcase(&round)=Y) %then %pvalrnd(pdata=_summ, pname=p,npname=newp) ;
%else %pval(pdata=_summ,pname=p,npname=newp) ;

data _summ ;
 set _summ ;
 if (_TYPE_=0) then do ;
 	 %IF &NB <=1 %THEN %DO;
		 call symput('HTP',newp) ;
     %END;
 	 %IF &NB > 1 %THEN %DO;
         call symput('HTP'||compress(flag),newp) ;
     %END;

 end ;
 run ;

** JRA, 13022015, add of I2 computation in the case of TOTAL parameter use;

data _summ;
set _summ;
length I2txt $30.;
	Q=HTCHI; 
	DF=HTDF;
	I2=100*(Q-DF)/Q;

	if Q > (DF+1) and DF > 1 then 
	B=0.5 * (log(Q) - log (DF))/ ( (2*Q)**0.5 - (2*DF-1)**0.5 );

	if Q <= (DF+1) and DF > 1  then 
	B=(1/(2*(DF-1)*(1-(1/(3*(DF-1)**2)))))**0.5;

	L=exp(0.5*log(Q/DF)-%sysevalf(&z)*B);
	U=exp(0.5*log(Q/DF)+%sysevalf(&z)*B);

	LLI2=(L**2-1)/(L**2)*100;
	ULI2=(U**2-1)/(U**2)*100;

	if .Z<LLI2<0   then LLI2=0;

	if .Z<I2 <0 then I2=0;

	if _TYPE_ =1 then do;
		if DF>1 then
		I2txt= compress(round(I2,0.1)) ||'% ('||compress(round(LLI2,0.1))||'% ; ' || compress(round(ULI2,0.1))||'%) ';
		else 
		I2txt= compress(round(I2,0.1))||'%'  ;
	end;

	drop L U B LLI2 ULI2 ;

run;

proc print data=_summ noobs split="*";
    var %if &nb >1 %then %do; flag %end; HTchi htdf p newp I2;
    title1  "Heterogeneity test using Peto's method";
    title2  "on file :  &summfile ";
    label HTchi='Heterog*Chi²' newp='Formatted*P-value' p='P-value' htdf='DF' I2="I2";
run;

* COMPUTES THE SUBTOTALS (in case subtotal=Y) and the grand total (in any case) ;

proc means %if &nb >1 %then %do; data=__summfile  (where=(flag = 99)) %end;
 %if &nb <=1 %then %do; data=&summfile  %end; noprint;

%if &nb <=1 %then %do;
	%if (%upcase(&subtotal)=Y) %then %do ;
       class &total ;
     %end ;
%end;
     var ndeath1 ndeath2 npat1 npat2 O_E varO_E ;
	 %if &nb >1 %then %do; by flag; %end;
     output out=_summ1 sum= ndeath1 ndeath2 npat1 npat2 O_E varO_E n=n;
run;


%IF &NB <=1 %THEN %DO;

	data _summ1;
		 merge _summ1 _summ ;
		 by _TYPE_
		%if (%upcase(&subtotal)=Y) %then %do ;
		 &total 
		 %end ;
		 ;
	 run;

 %END;

 %IF &NB > 1 %THEN %DO;

	%sort(data=_summ1,var=flag);
	%sort(data=_summ,var=flag);

	data _summ1;
	 merge _summ1 _summ ;
	 by flag;
	run;

 %END;

data &libtot..&nametot
 %IF &NB > 1 %THEN %DO;
 (where=(flag = 99))
 %END;
;
 attrib change length=$9 ;
 set _summ1 ;
 indic=_TYPE_ ;
 OR=exp(O_E/varO_E) ;
 LCL&conf1=exp(O_E/varO_E-(probit(1-(100-&conf1)/200)/sqrt(varO_E)));
 UCL&conf1=exp(O_E/varO_E+(probit(1-(100-&conf1)/200)/sqrt(varO_E)));
 LCL_&conf2=exp(O_E/varO_E-(probit(1-(100-&conf2)/200)/sqrt(varO_E)));
 UCL_&conf2=exp(O_E/varO_E+(probit(1-(100-&conf2)/200)/sqrt(varO_E)));
 cent1_OR=(1-OR)*100 ;
 HTp=newp ;
 if (abs(cent1_OR))<=0.5 then cent1_OR=0 ;
 SD=-int( round( (1-OR)*100 / (O_E/sqrt(varO_E)) ) ) ;
 if (CENT1_OR>0) then change='reduction' ;
 else do ;
               change='increase' ;
               CENT1_OR=-CENT1_OR ;
               end ;
 if (CENT1_OR=0) then do ;
              change='  ' ;
              end ;
 Cent1_OR=int(round(cent1_OR)) ;
 drop n _TYPE_ _FREQ_ newp p;
run;


 %IF &NB <=1 %THEN %DO;
	* COMPUTES THE INTERACTION TEST  (in case subtotal=Y) ;

	%if (%upcase(&subtotal)=Y) %then %do ;

	data _summ1;
	   set _summ1;
	   sqOE=(O_E)**2;
	   part1=sqOE/varO_E;
	   if (&total not=' ');
	run;
	proc means data=_summ1 noprint;
	     var part1 O_E varO_E;
	     output out=_summ2 n=n sum=smprt1 smOE smvarOE;
	run;
	data _summ3 (drop=itpeto);
	    set _summ2;
	    df=n-1;
	    ITPeto=smprt1-((smOE**2)/smvarOE);
	    p=1-probchi(ITPeto,df,0.0);
	    length ITchi $ 5;
	    ITchi=left(trim(round(itpeto,.01))) ;
	    call symput('ITCHI',ITchi);
	    call symput('ITDF', compress(df));
	run;

	%if (%upcase(&round)=Y) %then %pvalrnd(pdata=_summ3,pname=p, npname=newp) ;
	%else %pval(pdata=_summ3,pname=p, npname=newp) ;

	data _summ3 ;
	 set _summ3 ;
	 call symput('ITP', newp) ;
	 run ;

	proc print data=_summ3 noobs split="*";
	    var ITchi df p newp;
	    title1 "Test for interaction between treatment and &group" ;
	    title2 "using Peto's method";
	    title3 "on file &summfile ";
	    label ITchi ='Interaction*Chi-square'
	          p = 'P-value' newp='Formatted*P-value' df='df';
	    run;

	%end; * end of  subtotal=Y;
%END;

* COMPUTES THE TEST FOR TREATMENT EFFECT (subgroup=Y or subgroup=N) ;

proc means %IF &NB >1 %THEN %DO; data=__summfile (where=(_Num = 99)) %END;
%IF &NB <=1 %THEN %DO; data=&summfile 
	%IF (%upcase(&subtotal)=Y) %THEN (where=(&total ne .));
%END;
noprint;
   var O_E varO_E;
   output out=_pmeta sum= O_E varO_E n=n ;
run;

run;

data _pmeta (drop=Z absZ chi p);
set _pmeta;
   z=O_E/sqrt(varO_E);
   chi=z*z ;
   absz=abs(z);
   p=probnorm(absz);
   two_p=(1-p)*2;
    length TrtZ $ 5;
    length Trtchi $ 5;
    TrtZ=left(trim(round(z,.01))) ;
    Trtchi=left(trim(round(chi,.01))) ;
    call symput('LRCHI', Trtchi) ;
    call symput('LRZ',TrtZ);
run;

%if (%upcase(&round)=Y) %then %pvalrnd(pdata=_pmeta, pname=two_p, npname=newp) ;
%else %pval(pdata=_pmeta, pname=two_p, npname=newp) ;

data _pmeta ;
 set _pmeta ;
 call symput('LRP',newp) ; 
run ;

proc print data=_pmeta noobs split="*";
  title1 'Overall Logrank Test for treatment effect';
  title2 "on file &summfile";
  var TrtCHI TrtZ two_p newp ;
  label TrtCHI='Treatment effect*Chi-square(1)' Trtz='Z value' two_p='P-value' newp='Formatted*P-Value' ;
  run;

%if %upcase(&subgroup)=Y %then  %do;

* IN CASE OF SUBGROUP ANALYSIS: COMPUTES THE TREND TEST  ;
	 %IF &NB <=1 %THEN %DO;
		data _trend;
		    set &summfile ;
		    where (&group^=&tren) ;
		    weig=&group ;
		    A=varO_E;
		    B=weig*varO_E;
		    C=(weig**2)*varO_E;
		    D=O_E;
		    E=weig*O_E;
		    run;
	 %END;

	 %IF &NB > 1 %THEN %DO;
		%do i=1 %to &NB;

		data _trend&i;
		    set __summfile ;

		    weig=&&a&i ;
			if weig ne . then do;
		    A=varO_E;
		    B=weig*varO_E;
		    C=(weig**2)*varO_E;
		    D=O_E;
		    E=weig*O_E;
			end;
			if weig ne .;
		run;


		%end;

		data _trend;
			set %do i=1 %to &NB; _trend&i %end;;
		run;

		%sort(data=_trend,var=flag weig);

	 %END;

proc means data=_trend noprint;
  var A B C D E ;
   	 %IF &NB > 1 %THEN %DO; by flag; %END;
  output out=_summ5 n=n sum=A B C D E ;
  run;

data _summ5 (drop=trend abstrend chi p) ;
 set _summ5;
 df=n-1;
 trend=(E-(D*B/A))/((C-((B*B)/A))**0.5);
 chi=trend*trend ;
 abstrend=abs(trend);
 p=probnorm(abstrend);
 ttwo_p=2*(1-p);
 length TrZ $ 5;
  length Trchi $ 5 ;
   TrZ=left(trim(round(trend,.01))) ;
   Trchi=left(trim(round(chi, .01)));

%IF &NB <=1 %THEN %DO;
   call symput('TTZ',TrZ);
   call symput('TTCHI', TrChi);
%END;
%IF &NB > 1 %THEN %DO;
   call symput('TTZ'||compress(flag),TrZ);
   call symput('TTCHI'||compress(flag), TrChi);
%END;

label trchi='Trend Chi-square(1)' trz='z value' ttwo_p='P-value' ;
 run;


%if (%upcase(&round)=Y) %then %pvalrnd(pdata=_summ5, pname=ttwo_p, npname=newp) ;
%else %pval(pdata=_summ5, pname=ttwo_p, npname=newp) ;

data _summ5 ;
 set _summ5 ;
%IF &NB <=1 %THEN %DO;
  call symput('TTP',newp) ;
%END;
%IF &NB > 1 %THEN %DO;
  call symput('TTP'||compress(flag),newp) ;
%END;
run ;


proc print data=_summ5 noobs split="*";
 var %IF &NB > 1 %THEN %DO; flag %END;trchi trz ttwo_p newp;
 title1  "Test for trend of &tren";
 title2  "following EBCTCG 1985-1990, p.17";
 title3  "by LB 2/3/95";
 title4  "on file &summfile";
 label newp='Formatted*P-value' ;
 run;

proc datasets library=WORK nolist ;
 delete _trend _summ5 ;
 run ;

%end; * END of SUBGROUPS ANALYSIS ;

** JRA, 13022015, add of I2 computation ;

***********************************;
*** Computation of I2 (with 95% CI);
***********************************;

%IF &NB <=1 %THEN %DO;

data _I2;

	Q=&HTCHI;
	DF=&HTDF;
	I2=100*(Q-DF)/Q;

	if Q > (DF+1) and DF > 1 then 
	B=0.5 * (log(Q) - log (DF))/ ( (2*Q)**0.5 - (2*DF-1)**0.5 );

	if Q <= (DF+1) and DF > 1  then 
	B=(1/(2*(DF-1)*(1-(1/(3*(DF-1)**2)))))**0.5;

	L=exp(0.5*log(Q/DF)-%sysevalf(&z)*B);
	U=exp(0.5*log(Q/DF)+%sysevalf(&z)*B);

	LLI2=(L**2-1)/(L**2)*100;
	ULI2=(U**2-1)/(U**2)*100;

	if .Z<LLI2<0   then LLI2=0;

	if .Z < ULI2<0   then ULI2=0;

	if LLI2=0 and ULI2=0 then do;
		LLI2=.;
		ULI2=.;
	end;

	if .Z<I2 <0 then I2=0;
	call symput('I2val',compress(round(I2,0.1)));
	call symput('LLI2',compress(round(LLI2,0.1)));
	call symput('ULI2',compress(round(ULI2,0.1)));

run;

%let I2val=&I2val%;
%let LLI2=&LLI2%;
%let ULI2=&ULI2%;

%put I2val &I2val LLI2 &LLI2 ULI2 &ULI2;

%IF &HTDF > 1 %THEN %let I2=%str(&I2val (&LLI2 ; &ULI2));
%ELSE %let I2=%str(&I2val);

%IF &LLI2 =.% %THEN %let I2=%str(&I2val);
%IF &I2val =.% %THEN %let I2=;

%put I2 &I2;

%END;

%IF &NB > 1 %THEN %DO;

data _I2;

%do j=1 %to &NB;

	Q=&&HTCHI&j;
	DF=&&HTDF&j;
	I2=100*(Q-DF)/Q;
	if .Z<I2 <0 then I2=0;

	if Q > (DF+1) and DF > 1 then 
	B=0.5 * (log(Q) - log (DF))/ ( (2*Q)**0.5 - (2*DF-1)**0.5 );

	if Q <= (DF+1) and DF > 1  then 
	B=(1/(2*(DF-1)*(1-(1/(3*(DF-1)**2)))))**0.5;

	L=exp(0.5*log(Q/DF)-%sysevalf(&z)*B);
	U=exp(0.5*log(Q/DF)+%sysevalf(&z)*B);

	LLI2=(L**2-1)/(L**2)*100;
	ULI2=(U**2-1)/(U**2)*100;

	if .Z < LLI2<0   then LLI2=0;
	if .Z < ULI2<0   then ULI2=0;

	if LLI2=0 and ULI2=0 then do;
		LLI2=.;
		ULI2=.;
	end;

	if .Z< I2 <0 then I2=0;

	ind=&j;

	output;
	call symput('I2val'||compress(ind),compress(round(I2,0.1)));
	call symput('LLI2'||compress(ind),compress(round(LLI2,0.1)));
	call symput('ULI2'||compress(ind),compress(round(ULI2,0.1)));

%end;

run;

%do j=1 %to &NB;
	%let I2val&j=&&I2val&j%;
	%let LLI2&j=&&LLI2&j%;
	%let ULI2&j=&&ULI2&j%;

%IF &&HTDF&j > 1 %THEN %let I2&j=%str(&&I2val&j (&&LLI2&j ; &&ULI2&j));
%ELSE %let I2&j=%str(&&I2val&j);

%IF &&LLI2&j=.% %THEN %let I2&j=%str(&&I2val&j);
%IF &&I2val&j =.% %THEN %let I2&j=;

%end;


%END;



* BUILDING OF THE TEST DATA SET ;

data &libtest..&nametest ;
%IF &NB <=1 %THEN %DO;
  attrib TEv THv TIv TTRv length=$8 ;
  do i=1 to 4 ;
  TE=i ;
  TH=i ;
  TI=i ;
  TTR=i ;
  I2=i;

  **JRA, May2010, add of TREND variable in case of NB <=1 in the TEST data set;
  trend=&TRENDINDIC;

  if (i=1) then do ;
     *Chi-square test ;
     TEv=symget('LRCHI') ;
     THv=symget('HTCHI');
     TIv=symget('ITCHI');
     TTRv=symget('TTCHI') ;
	 I2v=symget('I2');
	 output;
     end ;
  if (i=2) then do ;
     * df of the test;
     TEv='1' ;
     THv=symget('HTDF');
     TIv=symget('ITDF') ;
     TTRv='1';
	 I2v='';
	 output ;
     end ;
  if (i=3) then do ;
     * p-value ;
     TEv=symget('LRP') ;
     THv=symget('HTP') ;
     TIv=symget('ITP');
     TTRv=symget('TTP');
 	 I2v='';
    output ;
     end;
  if (i=4) then do ;
     * z test;
     TEv=symget('LRZ') ;
     THv=' ' ;
     TIv=' ' ;
     TTRv=symget('TTZ');
 	 I2v='';
    output ;
     end;
  end ;

  drop i ;
%END;




%IF &NB > 1 %THEN %DO;
%do j=1 %to &NB;

	  flag=&j;
	  trend=&&TR&j;

	  attrib TEv THv TIv TTRv length=$8 I2v length=$30;
	  do i=1 to 4 ;
	  TE=i ;
	  TH=i ;
	  TTR=i ;
      TI=i ;
	  I2=i;

	  if (i=1) then do ;
	     *Chi-square test ;
	     TEv=symget("LRCHI") ;
	     THv=symget("HTCHI&j.");
	     TTRv=symget("TTCHI&j.") ;
	     TIv="";
 		** JRA, 13022015, add of I2 computation ;
 		 I2v=symget("I2&j");
       output;
	     end ;
	  if (i=2) then do ;
	     * df of the test;
	     TEv='1' ;
	     THv=symget("HTDF&j.");
	     TTRv='1';
	     TIv="";
  		 I2v="";
	    output ;
	     end ;
	  if (i=3) then do ;
	     * p-value ;
	     TEv=symget("LRP") ;
	     THv=symget("HTP&j.") ;
	     TTRv=symget("TTP&j.");
	     TIv="";
  		 I2v="";
	     output ;
	     end;
	  if (i=4) then do ;
	     * z test;
	     TEv=symget("LRZ") ;
	     THv=' ' ;
	     TTRv=symget("TTZ&j.");
	     TIv="";
  		 I2v="";
	     output ;
	     end;
	  end ;
	  drop i ;
%END;
%end;

run ;




proc print data=&libtot..&nametot noobs;
  title "TOTALS DATA SET &libtot..&nametot";
%if (%upcase(&subtotal)=Y) %then %do ;
  var indic &total Ndeath1 Npat1 Ndeath2 Npat2 O_E varO_E OR LCL&conf1 UCL&conf1
  LCL_&conf2 UCL_&conf2 cent1_OR change SD HTchi HTdf HTp ;
%end ;
%else %do ;
  var indic Ndeath1 Npat1 Ndeath2 Npat2 O_E varO_E OR LCL&conf1 UCL&conf1
  LCL_&conf2 UCL_&conf2 cent1_OR change SD HTchi HTdf HTp ;
%end ;
  run ;

proc print data=&libtest..&nametest label noobs;
 title "TESTS DATA SET &libtest..&nametest";
 var %IF &NB > 1 %THEN %DO; flag %END; TE TEv TH THv TI TIv TTR TTRv I2 I2v;
 label TE='Trt effect' TH='Heterogeneity' TI='Interaction' TTR='Trend' I2='I square';
 run ;

 
proc datasets library=WORK nolist ;
 delete _peto _summ _summ1 _summ2 _summ3 _pmeta __summfile _i2 z
	%IF &NB > 1 %THEN %DO;
	%do j=1 %to &NB;
	_trend&j
	%end;
	%END;
;
 run ;
quit;



%do i=1 %to &NBTR;
%let TR&i=;
%end;

%fin: ;


%mend testF2 ;

