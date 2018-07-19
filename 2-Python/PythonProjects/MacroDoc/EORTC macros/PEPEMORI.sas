/*************************************************************************************

PEPEMORI.SAS
**********

Compute the Pepe-Mori test for one event of interest accross two groups for competing risks data.
It compares the CIF-s of the two groups.

One SAS dataset is created in WORK library: Pepemori dataset
with Statistic, p-value exact and p-value rounded. 

Version date   : 10 february 2014
Software       : SAS version 9.4
Original author: Jerome RAPION
version: 2.0

Ref: 
-----
Pepe, M. S. and Mori, M. (1993). Kaplan–Meier, marginal or conditional probability curves in summarizing 
competing risks failure time data? Statistics in Medicine, 12, 737–751.
Book from Melania Pintilie, Competing risk, A pratical perspective, WILEY (Statistics in practise) (2004) 

***********************************************************************************
 REVISIONS:
 ---------
 October 2012  Modification of ROUND parameter
			   Instead of Y/N, put of a digit to specify the precision of rounding.
               Keep the format of RISVAR variable in the output dataset 

 Febrary 2013  In the output dataset PEPEMORI, the variable containing the risk named with the name of &RiskVar
			   is replaced with a variable named RISK.

 February 2014  JRA, Computation of results using SAS code instead of using R code.
               This new macro is based on the code of compcif.sas macro written by Melania Pintilie, 
               Princess Margaret Hospital, Toronto, CANADA.

			   see Paper: Pepe and Mori paper Statistics in Medicine 1993

			   see original macro code in http://www.uhnres.utoronto.ca/labs/hill/datasets/Pintilie/Rfunctions/compCIF.txt                

***********************************************************************************

PARAMETERS

    Data              :  Name of the data file (Required parameter)
    TestVar           :  Test variable (Required parameter)
    Timevar           :  Name of the time variable (Required parameter)
    RiskVar           :  Variable indicating patient's outcome (Required parameter) (see notes below)
    Censval           :  Value of &RiskVar indicating patient was censored (Optionnal parameter) (default=0)
    RiskVal           :  Value of &RiskVar indicating patient experienced the event of interest (Optionnal parameter)
						(default=1)
    Where             :  Where condition  (Optionnal parameter)
    ROUND             :  a digit to indicate the precision of the rounding for p-value,  
                         (default=4) (Optionnal parameter) (see notes)

NOTES

    For patients experiencing more than one event, only the earliest event is considered in the analysis.

    The &TimeVarVar variable should contain the time of the earliest event, whether this was the event of interest 
    or a competing event.  For censored patients, the &TimeVarVar variable should contain the time the patient was
    lost to follow up.

    The &RiskVar variable indicates which event the patient first experienced or whether the patient was 
    censored (neither event during the follow-up period).

    By default, the macro expects &RiskVar=0 to mean the patient was censored and &RiskVar=1 to mean the
    patient experienced the event of interest.  Use the &CensVal and &RiskVal parameters to override these defaults.

    Any patients with any other value of &RiskVar (even missing) are assumed to have experienced a competing
    event first.
  
	------
	ROUND:
	------
	ROUND=2 means rounding with 0.01 precision. By default, ROUND =4 with 0.0001 precision.

EXAMPLE
 
    %PEPEMORI(Data=Patient, TestVar=Trt1, TimeVar=DSUR, RiskVar=RISK);

************************************************************************************/


%macro PEPEMORI(data=, TimeVar=, RiskVar=, TestVar=,ROUND=4,CensVal=0,RiskVal=1,WHERE=);

%if %length(&where) = 0 %then %let where=1; 

data _data;
set &data;
	where &where;
run;

proc freq data=_data noprint;
table &TestVar./out=_tab ;
run;

data _null_;
set _tab end=eof;
i=_n_;
call symput('val'||compress(i),&testvar);
if eof then call symput('nval',i);

run;

%put &nval;

%if &nval > 2 %THEN %goto fin ;


%put val1 &val1;
%put val2 &val2;

proc freq data=_data noprint ;
tables &Riskvar. / out=_cens;
run;

data _cens;
set _cens;
ind=0;
if &RiskVar = &CensVal then ind=1;
if &RiskVar = &RiskVal then ind=2;
run;

data _null_;
set _cens;
where ind=0;
call symput('cmprisk',&RiskVar.);
run;

data _d1;set _data;
if &RiskVar in (&CensVal,&cmprisk.) then c02=1;else c02=0;
if &RiskVar ^ in (&CensVal,&RiskVal) then dcr=1;else dcr=0;
if &RiskVar=&RiskVal then dev=1;else dev=0;

data _dg1;set _d1;if &TestVar=&val1;keep &TimeVar c02 dev dcr;
proc means data=_dg1 sum noprint;
var c02 dev dcr ;class &TimeVar;
output out=_dg11 sum=c021 dev1 dcr1 ;
data _dg11;set _dg11;if _type_=1;keep &TimeVar c021 dev1 dcr1 ;

data _dg2;set _d1;if &TestVar=&val2; keep &TimeVar c02 dev dcr;
proc means data=_dg2 sum noprint;
var c02 dev dcr ;class &TimeVar;
output out=_dg21 sum=c022 dev2 dcr2 ;
data _dg21;set _dg21;if _type_=1;keep &TimeVar c022 dev2 dcr2 ;

proc means data=_d1 noprint;var &TimeVar;class &TestVar;
output out=_d2 max=tau n=n;
data __d21;set _d2;i=1;if &TestVar=&val1;rename n=n1 tau=tau1;keep n tau i;
data __d22;set _d2;i=1;if &TestVar=&val2;rename n=n2 tau=tau2;keep n tau i;

%sort(data=__d21,var=i);
%sort(data=__d22,var=i);

data _d2;merge __d21 __d22;by i;tau=min(tau1,tau2);keep n1 n2 tau i;

data _dg;merge _dg11 _dg21;by &TimeVar;
data _dg;set _dg;
if dev1=. then dev1=0;if dcr1=. then dcr1=0;if c021=. then c021=0;
if dev2=. then dev2=0;if dcr2=. then dcr2=0;if c022=. then c022=0;
data _dg;set _dg;if _n_=1 then set _d2;if &TimeVar<=tau;
data _dg;set _dg;retain atrisk1 s1 c1 f1 fcr1 atrisk2 s2 c2 f2 fcr2 ;
c021m=lag(c021);dev1m=lag(dev1);c022m=lag(c022);dev2m=lag(dev2);
if _n_=1 then do;
atrisk1=n1;
s1=(atrisk1-dev1-dcr1)/atrisk1;
c1=(atrisk1-c021)/atrisk1;
f1=dev1/atrisk1;
fcr1=dcr1/atrisk1;
atrisk2=n2;
s2=(atrisk2-dev2-dcr2)/atrisk2;
c2=(atrisk2-c022)/atrisk2;
f2=dev2/atrisk2;
fcr2=dcr2/atrisk2;
end;
else do;
atrisk1=atrisk1-c021m-dev1m;
s1=s1*(atrisk1-dev1-dcr1)/atrisk1;
c1=c1*(atrisk1-c021)/atrisk1;
atrisk2=atrisk2-c022m-dev2m;
s2=s2*(atrisk2-dev2-dcr2)/atrisk2;
c2=c2*(atrisk2-c022)/atrisk2;
end;
s1m=lag(s1);s2m=lag(s2); c1m=lag(c1);c2m=lag(c2);
if _n_=1 then do;
s1m=1;s2m=1; c1m=1;c2m=1;
end;

if _n_>1 then do;
f1=f1+dev1/atrisk1*s1m;
fcr1=fcr1+dcr1/atrisk1*s1m;
f2=f2+dev2/atrisk2*s2m;
fcr2=fcr2+dcr2/atrisk2*s2m;
end;

w=c1m*c2m*(n1+n2)/(n1*c1m+n2*c2m);
data _temp;set _dg;keep &TimeVar;
proc sort data=_temp;by descending &TimeVar ;
data _temp;set _temp;timem=lag(&TimeVar);
proc sort data=_temp;by &TimeVar ;

data _dg;merge _temp _dg;by &TimeVar;
data _dg;set _dg;
retain v1part v21part v22part;
if timem=. then delete;
deltat=timem-&TimeVar;
spart=w*(f1-f2)*deltat;

** variance;
if _n_=1 then do;
v1i=w*deltat;
v21i=v1i*f1;
v22i=v1i*f2;
v1part=w*deltat;
v21part=v1i*f1;
v22part=v1i*f2;
end;
else do;
v1i=w*deltat;
v21i=v1i*f1;
v22i=v1i*f2;
v1part=v1part+w*deltat;
v21part=v21part+v1i*f1;
v22part=v22part+v1i*f2;
end;

data _dlast;set _dg end=last;if last;
keep v1part v21part v22part;rename v1part=v1tot v21part=v21tot v22part=v22tot;
data _dg;set _dg;if _n_=1 then set _dlast;

data _dg;set _dg;
v1=v1tot-v1part+v1i;
v21=v21tot-v21part+v21i;
v22=v22tot-v22part+v22i;
v11=(1-fcr1)*v1-v21;
v12=(1-fcr2)*v1-v22;
if (atrisk1*(atrisk1-1)) ne 0 then 
sigma1=(v11**2*dev1+v21**2*dcr1)/(atrisk1*(atrisk1-1));
if (atrisk2*(atrisk2-1)) ne 0 then 
sigma2=(v12**2*dev2+v22**2*dcr2)/(atrisk2*(atrisk2-1));
run;

proc means data=_dg noprint;var spart sigma1 sigma2;
output out=_var sum=s sigma1 sigma2;
data _var;set _var;i=1;keep s sigma1 sigma2 i;

%sort(data=_var,var=i);
%sort(data=_d2,var=i);


data pepemori;merge _d2 _var ;
by i;
s=s*sqrt(n1*n2/(n1+n2));
sigma=n1*n2/(n1+n2)*(sigma1+sigma2);
stat=s**2/sigma;
pv=1-probchi(stat,1);

drop sigma: s n1 n2 tau i;

	label pv="p-value exact";
	label stat="Statistic";
	label pvr="p-value rounded";

	** Roundings, JRA, 15-10-2012;
	%if &round=1  %then %do;
		if round(pv,0.1)=0 then pvr='< 0.1';
		else pvr=input(round(pv,0.1),$15.);
	%end;
	%if &round=2  %then %do;
		if round(pv,0.01)=0 then pvr='< 0.01';
		else pvr=input(round(pv,0.01),$15.);
	%end;
	%if &round=3  %then %do;
		if round(pv,0.001)=0 then pvr='< 0.001';
		else pvr=input(round(pv,0.001),$15.);
	%end;
	%if &round=4  %then %do;
		if round(pv,0.0001)=0 then pvr='< 0.0001';
		else pvr=input(round(pv,0.0001),$15.);
	%end;
	%if &round=5  %then %do;
		if round(pv,0.00001)=0 then pvr='< 0.00001';
		else pvr=input(round(pv,0.00001),$15.);
	%end;
	%if &round=6  %then %do;
		if round(pv,0.000001)=0 then pvr='< 0.000001';
		else pvr=input(round(pv,0.000001),$15.);
	%end;
	%if &round=7  %then %do;
		if round(pv,0.0000001)=0 then pvr='< 0.0000001';
		else pvr=input(round(pv,0.0000001),$15.);
	%end;
	%if &round=8  %then %do;
		if round(pv,0.00000001)=0 then pvr='< 0.00000001';
		else pvr=input(round(pv,0.00000001),$15.);
	%end;
	%if &round=9  %then %do;
		if round(pv,0.000000001)=0 then pvr='< 0.000000001';
		else pvr=input(round(pv,0.000000001),$15.);
	%end;
	%if &round>=10  %then %do;
		if round(pv,0.0000000001)=0 then pvr='< 0.0000000001';
		else pvr=input(round(pv,0.0000000001),$15.);
	%end;

risk=&RiskVal;
df=1;

run;


proc datasets nolist;
delete _d: _temp _var __d: _cens;
run;
quit;

	%fin: ;


%mend;

