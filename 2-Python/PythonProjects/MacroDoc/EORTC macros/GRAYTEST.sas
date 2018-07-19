/*************************************************************************************

GRAYTEST.SAS
**********

Compute the Gray test for one event of interest accross groups for competing risks data.

One SAS dataset is created in WORK library: Graytest dataset
with Statistic, p-value exact, p-value rounded and Df columns. 

Version date   : 02 January 2014
Software       : SAS version 9.4
Original author: Jerome RAPION
version: 2.0

Ref: 
-----
Gray RJ (1988) A class of K-sample tests for comparing the cumulative incidnece of a competing risk, 
ANNALS OF STATISTICS, 16:1141-1154.
***********************************************************************************

 REVISIONS:
 ---------
 October 2012  JRA, Modification of ROUND parameter
			   Instead of Y/N, put of a digit to specify the precision of rounding.
               Keep the format of RISVAR variable in the output dataset 

 January 2014  JRA, Computation of results using PROC IML and SAS code instead of using R code.
               This new macro is based on the code of gray.sas macro written by Simon BOND, 
               Cambridge Clinical Trial Unit, Cambridge University Hospitals NHS Trust, UK.

			   see Paper: SAS code for the estimation and betweengroup comparison of cumulative incidence functions 
               in competing risks survival analysis (Pharmaceutical Programming, 2011, vol 4, PHUSE)
			   (saved in J:\UNIT\Stat\7. SAS and R\SAS TRAINING\Papers\COMPETING RISK)

			   see original macro code in http://www.mrc-bsu.cam.ac.uk/Software/SAS_Macros/Gray.sas                

***********************************************************************************

PARAMETERS

    Data              :  Name of the data file (Required parameter)
    TestVar           :  Test variable (Required parameter)
    Timevar           :  Name of the time variable (Required parameter)
    RiskVar           :  Variable indicating patient's outcome (Required parameter) (see notes below)
    Censval           :  Value of &RiskVar indicating patient was censored (Optional parameter) (default=0)
    StratVar          :  Stratification variable. Tests will be stratified on this variable.(Optional parameter)
						 (default=1) (All data in 1 stratum, if missing)						 
    RiskVal           :  Value of &RiskVar indicating patient experienced the event of interest (Optional parameter)
						(default=1)
    Where             :  Where condition  (Optional parameter)
    ROUND             :  a digit to indicate the precision of the rounding for p-value,  
                         (default=4) (Optional parameter) (see notes)

NOTES

    For patients experiencing more than one event, only the earliest event is considered in the analysis.

    The &TimeVar variable should contain the time of the earliest event, whether this was the event of interest 
    or a competing event.  For censored patients, the &TimeVar variable should contain the time the patient was
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
 
    %GRAYTEST(Data=Patient, TestVar=Trt1, TimeVar=EarliestTime, RiskVar=EarliestEvent,StratVar=Agegrp);

************************************************************************************/


%MACRO GRAYTEST(data=,Timevar=,TestVar=,RiskVar=,CensVal=0,RiskVal=1,stratvar=,ROUND=4,WHERE=);

%if %length(&where) = 0 %then %let where=1; 
%let rho=0;

data _temp; 
set &data; 
%if &stratvar= %then %do; _stratvar=1; %end;
	where &where;
run;

%if &stratvar= %then %let stratvar=_stratvar;

proc sort data=_temp out=_labels nodupkey;
   by &TestVar;
run;

data _labels;
set _labels;
&TestVar.2=_n_;
keep &TestVar &TestVar.2;
run;

proc sort data=_temp out=_temp;
   by &TestVar;
run;

data _temp;
merge _temp _labels;
by &TestVar;
run;


proc sort data=_temp out=_labels nodupkey;
   by &stratvar;
run;

data _labels;
set _labels;
&stratvar.2=_n_;
keep &stratvar &stratvar.2;
run;

proc sort data=_temp out=_temp;
   by &stratvar;
run;

data _temp;
merge _temp _labels;
by &stratvar;
run;



data _temp; set _temp; 
select( &RiskVar);
    when(&CensVal) &RiskVar.2=0;
    when(&RiskVal) &RiskVar.2=1;
    otherwise &RiskVar.2=2;
end;

rename &RiskVar.2=&RiskVar;
rename &TestVar.2=&TestVar;
rename &stratvar.2=&stratvar;
drop &RiskVar &TestVar &stratvar ;
run;


proc sort data=_temp out=_temp;
by &stratvar &timevar &RiskVar &TestVar;
run;

proc sql noprint; 
select count(*), max(&TestVar), max(&stratvar) into :no, :ng, :nst from _temp;
quit;

%let ng=%trim(&ng);
%let ng1=%trim(%eval(&ng-1));
%let ng2=%trim(%eval(&ng*&ng1/2));
%let vdim=%eval( &ng*(&ng-1)/2);
%let vdim=%trim(&vdim);

%do str = 1 %to &nst;
    data _databystrata; set _temp;
        where &stratvar=&str;
    run;

    proc sql noprint;
    select count(*) into :rs_1-:rs_&ng from _databystrata group by &TestVar;
    quit;


    proc freq data=_databystrata noprint;
    table &timevar*&RiskVar*&TestVar/out=_unique;
    run;
    data _unique; set _unique;
    id="d_"||compress(put(&RiskVar,2.))||"_"||compress(put(&TestVar,2.));
    run;

    proc transpose data=_unique out=_unique ;
    by &timevar;
    var count;
    id id;
    run;

    

    data _unique; set _unique;

    retain s_1-s_&ng1 0;
    retain v_1-v_&ng2 0;
    retain f1m_1-f1m_&ng 0;
    retain f1_1-f1_&ng 0;
    retain fm 0;
    retain f 0;
    retain skmm_1-skmm_&ng 1;
    retain skm_1-skm_&ng 1;
    retain v3_1 - v3_&ng 0;

    %do i= 1 %to &ng;
        retain rs_&i &&rs_&i;
        %do j =1 %to &ng1;
            retain v2_&j._&i 0;
        %end;
        %do j =1 %to &ng;
            retain c_&i._&j 0;
        %end;
    %end;



    %do st=0 %to 2;
        %do gp= 1 %to &ng;
            if( d_&st._&gp =.) then d_&st._&gp=0;
        %end;
    %end;
    nd1=sum(of d_1_1 - d_1_&ng);
    nd2=sum(of d_2_1 - d_2_&ng);

    if( nd1>0 OR nd2>0) then do;
        tr=0;
        tq=0;
        %do i=1 %to &ng;
            if( rs_&i >0) then do;
                td=d_1_&i + d_2_&i;
                skm_&i=skmm_&i *(rs_&i-td)/rs_&i;
                f1_&i=f1m_&i+(skmm_&i *d_1_&i )/ rs_&i;
                tr=tr+rs_&i/skmm_&i;
                tq=tq+rs_&i *( 1- f1m_&i)/skmm_&i;
            end;
        %end;
        f=fm+nd1/tr;
        fb=(1-fm)**&rho;
        
        %do i=1 %to &ng;
            %do j=&i %to &ng;
                a_&i._&j=0;
            %end;
            if( rs_&i>0) then do;
                t1=rs_&i/skmm_&i;
                a_&i._&i=fb*t1*(1-t1/tr);
                c_&i._&i=c_&i._&i+ a_&i._&i * nd1/(tr*(1-fm));
                %let k=%eval(&i+1);
                %if( &k <= &ng) %then %do;
                   %do j= &k %to &ng;
                        if(  rs_&j>0) then do;
                            a_&i._&j=-fb*t1*rs_&j./(skmm_&j.*tr);
                            c_&i._&j= c_&i._&j + a_&i._&j*nd1/(tr*(1-fm));
                        end;
                   %end;
                %end;
            end;
        %end;
        %do i= 2 %to &ng;
            %let k= %eval(&i-1);
            %do j= 1 %to &k;
                a_&i._&j=a_&j._&i;
                c_&i._&j=c_&j._&i;
            %end;
        %end;
        %do i=1 %to &ng1;
            if( rs_&i.>0) then do;
                s_&i=s_&i+fb*(d_1_&i - nd1 * rs_&i*(1-f1m_&i)/(skmm_&i*tq));
            end;
        %end;
        
        if( nd1>0) then do;
            %do k= 1 %to &ng;
                if( rs_&k>0) then do;
                    t4=1;
                    if( skm_&k >0) then t4 = 1 - (1-f)/skm_&k;
                    t5=1;
                    if( nd1>1) then t5=1- (nd1-1)/(tr*skmm_&k-1);
                    t3=t5*skmm_&k*nd1/(tr*rs_&k);
                    v3_&k=v3_&k+t4*t4*t3;
                    %do i = 1 %to &ng1;
                        t1=a_&i._&k - t4*c_&i._&k;
                        v2_&i._&k=v2_&i._&k + t1*t4*t3;
                        %do j= 1 %to &i;
                            %let l=%trim(%eval( &i*(&i-1)/2+&j ));
                            t2=a_&j._&k - t4 * c_&j._&k;
                            v_&l=v_&l+t1*t2*t3;
                        %end;
                    %end;
                end;
            %end;
        end;
        if( nd2>0) then do;
            %do k=1 %to &ng;
                if( skm_&k>0 AND d_2_&k >0) then do;
                    t4=(1-f)/skm_&k;
                    t5=1;
                    if( d_2_&k > 1) then t5=1-(d_2_&k -1.0)/(rs_&k-1.0) ;
                    t3=t5*((skmm_&k **2)* d_2_&k)/(rs_&k **2);
                    v3_&k=v3_&k +t4*t4*t3;
                    %do i = 1 %to &ng1;
                        t1=t4*c_&i._&k;
                        v2_&i._&k= v2_&i._&k - t1*t4*t3;
                        %do j=1 %to &i;
                            %let l=%trim(%eval( &i*(&i-1)/2+&j ));
                            t2=t4*c_&j._&k;
                            v_&l=v_&l + t1*t2*t3;
                        %end;
                    %end;
                end;
            %end;
        end;
    end;
    fm=f;
    %do i= 1 %to &ng;
        rs_&i=rs_&i - d_0_&i - d_1_&i - d_2_&i ;
        f1m_&i=f1_&i;
        skmm_&i=skm_&i;
    %end;
    run;
    %let l=0;


    data _unique; set _unique end=last;

    %do i=1 %to &ng1;
        %do j=1 %to &i;
            %let l=%trim(%eval(&l+1));
            %do k=1 %to &ng;
                v_&l = v_&l + c_&i._&k * c_&j._&k * v3_&k;
                v_&l = v_&l + c_&i._&k * v2_&j._&k;
                v_&l = v_&l + c_&j._&k * v2_&i._&k;
            %end;
        %end;
    %end;
    if last;
    run;


    data _stats; set
    %if &str>1 %then %do; _stats %end;
        _unique;
    run;
%end;

proc sql;
create table _stats2 as 
select
sum(s_1) as s_1
%if( &ng1>1) %then %do;
    %do i=2 %to &ng1;
        ,sum(s_&i) as s_&i
    %end;
%end;   
%do i=1 %to &vdim;
        , sum(v_&i) as v_&i
%end;
from _stats;
quit;



proc iml;
use _stats2;
%let l=0;
Z=shape(0,&ng1,1);
Sig=shape(0, &ng1,&ng1);
%do i = 1 %to &ng1;    
    read var{ s_&i} into _temp;
    Z[&i]=_temp;
    %do j=1 %to &i;
        %let l=%trim(%eval(&l+1));
        read var{v_&l} into _temp;
        Sig[&i, &j]=_temp;
        Sig[&j, &i]=Sig[&i, &j];
    %end;
%end;

 
Chisq=t(Z)*Inv(Sig)*Z   ;
PVal=1-probchi(Chisq,&ng1);
df=&ng1;
result=chisq||pval||df;
name={'stat' 'pv' 'df'};
create Graytest from result[ colname=name];
append from result;
quit;

data Graytest;
set Graytest;
	label pv="p-value exact";
	label stat="Statistic";
	label df="Df";
	label pvr="p-value rounded";

	risk=&RiskVal;

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

%if &stratvar ne _stratvar %then stratif="&StratVar.";;

run;



proc datasets nolist;
delete _databystrata _labels _stats _stats2 _temp _unique;
run;
quit;



%mend;

