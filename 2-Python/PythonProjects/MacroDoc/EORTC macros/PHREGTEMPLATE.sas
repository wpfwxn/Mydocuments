/*************************************************************************************

PHREGTEMPLATE.SAS
**************

Tabulates results coming from PROC PHREG

Creation date  : March 2014
Version date   : April 2015
version        : 1.1
Software       : SAS version 9.4
Original author: Jérôme RAPION
Modified by    : Jérôme RAPION

************************************************************************************

PARAMETERS

    OUTPUT            :  Path and name of the 'rtf' file (for instance, OUTPUT=c:\temp\cox.rtf)
						 if the user put PHREGTEMPLATE macro call inside and ODS RTT block, then he should
                         leave OUTPUT parameter empty. [default: OUTPUT=] (Optional parameter)

    TEST              :  ALL | GLOBAL 
						 GLOBAL: the user would get only the global test p-value with (df=xx) ;
						 ALL : in addition to the global test, the user would get an additional column with 
                         p-values for the pairwise level comparisons (only for categorical variables with more 
                         than 2 levels);
						 [default=GLOBAL] (Optional parameter)

    PARAMEST          :  Y | N
					     Set to Y to display one additionnal column with Parameter Estimates ans Standard Error
						 Set to N to not display this additional column.
						 [default=N] (Optional parameter)


    DF                :  Y | N
					     Set to Y to display the global p-value with (df=xx) 
						 Set to N to display the global p-value without degree of freedom
						 [default=Y] (Optional parameter)


************************************************************************************
NOTES

	PHREGTEMPLATE macro can work properly only in the condition of calling it after a PROC PHREG call.

	This PROC PHREG call should be invoked using ?phreg abbreviation.
    ?phreg abbreviation contains all the code needed so as to make PHREGTEMPLATE running properly.
	If the user doesn't use ?phreg abbreviation, PHREGTEMPLATE may not work properly

	?phreg abbreviation gives to the user this code:

	proc phreg data=<data>;
		class <list>/ param=ref ref=first order=internal;
		model <timevar>*<censor>(1) = <list> /r1;
		ods output  ParameterEstimates=__ParameterEstimates;
		ods output  ClassLevelInfo=__ClassLevelInfo;
		ods output  ModelANOVA=__type3;
	run;

	**********
	ALPHA=0.01
	**********

	If the user wants 99% CI for hazard ratio, then he should add alpha =0.01 option in the model statement
	of PROC PHREG

	****************************
    If OUTPUT parameter is empty
    ***********************;*****
	If the user calls PHREGTEMPLATE inside an ODS RTF BLOCK, he should add this statement :
		ODS RTF EXCLUDE ALL;
	before each PROC PHREG call
	
************************************************************************************
EXAMPLES

** Example 1: using OUTPUT option
	proc phreg data=patient; 
		class trt1 qval114 grad ageg /param=ref ref=first order=internal;
		model timesur*censur(1) = trt1 qval114 grad ageg  /rl alpha=0.01; 
	ods output  ParameterEstimates=__ParameterEstimates;
	ods output  ClassLevelInfo=__ClassLevelInfo;
	ods output  ModelANOVA=__type3;
	run;

	%PHREGTEMPLATE(Output=h:\desktop\fullmodel.rtf,PARAMEST=Y,TEST=GLOBAL);

 
** Example 2: inside an ODS RTF block
ODS RTF FILE="h:\desktop\report.rtf" STYLE=MyEORTCStyle1 bodytitle startpage=yes;

	%Tables2(data=patient62012,var=ageg trt1 qval114 grad,ODS=Y) ;

	ods rtf exclude ALL;
	proc phreg data=patient62012 ; 
	class ageg trt1 qval114 grad /param=ref ref=first order =internal;
		model timesur*censur(1) = ageg trt1 qval114 grad viol/rl ; 
	ods output  ParameterEstimates=__ParameterEstimates;
	ods output  ClassLevelInfo=__ClassLevelInfo;
	ods output  ModelANOVA=__type3;
	run;
	%PHREGTEMPLATE(PARAMEST=Y,TEST=ALL);

	ods rtf exclude ALL;
	proc phreg data=patient62012 ; 
	class ageg trt1   /param=ref ref=first order =internal;
		model timesur*censur(1) = ageg trt1 qval114 grad viol/rl alpha=0.01; 
	ods output  ParameterEstimates=__ParameterEstimates;
	ods output  ClassLevelInfo=__ClassLevelInfo;
	ods output  ModelANOVA=__type3;
	run;
	%PHREGTEMPLATE;


ODS RTF CLOSE;


************************************************************************************

REVISIONS

28-04-2015 : JRA, adapt the macro so as the numbel of levels is not limited to 6 for variables put in CLASS statement
This version is flexible and is adapted to the max number of levels contained in the dataset.
The display of p-values has also been adapted so as to allow < .001 display instead of having 0.000

01/12/2017 :  GIS change the ODS OUTPUT TYPE3= into ODS OUTPUT MODELANOVA= in the Phreg statement
************************************************************************************/

%MACRO PHREGTEMPLATE(Output=, TEST=GLOBAL,PARAMEST=N, DF=Y);

%if (%length(&Output)=0) %then %let Output=N;

	%let ClassLevelInfo=%sysfunc(exist(__ClassLevelInfo));
	%put ClassLevelInfo &ClassLevelInfo;

	data __ParameterEstimates;
	set __ParameterEstimates;
	%if &ClassLevelInfo>0 %then lab=tranwrd(label,trim(ClassVal0),"");;
	%if &ClassLevelInfo = 0 %then lab=label;;
	n=_n_;
	labelCI=trim(left(VLABEL(HRLowerCL)));

	if lab="" then lab=Parameter;

	run;

	data __aa;
	set __ParameterEstimates;
	if labelCI ne "" then a=substr(labelCI,1,2);
	run;

	data _null_;
	set __aa;
	call symput('CI',compress(a));
	run;

	%put ci &ci ;

	proc sort data=__ParameterEstimates out=__ListParam(keep=Parameter %if &ClassLevelInfo>0 %then %do; lab %end; n) nodupkey;
	by Parameter;
	run;

	%sort(data=__ListParam,var=n);

	data __ListParam;
	set __ListParam;
	n=_n_;
	run;

	%sort(data=__ParameterEstimates,var=Parameter);
	%sort(data=__ListParam,var=Parameter);

	data __ParameterEstimates;
	merge __ParameterEstimates(drop=n) __ListParam(in=a);
	by Parameter;
	if a;
	run;


	%if &ClassLevelInfo > 0 %then %DO;

	proc contents data=__ClassLevelInfo out=__class (where=(substr(NAME,1,1)="X"));
	run;

	data _null_;
	set __class end=eof;
	i+1;
	if eof then call symput('NX',compress(i));
	run;

	%put NX &NX.;


	data __ClassLevelInfo;
	set __ClassLevelInfo;
	retain Parameter;
	if Class ne '' then Parameter=Class;
	run;

	%sort(data=__ClassLevelInfo,var=Parameter);

	data __NbLevelInfo;
	set __ClassLevelInfo;
	retain cpt 0;
	by Parameter;
	if first.Parameter then cpt=1;
	else cpt+1;
	if last.Parameter;
	cpt=cpt-1;
	keep Parameter cpt;
	run;


	data __ClassLevelInfo;
		merge __ClassLevelInfo __NbLevelInfo;
		by Parameter;

		if cpt= 1 then do;
			if X1=0 then ref=1;
		end;

		* JRA, 29-04-2015, allow more than 6 levels in a class variable;
		* From now, it is adapted to the real number of levels in the variables; 

		%do i=2 %to &NX;
			if cpt= &i then do;

			 	if X1= 0 and  %do j=2 %to %eval(&i-1); X&j=0 and %end; X%eval(&i)=0 then ref=1;

			end;
		%end;

	run;

	data __Ref;
	set __ClassLevelInfo;
	if ref=1;
	ClassVal0=Value;
	keep Parameter ClassVal0 ref;
	run;

	%sort(data=__Ref,var=Parameter);
	%sort(data=__ListParam,var=Parameter);

	data __Ref;
	merge __Ref(in=a) __ListParam(in=b);
	by Parameter;
	if a and b;
	run;



	%END;


	data __ParameterEstimates;
	set __ParameterEstimates  %if &ClassLevelInfo > 0  %then %do; __Ref %END;;
	if HazardRatio=. then HazardRatio=1;

	if DF ne . then 
	HRStr = trim(left(put(HazardRatio,8.2)))||
	                         ' ('||trim(left(put(HRLowerCL,8.2)))||', '||trim(left(put(HRUpperCL,8.2)))||')';
							 else HRStr='1.00';
	run;


	%sort(data=__ParameterEstimates,var=Parameter n  %if &ClassLevelInfo > 0  %then %do; descending ref ClassVal0 %END; );


	data __ParameterEstimates;
	set __ParameterEstimates;
	length Par $30.;
	by Parameter n;
	if first.Parameter then Par=lab;
	else Par='';
	%if &ClassLevelInfo > 0  %then %do;
	label ClassVal0="Levels*";
	%end;
	label HRStr="Hazard Ratio*(&ci% CI)";
	label Par="Parameters*";
	label ProbChiSq="P-value*";

	run;

%if &ClassLevelInfo>0 %then %do;
	data __type3;
	set __type3;
	rename ProbChiSq=ProbChiSq3;
	run;

	data __ParameterEstimates;
	set __ParameterEstimates ;
		Effect=Parameter;
	run;
	%sort(data=__ParameterEstimates,var=Effect);
	%sort(data=__type3,var=Effect);

	data __ParameterEstimates;
		merge __ParameterEstimates __type3(keep=effect ProbChiSq3 df);
		by Effect;
	if HazardRatio=1 then _ProbChiSq=ProbChiSq3;
	else _ProbChiSq=.;

	if Par ne '' and ref=. then _ProbChiSq=ProbChiSq;

	%IF %UPCASE(&DF)=Y %THEN %DO; * JRA, 29-04-2015, replace format 8.3 by PVALUE6.3; 
		if _ProbChiSq ne . then  Prob=compress(put(_ProbChiSq,PVALUE6.3))||" (df="||compress(df)||")";
		label Prob="P-value*";
	%END;

	%IF %UPCASE(&DF)=N %THEN %DO; * JRA, 29-04-2015, replace format 8.3 by PVALUE6.3; 
		if _ProbChiSq ne . then  Prob=compress(put(_ProbChiSq,PVALUE6.3));
		label Prob="P-value*";
	%END;

	run;

%end;

%if &ClassLevelInfo=0 %then %do;

	data __ParameterEstimates;
	set __ParameterEstimates;
	_ProbChiSq=ProbChiSq;

	%IF %UPCASE(&DF)=Y %THEN %DO; * JRA, 29-04-2015, replace format 8.3 by PVALUE6.3; 
		if _ProbChiSq ne . then Prob=compress(put(_ProbChiSq,PVALUE6.3))||" (df="||compress(df)||")";
		label Prob="P-value*";
	%END;

	%IF %UPCASE(&DF)=N %THEN %DO; * JRA, 29-04-2015, replace format 8.3 by PVALUE6.3; 
		if _ProbChiSq ne . then  Prob=compress(put(_ProbChiSq,PVALUE6.3));
		label Prob="P-value*";
	%END;

	run;

%end;

%if %UPCASE(&PARAMEST)=Y %then %do;

	data __ParameterEstimates;
	set __ParameterEstimates;
	if Estimate ne . then _estimate=compress(put(Estimate,8.3))||' ('||compress(put(StdErr,8.4))||')';
	label _estimate="Parameter Estimate*(Standard Error)";
    run;

%end;


%if %UPCASE(&TEST)=ALL %then %do;

	data __ParameterEstimates;
	set __ParameterEstimates;
	if Estimate ne . then _estimate=compress(put(Estimate,8.3))||' ('||compress(put(StdErr,8.4))||')';
	label _estimate="Parameter Estimate*(Standard Error)";

	%if &ClassLevelInfo>0 %then %do;

	if round(ProbChiSq3,0.000000000000001)=round(ProbChiSq,0.000000000000001) then ProbChiSq=.;

	%end;

	* JRA, 29-04-2015, replace format 8.3 by PVALUE6.3; 

	pval=put(ProbChiSq,PVALUE6.3);
	if ProbChiSq=. then pval='';

	if Prob ne '' and pval ne '' then pval='';

	if pval='' then ppval=0;
	else ppval=1;

	label pval="P-value*";

    run;


	proc means data=__ParameterEstimates sum;
	var ppval;
	output out=__ppval sum=sum;
	run;

	data _null_;
	set __ppval;
	call symput('ppval',compress(sum));
	run;

	%put ppval &ppval;

%end;


	%sort(data=__ParameterEstimates,var=n  %if &ClassLevelInfo > 0  %then %do; descending ref ClassVal0 %END; );

	options nodate nonumber;

	data _null_;
		set __ParameterEstimates;
		ind=_n_;
		call symput('Noutput',compress(ind));
	run;

	%put Noutput &Noutput.;


	proc template;
	    define table PHREGTemplate;
	         column TestVarCol DF NCol NCol1b NCol1 NCol2 NCol3 ;
	         define TestVarCol;
	            define header TestVarColHdr;
	               text "Parameter";
	               just=l;
	               split='*';
	            end;
	            header=TestVarColHdr;
	            style=data {cellwidth=200};
	            just=l;
	            preformatted=on;
	         end;
	         define DF;
	            define header NColHdr;
	               text _label_;
	               just=c;
	               split='*';
	            end; 
	            header=NColHdr;
	           style=data {cellwidth=40};
	           just=r;
	            preformatted=on;
	            generic=on;
	         end;
	         define NCol1;
	            define header NColHdr;
	               text _label_;
	               just=c;
	               split='*';
	            end; 
	            header=NColHdr;
	           style=data {cellwidth=175};
	           just=r;
	            preformatted=on;
	            generic=on;
	         end;

	         define NCol1b;
	            define header NColHdr;
	               text _label_;
	               just=c;
	               split='*';
	            end; 
	            header=NColHdr;
	           style=data {cellwidth=175};
	           just=c;
	            preformatted=on;
	            generic=on;
	         end;

	         define NCol;
	            define header NColHdr;
	               text _label_;
	               just=l;
	               split='*';
	            end; 
	            header=NColHdr;
	           style=data {cellwidth=175};
	           just=l;
	            preformatted=on;
	            generic=on;
	         end;
	         define NCol2;
	            define header NColHdr;
	               text ' ';
	               just=c;
	               split='*';
	            end; 
	            header=NColHdr;
	           style=data {cellwidth=75};
	           just=r;
	            preformatted=on;
	            generic=on;
	         end;
	         define NCol3;
	            define header NColHdr;
	               text _label_;
	              %if %UPCASE(&TEST)=ALL %then %do;
	               just=l;
				  %END;
	              %if %UPCASE(&TEST)=GLOBAL %then %do;
	               just=c;
				  %END;
	               split='*';
	            end; 
	            header=NColHdr;
	           style=data {cellwidth=110};
	           just=r;
	            preformatted=on;
	            generic=on;
	         end;
	      

			cellstyle 
					%do i=1 %to %eval(&Noutput.-2);
					(_row_=&i) as {borderbottomstyle=hidden},
					%end;
	                (_row_=%eval(&Noutput.-1)) as {borderbottomstyle=hidden};

	      end;
	run;

	%IF &OUTPUT.^=N %THEN %DO;
	    ODS RTF FILE="&Output." STYLE=MyEORTCStyle1 bodytitle startpage=no;
	%END;

	%IF &OUTPUT.=N %THEN %DO;
	ods rtf select all;
	%END;
	           data _null_;
	               set __ParameterEstimates;
	               file print
	                    ods=(template='PHREGTemplate'
	                         columns=(TestVarCol=Par 
							 %if &ClassLevelInfo>0  %then %do;
							          NCol=ClassVal0 (generic=on ) %END;

									
							%if %UPCASE(&PARAMEST)=Y %then %do;
							          NCol1b=_estimate (generic=on ) 
							%END;
									  NCol1=HRStr (generic=on)

						%if &ClassLevelInfo>0  %then %do;
							%if %UPCASE(&TEST)=ALL %then %do;

								%if &ppval > 0 %then %do;
	                                  NCol2=pval (generic=on )
								%end;

							%END;
						%end;
	                                  NCol3=Prob (generic=on)

									)
							);
	               put _ods_;;
	              run;

	%IF &OUTPUT.^=N %THEN %DO;
	    ODS RTF CLOSE;
	%END;


	 proc datasets nolist;
	   delete  __:;
	   run;
	   quit;

%MEND;

