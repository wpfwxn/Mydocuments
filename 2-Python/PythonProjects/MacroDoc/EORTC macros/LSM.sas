%macro lsm(data,windat,var,where,colorlist,Rmatrix,alpha=0.01) ;

*** THIS MACROS PRODUCES PLOTS OF AVERAGE PROFILES BY VAR WITH CONFIDENCE BARS ;
*** Data=dataset containing the information (_SCQOL by default) ;
*** WINDAT= windows dataset (TIMEWIN by default) ;
*** VAR= QL variable to analyse ;
*** WHERE= selection based on steps (assessment times) ;
*** COLORLIST= list of colors to be used for the profiles: default= BLUE GREEN RED BLACk ;
*** RMATRIX: autocorrelation matrix for computing LSMEANS (default= AR(1)) ;
*** ALPHA: confidence level for confidence intervals (DEFAULT=0.01) ;

*** The output is stored as a CGM graphic on c:\sas ;
*** In addition, a table is stored in an RTF file, displaying the means + STD and cross-sectional comparison fr each timepoint ;


*** AUTHOR: LCO MAy 2002 version 1.0 ;



%if (%length(&data)=0) %then %let data=_SCQOL ;
%if (%length(&windat)=0) %then %let windat=timewin ;
%if (%length(&colorlist)=0) %then %let colorlist=BLUE GREEN RED BLACK ;
%if (%length(&Rmatrix)=0) %then %let Rmatrix=AR(1) ;

** retrieving formats and labels ;
data _null_  _fmt (keep=start label fmtname);
   set &windat end=lastobs ;
          where &where ;
          by step;
          retain ns 0 maxu . ;
          ns+1;
                  maxu=max(maxu,unit) ;
          if lastobs then do ;
            call symput('NS',compress(ns));
                        call symput('maxu',compress(maxu)) ;
                  end;
          call symput('Label'||trim(left(step+1)),trim(stepname)); * lower end of window ;
              *** to create the format for step ;
          start=step ;
          label=stepname ;
          fmtname='_Step' ;
                  run;

          proc format cntlin=_fmt ;
          run ;

proc contents data=&data out=_info noprint  ;
run ;
quit ;

data _info ;
set _info ;
if (trim(upcase(NAME))=trim(upcase("&var."))) then do;
      call symput("title", trim(left(LABEL)));
      end ;
if (trim(upcase(NAME))=trim(upcase('trt1'))) then do;
      call symput("trtfmt", trim(left(FORMAT)));
      end ;
run ;


** computing LS means and contrasts ;

ODS OUTPUT stat.mixed.lsmeans=&var.m ;
ODS OUTPUT stat.mixed.contrasts=&var.contrast ;
ODS LISTING CLOSE ;

proc mixed  data=&data method=ml scoring noclprint noitprint noprofile order=data ;
title "Covariance type  &Rmatrix, &var";
Class patid trt1 step ;
where withbase=1
%if %length(&where)>0 %then %do ;
and &where
%end ;
;
 model &var=trt1*step / s;
 repeated step/type=&Rmatrix subject=patid;
 random intercept / type=un subject=patid g;
lsmeans trt1*step;

%do i=1 %to &NS ;
 contrast "&&label&i" trt1*step
 %do k=1 %to %eval(2*&NS) ;
   %if (&k=&i) %then %do ;
   1
   %end ;
   %if (&k=%eval(&NS+&i)) %then %do ;
   -1
   %end ;
   %if (&k^=%eval(&NS+&i) and &k^=&i) %then %do ;
   0
   %end ;
  %end ;
  ;
%end ;
run;


*Ods RTF file="c:\sas\Contrasts_&var..rtf" STARTPAGE=NO;
*proc print data=&var.contrast noobs split='*' width=uniform ;
*var label probf ;
*label label="Treatment*Difference";
*run;
*ODS RTF close ;

ODS listing ;
** producing the pictures ;


proc sort data=&var.m ; by step trt1 ;run;

data _qoltrans (keep=trt1 step0  &var i rename=(step0=step)) _means (keep=trt1 step &var stepname estimate stderr )  ;
merge &var.m &windat;
where &where ;
by step ;
format step0 _step. trt1 &trtfmt..;
time=round((assesd*unit)/&maxu,.1) ;
step0=time+0 ;
if (trt1=1) then do ;
call symput('tval'||trim(left(step+1)),compress(time)) ;
end ;
if (trt1^=1) then step0=time+(trt1-1)*(&maxu/unit);
do i=-1 to 1 by 1 ;
&var=estimate+i*stderr*probit(1-&alpha./2) ;
label &var="&title" ;
output _qoltrans;
if (i=0) then output _means ;
end ;
run ;


** retrieving means and stderr ;
proc transpose data=_means out=_meanst ;
var estimate stderr;
by step stepname;
copy trt1;
run ;

** getting labels for treatment groups ;
proc sort data=_meanst out=trtval nodupkey ;
by trt1 ;
run ;

data _null ;
set trtval end=lastobs;
call symput('ftrt'||compress(_N_),put(trt1,&trtfmt..)) ;
if lastobs then do ;
call symput('Ntrt',compress(_N_)) ;
end ;
run ;

** preparing the data with means + stderr ;
data _truc (keep=step stepname mean1 mean2 );
retain rcol1  rcol2;
set _meanst ;
by step ;
if (first.step) then do ;
%do i=1 %to &NTRT ;
rcol&i=col&i;
%end ;
end ;
if (last.step) then do ;
%do i=1 %to &NTRT ;
mean&i=compress(round(rcol&i,.1))||' (SD='||compress(round(col&i,.01))||')' ;
label mean&i="&&ftrt&i" ;
%end ;
output ;
end ;
run ;


** merging with the contrasts P-values ;
proc sort data=_truc ;
by stepname ;
run ;
proc sort data=&var.contrast ;
by label ;
run ;
data _truc ;
merge _truc &var.contrast(rename=(label=stepname)) ;
by stepname ;
run ;
proc sort data=_truc ;
by step ;run ;

Ods RTF file="c:\sas\Contrasts_&var..rtf"  STARTPAGE=YES;
proc print data=_truc noobs split="*";
title1 "Contrasts and means (std error)" ;
title2 "Variable &title" ;
var
stepname
%do i=1 %to &Ntrt;
mean&i
%end ;
probf ;
label probf="Treatment*difference*P-value" ;
label stepname="Assessment*Time" ;
;
run ;
title ;
ODS RTF Close ;


** producing the pictures ;

%if (&maxu=1) %then %let labelu=Days ;
%if (&maxu=7) %then %let labelu=Weeks ;
%if (&maxu=30) %then %let labelu=Months ;
%if (&maxu=365) %then %let labelu=Years ;

goptions reset=all ;

goptions hsize=18cm vsize=14cm   /*rotate=landscape*/ gsfmode=replace
gsfname=grafout device=cgmof97p htext=1;
%let i=1 ;
%do %until(%scan(&colorlist,&i)=) ;

symbol&i i=hiloctj l=&i h=1 v=dot c=%scan(&colorlist,&i);
%let i=%eval(&i+1) ;
%end ;

axis1 label=(j=c "Time Since Randomization (&labelu)")/* major=(n=14)*/
/*
value=(
%do i=1 %to &NS ;
"&&label&i"
%end ;
)
order=(
%do i=1 %to &NS ;
%eval(&&tval&i)
%end ;
)
*/
minor=none ;
axis2 label=('') order=(0 to 100 by 10);

%let level=(1-&alpha./2) ;

title2 "&Title" ;
title3 "Means + &level.% CI" ;
filename grafout "c:\sas\&var.Profiles.cgm" ;
proc gplot data=_qoltrans ;
  plot &var*step=trt1/ haxis=axis1 vaxis=axis2 legend ;
label trt1='Treatment' ;
format step _step. trt1 &trtfmt..;
 run ;

quit ;

proc datasets library=WORK nolist ;
delete _fmt _info _qoltrans &var.m &var.contrast _truc _means _meanst;
run ;
quit ;

%mend lsm ;


** ALL AT ONCE;

%macro lsm_QL(data,windat,where,colorlist,form,Rmatrix) ;

* form=QLQ (QLQ-C30), HN, LC or BR ;

%if (%length(&form)=0) %then %let form=QLQ ;
%if (%length(&data)=0) %then %let data=_SCQOL ;
%if (%length(&windat)=0) %then %let windat=timewin ;
%if (%length(&colorlist)=0) %then %let colorlist=BLUE GREEN RED BLACK ;
%if (%length(&Rmatrix)=0) %then %let Rmatrix=AR(1) ;

%if (%upcase(&form)=QLQ) %then %do ;
%let varlist=QL PF RF EF CF Sf FA NV PA DY SL AP CO DI FI ;
%end ;
%if (%upcase(&form)=HN) %then %do ;
%let varlist=HNPA HNSW HNSE HNSP HNSO HNSC HNSX HNTE HNOM HNDR HNSS HNCO HNFI HNPK HNNU HNFE HNWL HNWG ;
%end ;
%if (%upcase(&form)=BR) %then %do ;
%let varlist=BRBI BESF BRSEE BRFU BRST BRBS BRAS BRHL ;
%end ;
%if (%upcase(&form)=LC) %then %do ;
%let varlist=LCDY LCDO LCHA LCSM LCDS LCPN LCHR LCPC LCPA LCPO ;
%end ;

%let c=1 ;
%do %until (%scan(&varlist,&c)=) ;
%let v_=%scan(&varlist,&c) ;
%lsm(data=&data,windat=&windat,where=&where,colorlist=&colorlist,var=&v_,rmatrix=&Rmatrix) ;
%let c=%eval(&c+1) ;
%end ;

%mend ;
