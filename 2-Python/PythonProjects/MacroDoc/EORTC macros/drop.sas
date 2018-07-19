%macro drop(data,windat,domain,sel,where,device) ;

*** QL MACRO THAT CREATES GRAPHICS OF PROFILES BY DROP OUT TIME ;
*** GRAPHS ARE STORED AS CGM FILES ON C:\sas ;

** AUTHOR: LCO May 2002 version 1.0 ;

****************;
** PARAMETERS : ;
****************;
** Data= contains the qoL data merged to the clinical data with step indicator 'step' and
** indicator of presence of baseline 'withbase' (_SCQOL by default, created by QOL_COmp_F);
**
** windat=windows dataset (Timewin by default);
**
** sel(ection)=WITH (if only with baseline included),ALL (otherwise), default=WITH ;
**
** where=if condition for selection of a subset of patients (eg: %str(trt1=1));
**
** domain= a list of QL domains for which drop-out profile patterns need to be produced ;
**
** device = CGM or PNG (by default, DEVICE=CGM: if you put nothing on DEVICE;
** parameter, it will choose CGM;
**
** graphs are cgm graphics stored in c:\sas with name dropQL, dropAP etc... ;
**;

**************;
** REVISION   ; 
***************;
** 08-12-2006 : Add of a DEVICE Parameter to choose between CGM or PNG file. (JRA)
** New handling of goptions with GOPTPNGSTD macro (this macro GOPTPNGSTD  is defined inside drop.sas) ;


%if (%length(&data)=0) %then %let data=_SCQOL ;
%if (%length(&windat)=0) %then %let windat=timewin ;
%if (%length(&sel)=0) %then %let sel=WITH ;

*drop out time ;
%gmax(data=&data,var=step,merge=&data) ; * dropout time ;

** number of patients by drop out time ;
%gmax(data=&data,var=step,outdata=_numbers,sel=
%if (%cmpres(&where)^= and %cmpres(&sel)=WITH)%then %do ;
withbase=1 AND &where
%end ;
%if (%cmpres(&where)^= and %cmpres(&sel)^=WITH) %then %do ;
&where
%end ;
%if (%cmpres(&sel)=WITH and %cmpres(&where)=) %then %do ;
withbase=1
%end ;
) ;

proc means data=_numbers noprint nway;
var mstep ;
class mstep ;
output out=_tot n=ntot ;
run ;


** creating formats and labels ;
proc sort data=&windat;
  by step;
  run;

data _windat ;
merge &windat _tot(rename=(mstep=step)) ;
by step ;
run ;

data _null_  _fmt (keep=start label fmtname);
   set _windat end=lastobs ;
          length x $40. ;
          by step;
          retain ns 0 ;
          ns+1;
          if lastobs then call symput('NS',compress(ns));
          call symput('Label'||trim(left(step+1)),trim(stepname)); * lower end of window ;
                  x=left(left(trim(stepname))||" (N="||compress(ntot)||")" );
          *** to create the format for step ;
          start=step ;
          label=x ;
          fmtname='_NStep' ;
          run;

          proc format cntlin=_fmt ;
          run ;


** computing mean profile ;
%LET K=1;
%DO %WHILE(%SCAN(&domain.,&k.) ne %STR());
 %let var=%SCAN(&domain.,&k.) ;



proc means data=&data noprint nway;
%if (%cmpres(&sel)=WITH and %cmpres(&where)^=) %then %do ;
where withbase=1 and (&where)  ;
%end ;
%if (%cmpres(&sel)=WITH and %cmpres(&where)=) %then %do ;
where  withbase=1 ;
%end ;
%if (%cmpres(&sel)^=WITH and %cmpres(&where)^=) %then %do ;
where &where ;
%end ;
var &var ;
class mstep step ;
output out=_meand mean=mean ;
run ;

data _meand ;
set _meand ;
call symput('title',label(mean)) ;
label mstep='Drop out time' ;
format mstep  _Nstep. ;
run  ;

** producing plot ;

*goptions hsize=18cm vsize=14cm ftext=simplex  rotate=landscape gsfmode=replace
gsfname=grafout device=cgmmw6c htext=1;

goptions reset=all ;

 %MACRO GOPTPNGSTD(hsize= ,vsize= ,rows= , cols=,devgrh=,font=,htext=,gunit=);
		goptions hsize=&hsize cm vsize=&vsize cm
		xpixels=15000 xmax=10 ypixels=15000 ymax=10 
		hpos=&cols
		vpos=&rows
		device=&devgrh
		ftext=&font
		htext=&htext
		htitle=2
		ctext=BLACK
		cback=WHITE
		gsfname=grafout
		gsfmode=REPLACE
		LFACTOR=10
		display
		noborder
		gunit=&gunit
	;
	
%MEND GOPTPNGSTD;

%IF &DEVICE.=CGM %THEN %DO;
	%GOPTPNGSTD(hsize=20, vsize=15, rows=, cols=,devgrh= CGMOF97P, font=HWCGM001, htext=2.2, gunit=pct);
%END;

%IF &DEVICE.=PNG %THEN %DO;
	%GOPTPNGSTD(hsize=12, vsize=11, rows=90, cols=110,devgrh= PNG, font=SWISSB, htext=1.4);
%END;

symbol1 value=dot c=black WIDTH=5;
%do i=1 %to &NS ;
%let j=%eval(&i+1) ;
symbol&j i=join v=none l=&i c=black WIDTH=2;
%end ;

axis1 label=(j=c "Assessment time")
 value=(
%do i=1 %to &NS ;
"&&label&i"
%end ;
) minor=none ;
axis2 label=('') order=(0 to 100 by 10);

filename grafout "c:\sas\drop&var..&DEVICE." ;
title 'Mean Score by drop out time';
title2 "&Title" ;
%if not(&where =) %then title3 "&where" ;
proc gplot data=_meand ;
plot mean*step=mstep / vaxis=axis2 haxis=axis1 ;
run ;

quit ;

%let k=%eval(&k+1) ;
%end ; * end of the while clause ;

proc datasets library=WORK nolist ;
delete _tot _meand _windat  _fmt _numbers;
run;


%mend drop ;


