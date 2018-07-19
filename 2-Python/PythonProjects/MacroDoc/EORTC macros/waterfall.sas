/*************************************************************************************

WATERFALL.SAS
**********

This macro is creating a waterfall plot.
You may plot for each patient from your dataset a vertical bar showing the evolution of a continuous variable
between two timepoints (i.e., % of change in PSA between baseline and week 4)

You may color the bars in a different way according to a Categorical variable.

Version date   : 20 october 2011
Software       : SAS version 9.2
Original author: Jérôme RAPION

version : 1.1

Modified by    : Jérôme RAPION

***********************************************************************************

PARAMETERS

    Data              :  Name of the data file (required parameter)
    Change            :  Variable containing the evolution variable (% of change) (required parameter)
    Category          :  Variable indicating a Categorical variable (optional parameter)
    Split             :  Y: if you want to get several waterfall plots (one per level of CATEGORY)
						 N: if you want to get only one waterfall plot with different colours per level of CATEGORY
						 Name of a variable: if you want to get several waterfall plots per level of CATEGORY 
                         and inside each plot to color your bars according to the level of SPLIT variable.
                         (see NOTES) (optional parameter) (default=N)
    Path              :  Complete path and name of the graph with extension (i.e. C:\temp\mygraph.png) (required parameter)
						 The extension can be PNG, TIF, JPEG, JPG or GIF.
	Title             :  Title of the graph, if you don't need a title, leave it blank (optional parameter)
    Colour            :  Set to Y to obtain graph in colour, N for black and white (optional parameter) (default=Y)
    Refline           :  Put a numerical value to draw a horizontal reference line (optional parameter) 


************************************************************************************

REVISIONS

20OCT2011  : Add of REFLINE parameter tobe able to draw a horizontal reference line at a specific value.
		     Add of frequencies (N=) on the plot
			 If CATEGORY is filled in and SPLT is also filled in with a variable name, possiblility to get 
             get several waterfall plots per level of CATEGORY and inside each plot to color your bars
             according to the level of another variable (JRA)
			
NOTES

********************
 SPLIT and CATEGORY parameters used in the same time
********************
 If CATEGORY is filled in with a variable with n levels and SPLIT is also filled in by a name of another variable, 
 then it means you will get n several plot (one per level of CATEGORY) and inside each plot, the bars will be colored
 according to the several levels of SPLIT variable.
 For instance, with CATEGORY=TRT1 and SPLIT=RESPONSE, you will get two plots (one per treatment arm), and on each plot,
 the bar will colored according to the level of RESPONSE (i.e. CR, R, SD PD)

EXAMPLES

%WATERFALL(data=waterfalls,change=changepsa,Category=resp,split=N, path=H:\desktop\waterfalls1.PNG,title=My title, colour=Y);

%WATERFALL(data=waterfalls,Category=trt1,change=changepsa,path=H:\desktop\waterfalls4.JPEG,title=,split=RESPONSE);

************************************************************************************/

%MACRO WATERFALL(data=,change=,split=,Category=,path=,title=,Colour=,Refline=);

%let SPLIT=%UPCASE(&SPLIT.);
%let Category=%UPCASE(&Category.);

%if %length(&Category.)=0 %then   %let Category=NO ;
%if %length(&split.)=0 %then   %let SPLIT=N ;
%if %length(&COLOUR.)=0 %then   %let COLOUR=Y ;
%if %length(&Refline.)=0 %then   %let Refline= ;


%macro GOPTPNGSTD(hsize= ,vsize= ,rows= , cols=,devgrh=,font=,htext=,gunit=,col=);

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

			%if &COL=Y %THEN colors=(blue red green black orange yellow NAVY OLIVE);
			%if &COL=N %THEN colors=(black darkgray gray dimgray lightgray );

;
      
%mend GOPTPNGSTD;

	data _data;
	set &data.;
	run;

%if &SPLIT=Y %then %do;

	%sort(data=_data,var=&Category. &change.);

	data _data;
	set _data;
	id=_n_;
	run;

%end;
%if &SPLIT=N %then %do;

	%sort(data=_data,var=&change.);

	data _data;
	set _data;
	id=_n_;
	run;


%END;


%if &SPLIT ne N and &SPLIT ne Y %then %do;


		proc contents data=_data out=_contdata noprint;
		run;

		data _contdata;
		set _contdata;
		NAME=UPCASE(NAME);
		FORMAT=UPCASE(FORMAT);
		if NAME = "&SPLIT"  then output;
		if NAME = "&CATEGORY"  then output;
		keep NAME FORMAT LABEL;
		run;

		data _null_;
		set _contdata;
		if NAME = "&SPLIT" then call symput('FORMATSPLIT',trim(left(format)));
		if NAME = "&SPLIT" then call symput('LABELSPLIT',trim(left(LABEL)));

		if NAME = "&CATEGORY" then call symput('FORMATCATEGORY',trim(left(format)));
		if NAME = "&CATEGORY" then call symput('LABELCATEGORY',trim(left(LABEL)));


		run;


		proc freq data=_data noprint ;
		table &SPLIT*&CATEGORY/out=_freq;
		RUN;

		proc freq data=_data noprint ;
		table &CATEGORY/out=_freq2;
		RUN;
		data _freq2;set _freq2;one=1;run;
		%sort(data=_freq2,var=one &CATEGORY);

		data _freq2;
		set _freq2;
		by one &CATEGORY.;
		if first.one then cum=PERCENT;
		else cum+PERCENT;
		cum2=lag(cum);
		if cum2=. then cum2=0;
		diff=(cum-cum2)/2;
		loc=round(cum2+diff,1);
		run;

		data _null_;
		set _freq2;
		i=_n_;
		call symput('COUNT'||compress(i),trim(left(COUNT)));
		call symput('LOC'||compress(i),trim(left(LOC)));
		call symput('VAL'||compress(i),"&LABELCATEGORY. : "||trim(left(put(&CATEGORY.,&FORMATCATEGORY..))));
		call symput('NVAL',trim(left(i)));
		call symput('VALUE'||compress(i),compress(&CATEGORY.));
		run;

		data _freq;
		set _freq;
		CROSS=compress(&category.)||compress(&split.);
		start=CROSS;
		end=CROSS;
		FMTNAME="CROSS";
		LABEL=put(&SPLIT.,&FORMATSPLIT..);
		TYPE='C';
		drop percent count &category. &split. ;
		run;


		data _data;
		set _data;
		_CROSS=compress(&category.)||compress(&split.);
		format _CROSS $CROSS.;
		label _CROSS ="&LABELSPLIT.";
		run;


		proc format cntlin=_freq;
		run;


%END;

%if &SPLIT = Y %then %do;

		proc freq data=_data noprint ;
		table &CATEGORY/out=_freq2;
		RUN;
		data _freq2;set _freq2;one=1;run;
		%sort(data=_freq2,var=one &CATEGORY);

		data _freq2;
		set _freq2;
		by one &CATEGORY.;
		if first.one then cum=PERCENT;
		else cum+PERCENT;
		cum2=lag(cum);
		if cum2=. then cum2=0;
		diff=(cum-cum2)/2;
		loc=round(cum2+diff,1);
		run;

		data _null_;
		set _freq2;
		i=_n_;
		call symput('COUNT'||compress(i),trim(left(COUNT)));
		call symput('LOC'||compress(i),trim(left(LOC)));
		call symput('NVAL',trim(left(i)));
		call symput('VALUE'||compress(i),compress(&CATEGORY.));
		run;

%END;

proc contents data=&data. noprint out=_cont;
run;

data _cont;
set _cont;
if UPCASE(NAME)=UPCASE("&change.") or UPCASE(NAME)=UPCASE("&Category.");
run;

data _null_;
set _cont;
if UPCASE(NAME)=UPCASE("&change.");
call symput('label',trim(left(label)));
run;

%IF &Category NE NO %THEN %DO;

	proc sort data=_data nodupkey out=_nlevel;
	by &Category.;
	run;

	%GCOUNT (DATA=_nlevel,BYVAR=&Category., OUTVAR=NLEVEL, OUTDATA=_nnlevel);

	data _nnlevel;
	set _nnlevel;
	call symput('NLEVEL',trim(left(NLEVEL)));
	run;

%do i=1 %to &NLEVEL;
	symbol&i i=needle w=3 r=1;
%end;


%END;

%ELSE %DO;
	symbol1 i=needle w=3 r=1;
%END;

%GOPTPNGSTD(hsize=12, vsize=11, rows=90, cols=110,devgrh= TIFFP, font=SWISSB, htext=1.4,col=&colour.);

filename grafout "&path.";

axis1 value=none minor=none major=none;
axis2 order=(-100 to +100 by 10) minor=none label=(angle=90 "&label" );

%if (%length(&title) ne 0) %then %do;
title "&title.";
%end;



%if &SPLIT = N %then %do;

	data _null_;
	set _data;
	i=_n_;
		call symput('NPAT',trim(left(i)));
	run;

	data Annotate;
		  length style when $ 8 text $ 50;
		   retain xsys ysys hsys '3'; x=50; y=15; text="n=&NPAT."; when="A";
		      style='swissb'; size=2; output;

		run;


	proc gplot data=_data annotate=Annotate;
	%IF &Category NE NO %THEN 
	plot &change.*id = &Category/ haxis=axis1 vaxis=axis2
		vref=(0 &refline.) CVREF=(black red) LVREF=(1 2) WVREF=1
	;;
	%IF &Category = NO %THEN 
	plot &change.*id/ haxis=axis1 vaxis=axis2
		vref=(0 &refline.) CVREF=(black red) LVREF=(1 2) WVREF=1
	;;
	label id="Patients";
	run;
	quit;
	title;

%END;

%if &SPLIT = Y %then %do;

	** Add a blank seperation between each plot per CATEGORY level;
	data _data;
	set _data;

	%do i=1 %to &NVAL;
	if &CATEGORY=&&VALUE&I then _add=%eval(&i-1);
	%end;
	ID=ID+_add*5;
	run;

	data Annotate;
	  length style when $ 8 text $ 50;

		%do i=1 %to &NVAL;

	   retain xsys ysys hsys '3'; x=%eval(&&LOC&i); y=15; text="n=&&COUNT&i"; when="A";
	      style='swissb'; size=2; output;
		  %end;

	run;

	proc gplot data=_data annotate=Annotate;
	plot &change.*id = &Category/ haxis=axis1 vaxis=axis2
		vref=(0 &refline.) CVREF=(black red) LVREF=(1 2) WVREF=1
	;
	label id="Patients";
	run;
	quit;
	title;

%END;



%if &SPLIT ne N and &SPLIT ne Y %then %do;

	data Annotate;
	  length style when $ 8 text $ 50;

		%do i=1 %to &NVAL;

	   retain xsys ysys hsys '3'; x=%eval(&&LOC&i); y=15; text="n=&&COUNT&i"; when="A";
	      style='swissb'; size=2; output;
	   retain xsys ysys hsys '3'; x=%eval(&&LOC&i); y=12; text="&&VAL&i"; when="A";
	      style='swissb'; size=1.5; output;

		  %end;

	run;


%sort(data=_data,var=_CROSS &change.);

	data _data;
		set _data;
		id=_n_;
	run;

	** Add a blank seperation between each plot per CATEGORY level;
	data _data;
	set _data;

	%do i=1 %to &NVAL;
	if &CATEGORY=&&VALUE&I then _add=%eval(&i-1);
	%end;
	ID=ID+_add*5;
	run;


	proc gplot data=_data annotate=Annotate;
	plot &change.*id = _CROSS/ haxis=axis1 vaxis=axis2
		vref=(0 &refline.) CVREF=(black red) LVREF=(1 2) WVREF=1
	;
	label id="Patients";
	run;
	quit;


%END;


proc datasets library=work nolist;
   delete 
	_data _fmtdesc _format _freq: _qwffmt 
	_nlevel _nnlevel
	_cont:;
quit;


%MEND;

*%WATERFALL(data=waterfalls,Category=,change=changepsa,path=H:\desktop\waterfalls1.PNG);
*%WATERFALL(data=_water,Category=resp2,change=changepsa,path=H:\desktop\waterfalls2b.PNG,colour=N,title=My title);


