/************************************************************************************
*
* PRED_FACT_FOREST.SAS
* ***********
*
* MACRO PRED_FACT_FOREST IS THE SECOND MODULE OF THE PRED_FACT macro ;
* It was created following the same inspiration as FOREST2 macro for meta-analysis;

* The purpose of this macro is to produce a forest plot showing HRs, CIs and p values for interaction test
* and trend test computed from a COX model using PROC PHREG
* This forest plot is based on the results from PROC PHREG computed into PRED_FACT_SUMMARY macro.
*
* Version date    : June 2014
* Software        : SAS version 9.4
* Original author : Jérôme RAPION
* inspired by FOREST2 macro for meta-analysis (written by Laurence COLLETTE)
*
*************************************************************************************
*
* HISTORY
*
*   Inspired from: FOREST2 Version 4.0 (by Laurence Collette)
*   A lot a graphical setting and annotations are taken from FOREST2
*      
*   REVISIONS:
*   ---------
*   030215: Change of the Font for PDF display (from Albany into ARIAL font)
*   so as to be readable into ILLUSTRATOR and INKSCAPE software.
*
************************************************************************************
* PARAMETERS
*
*    SUMMFILE  : name of the summary file containing the results to be displayed (Required parameter)
*    OUTFILE   : name of the path of the output file containing the graph without extension  (Required parameter)
*    DISPLAY   : PNG, TIFF, JPEG, PDF or SVG (Optional parameter) (default=PNG)
*    VAR       : List of variables to be analyzed. (Required parameter)
*    SCALE     : LIN or LOG, (Optional parameter) (default=LOG) 
*    FINAL     : Set to Y to remove footnote date (Optional parameter) (default=N)
*    COLOUR    : Set to Y to obtain graph in colour, N for black and white (Optional parameter) (default=Y)
*    SUBTIT    : Subtitle to add (Optional parameter)
*               (not necessary to write your subtitle between quotes, by default= no subtitle)
*
*************************************************************************************
*
* USAGE
*
*   Examples of macro calls:
*
*   %PRED_FACT_FOREST(summfile=working.bladsum,OUTFILE=c:\Temp\ForestPlot,
*            display=PNG,VAR=SEX);
*
*   %PRED_FACT_FOREST(summfile=working.bladsum,OUTFILE=c:\Temp\ForestPlot,
*            display=PDF,VAR=AGECAT, Scale=LIN,Final=Y,Colour=N);
*
*************************************************************************************
*
* NOTES
*
************************************************************************************/
option dlcreatedir;
libname GFONT0  'c:\sas\saswork';   * for the Gkeymap;
libname sasu  'c:\sas\saswork';   * for the Gkeymap;

************************************************************************;
*  macro to convert numerical to char macro variable without roundings  *;
************************************************************************;
** JRA, May 2010;
%macro numtochar(var=);


	if substr(&var,2,1)="." then do;

		if substr(&var,1,1) in ("0" "1" "2" "3" "4" "5" "6" "7" "8" "9") then a=substr(&var,1,1);
		b=".";
		if substr(&var,3,1) in ("1" "2" "3" "4" "5" "6" "7" "8" "9") then c=substr(&var,3,1);
		else c="0";
		if substr(&var,4,1) in ("1" "2" "3" "4" "5" "6" "7" "8" "9") then d=substr(&var,4,1);
		else d="0";
		&var.out=compress(a)||compress(b)||compress(c)||compress(d);

	drop a b c d;

	end;

	if substr(&var,3,1)="." then do;

		if substr(&var,1,1) in ("0" "1" "2" "3" "4" "5" "6" "7" "8" "9") then a1=substr(&var,1,1);
		if substr(&var,2,1) in ("0" "1" "2" "3" "4" "5" "6" "7" "8" "9") then a2=substr(&var,2,1);
		b=".";
		if substr(&var,4,1) in ("1" "2" "3" "4" "5" "6" "7" "8" "9") then c=substr(&var,4,1);
		else c="0";
		if substr(&var,5,1) in ("1" "2" "3" "4" "5" "6" "7" "8" "9") then d=substr(&var,5,1);
		else d="0";
		&var.out=compress(a1)||compress(a2)||compress(b)||compress(c)||compress(d);

		drop a1 a2 b c d;

	end;

    if substr(&var,1,1)="1" and substr(&var,2,1)="" then &var.out="1.00";
    if substr(&var,1,1)="2" and substr(&var,2,1)="" then &var.out="2.00";
    if substr(&var,1,1)="3" and substr(&var,2,1)="" then &var.out="3.00";
    if substr(&var,1,1)="4" and substr(&var,2,1)="" then &var.out="4.00";
    if substr(&var,1,1)="5" and substr(&var,2,1)="" then &var.out="5.00";
    if substr(&var,1,1)="6" and substr(&var,2,1)="" then &var.out="6.00";
    if substr(&var,1,1)="7" and substr(&var,2,1)="" then &var.out="7.00";
    if substr(&var,1,1)="8" and substr(&var,2,1)="" then &var.out="8.00";
    if substr(&var,1,1)="9" and substr(&var,2,1)="" then &var.out="9.00";

%mend;

*************************************************************;
*  program to map keys                                      *;
*  name : d:\sas\meta\gkeymap.sas                           *;
*  by LB 5/12/94                                            *;
*************************************************************;
%macro Keymap(inletr,outletr, inletr2, outletr2, inletr3, outletr3);
                       * hexadecimal code of the output letter see sasgraph 2 p. 989;
               *letter itself;

goptions reset=global;

proc gkeymap name=default out=sasu.temp;
run;

data sasu.new ;
   from= "&inletr";
   chartype=2;
   to= "&outletr"x;
   tolen=1 ;
   output ;
   from= "&inletr2";
   chartype=2;
   to= "&outletr2"x;
   tolen=1 ;
   output ;
   from="&inletr3";
   chartype=2 ;
   to="&outletr3"x;
   tolen=2 ;
   output ;
run;

proc sort data=sasu.new; by from ; run ;

data sasu.temp;
   update sasu.temp sasu.new;
   by from;
run;
proc gkeymap name = mykeymap
             data = sasu.temp
             keymap;
run;

%mend keymap;

*************************************************************;
*  macro to split a list of words into macro variables      *;
*  and to count the number of words into this vector        *;
*************************************************************;

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


%macro PRED_FACT_FOREST (summfile, OUTFILE, display, VAR, Scale, Final=, Colour=Y,subtit=);

** JRA, Feb 2009, decompositon of vector &VAR with each element created into &&A&i variable;
** Number of elements of vector &VAR put into &nb variable;
%global NB;
%if %length(&VAR.)>0 %then %RCHV(SUITE=&VAR,COMPTE=A,SEP=" ",N=NB);

%IF %LENGTH(&display.)=0 %THEN %LET display=PNG;

%let diamond=MSOLID;

%LET Scale=%UPCASE(&Scale.);
%LET Final=%UPCASE(&Final.);
%LET Colour=%UPCASE(&Colour.);

*JRA, 27MAY2010, Need distinguish if it is TTE or BIN endpoint, as FOREST and FORESTB2 are merged;

%if (%length(%cmpres(&VAR))>0) %then %let subVAR=Y ;
%if (%length(%cmpres(&VAR))=0) %then %let subVAR=N ;
%if (%length(%cmpres(&final))=0) %then %let final=N ;

%if (%length(%cmpres(&scale))=0 or %upcase(%substr(&scale,1,3))^=LIN) %then %let LOGS=Y ;
%if (%upcase(%substr(&scale,1,3))=LIN) %then %let LOGS=N ;

%IF &LOGS=N %THEN %LET LIMITU=2;
%IF &LOGS=Y %THEN %LET LIMITU=%sysevalf(1.3863);
%IF &LOGS=N %THEN %LET LIMITL=0;
%IF &LOGS=Y %THEN %LET LIMITL=%sysevalf(-1.3863);


* Define local variables for options which vary according to type of output required (value of DISPLAY);
%LOCAL OutFile GSFName GSFMode Device Font Rotate GUnit HPos VPos TextSize TitSize HSize;

%IF %UPCASE(&DISPLAY.)=TIFF %THEN %DO;
   %LET OutFile = %CMPRES(%scan(&OUTFILE.,1,.).tif);
   %LET GSFName = grafout;
   %LET GSFMode = replace;
 %IF %UPCASE(&COLOUR^=Y) %THEN %DO ; %LET Device = TIFFB ; %END ;
 %IF %UPCASE(&COLOUR=Y) %THEN %DO ; %LET Device = TIFFP  ; %END ;
   %LET Font = SWISS;
   %LET FontB = SWISSB;
   %LET Rotate = portrait;
   %LET GUnit=pct;
   %LET HPos=19720;
   %LET VPos=27160;
   %LET TextSize = 260;
   %LET TitSize  = 260;
   %LET HSize    = 260;

%END;

%ELSE %IF %UPCASE(&DISPLAY.)=JPEG %THEN %DO;
   %LET OutFile = %CMPRES(%scan(&OUTFILE.,1,.).jpg);
   %LET GSFName = grafout;
   %LET GSFMode = replace;
 %IF %UPCASE(&COLOUR^=Y) %THEN %DO ; %LET Device = JPEG300 ; %END ;
 %IF %UPCASE(&COLOUR=Y) %THEN %DO ; %LET Device = JPEG300  ; %END ;
   %LET Font = Arial;
   %LET FontB = Arial/Bold;
   %LET Rotate = portrait;
   %LET GUnit=pct;
   %LET HPos=19720;
   %LET VPos=27160;
   %LET TextSize = 260;
   %LET TitSize  = 260;
   %LET HSize    = 260;

%END;


%ELSE %IF %UPCASE(&DISPLAY.)=PNG %THEN %DO;
   %LET OutFile = %CMPRES(%scan(&OUTFILE.,1,.).png);
   %LET GSFName = grafout;
   %LET GSFMode = replace;
 %IF %UPCASE(&COLOUR^=Y) %THEN %DO ; %LET Device = TIFFB ; %END ;
 %IF %UPCASE(&COLOUR=Y) %THEN %DO ; %LET Device = TIFFP ; %END ;
   %LET Font = SWISS;
   %LET FontB = SWISSB;
   %LET Rotate = portrait;
   %LET GUnit=pct;
   %LET HPos=19720;
   %LET VPos=27160;
   %LET TextSize = 260;
   %LET TitSize  = 260;
   %LET HSize    = 260;

%END;

%ELSE %IF %UPCASE(&DISPLAY.)=SVG %THEN %DO;
   %LET OutFile = %CMPRES(%scan(&OUTFILE.,1,.).svg);
   %LET GSFName = grafout;
   %LET GSFMode = replace;
   %LET Device = SVG;
   %LET Font = Arial;
   %LET FontB = Arial/Bold;
   %LET Rotate = portrait;
   %LET GUnit=pct;
   %LET HPos=19720;
   %LET VPos=27160;
   %LET TextSize = 260;
   %LET TitSize  = 260;
   %LET HSize    = 260;
%END;

%ELSE %IF %UPCASE(&DISPLAY.)=PDF %THEN %DO;
   %LET OutFile = %CMPRES(%scan(&OUTFILE.,1,.).pdf);
   %LET GSFName = grafout;
   %LET GSFMode = replace;
 %IF %UPCASE(&COLOUR^=Y) %THEN %DO ; %LET Device = PDF ; %END ;
 %IF %UPCASE(&COLOUR=Y) %THEN %DO ; %LET Device = PDFC  ; %END ;
   %LET Font = Arial;
   %LET FontB = Arial/Bold;
   %LET Rotate = portrait;
   %LET GUnit=pct;
   %LET HPos=19720;
   %LET VPos=27160;
   %LET TextSize = 260;
   %LET TitSize  = 260;
   %LET HSize    = 260;
%END;


* Determine type of font chosen (for key map etc.).;
%LOCAL FontType;
%LET FontType=Other;

* Define macro variables for font colours, depending on whether graph is required 
  in colour or black and white;
%LOCAL Red Green;
%IF &Colour.=Y %THEN %DO;
   %LET Red=BLUE;
   %LET Green=GREEN;
%END;
%ELSE %DO;
   %LET Red=BLACK;
   %LET Green=BLACK;
%END;



proc datasets library=WORK nolist ;
 delete annoG ;
 run;




* COMPILE ANNOTATE MACROS ;
%ANNOMAC;

* OPTIONS ;

**  KEEP FORMATS FOR VAR AND TOTAL PARAMTERS- JRA, 16OCT2006 **;

proc contents data=&summfile out=format noprint;
run;

data format;
set format;
name=upcase(name);
run;

%local fgr ftot testfmt lfmt ;

%let gVAR=%UPCASE(&VAR);
%let ttestvar=%UPCASE(&testv);

%if %length(&VAR.)>0 %then %put &gVAR;

%IF &NB >1 %THEN %DO;

data format;
	set format;
	if name in (%do j=1 %to &NB; %UPCASE("&&a&j.") %end;) ;
		format=compress(format)||".";
	if format="." then format="8.";
	keep name format ;
run;
data _null_;
	set format;
	%do j=1 %to &NB;
	ind=&j;
	if name=%UPCASE("&&a&j.") then call symput("fgr"||compress(ind),format);
	%end;
	drop ind;
run;

%END;

%IF &NB <= 1 %THEN %DO;

data format;
	set format;
	if name in ("&gVAR" "&ttestvar");
	format=compress(format)||".";
	if format="." then format="8.";
	keep name format ;
	if name="&gVAR" then call symput('fgr',format);
run;

%if %length(&VAR.)>0 %then %put &fgr ;
%put &ftrt;

%END;

*JRA, 17APR2008, save of format name for TRT without a '.' in &testfmt;

%let lfmt=%length(&ftrt);
%let testfmt=%substr(&ftrt,1,%eval(&lfmt-1));


proc format lib=work cntlout=__format;
run;


data __format;
	set __format;
	label=COMPBL(label);
	n=length(label);
	where FMTNAME="&testfmt." and START ne "~~";
	ind=1;
run;

%sort(data=__format,var=ind start);

%local trt1 trt2;

data _null_;
	set __format;
	by ind start;
	if first.ind;
	call symput('trt2', trim(left(LABEL))); *JRA, 17APR2008; 
run;

data _null_;
	set __format;
	by ind start;
	if last.ind;
	call symput('trt1', trim(left(LABEL))); *JRA, 17APR2008; 
run;


*** SETS THE DEFAULT VALUES FOR THE GRAPHICAL ELEMENTS ;
%if (%length(%cmpres(&Trt1))=0) %then %let Trt1=%str(Exp.) ;
%if (%length(%cmpres(&Trt2))=0) %then %let Trt2=%str(Stand.) ;

%let lg1= %length(%trim(%left(&Trt1)));
%let lg2= %length(%trim(%left(&Trt2)));

%put lg1 &lg1.;
%put lg2 &lg2.;

%put trt1: &trt1.;
%put trt2: &trt2.;

%let d1=;
%let d2=;
%let d3=;
%let d4=;
%let d5=;
%let d6=;
 %let d7=;
%let d8=;
%let d9=;
%let d10=;
%let c1=1;
%let c2=2;
%let c3=3;
%let c4=4;
%let c5=5;
%let c6=6;
%let c7=7;
%let c8=8;
%let c9=9;
%let c10=10;
%let TE=%str(Unadjusted treatment effect) ;
%let TEp=%str(p) ;
%let TEz=;        
%let TH=%str(Test for heterogeneity) ; 
%let THchi=%str(Chi-square) ;
%let THdf=%str(df) ;
%let THp=%str(p) ;
 %let TI=;
 %let TIchi=;
 %let TIdf=;
 %let TIp=;
 %let TTR=;
 %let TTRchi=;
 %let TTRp=;
 %let TTRz=;       
%let lVAR=;
%let lVAR1=;
%let lVAR2=;
%let lVAR3=;
%let lVAR4=;
%let lVAR5=;
%let tit0=&title;
%let subtit=&Subtit.;  * JRA, apr 2008, if no labfile is used, 
it is still possible to add a subtitle with SUBTIT parameter; 
%let tit11=;
%let tit121=%str(Events);
%let tit122=%str(Patients);
%let tit13=%str(Statistics) ;

%let conf1=&alpha1.;

%let tit14=%str(HR & CI) ; * LCO;

%let tit21=;
%let tit231=%str((O-E)) ;
%let tit232=%str(Var.) ;

** JRA, May2010, replacement of |1-HR| % +/- SD by HR (&conf1. % CI); 
%let tit25=%str(HR (&conf1.% CI)) ;

%let tit54=%str(better) ;

** y coord of the line separating titles and plot ;
** warning: counted from bottom to top! ;
%let top      = 22500;

** size of the total square on the left of the plot ;
%let Tsqside  = 1000;

** length of drawing units for the scale ;
%if (&LOGS=Y) %then %let axisun = 1984;
%if (&LOGS=N) %then %let axisun = 2750;

%let space    = 80;
%let colspace = 30;
%let linesize = 4;
%let litsize=3 ; * size of the lines for the empty squares for litt based data ;
%let cisize   = 30; *line size of the CI;

%IF %UPCASE(&DISPLAY.)=SVG %THEN %let linesize = 1;


* LINES *;

** x start of line separating titles from plot ;
%let Ltit    = 1000;

** Ldescr middle of code and study descr ;
%let ldescr  = 1500;
*** positions for the text ;
%let LON1    = 5000;
%let LON2    = 6600;
%let LO_E    = 8000;
%let LvarO_E = 8900;
** text when blinded or no data ;
%let Lblind=7100 ;

*** Laxis0 x coord. for the vertical solid line ;
%if (&LOGS=N) %then %let Laxis0  = 10900+&axisun;
%if (&LOGS=Y) %then %let Laxis0  = 10900+1.3863*&axisun ;
*** Laxis1=starting point of the scale, origin for the drawings in linear scale ;
%let Laxis1=10900 ;
*** Laxis2=Laxis0 when Log scale and Laxis1 when linear scale ;
*** used for the diamonds ;
%if (&LOGS=Y) %then %let Laxis2=&Laxis0 ;
%if (&LOGS=N) %then %let Laxis2=&Laxis1 ;

*** middle for the reduction labels ;
%let Lred    = 17300 ;

* READS IN THE LABELS DATA FILE ;

************************************************************************;
************************************************************************;
%IF &NB<= 1 %THEN %DO; *USEFUL ONLY IF VAR DOESN T CONTAIN SEVERAL VARIABLES
************************************************************************;

	* READS IN THE TEST RESULTS ;

	data _tests ;
	 set &summfile;
	 where flag=99;
			    call symput('TE3', compress(put(ProbChiSq,8.3)));
	run;

	%put TE3 &TE3.;

	%if &TE3 =0.000 %then %let TE3b=< 0.001;

	%let effect=;
	* Treatment effect ;
	%let effect=%left(%trim(&TE)): %left(%trim(&TEp))= &TE3;

	%if &TE3 =0.000 %then %let effect=%left(%trim(&TE)): %left(%trim(&TEp)) &TE3b;

	%let nmiss=;
	data nmiss;
		set &summfile;
		if nmiss ne .;
	 	call symput('nmiss', compress(put(nmiss,8.0))) ;
	run;

	 data _null_ ;
		 set &summfile ;
		 if pvaltrend ne '' then do;
		    call symput('TTRp', compress(pvaltrend)) ;
		 end;
	run;

%END; * END IF NB<= 1 THEN DO;

%LOCAL LeftShift RightShift;
	%let LeftShift=(%length(%left(%trim(&trt2)))+2)*150 ;
	%let RightShift=(%length(%left(%trim(&trt1)))+2)*150 ;

* CREATES THE DATE-TIME FOR PRINTING ON THE PLOT ;

data _date_ ;
 date = date() ;
 time=time() ;
 format time time6. ;
 format date worddatx12.;
 call symput('_DATE_',left(put(date,worddatx12.))) ;
 call symput('_TIME_',left(put(time,time6.))) ;
 run ;

* HEADER ;
data annoH;

  * LENGTH OF ANNOTATE VARIABLES *;
  %DCLANNO;
  length text $ 60;

   * SYSTEM SETTINGS ;
   * X,Y,H-SYS=4 : Absolute coordinate system ;
   *                of graphics output area   ;
   *                units in cells            ;
   *                (1000 cells = 1 cm        ;
   *                (See also GOPTIONS)       ;
  %SYSTEM(4,4,4);
  * Y COORDINATES ;
  L=&hsize+&space;
  YL1=&top+(L*2)+(L/2);
  YT1=YL1+(2/5)*&hsize;
  YT2=&top+L+(L/2)+(2/5)*&hsize;
  YT3=&top+(L/2)+(2/5)*&hsize;
  * HEADERS ;

  %LABEL(&Ltit,               YT2, "&tit11",       BLACK,0,0,&hsize,&font.,6);
  %LABEL(&Ltit,               YT3, "&tit21"    ,           BLACK,0,0,&hsize,&font.,6);

  %LABEL((&LON1+&LON2)/2,     YT2, "&tit121 / &tit122",       BLACK,0,0,&hsize,&font.,5);

  %let plus1=%sysevalf((&lg2.-8)*70);
  %if &plus1 <0 %then %let plus1=0;
  %LABEL(&LON1-&plus1,               YT3, "&trt1",                BLACK,0,0,&hsize,&font.,5);
  %LABEL(&LON2,               YT3, "&trt2",                BLACK,0,0,&hsize,&font.,5);

  %LABEL(&LvarO_E+650,            YT3, "  &tit25",                 BLACK,0,0,&hsize,&font.,4);

  %LABEL(&Laxis0,             YT2, "&tit14",                 BLACK,0,0,&hsize,&font.,5);

  pos1=&Laxis0-&LeftShift ;
  pos2=&Laxis0+&RightShift ;


  %let plus2=%sysevalf((&lg2.-8)*100);
  %if &plus2 <0 %then %let plus1=0;


     %LABEL(&Laxis0-750,YT3,"(&trt1.",BLACK,0,0,&hsize,&font.,4); * Binary;
     %LABEL(&Laxis0,             YT3, ":",   BLACK,0,0,&hsize,&font.,5);
     %LABEL(&Laxis0+1250+&plus2,YT3,"&trt2.)",BLACK,0,0,&hsize,&font.,4);

	%IF NOT(%UPCASE(&DISPLAY.)=WORD OR %UPCASE(&DISPLAY.)=POWERPOINT OR %UPCASE(&DISPLAY.)=SCREEN) %THEN %DO;
		%LABEL(&Lred+400,               YT3, "Interaction test",                  BLACK,0,0,&hsize,&font.,5);
	%END;

  * TOPLINE ;
  %LINE(&Ltit,&top,&Laxis0+6000,&top,BLACK,1,&linesize);

run;

* TOTALS ;

DATA _NULL_ ;

set &summfile ;
where flag=99;

ntot=ndeath1+ndeath2;


 ** JRA, May2010, computation of the HR and CI roundings;
 ORround=round(OR,0.01);
 ORroundt=trim(left(ORround||""));
 %numtochar(var=ORroundt);

 LCL&conf1.r=roundz(LCL&conf1.,0.01);
 UCL&conf1.r=roundz(UCL&conf1.,0.01);

 LCL&conf1.t=trim(left(LCL&conf1.r||""));
 UCL&conf1.t=trim(left(UCL&conf1.r||""));
 %numtochar(var= LCL&conf1.t);
 %numtochar(var= UCL&conf1.t);

 if (_N_=1) then do ;
   call symput('Tndth1', ndeath1) ;
   call symput('Tnpat1', npat1) ;
   call symput('Tndth2', ndeath2) ;
   call symput('Tnpat2', npat2) ;
   call symput('NTotal', round(ntot, .1)) ;
%if (&LOGS=Y) %then %do ;
   call symput('TOR',log(OR)) ;
   call symput('TLCL1',log(LCL&conf1)) ;
   call symput('TUCL1',log(UCL&conf1)) ;
   call symput('RTOR',ORroundtout) ;
   call symput('RTLCL1',LCL&conf1.tout) ;
   call symput('RTUCL1',UCL&conf1.tout) ;
%end ;
%if (&LOGS=N) %then %do ;
   call symput('TOR',OR) ;
   call symput('TLCL1',LCL&conf1) ;
   call symput('TUCL1',UCL&conf1) ;
   call symput('RTOR',ORroundtout) ;
   call symput('RTLCL1',LCL&conf1.tout) ;
   call symput('RTUCL1',UCL&conf1.tout) ;
%end ;
   call symput('TR', cent1_OR) ;
   call symput('TRSD', SD);
   call symput('TRT', change) ;
   end ;

%if &NB <=1 %then %do; *IF VAR CONTAINS ONLY ONE OR ZERO VARIABLE;	
	if eof then call symput('nn', compress(ind));
	if (_N_=2) then	call symput('start', compress(ind));

  %end;

RUN ;

%put NTotal &NTotal;

%PUT RTOR &RTOR ;

%IF &NB >1 %THEN %DO; *IF VAR CONTAINS SEVERAL VARIABLES;

	 *** HETEREOGENEITY & TREND TESTS **;

	data _null_ ;
			 set &summfile ;
			 if flag=99;
			    call symput('TE3', compress(put(ProbChiSq,8.3)));
	run;

	%put TE3 &TE3.;

	%if &TE3 =0.000 %then %let TE3b=< 0.001;


	%let effect=;
	* Treatment effect ;
	%if (%length(&TE)^=0) %then %do ;
	  %let effect=%left(%trim(&TE)): %left(%trim(&TEp))= &TE3;
	  %end ;

	%if &TE3 =0.000 %then %let effect=%left(%trim(&TE)): %left(%trim(&TEp)) &TE3b;


	%let nmiss=;
	data nmiss;
		set &summfile;
		if nmiss ne .;
	run;

	%sort(data=nmiss,var=flag);

	data _null_;
	set nmiss;
		call symput('nmiss'||compress(flag), compress(put(nmiss,8.0))) ;
		call symput('flag'||compress(flag), compress(put(flag,8.0))) ;
	run;

	 data _null_ ;
		 set &summfile ;
		 if pvaltrend ne '' then do;
		    call symput('TTRp'||left(flag), compress(pvaltrend)) ;
		 end;
	run;

%END;

 ** Add of clause where - JRA, 16OCT2006 **;
data summfile1 ;
 set &summfile ;
	where flag ne 99;

		ntot=ndeath1+ndeath2;

**JRA, May 2010, rounding and formating of HR and CI;
 ORround=round(OR,0.01);
 LCL&conf1.r=roundz(LCL&conf1.,0.01);
 UCL&conf1.r=roundz(UCL&conf1.,0.01);

 ORroundt=trim(left(ORround||""));
 LCL&conf1.t=trim(left(LCL&conf1.r||""));
 UCL&conf1.t=trim(left(UCL&conf1.r||""));

 %numtochar(var=ORroundt);
 %numtochar(var= LCL&conf1.t);
 %numtochar(var= UCL&conf1.t);

 if ORround=1 then ORroundtout="1.00";
 if  LCL&conf1.r=1 then LCL&conf1.tout="1.00";
 if  UCL&conf1.r=1 then UCL&conf1.tout="1.00";

 format LCL&conf1.r UCL&conf1.r ORround 5.2;
 drop ORroundt  LCL&conf1.t UCL&conf1.t;

	 if (ndeath1=0 or ndeath2=0) then do;

			ORroundtout="";
			LCL&conf1.tout="";
			UCL&conf1.tout="";

			if ORround=0 and LCL&conf1.r = 0 then do;
				OR=.;
				LCL&conf1=.;
				UCL&conf1=.;

			end;

			if ORround > 10 and LCL&conf1.r = 0 then do;
				OR=.;
				LCL&conf1=.;
				UCL&conf1=.;

			end;


			if ntot= 0 then do;
				OR=.;
				LCL&conf1=.;
				UCL&conf1=.;

			end;

	 end;

run;


data an;
	set summfile1 ;
   * TO SAVE &VAR FORMATS - JRA, 16OCT2006**;	
   %if %upcase(&subVAR)=Y %then %do;

   		%if &NB > 1 %then %do;*IF VAR CONTAINS SEVERAL VARIABLES;
		   %do J=1 %to &NB;
				VAR&j.=put(&&a&j,&&fgr&J..);
			    label1&j.=vlabel(&&a&j.);
			    label1&j.=compbl(label1&j.);
				_lt&j.=length(label1&j.);

				if upcase(VAR&j.)="MISSING" then VAR&j.="";
				if upcase(VAR&j.)="UNKNOWN" then VAR&j.="";

		   %end;

		   VAR= trim(left(VAR1)) %DO J=2 %TO &NB; ||trim(left(VAR&j.)) %END;;

		   VAR=trim(left(tranwrd(VAR, ".", "")));

		   %if &NB= 1 %then %do;lt1=_lt1;%end;
		   %else %do;
		   lt1=max(_lt1 %do J=2 %to &NB; , _lt&j. %end;);;
		   %end;

		   %do J=1 %to &NB;
				if &&a&j ne . then label1=label1&j.;
		   %end;

		   %do J=1 %to &NB;
				drop _lt&j. VAR&j. label1&j.;
		   %end;

		%end;

   		%if &NB <= 1 %then %do;*IF VAR CONTAINS ONLY ONE OR ZERO VARIABLE;

			VAR=put(&VAR,&fgr.);
		    label1=vlabel(&VAR.);
		    label1=compbl(label1);
			lt1=length(label1);
		%end;

   %end;;


run;

data _null_;
	set an;
	call symput('lt1',lt1);
	call symput('lt2',lt2);
run;

%put lt1 &lt1;

%put &lt1 &lt2;

%if &NB >1 %then %sort(data=an,var=flag); *IF VAR CONTAINS SEVERAL VARIABLES;


data anno;
  set an end=lastobs;

  %if &NB >1 %then %do; by flag ; %end; *IF VAR CONTAINS SEVERAL VARIABLES;

  * LENGTH OF ANNOTATE VARIABLES;
  %DCLANNO;
  length text $ 50;

  * SYSTEM SETTINGS ;
  * X,Y,H-SYS=4 : Absolute coordinate system ;
  *                 of graphics output area  ;
  *                 units in cells           ;
  *                 (1000 cells = 1 cm       ;
  *                 (See also GOPTIONS)      ;
  %SYSTEM(4,4,4);

** TITLES ;
%if (%length(%cmpres(&tit0))>0) %then %do ;
 %LABEL(&Ltit+9000,&Top+2500, "&tit0", BLACK, 0,0,400,&FontB.,2) ;
%end ;
%if (%length(%cmpres(&subtit))>0) %then %do ;
 %LABEL(&Ltit+9000 ,&Top+2000, "&subtit", BLACK, 0,0,360,&font.,2) ;
%end ;


 if (_N_=1) then do ;
     * Y COORDINATE ;
     L=&space*4;
     H+L;
     YL=&top-H+(L/2);
     YT=YL;
 end ;

  * Y COORDINATE ;

* JRA, 18042014: sqside ->size of the square, percentage of number of events in each strata X size of the TOTAL square;
  sqside=int((ntot/&NTotal)*&Tsqside);

  L=max(&textsize,sqside,400)+&space;

  H+L;
  YL=&top-H+(L/2);
  YT=YL+(2/5)*&textsize;

  * DRAW TEXT ;

  %let Lright=10500;
  ** JRA, 4MAY2010, new standard size for add2;
  %let add1=500;


  ** CASE SUBVAR = Y;
	 %if &NB >= 1 %then %do;

		*IF VAR CONTAINS ONE or SEVERAL VARIABLES;

	    * JRA 4MAY2010, from now, if VAR is not empty, we display HR and CI for one 
		  or several variables into VAR parameter;

	           %LABEL(&Ltit, YT+300, label1,                BLACK,0,0,&textsize,&FontB.,6);
		       %LABEL(&Ltit+&add1., YT, VAR,                BLACK,0,0,&textsize,&font.,6);

	%end;

  if ((npat1>0 or npat2>0)) then do ;

  %LABEL(&LON1-&colspace*3,   YT, trim(ndeath1), BLACK,0,0,&textsize,&font.,4);
  %LABEL(&LON1,   YT, '/', BLACK,0,0,&textsize,&FontB.,5);
  %LABEL(&LON1+&colspace*3,   YT, left(npat1),   BLACK,0,0,&textsize,&font.,6);

  %LABEL(&LON2-&colspace*3,   YT, trim(ndeath2), BLACK,0,0,&textsize,&font.,4);
  %LABEL(&LON2,   YT, '/', BLACK,0,0,&textsize,&FontB.,5);
  %LABEL(&LON2+&colspace*3,   YT, left(npat2),   BLACK,0,0,&textsize,&font.,6);

  %LABEL(&LO_E, YT, compress(ORroundtout),                BLACK,0,0,&textsize,&font.,6);


  if NOT(ndeath2=0 or ndeath1=0) then do;
	  %LABEL(&LvarO_E, YT, '('||compress(LCL&conf1.tout)||' ; '||compress(UCL&conf1.tout)||')' ,                BLACK,0,0,&textsize,&font.,6);
  end;

  ** Interaction test;
  %LABEL(&Lred+400,               YT, trim(pval),                  BLACK,0,0,&hsize,&font.,5);


  end ;

  if ((npat1<=0 and npat2<=0)) then do ;
  %LABEL(&Lblind, YT, "(DATA NOT AVAILABLE)",BLACK,0,0,&textsize,&font.,5) ;  
  end ;


* SQUARES AND LINES  ;

if (not missing(OR)) then do ;

%if (&LOGS=Y) %then %do ;
 
* 1. No arrow at the upper end ;

 if (log(UCL&conf1)<1.3863 and log(UCL&conf1)>=-1.3863) then do;

     if (log(OR)<-1.3863) then do ;
     point=&Laxis0-(&axisun*1.5) ;
     end ;
     if (log(OR)>=-1.3863 and log(OR)=<1.3863) then do ; 
     point=&Laxis0+(&axisun*log(OR));
     end ;
     sqpos=point-sqside/2;
     cipos=&Laxis0+(&axisun*log(LCL&conf1));
       
        if (sqpos < cipos ) then  
           do;  * verification whether the square has to be devided into 4 parts lb 9/12/94 *;
              point=&Laxis0+(&axisun*log(OR));
              %BAR(point-sqside/2,YL-sqside/2,point+sqside/2,YL-&cisize,&Red.,1,SOLID);
              * lower part;
              %BAR(point-sqside/2,YL+&CIsize,point+sqside/2,YL+sqside/2,&Red.,1,SOLID);
              * upperpart;
               %BAR(point-sqside/2,YL-&cisize,&Laxis0+(&axisun*log(LCL&conf1)),YL+&cisize,&Red.,1,SOLID);
              * left central;
              if &Laxis0+(&axisun*log(UCL&conf1))<point+sqside/2 then
              do;
                   %BAR(&Laxis0+(&axisun*log(UCL&conf1)),YL-&cisize,point+sqside/2,YL+&cisize,&Red.,1,SOLID);
                   * right central1;
               end;
               else do;
                     %BAR(point+sqside/2,YL-&cisize,&Laxis0+(&axisun*log(UCL&conf1)),YL+&cisize,&Red.,1,SOLID); *right central2;
                end;
            end;

        if (sqpos<cipos ) then do;
              %RECT(point-sqside/2-&litsize/2,YL-sqside/2-&litsize/2,point+sqside/2+&litsize/2,YL+sqside/2+&litsize/2,&Red.,1,&litsize);
              %BAR(&Laxis0+(&axisun*log(UCL&conf1)),YL-&cisize,&Laxis0+(&axisun*log(UCL&conf1)),YL+&cisize,&Red.,1,SOLID);
        end ;
           
        
        if (sqpos>=cipos) then do ;

* 1.1.: No arrow at the upper nor the lower end ;

          if (log(LCL&conf1)>-1.3863) then do ; * Normal CI ;

            %BAR(&Laxis0+(&axisun*log(LCL&conf1)),YL-&cisize,&Laxis0+(&axisun*log(OR))-sqside/2,YL+&cisize,&Red.,1,SOLID);
            %BAR(&Laxis0+(&axisun*log(OR))+sqside/2,YL-&cisize,&Laxis0+(&axisun*log(UCL&conf1)),YL+&cisize,&Red.,1,SOLID);
            point=&Laxis0+(&axisun*log(OR));

           %BAR(point-sqside/2,YL-sqside/2,point+sqside/2,YL+sqside/2,&Red.,1,SOLID);

            end ;

* 1.2.: No arrow at the upper end, arrow at the lower end ;
         if (log(LCL&conf1)<=-1.3863 and log(LCL&conf1)>.Z) then do ;
           * arrow at the lower end ;
           if (log(OR)<-1.3863) then do ;
              point=&laxis0-(&axisun*1.5) ;
              end ;
           else do ;
              point=&Laxis0+(&axisun*log(OR)) ;
              end ;
              %BAR(point-sqside/2, YL-sqside/2, point+sqside/2, YL+sqside/2, &Red., 1,SOLID) ;
              %LINE(1,1,2,2, white, 1, &linesize); * dummy line to reset ;
              %POLY(    &Laxis0-(&axisun*1.32),     YL+&CISIZE*2, &Red.,SOLID,1);
              %POLYCONT(&Laxis0-(&axisun*1.32)-&cisize*3, YL,    &Red.        );
              %POLYCONT(&Laxis0-(&axisun*1.32),     YL-&cisize*2, &Red.        );
              %POLYCONT(&Laxis0-(&axisun*1.32),     YL+&cisize*2, &Red.        );
            if (log(OR)<-1.3863) then do ;
              %BAR(&Laxis0-(&axisun*1.32),YL-&CIsize,&Laxis0+(&axisun*log(UCL&conf1)),YL+&CIsize,&Red.,1,solid);
              end ;
            if (log(OR)>=-1.3863) then do ;
              %BAR(&Laxis0-(&axisun*1.32),YL-&CIsize,&Laxis0+(&axisun*log(OR))-sqside/2,YL+&CIsize,&Red.,1,solid);
              %BAR(&Laxis0+(&axisun*log(OR))+sqside/2,YL-&CIsize,&Laxis0+(&axisun*log(UCL&conf1)),YL+&CIsize,&Red.,1,solid);
              end ;
            end;
         end;
 end;


* 2. : Arrow at the upper end ;

if (log(UCL&conf1)>=1.3863) then do ;

    * arrow at the upper end ;

  if (log(OR)>1.3863) then do ;   
     point=&Laxis0+(&axisun*1.6) ;
  end ;
  if (log(OR)<-1.3863) then do ;
     point=&Laxis0-(&axisun*1.5) ;
     end ;
  if (log(OR)>=-1.3863 and log(OR)<=1.3863) then do ;
     point=&Laxis0+(&axisun*log(OR));
     end ;
     %BAR(point-sqside/2,YL-sqside/2,point+sqside/2,YL+sqside/2,&Red.,1,SOLID);
     %LINE(1,1,2,2, white,1,&linesize); *dummy line to reset;
     %POLY(    &Laxis0+(&axisun*1.3863),     YL+&CISIZE*2, &Red.,SOLID,1);      
	 %POLYCONT(&Laxis0+(&axisun*1.3863)+&cisize*3, YL,    &Red.       );
     %POLYCONT(&Laxis0+(&axisun*1.3863),     YL-&cisize*2, &Red.        );
     %POLYCONT(&Laxis0+(&axisun*1.3863),     YL+&cisize*2, &Red.        );

* 2.1.: Arrow at the upper end but not on the lower end ;

     if (log(LCL&conf1)>-1.3863 and log(LCL&conf1)<=1.3863) then do ; * arrow at the upper end only ;
        if (log(OR)>1.3863) then do ;
           %BAR(&Laxis0+(&axisun*log(LCL&conf1)),YL-&CIsize,&Laxis0+(&axisun*1.3863),YL+&CIsize,&Red.,1,solid);
           end ;
        if (log(OR)<=1.3863) then do ;
           %BAR(&Laxis0+(&axisun*log(LCL&conf1)),YL-&CIsize,&Laxis0+(&axisun*log(OR))-sqside/2,YL+&CIsize,&Red.,1,solid);
           %BAR(&Laxis0+(&axisun*log(OR))+sqside/2,YL-&CIsize,&Laxis0+(&axisun*1.3863),YL+&CIsize,&Red.,1,solid);
           end ;
        end ;

* 2.2: Arrow at both ends ;

     if (log(LCL&conf1)<=-1.3863 and log(LCL&conf1)>.Z) then do ;
     * arrows at both ends ;
     %POLY(    &Laxis0-(&axisun*1.32),     YL+&CISIZE*2, &Red.,SOLID,1);
     %POLYCONT(&Laxis0-(&axisun*1.32)-&cisize*3, YL,    &Red.        );
     %POLYCONT(&Laxis0-(&axisun*1.32),     YL-&cisize*2, &Red.        );
     %POLYCONT(&Laxis0-(&axisun*1.32),     YL+&cisize*2, &Red.        );
     if (log(OR)<-1.3863 or log(OR)>1.3863) then do ;
     %BAR(&Laxis0-(&axisun*1.32),YL-&CIsize,&Laxis0+(&axisun*1.3863),YL+&CIsize,&Red.,1,solid);
     end ;
     if (log(OR)>=-1.3863 and log(OR)<=1.3863) then do ;
     %BAR(&Laxis0-(&axisun*1.32),YL-&CIsize,&Laxis0+(&axisun*log(OR))-sqside/2,YL+&CIsize,&Red.,1,solid);
     %BAR(&Laxis0+(&axisun*log(OR))+sqside/2,YL-&CIsize,&Laxis0+(&axisun*1.3863),YL+&CIsize,&Red.,1,solid);
     end ;
     end;

end ;
*** Other situations: LCL>1.3863 or UCL<-1.3863: draw just the square ;
%end ; * end of Log scale ;

%if (&LOGS=N) %then %do ;

* 1. No arrow at the upper end ;

 if (UCL&conf1< 2 and UCL&conf1>.Z) then do;
     point=&Laxis1+(&axisun*OR);
     sqpos=point-sqside/2;
     cipos=&Laxis1+(&axisun*LCL&conf1);
       
        if (sqpos < cipos ) then   
           do; * verification whether the square has to be devided into 4 parts lb 9/12/94 *;
              point=&Laxis1+(&axisun*OR);
              %BAR(point-sqside/2,YL-sqside/2,point+sqside/2,YL-&cisize,&Red.,1,SOLID);
              * lower part;
              %BAR(point-sqside/2,YL+&CIsize,point+sqside/2,YL+sqside/2,&Red.,1,SOLID);
              * upperpart;
              ** LCO Sept 2004:changed Laxis0 to Laxis1 in next statement (caused errors), Laxis1=origin for lin scale ;
               %BAR(point-sqside/2,YL-&cisize,&Laxis1+(&axisun*LCL&conf1),YL+&cisize,&Red.,1,SOLID);
              * left central;
              if &Laxis1+(&axisun*UCL&conf1)<point+sqside/2 then
              do;
                   %BAR(&Laxis1+(&axisun*UCL&conf1),YL-&cisize,point+sqside/2,YL+&cisize,&Red.,1,SOLID);
                   * right central1;
               end;
               else do;
                     %BAR(point+sqside/2,YL-&cisize,&Laxis1+(&axisun*UCL&conf1),YL+&cisize,&Red.,1,SOLID); *right central2;
                end;
        end;
       
        if (sqpos>=cipos) then do ;

            %BAR(&Laxis1+(&axisun*LCL&conf1),YL-&cisize,&Laxis1+(&axisun*OR)-sqside/2,YL+&cisize,&Red.,1,SOLID);
            %BAR(&Laxis1+(&axisun*OR)+sqside/2,YL-&cisize,&Laxis1+(&axisun*UCL&conf1),YL+&cisize,&Red.,1,SOLID);
            point=&Laxis1+(&axisun*OR);

           %BAR(point-sqside/2,YL-sqside/2,point+sqside/2,YL+sqside/2,&Red.,1,SOLID);

         end ;
 end;

* 2. : Arrow at the upper end ;

if (UCL&conf1>=2) then do ;

   if (OR>2) then do ;   
     point=&Laxis1+(&axisun*2.14)+sqside/2 ;
   end ;
   if (OR<=2) then do ;
     point=&Laxis1+(&axisun*OR);
   end ;

     %BAR(point-sqside/2,YL-sqside/2,point+sqside/2,YL+sqside/2,&Red.,1,SOLID);

   %LINE(1,1,2,2, white,1,&linesize); *dummy line to reset;
   %POLY(    &Laxis1+(&axisun*2),     YL+&CISIZE*2, &Red.,SOLID,1);
   %POLYCONT(&Laxis1+(&axisun*2)+&cisize*3, YL,    &Red.       );
   %POLYCONT(&Laxis1+(&axisun*2),     YL-&cisize*2, &Red.        );
   %POLYCONT(&Laxis1+(&axisun*2),     YL+&cisize*2, &Red.        );

   if (OR>2) then do ;
     %BAR(&Laxis1+(&axisun*LCL&conf1),YL-&CIsize,&Laxis1+&axisun*2, YL+&CIsize,&Red.,1,solid);
   end ;
   if (OR<=2) then do ;
     %BAR(&Laxis1+(&axisun*LCL&conf1),YL-&CIsize,&Laxis1+(&axisun*OR)-sqside/2,YL+&CIsize,&Red.,1,solid);
     %BAR(&Laxis1+(&axisun*OR)+sqside/2,YL-&CIsize,&Laxis1+(&axisun*2),YL+&CIsize,&Red.,1,solid);
   end ;

 end ;

%end ; * end linear scale ;

end ; * end if OR>.Z ;


 %IF &NB>1 %THEN %DO;
	 if last.flag then do;
	     L=550;
	  	 H+1*L;
	     YL=&top-H+(L/2);
	     YT=YL+(3/5)*&textsize;

		 ****JRA, May2010, display for heterogeneity and trend test;

    	 %LINE(1,1,2,2,white,1,&linesize);      * DUMMY LINE TO RESET ANNO VARIABLES ;

	%IF &COLOUR.=Y %THEN
		 %LABEL(&LON1-400, YT - 200, 'N missing = '||compress(nmiss),BLUE,0,0,240,&font.,3) ;; 

	%IF &COLOUR.=N %THEN
		 %LABEL(&LON1-400, YT - 200, 'N missing = '||compress(nmiss),BLACK,0,0,240,&font.,3) ;; 


		 if TREND=1 then do;
		      * Trend test ;
			  %LABEL(&Lred-600, YT+200, 'Trend test'||' : '||left(symget('TTRp'||left(flag))),
				                   GREEN,0,0,240,&font.,3) ;

		 end; 
	  end ;

  %END;


  if lastobs then do;
  * TOTAL ;
     * Y COORDINATE ;
     L=max(&titsize,&Tsqside,400)+&space*4;
     H+1.5*L;
     YL=&top-H+(L/2);
     YT1=YL+(2/5)*&titsize;
     YT2=YL+(2/5)*&hsize;
     * SQUARE ;
     point=&Ltit+&Tsqside/2;

     %BAR(point-&Tsqside/2,YL-&Tsqside/2,point+&Tsqside/2,YL+&Tsqside/2,&Red.,1,SOLID);

     * TOTALS ;
     point=&Ltit+&Tsqside+(6*&colspace);
     %LABEL(point, YT1, "Total", BLACK,0,0,&titsize, &FontB.,6);

      %LABEL(&LON1-&colspace*3, YT2, trim(&Tndth1),  BLACK,0,0,&hsize,&FontB.,4);

       %LABEL(&LON1,   YT2, '/', BLACK,0,0,&textsize,&FontB.,5);

      %LABEL(&LON1+&colspace*3, YT2, left(&Tnpat1),  BLACK,0,0,&hsize,&FontB.,6);

      %LABEL(&LON2-&colspace*3, YT2, trim(&Tndth2),  BLACK,0,0,&hsize,&FontB.,4);
      %LABEL(&LON2,   YT2, '/', BLACK,0,0,&textsize,&FontB.,5);
      %LABEL(&LON2+&colspace*3, YT2, left(&Tnpat2),  BLACK,0,0,&hsize,&FontB.,6);


	  L=max(&textsize,sqside,400)+&space;

	 %IF &NB = 1 %THEN %DO;
		 %if &nmiss > 0 %then %do;

			 %IF &COLOUR.=Y %THEN
					 %LABEL(&LON1-400, YT-700, 'N missing = '||compress("&nmiss"),
					                   BLUE,0,0,240,&font.,3) ; ;  
			 %IF &COLOUR.=N %THEN
					 %LABEL(&LON1-400, YT-700, 'N missing = '||compress("&nmiss"),
					                   BLACK,0,0,240,&font.,3) ; ;  

		 %end;

		 if TREND=1 then do;
		      * Trend test ;
			  %LABEL(&Lred-600, YT-400, 'Trend test'||' : '||left(symget('TTRp')),
				                   GREEN,0,0,240,&font.,3) ;

		 end; 


	%END;


%put RTOR &RTOR;

	%IF NOT(%UPCASE(&DISPLAY.)=WORD OR %UPCASE(&DISPLAY.)=POWERPOINT OR %UPCASE(&DISPLAY.)=SCREEN) %THEN %DO;

	 ** HR and CI for big total; **May2010;
	 %LABEL(&LO_E,        YT2, trim(left("&RTOR")), BLACK,0,0,&hsize,&FontB.,6);
     %LABEL(&LvarO_E,        YT2, '('||compress("&RTLCL1")||' ; '||compress("&RTUCL1")||')' , BLACK,0,0,&hsize,&FontB.,6);

	%END;

	%IF %UPCASE(&DISPLAY.)=WORD OR %UPCASE(&DISPLAY.)=POWERPOINT OR %UPCASE(&DISPLAY.)=SCREEN %THEN %DO;

	 ** HR and CI for big total; **May2010;
	 %LABEL(&Lred-1200,        YT2, trim(left("&RTOR")), BLACK,0,0,&hsize,&FontB.,6);
     %LABEL(&Lred-450,        YT2, '('||compress("&RTLCL1")||';'||compress("&RTUCL1")||')' , BLACK,0,0,&hsize,&FontB.,6);

	%END;

     * PCT OF EVENTS ;
     %LABEL(&LON1, YT2-&hsize-&space*2, '('||trim(left(round((&Tndth1/&Tnpat1)*100,0.1)))||' %)',  BLACK,0,0,&hsize,&FontB.,5);
     %LABEL(&LON2, YT2-&hsize-&space*2, '('||trim(left(round((&Tndth2/&Tnpat2)*100,0.1)))||' %)',  BLACK,0,0,&hsize,&FontB.,5);

	 * DIAMOND ;
	    %LINE(1,1,2,2,WHITE,1,&linesize);      * DUMMY LINE TO RESET ANNO VARIABLES ;

		%POLY(&Laxis2+(&axisun*&TOR),  YL+100, &Green.,&diamond.,1);
		 *jan 2002: msolid instead of &diamond, green instead of red;

		%POLYCONT(&Laxis2+(&axisun*&TUCL1), YL,    &Green.         );
		%POLYCONT(&Laxis2+(&axisun*&TOR),  YL-100, &Green.         );
	    %POLYCONT(&Laxis2+(&axisun*&TLCL1), YL,    &Green.         );
		%POLYCONT(&Laxis2+(&axisun*&TOR),  YL+100, &Green.       );
		%POLYCONT(&Laxis2+(&axisun*&TOR),  YL-100, &Green.       );

  * AXIS ;

     * LINE ;
     L=&space*4;
     H+L;
     YL=&top-H+(L/2);

%if (&LOGS=Y) %then %do ;
     %LINE(&Laxis0-(&axisun*1.3863),YL,&Laxis0+(&axisun*1.3863),YL,BLACK,1,&linesize);
     %LINE(&Laxis0-(&axisun*1.3863),YL+80,&Laxis0-(&axisun*1.3863),YL,BLACK,1,&linesize);
     %LINE(&Laxis0-(&axisun*0.69315),  YL+80,&Laxis0-(&axisun*0.69315),  YL,BLACK,1,&linesize);
     %LINE(&Laxis0,      &top ,&Laxis0,      YL,BLACK,1,&linesize);
     %LINE(&Laxis0+(&axisun*0.69315),  YL+80,&Laxis0+(&axisun*0.69315),  YL,BLACK,1,&linesize);
     %LINE(&Laxis0+(&axisun*1.3863),    YL+80,&Laxis0+(&axisun*1.3863),    YL,BLACK,1,&linesize);


%end ; 
%if (&LOGS=N) %then %do ;
     %LINE(&Laxis0-(&axisun),YL,&Laxis0+(&axisun),YL,BLACK,1,&linesize);
     %LINE(&Laxis0-(&axisun),YL+80,&Laxis0-(&axisun),YL,BLACK,1,&linesize);
     %LINE(&Laxis0-(&axisun*0.75),YL+40,&Laxis0-(&axisun*0.75),YL,BLACK,1,&linesize);
     %LINE(&Laxis0-(&axisun*0.5),  YL+80,&Laxis0-(&axisun*0.5),  YL,BLACK,1,&linesize);
     %LINE(&Laxis0-(&axisun*0.25),YL+40,&Laxis0-(&axisun*0.25),YL,BLACK,1,&linesize);
     %LINE(&Laxis0,      &top ,&Laxis0,      YL,BLACK,1,&linesize);
     %LINE(&Laxis0+(&axisun*0.25),YL+40,&Laxis0+(&axisun*0.25),YL,BLACK,1,&linesize);
     %LINE(&Laxis0+(&axisun*0.5),  YL+80,&Laxis0+(&axisun*0.5),  YL,BLACK,1,&linesize);
     %LINE(&Laxis0+(&axisun*0.75),YL+40,&Laxis0+(&axisun*0.75),YL,BLACK,1,&linesize);
     %LINE(&Laxis0+(&axisun),    YL+80,&Laxis0+(&axisun),    YL,BLACK,1,&linesize);



%end ;

**********************************;
* VERTICAL LINE THROUGH TOTAL OR ;
 **********************************;
    %LINE(&Laxis2+(&axisun*&TOR), &top ,&Laxis2+(&axisun*&TOR), YL,&Green.,2,&linesize);
     * LABELS ;
     L=&textsize+&space;
     H+L;
     YT=&top-H+(L/2)+(2/5)*&textsize;

%if (&LOGS=Y) %then %do ;
     %LABEL(&Laxis0-(&axisun*1.3863),YT,'0.25',BLACK,0,0,&hsize,&FontB.,5);
     %LABEL(&Laxis0-(&axisun*0.69315), YT,'0.5',BLACK,0,0,&hsize,&FontB.,5);
     %LABEL(&Laxis0,     YT,'1.0',BLACK,0,0,&hsize,&FontB.,5);
     %LABEL(&Laxis0+(&axisun*0.69315), YT,'2.0',BLACK,0,0,&hsize,&FontB.,5);
     %LABEL(&Laxis0+(&axisun*1.3863),   YT,'4.0',BLACK,0,0,&hsize,&FontB.,5);


%end ; * end LOGS=Y ;

%if (&LOGS=N) %then %do ;
     %LABEL(&Laxis0-(&axisun),YT,'0.0',BLACK,0,0,&hsize,&FontB.,5);
     %LABEL(&Laxis0-(&axisun*0.5), YT,'0.5',BLACK,0,0,&hsize,&FontB.,5);
     %LABEL(&Laxis0,     YT,'1.0',BLACK,0,0,&hsize,&FontB.,5);
     %LABEL(&Laxis0+(&axisun*0.5), YT,'1.5',BLACK,0,0,&hsize,&FontB.,5);
     %LABEL(&Laxis0+(&axisun),   YT,'2.0',BLACK,0,0,&hsize,&FontB.,5);
%end ;

     labY=&top-H-&space*2;

     L=&titsize+&space*3;
     H+L;
     YT=&top-H+(L/2)+(2/5)*&titsize;

     %LABEL(&Laxis0-1375,YT,"&trt1.",BLACK,0,0,&titsize,&FontB.,5); *jan 2002 for rsy ;
     %LABEL(&Laxis0+1375,YT,"&trt2.",BLACK,0,0,&titsize,&FontB.,5); *jan 2002 for rsy ;

     L=&titsize+&space;
     H+L;
     YT=&top-H+(L/2)+(2/5)*&titsize;

	     %LABEL(&Laxis0-1375, YT,"&tit54",BLACK,0,0,&titsize,&FontB.,5);
	     %LABEL(&Laxis0+1375, YT,"&tit54",BLACK,0,0,&titsize,&FontB.,5);

     %LINE(&Laxis0, labY, &Laxis0, YT-(L/2)-(2/5)*&titsize,BLACK,1,&linesize);

     L=&titsize+&space*4;
     H+L;
     YT=&top-H+(L/2)+(2/5)*&titsize+40;
     %LABEL(&Laxis0-200, YT,"&effect",BLACK,0,0,&titsize,&FontB.,5);

     L=&titsize+&space*4;
     H+L;
     YT=&top-H+(L/2)+(2/5)*&titsize+200;


     YT=YT-2000 ;
     YT=YT-500 ;

*** DATE AND WARNING ;
     %if &final.=N %then %do;
        %LABEL(&Lred-1000,1500, "&_DATE_"||"  "||"&_TIME_" ,BLACK,0,0,180,&font.,C) ;    
     %end;
 end;



run;


	** Feb2009, JRA, To remove the repetition of the variable name for each level of this variable;
	data anno; 
		set anno;
		indz=_n_;

    if text="N missing = 0" then text=" ";

	run;

	%sort(data=anno,var=text flag x);

	data anno;
	set anno;
	by text flag x;
			if not(first.flag) and (text=label1) then delete;
	run;

	%sort(data=anno,var=indz);

%let dat=&_DATE_  &_TIME_;

data annoG;
	set annoH anno;
	pct=1-(Yt/&VPos);
	patid=1;

	** JRA, 5March2009, if label variable is empty replace "'" by blank;
	if text="'" then text="";

run;

** VERIFY ALL GRAPH AND DATA ARE INSIDE THE VISIBLE AREA;
** IF NOT, THEN CORRECTION OF X/Y COORDINATES *;
** JRA, 16OCT2006;

%let pas=0;


proc means data=annoG(where=(FUNCTION="LABEL" and text not in ("&dat"))) 
min noprint ;
var Y;
output out=min min=min;
run;


data _null_;
	set min;
	call symput('min',min);
run;

%let mymin=%sysevalf(&min,integer);  

%if &mymin <100 %then %let pas=1000;
%if &mymin <0 %then %let pas=%eval(&mymin*-1+1000);

%if &pas ne 0 %then %do;
data annoG;
	set annoG;
    if text in ("&dat" ) then Y=200;  
    else Y=Y+&pas; 
	if Y>26000 then Y=26000;
run;
%end;
%else %do;
data annoG;
	set annoG;
    if text in ("&dat" ) then Y=&min.-400;  
	if Y>26000 then Y=26000;
run;
%end;

** TO AUTOSIZE GRAPH **;

data myannog;
	set annoG;
	if FUNCTION="LABEL" and text not in ("&dat");
run;

%gmin(data=myannog,var=Y,OUTDATA =pct);
%gmax(data=myannog,var=pct,OUTDATA =pct1);

data _null_;
	merge pct pct1;
	by patid;
	nY=round(nY,1000)-1000;
	call symput('ajust',nY);
	call symput('pct',mpct);
run;

%put pct &pct.;

%sort(data=annoG,var=Yt);

%IF &DISPLAY NE PDF %THEN %DO;
data annoG;
	set annoG;
	Y=Y-&ajust.;
	if Y>0;
run;
%END;


proc datasets library=WORK nolist ;
 delete myannog an annoH anno  summfile1 format min _date_ pct pct1 testMV titt tit trtfmt
 totfile totid tot _tests __format _test _lab _t __litt nmiss  attributes;
quit;



** modified April 05 ;
%keymap(#,bf,?,f1,-,2d ) ;

%IF &GSFName.^=%STR() %THEN %DO;
   filename grafout "&OutFile.";
%END;

goptions reset=all;
options papersize=A4 ;

%IF %UPCASE(&DISPLAY.)=SVG %THEN %DO ;

%let Vpos1=%EVAL(&Vpos.-&ajust);
%let ymax =%SYSEVALF(29.7*&pct.);
%let ypix =%SYSEVALF(5940*&pct.);

goptions reset=all;
goptions gsfname=&GSFName.
         gsfmode=&GSFMode.      device=&Device.
         papersize=A4 noborder
         ftext=&Font.
         rotate=&Rotate.
         gunit=&GUnit.
         hpos=&HPos.
         vpos=&VPos1.
         keymap=mykeymap 
         display 
         xmax=21cm ymax=&ymax. cm hsize=21cm xpixels=4200 ypixels=&ypix. vsize=16 cm ;*horigin=0cm vorigin=0cm ;
%end ;


%ELSE %IF %UPCASE(&DISPLAY.)=PDF  %THEN %DO ;

%let Vpos1=%EVAL(&Vpos.-&ajust);
%let ymax =%SYSEVALF(29.7*&pct.);
%let ypix =%SYSEVALF(5940*&pct.);

goptions reset=all;
goptions gsfname=&GSFName.
         gsfmode=&GSFMode.      device=&Device.
         papersize=A4 noborder
         ftext=&Font.
         rotate=&Rotate.
         gunit=&GUnit.
         hpos=&HPos.
         vpos=&VPos.
         keymap=mykeymap 
         display 
         xmax=21cm ymax=29.7cm hsize=21cm xpixels=4200 ypixels=&ypix. vsize=29.7cm ;*horigin=0cm vorigin=0cm ;
%end ;
%ELSE %IF %UPCASE(&DISPLAY.)=TIFF %THEN %DO ;

%let Vpos1=%EVAL(&Vpos.-&ajust);
%let ymax =%SYSEVALF(29.7*&pct.);
%let ypix =%SYSEVALF(5940*&pct.);

goptions reset=all;
goptions gsfname=&GSFName.
         gsfmode=&GSFMode.      device=&Device.
         papersize=A4 noborder
         ftext=&Font.
         rotate=&Rotate.
         gunit=&GUnit.
         hpos=&HPos.
         vpos=&VPos1.
         keymap=mykeymap 
         display 
         xmax=21cm ymax=&ymax. cm hsize=21cm xpixels=4200 ypixels=&ypix. vsize=&ymax. cm ;*horigin=0cm vorigin=0cm ;
%end ;
%ELSE %IF %UPCASE(&DISPLAY.)=PNG %THEN %DO ;

%let Vpos1=%EVAL(&Vpos.-&ajust);
%let ymax =%SYSEVALF(29.7*&pct.);
%let ypix =%SYSEVALF(5940*&pct.);

goptions reset=all;
goptions gsfname=&GSFName.
         gsfmode=&GSFMode.      device=&Device.
         papersize=A4 noborder
         ftext=&Font.
         rotate=&Rotate.
         gunit=&GUnit.
         hpos=&HPos.
         vpos=&VPos1.
         keymap=mykeymap 
         display 
         xmax=21cm ymax=&ymax. cm hsize=21cm xpixels=4200 ypixels=&ypix. vsize=&ymax. cm ;*horigin=0cm vorigin=0cm ;
%end ;

%ELSE %IF %UPCASE(&DISPLAY.)=JPEG %THEN %DO ;

%let Vpos1=%EVAL(&Vpos.-&ajust);
%let ymax =%SYSEVALF(29.7*&pct.);
%let ypix =%SYSEVALF(5940*&pct.);

goptions reset=all;
goptions gsfname=&GSFName.
         gsfmode=&GSFMode.      device=&Device.
         papersize=A4 noborder
         ftext=&Font.
         rotate=&Rotate.
         gunit=&GUnit.
         hpos=&HPos.
         vpos=&VPos1.
         keymap=mykeymap 
         display 
         xmax=21cm ymax=&ymax. cm hsize=21cm xpixels=4200 ypixels=&ypix. vsize=&ymax. cm ;*horigin=0cm vorigin=0cm ;
%end ;

proc ganno annotate=annoG ; 
run;

goptions reset=all ;


%fin: ;

%SYMDEL NB;

%mend PRED_FACT_FOREST;
