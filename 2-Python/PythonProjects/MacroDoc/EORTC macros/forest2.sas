/************************************************************************************
*
* FOREST2.SAS
* ***********
*
* Part of a suite of programs to perform a meta analysis on time-to-event or binary
* data.  Input data can be in the form of either individual patient data or summary
* (literature) data - or both.
*
* Version date    : 10 July 2002
* Software        : SAS version 8.02
* Original author : Laurence Collette
*
*************************************************************************************
*
* HISTORY
*
*   Based on:
*     FOREST Version 4.0   
*     by Laurence Collette 
*     date: 14/09/96 
*     date: 20/03/2001: change LCL&conf2 to LCL_&conf2
*     based on
*     FOREST Version 3.0 by LC
*     FOREST  Version  2.0
*     By Luc Bijnens
*     based on Meta-Analysis V. 2.0
*     of Guido Hoctin Boes
*     name : d:\sas\meta\frstv2.sas
*     date : 19/01/95
*
*   REVISIONS:
*   ---------
*   May 2002      To ensure unknown (.U) values are handled correctly. KM.
*   July 2002     Added new Display option 'POWERPOINT'
*                 Modified format (font, layout etc.) for display=WORD
*                 Added parameters Final and Colour
*                 (Kate Moncrieff)
*   May 2003      Minor changes (e.g., converting parameter values to upper case).
*                 Updated documentation.   (KM)
*   Sep 2004      Changed the chi-square symbol for test stored in &comment2 to text "chi-square" (LCO)
*   Nov 2004      Changed drivers and options for output graphic for SAS version 9 (LCO)
*   Apr 2005      Changed reference to libraries SASU and GFONT0 to be c:\sas\saswork
*   Oct/Nov 2006  Add of clause where to summfile data in case of use of TOTAL parameter
*                 to avoid to show missing data for TOTAL parameter 
*                 Add of display in PNG device.
*                 Keep labels and format for GROUP and TOTAL parameter 
*                 and display of labels and format in graph
*                 Verification that all graph and data are INSIDE visible area
*                 If not, correction of X/Y coordinates 
*                 The label of time to event variable becomes the title of the plot
*                 (See notes below) 
*                 Update to let autosizing of graphs 
*                 The use of a LABFILE becomes now optionnal (label and format of time-to-event, GROUP and TOTAL 
*                 variable are used by default). Nevertheless, the use of LABFILE is possible if you want to
*                 have your own labels (JRA)
*   March 2007    The format of the two levels of &TESTV (global macro variable) becomes 
*                 the title of each arm for Event/Patient figures, for HR & CI plots
*                 and under the scale.
*                 If no LABFILE is used, it takes format of the two levels of &TESTV variable.
*                 If a LABFILE is used, it takes format of TRT1 and TRT2 variable in this dataset.
*                 If a LABFILE is used but TRT1 and TRT2 variable do not exist in this dataset, 
*                 it takes format of the two levels of &TESTV variable. 
*                 NB: The length of this TESTV format is truncated to 15 characters. (JRA)
*   Oct 2007      Add of a new intermediate variable to avoid problem related to values of 
*                 TOTAL parameter which do not begin by 1. (JRA)     
*   Nov 2007      Add of a message to clearly identify with which alpha CI are computed. (JRA)*
*   Apr 2008      Add of a parameter SUBTIT, so as to let the user add a subtitle without using a LABFILE. (JRA)*
*                 Add of the keep of format for TRIALs. So if no LABFILE is used, it takes format 
*                 of TRIAL variable from SUMMARY dataset.
*   Sept 2008     Add of possibility to draw a reference line to a specific value of HR,
*                 specified in ANYMARGIN parameter (JRA)
*   July 2010     Re-designing of the forest plot so as to avoid problems in display:
*                 -> if Group parameter is used alone or with TOTAL parameter: the label is displayed 
*                 one line above the values, to as to avoid superimposed texts and labels.
*                 Replacement of |1-HR| and %+/- SD by  HR (&conf1. CI) everywhere.
*                 Debuging of problems related to the display of heterogeneity test, ot test for trend.
*                 Add the possibility to perform a full litterature-based meta-analysis without 
*                 individual data also for TTE endpoint (JRA)
*                 Merging of FORESTB2 and FOREST2 macros inside FOREST2.
*                 --> FOREST2 is now handling TTE and binary endpoints and replacing FORESTB2 macro (JRA)
*   Sept 2011     Correction of a bug in case of use of TOTAL parameter:
*                 With the bug, the figures of the first line were equal to the figures of the subtotal. (JRA)
*                 Add of WMF format in DISPLAY parameter
*                 It is now possible to create a file with .wmf extension. (JRA)
*   June 2014     Add of SVG format in DISPLAY parameter
*                 Modification of PDF format to make it vectorial
*                 Cosmetic changes: remove the bug where 'Missing' is displayed wrongly
*                 in case of 2 variables with the same label, display of both labels instead of only one time 
*                 Deletion of WMF format possibility for DISPLAY parameter (not compatible with SAS 9.4). 
*                 WMF format is replaced by this 'modified' PDF format which is a true vectoriel format (JRA)
*  February 2015  Change of the Font for PDF display (from Albany into ARIAL font) so as to be readable into ILLUSTRATOR and INKSCAPE software. (JRA)
*                 Add of I2 (heterogeneity measure or inconsistency measure);
*                 Reformating of the display
*                 Deletion of SVG format which is not used in practice.
*                 Add of TITLE parameter to let the user to to remove the title ;
*
************************************************************************************
* PARAMETERS
*
*    summfile: name of the summary file containing the per study information
*    totfile : name of the file containing the total and subtotals information 
*    testfile: name of the file containing the tests results 
*    labfile : labels datafile (sas file) 
*    grfnr   : name of the output file for the graphical code
*    diamond : msolid if filled mempty if empty
*    display : SCREEN, WORD or POWERPOINT, CGM, WMF, TIFF, PNG or PDF (vectorial) ;
*    group   : the group indicator in a subgroup analysis
*    total   : the indicator of the grouping for the subtotals 
*    cumMA   : Y if you are performing a Cumulative MA 
*    BONF    : Y means that the diamonds will be made for conf2 , the single observations at conf1
*              ^=Y means that everything is made at conf1 
*    blinded : name of the variable whose value 1 indicates blinding 
*    Scale   : LIN or LOG, default=LOG 
*    Final   : Set to Y to remove footnote 'Not for publication...' and date
*    Colour  : Set to Y to obtain graph in colour, N for black and white (default=Y)
*    TITLE   : Set to Y to obtain a title (label of the endpoint variable taken from title variable in SUMMFILE dataset);
             : N to remove the title (default=Y)
*    Subtit  : Subtitle to add (not necessary to write your subtitle between quotes, by default= no subtitle)
*    EndPoint: Set to S if an event means "success", set to F if an event means "failure" 
*              (only with binary endpoint, default=F)
*  Anymargin : value of a reference hazard ratio to be compared to. It draws a line in red to this value,
*              and displays this value in red.
*
*************************************************************************************
*
* USAGE
*
*   Examples of macro calls:
*
*   %FOREST2(summfile=working.bladsum,totfile=working.bladtot,testfile=working.bladtest,labfile=working.labels,
*            grfnr=%str(c:\Temp\ForestPlot.cgm),
*            diamond=,display=WORD,group=,total=,cumMa=N,bonf=N,blinded=);
*
*   %FOREST2(summfile=working.bladsum,totfile=working.bladtot,testfile=working.bladtest,labfile=working.labels,
*            grfnr=%str(c:\Temp\ForestPlot.cgm),
*            diamond=,display=WORD,group=,total=,cumMa=N,bonf=N,blinded=,
*            Scale=LIN,Final=Y,Colour=N);
*
*************************************************************************************
*
* NOTES
*
*    Refer to the notes in the program header of the macro ALLFIXEDB2.SAS for information
*    on importing graphs into Word or PowerPoint.
*
*    Handling of TITLE of Forest Plot:
*
*      If variable tit0 exists in the &labfile dataset, the title remains text entered   
*      in tit0 variable (e.g. if nothing is entered, there will not be any title, 
*      if something is entered, it will remain text entered)
*      If variable tit0 DOES NOT exist in the &labfile dataset, the title of the plot 
*      will become the label of time-to-event or binary variable (from title variable into SUMMARY dataset) 
*
************************************************************************************/

*options mprint;
option dlcreatedir;
libname gfont0  'c:\sas\saswork'; * for the gkeymap;
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


%macro FOREST2 (summfile,totfile, testfile, labfile, 
               grfnr,diamond,display, 
               group, total, 
               cumMA, bonf, blinded, Scale, Final=, EndPoint=,Colour=Y,subtit=,anymargin=, TITLE=Y);

** JRA, Feb 2009, decompositon of vector &GROUP with each element created into &&A&i variable;
** Number of elements of vector &group put into &nb variable;
%global NB;
%if %length(&group.)>0 %then %RCHV(SUITE=&group,COMPTE=A,SEP=" ",N=NB);

** JRA, June2009, not possible to have several variables in GROUP and to use TOTAL parameter in the same time;
%if (%LENGTH(&total.)>0) and (&NB >1)  %then %do;

	%PUT ;%PUT WARNING !! ; %PUT ;
	%PUT IT IS NOT POSSIBLE TO HAVE A VARIABLE IN TOTAL PARAMETER ;
	%PUT AND IN THE SAME TIME SEVERAL VARIABLES IN GROUP PARAMETER;
	%PUT YOU SHOULD HAVE ONLY ONE VARIABLE IN GROUP PARAMETER IF YOU WANT TO USE TOTAL PARAMETER;
	%PUT OR NOTHING IN TOTAL PARAMETER IF YOU WANT TO HAVE SEVERAL VARIABLES IN GROUP PARAMETER;
	%PUT;

%goto fin ;
%end;


** JRA, Feb 2009, put SOLID for the diamond by default;
%if %length(&diamond.)=0 %then %let diamond=MSOLID;

%LET Scale=%UPCASE(&Scale.);
%LET Final=%UPCASE(&Final.);
%LET Colour=%UPCASE(&Colour.);
%LET EndPoint=%UPCASE(&EndPoint.);

** JRA, 26022015, Add of TITLE parameter;
%LET TITLE=%UPCASE(&TITLE.);

*JRA, 27MAY2010, Need distinguish if it is TTE or BIN endpoint, as FOREST and FORESTB2 are merged;
%IF &TYP_ENDPOINT =BIN %THEN %DO;
	%if (%length(%cmpres(&EndPoint))=0) %then %let EndPoint=F ;
	%if (%length(%cmpres(&Scale))=0) %then %let Scale=LIN ;
%END;

%if (%length(%cmpres(&anymargin))=0) %then %let anymargin=NO ;

%if (%length(%cmpres(&total))>0) %then %let subtotal=Y ;
%if (%length(%cmpres(&total))=0) %then %let subtotal=N ;
%if (%length(%cmpres(&group))>0) %then %let subgroup=Y ;
%if (%length(%cmpres(&group))=0) %then %let subgroup=N ;
%if (%length(%cmpres(&final))=0) %then %let final=N ;

%if (%length(%cmpres(&scale))=0 or %upcase(%substr(&scale,1,3))^=LIN) %then %let LOGS=Y ;
%if (%upcase(%substr(&scale,1,3))=LIN) %then %let LOGS=N ;

%IF &LOGS=N %THEN %LET LIMITU=2;
%IF &LOGS=Y %THEN %LET LIMITU=%sysevalf(1.3863);
%IF &LOGS=N %THEN %LET LIMITL=0;
%IF &LOGS=Y %THEN %LET LIMITL=%sysevalf(-1.3863);


* Define local variables for options which vary according to type of output required (value of DISPLAY);
%LOCAL OutFile GSFName GSFMode Device Font Rotate GUnit HPos VPos TextSize TitSize HSize;

%IF %UPCASE(&DISPLAY.)=CGM %THEN %LET DISPLAY=WORD;

%IF %UPCASE(&DISPLAY.)=WORD OR %UPCASE(&DISPLAY.)=POWERPOINT %THEN %DO;
   %LET OutFile = %CMPRES(%scan(&grfnr.,1,.).cgm);
   %LET GSFName = grafout;
   %LET GSFMode = replace;
   %LET Device = CGMOF97P;
   %LET Font = HWCGM001;
   %LET FontB = HWCGM002;
   %LET Rotate = portrait;
   %LET GUnit=pct;
   %LET HPos=19720;
   %LET VPos=27160;
   %LET TextSize = 260;
   %LET TitSize  = 260;
   %LET HSize    = 260;
%END;

%ELSE %IF %UPCASE(&DISPLAY.)=TIFF %THEN %DO;
   %LET OutFile = %CMPRES(%scan(&grfnr.,1,.).tif);
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

%ELSE %IF %UPCASE(&DISPLAY.)=PNG %THEN %DO;
   %LET OutFile = %CMPRES(%scan(&grfnr.,1,.).png);
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

%ELSE %IF %UPCASE(&DISPLAY.)=PDF %THEN %DO;
   %LET OutFile = %CMPRES(%scan(&grfnr.,1,.).pdf);
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

%ELSE %IF %UPCASE(&DISPLAY.)=SCREEN %THEN %DO;
   %LET OutFile = %STR();
   %LET GSFName = %STR();
   %LET GSFMode = %STR();
   %LET Device = %STR();
   %if &TYP_ENDPOINT =TTE %then %do;
	   %LET Font = SWISS;
	   %LET FontB = SWISSB;
   %end;	
   %if &TYP_ENDPOINT =BIN %then %do;
	   %LET Font = SWISSX;
	   %LET FontB = SWISSXB;
	   %LET Rotate = landscape;
   %end;	
   %LET GUnit=cells;
   %LET HPos=19720 ;
   %LET VPos=27160 ;
   %LET TextSize =350 ;
   %LET TitSize  =350 ;
   %LET HSize    =350;
%END;

** Check if there are a lot of levels to display or not;
data _null_;
set &summfile.;
call symput('Nsumfile',compress(_n_));
run;

%put Nsumfile &Nsumfile.;

%let vsize=16;
%if &Nsumfile. >= 15 %then %let vsize=29.7;

* Determine type of font chosen (for key map etc.).;
%LOCAL FontType;
%IF %UPCASE(&font.)=SIMPLEX or %UPCASE(&font.)=DUPLEX      
    or %UPCASE(&font.)=COMPLEX or %UPCASE(&font.)=TRIPLEX %THEN %LET FontType=Hershey;
%ELSE %IF %UPCASE(%SUBSTR(&font.,1,5))=%STR(HWCGM) %THEN %LET FontType=HWCGM;
%ELSE %IF %UPCASE(%SUBSTR(&font.,1,5))=%STR(HWPDF) %THEN %LET FontType=HWPDF;
%ELSE %LET FontType=Other;

* Define macro variables for font colours, depending on whether graph is required 
  in colour or black and white;
%LOCAL Red Green;
%IF &Colour.=Y %THEN %DO;
   %LET Red=RED;
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

**  KEEP FORMATS FOR GROUP AND TOTAL PARAMTERS- JRA, 16OCT2006 **;

proc contents data=&summfile out=format noprint;
run;

data format;
set format;
name=upcase(name);
run;

%local ttestvar fgr ftot ftrt testfmt lfmt foresttitle;


%let ggroup=%UPCASE(&group);
%let ttotal=%UPCASE(&total);
%let ttestvar=%UPCASE(&testv);

%if %length(&group.)>0 %then %put &ggroup;

%IF &NB >1 %THEN %DO;

data format;
	set format;
	if name in ("&ttestvar" %do j=1 %to &NB; %UPCASE("&&a&j.") %end;) ;
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
	if name="&ttestvar" then call symput('ftrt',format);
	drop ind;
run;

%END;

%IF &NB <= 1 %THEN %DO;

data format;
	set format;
	if name in ("&ggroup" "&ttotal" "&ttestvar");
	format=compress(format)||".";
	if format="." then format="8.";
	keep name format ;
	if name="&ggroup" then call symput('fgr',format);
	if name="&ttotal" then call symput('ftot',format);
	if name="&ttestvar" then call symput('ftrt',format);
run;

%if %length(&group.)>0 %then %put &fgr ;
%if %length(&total.)>0 %then %put &ftot;
%put &ftrt;

%END;

*JRA, 17APR2008, save of format name for TRT without a '.' in &testfmt;

	%if %length(&ttestvar.)>0 %then %do;
	* JRA, 20MAY2010, add of this condition in case of full litterature-based meta-analysis without individual data ;

		%let lfmt=%length(&ftrt);
		%let testfmt=%substr(&ftrt,1,%eval(&lfmt-1));

	%end;

data tit;
	set &summfile;
run;
	
**** JRA, 01JUL2007, Add of a condition for full literature-based meta-analysis ;
%IF &data NE NO %THEN %sort(data=tit,var=descending title);

data _null_;
	set tit(obs=1);
	call symput('foresttitle',title);*JRA, 30JUN2010: keep of the title in &title variable;
run;

data _null_;
	set &summfile;*JRA, 17APR2008: keep of the name of TRIAL variable in &NTRIAL;
	where ntrial ne "";
	call symput('ntrial', trim(left(ntrial))); *JRA, 17APR2008; 
run;

data _null_;
	set &summfile;*JRA, 17APR2008: keep of the format of TRIAL variable in &trialfmt;
	where ntrial ne "";
	call symput('trialfmt', trim(left(f&ntrial.))); *JRA, 17APR2008; 
run;

%if &trialfmt. =$ %then %let trialfmt=$30;
%put trialfmt &trialfmt.;

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

data _null_;
	set __format;
	by ind start;
	if first.ind;
	call symput('trt1', trim(left(LABEL))); *JRA, 17APR2008; 
run;

data _null_;
	set __format;
	by ind start;
	if last.ind;
	call symput('trt2', trim(left(LABEL))); *JRA, 17APR2008; 
run;

*** SETS THE DEFAULT VALUES FOR THE GRAPHICAL ELEMENTS ;
%if (%length(%cmpres(&Trt1))=0) %then %let Trt1=%str(Exp.) ;
%if (%length(%cmpres(&Trt2))=0) %then %let Trt2=%str(Stand.) ;
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
%let TE=%str(Treatment effect) ;
%let TEp=%str(p) ;
%let TEz=;        
%let TH=%str(Heterogeneity) ; 
%let THchi=%str(Q) ;
%let THdf=%str(df) ;
%let THp=%str(p) ;

** JRA, 13022015, add of I2 measure;
%let I2lab=%str(I2);

%if %UPCASE(&subtotal)=Y %then %do ;
 %let TI=%str(Heterogeneity among subtotals) ;
 %let TIchi=%str(Chi-square);
 %let TIdf=%str(df) ;
 %let TIp=%str(p) ;
%end ;
%else %do ;
 %let TI=;
 %let TIchi=;
 %let TIdf=;
 %let TIp=;
%end ;
%if %UPCASE(&subgroup)=Y %then %do ;
 %let TTR=%str(Trend);
 %let TTRchi=%str(Chi-square);
 %let TTRp=%str(p);
 %let TTRz=;       
%end ;
%else %do ;
 %let TTR=;
 %let TTRchi=;
 %let TTRp=;
 %let TTRz=;       
%end ;
%let lgroup=;
%let lgroup1=;
%let lgroup2=;
%let lgroup3=;
%let lgroup4=;
%let lgroup5=;

** JRA, 26022015, Add of TITLE parameter;
%if &TITLE=Y %then %let tit0=&foresttitle;
%if &TITLE=N %then %let tit0=;

%let subtit=&Subtit.;  * JRA, apr 2008, if no labfile is used, 
it is still possible to add a subtitle with SUBTIT parameter; 
%let tit11=;
%let tit121=%str(Events);
%let tit122=%str(Patients);
%let tit13=%str(Statistics) ;

%if &TYP_ENDPOINT=TTE %then %let tit14=%str(HR & CI*) ;; * LCO;
%if &TYP_ENDPOINT=BIN %then %let tit14=%str(OR & CI*) ;; * LCO;

%let tit21=;
%let tit231=%str((O-E)) ;
%let tit232=%str(Var.) ;

** JRA, May2010, replacement of |1-HR| % +/- SD by HR (&conf1. % CI); 
%let tit25=%str(HR (&conf1.% CI)) ;

%if &TYP_ENDPOINT=TTE %then %let tit25=%str(HR (&conf1.% CI)) ;; * LCO;
%if &TYP_ENDPOINT=BIN %then %let tit25=%str(OR (&conf1.% CI)) ;; * LCO;


%let tit54=%str(better) ;

** y coord of the line separating titles and plot ;
** warning: counted from bottom to top! ;

** JRA, 26022015, Add of TITLE parameter;
%if &TITLE=Y %then %let top      = 22500;
%if &TITLE=N %then %let top      = 25500;

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

* LINES *;

** JRA, 11JUNE2014, cosmetic changes;
%IF %UPCASE(&DISPLAY.)=PDF %THEN %let linesize = 3;

** x start of line separating titles from plot ;
%let Ltit    = 1000;

** Ldescr middle of code and study descr ;
%let ldescr  = 1500;
*** positions for the text ;
%let LON1    = 5000;
%let LON2    = 7100;
%let LO_E    = 8800;
%let LvarO_E = 9900;
** text when blinded or no data ;
%let Lblind=7100 ;

*** Laxis0 x coord. for the vertical solid line ;
%if (&LOGS=N) %then %let Laxis0  = 10400+&axisun;
%if (&LOGS=Y) %then %let Laxis0  = 10400+1.3863*&axisun ;
*** Laxis1=starting point of the scale, origin for the drawings in linear scale ;
%let Laxis1=10400 ;
*** Laxis2=Laxis0 when Log scale and Laxis1 when linear scale ;
*** used for the diamonds ;
%if (&LOGS=Y) %then %let Laxis2=&Laxis0 ;
%if (&LOGS=N) %then %let Laxis2=&Laxis1 ;

*** middle for the reduction labels ;
%let Lred    = 17300 ;

* READS IN THE LABELS DATA FILE ;

%if (%length(%cmpres(&labfile))>0) %then %do ;
 data labels ;
 set &labfile ;
 if (upcase(substr(labname,1,4))='DATE') then delete ;
 call symput(Labname, left(trim(labvalue))) ;
 run ;
%end;

************************************************************************;
************************************************************************;
%IF &NB<= 1 %THEN %DO; *USEFUL ONLY IF GROUP DOESN T CONTAIN SEVERAL VARIABLES
************************************************************************;

* READS IN THE TEST RESULTS ;

data _tests ;
 set &testfile;
 call symput('TE'||compress(TE), TEv) ;
 call symput('TTR'||compress(TTR), TTRv) ;
 call symput('TI'||compress(TI), TIv) ;
 call symput('TH'||compress(TH), THv) ;
 call symput('I2'||compress(I2), I2v) ;** JRA, 13022015, add of I2 measure;

 %if &NB=1 %then %do;** JRA, MAy2010, add TREND macro variable to flag if the test for trend is done or not;
 	call symput('TREND', trend) ;
 %end;
 run ;


* modifies the labels to incorporate the test results ;
%let effect=;
%let hetero1=;
%let hetero2=;
%let inter1=;
%let inter2=;
%let trend1=;
%let trend2=;
%let comment1=;
%let comment2=;
%let comment3=;
%let comment4=;
%let comment5=;
%let comment6=;

* Treatment effect ;
%if (%length(&TE)^=0 and %upcase(&cumMA)^=Y) %then %do ;
  %let effect=%left(%trim(&TE)): %left(%trim(&TEp))&TE3;
  %end ;

%if (%length(&TE)^=0 and %length(&TEz)^=0 and %upcase(&cumMA)^=Y) %then %do;
   %let effect=%left(%trim(&TE)):
%left(%trim(&TEz))=%left(%trim(&TE4)), %left(%trim(&TEp))&TE3;
   %end ;


**JRA, May2010, remove of &effect comment in the case of a non inferiority trial (ANYMARGIN option);
%if not(&ANYMARGIN=NO) %then %let effect=;

* Heterogeneity ;
%if (%length(&TH1)^=0 and &TH1 ne 0) %then %do;
         %let hetero1=%left(%trim(&TH)) ;
		 %put THchi &THchi. TH1 &TH1.;
          %if (%length(&THchi)^=0) %then %let
hetero2=%left(%trim(&THchi))=%left(%trim(&TH1)) (%left(%trim(&THdf))=%left(%trim(&TH2)))
%left(%trim(&THp))&TH3;
          %else %let hetero1=%left(%trim(&hetero1)): %left(%trim(&THp))&TH3 ;
          %end ;

* Interaction ;
%if (%length(&TI1)^=0) %then %do;
          %let inter1=%left(%trim(&TI)) ;
          %if (%length(&TIchi)^=0) %then %let
inter2=/*%left(%trim(&TIchi))*/%left(%trim(&TI1)) (%left(%trim(&TIdf))=%left(%trim(&TI2))) 
%left(%trim(&TIp))&TI3 ;
          %else %let inter1=%left(%trim(&inter1)): %left(%trim(&TIp))&TI3 ;
          %end ;

%put &inter2;

* Test for trend ;

%if (%length(&TTR1)^=0) %then %do ;
        %let trend1=%left(%trim(&TTR)) ;
        %if (%length(&TTRchi)^=0) %then %let
 trend2=/*%left(%trim(&TTRchi))=*/%left(%trim(&TTR1)) (df=1) %left(%trim(&TTRp))&TTR3 ;
        %if (%length(&TTRz)^=0) %then %let trend2=%left(%trim(&TTRz))=%left(%trim(&TTR4): %left(%trim(&TTRp))&TTR3) ;
        %if (%length(&TTRchi)=0 and %length(&TTRz)=0) %then %let trend1=%left(%trim(&trend1)): %left(%trim(&TTRp))&TTR3 ;
        %end ;

%if &NB=1 %then %do;
%if &TREND=0 %then %do;**JRA, 4MAY2010, in case we have only ONE variable into GROUP parameter, but it is a binary
variable, in this case TREND=0, so the test for trend is not needed;
	%let trend1=;
	%let trend2=;
%end;
%end;


%if (%length(&hetero1)^=0) %then %do ;
      %let comment1=&hetero1 ;
*      %if (%length(&hetero2)^=0) %then %let comment2=&hetero2 %str(,) &I2 ;** JRA, 13022015, add of I2 measure;
       %if (%length(&hetero2)^=0) %then %let comment2=&hetero2  ; 

	  %put &hetero1 &hetero2;

      %end ;

%if (%length(&inter1)^=0) %then %do ;
         %if (%length(&comment1)=0) %then  %do ;
            %let comment1=&inter1 ;
            %if (%length(&inter2)^=0) %then %let comment2=&inter2 ;
            %end ;
        %else %do ;
         %if (%length(&comment2)=0) %then %do ;
             %let comment2=&inter1 ;
             %if (%length(&inter2)^=0) %then %let comment3=&inter2 ;
             %end ;
         %else %do ;
            %let comment3=&inter1 ;
            %if (%length(&inter2)^=0) %then %let comment4=&inter2 ;
            %end ;
        %end ;
%end ;
%if (%length(&trend1)^=0) %then %do ;
         %if (%length(&comment1)=0) %then  %do ;
            %let comment1=&trend1 ;
            %if (%length(&trend2)^=0) %then %let comment2=&trend2 ;
            %end ;
        %else %do ;
            %if (%length(&comment2)=0) %then %do ;
             %let comment2=&trend1 ;
             %if (%length(&trend2)^=0) %then %let comment3=&trend2 ;
             %end ;
            %else %do ;
                 %if (%length(&comment3)=0) %then %do ;
                 %let comment3=&trend1 ;
                  %if (%length(&trend2)^=0) %then %let comment4=&trend2 ;
                  %end ;
                  %else %do ;
                       %if (%length(&comment4)=0) %then %do ;
                       %let comment4=&trend1 ;
                       %if (%length(&trend2)^=0) %then %let comment5=&trend2 ;
                       %end ;
                       %else %do ;
                       %let comment5=&trend1 ;
                       %if (%length(&trend2)^=0) %then %let comment6=&trend2 ;
                      %end ;
                  %end ;
              %end  ;
          %end ;
%end ;
%put &comment1;
%put &comment2;
%put &comment3;
%put &comment4;
%put &comment5;

%END; * END IF NB<= 1 THEN DO;

%LOCAL LeftShift RightShift;
%IF &EndPoint.=F %THEN %DO;
	%let LeftShift=(%length(%left(%trim(&trt2)))+2)*150 ;
	%IF &FontType. = HWCGM %THEN %let RightShift=(%length(%left(%trim(&trt1)))+5)*150 ;
	%ELSE %let RightShift=(%length(%left(%trim(&trt1)))+2)*150 ;
%END;
%ELSE %DO;
	%let LeftShift=(%length(%left(%trim(&trt1)))+2)*150 ;
	%IF &FontType. = HWCGM %THEN %let RightShift=(%length(%left(%trim(&trt2)))+5)*150 ;
	%ELSE %let RightShift=(%length(%left(%trim(&trt2)))+2)*150 ;
%END;

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
  length text $ 100;

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
  %LABEL(&LON1,               YT3, "&trt1",                BLACK,0,0,&hsize,&font.,5);
  %LABEL(&LON2,               YT3, "&trt2",                BLACK,0,0,&hsize,&font.,5);
  %LABEL(&LvarO_E-250,        YT2, "&tit13",              BLACK,0,0,&hsize,&font.,4);
  %LABEL(&LO_E,               YT3, "  &tit231",                 BLACK,0,0,&hsize,&font.,4);
  %LABEL(&LvarO_E,            YT3, "&tit232",                    BLACK,0,0,&hsize,&font.,4);
  %LABEL(&Laxis0,             YT2, "&tit14",                 BLACK,0,0,&hsize,&font.,5);
  %LABEL(&Laxis0,             YT3, ":",   BLACK,0,0,&hsize,&font.,5);

  pos1=&Laxis0-&LeftShift ;
  pos2=&Laxis0+&RightShift ;

  %IF &EndPoint.=F %THEN %DO;**JRA, 04JUN10, change of position for treatment format; 
     %LABEL(&Laxis0-400,YT3,"(&trt2.",BLACK,0,0,&hsize,&font.,4); * Binary;
     %LABEL(&Laxis0+2300,YT3,"&trt1.)",BLACK,0,0,&hsize,&font.,4);
  %END;
  %ELSE %DO;
     %LABEL(&Laxis0-400,YT3,"(&trt1.",BLACK,0,0,&hsize,&font.,4); *TTE;
     %LABEL(&Laxis0+2300,YT3,"&trt2.)",BLACK,0,0,&hsize,&font.,4);
  %END;

	%if (%upcase(&CUMMA)^=Y) %then %do ;
	**JRA, May2010, replacement of |1-HR| by HR (&conf1.% CI);
	%IF NOT(%UPCASE(&DISPLAY.)=WORD OR %UPCASE(&DISPLAY.)=POWERPOINT OR %UPCASE(&DISPLAY.)=SCREEN) %THEN %DO;
		%LABEL(&Lred+400,               YT3, "&tit25",                  BLACK,0,0,&hsize,&font.,5);
	%END;

	%IF %UPCASE(&DISPLAY.)=WORD OR %UPCASE(&DISPLAY.)=POWERPOINT OR %UPCASE(&DISPLAY.)=SCREEN %THEN %DO;
		%LABEL(&Lred,               YT3, "&tit25",                  BLACK,0,0,&hsize,&font.,5);
	%END;

	%end ;
		
	%if (%upcase(&Cumma)=Y) %then %do ;
	  %LABEL(&Lred+400,               YT2, "Treatment effect",                  BLACK,0,0,&hsize,&font.,5);
	%end ;
	
  * TOPLINE ;
  %LINE(&Ltit,&top,&Laxis0+6000,&top,BLACK,1,&linesize);

run;

* TOTALS AND SUBTOTALS READ IN THE TOTALS DATA FILE ;

%if &TYP_ENDPOINT.=BIN %then %do;

	%if (%upcase(&subtotal)=Y) %then %do ;
	data totfile; set &totfile;z=1;run;
	%sort(data=totfile,var=z &total);

	 data totid;	
	 	set totfile;
		by z &total;
		if first.z then ind=0;
		else ind+1;	
	 run;

	%end;
%end;


DATA _NULL_ ;

%if &TYP_ENDPOINT.=BIN %then %do;

	%if (%upcase(&subtotal)=Y) %then %do;
	   set totid end=eof;;
	%end;
	%else %do;
	   set &totfile end=eof;;
	%end;

%end;

%if &TYP_ENDPOINT.=TTE %then set &totfile ;;

 %if (%upcase(&subtotal)=Y) %then %do ;
** JRA, June2010, grind computation;
 grind=&total.;
%end;

 ** JRA, May2010, computation of the HR and CI roundings;
 ORround=round(OR,0.01);
 ORroundt=trim(left(ORround||""));
 %numtochar(var=ORroundt);

 LCL&conf1.r=roundz(LCL&conf1.,0.01);
 UCL&conf1.r=roundz(UCL&conf1.,0.01);
 LCL&conf2.r=roundz(LCL_&conf2.,0.01);
 UCL&conf2.r=roundz(UCL_&conf2.,0.01);

 LCL&conf1.t=trim(left(LCL&conf1.r||""));
 UCL&conf1.t=trim(left(UCL&conf1.r||""));
 LCL&conf2.t=trim(left(LCL&conf2.r||""));
 UCL&conf2.t=trim(left(UCL&conf2.r||""));
 %numtochar(var= LCL&conf1.t);
 %numtochar(var= UCL&conf1.t);
 %numtochar(var= LCL&conf2.t);
 %numtochar(var= UCL&conf2.t);

 if (_N_=1) then do ;
   call symput('Tndth1', ndeath1) ;
   call symput('Tnpat1', npat1) ;
   call symput('Tndth2', ndeath2) ;
   call symput('Tnpat2', npat2) ;
   call symput('TO_E', round(O_E, .1)) ;
   call symput('TvarO_E', round(varO_E, .1)) ;
%if (&LOGS=Y) %then %do ;
   call symput('TOR',log(OR)) ;
   call symput('TLCL1',log(LCL&conf1)) ;
   call symput('TUCL1',log(UCL&conf1)) ;
   call symput('TLCL2',log(LCL_&conf2)) ;
   call symput('TUCL2',log(UCL_&conf2)) ;
   call symput('RTOR',ORroundtout) ;
   call symput('RTLCL1',LCL&conf1.tout) ;
   call symput('RTUCL1',UCL&conf1.tout) ;
   call symput('RTLCL2',LCL&conf2.tout) ;
   call symput('RTUCL2',UCL&conf2.tout) ;
%end ;
%if (&LOGS=N) %then %do ;
   call symput('TOR',OR) ;
   call symput('TLCL1',LCL&conf1) ;
   call symput('TUCL1',UCL&conf1) ;
   call symput('TLCL2',LCL_&conf2) ;
   call symput('TUCL2',UCL_&conf2) ;
   call symput('RTOR',ORroundtout) ;
   call symput('RTLCL1',LCL&conf1.tout) ;
   call symput('RTUCL1',UCL&conf1.tout) ;
   call symput('RTLCL2',LCL&conf2.tout) ;
   call symput('RTUCL2',UCL&conf2.tout) ;
%end ;
   call symput('TR', cent1_OR) ;
   call symput('TRSD', SD);
   call symput('TRT', change) ;
   end ;
else do ;

  %if &NB <=1 %then %do; *IF GROUP CONTAINS ONLY ONE OR ZERO VARIABLE;	
   %if (%upcase(&subtotal)=Y) %then %do ;

	   call symput('DT1'||left(grind), ndeath1) ;
	   call symput('DT2'||left(grind), ndeath2) ;
	   call symput('PT1'||left(grind), npat1) ;
	   call symput('PT2'||left(grind), npat2) ;
	   call symput('OE'||left(grind), round(O_E,.1)) ;
	   call symput('VOE'||left(grind), round(varO_E, .1)) ;

	%if (&LOGS=Y) %then %do ;
	   call symput('OR'||left(grind), log(OR)) ;
	   call symput('L1'||left(grind),log(LCL&conf1)) ;
	   call symput('U1'||left(grind) ,log(UCL&conf1)) ;
	   call symput('L2'||left(grind),log(LCL_&conf2)) ;
	   call symput('U2'||left(grind),log(UCL_&conf2)) ;
			%if &TYP_ENDPOINT=BIN %then %do;
				   call symput('LL1'||left(ind),log(LCL&conf1)) ;
				   call symput('UU1'||left(ind),log(UCL&conf1)) ;
				   call symput('LL2'||left(ind),log(LCL_&conf2)) ;
				   call symput('UU2'||left(ind),log(UCL_&conf2)) ;
			%end ;


	%end ;
	%if (&LOGS=N) %then %do ;
	   call symput('OR'||left(grind), OR) ;
	   call symput('L1'||left(grind), LCL&conf1) ;
	   call symput('U1'||left(grind), UCL&conf1) ;
	   call symput('L2'||left(grind), LCL_&conf2) ;
	   call symput('U2'||left(grind), UCL_&conf2) ;
			%if &TYP_ENDPOINT=BIN %then %do;
				   call symput('LL1'||left(ind), LCL&conf1) ;
				   call symput('UU1'||left(ind), UCL&conf1) ;
				   call symput('LL2'||left(ind), LCL_&conf2) ;
				   call symput('UU2'||left(ind), UCL_&conf2) ;
			%end ;

	%end ;
	   call symput('SR'||left(grind), cent1_OR) ;
	   call symput('SD'||left(grind), SD) ;
	   call symput('SRT'||left(grind), change) ;
	   call symput('HCHI'||left(grind), compress(HTchi)) ;
	   call symput('HDF'||left(grind), compress(HTdf)) ;
	   call symput('HP'||left(grind), compress(HTp));

	%end;
	%end;

   end ;

  %if &NB <=1 %then %do; *IF GROUP CONTAINS ONLY ONE OR ZERO VARIABLE;	
	if eof then call symput('nn', compress(ind));
	if (_N_=2) then	call symput('start', compress(ind));

		 %if &TYP_ENDPOINT=BIN %then %do;
		 if (_N_>=2) then  do;* JRA, June 2010, Handling of GRIND and IND index and creation a correspondance between them;
			call symput('ind'||compress(_N_-1), compress(ind));
			call symput('grind'||compress(_N_-1), compress(grind));
		 end;
		 %end;

  %end;

RUN ;

%PUT RTOR &RTOR ;

%IF &NB >1 %THEN %DO; *IF GROUP CONTAINS SEVERAL VARIABLES;

	 *** HETEREOGENEITY & TREND TESTS **;

	 data _null_ ;
		 set &testfile ;
		 if TH=1 then do;
		    call symput('HCHI'||left(flag), compress(THv)) ;
		    call symput('FLAGTR'||left(flag), compress(trend)) ;
		    call symput('TE1', compress(TEv)) ;
		 end;

		 if TH=2 then do;
		    call symput('HDF'||left(flag), compress(THv)) ;
		 end;

		 if TH=3 then do;
		    call symput('HP'||left(flag), compress(THv));
		    call symput('TE3', compress(TEv)) ;
		 end;

		 if TH=4 then do;
		    call symput('TE4', compress(THv)) ;
		 end;

		 if TTR=1 then do;
		    call symput('TTRchi'||left(flag), compress(TTRv)) ;
		 end;

		 if TTR=2 then do;
		    call symput('TTRf'||left(flag), compress(TTRv)) ;
		 end;

		 if TTR=3 then do;
		    call symput('TTRp'||left(flag), compress(TTRv));
		 end;

		 if I2=1 then do;
		    call symput('I2'||left(flag), I2v);** JRA, 13022015, add of I2 measure;
		 end;

	run;

	%let effect=;
	* Treatment effect ;
	%if (%length(&TE)^=0 and %upcase(&cumMA)^=Y) %then %do ;
	  %let effect=%left(%trim(&TE)): %left(%trim(&TEp))&TE3;
	  %end ;

	%if (%length(&TE)^=0 and %length(&TEz)^=0 and %upcase(&cumMA)^=Y) %then %do;
	   %let effect=%left(%trim(&TE)):
	%left(%trim(&TEz))=%left(%trim(&TE4)), %left(%trim(&TEp))&TE3;
	   %end ;

	%put &TE1 &TE3 &TE4 &TTRchi1 &TTRchi2 &TTRf1 &TTRf2 &TTRp1 &TTRp2 &FLAGTR1 &FLAGTR2 &effect &I21 &I22;

%END;

 ** Add of clause where - JRA, 16OCT2006 **;
data summfile1 ;
 set &summfile ;
 %if &NB >1 %then %do;*IF GROUP CONTAINS SEVERAL VARIABLES;

	%if &TYP_ENDPOINT.=TTE %then where flag ne .;;
	%if &TYP_ENDPOINT.=BIN %then where flag ne 99;;

 %end;
 %if &subtotal=Y %then where &total ne .;; 

**JRA, May 2010, rounding and formating of HR and CI;
 ORround=round(OR,0.01);
 LCL&conf2.r=roundz(LCL_&conf2.,0.01);
 UCL&conf2.r=roundz(UCL_&conf2.,0.01);
 LCL&conf1.r=roundz(LCL&conf1.,0.01);
 UCL&conf1.r=roundz(UCL&conf1.,0.01);

 ORroundt=trim(left(ORround||""));
 LCL&conf2.t=trim(left(LCL&conf2.r||""));
 UCL&conf2.t=trim(left(UCL&conf2.r||""));
 LCL&conf1.t=trim(left(LCL&conf1.r||""));
 UCL&conf1.t=trim(left(UCL&conf1.r||""));

 %numtochar(var=ORroundt);
 %numtochar(var= LCL&conf2.t);
 %numtochar(var= UCL&conf2.t);
 %numtochar(var= LCL&conf1.t);
 %numtochar(var= UCL&conf1.t);

 if ORround=1 then ORroundtout="1.00";
 if  LCL&conf2.r=1 then LCL&conf2.tout="1.00";
 if  LCL&conf1.r=1 then LCL&conf1.tout="1.00";
 if  UCL&conf2.r=1 then UCL&conf2.tout="1.00";
 if  UCL&conf1.r=1 then UCL&conf1.tout="1.00";

 format LCL&conf2.r UCL&conf2.r LCL&conf1.r UCL&conf1.r ORround 5.2;
 drop ORroundt LCL&conf2.t UCL&conf2.t LCL&conf1.t UCL&conf1.t;

run;

%if &NB >1 %THEN %DO;*IF GROUP CONTAINS SEVERAL VARIABLES;

	%sort(data=summfile1,var=flag);
	data _test;
		set &testfile(where=(TE=1));
		keep flag trend;
	run;
	%sort(data=_test,var=flag);

	data summfile1 ;
		merge summfile1 _test;
		by flag;
	run;

%END;

%if (%length(%cmpres(&blinded))=0) %then %do ;

data summfile1 ;
  set summfile1 ;
 %let blinded=_blind ;
 _blind=0 ;
run ;

%end ;

%if (%upcase(&subtotal)=Y) %then %do ;
  %sort(data=summfile1,var=&total);
  data totals; set &totfile;run;
  %sort(data=totals,var=&total);

  %if &TYP_ENDPOINT.=BIN %then %do;
	data totid;
	set totid;
	rename
	LCL_&conf2.=_LCL_&conf2.
	UCL_&conf2.=_UCL_&conf2.
	LCL&conf1.=_LCL&conf1.
	UCL&conf1.=_UCL&conf1. 
	OR=_OR 
	;
	run;

	%sort(data= totid,var=&total);

	data summfile1;
		merge summfile1 totid(keep= &total ind
		_LCL_&conf2.
		_UCL_&conf2.
		_LCL&conf1.
		_UCL&conf1. 
		_OR 
		);
		by &total;
		where &total ne .;
    run;
   %end;

   %if &TYP_ENDPOINT.=TTE %then %do;

   ** JRA, 18MAR2011, correct bug with TTE data in case of use of TOTAL parameter ;
   ** With the bug, the figures of the first line were equal to the figures of the subtotal;

   data tot;
   set totals
 	(rename=( LCL_&conf2.=_LCL_&conf2. UCL_&conf2.=_UCL_&conf2. LCL&conf1.=_LCL&conf1. UCL&conf1.=_UCL&conf1. 
		 OR=_OR ) where=( &total ne .) drop=npat: ndeath: O_E VarO_E);
	run;

   ** JRA, 13022015, add of I2;
	data _null_;
	set tot;
	call symput('I2TOT'||compress(&TOTAL),trim(left(I2txt)));
	run;

	  data summfile1;
		merge summfile1(in=a) tot;
	 	by &total;
		if a;
	  run;

   %end;


   %if &TYP_ENDPOINT.=BIN %then %do;

      ** JRA, 13022015, add of I2;
	data _null_;
	set totals;
	call symput('I2TOT'||compress(&TOTAL),trim(left(I2txt)));
	run;

   %end;

   data summfile1;
     set summfile1;
	_ORround=round(_OR,0.01);
	_LCL&conf2.r=round(_LCL_&conf2.,0.01);
	_UCL&conf2.r=round(_UCL_&conf2.,0.01);
	_LCL&conf1.r=round(_LCL&conf1.,0.01);
	_UCL&conf1.r=round(_UCL&conf1.,0.01);

	_ORroundt=trim(left(_ORround||""));
	_LCL&conf2.t=trim(left(_LCL&conf2.r||""));
	_UCL&conf2.t=trim(left(_UCL&conf2.r||""));
	_LCL&conf1.t=trim(left(_LCL&conf1.r||""));
	_UCL&conf1.t=trim(left(_UCL&conf1.r||""));

	%numtochar(var=_ORroundt);
	%numtochar(var= _LCL&conf2.t);
	%numtochar(var= _UCL&conf2.t);
	%numtochar(var= _LCL&conf1.t);
	%numtochar(var= _UCL&conf1.t);

	 if _ORround=1 then _ORroundtout="1.00";
	 if _LCL&conf2.r=1 then _LCL&conf2.tout="1.00";
	 if _LCL&conf1.r=1 then _LCL&conf1.tout="1.00";
	 if _UCL&conf2.r=1 then _UCL&conf2.tout="1.00";
	 if _UCL&conf1.r=1 then _UCL&conf1.tout="1.00";

 	drop _ORroundt _LCL&conf2.t _UCL&conf2.t _LCL&conf1.t _UCL&conf1.t; 

	format _LCL&conf2.r _UCL&conf2.r _LCL&conf1.r _UCL&conf1.r _ORround 5.2;
  run;
   

%end ;

%let FLAG_MV=;
%if (%length(%cmpres(&labfile))>0) %then %do ;

	** TO FLAG IF &LABFILE CONTAINS C_ or D_ OR LTOTAL OR LGROUP macro variables  **; 
	data testMV;
	 	set &labfile end=eof;
		labname=upcase(labname);
		if substr(labname,1,1) in ("C" "D") 
		or upcase(substr(labname,1,3)) ="LGR"  or upcase(substr(labname,1,3)) ="LTO"
	    then n+1;
		if eof;;
		call symput('FLAG_MV',compress(n));
	run;
	%put &FLAG_MV;

	data _lab;
	set &labfile;
		y=length(labvalue); 
	run;
	%sort(data=_lab,var=y);

	data _null_;
	set _lab;
	call symput('_ltlab',y);
	run;

%end;

data an;
	set summfile1 ;

	%if &NB <= 1 %then %do;

		flag=1;	

	%end;

   * TO SAVE &GROUP FORMATS - JRA, 16OCT2006**;	
   %if %upcase(&subgroup)=Y %then %do;

   		%if &NB > 1 %then %do;*IF GROUP CONTAINS SEVERAL VARIABLES;
		   %do J=1 %to &NB;
				group&j.=put(&&a&j,&&fgr&J..);
			    label1&j.=vlabel(&&a&j.);
			    label1&j.=compbl(label1&j.);
				_lt&j.=length(label1&j.);

				** JRA, 11JUNE2014, cosmetic changes;

				if upcase(group&j.)="MISSING" then group&j.="";
				if upcase(group&j.)="UNKNOWN" then group&j.="";


		   %end;

		   group= trim(left(group1)) %DO J=2 %TO &NB; ||trim(left(group&j.)) %END;;

		   group=trim(left(tranwrd(group, ".", "")));

		   %if &NB= 1 %then %do;lt1=_lt1;%end;
		   %else %do;
		   lt1=max(_lt1 %do J=2 %to &NB; , _lt&j. %end;);;
		   %end;

		   %do J=1 %to &NB;
				if &&a&j ne . then label1=label1&j.;
		   %end;

		   %do J=1 %to &NB;
				drop _lt&j. group&j. label1&j.;
		   %end;

		%end;

   		%if &NB <= 1 %then %do;*IF GROUP CONTAINS ONLY ONE OR ZERO VARIABLE;

			group=put(&group,&fgr.);
		    label1=vlabel(&group.);
		    label1=compbl(label1);
			lt1=length(label1);
		%end;

   %end;;

   %if %upcase(&subtotal)=Y %then %do;
		total=put(&total,&ftot.);
	    label2=vlabel(&total.);
	    label2=compbl(label2)||": ";
		lt2=length(label2);
   %end;;

 _TRIAL=put(_Num,&trialfmt..);*JRA, 17APR2008: use of the format of TRIAL variable from &trialfmt;

	%IF (%length(&labfile.)>0) and &FLAG_MV >0 %then %do ;

 		%if &NB <= 1 %then %do;*IF GROUP CONTAINS ONLY ONE OR ZERO VARIABLE;

			   Tcode=symget('c'||left(_NUM)) ;
			   Tdescr=symget('d'||left(_NUM)) ;

			   %if (%length(&group.)>0) %then %do;

					Tgroup=symget('lgroup'||left(&group.));
					lgroup=symget('lgroup');
					lt1=length(lgroup);

			   %end;

			   %if (%length(%cmpres(&total.))>0) %then %do;
			   		Ttotal=symget('ltotal'||left(&total.));
					ltotal=symget('ltotal');
					lt2=length(ltotal);
			   %end;

		%end;

 		%if &NB > 1 %then %do;*IF GROUP CONTAINS SEVERAL VARIABLES;

			   %if (%length(&group.)>0) %then %do;
					Tgroup=symget( 'lgroup'  ||  compress(flag)  ||  left(_Num)   );
					lgroup=symget('lgroup'||compress(flag));
					lt1=length(lgroup);
			   %end;

		%end;

	%END;
run;

%if &TYP_ENDPOINT.=BIN %then %do;

	%if %upcase(&subtotal)=Y %then %do;

	data tot;
		set &totfile;
		L1=LCL&conf1;
		L2=LCL_&conf2;
		U1=UCL&conf1;
		U2=UCL_&conf2;
		ORt=OR;
		if &total ne .;
		keep &total L1 L2 U1 U2 ORt;
	run;
	%sort(data=tot,var=&total);
	%sort(data=an,var=&total);

	data an;
		merge an tot;
		by &total;
	run;

	%end;

%end;

data _null_;
	set an;
	call symput('lt1',lt1);
	call symput('lt2',lt2);
run;

%IF (%length(&labfile.)>0) %then %do;
	data _t;
	lt1=&lt1;_ltlab=&_ltlab;
	_lt1=max(lt1,_ltlab);
	run;

	data _null_;
	set _t;
	call symput('lt1',_lt1);
	run;

%end;

%put lt1 &lt1;

%if (%upcase(&subtotal)=Y) %then %do ;
	%sort(data=an,var= &total) ;
%end ;

%if (%upcase(&subtotal)=Y and %upcase(&subgroup)=Y) %then %do ;
	%sort(data=an,var= &total &group) ;
%end ;

%put &lt1 &lt2;

%if &NB >1 %then %sort(data=an,var=flag); *IF GROUP CONTAINS SEVERAL VARIABLES;

data anno;
  set an end=lastobs;

**JRA, May2010, concatenation of the label of TOTAL parameter and tha value to avoid spacing;
 llabel2=trim(left(label2))||" "||total;
  %if %length(&labfile.)>0 %then %do;
	  llabel2=trim(left(ltotal))||" "||ttotal;
  %end;

%if (%upcase(&subtotal)=Y and %upcase(&subgroup)=N) %then %do ;
  by &total ;

   grind=&total;

%end ;
%if (%upcase(&subtotal)=Y and %upcase(&subgroup)=Y) %then %do ;
  by &total &group;
   
  grind=&total;

%end ;

  %if &NB >1 %then %do; by flag ; %end; *IF GROUP CONTAINS SEVERAL VARIABLES;

  * LENGTH OF ANNOTATE VARIABLES;
  %DCLANNO;
  length text $ 100;

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



%if (%upcase(&subtotal)=Y) %then %do ;

  * SUBGROUP TITLE ;
   if first.&total then do;
     * Y COORDINATE ;
     L=&titsize+&space*4;
   if (_N_=1) then do ; H+1.5*L ; end ;
   else do ;
     H+2*L;
   end ;
     YL=&top-H+(L/2);
     YT=YL+(2/5)*&titsize;
     * DRAW TITLE ;

** - JRA, 16OCT2006 - Use of formatted variable total**; 	

  ** JRA, 4MAY2010, new standard size for add2;
  %let add2=500;

*** JRA, May2010, redesigning of the forest plot **;
  %if %upcase(&subgroup)=N %then %do;
	%if (%length(%cmpres(&labfile))>0) %then %do ;

		%LABEL(&Ldescr -500, YT, ltotal,  BLACK,0,0,&textsize,&fontB.,6);
	    %LABEL(&Ldescr +&add2.+1000, YT, Ttotal,                BLACK,0,0,&textsize,&font.,6);

		%end;

		%else %do ;
		**JRA, May2010, variable contaning the concatenation of label+value for TOTAL;
		%LABEL(&Ldescr -500, YT, llabel2,  BLACK,0,0,&textsize,&fontB.,6);

	%end;
  %end;

  %if %upcase(&subgroup)=Y %then %do; ****JRA, May2010;

  		%LABEL(&Ldescr -500, YT+100, llabel2,  BLACK,0,0,260,&fontB.,6);

   %end;

  end;
%end ;
%else %do ;
 if (_N_=1) then do ;
     * Y COORDINATE ;
     L=&space*4;
     H+L;
     YL=&top-H+(L/2);
     YT=YL;
 end ;
%end ;
  * Y COORDINATE ;
  %if (%upcase(&cumMA)=Y) %then %do ;
    sqside=int(0.2*&Tsqside) ;
  %end ;
  %else %do ;
   sqside=int((sqrt(varO_E)/sqrt(&TvarO_E))*&Tsqside);
  %end ;
 
  L=max(&textsize,sqside,400)+&space;

  H+L;
  YL=&top-H+(L/2);
  YT=YL+(2/5)*&textsize;

  * DRAW TEXT ;

  %let Lright=10500;
  ** JRA, 4MAY2010, new standard size for add2;
  %let add1=500;

  %if %upcase(&subgroup)^=Y %then %do;

	%if (%length(%cmpres(&labfile))>0) %then %do ; ****JRA, May2010;
	    %LABEL(&Ltit , YT, Tcode,             BLACK,0,0,&textsize,&font.,6);
		%LABEL(&Ltit +1000, YT, Tdescr,           BLACK,0,0,&textsize,&font.,6);
	%end;

	%else %do;
	****JRA, May2010;
    %LABEL(&Ltit, YT, _Trial,             BLACK,0,0,&textsize,&font.,6);
	%end;

  %end;

  %else %do ; ** - JRA, 16OCT2006 - Use of formatted variable group**;
  ** CASE SUBGROUP = Y;
	  %if &NB >= 1 %then %do;

		*IF GROUP CONTAINS ONE or SEVERAL VARIABLES;

	    * JRA 4MAY2010, from now, if GROUP is not empty, we display HR and CI for one 
		  or several variables into GROUP parameter;

		  %if (%length(%cmpres(&labfile))>0) %then %do ; 

		    ****JRA, May2010, Add of space between label and value of GROUP (vertically) -> YT+300 instead of YT;
 			%if %upcase(&subtotal)=Y %then %do;
			       if first.&total then do; 
	   			        %LABEL(&Ldescr -500, YT+300, lgroup,                BLACK,0,0,&textsize,&Font.,6);
				   end;
			%end;
 			%else %do;
	           %LABEL(&Ltit, YT+300, lgroup,                BLACK,0,0,&textsize,&FontB.,6);
			%end;

		     %LABEL(&Ltit+&add1., YT, Tgroup,                BLACK,0,0,&textsize,&font.,6);

		  %end;

		  %else %do;  ****JRA, May2010, Add of space between label and value of GROUP (vertically) -> YT+300 instead of YT;

 			%if %upcase(&subtotal)=Y %then %do;
			       if first.&total then do; 
					   %LABEL(&Ldescr -500, YT+300, label1,                BLACK,0,0,&textsize,&Font.,6);
				   end;
			%end;
 			%else %do;
	           %LABEL(&Ltit, YT+300, label1,                BLACK,0,0,&textsize,&FontB.,6);
			%end;

		       %LABEL(&Ltit+&add1., YT, group,                BLACK,0,0,&textsize,&font.,6);
		  %end;

	%end;

  %end;

  if ((npat1>0 or npat2>0) and &blinded^=1) then do ;

  %LABEL(&LON1-&colspace*3,   YT, trim(ndeath1), BLACK,0,0,&textsize,&font.,4);
  %LABEL(&LON1,   YT, '/', BLACK,0,0,&textsize,&FontB.,5);
  %LABEL(&LON1+&colspace*3,   YT, left(npat1),   BLACK,0,0,&textsize,&font.,6);

  %LABEL(&LON2-&colspace*3,   YT, trim(ndeath2), BLACK,0,0,&textsize,&font.,4);
  %LABEL(&LON2,   YT, '/', BLACK,0,0,&textsize,&FontB.,5);
  %LABEL(&LON2+&colspace*3,   YT, left(npat2),   BLACK,0,0,&textsize,&font.,6);

  %LABEL(&LO_E,             YT, trim(round(O_E,.1)),     BLACK,0,0,&textsize,&font.,4);
  %LABEL(&LvarO_E,          YT, trim(round(varO_E,.1)),  BLACK,0,0,&textsize,&font.,4);

  ** HR and CI for trial numbers; ****JRA, May2010;

 %IF NOT(%UPCASE(&DISPLAY.)=WORD OR %UPCASE(&DISPLAY.)=POWERPOINT OR %UPCASE(&DISPLAY.)=SCREEN) %THEN %DO;

	%LABEL(&Lred-600, YT, compress(ORroundtout),                BLACK,0,0,&textsize,&font.,6);
	%LABEL(&Lred+150, YT, '('||compress(LCL&conf1.tout)||' ; '||compress(UCL&conf1.tout)||')' ,                BLACK,0,0,&textsize,&font.,6);

  %END;

  %IF %UPCASE(&DISPLAY.)=WORD OR %UPCASE(&DISPLAY.)=POWERPOINT OR %UPCASE(&DISPLAY.)=SCREEN %THEN %DO;

	%LABEL(&Lred-1200, YT, compress(ORroundtout),                BLACK,0,0,&hsize,&font.,6);
	%LABEL(&Lred-450, YT, '('||compress(LCL&conf1.tout)||';'||compress(UCL&conf1.tout)||')' ,                BLACK,0,0,&hsize,&font.,6);

  %END;

  end ;

  if ((npat1<=0 and npat2<=0)) then do ;
  %LABEL(&Lblind, YT, "(DATA NOT AVAILABLE)",BLACK,0,0,&textsize,&font.,5) ;  
  end ;
  if (&blinded=1) then do ;
  %LABEL(&Lblind, YT, "BLINDED",BLACK,0,0,&textsize,&font.,5) ;  
  end ;


%if (%upcase(&CumMA)=Y) %then %do ;
 if (p^='') then do ;
 %LABEL(&Lred, YT, compress('Z='||left(trim(z))), BLACK, 0,0, &textsize, &font.,4) ;
 %LABEL(&Lred+1500, YT, ' p'||left(trim(p)) , BLACK, 0,0, &textsize, &font.,4) ;
 end ;
 %end ;

* SQUARES AND LINES  ;

if (not missing(OR) and &blinded^=1) then do ;

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
       
        if (sqpos < cipos and _lit^=1) then  
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

        if (sqpos<cipos and _lit=1) then do;
              %RECT(point-sqside/2-&litsize/2,YL-sqside/2-&litsize/2,point+sqside/2+&litsize/2,YL+sqside/2+&litsize/2,&Red.,1,&litsize);
              %BAR(&Laxis0+(&axisun*log(UCL&conf1)),YL-&cisize,&Laxis0+(&axisun*log(UCL&conf1)),YL+&cisize,&Red.,1,SOLID);
        end ;
           
        
        if (sqpos>=cipos) then do ;

* 1.1.: No arrow at the upper nor the lower end ;

          if (log(LCL&conf1)>-1.3863) then do ; * Normal CI ;

            %BAR(&Laxis0+(&axisun*log(LCL&conf1)),YL-&cisize,&Laxis0+(&axisun*log(OR))-sqside/2,YL+&cisize,&Red.,1,SOLID);
            %BAR(&Laxis0+(&axisun*log(OR))+sqside/2,YL-&cisize,&Laxis0+(&axisun*log(UCL&conf1)),YL+&cisize,&Red.,1,SOLID);
            point=&Laxis0+(&axisun*log(OR));
          if (_Lit=1) then do ;
           %RECT(point-sqside/2-&litsize/2,YL-sqside/2-&litsize/2,point+sqside/2+&litsize/2,YL+sqside/2+&litsize/2,&Red.,1,&litsize);
          end ;
          if (_lit=0) then do ;
           %BAR(point-sqside/2,YL-sqside/2,point+sqside/2,YL+sqside/2,&Red.,1,SOLID);
          end ;

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
              if (_Lit=1) then do ;
              %RECT(point-sqside/2-&litsize/2, YL-sqside/2-&litsize/2, point+sqside/2+&litsize/2, YL+sqside/2+&litsize/2, &Red., 1, &litsize) ;
              end ;
              if (_Lit=0) then do ;
              %BAR(point-sqside/2, YL-sqside/2, point+sqside/2, YL+sqside/2, &Red., 1,SOLID) ;
              end ;
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
     if (_Lit=1) then do ;
     %RECT(point-sqside/2-&litsize/2,YL-sqside/2-&litsize/2,point+sqside/2+&litsize/2,YL+sqside/2+&litsize/2,&Red.,1,&litsize);
     end ;
     if (_Lit=0) then do ;
     %BAR(point-sqside/2,YL-sqside/2,point+sqside/2,YL+sqside/2,&Red.,1,SOLID);
     end ;
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
       
        if (sqpos < cipos and _lit^=1) then   
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

        if (sqpos<cipos and _lit=1) then do;
              %RECT(point-sqside/2-&litsize/2,YL-sqside/2-&litsize/2,point+sqside/2+&litsize/2,YL+sqside/2+&litsize/2,&Red.,1,&litsize);
              %BAR(&Laxis1+(&axisun*UCL&conf1),YL-&cisize,&Laxis1+(&axisun*UCL&conf1),YL+&cisize,&Red.,1,SOLID);
        end ;
           
        
        if (sqpos>=cipos) then do ;

            %BAR(&Laxis1+(&axisun*LCL&conf1),YL-&cisize,&Laxis1+(&axisun*OR)-sqside/2,YL+&cisize,&Red.,1,SOLID);
            %BAR(&Laxis1+(&axisun*OR)+sqside/2,YL-&cisize,&Laxis1+(&axisun*UCL&conf1),YL+&cisize,&Red.,1,SOLID);
            point=&Laxis1+(&axisun*OR);
          if (_Lit=1) then do ;
           %RECT(point-sqside/2-&litsize/2,YL-sqside/2-&litsize/2,point+sqside/2+&litsize/2,YL+sqside/2+&litsize/2,&Red.,1,&litsize);
          end ;
          if (_lit=0) then do ;
           %BAR(point-sqside/2,YL-sqside/2,point+sqside/2,YL+sqside/2,&Red.,1,SOLID);
          end ;
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
   if (_Lit=1) then do ;
     %RECT(point-sqside/2-&litsize/2,YL-sqside/2-&litsize/2,point+sqside/2+&litsize/2,YL+sqside/2+&litsize/2,&Red.,1,&litsize);
   end ;
   if (_Lit=0) then do ;
     %BAR(point-sqside/2,YL-sqside/2,point+sqside/2,YL+sqside/2,&Red.,1,SOLID);
   end ;

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


  * SUBTOTALS ;

%if (%upcase(&subtotal)=Y) %then

%do;
 if last.&total then do;
     * Y COORDINATE ;
     sqside=(sqrt(symget('VOE'||left(grind)))/sqrt(&TvarO_E))*&Tsqside;
     L=max(&titsize,sqside,400)+&space*4;
     H+L;
     YL=&top-H+(L/2);
     YT1=YL+(2/5)*&titsize;
     YT2=YL+(2/5)*&textsize;

     * SQUARE left of the graph;
     point=&Ldescr;

	%IF &TYP_ENDPOINT =TTE %THEN %BAR(point-sqside/2,YL-sqside/2,point+sqside/2,YL+sqside/2,&Red.,1,SOLID);;
	%IF &TYP_ENDPOINT =BIN %THEN %BAR(point-sqside/2,YL-sqside/2,point+sqside/2,YL+sqside/2,&Red.,1,SOLID);;
	*jan 2002 rsy green instead of red for binary data;

     * SUBTOTALS ;
     point=&Ldescr+sqside/2+(6*&colspace);
     %LABEL(point, YT1, "Subtotal", BLACK,0,0,&titsize, &FontB.,6);

     %LABEL(&LON1-&colspace*3, YT2,      symget('DT1'||left(grind)),   BLACK,0,0,&textsize,&fontB.,4);
     %LABEL(&LON1,   YT2, '/', BLACK,0,0,&textsize,&fontB.,5);
     %LABEL(&LON1+&colspace*3, YT2, left(symget('PT1'||left(grind))),  BLACK,0,0,&textsize,&fontB.,6);
     %LABEL(&LON2-&colspace*3, YT2,      symget('DT2'||left(grind)),   BLACK,0,0,&textsize,&fontB.,4);
      %LABEL(&LON2,   YT2, '/', BLACK,0,0,&textsize,&fontB.,5);
     %LABEL(&LON2+&colspace*3, YT2, left(symget('PT2'||left(grind))),  BLACK,0,0,&textsize,&fontB.,6);
     %LABEL(&LO_E,           YT2,      symget('OE'||left(grind)),   BLACK,0,0,&textsize,&fontB.,4);
     %LABEL(&LvarO_E,        YT2,      symget('VOE'||left(grind)),   BLACK,0,0,&textsize,&fontB.,4);

     * PCT OF EVENTS ;
     %LABEL(&LON1, YT2-&hsize-&space*2, '('||trim(left(round((symget('DT1'||left(grind))/symget('PT1'||left(grind)))*100,0.1)))||' %)',  BLACK,0,0,&hsize,&FontB.,5);
     %LABEL(&LON2, YT2-&hsize-&space*2, '('||trim(left(round((symget('DT2'||left(grind))/symget('PT2'||left(grind)))*100,0.1)))||' %)',  BLACK,0,0,&hsize,&FontB.,5);

     * Test for Heterogeneity within the subtotal ;
     _check_=left(symget('HDF'||left(grind)))+ 0 ;
     if (_check_>0) then do ;

     ** JRA, 13022015, add of I2 measure;
	 %LABEL(&Ldescr+600, YT2-2*&hsize-&space*4, 'Heterogeneity Q='||left(symget('HCHI'||left(grind)))||' (df='||left(symget('HDF'||left(grind)))||') p'||left(symget('HP'||left(grind)))||', I',
                   BLACK,0,0,&textsize,&font.,6) ;  

		%IF &DISPLAY = PDF %THEN %DO;
			 %LABEL(., YT2-2*&hsize-&space*4, '2',BLACK,0,0,&textsize-120,&font.,4) ;  
		%END;
		%IF &DISPLAY ne PDF %THEN %DO;
		 %LABEL(., YT2-2*&hsize-&space*4, '2',BLACK,0,0,&textsize-120,&font.,6) ;  
		%END;


	 %LABEL(., YT2-2*&hsize-&space*4, '= '||left(symget('I2tot'||left(&TOTAL.))),BLACK,0,0,&textsize,&font.,6) ;  

     end ;

     * DIAMOND ;
     %LINE(1,1,2,2,WHITE,1,&linesize);      * DUMMY LINE TO RESET ANNO VARIABLES ;

	 %IF &TYP_ENDPOINT =TTE %THEN %DO;
     	%POLY(    &Laxis2+(&axisun*symget('OR'||left(grind))), YL+100,&Green.,&diamond.,1);
	 %END;
	 %IF &TYP_ENDPOINT =BIN %THEN %DO;
     	%POLY(    &Laxis2+(&axisun*symget('OR'||left(grind))), YL+100,&Green.,msolid,1); *jan 2002 msolid instead of &diamond, green instead of red, also in 7 statements below;
	 %END;

	 %IF &TYP_ENDPOINT =TTE %THEN %DO;

		%if (%upcase(&BONF)^=Y) %then %POLYCONT(&Laxis2+(&axisun*symget('U1'||left(grind))), YL,    &Green.         );;
		%if (%upcase(&BONF)=Y) %then  %POLYCONT(&Laxis2+(&axisun*symget('U2'||left(grind))), YL,    &Green.         );;
		     %POLYCONT(&Laxis2+(&axisun*symget('OR'||left(grind))), YL-100, &Red.         );
		%if (%upcase(&BONF)^=Y) %then %POLYCONT(&Laxis2+(&axisun*symget('L1'||left(grind))), YL,    &Green.         );;
		%if (%upcase(&BONF)=Y) %then  %POLYCONT(&Laxis2+(&axisun*symget('L2'||left(grind))), YL,    &Green.         );;

     %END;

	 %IF &TYP_ENDPOINT =BIN %THEN %DO;

		%if (%upcase(&BONF)^=Y) %then %do ;
			%do i=&start. %to &NN;
				%if %sysevalf(&&uu1&i.> &LIMITU.) %then %let u1&&grind&i.=&LIMITU.;
			%end;
		     %POLYCONT(&Laxis2+(&axisun*symget('U1'||left(grind))), YL,    &Green.         );
		%end ;

		%if (%upcase(&BONF)=Y) %then %do ;
			%do i=&start. %to &NN;
				%if %sysevalf(&&uu2&i.> &LIMITU.) %then %let u2&&grind&i.=&LIMITU.;
			%end;
		     %POLYCONT(&Laxis2+(&axisun*symget('U2'||left(grind))), YL,    &Green.         );
		%end ;
        %POLYCONT(&Laxis2+(&axisun*symget('OR'||left(grind))), YL-100, &Green.         );

		%if (%upcase(&BONF)^=Y) %then %do ;
			%do i=&start. %to &NN;
				%let a=%sysevalf(&&ll1&i.,floor);
				%put a &a;
				%if %sysevalf(&a < &LIMITL.) %then %let l1&&grind&i.=&LIMITL.;
			%end;
		     %POLYCONT(&Laxis2+(&axisun*symget('L1'||left(grind))), YL,    &Green.         );
		%end ;

		%if (%upcase(&BONF)=Y) %then %do ;
			%do i=&start. %to &NN;
				%let a=%sysevalf(&&ll2&i.,floor);
				%put a &a;
				%if %sysevalf(&a < &LIMITL.) %then %let l2&&grind&i.=&LIMITL.;
			%end;
			%POLYCONT(&Laxis2+(&axisun*symget('L2'||left(grind))), YL,    &Green.         );
		%end ;

	 %END;

	 %IF &TYP_ENDPOINT =TTE %THEN %DO;
	     %POLYCONT(&Laxis2+(&axisun*symget('OR'||left(grind))), YL+100, &Green.         );
	     %POLYCONT(&Laxis2+(&axisun*symget('OR'||left(grind))), YL-100, &Green.         );
	 %END;

	 %IF &TYP_ENDPOINT =BIN %THEN %DO;
	     %POLYCONT(&Laxis2+(&axisun*symget('OR'||left(grind))), YL+100, &Green.         );
	     %POLYCONT(&Laxis2+(&axisun*symget('OR'||left(grind))), YL-100, &Green.         );
	 %END;


	 %IF &TYP_ENDPOINT =BIN %THEN %DO;

			 %if (%upcase(&BONF)^=Y) %then %do;
			 	%if &LOGS=N %then %do;	if U1> &LIMITU.  then do;	%end;
			 	%if &LOGS=Y %then %do;	if log(U1)> &LIMITU.  then do;	%end;
			 %end;
			 %if (%upcase(&BONF)=Y) %then %do;
			 	%if &LOGS=N %then %do;	if U2> &LIMITU.  then do;	%end;
			 	%if &LOGS=Y %then %do;	if log(U2)> &LIMITU.  then do;	%end;
			 %end;

				   %if &LOGS=N %then %let delta1=%sysevalf(&axisun*2);
				   %if &LOGS=Y %then %let delta1=%sysevalf(2*(&axisun*1.38));

				   %POLY(    &Laxis1+&delta1,     YL+&CISIZE*2, &Green.,SOLID,1);
				   %POLYCONT(&Laxis1+&delta1 +&cisize*3, YL,    &Green.       );
				   %POLYCONT(&Laxis1+&delta1,     YL-&cisize*2, &Green.        );
				   %POLYCONT(&Laxis1+&delta1,     YL+&cisize*2, &Green.        );

			end;

			 %if (%upcase(&BONF)^=Y) %then %do;
			 	%if &LOGS=N %then %do;	if L1< &LIMITL.  then do;	%end;
			 	%if &LOGS=Y %then %do;	if log(L1)< &LIMITL.  then do;	%end;
			 %end;
			 %if (%upcase(&BONF)=Y) %then %do;
			 	%if &LOGS=N %then %do;	if L2< &LIMITL.  then do;	%end;
			 	%if &LOGS=Y %then %do;	if log(L2)< &LIMITL.  then do;	%end;
			 %end;

			 		%if &LOGS=N %then %let delta2=%sysevalf(&axisun*0);
				    %if &LOGS=Y %then %let delta2=%sysevalf(&axisun*1.38);

					  %POLY(    &Laxis2-&delta2,     YL+&CISIZE*2, &Green.,SOLID,1);
		              %POLYCONT(&Laxis2-&delta2-&cisize*3, YL,    &Green.        );
		              %POLYCONT(&Laxis2-&delta2,     YL-&cisize*2, &Green.        );
		              %POLYCONT(&Laxis2-&delta2,     YL+&cisize*2, &Green.        );

				end;

	 %END;

  %IF NOT(%UPCASE(&DISPLAY.)=WORD OR %UPCASE(&DISPLAY.)=POWERPOINT OR %UPCASE(&DISPLAY.)=SCREEN) %THEN %DO;
	****JRA, May2010, rounded OR + CI without right format;
	%LABEL(&Lred-600, YT1, compress(_ORroundtout),                BLACK,0,0,&textsize,&fontb.,6);
	 %IF (%upcase(&BONF)=Y) %THEN %DO;
	%LABEL(&Lred+150, YT1, '('||compress(_LCL&conf2.tout)||' ; '||compress(_UCL&conf2.tout)||')*' ,                BLACK,0,0,&textsize,&fontb.,6);
	 %END;
	 %IF (%upcase(&BONF)^=Y) %THEN %DO;
	%LABEL(&Lred+150, YT1, '('||compress(_LCL&conf1.tout)||' ; '||compress(_UCL&conf1.tout)||')' ,                BLACK,0,0,&textsize,&fontb.,6);
	 %END;
  %END;
  %IF %UPCASE(&DISPLAY.)=WORD OR %UPCASE(&DISPLAY.)=POWERPOINT OR %UPCASE(&DISPLAY.)=SCREEN %THEN %DO;
	****JRA, May2010, rounded OR + CI without right format;
	%LABEL(&Lred-1200, YT1, compress(_ORroundtout),                BLACK,0,0,&textsize,&fontb.,6);
	 %IF (%upcase(&BONF)=Y) %THEN %DO;
	%LABEL(&Lred-450, YT1, '('||compress(_LCL&conf2.tout)||';'||compress(_UCL&conf2.tout)||')*' ,                BLACK,0,0,&textsize,&fontb.,6);
	 %END;
	 %IF (%upcase(&BONF)^=Y) %THEN %DO;
	%LABEL(&Lred-450, YT1, '('||compress(_LCL&conf1.tout)||';'||compress(_UCL&conf1.tout)||')' ,                BLACK,0,0,&textsize,&fontb.,6);
	 %END;
  %END;

 end;
%end; * SUBTOTAL=Y ;


 %IF &NB>1 %THEN %DO;
	 if last.flag then do;
	     L=550;
	  	 H+1*L;
	     YL=&top-H+(L/2);
	     YT=YL+(3/5)*&textsize;

		** ICICICICICI;

		 ****JRA, May2010, display for heterogeneity and trend test;
	     * Heterogeneity test ;

		 %put &HCHI2;

		   ** JRA, 13022015, add of I2 measure;
	     %LABEL(&LON1-500, YT-200, 'Heterogeneity Q='||left(symget('HCHI'||left(flag)))||' (df='||left(symget('HDF'||left(flag)))||') p'||left(symget('HP'||left(flag)))||', ',
	                   BLACK,0,0,240,&font.,3) ;  
	     %LABEL(., YT-200, ' I',
	                   BLACK,0,0,240,&font.,3) ;  
		%IF &DISPLAY = PDF %THEN %DO;
			%LABEL(., YT,"2",BLACK,0,0,120,&font.,3);
		%END;

		%IF &DISPLAY NE PDF %THEN %DO;
			%LABEL(., YT,"2",BLACK,0,0,120,&font.,3);
		%END;

		 %LABEL(XLSTT, YT-200, "= "||left(symget('I2'||left(flag))),
	                   BLACK,0,0,240,&font.,3) ;   

	      * Trend test ;
		 IF TREND=1 THEN DO;; **JRA, May2010, put trend test only if TREND=1;

		     L=&titsize+&space;
		     H+L;
		     YT=&top-H+(2/5)*&titsize+200;

			  %LABEL(&LON1-500, YT-200, 'Trend  ',BLACK,0,0,240,&font.,3) ;
			  %LABEL(., YT-200, 'x',BLACK,0,0,240,CGREEK,3) ;
  			  %LABEL(., YT,"2",BLACK,0,0,120,&font.,3);

			  %LABEL(., YT-200, '='||left(symget('TTRchi'||left(flag)))||' (df='||left(symget('TTRf'||left(flag)))||') p'||left(symget('TTRp'||left(flag))),
			            BLACK,0,0,240,&font.,3) ;    

		 END; 
    	 %LINE(1,1,2,2,white,1,&linesize);      * DUMMY LINE TO RESET ANNO VARIABLES ;
	  end ;
  %END;

  if lastobs then do;
%if (%upcase(&CumMA)^=Y) %then %do ;
  * TOTAL ;
     * Y COORDINATE ;
     L=max(&titsize,&Tsqside,400)+&space*4;
     H+1.5*L;
     YL=&top-H+(L/2);
     YT1=YL+(2/5)*&titsize;
     YT2=YL+(2/5)*&hsize;
     * SQUARE ;
     point=&Ltit+&Tsqside/2;

 	 %IF &TYP_ENDPOINT =TTE %THEN
     %BAR(point-&Tsqside/2,YL-&Tsqside/2,point+&Tsqside/2,YL+&Tsqside/2,&Red.,1,SOLID);;

	 %IF &TYP_ENDPOINT =BIN %THEN
     %BAR(point-&Tsqside/2,YL-&Tsqside/2,point+&Tsqside/2,YL+&Tsqside/2,&Red.,1,SOLID);; *rsy 15 Jan 2002: green instead of red;

     * TOTALS ;
     point=&Ltit+&Tsqside+(6*&colspace);
     %LABEL(point, YT1, "Total", BLACK,0,0,&titsize, &FontB.,6);

      %LABEL(&LON1-&colspace*3, YT2, trim(&Tndth1),  BLACK,0,0,&hsize,&FontB.,4);

      %IF &TYP_ENDPOINT =TTE %THEN %LABEL(&LON1,   YT2, '/', BLACK,0,0,&textsize,&FontB.,5);;
	  %IF &TYP_ENDPOINT =BIN %THEN %LABEL(&LON1,   YT2, '/', BLACK,0,0,&textsize,&Font.,5);;

      %LABEL(&LON1+&colspace*3, YT2, left(&Tnpat1),  BLACK,0,0,&hsize,&FontB.,6);

      %LABEL(&LON2-&colspace*3, YT2, trim(&Tndth2),  BLACK,0,0,&hsize,&FontB.,4);
      %LABEL(&LON2,   YT2, '/', BLACK,0,0,&textsize,&FontB.,5);
     %LABEL(&LON2+&colspace*3, YT2, left(&Tnpat2),  BLACK,0,0,&hsize,&FontB.,6);
     %LABEL(&LO_E,           YT2, trim(&TO_E),    BLACK,0,0,&hsize,&FontB.,4);
     %LABEL(&LvarO_E,        YT2, trim(&TvarO_E), BLACK,0,0,&hsize,&FontB.,4);

	 %put RTOR &RTOR;

	%IF NOT(%UPCASE(&DISPLAY.)=WORD OR %UPCASE(&DISPLAY.)=POWERPOINT OR %UPCASE(&DISPLAY.)=SCREEN) %THEN %DO;

	 ** HR and CI for big total; **May2010;
	 %LABEL(&Lred-600,        YT2, trim(left("&RTOR")), BLACK,0,0,&hsize,&FontB.,6);

	 %IF (%upcase(&BONF)^=Y) %THEN
     %LABEL(&Lred+150,        YT2, '('||compress("&RTLCL1")||' ; '||compress("&RTUCL1")||')' , BLACK,0,0,&hsize,&FontB.,6);
	 %IF (%upcase(&BONF)=Y) %THEN
     %LABEL(&Lred+150,        YT2, '('||compress("&RTLCL2")||' ; '||compress("&RTUCL2")||')*' , BLACK,0,0,&hsize,&FontB.,6);

	%END;

	%IF %UPCASE(&DISPLAY.)=WORD OR %UPCASE(&DISPLAY.)=POWERPOINT OR %UPCASE(&DISPLAY.)=SCREEN %THEN %DO;

	 ** HR and CI for big total; **May2010;
	 %LABEL(&Lred-1200,        YT2, trim(left("&RTOR")), BLACK,0,0,&hsize,&FontB.,6);

	 %IF (%upcase(&BONF)^=Y) %THEN
     %LABEL(&Lred-450,        YT2, '('||compress("&RTLCL1")||';'||compress("&RTUCL1")||')' , BLACK,0,0,&hsize,&FontB.,6);
	 %IF (%upcase(&BONF)=Y) %THEN
     %LABEL(&Lred-450,        YT2, '('||compress("&RTLCL2")||';'||compress("&RTUCL2")||')*' , BLACK,0,0,&hsize,&FontB.,6);

	%END;

     * PCT OF EVENTS ;
     %LABEL(&LON1, YT2-&hsize-&space*2, '('||trim(left(round((&Tndth1/&Tnpat1)*100,0.1)))||' %)',  BLACK,0,0,&hsize,&FontB.,5);
     %LABEL(&LON2, YT2-&hsize-&space*2, '('||trim(left(round((&Tndth2/&Tnpat2)*100,0.1)))||' %)',  BLACK,0,0,&hsize,&FontB.,5);

	 * DIAMOND ;
     %LINE(1,1,2,2,WHITE,1,&linesize);      * DUMMY LINE TO RESET ANNO VARIABLES ;

	%IF &TYP_ENDPOINT =TTE %THEN %POLY(&Laxis2+(&axisun*&TOR),  YL+100, &Green.,&diamond.,1);;
	%IF &TYP_ENDPOINT =BIN %THEN %POLY(&Laxis2+(&axisun*&TOR),  YL+100, &Green.,msolid,1);;
	 *jan 2002: msolid instead of &diamond, green instead of red;

	%IF &TYP_ENDPOINT =TTE %THEN %DO;

		%if (%upcase(&BONF)^=Y) %then  %POLYCONT(&Laxis2+(&axisun*&TUCL1), YL,    &Green.         );
		%if (%upcase(&BONF)=Y) %then   %POLYCONT(&Laxis2+(&axisun*&TUCL2), YL,    &Green.         );
		%POLYCONT(&Laxis2+(&axisun*&TOR),  YL-100, &Green.         );
		%if (%upcase(&BONF)^=Y) %then  %POLYCONT(&Laxis2+(&axisun*&TLCL1), YL,    &Green.         );
		%if (%upcase(&BONF)=Y) %then   %POLYCONT(&Laxis2+(&axisun*&TLCL2), YL,    &Green.         );
		     %POLYCONT(&Laxis2+(&axisun*&TOR),  YL+100, &Green.       );
		     %POLYCONT(&Laxis2+(&axisun*&TOR),  YL-100, &Green.       );
	%END;

	%IF &TYP_ENDPOINT =BIN %THEN %DO;

		%if (%upcase(&BONF)^=Y) %then %do ;
			%if %sysevalf(&TUCL1.,integer) > &LIMITU. %then %let TUCL1=&LIMITU.;
		     %POLYCONT(&Laxis2+(&axisun*&TUCL1), YL,    &Green.         );
		%end ;

		%if (%upcase(&BONF)=Y) %then %do ;
			%if %sysevalf(&TUCL2.,integer) > &LIMITU. %then %let TUCL2=&LIMITU.;
		    %POLYCONT(&Laxis2+(&axisun*&TUCL2), YL,    &Green.         );
		%end ;

		%POLYCONT(&Laxis2+(&axisun*&TOR),  YL-100, &Green.         );

		%if (%upcase(&BONF)^=Y) %then %do ;
			%let b=%sysevalf(&TLCL1,floor);
			%put b &b;
			%if %sysevalf(&b < &LIMITL.) %then %let TLCL1=&LIMITL.;
	   		%POLYCONT(&Laxis2+(&axisun*&TLCL1), YL,    &Green.         );
	     %end ;

		%if (%upcase(&BONF)=Y) %then %do ;
			%let b=%sysevalf(&TLCL2,floor);
			%put b &b;
			%if %sysevalf(&b < &LIMITL.) %then %let TLCL2=&LIMITL.;
	  		%POLYCONT(&Laxis2+(&axisun*&TLCL2), YL,    &Green.         );
	     %end ;

	     %POLYCONT(&Laxis2+(&axisun*&TOR),  YL+100, &Green.       );
	     %POLYCONT(&Laxis2+(&axisun*&TOR),  YL-100, &Green.       );

	%END;

%END;*end if (%upcase(&CumMA)^=Y);

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

     *JRA, 29-08-2008, ADD OF A VERTICAL LINE THROUGH ANYMARGIN PARAMETER ;
	 %IF &ANYMARGIN NE NO %then %do;
     %LINE(&Laxis2+(&axisun*(log(&ANYMARGIN))), &top ,&Laxis2+(&axisun*(log(&ANYMARGIN))), YL,&Red.,2,&linesize);
	 %END;

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

     *JRA, 29-08-2008, ADD OF A VERTICAL LINE THROUGH ANYMARGIN PARAMETER ;
	 %IF &ANYMARGIN NE NO %then %do;
     %LINE(&Laxis2+(&axisun*&ANYMARGIN), &top ,&Laxis2+(&axisun*&ANYMARGIN), YL,&Red.,2,&linesize);
	 %END;

%end ;

     * VERTICAL LINE THROUGH TOTAL OR ;
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

     *JRA, 29-08-2008, ADD OF THE VALUE OF ANYMARGIN PARAMETER ;
	 %IF &ANYMARGIN NE NO %then %do;
     %LABEL(&Laxis2+(&axisun*(log(&ANYMARGIN))), YT,"&ANYMARGIN.",&Red.,0,0,&hsize,&FontB.,5);
	 %END;

%end ; * end LOGS=Y ;

%if (&LOGS=N) %then %do ;
     %LABEL(&Laxis0-(&axisun),YT,'0.0',BLACK,0,0,&hsize,&FontB.,5);
     %LABEL(&Laxis0-(&axisun*0.5), YT,'0.5',BLACK,0,0,&hsize,&FontB.,5);
     %LABEL(&Laxis0,     YT,'1.0',BLACK,0,0,&hsize,&FontB.,5);
     %LABEL(&Laxis0+(&axisun*0.5), YT,'1.5',BLACK,0,0,&hsize,&FontB.,5);
     %LABEL(&Laxis0+(&axisun),   YT,'2.0',BLACK,0,0,&hsize,&FontB.,5);

     *JRA, 29-08-2008, ADD OF THE VALUE OF ANYMARGIN PARAMETER ;
	 %IF &ANYMARGIN NE NO %then %do;
     %LABEL(&Laxis2+(&axisun*&ANYMARGIN), YT,"&ANYMARGIN.",&Red.,0,0,&hsize,&FontB.,5);
	 %END;

%end ;


     labY=&top-H-&space*2;

     L=&titsize+&space*3;
     H+L;
     YT=&top-H+(L/2)+(2/5)*&titsize;

     %IF &EndPoint.=F %THEN %DO;** JRA, 04JUN2010, change of position for "xx trt arm" better on the plot;
        %LABEL(&Laxis0-1500,YT,"&trt2.",BLACK,0,0,&titsize,&FontB.,5); *jan 2002 for rsy ;
        %LABEL(&Laxis0+1700,YT,"&trt1.",BLACK,0,0,&titsize,&FontB.,5); *jan 2002 for rsy ;
     %END;
     %ELSE %DO;
        %LABEL(&Laxis0-1500, YT,"&trt1.",BLACK,0,0,&titsize,&FontB.,5);
        %LABEL(&Laxis0+1700, YT,"&trt2.",BLACK,0,0,&titsize,&FontB.,5);
     %END;


	 %IF &NB<=1 %THEN   %LABEL(&Ltit, YT,"&comment1",BLACK,0,0,&textsize,&font.,6);

     L=&titsize+&space;
     H+L;
     YT=&top-H+(L/2)+(2/5)*&titsize;

	 %IF &TYP_ENDPOINT =TTE %THEN %DO;
	     %LABEL(&Laxis0-1375, YT,"&tit54",BLACK,0,0,&titsize,&FontB.,5);
	     %LABEL(&Laxis0+1375, YT,"&tit54",BLACK,0,0,&titsize,&FontB.,5);
	 %END;

	 %IF &TYP_ENDPOINT =BIN %THEN %DO;
		 %LABEL(&Laxis0-1375, YT,"&tit54",BLACK,0,0,&titsize,&FontB.,5);
	     %LABEL(&Laxis0+1375, YT,"&tit54",BLACK,0,0,&titsize,&FontB.,5);
	 %END;

     %IF &NB<=1 %THEN %DO;

	 %IF &SUBTOTAL=N %THEN %DO;
	 **JRA, 23022015 : Add of I2 measure (bottom left corner);

		 %IF &TH1 ne 0 %then %do;
			%LABEL(&Ltit, YT,"&comment2"||", I",BLACK,0,0,&textsize,&font.,6);

			%IF &DISPLAY = PDF %THEN %DO;
				%LABEL(., YT,"2",BLACK,0,0,120,&font.,4);
			%END;
			%IF &DISPLAY NE PDF %THEN %DO;
				%LABEL(., YT,"2",BLACK,0,0,120,&font.,6);
			%END;

			%LABEL(., YT,"= &I21",BLACK,0,0,&textsize,&font.,6);

		 %END;

	 %END;

	 %IF &SUBTOTAL=Y %THEN %DO;

		 %IF &TH1 ne 0 %then %do;

		 **JRA, 23022015 : Add of I2 measure (bottom left corner);
			%LABEL(&Ltit, YT,"&comment2"||", I",BLACK,0,0,&textsize,&font.,6);

			%IF &DISPLAY = PDF %THEN %DO;
				%LABEL(., YT,"2",BLACK,0,0,120,&font.,4);
			%END;

			%IF &DISPLAY NE PDF %THEN %DO;
				%LABEL(., YT,"2",BLACK,0,0,120,&font.,6);
			%END;

			%LABEL(., YT,"= &I21",BLACK,0,0,&textsize,&font.,6);

		 %END;


	 %END;


	 %END;

     %LINE(&Laxis0, labY, &Laxis0, YT-(L/2)-(2/5)*&titsize,BLACK,1,&linesize);

     L=&titsize+&space*4;
     H+L;
     YT=&top-H+(L/2)+(2/5)*&titsize+40;
     %LABEL(&Laxis0, YT,"&effect",BLACK,0,0,&titsize,&FontB.,5);

	 ** Interaction test and trend test if N <=1;
	 %IF &NB<=1 %THEN %DO;

		 %IF &comment3 ne %THEN %DO;
	    	%LABEL(&Ltit, YT,"&comment3",BLACK,0,0,&textsize,&Font.,6);

		     L=&titsize+&space*4;
		     H+L;
		     YT=&top-H+(L/2)+(2/5)*&titsize+200;

	/*		%LABEL(., YT,"Chi",BLACK,0,0,&textsize,&Font.,6);*/

		 	%IF &comment3=&trend1 %THEN %DO;
			%LABEL(&Ltit, YT, 'x',BLACK,0,0,300,CGREEK,6) ;
			%LABEL(., YT,"2",BLACK,0,0,&textsize-120,&Font.,6);
			%END;
		 	%IF &comment3=&inter1 %THEN %DO;
			%LABEL(&Ltit, YT, 'Q',BLACK,0,0,&textsize,&Font.,6) ;
			%END;

		 	%IF &comment3=&inter1 %THEN %DO;
			%LABEL(., YT,"="||"&inter2.",BLACK,0,0,&textsize,&Font.,6);
			%END;
		 	%IF &comment3=&trend1 %THEN %DO;
			%LABEL(., YT,"="||"&trend2.",BLACK,0,0,&textsize,&Font.,6);
			%END;

		 %END;

	 %END;


** remark when different CI used for totals and studies ;
    %if (%upcase(&BONF)=Y) %then %do ; 
     L=&titsize+&space*4;
     H+L;
     YT=&top-H+(2/5)*&titsize+200;
     %LABEL(&Ltit, YT, "*&conf2% CI for totals and subtotals, &conf1% CI elsewhere",   BLACK,0,0,&textsize,&Font.,6);
     %end ;

**** ADD: LCO, 21NOV2007 ;
    %if (%upcase(&BONF)^=Y) %then %do ; 
     L=&titsize+&space*4;
     H+L;
     YT=&top-H+(2/5)*&titsize+200;
     %LABEL(&Ltit, YT, "*&conf1% CI everywhere",   BLACK,0,0,&textsize,&Font.,6);
     %end ;

     YT=YT-2000 ;
     YT=YT-500 ;

*** DATE AND WARNING ;
     %if &final.=N %then %do;
        %LABEL(&Lred-1000,1500, "&_DATE_"||"  "||"&_TIME_" ,BLACK,0,0,180,&font.,C) ;    
        %LABEL(&Ltit+9000,1500,"Not for publication or citation",BLACK,0,0,300, &font.,B);
     %end;
 end;

run;

%IF not(&subtotal=Y and &subgroup=Y) %THEN %DO;
	** Feb2009, JRA, To remove the repetition of the variable name for each level of this variable;
	data anno; 
		set anno;
		indz=_n_;
	run;

	** JRA, 11JUNE2014, cosmetic changes: let the display of 2 times the same label in case of
	having two variables with the same label;
	%sort(data=anno,var=text flag x);

	data anno;
	set anno;
	by text flag x;
		%if (%length(%cmpres(&labfile))=0) %then %do ;	
			if not(first.flag) and (text=label1) then delete;
		%end;
		%if (%length(%cmpres(&labfile))>0) %then %do ;	
			if not(first.flag) and (text=lgroup) then delete;
		%end;
	run;

	%sort(data=anno,var=indz);
%END;

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


proc means data=annoG(where=(FUNCTION="LABEL" and text not in ("&dat" "Not for publication or citation"))) 
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
    if text in ("&dat" "Not for publication or citation") then Y=200;  
    else Y=Y+&pas; 
	if Y>26000 then Y=26000;
run;
%end;
%else %do;
data annoG;
	set annoG;
    if text in ("&dat" "Not for publication or citation") then Y=&min.-400;  
	if Y>26000 then Y=26000;
run;
%end;

** TO AUTOSIZE GRAPH **;

data myannog;
	set annoG;
	if FUNCTION="LABEL" and text not in ("&dat" "Not for publication or citation");
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
 totfile totid tot _tests __format _test _lab _t __litt;
quit;


** modified April 05 ;
%IF &FontType.=HWCGM  %THEN %keymap(#,7c,?,b1,-,2d ) ;
%ELSE %IF &FontType.=HWPDF  %THEN %keymap(#,6c,?,f1,-,2d ) ;
%ELSE %keymap(#,bf,?,f1,-,2d ) ;

%IF &GSFName.^=%STR() %THEN %DO;
   filename grafout "&OutFile.";
%END;

goptions reset=all;
options papersize=A4 ;

%IF %UPCASE(&DISPLAY.)=SCREEN  or %UPCASE(&DISPLAY.)=WORD or %UPCASE(&DISPLAY.)=POWERPOINT 
or %UPCASE(&DISPLAY.)=CGM %THEN %DO ;

%let Vpos1=%EVAL(&Vpos.-&ajust);
%let ymax =%SYSEVALF(29.7*&pct.);

goptions gsfname=&GSFName.
         gsfmode=&GSFMode.      device=&Device.
         papersize=A4 noborder
         ftext=&Font.
         rotate=&Rotate.
         gunit=&GUnit.
         hpos=&HPos.
         vpos=&VPos1.
         keymap=mykeymap 
         display vsize=&ymax cm ;
%END ;

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


proc ganno annotate=annoG ; 
run;

goptions reset=all ;

%fin: ;

%mend FOREST2;
