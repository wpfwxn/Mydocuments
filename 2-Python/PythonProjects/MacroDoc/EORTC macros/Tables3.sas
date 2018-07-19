/************************************************************************************
*
* TABLES3.SAS
* ***********
*
* Macro to produce two different types of tables:
*  1. For categorical variables, the macro produces a table displaying frequency counts
*     and percentages of patients in each category.  (Percentages are based on the number
*     of patients in each group.)
*  2. For numeric variables, the macro produces a table displaying summary statistics.
      (By default, the median, range and number of observations are output.)
*
* This version has an option to produce output as a Word document using ODS.
*
* Version date    : 10 March 2015
* Software        : SAS version 9.4
* Original author : Laurence Collette
* Modifications   : Jérôme RAPION
*
*************************************************************************************
*
* HISTORY
*
*      April 1999   Tables macro (Laurence Collette)
*    Revisions
*      Aug. 2000    (Laurence Collette)
*      March 2002   Added option to produce ODS output.
*                   Removed format parameter.
*                   (Kate Moncrieff)
*      April 2002   Modifications to ensure 'Unknown' and 'Missing' categories appear last.
*                   (Kate Moncrieff)
*      July 2002    Updates to documentation in program header. (KM)
*      October 2002 For ODS output, to get round the problem of titles disappearing
*                   when STARTPAGE=NO, title now forms part of table.
*                   For ODS output, program adapted so that when a table goes over more
*                   than one page, the page breaks appear without splitting variables.
*      Feb 2003     Added parameters MEAN and QUANTILES (see below).
*      Apr 2003     Modified to remove an e\rror sometimes occurring when "var" variable
*                   is numeric and has a long format name (8 chars). (KM)
*      May 2003     Added statements to convert MEAN and QUANTILES to uppercase. (KM)
*                   Correction to avoid taking log10 of zero. (KM)
*      June 2003    Correction to remove e\rror when all observations have missing &var. (KM);
*      August 2003  Correction to avoid e\rror when var labels contain special characters (KM);
*      Sept 2003    Modification to ensure "Missing" appears instead of "~~" for patients with
*                   unknown category. (KM)
*      Jan 2005     Update for SAS9: removed lines that check for format name length of 7 to
*                   get proper functioning in new version. (JBO)
*      May 2005     DEBUG : change to handling of format of output variable: format of last obs
*                   replaced by most precise format of all table lines (LCO)
*      June 2006    Correction : change to avoid the display of "Missing" instead of "Unknown" 
*					format value for variable coded ".U" for the column variable (&BY variable)
*					in the case of ODS RTF Output.
*                   Coding of missing and unknown value in two different cases instead of one 
*					case for missing values to distinguish '.' and '.U' (JRA)
*      July 2006    Correction : change to avoid a bug with &BY variable with "," (comma) in its
*					format. (For expample a categorie = " 1. yes,temporary inter." in a &BY
*                   variable. Use of %NRSTR function in the call of %QSCAN function (JRA)
*      Nov 2006     Change of Template of TABLE (new definition of proc template)
*                   New design of table (with a new style (EORTCstyle1))
*                   Add of HEADER parameter to keep or remove label of BY VARIABLE
*      Dec 2006     Remove of 'Variable' in Header of First Column (JRA)
*      Jan 2006     Add of BOTH parameter to let to display Median & Mean results
*                   in the same call of TABLES2. It is also possible to have Q1Q3 with mean 
*                   results in the same time - See notes below  (JRA)
*    March 2007     To keep format of BY variable (and avoid apparition of @ characters) (JRA)
*    June  2007     Deletion of a wrong definition of a macro variable NB - index of a loop (JRA)
*    Oct   2007     Add of REDUCE parameter to let the user have big tables with a lot of variables 
*                   displayed in one page
*                   Add of PCT parameter to let the user delete the display of percentages 
*                   for categorical variable 
*                   Add of ORDFREQ parameter to let the user order values by descending frequency count
*                   for categorical variable
*                   The use of BOTH parameter without using MEAN=Y is now possible. It means that if
*                   BOTH is set to Y, MEAN is automatically set to Y
*                   Add of UPCASE function for each parameter using Y/N, it means that a user may 
*                   use also y/n in lowcase without any error message. (JRA)
*   April 2010      Because of SAS 9.2 upgrade, replacement of tabulation by spaces, because tabulation
*		            are not recognized in SAS 9.2 (JRA)
*   Aug 2010        Upcase the value of ODS parameter (JRA)
*   June 2012       Because of SAS 9.3 upgrade, do the reverse as in 2010 for SAS 9.2 uprade:
*                   replacement of spaces by 1 tabulation (JRA)
					Beacause of SAS 9.3 update, modification of proc univariate and GMAX by proc means to remove 
					unexpected titles "PROC UNIVARIATE" in case of use of TABLES 2 with ODS RTF with BODYTITLE in SAS 9.3
*  March 2013       Replacement of GLAST by GLAST2 in the code (JRA);
*                   Modification of VarFmtNAme definition to be compatible with new version of SAS 9.3 TSM2 32 bit w7
*  March 2015       Move to TABLES3. (JRA)
*                   Change default value for ODS parameter from ODS=N to ODS=Y 
*                   Add the possibility to put a character variable into BY parameter 
*                   Add of MISSING parameter to let the user to exclude the MISSING (MISSING=OUT);
*                   Add of REGROUPMISS parameter to let the user to regroup all missing codes into one missing code 'Missing';
*                   Add of CATALOG parameter to let the user using format from CATALOG;
*                   Add of ADDCOLUMN parameter  to obtain an empty column at the right of the table that the user
*                   can fill with the appropriate p-value;
*                   Change of PCT parameter:
*                   Remove of RowPercent parameter - Replacement by PCT = ROW
*                   Add the possibility of PCT = CELL and PCT = COL (default)
*                   if PCT = N then no percent displayed.
*
*                   Resolution of bugs : 
*                   1- low - high
*                   2- VAR = X , BY = X , TOTAL=Y
*                   3- if NUM=Y, one level in BY variable, and TOTAL=N, deletion of 'Result1' bug
*                   4- deletion of %%
*
*
*************************************************************************************
*
* PARAMETERS
*
*    DATA      = Input raw data set
*    VAR       = Variable to analyse
*    BY        = By variable (trt)
*    WHERE     = Where conditon
*    TITLE     = Title to be printed on top of the table
*    NUM       = Y if it is a numeric variable and median range are to be presented
*    TOTAL     = Y if a column with totals is required
*    ODS       = Y if ODS output required (default is Y)
*    ODSFile   = The name of the 'rtf' file which is to contain the ODS output
*                (optional - see note below)
*    MEAN      = Y if mean and SD required instead of median and range (default is N)
*                (Only effective if NUM=Y)
*    BOTH      = Y if you want to display Mean and Median in the same time (default is N)
*                (Only effective if NUM=Y)
*    QUANTILES = Y if inter-quartile range required (default is N)
*                (Only effective if NUM=Y)
*    HEADER    = Y if you want to keep label of BY variable (or TRT variable)
*                N if you want to remove it. (default is Y)
*    ORDFREQ   = Y if you want to order values by descending frequency count for categorical variable
*                (default is N)
*    REDUCE    = Y if you want to have up to 20 colomns in a table (20 levels in a &BY categorical variable) 
*                (See notes, default is N)
*    PCT       = COL | ROW | CELL |N
*                N if you want to delete display of percentages for categorical variable (See notes, default is COL)
*    MISSING   = IN | OUT (default is IN)
*                IN: if you want to compute the percentages including all missing values in the TOTAL 
*                OUT: if you want to compute the percentages excluding all missing values in the TOTAL 
*   REGROUPMISS= Y if you want to regroup all missing values (Missing,  Unknown) into one level (., .U) 
*                which will be labeled 'Missing' (default is Y)   
*   ADDCOLUMN=   Y to obtain an empty column at the right of the table, with 'P-value' header.
*				 The user will be able to fill with the appropriate P-values afterwards (default is N)
*   CATALOG    = Name of the catalog containing formats for your dataset, (See notes, default is WORK)
*               
*************************************************************************************
*
* NOTES
*
*    --- PCT parameter ----
*
*    if PCT=COL, then all percentages displayed are based on the column total : you can add them up to 100% in column
*    if PCT=ROW, then all percentages displayed are based on the row total : you can add them up to 100% in row
*    if PCT=CELL, then all percentages displayed are based on the grand total : you can add them up to 100% for all cells
*    if PCT=N, then percentages are not displayed 
*    By default, PCT=COL
*
*    --- CATALOG parameter ----
*    
*    If the user gets a format CATALOG and wants to apply this format CATALOG to the input dataset without running PROC FORMAT,
*    the user can specify the name of this catalog in CATALOG parameter;
*    By default, this catalog is contained in WORK library. (CATALOG=WORK)
*
*    --- Reduce=Y ----
*
*	 In some specific cases, it may be interesting to have a Cross Table with a BY variables containing more than 10
*    levels. (upper limit=20 levels without use of TOTAL=Y option or 19 levels with the use of TOTAL=Y option)
*    In this case, you should use EORTCStyle3 style in an ODS RTF statment with OPTION ORIENTATION=LANDSCAPE.
*
*    Example: if you have a &BY variable called Y with 15 levels for instance;
*    
*    OPTIONS ORIENTATION=LANDSCAPE; 
*	 ODS RTF File="C:\TEMP\TEST.rtf" STYLE=EORTCStyle3;
*		%TABLES2(ODS=Y,DATA=Data1,VAR=x,BY=y,TOTAL=Y);
*    ODS RTF CLOSE;
*
*    In this case all the table will be displayed in one page.
*    WARNING: if you don't use ORIENTATION=LANDSCAPE and EORTCStyle3, it will not be the case anymore.
*
*    --- NUM=Y CASES ----
*	 with the new option BOTH, it is possible to identify now 6 cases of display 
*    for numerical variable :
*
*	   -- case 1 --
*      NUM = Y , Mean = N , Quantiles = N , Both = N
*      in this case, it displays: Median - Range - N
*
*	   -- case 2 --
*      NUM = Y , Mean = Y , Quantiles = N , Both = N
*      in this case, it displays: Mean - N
*
*	   -- case 3 --
*      NUM = Y , Mean = Y , Quantiles = N , Both = Y
*      in this case, it displays: Median - Range - Mean - N
*
*	   -- case 4 --
*      NUM = Y , Mean = Y , Quantiles = Y , Both = Y
*      in this case, it displays: Median - Range - q1q3 - Mean - N
*
*	   -- case 5 --
*      NUM = Y , Mean = N , Quantiles = Y , Both = N
*      in this case, it displays: Median - Range - q1q3 - N
*
*	   -- case 6 --
*      NUM = Y , Mean = Y , Quantiles = Y , Both = N
*      in this case, it displays: Mean - q1q3 - N
*
*    --- ODS OPTIONS ----
*
*    Use ODS=Y if you wish to send the output to a Word document in rich text format (rtf).
*    The ODSFile parameter should contain the name of the rtf file to which you wish to send the
*    output.  However, you can choose to take control yourself of opening and closing ODS
*    destination(s) by leaving the ODSFile parameter blank.  (This could be useful if you wish to
*    call the macro several times and send all the results to the same file.)
*
*    For Example:
*
*    To put several tables in the same file:
*
*           ODS RTF FILE='C:\TEMP\TABLE.RTF' STYLE=EORTCStyle STARTPAGE=NO;
*           %TABLES2(Data1,x,Treat,,'Title',N,Y,ODS=Y);
*           %TABLES2(Data2,y z,Treat,,'Title',N,Y,ODS=Y);
*           ODS RTF CLOSE;
*
*    To put each table in a separate file:
*
*           %TABLES2(Data1,x,Treat,,'Title',N,Y,ODS=Y,ODSFile=%STR(c:\Temp\Table1.rtf));
*           %TABLES2(Data2,y z,Treat,,'Title',N,Y,ODS=Y,ODSFile=%STR(c:\Temp\Table2.rtf));
*
*    The macro uses the format already assigned to the BY= variable for the column headers.  Use the
*    "@" character in the format definition to split a column header over more than one line.
*
*    For example:  PROC FORMAT;
*                  VALUE TREAT 1='Re-Induction@Chemotherapy' 2='Combined Taxotere@and CPT11';
*                  RUN;
*
*    If you do choose to open and close the ODS destinations yourself, please note:
*
*    1. Add "STYLE=EORTCStyle" to your ODS statement to use the standard (pre-defined**) EORTC table style.
*       This style is preferable to the default table style as the default style has fairly small margins.
*
*       For example:
*       ODS RTF FILE='C:\TEMP\TABLE.RTF' STYLE=EORTCStyle;
*
*       ** Note: There is no longer any need to define the style yourself using PROC TEMPLATE as the style
*                is now defined in the program K:\SAS\Start Up\EORTCAutoExec.SAS.  (This program can be
*                run automatically at the start of every SAS session by including the line:
*                %INCLUDE "K:\SAS\Start Up\EORTCAutoExec.SAS"; in your own AutoExec.sas.)
*
*
*    2. In order to avoid starting a new page (and a new section) each time the macro
*       is called, add STARTPAGE=NO to your ODS statement.
*
*       For example:
*       ODS RTF FILE='C:\TEMP\TABLE.RTF' STYLE=EORTCStyle STARTPAGE=NO;
*
*
*  Note: At present there is a bug in SAS which causes titles to disappear when STARTPAGE is set to NO.
*        (See http://www.sas.com/service/techsup/unotes/SN/005/005620.html.)
*        To get round this problem, this macro has been adapted so that the table title forms part of the
*        table itself.  Following this modification, it is no longer necessary to use the BODYTITLE
*        option in your ODS statement as was previously recommended.  Infact it's safer not to use BODYTITLE.
*       (If you do use BODYTITLE, you need at least one of the DATE and NUMBER system options switched on.
*        If you have both NODATE and NONUMBER, the Word document created will not be readable - SAS bug.)
*
************************************************************************************/

%macro Tables3(data,var,by,where,title,num,total,header=Y,ODS=Y,ODSFile=,Mean=N,Both=N,Quantiles=N,
ordfreq=N,reduce=N,Pct=COL,missing=IN, ADDCOLUMN=N, CATALOG=WORK, REGROUPMISS=Y) ;

%if (%length(&where)=0) %then %let where=1 ;
%if (%length(&num)=0) %then %let num=N ;
%if (%length(&by)=0) %then %let by=trt1 ;
%if (%length(&total)=0) %then %let total=N ;
%if (%length(&Header)=0) %then %let Header=Y;

*Sept2007, JRA, add of upcase statement for NUM parameter;
%let num=%upcase(&num) ;
%let total=%upcase(&total) ;
%let ordfreq=%upcase(&ordfreq) ;
%let reduce=%upcase(&reduce) ;
%LET Mean=%UPCASE(&Mean.);
%LET Quantiles=%UPCASE(&Quantiles.);
%LET BOTH=%UPCASE(&BOTH.);
%LET PCT=%UPCASE(&PCT.);
%LET missing=%UPCASE(&missing.);
%let ADDCOLUMN=%upcase(&ADDCOLUMN) ;
%let REGROUPMISS=%upcase(&REGROUPMISS) ;

*Aug2010, JRA, upcase the value of ODS parameter;
%LET ODS=%UPCASE(&ODS.);
*Sept2007, JRA, if BOTH parameter is used, MEAN is set to Y automatically;
%IF &BOTH.=Y %then %let Mean=Y;
%let PS=54 ;
%let varlist=&var;
%let i=1 ;

data _null_ ;
set &data. (obs=1);
length fmtname format $20;
fmtname=trim(left(vformatn(&by.)));
if fmtname='$' then format='$'||trim(left(put(vformatw(&by.),3.)));
else format=trim(left(vformatn(&by.)));
if format="F" or format="BEST" then format="8";
call symput('format',trim(left(format))||'.');
call symput('form',trim(left(format)));
call symput('fmtname',trim(left(fmtname)));
keep format fmtname &by.;
run;

%put format &format form &form fmtname &fmtname;


* Macro used to seperate words in several macro variables (&&COMPTE&N) ;
* from a SUITE of words (in SEP) seperated by a seperator SEP ;
* and to count the number of words (in N) - JRA, Oct2006*;

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

%let MAXSIZ=10.5;

* Handling of format of &BY macro variable -;
* In particular, if the format contains " " then put of "@" : indicator of separation ;
* Storing also of maximal width of format in MAXSIZ - JRA, OCT 2006;
 
%IF &form. ne 8 AND &form. ne BEST and &fmtname ne $ %then %do;

	proc format lib=&catalog. cntlout=__format;
	run;

	data __format;
		set __format;
		label=COMPBL(label);
		n=length(label);
		where FMTNAME="&form." and START ne "~~";
		%if &ODS.=Y %then %do;
			LABEL=SUBSTR(TRANWRD(label," ","@"),1,n);;
		%end;
	run;

	data _null_ ;
	set __format end=final;
	m+1;
	call SYMPUT('labform'||compress(m),compress(label));
	call SYMPUT('valform'||compress(m),compress(START));
	if final then call symput('NB',compress(m));
	run;

	%put NB &nb;

	proc format ;
		value &form. %DO J=1 %TO &NB.; &&valform&J.="&&labform&J." %END;
		;
	run;

	** To avoid problems with a &BY variable containg "," into his format value **;
	%DO J=1 %TO &NB.;

		data __q&j;
		i=&j;
		z="&&&labform&J.";
		output;
		run;

	%END;

	data __q;
		length zz z $100.;
		set %DO J=1 %TO &NB.; __q&j %end;
		;
		** To avoid problems with a &BY variable containg "," into his format value **;
		zz=tranwrd(z, ",", " ");
	run;

	%DO J=1 %TO &NB.;

		data _null_;
		set __q;
			if i=&j then call SYMPUT('llabform'||compress(i),compress(zz));
		run;

		%put llabform&j &&llabform&j.;
		%RCHV(SUITE=&&llabform&J.,COMPTE=V&J,SEP="@",N=N&J.);
	%END;

	data __s;
		%DO J=1 %TO &NB.;
		length text  $250.;
			%DO K=1 %TO &&N&J.;
				text="&&V&J&K.";
				u=&J.;
		        patid=1;output;
			%END;
		%END;
	run;
%END;

** Handling of space line in header of columns **; 

proc datasets library=&catalog nolist;
delete __tabt;
quit;

proc freq data=&data noprint ;
tables &by / out=__tabt sparse;
where &where ;
run;

%let Ncat=0 ;

data __tabt ;
set __tabt end=lastobs ;
retain total 0 ;
call symput('Ntot'||compress(put(_N_,8.)),compress(put(count,8.))) ;

** JRA, 16-02-2015, Add of the possibility to have a char var into &BY parameter;
%IF &fmtname ne $ %THEN %DO; ** JRA, 16-02-2015, if &BY is not a char variable;
	if (&by.=.) then call symput('Header'||trim(left(put(_n_,8.))),
	                                  'Missing@(N='||trim(left(put(count,8.)))||')');
	else if (&by.=.U) then call symput('Header'||trim(left(put(_n_,8.))),
	                                  'Unknown@(N='||trim(left(put(count,8.)))||')');
	else call symput('Header'||trim(left(put(_n_,8.))),
	                 trim(left(put(&by.,&format.)))||'@(N='||trim(left(put(count,8.)))||')');
	total=sum(total,count) ;


	if lastobs then do ;
	 call symput('Ncat',compress(put(_N_,8.))) ;
	 call symput('Max', compress(put(&by,best8.))) ;
	 call symput('GTotal', compress(put(total,8.)));
	 call symput('Header'||trim(left(put(_n_+1,8.))),
	             'Total@(N='||trim(left(put(total,8.)))||')');
end ;
%END;

%IF &fmtname = $ %THEN %DO;** JRA, 16-02-2015, if &BY is a char variable;
	if (&by.="") then call symput('Header'||trim(left(put(_n_,8.))),
	                                  'Missing@(N='||trim(left(put(count,8.)))||')');
	else call symput('Header'||trim(left(put(_n_,8.))),
	                 trim(left(put(&by.,&format.)))||'@(N='||trim(left(put(count,8.)))||')');
	total=sum(total,count) ;

	if lastobs then do ;
	 call symput('Ncat',compress(put(_N_,8.))) ;
	 call symput('Max', trim(left(put(&by,&format.)))) ;
	 call symput('GTotal', compress(put(total,8.)));
	 call symput('Header'||trim(left(put(_n_+1,8.))),
	             'Total@(N='||trim(left(put(total,8.)))||')');
end ;
%END;

run ;

*%put Ntot1 &Ntot1 Ntot2 &Ntot2 GTotal &GTotal.;

** JRA, 18022015: Add of BQUOTE function to solve problem of some value with a quote inside;
%do k = 1 %to &Ncat;
	%let Header&k=%bquote(&&Header&k);
%end; 
%let Max=%bquote(&Max);

%put &Header1 &Header2 &Header3 &Header4 &Ncat &Max &GTotal  ;

%IF &form. ne 8 AND &form. ne BEST and &fmtname ne $  %then %do;
	%DO J=1 %TO &NB.;
		%RCHV(SUITE=&&llabform&J.,COMPTE=VV&J,SEP="@",N=NN&J.);
	%END;

	data __z;
		%do A=1 %TO &NB;
			a&a.=&&NN&a;
			b&a.="&&llabform&a.";
			c&a.="&&labform&a.";
		%end;
		%LET NNB=%EVAL(&NB-1);
		%IF &NNB ne 0 %then %do;  max=max( %DO A=1 %TO &NNB; a&a. , %END; a&NB.); %end;

		%IF &NNB = 0 %then %do; max=a1;%end;

		%do A=1 %TO &NB;
			diff&a.=max-a&a.;
		%end;
		output;
	run;

	data _null_;
		set __z;
		%do B=1 %TO &NB;
			call symput ("diff&b.",diff&b.);
		%end;
	run;
%END;


%if %eval(&Ncat)=0 %then %goto fin ;

%IF &ODS.=N %THEN %DO;

   %if (&total=Y) %then %do ;
      %let dist=%eval(75/(&Ncat+1)) ;
      %let last=%eval(&dist+(&Ncat+1)*15+12);
      %end ;
   %else %do ;
      %let dist=%eval(75/&Ncat) ;
      %if %eval(&Ncat)=1 %then %let dist=35;
      %let last=%eval(&dist+&Ncat*15+12);
      %end ;
   %let LS=%eval(&last+10) ;

%END;

%if (&total=Y) %then %let Ncat=%eval(&Ncat+1) ;


****  DO LOOP TO CHECK IF &VARLIST is EMPTY *****;

%DO %UNTIL (%scan(&varlist,%eval(&i),%str( ))=) ;

   %if (%eval(&i)=1) %then %let first=Y ; %else %let first=N ;
   %let var=%UPCASE(%scan(&varlist,%eval(&i),%str( ))) ;
   %let i=%eval(&i+1) ;

   proc datasets library=&catalog. nolist ;
   delete __tab
   %if (&Num=Y) %then %do ;
   __sorted
   %end ;
   %if (&total=Y) %then %do ;
   __tab2
   %end ;
   ;
   quit;

   %put var &var.;

   * Get format of current variable;
   data _null_;
   set &data (obs=1);
   length VarFmt VarFmtName VarFmtWidth $20 VarFmtType $4; 
   *Sept2007, JRA, Change of the lenght of  VarFmt VarFmtName VarFmtWidth variable from 10 to 20;
   VarFmt=vformat(&var.);
   VarFmtName=vformatn(&var.);
   VarFmtWidth=put(vformatw(&var.),8.);
   call symput('VarFmt',trim(left(VarFmt)));
   call symput('VarFmtName',trim(left(VarFmtName)));
   call symput('VarFmtWidth',trim(left(VarFmtWidth)));
   if VarFmtName in ('$') then VarFmtType='SASC';
   else if VarFmtName in ('BEST','F','DDMMYY','DATE') then VarFmtType='SASN';
   else if VarFmtName^='' then VarFmtType='User';
   call symput('VarFmtType',trim(left(VarFmtType)));
   run;

 ***********************************************;
 ***  CASE OF CATEGORICAL DATA : NUM NE Y   ****;
 ***********************************************;
   %IF (&num^=Y) %THEN %DO ;

	data _data;
		set &data;
		**JRA, 19FEB2015, solve bug in case by = var;
		%IF &BY=&VAR %THEN _&by=&by;;
	run;


	%IF &REGROUPMISS=Y %THEN %DO;
	
		data _data;
		set _data;
			%IF &VarFmtType=SASN or &VarFmtType=User %THEN %DO;
				if missing(&VAR) then &VAR=.;
			%END;
			%IF &VarFmtType=SASC %THEN %DO;
				if missing(&VAR) then &VAR="";
			%END;

		run;

	%END;

	%IF &MISSING=OUT %THEN %DO;

		data _data;
			set _data ;
			%IF &VarFmtType=SASN or &VarFmtType=User %THEN %DO;
				if missing(&VAR) then &VAR=.;
			%END;
			%IF &VarFmtType=SASC %THEN %DO;
				if missing(&VAR) then &VAR="";
			%END;
		run;

	%END;

	* JRA, 16FEB2015, Possibility to include or exclude missing data;
	%IF &MISSING=IN %THEN %DO;
      proc freq data=_data noprint ;
		**JRA, 19FEB2015, solve bug in case by = var;
		%IF &BY=&VAR %THEN tables &var*_&by / norow nopercent missing out=__tab sparse outpct;;
 		%IF &BY ne &VAR %THEN tables &var*&by / norow nopercent missing out=__tab sparse outpct;;
     where &where ;
      run;
	%END;

	%IF &MISSING=OUT %THEN %DO;
      proc freq data=_data noprint ;
		**JRA, 19FEB2015, solve bug in case by = var;
		%IF &BY=&VAR %THEN tables &var*_&by / norow nopercent out=__tab sparse outpct;;
 		%IF &BY ne &VAR %THEN tables &var*&by / norow nopercent out=__tab sparse outpct;;
      where &where ;
      run;
	%END;

      %if (&total=Y) %then %do ;


	* JRA, 16FEB2015, Possibility to include or exclude missing data;
 		%IF &MISSING=IN %THEN %DO;
	        proc freq data=_data noprint ;
	         tables &var / missing  out=__tab2 sparse;
	         where &where ;
	         run;
		%END;
 		%IF &MISSING=OUT %THEN %DO;
	        proc freq data=_data noprint ;
	         tables &var / out=__tab2 sparse;
	         where &where ;
	         run;
		%END;

         data __tab2 ;
         set __tab2 ;

			%IF &fmtname ne $ %THEN %DO; ** JRA, 16FEB2015, if &BY is not a char variable;
				**JRA, 19FEB2015, solve bug in case by = var;
		         %IF &BY=&VAR %THEN _&by=%eval(&max+1) ;;
		         %IF &BY ne &VAR %THEN &by=%eval(&max+1) ;;
			%END;

			%IF &fmtname = $ %THEN %DO; ** JRA, 16FEB2015, if &BY is a char variable;
				**JRA, 19FEB2015, solve bug in case by = var;
		         %IF &BY=&VAR %THEN _&by="ZTotal" ;;
		         %IF &BY ne &VAR %THEN &by="ZTotal" ;;
		    %END;

         %if &PCT=COL %THEN PCT_COL=PERCENT ;;
         %if &PCT=ROW %THEN PCT_ROW=PERCENT ;;

         run ;

         data __tab ;
         set __tab __tab2 ;
         run ;

		 %IF &BY=&VAR %THEN %sort(data=__tab,var=&var _&by);;
		 %IF &BY ne &VAR %THEN %sort(data=__tab,var=&var &by);;
			
		 	** Add of ORDFREQ parameter: TABLES results may be ordered by frequence counts;
			%if &ordfreq=Y %then %do;

				 data __tabord;
				 	set __tab;

					%IF &fmtname ne $ %THEN %DO; ** JRA, 16FEB2015, if &BY is not a char variable;
				**JRA, 19FEB2015, solve bug in case by = var;
				         %IF &BY=&VAR %THEN if _&by.=%eval(&max+1);;
				         %IF &BY ne &VAR %THEN if &by.=%eval(&max+1);;
					%END;

					%IF &fmtname = $ %THEN %DO; ** JRA, 16FEB2015, if &BY is a char variable;
				**JRA, 19FEB2015, solve bug in case by = var;
				         %IF &BY=&VAR %THEN if _&by.="ZTotal" ;;
				         %IF &BY ne &VAR %THEN if &by.="ZTotal" ;;
					%END;

				 run; 

		         proc sort data=__tabord ;
		         by descending COUNT;
		         run ;

				 data __tabord ;
				 	set __tabord ;
					ord=_n_;
				 run;

				**JRA, 19FEB2015, solve bug in case by = var;
				%IF &BY=&VAR %THEN %sort(data=__tabord,var=&var _&by);;
				%IF &BY ne &VAR %THEN %sort(data=__tabord,var=&var &by);;
				
				 data __tab;
				 	merge __tab __tabord(keep=&var ord);
					by &var;
				 run;

				**JRA, 19FEB2015, solve bug in case by = var;
				%IF &BY=&VAR %THEN %sort(data=__tab,var=ord &var &by);;
				%IF &BY ne &VAR %THEN %sort(data=__tab,var=ord &var &by);;


			 %end;

      	%end ;

	  %else %do;

   		%if &ordfreq=Y %then %do;

	* JRA, 16FEB2015, Possibility to include or exclude missing data;

 		%IF &MISSING=IN %THEN %DO;

	      	 proc freq data=_data noprint ;
	         tables &var / missing out=__tabord sparse;
	         where &where ;
	         run;

		%END;

 		%IF &MISSING=OUT %THEN %DO;

	      	 proc freq data=_data noprint ;
	         tables &var / out=__tabord sparse;
	         where &where ;
	         run;

		%END;

	         data __tabord ;
	         set __tabord ;

				%IF &BY=&VAR %THEN _&by=%eval(&max+1) ;;
				%IF &BY ne &VAR %THEN &by=%eval(&max+1) ;;
	         
	         run ;

			 proc sort data=__tabord ;
		         by descending COUNT;
		     run ;

	         data __tabord ;
	         set __tabord ;

					%IF &fmtname ne $ %THEN %DO; ** JRA, 16-02-2015, if &BY is not a char variable;

				**JRA, 19FEB2015, solve bug in case by = var;
						%IF &BY=&VAR %THEN if _&by.=%eval(&max+1);;
						%IF &BY ne &VAR %THEN if &by.=%eval(&max+1);;

					%END;

					%IF &fmtname = $ %THEN %DO; ** JRA, 16-02-2015, if &BY is a char variable;

				**JRA, 19FEB2015, solve bug in case by = var;
						%IF &BY=&VAR %THEN if _&by.="ZTotal";;
						%IF &BY ne &VAR %THEN if &by.="ZTotal";;

					%END;
	         run ;

			 proc sort data=__tabord ;
		         by descending COUNT;
		     run ;

			 data __tabord ;
			 	set __tabord ;
				ord=_n_;
			 run;

			 %sort(data=__tabord,var=&var &by);
			 %sort(data=__tab,var=&var &by);

			 data __tab;
			 	merge __tab __tabord(keep=&var ord);
				by &var;
			 run;

		**JRA, 19FEB2015, solve bug in case by = var;
		   %IF &BY=&VAR %THEN %sort(data=__tab,var=ord &var _&by);;
		   %IF &BY ne &VAR %THEN %sort(data=__tab,var=ord &var &by);;
	 

		 %end;
		 %else %do;

		**JRA, 19FEB2015, solve bug in case by = var;
		   %IF &BY=&VAR %THEN %sort(data=__tab,var=&var _&by);;
		   %IF &BY ne &VAR %THEN %sort(data=__tab,var=&var &by);;

		%end;
	  %end;

      * Make sure variable analysed is character...;

      data _null_;
      set __tab;
      call symput('VTYPE',trim(left(vtype(&var.))));
      run;

	  %LET CHECK_FMT=;

	  %PUT VTYPE &VTYPE.;  	

	  ** HANDLING OF FORMATS **;
      %IF &VTYPE.=N %THEN %DO; 

         data _temp_ ;
         set __tab end=last;
         length VarLabel $200;
         call label(&var.,VarLabel);
         call symput('VarLabel', trim(left(VarLabel)));

            if missing(&var.) then do;
               __d=0;
               __w=1;
            end;
            else do;
               if floor(&var.)=fuzz(&var.) then __d=0;
               else if round(&var.,0.1)=round(&var.,0.001) then __d=1;
               else __d=2;
               if fuzz(&var.)=0 then __w=3+__d;
               else __w=floor(log10(&var.)+1)+2+__d;
            end;

     run;

     proc means noprint data=_temp_ ;
     var __d __w ;
     output out=__temp max=__d __w ;
     run ;

     data __temp ;
     set __temp ;
            call symput('w', trim(left(put(__w,3.))));
            call symput('d', trim(left(put(__d,1.))));
     run ;

	 proc datasets library = &catalog. nolist ;
         delete _temp_ __temp ;
         run ;
         quit ;

         data __tab;
         %IF %EVAL(&w.<2) %THEN %STR(length &var. $2;);
         %ELSE %STR(length &var. $&w.;);
         set __tab (rename=(&var.=Original));
         if Original=. then &var.='~~';
         else if missing(Original) then &var.='~'||put(Original,1.);
         else &var.=put(Original,&w..&d.);
         label &var.="&VarLabel.";
         run;

		 ** JRA, 06052015, add of a sort by ord in case of ORDFREQ=Y;
		 %if &ordfreq=Y %then %do;
			 %sort(data=__tab,var=ord &var. &by.);
		 %end;
		 %if &ordfreq=N %then %do;
			 %sort(data=__tab,var=&var. &by.);
		 %end;

         * Update format too;

         %IF &VarFmtType.=%STR(User) %THEN %DO;

            proc format cntlout=__UserFmt library=&catalog.;
            select &VarFmtName.;
            run;

			** JRA, 19FEB2015, resolution of bug with LOW - HIGH formats;
			data _null_;
				set __UserFmt;
				if _n_=1 then do;

					call symput('CHECK_FMT',compress(start));
				end;
			run;

			%put CHECK_FMT &CHECK_FMT.;

			** JRA, 19FEB2015, resolution of bug with LOW - HIGH formats;
			%IF &CHECK_FMT NE LOW %THEN %DO;

				data __UserFmt;
					set __UserFmt;
					if FMTNAME="&VarFmtName." then FMTNAME="N"||"&VarFmtName.";
				run;

	            data _null_;
	            set __UserFmt;
	            call symput('LabelLength',put(vlength(Label),3.));
				call symput('VarFmtName',FMTNAME);
	            run;

	            data __UserFmt;
	            %IF %EVAL(&LabelLength.<7) %THEN %STR(length Label $7;);
	            set __UserFmt end=LastObs;
	            retain AnyMissingLabel (0);
	            if missing(input(compress(start),2.)) and compress(start)=compress(end) then do;
	              if compress(start)='.' then do;
	                start='~~';
	                AnyMissingLabel=1;
	              end;
	              else start='~'||compress(start,' .');
	              end=start;
	            end;
	            else do;
	               if &d.>0 then IntWidth=&w.-&d.-1;
	               else IntWidth=&w.;
	               if index(start,'.') then NStartSpaces=IntWidth-(index(left(start),'.')-1);
	               else NStartSpaces=IntWidth-length(left(start));
	               if NStartSpaces>0 then start=repeat(' ',NStartSpaces-1)||trim(left(start));
	               else start=trim(left(start));
	               if index(end,'.') then NEndSpaces=IntWidth-(index(left(end),'.')-1);
	               else NEndSpaces=IntWidth-length(left(end));
	               if NEndSpaces>0 then end=repeat(' ',NEndSpaces-1)||trim(left(end));
	               else end=trim(left(end));
	            end;
	            fuzz=0;
	            type='C';
	            default=max(default,7);
	            length=max(length,7);
	            output;
	            if start=end and &d.>0 and not index(start,'.') and not index(start,'~') then do;
	               start=trim(start)||'.'||repeat('0',&d.-1);
	               end=start;
	               output;
	            end;
	            if LastObs and not AnyMissingLabel then do;
	               start='~~';
	               end=start;
	               label='Missing';
	               output;
	            end;
	            run;

	            proc format cntlin=__UserFmt library=&catalog.;
	            run;

			%END;

			** JRA, 19FEB2015, resolution of bug with LOW - HIGH formats;
			%IF &CHECK_FMT = LOW %THEN %DO;

	            data _null_;
	            set __UserFmt;
	            call symput('LabelLength',put(vlength(Label),3.));
	            run;

			%END;

			* JRA, 25MAR2013, Modification of VarFmtNAme
			from LET VarFmtName = %STR($)%TRIM(%LEFT(&VarFmtName.)) to LET VarFmtName = %TRIM(%LEFT(&VarFmtName.));

            %LET VarFmtName = %TRIM(%LEFT(&VarFmtName.));

         %END;

         %ELSE %IF &VarFmtType.=%STR() %THEN %DO;

            %LET VarFmtName=%STR($MissFmt);
            proc format;
            value $MissFmt '~~'='Missing';
            run;

         %END;

      %END;  ** END OF HANDLING OF FORMATS **;

      %IF &VTYPE.=C %THEN %DO; 

		  data __tab;
		  	set __tab;
			Original=&var;
		  run;


	  %END;


	 ** DISPLAY IN A CASE OF ODS RTF FILE **;

     %IF &ODS.=Y %THEN %DO;

  		%IF &CHECK_FMT ne LOW %THEN %DO;

	        data __VarLength;
	                   set __tab;
	          %IF &VarFmtType.=%STR(SASN) OR &VarFmtType.=%STR(SASC) %THEN %STR(vallength=&VarFmtWidth.;);
	                   %ELSE %IF %LENGTH(&VarFmtName.)>0 %THEN %STR(vallength=length(put(&var.,&VarFmtName..)););
	                   %ELSE %STR(vallength=length(&var.););
	                   keep &var. vallength;
	         run;

         %END;

  		 %IF &CHECK_FMT = LOW %THEN %DO;

	        data __VarLength;
	                   set __tab;
					   vallength = 50 ;
	                   keep &var. vallength;
	         run;

         %END;


		  %sort(data=__Varlength,var=vallength);

         data _null_;
         set __VarLength end=last;
         if last;
         length VarLabel $200;
         call label(&var.,VarLabel);
         VarLength=max(length(VarLabel),vallength+14);
         call symput("VarLength&i.", trim(left(put(VarLength,3.))));
         run;
      
		  %IF &MISSING=OUT %THEN %DO;

			   ***JRA, 18022015 : add of totals into __Output datasets;
			   data __totals;
			   set __tabt;
			   keep &by count;
			   run;

			   %IF &TOTAL = Y %THEN %DO;
				   data __ww;	   
					&by=&max+1;	
					    %IF &fmtname ne $ %THEN %DO; ** JRA, 16-02-2015, if &BY is not a char variable;
						&by=&max+1;	
						%END;
					    %IF &fmtname = $ %THEN %DO; ** JRA, 16-02-2015, if &BY is not a char variable;
						&by.="ZTotal" ;
						%END;

						count=&Gtotal;	   
					run;

				   data __totals;
				     set __totals __ww;
				   run;

			   %END;

			   data __totals;
			     set __totals ;
				 rename count=Ntot;
			   run;

			   %sort(data=__totals,var=&by);
			   %sort(data=__tab,var=&by);

			   data __tab;
			   merge __tab __totals;
			   by &by.;


			 %IF &VarFmtType. ne %STR(SASC) %THEN %DO;
			   if Original=. then do;
				count=Ntot-count;
			   pct_col=count/Ntot*100;
			  %END;

			 %IF &VarFmtType. = %STR(SASC) %THEN %DO;
			  if compress(&var)='' then do;
				count=Ntot-count;
			   pct_col=count/Ntot*100;
			  %END;
			   end;

			   run;
			   %sort(data=__tab,var=original &by);


	 	%IF &VarFmtType.=%STR(SASC) %THEN %DO;
		 	data __tab;
			set __tab;
			if compress(&var)='' then &var=" N obs";
			run;
		 	
		   %sort(data=__tab,var=&var &by);

		 %END;


		**JRA, 09032015: check if there is really missing data or not for &VAR;

		 data _missing;
			 set __tab;
			 if missing(Original);
		 run;
			
		proc contents data=_missing out=_attributes noprint;
		run;

		data _null_;
		set _attributes;
		if _n_=1;
		call symput ('NOBSMISSING',compress(nobs));
		run; 

		%put NOBSMISSING &NOBSMISSING.;

		%IF &NOBSMISSING=0 %then %do;

			data _copytab;			set __tab;			run;

			proc sort data=_copytab out=_copytab2(keep=original) nodupkey;
			by original;
			run;

			data _copytab2;			set _copytab2;			ind=_n_;			run;

			data _fakemissing;
			merge _copytab _copytab2;
			by original;
			if ind=1;
			count=Ntot;
			PCT_COL=100;
			PCT_ROW=.;
			PERCENT=.;

 			%IF &VarFmtType.=%STR(SASN) %THEN %DO;
				Original=. ;
			 %END;
			 %IF &VarFmtType.=%STR(User) %THEN %DO;
				 Original=. ;
			 %END;
			 %IF &VarFmtType.=%STR(SASC) %THEN %DO;
				Original=1 ;
				&var=" N obs";
			 %END;

			run;

			data __tab;
			set  _fakemissing __tab; 
			run;

	
		%end;


		%END;

       data __Output&i.;
         length yy $5.;
         array cnt {&Ncat} ;
         array pct {&Ncat} $7 ;
         array cntpct {&Ncat} $100;
         set __tab end=eof ;
         do i=1 to %eval(&Ncat) ;
           set __tab end=eof ;
           cnt[i]=count ;
		   %if &PCT.=COL %then xx=round(pct_col,0.1) ;;
 		   %if &PCT.=ROW %then xx=round(pct_row,0.1) ;;
 		   %if &PCT.=CELL %then xx=round(percent,0.1) ;;


			 %IF &MISSING=OUT %then %DO;
			 %IF &VarFmtType.=%STR(SASN) %THEN %DO;
				if Original=. then xx=round(pct_col,0.1) ;
			 %END;
			 %IF &VarFmtType.=%STR(User) %THEN %DO;
				if Original=. then xx=round(pct_col,0.1) ;
			 %END;
			 %IF &VarFmtType.=%STR(SASC) %THEN %DO;
				if &var=" N obs" then xx=round(pct_col,0.1) ;
			 %END;
			 %END;


          if (xx=floor(xx)) then yy=compress(put(xx,8.))||'.0' ;
           else yy=compress(put(xx,8.1)) ;
           if (length(compress(yy))<4) then yy=' '||yy ;
           pct[i]=compress('('||yy||')') ;

          cntpct[i] = put(cnt[i],5.)||' '||trim(left(pct[i]));

		   %if &pct.=N %then  cntpct[i] = put(cnt[i],5.);;

		 %IF &MISSING=OUT %then %DO;

			 %IF &PCT =CELL OR &PCT=ROW %THEN %DO;
				 %IF &VarFmtType.=%STR(SASN) OR &VarFmtType.=%STR(User) %THEN %DO;
					if Original=. then cntpct[i] = put(cnt[i],5.);;
				 %END;

				  %IF &VarFmtType.=%STR(SASC) %THEN %DO;
					if &var=" N obs" then cntpct[i] = put(cnt[i],5.);;
				 %END;

			 %end;



		 %END;

         end ;


         length VarLabel $200.;
         call label(&var.,VarLabel);
         length VarValue $&&VarLength&i..;

		 %IF &CHECK_FMT ne LOW %THEN %DO;

	         %IF &VarFmtType.=%STR(SASN) %THEN %DO;
	            if &Var.='~~' then VarValue='Missing';
	            else if substr(&Var.,1,1)='~' then VarValue=&Var.;
	            else do;
	               VarAsNum=input(&var.,best16.);
	               VarValue=put(VarAsNum,&VarFmt.);
	            end;
	         %END;
	         %ELSE %IF &VarFmtType.=%STR(SASC) %THEN %DO;
	            if &Var.='' then VarValue='Missing';
	            else VarValue=put(&var.,&VarFmt.);
	         %END;
	         %ELSE %IF %LENGTH(&VarFmtName.)>0 %THEN %STR(VarValue=put(&var.,&VarFmtName..););
	         %ELSE %STR(VarValue=&var.;);

		%END;

		 %IF &CHECK_FMT = LOW %THEN %DO;
		 		VarValue=put(Original,&VarFmtName..);
		 %END;

		 ** JRA, 06JUN2012, Because of SAS 9.3 upgrade, replacement of spaces by tabulation, because tabulation
		 are again recognized in SAS 9.3 (but it was not the case in SAS 9.2);
         VarValue = '09'x||trim(left(VarValue));

		 %IF &MISSING=OUT %then %DO;

		 %IF &VarFmtType.=%STR(SASN) %THEN %DO;
			if Original=. then VarValue='09'x||'09'x||'09'x||"N obs";
		 %END;
		 %IF &VarFmtType.=%STR(User) %THEN %DO;
			if Original=. then VarValue='09'x||'09'x||'09'x||"N obs";
		 %END;
		 %IF &VarFmtType.=%STR(SASC) %THEN %DO;
			if &var=" N obs" then VarValue='09'x||'09'x||'09'x||"N obs";
		 %END;

		 %END;

	       keep VarLabel VarValue cntpct1 - cntpct&Ncat.;
         run;

      %END;


	  ** DISPLAY IN A CASE OF FILE PRINTER (NO ODS RTF FILE) **;

 	  %IF &ODS. ne Y %THEN %DO; 

		data __tab;
		length &var $25.;
		set __tab;
		run;


		  %IF &MISSING=OUT %THEN %DO;

			   ***JRA, 18022015 : add of totals into __Output datasets;
			   data __totals;
			   set __tabt;
			   keep &by count;
			   run;

			   %IF &TOTAL = Y %THEN %DO;

			   data __ww;	   
				&by=&max+1;	
				    %IF &fmtname ne $ %THEN %DO; ** JRA, 16-02-2015, if &BY is not a char variable;
					&by=&max+1;	
					%END;
				    %IF &fmtname = $ %THEN %DO; ** JRA, 16-02-2015, if &BY is not a char variable;
					&by.="ZTotal" ;
					%END;

					count=&Gtotal;	   
				run;


			   data __totals;
			     set __totals __ww;
			   run;

			   %END;

			   data __totals;
			     set __totals ;
				 rename count=Ntot;
			   run;

			   %sort(data=__totals,var=&by);
			   %sort(data=__tab,var=&by);

			   data __tab;
			   merge __tab __totals;
			   by &by.;

			 %IF &VarFmtType. ne %STR(SASC) %THEN %DO;
			   if Original=. then do;
				count=Ntot-count;
			   pct_col=count/Ntot*100;
			  %END;

			 %IF &VarFmtType. = %STR(SASC) %THEN %DO;
			  if compress(&var)='.' then do;
				count=Ntot-count;
			   pct_col=count/Ntot*100;
			  %END;
			   end;

			   run;
			   %sort(data=__tab,var=original &by);


		 %IF &VarFmtType.=%STR(SASC) %THEN %DO;
		 	data __tab;
			set __tab;
			if compress(&var)='.' then &var="*N OBS";
			run;
		 	
		   %sort(data=__tab,var=&var &by);
		  %END;

		 %IF &VarFmtType.=%STR(User) or &VarFmtType.=%STR(SASN)%THEN %DO;
		 	data __tab;
			set __tab;
			if Original=. then &var="N OBS ";
			run;
		 	
		*   %sort(data=__tab,var=&var &by);
		  %END;


		**JRA, 09032015: check if there is really missing data or not for &VAR;

		 data _missing;
			 set __tab;
			 if missing(Original);
		 run;
			
		proc contents data=_missing out=_attributes noprint;
		run;

		data _null_;
		set _attributes;
		if _n_=1;
		call symput ('NOBSMISSING',compress(nobs));
		run; 

		%put NOBSMISSING &NOBSMISSING.;

		%IF &NOBSMISSING=0 %then %do;

			data _copytab;			set __tab;			run;

			proc sort data=_copytab out=_copytab2(keep=original) nodupkey;
			by original;
			run;

			data _copytab2;			set _copytab2;			ind=_n_;			run;

			data _fakemissing;
			merge _copytab _copytab2;
			by original;
			if ind=1;
			count=Ntot;
			PCT_COL=100;
			PCT_ROW=.;
			PERCENT=.;

 			%IF &VarFmtType.=%STR(SASN) %THEN %DO;
				Original=. ;&var.="N OBS ";
			 %END;
			 %IF &VarFmtType.=%STR(User) %THEN %DO;
				 Original=. ;&var.="N OBS ";
			 %END;
			 %IF &VarFmtType.=%STR(SASC) %THEN %DO;
				Original=1 ;&var.="N OBS ";
			 %END;

			run;

			data __tab;
			set  _fakemissing __tab; 
			run;

	
		%end;


		 %END;

		 data __tab; set __tab; __xxone=1;run;
		 %GMAX (DATA=__tab, VAR=PCT_COL, OUTVAR=maxpct_col,BYVAR=__xxone, OUTDATA=_maxpctcol);
	
		 data _null_;
		 set  _maxpctcol;
		 call symput('maxpct_col',compress(maxpct_col));
		 run;

         data _null_ ;
          length _lab _top $45. ;
          array cnt {&Ncat} ;

		  %if %sysevalf(&maxpct_col=100) %then %do;
          array pct {&Ncat} $7 ;
          length yy $5.;
		  %end;

		  %if %sysevalf(&maxpct_col<100) %then %do;
          array pct {&Ncat} $6 ;
          length yy $4.;
		  %end;

          array title {&Ncat} $20;
          array rt {&Ncat} ;

          set __tab end=eof ;
          do i=1 to %eval(&Ncat) ;
            set __tab end=eof ;
            cnt[i]=count ;
    	   %if &PCT.=COL %then xx=round(pct_col,0.1) ;;
 		   %if &PCT.=ROW %then xx=round(pct_row,0.1) ;;
 		   %if &PCT.=CELL %then xx=round(percent,0.1) ;;

			 if percent=pct_row and percent=. then do;
				 xx=round(pct_col,0.1) ;
			 end;

              if (xx=floor(xx)) then yy=compress(put(xx,8.))||'.0' ;
               else yy=compress(put(xx,8.1)) ;
            if (length(compress(yy))<4) then yy=' '||yy ;

			**JRA, Sept2007, Add of PCT parameter;
			%if &PCT=N %then  pct[i]=' ';
            %else pct[i]='('||yy||')' ;;

			%if &PCT=CELL OR &PCT=ROW %then  %do;
				if &var="N OBS" then pct[i]=' ';
			%end;

            title[i]=put(&by,&format) ;

            %if (&total=Y) %then %do ;
              if (i=%eval(&Ncat)) then title[i]='Total' ;
            %end ;

            if trim(left(title[i]))='.' then title[i]='Missing' ;
            rt[i]=length(trim(left(title[i])))/2 ;

			%IF &MISSING = OUT %then %do;
				if &var ='~~' then delete;
			%end;

          end ;

          %if (&first=Y) %then %do ;
          file print LS=&LS PS=&PS header=pagetop linesleft=lines notitles;
          if lines<=3 then do; put; put @5 &last*'_'; put _blankpage_; end;
          %end ;
          %else %do ;
          file print LS=&LS PS=&PS linesleft=lines notitles;
          %end ;

              call label(&var,_lab) ;
              call label(&by,_top) ;
              rtop=floor(length(trim(left(_top)))/2) ;

              if _N_=1 then do ; put ; put @7 _lab;end ;
              %IF &VarFmtType.=%STR(SASN) %THEN %DO;

                 length VarValue $20;
                 if &Var.='~~' then VarValue='Missing';
                 else if substr(&Var.,1,1)='~' then VarValue=&Var.;
                 else do;
                    VarAsNum=input(&var.,best16.);
                    VarValue=put(VarAsNum,&VarFmt.);

					%IF &MISSING = OUT %then %do;
						if Original=. then VarValue="N OBS";
						if ind=1 then VarValue="N OBS";

					%END;

                 end;

				  if VarValue ="N OBS" then do;
					put @8 VarValue @;;
				  end;
				  else do;
					put @10 VarValue @;;
				  end;
                 

              %END;
              %ELSE %IF &VarFmtType.=%STR(SASC) %THEN %DO;
                 length VarValue $20;
                 if &Var.='' then VarValue='Missing';
                 else VarValue=put(&Var.,&VarFmt.);

				  if VarValue ="N OBS" then do;
					put @8 VarValue @;;
				  end;
				  else do;
					put @10 VarValue @;;
				  end;


              %END;
              %ELSE %DO;

			  if &var ="N OBS" then do;
				%STR(put @8 &var &VarFmtName.. @;);
			  end;
			  else do;
				%STR(put @10 &var &VarFmtName.. @;);
			  end;

			  %END;
			
			  %if %sysevalf(&maxpct_col=100) %then %do;

	              put %do j=1 %to &Ncat ;
	                      @(&dist+&j*15) cnt&j 6. @(&dist+7+&j*15) pct&j $7.-r
	                  %end ;;
	                 ;
			  %end;

			  %if %sysevalf(&maxpct_col < 100) %then %do;

	              put %do j=1 %to &Ncat ;
	                      @(&dist+&j*15) cnt&j 6. @(&dist+7+&j*15) pct&j $6.-r
	                  %end ;;
	                 ;
			  %end;

              if eof then put ; *@5 &last*'_';


          RETURN;
         %if (&first=Y) %then %do ;
             PAGETOP:
	         %if (%length(&title)>0) %then %do ;
	              put   @7 &title;
	         %end ;
              put   @5 &last*'_';
	         if (rtop>1) then do ;
	              put /  @(&dist+15+&Ncat*7-rtop) _top ;
	         end ;
              put
	         %do j=1 %to &Ncat ;
	              @(&dist+7+&j*15-rt&j) title&j
	         %end ;
	         ;
	              put
	         %do j=1 %to &Ncat ;
	         %if (%eval(&j)^=%eval(&Ncat) or &total^=Y) %then %do ;
	              @(&dist+6+&j*15-%length(&&Ntot&j)/2-1) "(N=&&Ntot&j)"
	           %end ;
	         %else %do ;
	              @(&dist+6+&j*15-%length(&Gtotal)/2-1) "(N=&Gtotal)"
	           %end ;
	         %end ;
	         ;
			**JRA, Oct2007, Add of PCT parameter;

              put / 

			%if &PCT.=COL %then %do;
		         %do j=1 %to &Ncat ;
		              @(&dist+3+&j*15) "N"  @(&dist+9+&j*15) "(%)" 
		         %end ;
			%end; 
			%if &PCT.=N %then %do;
		         %do j=1 %to &Ncat ;
		              @(&dist+5+&j*15) "N"  
		         %end ;
			%end; 
			%if &PCT.=ROW %then %do;
		         %do j=1 %to %eval(&Ncat-1) ;
		              @(&dist+3+&j*15) "N"  @(&dist+6+&j*15) "(row %)" 
		         %end ;
		       %if &total=Y %then %do;       @(&dist+3+&j*15) "N"  @(&dist+6+&j*15) "(column %)" %end;
		       %if &total=N %then %do;       @(&dist+3+&j*15) "N"  @(&dist+6+&j*15) "(row %)" %end;
			%end; 
			%if &PCT.=CELL %then %do;
		         %do j=1 %to %eval(&Ncat-1) ;
		              @(&dist+3+&j*15) "N"  @(&dist+6+&j*15) "(cell %)" 
		         %end ;
		       %if &total=Y %then %do;       @(&dist+3+&j*15) "N"  @(&dist+6+&j*15) "(column %)"  %end;
		       %if &total=N %then %do;       @(&dist+3+&j*15) "N"  @(&dist+6+&j*15) "(cell %)"  %end;
			%end; 

         ;
              put @5 &last*'_' /;
                RETURN;
         %end ;
         run;

      %END;

   %END ;

 *******************************************;
 ***  CASE OF NUMERICAL DATA : NUM = Y  ****;
 *******************************************;

   %IF (&num=Y) %THEN %DO ;

      proc sort data=&data out=__sorted ; by &by ;run ;
      data _NULL_ (keep=_lab);
      set __sorted end=lastobs;
      length _lab $200.;
      call label(&var,_lab) ;
      if lastobs then call symput('LAB',trim(left(_lab))) ;
      run;


	**JRA, 11JUN2012, Beacause of SAS 9.3 update, modification of proc univariate by proc means to remove unexpected titles
	"PROC UNIVARIATE" in case of use of TABLES 2 with ODS RTF with BODYTITLE in SAS 9.3;

      proc means data=__sorted noprint;
       by &by ;
       var &var ;
       output out=__tab median=med min=min max=max q1=q1 q3=q3 mean=mean std=std n=nobs;
       where &where ;
       run ;

      %if (&total=Y) %then %do ;

		**JRA, 11JUN2012, Beacause of SAS 9.3 update, modification of proc univariate by proc means to remove unexpected titles
		"PROC UNIVARIATE" in case of use of TABLES 2 with ODS RTF with BODYTITLE in SAS 9.3;

         proc means data=__sorted noprint;
          var &var ;
          output out=__tab2 median=med min=min max=max q1=q1 q3=q3 mean=mean std=std n=nobs;
          where &where ;
          run ;

         data __tab2 ;
         set __tab2 ;

			%IF &fmtname ne $ %THEN %DO; ** JRA, 16-02-2015, if &BY is not a char variable;
		         &by=%eval(&max+1) ;
			%END;

			%IF &fmtname = $ %THEN %DO; ** JRA, 16-02-2015, if &BY is a char variable;
		         &by="ZTotal" ;
			%END;
         run;

         data __tab ;
         set __tab __tab2 ;
         run ;

      %end ;

   ** DISPLAY IN A CASE OF ODS RTF FILE **;
     %IF &ODS.=Y %THEN %DO;

         data __Output&i.;
         array minn {&Ncat} ;
         array maxn {&Ncat} ;
         array medn{&Ncat} ;
         array q1n{&Ncat} ;
         array q3n{&Ncat} ;
         array meann{&Ncat} ;
         array stdn{&Ncat} ;
         array nobsn{&Ncat} ;
         do i=1 to %eval(&Ncat) ;
           set __tab end=eof ;
           minn[i]=round(min,.1) ;
           maxn[i]=round(max,.1);
           medn[i]=round(med,.1) ;
           q1n[i]=round(q1,.1);
           q3n[i]=round(q3,.1);
           meann[i]=round(mean,.01);
           stdn[i]=round(std,.01);
           nobsn[i]=nobs ;
         end ;
         length VarLabel $200.;
         VarLabel="%QTRIM(%QLEFT(%BQUOTE(&lab.)))";
         VarLength=max(length(VarLabel),19);
         call symput("VarLength&i.", trim(left(put(VarLength,3.))));
         keep VarLabel minn1 - minn&Ncat.
                       maxn1 - maxn&Ncat.
                       medn1 - medn&Ncat.
                       q1n1 - q1n&Ncat.
                       q3n1 - q3n&Ncat.
                       meann1 - meann&Ncat.
                       stdn1 - stdn&Ncat.
                       nobsn1 - nobsn&Ncat.;
         run;

      %END;

	  ** DISPLAY IN A CASE OF FILE PRINTER (NO ODS RTF FILE) **;

      %IF &ODS. ne Y %THEN %DO;
         data _NULL_ ;
          length _top $45. ;
          array minn {&Ncat} ;
          array maxn {&Ncat} ;
          array medn{&Ncat} ;
          array q1n{&Ncat} ;
          array q3n{&Ncat} ;
          array meann{&Ncat} ;
          array stdn{&Ncat} ;
          array nobsn{&Ncat} ;

          array title {&Ncat} $20;
          array rt {&Ncat}       ;

          do i=1 to %eval(&Ncat) ;
            set __tab end=eof ;
            minn[i]=round(min,.1) ;
            maxn[i]=round(max,.1);
            medn[i]=round(med,.1) ;
            q1n[i]=round(q1,.1);
            q3n[i]=round(q3,.1);
            meann[i]=round(mean,.01);
            stdn[i]=round(std,.01);
            nobsn[i]=nobs ;
            title[i]=put(&by,&format.) ;
           %if (&total=Y) %then %do ;
            if (i=%eval(&Ncat)) then title[i]='Total' ;
           %end ;

           rt[i]=length(trim(left(title[i])))/2 ;

          end ;
         %if (&first=Y) %then %do ;
          file print LS=&LS PS=&PS header=pagetop linesleft=lines notitles;
          if lines<=3 then do; put; put @5 &last*'_'; put _blankpage_; end;
         %end ;
         %else %do ;
          file print LS=&LS PS=&PS linesleft=lines notitles;
         %end ;

              call label(&by,_top) ;
              rtop=floor(length(trim(left(_top)))/2) ;

              if _N_=1 then do ; put; put @7 "&lab"; end ;

			** CASE 1 : Median - Range - N ;
			   %IF &Quantiles.=N and &MEAN.=N and &BOTH.=N %THEN %DO;

	                  put @10 'Median'
	                %do j=1 %to &Ncat ;
	                  @(&dist+3+&j*15) medn&j
	                %end ;
	                ;
	                  put @10 'Range'
	                %do j=1  %to &Ncat ;
	                 @(&dist+&j*15) minn&j  @(&dist+7+&j*15) maxn&j
	                %end ;
	                ;
				%END;

			** CASE 2 : Mean - N ;
			   %IF &Quantiles.=N and &MEAN.=Y and &BOTH.=N %THEN %DO;

                  put @10 'Mean (SD)' @;
                     %DO j=1 %to &Ncat.;
                        length MeanSDText $20;
                        MeanSDText = trim(left(put(meann&j.,7.2)))||' ('||trim(left(put(stdn&j.,7.2)))||')';
                        put @(&dist.+1+&j.*15) MeanSDText @;
                     %END;
                  put;

				%END;

			** CASE 3 : Median - Range - Mean - N ;
			   %IF &Quantiles.=N and &MEAN.=Y and &BOTH.=Y %THEN %DO;

	                  put @10 'Median'
	                %do j=1 %to &Ncat ;
	                  @(&dist+3+&j*15) medn&j
	                %end ;
	                ;
	                  put @10 'Range'
	                %do j=1  %to &Ncat ;
	                 @(&dist+&j*15) minn&j  @(&dist+7+&j*15) maxn&j
	                %end ;
	                ;

	                  put @10 'Mean (SD)' @;
	                     %DO j=1 %to &Ncat.;
	                        length MeanSDText $20;
	                        MeanSDText = trim(left(put(meann&j.,7.2)))||' ('||trim(left(put(stdn&j.,7.2)))||')';
	                        put @(&dist.+1+&j.*15) MeanSDText @;
	                     %END;
	                  put;

			   %END;

			** CASE 4 : Median - Range - Q1Q3 - Mean - N ;
			   %IF &Quantiles.=Y and &MEAN.=Y and &BOTH.=Y %THEN %DO;

	                  put @10 'Median'
	                %do j=1 %to &Ncat ;
	                  @(&dist+3+&j*15) medn&j
	                %end ;
	                ;
	                  put @10 'Range'
	                %do j=1  %to &Ncat ;
	                 @(&dist+&j*15) minn&j  @(&dist+7+&j*15) maxn&j
	                %end ;
	                ;

                   put @10 'Q1 - Q3'
                      %DO j=1 %to &Ncat.;
                         @(&dist.+&j.*15) q1n&j  @(&dist+7+&j*15) q3n&j
                      %END;
                      ;

	                  put @10 'Mean (SD)' @;
	                     %DO j=1 %to &Ncat.;
	                        length MeanSDText $20;
	                        MeanSDText = trim(left(put(meann&j.,7.2)))||' ('||trim(left(put(stdn&j.,7.2)))||')';
	                        put @(&dist.+1+&j.*15) MeanSDText @;
	                     %END;
	                  put;

			   %END;

			** CASE 5 : Median - Range - Q1Q3 - N ;
			   %IF &Quantiles.=Y and &MEAN.=N and &BOTH.=N %THEN %DO;

	                  put @10 'Median'
	                %do j=1 %to &Ncat ;
	                  @(&dist+3+&j*15) medn&j
	                %end ;
	                ;
	                  put @10 'Range'
	                %do j=1  %to &Ncat ;
	                 @(&dist+&j*15) minn&j  @(&dist+7+&j*15) maxn&j
	                %end ;
	                ;

                   put @10 'Q1 - Q3'
                      %DO j=1 %to &Ncat.;
                         @(&dist.+&j.*15) q1n&j  @(&dist+7+&j*15) q3n&j
                      %END;
                      ;
			   %END;
       
			** CASE 6 : Mean - Q1Q3 - N ;
			   %IF &Quantiles.=Y and &MEAN.=Y and &BOTH.=N %THEN %DO;

	                  put @10 'Mean (SD)' @;
	                     %DO j=1 %to &Ncat.;
	                        length MeanSDText $20;
	                        MeanSDText = trim(left(put(meann&j.,7.2)))||' ('||trim(left(put(stdn&j.,7.2)))||')';
	                        put @(&dist.+&j.*15) MeanSDText @;
	                     %END;
	                  put;

                   put @10 'Q1 - Q3'
                      %DO j=1 %to &Ncat.;
                         @(&dist.+&j.*15) q1n&j  @(&dist+7+&j*15) q3n&j
                      %END;
                     ;

		   		%END;

             put @10 'N obs'
             %do j=1 %to &Ncat ;
               @(&dist+3+&j*15) nobsn&j
             %end ;
             ;

             if eof then put;* @5 &last*'_';

             RETURN;

         %if (&first=Y) %then %do ;
             PAGETOP:
         %if (%length(&title)>0) %then %do ;
              put   @7 &title;
         %end ;
              put   @5 &last*'_';
         if (rtop>1) then do ;
              put /  @(&dist+15+&Ncat*7-rtop) _top ;
         end ;
              put /
         %do j=1 %to &Ncat ;
              @(&dist+7+&j*15-rt&j) title&j
         %end ;
         ;
              put
         %do j=1 %to &Ncat ;
              %if (%eval(&j)^=%eval(&Ncat) or &total^=Y) %then %do ;
              @(&dist+6+&j*15-%length(&&Ntot&j)/2-1) "(N=&&Ntot&j)"
              %end ;
              %else %do ;
              @(&dist+6+&j*15-%length(&Gtotal)/2-1) "(N=&Gtotal)"
              %end ;

         %end ;
         ;
              put @5 &last*'_' /;
             RETURN;
         %end ;
         run;
      %END;

   %END ;

%end ;** end of the do.. until loop ;

****  IF &VARLIST is EMPTY   *****;
%if (%scan(&varlist,%eval(&i),%str( ))=) %then %do ;

   ** DISPLAY IN A CASE OF ODS RTF FILE **;
   %IF &ODS.=Y %THEN %DO;

      options papersize=A4 nodate;

      data _null_;
      set __tab;
      length ByLabel $45;
      call label(&by.,ByLabel);
      call symput('ByLabel', trim(left(ByLabel)));
      run;

      %IF &ODSFile^=%STR() %THEN %DO;
         ods rtf file= "&ODSFile." style=EORTCStyle1;
      %END;

      %LOCAL VarLength;
      %LET VarLength = &VarLength2;
      %IF &i.>2 %THEN %DO j = 3 %TO &i.;
         %IF &&VarLength&j..>&VarLength. %THEN %LET VarLength = &&VarLength&j..;
      %END;

      * Find number of rows available for results...;

         * First need to count rows in header...;

         %LOCAL NHeaderRows;

         %LET NHeaderRows = 1;

             %DO j = 1 %TO &NCat.;

			 %put header&j &&header&j;

            %LOCAL NHeaderRows&j.;
            %LET NHeaderRows&j.=0;

            %DO %UNTIL (%QSCAN(%NRSTR(&&header&j..),&&NHeaderRows&j..,'@')=%STR());
               %LET NHeaderRows&j.=%EVAL(&&NHeaderRows&j..+1);
            %END;
            %LET NHeaderRows&j. = %EVAL(&&NHeaderRows&j..-1);

            %IF %EVAL(&&NHeaderRows&j..>&NHeaderRows.) %THEN
               %LET NHeaderRows = &&NHeaderRows&j..;

         %END;

         * Subtract from total number of rows per page to get N rows available for results...;

         %LOCAL NResultRows;
         %IF &num.=Y %THEN %LET NResultRows=%EVAL(42 - 2 - &NHeaderRows.);
         %ELSE %LET NResultRows=%EVAL(42 - 3 - &NHeaderRows.);

**************************************************;
*******  CASE OF CATEGORICAL DATA : NUM NE Y  ****;
**************************************************;

    %IF &num.^=Y %THEN %DO;

         data __Output;
         length VarValue $%EVAL(&VarLength.);
         set %DO j=2 %TO &i.; __Output&j. (in=in&j.) %END; ;
         %DO j=2 %TO &i.;
            if in&j. then VarNumber = &j.;
         %END;
		   *     VarValue = "     "||trim(left(VarValue));
		         VarValue = trim(left(VarValue));
        	keep VarNumber VarLabel VarValue cntpct1 - cntpct&Ncat.;
         run;

         * Following code added so that table is split over pages without splitting results for any one
           variable.  (Cannot use LINESLEFT= option as PUT _PAGE_ does not work with ODS.);

         proc freq data=__Output noprint;
         tables VarNumber / noprint out=__NValues;
         run;

         data __Output;
         merge __Output __NValues (keep=VarNumber Count rename=(Count=__NValues)) end=LastObs;
         by VarNumber;
         retain PageNumber EndOfVarLineNumber;
         if _n_=1 then do;
            PageNumber=1;
            EndOfVarLineNumber=0;
         end;
         if first.VarNumber then do;
            EndOfVarLineNumber=EndOfVarLineNumber + __NValues + 1;
            if EndOfVarLineNumber > &NResultRows. then do;
               PageNumber = PageNumber + 1;
               EndOfVarLineNumber = __NValues+1;
            end;
         end;
         if LastObs then call symput('NPages',trim(left(put(PageNumber,3.))));
         keep PageNumber VarNumber VarLabel VarValue cntpct1 - cntpct&Ncat.;
         run;

		 ** Determination of maximum size for First column *;

		 data __out;
		 	set __Output(in=a) __Output(in=b) ;
			if a then text=VarValue;
			if b then text=VarLabel;
			patid=1;
			lt=length(text);
		 run;


	     ** JRA, 11MAR2013, Replacement of GLAST by GLAST2;
	     %sort(data=__out,var=lt);
	     %glast2(data=__out,var=lt,outdata=__outv,outvar=max,addvar=varlabel text);
	     %glast2(data=__out,var=lt,merge=__outv,outvar=max2,SEL=varlabel ne text );

		 data _null_;
			set __outv;
			if lvarlabel=ltext then tit=1;
			call symput('maxSIZ1',compress(max));
			call symput('maxSIZssTIT',compress(max2));
			call symput('tit',compress(tit));
		 run;

		 %PUT &MAXSIZ1 &maxSIZssTIT &tit;

		 ** Determination of maximum size for BYVAR column *;

		 %let Nncat=%EVAL(&Ncat. - 1);

		 data __out2;
		 set 
			%Do J=1 %to &Nncat.;__Output(in=a&j) %END;

			%IF &form. ne 8 AND &form. ne BEST %then %do;
			__s
			%end;
			;
			patid=1;
			r=length(text);
		run;

		%sort(data=__out2,var=r);

		**JRA, 11JUN2012, Beacause of SAS 9.3 update, modification of GMAX by proc means to remove unexpected titles
		"PROC UNIVARIATE" in case of use of TABLES 2 with ODS RTF with BODYTITLE in SAS 9.3;
		proc means data=__out2 max noprint;
		var r;
		output out=__t max=max;
		run;


		data _null_;
			set __t;
			call symput('maxSIZ',compress(max));
		run;

		 data __out3;
		 set __Output;
			text=cntpct&Ncat.;
			patid=1;
			r=length(text);
		run;

		%sort(data=__out3,var=r);
		**JRA, 11JUN2012, Beacause of SAS 9.3 update, modification of GMAX by proc means to remove unexpected titles
		"PROC UNIVARIATE" in case of use of TABLES 2 with ODS RTF with BODYTITLE in SAS 9.3;

		proc means data=__out3 max noprint;
		var r;
		output out=__t1 max=max;
		run;

		data _null_;
			set __t1;
			call symput('maxSIZ2',compress(max));
		run;

		**********************************************;
		** HANdling of space line in RESULT COLUMNS **; 
		**********************************************;

		data __Output;
			set __Output;
			by VarNumber;
			v=VarValue;
			if first.VarNumber then do; VarValue=VarLabel;output;end;output;
		 run;

         data __Output;
			set __Output;
			by VarNumber;
			if first.VarNumber then n=1;
			else n+1;
		 run;

         data __Output;
			set __Output;
			lt=length(VarValue);
			if n=2 then VarValue=v;
			if n=1 then do;
				%DO J=1 %TO  &Ncat.;
					cntpct&j.="";
				%END;
			end;
			%IF &ADDCOLUMN=Y %THEN blank="";;
		run;

		 %IF &TIT = 1 %THEN %DO;
		 	%let MAXSIZ1=&MAXSIZsstit.;
		 %END;

		 *Sept2007, JRA, Change of the size of columns tables initialization;
		 %IF &MAXSIZ1 <20 %then %let MAXSIZ1=20;
	   	 %IF &MAXSIZ1 >=20 and &MAXSIZ1<35 %then %let MAXSIZ1=30;
	   	 %IF &MAXSIZ1 >=35 and &MAXSIZ1<50 %then %let MAXSIZ1=45;
		 %IF &MAXSIZ2 <10 %then %let MAXSIZ2=10.5;
		 %IF &MAXSIZ <10 %then %let MAXSIZ=10.5;

		  **********************************************;
		  *** PROC TEMPLATE - DEFINITON OF TABLE DESIGN **;
		  ** CASE OF NUM NE Y **;
		  **********************************************;

	      proc template;
	      define table TablesTemplate;
	         dynamic ColHeader ColHeader2;
	         header TableTitle ByVarLabel %DO j = 1 %TO &NCat.; ByValLabel&j. %END; ;
	         column FirstCol %DO j = 1 %TO &NCat.; ResultCol&j. %END; %IF &ADDCOLUMN=Y %THEN %DO; LastCol %END; ;;
			 mvar Title;
	         define TableTitle;
	            text Title;
	         	style = header {                                                     
	            font_size = 2
	            protectspecialchars=off
	            pretext="\brdrb\brdrs\brdrw1 "
	            }; 
				start=FirstCol;
	         end;
	         define ByVarLabel;
			 	%IF &Header.=Y %THEN %DO;
	              text "&ByLabel.";
				         style = header;
			 	%END;
						 start=ResultCol1;
	                        %IF &total.=Y %THEN %DO;
	                           %LOCAL PenCol;
	                           %LET PenCol = %EVAL(&NCat.-1);
	               				end=ResultCol&PenCol.;
	                        %END;
	         end;

			 * ADJUSTING OF First COLUMN SIZE - JRA, Oct2006*;

			 %let size1=%sysevalf((&MAXSIZ1/50)*10);
			 %put &size1;

			**JRA, Sept2007, Add of reduce parameter;
			 %if &reduce.=Y %then %let size1=1.3;

	         define FirstCol;
	            define header TestVarColHdr;
	               just=l;
	            end;
	            header=ColHeader;
	            style=header {                                                     
	            cellwidth = &size1. cm                                                   
	            };            
	            just=l; 
	            preformatted=on;
			  end;


	           %DO j = 1 %TO &NCat.;
	             define ByValLabel&j.;
	                    text "&&header&j..";
				         style = header ;
	                split='@';
	                        start=ResultCol&j.;
	                end=ResultCol&j.;
	             end;

			   %END;

			   * ADJUSTING OF BYVAR AND TOTAL COLUMNS SIZE - JRA, Oct2006*;

			   %let size=%sysevalf(((&MAXSIZ+1)/55)*10);
			   %put &size;

			**JRA, Sept2007, Add of reduce parameter;
			   %if &reduce.=Y %then %let size=1.3;

			   %let Nncat=%eval(&NCat.-1);

	           %DO j = 1 %TO &NnCat.;
	            define ResultCol&j.;
	               header=ColHeader;
		            style = data {
		            outputwidth = &size. cm
		            };   
	               preformatted=on;
	               just=c;
	            end;
	         %END;

			 %IF &ADDCOLUMN=Y %THEN %DO;
	            define LastCol;
	               header=ColHeader;
		            style = data {
		            outputwidth = &size. cm
		            };   
	               preformatted=on;
	               just=c;
	            end;
			 %END;

			   %let size2=%sysevalf((&MAXSIZ2/55)*10);
			   %put &size2;

			**JRA, Sept2007, Add of reduce parameter;
			   %if &reduce.=Y %then %let size2=1.3;

	            define ResultCol&NCat.;
	               header=ColHeader2;
	               style=data {                                                     
	               outputwidth = &size2. cm                                                 
	               };           
	               preformatted=on;
	               just=c;
	            end;

	      end;
	      run;

          %DO i = 1 %TO &NPages.;

			ods escapechar='^';

            data _NULL_ ;
            set __Output;
            where PageNumber=&i.;
            file print
                 ods=(template='TablesTemplate'
                      columns=(FirstCol=VarValue 
                         %DO j=1 %TO &NCat.;
                                  ResultCol&j.=cntpct&j. 
							%if &PCT.=N %then  %do;
							(
							dynamic=(
							ColHeader='N' 
							ColHeader2='N' 
							))

							%end;

  							%if &PCT.=ROW %then %do;	
							(
							dynamic=(
							ColHeader='N (row ^{unicode 0025})' 
								%if &TOTAL.=Y %then %do; ColHeader2='N (column ^{unicode 0025})' %end;
								%if &TOTAL.=N %then %do;  ColHeader2='N (row ^{unicode 0025})'   %end;
							))
							
							%end;

  							%if &PCT.=COL %then %do; 
							(
							dynamic=(
							ColHeader='N (^{unicode 0025})' 
							ColHeader2='N (^{unicode 0025})' 
							))

							 %end; 

  							%if &PCT.=CELL %then %do;
							(
							dynamic=(
							ColHeader='N (cell ^{unicode 0025})' 
								%if &TOTAL.=Y %then %do; ColHeader2='N (column ^{unicode 0025})' %end;
								%if &TOTAL.=N %then %do;  ColHeader2='N (cell ^{unicode 0025})'   %end;
							))
							%end; 

                         %END; 

							%IF &ADDCOLUMN=Y %THEN %do;
							LastCol=blank(dynamic=(ColHeader='P-value')) %end;

						));;

            put _ods_;
            run;

          %END; 

          %END; 

***********************************************;
*******  CASE OF NUMERICAL DATA : NUM = Y  ****;
************************************** ********;
          %IF &NUM.=Y %THEN %DO;

         data __Output;
         set %DO j=2 %TO &i.; __Output&j. (in=in&j.) %END; ;
         %DO j=2 %TO &i.;
            if in&j. then do;
               VarNumber = &j.;
            end;
         %END;
         widestval=max(abs(minn1), abs(maxn1)
                       %IF &NCat.>1 %THEN %DO j=2 %TO &NCat.;
                         , abs(minn&j.), abs(maxn&j.)
                                           %END;
                                            );
         keep VarNumber VarLabel
              minn1 - minn&Ncat.
              maxn1 - maxn&Ncat.
              medn1 - medn&Ncat.
              q1n1 - q1n&Ncat.
              q3n1 - q3n&Ncat.
              meann1 - meann&Ncat.
              stdn1 - stdn&Ncat.
              nobsn1 - nobsn&Ncat.
              widestval;
         run;

		**JRA, 11JUN2012, Beacause of SAS 9.3 update, modification of proc univariate by proc means to remove unexpected titles
		"PROC UNIVARIATE" in case of use of TABLES 2 with ODS RTF with BODYTITLE in SAS 9.3;

         proc means data=__Output noprint;
         var widestval;
         output out=__WidestVal max=widestval;
         run;

         data _null_;
         set __WidestVal;
         ResultLength=floor(log10(widestval)+1)+4;
         call symput('ResultLength', trim(left(put(ResultLength,3.))));
         run;

         * Following code added so that table is split over pages without splitting results for any one
           variable.  (Cannot use LINESLEFT= option as PUT _PAGE_ does not work with ODS.);

         %LOCAL NLinesPerVar;
         %IF &Mean.=Y %THEN %LET NLinesPerVar=3;
         %ELSE %DO;
            %IF &Quantiles.=Y %THEN %LET NLinesPerVar=5;
            %ELSE %LET NLinesPerVar=4;
         %END;

         data __Output;
         set __Output end=LastObs;
         retain PageNumber EndOfVarLineNumber;
         if _n_=1 then do;
            PageNumber=1;
            EndOfVarLineNumber=0;
         end;
         EndOfVarLineNumber=EndOfVarLineNumber + &NLinesPerVar.;
         if EndOfVarLineNumber > &NResultRows. then do;
            PageNumber = PageNumber + 1;
            EndOfVarLineNumber = &NLinesPerVar.;
         end;
         if LastObs then call symput('NPages',trim(left(put(PageNumber,3.))));
		 lt=length(VarLabel);
         run;

		 data __OutputB;
		    length 
            %DO j=1 %TO &NCat.;
			Result1&j. Result2&j. Result3&j. Result4&j. Result5&j. $18.
			%END;;

	 	    set __Output;
            %DO j=1 %TO &NCat.;
			
				** CASE 1 : Median - Range - N ;
			   %IF &Quantiles.=N and &MEAN.=N and &BOTH.=N %THEN %DO;

				  if medn&j. ne . then do;
                  Result1&j. = trim(left(put(medn&j.,%CMPRES(&ResultLength..1))));
                  end;
				  if minn&j. ne . then do;
				  Result2&j. = trim(left(put(minn&j.,%CMPRES(&ResultLength..1))))||' - '||
                           trim(left(put(maxn&j.,%CMPRES(&ResultLength..1))));
				  end;
                  Result3&j. = trim(left(put(nobsn&j.,8.)));
				  %LET NSTAT=3;
				  drop Result4&j. Result5&j.;
			   %END;
			   
				** CASE 2 : Mean - N ;
			   %IF &Quantiles.=N and &MEAN.=Y and &BOTH.=N %THEN %DO;

			      if meann&j. ne . then do;
                  	Result1&j. = trim(left(put(meann&j.,%CMPRES(&ResultLength..2))))||' ('||trim(left(put(stdn&j.,%CMPRES(&ResultLength..2))))||')';
				  end;
				  Result2&j. = trim(left(put(nobsn&j.,8.)));
				  drop Result3&j. Result4&j. Result5&j.;
				  %LET NSTAT=2;
			   %END;

				** CASE 3 : Median - Range - Mean - N ;
			   %IF &Quantiles.=N and &MEAN.=Y and &BOTH.=Y %THEN %DO;

				  if medn&j. ne . then do;
                  Result1&j. = trim(left(put(medn&j.,%CMPRES(&ResultLength..1))));
                  end;
				  if minn&j. ne . then do;
				  Result2&j. = trim(left(put(minn&j.,%CMPRES(&ResultLength..1))))||' - '||
                           trim(left(put(maxn&j.,%CMPRES(&ResultLength..1))));
				  end;
   			      if meann&j. ne . then do;
                  	Result3&j. = trim(left(put(meann&j.,%CMPRES(&ResultLength..2))))||' ('||trim(left(put(stdn&j.,%CMPRES(&ResultLength..2))))||')';
				  end;
	              Result4&j. = trim(left(put(nobsn&j.,8.)));
				  %LET NSTAT=4;
				  drop Result5&j.;
			   %END;

				** CASE 4 : Median - Range - Q1Q3 - Mean - N ;
			   %IF &Quantiles.=Y and &MEAN.=Y and &BOTH.=Y %THEN %DO;

				  if medn&j. ne . then do;
                  Result1&j. = trim(left(put(medn&j.,%CMPRES(&ResultLength..1))));
                  end;
				  if minn&j. ne . then do;
				  Result2&j. = trim(left(put(minn&j.,%CMPRES(&ResultLength..1))))||' - '||
                           trim(left(put(maxn&j.,%CMPRES(&ResultLength..1))));
				  end;
  			      if q1n&j. ne . then do;
                     Result3&j. = trim(left(put(q1n&j.,%CMPRES(&ResultLength..1))))||' - '||
                              trim(left(put(q3n&j.,%CMPRES(&ResultLength..1))));
				  end;
				  if meann&j. ne . then do;
                  	Result4&j. = trim(left(put(meann&j.,%CMPRES(&ResultLength..2))))||' ('||trim(left(put(stdn&j.,%CMPRES(&ResultLength..2))))||')';
				  end;
             	  Result5&j. = trim(left(put(nobsn&j.,8.)));
				  %LET NSTAT=5;
			   %END;


				** CASE 5 : Median - Range - Q1Q3 - N ;
			   %IF &Quantiles.=Y and &MEAN.=N and &BOTH.=N %THEN %DO;

				  if medn&j. ne . then do;
                  Result1&j. = trim(left(put(medn&j.,%CMPRES(&ResultLength..1))));
                  end;
				  if minn&j. ne . then do;
				  Result2&j. = trim(left(put(minn&j.,%CMPRES(&ResultLength..1))))||' - '||
                           trim(left(put(maxn&j.,%CMPRES(&ResultLength..1))));
				  end;
  			      if q1n&j. ne . then do;
                     Result3&j. = trim(left(put(q1n&j.,%CMPRES(&ResultLength..1))))||' - '||
                              trim(left(put(q3n&j.,%CMPRES(&ResultLength..1))));
				  end;
             	  Result4&j. = trim(left(put(nobsn&j.,8.)));
				  %LET NSTAT=4;
				  drop Result5&j.;
			   %END;


				** CASE 6 : Mean - Q1Q3 - N ;
			   %IF &Quantiles.=Y and &MEAN.=Y and &BOTH.=N %THEN %DO;

  				  if meann&j. ne . then do;
                  	Result1&j. = trim(left(put(meann&j.,%CMPRES(&ResultLength..2))))||' ('||trim(left(put(stdn&j.,%CMPRES(&ResultLength..2))))||')';
				  end;
			      if q1n&j. ne . then do;
                     Result2&j. = trim(left(put(q1n&j.,%CMPRES(&ResultLength..1))))||' - '||
                              trim(left(put(q3n&j.,%CMPRES(&ResultLength..1))));
				  end;
             	  Result3&j. = trim(left(put(nobsn&j.,8.)));
				  %LET NSTAT=3;
				  drop Result4&j. Result5&j.;
		   %END;

          	%END;
          run;


		%DO K=1 %TO &NSTAT;
		data __OutputB&K;
		    set __OutputB;
			%DO L=1 %TO &Ncat.;
				Result&L.=Result&K&L.;

		 ** JRA, 06JUN2012, Because of SAS 9.3 upgrade, replacement of spaces by tabulation, because tabulation
		 are again recognized in SAS 9.3 (but it was not the case in SAS 9.2);

				** CASE 1 : Median Range N **;
			   %IF &Quantiles.=N and &MEAN.=N and &BOTH.=N %THEN %DO;
						%IF &K=1 %THEN VarLabel='09'x||'Median';;
	 					%IF &K=2 %THEN VarLabel='09'x||'Range';;
	 					%IF &K=3 %THEN VarLabel='09'x||'N obs';;
			   %END;

				** CASE 2 : Mean - N ;
			   %IF &Quantiles.=N and &MEAN.=Y and &BOTH.=N %THEN %DO;
					%IF &K=1 %THEN VarLabel='09'x||'Mean (SD)';;
 					%IF &K=2 %THEN VarLabel='09'x||'N obs';;
			   %END;

				** CASE 3 : Median - Range - Mean - N ;
			   %IF &Quantiles.=N and &MEAN.=Y and &BOTH.=Y %THEN %DO;
						%IF &K=1 %THEN VarLabel='09'x||'Median';;
	 					%IF &K=2 %THEN VarLabel='09'x||'Range';;
	 					%IF &K=3 %THEN VarLabel='09'x||'Mean (SD)';;
	  					%IF &K=4 %THEN VarLabel='09'x||'N obs';;
			   %END;

 				** CASE 4 : Median - Range - Q1Q3 - Mean - N ;
			   %IF &Quantiles.=Y and &MEAN.=Y and &BOTH.=Y %THEN %DO;
						%IF &K=1 %THEN VarLabel='09'x||'Median';;
	 					%IF &K=2 %THEN VarLabel='09'x||'Range';;
	 					%IF &K=3 %THEN VarLabel='09'x||'Q1-Q3';;
	 					%IF &K=4 %THEN VarLabel='09'x||'Mean (SD)';;
	  					%IF &K=5 %THEN VarLabel='09'x||'N obs';;
			   %END;

				** CASE 5 : Median - Range - Q1Q3 - N ;
			   %IF &Quantiles.=Y and &MEAN.=N and &BOTH.=N %THEN %DO;
						%IF &K=1 %THEN VarLabel='09'x||'Median';;
	 					%IF &K=2 %THEN VarLabel='09'x||'Range';;
	 					%IF &K=3 %THEN VarLabel='09'x||'Q1-Q3';;
	  					%IF &K=4 %THEN VarLabel='09'x||'N obs';;
			   %END;

				** CASE 6 : Mean - Q1Q3 - N ;
			   %IF &Quantiles.=Y and &MEAN.=Y and &BOTH.=N %THEN %DO;
						%IF &K=1 %THEN VarLabel='09'x||'Mean (SD)';;
		 				%IF &K=2 %THEN VarLabel='09'x||'Q1-Q3';;
	  					%IF &K=3 %THEN VarLabel='09'x||'N obs';;
		   	   %END;
			%END;
		run;
		%END;

		data __OutputC;
		    set __Output(keep=VarLabel VarNumber PageNumber)
			%DO K=1 %TO &NSTAT;
			__OutputB&K.
			%END;;
		run;

		%sort(data=__OutputC,var=VarNumber);

		 ** Determination of maximum size for First column *;

		 data __outB;
		 	set __OutputC ;
			patid=1;
			lt=length(VarLabel);
		 run;

	  	 %sort(data=__outB,var=lt);

		**JRA, 11JUN2012, Beacause of SAS 9.3 update, modification of GMAX by proc means to remove unexpected titles
		"PROC UNIVARIATE" in case of use of TABLES 2 with ODS RTF with BODYTITLE in SAS 9.3;
		proc means data=__outB max noprint;
		var lt;
		output out=__outv max=max;
		run;

		 data _null_;
			set __outv;
			call symput('maxBSIZ1',compress(max));
		 run;

		 %PUT &MAXBSIZ1;

		 ** Determination of maximum size for First column *;

		 data __outB;
		 	set __OutputC ;
			patid=1;
			lt=length(VarLabel);
		 run;

	  	 %sort(data=__outB,var=lt);
	     %gmax(data=__outB,var=lt,outdata=__outv,outvar=max);

		 data _null_;
			set __outv;
			call symput('maxBSIZ1',compress(max));
		 run;

		 %PUT &MAXBSIZ1;

		 ** Determination of maximum size for BYVAR column *;

		 %let Nncat=%eval(&NCat.-1);

		 **JRA, 20FEB2015, resolution of bug to avoid 'Result 1' appearing in case NUM=Y one level in BY var TOTAL=N;
		 %if &Nncat=0 %then %let Nncat=1;

		 data __outB2;
		 	set __OutputC;
			%DO J=1 %TO &Ncat.;
				lt&J=length(Result&j.);
			%END;
			%LET Nnncat=%eval(&Nncat-1);

			%if &Nnncat.= 0 %then %do;
			lt=lt1;
			%end;
			%if &Nnncat.> 0 %then %do;
			lt=max(	%DO J=1 %TO &Nnncat.;lt&J,%END; lt&Nncat.);;
			%end;
          run;

		 data __outB21;
		 set __outB2 	%IF &form. ne 8 AND &form. ne BEST and &fmtname ne $ %then %do;
			__s(in=a)
			%end;
			;
			%IF &form. ne 8 AND &form. ne BEST and &fmtname ne $ %then %do;
			if a then lt=length(text);
			%end;
			patid=1;
		run;

		%sort(data=__outB21,var=lt);

		**JRA, 11JUN2012, Beacause of SAS 9.3 update, modification of GMAX by proc means to remove unexpected titles
		"PROC UNIVARIATE" in case of use of TABLES 2 with ODS RTF with BODYTITLE in SAS 9.3;
		proc means data=__outB21 max noprint;
		var lt;
		output out=__t max=max;
		run;

		data _null_;
			set __t;
			call symput('maxSIZB',compress(max));
		run;

		** Determination of maximum size for TOTAL column *;

		%sort(data=__outB21,var=lt&Ncat);

		**JRA, 11JUN2012, Beacause of SAS 9.3 update, modification of GMAX by proc means to remove unexpected titles
		"PROC UNIVARIATE" in case of use of TABLES 2 with ODS RTF with BODYTITLE in SAS 9.3;
		proc means data=__outB21 max noprint;
		var lt&Ncat;
		output out=__t2 max=max;
		run;


		data _null_;
			set __t2;
			call symput('maxSIZB3',compress(max));
		run;

		**********************************************;
		** HANdling of space line in RESULT COLUMNS **; 
		**********************************************;

	     data __OutputC;
			set __OutputC;
			by VarNumber;
			if first.VarNumber then n=1;
			else n+1;
		 run;

	     data __OutputC;
			set __OutputC;
			lt=length(VarLabel);
			if n=1 and lt >26 then do;
				%DO J=1 %TO &Ncat.;
				Result&J.=" ";
				%END;
			end;

			%IF &ADDCOLUMN=Y %THEN blank="";;

		 run;

		%IF &MAXBSIZ1 >20 %then %let MAXBSIZ1=26;
		%IF &MAXBSIZ1 <15 %then %let MAXBSIZ1=20;
		%IF &maxSIZB  <10 %then %let maxSIZB=10.5;
		%IF &maxSIZB3 <10 %then %let maxSIZB3=10.5;

      ** CASE OF NUM = Y **;
	  *** PROC TEMPLATE - DEFINITON OF TABLE DESIGN **;

      proc template;
      define table TablesTemplate;
         dynamic ColHeader;
         header TableTitle ByVarLabel %DO j = 1 %TO &NCat.; ByValLabel&j. %END; ;
         column FirstCol %DO j = 1 %TO &NCat.; ResultCol&j. %END; %IF &ADDCOLUMN=Y %THEN %DO; LastCol %END; ;;
		 mvar Title;
	     define TableTitle;
	            text Title;
	         	style = header {                                                     
	            font_size = 2
	            protectspecialchars=off
	            pretext="\brdrb\brdrs\brdrw1 "
	            }; 
				start=FirstCol;
		 end;
         define ByVarLabel;
			 	%IF &Header.=Y %THEN %DO;
	              text "&ByLabel.";
				         style = header {
				            };
			 	%END;
                    start=ResultCol1;
                        %IF &total.=Y %THEN %DO;
                           %LOCAL PenCol;
                           %LET PenCol = %EVAL(&NCat.-1);
               				end=ResultCol&PenCol.;
                        %END;
         end;

		 * ADJUSTING OF First COLUMN SIZE - JRA, Oct2006*;

		 %let sizeB1=%sysevalf((&MAXBSIZ1/55)*10);
		 %put &sizeB1;

		**JRA, Sept2007, Add of reduce parameter;
		 %if &reduce.=Y %then %let sizeB1=1.3;

         define FirstCol;
            define header TestVarColHdr;
               just=l;
            end;
            header=ColHeader;
            style=header {                                                     
            cellwidth = &sizeB1. cm                                                 
            };            
            just=l; 
            preformatted=on;
		  end;

           %DO j = 1 %TO &NCat.;
             define ByValLabel&j.;
                    text "&&header&j..";
                style=header{
				} ;
                split='@';
                        start=ResultCol&j.;
                end=ResultCol&j.;
             end;

		   %END;

		   * ADJUSTING OF BYVAR AND TOTAL COLUMN SIZE - JRA, Oct2006*;

		   %let sizeB=%sysevalf((&MAXSIZB/55)*10);
		   %put &sizeB;

		   **JRA, Sept2007, Add of reduce parameter;
	 	   %if &reduce.=Y %then %let sizeB=1.3;

          %DO j = 1 %TO &NnCat.;
            define ResultCol&j.;
               header=ColHeader;
		            style = data {
		            outputwidth = &sizeB. cm
		            };   
               preformatted=on;
               just=c;
            end;

		   %let sizeB3=%sysevalf((&MAXSIZB3/55)*10);
		   %put &sizeB3;

	       **JRA, Sept2007, Add of reduce parameter;
		   %if &reduce.=Y %then %let sizeB3=1.3;

            define ResultCol&NCat.;
               header=ColHeader;
		            style = data {
		            outputwidth = &sizeB3. cm
		            };   
               preformatted=on;
               just=c;
            end;

			 %IF &ADDCOLUMN=Y %THEN %DO;
	            define LastCol;
	               header=ColHeader;
		            style = data {
		            outputwidth = &sizeB. cm
		            };   
	               preformatted=on;
	               just=c;
	            end;
			 %END;



         %END;
      end;
      run;	

     %DO i = 1 %TO &NPages.;

            data _NULL_ ;
            set __OutputC;
            where PageNumber=&i.;
            file print
                 ods=(template='TablesTemplate'
                      columns=(FirstCol=VarLabel 
                               %DO j=1 %TO &NCat.;
                                  ResultCol&j.=result&j. 
                               %END; 

					%IF &ADDCOLUMN=Y %THEN LastCol=blank(dynamic=(ColHeader='P-value'));

									));
            put _ods_;
            run;


           %END;

  %END;  *** END OF CASE OF NUMERICAL DATA : NUM = Y;

  %IF &ODSFile^=%STR() %THEN %DO;
		%STR(ods rtf close;);
		%clean_table(path=&ODSFile.);
  %END;

   %END;
   %ELSE %DO;

      data _NULL_ ;
       file print LS=&LS PS=&PS linesleft=lines notitles;
         put @5 &last*'_' /;
      run;

   %END;

** RE - INITILIAZE FORMAT CHANGES TO AVOID @ PRINTS **;

%IF &form. ne 8 AND &form. ne BEST and &fmtname ne $ %then %do;

	proc format lib=&catalog. cntlout=__format2;
	run;

	data __format2;
		set __format2;
		where FMTNAME="&form." and START ne "~~";
		LABEL2=TRANWRD(label,"@"," ");
	run;

	data _null_;
	set __format2 end=final;
	m+1;
	call SYMPUT('labform'||compress(m),trim(left(label2)));
	call SYMPUT('valform'||compress(m),compress(START));
	if final then call symput('NB',compress(m));
	run;

	proc format ;
		value &form. %DO J=1 %TO &NB.; &&valform&J.="&&labform&J." %END;
		;
	run;

%END;



	proc datasets library=work nolist;
	   delete __format __format2
			_attributes
			_copytab:
			_fakemissing
			_missing
			__totals
            __ww
			__nvalues
			__out:
			__q:
			__S
			__t
			__t1
			__t2
			__tab:
			__varlength
			__z
			__sorted
			__widestval
            __userfmt _data; 
	quit;

%end;

%fin: ;


%mend ;

