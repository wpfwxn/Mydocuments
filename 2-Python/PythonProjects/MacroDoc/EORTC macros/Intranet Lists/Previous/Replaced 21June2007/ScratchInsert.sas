********************************************************************************;

*  PROGRAM:   ScratchInsert.SAS
   
   LOCATION:  K:\SAS\EORTC macros\Intranet Lists\Programs

   PURPOSE:   To add a new macro to the MacroList table

   AUTHOR:    Kate Moncrieff
   DATE:      5 July 2002

   SOFTWARE:  SAS v8.2

********************************************************************************;

%INCLUDE "K:\SAS\EORTC macros\Intranet Lists\Programs\Setup.sas";

data NewRecords;
run;

data NewRecords;
set ML.MacroList (obs=0) NewRecords;
Macro='CIPlot';
Current='Y';
SAS8='Y';
Type='G';
Desc_Sh='Plots cumulative incidence (competing risks)';
Desc_Ln='Constructs ASCII data files for GraphLib to create cumulative incidence curves';
*Notes='';
*BasedOn='';
OrigDate='09SEP2003'd;
*ModDate=''d;
Author1='KM';
*Author2='';
*Author3='';
*TestDate=''d;
*TestVer='';
output;
run;

proc append base=ML.MacroList data=NewRecords;
run;

/*
* Check for dups: ;

data MacroList;
set ML.MacroList;
length StandardisedName $32;
StandardisedName=trim(left(upcase(Macro)));
run;

proc sort data=MacroList;
by StandardisedName;
run;

data _null_;
set MacroList;
by StandardisedName;
if not (first.StandardisedName and last.StandardisedName);
put '! MACRO APPEARS IN TABLE MORE THAN ONCE: ' _all_ /;
run;
*/