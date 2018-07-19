********************************************************************************;

*  PROGRAM:   ListHTML.SAS
   
   LOCATION:  C:\Document\Working\SAS Related\Macro Lists on Intranet\Programs\

   PURPOSE:   To produce macro lists in HTML for Intranet from SAS dataset.

   AUTHOR:    Kate Moncrieff  (adapted from JR's program print_list)
   DATE:      19 March 2002

   SOFTWARE:  SAS v8.2

********************************************************************************;

%INCLUDE "H:\My Documents\Working\SAS Related\Macro Lists on Intranet\Programs\Setup.sas";

data ListHTML;
set ML.MacroList;
length OrderVar $32;
OrderVar=upcase(Macro);
if missing(ModDate) then LastDate=OrigDate;
else LastDate=ModDate;
format LastDate ddmmyy10.;
label LastDate='Last date';
run;

proc sort data=ListHTML;
by Type OrderVar;
run;


* List macros currently in use (summary);
****************************************;

options pageno=1;

ods html file="&RootDir.\Output\macros-current-summary.htm" style=statdoc;

title  'SUMMARY - EORTC MACROS - CURRENT';
title2 'Sorted by Category and Name';

proc print data=ListHTML label noobs;
var Macro Type Desc_Sh Desc_Ln LastDate Author1 - Author3;
where SAS8 in ('Y','?') and Current='Y';
run;

title;

ods html close;


* List macros currently in use (full);
*************************************;

options pageno=1;

ods html file="&RootDir.\Output\macros-current.htm" style=statdoc;

title  'EORTC MACROS - CURRENT';
title2 'Sorted by Category and Name';

proc print data=ListHTML label noobs;
var Macro Type Current--TestVer;
where SAS8 in ('Y','?') and Current='Y';
run;

title;

ods html close;


* List all macros;
*****************;

options pageno=1;

ods html file="&RootDir.\Output\macros-all.htm" style=statdoc;

title 'EORTC MACROS - ALL';
title2 'Sorted by Category and Name';

proc print data=ListHTML label noobs;
var Macro Type Current--TestVer;
run;

title;

ods html close;
