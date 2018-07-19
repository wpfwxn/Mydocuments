********************************************************************************;

*  PROGRAM:   ListHTML.SAS

   LOCATION:  K:\SAS\EORTC macros\Intranet Lists\Programs

   PURPOSE:   To produce macro lists in HTML for Intranet from SAS dataset.

   AUTHOR:    Kate Moncrieff  (adapted from JR's program print_list)
   DATE:      19 March 2002

   SOFTWARE:  SAS v8.2

********************************************************************************;

%INCLUDE "K:\SAS\EORTC macros\Intranet Lists\Programs\Setup.sas";

data Processed;
set ML.MacroList;
length Name $32 NameType $100;
Name=upcase(Macro);
NameType=trim(left(Name))||'^{<BR>}('||trim(left(put(Type,$FType.)))||')';
length Purpose $215;
if not missing(Desc_sh) then Purpose=Desc_Sh;
if not missing(Desc_Ln) then do;
  if missing(Purpose) then Purpose=Desc_Ln;
  else Purpose=trim(left(Purpose))||'^{<BR>}Details: '||trim(left(Desc_Ln));
end;
if not missing(Notes) then
  Purpose=trim(left(Purpose))||'^{<BR>}Notes: '||trim(left(Notes));
if not missing(BasedOn) then
  Purpose=trim(left(Purpose))||'^{<BR>}Basen on: '||trim(left(BasedOn));
length Authors $100;
if not missing(Author1) then Authors=put(Author1,$FAuthor.);
if not missing(Author2) then
  Authors=trim(left(Authors))||',^{<BR>}'||trim(left(put(Author2,$FAuthor.)));
if not missing(Author3) then
  Authors=trim(left(Authors))||',^{<BR>}'||trim(left(put(Author3,$FAuthor.)));
keep Name Type NameType Current Purpose Authors OrigDate ModDate;
format OrigDate ModDate date9.;
label Name='Macro'
      NameType='Macro (Category)'
      Purpose='Purpose'
      Authors='Authors/Modifiers'
      OrigDate='Original Date'
      ModDate='Last Modified';
run;


options linesize=240 missing=' ';
ods escapechar='^';
ods listing close;
footnote;


* List macros currently in use (alphabetically);

proc sort data=Processed out=List;
by NameType;
where Current='Y';
run;

options pageno=1;
ods html file="&RootDir.\Output\CurrentMacros.htm" style=statdoc;
title1 h=2.4  'EORTC MACROS Currently in Use';
proc print data=List label noobs
           style (header) = header [just=l]
           style (data) = data [just=l];
var NameType Purpose Authors OrigDate ModDate;
run;
title1;
ods html close;


* List macros currently in use (by category);

proc sort data=Processed out=List;
by Type Name;
where Current='Y';
run;

options pageno=1;
ods html file="&RootDir.\Output\CurrentMacrosByCategory.htm" style=statdoc;
title1 h=2.4  'EORTC MACROS Currently in Use by Category';
proc print data=List label noobs
           style (header) = header [just=l]
           style (data) = data [just=l];
by Type;
id Type;
var Name Purpose Authors OrigDate ModDate;
run;
title1;
ods html close;


* List macros no longer in use;

proc sort data=Processed out=List;
by NameType;
where Current^='Y';
run;

options pageno=1;
ods html file="&RootDir.\Output\PastMacros.htm" style=statdoc;
title1 h=2.4 'EORTC MACROS No Longer in Use';
proc print data=List label noobs
           style (header) = header [just=l]
           style (data) = data [just=l];
var NameType Purpose Authors OrigDate ModDate;
run;
title1;
ods html close;


options linesize=81;
ods listing;
