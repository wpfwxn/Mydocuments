**** PRINT_LIST.SAS ****;

* John Roberts;
* 29 March 2001  23 April 2001;




libname ml 'c:\sas\eortc macros\macro list';
libname library 'c:\sas\eortc macros\macro list';

options papersize=A4 orientation=landscape pageno=1;


proc contents data=ml.macrolist position;
run;
 

data temp;
   set ml.macrolist;
   upmacro=upcase(macro);
   if missing(moddate) then lastdate=origdate;
   else lastdate=moddate;
run;

 
proc sort data=temp;
   by type upmacro;
run;


*** Print;
options pageno=1 linesize=200;
ods rtf file=
  "c:\sas\eortc macros\macro list\list1.rtf"
      /*style=minimal*/
      author='John Roberts'
      keepn;
proc print data=temp label noobs;
   var macro--desc_sh basedon--author3;
   title'EORTC MACROS';
   format author1 author2 author3;
run;
options linesize=135;
title;
ods rtf close;

options pageno=1 linesize=200;
ods rtf file=
  "c:\sas\eortc macros\macro list\list2.rtf"
      /*style=minimal*/
      author='John Roberts'
      keepn;
proc print data=temp label noobs;
   var macro desc_sh desc_ln notes;
   title'EORTC MACROS';
   format author1 author2 author3;
run;
options linesize=135;
title;
ods rtf close;


*** HTML output for use on EORTC Intranet;
options pageno=1 linesize=200;
ods html file="c:\sas\eortc macros\macro list\macros-current-summary.htm"
      style=statdoc;
proc print data=temp label noobs;
   var macro type desc_sh desc_ln lastdate author1-author3;
   where sas8 in('Y','?') and current='Y';
   title  'SUMMARY - EORTC MACROS - CURRENT';
   title2 'Sorted by category and name';
*   format author1 author2 author3;
   format lastdate ddmmyy10.;
   label lastdate='Last date';
run;
options linesize=135;
title;

ods html close;
options pageno=1 linesize=200;
ods html file="c:\sas\eortc macros\macro list\macros-current.htm"
      style=statdoc;
proc print data=temp label noobs;
   var macro type current--testver;
   where sas8 in('Y','?') and current='Y';
   title 'EORTC MACROS - CURRENT';
   title2 'Sorted by category and name';
*   format author1 author2 author3;
run;
options linesize=135;
title;
ods html close;

ods html close;
options pageno=1 linesize=200;
ods html file="c:\sas\eortc macros\macro list\macros-all.htm"
      style=statdoc;
proc print data=temp label noobs;
   var macro type current--testver;
   title 'EORTC MACROS - ALL';
   title2 'Sorted by category and name';
*   format author1 author2 author3;
run;
options linesize=135;
title;
ods html close;

