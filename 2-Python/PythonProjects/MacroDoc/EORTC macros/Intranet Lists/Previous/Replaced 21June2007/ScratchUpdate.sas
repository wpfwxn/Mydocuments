********************************************************************************;

*  PROGRAM:   ScratchUpdate.SAS
   
   LOCATION:  K:\SAS\EORTC macros\Intranet Lists\Programs

   PURPOSE:   To update the MacroList table

   AUTHOR:    Kate Moncrieff
   DATE:      3 June 2002

   SOFTWARE:  SAS v8.2

********************************************************************************;

%INCLUDE "K:\SAS\EORTC macros\Intranet Lists\Programs\Setup.sas";

* DONT INSERT A SEMI COLON AFTER THE SET STATEMENT - ALL RECORDS WILL BE UPDATED!;
proc sql;
update ML.MacroList
set ModDate = '29SEP2003'd /*!!! NO SEMICOLON HERE*/
where upcase(macro) in ('CIPLOT');
quit;
