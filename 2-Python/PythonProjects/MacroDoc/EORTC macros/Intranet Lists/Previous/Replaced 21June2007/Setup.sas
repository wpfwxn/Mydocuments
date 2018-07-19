********************************************************************************;

*  PROGRAM:   Setup.SAS
   
   LOCATION:  K:\SAS\EORTC macros\Intranet Lists\Programs

   PURPOSE:   Setup program.

   AUTHOR:    Kate Moncrieff
   DATE:      5 July 2002

   SOFTWARE:  SAS v8.2

********************************************************************************;

%LET RootDir = %STR(K:\SAS\EORTC macros\Intranet Lists);

libname ML "&RootDir.\Data";

options papersize=A4 orientation=landscape linesize=200;

proc format;
value $ftype 'U' ='Utility'
             'MA'='Meta-analysis'
             'G' ='Graph'
             'P' ='Program'
             'A' ='Analysis'
             'GM'='G Macro'
             'R' ='Report'
             'QL'='Quality of Life';
value $fauthor 'LC' ='Laurence Collette'
               'LB' ='Luc Bijnens'
               'GHB'='Guido Hoctin Boes'
               'JR' ='John Roberts'
               'AI' ='A Ivanov'
               'DC' ='Desmond Curran'
               'KVS'='Kristel Van Steen'
               'BBA'='Benoit Baron'
               'KM' = 'Kate Moncrieff';
run;
