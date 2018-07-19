********************************************************************************;

*  PROGRAM:   Setup.SAS
   
   LOCATION:  K:\SAS\EORTC macros\Intranet Lists\Programs

   PURPOSE:   Setup program.

   AUTHOR:    J�r�me RAPION  (adapted from KM's program print_list)
   DATE:      19 June 2007

   SOFTWARE:  SAS v9.1

********************************************************************************;

%LET RootDir = %STR(C:\Users\hazobou\PythonProjects\MacroDoc\EORTC macros\Intranet Lists);

libname ML "&RootDir.\Data";

options papersize=A4 orientation=landscape linesize=200;

proc format;
value $fauthor 'LC' ='Laurence Collette'
               'LB' ='Luc Bijnens'
               'GHB'='Guido Hoctin Boes'
               'JRO' ='John Roberts'
               'AI' ='A Ivanov'
               'MM' ='Murielle Mauer'
               'DC' ='Desmond Curran'
               'KVS'='Kristel Van Steen'
               'BBA'='Benoit Baron'
               'KM' ='Kate Moncrieff'
               'JBO'='Jan Bogaerts'
               'CCO'='Corneel Coens'
               'SC'='Sandra Collette'
               'NMA'='Nicolas Martin'
               'JRA'='J�r�me Rapion'
				'GIS'='Gaelle Isaac';
value $ftype 'U' ='Utility'
             'MA'='Meta-analysis'
             'G' ='Graph'
             'P' ='Program'
             'L' ='Laboratory data'
             'A' ='Analysis'
             'GM'='G Macro'
             'R' ='Report'
             'QL'='Quality of Life'
			 'S' ='Simulation'
			 'D' ='Design'
;
value $yn 'Y' ='Valid.'
   		  'N' ='Not valid.'
;
run;