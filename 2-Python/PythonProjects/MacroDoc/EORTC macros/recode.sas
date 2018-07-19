***********************************************************************************

RECODE.SAS
**********

Macro to recode variable 
(with category including values up to and including next cut point)

Version date   : 22/03/2001
Software       : SAS version 8.1
Original author: Laurence Collette
***********************************************************************************
HISTORY

Nov 1999   Original version
           (Laurence Collette)
Revisions 
22/03/2001 Changes
           - Output variable &outname works (original version always used &var.c)
           - User-defined missing values not recoded
           - Label generated
           (John Roberts)
***********************************************************************************
PARAMETERS

var    =variable to be recoded 
cut    =list of cut points 
        (e.g. a b to recode var such that if var>. and var<=a then varC=1 
                                          if var>a and var<=b then varC=2 
                                          if var>b then varC=3 )
outname=optional name of new variable, default is varC

NOTES

The CUT values should be listed in an increasing order.
In the case of recoding of some haematology or biochemistry variables in Grade variables,
the use of this macro is inappropriated (use RecodeS in this case and see notes of 
%RecodeS). (JRA, 04/08/2006)

***********************************************************************************;


%macro recode(var, cut, outname) ;

%* Default output var name and label;
%if (%length(&outname)=0) %then 
   %do;
      %let outname=&var.C ;
      label &outname="Recode of &var";
   %end;

%* Initialize output var;
&outname=.;
%* Find first cut point;
%let count=1 ;
%let currcut=%qscan(&cut,&count,%str( )) ;

%* Loop until no more cut points;
%do %while (&currcut NE) ;
%put &var= &outname= &currcut=;
  if (&var>.Z and &outname=. and &var <= &currcut) then &outname=&count ;
  %let count=%eval(&count+1) ;
  %let currcut=%qscan(&cut,&count,%str( )) ;
%end ;

%* Deal with values above final cut point;
%let currcut=%qscan(&cut,%eval(&count-1),%str( )) ;
if (&var>.Z and &outname=. and &var>&currcut ) then &outname=&count ;

%mend ;
