
%macro corrtable(data,vars,with,cutoff,file,type);

/*************************************************************************

This macro returns an overview table of the correlations between the specified variables. 

version: SAS 8.2
created: 12 January 2005
author:  Corneel Coens

**************************************************************************

PARAMETERS :

    DATA    = Name of the input dataset. (Required parameter)
    VARS	= List of variables of interest.
    WITH	= List of variables to correlate with the list of VARS. If left blank,
		the variables of VARS are cross-correlated between themselves.
    CUTOFF	= Cut-off value to highlight correlations in the output table that are
		higher than this value.
    FILE	= file path for the output directory
    TYPE	= P for Pearson correlations [default], S for Spearman rank correlations.
    
    
*************************************************************************/


%if (%length(&cutoff)=0) %then %let cutoff=2;
%if (%length(&file)=0) %then %let file='c:\corrtab.rtf';
%if (%length(&with)=0) %then %let with=&vars;
%if (%length(&type)=0) %then %let typ=P;
%else %let typ=%upcase(&type);
%if &typ=S %then %let titext=Spearman rank correlations;
%else %let titext=Pearson correlations matrix;

data _null_;
   set &data (obs=1);
   %let i=1;
   %let word=%qscan(&with,&i,%str( ));
   %do %while(&word ne);
    call symput("label&i",trim(left(vlabel(&word.))));
    %let i=%eval(&i+1) ;
    %let word=%qscan(&with,&i,%str( ));
   %END;
   %let i=%eval(&i-1) ;
run;

PROC CORR DATA=&data NOPRINT 
%if &typ=S %then OUTS=_CORROUT;
%else OUTP=_CORROUT; ;
VAR &vars;
%if (%length(&with)>0) %then WITH &with; ;
RUN;
DATA _CORROUT;
 SET _CORROUT(KEEP=_NAME_ &vars);
 FORMAT &vars 6.3;
IF _N_>3;
RENAME _NAME_=VARIABLE;
TITLE 'CORRELATIONS AMONG VARS';

DATA _CORROUT;
SET _CORROUT;
LENGTH VarLabel $45;
%do ii=1 %to &i;
 if _N_=&ii then VarLabel="&&label&ii";
%end;
RUN;


PROC FORMAT;
VALUE tlight low--&cutoff.='red' &cutoff.-high='red';
RUN;
title "&titext";
ODS RTF FILE="&file" STARTPAGE=NO bodytitle;
PROC PRINT noobs label; 
VAR VarLabel;
VAR &vars / style={foreground=tlight.};
RUN;
ODS RTF CLOSE;
title;

%mend;
