%macro sort(data,var=patid) ;
proc sort data=&data;
by &var;
run;
%mend ;
