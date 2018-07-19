**********************************************************************************************************;
** MACRO INPUT_UNIT                                                                                     **
** written by Jérôme RAPION                                                                             **
**                                                                                                      **
** Inputation of the unit used for each LAB test                                                        **
**                                                                                                      **
** This macro is supposed to be run after RECOGNITION macro.                                            **
** From all the variables scanned during RECOGNITION macro run, identification of each unit according   **
** to the DM standard naming. (see Method for standard naming DM-002-GD-01 file into                    **
** J:\UNIT\Data Management\Guidelines and technical documents\Form Definition\Variables standard naming)**
**                                                                                                      **
** For some lab test, the unit is always the same, so no need to identify it:                           **
** For instance, for WBC (White blood cells), PLT (Platelets) the unit will always be imputed to        **
** 109 cells/L. But for other example, it will be according to the suffix of the variable,              **
** for instance:                                                                                        **
** HBMML, in this case, because of the suffix, automatically, the unit will be identified as mmol/L.    **
** For HBGDL, automatically, the unit will be identified as g/dL, etc…                                  **
**                                                                                                      **
** Technically, from INPUT_VAR dataset, a new column named UNIT with the unit will be added.            **
** A last column called ROOT will contain the name of the LAB test without SUFFIX unit.                 **
** For instance, if the LAB test is called 'BILIMDL' or 'BILIUML' in the NAME variable, it will take    **
** only 'BILI' value into ROOT variable.                                                                **
**                                                                                                      **
** The result is saved in the same dataset as the output dataset (usually INPUT_VAR dataset)            **                                                                                                     **
**                                                                                                      **
** Creation date  : 18-11-2010                                                                          **
** Version date   : 21-09-2011                                                                          **
** Software       : SAS version 9.2                                                                     **
** Original author: Jérôme RAPION                                                                       **
** Modified by    : Jérôme RAPION                                                                       **
** macro version : 1.0                                                                                  **
**********************************************************************************************************

PARAMETERS

    DATA              :  Name of the input dataset containing the list of all LAB exams coming 
                         from RECOGNITION macro (Required parameter)
**********************************************************************************************************;

%MACRO INPUT_UNIT (data=);

** JRA, 18NOV2010;
** Automatic Recovery of UNITs from suffix from variables names;

data &data;
	length ROOT UNIT $20.;
	set &data;
		length=length(NAME);
		u=length-3;
		l=length-2;
		if l > 0 then do;
		suffix= substr(NAME,l,3);
		end;
			if suffix  in ('GDL' 'GL' 'MEL' 'MDL' 'MML' 'UKL' 'UML' 'UL' 'PCT' ) then f_unit=1;
			else f_unit=0;

			* Creation of a ROOT variable containing the LAB root exam without any unit suffix;

			if f_unit=1 then do;
					ROOT=substr(NAME,1,u);
					_UNIT=substr(NAME,l,3);
			end;
			else do;
				ROOT=NAME;
				_UNIT="";
			end;
			unit=_unit;

			drop suffix f_unit u l length;

run;

%if %sysfunc (fileexist(%STR(K:\SAS\EORTC macros\LABTOX\Dictionnary\CONVERT.sas7bdat))) > 0 %then %do;
libname dico "K:\SAS\EORTC macros\LABTOX\Dictionnary";
%end;

%if %sysfunc (fileexist(%STR(K:\SAS\EORTC macros\LABTOX\Dictionnary\CONVERT.sas7bdat))) = 0 %then %do;
libname dico "C:\SAS\EORTC macros\LABTOX\Dictionnary";
%end;


** JRA, 18NOV2010;
** Importation of UNIT Conversion dictionnary;

data CONVERT;
	set dico.CONVERT;
	ROOT=test;
run;

%sort(data=convert,var=root unit);
%sort(data=&data,var=root unit);

** JRA, 18NOV2010;
* Inputation of UNIT from CONVERT (dictionnary) table if UNIT is empty;
data &data.1 &data.2;
length LAB_UNIT $10.;
	merge &data(in=a) convert(keep=root unit LAB_UNIT);
	by root unit;
	if a;

	if unit="" then output &data.1 ;
	if unit ne "" then output &data.2 ;

	format LAB_UNIT $10.;

run;

data CONVERT2;
	set dico.CONVERT;
	ROOT=test;
	if (factor =1 and unit ne "MEL") or factor=100;
run;

%sort(data=convert2,var=root );
%sort(data=&data.1,var=root unit);
data &data.1;
merge &data.1 (in=a) convert2(keep=root unit LAB_UNIT);
	by root ;
	if a;

	if ROOT="ANC" and UNIT="PCT" then delete;
	if ROOT="LYM" and UNIT="PCT" then delete;

run;

data &data.;
set &data.1 &data.2;

	label unit="CODE UNIT";
	label lab_unit="UNIT";
	label REL="Relationship";
	label SAE="SAE";
	label AT="Action taken";

	if _unit ne "" then UNIT=_unit;

	if UNIT="" then do;
		UNIT="UNK";
		LAB_UNIT="Unknown";
	end;

	drop _unit;

run;

%sort(data=&data.,var=root );

	proc datasets nolist;
		 delete CONVERT CONVERT2 &data.1 &data.2;
	run;
	quit;


%MEND;

