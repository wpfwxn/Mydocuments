
/************************************************************************************/ 
 %MACRO GFIRST2 (DATA, SEL, VAR, ADDVAR, BYVAR, OUTVAR, OUTDATA, MERGE, MISSING);
/*************************************************************************************
 
By default, this macro returns the first non-empty value of a variable in 
the input dataset for each patient. 

Version : 1.00    Created : 21/04/94    Tested  :

Modified : 30/05/2002  To ensure unknown (.U) values are handled correctly. KM.

Modified : 24/05/2006  Sort of the input dataset by the BYVAR variable. 
                       If no BYVAR is specified, the input dataset is sorted by PATID. 
                       It is necessary to run the macro correctly.
                       Only the result of this sorting (a table named s_&dsname) is used afterwards.
                       With this intermediate table, the original order of the input dataset
                       is not modified. 
                       Sort af the &MERGE dataset by BYVAR
                       Deleting of the intermediate table s_&dsname. JRA

 Modified : 02/02/2007  Add of UPCASE for BYVAR parameter to avoid the fact that 
 					   BYVAR is CASE sensitive.
 					   Add of ERROR message in case of BYVAR statement different of PATID 
 					   without the use of MERGE or OUTDATA statement
                       JRA

Modified : 08/10/2007  Add of (in=a) and if a condition for the merge at the end: case of MERGE statement
                       JRA

Modified : 16/07/2010  Add of MISSING parameter
 					   This parameter indicates if the user wants to take into account missing value or not
 					   in the data selected.
 					   If MISSING=IN, then the macro will return the first value of a variable for each patient.
 					   If MISSING=OUT, then the macro will return the first non-empty value of a variable for each patient,
                       so excluding missing values.
                       By default, MISSING=OUT;
                       If a character variable is entered into VAR parameter, MISSING should be equal to IN
 					   otherwise GFIRST2 macro is stopped and an ERROR message is delivered;  JRA

**************************************************************************

PARAMETERS :

    DATA    = Name of the input dataset. (Required parameter)
	SEL     = Logical condition to select records from the input dataset
	          if not all records are to be taken into account.
	          (Optional parameter)
	          Default : SEL =(&VAR>.Z), i.e. select records where VAR is 
              not missing.
    VAR     = Name of the variable from the input dataset for which the 
              first value of each group has to be returned. (Required 
              parameter)
    ADDVAR  = List of additional variables to return from the record 
              containing the first value of VAR of each group. (Optional
              parameter)
              Default : ADDVAR = ' ', i.e. no additional variables.
	OUTVAR  = Name of the output variable(s) containing the statistic for 
	          each group. (Optional parameter)
	          Default : OUTVAR = 'F' + variable-name.
	OUTDATA = Name of the output dataset which contains the output 
              variable(s). (Optional parameter)
	          Default : OUTDATA = OUT.

	PARAMETERS FOR ADVANCED TOPICS
	
	BYVAR   = Variable list that defines the groups. (Optional parameter) 
	          The statistics are calculated for each group.
	          Default : BYVAR = PATID, i.e. statistics will be calculated 
	          by patient.
	MERGE   = Name of the dataset to which the output variable will be 
              added (Optional parameter)
              Default : PATIENT
              Note : if OUTDATA is specified, no merge will be performed, 
              even if the MERGE-parameter is specified.

	MISSING  = OUT | IN
 			   Paramater to indicate if GFIRST2 should take into account missing values int the data selected.
               OUT, if the user wants to exclude missing data.
               -> In this case, GFIRST2 will returns the first non-empty value of the variable for each patient
               IN, if the user wants to include missing data.
               -> In this case, GFIRST2 will returns the first value of the variable for each patient (including missing values)
               Default : OUT
USAGE : 

    GROUP-MACROS IN GENERAL :
    
    Athough these macros can be used on any data, the default settings 
    are set specifically for data exported from SMART.
    By default, all statistics are calculated by PATID, i.e. for each 
    patient. All datasets from a trial exported from SMART contain the 
    variable PATID.
    By default, the result is added as a new variable to a dataset called 
    PATIENT. All trials, exported form SMART, contain this dataset. This 
    dataset has exactly one record for each randomized/registered patient.
    The variables in the PATIENT dataset are :
        - PATID  = Patient id
        - HOSPNO = Hospital number
        - DOR    = Date of randomisation/registration
        - CNT    = Counter
        - DOB    = Date of birth

 
    In the following examples, FORM31 is the name of the dataset 
    containing the follow up information.
 
    GFIRST2-MACRO :
    
    In its simplest form, this macro performs the default actions 
    described above. For example,
                    %GFIRST2(DATA=FORM31,VAR=SDP3);
    returns for each patient the first non-missing value of the variable 
    SDP3 (site of disease progression). The new variable containing the 
    first non-missing value of SDP3 will be added to the PATIENT dataset
    under the name FSDP3. If a patient has no records in the input 
    dataset, he will receive a missing value for the new variable.

    Instead of using the default name, i.e. the character 'F' followed by 
    the name of the VAR variable, you can specify a name yourself. For 
    example,
                %GFIRST2(DATA=FORM31,VAR=SDP3,OUTVAR=FIRSTPS);
    will add the variable FIRSTPS to the dataset PATIENT.
    
    You may request additional variables from the record containing the 
    first occurence of VAR. For example, to obtain the additional 
    variables LSD3 (date last seen) and MTP3 (main therapy for this 
    progression) from the first follow up form where the variable SDP3 is 
    non-missing, you may give the following statement :
               %GFIRST2(DATA=FORM31,VAR=SDP3,ADDVAR=LSD3 MTP3);
    This statement will add the variables FSDP3, FLSD3 and FMTP3 to the 
    PATIENT dataset.

    When using the OUTVAR option to rename the output variables, the 
    first name specified will be used for the variable containing the 
    first occurence of the VAR variable. Subsequent names in the OUTVAR 
    option will rename the ADDVAR variables using the same order in which 
    they were specified in the ADDVAR option. For example,
            %GFIRST2(DATA=FORM31,VAR=SDP3,ADDVAR=LSD3 MTP3,
                    OUTVAR=FIRSTPS DATEFPS THERFPS);
    will add the variables FIRSTPS, DATEFPS and THERFPS to the PATIENT 
    dataset.
    
    An output dataset can be created by specifying its name. For example,
            %GFIRST2(DATA=FORM31,VAR=SDP3,ADDVAR=LSD3 MTP3,
                    OUTVAR=FIRSTPS DATEFPS THERFPS,OUTDATA=FSTPROG);
    will create the output dataset FSTPROG. The FSTPROG dataset will 
    contain 4 variables : PATID, FIRSTPS, DATEFPS and THERFPS. If a 
    patient has no records in the input dataset, no record will be created
    in the output dataset. If an output dataset is specified, the new 
    variables will not be added to the PATIENT dataset.
    
    By default, all records from the input dataset with non-missing values
    for the VAR variable are taken into account for returning the first 
    value of VAR. By specifying a selection, only those records for which
    the condition is true are taken into account. Note that this 
    selection is carried out before the statistic is calculated. For 
    example,
    %GFIRST2(DATA=FORM31,SEL=(SDP3=2 OR SDP3=3),VAR=SDP3,ADDVAR=LSD3 MTP3,
            OUTVAR=DISTPS DATEDPS THERDPS);
    will add the variables DISTPS, DATEDPS and THERDPS to the PATIENT 
    dataset. The new variables contain the values from first record where
    SDP3 is equal to 2 or 3, i.e. the first follow up form where the site 
    of progression was 'distant only' or 'both local and distant'. 

    ADVANCED TOPICS / HANDLING 'NON-SMART DATA'
    
    By specifying a merge dataset with the MERGE= option, you can add the
    new variable to another dataset than the PATIENT dataset. Note that 
    no output dataset can be created at the same time.
    
    Using a group-variable (BYVAR) other than PATID, will calculate the 
    number of records in the input dataset for each group, defined by the 
    BYVAR. This might be necessary to analyse sequential forms with 
    "subsequences", for example, a sequential treatment form (TRT) on 
    which a physician can record any number and type of toxicities. To 
    calculate for each toxicity with grade 3 - 4, the treatment cycle at
    which it first occured, you might give the following statement (after
    sorting TRT by TOXTYPE and CYCLE) :
    %GFIRST2(DATA=TRT,SEL=(GRADE>=3),BYVAR=TOXTYPE,VAR=CYCLE,OUTDATA=TOX);
    Note that, if the BYVAR is not equal to PATID, the statistic is not
    calculated per patient and the new variable can not be added to the 
    PATIENT dataset. Therefore, you need to create an output dataset or 
    merge it to an appropriate dataset.
    
    
WARNINGS / NOTES :

    This macro must be used as a separate statement, i.e. it can not be 
    used within a datastep.

    The input dataset has to be sorted according to the BYVAR prior to 
    running this macro. If no BYVAR is specified, the input dataset has 
    to be sorted by PATID and the variable (date) that defines the 
    ordering of the sequences.

    The SAS rules for naming variables and datasets need to be followed, 
    i.e. maximum length of 8 characters. The default names generated by 
    this macro will be truncated to 8 characters if necessary.

    Be sure that the new variable names created are unique. 

    Be sure to use a unique output dataset name when creating output 
    datasets.

    This macro can work with temporary as well as with permanent input and
    output datasets, i.e. you may use one or two-level names for datasets.

    If a character variable is entered into VAR parameter, MISSING should be equal to IN
    otherwise GFIRST2 macro is stopped and an ERROR message is delivered:
	'IT IS MANDATORY TO ENTER THE VALUE "IN" INTO "MISSING" PARAMETER IF VAR CONTAINS A CHARACTER VARIABLE
	THE EXECUTION OF GFIRST2 MACRO IS STOPPED'
    
*************************************************************************/

/************************************************
*   PARAMETER HANDLING                          *
************************************************/

**JRA, 15JUL2010, Add of MISSING parameter;
** By default, MIISING=OUT; 

%let missing=%UPCASE(&missing.);
%if %length(&missing)=0 %then %let missing=OUT; ** By default, MISSING=OUT;

%local libr dsname i;

%if %scan(&data,2) ne 
    %then %do;
            %let libr=%scan(&data,1);
            %let dsname=%scan(&data,2);
          %end;
	%else %do;
	        %let libr=WORK;
	        %let dsname=&data;
	      %end;

%if %length(&outvar)=0 
    %then %let ren= &var=f&var;
    %else %let ren= &var=%scan(&outvar,1);
%let i=1;
%do %while(%scan(&outvar,&i+1) ne) ;
    %let ren&i= %scan(&addvar,&i)=%scan(&outvar,&i+1);
    %let i=%eval(&i+1);
%end;
%do %while(%scan(&addvar,&i) ne) ;
    %let ren&i= %scan(&addvar,&i)=f%scan(&addvar,&i);
    %let i=%eval(&i+1);
%end;
%let novr=&i;
%let i=1;
%do %while(&i<&novr);
    %let ren=&ren &&ren&i;
    %let i=%eval(&i+1);
%end;
 
%if %length(&byvar)=0 
    %then %do;
            %let byvar=PATID;
            %let bv1=PATID;
            %let nbvr=1;
          %end;
    %else %do;
            %let i=1;
            %do %while(%scan(&byvar,&i) ne) ;
                %let bv&i=%scan(&byvar,&i);
                %let i=%eval(&i+1);
            %end;
            %let nbvr=%eval(&i-1);
          %end;

** JRA, 15JUL2010, HANDLING of MISSING paramter;
%if &MISSING=OUT %THEN %DO; * Case where missing data are excluded from the selection;
	%if %length(&sel)=0 
	    %then %let where=(&var>.Z);

	** Deletion of missing observations also in the case where SEL parameter is not empty;
	%if %length(&sel)^=0 
	    %then %let where=&sel. and &var>.Z;
%end;

%if &MISSING=IN %THEN %DO;* Case where missing data are included from the selection;
	%if %length(&sel)=0 
	    %then %let where=1;

	%if %length(&sel)^=0 
	    %then %let where=&sel.;
%end;

%put WHERE= &WHERE.;

%if %length(&merge)=0 and %UPCASE(&byvar)=PATID  /* JRA, 02 FEB 2007 */
    %then %let merge=&libr..PATIENT;

%if %length(&outdata)=0 
    %then %let outdata=OUT;

** JRA, 16JULY2010;
** Get back the type of VAR parameter into TYPE macro variable;
** if TYPE=1, it means that VAR contained a numeric variable;
** if TYPE=2, it means that VAR contained a character variable;

proc contents data=&data out=_&data. noprint;
run;

data _null_ ;
set _&data.;
if upcase(NAME)="%upcase(&VAR.)" then call symput('TYPE',TYPE);
run;

** JRA, 16JUM2010:
** IF VAR parameter contains a character variable and MISSING=OUT then an error message is diplayed;
%if &MISSING.=OUT and &TYPE=2 %then %do;

	%PUT ;%PUT ERROR IN THE USE OF THIS MACRO (EORTC Headquarters) :; %PUT;
	%PUT IT IS MANDATORY TO ENTER THE VALUE "IN" INTO "MISSING" PARAMETER IF VAR CONTAINS A CHARACTER VARIABLE;
	%PUT THE EXECUTION OF GFIRST2 MACRO IS STOPPED;
	%PUT;

%goto fin ;
%end;

/************************************************
*   SORTING OF INPUT DATASET                   *
************************************************/

proc sort data=&data out=s_&dsname;
	by &byvar ;
run; *(JRA, 24 MAY 2006); 

/************************************************
*   SELECTION                                   *
************************************************/

data &outdata (keep=&byvar &var &addvar);
     set s_&dsname; *(JRA, 24 MAY 2006); 
     by &byvar;
     if &where;
run;

/************************************************
*   CALCULATION                                 *
************************************************/

data &outdata (rename=(&ren));
	 set &outdata;
	 by &byvar ; *(JRA, 24 MAY 2006); 
	 if first.&&bv&nbvr;
run;

/************************************************
*   MERGE / DELETE OUTDATA                      *
************************************************/

%if %length(&merge)^=0 and &outdata=OUT
    %then %do;

			%sort(data=&merge,var=&byvar);*(JRA, 24 MAY 2006); 

            data &merge;
               merge &merge(in=a) &outdata;
               by &byvar;
			   if a;*(JRA, 8 OCT 2007);
            run;
          %end;

%if &outdata=OUT 
    %then %do;
            proc datasets nolist;
                 delete OUT;
            run;
          %end;

%if (%length(&merge)=0 and NOT(%INDEX(%UPCASE(&byvar),PATID)>0) and %length(&byvar) ne 0
and &outdata = OUT) 
or (&outdata=OUT and %UPCASE(&byvar) ne PATID and %length(&byvar) ne 0 and %length(&merge)=0) 
%then %do;
%PUT;%PUT;
%PUT ERROR IN THE USE OF THIS MACRO (EORTC Headquarters);
%PUT PB : You miss to use a MERGE or OUTDATA statement with your BYVAR statement;
%PUT It is not possible to merge this COLUMN with PATIENT dataset;
%PUT One or several variables from this LIST: < &BYVAR >  do not belong to PATIENT dataset;
%PUT;%PUT;
%end;

proc datasets nolist;
	delete s_&dsname _&data.;
quit;*(JRA, 24 MAY 2006); 

%fin: ;

%mend gfirst2;


