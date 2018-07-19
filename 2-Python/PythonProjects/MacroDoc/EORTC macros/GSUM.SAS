 
/************************************************************************/ 
 %MACRO GSUM (DATA, SEL, VAR, BYVAR, OUTVAR, OUTDATA, MERGE);
/*************************************************************************

By default, this macro returns the sum of a variable in the input dataset 
for each patient. 

Version : 1.0   Created : 21/04/94    Tested  :

Modified : 09/05/2000  1) To use any label from the input variable in the
                          label for the summary variable ("The sum of ...").
                       2) Minor changes to documentation: 
                          - References to SMART changed to VISTA 
                          - SAS version 8.0 allows longer variable names
                       Laurence Colette & John Roberts 
                       Modifications tested 09/05/2000 by John Roberts

Modified : 14/09/2001  Can now summarise a list of variables instead of just
                       one.  To do this, assign the VAR parameter to the list
                       of variables to be summarised, separated by spaces. 
                       If desired, the OUTVAR parameter may be assigned to
                       a corresponding list of variables to contain the results.
                       If the OUTVAR parameter is not assigned, each of the
                       variables containing the results will be named:
                       S + Name of original variable.
                       For example:
                       %GSUM(DATA=TRT,VAR=AMDOSE PMDOSE);
                       will add the variables SAMDOSE and SPMDOSE to the PATIENT
                       dataset.
                       %GSUM(DATA=TRT,VAR=AMDOSE PMDOSE, OUTVAR=TOTAMDOSE TOTPMDOSE);
                       will add the variables TOTAMDOSE and TOTPMDOSE to the PATIENT
                       dataset.
                       Kate Moncrieff

Modified : 17/05/2006  Sort of the input dataset by the BYVAR variable. 
                       If no BYVAR is specified, the input dataset is sorted by PATID 
                       It is necessary to run the macro correctly.
                       Only the result of this sorting (a table named s_&dsname) is used 
                       afterwards to find the sum of the VAR variable  
                       in the input dataset for each patient.   
                       With this intermediate table, the original order of the input dataset                        
                       is not modified. 
                       Sort af the &MERGE dataset by BYVAR
                       Deleting of the intermediate tables: s_&dsname, temp and _info table. 
                       JRA

Modified : 02/02/2007  Add of UPCASE for BYVAR parameter to avoid the fact that 
 					   BYVAR is CASE sensitive.
 					   Add of ERROR message in case of BYVAR statement different of PATID 
 					   without the use of MERGE or OUTDATA statement
                       JRA

Modified : 08/10/2007  Add of (in=a) and if a condition for the merge at the end: case of MERGE statement
                       JRA

**************************************************************************

PARAMETERS :

    DATA    = Name of the input dataset. (Required parameter)
	SEL     = Logical condition to select records from the input dataset
	          if not all records are to be taken into account.
	          (Optional parameter)
	          Default : SEL = 1, i.e. select all records.
    VAR     = Name of the variable(s) from the input dataset for which the 
              sum for each group has to be returned. (Required parameter)
	OUTVAR  = Name of the output variable(s) containing the statistic for 
	          each group. (Optional parameter)
	          Default : OUTVAR = 'S' + variable-name.
	OUTDATA = Name of the output dataset which contains the output 
              variable. (Optional parameter)
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

USAGE : 

    GROUP-MACROS IN GENERAL :
    
    Athough these macros can be used on any data, the default settings 
    are set specifically for data exported from VISTA.
    By default, all statistics are calculated by PATID, i.e. for each 
    patient. All datasets from a trial exported from VISTA contain the 
    variable PATID.
    By default, the calculated statistic is added as a new variable to a
    dataset called PATIENT. All trials, exported form VISTA, contain this
    dataset. This dataset has exactly one record for each randomized / 
    registered patient.
    The variables in the PATIENT dataset are :
        - PATID  = Patient id
        - HOSPNO = Hospital number
        - DOR    = Date of randomisation/registration
        - CNT    = Counter
        - DOB    = Date of birth

 
    In the following examples, TRT is the name of the dataset containing 
    the treatment information for each treatment cycle.
 
    GSUM-MACRO :
    
    In its simplest form, this macro performs the default actions 
    described above. For example,
                    %GSUM(DATA=TRT,VAR=DOSE);
    returns for each patient the total dose. The new variable DOSE will be
    added to the PATIENT dataset under the name SDOSE. The macro uses 
    the PROC UNIVARIATE procedure which disregards all missing values when 
    calculating the statistic. If a patient has only missing values for 
    the VAR variable, he will receive a missing value for the new 
    variable. If a patient has no records in the input dataset, he will
    receive a missing value for the new variable.

    Instead of using the default name, i.e. the character 'S' followed by 
    the name of the VAR variable, you can specify a name yourself. For 
    example,
                %GSUM(DATA=TRT,VAR=DOSE,OUTVAR=TOTDOSE);
    will add the variable TOTDOSE to the dataset PATIENT.
    
    An output dataset can be created by specifying its name. For example,
          %GSUM(DATA=TRT,VAR=DOSE,OUTVAR=TOTDOSE,OUTDATA=TRTDOSE);
    will create the output dataset TRTDOSE. The TRTDOSE dataset will 
    contain 2 variables : PATID and TOTDOSE. If a patient has no records
    in the input dataset, he will have no record in the output dataset. 
    If an output dataset is specified, the new variables will not be added
    to the PATIENT dataset.
    
    By default, all records from the input dataset are taken into account
    when calculating the totals of VAR. By specifying a selection, only 
    those records for which the condition is true are taken into account.
    Note that this selection is carried out before the statistic is 
    calculated. Note further that missing values are always disregarded. 
    For example,
       %GSUM(DATA=TRT,SEL=(CYCLE<=6),VAR=DOSE,OUTVAR=TOTDOSE);
    will add the variable TOTDOSE to the PATIENT dataset. The new 
    variable contains the total dose administered during the first 6 
    treatment cycles.

    ADVANCED TOPICS / HANDLING 'NON-VISTA DATA'
    
    By specifying a merge dataset with the MERGE= option, you can add the
    new variable to another dataset than the PATIENT dataset. Note that 
    no output dataset can be created at the same time.
    
    Using a group-variable (BYVAR) other than PATID, will calculate the 
    statistic for each group defined by the BYVAR. This might be necessary
    to analyse sequential forms with "subsequences", for example, a 
    sequential treatment form (TRTMED) on which a physician can record any
    number and type of concommitant medication. To calculate total dose 
    for each medication (MED) in each treatment cycle for each patient, 
    you might give the following statement (after sorting TRTMED by PATID, 
    CYCLE and MED) :
      %GSUM(DATA=TRTMED,BYVAR=PATID CYCLE MED,VAR=DOSE,OUTDATA=CONMED);
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
    i.e. minimum length of 32 characters in SAS version 8. The default 
    names generated by this macro will be truncated to 32 characters if 
    necessary.

    Be sure that the new variable names created are unique. 

    Be sure to use a unique output dataset name when creating output 
    datasets.

    This macro can work with temporary as well as with permanent input and
    output datasets, i.e. you may use one or two-level names for datasets.
    
*************************************************************************/

/************************************************
*   PARAMETER HANDLING                          *
************************************************/

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

* KM, 14 SEP 2001;
%LET i=1;
%DO %WHILE(%SCAN(&var.,&i.) ne %STR());
   %LOCAL v&i. ov&i.;
   %LET v&i.=%SCAN(&var.,&i.);
   %IF %LENGTH(&outvar.)>0 and %SCAN(&outvar.,&i.) ne %STR() %THEN
     %LET ov&i.=%SCAN(&outvar.,&i.);
   %ELSE %LET ov&i.=s&&v&i..;  
   %LET i=%EVAL(&i.+1);
%END;
%LOCAL Nvr;
%LET Nvr=%EVAL(&i.-1);

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

%if %length(&sel)=0 
    %then %let sel=1;

%if %length(&merge)=0 and %UPCASE(&byvar)=PATID  /* JRA, 02 FEB 2007 */
    %then %let merge=&libr..PATIENT;

%if %length(&outdata)=0 
    %then %let outdata=OUT;


/************************************************
*  RETRIEVE LABEL OF VAR                        *
************************************************/
/*LC&JR May2000*/

proc contents data=&data out=_info noprint  ;
run ;

data _info ;
set _info ;
select(trim(upcase(NAME)));
   * KM, 14 SEP 2001;
   %DO i = 1 %TO &Nvr.;
      when (trim(upcase("&&v&i.."))) do;
         /* Process label */
         if label=' ' then              /* No label     */
            call symput("lv&i.", trim(left(NAME)));
         else                           /* Label exists */
            call symput("lv&i.", trim(left(LABEL)));
      end;
   %END;
   otherwise;
end;
run ;

/************************************************
*   SORTING OF INPUT DATASET                   *
************************************************/

proc sort data=&data out=s_&dsname;
	by &byvar ;
run; *(JRA, 17 MAY 2006); 

/************************************************
*   SELECTION                                   *
************************************************/

data temp (keep=&byvar &var);
     set s_&dsname; *(JRA, 17 MAY 2006); 
     by &byvar;
     if &sel;
run;


/************************************************
*   CALCULATION                                 *
************************************************/

proc univariate data=temp noprint;
     var &var;
     by &byvar;
     output out=&outdata 
            sum=%DO i=1 %TO &Nvr.; &&ov&i.. %END;;  *(KM, 14 SEP 2001);
run;


/************************************************
*   MERGE / DELETE OUTDATA                      *
************************************************/

/* Merge and add label */
%if %length(&merge)^=0 and &outdata=OUT
    %then %do;

		    %sort(data=&merge,var=&byvar);*(JRA, 17 MAY 2006); 

            data &merge;
               merge &merge(in=a) &outdata;
               by &byvar;
			   if a;*(JRA, 8 OCT 2007);
               %DO i = 1 %TO &Nvr.; *(KM, 14 SEP 2001);
                 label &&ov&i..="The sum of &&lv&i.." ; /*LC&JR May2000*/
               %END;
            run;
          %end;

/* Add label  - when no merge needed */
/*LC&JR May2000*/
%if %length(&outdata)>0 and &outdata^=OUT
    %then %do;                                  
            data &outdata;
               set &outdata ;
               %DO i = 1 %TO &Nvr.;
                 label &&ov&i..="The sum of &&lv&i.." ;
               %END;
            run;
          %end;

/* Delete unwanted dataset after merge */
%if &outdata=OUT 
    %then %do;
            proc datasets nolist ;
               delete OUT;
            run;
          %end;

%if (%length(&merge)=0 and NOT(%INDEX(%UPCASE(&byvar),PATID)>0) and %length(&byvar) ne 0
and &outdata = OUT) 
or (&outdata=OUT and %UPCASE(&byvar) ne PATID and %length(&byvar) ne 0 and %length(&merge)=0) 
%then %do;
%PUT;%PUT;
%PUT ERROR IN THE USE OF THIS MACRO (EORTC Data Center);
%PUT PB : You miss to use a MERGE or OUTDATA statement with your BYVAR statement;
%PUT It is not possible to merge this COLUMN with PATIENT dataset;
%PUT One or several variables from this LIST: < &BYVAR >  do not belong to PATIENT dataset;
%PUT;%PUT;
%end;

proc datasets nolist;
	delete s_&dsname temp _info;
quit;*(JRA, 17 MAY 2006); 

%mend gsum;
