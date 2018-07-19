 
/************************************************************************/ 
 %MACRO GCOUNT (DATA, SEL, BYVAR, OUTVAR, OUTDATA, MERGE);
/*************************************************************************

By default, this macro calculates the number of records in the input 
dataset for each patient. 

Version : 1.00    Created : 21/04/94    Tested  :

Modified : 17/05/2006  Sort of the input dataset by the BYVAR variable. 
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

**************************************************************************

PARAMETERS :

    DATA    = Name of the input dataset. (Required parameter)
	SEL     = Logical condition to select records from the input dataset
	          if not all records are to be taken into account.
	          (Optional parameter)
	          Default : SEL = 1, i.e. select all records.
	OUTVAR  = Name of the output variable containing the statistic for 
	          each group. (Optional parameter)
	          Default : OUTVAR = 'C' + dataset-name.
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
 
    GCOUNT-MACRO :
    
    In its simplest form, this macro performs the default actions 
    described above. For example,
                        %GCOUNT(DATA=FORM31);
    calculates the number of follow up records (forms) per patient. The 
    new variable containing the number of follow up records will be added
    to the PATIENT dataset under the name CFORM31. If a patient has no 
    records in the input dataset, he will receive a missing value for the 
    new variable.

    Instead of using the default name, i.e. the character 'C' followed by 
    the name of the input dataset, you can specify a name yourself. For 
    example,
                %GCOUNT(DATA=FORM31,OUTVAR=N_FUP);
    will add the variable N_FUP to the dataset PATIENT.
    
    An output dataset can be created by specifying its name. For example,
            %GCOUNT(DATA=FORM31,OUTVAR=N_FUP,OUTDATA=FOLUP);
    will create the output dataset FOLUP. The FOLUP dataset will contain
    two variables : PATID and N_FUP. If a patient has no records in the 
    input dataset, he will have no record in the output dataset.
    If an output dataset is specified, the new variable will not be added 
    to the PATIENT dataset
    
    By default, all records from the input dataset are taken into account
    for calculating the number of records. By specifying a selection, only
    those records for which the condition is true are taken into account.
    For example,
                    %GCOUNT(DATA=FORM31,SEL=(PROG=2));
    will count the number of records where the variable 'PROG' is equal 
    to 2.

    ADVANCED TOPICS / HANDLING 'NON-SMART DATA'
    
    By specifying a merge dataset with the MERGE= option, you can add the
    new variable to another dataset than the PATIENT dataset. Note that no
    output dataset can be created at the same time.
    
    Using a group-variable (BYVAR) other than PATID, will calculate the 
    number of records in the input dataset for each group, defined by the 
    BYVAR. This might be necessary to analyse sequential forms with 
    "subsequences", for example, a sequential treatment form (TRT) on 
    which a physician can record any number and type of toxicities. To 
    calculate the number of times a toxicity was reported, per treatment
    cycle (CYCLE), you might give the following statement :
           %GCOUNT(DATA=TRT, BYVAR=CYCLE TOXTYPE, OUTDATA=TOX);
    Note that, if the BYVAR is not equal to PATID, the statistic is not
    calculated per patient and the new variable can not be added to the 
    PATIENT dataset. Therefore, you need to create an output dataset or 
    merge it to an appropriate dataset.
    
    
WARNINGS / NOTES :

    This macro must be used as a separate statement, i.e. it can not be 
    used within a datastep.

    The input dataset has to be sorted according to the BYVAR prior to 
    running this macro. If no BYVAR is specified, the input dataset has 
    to be sorted by PATID.

    The SAS rules for naming variables and datasets need to be followed, 
    i.e. maximum length of 8 characters. The default names generated by 
    this macro will be truncated to 8 characters if necessary.

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

%if %length(&outvar)=0 
    %then %let outvar=c&dsname;
 
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
*   SORTING OF INPUT DATASET                   *
************************************************/

proc sort data=&data out=s_&dsname;
	by &byvar ;
run; *(JRA, 17 MAY 2006); 

/************************************************
*   SELECTION                                   *
************************************************/

data &outdata (keep=&byvar);
     set s_&dsname; *(JRA, 17 MAY 2006); 
     by &byvar;
     if &sel;
run;

/************************************************
*   CALCULATION                                 *
************************************************/

data &outdata;
	 set &outdata;
	 by &byvar;
	 if first.&&bv&nbvr 
	    then &outvar=0;
	 &outvar+1;
	 if last.&&bv&nbvr;
run;

/************************************************
*   MERGE / DELETE OUTDATA                      *
************************************************/

%if %length(&merge)^=0 and &outdata=OUT
    %then %do;

		    %sort(data=&merge,var=&byvar);*(JRA, 17 MAY 2006); 

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
%PUT ERROR IN THE USE OF THIS MACRO (EORTC Data Center);
%PUT PB : You miss to use a MERGE or OUTDATA statement with your BYVAR statement;
%PUT It is not possible to merge this COLUMN with PATIENT dataset;
%PUT One or several variables from this LIST: < &BYVAR >  do not belong to PATIENT dataset;
%PUT;%PUT;
%end;

proc datasets nolist;
	delete s_&dsname;
quit;*(JRA, 17 MAY 2006); 

%mend gcount;


