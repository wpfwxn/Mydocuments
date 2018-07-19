/***************************************************************************************************

INFERENCE2.SAS
************* 

Summarises survival data.

For each level of a test variable, calculates:
  *  N patients, N observed events,
  *  Median survival time with non parametic or reflected confidence limits,
  *  Maximum survival/censored time,
  *  % survival at a given time point with confidence limits and standard e/rror.

Also finds:
  *  Score test, Log-rank and Wilcoxon tests for difference (stratified or not)
 
It computes:
  *  Hazard ratio and confidence limits,
  *  Variance and N expected events (only if the test variable has 2 levels),

       Median survival times are calculated using the non-parametric method.

       Hazard ratios and confidence intervals are calculated using the Cox model.

Two temporary data sets are created: Inference_ByLevel and Inference_Overall. Results are 
also printed in the Output window and, if required, can be sent to a Word document.

This macro also invokes the macro KMPLOT2 to create files that can be translated into
survival curves using EORTC software GraphLib.

Version date   : June 2013
Software       : SAS version 9.3 (64bit)
Original author: Jérôme RAPION
INFERENCE2 macro is based on INFERENCE macro (Original author: Kate Moncrieff (2002))

************************************************************************************

----------------------------
HISTORY OF INFERENCE macro
----------------------------

Calculations of median survival time and % survival are based on existing macros 
MEDIANR and PCTY2.

REVISIONS

23OCT2002,KM  Adapted so that when TEST parameter is set to N, no tests are printed.
              (Works for any number of levels.  Also removes p-values from KMPLOT2 output.)
31OCT2002,KM  Corrected KMPLOT2 call.  (Added CENSVAL=...)
26MAY2003,KM  Added parameter STRATVAR to allow for stratification.
              Removed calculation of number of expected events when test variable has >2 levels.
06JUN2003,KM  Now outputs w/arning when median isn't reached.
09SEP2003,KM  Updated so that, when &Where parameter is assigned, the selection condition
              is passed to KMPlot2.  (This ensures that the condition appears in GraphLib.)
23FEB2004,LCO Updated so that the parameter &increas is added to macro call and passed on to KMplot2.
06JAN2005,JBO Updated to function with SAS version 9. Statement "survival out ..." and 
              "where stratum ^= .T".
16AUG2005,LCO Updated so that the confidence intervals for the %survival at a given time t is based 
              on the default method: log(log) transformation, also to be consistent with %pcty2 ;
              Correction so that the alpha level for the CI is passed to the macro ;
              Addition of option CONFTYPE=LOGLOG or LINEAR or any other valid value (see SAS manual) ;
27JUL2006,JRA Initialization of &WHERE parameter to 1 if &WHERE is blank.
              Simplification of the call of %KMPLOT : one call of %KMPLOT2 instead of two calls.   
01AUG2006,JRA Deletion of the description of calculation method for the confidence limits 
              for the percentage of survival in the header of column of statistical table for % survival at X year(s).				
              For instance, "% at 1 Year(s) (95% CI, LOGLOG transform)" becomes              
              "% at 1 Year(s) (95% CI)"  
07APR2010,JRA Because of SAS upgrade 9.2, there was a change of name of variable into ODS Output dataset __ParameterEstimates;
              In SAS 9.1, the name identifying the variables was "Variable". In SAS 9.2, this name is "Parameter";
			    The ODS Output table for LOGRANK matrix has also been changed from LoghomCov to LogrankhomCov in SAS 9.2;
              With the use of ODSFile parameter, change default style from EORTCStyle to EORTCStyle1;
20JAN2011,JRA Add of MEDIAN parameter to let the user choos the way of computation for the CL of the median survival
              (non parametric from Brookmeyer and Crowley or reflected method)
20FEB2012,JRA Change default value for OUTPUT parameter from OUTPUT=Y to OUTPUT=ODS

-----------------------------
HISTORY OF INFERENCE2 macro
----------------------------

15JUL2013,JRA Creation of INFERENCE2 based on INFERENCE macro.
               The layout of the output is a bit modified (in case of OUTPUT=ODS): the output is 
               split in two parts (a Non-parametric part, and a Cox model part)

               TIES parameter:
               Add of TIES parameter to let the user choosing between BRESLOW and EFRON methods.

               STRATVAR parameter:
			   STRATVAR can contain one or several variables. It may be a list of variables.

               TEST parameter: 
               It takes the values L, W, LW, S, and N instead of L, W, B and N. 
               LW replaces B to get Log-rank and Wilcoxon tests.
               S is a new option to compute the score test.
               The score test is always displayed except if TEST=N option has been choosen.
               If TESTVAR contains more than 2 levels, each test L, W, LW or S are applied (the Wald test has been removed).
               If STRATVAR parameter is used, you get also each test L, W, LW or S stratified.
               In this case, the Wald test has also been removed

               DETAILS parameter: 
               If you use DETAILS=Y with a TESVAR with 2 levels or more, you get always a common table 
               containing all the test called 'TESTS FOR DIFFERENCE', each time TEST parameter is filled in.

17JAN2014,JRA  In the previous version, if TESTVAR variable had not any format, the output table produced by INFERENCE2
               was empty. This bug has been fixed and INFERENCE2 is working properly now. 

************************************************************************************

PARAMETERS

    Data       :  Name of the data file
    TestVar    :  Test variable
    TimeVar    :  Time variable (in days)
    CensVar    :  Censoring variable (see notes)
    CensVal    :  Value of CensVar for a censored observation [default=l]
    StratVar   :  Stratification variable(s) (optional) [empty by default, i.e., no stratification]
    Test       :  Test(s) to be printed when the test variable has 2 levels:
                  L=Log-rank, W=Wilcoxon, LW=Log-rank and Wilcoxon,S=Score test, N=No tests [default=S]
    Trend      :  Set to Y if levels of the test variable are ordered [default=N]
    Alpha_HR   :  Alpha for CI around hazard ratio (in range 0.0001-0.999)
    Alpha_Surv :  Alpha for all other statistics (medians, % survival etc.)
    Conftype   : Type of confidence intervarl for the % survival (default=LOGLOG, other possibility is LINEAR or as in SAS manual ) ;
    Unit       :  Time unit to be used for displaying results (D or W or M or Y) [default=Y]
    Where      :  Selection condition
    Year       :  Year at which % survival required
    Output     :  Required form of printed output:  N=No printed output (data sets only), 
                                                    Y=Print results in output window,
                                                    ODS=Send output to a Word document using ODS
    ODSFile    :  If Output=ODS, the path and name of the 'rtf' file which is to contain the ODS
                  output. (Optional - see note below.)
    Details    :  Include extra information (such as variance) in printed output Y or N [default=N]
    PlotYN     :  Set to Y if plot required [default=Y]
    Increas    :  Set to Y if you want a death curve starting at 0 and increasing [optional so that default=N]
    ProjName   :  Project name for Graphlib (passed to KMPLOT2)
    Median     :  Way of computation for the confidence limits of the median survival
                  NP if you want to compute the confidence limit for the median with the non parametic method 
                  R  if you want to compute the confidence limit for the median with the reflected method.
				  (default is NP --> see note below.)
    Ties       : Specifies the method of handling ties in failure times 
                 B=BRESLOW: uses the approximate likelihood of Breslow (1974), 
                 E=EFRON: uses the approximate likelihood of Efron (1977),
                 [default=B], see Notes.



NOTES

    ----------------------------------------------------------------------------------------------------------
    The choice of MEDIAN parameter will indicates which method the user wants to use to compute the confidence
    limits for the median survival :
    ----------------------------------------------------------------------------------------------------------
    ** the reflected method (R):
    ----------------------------
	In this case, the confidence limits are obtained by projecting the CI limits of the survival curve parallel to 
	the time axis, on the survival curve itself. The corresponding time values are taken to be the CI for the median. 
	In this macro, the time value just below the one which is searched is choosen.
	See:  Slud, E.V., Byar, D.P., and Green, S.B., (1984), “A Comparison of Reflected Versus Test-Based Confidence 
	Intervals for the Median Survival Time, Based on Censored Data”, Biometrics, 40, 587 - 600.
    ----------------------------------
    ** the non-parametric method (NP)
    ----------------------------------
	In this case, the confidence intervals for the median survival time are based on the signed test of 
	Brookmeyer and Crowley 1982). The 100(1- alpha) % confidence interval for the median survival time is given by : 
	IC1-alpha = {t: (1-S(t)-0.5)² <= c_alp sigma²(S(t))}  where c_alp is the upper alpha percentile of a central 
	chi-squared distribution with 1 degree of freedom.
	See: SAS/STAT® PROC LIFETEST documentation, and Brookmeyer, R. and Crowley, J. (1982), “A Confidence Interval 
	for the Median Survival Time”, Biometrics, 38, 29 - 41.

    ----------------------------
    TIES parameter :
    ----------------------------
    B (Breslow): see Breslow, N. E. (1974), “Covariance Analysis of Censored Survival Data,” Biometrics, 30, 89–99. 
    E (Efron): see Efron, B. (1977), “The Efficiency of Cox’s Likelihood Function for Censored Data,” 
    Journal of the American Statistical Association, 72, 557–565. 

    ----------------------------
    CENSVAR parameter : 
    ----------------------------
    the name of the censoring variable should not be CENSORED in your dataset because it may cause some troubles 
    if you use also a Censvar PARAMETER different of 1 in the same time. (JRA, 23DEC2006)

    ----------------------------
    TESTVAR parameter : 
    ----------------------------
    The TESTVAR parameter must be assigned. If all patients are to be summarised
    together, they should all have the same value of TESTVAR.

    ----------------------------------------
    TEST, STRATVAR and TREND parameters: 
    ----------------------------------------
    The TEST parameter allows you to specify the tests to appear in the printed output when the 
    test variable has two levels and STRATVAR is NOT assigned.  However, setting TEST to N removes 
    all tests, regardless of the number of levels of the test variable (and regardless of whether 
    STRATVAR is assigned).
    If STRATVAR is assigned, the stratified test are computed (Log rank stratified, Wilcoxon stratifed, etc..).
    STRATVAR can contain one or several variables.
    When you use a list of variables, each variable should be seperated by a space.

    As well as determining which tests for difference are displayed in the output, the TEST
    parameter is also used in the invokation of KMPLOT2.

    The TREND parameter has no effect unless the test variable has more than 2 levels.

    ----------------------------------------
    ALPHA_HR and ALPHA_SURV parameters : 
    ----------------------------------------
    If only one of the two parameters ALPHA_HR and ALPHA_SURV is assigned, the 
    single value provided is used for all calculations.  If neither ALPHA_HR nor
    ALPHA_SURV are assigned, 0.05 is used for all calculations.

    ----------------------------------------------------
    OUTPUT and ODSFile parameters - ODS RTF statement: 
    -----------------------------------------------------
    Use Output=ODS if you wish to send the output to a Word document in rich text format (rtf).  
    The ODSFile parameter should contain the name of the rtf file to which you wish to send the
    output.  However, you can choose to take control yourself of opening and closing ODS 
    destination(s) by leaving the ODSFile parameter blank.  (This could be useful if you wish to
    call the macro several times and send all the results to the same file.)
 
    For Example:

    To put several sets of results in the same file:
 
            ODS RTF FILE='C:\Temp\Inference.rtf' STYLE=EORTCStyle STARTPAGE=NO;
            %INFERENCE(Data=Patient,TestVar=Trt1,TimeVar=TimePFS,CensVar=PFS,Output=ODS,PlotYN=N);
            %INFERENCE(Data=Patient,TestVar=Sex,TimeVar=TimePFS,CensVar=PFS,Output=ODS,PlotYN=N);
            ODS RTF CLOSE;
 
    To put each set of results in a separate file:
 
            %INFERENCE(Data=Patient,TestVar=Trt1,TimeVar=TimePFS,CensVar=PFS,
                       Output=ODS,ODSFile=%STR(C:\Temp\InferenceByTrt.rtf),
                       PlotYN=N);
            %INFERENCE(Data=Patient,TestVar=Sex,TimeVar=TimePFS,CensVar=PFS,
                       Output=ODS,ODSFile=%STR(C:\Temp\InferenceBySex.rtf),
                       PlotYN=N);

    If you do choose to open and close the ODS destinations yourself, please refer to the
    notes in the program header of the macro TABLES2.SAS (K:\SAS\EORTC MACROS\TABLES2.SAS).

    Note: If Output=ODS and Details=Y, do not use STARTPAGE=NO in your ODS RTF statement as
    it will cause the titles to disappear.  

***************************************************************************************************/


%MACRO INFERENCE2(Data,TestVar,TimeVar,CensVar,CensVal=1,StratVar=%STR(),
                 Test=,Trend=N,Alpha_HR=,Alpha_Surv=,conftype=LOGLOG,
                 Unit=Y,Where=,Year=1,
                 Output=ODS,ODSFile=,Details=N,
                 PlotYN=Y,Increas=,ProjName=,Median=NP,TIES=B);

footnote;

 
   /* (Set Testing to Y when testing macro to include information about N levels etc. in table title.);*/
   %LOCAL Testing;
   %LET Testing=N;


   * Standardise parameter values and set defaults;
   ***********************************************; 
%Global ScoreLabel ;

%Local Default;
 %IF %LENGTH(&Test.)=0 %THEN %LET Default=Y;
 %IF %LENGTH(&Test.)=0 %THEN %LET Test=S;

   %LET TIES=%UPCASE(&TIES.);
   %IF &TIES.=B %THEN %LET VALUETIES=BRESLOW;
   %IF &TIES.=E %THEN %LET VALUETIES=EFRON;

   %IF %EVAL(%LENGTH(&Test.)>2) %THEN %LET Test=%SUBSTR(%UPCASE(&Test.),1,2);

   %IF &Alpha_HR.=%STR() AND &Alpha_Surv.=%STR() %THEN %DO;
     %LET Alpha_HR=0.05;
     %LET Alpha_Surv=0.05;
   %END;
   %ELSE %IF &Alpha_HR.=%STR() %THEN %LET Alpha_HR=&Alpha_Surv.;
   %ELSE %IF &Alpha_Surv.=%STR() %THEN %LET Alpha_Surv=&Alpha_HR.;

   %LET Unit=%SUBSTR(%UPCASE(&Unit.),1,1);

   %LET Output=%UPCASE(&Output.);
   %IF &Output. NE N AND &Output. NE Y %THEN %LET Output = ODS;

   %LET Details=%UPCASE(&Details.);

   %IF &Conftype.=%str() %THEN %LET ConfType=LOGLOG ;
   %let Conftype=%UPCASE(&CONFTYPE) ;
   %let PLOTYN=%UPCASE(&PLOTYN) ;
   %IF (%LENGTH(&Where.)=0) %THEN %LET WHERE=1;

   %put where &where;

   * Prepare input data set;
   ************************; 

	%MACRO RCHV(SUITE=,COMPTE=,SEP= ,N=);
		%LOCAL NUMERO MOT;
		%LET NUMERO=1;
		%LET MOT=%NRBQUOTE(%SCAN(%STR(&SUITE),&NUMERO,%STR(&SEP)));

		%DO %WHILE(&MOT ^=);
		%GLOBAL PV_&NUMERO &COMPTE&NUMERO;
		%LET &COMPTE&NUMERO=%TRIM(&MOT);
		%LET PV_&NUMERO=%SUBSTR(&MOT,1,1);
		%LET NUMERO=%EVAL(&NUMERO+1);
		%LET MOT=%NRBQUOTE(%QSCAN(%STR(&SUITE),&NUMERO,%STR(&SEP)));

		%PUT MOT : &MOT;


		%END;
		%GLOBAL &N;
		%LET &N=%EVAL(&NUMERO-1);
	%MEND RCHV;

	%RCHV(SUITE=&StratVar.,COMPTE=STRAT,SEP=" ",N=NSTRAT);


   * Prepare input data set;
   ************************; 

   data __Input;
   set &Data.;
   %IF (%LENGTH(&Where.)>0) %THEN %STR(where &Where.;);
   if &CensVar.=&CensVal. then Censored=1;
   else Censored=0;
   keep &TestVar. &CensVar. Censored &TimeVar. &StratVar.;
   run;

   data __Input;
   set __Input;
   where not missing(&TestVar.);
   run;

   * Add numeric variable for levels of test variable;

   proc sort data=__Input (keep=&TestVar.) out=__TestVarValues nodupkey;
   by &TestVar.;
   run;

   data __TestVarValues;
   set __TestVarValues;
   by &TestVar.;
   __LevelNo=_n_;
   keep &TestVar. __LevelNo;
   run;  

   proc sort data=__Input;
   by &TestVar.;
   run;

   data __Input;
   merge __Input __TestVarValues;
   by &TestVar.;
   keep __LevelNo &TestVar. &CensVar. Censored &TimeVar. &StratVar.;
   run;


   * Derive local macro variables;
   ******************************;

   * Number of levels of test variable;

   data _null_;
   set __TestVarValues end=LastObs;
   by __LevelNo;
   if LastObs then call SYMPUT('NLevels',trim(left(put(__LevelNo,8.))));
   run;

   * Stratified analysis (Y/N);

   %LOCAL Stratified;
   %IF &StratVar. ^= %STR() AND %EVAL(&NLevels.>1) %THEN %LET Stratified=Y;
   %ELSE %LET Stratified=N;

   * Case: A=1 level, B=2 levels, C1=>2 levels + trend, C2=>2 levels, no trend;

   %LOCAL Case;
   %IF %EVAL(&NLevels.=1) %THEN %LET Case=A;
   %ELSE %IF %EVAL(&NLevels.=2) %THEN %LET Case=B;
   %ELSE %IF %EVAL(&NLevels.>2) %THEN %DO;
      %IF &Trend.=Y %THEN %LET Case=C1;
      %ELSE %LET Case=C2;
   %END;

   * Unit name (for printed output); 

   %LOCAL UnitName;
   %IF &Unit.=D %THEN %LET UnitName=Days;
   %ELSE %IF &Unit.=W %THEN %LET UnitName=Weeks;
   %ELSE %IF &Unit.=M %THEN %LET UnitName=Months;
   %ELSE %IF &Unit.=Y %THEN %LET UnitName=Years;

   * Variable labels and formats (for printed output);
   %GLOBAL StratVarLbl;

   DATA _null_;
	   set &Data.;
	   if _n_=1;
	   length TimeVarLbl $100;
	   if vlabel(&TimeVar.)=vname(&TimeVar.) then TimeVarLbl='Survival Time';
	   else TimeVarLbl=vlabel(&TimeVar.);
	   call symput('TimeVarLbl',trim(left(TimeVarLbl)));

	   call symput('CensVarLbl',trim(left(vlabel(&CensVar.))));


	   call symput('TestVarLbl',trim(left(vlabel(&TestVar.))));
	   call symput('TestVarFmt',trim(left(vformatn(&TestVar.))));

   RUN;

   %put &TimeVarLbl.;
   %put &CensVarLbl.;
   %put &StratVarLbl.;

   proc contents data=&Data. out=_contents noprint;
   run;

	 %DO i=1 %TO &NSTRAT.;

	   data _contents&i;
		set _contents;
	    if UPCASE(NAME)=%UPCASE("&&STRAT&i");
		keep name label format;
	   run;
	 %END;

	   data _contentsALL;
		set _contents;
       delete;
		keep name label format;
	   run;

	 %DO i=1 %TO &NSTRAT.;

	   data _contentsALL;
	   set _contentsALL _contents&i;
	   run;

	 %END;

	   %IF &Stratified.=Y %THEN %DO;

	   data _null_;
	   set _contentsALL;

		   %DO i=1 %TO &NSTRAT.;
			  call symput('StratVarLbl'||left(_n_),LABEL);
		   %END;

	   %END;
	   run;

%DO i=1 %TO &NSTRAT.;
	 %put STRAT&i &&STRAT&i &&StratVarLbl&i.;
%END;


   %IF &Stratified.=Y %THEN %DO;

	   %IF &NSTRAT.=1 %THEN %let StratVarLbl=&StratVarLbl1.;

	   %IF &NSTRAT.>1 %THEN %DO;

	   		   %let StratVarLbl=&StratVarLbl1.;;
	 		   %DO i=2 %TO &NSTRAT.;
				   %let StratVarLbl=&StratVarLbl. and &&StratVarLbl&i.;;
	   		   %END;

	   %END;


   %END;

%put StratVarLbl &StratVarLbl.;

   * Table title;

   %LOCAL Title StratTitle;
   %IF &Testing.=Y %THEN %DO;
      %IF %EVAL(&NLevels.>2) %THEN
         %LET Title=%STR(INFERENCE MACRO:  N LEVELS=&NLevels., TREND=&Trend., CASE=&Case.);
      %ELSE %LET Title=%STR(INFERENCE MACRO:  N LEVELS=&NLevels., CASE=&Case.);
   %END;
   %ELSE %IF &Output.=Y %THEN
      %LET Title=%STR(INFERENCE: &TimeVarLbl.); 
   %ELSE %LET Title=&TimeVarLbl.;
   %IF &Stratified.=Y %THEN %LET StratTitle=%STR((Stratified for &StratVarLbl.));
   %ELSE %LET StratTitle=%STR();
   %put StratTitle &StratTitle.;

   %let lengthtit=%LENGTH(&Title.);
   %let lengthstrattit=%LENGTH(&StratTitle.);

   %let lengthtitle=%EVAL(&lengthtit. + &lengthstrattit. );
   %put &lengthtitle.;

   * Confidence limits % corresponding to alphas (for printed output);

   %LOCAL CLPct_Surv;
   %LET CLPct_Surv = %TRIM(%SYSEVALF((1-&Alpha_Surv.)*100))%;
   %LOCAL CLPct_HR;
   %LET CLPct_HR = %TRIM(%SYSEVALF((1-&Alpha_HR.)*100))%;

   * Value for TEST parameter of macro KMPLOT2;
   
   %LOCAL KMPLOT2Test;
   %IF &Test.=N %THEN %LET KMPLOT2Test = %STR();
   %ELSE %DO;
   %IF &CASE.=B OR &CASE.=C2 %THEN %DO;
         %IF &Test.=W %THEN %LET KMPLOT2Test = WO;
		 %ELSE %IF &Test.=L %THEN %LET KMPLOT2Test = L;
         %ELSE %IF &Test.=LW %THEN %LET KMPLOT2Test = LW;
         %ELSE %IF &Test.=S %THEN %LET KMPLOT2Test = S;
   %END;
   %IF &CASE.=C1 %THEN %DO;
         %IF &Test.=W %THEN %LET KMPLOT2Test = WT;
		 %ELSE %IF &Test.=L %THEN %LET KMPLOT2Test = T;
         %ELSE %IF &Test.=LW %THEN %LET KMPLOT2Test = LWT;
         %ELSE %IF &Test.=S %THEN %LET KMPLOT2Test = S;
   %END;
   %END;


   * Check conditions OK for stratifying (if necessary);
   ****************************************************;

   %IF &Stratified.=Y %THEN %DO;

      proc sort data=__Input out=__StratCheck;
      by &StratVar.;
      run;

      proc univariate data=__StratCheck noprint;
      var Censored;
      by &StratVar.;
      output out=__StratCheck nobs=NPatients sum=NCensored;
      run;
      
      data _null_;
      set __StratCheck end=LastObs;
      length FewEvents ManyLevels $1;
      retain FewEvents ManyLevels;
      if _n_=1 then do;
         FewEvents='N';
         ManyLevels='N';
      end;
      NEvents=NPatients-NCensored;
      if NEvents<10 then FewEvents='Y';
      if LastObs then do;
         call symput('FewEvents',trim(left(FewEvents)));
         if _n_>6 then ManyLevels='Y';
         call symput('ManyLevels',trim(left(ManyLevels)));
      end;
      run;

      %LOCAL ContinueYN;
      %LET ContinueYN=Y;

      * Issue w/arning if any stratification level has fewer than 10 events;

      %IF &FewEvents.=Y %THEN %DO;
         %WINDOW FewEventsWin
                 #4 @5 "W" @6 "ARNING: At least one stratification level has fewer than 10 events."
                 #5 @5 "You may need to consider combining or discarding levels."
                 #8 @5 "Do you wish to continue?  Enter Y or N, then press <ENTER>:" @66 ContinueYN 1 attr=underline;
         %DISPLAY FewEventsWin;
         %IF %UPCASE(&ContinueYN.)=N %THEN %GOTO EndOfMacro;
      %END;

      * Issue w/arning if stratification variable has more than 6 levels;

      %IF &ManyLevels.=Y %THEN %DO;
         %WINDOW ManyLevelsWin
                 #4 @5 "W" @6 "ARNING: The stratification variable has more than 6 levels."
                 #5 @5 "You may need to consider combining or discarding levels."
                 #8 @5 "Do you wish to continue?  Enter Y or N, then press <ENTER>:" @66 ContinueYN 1 attr=underline;
         %DISPLAY ManyLevelsWin;
         %IF %UPCASE(&ContinueYN.)=N %THEN %GOTO EndOfMacro;
      %END;

   %END;


   * Switch off RTF destination while results are put together;
   ***********************************************************;
   %IF &Output.=ODS AND &ODSFile.=%STR() %THEN %STR(ods rtf exclude all;);


   * Obtain all required data sets from LIFETEST procedure;
   *******************************************************; 

  %IF &Case.=A %THEN %DO;
   ods listing file = "C:\temp\log.txt";
	  ods output censoredsummary=__CensoredSummary;
	  ods output quartiles=_quartiles;    * JRA, Nov 2010, add of quartiles output dataset;

	   * JRA, Nov 2010, deletion of notables option;
	   proc lifetest data=__Input alpha=&Alpha_Surv. ALPHAQT=&Alpha_Surv.; ** JRA, 24JAN2011, add of AlphaQT option for quartiles;
	   survival out=__OutSurv conftype=&Conftype stderr ;
	   time &TimeVar.*&CensVar.(&CensVal.);
	   strata __LevelNo ;
	   run ;

	   ods output close;
   ods listing ;

   * Remove "all groups combined" record from CENSOREDSUMMARY data set;

   proc sort data=__CensoredSummary;
   where Stratum ^= .T;
   by __LevelNo;
   run;

  %END;


   %IF &Case.=B %THEN %DO;

   ods listing file = "C:\temp\log.txt";

	ods output homtests=__HomTests;
	ods output homstats=__HomStats;
	ods output logrankhomcov=__LogHomCov; * JRA, April2010, change from loghomcov to logrankhomcov;
    ods output CensoredSummary = __CensoredSummaryALL; 


	   * JRA, Nov 2010, deletion of notables option;
	   proc lifetest data=__Input alpha=&Alpha_Surv. ALPHAQT=&Alpha_Surv.; ** JRA, 24JAN2011, add of AlphaQT option for quartiles;
	   survival out=__OutSurv conftype=&Conftype stderr ;
	   time &TimeVar.*&CensVar.(&CensVal.);

	   %IF &Stratified.^=Y %THEN %DO;
	   strata __LevelNo ;
	   %END;

	   %IF &Stratified.=Y %THEN %DO;
	   strata &Stratvar./group= __LevelNo ;
	   %END;

	   run ;

	   ods output close;


	  ods output censoredsummary=__CensoredSummary;
	  ods output quartiles=_quartiles;    * JRA, Nov 2010, add of quartiles output dataset;

	   * JRA, Nov 2010, deletion of notables option;
	   proc lifetest data=__Input alpha=&Alpha_Surv. ALPHAQT=&Alpha_Surv.; ** JRA, 24JAN2011, add of AlphaQT option for quartiles;
	   survival out=__OutSurv conftype=&Conftype stderr ;
	   time &TimeVar.*&CensVar.(&CensVal.);
	   strata __LevelNo ;

	   run ;

	   ods listing ;

   * Remove "all groups combined" record from CENSOREDSUMMARY data set;

	   proc sort data=__CensoredSummary;
	   where Stratum ^= .T;
	   by __LevelNo;
	   run;

   * Convert __LevelNo from char back to num in HOMSTATS data set;

	      data __HomStats;
	      set __HomStats (rename=(__LevelNo=__LevelNoCh));
	      __LevelNo=input(__LevelNoCh,8.);
	      drop __LevelNoCh;
	      run;

	      proc sort data=__HomStats;
	      by __LevelNo;
	      run;

  %END;



   %IF &Case.=C1 %THEN %DO;

   ods listing file = "C:\temp\log.txt";
	ods output trendtests=__trendtests ; 
    ods output CensoredSummary = __CensoredSummaryALL; 

	   * JRA, Nov 2010, deletion of notables option;
	   proc lifetest data=__Input alpha=&Alpha_Surv. ALPHAQT=&Alpha_Surv.; ** JRA, 24JAN2011, add of AlphaQT option for quartiles;
	   survival out=__OutSurv conftype=&Conftype stderr ;
	   time &TimeVar.*&CensVar.(&CensVal.);

	   %IF &Stratified.^=Y %THEN %DO;
	   strata __LevelNo/trend ;
	   %END;

	   %IF &Stratified.=Y %THEN %DO;
	   strata &Stratvar./trend group= __LevelNo ;
	   %END;

	   run ;

	   ods output close;


	  ods output censoredsummary=__CensoredSummary;
	  ods output quartiles=_quartiles;    * JRA, Nov 2010, add of quartiles output dataset;

	   * JRA, Nov 2010, deletion of notables option;
	   proc lifetest data=__Input alpha=&Alpha_Surv. ALPHAQT=&Alpha_Surv.; ** JRA, 24JAN2011, add of AlphaQT option for quartiles;
	   survival out=__OutSurv conftype=&Conftype stderr ;
	   time &TimeVar.*&CensVar.(&CensVal.);
	   strata __LevelNo/trend ;

	   run ;

	   ods output close;



	   ods listing ;


   * Remove "all groups combined" record from CENSOREDSUMMARY data set;

   proc sort data=__CensoredSummary;
   where Stratum ^= .T;
   by __LevelNo;
   run;


  %END;


   %IF &Case.=C2 %THEN %DO;

   ods listing file = "C:\temp\log.txt";
      ods output homtests=__HomTests;
	  ods output homstats=__HomStats;
	  ods output logrankhomcov=__LogHomCov; * JRA, April2010, change from loghomcov to logrankhomcov;
      ods output CensoredSummary = __CensoredSummaryALL; 


	   * JRA, Nov 2010, deletion of notables option;
	   proc lifetest data=__Input alpha=&Alpha_Surv. ALPHAQT=&Alpha_Surv.; ** JRA, 24JAN2011, add of AlphaQT option for quartiles;
	   survival out=__OutSurv conftype=&Conftype stderr ;
	   time &TimeVar.*&CensVar.(&CensVal.);

	   %IF &Stratified.^=Y %THEN %DO;
	   strata __LevelNo ;
	   %END;

	   %IF &Stratified.=Y %THEN %DO;
	   strata &Stratvar./group= __LevelNo ;
	   %END;

		run ;

	   ods output close;


	  ods output censoredsummary=__CensoredSummary;
	  ods output quartiles=_quartiles;    * JRA, Nov 2010, add of quartiles output dataset;

	   * JRA, Nov 2010, deletion of notables option;
	   proc lifetest data=__Input alpha=&Alpha_Surv. ALPHAQT=&Alpha_Surv.; ** JRA, 24JAN2011, add of AlphaQT option for quartiles;
	   survival out=__OutSurv conftype=&Conftype stderr ;
	   time &TimeVar.*&CensVar.(&CensVal.);
	   strata __LevelNo ;

	   run ;

	   ods output close;
	   ods listing ;

   * Remove "all groups combined" record from CENSOREDSUMMARY data set;

   proc sort data=__CensoredSummary;
   where Stratum ^= .T;
   by __LevelNo;
   run;


   * Convert __LevelNo from char back to num in HOMSTATS data set;

	      data __HomStats;
	      set __HomStats (rename=(__LevelNo=__LevelNoCh));
	      __LevelNo=input(__LevelNoCh,8.);
	      drop __LevelNoCh;
	      run;

	      proc sort data=__HomStats;
	      by __LevelNo;
	      run;


  %END;

   * Obtain all required data sets from PHREG procedure;
   ****************************************************; 

   %IF &Case.=B %THEN %DO;

   ods listing file = "C:\temp\log.txt";
      ods output parameterestimates=__ParameterEstimates;
      ods output globaltests=__GlobalTests;
      proc phreg data=__Input;
      model &TimeVar.*&CensVar.(&CensVal.)=__LevelNo / risklimits alpha=&Alpha_HR. TIES=&VALUETIES;
      %IF &Stratified.=Y %THEN %STR(strata &Stratvar.;);
      run;
      ods output close;
   ods listing ;

   %END;

   %ELSE %IF &Case.=C1 %THEN %DO;

   ods listing file = "C:\temp\log.txt";
      ods output parameterestimates=__ParameterEstimates;
      ods output globaltests=__GlobalTests;
      proc phreg data=__Input;
      model &TimeVar.*&CensVar.(&CensVal.)=__LevelNo / risklimits alpha=&Alpha_HR. TIES=&VALUETIES;
      %IF &Stratified.=Y %THEN %STR(strata &Stratvar.;);
      run;
      ods output close;
      ods listing;


   %END;

   %ELSE %IF &Case.=C2 %THEN %DO;

   ods listing file = "C:\temp\log.txt";
      ods output parameterestimates=__ParameterEstimates;
      ods output globaltests=__GlobalTests;
      proc phreg data=__Input;
      model &TimeVar.*&CensVar.(&CensVal.)=
           %DO i=2 %TO &NLevels.; %STR(Level&i.) %END; / risklimits alpha=&Alpha_HR. TIES=&VALUETIES;
      %DO i=2 %TO &NLevels.;
         if __LevelNo=&i. then Level&i.=1;
         else Level&i.=0;
      %END;
      %IF &Stratified.=Y %THEN %STR(strata &Stratvar.;);
      run;
      ods output close;
      ods listing;

    %END;


   * Get N patients and N observed events;
   **************************************;

   data __Observed;
   set __CensoredSummary (rename=(Total=NPatients Failed=Observed));
   keep __LevelNo NPatients Observed;
   label NPatients='Patients*(N)'
         Observed='Observed*Events*(O)';
   run;

   * If 2 levels, get N expected events and variance;
   *************************************************;

   %IF &Case.=B %THEN %DO;

      %IF &Stratified.=Y %THEN %DO;

         data __Expected;
         set __Observed (keep=__LevelNo Observed);
         if _n_=1 then set __ParameterEstimates (keep=Estimate StdErr);
         if __LevelNo=1 then OMinusE=0-Estimate/(StdErr**2);
         else OMinusE=Estimate/(StdErr**2);
         Expected=Observed - OMinusE;
         keep __LevelNo Expected OMinusE;
         label Expected='Expected*Events*(E)'
               OMinusE='O - E';
         format Expected OMinusE 8.2;
         run;

         data __Variance;
         set __ParameterEstimates;
         Variance=1/(StdErr**2);
         keep Variance;
         label Variance='Variance';
         format Variance 8.3;
         run;

      %END;

      %ELSE %DO;

         data __Expected;
         merge __Observed (keep=__LevelNo Observed)
               __HomStats (keep=__LevelNo LogRank rename=(LogRank=OMinusE));
         by __LevelNo;
         Expected=Observed - OMinusE;
         keep __LevelNo Expected OMinusE;
         label Expected='Expected*Events*(E)'
               OMinusE='O - E';
         format Expected OMinusE 8.2;
         run;

         data __Variance;
         set __LogHomCov;
         if _n_=1;
         Variance=_1;
         keep Variance;
         label Variance='Variance';
         format Variance 8.3;
         run;

      %END;

   %END;


   * Get hazard ratios and test statistics;
   ***************************************;

   %IF &Case.=B %THEN %DO;

      data __HazardRatio;
      set __ParameterEstimates;
      keep HazardRatio HRLowerCL HRUpperCL;
      format HazardRatio HRLowerCL HRUpperCL 8.2;
      run;

      data __ScoreTest;
         set __GlobalTests ;
         where upcase(Test)='SCORE';
         keep ProbChiSq DF test ChiSq;
         format ProbChiSq 8.3;
      run;

         data __LRWilcoxonTests;
         set __HomTests __ScoreTest 
		 end=last;
         retain LRChiSq LRDF LRProb WilcChiSq WilcDF WilcProb ScoreProb ScoreDF;
         select (upcase(Test));
            when ('LOG-RANK') do;
               LRChiSq=ChiSq;
               LRDF=compress(DF);
               LRProb=ProbChiSq;
            end;
            when ('WILCOXON') do;
               WilcChiSq=ChiSq;
               WilcDF=compress(DF);
               WilcProb=ProbChiSq;
            end;
             when ('SCORE') do;
               ScoreChiSq=ChiSq;
               ScoreDF=compress(DF);
               ScoreProb=ProbChiSq;
            end;
           otherwise;
         end;
         if last;
         keep LRChiSq LRDF LRProb WilcChiSq WilcDF WilcProb ScoreChiSq ScoreDF ScoreProb;
		 ;


		%IF &Stratified.^=Y %THEN %DO;
         label LRChiSq='Chi-Square*(Log-Rank)'
               LRDF='DF'
               LRProb='P-Value*(Log-Rank)'
               WilcChiSq='Chi-Square*(Wilcoxon)'
               WilcDF='DF'
               WilcProb='P-Value*(Wilcoxon)'
               ScoreProb='P-Value*(Score Test)'
               ScoreDF='DF'
               ScoreChiSq='Chi-Square*(Score Test)';
		%END;
		%IF &Stratified.=Y %THEN %DO;
         label LRChiSq='Chi-Square*(Log-Rank)'
               LRDF='DF'
               LRProb='P-Value*(Log-Rank stratified)'
               WilcChiSq='Chi-Square*(Wilcoxon )'
               WilcDF='DF'
               WilcProb='P-Value*(Wilcoxon stratified)'
               ScoreProb='P-Value*(Score Test stratified)'
               ScoreDF='DF'
               ScoreChiSq='Chi-Square*(Score Test)';
		%END;


         format LRChiSq WilcChiSq ScoreChiSq 8.3 LRProb WilcProb ScoreProb 8.3;
         run; 


   %END;

   %ELSE %IF &Case.=C1 %THEN %DO;

      data __HazardRatio;
      set __ParameterEstimates;
      keep HazardRatio HRLowerCL HRUpperCL;
      format HazardRatio HRLowerCL HRUpperCL 8.2;
      run;

      data __ScoreTest;
         set __GlobalTests ;
         where upcase(Test)='SCORE';
         keep ProbChiSq DF test ChiSq;
         format ProbChiSq 8.3;
      run;

       data __LRWilcoxonTests;
         set __trendtests __ScoreTest 
		 end=last;
         retain LRz LRDF LRProb Wilcz WilcDF WilcProb ScoreProb ScoreDF;
         select (upcase(Test));
            when ('LOG-RANK') do;
               LRz=zScore;
               LRDF=compress(1);
               LRProb=Probz;
            end;
            when ('WILCOXON') do;
               Wilcz=zScore;
               WilcDF=compress(1);
               WilcProb=Probz;
            end;
             when ('SCORE') do;
               ScoreChiSq=ChiSq;
               ScoreDF=compress(DF);
               ScoreProb=ProbChiSq;
            end;
           otherwise;
         end;
         if last;
         keep LRz LRDF LRProb Wilcz WilcDF WilcProb ScoreChiSq ScoreProb ScoreDF;



		%IF &Stratified.^=Y %THEN %DO;
         label LRz='Z-score*(Log-Rank)'
               LRDF='DF'
               LRProb='P-Value*(Trend Log-Rank)'
               Wilcz='Z-score*(Wilcoxon)'
               WilcDF='DF'
               WilcProb='P-Value*(Trend Wilcoxon)'
               ScoreChiSq='Chi-Square*(Score test)'
               ScoreDF='DF*(Score test)'
               ScoreProb='P-Value*(Trend Score test)' ;
		%END;
		%IF &Stratified.=Y %THEN %DO;
         label LRz='Z-score*(Log-Rank)'
               LRDF='DF'
               LRProb='P-Value*(Trend Log-Rank stratified)'
               Wilcz='Z-score*(Wilcoxon)'
               WilcDF='DF'
               WilcProb='P-Value*(Trend Wilcoxon stratified)'
               ScoreChiSq='Chi-Square*(Score test)'
               ScoreDF='DF*(Score test)'
               ScoreProb='P-Value*(Trend Score test stratified)' ;
		%END;


         format LRz Wilcz ScoreChiSq 8.3 LRProb WilcProb ScoreProb 8.3;
         run; 

   %END;

   %ELSE %IF &Case.=C2 %THEN %DO;

      *JRA, APR2010, Because of SAS 9.2 upgrade, change from Variable to Parameter in the name of the variable in __ParameterEstimates ods output dataset;

      data __HazardRatio;
      set __ParameterEstimates ;
      __LevelNo=input(substr(Parameter,6),8.);** JRA, Apr2010 hange here;
      keep __LevelNo HazardRatio HRLowerCL HRUpperCL ;
      format HazardRatio HRLowerCL HRUpperCL 8.2 ;
      run;

      data __ScoreTest;
         set __GlobalTests ;
         where upcase(Test)='SCORE';
         keep ProbChiSq DF test ChiSq;
         format ProbChiSq 8.3;
      run;

      data __LRWilcoxonTests;
         set __HomTests __ScoreTest end=last;
         retain LRChiSq LRDF LRProb WilcChiSq WilcDF WilcProb;
         select (upcase(Test));
            when ('LOG-RANK') do;
               LRChiSq=ChiSq;
               LRDF=DF;
               LRProb=ProbChiSq;
            end;
            when ('WILCOXON') do;
               WilcChiSq=ChiSq;
               WilcDF=DF;
               WilcProb=ProbChiSq;
            end;
             when ('SCORE') do;
               ScoreChiSq=ChiSq;
               ScoreDF=compress(DF);
               ScoreProb=ProbChiSq;
            end;
           otherwise;
         end;
         if last;
         keep LRChiSq LRDF LRProb WilcChiSq WilcDF WilcProb ScoreChiSq ScoreDF ScoreProb;


		%IF &Stratified.^=Y %THEN %DO;
         label LRChiSq='Chi-Square*(Log-Rank)'
               LRDF='DF*(Log-Rank)'
               LRProb='P-Value*(Log-Rank)'
               WilcChiSq='Chi-Square*(Wilcoxon)'
               WilcDF='DF*(Wilcoxon)'
               WilcProb='P-Value*(Wilcoxon)'
               ScoreChiSq='Chi-Square*(Score test)'
               ScoreDF='DF*(Score test)'
               ScoreProb='P-Value*(Score test)'
		 ;
		%END;
		%IF &Stratified.=Y %THEN %DO;
         label LRChiSq='Chi-Square*(Log-Rank)'
               LRDF='DF*(Log-Rank)'
               LRProb='P-Value*(Log-Rank stratified)'
               WilcChiSq='Chi-Square*(Wilcoxon)'
               WilcDF='DF*(Wilcoxon)'
               WilcProb='P-Value*(Wilcoxon stratified)'
               ScoreChiSq='Chi-Square*(Score test)'
               ScoreDF='DF*(Score test)'
               ScoreProb='P-Value*(Score test stratified)'
		 ;
		%END;

         format LRChiSq WilcChiSq ScoreChiSq 8.3 LRProb WilcProb ScoreProb 8.3;
       run; 


   %END;

   ** Get MaxTime ;

   	   proc univariate data=__OutSurv noprint;
	   by __LevelNo;
	   var &TimeVar.;
	   output out=__MaxTime max=MaxTime;
	   run;

	%IF &MEDIAN.=R %THEN %DO;  
	*JRA, Nov2010, Let the user choose the way of computation for Median CIs: R is reflected method;

	   * Get reflected medians;
	   ***********************;

	   data __OutSurvEvents;
	   set __OutSurv;
	   where _CENSOR_=0;
	   keep __LevelNo &TimeVar. Survival SDF_LCL SDF_UCL;
	   run;

	   data _null_;
	   set __OutSurv;
	   where not missing(Survival);
	   by __LevelNo &TimeVar. descending Survival;
	   if last.__LevelNo and Survival >= 0.50001;
	   put @1 'W' @2 'ARNING: MEDIAN NOT REACHED FOR LEVEL' __LevelNo 2. ' OF TEST VARIABLE';
	   run;

	   data __EarliestBelow;
	   set __OutSurvEvents;
	   by __LevelNo &TimeVar.;
	   where Survival<0.5;
	   if first.__LevelNo;
	   keep __LevelNo &TimeVar. SDF_LCL SDF_UCL;
	   rename &TimeVar.=EarliestBelow
	          SDF_LCL=SDFLCLBelow
	          SDF_UCL=SDFUCLBelow;
	   run;
	   
	   data __LatestNearAndAbove;
	   set __OutSurvEvents;
	   by __LevelNo &TimeVar.;
	   where Survival>=0.5 and Survival < 0.50001;
	   if last.__LevelNo;
	   keep __LevelNo &TimeVar. SDF_LCL SDF_UCL;
	   rename &TimeVar.=LatestNearAndAbove
	          SDF_LCL=SDFLCLAbove
	          SDF_UCL=SDFUCLAbove;
	   run;

	   data __MedianTime;
	   merge __EarliestBelow (in=inBelow) __LatestNearAndAbove (in=inAbove);
	   by __LevelNo;
	   MedTime=mean(EarliestBelow,LatestNearAndAbove);
	   if inBelow then do;
	      LOL=SDFLCLBelow;
	      UPL=SDFUCLBelow;
	   end;
	   else do;
	      LOL=SDFLCLAbove;
	      UPL=SDFUCLAbove;
	   end;
	   keep __LevelNo MedTime LOL UPL;
	   run;

	   data __ReflectedCIs;
	   merge __OutSurvEvents __MedianTime (in=inMed keep=__LevelNo LOL UPL);
	   by __LevelNo;
	   if inMed;
	   keep __LevelNo LOL UPL &TimeVar. Survival;
	   run;

	   proc sort data=__ReflectedCIs;
	   by __LevelNo Survival;
	   run;

	   data __ReflectedCIs;
	   set __ReflectedCIs;
	   by __LevelNo Survival;
	   retain MedLCL MedUCL;
	   if first.__LevelNo then do;
	      MedLCL=.;
	      MedUCL=.;
	   end;
	   if Survival<=UPL then MedLCL=&TimeVar.;
	   if Survival<=LOL then MedUCL=&TimeVar.;
	   * (...Will eventually get time associated with max survival <= UPL/LOL.);
	   if last.__LevelNo;
	   keep __LevelNo MedLCL MedUCL;
	   run;

	   data __MedianTime;
	   merge __MedianTime __ReflectedCIs __MaxTime;
	   by __LevelNo;
	   run;

   %END;
	
   %IF &MEDIAN.=NP %THEN %DO;
	*JRA, Nov2010, Let the user choose the way of computation for Median CIs: NP is Non Parametric method;

	   data __MedianTime;
		   set _quartiles;
	   		where percent=50;
			   MedTime=estimate;
		       MedLCL=LowerLimit;
		       MedUCL=UpperLimit;
			keep MedTime MedLCL MedUCL __LevelNo;
	   run;
	
	   data __MedianTime;
		   merge __MedianTime __MaxTime;
		   by __LevelNo;
	   run;

   %END;

   data __MedianTime;
	   set __MedianTime;

   %IF &Unit.=W %THEN %DO;
      MedTime=round(MedTime/7,0.01);
      MedLCL=round(MedLCL/7,0.01);
      MedUCL=round(MedUCL/7,0.01);
      MaxTime=round(MaxTime/7,0.01);
   %END;
   %ELSE %IF &Unit.=M %THEN %DO;
      MedTime=round(MedTime/30.4375,0.01);
      MedLCL=round(MedLCL/30.4375,0.01);
      MedUCL=round(MedUCL/30.4375,0.01);
      MaxTime=round(MaxTime/30.4375,0.01);
   %END;
   %ELSE %IF &Unit.=Y %THEN %DO;
      MedTime=round(MedTime/365.25,0.01);
      MedLCL=round(MedLCL/365.25,0.01);
      MedUCL=round(MedUCL/365.25,0.01);
      MaxTime=round(MaxTime/365.25,0.01);
   %END;
   if missing(MedTime) then MedTime=.N;
   if missing(MedLCL) then MedLCL=.N;
   if missing(MedUCL) then MedUCL=.N;
   keep __LevelNo MedTime MedLCL MedUCL MaxTime;

   label MedTime="Median*(&UnitName.)"
         MedLCL="&CLPct_Surv. Lower Confidence Limit for Median (&UnitName.)" 
         MedUCL="&CLPct_Surv. Upper Confidence Limit for Median (&UnitName.)" 
         MaxTime="Max &TimeVarLbl.*(&UnitName.)";
   format MedTime MedLCL MedUCL MaxTime 8.2;
   run;


   * Get % survival;
   ****************;

   data __PCTSurvival;
   set __OutSurv;
   where (&TimeVar.<(&Year.*365.25) and _Censor_=0);
   by __LevelNo &TimeVar.;
   if last.__LevelNo;
   TimeUsed=round(&TimeVar./365.25,.01);
   DaysDiff=abs(&TimeVar.-&Year.*365.25) ;
%if (&Increas^=Y) %then %do ;
   PCTSurvival=round(Survival*100, 0.01);
   PCTSurvLCL=round(SDF_LCL*100,0.01);
   PCTSurvUCL=round(SDF_UCL*100,0.01);
%end ;
%if (&Increas=Y) %then %do ;
   PCTSurvival=round(100-(Survival*100),0.01);
   PCTSurvUCL=round(100-(SDF_LCL*100),0.01);
   PCTSurvLCL=round(100-(SDF_UCL*100),0.01);
%end ;


PCTSurvSTD=round(100*SDF_STDERR,0.001) ;
   keep __LevelNo
        TimeUsed DaysDiff
        PCTSurvival PCTSurvLCL PCTSurvUCL PCTSurvSTD;
   label TimeUsed="Time Used (Years)"
         DaysDiff='Days out'
         PCTSurvival="% &TimeVarLbl.*at &Year. Year(s)" 
         PCTSurvLCL="&CLPct_Surv. Lower Confidence Limit" 
         PCTSurvUCL="&CLPct_Surv. Upper Confidence Limit" 
         PCTSurvSTD="Std Error*(%)";
   format PCTSurvival PCTSurvLCL PCTSurvUCL 8.1 PCTSurvSTD 8.3;
   run;


   * Define table template for ODS output if necessary;
   * Details =N;
   ***************************************************;

   %IF &Output.=ODS AND &Details.=N %THEN %DO;

      proc template;
      define table InferenceTemplate;
         header TableTitle;
         column TestVarCol NCol NCol2 MedianCol PCTSurvCol PValColW PValColL DFColNP HRCol  PValColS  DFCol;
         define TestVarCol;
            define header TestVarColHdr;
               text _label_;
               just=l;
               split='*';
            end;
            header=TestVarColHdr;
            style=data {cellwidth=175 background=cxE0E0E0};
            just=l;
            preformatted=on;
         end;
         define NCol;
            define header NColHdr;
			style=header ;
               text _label_;
               just=c;
               split='*';
            end; 
            header=NColHdr;
            %IF &TEST.=LW %THEN style=data {cellwidth=90};
            %ELSE style=data {cellwidth=110};
           just=r;
            preformatted=on;
            generic=on;
         end;
         define NCol2;
            define header NColHdr;
			style=header {borderrightstyle=dotted};
               text _label_;
               just=c;
               split='*';
            end; 
            header=NColHdr;
            %IF &TEST.=LW %THEN style=data {cellwidth=90};
            %ELSE style=data {cellwidth=110};
           just=r;
            preformatted=on;
            generic=on;
         end;
         define MedianCol;
            define header MedianColHdr;
               text _label_;
               just=c;
               split='*';
            end; 
            header=MedianColHdr;

            %IF &TEST.=LW %THEN style=data {cellwidth=165};
            %ELSE style=data {cellwidth=175};

            just=r;
            preformatted=on;
         end;
         define PCTSurvCol;
            define header PCTSurvColHdr;
			%IF &CASE. =B OR &CASE. = C1 or &CASE. = C2  %THEN %DO;
               %IF &TEST = N or &TEST = S %THEN %DO;style=header {borderrightstyle=dotted};%END;
			%ENd;

               text _label_;
               just=c;
               split='*';
            end; 
            header=PCTSurvColHdr;
			%IF &TEST.=LW %THEN style=data {cellwidth=160};
            %ELSE style=data {cellwidth=170};

            just=r;
            preformatted=on;
         end;
         define PValColL;
            define header PValColHdr;
			%IF &CASE. =B or &CASE. =C1 or &CASE. =C2 %THEN %DO;
				%IF &TEST. = L %then style=header {borderrightstyle=dotted};;
				%IF &TEST. = LW %then style=header {borderrightstyle=dotted};;
			%END;
               text _label_;
               just=c;
               split='*';
            end; 
            header=PValColHdr;

            %IF &TEST.=LW %THEN style=data {cellwidth=115};
            %ELSE style=data {cellwidth=130};

            just=r;
            preformatted=on;
            generic=on;
         end;
         define PValColW;
            define header PValColHdr;
			%IF &CASE. =B or &CASE. =C1 or &CASE. =C2  %THEN %DO;
				%IF &TEST. = W %then style=header {borderrightstyle=dotted};;
			%END;
               text _label_;
               just=c;
               split='*';
            end; 
            header=PValColHdr;

            %IF &TEST.=LW %THEN style=data {cellwidth=115};
            %ELSE style=data {cellwidth=130};

            just=r;
            preformatted=on;
            generic=on;
         end;
         define HRCol;
            define header HRColHdr;
               text _label_;
               just=c;
               split='*';
            end; 
            header=HRColHdr;

		  style=data {cellwidth=135};

            just=r;
            preformatted=on;
         end;

         define PValColS;
            define header PValColHdr2;
               text _label_;
               just=c;
               split='*';
            end; 
            header=PValColHdr2;

            %IF &TEST.=LW %THEN style=data {cellwidth=115};
            %ELSE style=data {cellwidth=130};

            just=r;
            preformatted=on;
            generic=on;
         end;
         define DFCol;
            define header DFColHdr;
               text _label_;
               just=c;
               split='*';
            end; 
            header=DFColHdr;
            style=data {cellwidth=50};
            just=c;
            preformatted=on;
            generic=on;
        end;
         define DFColNP;
            define header DFColHdr;
			style=header {borderrightstyle=dotted};
               text _label_;
               just=c;
               split='*';
            end; 
            header=DFColHdr;
            style=data {cellwidth=50};
            just=c;
            preformatted=on;
            generic=on;
        end;

		%IF &Details.=N %THEN %DO;
			%IF &CASE. ne A %THEN %DO;
	            define header nonParam;
	               text "Non-parametric";
				   style=header {cellheight=35 font_size=3 borderrightstyle=dotted}
	               start=MedianCol;
	                    %IF &TEST = L or &TEST = LW %THEN %DO;end=PValColL;%END;
	                    %IF &TEST = W  %THEN %DO;end=PValColW;%END;

				   %IF &CASE=B OR &CASE=C1 OR &CASE=C2  %THEN %DO;
		               %IF &TEST = N  or &TEST = S %THEN %DO;end=PCTSurvCol;%END;
				   %END;

	           end;
	            define header Cox;
	               text "Cox model";
				   style=header {cellheight=35 font_size=3}
	               start=HRCol;
	               end=PValColS;
	            end;
	            define header Endpoint;

				%IF &Stratified.^=Y %THEN %DO;
					%IF &lengthtitle. <=25 %THEN %DO;
		               text "&Title.";
					   style=header {cellheight=35 font_size=5 borderrightstyle=dotted textdecoration=underline}
					 %END;

					%IF &lengthtitle. >25 %THEN %DO;
		               text "&Title.";
					   style=header {cellheight=75 font_size=5 borderrightstyle=dotted textdecoration=underline}
					 %END;
				%END;

				%IF &Stratified.=Y %THEN %DO;
	               text "&Title. &StratTitle.";
					%IF &lengthtitle. <=67 %THEN %DO;
				   style=header {cellheight=75 font_size=3 borderrightstyle=dotted textdecoration=underline}
				   %END;
					%IF &lengthtitle. > 67 %THEN %DO;
				   style=header {cellheight=95 font_size=3 borderrightstyle=dotted textdecoration=underline}
				   %END;
				%END;

	               start=TestVarCol;
	               end=NCol2;
	            end;

			%END;

			%IF &CASE. = A %THEN %DO;

	            define header Endpoint;
	               text "&Title.";
				   style=header {cellheight=40 font_size=5}
	               start=TestVarCol;
	               end=PCTSurvCol;
	            end;


			%END;

		%END;


	%IF &CASE.=A OR &CASE.=B  %THEN %DO;

				%IF &TEST = L or &TEST = W %THEN %DO;
				cellstyle 
				(_col_=6 and _row_=1) as {borderrightstyle=dotted },
				(_col_=6 and _row_=2) as {borderrightstyle=dotted},
				(_col_=3 and _row_=1) as {borderrightstyle=dotted },
				(_col_=3 and _row_=2) as {borderrightstyle=dotted };
				%END;

				%IF &TEST = LW %THEN %DO;
				cellstyle 
				(_col_=7 and _row_=1) as {borderrightstyle=dotted },
				(_col_=7 and _row_=2) as {borderrightstyle=dotted},
				(_col_=3 and _row_=1) as {borderrightstyle=dotted },
				(_col_=3 and _row_=2) as {borderrightstyle=dotted };
				%END;

	 %END;


     %IF &CASE. ne A %THEN %DO;

				%IF &TEST = N or &TEST = S %THEN %DO;
				cellstyle 
				(_col_=5 and _row_=1) as {borderrightstyle=dotted },
	            (_col_=5 and _row_=2) as {borderrightstyle=dotted},
				(_col_=3 and _row_=1) as {borderrightstyle=dotted },
				 %IF &Default. =Y %then %do;
				(_col_=3 and _row_=3) as {borderrightstyle=dotted },
				(_col_=5 and _row_=3) as {borderrightstyle=dotted},
				 %END;
				(_col_=3 and _row_=2) as {borderrightstyle=dotted };
				%END;

	 %END;

	%IF &CASE.=C1 OR &CASE.=C2 %THEN %DO;

				%IF &TEST = L or &TEST = W %THEN %DO;
				cellstyle 
				%do i=1 %to %eval(&NLevels. -1);
					(_col_=6 and _row_=&i) as {borderrightstyle=dotted },
					(_col_=3 and _row_=&i) as {borderrightstyle=dotted },
			    %end;
		    	(_col_=3 and _row_=&NLevels.) as {borderrightstyle=dotted },
        		(_col_=6 and _row_=&NLevels.) as {borderrightstyle=dotted };
				%END;


				%IF &TEST = LW %THEN %DO;
				cellstyle 
				%do i=1 %to %eval(&NLevels. -1);
					(_col_=7 and _row_=&i) as {borderrightstyle=dotted },
					(_col_=3 and _row_=&i) as {borderrightstyle=dotted },
				%end;
			   	(_col_=3 and _row_=&NLevels.) as {borderrightstyle=dotted },
				(_col_=7 and _row_=&NLevels.) as {borderrightstyle=dotted };
				%END;

				%IF &TEST = N or &TEST = S %THEN %DO;

				%IF &Default. ^=Y %then %do;
				cellstyle 
				%do i=1 %to %eval(&NLevels. -1);
				(_col_=5 and _row_=&i) as {borderrightstyle=dotted},
				(_col_=3 and _row_=&i) as {borderrightstyle=dotted},
				%end;
				(_col_=5 and _row_=&NLevels.) as {borderrightstyle=dotted},
				(_col_=3 and _row_=&NLevels.) as {borderrightstyle=dotted};
				%END;


				%IF &Default. =Y %then %do;
				cellstyle 
				%do i=1 %to %eval(&NLevels.);
				(_col_=5 and _row_=&i) as {borderrightstyle=dotted},
				(_col_=3 and _row_=&i) as {borderrightstyle=dotted},
				%end;
				(_col_=5 and _row_=%eval(&NLevels.+1)) as {borderrightstyle=dotted},
				(_col_=3 and _row_=%eval(&NLevels.+1)) as {borderrightstyle=dotted};
				%END;


				%END;

		 %END;


      end;
      run;

   %END;

   * Define table template for ODS output if necessary;
   * Details =Y;
   ***************************************************;

   %IF &Output.=ODS AND &Details.=Y %THEN %DO;

      proc template;
      define table InferenceTemplate;
         column TestVarCol NCol ;
         define TestVarCol;
            define header TestVarColHdr;
               text _label_;
               just=l;
               split='*';
            end;
            header=TestVarColHdr;
            style=data {cellwidth=175};
            just=l;
            preformatted=on;
         end;
         define NCol;
            define header NColHdr;
               text _label_;
               just=c;
               split='*';
            end; 
            header=NColHdr;
           style=data {cellwidth=100};
           just=c;
            preformatted=on;
            generic=on;
         end;
      end;
      run;

   %END;


   * Change of format for TESTVAR, replacing 'missing' 'not done' and 'unknown' by '';
   *************************;

	data _null_ ;
	set &data. (obs=1);
	length fmtname format $20;
	fmtname=trim(left(vformatn(&testvar.)));
	if fmtname='$' then format='$'||trim(left(put(vformatw(&testvar.),3.)));
	else format=trim(left(vformatn(&testvar.)));
	if format="F" or format="BEST" then format="8";
	call symput('format',trim(left(format))||'.');
	call symput('form',trim(left(format)));
	keep format fmtname &testvar.;
	run;
	%put &format &form;

	proc format cntlout=__formats(where=(FMTNAME="&form."));
	run;


	**JRA, 17JAN2014, Check if there are any format for TESTVAR variable or not;
	ods listing file = "C:\temp\log.txt";

	proc datasets ;
	contents data=__formats ;
	ods output attributes=attributes;
	quit;

	ods listing;

	data attributes;
	set attributes;
	if Label2='Observations';
	run;

	data _null_;
	set attributes;
	if Label2='Observations' then do;
	call symput('NOBSFMT',cValue2);
	end;
	run;

	%put NOBSFMT &NOBSFMT ;

	**JRA, 17JAN2014, if there is a format, then modification of the format;
	%if &NOBSFMT >0 %then %do;

		data __formats;
		set __formats;
		FMTNAME="N"||"&form.";

		if compress(START)='.' or compress(START)='.U' or compress(START)='.N' then do;
		LABEL='';
		end;

		run;

		proc format cntlin=__formats;
		run;

    %end;

   * Define titles and set options for outputting results;
   ******************************************************;

   %IF (&Output.=Y OR &Output.=ODS) AND &Details.=Y %THEN %DO;
      title1 "&Title.";
      title2 "ENDPOINT: &TimeVarLbl., CENSOR: &CensVarLbl. (Censored is &CensVal.)";
   %END;
   %ELSE %IF &Output.=Y AND &Details=N %THEN %DO;
      title1 "&Title. &StratTitle.";
      title2 "ENDPOINT: &TimeVarLbl., CENSOR: &CensVarLbl. (Censored is &CensVal.)";
   %END;

   %IF &Output.=ODS %THEN %DO;
      options orientation=landscape;      
      %IF &ODSFile.^=%STR() %THEN %DO;
         ods rtf file= "&ODSFile." style=EORTCStyle1; *JRA, Apr2010, Change default style from EORTCStyle to EORTCStyle1;
      %END;
      ods rtf select all;  * (Switch rtf destination back on);
   %END;
 
   options pageno=1 nodate missing=' ';


   * Pool results and output;
   *************************;

   %IF &Case.=A %THEN %DO;

      data Inference_PerLevel;
      informat &TestVar.
               NPatients Observed
               MedTime MedLCL MedUCL MaxTime
               TimeUsed DaysDiff PCTSurvival PCTSurvLCL PCTSurvUCL PCTSurvSTD;
      merge __Observed __MedianTime __PCTSurvival __TestVarValues;
      by __LevelNo;
      keep &TestVar.
           NPatients Observed
           MedTime MedLCL MedUCL MaxTime
           TimeUsed DaysDiff PCTSurvival PCTSurvLCL PCTSurvUCL PCTSurvSTD;
      label &TestVar.="&TestVarLbl.";
      format &TestVar. &TestVarFmt..;
      run;

      %IF &Output.=Y OR &Output.=ODS %THEN %DO;

         %IF &Details.=Y %THEN %DO;

		 options center;

            title4 'NUMBER OF PATIENTS AND NUMBER OF EVENTS';
            proc print data=Inference_PerLevel label split='*' noobs 
            style (data) = data[JUST=C]
            style (HEADER) = HEADER[JUST=C];
            var &TestVar. NPatients Observed;
            run;
            title4;

            title4 "MEDIAN %UPCASE(&TimeVarLbl.)"; 
            proc print data=Inference_PerLevel label split='*' noobs 
            style (data) = data[JUST=C]
            style (HEADER) = HEADER[JUST=C];
            var &TestVar. MedTime MedLCL MedUCL MaxTime;
            run;
            title4;

            title4 "PERCENT %UPCASE(&TimeVarLbl.) AT &Year. YEAR(S)" ;
			title5 "Confidence intervals using &Conftype transform" ;
            proc print data=Inference_PerLevel label split='*' noobs 
            style (data) = data[JUST=C]
            style (HEADER) = HEADER[JUST=C];
            var &TestVar. TimeUsed PCTSurvival PCTSurvLCL PCTSurvUCL PCTSurvSTD;
            run;
            title4;

         %END;

         %ELSE %DO;

            data __Print;
            set Inference_PerLevel;
            length MedTimeStr $50;
            if MedTime=.N then MedTimeStr='Not reached';
            else MedTimeStr = trim(left(put(MedTime,9.2)))||
                            ' ('||trim(left(put(MedLCL,9.2)))||', '||trim(left(put(MedUCL,9.2)))||')';
            label MedTimeStr="Median (&CLPct_Surv. CI)*(&UnitName.)";
            length PCTSurvStr $50;
            PCTSurvStr = trim(left(put(PCTSurvival,8.1)))||
                            ' ('||trim(left(put(PCTSurvLCL,8.1)))||', '||trim(left(put(PCTSurvUCL,8.1)))||')';
            label PCTSurvStr="% at &Year. Year(s)*(&CLPct_Surv. CI)"; 
            run;

            %IF &Output.=ODS %THEN %DO;

               data _null_;
               set __Print;
               file print
                    ods=(template='InferenceTemplate'
                         columns=(TestVarCol=&TestVar.
                                  NCol=NPatients (generic=on)
                                  NCol=Observed (generic=on)
                                  MedianCol=MedTimeStr
                                  PCTSurvCol=PctSurvStr)
                         dynamic=(TitleText="&Title."));;
               put _ods_;
               format &TestVar. &TestVarFmt.30.
                      NPatients 10. Observed 10. MedTimeStr $30. PCTSurvStr $25.;
               run;

            %END;

            %ELSE %DO;

            
               proc print data=__Print label split='*' noobs
                     style (data) = data[JUST=C]
                     style (HEADER) = HEADER[JUST=C];
               var &TestVar. NPatients Observed MedTimeStr PCTSurvStr;
               run;

            %END;
  
         %END;

      %END;

   %END;

   %ELSE %IF &Case.=B %THEN %DO;

      data Inference_PerLevel;
      informat &TestVar.
               NPatients Observed Expected OMinusE
               HazardRatio HRLowerCL HRUpperCL
               MedTime MedLCL MedUCL MaxTime
               TimeUsed DaysDiff PCTSurvival PCTSurvLCL PCTSurvUCL PCTSurvSTD;
      merge __Observed __Expected __MedianTime __PCTSurvival __TestVarValues;
      by __LevelNo;
      if __LevelNo=1 then HazardRatio=1;
      else set __HazardRatio;
      keep &TestVar.
           NPatients Observed Expected OMinusE
           HazardRatio HRLowerCL HRUpperCL
           MedTime MedLCL MedUCL MaxTime
           TimeUsed DaysDiff PCTSurvival PCTSurvLCL PCTSurvUCL PCTSurvSTD;
      label &TestVar.="&TestVarLbl.";
      format &TestVar. &TestVarFmt..;
      run;

      data Inference_Overall;
         informat Variance LRChiSq LRDF LRProb WilcChiSq WilcDF WilcProb;
         set __Variance;
         set __LRWilcoxonTests;
         keep Variance LRChiSq LRDF LRProb WilcChiSq WilcDF WilcProb ScoreChiSq ScoreDF ScoreProb;
      run;

      %IF &Output.=Y OR &Output.=ODS %THEN %DO;

         %IF &Details.=Y %THEN %DO;

            data __Print;
            set Inference_PerLevel;
            if _n_=1 then set Inference_Overall (keep=Variance);
            run;

            title4 "NUMBER OF PATIENTS, NUMBER OF EVENTS AND VARIANCE %UPCASE(&StratTitle.)";

               data _null_;
               set __Print;
               file print
                    ods=(template='InferenceTemplate'
                         columns=(TestVarCol=&TestVar.
                                  NCol=NPatients (generic=on)
                                  NCol=Observed (generic=on)
                                  NCol=Expected (generic=on)
                                  NCol=OMinusE (generic=on)
                                  NCol=Variance (generic=on)
								)
						);
               put _ods_;
              run;

			%IF &Stratified.=Y %THEN %DO;

            title4 "NUMBER OF PATIENTS AND NUMBER OF EVENTS PER STRATA";

			data __censoredsummaryall;
			set __censoredsummaryall;
			  label total="Patients*(N)";
			  label failed="Observed*Events*(O)";
			  if  &STRAT1 ne ._;
            run;

              data _null_;
               set __censoredsummaryall;
               file print
                    ods=(template='InferenceTemplate'
                         columns=(

						 %DO i=1 %TO &NSTRAT;
                                  NCol=&&STRAT&i (generic=on)
						 %END;
                                  NCol=Total (generic=on)
                                  NCol=Failed (generic=on)
								)
						);
               put _ods_;
              run;

			  %END;

		  title4 "Non-parametric";

           title6 "MEDIAN %UPCASE(&TimeVarLbl.)"; 
           data _null_;
               set Inference_PerLevel;
               file print
                    ods=(template='InferenceTemplate'
                         columns=(TestVarCol=&TestVar.
                                  NCol=MedTime (generic=on)
                                  NCol=MedLCL (generic=on)
                                  NCol=MedUCL (generic=on)
                                  NCol=MaxTime (generic=on)
								)
						);
               put _ods_;
            run;


           title6 "PERCENT %UPCASE(&TimeVarLbl.) AT &Year. YEAR(S) (&Conftype tranform)" ;

           data _null_;
               set Inference_PerLevel;
               file print
                    ods=(template='InferenceTemplate'
                         columns=(TestVarCol=&TestVar.
                                  NCol=TimeUsed (generic=on)
                                  NCol=PCTSurvival (generic=on)
                                  NCol=PCTSurvLCL (generic=on)
                                  NCol=PCTSurvUCL (generic=on)
                                  NCol=PCTSurvSTD (generic=on)
								)
						);
               put _ods_;
            run;
               title6;


  		  title4 "Cox model";
          title6 "HAZARD RATIO AND CONFIDENCE LIMITS %UPCASE(&StratTitle.)";

               data _null_;
               set Inference_PerLevel;
               file print
                    ods=(template='InferenceTemplate'
                         columns=(TestVarCol=&TestVar.
                                  NCol=HazardRatio (generic=on)
                                  NCol=HRLowerCL (generic=on)
                                  NCol=HRUpperCL (generic=on)
								)
						);
               put _ods_;
              run;


            %IF &Test.^=N %THEN %DO;

                     proc print data=Inference_Overall label split='*' noobs   
                     style (data) = data[JUST=C]
                     style (HEADER) = HEADER[JUST=C];
                     title6 "TESTS FOR DIFFERENCE  %UPCASE(&StratTitle.)";
                     var LRChiSq LRDF LRProb WilcChiSq WilcDF WilcProb ScoreChiSq ScoreDF ScoreProb;
  				     run;
               run;
               title6;

            %END;
		title4;
         %END;

         %ELSE %DO;

            data __Print;
            set Inference_PerLevel;
            if _n_=1 then set Inference_Overall;
            else do;
               LRChiSq=.;
               LRDF=.;
               LRProb=.;
               WilcChiSq=.;
               WilcDF=.;
               WilcProb=.;
               WaldProb=.;
               WaldDF=.;
			   ScoreChiSq=.; 
               ScoreDF=.; 
               ScoreProb=.;
            end;
            length HRStr PvalStr $50;
            if _n_=1 then HRStr=trim(left(put(HazardRatio,8.2)));
            else HRStr = trim(left(put(HazardRatio,8.2)))||
                         ' ('||trim(left(put(HRLowerCL,8.2)))||', '||trim(left(put(HRUpperCL,8.2)))||')';

		    PvalStr=trim(left(put(ScoreProb,8.3)));


            label WilcProb='P-Value*(Wilcoxon)';
            label LRProb='P-Value*(Log-Rank)';
            label WaldProb='P-Value*(Wald)';
            label ScoreProb='P-Value*(Score test)';
			label PvalStr='P-Value*(Score test)';
            label HRStr="Hazard Ratio*(&CLPct_HR. CI)";

            length MedTimeStr $50;
            if MedTime=.N then MedTimeStr='Not reached';
            else MedTimeStr = trim(left(put(MedTime,8.2)))||
                            ' ('||trim(left(put(MedLCL,8.2)))||', '||trim(left(put(MedUCL,8.2)))||')';
            label MedTimeStr="Median (&CLPct_Surv. CI)*(&UnitName.)";
            length PCTSurvStr $50;
            PCTSurvStr = trim(left(put(PCTSurvival,8.1)))||
                            ' ('||trim(left(put(PCTSurvLCL,8.1)))||', '||trim(left(put(PCTSurvUCL,8.1)))||')';
            label PCTSurvStr="% at &Year. Year(s)*(&CLPct_Surv. CI)"; 
			sep="|";

			**JRA, 17JAN2014, if there is a format for TESTVAR variable then apply the new format to TESTVAR;
			%if &NOBSFMT >0 %then %do;
				format &TestVar. N&format.;
			%end;

            run;

			%IF &Default. =Y %then %do;
			data _null_;
			set __LRWilcoxonTests;
			LRProb2=round(LRProb,0.001);
			call symput('PvalLR',compress(put(LRProb2,8.3)));
		    run;

			data __logrank;
			length HRStr $50. ;

			HRStr="Log-rank test: ";
			PvalStr="p-value=&PvalLR.";
			sep="|";
			run;

			data __Print;
			set __Print __logrank;
			run;

			%End;
			

            %IF &Output.=ODS %THEN %DO;

               data _null_;
               set __Print;
               file print
                    ods=(template='InferenceTemplate' 
                         columns=(TestVarCol=&TestVar.
                                  NCol=NPatients (generic=on)
                                  NCol2=Observed (generic=on)
                                  MedianCol=MedTimeStr
                                  PCTSurvCol=PctSurvStr
                                  %IF &Test.^=N %THEN %DO;

                                        %IF &Test.=W %THEN %DO;
											PValColW=WilcProb (generic=on)
										%END;

                                        %IF &Test.=L %THEN %DO;
											PValColL=LRProb (generic=on)
										%END;

                                        %IF &Test.=LW %THEN %DO;
											PValColW=WilcProb (generic=on)
											PValColL=LRProb  (generic=on)
										%END;

	                                 PValColS=PValStr (generic=on)

                                  %END;
                                 HRCol=HRStr

                                 )
                      );;
               put _ods_;
               format 
                      NPatients 10. Observed 10. HRStr $35. 
                      MedTimeStr $30. PCTSurvStr $20.;
               run;


            %END;

            %ELSE %DO;
            
			proc report data=__Print split='*' nowindows  HEADSKIP COMPLETECOLS ;
			column &TestVar. NPatients Observed  sep ('Non-parametric' MedTimeStr PCTSurvStr 
	            %IF &Test.=L %THEN %STR(LRProb );
	            %IF &Test.=W %THEN %STR(WilcProb );
	            %IF &Test.=LW %THEN %STR(WilcProb LRProb );
            ) 
            sep 
            ('Cox model' HRStr
	            %IF &Test.=S or &Test.=W or &Test.=L or &Test.=LW %THEN  %STR(PvalStr );

            );
            define  &TestVar. / width=17;
            define  NPatients / width=8;
            define  Observed / width=8;
            define  MedTimeStr / width=17 '*Median (95% CI)*(Years)';
            define  PCTSurvStr / width=20;

	            %IF &Test.=L %THEN %DO;
	            define  LRProb / width=14;
				%END;
	            %IF &Test.=W %THEN %DO;
	            define  WilcProb / width=14;
				%END;
	            %IF &Test.=LW %THEN %DO;
	            define  LRProb / width=14;
	            define  WilcProb / width=17;
				%END;

            define  sep / width=5 '|*|*|';

            define  HRStr / width=20 '*Hazard Ratio* (95% CI)';

				%IF &Test.=S or &Test.=W or &Test.=L or &Test.=LW %THEN %DO;
	            define  PvalStr / width=14;
			%END;

           run;

            %END;

         %END;

      %END;

   %END;


   %ELSE %IF &Case.=C1 %THEN %DO;

      data Inference_PerLevel;
      informat &TestVar.
               NPatients Observed
               MedTime MedLCL MedUCL MaxTime
               TimeUsed DaysDiff PCTSurvival PCTSurvLCL PCTSurvUCL PCTSurvSTD;
      merge __Observed __MedianTime __PCTSurvival __TestVarValues;
      by __LevelNo;
      keep &TestVar.
           NPatients Observed
           MedTime MedLCL MedUCL MaxTime
           TimeUsed DaysDiff PCTSurvival PCTSurvLCL PCTSurvUCL PCTSurvSTD;
      label &TestVar.="&TestVarLbl.";
      format &TestVar. &TestVarFmt..;
      run;

      data Inference_Overall;
      informat HazardRatio HRLowerCL HRUpperCL;
      set __HazardRatio;
	  set __LRWilcoxonTests;
      keep HazardRatio HRLowerCL HRUpperCL
		   LRz LRDF LRProb Wilcz WilcDF WilcProb ScoreChiSq ScoreDF ScoreProb;
      run;

      %IF &Output.=Y OR &Output.=ODS %THEN %DO;

         %IF &Details.=Y %THEN %DO;

            title4 "NUMBER OF PATIENTS AND NUMBER OF EVENTS %UPCASE(&StratTitle.)";

               data _null_;
               set Inference_PerLevel;
               file print
                    ods=(template='InferenceTemplate'
                         columns=(TestVarCol=&TestVar.
                                  NCol=NPatients (generic=on)
                                  NCol=Observed (generic=on)
								)
						);
               put _ods_;
              run;
            title4;

			%IF &Stratified.=Y %THEN %DO;

            title4 "NUMBER OF PATIENTS AND NUMBER OF EVENTS PER STRATA";

			data __censoredsummaryall;
			set __censoredsummaryall;
			  label total="Patients*(N)";
			  label failed="Observed*Events*(O)";
			  if  &STRAT1 ne ._;
            run;

              data _null_;
               set __censoredsummaryall;
               file print
                    ods=(template='InferenceTemplate'
                         columns=(

						 %DO i=1 %TO &NSTRAT;
                                  NCol=&&STRAT&i (generic=on)
						 %END;
                                  NCol=Total (generic=on)
                                  NCol=Failed (generic=on)
								)
						);
               put _ods_;
              run;
            %END;


		  title4 "Non-parametric";

            title6 "MEDIAN %UPCASE(&TimeVarLbl.)"; 
               data _null_;
               set Inference_PerLevel;
               file print
                    ods=(template='InferenceTemplate'
                         columns=(TestVarCol=&TestVar.
                                  NCol=MedTime (generic=on)
                                  NCol=MedLCL (generic=on)
                                  NCol=MedUCL (generic=on)
                                  NCol=MaxTime (generic=on)
								)
						);
               put _ods_;
              run;
            title6;


 
            title6 "PERCENT %UPCASE(&TimeVarLbl.) AT &Year. YEAR(S) (&conftype transform)" ;

			  data _null_;
               set Inference_PerLevel;
               file print
                    ods=(template='InferenceTemplate'
                         columns=(TestVarCol=&TestVar.
                                  NCol=TimeUsed (generic=on)
                                  NCol=PCTSurvival (generic=on)
                                  NCol=PCTSurvLCL (generic=on)
                                  NCol=PCTSurvUCL (generic=on)
                                  NCol=PCTSurvSTD (generic=on)
								)
						);
               put _ods_;
              run;
            title6;

  		  title4 "Cox model";

            title6 "HAZARD RATIO AND CONFIDENCE LIMITS %UPCASE(&StratTitle.)";
            proc print data=Inference_Overall label split='*' noobs   style (data) = data[JUST=C]
                     style (HEADER) = HEADER[JUST=C];
            var HazardRatio HRLowerCL HRUpperCL;
            run;
            title6;


            %IF &Test.^=N %THEN %DO;

                     proc print data=Inference_Overall label split='*' noobs   
                     style (data) = data[JUST=C]
                     style (HEADER) = HEADER[JUST=C];
                     title6 "TREND TESTS FOR DIFFERENCE  %UPCASE(&StratTitle.)";
                     var LRz LRDF LRProb Wilcz WilcDF WilcProb ScoreChiSq ScoreDF ScoreProb;
  				     run;
               run;
               title6;

            %END;

		title4;

         %END;

         %ELSE %DO;

            data __Print;
            set Inference_PerLevel;
            if _n_=1 then set Inference_Overall;
            else do;
               HazardRatio=.;
               HRLowerCL=.;
               HRUpperCL=.;
               WaldProb=.;
               WaldDF=.;
			   LRDF=.; LRProb=.; WilcDF=.; WilcProb=.;
			   LRz=.; Wilcz=.; ScoreChiSq=.; ScoreDF=.; ScoreProb=.;

            end;
            if _n_=1 then do;
               length HRStr PvalStr $50;
               HRStr = trim(left(put(HazardRatio,8.2)))||
                          ' ('||trim(left(put(HRLowerCL,8.2)))||', '||trim(left(put(HRUpperCL,8.2)))||')';
               label HRStr="Hazard Ratio*(&CLPct_HR. CI)";
            end; 
            length MedTimeStr $50;
            if MedTime=.N then MedTimeStr='Not reached';
            else MedTimeStr = trim(left(put(MedTime,8.2)))||
                            ' ('||trim(left(put(MedLCL,8.2)))||', '||trim(left(put(MedUCL,8.2)))||')';
            label MedTimeStr="Median (&CLPct_Surv. CI)*(&UnitName.)";
            length PCTSurvStr $50;
            PCTSurvStr = trim(left(put(PCTSurvival,8.1)))||
                            ' ('||trim(left(put(PCTSurvLCL,8.1)))||', '||trim(left(put(PCTSurvUCL,8.1)))||')';
            label PCTSurvStr="% at &Year. Year(s)*(&CLPct_Surv. CI)"; 
 			sep="|";

		    PvalStr=trim(left(put(ScoreProb,8.3)));
			label PvalStr='P-Value*(Score test)';

            label ScoreProb='P-Value*(Trend*Score test)';
            label WilcProb='P-Value*(Trend Wilcoxon)';
            label LRProb='P-Value*(Trend Log-Rank)';

			**JRA, 17JAN2014, if there is a format for TESTVAR variable then apply the new format to TESTVAR;

			%if &NOBSFMT >0 %then %do;
				format &TestVar. N&format.;
			%end;

           run;

	
		   %IF &Default. =Y %then %do;
			data _null_;
			set __LRWilcoxonTests;
			LRProb2=round(LRProb,0.001);
			call symput('PvalLR',compress(put(LRProb2,8.3)));
			run;

			%put PvalLR &PvalLR.;

			data __logrank;
			length HRStr $50. ;

			HRStr="Log-rank test: ";
			PvalStr="p-value=&PvalLR.";
			sep="|";
			run;

			data __Print;
			set __Print __logrank;
			run;

			%End;



            %IF &Output.=ODS %THEN %DO;

               data _null_;
               set __Print;
               file print
                    ods=(template='InferenceTemplate'
                         columns=(TestVarCol=&TestVar.
                                  NCol=NPatients (generic=on)
                                  NCol2=Observed (generic=on)

                                   %IF &Test.=W %THEN %DO;
										PValColW=WilcProb (generic=on)
								   %END;

                                   %IF &Test.=L %THEN %DO;
										PValColL=LRProb (generic=on)
								   %END;

                                   %IF &Test.=LW %THEN %DO;
										PValColW=WilcProb (generic=on)
										PValColL=LRProb (generic=on)
								   %END;

                                  HRCol=HRStr

								  %IF &TEST ne  N %THEN %DO;
								  PValColS=PvalStr (generic=on)
								  %END;

                                  MedianCol=MedTimeStr
                                  PCTSurvCol=PctSurvStr)
                        );
               put _ods_;
               format 
                      NPatients 10. Observed 10. HRStr $20. 
                      MedTimeStr $30. PCTSurvStr $20.;
               run;

            %END;

            %ELSE %DO;
            
 			proc report data=__Print split='*' nowindows  HEADSKIP COMPLETECOLS ;
			column &TestVar. NPatients Observed  sep ('Non-parametric'  MedTimeStr PCTSurvStr
	            %IF &Test.=L %THEN %STR(LRProb );
	            %IF &Test.=W %THEN %STR(WilcProb );
	            %IF &Test.=LW %THEN %STR(WilcProb LRProb );
            ) 
            sep 
            ('Cox model' HRStr
	        %IF &Test.^=N %THEN  %STR(PvalStr);
            );
            define  &TestVar. / width=17;
            define  NPatients / width=8;
            define  Observed / width=8;
            define  MedTimeStr / width=17;
            define  PCTSurvStr / width=20;

            define  sep / width=5 '|*|*|';

            define  HRStr / width=17 '*Hazard Ratio* (95% CI)';

			%IF &Test.^=N %THEN %DO; 
            define  PvalStr / width=15;
			%END;

            %IF &Test.=L %THEN %DO;
            define  LRProb / width=14;
			%END;
            %IF &Test.=W %THEN %DO;
            define  WilcProb / width=14;
			%END;
            %IF &Test.=LW %THEN %DO;
            define  LRProb / width=14;
            define  WilcProb / width=17;
			%END;

           run;


            %END;

         %END;

      %END;

   %END;

   %ELSE %IF &Case.=C2 %THEN %DO;

      data Inference_PerLevel;
      informat &TestVar.
               NPatients Observed
               HazardRatio HRLowerCL HRUpperCL 
               MedTime MedLCL MedUCL MaxTime
               TimeUsed DaysDiff PCTSurvival PCTSurvLCL PCTSurvUCL PCTSurvSTD;
      merge __Observed __HazardRatio (in=inHR) __MedianTime __PCTSurvival __TestVarValues;
      by __LevelNo;
      if __LevelNo=1 and not inHR then HazardRatio=1;

      keep &TestVar.
           NPatients Observed
           HazardRatio HRLowerCL HRUpperCL 
           MedTime MedLCL MedUCL MaxTime
           TimeUsed DaysDiff PCTSurvival PCTSurvLCL PCTSurvUCL PCTSurvSTD;
      label &TestVar.="&TestVarLbl.";
      format &TestVar. &TestVarFmt..;
      run;

      data Inference_Overall;
         set __LRWilcoxonTests;
         keep LRChiSq LRDF LRProb WilcChiSq WilcDF WilcProb ScoreChiSq ScoreDF ScoreProb;
      run;

      %IF &Output.=Y OR &Output.=ODS %THEN %DO;

         %IF &Details.=Y %THEN %DO;

            title4 "NUMBER OF PATIENTS AND NUMBER OF EVENTS %UPCASE(&StratTitle.)";

               data _null_;
               set Inference_PerLevel;
               file print
                    ods=(template='InferenceTemplate'
                         columns=(TestVarCol=&TestVar.
                                  NCol=NPatients (generic=on)
                                  NCol=Observed (generic=on)
								)
						);
               put _ods_;
              run;
            title4;

			%IF &Stratified.=Y %THEN %DO;

            title4 "NUMBER OF PATIENTS AND NUMBER OF EVENTS PER STRATA";

			data __censoredsummaryall;
			set __censoredsummaryall;
			  label total="Patients*(N)";
			  label failed="Observed*Events*(O)";
			  if  &STRAT1 ne ._;
            run;

              data _null_;
               set __censoredsummaryall;
               file print
                    ods=(template='InferenceTemplate'
                         columns=(

						 %DO i=1 %TO &NSTRAT;
                                  NCol=&&STRAT&i (generic=on)
						 %END;
                                  NCol=Total (generic=on)
                                  NCol=Failed (generic=on)
								)
						);
               put _ods_;
              run;

			  %END;


   		   title4 "Non-parametric";
           title6 "MEDIAN %UPCASE(&TimeVarLbl.)"; 
               data _null_;
               set Inference_PerLevel;
               file print
                    ods=(template='InferenceTemplate'
                         columns=(TestVarCol=&TestVar.
                                  NCol=MedTime (generic=on)
                                  NCol=MedLCL (generic=on)
                                  NCol=MedUCL (generic=on)
                                  NCol=MaxTime (generic=on)
								)
						);
               put _ods_;
              run;
            title6;

            title6 "PERCENT %UPCASE(&TimeVarLbl.) AT &Year. YEAR(S) (&conftype Transform)" ;
               data _null_;
               set Inference_PerLevel;
               file print
                    ods=(template='InferenceTemplate'
                         columns=(TestVarCol=&TestVar.
                                  NCol=TimeUsed (generic=on)
                                  NCol=PCTSurvival (generic=on)
                                  NCol=PCTSurvLCL (generic=on)
                                  NCol=PCTSurvUCL (generic=on)
                                  NCol=PCTSurvSTD (generic=on)
								)
						);
               put _ods_;
              run;
            title6;

   		    title4 "Cox model";
            title6 "HAZARD RATIO AND CONFIDENCE LIMITS (PER LEVEL) %UPCASE(&StratTitle.)";

               data _null_;
               set Inference_PerLevel;
               file print
                    ods=(template='InferenceTemplate'
                         columns=(TestVarCol=&TestVar.
                                  NCol=HazardRatio (generic=on)
                                  NCol=HRLowerCL (generic=on)
                                  NCol=HRUpperCL (generic=on)
								)
						);
               put _ods_;
              run;
            title6;


        
            %IF &Test.^=N %THEN %DO;

                     proc print data=Inference_Overall label split='*' noobs   
                     style (data) = data[JUST=C]
                     style (HEADER) = HEADER[JUST=C];
                     title6 "TESTS FOR DIFFERENCE  %UPCASE(&StratTitle.)";
                     var LRChiSq LRDF LRProb WilcChiSq WilcDF WilcProb ScoreChiSq ScoreDF ScoreProb;
  				     run;

            %END;

		title6;

         %END;

         %ELSE %DO;


            data __Print;
            set Inference_PerLevel;
            length HRStr $50;
            if _n_=1 then HRStr=trim(left(put(HazardRatio,8.2)));
            else HRStr = trim(left(put(HazardRatio,8.2)))||
                         ' ('||trim(left(put(HRLowerCL,8.2)))||', '||trim(left(put(HRUpperCL,8.2)))||')';
            label HRStr="Hazard Ratio*(&CLPct_HR. CI)";
            if _n_=1 then do;
               set Inference_Overall ;
            end; 
            else do;
			   LRChiSq=.;LRDF=.;LRProb=.;
			   WilcChiSq=.;WilcDF=.;WilcProb=.;
			   ScoreChiSq=.; 
               ScoreDF=.; 
               ScoreProb=.;
			end;

            length MedTimeStr $50;
            if MedTime=.N then MedTimeStr='Not reached';
            else MedTimeStr = trim(left(put(MedTime,8.2)))||
                            ' ('||trim(left(put(MedLCL,8.2)))||', '||trim(left(put(MedUCL,8.2)))||')';
            label MedTimeStr="Median (&CLPct_Surv. CI)*(&UnitName.)";
            length PCTSurvStr $50;
            PCTSurvStr = trim(left(put(PCTSurvival,8.1)))||
                            ' ('||trim(left(put(PCTSurvLCL,8.1)))||', '||trim(left(put(PCTSurvUCL,8.1)))||')';
            label PCTSurvStr="% at &Year. Year(s)*(&CLPct_Surv. CI)"; 
			sep='|';

			if _n_=1 then do;
            length WilcProbStr LRProbStr ScoreProbStr $50;
            WilcProbStr = trim(left(put(WilcProb,8.3)))||' (df='||trim(left(put(WilcDF,8.0)))||')';
            LRProbStr = trim(left(put(LRProb,8.3)))||' (df='||trim(left(put(LRDF,8.0)))||')';
            ScoreProbStr = trim(left(put(ScoreProb,8.3)))||' (df='||trim(left(put(ScoreDF,8.0)))||')';
			end;

            label WilcProbStr="P-Value*(Wilcoxon)"; 
            label LRProbStr="P-Value*(Log-Rank)"; 
            label ScoreProbStr="P-Value*(Score test)"; 
            label WilcProb='P-Value*(Wilcoxon)';
            label LRProb='P-Value*(Log-Rank)';
            label ScoreProb='P-Value*(Score test)';

			**JRA, 17JAN2014, if there is a format for TESTVAR variable then apply the new format to TESTVAR;
			%if &NOBSFMT >0 %then %do;
				format &TestVar. N&format.;
			%end;

            run;

		   %IF &Default. =Y %then %do;
			data _null_;
			set __LRWilcoxonTests;
			length LRProbStr $50;
			LRProb2=round(LRProb,0.001);
			call symput('PvalLR',compress(put(LRProb2,8.3)));
			run;

			%put PvalLR &PvalLR.;

			data __logrank;
			length HRStr $50. ;

			HRStr="Log-rank test: ";
			ScoreProbStr="p-value=&PvalLR.";
			sep="|";
			run;

			data __Print;
			set __Print __logrank;
			run;

			%End;



            %IF &Output.=ODS %THEN %DO;

               data _null_;
               set __Print;
               file print
                    ods=(template='InferenceTemplate'
                         columns=(TestVarCol=&TestVar.
                                  NCol=NPatients (generic=on)
                                  NCol2=Observed (generic=on)
                                  HRCol=HRStr

                                   %IF &Test.=W %THEN %DO;
										PValColW=WilcProbStr (generic=on)
								   %END;

                                   %IF &Test.=L %THEN %DO;
										PValColL=LRProbStr (generic=on)
								   %END;

                                   %IF &Test.=LW %THEN %DO;
										PValColW=WilcProbStr (generic=on)
										PValColL=LRProbStr (generic=on)
								   %END;

                                  %IF &Test.^=N %THEN %DO;
									  PValColS=ScoreProbStr (generic=on)
 								  %END;


                                  MedianCol=MedTimeStr
                                  PCTSurvCol=PctSurvStr)
                         );
               put _ods_;
               format 
                      NPatients 10. Observed 10. HRStr $20.
                      MedTimeStr $30. PCTSurvStr $20.;
               run;

            %END;

            %ELSE %DO;
            
	 			proc report data=__Print split='*' nowindows  HEADSKIP COMPLETECOLS ;
				column &TestVar. NPatients Observed  sep ('Non-parametric' MedTimeStr PCTSurvStr 

	            %IF &Test.=L %THEN %STR(LRProbStr );
	            %IF &Test.=W %THEN %STR(WilcProbStr );
	            %IF &Test.=LW %THEN %STR(WilcProbStr LRProbStr );


	            ) 
	            sep 
	            ('Cox model' HRStr
		        %IF &Test.^=N %THEN  %STR(ScoreProbStr);
	            );
	            define  &TestVar. / width=17;
	            define  NPatients / width=8;
	            define  Observed / width=8;
	            define  MedTimeStr / width=17;
	            define  PCTSurvStr / width=20;

	            define  sep / width=5 '|*|*|';

	            define  HRStr / width=17 '*Hazard Ratio* (95% CI)';

				%IF &Test.^=N %THEN %DO; 
	            define  ScoreProbStr / width=15;
				%END;

	            %IF &Test.=L %THEN %DO;
	            define  LRProbStr / width=14;
				%END;
	            %IF &Test.=W %THEN %DO;
	            define  WilcProbStr / width=14;
				%END;
	            %IF &Test.=LW %THEN %DO;
	            define  LRProbStr / width=14;
	            define  WilcProbStr / width=17;
				%END;

	           run;


            %END;

         %END;

      %END;

   %END;

   %IF &Output.=Y OR (&Output.=ODS AND &Details=Y) %THEN %STR(title1;);

   %IF &Output.=ODS AND &ODSFile.^=%STR() %THEN %STR(ods rtf close;);


   * Call KMPLOT;
   *************;

   %IF &PlotYN.=Y %THEN %DO;

   * If analysis is stratified or test variable has more than 2 levels, first put required
     P value and DF in the macro variable WALD.  (This will be used by KMPLOT2.);

      data _null_;
      set Inference_Overall;
      length PStr $50 ScoreStr $250 TestDesc $200;

	  %IF &Case.=B or &Case.=C2 %THEN TestDesc='Overall Score test';;
	  %IF &Case.=C1 %THEN TestDesc='Trend Score test';;

      %IF &Stratified.=Y %THEN %DO;
         TestDesc=trim(left(TestDesc))||" stratified for &StratVarLbl.";
      %END;

	  %IF &Case.=B or &Case.=C1 %THEN %DO;
      if round(ScoreProb,0.0001)>0 then PStr = '='||trim(left(put(ScoreProb,6.3)));
      else PStr='<0.0001';
      %END;

	  %IF &Case.=C2 %THEN %DO;
      if round(ScoreProb,0.0001)>0 then PStr = '='||trim(left(put(ScoreProb,6.3)))||' (df='||trim(left(put(ScoreDF,6.3)))||')';
      else PStr='<0.0001'||'(df='||trim(left(put(ScoreDF,6.3)))||')';
      %END;


      ScoreStr=trim(left(TestDesc))||': p'||trim(left(PStr));
      call symput('ScoreLabel',ScoreStr);

      %IF &Stratified.=Y %THEN %DO;

		  %IF &TEST.=L OR &TEST.=LW  %THEN %DO;

	      if round(LRProb,0.0001)>0 then PStr = '='||trim(left(put(LRProb,6.3)));
	      else PStr='<0.0001';
		  call symput('TE3s',trim(left(PStr)));
		  call symput('dflr',trim(left(LRDF)));


		  %END;


		  %IF &TEST.=W OR &TEST.=LW %THEN %DO;

	      if round(WilcProb,0.0001)>0 then PStr = '='||trim(left(put(WilcProb,6.3)));
	      else PStr='<0.0001';
		  call symput('TEWILs',trim(left(PStr)));
		  call symput('dfw',trim(left(WilcDF)));

		  %END;

	  %END;

      run;

      %IF &Stratified.=Y and &Case.=C2 %THEN %DO;
		  %IF &TEST.=L OR &TEST.=LW  %THEN %DO;
          %IF &NLevels>2 %then %let TE3s=&TE3s. (df=&dflr.);;
		  %END;
		  %IF &TEST.=W OR &TEST.=LW %THEN %DO;
          %IF &NLevels>2 %then %let TEWILs=&TEWILs. (df=&dfw.);;
		  %END;

	  %put TE3s &TE3s. TEWILs &TEWILs.;

	  %END;



      %IF &Output.=ODS AND &ODSFile.=%STR() %THEN %STR(ods rtf exclude all;);

         %KMPLOT2(data=&Data.,
                  timevar=&TimeVar.,
                  censvar=&CensVar.,
                  censval=&CensVal.,
                  testvar=&TestVar.,
                  test=&KMPLOT2Test.,
                  where=&Where.,
                  project=&ProjName.,increas=&Increas.);

      %IF &Output.=ODS AND &ODSFile.=%STR() %THEN %STR(ods rtf select all;);

   %END;


   * Clear temporary data sets;
   ***************************;
 
   proc datasets nolist;
   delete  __Input __TestVarValues __StratCheck
           __OutSurv __CensoredSummary __HomTests __HomStats __LogHomCov
           __Observed __Expected __Variance __LRWilcoxonTests
           __ParameterEstimates __GlobalTests __HazardRatio __WaldTest
           __MaxTime __OutSurvEvents __EarliestBelow __LatestNearAndAbove __ReflectedCIs
           __MedianTime __PCTSurvival __zz _contents: __censoredsummaryall __formats __logrank
           __scoretest __trendtests:
           __Print _fmtdesc _quartiles _qwffmt;
   run;
   quit;
  

** Deletion of global macro variables;

%DO i=1 %TO &NSTRAT.; 
 %symdel STRAT&i ;
%END ;

%symdel ScoreLabel  STRATVARLBL NSTRAT;



%EndOfMacro: %MEND;
