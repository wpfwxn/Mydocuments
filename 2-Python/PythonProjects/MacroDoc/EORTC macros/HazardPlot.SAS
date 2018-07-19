/*************************************************************************************

HazardPlot.SAS
**************

Plots Hazard function vs time

Creation date  : June 2003
Version date   : August 2014
Software       : SAS version 9.3
Original author: Laurence Collette / Kate Moncrieff
Modified by    : Jérôme RAPION

************************************************************************************

PARAMETERS

    Data              :  Name of the data file
    TestVar           :  Test variable
    TimeVar           :  Time variable (in days)
    CensVar           :  Censoring variable
    TYPE              :  Type of graph produced 
                         Set to I to get one graph: Hazard Function by Intervals versus time
                         Set to S to get one graph: Epanechnikov Kernel-Smoothed Hazard Functions versus time 
                         Set to B to get two graphs: Hazard Function by Intervals versus time and
                         Epanechnikov Kernel-Smoothed Hazard Functions versus time [default=I]
    OutFile           :  Path and name of the file to contain the graphical output without
                         its extension
    Filefmt           :  Extension File Format for output:
                         TIFF = TIF file, PNG = PNG file, PS = PS file, EMF = EMF file , GIF= GIF file,
                         PDF = PDF file, SVG = SVG file [default=PNG]
    CensVal           :  Value of CensVar for a censored observation [default=l]
    TimeUnit          :  Unit of time to be used for time intervals and to appear on plot:
                         D=Days, W=Weeks, M=Month, Y=Years.  See notes below. [default=Y]
    TimePoints        :  Required intervals for time variable.  See notes below.  [default=1]
    Alpha             :  Alpha for CI around hazard or survival estimates
                         (in range 0.0001-0.999, default=0.05)
    SmoothLevel       :  If the user choosed Type=S, 
                         the smoothed hazard will be shown inside a specific window between the time 
                         with (SmoothLevel)% of events and the time with 100-(SmoothLevel)% of events
                         [default=5]
    CIdisplay         :  Confidence Interval display
                         Set to I to display CI for the plot 'Hazard Function by Intervals versus time'
                         Set to S to display CI for the plot 'Epanechnikov Kernel-Smoothed Hazard Functions versus time' 
                         Set to B to display CI for the two plots
                         Set to N to remove CI everywhere.
						 Defaults: if TYPE=S then CIDISPLAY=N, if TYPE=I or TYPE=B then CIDISPLAY=I 
    RiskSets          :  Risk sets display below the graph 
                         Set to Y to display risk sets for the plot 'Hazard Function by Intervals versus time'
                         Set to N to remove the risk sets.[default=Y]
                         There is no risk sets display for the plot Epanechnikov Kernel-Smoothed Hazard Functions versus time
    Where             :  Selection condition
    Colour            :  Set to Y to obtain graph in colour, N for black and white [default=Y]
    GRID              :  Set to Y to obtain grid on the background of the graph [default=N]
    BANDWIDTH         :  specifies what bandwidth is chosen for the kernel-smoothing and how it is chosen.
                         You can specify one of the following bandwidth-options:
                         -> value: sets the bandwidth to the given value. 
                         -> numeric-list: selects the bandwidth from the given numeric-list that minimizes 
                            the mean integrated squared error
                         -> RANGE(lower,upper): selects the bandwidth from the interval (lower, upper)
                            that minimizes the mean integrated squared error
                         Default: BANDWIDTH= RANGE(0.2b,20b), 
                         where b=(maximum event time - time origin)/(time when total number of noncensored observations occured)²

NOTES

    TimePoints= determines the time points at which the response variable (e.g. hazard function)
    should be calculated.

    TimeUnit= specifies the units in which TimePoints= is expressed.
    For example: "TimeUnit=Y,TimePoints=1 2 3 5" indicates that the response variable
    should be calculated for the intervals 0-1 year, 1-2 years, 2-3 years and 3-5 years.

    TimeUnit= also determines the time units appearing on the plot.

    TimePoints= can be set to either a list of values or to a single value.  When set to
    a single value, the response variable is calculated at regular intervals.
    For example: "TimeUnit=M,TimePoints=3" indicates that the response variable should be 
    calculated at 3-monthly intervals.

    If TimePoints= is set to a list of values, that list is passed to the INTERVALS=
    option of the LIFETEST procedure.  Therefore, the following are all possible:
    "TimePoints=1 3 5 7", "TimePoints=%STR(1,3,5,7)", "TimePoints=1 to 6 by 2",
    "TimePoints=%STR(1,3 to 5,7)" etc.. (See SAS doc.)
    (If the list contains commas, use the %STR() function.)

    (If TimePoints= is set to a single value, that value is passed to the WIDTH=
    option of the LIFETEST procedure.)


EXAMPLES
 
    %HazardPlot(Data=Patient,TestVar=TrtC,TimeVar=TimePFS,CensVar=PFS, Filefmt=PNG,
                OutFile=C:\Temp\30895c,Censval=0,TimeUnit=M,TimePoints=%STR(6,12,24,36),
                Alpha=0.01,Colour=N);

    %HazardPlot(Data=Patient,TestVar=TrtC,TimeVar=TimePFS,CensVar=PFS, Filefmt=PNG,
                OutFile=C:\Temp\30895d,TYPE=I);

    %HazardPlot(Data=Patient,TestVar=TrtC,TimeVar=TimePFS,CensVar=PFS, Filefmt=TIF,
                OutFile=C:\Temp\30895e,TYPE=B);

    %HazardPlot(Data=Patient,TestVar=TrtC,TimeVar=TimePFS,CensVar=PFS, 
                OutFile=C:\Temp\30895g,TYPE=S, SmoothLevel=1);

************************************************************************************

REVISIONS

27Jun2003,KM  Added Colour parameter.
              N patients and N events now printed below graph (when RiskSets=Y).
18Sep2003,KM  Minor changes to documentation.
30Sep2003,KM  Changed approx. of N days in a month/year from 30/365 to 30.4375/365.25.
04Sep2006,JRA Add of Filefmt Parameter: multiple choice of file format (CGM, TIF or PNG file)
              Add of RESDEV Plot in Plot parameter (Plot of Deviance residuals) 
              Add of WRESSCHO Plot in Plot parameter (Plot of Weighted Schoenfeld residuals) 
              Add of ADJUST Parameter to add adjusting covariates in a COx Model.
              Add of ODS Parameter to create an ODS rtf file.
              Add of SODS Parameter to allow user to put several graph in the same ODS rtf file.
              Add of SMOOTH Parameter to indicate the precision of the LOWESS smooth line 
              to fit the data.
06Oct2006,JRA Handling of round of time points in Annotate data set.
              Remove of the label "X time +" 
              Remove of the level "missing" in plots if one level of TESTVAR is missing 
26Oct2010,JRA Beacause of SAS 9.2 versio, change of PNG device.
25Apr2013,JRA Deep changes on the macro. 
              Remove of ADJUST, PLOT, HazardTimeUnit, ODS, SODS and Smooth parameters.
              Add of TYPE, SmoothLevel, CIDisplay, GRID and BANDWIDTH parameters.
              The macro will only plot Hazard vs time from now.
              It could be done by intervals (TYPE=I), Smoothed Hazard Functions versus time (TYPE=S),
              or both (TYPE=B)
23Jan2014,JRA Very light change in the way of computing RISKSET tables
              In case of several lines for the same time, take the last line (with more events).
22Aug2014,JRA Fix the bug for RISKSETS display in the case the intervals between each timepoint are not constant.

************************************************************************************/

%MACRO HazardPlot(Data,TestVar,TimeVar,CensVar,OutFile,Filefmt,BANDWIDTH,
                  CensVal=1,TimeUnit=Y,TimePoints=1,
                  Alpha=0.05,RiskSets=Y,Where=%STR(),Colour=Y,GRID=N,CIdisplay=,TYPE=I,SmoothLevel=5);


   * Standardise parameter settings;
   ********************************;
   %LET TYPE = %UPCASE(&TYPE.);
   %LET CIdisplay = %UPCASE(&CIdisplay.);
   %LET RiskSets = %UPCASE(&RiskSets.);

   %IF (%LENGTH(&CIdisplay.))=0 %THEN %DO;
	   %IF &TYPE=B OR &TYPE=I %THEN %LET CIdisplay=I;
	   %IF &TYPE=S %THEN %LET CIdisplay=N;
   %END;

   %IF (%LENGTH(&BANDWIDTH.))=0 %THEN %LET BANDWITHFLAG=N;
   %IF (%LENGTH(&BANDWIDTH.))>0 %THEN %LET BANDWITHFLAG=Y;

   %LET TimeUnit = %UPCASE(%SUBSTR(&TimeUnit.,1,1));
   %IF &TimeUnit. NE D AND &TimeUnit. NE W AND &TimeUnit. NE M %THEN %LET TimeUnit = Y;

   %IF &TimeUnit. = D %then %let TimeUnitLabel=(in days);
   %IF &TimeUnit. = W %then %let TimeUnitLabel=(in weeks);
   %IF &TimeUnit. = M %then %let TimeUnitLabel=(in months);
   %IF &TimeUnit. = Y %then %let TimeUnitLabel=(in years);

   %IF &TimeUnit. = D %then %let TimeUnitLong=DAY;
   %IF &TimeUnit. = W %then %let TimeUnitLong=WEEK;
   %IF &TimeUnit. = M %then %let TimeUnitLong=MONTH;
   %IF &TimeUnit. = Y %then %let TimeUnitLong=YEAR;

   %LET RiskSets = %UPCASE(%SUBSTR(&RiskSets.,1,1));

   %IF &TYPE=S %THEN %LET RiskSets=N;

   %LET Colour=%UPCASE(%SUBSTR(&Colour.,1,1));
   %IF (%LENGTH(&FILEFMT.))=0 %THEN %LET FILEFMT=PNG;
   %IF &FILEFMT.=CGM %THEN %LET DEVICE=CGM;
   %IF &FILEFMT.=TIF %THEN %LET DEVICE=PNG;
   %IF &FILEFMT.=PNG %THEN %LET DEVICE=PNG;

   %MACRO RCHV(SUITE,COMPTE,SEP,NV);
		%LOCAL NUMERO MOT;
		%LET NUMERO=1;
		%LET MOT=%NRBQUOTE(%SCAN(&SUITE,&NUMERO,&SEP ));
		%DO %WHILE(&MOT^=);
		%GLOBAL &COMPTE&NUMERO;
		%LET &COMPTE&NUMERO=%UPCASE(&MOT);
		%LET NUMERO=%EVAL(&NUMERO+1);
		%LET MOT=%NRBQUOTE(%SCAN(&SUITE,&NUMERO,&SEP ));
		%END;
		%GLOBAL &NV;
		%LET &NV=%EVAL(&NUMERO-1);
	%MEND RCHV;

	%local nn;

	%LET Title=Hazard vs Time;

   * Restrict data to required records;
   ***********************************;

   data __Data;
   set &Data.;
   %IF (%LENGTH(&Where.)>0) %THEN %STR(where &Where.;);
   run;

   * Determine whether time intervals are provided as a list of time points or as a single value;
   *********************************************************************************************;

   %LOCAL SingleTimePoint;
   %IF %SCAN(&TimePoints.,2,', ')=%STR() %THEN %LET SingleTimePoint=Y;
   %ELSE %LET SingleTimePoint=N;

   * Derive macro vars to convert days to time unit and to describe time unit (used later);
   ***************************************************************************************;

   %LOCAL TimeFactor UnitName;
   %IF &TimeUnit.=D %THEN %DO;
      %LET TimeFactor = 1;
      %LET UnitName=Day;
   %END;
   %ELSE %IF &TimeUnit.=W %THEN %DO;
      %LET TimeFactor = 7;
      %LET UnitName=Week;
   %END;
   %ELSE %IF &TimeUnit.=M %THEN %DO;
      %LET TimeFactor = 30.4375;
      %LET UnitName=Month;
   %END;
   %ELSE %IF &TimeUnit.=Y %THEN %DO;
      %LET TimeFactor = 365.25;
      %LET UnitName=Year;
   %END;

   data __Data;
   set __Data;
   _time=&TimeVar./&TimeFactor.;
   run;


   * Derive macro vars for name and width of test variable format (used later);
   ***************************************************************************;

   data _null_;
   set __Data (obs=1);
   length TestVarFmt $15;
   TestVarFmt=trim(left(vformatn(&TestVar.)));
   if TestVarFmt='$' then TestVarFmt='$'||trim(left(put(vformatw(&TestVar.),3.)));
   call symput('TestVarFmt',trim(left(TestVarFmt)));
   TestVarFmtW=max(8,vformatw(&TestVar.));
   call symput('TestVarFmtW',trim(left(put(TestVarFmtW,2.))));
   run;

   * Number each level of test variable (used later);
   *************************************************;

   proc sort data=__Data out=__TestLevels (keep=&TestVar. where=(&TestVar. ne .)) nodupkey;
   by &TestVar.;
   run;

   data __TestLevels;
   set __TestLevels end=LastObs;
   LevelNo=_n_;
   _Stratum=put(&TestVar.,&TestVarFmt..);
   if LastObs then call symput('NLevels',trim(left(put(_n_,3.))));
   run;

   %put NLevels &NLevels.;


   * Add one additional timepoint at the end of the list of Timepoints;
   ****************************************************************;

   %IF &SingleTimePoint.=N %then %do;

	  %if %INDEX(&TimePoints,%str(,))>0 %THEN %DO;
		%RCHV(SUITE=&TimePoints,COMPTE=time,SEP=%STR(,),NV=nbtime);
	  %end;

	  %if %INDEX(&TimePoints,%str( ))>0 %THEN %DO;
		%RCHV(SUITE=&TimePoints,COMPTE=time,SEP=%str( ),NV=nbtime);
	  %end;

	   %let nbtime1=%eval(&nbtime.-1);
	   %let interval=%sysevalf(&&time&nbtime. -  &&time&nbtime1.);

	   %let LastTimePoints=&&time&nbtime.;
       %put &LastTimePoints. ;

	   %let LastTimePointsAdd=%sysevalf(&&time&nbtime. + &interval.);
	   %put &LastTimePointsAdd.  ;
	   

	  %if %INDEX(&TimePoints,%str(,))>0 %THEN %DO;
	    %let TimePoints=%STR(&TimePoints.,&LastTimePointsAdd.);
	  %end;

	  %if %INDEX(&TimePoints,%str( ))>0 %THEN %DO;
	    %let TimePoints=%STR(&TimePoints. &LastTimePointsAdd.);
	  %end;

	   %put TimePoints &TimePoints.;


   %end;


   * Convert list of required intervals into days;
   * (Necessary for LIFETEST procedure since input data is in days);
   ****************************************************************;

   %LOCAL TimePointsConverted;
   %LOCAL Remaining;
   %LET Remaining=%BQUOTE(&TimePoints.);
   %LOCAL HangingTo;
   %LET HangingTo=N;
   %DO %WHILE(%BQUOTE(&Remaining.) ne %STR());
      %LOCAL ReadWord WriteWord;
      %LET ReadWord=%SCAN(%BQUOTE(&Remaining.),1,', ');
      %IF NOT %VERIFY(&ReadWord.,1234567890.) %THEN %LET WriteWord = %SYSEVALF(&ReadWord.*&TimeFactor.);
      %ELSE %LET WriteWord = &ReadWord.;
      %IF %UPCASE(&ReadWord.)=TO %THEN %LET HangingTo=Y;
      %ELSE %IF %UPCASE(&ReadWord.)=BY %THEN %LET HangingTo=N;
      %LET TimePointsConverted = %QTRIM(%BQUOTE(&TimePointsConverted.))%STR( )%TRIM(&WriteWord.);
      %IF %SCAN(%BQUOTE(&Remaining.),2,', ')^=%STR() %THEN %DO;
         %LET Remaining = %SUBSTR(%BQUOTE(&Remaining.),%EVAL(%INDEX(%BQUOTE(&Remaining.),&ReadWord.)+%LENGTH(&ReadWord.)));
         %IF %SUBSTR(%BQUOTE(&Remaining.),1,1)=%STR(,) %THEN %DO;
            %IF &HangingTo.=Y %THEN %DO;
               %LET TimePointsConverted = %QTRIM(%BQUOTE(&TimePointsConverted.))%STR( by &TimeFactor.);
               %LET HangingTo=N;
            %END;
            %LET TimePointsConverted = %QTRIM(%BQUOTE(&TimePointsConverted.))%STR(,);
            %LET Remaining = %SUBSTR(%BQUOTE(&Remaining.),2);
         %END;
      %END;
      %ELSE %DO;
         %IF &HangingTo.=Y %THEN %LET TimePointsConverted = %QTRIM(%BQUOTE(&TimePointsConverted.))%STR( by &TimeFactor.);
         %LET Remaining=%STR();
      %END;
   %END;


   %put TimePointsConverted &TimePointsConverted;
   
   * Get points to be plotted from LIFETEST procedure;
   **************************************************;

   * First prepare the PROC LIFETEST option that will be used to define the intervals.  
     Which option to use depends on whether the TimePoints parameter has been set to a single value (WIDTH=x)
     or a list of values (TimePoints=...);
     
   ods listing close;
   %LOCAL IntervalOption;
   %IF &SingleTimePoint.=Y %THEN %LET IntervalOption=%STR(width=&TimePointsConverted.);
   %ELSE %LET IntervalOption=%STR(intervals=&TimePointsConverted.);

    proc lifetest data=__Data notables noprint 
                 outsurv=__OutSurv 
				 method=act &IntervalOption. alpha=&Alpha.;
	   time &TimeVar.*&CensVar.(&CensVal.);
	   strata &TestVar.;
	   run;

   ****************************************************;

   data __Plot;
   set __OutSurv;
   if not missing(&TimeVar.) then TimePoint=&TimeVar./&TimeFactor.;
   if not missing(MidPoint) then MidPoint=MidPoint/&TimeFactor.;
   label MidPoint="&UnitName.s";
   run;

   
   * Determine distinct time points;
   ********************************;

   data __Plot2;
   set __Plot;
   if MidPoint ne .;
   run;

   proc sort data=__Plot2 out=__TimePoints  (keep=TimePoint MidPoint) nodupkey;
   by TimePoint;
   run;


 data __TimePoints;
   set __TimePoints ; 
   by TimePoint;
   if _n_>1 then TimeInterval=TimePoint-LastTimePoint;
   retain LastTimePoint;
   LastTimePoint=TimePoint;
   TimeY=TimePoint/TimeInterval;
   keep TimePoint TimeInterval MidPoint TimeY;
   run;


   * Determine latest time point to appear on plot;
   ***********************************************;


   data __TimePoints;
   set __TimePoints end=LastObs;
   by TimePoint;
   if LastObs then do;
      LatestTimePoint=max(TimePoint,MidPoint);
      call symput('LatestTimePoint',trim(left(put(LatestTimePoint,best8.))));
   end;
   run;


   * Determine shortest time interval;
   **********************************;

   proc univariate data=__TimePoints noprint;
   var TimeInterval;
   output out=__TimeInterval min=TimeInterval;
   run;

   data _null_;
   set __TimeInterval;
   call symput('TimeInterval',trim(left(put(TimeInterval,best8.))));
   run;

   
   * Derive response values; 
   ************************;
   
   data __Plot;
   set __Plot;
      where Hazard is not missing;
     * (If Hazard plot required, calculate Hazard (+CI) for required time unit);
      Response=Hazard*&TimeFactor.;
      if Hazard>0 then do;
         LCL=Haz_LCL*&TimeFactor.;
         UCL=Haz_UCL*&TimeFactor.;
      end;
      else do;
         if missing(Haz_LCL) then LCL=0;
         if missing(Haz_UCL) then UCL=0;
      end;

	* Remove data when LCL and UCL could not have been computed;
	if Response=LCL and Response=UCL then delete;

	IntervalStartTime =MIDPOINT-0.5;

   keep &TestVar. MidPoint Response LCL UCL IntervalStartTime TimePoint;
   run;


   data __Plotmax;
	   set __Plot;
	   CL=Response;
	   output;
	   Response=.;
	   CL=LCL;
	   output;
	   CL=UCL;
	   output;
	   keep &TestVar. MidPoint Response CL;
    run;



   * Slightly shift x-axis values of each test group to separate them;
   ******************************************************************;
   proc sort data=__Plot;
   by &TestVar.;
   run;

data __Plot;
   merge __Plot (in=inP) __TestLevels;
   by &TestVar.;
   if inP;
      if not missing(MidPoint) then 
         MidPoint=MidPoint+((LevelNo-ceil(&NLevels./2))*(&LatestTimePoint./300));
   run;

   * Compute the maximum time until where the plot should go;

   proc means data=__Plot max noprint;
   var IntervalStartTime;
   output out=__max;
   run;

   data __max;
	   set __max;
	   TimePoint=IntervalStartTime;
	   if _STAT_="MAX";
	   keep TimePoint;
   run;

   data __max;
   set __max;
   TimePoint=ROUND(TimePoint,1);
   run;

	data _null_;
	  set __max;
	  call SYMPUT('maxTimePt',compress(TimePoint));
	  run;

	  %let maxTimePoint=%eval(&maxTimePt.+1);
	  %put maxTimePoint &maxTimePoint.;


	** Retrieving format and label of TESTVAR *;
	 proc contents data=&data out=__contents noprint;
	 run;

	 data __contents;
	 set __contents;
	 NAME=UPCASE(NAME);
	 if NAME="%UPCASE(&TestVar.)";
	 if format="" then format="BEST12";
	 run;

	 data _null_;
	 set __contents;
	 call symput('testvarf',compress(format));
	 run;

	 proc format cntlout=__formats(where=(FMTNAME="%upcase(&testvarf.)"));
	 run;

	 data _null_;
	 set __formats;
	 call symput('formatlength',compress(length));
	 run;

	 %if &testvarf=BEST12 %then %let formatlength=9;
	 %put formatlength &formatlength.;
	 

   * Creation of namefile and namepath macro variable from &Outfile;
   **********************;

  data __countw;
  allpath="&OutFile";
  n1=COUNTW(allpath,"/");
  n2=COUNTW(allpath,"\");
  n=max(n1,n2);
  run;

  data _null_;
  set __countw;
  call symput ('nbpath',compress(n));
  run;

  %let aa1=%scan(&OutFile,&nbpath.,'\');
  %let aa2=%scan(&OutFile,&nbpath.,'/');

  %if %length(&aa1.)=0 %then %let namegraph=&aa2;;
  %if %length(&aa2.)=0 %then %let namegraph=&aa1;;

  %put &namegraph.;

  data __countw;
  set __countw;
  file="&namegraph.";
  path=TRANWRD(allpath,file,"");
  run;

  data _null_;
  set __countw;
  call symput ('namefile',trim(left(file)));
  call symput ('namepath',trim(left(path)));
  run;
	
  %put namefile &namefile.;
  %put namepath &namepath.;



* Set graphics templates;
**********************;

%IF &COLOUR=Y %THEN %DO;

	proc template; 
		   define style MyStyleDefault;
		   parent=Styles.Journal;   

		  class GraphColors /              
		         'gcdata10' = cxB38EF3 
		         'gcdata9' = cx111111  
		         'gcdata8' = cx01665E                                       
		         'gcdata7' = cx9D3CDB                                             
				 'gcdata6'=  cxAAAAAA  
		         'gcdata5' = cx543005                                        
		         'gcdata4' = cx47A82A                                           
		         'gcdata3' = cx2597FA                                              
		         'gcdata2' = cx2A25D9                                           
		         'gcdata1' = cxB2182B   
		         'ggrid' = cxECECEC 
	             ;

		     * gcdata10 -> cxB38EF3 violet; 
		     * gcdata9 -> cx111111 black; 
		     * gcdata8 -> cx01665E dark green; 
		     * gcdata7 -> cx9D3CDB pink; 
		     * gcdata6 -> cxAAAAAA grey; 
		     * gcdata5 -> cx543005 brown; 
		     * gcdata4 -> cx47A82A light green; 
	         * gcdata3 -> cx2597FA light blue; 
		     * gcdata2 -> cx2A25D9 blue; 
		     * gcdata1 -> cxB2182B red; 
		     * ggrid -> cxECECEC grey; 


		    class GraphGridLines /                                                  
		         displayopts = "auto"                                                 
		         linethickness = 1px                                                  
		         linestyle = 1                                                        
		         contrastcolor = GraphColors('ggrid')                                 
		         color = GraphColors('ggrid');              

		 style GraphData1 from GraphData1 /   
		         linestyle = 1      contrastcolor = GraphColors('gcdata1')                               
		         color = GraphColors('gcdata1');        
		 style GraphData2 from GraphData2 /   
		         linestyle = 1      contrastcolor = GraphColors('gcdata2')                               
		         color = GraphColors('gcdata2');        
		 style GraphData3 from GraphData3 /   
		         linestyle = 1      contrastcolor = GraphColors('gcdata3')                               
		         color = GraphColors('gcdata3');        
		 style GraphData4 from GraphData4 /   
		         linestyle = 1      contrastcolor = GraphColors('gcdata4')                               
		         color = GraphColors('gcdata4');        
		 style GraphData5 from GraphData5 /   
		         linestyle = 1      contrastcolor = GraphColors('gcdata5')                               
		         color = GraphColors('gcdata5');        
		 style GraphData6 from GraphData6 /   
		         linestyle = 1      contrastcolor = GraphColors('gcdata6')                               
		         color = GraphColors('gcdata6');   
		 style GraphData7 from GraphData7 /   
		         linestyle = 1      contrastcolor = GraphColors('gcdata7')                               
		         color = GraphColors('gcdata7');   
		 style GraphData8 from GraphData8 /   
		         linestyle = 1      contrastcolor = GraphColors('gcdata8')                               
		         color = GraphColors('gcdata8');
		 style GraphData9 from GraphData9 /   
		         linestyle = 1      contrastcolor = GraphColors('gcdata9')                               
		         color = GraphColors('gcdata9');
		 style GraphData10 from GraphData10 /   
		         linestyle = 1      contrastcolor = GraphColors('gcdata10')                               
		         color = GraphColors('gcdata10');
		end;
		run;;

%END;

%IF &COLOUR=N %THEN %DO;

	proc template; 
		   define style MyStyleDefault;
		   parent=Styles.Journal;   

		  class GraphColors /              
		         'gcdata10' = black 
		         'gcdata9' = black  
		         'gcdata8' = black                                       
		         'gcdata7' = black                                             
				 'gcdata6'=  black  
		         'gcdata5' = black                                        
		         'gcdata4' = black                                           
		         'gcdata3' = black                                              
		         'gcdata2' = black                                           
		         'gcdata1' = black   
		         'ggrid' = cxECECEC 
	             ;
		    class GraphGridLines /                                                  
		         displayopts = "auto"                                                 
		         linethickness = 1px                                                  
		         linestyle = 1                                                        
		         contrastcolor = GraphColors('ggrid')                                 
		         color = GraphColors('ggrid');              

		 style GraphData1 from GraphData1 /   
		         linestyle = 1      contrastcolor = GraphColors('gcdata1')                               
		         color = GraphColors('gcdata1');        
		 style GraphData2 from GraphData2 /   
		         linestyle = 2      contrastcolor = GraphColors('gcdata2')                               
		         color = GraphColors('gcdata2');        
		 style GraphData3 from GraphData3 /   
		         linestyle = 3      contrastcolor = GraphColors('gcdata3')                               
		         color = GraphColors('gcdata3');        
		 style GraphData4 from GraphData4 /   
		         linestyle = 4      contrastcolor = GraphColors('gcdata4')                               
		         color = GraphColors('gcdata4');        
		 style GraphData5 from GraphData5 /   
		         linestyle = 5      contrastcolor = GraphColors('gcdata5')                               
		         color = GraphColors('gcdata5');        
		 style GraphData6 from GraphData6 /   
		         linestyle = 6      contrastcolor = GraphColors('gcdata6')                               
		         color = GraphColors('gcdata6');   
		 style GraphData7 from GraphData7 /   
		         linestyle = 7      contrastcolor = GraphColors('gcdata7')                               
		         color = GraphColors('gcdata7');   
		 style GraphData8 from GraphData8 /   
		         linestyle = 8      contrastcolor = GraphColors('gcdata8')                               
		         color = GraphColors('gcdata8');
		 style GraphData9 from GraphData9 /   
		         linestyle = 9      contrastcolor = GraphColors('gcdata9')                               
		         color = GraphColors('gcdata9');
		 style GraphData10 from GraphData10 /   
		         linestyle = 10      contrastcolor = GraphColors('gcdata10')                               
		         color = GraphColors('gcdata10');
		end;
		run;;

%END;

   * Define horizontal axis (for time variable);
   **********************************************;
   data __TickMarks;
   TimePoint=0;
   do until (TimePoint>=&LatestTimePoint.);
      output;
      TimePoint+&TimeInterval.;
   end;
   output;
   keep TimePoint;
   run;

   data __windowPlot;
   set __Plot;
   if LevelNo=1;
   TimePoint=IntervalStartTime;
   keep TimePoint;
   run;

   proc sort data=__windowPlot nodupkey;
   by TimePoint;
   run;

   data __TimePointLabels;
   set __tickmarks ;
   %if &SingleTimePoint.=N %THEN if TimePoint <= &maxTimePoint. and TimePoint <= &LastTimePoints.;;
   %if &SingleTimePoint.=Y %THEN if TimePoint <= &maxTimePoint. ;;
   run;

   
   data _null_;
   set __TimePointLabels;
   call symput('maxTimePointScale',compress(TimePoint));
   run;

   %put maxTimePointScale &maxTimePointScale.;

   data __TimePointLabels;
   set __TimePointLabels end=LastObs;
   by TimePoint;
   length TimePointLabel $30;

   int=INT(TimePoint);

   IF INT=TimePoint THEN TimePointLabel='"'||trim(left(put(TimePoint,3.)));
   ELSE TimePointLabel='"'||trim(left(put(TimePoint,3.1)));
   TimePointLabel=trim(left(TimePointLabel))||'"';
   run;

   data __TimePointLabels2;
   merge __TimePointLabels(in=a) __TickMarks end=LastObs;
   by TimePoint;
   if a;
   run;

 data _null_  ;
   set __TimePointLabels2 end=LastObs;
   by TimePoint;
   length OrderList ValueList $200;
   retain OrderList ValueList;
   if _n_=1 then do;
      OrderList=trim(left(put(TimePoint,best8.)));
      ValueList='"'||trim(left(put(TimePoint,best8.)))||'"';;
   end;
   else do;
      OrderList=trim(left(OrderList))||' '||trim(left(put(TimePoint,best8.)));
      ValueList=trim(left(ValueList))||' '||'"'||trim(left(TimePoint))||'"';;
   end;

   if LastObs then do;
      call symput('OrderList',trim(left(OrderList)));
      call symput('ValueList',trim(left(ValueList)));
   end;
   run;

*** maxTimeY;

    %sort(data=__timepoints,var=TimePoint);
    %sort(data=__timepointlabels,var=TimePoint);
	data __timepoints2;
	merge __timepoints __timepointlabels(in=a);
	by TimePoint;
	if a;
	one=1;
	run;

	%sort(data=__timepoints2,var=one TimeY);

	data __timepoints2;
	set __timepoints2;
	by one TimeY;
	if last.one;
	run;

 data _null_  ;
   set __timepoints2 ;
	      call symput('maxTimeY',trim(left(TimeY)));
 
  run;

  %put maxTimeY &maxTimeY. TimeInterval &TimeInterval.;


   * Define vertical axes (for response variable);
   **********************************************;

   * First prepare macro variables to be used in axis definitions for scaling.
     (Necessary because final graph consists of two plots which are superimposed and
      must therefore be forced to have the same scaling.);


		   proc univariate data=__Plotmax noprint;
			   var CL;
			   output out=__YMinMax min=YMin max=YMax;
		   run;

		   data _null_;
		   set __YMinMax;
		   YIncrement=10**(round(log10((YMax-YMin)/10),1));
		   YFrom=round(YMin,YIncrement)-YIncrement;
		   YTo=round(YMax,YIncrement)+YIncrement;
		   NPoints=((YTo-YFrom)/YIncrement) + 1;
		   if NPoints > 20 then do;
		      if YFrom/(2*YIncrement)^=floor(YFrom/(2*YIncrement)) then YFrom=YFrom-YIncrement;
		      if YTo/(2*YIncrement)^=floor(YTo/(2*YIncrement)) then YTo=YTo+YIncrement;
		      YIncrement=YIncrement*2;
		   end;
		   %STR(if .Z<YFrom<0 then YFrom=0;);
		   call symput('YIncrement',trim(left(put(YIncrement,best8.))));
		   call symput('YFrom',trim(left(put(YFrom,best8.))));
		   call symput('YTo',trim(left(put(YTo,best8.))));
		   run;


      * Prepare place for RISKSETS;

		%if &Nlevels =1 or &Nlevels =2 %then %let Percentpad=10;
		%if &Nlevels =3 or &Nlevels =4 %then %let Percentpad=15;
		%if &Nlevels =5 or &Nlevels =6 %then %let Percentpad=22;
		%if &Nlevels >6 %then %let Percentpad=27;

		%put formatlength &formatlength.;
		%if &formatlength <=10 %then %let RightPercentpad=20;
		%if &formatlength <=20 and &formatlength>10 %then  %let RightPercentpad=30;



 ************************;
 ************************;
* Prepare risk sets data;
 ************************;
 ************************;

 * Get data set with N patients remaining at each event time;

      ods output productlimitestimates=__ProductLimitEstimates;
      ods output censoredsummary=__CensoredSummary;

	  title;
      proc lifetest data=__Data method=KM ;
      time &TimeVar.*&CensVar.(&CensVal.);
      strata &TestVar.;
      run;
      title;
      ods output close;



      * Get data set with N patients remaining at each event time overall;

      ods output productlimitestimates=__ProductLimitEstimatesALL;
      ods output censoredsummary=__CensoredSummaryALL;

	  title;
      proc lifetest data=__Data method=KM ;
      time &TimeVar.*&CensVar.(&CensVal.);
      run;
      title;
      ods output close;

     * Select record with maximum N remaining for each distinct time point;

	  ** JRA, 23JAN2014: replacement of descending Left by Left;

      proc sort data=__ProductLimitEstimates;
      by &TestVar. &TimeVar. Left;
      run;

      data __ProductLimitEstimates2;
      set __ProductLimitEstimates;
      by &TestVar. &TimeVar. Left;
      if first.&TimeVar.;
      keep &TestVar. &TimeVar. Left Failed stratum;
      run;


      proc sort data=__ProductLimitEstimates2;
      by stratum &TimeVar. ;
      run;

      data __ProductLimitEstimates2;
      set __ProductLimitEstimates2;
      by stratum &TimeVar. ;
      if first.stratum then delete;
      keep &TestVar. &TimeVar. Left Failed ;
      run;


      proc sort data=__ProductLimitEstimates;
      by stratum &TimeVar. descending Left;
      run;

      data __ProductLimitEstimates2b;
      set __ProductLimitEstimates;
      by stratum &TimeVar. descending Left;
      if first.stratum;
      keep &TestVar. &TimeVar. Left Failed;
      run;

	  data __ProductLimitEstimates3;
	  set __ProductLimitEstimates2b __ProductLimitEstimates2 ;
	  run;

      * Match start of each time interval with all time points on or before it;

      proc sql;
      create table __RiskSets as
      select o.&TestVar., o.&TimeVar. as IntervalStartTime, o.MidPoint, p.&TimeVar. as EventTime, p.Left as N,
	  p.Failed as O
      from __OutSurv o, __ProductLimitEstimates3 p
      where o.&TestVar.=p.&TestVar.
        and o.&TimeVar.>=p.&TimeVar.;
      quit;

      * Pick record nearest to start of each time interval;

      proc sort data=__RiskSets;
      by &TestVar. IntervalStartTime EventTime;
      run;   

      data __RiskSets;
      set __RiskSets;
      by &TestVar. IntervalStartTime EventTime;
      if last.IntervalStartTime;
      if not missing(IntervalStartTime) then IntervalStartTime=IntervalStartTime/&TimeFactor.;
      EventTime=EventTime/&TimeFactor.;
      keep &TestVar. IntervalStartTime EventTime N O;
      run;

      proc sort data=__RiskSets out=__MaxRisk;
      by IntervalStartTime;
      run;

	  data _null_;
	  set __MaxRisk;
	  call symput('maxtime',compress(IntervalStartTime));
	  run;

	  data _null_;
	  set __testlevels end=final;
	  m+1;
	  call SYMPUT('valtrt'||compress(m),compress(&TestVar.));
	  if final then call symput('maxLevelNo',compress(LevelNo));
	  run;

      data __alltimepoints;
      set __timepoints;
	  if TimePoint <=&maxtime.;
	  IntervalStartTime=TimePoint;
	  keep IntervalStartTime;
	  run;

	  %put maxtime &maxtime. valtrt1 &valtrt1. valtrt2 &valtrt2.;


	  data __theoricaltimepoints;
	  	set __timepoints;
		IntervalStartTime=TimePoint;
		if MIDPOINT ne .;
		keep IntervalStartTime;
	  run;

	  %sort(data=__CensoredSummary,var=&TestVar.);
	  %sort(data=__testlevels,var=&TestVar.);

	  data __CensoredSummary2;
	  merge __CensoredSummary __testlevels(in=a);
	  by &TestVar.;
	  if a;
	  run;

	  data __CensoredSummaryALL;
	  set __CensoredSummaryALL;
	  Failed5p=round(Failed*(%sysevalf(&SmoothLevel/100)),1);
	  Failed95p=round(Failed*(%sysevalf((100-&SmoothLevel)/100)),1);
      run;


      proc sql;
      create table __limitslow as
      select o.Failed5p, p.&TimeVar. as EventTime
      from __CensoredSummaryALL o, __productlimitestimatesall p
      where o.Failed5p>=p.Failed;
      quit;

      proc sql;
      create table __limitsup as
      select o.Failed95p, p.&TimeVar. as EventTime
      from __CensoredSummaryALL o, __productlimitestimatesall p
      where o.Failed95p<=p.Failed;
      quit;

	  data __limitslow;
	  set __limitslow;
	  __time=EventTime/&TimeFactor.;
	  run;

	  data __limitsup;
	  set __limitsup;
	  __time=EventTime/&TimeFactor.;
	  run;

	  %sort(data=__limitslow,var= Failed5p EventTime);
	  %sort(data=__limitsup,var= Failed95p EventTime);

	  data __limitslow ;
	  set __limitslow ;
	  by Failed5p EventTime;
	  if last.Failed5p;
	  run;

	  data __limitsup;
	  set __limitsup ;
	  by Failed95p EventTime;
	  if first.Failed95p;
	  run;

	  data __limitslow ;
	  set __limitslow ;
	  %IF &TIMEUNIT.=D %THEN __roundtime=round(__time,10);;
	  %IF &TIMEUNIT.=W %THEN __roundtime=round(__time,10);;
	  %IF &TIMEUNIT.=M %THEN __roundtime=round(__time,1);;
	  %IF &TIMEUNIT.=Y %THEN __roundtime=round(__time,0.5);;
	  run;

	  data __limitsup ;
	  set __limitsup ;
	  %IF &TIMEUNIT.=D %THEN __roundtime=round(__time,10);;
	  %IF &TIMEUNIT.=W %THEN __roundtime=round(__time,10);;
	  %IF &TIMEUNIT.=M %THEN __roundtime=round(__time,1);;
	  %IF &TIMEUNIT.=Y %THEN __roundtime=round(__time,0.5);;
	  run;

	  data _null_;
	  set __limitslow;
	  call symput ('limitlow',compress(__time));
	  call symput ('limitlowround',compress(__roundtime));
	  run;

	  data  _null_;
	  set __limitsup;
	  call symput ('limitup',compress(__time));
	  call symput ('limitupround',compress(__roundtime));
	  run;

	  %put limitlow &limitlow  limitup &limitup;

	  %do i=1 %to &maxLevelNo.;

		  data _null_;
		  set __CensoredSummary2;
		      call SYMPUT('Failed'||compress(LevelNo),compress(Failed));
		  run;

		  data __RiskSets&i.;
		  set __RiskSets;
		  if &TestVar.=&&valtrt&i.;
		  run;

	  	  data __RiskSets&i.;
		  merge __theoricaltimepoints __RiskSets&i. ;
		  by IntervalStartTime;
		  run;		

		  data _check&i;
		  set __RiskSets&i.;
		  if IntervalStartTime=&maxTimePoint.;
		  run;

		%let dsid&i = %sysfunc(open(_check&i)); 
		%let ISDATA&i=%SYSFUNC(attrn(&&DSID&i,NOBS));

		%if &&ISDATA&i LE 0  %then %DO;

		data __add&i;
		IntervalStartTime=&maxTimePoint.;
		run;

	  	  data __RiskSets&i.;
		  set __RiskSets&i. __add&i;
		  run; 
		%END;

		%let rc = %sysfunc(close(&&DSID&i));

	  	data __RiskSets&i.;
		  set __RiskSets&i.;
		  if &TestVar.=. then &TestVar.=&&valtrt&i.;
		  if N=. then do;N=0;O=&&Failed&i.;end;
	
		  lag=lag(O);
		  NbEve=O-lag;

		  drop lag ;

		 run;

	  %end;

	  data __RiskSets;
	  set  %do i=1 %to &maxLevelNo.; __RiskSets&i. %end;;
	  run;

	  data __RiskSets;
	  merge __RiskSets __CensoredSummary2 (keep=&testVar. LevelNo) ;
	  by &testVar.;

	  if IntervalStartTime <=&maxTimePoint. and  EventTime=. then EventTime=.Y ;


	  %if &SingleTimePoint.=N %THEN if IntervalStartTime <= &maxTimePoint. and IntervalStartTime <= &LastTimePoints.;;
	  %if &SingleTimePoint.=Y %THEN if IntervalStartTime <= &maxTimePoint. ;;

	  run;


      proc sort data=__RiskSets;
      by &TestVar. IntervalStartTime;
      run;
      proc sort data=__TestLevels;
      by &TestVar. ;
      run;
      proc sort data=__CensoredSummary ;
      by &TestVar. ;
      run;


	  data __RiskSets;
	  set __RiskSets;
	  by &TestVar. IntervalStartTime;
	  previous=lag(IntervalStartTime);
	  if first.&TestVar. then previous= 0;
	
	  mid=	 (IntervalStartTime- previous)/2 + previous;

	  run;


	  %sort(data=__RiskSets,var=&TestVar. IntervalStartTime);	
	  %sort(data=__plot,var=&TestVar. IntervalStartTime);	


	  data __Annotate1;
	  merge __RiskSets __plot ;
	  by &TestVar. IntervalStartTime  ;
	  run;

	  data __Annotate1;
	  merge __Annotate1 __CensoredSummary (where=(Stratum ^= .T) );
	  by &TestVar.;

	  if Total=N then NbEve=Failed;

	  drop Total Failed Censored;

	  run;


	  data __Annotate1;
	  set __Annotate1;

	  length label2 label3 $100;

	  label2=compress(NbEve);
	  label3=compress(N);
	
	  run;

      * Put risk set info into an annotate data set so that it can be printed below the graph;

	%if &Nlevels =1 or &Nlevels =2 %then %let Yplace=8;
	%if &Nlevels =3 or &Nlevels =4 %then %let Yplace=14;
	%if &Nlevels =5 or &Nlevels =6 %then %let Yplace=20;
	%if &Nlevels >6 %then %let Yplace=25;

     data __Annotate;
      set __Annotate1 ;
	  by &TestVar.;

      length function $8 label $160;
      retain function 'text' y1space 'graphpercent' x1space 'datavalue    ' y1 &Yplace. width 15 size 4 ;

	  x1=IntervalStartTime;
      
      if first.&TestVar. then do;
	  TEXTSTYLE='italic';
	  TEXTWEIGHT= 'normal  ';
         y1=&Yplace.;
	     x1space= "graphpercent";
		 x1=3;
         label='O';
         output;
	  TEXTSTYLE='normal';
	  TEXTWEIGHT= 'bold  ';
         y1=&Yplace.;
	     x1space= "graphpercent";
		 x1=7;
         label='N';
         output;
	  end;

	  if IntervalStartTime ne 0 and EventTime ne . then do;
	  TEXTSTYLE='normal';
	      TEXTWEIGHT= 'bold';
	      y1=&Yplace.+LevelNo*(-3);
	      x1space= "datavalue";
		  label=label3;
	      output;


	  TEXTSTYLE='italic';
	      TEXTWEIGHT= 'normal';
	      y1=&Yplace.+LevelNo*(-3);
	      x1space= "datavalue";
		  label=label2;

		  x2=(100/&maxTimeY.)/2;
		  x2b=100/&maxTimeY.;
		  x1=mid;
	      output;


      end;

	  if IntervalStartTime = 0 then do;
	  TEXTSTYLE='normal';
	      TEXTWEIGHT= 'bold';
	      y1=&Yplace.+LevelNo*(-3);
		     x1space= "graphpercent";
			 x1=7;
		  label=label3;
	      output;
	  TEXTSTYLE='italic';
	      TEXTWEIGHT= 'normal';
	      y1=&Yplace.+LevelNo*(-3);
		     x1space= "graphpercent";
			 x1=3;
		  label=label2;
	      output;

      end;


      run;

 	** Annotate for TESTVAR label *;
    data __Annotate2a;
	 set __contents(keep=label);
      length function $8 ;
      retain function 'text' y1space 'graphpercent' x1space 'graphpercent' width 15 size 4 ;
	  TEXTSTYLE='normal';
	      TEXTWEIGHT= 'bold  ';
	      y1=&Yplace.;
	      x1space= "graphpercent";

		  %put formatlength &formatlength.;
		  %if &formatlength <=10 %then x1=92;;
		  %if &formatlength <=20 and &formatlength>10 %then x1=88;;
    run;

	** Annotate for TESTVAR values *;
     data __Annotate2b;
	 set __testlevels;
      length function $8 ;
      retain function 'text' y1space 'graphpercent' x1space 'graphpercent' width 15 size 4 ;
	  TEXTSTYLE='normal';
	   TEXTWEIGHT= 'normal';
	      y1=&Yplace.+LevelNo*(-3);
		  x1space= "graphpercent";
		  %if &formatlength <=10 %then x1=92;;
		  %if &formatlength <=20 and &formatlength>10 %then x1=88;;
		  label=put(&Testvar.,&testvarf..);
	      output;
     run;

 	** Annotate for TimeUnit on X-axis *;
    data __Annotate2c;
      length function $8 label $15.;
      retain function 'text' y1space 'graphpercent' x1space 'graphpercent' width 15 size 4 ;
	  TEXTSTYLE='normal';
	      TEXTWEIGHT= 'normal';
	      y1=%EVAL(&Yplace.+4);
	      x1space= "graphpercent";
		  label="&TimeUnitLabel.";

		  %put formatlength &formatlength.;
		  %if &formatlength <=10 %then x1=92;;
		  %if &formatlength <=20 and &formatlength>10 %then x1=88;;
    run;

 	** Annotate for 'Nb of events...' *;
    data __Annotate2d;
      length function $8 label $150.;
      retain function 'text' y1space 'graphpercent' x1space 'graphpercent' width 150 size 4 ;
	  TEXTSTYLE='italic';
	      TEXTWEIGHT= 'normal';
	      y1=&Yplace.;
	      x1space= "graphpercent";
		  label="Number of observed events during interval";
          x1=30;
    run;
    data __Annotate2d2;
      length function $8 label $150.;
      retain function 'text' y1space 'graphpercent' x1space 'graphpercent' width 150 size 4 ;
	  TEXTSTYLE='normal';
	      TEXTWEIGHT= 'bold';
	      y1=&Yplace.;
	      x1space= "graphpercent";
		  label="  Number of patients at risk after interval";
          x1=61;
    run;

data __Annotate;
set __Annotate __Annotate2a __Annotate2b __Annotate2c __Annotate2d __Annotate2d2;
run;

data __Annotate;	set __Annotate; if IntervalStartTime <= &maxTimePoint.;  run;


%IF &TYPE.=I or &TYPE.=B %THEN %DO;

ods graphics on/OUTPUTFMT=&Filefmt. imagename="&namefile._I" reset=index
width=960px height=720px ;
ods listing gpath="&namepath." sge=on style=MyStyleDefault;


			title bold h=14pt "Hazard versus Time"  ;
		   proc sgplot data=__Plot (where=( MidPoint <= &maxTimePointScale.))
           %if &RiskSets.=Y %then  sganno=__Annotate pad=(bottom=&Percentpad.% right=&RightPercentpad.%) ;;
			 series x=MidPoint y=Response/ group=&TestVar. name="serie" LINEATTRS=(thickness=2) ;
			 %IF &CIDisplay=I or &CIDisplay=B %THEN %DO;
		     highlow  x=MidPoint high=ucl low=lcl / group=&TestVar. close=Response open=Response highcap=SERIF  
			 LOWCAP=SERIF LINEATTRS=(thickness=2) ;;
			 %END;

			 %if &RiskSets.=Y %then 
			 xaxis values=(&OrderList.) display=(nolabel) VALUEATTRS=(SIZE=9) %IF &GRID= Y %THEN grid ;;
			 %if &RiskSets.=N %then 
             xaxis values=(&OrderList.) label="&TimeUnitLabel." VALUEATTRS=(SIZE=9) %IF &GRID= Y %THEN grid ;;

			 yaxis VALUES= (0 to &YTo. by &YIncrement.) label="Hazard" LABELATTRS=(WEIGHT=BOLD) 
			 VALUEATTRS=(SIZE=9) %IF &GRID= Y %THEN grid ;;
			 keylegend "serie" / location=inside DOWN=2
			 position=topleft NOBORDER VALUEATTRS=(WEIGHT=BOLD);
			run;


ods listing gpath=none sge=off;
ods graphics off;
ods listing close;
%END;



%IF &TYPE.=S or &TYPE.=B %THEN %DO;

ods graphics on;
	   proc lifetest data=__data plots= (H (KERNEL=E CL %IF &BANDWITHFLAG.=Y %THEN BW=&BANDWIDTH.; )) alpha=&Alpha.;	  
	   time _time*&CensVar.(&CensVal.);
	   strata &TestVar.;
       ods output  Lifetest.HazardPlot = __smooth; 
      run;	  
ods graphics off;


%sort(data=__smooth,var=Stratum Time);
%sort(data=__testlevels,var=_Stratum);

data __smooth;
merge __smooth __testlevels(rename=(_Stratum=Stratum));
by Stratum;
if Time >=&limitlow. and Time<= &limitup.;
one=1;
run;

** Definition of x-axis values for Smoothed curve;

%GMAX (DATA=__smooth, VAR=Time, BYVAR=one, OUTDATA=__lastsmooth);

data __timeinterval ;set __timeinterval; one=1; run;

data __lastsmooth;
merge __lastsmooth __timeinterval;
by one;
__timescale=int(mtime)+TimeInterval;
call symput ('limitupscale',compress(__timescale));
run;

data _null_  ;
	   set __TimePointLabels2 end=LastObs;
	   by TimePoint;
	   length OrderList ValueList $200;
	   retain OrderList ValueList;
	   if _n_=1 then do;
	      OrderList=trim(left(put(TimePoint,best8.)));
	   end;
	   else do;
	      OrderList=trim(left(OrderList))||' '||trim(left(put(TimePoint,best8.)));
	   end;

	   if LastObs then do;
	      call symput('OrderListSmoothed',trim(left(OrderList)));
	   end;

	   where Timepoint <= &limitupscale.;

run;


%sort(data=__smooth,var=&TestVar. Time);

%let alphalevel=%sysevalf(100-100*&ALPHA);


ods graphics on/OUTPUTFMT=&Filefmt. imagename="&namefile._S" reset=index
	width=960px height=720px ;

	ods listing gpath="&namepath." sge=on style=MyStyleDefault ;

		   title bold h=14pt "Epanechnikov Kernel-Smoothed Hazard Function"  ;
		   proc sgplot data=__smooth  ;
		    %IF &CIDisplay=S or &CIDisplay=B %THEN %DO;
	         band x=Time Upper=UPPER Lower=LOWER / group=&TestVar. transparency=.8; 
			%END; 
			 series x=Time y=Hazard/ group=&TestVar. name="serie" LINEATTRS=(thickness=2) ;
		    %IF &CIDisplay=S or &CIDisplay=B %THEN %DO;
			 title2  h=10pt "with &alphalevel.% Confidence Limits"  ;
			 keylegend "serie" / location=inside DOWN=2
			 position=BOTTOMRIGHT  NOBORDER VALUEATTRS=(WEIGHT=BOLD);
			%END;

		    %IF &CIDisplay=I or &CIDisplay=N %THEN %DO;
			 keylegend "serie" / location=inside DOWN=2
			 position=topleft NOBORDER VALUEATTRS=(WEIGHT=BOLD);
			%END;

			 xaxis values=(&OrderListSmoothed.)  label="&TimeUnitLabel." VALUEATTRS=(SIZE=9) %IF &GRID= Y %THEN grid ;;
			 yaxis VALUES= (0 to &YTo. by &YIncrement.)  label="Estimated Hazard Rate" LABELATTRS=(WEIGHT=BOLD) 
			 VALUEATTRS=(SIZE=9) %IF &GRID= Y %THEN grid ;;
			run;



    ods listing gpath=none sge=off;
	ods graphics off;




%END;


 ************************;
 ************************;
* Prepare rtf file for Interval Hazard ;
 ************************;
 ************************;

	 %IF &TYPE=I OR &TYPE=B %THEN %DO;



		data __risksetsA;
		set __risksets;
		IntervalEndTime=IntervalStartTime;
		IntervalStartTime=lag(IntervalEndTime);
		N1=lag(N);
		if NbEve=. then delete;
		run;

		%sort(data=__risksetsA,var=&TestVar. IntervalStartTime);

		data __risksetsA;
		set __risksetsA;
		by &TestVar. IntervalStartTime;
		if first.&TestVar. then IntervalNb=1;
		else IntervalNb+1;

		run;


		%sort(data=__plot,var=&TestVar. Timepoint);

		data __Plot;
		set __Plot;
		drop IntervalStartTime;
		run;

		data __rtf;
		merge __risksetsA __plot(rename=(TimePoint=IntervalStartTime));
		by &TestVar. IntervalStartTime;
		if IntervalEndTime <= &maxTimePointScale. and IntervalEndTime >.Z;
		drop mid previous;
		run;

		OPTIONS ORIENTATION=LANDSCAPE center nodate nonumber;

		ODS RTF FILE="&OutFile._I.rtf" STYLE=EORTCStyle1 bodytitle startpage=no ;

		ods listing;

		title "Hazard versus time by Intervals";

		proc print data=__rtf noobs label style (data) = data [JUST=C]  style (HEADER) = HEADER [JUST=C]
        WIDTH=FULL split='$';
		var IntervalNb IntervalStartTime IntervalEndTime NbEve O N1 N Response %IF &CIDisplay=I or &CIDisplay=B
        %THEN LCL UCL ;;
		by &TestVar.;
		;
		label IntervalNb="Interval number";
		label IntervalStartTime="Interval Start time$&TimeUnitLabel.";
		label IntervalEndTime="Interval End time$&TimeUnitLabel.";
		label NbEve="Number of events$per interval";
		label O="Cumulative number of$events per interval";
		label N1="Number of patients$at risk at the start$of the interval";
		label N="Number of patients$at risk at the end of$the interval";
		label Response="Hazard";

		%IF &TimeUnit.=D %THEN 	%DO; format Response 8.4;%END;
		%ELSE %DO; format Response 8.3;%END;

		%IF &CIDisplay=I or &CIDisplay=B %THEN %do; 

			%IF &TimeUnit.=D %THEN 	%DO; format LCL UCL 8.4;%END;
			%ELSE %DO; format LCL UCL 8.3;%END;

		%end;

		run;

		ODS RTF CLOSE;
		ods listing close;

	%END;


   * Clear temporary data sets;
   ***************************;

   proc datasets nolist;
   delete  __add: _check: __alltimepoints: __Data __TestLevels __OutSurv __OutPh: __TimePoints: __TimeInterval __Plot2 __Plotmax
           __ProductLimitEstimates: __CensoredSummary: __RiskSets:  __Annotate: __contents __countw:
           __TickMarks __TimePointLabels: __YMinMax __YMinMax1 __Phres Inrisks __Ymin: TPHMax 
           __formats __limits: __max:  __theoricaltimepoints: __windowplot __lastsmooth;
   run;
   quit;


%MEND HazardPlot;
