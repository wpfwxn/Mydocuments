/*------------------------------------------------------------------*
| MACRO NAME  : newsurv
| SHORT DESC  : Creates Kaplan-Meier survival plots with summary
|               built into plot.  Can also output summary table.
*------------------------------------------------------------------*
| CREATED BY  : Meyers, Jeffrey                 (01/20/2013 3:00)
| website: http://www.sascommunity.org/mwiki/images/6/62/Newsurv_08012016.sas
*------------------------------------------------------------------*
| VERSION UPDATES:
| 5.7 (BETA) - 07/29/2016
|  Corrections to CIF confidence limits when CONFTYPE^=LOG.
| 5.7 (BETA) - 05/26/2016
|  Corrections to program when REFLINEMETHOD=FULL and REFLINEAXIS=Y or BOTH.
| 5.7 (BETA) - 05/09/2016
|  Added error checking for TIFF plots and added TIFFDEVICE parameter to
    choose GDEVICE for TIFF plots.
|  Updated code on naming files with TIFF files
|  Added error checking for HEIGHT and WIDTH when making a TIFF file.
| 5.6 (BETA) - 04/28/2016
|  Correction to CIF code when CLASS and STRATA are both supplied
| 5.6 (BETA) - 04/19/2016
|  Added additional error checking for TIME, CENS, CEN_VL, and EV_VL parameters.
    Now confirms TIME and CENS are numeric variables, and confirms that 
    CEN_VL and EV_VL are numeric values and not missing (where applicable).
| 5.5 (BETA) - 03/23/2016
|  Added new parameter LEGENDLINELENGTH to control the length of the lines
    within the legend.  Only available in SAS 9.4M1 or later.
   Corrected a bug making the legend lines very long.
| 5.4 (BETA) - 03/18/2016
|  Made correction to how footnotes for p-values are created
| 5.3 (BETA) - 11/11/2015
|  Made correction to how the graph will cut off the curves at the XMAX
    time.
| 5.2 (BETA) - 11/10/2015
|  Made correction to prevent dataset _TIMELIST doesn't exist error when 
    INCREASE=1 and no TIMELIST is specified.
| 5.1 (BETA) - 10/12/2015
|  Added parameters HRDIGITS, KMESTDIGITS, MEDIANDIGITS, and PVALDIGITS to control
    the number of significant digits shown in the statistical tables
|  Small fix to summary table logic
| 5.0 (BETA) - 09/28/2015
|  Newest beta feature: Cumulative incidence functionality.  The parameter METHOD was
    added to determine whether CIF or KM is used.  The programming used was based on
    the SAS autocall macro %CIF (written by SAS) and compared to the R package CMPRSK.
    The %CIF macro was originally written in IML, which not all locations will have
    installed, so the code was transcribed into data step array formats.  The p-value
    calculation needs the inverse of a matrix calculated, for which PROC FCMP is used.
    There are two options to calculate the variance, COUNT and DELTA.  Hazard ratios are
    available if the user is using SAS 9.4.  An event value of interest must be specified
    in CIF analyses.  See references section.
|  Confidence bounds can be enabled with several new parameters.  PLOTCI enables the
    confidence bounds.  There are options to customize the lines and fill of the
    confidence bounds.  Confidence bounds are now enabled by default when no CLASS
    variable is indicated.
|  Reference lines can be enabled with several new parameters.  They can be set to appear
    at the times set by the TIMELIST parameter or at the median time-to-event time.
    The size and color can be modified, and the reference lines can be set to the
    X, Y or both axes.
|  Labeling corrections were added for CIF and 1-S scenarios for the plot and summary table
|  Edits to error checking programming.
|  Summary table (from PROC REPORT) modified to indicate when the analysis is S, 1-S or CIF
|  Added option to DISPLAY parameter, STANDARD, that allows the macro to create a standard
    list of statistics to be displayed in the plot depending on system version and method.
|  Added logic to determine if columns listed in the TABLEDISPLAY parameter should be automatically 
    disabled if the statistics don't exist
|  Modified footnote markers for table to be better compatible with PDF destinations
|  Made logic corrections with how footnote markers show in the summary table.
|  Added examples of using confidence bounds, reference lines and CIF method
| 4.7 - 03/03/2015
|  Corrected an issue when TIMELIST contains digits
| 4.6 - 02/26/2015
|  Added code to include covariate level p-values from Cox model (adjusted or
    unadjusted) in the plot and summary table.
|  Added parameter TABLEMERGEPVAL to combine covariate level and model level 
    p-values into same column in summary report
|  Added parameters COVPVALHEADER and COVPVALMVHEADER to allow modification of
    the covariate p-value headers in the plot statistics table
|  Added paramters TCOVPVALHEADER, TCOVPVALMVHEADER, TCOVPVALWIDTH, and TCOVPVALMVWIDTH
    to allow the modification of covariate p-value columns in summary report
|  Added parameters REFHRTEXT and REFPTEXT to allow the modification of the text shown
    in plot statistics and summary reports when the CLASS value is the reference level
|  Made a code correction to avoid labeling all values as reference when creating a lattice
    of plots and including extra spaces in macro call
|  Enabled the use of the ` character to indicate a new line for plot statistical table 
    headers and plot titles
| 4.5 - 02/02/2015
|  Added parameter SHOWWALLS to turn the top and right border of the plot
    on or off
|  Added option to paramter PARALIGN (LABELS) to allow the patients-at-risk header
    to appear above the value labels.
| 4.4 - 01/23/2015
|  Fixed a spacing issue when PVAL, Censor legend, and table comments were
   all not being displayed.
| 4.3 - 01/05/2015
|  Corrected an issue with patients-at-risk axis sometimes not matching
     up to plot axis.
| 4.2 - 10/17/2014
| Added description of CLASSDESC parameter to documentation
| 4.1 - 9/30/2014 
|  Added parameter UNIFORMHEIGHT to allow risk space allocations to be
     uniform when at least one value of RISKLOCATION=BOTTOM.  
|  Modified the logrank and Wilcoxon p-values to be the stratified versions
     if STRATA is not missing.  Also changed the default PLOTHEADER to
     list Stratified ... P-value if STRATA is not missing
|  Fixed logic where Wilcoxon p-value was not being pulled properly.
| 4.0 - 9/22/2014 (MWSUG Version)
|  Added parameter NMODELS to allow the user to specify number of models in
     a lattice without repeating values in the TIME parameter
|  Corrected an issue when RISKLLABELLOCATION=ABOVE
|  Corrected the order in SAS 9.4 with RISKLOCATION=INSIDE
|  Changed the ODS OUTPUT Table name for TYPE3 after a certain SAS version
|  Added macro run-time to end of macro line
|  Added code to help create proper TIFF files per a message from Martin Mincey.
     Note this might not work in all versions depending on DPI settings
|  Added code to help SAS 9.4 create better EMF files.  This may change in a later
     version based on findings from SAS technical support
|  Added a check that SAS is in version 9.2 at least
|  Updated documentation
| 3.3 - 7/25/2014
|  Removed the CLASSORDERMETHOD parameter and merged its effects into CLASSORDER
|  Modified error checking code for CLASSORDER parameter
| 3.2 - 7/14/2014
|  Corrected situation when RISKLOCATION=BOTTOM and RISKLABELLOCATION=ABOVE
| 3.1 - 6/25/2014
|  Added WALD as an option to PLOTPVAL and PLOTPVALMV parameters.  Gives the
     type 3 Wald Chi-Square p-value.
|  Corrected the ev_n numbers pulling from the wrong location
|  Corrected the TOTALMVHEADER not correctly saving.
| 3.0 - 6/13/2014
|  Changed method that creates the patients-at-risk tables inside and below
     from scatterplots to blockplots.  The inside patients-at-risk table
     is now placed inside of an innermargin block.
|  Y axis is no longer formatted to add class values
|  Added several new items that can be displayed on the plot summary table
|  Made separate display columns for multivariate Cox hazards ratio to
      plot summary table
|  Added second possible PHREG procedure to calculate adjusted ratios using
      complete case data with adjusting factors.
|  Second LIFETEST Procedure was removed as a way to get all necessary output 
      from one was discovered.
|  Items in the plot summary table can now be reordered columnwise based on how
      they are listed in the DISPLAY parameter.  The Legend will always come
      first if listed.
|  Items in the statistical report table can now be reordered columnwise based on how
      they are listed in the TABLEDISPLAY parameter.
|  The shading in the statistical report table can now be turned on or off with the
      TABLESHADING parameter.
| The column in the plot summary table for the Kaplan-Meier time-points can now be 
      turned off with the LISTTIMEPOINTS parameter when TIMELIST is specified in
      the DISPLAY parameter.
| The ODS PATH is now set to update in the work directory while the macro is running,
      and is reset after the macro finishes to the path prior to the macro.
| The class level statistics in the plot summary table can now be colored to match
      the corresponding plot lines with the STATCOLOR parameter.
| 2.9 - 5/27/2014 (PharmaSUG version)
|  Forced graph to cut off plots at XMAX and YMIN. Corrects an issue with
     exporting graphs to EMF files to import into Microsoft Office products.
|  Made corrections to plot summary table to allow second row of statistics
     (p value, table comments, and censor indicator legend) to show without
     any of the first row statistics (legend, total, event, etc.).
| 2.8 - 5/22/2014
  Changed how the options are saved in the macro.  This was causing an
     issue with creating EMF files.
| 2.7 - 4/21/2014
|  Fixed an issue where '' would show up as risk labels when there is
     no class parameter and CLASSDESC is missing.
| 2.6 - 4/17/2014 
|  Added ability to output the plot dataset with parameter OUTP.
| 2.5 - 4/11/2014
|  Corrected an issue with WHERE parameter when using a quoted string
|  Updated documentation
| 2.4 - 4/01/2014
|  Macro now deletes temporary formats created within the macro
| 2.3 - 3/23/2014
  Added ability to color the numbers in the patients-at-risk table when
|    RISKLOCATION is equal to BOTTOM or INSIDE.  This is set by the new 
|    parameter RISKCOLOR.
| Updated documentation for SVG parameter.
| Added ability to turn the black border around the image on or off with
|   new paramter BORDER.
| 2.2 - 3/18/2014
|  Modified the patients-at-risk header to avoid creating extra space below
|    plot titles.  Increased default font size to 10pt.
|  Changed default linesize to 1pt from 2pt.  2pt works well in scalable vector
|    graphics but is too thick for default plotting.
|  Changed default symbolsize from 4pt to 3pt to better match new default linesize.
|  Changed default lsize (label size) from 8pt to 10pt for better visibility.
| 2.1 - 3/17/2014
|  Fixed an error with the error handling section for class variable
|    reference values.
| 2.0 - 3/6/2014
|  DSN parameter renamed to DATA to comply with external guidelines
|  CENS_VL parameter renamed to CEN_VL to comply with external guidelines
|  CENSOR parameter renamed to CENSORMARKERS for clarity
|  OUTTABLE parameter renamed to OUT to comply with external guidelines
|  Footnote capability added
|  Patients-at-risk for BOTTOM and INSIDE redesigned.  The labels can now 
|    come above the patients-at-risk values as well as to the left.  Two 
|    new parameters control this, RISKLABELLOCATION and RISKLABELALIGN.
|  RISKLABELWEIGHT parameter added to control font weight when RISKLABELOCATION=ABOVE.
|  RISKROWWEIGHTS parameter changed from specifying multiple weights 
|    to specifying the weight desired for each row uniformly of Patients-at-risk
|    table when set to BOTTOM.
|  XLABEL and YLABEL parameters modified to allow superscripts and 
|    unicode characters
|  Massive amount of error checking added
|  Multiple options for summary table added including fonts, column widths,
|    column headings, displayed columns, and layout
|  Options added for title alignments
|  LANDMARK parameter created to allow time to be land-marked by a number
|    or a variable
|  TABLECOMMENTS parameter created to allow free-text comments to be included
|    in plot table summary.
|  Patients-at-Risk table header PARHEADER parameter added 
|  OPTIONS NONOTES enabled while macro runs to reduce run-time
|  Default HEIGHT and WEIGHT set to 6in and 8in respectively
|  Censor legend marker in plot summary table modified to match size of fonts
|  XMAXOFFSET, XMINOFFSET, YMAXOFFSET, and YMINOFFSET parameters added to
|    allow more or less space between plot and plot window
|  WHERE parameter added for subsetting datasets down differently for different
|    models
*------------------------------------------------------------------*
| PURPOSE
|
| This macro runs survival analysis on a time variable with or 
| without a class variable.  The analyses that are run are:
| number of patients, number of events, median time to event,
| hazard ratios, and p-values (logrank, score, and likelihood-
| ratio).  These analyses are stored in a summary dataset that
| is then be used to output a Kaplan-Meier survival plot
| with the summary information listed on the plot (which output
| appears in the plot is customizable), in a summary table, or
| both.  The plot is extremely customizable and the time can
| be transformed by a factor.  The summary table can be stored
| and added to with further calls of the macro (when newtable=0),
| allowing for the output of multiple models.
| 
| Multiple models can be ran in one one macro call, either to add
| multiple models statistics to the summary table or to plot 
| multiple Kaplan-Meier curves into a lattice plot.  Each of these
| plots and lattices are customizable individually by separating
| each model's attributes with a | delimiter.  The number of models
| run is determined by the number of time variables given.  For Example
| to run two models through the macro enter TIME=var1|var2.  See example
| 6 for more details.  The same time variable can be listed multiple 
| times.  For other parameters, if the macro parameter will not change
| across models, the parameter only needs to be listed once and will
| be assumed to be the same across all models.  For example, if 
| TIME=var1|var2 and CLASS=class1, the macro will assume that class1 
| will be the class variable for both models.  If instead the class 
| parameter is: CLASS=class1|class2, then class1 will be used for 
| model 1 and class2 will be used for model 2.  Blank values are also
| accepted.
|
| 1.0: REQUIRED PARAMETERS
| DATA = dataset that contains the time variable, censor variable,
|       and optional class, adjusting variables, and stratification variables.
| TIME = Variable containing time to event information
| CENS = Numeric variable containing event (coded as a binary variable)
| CEN_VL = Numeric constant representing value of a non-event in CENS.
|          Default = 0.
| METHOD = Determines the method for calculating the survival estimates.  Options are
|          KM for Kaplan-Meier and CIF for cumulative incidence function (Competing Risks).
|          Default is KM.
| EV_VL = Numeric constant representing the event of interest in a competing risks
|         analysis.  There is no default for this variable.  It is only required when
|         method = CIF.
|
| 2.0: Optional PARAMETERS
| 2.1: GLOBAL OPTIONS
| 2.1.1: OUTPUT CONTROLLING OPTIONS
| TABLEFMT = Type of ODS output when creating a document. 
|               Default is RTF, options are RTF, PDF, HTML.
| NEWTABLE = A flag variable to determine if a new summary output table is created
|            or if the results will be saved into a previously created dataset.
|            1 = New table made, 0= previous dataset.  Default=1.
| OUT = A name for the output dataset for the statistical report table. Default is ReportData
| ODSFILE = Filepath with name at end to send the output.
|          Example: ~/ibm/example.doc
| OUTP = A name for the output dataset for the plot dataset. Default is PlotData
| PLOT = A flag variable to turn printing the plot off (0) or on (1)
|        Default = 1 (on), options = 1 (on) or 0 (off).
| SUMMARY = A flag variable to display a summary table at the end of
|           the macro off (0) or on (1)
|           Default = 1 (on), options = 1 (on) or 0 (off).
| 2.1.2: IMAGE CONTROLLING OPTIONS
| ANTIALIASMAX = Maximum threshold to keep line smoothing activated.  Set to
|                arbitrarily large number if large file.
| BORDER = Turns the black border in the plot image on (1) or off (0).  Options are
|          1 or 0, default is 0. 
| DPI = Determines the dots per inch of the image file.  Default=200.
| GPATH = Determines the path that the image is saved to.  Defaults to the path 
|         the SAS session is opened to.
| HEIGHT = Sets the height of plot window.  Default is 5in.  Set by a
|          numeric constant followed by px or in.  Must be in for TIFF.
| PLOTNAME = Names the image file.  Plot will be saved per the GPATH parameter.  
|            Default is _surv.
| PLOTFMT = Determines the image type of the plot.  Default is png, options
|            are png, tiff, jpeg, emf, gif.  
|            NOTE: There is special code added for TIFF plots in order to make 
|                  appropriately sized image files.  If DPI settings are too high
|                  depending on operating system and SAS version, this may not work.
|            NOTE2: Special code is made for SAS 9.4 for EMF files.  This is due to SAS
|                   changing the default drivers for EMF files in 9.4, but this change
|                   causes the EMF files to not build properly when trying to convert to
|                   Windows objects.  Code given by Martin Mincey to add registry keys
|                   is used to temporarily add registry keys, then these are removed at
|                   the end of the macro.  This only occurs when making EMF files in SAS 9.4
| SVG = Turns scalable vector graphics on or off.  Only compatible with SAS 9.3 or later.
|       Possible Scalable Vector Graphics formats are EMF within or not within RTF, 
|       PDF, and HTML.  In order to activate the scalable vector graphics, the 
|       TABLEFMT must be used in conjunction with the SVG parameter.  To create
|       SVG EMF files use TABLEFMT=RTF and PLOTFMT=EMF.  To create SVG PDF files
|       use TABLEFMT=PDF.  To create SVG HTML files use TABLEFMT=HTML.
|       Default is 0 (off).  options are 1 or 0
| TIFFDEVICE = Determines the GDEVICE to use when making TIFF plots.  Default is TIFFP.
|              Options can be found with PROC GDEVICE catalogue=sashelp.devices;run;
| WIDTH = Sets the width of plot window.  Default is 7in.  Set by a
|         numeric constant followed by px or in. Must be in for TIFF.
| 2.1.3: LATTICE CONTROLLING OPTIONS
| COLUMNS = Sets the number of columns in a plot lattice
| ROWS = Sets the number of rows in a plot lattice
| NMODELS = Sets the number of models computed within the macro.
|           If PLOT=1 then determines the number of plots generated within the graphic
|           and should be less than or equal to ROWS*COLUMNS.  Default=1.
| ORDER = sets the order that plots are placed into a lattice.  Options are
|         columnmajor and rowmajor.  Rowmajor fills rows first.  Columnmajor fills 
|         columns first.  Default is rowmajor.
|
| 2.2: OPTIONAL CLASS VARIABLES
| CLASS = Variable used to subset patients for comparison (Character or
|         Numeric).  Can be formatted; numeric or character.
| CLASSCOV = List of discrete variables to be used as adjusting covariates in
|            multivariate hazard ratio models.  These are included in the PROC
|            PHREG CLASS statement. Must be variable names in list separated by 
|            spaces.
| CLASSORDER = List of numbers corresponding to the preferred order of the
|              alphabetically sorted class variable formatted values.
|              Example: Values = A, B, C.  CLASSORDER = 2 1 3 would cause
|              order to be B, A, C.
| CLASSREF = Value to use as a reference for hazard ratios.  Must match
|            exact value of the class variable after formatting.
| CONTCOV = List of continuous variables to be used as adjusting covariates in
|            multivariate hazard ratio models.  These are not included in the PROC
|            PHREG CLASS statement. Must be variable names in list separated by 
|            spaces.
| DESC = Reverses the order that the class variable is displayed.
|        Default is 0. Can be 0 or 1.  Enter desc to reverse order. 
| TIES = Determines the method for dealing with ties in the PROC PHREG model 
|          statement.  Default is BRESLOW.  Options are: BRESLOW, DISCRETE, EFRON, 
|          and EXACT.
| PLOTPVAL = Type of P-value to display in plot and summary document. 
|            Default is logrank, options are: score, logrank, lr, wilcoxon, and wald.
|            LR stands for Likelihood-ratio
| PLOTPVALMV = Type of adjusted P-value to display in plot and summary document. 
|              Default is Score, options are: score, wald, and lr.
|              LR stands for Likelihood-ratio
| REFHRTEXT = Text to be shown for reference level within the hazard ratio column.  
|             Default is REF.
| REFPTEXT = Text to be shown for reference level within the covariate p-value column.  
|            Default is --.
| STRATA = Variable(s) separated by spaces to use as a stratification    
|          in Cox models.  Included in the STRATA statement in PHREG.
| 
| 2.3: OPTIONAL DATASET MODIFIERS
| LANDMARK = Gives either a variable or a number to landmark the TIME
|            variable by for the analysis.  Number must be greater than 0.
| XDIVISOR = Numeric constant to transform the time variable into other units. 
|            Will divide the time variable before Kaplan-Meier survival estimates are
|            computed.
| WHERE = Gives a where clause to subset the DATA dataset down.  Type
|         exactly as would be in a procedure step.  
|         Example: where=age>70;
| 
| 2.4: OPTIONAL PATIENT TIME-POINT ESTIMATE OPTIONS
| TIMELIST = Numeric time-points to collect Kaplan-Meier survival
|           estimates w/confidence intervals.  Can be entered as 
|           numeric values separated by commas, or in a list format
|           (example: 0 to 60 by 10).  Time-points should match
|           the TRANSFORMED time-scale of time variable if XDIVISOR is used.
| CONFTYPE = Method of computing confidence intervals for Kaplan-Meier
|            survival estimates. Default is LOG, options are: LOGLOG,
|            ASINSQRT, LOGLOG, LINEAR, LOGIT.
| CIFVAR = Sets the methods for CIF calculation of the variance.  Options are COUNT (Counting method)
|          and DELTA (Marubini's delta method)  Default is COUNT.
| 2.5: OPTIONAL PLOT MODIFIERS
| 2.5.1: AXIS OPTIONS 
| LFAMILY = Sets the font family for the x/y labels. Default is Albany AMT.   
| LSIZE = Sets the font size for the text in the x/y labels. Default=10pt.
|         Must be followed by pt.
| LWEIGHT = Sets the weight of the text in the x/y labels. Default=bold.
|           Options = medium or bold.
| SHOWWALLS = Flag indicator to turn the top and right walls of the plot border
             on (1) or off (0).  Default is 0, options are 0 and 1.
| XINCREMENT = Designates the distance between tick marks on the X-axis. If 
|             left missing this will be calculated as (XMAX-XMIN)/5.
| XLABEL = Sets a label for the x-axis.
| XMAX = Designates the maximum for the x-axis. If left missing this will 
|        calculated as the maximum time value rounded up to the next 
|        number divisible by 5.
| XMAXOFFSET = Designates the amount of space the plot cannot take up at
|              the maximum side of the x-axis. Can be used to help patients-
|              at risk table fit into plot.  Range between [0-1). Default=blank.
|              Blank will be automatically calculated.
| XMIN = Designates the minimum for the x-axis. Default=0.
| XMINOFFSET = Designates the amount of space the plot cannot take up at
|              the minimum side of the x-axis. Can be used to help patients-
|              at risk table fit into plot.  Range between [0-1). Default=blank.
|              Blank will be automatically calculated.
| XTICKVALFAMILY = Sets the font for the x axis tick values. Default=Albany AMT 
| XTICKVALSIZE = Sets the font size for x axis tick values. Default=8pt
| XTICKVALWEIGHT = Sets the font weight for the x axis tick values.
|                  Default=normal, bold=bold.
| YINCREMENT = Designates the distance between tick marks on the Y-axis.  If
|              left missing will be set to 0.1 or 10 depending on YTYPE.
| YLABEL = Sets a label for the y-axis.
| YMAX = Designates the maximum for the y-axis. If left missing will be set
|        to either 1 or 100 depending on YTYPE.
| YMAXOFFSET = Designates the amount of space the plot cannot take up at
|              the maximum side of the y-axis. Range between [0-1). Default=blank.
|              Blank will be automatically calculated.
| YMIN = Designates the minimum for the y-axis.  Default=0.
| YMINOFFSET = Designates the amount of space the plot cannot take up at
|              the minimum side of the y-axis. Range between [0-1). Default=blank.
|              Blank will be automatically calculated.
| YTICKVALFAMILY = Sets the font for the y axis tick values. Default=Albany AMT 
| YTICKVALSIZE = Sets the font size for y axis tick values. Default=8pt
| YTICKVALWEIGHT = Sets the font weight for the y axis tick values.
|                  Default=normal, bold=bold.
| YTYPE = Determines whether Kaplan-Meier survival estimates are in
|         percentages or proportions.  Default is pct, options are:
|         pct and ppt.
| 2.5.2: PLOT STATISTICAL TABLE OPTIONS  
| ALPHAHR	= Specifies the level of significance  for % confidence intervals. 
| 			  The value number must be between 0 and 1. Default value is 0.05, which results in 95% intervals.
| AUTOALIGN = Set a list separated by spaces of locations for the summary
|             table in the plot.  Default is topright bottomleft.  Options
|             are topleft, top, topright, left, center, right, bottomleft,
|             bottom, bottomright. The plot will attempt to fit the table
|             into the alignment that least interferes with the plot lines
|             starting from left to right.
| CLASSVALALIGN = Sets the horizontal alignment for the listed class values.
|                 Options are left,right, and center.  Default=center.
| CLASSDESC = Functions differently depending on whether CLASS is missing or not.
|             No CLASS variable: Gives a description to be used in the legend and
|             patients-at-risk table label
|             CLASS Variable present: Gives a column header for the class levels in
|             the plot summary table.  Using %STR( ) will make this header blank.  Using
|             a ` delimiter will cause a line break
| COVPVALHEADER = Changes the text above the covariate level p-values within the
|                 plot summary table.  Default is Wald P-value. Using
|                 a ` delimiter will cause a line break
| COVPVALMVHEADER = Changes the text above the adjusted covariate level p-values within the
|                   plot summary table.  Default is Adj Wald P-value. Using
|                   a ` delimiter will cause a line break
| DISPLAY = A list of metrics to display in the plot window. Any combination
|           of the following may be entered separated by spaces: legend,
|           hr, median, total, event, timelist, pval, covpval, tablecomments, ev_n, n_ev,
|           hrmv, totalmv, eventmv, pvalmv, covpvalmv, ev_nmv, and n_evmv.  EV_N is a
|           combination of total and event of the format EVENTS/TOTAL.  N_EV is
|           a combination of event and total in the format TOTAL (EVENTS).
|           Options with the MV tag will show the values from an adjusted model
|           given that CLASSCOV or CONTCOV are used.  COVPVAL and COVPVAL are the covariate
|           level p-values from a univariate and adjusted Cox model, respectively.
|           The order that items are written in this parameter determines the order they 
|           appear on the plot with one exception, Legend will always come first if listed.
|           The default is set to STANDARD, which will select default options depending on
|           plot method and SAS version.
| EV_NHEADER = Changes the text above the combined events/total values within the
|              plot summary table.  Default is Events/Total. Using
|              a ` delimiter will cause a line break
| EV_NMVHEADER = Changes the text above the combined multivariate events/total values 
|                within the plot summary table.  Default is Events/Total. Using
|                a ` delimiter will cause a line break
| EVENTHEADER = Changes the text above the Event sample size values within the
|               plot summary table.  Default is Events. Using
|               a ` delimiter will cause a line break
| EVENTMRHEADER = Changes the text above the Multivariate Event sample size values 
|                 within the plot summary table.  Default is MV Events. Using
|                 a ` delimiter will cause a line break
| HRHEADER = Changes the text above the hazard ratio values within the plot
|            summary table. Default is HR (95% CI). Using
|            a ` delimiter will cause a line break
| HRMVHEADER = Changes the text above the adjusted hazard ratio values within the plot
|              summary table. Default is Adj HR (95% CI). Using
|              a ` delimiter will cause a line break
| KMESTHEADER = Changes the text above the Kaplan-Meier Estimates within the
|               plot summary table.  Default is KM Est (95% CI). Using
|               a ` delimiter will cause a line break
| LEGENDHEADER = Changes the text above the legend lines within the plot
|                summary table.  Default is blank.  Using
|                a ` delimiter will cause a line break
| LEGENDLINELENGTH = Sets the length of the legend lines within the summary table.
|                    Default is null.  Example is 0.5in.  Only available in
|                    SAS 9.4M1 or later.
| LISTTIMEPOINTS = Flag variable to determine if the column showing the time-points 
|                  for the Kaplan-Meier event-free rates is displayed.  
|                  Options are 1 (on) and 0 (off).  Default is 1.
| LOCATION = Sets the summary table in the plot to be inside the plot window
|            (inside) or outside the plot window (outside).
| MEDIANHEADER = Changes the text above the hazard ratio values within the plot
|                summary table.  Default is Median (95% CI). Using
|                a ` delimiter will cause a line break
| N_EVHEADER = Changes the text above the combined Total (Events) values 
|              within the plot summary table.  Default is Total (Events). Using
|              a ` delimiter will cause a line break 
| N_EVMVHEADER = Changes the text above the combined multivariate Total (Events) values 
|              within the plot summary table.  Default is MV Total (Events). Using
|              a ` delimiter will cause a line break
| PVALHEADER = Changes the text that comes before the p-value within the plot
|              summary table.  Default is determined by the p-value type chosen. Using
|              a ` delimiter will cause a line break
| PVALMVHEADER = Changes the text that comes before the adjusted p-value within the plot
|                summary table.  Default is determined by the p-value type chosen. Using
|                a ` delimiter will cause a line break
| PTABFAMILY = Sets the font family for the text in the plot.  Default is
|              Albany AMT.
| PTABSIZE = Sets the font size for the text in the plot.  Default is
|            8pt. Must be followed by pt.
| STATCOLOR = A flag variable to enable the text for the statistical values
|             to be colored to match the color of the corresponding plot lines.
|             Options are 1 (on) and 0 (off).  Default is 0.
| RISKTABLEHEADER = Changes the text above the patients at risk list within
|                   the plot summary table.  Default is N at Risk. Using
|                   a ` delimiter will cause a line break
| TABLECOMMENTS = A text string or series of text strings that will show up in
|                 the bottom of the plot summary table.  These text strings are
|                 manually entered into the plot and show up as typed.  These
|                 comments can be broken into multiple lines by splitting them
|                 with the ` delimiter (e.g. Comment 1`Comment2).
| TIMEDX = Acts as a label for the summary table and plot table to
|          describe the time units.
|          Example: TIMELIST=6 and TIMEDX=Months will list 6 Months
| TIMELISTHEADER = Changes the text above the time list values (not estimates)
|                  within the plot summary table.  Default is Time-Point.Using
|                  a ` delimiter will cause a line break
| TOTALHEADER = Changes the text above the total sample size values within the
|               plot summary table.  Default is Total. Using
|               a ` delimiter will cause a line break
| TOTALMVHEADER = Changes the text above the multivariate total sample size values 
|                 within the plot summary table.  Default is MV Total. Using
|                 a ` delimiter will cause a line break
| 2.5.3: OPTIONAL PATIENTS-AT-RISK OPTIONS
| PARALIGN = Determines the alignment of the patients-at-risk subtitle.
|            Default is CENTER.  Options are LEFT|CENTER|RIGHT|LABELS.  LABELS
|            will place subtitle above class values when RISKLABELLOCATION=LEFT.
| PARFAMILY = Determines the font of the text in the patients-at-risk subtitle.
|             Default is Albany AMT.  
| PARHEADER = Creates a sub-title for the patients-at-risk table for the
|             BOTTOM and INSIDE locations.  Default is Patients-at-Risk.
| PARSIZE = Determines the size of the text in the patients-at-risk subtitle.
|           Default is 10pt.  Must contain a number and pt.
| PARWEIGHT = Determines the font weight of the text in the patients-at-risk
|             subtitle.  Default is normal. Options are bold and normal.
| RISKCOLOR = A flag variable that causes the patients-at-risk numbers to 
|             match the colors of the plot lines.  Options are 1 or 0.
|             Default is 0.
| RISKDIVCOLOR = Sets the color of the RISKDIVIDER line.  Default=black.
| RISKDIVIDER = A flag variable to turn off the dividor line between the
|               plot window and the patients at risk table when
|               RISKLIST=INSIDE.  Default=1, options are 1 or 0.
| RISKDIVSTYLE = Sets the line style of the RISKDIVIDER line.  Default=solid.
|                Values can be numbers from 1-46 or values such as: shortdash,
|                mediumdash, longdash, dashdashdot, dash, dot, thindot
| RISKLABELALIGN = Sets the alignment for the patients-at-risk table when RISKLOCATION
|                  is set to INSIDE or BOTTOM and RISKLABELLOCATION=ABOVE.  Default is
|                  LEFT. Options are LEFT, CENTER, or RIGHT.
| RISKLABELDLM = Determines the delimiter between the Risk label and the patients-at-risk
|                table when RISKLOCATION is set to BOTTOM and RISKLABELLOCATION is set to LEFT.
|                Default is -.
| RISKLABELLOCATION = Sets the location of the class level labels for the
|                     patients-at-risk tables when RISKLOCATION is set to 
|                     BOTTOM or INSIDE.  Default is LEFT.  Options are LEFT, ABOVE, and nothing.
| RISKLABELWEIGHT = Sets the font weights of the class level labels for the
|                   patients-at-risk tables when RISKLOCATION is set to 
|                   BOTTOM or INSIDE.  Default is NORMAL.  Options are NORMAL and BOLD.
| RISKLIST = Numeric time-points to collect number of patients at risk.
|            Can be entered as numeric values separated by commas, or in
|            a list format (example: 0 to 60 by 10). 
|            These must be in the TRANSFORMED time units if TIMEDX is used.
| RISKLOCATION = Location for the number at risk to show on the plot.  The
|                The default is nothing, the options are: nothing, bottom (below
|                the x-axis), INSIDE (above x-axis below the plot), and
|                TIMELIST (Displays the number at risk at each value of
|                the timelist variable).  
| RISKROWWEIGHTS = When the RISKLOCATION=BOTTOM option is selected this sets
|                  the weights for the plot window and patients-at-risk table.
|                  Gives the weight value uniformly  to each class level.  
|                  Default is 0.025, range is (0,1)
| UNIFORMHEIGHT = When any of the requested plots have a patients-at-risk table with
|                 RISKLOCATION=BOTTOM, the space below each plot's x-axis will be
|                 uniformly set based on the maximum number of rows and maximum value
|                 of RISKROWWEIGHTS.  Default is 0 (off).  Values are 1 (On) or 0 (Off).
| 2.5.4: OPTIONAL REFERENCE LINE OPTIONS
| REFLINES = Indicates which time-point referencelines are requested at. The options are TIMEPOINTS for
|            the times listed in the TIMELIST parameter and MEDIANS for the median time-to-event times.
|            The default is the null value, which will prevent any reference lines from showing.
| REFLINESIZE = Sets the line thickness for the reference lines.  Default is 1pt.
| REFLINEPATTERN = Sets the pattern for the reference lines.  The default is 2 (for dashed lines).
|                  Options are to do numbers between 1 and 46, or: SOLID, SHORTDASH, MEDIUMDASH, LONGDASH,
|                  MEDIUMDASHSHORTDASH, DASHDASHDOT, DASHDOTDOT, DASH, LONGDASHSHORTDASH,
|                  DOT, THINDOT, SHORTDASHDOT, and MEDIUMDASHDOTDOT.
| REFLINECOLOR = Sets the color of the reference lines.  Default is grey.
| REFLINEMETHOD = Sets the type of reference line.  Options are FULL for lines that go from one of the axis
|                 to the other and DROP for lines that go from the Kaplan-Meier curves to the axis.  Default is DROP.
| REFLINEAXIS = Sets the axis that the reference lines are based off of.  The options are X, Y and Both.  Default is X.
| 2.5.5: OPTIONAL CONFIDENCE INTERVAL OPTIONS
| PLOTCI = Determines if confidence bounds will be drawn.  Options are 0 (No), 1 (Yes), and 2 (Auto).  Setting 2 will
|          enable confidence bounds on plots without a CLASS variable but not on plots with a CLASS variable.
|          Default is 2.
| PLOTCIFILL = Determines if a band plot will be drawn to fill in the space between the confidence intervals.  Options
|              are 0 (No) and 1 (Yes).  Default is 1.
| PLOTCIFILLCOLOR = Sets the colors for the band plots.  Default is null.  Leaving this option null will cause the colors
|                   to match the plot lines from the COLOR parameter.  Otherwise 1 color must be specified per CLASS
|                   group (e.g. Black Red Blue).
| PLOTCIFILLTRANSPARENCY = Sets the transparency of the band plots.  This will cause the fill to be more see-through and the graphs
|                          to be less cluttered.  Must be a number between 0 and 1, larger numbers are more transparent.
|                          Default is 0.95.
| PLOTCILINECOLOR = Sets the colors for the confidence lines.  Default is null.  Leaving this option null will cause the colors
|                   to match the plot lines from the COLOR parameter.  Otherwise 1 color must be specified per CLASS
|                   group (e.g. Black Red Blue).
| PLOTCILINESIZE = Sets the thickness of the confidence interval lines.  Setting this to zero will cause the lines to not show,
|                  but the fill can still be enabled.  Default is 0pt.
| PLOTCILINEPATTERN = Sets the pattern of the confidence interval lines. Default is null. Leaving this option null will cause the patterns
|                     to match the plot lines from the PATTERN parameter.  Otherwise 1 pattern must be specified per CLASS
|                     group (e.g. 1 2 3), or one pattern must be specified to be applied to all. Default is 2 (for dashed lines).  
|                     Options are to do numbers between 1 and 46, or: SOLID, SHORTDASH, MEDIUMDASH, LONGDASH,
|                     MEDIUMDASHSHORTDASH, DASHDASHDOT, DASHDOTDOT, DASH, LONGDASHSHORTDASH,
|                     DOT, THINDOT, SHORTDASHDOT, and MEDIUMDASHDOTDOT.
| 2.5.7: PLOT LINES/SYMBOLS OPTIONS
| CENSORMARKERS = A flag variable to turn on the display of censor marks on the plot. 
|                 Options are 1 (on) and 0 (off).  Default=1.     
| COLOR = A list of colors separated by spaces to color lines in the plot.
|         Default is black.  If only one color is listed, then the lines will
|         change in pattern.  If multiple colors are listed, then all the
|         lines will be solid unless PATTERN is specified.
| LINESIZE = Size of the lines in the Kaplan-Meier curve.  Default = 1pt.
| PATTERN = A list of line patterns separted by spaces to set the line types
|           in the plots.  Default is AUTO (picks numbers if only one color,
|           does solid if multiple colors).  Options are to do numbers between
|           1 and 46, or: SOLID, SHORTDASH, MEDIUMDASH, LONGDASH,
|           MEDIUMDASHSHORTDASH, DASHDASHDOT, DASHDOTDOT, DASH, LONGDASHSHORTDASH,
|           DOT, THINDOT, SHORTDASHDOT, and MEDIUMDASHDOTDOT.
| INCREASE = Flag variable to plot 1-Survival instead of Survival
|            Default=0.  1 causes 1-Survival to be plotted
| SYMBOLSIZE = Size of the censor markers.  Default = 2pt.     
| 2.5.8: TITLE/FOOTNOTE OPTIONS
| FNFAMILY = Sets the font family for the title. Default is Albany AMT.
| FNSIZE = Sets the font size for the text in the footnotes. Default=12pt.
|         Must be followed by pt.
| FNWEIGHT = Sets the weight of the text in the plot title. Default=bold.
|           Options = medium or bold.
| FOOTNOTE = Creates footnotes in the individual plot panes.  Multiple
|            footnotes can be created by separating them with the `
|            delimiter.
| FOOTNOTEALIGN = Sets the horizontal alignment for the footnotes.
|                 Options are left, right and center.  Default=center
| OVFNFAMILY = Sets the font family for the overall footnote. Default is Albany AMT.
| OVFNSIZE = Sets the font size for the text in the overall plot footnote.
|           Default=12pt. Must be followed by pt.
| OVFNWEIGHT = Sets the weight of the text in the overall plot footnote.
|             Default=bold. Options = medium or bold.
| OVFOOTNOTE = Sets the overall footnote for lattice plots
| OVTFAMILY = Sets the font family for the overall title. Default is Albany AMT.
| OVTITLE = Sets the overall title for lattice plots
| OVTSIZE = Sets the font size for the text in the overall plot title.
|           Default=12pt. Must be followed by pt.
| OVTWEIGHT = Sets the weight of the text in the overall plot title.
|             Default=bold. Options = medium or bold.
| TFAMILY = Sets the font family for the title. Default is Albany AMT.
| TITLE = Sets the title for the plot.
| TITLEALIGN = Sets the horizontal alignment for the title.  Options are
|              left, right and center.  Default=center
| TSIZE = Sets the font size for the text in the plot title. Default=12pt.
|         Must be followed by pt.
| TWEIGHT = Sets the weight of the text in the plot title. Default=bold.
|           Options = medium or bold.
| 
| 2.6: OPTIONAL TABLE SUMMARY MODIFIERS
| 2.6.1: DISPLAY OPTIONS
| TABLEDISPLAY = Controls the columns displayed in the table summary.  
|                Options are title, footnote, event, total, ev_n, median, hr,
|                timelist, pval, totalmv, eventmv, ev_nmv, hrmv, and pvalmv.
|                The desired list of outputted columns 
|                should be listed with each parameter separated by a space.  
|                Default=title footnote ev_n median hr timelist pval. Title 
|                turns the TABLETITLE display on or off.  Footnote turns the 
|           TABLEFOOTNOTE on or off. Ev_n combines event and total into one
|           column.  Variables with MV are from adjusted multivariate Cox models.
| TABLEFOOTNOTE = Creates a footnote at the end of the summary table.  Use a ` to
|                designate a line break to create multiple footnotes.
| TABLESHADING = Flag variable to turn alternating shading on or off within the table
|                summary. 1 is on and 0 is off.  Default is 0.
| TABLETITLE = Creates a title at the top of the summary table.
| 2.6.2: FONT OPTIONS
| TABLEDATAFAMILY = Determines the font for the area between the headers and footnotes.
|                  Default is Arial.
| TABLEDATASIZE = Determines the font size for the area between the headers and footnotes.
|                Default is 9pt.
| TABLEDATAWEIGHT = Determines the font weight for the area between the headers and footnotes.
|                  Default is medium. Options are medium and bold.
| TABLEFOOTNOTEFAMILY = Determines the font for the footnotes. Default is Arial.
| TABLEFOOTNOTESIZE = Determines the font size for the footnotes. Default is 10pt.
| TABLEFOOTNOTEWEIGHT = Determines the font weight for the footnotes.
|                      Default is medium. Options are medium and bold.
| TABLEHEADERFAMILY = Determines the font for the headers. Default is Arial.
| TABLEHEADERSIZE = Determines the font size for the headers. Default is 10pt.
| TABLEHEADERWEIGHT = Determines the font weight for the headers.
|                    Default is bold. Options are medium and bold.   
| 2.6.3: COLUMN HEADING OPTIONS
| TCOVPVALHEADER = Determines the header text for the covariate level p-value column.  
|                  Default is Covariate Level^nP-values
| TCOVPVALMVHEADER = Determines the header text for the adjusted covariate level p-value column.  
|                    Default is Adjusted^nCovariate Level^nP-values
| TTOTALHEADER = Determines the header text for the total column.  Default is Total.
| TTOTALMVHEADER = Determines the header text for the multivariate total column.  Default is MV Total.
| TEVENTHEADER = Determines the header text for the event column.  Default is Event.
| TEVENTMVHEADER = Determines the header text for the multivariate event column.  Default is MV Event.
| TEV_NHEADER = Determines the header text for the combined events/total column.  Default is Events/Total.
| TEV_NMVHEADER = Determines the header text for the multivariate events/total column.  Default is MV Events/Total.
| TMEDIANHEADER = Determines the header text for the median column.  Default is Median^n(95% CI).
| THRHEADER = Determines the header text for the hr column.  Default is Hazard Ratio^n(95% CI).
| THRMVHEADER = Determines the header text for the mulutvariate hr column.  Default is Adjusted^nHazard Ratio^n(95% CI).
| TKMESTHEADER = Determines the header text for the timelist column.  
|               Default is Survival Estimates^n(95% CI).
| TPVALHEADER = Determines the header text for the pval column.  Default is P-value.
| TPVALMVHEADER = Determines the header text for the multivariate pval column.  Default is Adjusted^nP-value.
| 2.6.4: COLUMN WIDTH OPTIONS
| TCOVPVALWIDTH = Determines the column width for the covariate level pval column.  Default is 0.7in.
| TCOVPVALMVWIDTH = Determines the column width for the multivariate covariate level pval column.  Default is 0.7in.
| TTOTALWIDTH = Determines the column width for the total column.  Default is 0.5in.
| TTOTALMVWIDTH = Determines the column width for the multivariate total column.  Default is 0.5in.
| TEVENTWIDTH = Determines the column width for the event column.  Default is 0.5in.
| TEVENTMVWIDTH = Determines the column width for the multivariate event column.  Default is 0.5in.
| TEV_NWIDTH = Determines the column width for the ev_n column.  Default is 1in.
| TEV_NWIDTH = Determines the column width for the multivariate ev_n column.  Default is 1in.
| TMEDIANWIDTH = Determines the column width for the median column.  Default is 1.3in.
| THRWIDTH = Determines the column width for the hr column.  Default is 1.1in.
| THRWIDTH = Determines the column width for the multivariate hr column.  Default is 1.1in.
| TTIMELISTWIDTH = Determines the column width for the timelist column.  Default is 1.6in.
| TPVALWIDTH = Determines the column width for the pval column.  Default is 0.7in.
| TPVALMVWIDTH = Determines the column width for the multivariate pval column.  Default is 0.7in.
| 2.7: STATISTICAL SIGNIFICANT DIGITS OPTIONS
| HRDIGITS = Number of significant digits to show for the hazard ratios and confidence bounds.  Default is 2.
| KMESTDIGITS = Number of significant digits to show for the hazard ratios and confidence bounds.  AUTO
|               will determine the number of significant digits based on the YTYPE parameter.  Default is AUTO.
| MEDIANDIGITS = Number of significant digits to show for the median time-to-event and confidence bounds. 
|                Default is 1.
| PVALDIGITS = Number of significant digits to show for all p-values.  Default is 4.
*------------------------------------------------------------------*
| OPERATING SYSTEM COMPATIBILITY
| SAS v9.1 or lower : NO
| UNIX SAS v9.2   :   YES (Limited)
| UNIX SAS v9.3   :   YES
| PC SAS v9.2     :   YES (Limited)
| PC SAS v9.3     :   YES
*------------------------------------------------------------------*
| MACRO CALL
|
| %newsurv (
|            DATA=,
|            TIME=,
|            CENS=,
|            CEN_VL=
|          );
*------------------------------------------------------------------*
| REQUIRED PARAMETERS
|
| Name      : DATA
| Default   : 
| Type      : Dataset Name
| Purpose   : REFER TO REFERENCE SECTION
|
| Name      : TIME
| Default   :
| Type      : Variable Name
| Purpose   : REFER TO REFERENCE SECTION
|
| Name      : CENS
| Default   :
| Type      : Variable Name
| Purpose   : REFER TO REFERENCE SECTION
|
| Name      : CEN_VL
| Default   :
| Type      : Variable Name
| Purpose   : REFER TO REFERENCE SECTION
|
| Name      : METHOD
| Default   :
| Type      : Variable Name
| Purpose   : REFER TO REFERENCE SECTION
|
| Name      : EV_VL
| Default   :
| Type      : Variable Name
| Purpose   : REFER TO REFERENCE SECTION
|
*------------------------------------------------------------------*
| EXAMPLES (MUST BE RUN IN 9.3+ AS SASHELP.BMT DOES NOT EXIST IN 9.2)
|
| Example 1: Basic Example Call:
| %newsurv(data=sashelp.bmt, time=T, cens=STATUS, cen_vl=0);
| Example 2: Call with Class Variable:
| %newsurv(data=sashelp.bmt, time=T, cens=STATUS, cen_vl=0,class=Group);
| Example 3: Call with Class Variable and more options:
| %newsurv(data=sashelp.bmt, time=T, cens=STATUS, cen_vl=0,class=group,
    classref=ALL,summary=0, plot=1,xmin=0, xmax=2500,ptabsize=7pt,
    xincrement=500,symbolsize=5pt,
    xlabel=Time (Days), ylabel=Proportion Alive,autoalign=topright,
    title=Example 3,ytype=ppt,color=BLACK BLUE RED);
| Example 4: Call with Patients-At-Risk table within Plot Window:
| %newsurv(data=sashelp.bmt, time=T, cens=STATUS, cen_vl=0,class=group,
    summary=0, risklist=0 to 2500 by 500, risklocation=INSIDE,
    xmin=0, xmax=2500, xincrement=500,outp=test);
| Example 5: Call with Patients-At-Risk table below Plot Window:
| %newsurv(data=sashelp.bmt, time=T, cens=STATUS, cen_vl=0,class=group,
    summary=0, risklist=0 to 2500 by 500, risklocation=BOTTOM,
    xmin=0, xmax=2500, xincrement=500,risklabellocation=above);
| Example 6: Call with Kaplan-Meier Time-Point Estimates:
| %newsurv(data=sashelp.bmt, time=T, cens=STATUS, cen_vl=0,class=group,
    summary=0, timelist=1000 2000, timedx=Days);
| Example 7: Call with a basic lattice set-up:
| %newsurv(data=sashelp.bmt, time=T, cens=STATUS, cen_vl=0,class=|group,
    nmodels=2,summary=0,height=8in,rows=2,xdivisor=365.25,
    title=Overall Survival|Overall Survival by Disease Group);
| Example 8: Call with confidence intervals:
| %newsurv(data=sashelp.bmt, time=T, cens=STATUS, cen_vl=0,class=group,
    summary=0,height=4in,xdivisor=365.25,plotci=1,color=black red blue);
| Example 9: Call with reference lines:
| %newsurv(data=sashelp.bmt, time=T, cens=STATUS, cen_vl=0,class=group,
    summary=0,height=4in,xdivisor=365.25,reflines=medians,reflineaxis=both);

|**Example dataset for CIF Examples:
proc format;
   value grpLabel 1='ALL' 2='AML low risk' 3='AML high risk';
run;

data BMT;
        input DIAGNOSIS Ftime Status Gender@@;
        label Ftime="Days";
        format Diagnosis grpLabel.;
datalines;
1       2081       0       1       1       1602    0       1
1       1496       0       1       1       1462    0       0
1       1433       0       1       1       1377    0       1
1       1330       0       1       1       996     0       1
1       226        0       0       1       1199    0       1
1       1111       0       1       1       530     0       1
1       1182       0       0       1       1167    0       0
1       418        2       1       1       383     1       1
1       276        2       0       1       104     1       1
1       609        1       1       1       172     2       0
1       487        2       1       1       662     1       1
1       194        2       0       1       230     1       0
1       526        2       1       1       122     2       1
1       129        1       0       1       74      1       1
1       122        1       0       1       86      2       1
1       466        2       1       1       192     1       1
1       109        1       1       1       55      1       0
1       1          2       1       1       107     2       1
1       110        1       0       1       332     2       1
2       2569       0       1       2       2506    0       1
2       2409       0       1       2       2218    0       1
2       1857       0       0       2       1829    0       1
2       1562       0       1       2       1470    0       1
2       1363       0       1       2       1030    0       0
2       860        0       0       2       1258    0       0
2       2246       0       0       2       1870    0       0
2       1799       0       1       2       1709    0       0
2       1674       0       1       2       1568    0       1
2       1527       0       0       2       1324    0       1
2       957        0       1       2       932     0       0
2       847        0       1       2       848     0       1
2       1850       0       0       2       1843    0       0
2       1535       0       0       2       1447    0       0
2       1384       0       0       2       414     2       1
2       2204       2       0       2       1063    2       1
2       481        2       1       2       105     2       1
2       641        2       1       2       390     2       1
2       288        2       1       2       421     1       1
2       79         2       0       2       748     1       1
2       486        1       0       2       48      2       0
2       272        1       0       2       1074    2       1
2       381        1       0       2       10      2       1
2       53         2       0       2       80      2       0
2       35         2       0       2       248     1       1
2       704        2       0       2       211     1       1
2       219        1       1       2       606     1       1
3       2640       0       1       3       2430    0       1
3       2252       0       1       3       2140    0       1
3       2133       0       0       3       1238    0       1
3       1631       0       1       3       2024    0       0
3       1345       0       1       3       1136    0       1
3       845        0       0       3       422     1       0
3       162        2       1       3       84      1       0
3       100        1       1       3       2       2       1
3       47         1       1       3       242     1       1
3       456        1       1       3       268     1       0
3       318        2       0       3       32      1       1
3       467        1       0       3       47      1       1
3       390        1       1       3       183     2       0
3       105        2       1       3       115     1       0
3       164        2       0       3       93      1       0
3       120        1       0       3       80      2       1
3       677        2       1       3       64      1       0
3       168        2       0       3       74      2       0
3       16         2       0       3       157     1       0
3       625        1       0       3       48      1       0
3       273        1       1       3       63      2       1
3       76         1       1       3       113     1       0
3       363        2       1
;
run;
| Example 10: Example using the CIF method (run above data step first)
| %newsurv(data=bmt, time=ftime, cens=STATUS, cen_vl=0,ev_vl=1,
    method=cif,class=diagnosis,height=4in,
    summary=0, risklist=0 to 2500 by 500, risklocation=BOTTOM,
    xmin=0, xmax=2500, xincrement=500,risklabellocation=left,
    timelist=250 500, timedx=Days,parheader=);
*------------------------------------------------------------------*
| REFERENCES
| The code for the CIF method is transcribed from the SAS autocall
|   macro %CIF.  The code was originally written primarily in IML,
|   so within this macro it was rewritten in data step language
|   instead.  The references within the %CIF macro are as follows:
|   1. Marubini, E. and Valsecchi, M.G. (1995), Analysing survival data from clinical trials
|      and observational studies, John Wiley.
|   2. Gray, R.J. (1988). "A class of K-sample tests for comparing the cumulative incidence of
|      a competing risk," Annals of statistics, 16(3), 1141--1154.
|   3. Klein, J.P. and Moeschberger, M.L., (2003), Survival analysis: techniques for censored
|      and truncated data, Springer Verlag.
*------------------------------------------------------------------*
| This program is free software; you can redistribute it and/or
| modify it under the terms of the GNU General Public License as
| published by the Free Software Foundation; either version 2 of
| the License, or (at your option) any later version.
|
| This program is distributed in the hope that it will be useful,
| but WITHOUT ANY WARRANTY; without even the implied warranty of
| MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
| General Public License for more details.
*------------------------------------------------------------------*/


%macro newsurv(
    /*** 1.0: Required Variables ***/
    cens=,cen_vl=0,data=,ev_vl=,method=KM,time=,
    
    /*** 2.0: Optional Variables ***/
    /** 2.1: Global Options **/
    /* 2.1.1: Output Controlling Options*/
    border=0,tablefmt=rtf,newtable=1,out=reportdata,odsfile=,
    outp=plotdata,plot=1,summary=1,
    /* 2.1.2: Image Controlling Options*/
    antialiasmax=1000,dpi=200,gpath=,height=6in,
    plotname=_surv,plotfmt=png,svg=0,tiffdevice=TIFFP,width=8in, 
    /* 2.1.3: Lattice Controlling Options*/
    columns=1,nmodels=1,order=columnmajor,rows=1,
    
    /** 2.2: Class Variables **/
    class=,classcov=,classdesc=,classorder=,
    classref=,contcov=,desc=0,ties=BRESLOW,plotpval=,
    refhrtext=Reference,refptext=--,strata=,plotpvalmv=,
    /** 2.3: Dataset Modifiers **/
    landmark=,xdivisor=, where=,
    
    /** 2.4: Patient Kaplan-Meier Time-point Estimate Options **/
    cifvar=COUNT,conftype=LOGLOG,timelist=,
    
    /** 2.5: Plot Options **/
    /* 2.5.1: Axis Options*/
    lfamily=Arial,lsize=10pt,lweight=bold,showwalls=0,
    xincrement=,xlabel=,xmax=,xmaxoffset=,xmin=0,xminoffset=,
    xtickvalfamily=Arial,xtickvalsize=8pt,xtickvalweight=normal,
    yincrement=,ylabel=,ymax=,ymaxoffset=,ymin=0,yminoffset=,
    ytickvalfamily=Arial,ytickvalsize=8pt,ytickvalweight=normal,ytype=pct,
    /* 2.5.2: Plot Statistal Table Options*/
    autoalign=topright bottomleft,classvalalign=center,covpvalheader=Wald P-value,covpvalmvheader=Adj Wald P-value,
    display=standard,legendlinelength=,alphaHR=0.05,
    ev_nheader=Events/Total,eventheader=Event,hrheader=HR (95% CI),KMEstheader=KM Est (95% CI),
    legendheader=%str( ),location=inside,medianheader=Median (95% CI),n_evheader=Total (Events),
    totalmvheader=MV Total,eventmvheader=MV Event,hrmvheader=Adj HR (95% CI),
    ev_nmvheader=MV Events/Total,n_evmvheader=MV Total (Events),pvalmvheader=,
    ptabsize=8pt, ptabfamily=Albany AMT,pvalheader=,risktableheader=N at Risk,statcolor=0,tablecomments=,
    timedx=,listtimepoints=1,timelistheader=Time-Point,totalheader=Total,       
    /* 2.5.3: Patients-at-Risk Options*/
    paralign=CENTER,parfamily=Albany AMT,parheader=Patients-at-Risk,parsize=10pt,parweight=normal,
    riskcolor=0,riskdivcolor=black,riskdivider=1,riskdivstyle=solid,
    risklabelalign=LEFT,risklabeldlm=-,risklabellocation=LEFT,risklabelweight=normal,
    risklist=,risklocation=,riskrowweights=0.025,uniformheight=0,
    /* 2.5.4: Plot Lines/Symbols Options*/
    censormarkers=1,color=black,linesize=1pt,pattern=AUTO,
    increase=0, symbolsize=5pt,
    /*2.5.5: Title/Footnote Options*/
    fnfamily=Albany AMT,fnsize=8pt,fnweight=normal,
    footnote=, footnotealign=left,
    ovfnfamily=Albany AMT,ovfnsize=8pt,ovfnweight=normal,
    ovfootnote=,ovfootnotealign=left,
    ovtfamily=Albany AMT,ovtitle=,ovtitlealign=center,ovtsize=12pt,ovtweight=bold, 
    tfamily=Albany AMT,title=, titlealign=center,
    tsize=12pt,tweight=bold,
    /* 2.5.4: Reference Line Options*/
    reflines=,reflinesize=1pt,reflinepattern=2,reflinecolor=grey,reflinemethod=drop,reflineaxis=X,
    /* 2.5.5: Confidence Interval Options*/
    plotci=2,plotcifill=1,plotcifillcolor=,plotcifilltransparency=0.95,
    plotcilinecolor=,plotcilinesize=0pt,plotcilinepattern=2,
    
    /** 2.6: Optional Table Summary Options **/ 
    /* 2.6.1: Display Options */
    tablemergepval=0,tablefootnote=,
    tabledisplay=title footnote ev_n median hr timelist pval, 
    tableshading=0,tabletitle=,
    /* 2.6.2: Font Options */
    tabledatafamily=Arial,tabledatasize=9pt,tabledataweight=medium,
    tablefootnotefamily=Arial,tablefootnotesize=10pt,tablefootnoteweight=medium,
    tableheaderfamily=Arial,tableheadersize=10pt,tableheaderweight=bold,
    /* 2.6.3: Column Heading Options*/
    tcovpvalheader=Covariate Level^nP-values,tcovpvalmvheader=Adjusted^nCovariate Level^nP-values,
    teventheader=Event,teventmvheader=MV Event,tev_nheader=Event/Total,tev_nmvheader=MV Event/Total,
    thrheader=Hazard Ratio^n(95% CI),thrmvheader=Adjusted^nHazard Ratio^n(95% CI),
    ttimelistheader=Survival Estimates^n(95% CI),
    tmedianheader=Median^n(95% CI),
    tpvalheader=P-value,tpvalmvheader=Adjusted^nP-value,
    ttotalheader=Total,ttotalmvheader=MV Total, 
    /* 2.6.4: Column Width Options */
    tcovpvalwidth=0.7in,tcovpvalmvwidth=0.7in,
    ttotalwidth=0.5in,teventwidth=0.5in,tev_nwidth=1in,
    ttotalmvwidth=0.5in,teventmvwidth=0.5in,tev_nmvwidth=1in,
    thrwidth=1.1in,thrmvwidth=1.1in,tmedianwidth=1.3in,
    tpvalwidth=0.7in,tpvalmvwidth=0.7in,ttimelistwidth=1.6in,
    /* 2.7: Statistic Significant Digits Options */
    pvaldigits=4,hrdigits=2,mediandigits=1,kmestdigits=auto);


    /**Save current options to reset after macro runs**/
    %local _mergenoby _notes _qlm _odspath _starttime _device _gsfname
        _xmax _ymax _xpixels _ypixels _imagestyle _iback;
    %let _starttime=%sysfunc(time());
    %let _notes=%sysfunc(getoption(notes));
    %let _mergenoby=%sysfunc(getoption(mergenoby));
    %let _qlm=%sysfunc(getoption(quotelenmax)); 
    %let _device=%sysfunc(getoption(device));
    %let _gsfname=%sysfunc(getoption(gsfname));
    %let _xmax=%sysfunc(getoption(xmax));
    %let _ymax=%sysfunc(getoption(ymax));
    %let _xpixels=%sysfunc(getoption(xpixels));
    %let _ypixels=%sysfunc(getoption(ypixels));
    %let _imagestyle=%sysfunc(getoption(imagestyle));
    %let _iback=%sysfunc(getoption(iback));
    %let _odspath=&sysodspath;
    %if %sysevalf(%superq(_odspath)=,boolean) %then %let _odspath=WORK.TEMPLAT(UPDATE) SASHELP.TMPLMST (READ);
    /**Turn off warnings for merging without a by and long quote lengths**/
    /**Turn off notes**/
    options mergenoby=NOWARN nonotes noquotelenmax;
    ods path WORK.TEMPLAT(UPDATE) SASHELP.TMPLMST (READ);
    
    /*Don't send anything to output window, results window, and set escape character*/
    ods select none;
    ods noresults escapechar='^';
    
    /**Process Error Handling**/
    %if &sysver < 9.2 %then %do;
        %put ERROR: SAS must be version 9.2 or later;
        %goto errhandl;
    %end;       
    %else %if %sysfunc(exist(&data))=0 %then %do;
        %put ERROR: Dataset &data does not exist;
        %put ERROR: Please enter a valid dataset;
        %goto errhandl;
    %end;
    %else %if %sysevalf(%superq(data)=,boolean)=1 %then %do;
        %put ERROR: DATA parameter is required;
        %put ERROR: Please enter a valid dataset;
        %goto errhandl;
    %end;
    %if %sysfunc(find(&method,CIF,i))>0 %then %do;
        %put WARNING: Competing risks analysis still in beta, some functionality limited to SAS version 9.4+;
        %put WARNING: Please forward any issues found to meyers.jeffrey@mayo.edu;
    %end;
    
    /**Pull dataset information**/
    proc contents data=&data out=_temp noprint;
    run;
    
    /**Create list of macro variables that can vary across different models called**/
    /**Sets up for lattice plots**/
    %local _mvarlist;
    %let _mvarlist=%sysfunc(compress(autoalign|
        cens|cen_vl|censormarkers|class|classcov|classdesc|
        classorder|classref|classvalalign|cifvar|color|conftype|contcov|covpvalheader|covpvalmvheader|
        desc|display|ev_nheader|ev_nmvheader|ev_vl|eventheader|eventmvheader|fnfamily|fnsize|fnweight|footnote|
        footnotealign|hrheader|hrmvheader|ties|KMEstheader|alphaHR|
        landmark|legendheader|legendlinelength|lfamily|linesize|listtimepoints|location|lsize|lweight|medianheader|
        method|n_evheader|n_evmvheader|paralign|parfamily|parheader|parsize|parweight|
        pattern|plotci|plotcifill|plotcifillcolor|plotcifilltransparency|
        plotcilinecolor|plotcilinesize|plotcilinepattern|    
        plotpval|plotpvalmv|pvalheader|pvalmvheader|ptabfamily|ptabsize|refhrtext|refptext|
        riskcolor|riskdivcolor|riskdivider|riskdivstyle|risklabelalign|risklabeldlm|risklabellocation|
        risklabelweight|risklist|risklocation|riskrowweights|risktableheader|increase|statcolor|strata|symbolsize|tablecomments|
        tfamily|time|timedx|timelist|timelistheader|title|titlealign|totalheader|totalmvheader|tsize|tweight|
        where|xdivisor|xincrement|xlabel|xmax|xmaxoffset|xmin|xminoffset|
        reflinecolor|reflinemethod|reflinepattern|reflines|reflinesize|reflineaxis|
        xtickvalfamily|xtickvalsize|xtickvalweight|
        ylabel|yincrement|ymax|ymaxoffset|ymin|yminoffset|ytickvalfamily|ytickvalsize|ytickvalweight|ytype|
        pvaldigits|hrdigits|mediandigits|kmestdigits));
    %local i j;
    %do i = 1 %to &nmodels;
        /**Cycle through each macro parameter**/
        %do j = 1 %to %sysfunc(countw(&_mvarlist,|));
            %local v&j;
            %let v&j=%scan(%superq(_mvarlist),&j,|);
            %local &&v&j..&i;
            /**If the | delimiter is detected, assign the different values between | to numbered parameters**/
            /**Else Assign the same value to all numbered parameters**/
            %if %index(%superq(&&v&j),|)>0 %then %let &&v&j..&i=%qscan(%superq(&&v&j),&i,|,m);
            %else %let &&v&j..&i=%qscan(%superq(&&v&j),1,|,m); 
        %end;                
    %end;                       
    %local z nerror;
    %let nerror=0;
    /**Error Handling on Individual Model Variables**/
    %macro _varcheck(var,require,numeric) / mindelimiter=',';
        %local _z _numcheck;
        %do z = 1 %to &nmodels;
            /**Check if variable parameter is missing**/
            %if %sysevalf(%superq(&var.&z)=,boolean)=0 %then %do;
                %if %sysfunc(notdigit(%superq(&var.&z))) > 0 %then
                    %do _z = 1 %to %sysfunc(countw(%superq(&var.&z),%str( )));
                    /**Check to make sure variable names are not just numbers**/    
                    %local datid;
                    /**Open up dataset to check for variables**/
                    %let datid = %sysfunc(open(&data));
                    /**Check if variable exists in dataset**/
                    %if %sysfunc(varnum(&datid,%scan(%superq(&var.&z),&_z,%str( )))) = 0 %then %do;
                        %put ERROR: (Model &z: %qupcase(&var)) Variable %qupcase(%scan(%superq(&var.&z),&_z,%str( ))) does not exist in dataset &data;
                        %local closedatid;
                        /**Close dataset**/
                        %let closedatid=%sysfunc(close(&datid));
                        %let nerror=%eval(&nerror+1);
                    %end;
                    %else %do;
                        %local closedatid;
                        %let closedatid=%sysfunc(close(&datid));
                        %if &numeric=1 %then %do;
                            data _null_;
                                set &data (obs=1);
                                call symput('_numcheck',strip(vtype(%superq(&var.&z))));
                            run;
                            %if %sysevalf(%superq(_numcheck)^=N,boolean) %then %do;
                                %put ERROR: (Model &z: %qupcase(&var)) Variable must be numeric;
                                %let nerror=%eval(&nerror+1);
                            %end;   
                        %end;                         
                    %end;
                %end;
                %else %do;
                    /**Give error message if variable name is number**/
                    %put ERROR: (Model &z: %qupcase(&var)) Variable is not a valid SAS variable name (%superq(&var.&z));
                    %let nerror=%eval(&nerror+1);
                %end;
            %end;
            %else %if &require=1 %then %do;
                /**Give error if required variable is missing**/
                %put ERROR: (Model &z: %qupcase(&var)) Variable is a required variable but has no value;
                %let nerror=%eval(&nerror+1);
            %end;
        %end;
    %mend;
    /**Check time variables**/
    %_varcheck(time,1,1)
    /**Censor Variables**/
    %_varcheck(cens,1,1)
    /**Class Variables**/
    %_varcheck(class,0)
    /**Strata Variables**/
    %_varcheck(strata,0)
    /**Class Type Covariate Variables**/
    %_varcheck(classcov,0)
    /**Continuous Type Covariate Variables**/
    %_varcheck(contcov,0)
    /**Landmark Variables**/
    %do z = 1 %to &nmodels;
        /**Check if variable parameter is missing**/
        %if %sysevalf(%superq(landmark&z)=,boolean)=0 %then %do;
            %if %sysfunc(notdigit(%sysfunc(compress(%superq(landmark&z),.-)))) > 0 %then %do;
                /**Check to make sure variable names are not just numbers**/
                %local datid;
                /**Open up dataset to check for variables**/
                %let datid = %sysfunc(open(&data));
                /**Check if variable exists in dataset**/
                %if %sysfunc(varnum(&datid,%superq(landmark&z))) = 0 %then %do;
                    %put ERROR: (Model &z: %qupcase(landmark)) Variable %qupcase(%superq(landmark&z)) does not exist in dataset &data;
                    %local closedatid;
                    /**Close dataset**/
                    %let closedatid=%sysfunc(close(&datid));
                    %let nerror=%eval(&nerror+1);
                %end;
                %else %do;
                    %local closedatid;
                    %let closedatid=%sysfunc(close(&datid));
                %end;
            %end;
        %end;
    %end;
    
             
    /**Error Handling on Individual Model Parameters Involving units**/
    %macro _unitcheck(parm,allowmissing);
        %do z = 1 %to &nmodels;
            %if %sysevalf(%superq(&parm.&z)=,boolean)=1 %then %do;
                %if %sysevalf(&allowmissing^=1,boolean) %then %do;
                    /**Check for missingness**/
                    %put ERROR: (Model &z: %qupcase(&parm)) Cannot be set to missing;
                    %let nerror=%eval(&nerror+1);
                 %end;
            %end;
            %else %if %sysfunc(compress(%superq(&parm.&z),ABCDEFGHIJKLMNOPQRSTUVWXYZ,i)) lt 0 %then %do;
                /**Check if value is less than zero**/
                %put ERROR: (Model &z: %qupcase(&parm)) Cannot be less than zero (%qupcase(%superq(&parm.&z)));
                %let nerror=%eval(&nerror+1);
            %end;
        %end;
    %mend;
    /**Font Sizes**/
    /*Y Tick Value Font Size**/
    %_unitcheck(ytickvalsize)
    /**X Tick Value Font Size**/
    %_unitcheck(xtickvalsize)
    /***Label Font Size**/
    %_unitcheck(lsize)
    /**Plot Table Font Size**/
    %_unitcheck(ptabsize)
    /**Title Font Size**/
    %_unitcheck(tsize)
    /**Footnote Font Size**/
    %_unitcheck(fnsize)
    /***Patients-at-Risk Header Font Size**/
    %_unitcheck(parsize)
    /**Plot Line Size**/
    %_unitcheck(linesize)
    /**Plot Symbol Size**/
    %_unitcheck(symbolsize)
    /**Plot Confidence Bounds Line Size**/
    %_unitcheck(plotcilinesize)
    /**Plot Reference Line Size**/
    %_unitcheck(reflinesize)
    /**Plot Reference Line Size**/
    %_unitcheck(legendlinelength,1)
    /**Error Handling on Individual Model Numeric Variables**/
    %macro _numcheck(parm,min,contain,default);
        %do z = 1 %to &nmodels;
            /**Check if missing**/
            %if %sysevalf(%superq(&parm.&z)=,boolean)=0 %then %do;
                %if %sysfunc(notdigit(%sysfunc(compress(%superq(&parm.&z),.)))) > 0 %then %do;
                    /**Check if character values are present**/
                    %put ERROR: (Model &z: %qupcase(&parm)) Must be numeric.  %qupcase(%superq(&parm.&z)) is not valid.;
                    %let nerror=%eval(&nerror+1);
                %end;  
                %else %if %superq(&parm.&z) le &min and &contain=0 %then %do;
                    /**Check if value is below minimum threshold**/
                    %put ERROR: (Model &z: %qupcase(&parm)) Must be greater than &min.  %qupcase(%superq(&parm.&z)) is not valid.;
                    %let nerror=%eval(&nerror+1);
                %end;  
                %else %if %superq(&parm.&z) lt &min and &contain=1 %then %do;
                    /**Check if value is below minimum threshold**/
                    %put ERROR: (Model &z: %qupcase(&parm)) Must be greater than or equal to &min.  %qupcase(%superq(&parm.&z)) is not valid.;
                    %let nerror=%eval(&nerror+1);
                %end; 
            %end;   
            %else %let &parm.&z=&default;        
        %end;
    %mend;
    /**X Axis Minimum Value**/
    %_numcheck(xmin,0,1,0)
    /**Y Axis Minimum Value**/
    %_numcheck(ymin,0,1,0)
    /**Y Axis Minimum Value**/
    %_numcheck(ymin,0,1,0)  
    /**Censor Value**/
    %do z = 1 %to &nmodels;
        /**Check if missing**/
        %if %sysevalf(%superq(cen_vl&z)=,boolean)=0 %then %do;
            %if %sysfunc(notdigit(%sysfunc(compress(%superq(CEN_VL&z),.)))) > 0 %then %do;
                /**Check if character values are present**/
                %put ERROR: (Model &z: %qupcase(CEN_VL)) Must be numeric.  %qupcase(%superq(CEN_VL&z)) is not valid.;
                %let nerror=%eval(&nerror+1);
            %end;  
        %end;   
        %else %do;
            /**Check if character values are present**/
            %put ERROR: (Model &z: %qupcase(CEN_VL)) Is Required and cannot be missing;
            %let nerror=%eval(&nerror+1);
        %end; 
    %end;   
    /**Event Code for CIF Method**/
    %do z = 1 %to &nmodels;
        %if %sysevalf(%qupcase(%superq(method&z))=CIF,boolean) %then %do;
            /**Check if missing**/
            %if %sysevalf(%superq(ev_vl&z)=,boolean)=0 %then %do;
                %if %sysfunc(notdigit(%sysfunc(compress(%superq(EV_VL&z),.)))) > 0 %then %do;
                    /**Check if character values are present**/
                    %put ERROR: (Model &z: %qupcase(EV_VL)) Must be numeric.  %qupcase(%superq(EV_VL&z)) is not valid.;
                    %let nerror=%eval(&nerror+1);
                %end;  
            %end;   
            %else %do;
                /**Check if character values are present**/
                %put ERROR: (Model &z: %qupcase(EV_VL)) Is Required and cannot be missing when METHOD=CIF;
                %let nerror=%eval(&nerror+1);
            %end; 
        %end;
    %end;             
    /***Error checking for RISKROWWEIGHTS***/
    %do z = 1 %to &nmodels;
        /**Check if RISKROWWEIGHTS is missing when RISKLOCATION is set to BOTTOM**/
        %if %sysevalf(%superq(riskrowweights&z)=,boolean)=0 and %superq(risklocation&z)=BOTTOM %then %do;
            %if %sysfunc(notdigit(%sysfunc(compress(%superq(riskrowweights&z),.-)))) %then %do;
                %put ERROR: (Model &z: RISKROWWEIGHTS) Must be a numeric value (%superq(riskrowweights&z));
                %put ERROR: Macro NEWSURV will cease;
                %let nerror=%eval(&nerror+1);
            %end;
            %else %if %sysevalf(%superq(riskrowweights&z)<0,boolean)=1 or %sysevalf(%superq(riskrowweights&z)>=1.0,boolean)=1 %then %do;
                %put ERROR: (Model &z: RISKROWWEIGHTS) Is not between 0 and 1 (%superq(riskrowweights&z));
                %put ERROR: Macro NEWSURV will cease;
                %let nerror=%eval(&nerror+1);
            %end;
        %end;
        %else %if %superq(risklocation&z)=BOTTOM %then %do;
            %put ERROR: (Model &z: RISKROWWEIGHTS) No risk row weights specified when risklocation=BOTTOM;
            %put ERROR: Macro NEWSURV will cease;
            %let nerror=%eval(&nerror+1);
        %end;
    %end;
        
    /**Error Handling on Individual Model Parameters**/
    %macro _parmcheck(parm, parmlist,kmlist,ciflist);
        %do z = 1 %to &nmodels;  
            %if %sysevalf(%superq(&parm.&z)=,boolean)=0 %then %let &parm.&z=%sysfunc(compress(%qupcase(%superq(&parm.&z)),'""'));
            %local _test _z;
            %let _test=;
            %if %sysevalf(%superq(parmlist)^=,boolean) %then %do;
                %do _z=1 %to %sysfunc(countw(&parmlist,|,m));
                    %if %superq(&parm.&z)=%scan(&parmlist,&_z,|,m) %then %let _test=1;
                %end;
                %if &_test ^= 1 %then %do;
                    %put ERROR: (Model &z: %qupcase(&parm)): %superq(&parm.&z) is not a valid value;
                    %put ERROR: (Model &z: %qupcase(&parm)): Possible values are &parmlist;
                    %let nerror=%eval(&nerror+1);
                %end;
            %end;
            %else %do;
                %if %sysevalf(%qupcase(%superq(method&z))=KM,boolean) %then %do;
                    %do _z=1 %to %sysfunc(countw(&kmlist,|,m));
                        %if %superq(&parm.&z)=%scan(&kmlist,&_z,|,m) %then %let _test=1;
                    %end;
                    %if &_test ^= 1 %then %do;
                        %put ERROR: (Model &z: %qupcase(&parm)): %superq(&parm.&z) is not a valid value;
                        %put ERROR: (Model &z: %qupcase(&parm)): Possible values for Kaplan-Meier method are &kmlist;
                        %let nerror=%eval(&nerror+1);
                    %end;
                %end;
                %if %sysevalf(%qupcase(%superq(method&z))=CIF,boolean) %then %do;
                    %do _z=1 %to %sysfunc(countw(&ciflist,|,m));
                        %if %superq(&parm.&z)=%scan(&ciflist,&_z,|,m) %then %let _test=1;
                    %end;
                    %if &_test ^= 1 %then %do;
                        %put ERROR: (Model &z: %qupcase(&parm)): %superq(&parm.&z) is not a valid value;
                        %put ERROR: (Model &z: %qupcase(&parm)): Possible values for Competing Risks method are &ciflist;
                        %let nerror=%eval(&nerror+1);
                    %end;
                %end;
            %end;
        %end;
    %mend;
    /**Method**/
    %_parmcheck(method,KM|CIF) 
    /**Y axis Type**/
    %_parmcheck(ytype,PPT|PCT)    
    /**Risk List Location**/
    %_parmcheck(risklocation,|BOTTOM|INSIDE|TIMELIST)
    /**Inside Risk List Dividor Line On/Off Option**/
    %_parmcheck(riskdivider,0|1)
    /**Class Descending Order**/
    %_parmcheck(desc,0|1)
    /**Set possible default differences between KM and CIF methods**/
    %local z;
    %do z = 1 %to &nmodels;
        %if %sysevalf(%qupcase(%superq(method&z))=KM) %then %do;
            %if %sysevalf(%superq(plotpval&z)=,boolean) %then %let plotpval&z=LOGRANK;
            %if %sysevalf(%superq(plotpvalmv&z)=,boolean) %then %let plotpvalmv&z=LR;
        %end;
        %else %if %sysevalf(%qupcase(%superq(method&z))=CIF) %then %do;
            %if %sysevalf(%superq(plotpval&z)=,boolean) %then %let plotpval&z=GRAY;
            %else %if %sysevalf(%qupcase(%superq(plotpval&z))=WALD,boolean) and &sysver < 9.4 %then %do;
                %put ERROR: (Model &z: PLOTPVAL): Wald p-value unavailable in competing risks analysis when SAS version is less than 9.4;
                %put ERROR: (Model &z: PLOTPVAL): Please use GRAY for Grays test for equality of cumulative incidence functions instead;
                %let nerror=%eval(&nerror+1);
            %end;
            %if %sysevalf(%superq(plotpvalmv&z)=,boolean) and &sysver >= 9.4 %then %let plotpvalmv&z=WALD;
            %else %if %sysevalf(%qupcase(%superq(plotpvalmv&z))=WALD,boolean) and &sysver < 9.4 %then %do;
                %put ERROR: (Model &z: PLOTPVALMV): Adjusted Wald p-value unavailable in competing risks analysis when SAS version is less than 9.4;
                %let nerror=%eval(&nerror+1);
            %end;
        %end; 
    %end; 
    /**Confidence Interval Type**/
    %_parmcheck(conftype,,LOG|ASINSQRT|LOGLOG|LINEAR|LOGIT,LOG|ASINSQRT|LOGLOG|LINEAR|LOGIT)
    /**CIF Variance Calculation Method**/
    %_parmcheck(cifvar,COUNT|DELTA,COUNT|DELTA)
    /**Plot P-Value**/
    %_parmcheck(plotpval,,SCORE|LR|LOGRANK|WILCOXON|WALD|%str( ),WALD|GRAY|%str( ))
    /**Adjusted Plot P-Value**/
    %_parmcheck(plotpvalmv,,SCORE|LR|WALD|%str( ),WALD|%str( ))
    /**S-Reverse Options**/  
    %_parmcheck(increase,,0|1,0)
    /**Censor Values On/Off Option**/
    %_parmcheck(censormarkers,0|1)
    /**Class Value Align Option**/
    %_parmcheck(classvalalign,LEFT|CENTER|RIGHT)
    /**Title Align Option**/
    %_parmcheck(titlealign,LEFT|CENTER|RIGHT)
    /**Foot Note Align Option**/
    %_parmcheck(footnotealign,LEFT|CENTER|RIGHT)
    /**X-axis tick value weight Option**/
    %_parmcheck(xtickvalweight,NORMAL|BOLD)
    /**Y-axis tick value weight Option**/
    %_parmcheck(ytickvalweight,NORMAL|BOLD)
    /**Location Options**/
    %_parmcheck(location,INSIDE|OUTSIDE)
    /**Label weight Option**/
    %_parmcheck(lweight,NORMAL|BOLD)
    /**Title weight Option**/
    %_parmcheck(tweight,NORMAL|BOLD)
    /**Footnote weight Option**/
    %_parmcheck(fnweight,NORMAL|BOLD)
    /**Patients-at-Risk weight Option**/
    %_parmcheck(parweight,NORMAL|BOLD)
    /**Hazard Ratio Ties Method Option**/
    %_parmcheck(ties,BRESLOW|DISCRETE|EFRON|EXACT)
    /**Risk Table Label Location Option**/
    %_parmcheck(risklabellocation,LEFT|ABOVE|)
    /**Risk Table Label Alignment Option**/
    %_parmcheck(risklabelalign,LEFT|CENTER|RIGHT)
    /**Risk Table Patients-at-Risk Subheader Alignment Option**/
    %_parmcheck(paralign,LEFT|CENTER|RIGHT|LABELS)
    /**Risk Table Label weight Option**/
    %_parmcheck(risklabelweight,NORMAL|BOLD)
    /**Risk Numbers and Colors Option**/
    %_parmcheck(riskcolor,0|1)
    /**List Time-points in Plot Summary Table Option**/
    %_parmcheck(listtimepoints,0|1)
    /**Color Statistics in Plot Summary Table Option**/
    %_parmcheck(statcolor,0|1)
    /**Plot Confidence Intervals Enabled**/
    %_parmcheck(plotci,0|1|2)
    /**Plot Confidence Intervals Background Fill Enabled**/
    %_parmcheck(plotcifill,0|1)
    /**Plot X Reference Line Location**/
    %_parmcheck(reflines,|TIMEPOINTS|MEDIANS)
    /**Plot Reference Line Method**/
    %_parmcheck(reflinemethod,FULL|DROP)
    /**Plot Reference Line Axis**/
    %_parmcheck(reflineaxis,X|Y|BOTH)
    
    /**Auto Align Options**/
    %local _z _z2 _test;
    %do z = 1 %to &nmodels;
        /**Check for missing values**/
        %if %sysevalf(%superq(autoalign&z)=,boolean)=0 %then %do _z2=1 %to %sysfunc(countw(%superq(autoalign&z),%str( )));
            /**Check all given values against the possible allowed values**/
            %let _test=;
            %do _z = 1 %to %sysfunc(countw(TOPLEFT|TOP|TOPRIGHT|LEFT|CENTER|RIGHT|BOTTOMLEFT|BOTTOM|BOTTOMRIGHT,|));
                %if %qupcase(%scan(%superq(autoalign&z),&_z2,%str( )))=%scan(TOPLEFT|TOP|TOPRIGHT|LEFT|CENTER|RIGHT|BOTTOMLEFT|BOTTOM|BOTTOMRIGHT,&_z,|,m) %then %let _test=1;
            %end;
            /**If any values are not in the possible list then throw an error**/
            %if &_test ^= 1 %then %do;
                %put ERROR: (Model &z: %qupcase(autoalign)): %qupcase(%scan(%superq(autoalign&z),&_z2,%str( ))) is not in the list of valid values;
                %put ERROR: (Model &z: %qupcase(autoalign)): Possible values are TOPLEFT|TOP|TOPRIGHT|LEFT|CENTER|RIGHT|BOTTOMLEFT|BOTTOM|BOTTOMRIGHT;
                %let nerror=%eval(&nerror+1);
            %end;
        %end;
        %else %do;
            /**If missing then show error**/
            %put ERROR: (Model &z: %qupcase(autoalign)): Cannot be missing;
            %put ERROR: (Model &z: %qupcase(autoalign)): Possible values are TOPLEFT|TOP|TOPRIGHT|LEFT|CENTER|RIGHT|BOTTOMLEFT|BOTTOM|BOTTOMRIGHT;
            %let nerror=%eval(&nerror+1);
        %end;           
    %end;
    /**Plot Display Variables**/
    %local _z _z2 _test _displaylist;
    %do z = 1 %to &nmodels;
        %if %sysevalf(%qupcase(%superq(display&z))=STANDARD,boolean) %then %do;
            %if %sysevalf(%qupcase(%superq(method&z))=KM,boolean) and %sysevalf(%superq(class&z)^=,boolean) %then %let display&z=legend ev_n median hr timelist pval tablecomments;
            %else %if %sysevalf(%qupcase(%superq(method&z))=KM,boolean) and %sysevalf(%superq(class&z)=,boolean) %then %let display&z=ev_n median timelist tablecomments;
            %else %if %sysevalf(%qupcase(%superq(method&z))=CIF,boolean) and &sysver < 9.4 %then %let display&z=legend total event median timelist pval tablecomments;
            %else %if %sysevalf(%qupcase(%superq(method&z))=CIF,boolean) and &sysver >= 9.4 %then %let display&z=legend total event median hr timelist pval tablecomments;
        %end;
        %if %sysevalf(%qupcase(%superq(method&z))=KM,boolean) %then 
            %let _displaylist=legend|hr|median|total|event|ev_n|n_ev|timelist|pval|tablecomments|totalmv|eventmv|ev_nmv|n_evmv|hrmv|pvalmv|covpval|covpvalmv;
        %else %if %sysevalf(%qupcase(%superq(method&z))=CIF,boolean) %then %do;
            %if &sysver >= 9.4 %then %let _displaylist=legend|hr|median|total|event|ev_n|n_ev|timelist|pval|tablecomments|totalmv|eventmv|ev_nmv|n_evmv|hrmv|pvalmv|covpval|covpvalmv;
            %else %let _displaylist=legend|median|total|event|ev_n|n_ev|timelist|pval|tablecomments;
        %end;
        /**Check for missing values**/
        %if %sysevalf(%superq(display&z)=,boolean)=0 %then %do _z2=1 %to %sysfunc(countw(%superq(display&z),%str( )));
            /**Check all given values against the possible allowed values**/
            %let _test=;
            %do _z = 1 %to %sysfunc(countw(&_displaylist,|));
                %if %qupcase(%scan(%superq(display&z),&_z2,%str( )))=%scan(%qupcase(&_displaylist),&_z,|,m) %then %let _test=1;
                %end;
            /**If any values are not in the possible list then throw an error**/
            %if &_test ^= 1 %then %do;
                %put ERROR: (Model &z: %qupcase(display)): %qupcase(%scan(%superq(display&z),&_z2,%str( ))) is not in the list of valid values;
                %put ERROR: (Model &z: %qupcase(display)): Possible values are %qupcase(&_displaylist);
                %if %sysevalf(%qupcase(%superq(method&z))=CIF,boolean) and &sysver lt 9.4 %then 
                    %put ERROR: (Model &z: %qupcase(display)): Hazard ratios for competing risks are only available in SAS 9.4 or later;
                %let nerror=%eval(&nerror+1);
                %end;
            %end;
        %end;
    
    /**Line Pattern Variables**/
    %macro _linepattern(var=,_patternlist=AUTO|SOLID|SHORTDASH|MEDIUMDASH|LONGDASH|MEDIUMDASHSHORTDASH|
        DASHDASHDOT|DASH|LONGDASHSHORTDASH|DOT|THINDOT|SHORTDASHDOT|MEDIUMDASHDOTDOT);
        %local _z _z2 _test;
        %do z = 1 %to &nmodels;
            /**Check for missing values**/
            %if %sysevalf(%superq(&var.&z)=,boolean)=0 %then %do _z2=1 %to %sysfunc(countw(%superq(&var.&z),%str( )));
                %let _test=;
                /**Check if values are either in the approved list, or are between 1 and 46**/
                %if %sysfunc(notdigit(%scan(%superq(&var.&z),&_z2,%str( ))))>0 %then %do _z = 1 %to %sysfunc(countw(&_patternlist,|));
                    %if %qupcase(%scan(%superq(&var.&z),&_z2,%str( )))=%scan(%qupcase(%sysfunc(compress(&_patternlist))),&_z,|,m) %then %let _test=1;
                %end;
                %else %if %scan(%superq(&var.&z),&_z2,%str( )) ge 1 and %scan(%superq(&var.&z),&_z2,%str( )) le 46 %then %let _test=1;
                %if &_test ^= 1 %then %do;
                    /**Throw error**/
                    %put ERROR: (Model &z: %qupcase(&var.)): %qupcase(%scan(%superq(&var.&z),&_z2,%str( ))) is not in the list of valid values;
                    %put ERROR: (Model &z: %qupcase(&var.)): Possible values are %qupcase(&_patternlist) or Numbers Between 1 and 46;
                    %let nerror=%eval(&nerror+1);
                %end;
            %end;
            %else %do;
                /**Throw error**/
                %put ERROR: (Model &z: %qupcase(&var.)): %qupcase(%superq(&var.&z)) is not in the list of valid values;         
                %put ERROR: (Model &z: %qupcase(&var.)): Possible values are %qupcase(&_patternlist) or Numbers Between 1 and 46;
                %let nerror=%eval(&nerror+1);       
            %end;
        %end;
    %mend;
    /**Plot Line Patterns**/
    %_linepattern(var=pattern)
    /**Patients-at-Risk INSIDE option Dividor Line Style**/
    %_linepattern(var=riskdivstyle)
    /**Plot Confidence Bounds Confidence Interval**/
    %_linepattern(var=plotcilinepattern)
    /**Plot X Referenceline line style**/
    %_linepattern(var=reflinepattern)
        
    /**Range Value Check**/
    %macro _rangecheck(var=,min=,max=,incmax=,incmin=);
        %do z = 1 %to &nmodels;
            /**Check for missing values**/
            %if %sysevalf(%superq(&var.&z)=,boolean)=0 %then %do;
                %if %sysfunc(notdigit(%sysfunc(compress(%superq(&var.&z),-.)))) > 0 %then %do;
                    /**Checks for character values**/
                    %put ERROR: (Model &z: %qupcase(&var.)) Must be numeric. %qupcase(%superq(&var.&z)) is not valid.;
                    %let nerror=%eval(&nerror+1);
                %end;
                %else %if %superq(&var.&z) le &min and &incmin=0 %then %do;
                    /**Checks if less than or equal to min**/
                    %put ERROR: (Model &z: %qupcase(&var.)) Cannot be less than or equal to &min (%superq(&var.&z));
                    %let nerror=%eval(&nerror+1);
                %end;
                %else %if %superq(&var.&z) lt &min %then %do;
                    /**Checks if less than min**/
                    %put ERROR: (Model &z: %qupcase(&var.)) Cannot be less than &min (%superq(&var.&z));
                    %let nerror=%eval(&nerror+1);
                %end;
                %else %if %superq(&var.&z) ge &max and &incmax=0 %then %do;
                    /**Checks if greater than or equal to max**/
                    %put ERROR: (Model &z: %qupcase(&var.)) Cannot be Greater Than or Equal to &max;
                    %let nerror=%eval(&nerror+1);
                %end;
                %else %if %superq(&var.&z) gt &max %then %do;
                    /**Checks if greater than max**/
                    %put ERROR: (Model &z: %qupcase(&var.)) Cannot be Greater Than &max;
                    %let nerror=%eval(&nerror+1);
                %end;
            %end;
        %end; 
    %mend; 
    /**X Axis Minimum offset Value**/
    %_rangecheck(var=xminoffset,min=0,max=1,incmax=0,incmin=1)
    /**Y Axis Minimum offset Value**/
    %_rangecheck(var=yminoffset,min=0,max=1,incmax=0,incmin=1)
    /**X Axis Maximum offset Value**/
    %_rangecheck(var=xmaxoffset,min=0,max=1,incmax=0,incmin=1)
    /**Y Axis Maximum offset Value**/
    %_rangecheck(var=ymaxoffset,min=0,max=1,incmax=0,incmin=1)
    /**Plot Confidence Intervals transparency**/
    %_rangecheck(var=plotcifilltransparency,min=0,max=1,incmax=1,incmin=1)
    
    /**Y Axis Maximum Value**/
    %do z = 1 %to &nmodels;
        /**Check for missing values**/
        %if %sysevalf(%superq(ymax&z)=,boolean)=0 %then %do;
            %if %sysfunc(notdigit(%sysfunc(compress(%superq(ymax&z),-.)))) > 0 %then %do;
                /**Checks for character values**/
                %put ERROR: (Model &z: %qupcase(ymax)) Must be numeric.  %qupcase(%superq(ymax&z)) is not valid.;
                %let nerror=%eval(&nerror+1);
            %end;
            %else %if %superq(ymax&z) le %superq(ymin&z) %then %do;
                /**Makes sure the maximum is not less than the minimum**/
                %put ERROR: (Model &z: %qupcase(ymax)) Cannot be less than or equal to YMIN (%superq(ymax&z) vs. %superq(ymin&z));
                %let nerror=%eval(&nerror+1);
            %end;
            %else %if (%superq(ymax&z) gt 100 and %qupcase(%superq(ytype&z))=PCT) or 
                (%superq(ymax&z) gt 1 and %qupcase(%superq(ytype&z))=PPT) %then %do;
                /**Makes sure the maximum cannot be greater than the maximum survival estimate**/
                %if %qupcase(%superq(ytype))=PPT %then %do;
                    %put ERROR: (Model &z: %qupcase(ymax)) Cannot be Greater Than 1 When YTYPE=PPT;
                    %let nerror=%eval(&nerror+1);
                %end;
                %else %if %qupcase(%superq(ytype))=PCT %then %do;
                    %put ERROR: (Model &z: %qupcase(ymax)) Cannot be Greater Than 100 When YTYPE=PCT;
                    %let nerror=%eval(&nerror+1);
                %end;
            %end;
        %end;
        %else %if %qupcase(%superq(ytype&z))=PPT %then %let ymax&z=1;
        %else %if %qupcase(%superq(ytype&z))=PCT %then %let ymax&z=100;
    %end;
    
    /**Y Axis Increment Value**/
    %do z = 1 %to &nmodels;
        /**Check for missing values**/
        %if %sysevalf(%superq(yincrement&z)=,boolean)=0 %then %do;
            %if %sysfunc(notdigit(%sysfunc(compress(%superq(yincrement&z),-.)))) > 0 %then %do;
                /**Checks for character values**/
                %put ERROR: (Model &z: %qupcase(yincrement)) Must be numeric.  %qupcase(%superq(yincrement&z)) is not valid.;
                %let nerror=%eval(&nerror+1);
            %end;
            %else %if %superq(yincrement&z) gt %sysevalf(%superq(ymax&z)-%superq(ymin&z)) %then %do;                    
                /**Makes sure the increment is not greater than the distance between max and min**/
                %put ERROR: (Model &z: %qupcase(yincrement)) Cannot be less than or equal to difference between YMAX and YMIN (%superq(yincrement&z) vs. %sysfunc(sum(%superq(ymax&z),-%superq(ymin&z))));
                %let nerror=%eval(&nerror+1);
            %end;
            %else %if %superq(yincrement&z) le 0 %then %do;
                /**Makes sure the increment is greater than zero**/
                %put ERROR: (Model &z: %qupcase(yincrement)) Cannot be less than or equal to 0;
                %let nerror=%eval(&nerror+1);
            %end;
        %end;
        %else %if %qupcase(%superq(ytype&z))=PPT %then %let yincrement&z=0.1;
        %else %if %qupcase(%superq(ytype&z))=PCT %then %let yincrement&z=10;
    %end;
            
    /**Error Handling on Global Parameters**/
    %macro _gparmcheck(parm, parmlist);          
        %local _test _z;
        /**Check if values are in approved list**/
        %do _z=1 %to %sysfunc(countw(&parmlist,|,m));
            %if %qupcase(%superq(&parm))=%qupcase(%scan(&parmlist,&_z,|,m)) %then %let _test=1;
        %end;
        %if &_test ^= 1 %then %do;
            /**If not then throw error**/
            %put ERROR: (Global: %qupcase(&parm)): %superq(&parm) is not a valid value;
            %put ERROR: (Global: %qupcase(&parm)): Possible values are &parmlist;
            %let nerror=%eval(&nerror+1);
        %end;
    %mend;
    /**Plot On/Off Options**/
    %_gparmcheck(plot,0|1)
    /**Summary On/Off Options**/
    %_gparmcheck(summary,0|1)
    /**Plot Wall On/Off Options**/
    %_gparmcheck(showwalls,0|1)
    /**New Table On/Off Options**/
    %_gparmcheck(newtable,0|1)
    /**Lattice Order Options**/
    %_gparmcheck(order,COLUMNMAJOR|ROWMAJOR)
    /**Destination Options**/
    %_gparmcheck(tablefmt,RTF|PDF|HTML)
    /**Overall Title weight Option**/
    %_gparmcheck(ovtweight,NORMAL|BOLD)
    /**Overall Footnote weight Option**/
    %_gparmcheck(ovfnweight,NORMAL|BOLD)
    /**Table Header weight Option**/
    %_gparmcheck(tableheaderweight,MEDIUM|BOLD)
    /**Table Footnote weight Option**/
    %_gparmcheck(tablefootnoteweight,MEDIUM|BOLD)
    /**Table Data Columns weight Option**/
    %_gparmcheck(tabledataweight,MEDIUM|BOLD)
    /**Overall Title Align Option**/
    %_gparmcheck(ovtitlealign,LEFT|CENTER|RIGHT)
    /**Overall Foot Note Align Option**/
    %_gparmcheck(ovfootnotealign,LEFT|CENTER|RIGHT) 
    /**Border around plot image Option**/
    %_gparmcheck(border,0|1)
    /**Table Background Shading**/
    %_gparmcheck(tableshading,0|1)
    /**Uniform Height below X-axis**/
    %_gparmcheck(uniformheight,0|1)
    /**Merge covariate p-values into overall p-value column**/
    %_gparmcheck(tablemergepval,0|1)
    /*Tiff Device Check*/
    %if %sysevalf(%qupcase(&plotfmt)=TIFF,boolean) or  %sysevalf(%qupcase(&plotfmt)=TIF,boolean) %then %do;
        ods output gdevice=_gdevice;
        proc gdevice catalog=sashelp.devices nofs;
            list _all_;
        run;
        quit;
        %global _tifflist _tiffcheck;
        proc sql noprint;
            select 1 into :_tiffcheck from _gdevice where upcase(name)=upcase("&tiffdevice");
            select distinct upcase(name) into :_tifflist separated by '|' from _gdevice
                where substr(upcase(name),1,3)='TIF';
            %if %sysevalf(%superq(_tiffcheck)^=1,boolean) %then %do;
                /**If not then throw error**/
                %put ERROR: (Global: TIFFDEVICE): %qupcase(%superq(tiffdevice)) is not on the installed list of devices;
                %put ERROR: (Global: TIFFDEVICE): Please select from the following list &_tifflist;
                %let nerror=%eval(&nerror+1);
            %end;
            drop table _gdevice;
        quit;
    %end;
    /**Scalable Vector Graphics On/Off Options**/
    %if &sysver ge 9.3 %then %_gparmcheck(svg,0|1);
    %else %if &svg=1 %then %do;
        /**Throw error**/
        %put ERROR: (Global: %qupcase(svg)): SVG cannot be set to 1 when SAS version less than 9.3 (&sysver);
        %let nerror=%eval(&nerror+1);
    %end;
    
    /**Error Handling on Global Parameters Involving units**/
    %macro _gunitcheck(parm);
        %if %sysevalf(%superq(&parm)=,boolean)=1 %then %do;
            /**Check if missing**/
            %put ERROR: (Global: %qupcase(&parm)) Cannot be set to missing;
            %let nerror=%eval(&nerror+1);
        %end;
        %else %if %sysfunc(compress(%superq(&parm),ABCDEFGHIJKLMNOPQRSTUVWXYZ,i)) lt 0 %then %do;
            /**Throw error**/
            %put ERROR: (Global: %qupcase(&parm)) Cannot be less than zero (%qupcase(%superq(&parm)));
            %let nerror=%eval(&nerror+1);
        %end;
    %mend;
    /**Overall Title Font Size**/
    %_gunitcheck(ovtsize)
    /**Overall Footnote Font Size**/
    %_gunitcheck(ovfnsize)
    /**Plot Width**/
    %_gunitcheck(width)
    %if %sysevalf(%qupcase(%superq(plotfmt))=TIFF,boolean) or
        %sysevalf(%qupcase(%superq(plotfmt))=TIF,boolean) %then %do;
        %if %sysfunc(find(%superq(width),px,i))>0 %then %do;
            /**Throw error**/
            %put ERROR: (Global: WIDTH) Must use units of IN when PLOTFMT=%qupcase(&plotfmt);
            %let nerror=%eval(&nerror+1);
        %end;
    %end;
    /**Plot Height**/
    %_gunitcheck(height)
    %if %sysevalf(%qupcase(%superq(plotfmt))=TIFF,boolean) or
        %sysevalf(%qupcase(%superq(plotfmt))=TIF,boolean) %then %do;
        %if %sysfunc(find(%superq(height),px,i))>0 %then %do;
            /**Throw error**/
            %put ERROR: (Global: HEIGHT) Must use units of IN when PLOTFMT=%qupcase(&plotfmt);
            %let nerror=%eval(&nerror+1);
        %end;
    %end;
    /**Table Header Font Size**/
    %_gunitcheck(tableheadersize)
    /**Table Footnote Font Size**/
    %_gunitcheck(tablefootnotesize)
    /**Table Data Columns Font Size**/
    %_gunitcheck(tabledatasize)
    /**Table Total Count Column Width**/
    %_gunitcheck(ttotalwidth)
    /**Table Events Count Column Width**/
    %_gunitcheck(teventwidth)
    /**Table Combined Total Counts and Events Count Column Width**/
    %_gunitcheck(tev_nwidth)
    /**Table Median Column Width**/
    %_gunitcheck(tmedianwidth)
    /**Table Hazard Ratio Column Width**/
    %_gunitcheck(thrwidth)
    /**Table Time point estimates Column Width**/
    %_gunitcheck(ttimelistwidth)
    /**Table P-value Column Width**/
    %_gunitcheck(tpvalwidth)
    
    /**Error Handling on Individual Model Numeric Variables**/
    %macro _gnumcheck(parm, min);
        /**Check if missing**/
        %if %sysevalf(%superq(&parm)^=,boolean) %then %do;
            %if %sysfunc(notdigit(%sysfunc(compress(%superq(&parm),-.)))) > 0 %then %do;
                /**Check if character value**/
                %put ERROR: (Global: %qupcase(&parm)) Must be numeric.  %qupcase(%superq(&parm)) is not valid.;
                %let nerror=%eval(&nerror+1);
            %end;
            %else %if %superq(&parm) < &min %then %do;
                /**Makes sure number is not less than the minimum**/
                %put ERROR: (Global: %qupcase(&parm)) Must be greater than %superq(min). %qupcase(%superq(&parm)) is not valid.;
                %let nerror=%eval(&nerror+1);
            %end;
        %end;
        %else %do;
            /**Throw Error**/
            %put ERROR: (Global: %qupcase(&parm)) Cannot be missing;
            %put ERROR: (Global: %qupcase(&parm)) Possible values are numeric values greater than or equal to %superq(min);
            %let nerror=%eval(&nerror+1);           
        %end;       
    %mend;
    /**Digital Pixels per Inch Value**/
    %_gnumcheck(dpi,100)
    /**Anti-alias Maximum Value**/
    %_gnumcheck(antialiasmax,1)
    /**Number of Lattice Columns**/
    %_gnumcheck(columns,1)
    /**Number of Lattice Rows**/
    %_gnumcheck(rows,1)
    /**Number of Models**/
    %_gnumcheck(nmodels,1)
    %if &plot=1 and &nerror=0 %then %do;
        %if %sysevalf(&rows*&columns < &nmodels,boolean) %then %do;
            /**Throw Error**/
            %put ERROR: (Global: NMODELS) if PLOT=1 then NMODELS (&nmodels) must be less than or equal to ROWS*COLUMNS (%sysevalf(&rows*&columns));
            %let nerror=%eval(&nerror+1);
        %end;
    %end;
    
    /**Summary Table Display Variables**/
    %local _z2 _test;
    /**Check if missing**/
    %if %sysevalf(%superq(tabledisplay)=,boolean)=0 %then %do _z2=1 %to %sysfunc(countw(%superq(tabledisplay),%str( )));
        %let _test=;
        /**Check if submitted values are in approved list**/
        %do _z = 1 %to %sysfunc(countw(%qupcase(title|footnote|total|event|ev_n|median|hr|timelist|pval|totalmv|eventmv|
                                                ev_nmv|hrmv|pvalmv|covpval|covpvalmv),|));
            %if %qupcase(%scan(%superq(tabledisplay),&_z2,%str( )))=
                %scan(%qupcase(title|footnote|total|event|ev_n|median|hr|timelist|pval|totalmv|
                               eventmv|ev_nmv|hrmv|pvalmv|covpval|covpvalmv),&_z,|,m) %then %let _test=1;
        %end;
        %if &_test ^= 1 %then %do;
            /**Throw errors**/
            %put ERROR: (Global: %qupcase(tabledisplay)): %qupcase(%scan(%superq(tabledisplay),&_z2,%str( ))) is not in the list of valid values;
            %put ERROR: (Global: %qupcase(tabledisplay)): Possible values are %qupcase(title|footnote|total|event|ev_n|median|hr|timelist|pval|totalmv|eventmv|ev_nmv|hrmv|pvalmv|covpval|covpvalmv);
            %let nerror=%eval(&nerror+1);
        %end;
    %end;
    /**If confidence interval colors are missing, set them to line colors**/
    %local z;
    %do z = 1 %to &nmodels;
        %if %sysevalf(%superq(plotcifillcolor&z)=,boolean) %then %let plotcifillcolor&z=%superq(color&z);
        %if %sysevalf(%superq(plotcilinecolor&z)=,boolean) %then %let plotcilinecolor&z=%superq(color&z);
    %end;
    /*** If any errors exist, stop macro and send to end ***/
    %if &nerror > 0 %then %do;
        %put ERROR: &nerror pre-run errors listed;
        %put ERROR: Macro NEWSURV will cease;
        %goto errhandl;
    %end;    
    
    %do z = 1 %to &nmodels;/**Start of Analysis Section**/
        %local nerror_run;
        %let nerror_run=0;
        data _null_;
            set &data;
            if _n_=1 then do;
            /**If missing x label, grab label or name of time variable**/
            %if %sysevalf(%superq(xlabel&z)=,boolean) %then %do;
                call symput("xlabel&z",vlabel(%superq(time&z)));
            %end;
            %if %sysevalf(%superq(class&z)=,boolean)=0 %then %do;
                %local format&z label&z;            
                /**Selects format for class variable**/
                call symput("label&z",vlabel(%superq(class&z)));
                call symput("format&z",vformat(%superq(class&z)));
            %end;               
            end;
        run;
        /**If missing y label, make generic label depending on y axis type**/
        %if %sysevalf(%superq(ylabel&z)=,boolean)=1 %then %do;
            %if %qupcase(%superq(ytype&z))=PCT %then %do;
                %if %qupcase(%superq(method&z))=CIF or %superq(increase&z)=1 %then %let ylabel&z=Percent With Event;
                %else %let ylabel&z=Percent Without Event;
            %end;
            %else %do;
                %if %qupcase(%superq(method&z))=CIF or %superq(increase&z)=1 %then %let ylabel&z=Proportion With Event;
                %else %let ylabel&z=Proportion Without Event; 
            %end;               
        %end;
        proc sql;
            reset noprint;    
            /**Check for class variable**/ 
            %if %sysevalf(%superq(class&z)=,boolean)=0 %then %do;
                /**Create temporary dataset for analysis**/
                /**Changes class variable to character variable by applying format**/
                /**Makes sure there are no missing values in the key variables**/
                /**Apply where clause**/
                create table _tempdsn&z (rename=(%superq(class&z)=_tempvar_
                                                 %superq(time&z)=_time_ %superq(cens&z)=_cens_ 
                                                 %do i=1 %to %sysfunc(countw(%superq(classcov&z),%str( ))); 
                                                    %scan(%superq(classcov&z),&i,%str( ))=_classcov_&i
                                                 %end;
                                                 %do i=1 %to %sysfunc(countw(%superq(contcov&z),%str( ))); 
                                                     %scan(%superq(contcov&z),&i,%str( ))=_contcov_&i
                                                 %end;                                               
                                                 %do i=1 %to %sysfunc(countw(%superq(strata&z),%str( ))); 
                                                    %scan(%superq(strata&z),&i,%str( ))=_strata_&i
                                                 %end;
                                                 %if %sysfunc(notdigit(0%sysfunc(compress(%superq(landmark&z),.-)))) > 0 %then %do;
                                                    %superq(landmark&z)=_landmark_
                                                 %end;)
                                                 keep = %superq(class&z) %superq(time&z) %superq(cens&z) %superq(classcov&z)
                                                        %superq(contcov&z) %superq(strata&z)
                                                 %if %sysfunc(notdigit(0%sysfunc(compress(%superq(landmark&z),.-)))) > 0 %then %do;
                                                    %superq(landmark&z)
                                                 %end;) as
                    select *
                    from &data (
                    where=(^missing(%superq(time&z)) and ^missing(%superq(cens&z)) 
                           and ^missing(%superq(class&z)) 
                    %if %sysfunc(notdigit(0%sysfunc(compress(%superq(landmark&z),.-)))) > 0 %then %do;
                        and ^missing(%superq(landmark&z))
                    %end;
                    %if %sysevalf(%superq(strata&z)=,boolean)=0 %then %do i = 1 %to %sysfunc(countw(%superq(strata&z),%str( )));/**Not missing stratification variables**/
                        and ^missing(%scan(%superq(strata&z),&i,%str( )))
                    %end; 
                    %if %sysevalf(%superq(where&z)=,boolean)=0 %then %do;
                        and &&where&z
                    %end;));
                alter table _tempdsn&z
                    add _class_ char(300);
                update _tempdsn&z
                    set _class_=strip(put(_tempvar_,%superq(format&z)));
                alter table _tempdsn&z
                    drop _tempvar_;
                
                %if %sysevalf(%superq(xdivisor&z)=,boolean)=0 %then %do;
                    update _tempdsn&z
                        set _time_=_time_/%superq(xdivisor&z);
                %end;
                /**Check if the provided class reference value is in the data**/
                %if %sysevalf(%superq(classref&z)=,boolean)=0 %then %do;
                    %local _classrefcheck;
                    %let _classrefcheck=;
                    select distinct 1 into :_classrefcheck
                        from _tempdsn&z
                        where strip(_class_)=strip("%superq(classref&z)");
                    %if %superq(_classrefcheck) ^=1 %then %do;
                        /**If not in the dataset then throw error**/
                        %put ERROR: (Model &z: CLASSREF): Indicated class reference value (%superq(classref&z)) does not exist in dataset (%superq(data));
                        %put ERROR: (Model &z: CLASSREF): Class reference value must be an existing value with same case as value in dataset;
                        %let nerror_run=%eval(&nerror_run+1);
                    %end;
                %end;  
            %end;
            %else %do;
                /**Get rid of missing values from key variables**/
                /**Apply where clause**/
                create table _tempdsn&z (rename=(%superq(time&z)=_time_ %superq(cens&z)=_cens_
                                                 %if %sysfunc(notdigit(0%sysfunc(compress(%superq(landmark&z),.-)))) > 0 %then %do;
                                                    %superq(landmark&z)=_landmark_
                                                 %end;)
                                                 keep = %superq(time&z) %superq(cens&z) 
                                                 %if %sysfunc(notdigit(0%sysfunc(compress(%superq(landmark&z),.-)))) > 0 %then %do;
                                                    %superq(landmark&z)
                                                 %end;) as
                select * from &data (where=(^missing(%superq(time&z)) and ^missing(%superq(cens&z))
                %if %sysfunc(notdigit(0%sysfunc(compress(%superq(landmark&z),.-)))) > 0 %then %do;
                    and ^missing(%superq(landmark&z))
                %end;
                %if %sysevalf(%superq(where&z)=,boolean)=0 %then %do;
                    and &&where&z
                %end;));
                %if %sysevalf(%superq(xdivisor&z)=,boolean)=0 %then %do;
                    update _tempdsn&z
                        set _time_=_time_/%superq(xdivisor&z);
                %end;                    
            %end;
            /**Apply landmark if not missing**/
            %if %sysevalf(%superq(landmark&z)=,boolean)=0 %then %do;
                update _tempdsn&z
                    set _time_=_time_-
                    %if %sysfunc(notdigit(0%sysfunc(compress(%superq(landmark&z),.-)))) > 0 %then %do;
                        _landmark_
                    %end;
                    %else %do;
                        %superq(landmark&z)
                    %end;;
                delete from _tempdsn&z
                    where _time_ le 0;
            %end;
            /*** X-axis Maximum ***/
            %local _maxt;
            /**Select greatest time value**/
            select max(_time_) into :_maxt
                from _tempdsn&z;
            /**Check if missing**/
            %if %sysevalf(%superq(xmax&z)=,boolean)=1 %then %do;
                /**Set x-maximum to maximum time rounded to next number divisible by 5**/
                %let xmax&z=%sysfunc(sum(%sysfunc(ceil(%superq(_maxt))),5,-%sysfunc(mod(%sysfunc(ceil(%superq(_maxt)-%superq(xmin&z))),5))));
            %end;
            %else %if %sysfunc(notdigit(%sysfunc(compress(%superq(xmax&z),.-)))) > 0 %then %do;
                /**If character value then throw error**/
                %put ERROR: (Model &z: XMAX) Must be a numeric value (%qupcase(%superq(xmax&z)));
                %let nerror_run=%eval(&nerror_run+1);
            %end;               
            %else %if %sysevalf(%superq(xmax&z) le %superq(xmin&z),boolean) %then %do;
                /**Make sure maximum is not less or equal to than minimum**/
                %put ERROR: (Model &z: XMAX) Cannot be less than or equal to XMIN (%superq(xmax&z) vs. %superq(xmin&z));
                %let nerror_run=%eval(&nerror_run+1);                   
            %end;
            /*** X-axis Incrementation ***/
            %if %sysevalf(%superq(xincrement&z)=,boolean)=1 %then %do;
                /**If missing then set increment to be range cut into 5 pieces**/
                %let xincrement&z=%sysfunc(sum((%superq(xmax&z)-%superq(xmin&z))/5));
            %end;
            %else %if %sysfunc(notdigit(%sysfunc(compress(%superq(xincrement&z),.-)))) > 0 %then %do;
                /**If character value then throw error**/
                %put ERROR: (Model &z: XINCREMENT) Must be a numeric value (%qupcase(%superq(xincrement&z)));
                %let nerror_run=%eval(&nerror_run+1);
            %end;
            %else %if %sysevalf(%superq(xincrement&z) gt (%sysevalf(%superq(xmax&z)-%superq(xmin&z))),boolean) %then %do;
                /**Make sure increment cannot be greater than range**/
                %put ERROR: (Model &z: XINCREMENT) Cannot be greater than the difference between XMAX and XMIN (%superq(xincrement&z) vs. %sysfunc(sum(%superq(xmax&z),-%superq(xmin&z))));                 
                %let nerror_run=%eval(&nerror_run+1);
            %end;
        quit;  
        %if &nerror_run > 0 %then %goto errhandl2; 
          
        /**Assign multiplicative constant for y-axis**/
        %local xmult_&z tfmt_&z;
        %if %qupcase(%superq(ytype&z))=PCT %then %let xmult_&z=100;
        %else %if %qupcase(%superq(ytype&z))=PPT %then %let xmult_&z=1;
        /**Assign formats for y-axis**/
        %if %sysevalf(%qupcase(%superq(kmestdigits&z))=AUTO,boolean) %then %do;
            %if %qupcase(%superq(ytype&z))=PCT %then %let tfmt_&z=5.1;
            %else %if %qupcase(%superq(ytype&z))=PPT %then %let tfmt_&z=4.2;
        %end;
        %else %let tfmt_&z=%sysevalf(12.&&kmestdigits&z);
        
        
        %local risklist_&z;
        %if %qupcase(%superq(risklocation&z))=TIMELIST %then %let risklist_&z=%superq(timelist&z);
        %else %let risklist_&z = %superq(risklist&z);
            
        %if %sysevalf(%qupcase(%superq(method&z))=KM,boolean) %then %do;
            /**Get survival times**/
            /**median, logrank test, time estimates**/
            ods graphics on;
            proc lifetest data=_tempdsn&z
                /**Set up dataset with time-point estimate numbers**/
                %if %sysevalf(%superq(timelist&z)=,boolean)=0 %then %do;
                    reduceout timelist=%sysfunc(compress(%superq(timelist&z), "'"))
                    outs=_timelist
                %end;     
                /***Set up dataset with patients-at-risk numbers***/ 
                plot=(survival(cl
                    %if %sysevalf(%superq(risklist&z)=,boolean)=0  %then %do; 
                        atrisk= %sysfunc(compress(%superq(risklist_&z), "'"))
                    %end;))     
                    /**Set confidence interval type**/ 
                    conftype=%superq(conftype&z);
                /**If class variable then split into groups**/
                %if %sysevalf(%superq(class&z)=,boolean)=0 %then %do;
                    strata _class_
                    %if %qupcase(%superq(plotpval&z))=LOGRANK and %sysevalf(%superq(strata&z)=,boolean) %then %do;
                        / logrank
                    %end;
                    %else %if %qupcase(%superq(plotpval&z))=WILCOXON and %sysevalf(%superq(strata&z)=,boolean) %then %do;
                        / wilcoxon
                    %end;;
                %end;
                /**Run Model**/
                time _time_ * _cens_(%superq(cen_vl&z));
                
                ods output censoredsummary=_sum /**Contains events/totals**/
                    quartiles=_quart (where=(percent=50)) /**Containts Medians**/
                    %if %sysevalf(%superq(class&z)=,boolean)=0 and %sysevalf(%superq(strata&z)=,boolean) %then %do;
                        homtests=_ltest /**Contains Logrank and/or Wilcoxon test**/
                    %end;
                    %if %sysevalf(%superq(risklist&z)=,boolean)=0 %then %do;
                        survivalplot=_splot
                        (keep=time tatrisk stratumnum atrisk
                        where=(tatrisk ^=.)) /**Outputs dataset with patients-at-risk numbers**/
                    %end;
                    survivalplot=_surv (where=(event>.)
                        rename=(%if %sysevalf(%superq(class&z)=,boolean)=0 %then %do; stratum=cl1 %end;
                            survival=s1 time=t1 censored=c1 sdf_lcl=lcl1 sdf_ucl=ucl1)
                        keep= survival time censored event sdf_lcl sdf_ucl
                            %if %sysevalf(%superq(class&z)=,boolean)=0 %then %do; stratum stratumnum %end;);/**Set up dataset with survival numbers**/
            run;   
            ods graphics off; 
            /**calculate stratified p-values**/
            %if %sysevalf(%superq(strata&z)^=,boolean) and %sysevalf(%superq(class&z)^=,boolean) %then %do;
                proc lifetest data=_tempdsn&z;
                    strata %do i = 1 %to %sysfunc(countw(%superq(strata&z),%str( )));_strata_&i %end; / group=_class_
                        %if %qupcase(%superq(plotpval&z))=LOGRANK %then %do;
                            test=logrank
                        %end;
                        %else %if %qupcase(%superq(plotpval&z))=WILCOXON %then %do;
                            test=wilcoxon
                        %end;;
                /**Run Model**/
                time _time_ * _cens_(%superq(cen_vl&z));                
                ods output homtests=_ltest; /**Contains Stratified Logrank and/or Wilcoxon test**/
                run;   
            %end;
            /**Causes the survival estimates to continue**/
            /**for each time value in the dataset**/
            data _surv;
                set _surv;
                if s1 = . then s1 = c1;
                drop event;
            run;  
        %end; /**End KM Method Section**/
        %else %if %sysevalf(%qupcase(%superq(method&z))=CIF,boolean) %then %do;
            %if %sysevalf(%superq(timelist&z)^=,boolean) %then %do;            
                %local _cif_timepoints_;
                /**Created from XX to XX by XX format**/
                %if %sysfunc(find(%superq(timelist&z),to,i))>0 %then %do;
                    data _list_;            
                        do i = %superq(timelist&z);
                            timepoint=i;
                            output;
                        end;
                        drop i;
                    run;
                    proc sort data=_list_;
                        by timepoint;
                    run;
                    proc sql noprint;
                        select distinct timepoint into :_cif_timepoints_ separated by ',' from _list_;
                        drop table _list_;
                    quit;
                %end;
                /**Created from numeric list**/
                %else %do i = 1 %to %sysfunc(countw(%superq(timelist&z),%str( )));
                    %if &i=1 %then %let _cif_timepoints_=%scan(%superq(timelist&z),&i,%str( ));
                    %else %let _cif_timepoints_=&_cif_timepoints_.,%scan(%superq(timelist&z),&i,%str( ));
                %end; 
            %end;
            %if %sysevalf(%superq(risklist&z)^=,boolean) %then %do;       
                %local _cif_riskpoints_;
                /**Created from XX to XX by XX format**/
                %if %sysfunc(find(%superq(risklist_&z),to,i))>0 %then %do;
                    data _list_;            
                        do i = %superq(risklist_&z);
                            riskpoint=i;
                            output;
                        end;
                        drop i;
                    run;
                    proc sort data=_list_;
                        by riskpoint;
                    run;
                    proc sql noprint;
                        select distinct riskpoint into :_cif_riskpoints_ separated by ',' from _list_;
                        drop table _list_;
                    quit;
                %end;
                /**Created from numeric list**/
                %else %do i = 1 %to %sysfunc(countw(%superq(risklist),%str( )));
                    %if &i=1 %then %let _cif_riskpoints_=%scan(%superq(risklist&z),&i,%str( ));
                    %else %let _cif_riskpoints_=&_riskpoints_.,%scan(%superq(risklist&z),&i,%str( ));
                %end; 
            %end;
            proc sql noprint;
                %local _nstrata _ntimes _glist _ngroup;
                %let _nstrata=;%let _ntimes=;%let _glist=;%let _ngroup=;
                create table _tempcif as
                    select *
                        %if %sysevalf(%superq(class&z) =,boolean) %then %do;
                            ,1 as _class_
                        %end; 
                        %if %sysevalf(%superq(strata&z) =,boolean) %then %do;
                            ,1 as _strata_1
                        %end;
                        from _tempdsn&z
                        order by _class_,_time_;
        
                    select distinct _class_ into :_glist separated by '|' from _tempcif;
                    %if %sysevalf(%superq(strata&z)^=,boolean) %then %do;
                        select count(*) into :_nstrata
                            from (select distinct _strata_1
                            %do i = 2 %to %sysfunc(countw(%superq(strata&z),%str( )));
                                ,_strata_
                            %end;  from _tempcif);
                    %end;
                    %else %let _nstrata=1;

                select count(distinct _time_)+1 into :_ntimes from _tempcif;
            quit;
            %let _ngroup=%sysfunc(countw(%superq(_glist),|)); 
            data _surv (keep=_class_ groupn _time_ cif censor sdf_lcl sdf_ucl vcif
                        rename=(groupn=stratumnum _time_=t1 cif=s1 _class_=cl1 censor=c1 sdf_lcl=lcl1 sdf_ucl=ucl1))
                _sum (keep=control_var _class_ total failed)
                _quart (keep=stratum _class_ percent estimate lowerlimit upperlimit)
                %if %sysevalf(%superq(timelist)^=,boolean) %then %do;
                    _timelist (keep= stratum timelist _class_ _time_ survival sdf_lcl sdf_ucl)
                %end;
                %if %sysevalf(%superq(risklist)^=,boolean) %then %do;
                    _splot (keep= tatrisk _class_ atrisk stratumnum _time_ rename=(_time_=time))
                %end;;
                set _tempcif end=last;
                by _class_ _time_;
                array times {&_ngroup,&_ntimes} _temporary_;
                array events {&_ngroup,&_ntimes} _temporary_;
                array censors {&_ngroup,&_ntimes} _temporary_;
                array otherevs {&_ngroup,&_ntimes} _temporary_;
                array _survkm {&_ngroup,&_ntimes} _temporary_;
                array _nrisk {&_ngroup,&_ntimes} _temporary_;
                array _cif {&_ngroup,&_ntimes} _temporary_;
                array _vcif {&_ngroup,&_ntimes} _temporary_;
                %if %sysevalf(%superq(timelist&z)^=,boolean) %then %do;
                    array _timepoints {%sysfunc(countw(%superq(_cif_timepoints_),%str(,)))} (&_cif_timepoints_);
                %end;
                %if %sysevalf(%superq(risklist&z)^=,boolean) %then %do;
                    array _riskpoints {%sysfunc(countw(%superq(_cif_riskpoints_),%str(,)))} (&_cif_riskpoints_);
                %end;
                
                
                if first._class_ then do;
                    groupn+1;
                    npat=1;
                    times(groupn,1)=0;
                end;
                else npat+1;
                
                if _n_=1 or first._class_ then do;
                    ntime=2;
                    events(groupn,1)=0;
                    censors(groupn,1)=0;
                    otherevs(groupn,1)=0;
                    _survkm(groupn,1)=1;
                    _cif(groupn,1)=0;
                    ncens=0;nev=0;nother=0;
                end;
                if first._time_ then do;
                    ncens=0;nev=0;nother=0;
                end;
                
                if _cens_=%superq(ev_vl&z) then nev+1;
                else if _cens_=%superq(cen_vl&z) then ncens+1;
                else nother+1;
                
                if last._time_ or last._class_ then do;
                    times(groupn,ntime)=_time_;
                    events(groupn,ntime)=nev;
                    censors(groupn,ntime)=ncens;
                    otherevs(groupn,ntime)=nother;
                    ntime+1;
                end;
                
                if last._class_ then do;
                    _nrisk(groupn,1)=npat;
                    _vcif(groupn,1)=0;
                    stratum=groupn;
                    v11=0;v12=0;v13=0;v21=0;v22=0;v23=0;
                    do i=2 to ntime-1;
                        _nrisk(groupn,i)=_nrisk(groupn,i-1)-events(groupn,i-1)-censors(groupn,i-1)-otherevs(groupn,i-1);
                        /**KM Calculation**/
                        if events(groupn,i)>0 or otherevs(groupn,i) > 0 then do;
                            _survkm(groupn,i)=_survkm(groupn,i-1)*(_nrisk(groupn,i)-(events(groupn,i)+otherevs(groupn,i)))/_nrisk(groupn,i);
                        end;
                        else _survkm(groupn,i)=_survkm(groupn,i-1);
                        
                        
                        /**CIF Calculation**/
                        if events(groupn,i)>0 then _cif(groupn,i)=_cif(groupn,i-1)+ _survkm(groupn,i-1)*(events(groupn,i)/_nrisk(groupn,i));
                        else _cif(groupn,i)=_cif(groupn,i-1);
                    
                        %if %sysevalf(%qupcase(%superq(cifvar&z))=DELTA,boolean) %then %do;
                            /**Variance Delta Method**/
                            if events(groupn,i)>0 or otherevs(groupn,i) > 0 then do;    
                                t1=0;
                                if _nrisk(groupn,i)-(events(groupn,i)+otherevs(groupn,i))>0 then 
                                    t1=((events(groupn,i)+otherevs(groupn,i))/
                                        _nrisk(groupn,i))/
                                        (_nrisk(groupn,i)-(events(groupn,i)+otherevs(groupn,i)));
                                t2=(_survkm(groupn,i-1)*events(groupn,i)/_nrisk(groupn,i))/_nrisk(groupn,i);
                                t3=_survkm(groupn,i-1)*(1-events(groupn,i)/_nrisk(groupn,i))*t2;
                                t4=_cif(groupn,i)**2;
                                v11=v11+t1;
                                v12=v12-2*(_cif(groupn,i)*t1+t2);
                                v13=v13+t4*t1+2*_cif(groupn,i)*t2+t3;
                                _vcif(groupn,i)=t4*v11+_cif(groupn,i)*v12+v13;
                            end;
                        %end;
                        %else %if %sysevalf(%qupcase(%superq(cifvar&z))=COUNT,boolean) %then %do;
                            /**Variance Gray counting Method**/
                            if otherevs(groupn,i)>0 and _survkm(groupn,i)>0 then do;
                                vt1=1;
                                if otherevs(groupn,i)>1 then vt1=1-(otherevs(groupn,i)-1)/(_nrisk(groupn,i)-1);
                                vt2=_survkm(groupn,i-1)**2 * vt1 * otherevs(groupn,i)/_nrisk(groupn,i)**2;
                                vt3=1/_survkm(groupn,i);
                                vt4=_cif(groupn,i)/_survkm(groupn,i);
                                v21=v21 + vt2*vt4**2;
                                v22=v22 + vt2*vt3*vt4;
                                v23=v23 + vt2*vt3**2;
                            end;
                            if events(groupn,i) > 0 then do;
                                vt1=1;
                                if events(groupn,i) > 1 then vt1=1-(events(groupn,i)-1)/(_nrisk(groupn,i)-1);
                                vt2=_survkm(groupn,i-1)**2 * vt1 * events(groupn,i)/_nrisk(groupn,i)**2;
                                if _survkm(groupn,i)>0 then vt3=1/_survkm(groupn,i);
                                else vt3=0;
                                vt4=1+vt3*_cif(groupn,i);
                                v21=v21+vt2*vt4**2;
                                v22=v22+vt2*vt3*vt4;
                                v23=v23+vt2*vt3**2;
                                _vcif(groupn,i)=v21 - 2*_cif(groupn,i)*v22 + _cif(groupn,i)**2*v23;
                            end;  
                        %end;     
                    end;
                    /**Output Calculated CIF Values**/
                    do i = 1 to ntime-1;
                        censor=.;
                        if i=1 or i=ntime-1 or events(groupn,i)>0 then do;
                            _time_=times(groupn,i);
                            nev=events(groupn,i);
                            ncens=censors(groupn,i);
                            notherevs=otherevs(groupn,i);
                            nrisk=_nrisk(groupn,i)-nev-ncens-notherevs;
                            survkm=_survkm(groupn,i);
                            cif=_cif(groupn,i);
                            if censors(groupn,i)>0 then censor=cif;
                            if ^missing(_vcif(groupn,i)) then vcif=sqrt(_vcif(groupn,i));
                            
                            if i > 1  and cif>0 then do;
                                %if %sysevalf(%qupcase(%superq(conftype&z))=LOGLOG,boolean) %then %do;
                                    band_log=1.96*abs(vcif/(cif*log(cif)));
                                    sdf_lcl=cif**(exp(band_log));
                                    sdf_ucl=cif**(exp(-band_log));
                                %end;
                                %else %if %sysevalf(%qupcase(%superq(conftype&z))=LOG,boolean) %then %do;
                                    band_log=1.96*sqrt(vcif**2/(cif**2));
                                    sdf_ucl=cif*exp(band_log);
                                    sdf_lcl=cif*exp(-band_log);
                                %end;
                                %else %if %sysevalf(%qupcase(%superq(conftype&z))=LINEAR,boolean) %then %do;
                                    sdf_lcl=max(0,cif-1.96*vcif);
                                    sdf_ucl=min(1,cif+1.96*vcif);
                                %end;
                                %else %if %sysevalf(%qupcase(%superq(conftype&z))=LOGIT,boolean) %then %do;
                                    sdf_lcl=cif/(cif+(1-cif)*exp(1.96*vcif/(cif*(1-cif))));
                                    sdf_ucl=cif/(cif+(1-cif)*exp(-1.96*vcif/(cif*(1-cif))));
                                %end;
                                %else %if %sysevalf(%qupcase(%superq(conftype&z))=ASINSQRT,boolean) %then %do;
                                    sdf_lcl=(sin(max(0,arsin(sqrt(cif))-1.96*vcif/(2*sqrt(cif*(1-cif))))))**2;
                                    sdf_ucl=(sin(min(constant('pi')/2,arsin(sqrt(cif))+1.96*vcif/(2*sqrt(cif*(1-cif))))))**2;
                                %end;
                                end;
                            else do;
                                sdf_lcl=0;
                                sdf_ucl=0;
                            end;
                            output _surv;
                        end;
                        else if censors(groupn,i) > 0 then do;                            
                            cif=_cif(groupn,i);
                            if cif > 0 then do;
                                %if %sysevalf(%qupcase(%superq(conftype&z))=LOGLOG,boolean) %then %do;
                                    band_log=1.96*abs(vcif/(cif*log(cif)));
                                    sdf_lcl=cif**(exp(band_log));
                                    sdf_ucl=cif**(exp(-band_log));
                                %end;
                                %else %if %sysevalf(%qupcase(%superq(conftype&z))=LOG,boolean) %then %do;
                                    band_log=1.96*sqrt(vcif**2/(cif**2));
                                    sdf_ucl=cif*exp(band_log);
                                    sdf_lcl=cif*exp(-band_log);
                                %end;
                                %else %if %sysevalf(%qupcase(%superq(conftype&z))=LINEAR,boolean) %then %do;
                                    sdf_lcl=max(0,cif-1.96*vcif);
                                    sdf_ucl=min(1,cif+1.96*vcif);
                                %end;
                                %else %if %sysevalf(%qupcase(%superq(conftype&z))=LOGIT,boolean) %then %do;
                                    sdf_lcl=cif/(cif+(1-cif)*exp(1.96*vcif/(cif*(1-cif))));
                                    sdf_ucl=cif/(cif+(1-cif)*exp(-1.96*vcif/(cif*(1-cif))));
                                %end;
                                %else %if %sysevalf(%qupcase(%superq(conftype&z))=ASINSQRT,boolean) %then %do;
                                    sdf_lcl=(sin(max(0,arsin(sqrt(cif))-1.96*vcif/(2*sqrt(cif*(1-cif))))))**2;
                                    sdf_ucl=(sin(min(constant('pi')/2,arsin(sqrt(cif))+1.96*vcif/(2*sqrt(cif*(1-cif))))))**2;
                                %end;
                            end;
                            censor=cif;
                            _time_=times(groupn,i);
                            output _surv;
                        end;
                    end;
                    /**Output Number of Events/Patients**/
                    total=_nrisk(groupn,1);
                    failed=0;
                    control_var='';
                    do i = 1 to ntime-1;
                        failed=failed+events(groupn,i);
                    end;
                    output _sum;
                    /**Median Time-to-Event**/
                    estimate=.;lowerlimit=.;upperlimit=.;percent=50;sdf_lcl=.;sdf_ucl=.;
                    do i = 1 to ntime-1;
                        cif=_cif(groupn,i);
                        if ^missing(_vcif(groupn,i)) then vcif=sqrt(_vcif(groupn,i));
                        if cif > 0 then do;
                            %if %sysevalf(%qupcase(%superq(conftype&z))=LOGLOG,boolean) %then %do;
                                band_log=1.96*abs(vcif/(cif*log(cif)));
                                sdf_lcl=cif**(exp(band_log));
                                sdf_ucl=cif**(exp(-band_log));
                            %end;
                            %else %if %sysevalf(%qupcase(%superq(conftype&z))=LOG,boolean) %then %do;
                                band_log=1.96*sqrt(vcif**2/(cif**2));
                                sdf_lcl=cif*exp(band_log);
                                sdf_ucl=cif*exp(-band_log);
                            %end;
                            %else %if %sysevalf(%qupcase(%superq(conftype&z))=LINEAR,boolean) %then %do;
                                sdf_lcl=max(0,cif-1.96*vcif);
                                sdf_ucl=min(1,cif+1.96*vcif);
                            %end;
                            %else %if %sysevalf(%qupcase(%superq(conftype&z))=LOGIT,boolean) %then %do;
                                sdf_lcl=cif/(cif+(1-cif)*exp(1.96*vcif/(cif*(1-cif))));
                                sdf_ucl=cif/(cif+(1-cif)*exp(-1.96*vcif/(cif*(1-cif))));
                            %end;
                            %else %if %sysevalf(%qupcase(%superq(conftype&z))=ASINSQRT,boolean) %then %do;
                                sdf_lcl=(sin(max(0,arsin(sqrt(cif))-1.96*vcif/(2*sqrt(cif*(1-cif))))))**2;
                                sdf_ucl=(sin(min(constant('pi')/2,arsin(sqrt(cif))+1.96*vcif/(2*sqrt(cif*(1-cif))))))**2;
                            %end;
                        end;
                        if ^missing(_cif(groupn,i)) and ^missing(_vcif(groupn,i)) then do;
                            if _cif(groupn,i) ge 0.5 and estimate=. then estimate=times(groupn,i);
                            if sdf_lcl ge 0.5 and lowerlimit=. then lowerlimit=times(groupn,i);
                            if sdf_ucl ge 0.5 and upperlimit=. then upperlimit=times(groupn,i);
                        end;
                    end;
                    output _quart;
                    /**Time-point values**/
                    %if %sysevalf(%superq(timelist)^=,boolean) %then %do;
                        do j = 1 to dim(_timepoints);
                            _tout=0;
                            survival=.;sdf_lcl=.;sdf_ucl=.;survkm=.;
                            do i = _tout+1 to ntime-1;
                                if times(groupn,i) <= _timepoints(j) and ^missing(_cif(groupn,i)) and ^missing(_vcif(groupn,i)) then do;
                                    survival=_cif(groupn,i);
                                    cif=_cif(groupn,i);
                                    if ^missing(_vcif(groupn,i)) then vcif=sqrt(_vcif(groupn,i));
                                    if cif > 0 then do;
                                        %if %sysevalf(%qupcase(%superq(conftype&z))=LOGLOG,boolean) %then %do;
                                            band_log=1.96*abs(vcif/(cif*log(cif)));
                                            sdf_lcl=cif**(exp(band_log));
                                            sdf_ucl=cif**(exp(-band_log));
                                            drop band_log;
                                        %end;
                                        %else %if %sysevalf(%qupcase(%superq(conftype&z))=LOG,boolean) %then %do;
                                            band_log=1.96*sqrt(vcif**2/(cif**2));
                                            sdf_ucl=cif*exp(band_log);
                                            sdf_lcl=cif*exp(-band_log);
                                            drop band_log;
                                        %end;
                                        %else %if %sysevalf(%qupcase(%superq(conftype&z))=LINEAR,boolean) %then %do;
                                            sdf_lcl=max(0,cif-1.96*vcif);
                                            sdf_ucl=min(1,cif+1.96*vcif);
                                        %end;
                                        %else %if %sysevalf(%qupcase(%superq(conftype&z))=LOGIT,boolean) %then %do;
                                            sdf_lcl=cif/(cif+(1-cif)*exp(1.96*vcif/(cif*(1-cif))));
                                            sdf_ucl=cif/(cif+(1-cif)*exp(-1.96*vcif/(cif*(1-cif))));
                                        %end;
                                        %else %if %sysevalf(%qupcase(%superq(conftype&z))=ASINSQRT,boolean) %then %do;
                                            sdf_lcl=(sin(max(0,arsin(sqrt(cif))-1.96*vcif/(2*sqrt(cif*(1-cif))))))**2;
                                            sdf_ucl=(sin(min(constant('pi')/2,arsin(sqrt(cif))+1.96*vcif/(2*sqrt(cif*(1-cif))))))**2;
                                        %end;
                                    end;
                                    _tout=i;
                                end;
                                if times(groupn,i) = _timepoints(j) or
                                    (times(groupn,i) < _timepoints(j) and times(groupn,i+1)>_timepoints(j)) then do;
                                    _time_=times(groupn,_tout);
                                    timelist=_timepoints(j);
                                    stratum=groupn;
                                    output _timelist;
                                end;
                                if times(groupn,i) ge _timepoints(j) then i=ntime;
                            end;
                            if _timepoints(j) > times(groupn,ntime-1) then do;
                                do  i = j to dim(_timepoints);
                                    survival=.;sdf_lcl=.;sdf_ucl=.;
                                    timelist=_timepoints(i);
                                    stratum=groupn;
                                    output _timelist;
                                end;
                                j=dim(_timepoints);
                            end;
                        end;
                    %end;
                
                    %if %sysevalf(%superq(risklist)^=,boolean) %then %do;
                        do j = 1 to dim(_riskpoints);
                            _rout=0;
                            if j=1 then atrisk=_nrisk(groupn,1);
                            if atrisk>0 then do i = _rout+1 to ntime-1;
                                if times(groupn,i)=_riskpoints(j) or
                                    (times(groupn,i)<_riskpoints(j) and times(groupn,i+1)>_riskpoints(j)) then do;
                                    _time_=_riskpoints(j);
                                    if times(groupn,i)=_riskpoints(j) then atrisk=_nrisk(groupn,i);
                                    else atrisk=_nrisk(groupn,i)-events(groupn,i)-censors(groupn,i)-otherevs(groupn,i);
                                    tatrisk=_riskpoints(j);
                                    stratumnum=groupn;
                                    output _splot;
                                    _rout=i;
                                end;
                                else if times(groupn,i) < _riskpoints(j) and i=ntime-1 then do;
                                    _time_=_riskpoints(j);
                                    atrisk=_nrisk(groupn,i)-events(groupn,i)-censors(groupn,i)-otherevs(groupn,i);
                                    tatrisk=_riskpoints(j);
                                    stratumnum=groupn;
                                    output _splot;
                                    _rout=i;
                                end;
                            if _rout = i or times(groupn,i) > _riskpoints(j) then i=ntime;
                            end;
                            if _riskpoints(j) > times(groupn,ntime-1) then j=dim(_riskpoints);
                        end;
                    %end;
                end;
            run;
            %if %sysevalf(%superq(class&z)^=,boolean) %then %do;
                proc sort data=_tempcif;
                    by _strata_1
                        %do i = 2 %to %sysfunc(countw(%superq(strata&z),%str( )));
                            _strata_&i
                        %end;  _class_ _time_;
                run;
                data _tempcif2;
                    set _tempcif end=last;
                    by _strata_1
                    %do i = 2 %to %sysfunc(countw(%superq(strata&z),%str( )));
                        _strata_&i
                    %end; _class_ _time_;
                
                    if first._strata_%sysfunc(max(1,%sysfunc(countw(&strata,%str( ))))) then do;
                    _sg_+1;
                    groupn=0;
                    end;
                    if first._class_ then groupn+1;        
                run;
                    
                proc sort data=_tempcif2;
                    by _sg_ _time_ _class_ ;
                run;
                    
                data _score (keep=score_st:) _variance (keep=_var:);
                    set _tempcif2 end=last;
                    by _sg_ _time_ _class_;
                    
                    array times {&_nstrata,&_ntimes} _temporary_;
                    array events {&_nstrata,&_ngroup,&_ntimes} _temporary_;
                    array censors {&_nstrata,&_ngroup,&_ntimes} _temporary_;
                    array otherevs {&_nstrata,&_ngroup,&_ntimes} _temporary_;
                    array _nrisk {&_nstrata,&_ngroup} (%sysevalf(&_nstrata*&_ngroup)*0);
                    retain _nrisk;
                    
                    array score {&_nstrata,%sysevalf(&_ngroup-1)} _temporary_;
                    array score_st {%sysevalf(&_ngroup-1)} (%sysevalf(&_ngroup-1)*0);
                    array vcov {&_nstrata,%sysevalf((&_ngroup-1)*&_ngroup/2)} _temporary_;
                    array vcov_st {%sysevalf((&_ngroup-1)*&_ngroup/2)} (%sysevalf((&_ngroup-1)*&_ngroup/2)*0);
                    array nrisk {&_nstrata,&_ngroup} _temporary_;
                    
                    array cifgleft {&_nstrata,&_ngroup} _temporary_;
                    array cifg {&_nstrata,&_ngroup} _temporary_;
                    
                    array skmgleft {&_nstrata,&_ngroup} _temporary_;
                    array skmg {&_nstrata,&_ngroup} _temporary_;
                    
                    array vtvec {&_nstrata,&_ngroup} _temporary_;
                    array vv(%sysevalf(&_ngroup-1),%sysevalf(&_ngroup-1));
                    array _var(%sysevalf(&_ngroup-1));
                    
                    array vtmatrix {&_nstrata,%sysevalf(&_ngroup-1),&_ngroup} _temporary_;
                    array a {&_nstrata,&_ngroup,&_ngroup} _temporary_;
                    array c {&_nstrata,&_ngroup,&_ngroup} _temporary_;
                    
                    if first._sg_ then do;
                        ntime=1;
                        max_grp=groupn;
                    end;
                    max_grp=max(max_grp,groupn);
                    retain max_grp;
                    
                    if first._class_ or first._time_ or first._sg_ then do;
                        events(_sg_,groupn,ntime)=0;
                        censors(_sg_,groupn,ntime)=0;
                        otherevs(_sg_,groupn,ntime)=0;
                    end;
                    
                    if _cens_=%superq(ev_vl&z) then events(_sg_,groupn,ntime)+1;
                    else if _cens_=%superq(cen_vl&z) then censors(_sg_,groupn,ntime)+1;
                    else otherevs(_sg_,groupn,ntime)+1;
                    
                    if last._time_ then do;
                        times(_sg_,ntime)=_time_;
                        ntime+1;
                    end;
                    
                    _nrisk(_sg_,groupn)=_nrisk(_sg_,groupn)+1;
                    
                    if last._sg_ then do;
                        ng=max_grp;
                        ng1=ng-1;
                        ng2=ng*ng1/2;
                        l=0;
                        cif0left=0;
                        cif0=0;
                        ll=1;
                        rho=0;
                        do i = 1 to ng2; 
                            vcov(_sg_,i)=0;
                        end;
                        do i =1 to ng;
                            if i le ng1 then score(_sg_,i)=0; 
                            cifgleft(_sg_,i)=0;
                            cifg(_sg_,i)=0;
                            vtvec(_sg_,i)=0;
                            skmgleft(_sg_,i)=1;
                            skmg(_sg_,i)=1;
                            do j = 1 to ng;
                                if i le ng-1 then vtmatrix(_sg_,i,j)=0;
                                c(_sg_,i,j)=0;
                            end;
                        end;
                        do i = 1 to ntime;
                            do k = 1 to ng;                
                                do j = 1 to ng;
                                    a(_sg_,k,j)=0;
                                end;
                            end;
                            nd1=0;
                            nd2=0;
                            do j = 1 to ng;
                                if missing(events(_sg_,j,i)) then events(_sg_,j,i)=0;
                                if missing(censors(_sg_,j,i)) then censors(_sg_,j,i)=0;
                                if missing(otherevs(_sg_,j,i)) then otherevs(_sg_,j,i)=0;
                                nd1=sum(nd1,events(_sg_,j,i));
                                nd2=sum(nd2,otherevs(_sg_,j,i));
                            end;
                            if nd1>0 or nd2 > 0 then do;
                                hdot=0;rdot=0;
                                do g=1 to ng;
                                    if _nrisk(_sg_,g)> 0 then do;
                                        td=sum(0,events(_sg_,g,i),otherevs(_sg_,g,i));
                                        skmg(_sg_,g)=skmgleft(_sg_,g)*(_nrisk(_sg_,g)-td)/_nrisk(_sg_,g);
                                        cifg(_sg_,g)=sum(0,cifgleft(_sg_,g),skmgleft(_sg_,g)*events(_sg_,g,i)/_nrisk(_sg_,g));
                                        hdot=sum(hdot,_nrisk(_sg_,g)/skmgleft(_sg_,g));
                                        rdot=sum(rdot,_nrisk(_sg_,g)*(1-cifgleft(_sg_,g))/skmgleft(_sg_,g));
                                    end;
                                end;
                                cif0=sum(cif0left,nd1/hdot);
                                grho=(1-cif0left)**rho;
                            
                                do g=1 to ng1;
                                    if _nrisk(_sg_,g)> 0 then score(_sg_,g)=sum(score(_sg_,g),
                                    grho*(events(_sg_,g,i)-nd1*_nrisk(_sg_,g)*(1-cifgleft(_sg_,g))/skmgleft(_sg_,g)/rdot));
                                end;
                                do g=1 to ng;
                                    if _nrisk(_sg_,g)> 0 then do;
                                        t1=_nrisk(_sg_,g)/skmgleft(_sg_,g);
                                        a(_sg_,g,g)=grho*t1*(1-t1/hdot);
                                        c(_sg_,g,g)=sum(c(_sg_,g,g),a(_sg_,g,g)*nd1/hdot/(1-cif0left));
                                        do j = g+1 to ng;
                                            if _nrisk(_sg_,j)> 0 then do;
                                                a(_sg_,g,j)=-grho*t1*_nrisk(_sg_,j)/skmgleft(_sg_,j)/hdot;
                                                a(_sg_,j,g)=a(_sg_,g,j);
                                                c(_sg_,g,j)=sum(c(_sg_,g,j),a(_sg_,g,j)*nd1/hdot/(1-cif0left));
                                                c(_sg_,j,g)=c(_sg_,g,j);                                    
                                            end;
                                        end;                                
                                    end;                        
                                end;
                                /**Variance Estimators**/
                                if (nd1^=0) then do k=1 to ng;
                                    if _nrisk(_sg_,k)>0 then do;
                                        if skmg(_sg_,k)>0 then vt1=1-(1-cif0)/skmg(_sg_,k);
                                        else vt1=1;
                                        if nd1>1 then vt2=1-(nd1-1)/(hdot*skmgleft(_sg_,k)-1);
                                        else vt2=1;
                                        vt3=vt2*skmgleft(_sg_,k)*nd1/(hdot*_nrisk(_sg_,k));
                                        vtvec(_sg_,k)=sum(vtvec(_sg_,k),vt1**2*vt3);
                                        do g = 1 to ng1;
                                            vt4=a(_sg_,g,k)-vt1*c(_sg_,g,k);
                                            vtmatrix(_sg_,g,k)=sum(vtmatrix(_sg_,g,k),vt4*vt1*vt3);
                                            do j = 1 to g;
                                                l=g*(g-1)/2+j;
                                                vt5=a(_sg_,j,k)-vt1*c(_sg_,j,k);
                                                vcov(_sg_,l)=sum(vcov(_sg_,l),vt3*vt4*vt5);
                                            end;
                                        end;
                                    end;
                                end;
                                if (nd2^=0) then do k=1 to ng;
                                    if (skmg(_sg_,k)>0 and otherevs(_sg_,k,i)) then do;
                                        vt1=(1-cif0)/skmg(_sg_,k);
                                        vt2=1;
                                        if otherevs(_sg_,k,i)>1 then vt2=1-(otherevs(_sg_,k,i)-1)/(_nrisk(_sg_,k)-1);
                                        vt3=vt2*((skmgleft(_sg_,k)**2)*otherevs(_sg_,k,i))/(_nrisk(_sg_,k)**2);
                                        vtvec(_sg_,k)=sum(vtvec(_sg_,k),vt1**2*vt3);
                                        do g=1 to ng1;
                                            vt4=vt1*c(_sg_,g,k);
                                            vtmatrix(_sg_,g,k)=sum(vtmatrix(_sg_,g,k),-vt4*vt1*vt3);
                                            do j=1 to g;
                                                l=g*(g-1)/2+j;
                                                vt5=vt1*c(_sg_,j,k);
                                                vcov(_sg_,l)=sum(vcov(_sg_,l),vt3*vt4*vt5);
                                            end;
                                        end;
                                    end;
                                end;
                            end;
                            do g = 1 to ng;
                                _nrisk(_sg_,g)=sum(_nrisk(_sg_,g),-censors(_sg_,g,i),
                                -events(_sg_,g,i),-otherevs(_sg_,g,i));
                                cifgleft(_sg_,g)=cifg(_sg_,g);
                                skmgleft(_sg_,g)=skmg(_sg_,g);
                            end;
                            cif0left=cif0;
                        end;
                        pos=0;
                        do g = 1 to ng1;
                            do j = 1 to g;
                                pos=pos+1;
                                do k = 1 to ng;
                                    vcov(_sg_,pos)=sum(vcov(_sg_,pos),
                                    c(_sg_,g,k)*c(_sg_,j,k)*vtvec(_sg_,k));
                                    vcov(_sg_,pos)=sum(vcov(_sg_,pos),
                                    c(_sg_,g,k)*vtmatrix(_sg_,j,k));
                                    vcov(_sg_,pos)=sum(vcov(_sg_,pos),
                                    c(_sg_,j,k)*vtmatrix(_sg_,g,k));
                                end;
                            end;
                        end;            
                    end;
                    if last then do;
                        do i = 1 to _sg_;
                            do j = 1 to ng1;
                                score_st(j)=sum(score_st(j),score(i,j));
                            end;
                        end;
                        do i = 1 to _sg_;
                            do j = 1 to ng1*ng/2;
                                vcov_st(j)=sum(vcov_st(j),vcov(i,j));
                            end;
                        end;
                    
                        do i = 1 to ng1;
                            do j = 1 to ng1;
                                vv(i,j)=0;
                            end;
                        end;
                        pos=0;
                        do i = 1 to ng1;
                            do j = 1 to i;
                                pos=pos+1;
                                vv(i,j)=vcov_st(pos);
                                vv(j,i)=vv(i,j);
                            end;
                        end;
                        output _score;
                        do i = 1 to ng1;
                            do j = 1 to ng1;
                                _var(j)=vv(i,j);
                            end;
                            output _variance;
                        end;    
                    end;
                run;
                
                proc fcmp;
                    %if &_ngroup > 2 %then %do;
                        array x[%sysevalf(&_ngroup-1),%sysevalf(&_ngroup-1)] / nosymbols;
                        array xinverse[%sysevalf(&_ngroup-1),%sysevalf(&_ngroup-1)];
                    %end;
                    %else %do;
                        array x[1] / nosymbols;
                        array xinverse[1];
                    %end;
                    array score[%sysevalf(&_ngroup-1)]/ nosymbols;
                    array scoret[%sysevalf(&_ngroup-1),1];
                    array step1[1,%sysevalf(&_ngroup-1)];
                    array step2[1];
                    
                    rc=read_array('_variance',x);
                    rc=read_array('_score',score);
                    %if &_ngroup > 2 %then %do;
                        call inv(x,xinverse);
                    %end;
                    %else %do;
                        xinverse[1]=1/x[1];
                    %end;
                    call transpose(score,scoret);
                    call mult(score,xinverse,step1);
                    call mult(step1,scoret,step2);
                    rc=write_array('_stat',step2);
                run;
                
                data _ltest;
                    set _stat (rename=(step21=test_stat));
                    df=&_ngroup-1;
                    probchisq=1-probchi(test_stat,df);
                run;
            %end;
        %end;
        %if %sysevalf(%superq(class&z)=,boolean)=0 and ^(%sysevalf(%qupcase(%superq(method&z))=CIF and &sysver < 9.4)) %then %do;
            /**hazard ratio, p-value**/
            proc phreg data=_tempdsn&z;
                /***Set up class variable and class covariates***/
                class 
                    /***Class Variable and Reference Group***/
                    _class_ %if %sysevalf(%superq(classref&z)=,boolean)=0 %then %do; (ref="%superq(classref&z)") %end;;
                /**Apply strata**/
                %if %sysevalf(%superq(strata&z)=,boolean)=0 %then %do;
                    strata %do i = 1 %to %sysfunc(countw(%superq(strata&z),%str( ))); _strata_&i %end;;
                %end;
                /**Run model**/
                model _time_ * _cens_(%superq(cen_vl&z)) = _class_ /**Class Variable**/
                    / alpha=&alphaHR. rl ties=%superq(ties&z) type3 (SCORE LR WALD)
                    %if %sysevalf(%qupcase(%superq(method&z))=CIF) %then %do;
                        eventcode=%superq(ev_vl&z)
                    %end;; /**P-values, Ties, and Confidence Bounds, Competing Risks Event Level**/
                ods output parameterestimates=_parm (where=(upcase(strip(parameter))=upcase(strip("_class_")))) /***Hazard ratios and confidence intervals***/
                    %if %sysevalf(9.04.01M2P072314 > &sysvlong,boolean) %then %do;
                        type3=_t3 (where=(upcase(effect)=upcase("_class_"))) /**P-values**/
                    %end;
                    %else %do;
                        modelanova=_t3 (where=(upcase(strip(effect))=upcase(strip("_class_")))) /**P-values**/
                    %end;; 
            run;
            %if %sysevalf(%superq(classcov&z)^=,boolean) or %sysevalf(%superq(contcov&z)^=,boolean) %then %do;
                /**Adjusted hazard ratio, p-value**/
                proc phreg data=_tempdsn&z;
                    /***Set up class variable and class covariates***/
                    class 
                        /***Class Variable and Reference Group***/
                        _class_ %if %sysevalf(%superq(classref&z)=,boolean)=0 %then %do; (ref="%superq(classref&z)") %end;
                        /**Class covariates**/
                        %if %sysevalf(%superq(classcov&z)=,boolean) =0 %then %do i = 1 %to %sysfunc(countw(%superq(classcov&z),%str( ))); 
                            _classcov_&i 
                        %end;;
                    /**Apply strata**/
                    %if %sysevalf(%superq(strata&z)=,boolean)=0 %then %do;
                        strata %do i = 1 %to %sysfunc(countw(%superq(strata&z),%str( ))); _strata_&i %end;;
                    %end;
                    /**Run model**/
                    model _time_ * _cens_(%superq(cen_vl&z)) = _class_ /**Class Variable**/
                        %if %sysevalf(%superq(classcov&z)=,boolean) =0 %then %do i = 1 %to %sysfunc(countw(%superq(classcov&z),%str( ))); _classcov_&i %end; /**Class Covariates**/
                        %if %sysevalf(%superq(contcov&z)=,boolean) =0 %then %do i = 1 %to %sysfunc(countw(%superq(contcov&z),%str( ))); _contcov_&i %end; /**Numeric Covariates**/
                        / alpha=&alphaHR. rl ties=%superq(ties&z) type3 (SCORE LR WALD)
                        %if %sysevalf(%qupcase(%superq(method&z))=CIF) %then %do;
                            eventcode=%superq(ev_vl&z)
                        %end;; /**P-values, Ties, and Confidence Bounds, and Competing Risks Event Code**/
                    ods output parameterestimates=_parmmv (where=(upcase(strip(parameter))=upcase(strip("_class_")))) /***Hazard ratios and confidence intervals***/
                        %if %sysevalf(9.04.01M2P072314 > &sysvlong,boolean) %then %do;
                            type3=_t3mv (where=(upcase(effect)=upcase("_class_"))) /**P-values**/
                        %end;
                        %else %do;
                            modelanova=_t3mv (where=(upcase(strip(effect))=upcase(strip("_class_")))) /**P-values**/
                        %end;;
                    where missing(_class_)=0
                        %if %sysevalf(%superq(classcov&z)=,boolean) =0 %then %do i = 1 %to %sysfunc(countw(%superq(classcov&z),%str( )));
                            and missing(_classcov_&i)=0
                        %end;
                        %if %sysevalf(%superq(contcov&z)=,boolean) =0 %then %do i = 1 %to %sysfunc(countw(%superq(contcov&z),%str( )));
                            and missing(_contcov_&i)=0
                        %end;  ;
                run;
            %end;
        %end;
        proc sql;
            /**Creates a fresh table to insert analysis into for summary table if flagged**/
            /**If not flagged, inserts new anlysis into current table**/
            %local _model;
            ***Check if output table has been determined;
            %if %sysevalf(%superq(out)=,boolean)=0 %then %do;
                %if (&newtable=1 or %sysfunc(exist(&out))=0) and &z = 1 %then %do;
                    create table &out
                        (modelnum num 'Model Number',/**Tracks models to distinguish later in PROC REPORT**/
                        modeltype num, /**Tracks whether the data is KM or CIF**/
                        subind num 'Indentation Indicator',/**Determines if a row is indented or not in PROC REPORT**/
                        subtitle char(100) 'Factor Label',/**Sub-title for each model's analysis.  Generally a description of the class variable**/
                        title char(2000) 'Model Title',/**Title for each model's analysis**/
                        footnote char(2000) 'Model Footnote',/**Footnotes for each model's analysis**/
                        total num 'Total',/**Number of patients**/
                        event num 'Events',/**Number of events**/
                        ev_n char(100) 'Events/N',/**Combination of events and patients.  Format events/patients**/
                        median char(50) 'Median',/**Median time-to-event with confidence interval**/
                        hr char(50) 'Hazard Ratio (95% CI)',/**Hazard ratio with confidence interval**/ 
                        timelist char(500) 'KM Estimate (95% CI)',/**Event time-point estimates**/
                        covpval char(50) 'Covariate Level P-value',/**Covariate Level P-value**/
                        pval char(50) 'Displayed P-value',/**Chosen P-value to display for model**/
                        totalmv num 'Total (Multivariate)',/**Number of patients**/
                        eventmv num 'Events (Multivariate)',/**Number of events**/
                        ev_nmv char(100) 'Events/N (Multivariate)',/**Combination of events and patients.  Format events/patients**/
                        hrmv char(50) 'Hazard Ratio (95% CI) (Multivariate)',/**Hazard ratio with confidence interval**/ 
                        pvalmv char(50) 'Displayed P-value (Multivariate)',/**Chosen P-value to display for model**/
                        covpvalmv char(50) 'Covariate Level P-value (Multivariate)'/**Covariate Level P-value**/);
                    %let _model=0;           
                %end;
                %else %if &z =1 %then %do;
                    /**Grab maximum modelnum from previous table to increment upon**/
                    select max(modelnum) into :_model from &out;
                %end;
                /**Create temporary table to insert analysis into before inserting into summary table**/
                create table _temptable like &out;
            %end;
            %else %do;
                %if &z=1 %then %do;
                /**Default if no output table requested**/
                create table _summary
                    (modelnum num 'Model Number',/**Tracks models to distinguish later in PROC REPORT**/
                    modeltype num, /**Tracks whether the data is KM or CIF**/
                    subind num 'Indentation Indicator',/**Determines if a row is indented or not in PROC REPORT**/
                    subtitle char(100) 'Factor Label',/**Sub-title for each model's analysis.  Generally a description of the class variable**/
                    title char(2000) 'Model Title',/**Title for each model's analysis**/
                    footnote char(2000) 'Model Footnote',/**Footnotes for each model's analysis**/
                    total num 'Total',/**Number of patients**/
                    event num 'Events',/**Number of events**/
                    ev_n char(100) 'Events/N',/**Combination of events and patients.  Format events/patients**/
                    median char(50) 'Median',/**Median time-to-event with confidence interval**/
                    hr char(50) 'Hazard Ratio (95% CI)',/**Hazard ratio with confidence interval**/ 
                    timelist char(500) 'KM Estimate (95% CI)',/**Event time-point estimates**/
                    covpval char(50) 'Covariate Level P-value',/**Covariate Level P-value**/
                    pval char(50) 'Displayed P-value',/**Chosen P-value to display for model**/
                    totalmv num 'Total (Multivariate)',/**Number of patients**/
                    eventmv num 'Events (Multivariate)',/**Number of events**/
                    ev_nmv char(100) 'Events/N (Multivariate)',/**Combination of events and patients.  Format events/patients**/
                    hrmv char(50) 'Hazard Ratio (95% CI) (Multivariate)',/**Hazard ratio with confidence interval**/ 
                    pvalmv char(50) 'Displayed P-value (Multivariate)',/**Chosen P-value to display for model**/
                    covpvalmv char(50) 'Covariate Level P-value (Multivariate)'/**Covariate Level P-value**/);
                %let _model=0;
                %end;
                /**Create temporary table to insert analysis into before inserting into summary table**/
                create table _temptable like _summary;
            %end;          
            reset noprint;
            
            %local nclass_&z;
            %if %sysevalf(%superq(class&z)=,boolean)=0 %then %do;
                /**Saves number of class levels into macro variable**/
                select count(distinct _class_) into :nclass_&z from _sum
                    where control_var = '';
            %end;
            %else %do;
                /**Set number of class levels to 1**/
                %let nclass_&z=1;
            %end; 
            /***Error Check if Enough Colors were Specified***/
            %if %sysevalf(%superq(color&z)=,boolean)=0 %then %do;
                %if %superq(nclass_&z) > %sysfunc(countw(%superq(color&z),%str( ))) and
                    %sysfunc(countw(%superq(color&z),%str( ))) > 1 %then %do;
                    %put ERROR: (Model &z: COLOR) Not enough line colors specified for number of class levels (%qupcase(%superq(color&z)) vs. %sysfunc(strip(%superq(nclass_&z))) Class Levels);
                    %let nerror_run=%eval(&nerror_run+1);
                %end;
            %end;
            %else %do;
                %put ERROR: (Model &z: COLOR) No line colors specified;
                %let nerror_run=%eval(&nerror_run+1);
            %end;
            /***Error Check if Enough Plot Line Patterns were Specified***/
            %if %sysevalf(%superq(pattern&z)=,boolean)=0 %then %do;
                %if %superq(nclass_&z) > %sysfunc(countw(%superq(pattern&z),%str( ))) and
                    %sysfunc(countw(%superq(pattern&z),%str( ))) > 1 %then %do;
                    %put ERROR: (Model &z: PATTERN) Not enough patterns specified for number of class levels (%qupcase(%superq(pattern&z)) vs. %sysfunc(strip(%superq(nclass_&z))) Class Levels);
                    %let nerror_run=%eval(&nerror_run+1);
                %end;
            %end;
            %else %do;
                %put ERROR: (Model &z: PATTERN) No line patterns specified;
                %let nerror_run=%eval(&nerror_run+1);
            %end;
            %if %sysevalf(%superq(plotci&z)=1,boolean) %then %do;
                /**If Plot CI background fill is enabled**/
                %if %sysevalf(%superq(plotcifill&z)=1,boolean) %then %do;
                    /***Error Check if Enough Colors were Specified for Confidence Bounds Fill***/
                    %if %sysevalf(%superq(plotcifillcolor&z)=,boolean)=0 %then %do;
                        %if %superq(nclass_&z) > %sysfunc(countw(%superq(plotcifillcolor&z),%str( ))) and
                            %sysfunc(countw(%superq(plotcifillcolor&z),%str( ))) > 1 %then %do;
                            %put ERROR: (Model &z: PLOTCIFILLCOLOR) Not enough line colors specified for number of class levels (%qupcase(%superq(plotcifillcolor&z)) vs. %sysfunc(strip(%superq(nclass_&z))) Class Levels);
                            %let nerror_run=%eval(&nerror_run+1);
                        %end;
                    %end;
                    %else %do;
                        %put ERROR: (Model &z: PLOTCIFILLCOLOR) No line colors specified;
                        %let nerror_run=%eval(&nerror_run+1);
                    %end;
                %end;
                /***Error Check if Enough Colors were Specified for Confidence Bounds Lines***/
                %if %sysevalf(%superq(plotcilinecolor&z)=,boolean)=0 %then %do;
                    %if %superq(nclass_&z) > %sysfunc(countw(%superq(plotcilinecolor&z),%str( ))) and
                        %sysfunc(countw(%superq(plotcilinecolor&z),%str( ))) > 1 %then %do;
                        %put ERROR: (Model &z: PLOTCILINECOLOR) Not enough line colors specified for number of class levels (%qupcase(%superq(plotcilinecolor&z)) vs. %sysfunc(strip(%superq(nclass_&z))) Class Levels);
                        %let nerror_run=%eval(&nerror_run+1);
                    %end;
                %end;
                %else %do;
                    %put ERROR: (Model &z: PLOTCILINECOLOR) No line colors specified;
                    %let nerror_run=%eval(&nerror_run+1);
                %end;
                /***Error Check if Enough Plot Line Patterns were Specified***/
                %if %sysevalf(%superq(color&z)=,boolean)=0 %then %do;
                    %if %superq(nclass_&z) > %sysfunc(countw(%superq(plotcilinepattern&z),%str( ))) and
                        %sysfunc(countw(%superq(plotcilinepattern&z),%str( ))) > 1 %then %do;
                        %put ERROR: (Model &z: PLOTCILINEPATTERN) Not enough patterns specified for number of class levels (%qupcase(%superq(plotcilinepattern&z)) vs. %sysfunc(strip(%superq(nclass_&z))) Class Levels);
                        %let nerror_run=%eval(&nerror_run+1);
                    %end;
                %end;
                %else %do;
                    %put ERROR: (Model &z: PLOTCILINEPATTERN) No line patterns specified;
                    %let nerror_run=%eval(&nerror_run+1);
                %end;
            %end;
            /***Error Check if Enough Class Order Levels were Specified if Not Missing***/
            %if %sysevalf(%superq(classorder&z)^=,boolean) %then %do;
                %if %superq(nclass_&z) ^= %sysfunc(countw(%superq(classorder&z),%str( ))) %then %do;
                    %put ERROR: (Model &z: CLASSORDER) Different number of classorders specified than number of class levels (%qupcase(%superq(classorder&z)) vs. %sysfunc(strip(%superq(nclass_&z))) Class Levels);
                    %let nerror_run=%eval(&nerror_run+1);
                %end;
            %end;   
            /***Error Check if each level of class was specified in the class order list***/    
            %if %sysevalf(%superq(classorder&z)^=,boolean) and &nerror_run=0 %then %do;
                %do i = 1 %to %superq(nclass_&z);
                    %local _test;
                    %let _test=;
                    %do j = 1 %to %sysfunc(countw(%superq(classorder&z),%str( )));
                        %if &i = %scan(%superq(classorder&z),&j,%str( )) %then %let _test=1;
                    %end;
                    %if &_test ^=1 %then %do;
                        %put ERROR: (Model &z: CLASSORDER) Number &i was not found in the CLASSORDER list;
                        %put ERROR: (Model &z: CLASSORDER): Each number from 1 to maximum number of levels in CLASS variable %qupcase(%superq(class&z)) (%sysfunc(strip(%superq(nclass_&z)))) must be represented;
                        %let nerror_run=%eval(&nerror_run+1);                            
                    %end;
                %end;
            %end;                   
            %if &nerror_run > 0 %then %goto errhandl2;
            
            
            /***Save class values into macro variables***/
            %do i = 1 %to %superq(nclass_&z);
                %local class_&z._&i;
            %end;               
            %if %sysevalf(%superq(class&z)=,boolean) =0 %then %do;
                /**If order set to auto then select class values in order sorted by class variable**/
                %if %sysevalf(%superq(classorder&z)=,boolean) %then %do;
                    select distinct _class_ into :class_&z._1-:class_&z._%sysfunc(strip(%superq(nclass_&z)))
                        from _sum
                        where control_var = ''
                        order by _class_
                        %if %superq(desc&z)=1 %then %do; DESC %end; /**If desc=DESC then reverse order of class variable**/;
                %end;
                %else %do;
                    /**Select class values per requested order**/
                    %local ___classlevels_&z;
                    select distinct _class_ into :___classlevels_&z separated by '|'
                        from _sum
                        where control_var = ''
                        order by _class_;
                    %local i2;
                    %do i = 1 %to %superq(nclass_&z);
                        %if %superq(desc&z)=1 %then %let i2=%sysfunc(abs(%sysfunc(sum(&i,-%superq(nclass_&z),-1))));
                        %else %let i2=&i;
                        %let class_&z._&i = %scan(%superq(___classlevels_&z),%scan(%superq(classorder&z),&i2),|,m);
                    %end;
                %end;
            %end;
            %else %let class_&z._1=%superq(classdesc&z);
            
            %if %superq(increase&z) = 1 and %sysevalf(%superq(timelist&z)^=,boolean) %then %do;
                update _timelist
                    set survival=1-survival,
                    sdf_lcl=1-sdf_ucl,
                    sdf_ucl=1-sdf_lcl;
            %end;
            
            %if %sysevalf(%superq(timelist&z)=,boolean)=0 %then %do i = 1 %to %superq(nclass_&z);
                /**Create a macro variable storing values of timelist survival estimates**/
                /**^n creates a new line, ^_ unbreakable space**/
                %local tl&i;
                select strip(put(timelist, best12.3)) || " %superq(timedx&z): ^n " ||
                    '^_^_^_' ||
                    case(survival)
                        when . then 'NE'
                    else strip(put(%superq(xmult_&z) *survival,%superq(tfmt_&z)))end || ' (' ||
                    case(sdf_lcl)
                        when . then 'NE'
                    else strip(put(%superq(xmult_&z) *sdf_lcl,%superq(tfmt_&z))) end || '-' ||
                    case (sdf_ucl)
                        when . then 'NE'
                    else strip(put(%superq(xmult_&z) *sdf_ucl,%superq(tfmt_&z))) end ||
                    %if %sysfunc(upcase(%superq(ytype&z))) = PCT %then %do; '%' || %end; ')'
                    into :tl&i separated by '^n '
                    from _timelist
                    %if %sysevalf(%superq(class&z)=,boolean)=0 %then %do;
                        where strip(_class_)=strip("%superq(class_&z._&i)")
                    %end;
                    ;
            %end;
            
            /**Insert analysis into final summary dataset**/
            insert into _temptable
                /*Inserts Header row for model*/
                set modelnum=sum(&z,&_model),
                modeltype=%if %sysevalf(%qupcase(%superq(method&z))=KM,boolean) %then %do;
                              %if %sysevalf(%superq(increase&z)=1,boolean) %then %do;
                                1
                              %end;
                              %else %do;
                                0
                              %end;
                          %end;
                          %else %do;
                            2
                          %end;,
                title=strip("%superq(title&z)"),
                footnote=strip(tranwrd("%superq(footnote&z)",'`','^n')),
                subind=1,
                %if %sysevalf("%superq(classdesc&z)"="",boolean) and %sysevalf(%superq(class&z)^=,boolean) %then %do;
                    subtitle=strip("%superq(label&z)"),
                %end;
                %else %do;
                    subtitle=strip("%superq(classdesc&z)"),
                %end;
                %if %sysevalf(%superq(class&z)=,boolean) %then %do; 
                    total=(select total from _sum),
                    event=(select failed from _sum),
                    ev_n=(select strip(put(failed,12.))||'/'||strip(put(total,12.)) from _sum),
                    median=(select case (estimate)
                        when . then 'NE'
                    else strip(put(estimate, %sysevalf(%sysevalf(12.&&mediandigits&z)))) end || ' (' ||
                    case (lowerlimit)
                        when . then 'NE'
                    else strip(put(lowerlimit, %sysevalf(12.&&mediandigits&z))) end || '-' ||
                    case (upperlimit)
                        when . then 'NE'
                    else strip(put(upperlimit, %sysevalf(12.&&mediandigits&z))) end || ')'
                    from _quart)                    
                    %if %sysevalf(%superq(timelist&z)=,boolean)=0 %then %do;
                        , timelist=strip("&tl1")
                    %end;
                %end;
                %else %do;
                    pval=%if %qupcase(%superq(plotpval&z))=LR %then %do;
                            strip(put((select problrchisq from _t3
                            where upcase(effect)=upcase(strip("_class_"))),pvalue%sysevalf(6.&&pvaldigits&z)))||'^{super #}'
                        %end;
                        %else %if %qupcase(%superq(plotpval&z))=SCORE %then %do;
                            strip(put((select probscorechisq from _t3
                            where upcase(effect)=upcase(strip("_class_"))),pvalue%sysevalf(6.&&pvaldigits&z)))||'^{super $}'
                        %end;
                        %else %if %qupcase(%superq(plotpval&z))=WALD and ^(%sysevalf(%qupcase(%superq(method&z))=CIF,boolean) and &sysver < 9.4) %then %do;
                            strip(put((select probchisq from _t3
                            where upcase(effect)=upcase(strip("_class_"))),pvalue%sysevalf(6.&&pvaldigits&z)))||'^{super +}'
                        %end;
                        %else %if %qupcase(%superq(plotpval&z))=LOGRANK %then %do;
                            strip(put((select probchisq from _ltest),pvalue%sysevalf(6.&&pvaldigits&z)))||'^{super *}'
                        %end;
                        %else %if %qupcase(%superq(plotpval&z))=WILCOXON %then %do;
                            strip(put((select probchisq from _ltest),pvalue%sysevalf(6.&&pvaldigits&z)))||'^{super @}'
                        %end;
                        %else %if %qupcase(%superq(plotpval&z))=GRAY %then %do;
                            strip(put((select probchisq from _ltest),pvalue%sysevalf(6.&&pvaldigits&z)))||'^{super G}'
                        %end;
                        %else %do;
                            ''
                        %end;
                        %if %sysevalf(%superq(classcov&z)^=,boolean) or %sysevalf(%superq(contcov&z)^=,boolean) %then %do;
                            ,pvalmv=%if %qupcase(%superq(plotpvalmv&z))=LR %then %do;
                                        strip(put((select problrchisq from _t3mv
                                        where upcase(effect)=upcase(strip("_class_"))),pvalue%sysevalf(6.&&pvaldigits&z)))||'^{super #}'
                                    %end;
                                    %else %if %qupcase(%superq(plotpvalmv&z))=SCORE %then %do;
                                        strip(put((select probscorechisq from _t3mv
                                        where upcase(effect)=upcase(strip("_class_"))),pvalue%sysevalf(6.&&pvaldigits&z)))||'^{super $}'
                                    %end;
                                    %else %if %qupcase(%superq(plotpvalmv&z))=WALD and ^(%sysevalf(%qupcase(%superq(method&z))=CIF,boolean) and &sysver < 9.4)  %then %do;
                                        strip(put((select probchisq from _t3mv
                                        where upcase(effect)=upcase(strip("_class_"))),pvalue%sysevalf(6.&&pvaldigits&z)))||'^{super +}'
                                    %end;
                                    %else %do;
                                        ''
                                    %end;
                        %end; 
                %end;
                /*Inserts 1 row for each level of class variable*/ 
                %if %sysevalf(%superq(class&z)=,boolean)=0 %then %do i = 1 %to %superq(nclass_&z);
                    set modelnum=sum(&z,&_model),
                    modeltype=%if %sysevalf(%qupcase(%superq(method&z))=KM,boolean) %then %do;
                                %if %sysevalf(%superq(increase&z)=1,boolean) %then %do;
                                    1
                                %end;
                                %else %do;
                                    0
                                %end;
                              %end;
                              %else %do;
                                2
                              %end;,
                    title=strip("%superq(title&z)"),
                    footnote=strip(tranwrd("%superq(footnote&z)",'`','^n')),
                    subind=0,subtitle=strip("%superq(class_&z._&i)") ,                 
                    total=(select total from _sum where strip(_class_)=strip("%superq(class_&z._&i)")),
                    event=(select failed from _sum where strip(_class_)=strip("%superq(class_&z._&i)")),
                    ev_n=(select strip(put(failed,12.))||'/'||strip(put(total,12.)) from _sum
                            where strip(_class_)=strip("%superq(class_&z._&i)")),
                    median=(select case (estimate)
                                    when . then 'NE'
                                else strip(put(estimate, %sysevalf(12.&&mediandigits&z))) end || ' (' ||
                                case (lowerlimit)
                                    when . then 'NE'
                                else strip(put(lowerlimit, %sysevalf(12.&&mediandigits&z))) end || '-' ||
                                case (upperlimit)
                                    when . then 'NE'
                                else strip(put(upperlimit, %sysevalf(12.&&mediandigits&z))) end || ')'
                            from _quart where strip(_class_) = strip("%superq(class_&z._&i)")),
                    %if ^(%sysevalf(%qupcase(%superq(method&z))=CIF,boolean) and &sysver lt 9.4) %then %do;
                        hr = case ((select hazardratio from _parm where
                                upcase(strip(parameter))=upcase(strip("_class_")) and upcase(strip(classval0))=upcase(strip("%superq(class_&z._&i)"))))
                                when . then "%superq(refhrtext&z)"
                             else (select strip(put(hazardratio, %sysevalf(12.&&hrdigits&z)))
                             || ' (' ||
                             strip(put(hrlowercl, %sysevalf(12.&&hrdigits&z))) || '-' ||
                             strip(put(hruppercl, %sysevalf(12.&&hrdigits&z))) || ')' from _parm where
                             upcase(strip(parameter))=upcase(strip("_class_")) and upcase(classval0)=upcase(strip("%superq(class_&z._&i)"))) end,
                        covpval = case ((select hazardratio from _parm where
                                        upcase(strip(parameter))=upcase(strip("_class_")) and 
                                        upcase(strip(classval0))=upcase(strip("%superq(class_&z._&i)"))))
                                    when . then "%superq(refptext&z)"
                                else (select strip(put(probchisq,pvalue%sysevalf(6.&&pvaldigits&z)))||'^{super +}' from _parm where
                                upcase(strip(parameter))=upcase(strip("_class_")) and upcase(classval0)=upcase(strip("%superq(class_&z._&i)"))) end
                    %end;
                    %else %do;
                        hr = '',covpval=''
                    %end;
                    %if %sysevalf(%superq(classcov&z)^=,boolean) or %sysevalf(%superq(contcov&z)^=,boolean) %then %do;
                        ,eventmv=(select count(*) from _tempdsn&z where strip(_class_)=strip("%superq(class_&z._&i)") and _cens_^=%superq(cen_vl&z)
                        %if %sysevalf(%qupcase(%superq(method))=CIF,boolean) %then %do;
                            and _cens_=%superq(ev_vl)
                        %end;
                        %if %sysevalf(%superq(classcov&z)=,boolean) =0 %then %do k = 1 %to %sysfunc(countw(%superq(classcov&z),%str( )));
                            and missing(_classcov_&k)=0
                        %end;
                        %if %sysevalf(%superq(contcov&z)=,boolean) =0 %then %do k = 1 %to %sysfunc(countw(%superq(contcov&z),%str( )));
                            and missing(_contcov_&k)=0
                        %end;),
                        totalmv=(select count(*) from _tempdsn&z where strip(_class_)=strip("%superq(class_&z._&i)")
                            %if %sysevalf(%superq(classcov&z)=,boolean) =0 %then %do k = 1 %to %sysfunc(countw(%superq(classcov&z),%str( )));
                                and missing(_classcov_&k)=0
                            %end;
                            %if %sysevalf(%superq(contcov&z)=,boolean) =0 %then %do k = 1 %to %sysfunc(countw(%superq(contcov&z),%str( )));
                                and missing(_contcov_&k)=0
                            %end;),
                        ev_nmv=strip(put((select count(*) from _tempdsn&z where strip(_class_)=strip("%superq(class_&z._&i)") and _cens_^=%superq(cen_vl&z)
                            %if %sysevalf(%qupcase(%superq(method&z))=CIF,boolean) %then %do;
                                and _cens_=%superq(ev_vl&z)
                            %end;
                            %if %sysevalf(%superq(classcov&z)=,boolean) =0 %then %do k = 1 %to %sysfunc(countw(%superq(classcov&z),%str( )));
                                and missing(_classcov_&k)=0
                            %end;
                            %if %sysevalf(%superq(contcov&z)=,boolean) =0 %then %do k = 1 %to %sysfunc(countw(%superq(contcov&z),%str( )));
                                and missing(_contcov_&k)=0
                            %end;),12.))||'/'||
                            strip(put((select count(*) from _tempdsn&z where strip(_class_)=strip("%superq(class_&z._&i)")
                            %if %sysevalf(%superq(classcov&z)=,boolean) =0 %then %do k = 1 %to %sysfunc(countw(%superq(classcov&z),%str( )));
                                and missing(_classcov_&k)=0
                            %end;
                            %if %sysevalf(%superq(contcov&z)=,boolean) =0 %then %do k = 1 %to %sysfunc(countw(%superq(contcov&z),%str( )));
                                and missing(_contcov_&k)=0
                            %end;),12.)),
                        %if ^(%sysevalf(%qupcase(%superq(method&z))=CIF,boolean) and &sysver lt 9.4) %then %do;
                            hrmv = case ((select hazardratio from _parm where
                                            upcase(strip(parameter))=upcase(strip("_class_")) and 
                                            upcase(classval0)=upcase(strip("%superq(class_&z._&i)"))))
                                        when . then "%superq(refhrtext&z)"
                                   else (select strip(put(hazardratio, %sysevalf(12.&&hrdigits&z)))
                                   || ' (' ||
                                   strip(put(hrlowercl, %sysevalf(12.&&hrdigits&z))) || '-' ||
                                   strip(put(hruppercl, %sysevalf(12.&&hrdigits&z))) || ')' from _parmmv where
                                   upcase(strip(parameter))=upcase(strip("_class_")) and upcase(classval0)=upcase(strip("%superq(class_&z._&i)"))) end,
                            covpvalmv = case ((select hazardratio from _parm where
                                            upcase(strip(parameter))=upcase(strip("_class_")) and 
                                            upcase(strip(classval0))=upcase(strip("%superq(class_&z._&i)"))))
                                       when . then "%superq(refptext&z)"
                                    else (select strip(put(probchisq,pvalue%sysevalf(6.&&pvaldigits&z)))||'^{super +}' from _parmmv where
                                    upcase(strip(parameter))=upcase(strip("_class_")) and upcase(classval0)=upcase(strip("%superq(class_&z._&i)"))) end
                        %end;
                        %else %do;
                            hrmv='',covpvalmv=''
                        %end;
                    %end;
                    %if %sysevalf(%superq(timelist&z)^=,boolean) %then %do;
                        , timelist=strip("%superq(tl&i)")
                    %end;
                %end;
                ;            
        quit;
    
        /**Create a dataset for plotting**/
        data _plot_&z;
            /**Reverses Order if DESC=1**/
            %local i2;
            merge
                /*One set of columns for each class variable level*/
                %do i = 1 %to %superq(nclass_&z);
                    _surv (
                        rename=(
                            %if %sysevalf(%superq(class&z)=,boolean)=0 %then %do; cl1=cl&i._&z %end; /**Class Variable**/
                            t1=t&i._&z /**Time Variable**/
                            c1=c&i._&z /**Censor Variable**/
                            s1=s&i._&z /**Survival Estimate Variable**/
                            lcl1=lcl&i._&z /**Survival 95% lower bound Variable**/
                            ucl1=ucl&i._&z /**Survival 95% upper bound Variable**/
                            %if %sysevalf(%qupcase(%superq(method&z))=CIF,boolean) %then %do;
                                vcif=vcif&i._&z /**Standard Error for CIF Function**/
                            %end;)
                        %if %sysevalf(%superq(class&z)=,boolean)=0 %then %do;
                        /**Stratumnum is numbered by PROC LIFETEST in the order the class variables are displayed**/
                            %if %superq(desc&z)=1 %then %let i2=%sysfunc(abs(%sysfunc(sum(&i,-%superq(nclass_&z),-1))));
                            %else %let i2=&i;
                            %if %sysevalf(%superq(classorder&z)^=,boolean) and %sysevalf(%superq(class&z)=,boolean)=0 %then %do;
                                where=(stratumnum=%scan(%superq(classorder&z),&i2))
                            %end;
                            %else %do;
                                where=(stratumnum=&i2)
                            %end;
                        %end;)
                %end;
                ;
            %if %sysevalf(%superq(class&z)=,boolean)=0 %then %do i = 1 %to %superq(nclass_&z);
                cl&i._&z=strip("%superq(class_&z._&i)");
            %end;
            %else %do;
                cl1_&z=strip("%superq(classdesc&z)");
            %end;
            /**If requested to plot 1-S instead of S**/
            %if %superq(increase&z) = 1 %then %do i = 1 %to %superq(nclass_&z);
                s&i._&z = 1-s&i._&z; /**Flip Survival Estimate Variable**/
                c&i._&z = 1-c&i._&z; /**Flip Censor Marker Survival Estimate**/
                _temp_=ucl&i._&z;/**Hold UCL Value**/
                ucl&i._&z = 1-lcl&i._&z; /**Flip Survival Confidence Interval Upper Bound**/
                lcl&i._&z = 1-_temp_; /**Flip Survival Confidence Interval Lower Bound**/
            %end;
                
            label 
                %do i = 1 %to %superq(nclass_&z);
                    %if %sysevalf(%superq(class&z)=,boolean)=0 %then %do;
                        t&i._&z="Time Class Level &i: Plot &z"
                        s&i._&z="SDF Estimate Class Level &i: Plot &z"
                        lcl&i._&z="SDF 95% Lower Bound Class Level &i: Plot &z"
                        ucl&i._&z="SDF 95% Upper Bound Class Level &i: Plot &z"
                        c&i._&z="Censor Estimate Class Level &i: Plot &z"
                        cl&i._&z="Class Level &i: Plot &z"
                        %if %sysevalf(%qupcase(%superq(method&z))=CIF,boolean) %then %do;
                            vcif&i._&z="CIF Function Standard Error Class Level &i: Plot &z"
                        %end;
                    %end;
                    %else %do;
                        t&i._&z="Time: Plot &z"
                        s&i._&z="SDF Estimate: Plot &z"
                        lcl&i._&z="SDF 95% Lower Bound: Plot &z"
                        ucl&i._&z="SDF 95% Upper Bound: Plot &z"
                        c&i._&z="Censor Estimate: Plot &z"
                        cl&i._&z="Population Description &i: Plot &z"
                        %if %sysevalf(%qupcase(%superq(method&z))=CIF,boolean) %then %do;
                            vcif&i._&z="CIF Function Standard Error: Plot &z"
                        %end;
                    %end;
                %end;;
        run;
    
        /**Sets up dataset for patients at risk**/ 
        %if %sysevalf(%superq(risklist&z)=,boolean)=0 %then %do;
            %local partitle_&z;
            /**Determine if patients-at-risk header is requested**/
            %if %sysevalf(%superq(parheader&z)=%str(),boolean)=0 %then %let partitle_&z = 1;
            %else %let partitle_&z = 0;
            %local i2;
            data _riskplot;
                merge
                    /**Make one set of columns per class variable**/
                    %do i = 1 %to %superq(nclass_&z);
                        /**Reverses Order if DESC=1**/
                        %if %superq(desc&z)=1 %then %let i2=%sysfunc(abs(%sysfunc(sum(&i,-%superq(nclass_&z),-1))));
                        %else %let i2=&i;
                        _splot (rename=(
                            time=time&i._&z /**Time Variable**/ 
                            atrisk=risk&i._&z /**Number of Patients-at-Risk Variable**/)
                            /**Grab different class values depending on class order requested**/
                            /**Stratumnum is numbered by PROC LIFETEST in the order the class variables are displayed**/
                            %if %sysevalf(%superq(classorder&z)^=,boolean) and %sysevalf(%superq(class&z)=,boolean)=0 %then %do;
                                where=(stratumnum=%scan(%superq(classorder&z),&i2)))
                            %end;
                            %else %do;
                                where=(stratumnum=&i2))
                            %end;
                    %end;                    
                    ;
                /**Sets up Variables for the header in the Patients-at-Risk table when RISKLOCATION=INSIDE**/
                %if &&partitle_&z =1 %then %do;
                    if _n_ = 1 then do;
                    partitle_&z="&&parheader&z";
                    end;
                    label partitle_&z="PAR Subheader: Plot &z";
                %end;
                
                %do i = 1 %to %superq(nclass_&z);
                    length atrisk&i._&z $12.;
                    /**To be used in a BLOCKPLOT Value option**/
                    atrisk&i._&z=strip(put(risk&i._&z, 12.));
                    drop risk&i._&z;
                %end;
                label
                    %do i = 1 %to %superq(nclass_&z);
                        %if %sysevalf(%superq(class&z)=,boolean)=0 %then %do; 
                            time&i._&z="PAR Time Class Level &i: Plot &z"
                            atrisk&i._&z="PAR N Class Level &i: Plot &z"
                        %end;
                        %else %do;
                            time&i._&z="PAR Time: Plot &z"
                            atrisk&i._&z="PAR N: Plot &z"
                        %end;
                    %end;   ;                          
                drop stratumnum tatrisk;   
            run;
            
            /**Merges patients-at-risk dataset with plot datset**/
            data _plot_&z;
                merge _plot_&z _riskplot;
            run;
        %end;
        
        /**Reference lines **/
        %if %sysevalf(%superq(reflines&z)^=,boolean) %then %do;
            %if %sysevalf(%qupcase(%superq(reflines&z))=TIMEPOINTS,boolean) and 
                %sysevalf(%superq(timelist&z)^=,boolean) %then %do;    
                /**Create at table of values for the plot**/
                proc sql noprint;
                        %if %sysevalf(%qupcase(%superq(reflinemethod&z))=FULL,boolean) %then %do;
                            create table _reflines_t as
                                select distinct timelist as ref_t_&z "Reference Lines Time Coordinates &z"
                                from _timelist;
                            create table _reflines_y as
                                select distinct %superq(xmult_&z)*survival as ref_y_&z "Reference Lines Y-Coordinate Coordinates &z"
                                from _timelist;
                            data _reflines;
                                merge _reflines_t _reflines_y;
                            run;
                        %end;
                        %else %if %sysevalf(%qupcase(%superq(reflinemethod&z))=DROP,boolean) %then %do;
                            create table _reflines as
                                select timelist as ref_t_&z "Reference Lines Time Coordinates &z",
                                       %superq(xmult_&z)*SURVIVAL as ref_y_&z "Reference Line Y-Coordinate &z"
                                from _timelist;
                        %end;
                quit;
            %end;
            %else %if %sysevalf(%qupcase(%superq(reflines&z))=MEDIANS,boolean) %then %do;    
                /**Create at table of values for the plot**/
                proc sql noprint;
                    create table _reflines as
                        %if %sysevalf(%qupcase(%superq(reflinemethod&z))=FULL,boolean) %then %do;
                            select distinct estimate as ref_t_&z "Reference Lines Time Coordinates &z",
                                   %superq(xmult_&z)*0.50 as ref_y_&z "Reference Line Y-Coordinate &z"
                            from _quart;
                        %end;
                        %else %if %sysevalf(%qupcase(%superq(reflinemethod&z))=DROP,boolean) %then %do;
                            select distinct estimate as ref_t_&z "Reference Lines Time Coordinates &z",
                                   %superq(xmult_&z)*0.50 as ref_y_&z "Reference Line Y-Coordinate &z"
                            from _quart;
                        %end;
                quit;
            %end;
            %if (%sysevalf(%qupcase(%superq(reflines&z))=TIMEPOINTS,boolean) and 
                %sysevalf(%superq(timelist&z)^=,boolean)) or
                %sysevalf(%qupcase(%superq(reflines&z))=MEDIANS,boolean) %then %do; 
                /**Merges reference lines dataset with plot datset**/
                data _plot_&z;
                    merge _plot_&z _reflines;
                run;
            %end;
        %end;
    
        /**Determine which statistics are displayed in the plot**/
        /*Class Level Gridded Block*/
        %local _ndisplay_class_&z classcolumns hrcolumns;
        %let _ndisplay_class_&z=0;
        %let classcolumns=TOTAL|EVENT|MEDIAN|EV_N|N_EV|TOTALMV|EVENTMV|EV_NMV|N_EVMV;
        %let hrcolumns=HR|HRMV|COVPVAL|COVPVALMV;
        /*Model Level Gridded Block*/
        %local _ndisplay_model_&z _ndisplay_mstats_&z;
        %let _ndisplay_model_&z=0;
        %let _ndisplay_mstats_&z=0;
        
        %if %index(%qupcase(%superq(display&z)),LEGEND) > 0 %then %do;
            %let _ndisplay_class_&z=%sysevalf(&&_ndisplay_class_&z+1);
            %let _display_class&&_ndisplay_class_&z.._&z = LEGEND;
            %let _ndisplay_class_&z=%sysevalf(&&_ndisplay_class_&z+1);
            %let _display_class&&_ndisplay_class_&z.._&z = LEGEND2;
            %if %index(%qupcase(%superq(display&z)),PVAL) > 0 or 
                %index(%qupcase(%superq(display&z)),TABLECOMMENTS) > 0 or %superq(censormarkers&z)=1 %then %do;
                %let _ndisplay_model_&z=%sysevalf(&&_ndisplay_model_&z+1);
                %let _display_model&&_ndisplay_model_&z.._&z = LEGEND;
            %end;  
        %end;        
        %if %index(%qupcase(%superq(display&z)),PVAL) > 0 or %index(%qupcase(%superq(display&z)),TABLECOMMENTS) > 0 %then %do;
            %let _ndisplay_model_&z=%sysevalf(&&_ndisplay_model_&z+1);
            %let _display_model&&_ndisplay_model_&z.._&z = STATS;
        %end;
        %if %superq(censormarkers&z)=1 %then %do;
            %let _ndisplay_model_&z=%sysevalf(&&_ndisplay_model_&z+1);
            %let _display_model&&_ndisplay_model_&z.._&z = CENSORS;
        %end;
        
        %if %sysevalf(%superq(display&z)=,boolean)=0 %then %do i = 1 %to %sysfunc(countw(%superq(display&z),%str( )));
            %local _display_current;
            %let _display_current=%qupcase(%scan(%superq(display&z),&i,%str( )));
            %if %sysevalf(%superq(class&z)^=,boolean) %then %do j = 1 %to %sysfunc(countw(&hrcolumns,|));
                %if &_display_current=%scan(&hrcolumns,&j,|) %then %do;
                    %let _ndisplay_class_&z=%sysevalf(&&_ndisplay_class_&z+1);
                    %let _display_class&&_ndisplay_class_&z.._&z = %scan(&hrcolumns,&j,|);
                %end;
            %end;
            %do j = 1 %to %sysfunc(countw(&classcolumns,|));
                %if &_display_current=%scan(&classcolumns,&j,|) %then %do;
                    %let _ndisplay_class_&z=%sysevalf(&&_ndisplay_class_&z+1);
                    %let _display_class&&_ndisplay_class_&z.._&z = %scan(&classcolumns,&j,|);
                %end;
            %end;
            %if &_display_current=TIMELIST %then %do;
                %if %sysevalf(%superq(timelist&z)^=,boolean) %then %do;
                    %let _ndisplay_class_&z=%sysevalf(&&_ndisplay_class_&z+1);
                    %let _display_class&&_ndisplay_class_&z.._&z = TIMELIST;
                    %let _ndisplay_class_&z=%sysevalf(&&_ndisplay_class_&z+%superq(listtimepoints&z));
                    %if %superq(listtimepoints&z) %then %do;
                        %let _display_class&&_ndisplay_class_&z.._&z = TIMEPOINTS;
                    %end;
                %end;                            
                %if  %qupcase(%superq(risklocation&z))= TIMELIST and %sysevalf(%superq(risklist&z)^=,boolean) %then %do;
                    %let _ndisplay_class_&z=%sysevalf(&&_ndisplay_class_&z+1);
                    %let _display_class&&_ndisplay_class_&z.._&z = RISKTABLE;
                %end;
            %end; 
            %do j = 1 %to %sysfunc(countw(PVAL|PVALMV,|));
                %if &_display_current=%scan(PVAL|PVALMV,&j,|) and %sysevalf(%superq(class&z)^=,boolean) %then %do;
                    %let _ndisplay_mstats_&z=%sysevalf(&&_ndisplay_mstats_&z+1);
                    %let _display_mstats&&_ndisplay_mstats_&z.._&z = %scan(PVAL|PVALMV,&j,|);
                %end;
            %end;
            %if &_display_current=TABLECOMMENTS and %sysevalf(%superq(tablecomments&z)^=,boolean) %then %do;
                %let _ndisplay_mstats_&z=%sysevalf(&&_ndisplay_mstats_&z+1);
                %let _display_mstats&&_ndisplay_mstats_&z.._&z = TABLECOMMENTS; 
                %let _ndisplay_mstats_&z=%sysevalf(&&_ndisplay_mstats_&z-1+%sysfunc(countw(%superq(tablecomments&z),`,m)));              
            %end;
        %end;
       
        proc sql noprint;
            /**saves values of each metric to macro variables**/
            /**Hazard Ratios and Confidence Bounds**/
            %if %sysevalf(%superq(class&z)^=,boolean) %then %do;
                %local hr&z;
                select hr into :hr&z separated by '|'
                    from _temptable
                    where ^missing(hr);
                %if %sysevalf(%superq(classcov&z)^=,boolean) or %sysevalf(%superq(contcov&z)^=,boolean) %then %do;
                    %local hrmv&z;
                    select hrmv into :hrmv&z separated by '|'
                        from _temptable
                        where ^missing(hrmv);
                    /**Total number of patients**/
                    %local totalmv&z;
                    select totalmv into :totalmv&z separated by '|'
                        from _temptable
                        where ^missing(totalmv);
                    /**Total number of events**/
                    %local eventmv&z;
                    select eventmv into :eventmv&z separated by '|'
                        from _temptable
                        where ^missing(eventmv);
                    /**Formatted events/total**/
                    %local ev_nmv&z;
                    select ev_nmv into :ev_nmv&z separated by '|'
                        from _temptable
                        where ^missing(ev_nmv);
                    /**Formatted total (events)**/
                    %local ev_nmv&z;
                    select strip(put(totalmv,12.))||' ('||strip(put(eventmv,12.))||')' into :n_evmv&z separated by '|'
                        from _temptable
                        where ^missing(totalmv) and ^missing(eventmv);
                    %local pvalmv&z;                
                    select scan(pvalmv,1,'^') into :pvalmv&z
                        from _temptable
                        where ^missing(pvalmv);
                %end;          
            %end;
            /**Median time-to-events and confidence bounds**/
            %local median&z;
            select median into :median&z separated by '|'
                from _temptable
                where ^missing(median);
            /**Total number of patients**/
            %local total&z;
            select total into :total&z separated by '|'
                from _temptable
                where ^missing(total);
            /**Total number of events**/
            %local event&z;
            select event into :event&z separated by '|'
                from _temptable
                where ^missing(event);
            /**Formatted events/total**/
            %local ev_n&z;
            select ev_n into :ev_n&z separated by '|'
                from _temptable
                where ^missing(ev_n);
            /**Formatted total (events)**/
            %local ev_n&z;
            select strip(put(total,12.))||' ('||strip(put(event,12.))||')' into :n_ev&z separated by '|'
                from _temptable
                where ^missing(total) and ^missing(event);
            /**P-values**/
            %if %sysevalf(%superq(class&z)^=,boolean) %then %do;
                /**Model Level P-values**/
                %local pval&z;                
                select scan(pval,1,'^') into :pval&z
                    from _temptable
                    where ^missing(pval);
                /**Covariate Level P-values**/                
                %local covpval&z;
                select scan(covpval,1,'^') into :covpval&z separated by '|'
                    from _temptable
                    where ^missing(covpval);
                %if %sysevalf(%superq(classcov&z)^=,boolean) or %sysevalf(%superq(contcov&z)^=,boolean) %then %do;
                    %local covpvalmv&z;
                    select scan(covpvalmv,1,'^') into :covpvalmv&z separated by '|'
                        from _temptable
                        where ^missing(covpvalmv);
                %end;
            %end;
            /**Time-point estimates**/
            %local ntl_&z;
            %if %sysevalf(%superq(timelist&z)^=,boolean) %then %do;
                select count(distinct timelist)  into :ntl_&z
                    from _timelist;
                %local timelistn&z timelistv&z;
                select timelist,strip(put(timelist, best12.3)) || " %superq(timedx&z)"
                    into :timelistn&z separated by '|',:timelistv&z separated by '|'
                    from (select distinct timelist from _timelist);
                %do k = 1 %to %superq(ntl_&z);
                    %local timelist_&k._&z;
                    select 
                        case(survival)
                            when . then 'NE'
                        else strip(put(%superq(xmult_&z) *survival,%superq(tfmt_&z)))end || ' (' ||
                        case(sdf_lcl)
                            when . then 'NE'
                        else strip(put(%superq(xmult_&z) *sdf_lcl,%superq(tfmt_&z))) end || '-' ||
                        case (sdf_ucl)
                            when . then 'NE'
                        else strip(put(%superq(xmult_&z) *sdf_ucl,%superq(tfmt_&z))) end ||
                    %if %sysfunc(upcase(%superq(ytype&z))) = PCT %then %do; '%' || %end; ')'
                    into :timelist_&k._&z separated by '|'
                    from (
                        %if %sysevalf(%superq(class&z)=,boolean) %then %do;
                            select * from _timelist
                        %end;
                        %else %do i = 1 %to %superq(nclass_&z);
                            select * from _timelist
                            %if %sysevalf(%superq(class&z)=,boolean)=0 %then %do;
                                where strip(_class_)=strip("%superq(class_&z._&i)")
                            %end;
                            %if &i < %superq(nclass_&z) %then %do; OUTER UNION CORR %end;
                        %end;)
                    where timelist=%scan(%superq(timelistn&z),&k,|);         
                %end;
            %end;
            /**Patients-at-Risk inside plot summary table**/
            %if %sysevalf(%superq(timelist&z)^=,boolean) and %sysevalf(%superq(risklist&z)^=,boolean) and
                %qupcase(%superq(risklocation&z))=TIMELIST %then %do k = 1 %to %superq(ntl_&z);
                %local risklist_&k._&z;
                select strip(atrisk) into :risklist_&k._&z separated by '|'
                    from
                    (select atrisk1_&z as atrisk from _riskplot
                    where time1_&z=%scan(%superq(timelistn&z),&k,|)                        
                    %do i = 2 %to %superq(nclass_&z);
                        OUTER UNION CORR
                        select atrisk&i._&z as atrisk
                            from _riskplot
                            where time&i._&z=%scan(%superq(timelistn&z),&k,|)
                    %end;);           
            %end;
            /**Inserts values from temporary analysis summary into output dataset**/
            %if %sysevalf(%superq(out)=,boolean)=0 %then %do;
                insert into &out
                    select * from _temptable;
            %end;
            %else %do;
                insert into _summary
                select * from _temptable;
            %end;
        quit;
                
        /**Run-time Errors are sent there to delete temporary datasets before being sent to 
        errhandl, which stops the macro**/
        %errhandl2:
        proc datasets nodetails nolist;
           delete _temptable _ltest _parm _parmmv _quart _sum _surv _t3 _t3mv _reflines _reflines_t _reflines_y
                _riskplot _splot _timelist _tempdsn&z _tempcif _tempcif2 _score _variance _stat;
        quit; 
        /**If errors occurred then throw message and end macro**/
        %if &nerror_run > 0 %then %do;
            %put ERROR: &nerror_run run-time errors listed;
            %put ERROR: Macro NEWSURV will cease;           
            %goto errhandl;
        %end;
    %end;/**Ends Analysis Loop**/
    
    /**Put all model plot datasets together for final plot dataset**/
    data _plot;
        merge
            %do z = 1 %to &nmodels;
                _plot_&z
            %end; ;
    run;
    /**Delete model plot datasets**/
    proc datasets nodetails nolist;
        delete
            %do z = 1 %to &nmodels;
                _plot_&z
            %end; ;
    quit;
    
    /***Delete tails after xmax or below ymin for plotting***/
    data _plot;
        set _plot;
        
        %do z = 1 %to &nmodels;
            %do j = 1 %to %superq(nclass_&z);
                s&j._&z._lag=lag1(s&j._&z);
                lcl&j._&z._lag=lag1(lcl&j._&z);
                ucl&j._&z._lag=lag1(ucl&j._&z);
                retain flag&j._&z;
                if flag&j._&z^=1 then do;
                    if t&j._&z > %superq(xmax&z) then do;
                        t&j._&z = %superq(xmax&z);
                        s&j._&z=s&j._&z._lag;
                        lcl&j._&z=lcl&j._&z._lag;
                        ucl&j._&z=ucl&j._&z._lag;
                        flag&j._&z=1;
                    end;
                    if s&j._&z*%superq(xmult_&z) lt %superq(ymin&z) then do;
                        s&j._&z = %superq(ymin&z)/%superq(xmult_&z);
                        flag&j._&z=1;
                    end;
                end;
                else do;
                    t&j._&z = .;
                    s&j._&z = .;
                    lcl&j._&z = .;
                    ucl&j._&z = .;
                    end;
                drop flag&j._&z s&j._&z._lag lcl&j._&z._lag ucl&j._&z._lag;
            %end;
        %end;
    run;
               
    /**Creates template for Kaplan-Meier curve**/
    %if &uniformheight=1 and &nmodels > 1 and %sysfunc(find(%superq(risklocation),bottom,i))>0 %then %do;
        %local _maxrows _maxrowweights;
        %let _maxrows=%sysevalf(%superq(nclass_1)*(1+%sysevalf(%qupcase(%superq(risklabellocation1))=ABOVE,boolean))+
            %sysevalf(%superq(parheader1)^=,boolean));
        %let _maxrowweights=%superq(riskrowweights1);
        %do i = 2 %to &nmodels;
            %let _maxrows=%sysfunc(max(&_maxrows,%sysevalf(%superq(nclass_&i)*(1+%sysevalf(%qupcase(%superq(risklabellocation&i))=ABOVE,boolean))+
            %sysevalf(%superq(parheader&i)^=,boolean))));
            %let _maxrowweights=%sysfunc(max(&_maxrowweights,%superq(riskrowweights&i)));
        %end;       
    %end;
    proc template;
        define statgraph _km;
            begingraph / designheight=&height designwidth=&width 
                /**Turns the border around the plot off if border=0**/
                %if %superq(border)=0 %then %do;
                    border=false
                %end;;
                
                /**Create overall plot title**/
                %if %sysevalf(%superq(ovtitle)=,boolean)=0 %then %do i = 1 %to %sysfunc(countw(%superq(ovtitle),`,m));
                    entrytitle halign=&ovtitlealign "%scan(%superq(ovtitle),&i,`,m)" / 
                        textattrs=(weight=&ovtweight size=&ovtsize family="&ovtfamily" style=normal);
                %end;
                /**Create overall plot footnote**/
                %if %sysevalf(%superq(ovfootnote)=,boolean)=0 %then %do;
                    entryfootnote halign=&ovfootnotealign "&ovfootnote" / 
                        textattrs=(weight=&ovfnweight size=&ovfnsize family="&ovfnfamily" style=normal);
                %end;
                /**Creates outer lattice block to contain all model plots**/
                layout lattice / columns=&columns rows=&rows
                    order=&order columndatarange=DATA rowdatarange=DATA opaque=false;
                    /**Begins to fill in each cell of outer lattice block**/
                    %do z = 1 %to &nmodels;
                        /**Creates inner lattice block.  Adds a row if RISKLOCATION=BOTTOM.  Row Weights determined by RISKROWWEIGHTS**/
                        layout lattice / columns=1 columndatarange=union opaque=false 
                            %if %sysevalf(%qupcase(%superq(risklocation&z))=BOTTOM,boolean)=1 or 
                                (&uniformheight=1 and &nmodels > 1 and %sysfunc(find(%superq(risklocation),bottom,i))>0) %then %do; 
                                %if &uniformheight=1 and &nmodels > 1 and %sysfunc(find(%superq(risklocation),bottom,i))>0 %then %do;
                                    rows=%sysevalf(1+&_maxrows)
                                    rowweights=(%sysevalf(1-%sysevalf(&_maxrows*&_maxrowweights)) 
                                    %do i=1 %to %sysevalf(&_maxrows);
                                        &_maxrowweights
                                    %end;)
                                %end;
                                %else %do;
                                    rows=%sysevalf(1+
                                        (%sysevalf(%qupcase(%superq(risklabellocation&z))=ABOVE,boolean)+1)*%superq(nclass_&z)+
                                        %sysevalf(%qupcase(%superq(parheader&z))^=,boolean))
                                    rowweights=(%sysevalf(1-%sysevalf((%sysevalf(%qupcase(%superq(risklabellocation&z))=ABOVE,boolean)+1)*%superq(nclass_&z)+
                                        %sysevalf(%qupcase(%superq(parheader&z))^=,boolean))*%superq(riskrowweights&z))
                                        %do i = 1 %to %sysevalf((%sysevalf(%qupcase(%superq(risklabellocation&z))=ABOVE,boolean)+1)*%superq(nclass_&z)+
                                            %sysevalf(%qupcase(%superq(parheader&z))^=,boolean));
                                            %sysevalf(%superq(riskrowweights&z))
                                        %end;)
                                %end;
                                rowgutter=0 
                            %end;
                            %else %do; rows=1 %end;;
                        
                            /**Creates footnotes at bottom of inner lattice block**/
                            %if %sysevalf(%superq(footnote&z)=,boolean)=0 %then %do;
                                /**SIDEBAR block extends entire bottom length of inner lattice block**/
                                sidebar / align=bottom;
                                    /**Layout Gridded allows multiple rows of ENTRY statements within one SIDEBAR**/
                                    layout gridded / rows=%sysfunc(countw(%superq(footnote&z),`,m)) border=false;
                                        %do i = 1 %to %sysfunc(countw(%superq(footnote&z),`,m));
                                            entry halign=%superq(footnotealign&z) "%scan(%superq(footnote&z),&i,`,m)" /
                                                textattrs=(weight=%superq(fnweight&z) size=%superq(fnsize&z) family="%superq(fnfamily&z)");
                                        %end;
                                    endlayout;
                                endsidebar;
                            %end;
                            
                            /**Creates a SIDEBAR block in the plot layout block to allow for individual model titles**/
                            sidebar /align=top;
                                %if %sysevalf(%superq(title&z)=,boolean)=0 %then %do;
                                    layout gridded / rows=%sysfunc(countw(%superq(title&z),`,m)) border=false;
                                        %do i = 1 %to %sysfunc(countw(%superq(title&z),`,m));
                                            entry halign=%superq(titlealign&z) "%scan(%superq(title&z),&i,`,m)" / 
                                                textattrs=(weight=%superq(tweight&z) size=%superq(tsize&z) family="%superq(tfamily&z)");
                                        %end;
                                    endlayout;
                                %end;
                            endsidebar;
                            /**Sets up axes for the Kaplan-Meier Curves**/
                            layout overlay /  %if &showwalls=1 %then %do;
                                    walldisplay=(outline)
                                %end;
                                %else %do;
                                    walldisplay=none
                                %end;
                                /**Y-Axis**/
                                yaxisopts=(
                                    display=(line ticks tickvalues label) 
                                    label="%superq(ylabel&z)" labelattrs=(size=%superq(lsize&z) weight=%superq(lweight&z) family="%superq(lfamily&z)")
                                    type=linear tickvalueattrs=(size=%superq(ytickvalsize&z) weight=%superq(ytickvalweight&z) family="%superq(ytickvalfamily&z)")
                                    /**Offset creates space at the top or bottom of the window that the plot cannot use, proportion from 0 to 1**/
                                    %if %sysevalf(%superq(ymaxoffset&z)^=,boolean) %then %do; offsetmax=%superq(ymaxoffset&z)%end;
                                    %if %sysevalf(%superq(yminoffset&z)^=,boolean) %then %do; offsetmin=%superq(yminoffset&z) %end;   
                                    linearopts=(tickvaluesequence=(start=%superq(ymin&z) end=%superq(ymax&z) increment=%superq(yincrement&z))
                                    /**VIEWMAX and VIEWMIN are also required to show the desired range**/
                                    viewmin=%superq(ymin&z) viewmax=%superq(ymax&z)))
                                
                                /**X-Axis**/

                                xaxisopts=(display=(line ticks tickvalues label)  label="&&xlabel&z"
                                    type=linear labelattrs=(size=%superq(lsize&z) weight=%superq(lweight&z) family="%superq(lfamily&z)")
                                    tickvalueattrs=(size=%superq(xtickvalsize&z) weight=%superq(xtickvalweight&z) family="%superq(xtickvalfamily&z)")               
                                    /**Offset creates space at the top or bottom of the window that the plot cannot use, proportion from 0 to 1**/
                                    %if %sysevalf(%superq(xmaxoffset&z)^=,boolean) %then %do; offsetmax=%superq(xmaxoffset&z)%end;
                                    %if %sysevalf(%superq(xminoffset&z)^=,boolean) %then %do; offsetmin=%superq(xminoffset&z)%end;
                                    /**TICKVALUESEQUENCE automatically calculates tick marks**/
                                    linearopts=(tickvaluesequence=(start=%superq(xmin&z) end=%superq(xmax&z) increment=%superq(xincrement&z))
                                    /**VIEWMAX and VIEWMIN are also required to show the desired range**/
                                    viewmin=%superq(xmin&z) viewmax=%superq(xmax&z)));
                            
                                /**Confidence Bounds if flagged**/
                                %if %superq(plotci&z) = 1 or 
                                    (%superq(plotci&z)=2 and %sysevalf(%superq(class&z)=,boolean)) %then %do i = 1 %to %superq(nclass_&z); ;
                                    bandplot x=t&i._&z limitlower=eval(%superq(xmult_&z) *lcl&i._&z) 
                                        limitupper=eval(%superq(xmult_&z) *ucl&i._&z) / type=step
                                        display=(outline %if %sysevalf(%superq(plotcifill)=1,boolean) %then %do;
                                                            fill
                                                        %end;)
                                        fillattrs=(transparency=%superq(plotcifilltransparency&z) 
                                                   %if %sysfunc(countw(%superq(plotcifillcolor&z))) = 1 %then %do;
                                                      color=%superq(plotcifillcolor&z)
                                                   %end;
                                                   %else %do;
                                                       color=%scan(%superq(plotcifillcolor&z), &i)
                                                   %end;)
                                        outlineattrs=(thickness=%superq(plotcilinesize&z)
                                                      %if %sysfunc(countw(%superq(plotcilinecolor&z))) = 1 %then %do;
                                                          color=%superq(plotcilinecolor&z)
                                                      %end;
                                                      %else %do;
                                                          color=%scan(%superq(plotcilinecolor&z), &i)
                                                      %end;
                                                      %if %sysfunc(countw(%superq(plotcilinepattern&z))) = 1 %then %do;
                                                          pattern=%superq(plotcilinepattern&z)
                                                      %end;
                                                      %else %do;
                                                          pattern=%scan(%superq(plotcilinepattern&z), &i)
                                                      %end;);
                                %end;
                                /**Displays Censors if flagged**/
                                /*%if %superq(censormarkers&z) = 1 %then %do;
                                    /**creates scatterplot to use in legend, overwritten later since this is rendered first**/
                                    /**Has same points and size as the first class level*
                                    if (max(c1_&z)>.) 
                                        scatterplot x=t1_&z y=eval(%superq(xmult_&z) * c1_&z) / name="cens" legendlabel='Censor'
                                            markerattrs=(color=black symbol=circle size=&symbolsize);
                                        %do i = 2 %to %superq(nclass_&z);
                                            else if (max(c&i._&z)>.) 
                                            scatterplot x=t&i._&z y=eval(%superq(xmult_&z) * c&i._&z) / name="cens" legendlabel='Censor'
                                            markerattrs=(color=black size=&symbolsize
											%if &i=2 %then symbol=triangle;
											%if &i=3 %then symbol=square;
											%if &i=4 %then symbol=diamond;
											%if &i>4 %then symbol=plus;
											);
                                        %end;
                                        %do i = 1 %to %superq(nclass_&z);
                                    endif;
                                    %end;
                                %end;  */ 
                                
                                %if %superq(censormarkers&z) = 1 %then %do i = 1 %to %superq(nclass_&z);           
                                    /**Draws censor symbols*/
                                    /**xmult_&z is a factor based on YTYPE (PPT vs PCT)**/
                                    if (max(c&i._&z)>.)
                                        scatterplot x=t&i._&z y=eval(%superq(xmult_&z) * c&i._&z) / 
                                            markerattrs=(
                                            %if %sysfunc(countw(%superq(color&z))) = 1 %then %do;
                                                color=%superq(color&z)
                                            %end;
                                            %else %do;
                                                color=%scan(%superq(color&z), &i)
                                            %end;
											size=%superq(symbolsize&z)
											%if &i=1 %then %do; symbol=circle);%end;
											%if &i=2 %then %do; symbol=triangle);%end;
											%if &i=3 %then %do; symbol=square);%end;
											%if &i=4 %then %do; symbol=diamond);%end;
											%if &i>4 %then %do; symbol=plus);%end;
                                            
                                            
                                    endif;
                                %end;
                                
                                
                                /**The first STEPPLOT is rendered in WHITE so that is it is overwritten later**/
                                /**This is made to create a white line legend entry for the summary table later**/
                                /**Hence the name, spacer**/
                                stepplot x=t1_&z y=eval(%superq(xmult_&z) * s1_&z) / legendlabel=' '
                                    lineattrs=(thickness=0pt /*&linesize*/ color=white) name="spacer";
                                /**Generates the Kaplan-Meier Curves**/
                                %do i = 1 %to %superq(nclass_&z); 
                                    /**One STEPPLOT per class level**/
                                    /**Legend labels are added manually with ENTRY statements later**/ 
                                    stepplot x=t&i._&z y=eval(%superq(xmult_&z) * s&i._&z) / legendlabel=' '
                                        lineattrs=(thickness=&linesize
                                    %if %sysfunc(countw(%superq(color&z))) = 1 %then %do;
                                        color=%superq(color&z)
                                        %if %qupcase(%superq(pattern&z)) = AUTO %then %do;
                                            /**When all colors are the same, AUTO makes each pattern different**/
                                            pattern=&i
                                        %end;
                                        %else %do;
                                            %if %sysfunc(countw(%superq(pattern&z))) > 1 %then %do; pattern=%scan(%superq(pattern&z),&i) %end;
                                            %else %do; pattern=%superq(pattern&z) %end;
                                        %end;
                                    %end;
                                    %else  %do;
                                        %if %upcase(%superq(pattern&z)) = AUTO %then %do;
                                            /**When all colors are the different, AUTO makes each pattern solid**/
                                            pattern=solid
                                        %end;
                                        %else %do;
                                            %if %sysfunc(countw(%superq(pattern&z))) > 1 %then %do; pattern=%scan(%superq(pattern&z),&i) %end;
                                            %else %do; pattern=%superq(pattern&z) %end;
                                        %end;                            
                                        color=%scan(%superq(color&z), &i)                                        
                                    %end;
                                    ) name="plot&i._&z" /**Plot names are saved for DISCRETELEGEND statements later**/;
                                %end;
                                /**Draw X axis Reference Lines**/
                                %if (%sysevalf(%qupcase(%superq(reflines&z))=TIMEPOINTS,boolean) and 
                                    %sysevalf(%superq(timelist&z)^=,boolean)) or 
                                    %sysevalf(%qupcase(%superq(reflines&z))=MEDIANS,boolean) %then %do;  
                                    /**Full lines**/
                                    %if %sysevalf(%qupcase(%superq(reflinemethod&z))=FULL,boolean) %then %do;
                                        %if %sysevalf(%qupcase(%superq(reflineaxis))=X,boolean) or 
                                            %sysevalf(%qupcase(%superq(reflineaxis))=BOTH,boolean) %then %do;
                                            referenceline x=ref_t_&z / 
                                                lineattrs=(thickness=%superq(reflinesize&z) color=%superq(reflinecolor&z) pattern=%superq(reflinepattern&z));
                                            %end;                       
                                        %if %sysevalf(%qupcase(%superq(reflineaxis))=Y,boolean) or 
                                            %sysevalf(%qupcase(%superq(reflineaxis))=BOTH,boolean) %then %do;
                                            referenceline y=ref_y_&z / 
                                                lineattrs=(thickness=%superq(reflinesize&z) color=%superq(reflinecolor&z) pattern=%superq(reflinepattern&z));
                                        %end;
                                    %end; 
                                    /**Drop lines**/ 
                                    %else %if %sysevalf(%qupcase(%superq(reflinemethod&z))=DROP,boolean) %then %do; 
                                        %if %sysevalf(%qupcase(%superq(reflineaxis))=X,boolean) or 
                                            %sysevalf(%qupcase(%superq(reflineaxis))=BOTH,boolean) %then %do;
                                            dropline x=ref_t_&z y=ref_y_&z / 
                                                lineattrs=(thickness=%superq(reflinesize&z) color=%superq(reflinecolor&z) pattern=%superq(reflinepattern&z))
                                                dropto=X;    
                                        %end; 
                                        %if %sysevalf(%qupcase(%superq(reflineaxis))=Y,boolean) or 
                                            %sysevalf(%qupcase(%superq(reflineaxis))=BOTH,boolean) %then %do;
                                            dropline x=ref_t_&z y=ref_y_&z / 
                                                lineattrs=(thickness=%superq(reflinesize&z) color=%superq(reflinecolor&z) pattern=%superq(reflinepattern&z))
                                                dropto=Y;    
                                        %end;
                                    %end;
                                %end;/**End Reference Lines**/
                                /**Patients-at-Risk table when RISKLOCATION=INSIDE**/
                                %if %sysevalf(%superq(risklist&z)=,boolean)=0 and %qupcase(%superq(risklocation&z))=INSIDE %then %do;
                                    innermargin / align=bottom;
                                        /**Makes a header for the Patients-at-Risk table**/
                                        %if %superq(partitle_&z) =1 and &sysver ge 9.4 %then %do;
                                            blockplot x=eval(ifn(^missing(partitle_&z),%superq(xmin&z),.)) block=partitle_&z / 
                                                %if %sysevalf(%qupcase(%superq(paralign&z))=LABELS,boolean) %then %do;
                                                    display=(values label) label="%superq(parheader&z)"
                                                        labelattrs=(size=%superq(parsize&z) weight=%superq(parweight&z) family="%superq(parfamily&z)")
                                                %end;
                                                %else %do;
                                                    display=(values) valuehalign=%superq(paralign&z) 
                                                %end;
                                                valueattrs=(size=%superq(parsize&z)
                                                %if %sysevalf(%qupcase(%superq(paralign&z))=LABELS,boolean) %then %do;
                                                    color=white
                                                %end;
                                                weight=%superq(parweight&z) family="%superq(parfamily&z)");
                                        %end;
                                        /**Makes one block plot per class level**/
                                        %do i = %sysevalf(%superq(nclass_&z)*(&sysver lt 9.4)+1*(&sysver ge 9.4)) %to 
                                            %sysevalf(1*(&sysver lt 9.4)+%superq(nclass_&z)*(&sysver ge 9.4)) 
                                            %by %sysevalf(1*(&sysver ge 9.4)+-1*(&sysver lt 9.4));
                                            %if %qupcase(%superq(risklabellocation&z))=ABOVE and &sysver ge 9.4 and
                                                (%sysevalf(%superq(class&z)^=,boolean) or %sysevalf(%superq(classdesc&z)^=,boolean)) %then %do;
                                                blockplot x=eval(ifn(t&i._&z>.,%superq(xmin&z),.)) block=cl&i._&z / display=(values)
                                                    valuehalign=%superq(risklabelalign&z)
                                                    valueattrs=(size=%superq(ptabsize&z) family="%superq(ptabfamily&z)" weight=%superq(risklabelweight));
                                            %end; 
                                            blockplot x=eval(ifn(time&i._&z ge %superq(xmin&z),time&i._&z,.))
                                                block=atrisk&i._&z / repeatedvalues=TRUE
                                                %if %qupcase(%superq(risklabellocation&z))=LEFT %then %do;
                                                    display=(values label)                
                                                    label=
                                                        %if %sysevalf(%superq(class_&z._&i)^=,boolean) %then %do;
                                                            "%qtrim(%superq(class_&z._&i))"
                                                        %end;
                                                        %else %do;
                                                            " "
                                                        %end;
                                                    labelattrs=(size=%superq(ptabsize&z) weight=%superq(risklabelweight&z) family="%superq(ptabfamily&z)")
                                                %end;
                                                %else %do;
                                                    display=(values)
                                                %end;
                                                valuehalign=start
                                                valueattrs=(size=%superq(ptabsize&z) family="%superq(ptabfamily&z)"
                                                %if %superq(riskcolor&z)=1 and %sysfunc(countw(%superq(color&z))) = 1 %then %do;
                                                    color=%superq(color&z)
                                                %end;
                                                %else %if %superq(riskcolor&z)=1 %then %do;
                                                    color=%scan(%superq(color&z), &i)
                                                %end;);
                                            %if %qupcase(%superq(risklabellocation&z))=ABOVE and &sysver lt 9.4 and
                                                (%sysevalf(%superq(class&z)^=,boolean) or %sysevalf(%superq(classdesc&z)^=,boolean)) %then %do;
                                                blockplot x=eval(ifn(t&i._&z>.,%superq(xmin&z),.)) block=cl&i._&z / display=(values)
                                                valuehalign=%superq(risklabelalign&z)
                                                valueattrs=(size=%superq(ptabsize&z) family="%superq(ptabfamily&z)" weight=%superq(risklabelweight));
                                            %end;          
                                        %end;
                                        /**Makes a header for the Patients-at-Risk table**/
                                        %if %superq(partitle_&z) =1 and &sysver lt 9.4 %then %do;
                                            blockplot x=eval(ifn(^missing(partitle_&z),%superq(xmin&z),.)) block=partitle_&z / 
                                                %if %sysevalf(%qupcase(%superq(paralign&z))=LABELS,boolean) %then %do;
                                                    display=(values label) label="%superq(parheader&z)"
                                                    labelattrs=(size=%superq(parsize&z) weight=%superq(parweight&z) family="%superq(parfamily&z)")
                                                %end;
                                                %else %do;
                                                    display=(values)
                                                    valuehalign=%superq(paralign&z) 
                                                %end;
                                                valueattrs=(size=%superq(parsize&z) 
                                                %if %sysevalf(%qupcase(%superq(paralign&z))=LABELS,boolean) %then %do;
                                                    color=white
                                                %end;
                                                weight=%superq(parweight&z) family="%superq(parfamily&z)");
                                        %end;
                                    endinnermargin;                         
                                    /**Places a reference line at the minimum Y-axis value to separate the patients-at-risk table from the plot**/               
                                    %if %superq(riskdivider&z)=1 %then %do;
                                        referenceline y=%superq(ymin&z) / lineattrs=(color=%superq(riskdivcolor) pattern=%superq(riskdivstyle&z));
                                    %end;
                                %end;/**Ends Inner 
                                
                                /**Design the Statistics Summary Table**/
                                %if %superq(_ndisplay_class_&z) gt 0 or %superq(_ndisplay_model_&z)>0 %then %do; 
                                    /**Creates the outer gridded block with up to 2 rows**/ 
                                    layout gridded / rows=%sysevalf(%sysevalf(%superq(_ndisplay_class_&z)>0,boolean) + %sysevalf(%superq(_ndisplay_model_&z)>0,boolean)) columns=1 border=false
                                        location=%superq(location&z) autoalign=(%superq(autoalign&z));
                                        /**Creates the class-level gridded block**/
                                        %if %sysevalf(%superq(_ndisplay_class_&z)>0,boolean) %then %do;
                                            layout gridded / rows=%sysevalf(%superq(nclass_&z)+1) /**One row per class level**/
                                                columns=%superq(_ndisplay_class_&z) /**Determined by number of statistics called in the DISPLAY parameter**/                        
                                                border=false valign=top halign=center;
                                                /**Creates the headers for the class-level gridded block**/
                                                %local pclass_check;
                                                %do i = 1 %to %superq(_ndisplay_class_&z);
                                                    %let pclass_check=0;
                                                    %if %superq(_display_class&i._&z)=LEGEND %then %do;
                                                        /**Legend**/
                                                        layout gridded / columns=1 rows=%sysfunc(countw(%superq(legendheader&z),`,m)) border=false
                                                            halign=center valign=bottom;
                                                            %do k=1 %to %sysfunc(countw(%superq(legendheader&z),`,m));
                                                                entry halign=center "%scan(&&legendheader&z,&k,`,m)" / 
                                                                    valign=bottom textattrs=(weight=bold size=%superq(ptabsize&z) family="%superq(ptabfamily&z)");
                                                            %end;
                                                        endlayout;
                                                        /**Class Levels**/
                                                        %if %sysevalf("%superq(classdesc&z)"^="",boolean) and %sysevalf(%superq(class&z)^=,boolean) %then %do;
                                                            layout gridded / columns=1 rows=%sysfunc(countw(%superq(classdesc&z),`,m)) border=false
                                                                halign=center valign=bottom;
                                                                %do k=1 %to %sysfunc(countw(%superq(classdesc&z),`,m));
                                                                    entry halign=center "%scan(&&classdesc&z,&k,`,m)" / valign=bottom 
                                                                        textattrs=(weight=bold size=%superq(ptabsize&z) family="%superq(ptabfamily&z)");
                                                                %end;
                                                            endlayout;
                                                        %end;
                                                        %else %if %sysevalf(%superq(class&z)^=,boolean) %then %do;
                                                            layout gridded / columns=1 rows=%sysfunc(countw(%superq(label&z),`,m)) border=false
                                                                halign=center valign=bottom;
                                                                %do k=1 %to %sysfunc(countw(%superq(label&z),`,m));
                                                                    entry halign=center "%scan(&&label&z,&k,`,m)" / valign=bottom 
                                                                        textattrs=(weight=bold size=%superq(ptabsize&z) family="%superq(ptabfamily&z)");
                                                                %end;
                                                            endlayout;
                                                        %end;
                                                        %else %do;
                                                            entry halign=center " " / valign=bottom 
                                                                textattrs=(weight=bold size=%superq(ptabsize&z) family="%superq(ptabfamily&z)");
                                                        %end;
                                                        %let i = %sysevalf(&i+1);
                                                        %let pclass_check=1;  
                                                    %end;/**End Legend**/
                                                    %if %superq(pclass_check)=0 %then %do j=1 %to %sysfunc(countw(&classcolumns,|));
                                                        /**Total|Events|Medians**/
                                                        %if %superq(_display_class&i._&z)=%scan(&classcolumns,&j,|) %then %do;
                                                            %local header;
                                                            %let header=%superq(%sysfunc(compress(%superq(_display_class&i._&z)header))&z);
                                                            layout gridded / columns=1 rows=%sysfunc(countw(%superq(header),`,m)) border=false
                                                                halign=center valign=bottom;
                                                                %do k=1 %to %sysfunc(countw(%superq(header),`,m));
                                                                    entry halign=center "%scan(&header,&k,`,m)" / valign=bottom
                                                                        textattrs=(weight=bold size=%superq(ptabsize&z) family="%superq(ptabfamily&z)");
                                                                %end;
                                                            endlayout;
                                                            %let pclass_check=1;  
                                                        %end;
                                                    %end;/**Ends Total/Events/Medians**/
                                                    %if %superq(pclass_check)=0 and %sysevalf(%superq(class&z)^=,boolean) %then %do j=1 %to %sysfunc(countw(&hrcolumns,|));
                                                        /**Hazard Ratios**/
                                                        %if %superq(_display_class&i._&z)=%scan(&hrcolumns,&j,|) %then %do;
                                                            %local header;
                                                            %let header=%superq(%sysfunc(compress(%superq(_display_class&i._&z)header))&z);
                                                            layout gridded / columns=1 rows=%sysfunc(countw(%superq(header),`,m)) border=false
                                                                halign=center valign=bottom;
                                                                %do k=1 %to %sysfunc(countw(%superq(header),`,m));
                                                                entry halign=center "%scan(&header,&k,`,m)" / valign=bottom 
                                                                    textattrs=(weight=bold size=%superq(ptabsize&z) family="%superq(ptabfamily&z)");
                                                                %end;
                                                            endlayout;
                                                            %let pclass_check=1;  
                                                        %end;
                                                    %end;/**Ends Hazard Ratios**/
                                                    %if %superq(pclass_check)=0 and %superq(_display_class&i._&z)=TIMELIST %then %do;
                                                        /**Survival Time-point Estimates**/
                                                        %if %sysevalf(%superq(listtimepoints&z)=1,boolean) %then %do;
                                                            layout gridded / columns=1 rows=%sysfunc(countw(%superq(timelistheader&z),`,m)) border=false
                                                                halign=center valign=bottom;
                                                                %do k=1 %to %sysfunc(countw(%superq(timelistheader&z),`,m));
                                                                    entry halign=center "%scan(&&timelistheader&z,&k,`,m)" / valign=bottom 
                                                                        textattrs=(weight=bold size=%superq(ptabsize&z) family="%superq(ptabfamily&z)");
                                                                %end;
                                                             endlayout;
                                                        %end;
                                                        layout gridded / columns=1 rows=%sysfunc(countw(%superq(kmestheader&z),`,m)) border=false
                                                            halign=center valign=bottom;
                                                            %do k=1 %to %sysfunc(countw(%superq(kmestheader&z),`,m));
                                                                %if %sysevalf(%superq(kmestheader&z)=%str(KM Est %(95%% CI%)),boolean) and
                                                                    %sysevalf(%qupcase(%superq(method&z))=CIF,boolean) %then %let kmestheader&z=CIF Est (95% CI);
                                                                %else %if %sysevalf(%superq(kmestheader&z)=%str(KM Est %(95%% CI%)),boolean) and
                                                                    %sysevalf(%qupcase(%superq(increase&z))=1,boolean) %then %let kmestheader&z=1-KM Est (95% CI);
                                                                entry halign=center "%scan(&&kmestheader&z,&k,`,m)" / valign=bottom 
                                                                    textattrs=(weight=bold size=%superq(ptabsize&z) family="%superq(ptabfamily&z)");
                                                            %end;
                                                        endlayout;
                                                        %let i = %sysevalf(&i+%superq(listtimepoints&z));
                                                        %let pclass_check=1; 
                                                        %if  %qupcase(%superq(risklocation&z))= TIMELIST and %sysevalf(%superq(risklist&z)^=,boolean) %then %do;
                                                            /**Patients-at-Risk Table**/    
                                                            layout gridded / columns=1 rows=%sysfunc(countw(%superq(risktableheader&z),`,m)) border=false
                                                                halign=center valign=bottom;
                                                                %do k=1 %to %sysfunc(countw(%superq(risktableheader&z),`,m));
                                                                    entry halign=center "%scan(&&risktableheader&z,&k,`,m)" / valign=bottom 
                                                                        textattrs=(weight=bold size=%superq(ptabsize&z) family="%superq(ptabfamily&z)");
                                                                %end;
                                                            endlayout;                                
                                                            %let i = %sysevalf(&i+1);
                                                        %end;
                                                    %end;                                
                                                %end;
                                                
                                                /**Creates the values for the class-level gridded block**/
                                                %local c;
                                                %if %sysevalf(%superq(ntl_&z)=,boolean) %then %let ntl_&z = 1;
                                                %do c = 1 %to %superq(nclass_&z);   
                                                    %local pclass_check;
                                                    %do i = 1 %to %superq(_ndisplay_class_&z);
                                                        %let pclass_check=0;
                                                        %if %superq(_display_class&i._&z)=LEGEND %then %do;
                                                            /**Legend**/
                                                            /**Each DISCRETELEGEND statement only contains one class level of the plot**/
                                                            discretelegend "plot&c._&z" / opaque=false
                                                                %if &sysvlong >= 9.04.01M1 and %sysevalf(%superq(legendlinelength&z)^=,boolean) %then %do;
                                                                    itemsize=(linelength=%superq(legendlinelength&z))
                                                                %end;
                                                                across=1 down=1 border=false  valign=top halign=center displayclipped=true
                                                                valueattrs=(size=%superq(ptabsize&z) family="%superq(ptabfamily&z)"
                                                                %if %superq(statcolor&z)=1 and %sysfunc(countw(%superq(color&z))) = 1 %then %do;
                                                                    color=%superq(color&z)
                                                                %end;
                                                                %else %if %superq(statcolor&z)=1 %then %do;
                                                                    color=%scan(%superq(color&z), &c)
                                                                %end;);
                                                            /**Class Levels**/
                                                            entry halign=%superq(classvalalign&z) "%trim(%superq(class_&z._&c))" / valign=top
                                                                textattrs=(size=%superq(ptabsize&z) family="%superq(ptabfamily&z)"
                                                                %if %superq(statcolor&z)=1 and %sysfunc(countw(%superq(color&z))) = 1 %then %do;
                                                                   color=%superq(color&z)
                                                               %end;
                                                               %else %if %superq(statcolor&z)=1 %then %do;
                                                                   color=%scan(%superq(color&z), &c)
                                                               %end;);
                                                            %let i = %sysevalf(&i+1);
                                                            %let pclass_check=1;  
                                                        %end;
                                                        %if %superq(pclass_check)=0 %then %do j=1 %to %sysfunc(countw(&classcolumns,|));
                                                            /**Total|Events|Medians**/
                                                            %if %superq(_display_class&i._&z)=%scan(&classcolumns,&j,|) %then %do;
                                                                entry halign=center "%scan(%superq(%scan(&classcolumns,&j,|)&z),&c,|)" / valign=top
                                                                    textattrs=(size=%superq(ptabsize&z) family="%superq(ptabfamily&z)"
                                                                %if %superq(statcolor&z)=1 and %sysfunc(countw(%superq(color&z))) = 1 %then %do;
                                                                    color=%superq(color&z)
                                                                %end;
                                                                %else %if %superq(statcolor&z)=1 %then %do;
                                                                    color=%scan(%superq(color&z), &c)
                                                                %end;);
                                                                %let pclass_check=1;  
                                                            %end;
                                                        %end;
                                                        %if %superq(pclass_check)=0 and %sysevalf(%superq(class&z)^=,boolean) %then %do j=1 %to %sysfunc(countw(&hrcolumns,|));
                                                            /**Hazard Ratios**/
                                                            %if %superq(_display_class&i._&z)=%scan(&hrcolumns,&j,|) %then %do;
                                                                entry halign=center "%scan(%superq(%scan(&hrcolumns,&j,|)&z),&c,|)" / valign=top
                                                                    textattrs=(size=%superq(ptabsize&z) family="%superq(ptabfamily&z)"
                                                                    %if %superq(statcolor&z)=1 and %sysfunc(countw(%superq(color&z))) = 1 %then %do;
                                                                        color=%superq(color&z)
                                                                    %end;
                                                                    %else %if %superq(statcolor&z)=1 %then %do;
                                                                        color=%scan(%superq(color&z), &c)
                                                                    %end;);
                                                            %let pclass_check=1;  
                                                            %end;
                                                        %end;
                                                        %if %superq(pclass_check)=0 and %superq(_display_class&i._&z)=TIMELIST %then %do;
                                                            /**Survival Time-point Estimates**/                                 
                                                            %if %sysevalf(%superq(listtimepoints&z)=1,boolean) %then %do;
                                                                /*Time-point labels*/
                                                                layout gridded / columns=1 rows=%superq(ntl_&z) border=false opaque=false;
                                                                    %do k = 1 %to %superq(ntl_&z);
                                                                        entry halign=center "%scan(%superq(timelistv&z),&k,|)" / valign=top
                                                                            textattrs=(size=%superq(ptabsize&z) family="%superq(ptabfamily&z)"
                                                                            %if %superq(statcolor&z)=1 and %sysfunc(countw(%superq(color&z))) = 1 %then %do;
                                                                                color=%superq(color&z)
                                                                            %end;
                                                                            %else %if %superq(statcolor&z)=1 %then %do;
                                                                                color=%scan(%superq(color&z), &c)
                                                                            %end;);
                                                                    %end;
                                                                endlayout;
                                                            %end;
                                                            /*Survival Estimates*/
                                                            layout gridded / columns=1 rows=%superq(ntl_&z) border=false opaque=false;
                                                                %do k = 1 %to %superq(ntl_&z);
                                                                    entry halign=center "%scan(%superq(timelist_&k._&z),&c,|)" / valign=top
                                                                        textattrs=(size=%superq(ptabsize&z) family="%superq(ptabfamily&z)"
                                                                        %if %superq(statcolor&z)=1 and %sysfunc(countw(%superq(color&z))) = 1 %then %do;
                                                                            color=%superq(color&z)
                                                                        %end;
                                                                        %else %if %superq(statcolor&z)=1 %then %do;
                                                                            color=%scan(%superq(color&z), &c)
                                                                        %end;);
                                                                %end;
                                                            endlayout;
                                                            %let i = %sysevalf(&i+%superq(listtimepoints&z));
                                                            %let pclass_check=1; 
                                                            %if  %qupcase(%superq(risklocation&z))= TIMELIST and %sysevalf(%superq(risklist&z)^=,boolean) %then %do;
                                                                /**Patients-at-Risk Table**/ 
                                                                layout gridded / columns=1 rows=%superq(ntl_&z) border=false opaque=false;
                                                                    %do k = 1 %to %superq(ntl_&z);
                                                                        entry halign=center "%scan(%superq(risklist_&k._&z),&c,|)" / valign=top
                                                                            textattrs=(size=%superq(ptabsize&z) family="%superq(ptabfamily&z)"
                                                                            %if %superq(statcolor&z)=1 and %sysfunc(countw(%superq(color&z))) = 1 %then %do;
                                                                                color=%superq(color&z)
                                                                            %end;
                                                                            %else %if %superq(statcolor&z)=1 %then %do;
                                                                                color=%scan(%superq(color&z), &c)
                                                                            %end;);
                                                                    %end;
                                                                endlayout;                                   
                                                                %let i = %sysevalf(&i+1);
                                                            %end;
                                                        %end;/**Ends Time-point estimate Section**/                               
                                                    %end;/**Ends column filling loop for class-level gridded block***/
                                                %end;/**Ends row filling loop for class-level gridded block**/            
                                            endlayout;/**Ends class-level gridded block**/
                                        %end;/**Ends the class-level gridded block**/
                                        /**Creates model-level gridded block**/
                                        %if %superq(_ndisplay_model_&z)>0 %then %do;
                                            layout gridded / rows=1 columns=%superq(_ndisplay_model_&z) opaque=false
                                                border=false valign=top halign=right;
                                                %do i = 1 %to %superq(_ndisplay_model_&z);                            
                                                    /**Uses the plot that was colored white earlier to create the correct amount of space on the left**/
                                                    %if %superq(_display_model&i._&z)=LEGEND %then %do;
                                                        discretelegend "spacer" / opaque=false 
                                                            %if &sysvlong >= 9.04.01M1 and %sysevalf(%superq(legendlinelength&z)^=,boolean) %then %do;
                                                                itemsize=(linelength=%superq(legendlinelength&z))
                                                            %end;
                                                            across=1 down=1 border=false valign=top halign=left displayclipped=true
                                                            valueattrs=(size=1pt family="%superq(ptabfamily&z)");
                                                    %end;
                                                    %if %superq(_display_model&i._&z)=STATS and %sysevalf(%superq(_ndisplay_mstats_&z)>0,boolean) %then %do;
                                                        /**Print text for P-values and user entered Table Comments**/                           
                                                        layout gridded / columns=1 rows=%superq(_ndisplay_mstats_&z) valign=top halign=left
                                                            opaque=false border=false;
                                                            %do k = 1 %to %superq(_ndisplay_mstats_&z);
                                                                %if %superq(_display_mstats&k._&z)=PVAL %then %do;
                                                                    /**P-value**/
                                                                    %if %qupcase(%superq(plotpval&z))=LR %then %let plotpval&z=Likelihood-Ratio; 
                                                                    %if %qupcase(%superq(plotpval&z))=GRAY %then %let plotpval&z=Gray K-Sample Test; 
                                                                    entry halign=left
                                                                        %if %sysevalf(&&pvalheader&z=,boolean)=0 %then %do;
                                                                            "&&pvalheader&z %sysfunc(strip(%superq(pval&z)))"
                                                                        %end;
                                                                        %else %if %sysevalf(%superq(strata&z)^=,boolean) %then %do;
                                                                            "Stratified %sysfunc(propcase(%superq(plotpval&z))) P-value: %sysfunc(strip(%superq(pval&z)))"
                                                                        %end;
                                                                        %else %do;
                                                                            "%sysfunc(propcase(%superq(plotpval&z))) P-value: %sysfunc(strip(%superq(pval&z)))"
                                                                        %end; / valign=top textattrs=(size=%superq(ptabsize&z) family="%superq(ptabfamily&z)");
                                                                %end;
                                                                %if %superq(_display_mstats&k._&z)=PVALMV %then %do;
                                                                    /**P-value**/
                                                                    %if %qupcase(%superq(plotpvalmv&z))=LR %then %let plotpvalmv&z=Likelihood-Ratio; 
                                                                    entry halign=left
                                                                    %if %sysevalf(&&pvalmvheader&z=,boolean)=0 %then %do;
                                                                        "&&pvalmvheader&z %sysfunc(strip(%superq(pvalmv&z)))"
                                                                    %end;
                                                                    %else %do;
                                                                        "Adjusted %sysfunc(propcase(%superq(plotpvalmv&z))) P-value: %sysfunc(strip(%superq(pvalmv&z)))"
                                                                    %end; / valign=top textattrs=(size=%superq(ptabsize&z) family="%superq(ptabfamily&z)");
                                                                %end;
                                                                %if %superq(_display_mstats&k._&z)=TABLECOMMENTS %then %do;
                                                                    /**User-provided Table Comments**/
                                                                    %do j=1 %to %sysfunc(countw(%superq(tablecomments&z),`,m));                   
                                                                        entry halign=left "%scan(&&tablecomments&z,&j,`,m)" / valign=top border=false
                                                                            textattrs=(size=%superq(ptabsize&z) family="%superq(ptabfamily&z)");
                                                                    %end;
                                                                    %let k=%sysevalf(&k+%sysfunc(countw(%superq(tablecomments&z),`,m))-1);
                                                                %end;
                                                            %end;
                                                        endlayout;
                                                    %end;
                                                    %if %superq(_display_model&i._&z)=CENSORS %then %do;
                                                        /**Create legend statement for censor values**/
                                                        discretelegend 'cens' / border=false halign=right valign=top displayclipped=true opaque=false
                                                            %if &sysver ge 9.3 %then %do; autoitemsize=TRUE %end;
                                                            valueattrs=(size=%superq(ptabsize&z) family="%superq(ptabfamily&z)");   
                                                    %end;   
                                                %end;
                                            endlayout;/**Ends model-level gridded block**/
                                        %end;/**Ends model-level gridded block**/
                                    endlayout;/**Ends the outer gridded block**/ 
                                %end;
                            endlayout;/**Closes the LAYOUT OVERLAY**/
                            /**Creates the patients-at-risk block**/
                            /**Makes one block plot per class level**/
                            %if %sysevalf(%superq(risklist&z)=,boolean)=0 and %qupcase(%superq(risklocation&z))=BOTTOM %then %do;
                            /**Makes a header for the Patients-at-Risk table**/
                                %if %superq(partitle_&z) =1 %then %do;
                                    layout overlay / border=false walldisplay=none
                                        xaxisopts=(display=none type=linear                 
                                            /**Offset creates space at the top or bottom of the window that the plot cannot use, proportion from 0 to 1**/
                                            %if %sysevalf(%superq(xmaxoffset&z)^=,boolean) %then %do; offsetmax=%superq(xmaxoffset&z)%end;
                                            %if %sysevalf(%superq(xminoffset&z)^=,boolean) %then %do; offsetmin=%superq(xminoffset&z)%end;
                                            /**VIEWMAX and VIEWMIN are also required to show the desired range**/
                                            linearopts=(viewmin=%superq(xmin&z) viewmax=%superq(xmax&z)));
                                        blockplot x=eval(ifn(^missing(partitle_&z),%superq(xmin&z),.)) block=partitle_&z /
                                            %if %sysevalf(%qupcase(%superq(paralign&z))=LABELS,boolean) %then %do;
                                                display=(values label) label="%superq(parheader&z)"
                                                labelattrs=(size=%superq(parsize&z) weight=%superq(parweight&z) family="%superq(parfamily&z)")
                                            %end;
                                            %else %do;
                                                display=(values)
                                                valuehalign=%superq(paralign&z) 
                                            %end;
                                            valueattrs=(size=%superq(parsize&z)
                                                %if %sysevalf(%qupcase(%superq(paralign&z))=LABELS,boolean) %then %do;
                                                    color=white
                                                %end;
                                                weight=%superq(parweight&z) family="%superq(parfamily&z)");
                                    endlayout;
                                %end;
                                %do i = 1 %to %superq(nclass_&z);
                                    %if %qupcase(%superq(risklabellocation&z))=ABOVE and
                                        (%sysevalf(%superq(class&z)^=,boolean) or %sysevalf(%superq(classdesc&z)^=,boolean)) %then %do;
                                        layout overlay / border=false walldisplay=none
                                            xaxisopts=(display=none  type=linear                
                                            /**Offset creates space at the top or bottom of the window that the plot cannot use, proportion from 0 to 1**/
                                            %if %sysevalf(%superq(xmaxoffset&z)^=,boolean) %then %do; offsetmax=%superq(xmaxoffset&z)%end;
                                            %if %sysevalf(%superq(xminoffset&z)^=,boolean) %then %do; offsetmin=%superq(xminoffset&z)%end;
                                            /**VIEWMAX and VIEWMIN are also required to show the desired range**/
                                            linearopts=(viewmin=%superq(xmin&z) viewmax=%superq(xmax&z)));
                                            blockplot x=eval(ifn(t&i._&z>.,%superq(xmin&z),.)) block=cl&i._&z / display=(values)
                                                valuehalign=%superq(risklabelalign&z)
                                                valueattrs=(size=%superq(ptabsize&z) family="%superq(ptabfamily&z)" weight=%superq(risklabelweight));
                                        endlayout;
                                    %end;
                                    layout overlay / border=false walldisplay=none
                                        xaxisopts=(display=none  type=linear    
                                            /**Offset creates space at the top or bottom of the window that the plot cannot use, proportion from 0 to 1**/
                                            %if %sysevalf(%superq(xmaxoffset&z)^=,boolean) %then %do; offsetmax=%superq(xmaxoffset&z)%end;
                                            %if %sysevalf(%superq(xminoffset&z)^=,boolean) %then %do; offsetmin=%superq(xminoffset&z)%end;
                                            /**VIEWMAX and VIEWMIN are also required to show the desired range**/
                                            linearopts=(tickvaluesequence=(start=%superq(xmin&z) end=%superq(xmax&z) increment=%superq(xincrement&z)) viewmin=%superq(xmin&z) viewmax=%superq(xmax&z)));
                                            blockplot x=eval(ifn(time&i._&z ge %superq(xmin&z),time&i._&z,.)) block=atrisk&i._&z /repeatedvalues=TRUE
                                                %if %qupcase(%superq(risklabellocation&z))=LEFT %then %do;
                                                    display=(values label)                
                                                    label=
                                                        %if %sysevalf(%superq(class_&z._&i)^=,boolean) %then %do;
                                                            "%qtrim(%superq(class_&z._&i))%superq(risklabeldlm&z)"
                                                        %end;
                                                        %else %do;
                                                            " "
                                                        %end;
                                                    labelattrs=(size=%superq(ptabsize&z) weight=%superq(risklabelweight&z) family="%superq(ptabfamily&z)")
                                                %end;
                                                %else %do;
                                                    display=(values)
                                                %end;
                                                valuehalign=start
                                                valueattrs=(size=%superq(ptabsize&z) family="%superq(ptabfamily&z)"
                                                %if %superq(riskcolor&z)=1 and %sysfunc(countw(%superq(color&z))) = 1 %then %do;
                                                    color=%superq(color&z)
                                                %end;
                                                %else %if %superq(riskcolor&z)=1 %then %do;
                                                    color=%scan(%superq(color&z), &i)
                                                %end;);
                                    endlayout;
                                %end;
                            %end;     
                        endlayout;/**Ends inner lattice block**/     
                    %end;/**Ends Model-by-model loop**/
                endlayout;/**Ends outer lattice block**/  
            endgraph;
        end;
    run;   
        
    /**Turn Results and ODS back on**/
    ods select all;
    ods results;                  
    /**Creates document to save**/
    %if %sysevalf(%superq(odsfile)=,boolean)=0 %then %do;
        ods escapechar='^';
        /**Sets up DPI and ODS generated file**/
        ods &tablefmt 
            %if %qupcase(&tablefmt)=RTF %then %do; 
                file="&odsfile"
                image_dpi=&dpi startpage=NO 
			%end;
            %else %if %qupcase(&tablefmt)=HTML %then %do; 
                image_dpi=&dpi 
                %if %upcase(&sysscpl)=LINUX or %upcase(&sysscpl)=UNIX %then %do;
                    path="%substr(&odsfile,1,%sysfunc(find(&odsfile,/,-%sysfunc(length(&odsfile)))))"
                    file="%scan(&odsfile,1,/,b)"
                %end;
                %else %do;
                    path="%substr(&odsfile,1,%sysfunc(find(&odsfile,\,-%sysfunc(length(&odsfile)))))"
                    file="%scan(&odsfile,1,\,b)"
                %end;
                %if %sysevalf(%superq(gpath)=,boolean)=0 %then %do;
                    gpath="&gpath" (url=none)
                %end;
            %end;
            %else %if %qupcase(&tablefmt)=PDF %then %do; 
                dpi=&dpi startpage=NO bookmarkgen=off notoc
                file="&odsfile"
            %end;;
    %end;
    %else %do;
        ods listing image_dpi=&dpi;
    %end;
    /**Create plot if flagged**/
    %if &plot = 1 %then %do;
        /**Save image to specified location**/
        %if %sysevalf(%superq(gpath)=,boolean)=0 %then %do;
            ods listing gpath="&gpath";
        %end;
        /**Names and formats the image**/
        %if %sysevalf(%superq(plotfmt)^=,boolean) %then %do; 
            %if %qupcase(&plotfmt)=EMF %then %do;
                options printerpath='emf';
                ods graphics / imagefmt=&plotfmt;  
                %if &sysver=9.4 %then %do;
                    /**Modifies temporary registry keys to create better EMF image in 9.4**/
                    /**Taken from SAS Technical Support Martin Mincey**/
                    %local workdir;
                    %let workdir=%trim(%sysfunc(pathname(work))); 
                    /**Creates the new keys**/
                    data _null_;
                    %if %qupcase(&sysscp)=WIN %then %do; 
                        file "&workdir.\_newsurv_emf94.sasxreg";
                    %end;
                    %else %do;
                        file "&workdir./_newsurv_emf94.sasxreg";
                    %end;
                    put '[CORE\PRINTING\PRINTERS\EMF\ADVANCED]';
                    put '"Description"="Enhanced Metafile Format"';
                    put '"Metafile Type"="EMF"';
                    put '"Vector Alpha"=int:0';
                    put '"Image 32"=int:1';
                    run;    
                    %if %qupcase(&sysscp)=WIN %then %do; 
                        proc registry export="&workdir.\_newsurv_preexisting.sasxreg";/* Exports current SASUSER Keys */
                        proc registry import="&workdir.\_newsurv_emf94.sasxreg"; /* Import the new keys */
                        run;
                    %end;
                    %else %do;
                        proc registry export="&workdir./_newsurv_preexisting.sasxreg";/* Exports current SASUSER Keys */
                        proc registry import="&workdir./_newsurv_emf94.sasxreg"; /* Import the new keys */
                        run;
                    %end;
                %end;
                %else %do;
                    ods graphics / imagefmt=&plotfmt;  
                %end;
            %end;
            %else %if %qupcase(&plotfmt)=TIFF or %qupcase(&plotfmt)=TIF %then %do;
                ods graphics / imagefmt=png;    
            %end;
            %else %do;
                ods graphics / imagefmt=&plotfmt;  
            %end;          
        %end;
        %if %sysevalf(%superq(plotname)^=,boolean) %then %do; 
            ods graphics / reset=index imagename="&plotname";
        %end;  
        /**Turns on Scalable-Vector-Graphics**/
        %if &svg = 1 %then %do;
            %if %qupcase(&tablefmt) = RTF %then %do;
                ods graphics / OUTPUTFMT=EMF;
            %end;
            %else %if %qupcase(&tablefmt) = HTML %then %do;
                ods graphics / OUTPUTFMT=SVG;
            %end;
            %else %do;
                ods graphics / OUTPUTFMT=STATIC;
            %end;
        %end;
    
        /**Sets plot options**/
        ods graphics /  antialias antialiasmax=&antialiasmax scale=off width=&width height=&height ;
        /**Generates the Plot**/
        options notes;
        proc sgrender data=_plot template=_km;
        run;
        %if %qupcase(&plotfmt)=TIFF or %qupcase(&plotfmt)=TIF %then %do;
            %local _fncheck;
            options nonotes;
            %if %sysevalf(%superq(gpath)=,boolean) %then %do;
                filename nsurvpng "./&plotname..png"; 
                filename nsurvtif "./&plotname..tiff";
                data _null_;
                    x=fexist('nsurvpng');
                    call symput('_fncheck',strip(put(x,12.)));
                run;
                %if %sysevalf(%superq(_fncheck)^=1,boolean) %then %do;
                    filename nsurvpng "./&plotname.1.png"; 
                %end;
            %end;
            %else %do;
                filename nsurvpng "%sysfunc(tranwrd(&gpath./&plotname..png,//,/))"; 
                filename nsurvtif "&gpath./&plotname..tiff"; 
                data _null_;
                    x=fexist('nsurvpng');
                    call symput('_fncheck',strip(put(x,12.)));
                run;
                %if %sysevalf(%superq(_fncheck)^=1,boolean) %then %do;
                    filename nsurvpng "%sysfunc(tranwrd(&gpath./&plotname.1.png,//,/))"; 
                %end;
            %end;
            options notes;
            goptions device=&tiffdevice gsfname=nsurvtif
                xmax=&width ymax=&height 
                xpixels=%sysevalf(%sysfunc(compress(&width,abcdefghijklmnopqrstuvwxyz,i))*&dpi) 
                ypixels=%sysevalf(%sysfunc(compress(&height,abcdefghijklmnopqrstuvwxyz,i))*&dpi)
                imagestyle=fit iback=nsurvpng;
            proc gslide;
            run;
            quit; 
            data _null_;
                x=fdelete('nsurvpng');
            run;
            filename nsurvpng clear;
            filename nsurvtif clear;
        %end;
        options nonotes; 
        %if %qupcase(&sysscp)=WIN and (%qupcase(&plotfmt)=EMF or (&svg=1 and %qupcase(&tablefmt)=RTF)) %then %do;
            proc registry clearsasuser; /* Deletes the SASUSER directory */
            proc registry export="&workdir.\_newsurv_preexisting.sasxreg";/* Imports starting SASUSER Keys */
            run;
        %end;
        %else %if &sysver=9.4 and (%qupcase(&plotfmt)=EMF or (&svg=1 and %qupcase(&tablefmt)=RTF)) %then %do;
            proc registry clearsasuser; /* Deletes the SASUSER directory */
            proc registry export="&workdir./_newsurv_preexisting.sasxreg";/* Imports starting SASUSER Keys */
            run;
        %end;
    %end;
    
    /**Print out summary table**/
    %if &summary=1 %then %do;
        %local _scorecheck _wilcheck _lrcheck _logcheck _waldcheck _graycheck _multmethodcheck _multmethodlist;
        /**Check p-values for footnote purposes**/
        proc sql noprint;
            %if &tablemergepval=1 %then %do;
                update %if %sysevalf(%superq(out)=,boolean)=0 %then %do; &out %end;
                       %else %do; _summary %end;
                    set pval=strip(covpval)
                    where ^missing(covpval) and missing(pval);
                update %if %sysevalf(%superq(out)=,boolean)=0 %then %do; &out %end;
                %else %do; _summary %end;
                    set pvalmv=strip(covpvalmv)
                    where ^missing(covpvalmv) and missing(pvalmv);
            %end;
            select count(distinct modeltype) into :_multmethodcheck
                from 
                    %if %sysevalf(%superq(out)=,boolean)=0 %then %do; &out %end;
                    %else %do; _summary %end;;
            select distinct modeltype into :_multmethodlist separated by '|'
                from 
                    %if %sysevalf(%superq(out)=,boolean)=0 %then %do; &out %end;
                    %else %do; _summary %end;;
            /**Print Summary with PROC REPORT**/
            /**Determine columns to be showin in report**/                    
            /*Model Classifications*/
            %local _tndisplay_model modelcolumns;
            %let _tndisplay_model=0;
            %let modelcolumns=TITLE|FOOTNOTE;            
            /*Statistics*/
            %local _tndisplay_stat statcolumns _display_current _med_check _hr_check _tl_check _pval_check _pvalmv_check _covpval_check _covpvalmv_check;
            %let _pval_check=0;
            %let _pvalmv_check=0;
            %let _covpval_check=0;
            %let _covpvalmv_check=0;
            %let _tndisplay_stat=0;        
            %let statcolumns=TOTAL|EVENT|MEDIAN|TIMELIST|EV_N|N_EV|TOTALMV|EVENTMV|EV_NMV|N_EVMV|HR|HRMV|PVAL|PVALMV|COVPVAL|COVPVALMV;          
            /*Statistics*/
            /**Take only first entry if repeated entries are listed**/
            %let _tabledisplay=%qupcase(%scan(&tabledisplay,1,%str( )));
            %do i = 2 %to %sysfunc(countw(&tabledisplay,%str( )));
                %let _display_current=%qupcase(%scan(%superq(tabledisplay),&i,%str( )));
                %let _test=0;
                %do j = 1 %to %sysevalf(&i-1);
                    %if &_display_current=%qupcase(%scan(%superq(_tabledisplay),&j,|)) %then %let _test=1;
                %end;
                %if ^&_test %then %let _tabledisplay=&_tabledisplay|&_display_current;
            %end;           
            %do i = 1 %to %sysfunc(countw(&_tabledisplay,|));            
                %let _display_current=%qupcase(%scan(%superq(_tabledisplay),&i,|));
                %let _test=0;
                select ifn(count(*)>0,1,0) into :_test 
                    from %if %sysevalf(%superq(out)=,boolean)=0 %then %do; &out %end;
                         %else %do; _summary %end;
                    where ^missing(&_display_current);
                %if &_test=1 %then %do j = 1 %to %sysfunc(countw(&modelcolumns,|));
                    %if &_display_current=%scan(&modelcolumns,&j,|) %then %do;
                        %let _tndisplay_model=%sysevalf(&_tndisplay_model+1);
                        %let _tndisplay_model_&_tndisplay_model=&_display_current;
                    %end;
                %end;
                %if &_test=1 %then %do j = 1 %to %sysfunc(countw(&statcolumns,|));
                    %if &_display_current=%scan(&statcolumns,&j,|) %then %do;
                        %let _tndisplay_stat=%sysevalf(&_tndisplay_stat+1);
                        %let _tndisplay_stat_&_tndisplay_stat=&_display_current;
                        %if %sysevalf(%qupcase(&_display_current)=MEDIAN,boolean) %then %let _med_check=1;
                        %else %if %sysevalf(%qupcase(&_display_current)=TIMELIST,boolean) %then %let _tl_check=1;
                        %else %if %sysevalf(%qupcase(&_display_current)=PVAL,boolean) %then %let _pval_check=1;
                        %else %if %sysevalf(%qupcase(&_display_current)=COVPVAL,boolean) %then %let _covpval_check=1;
                        %else %if %sysevalf(%qupcase(&_display_current)=PVALMV,boolean) %then %let _pvalmv_check=1;
                        %else %if %sysevalf(%qupcase(&_display_current)=COVPVALMV,boolean) %then %let _covpvalmv_check=1;
                        %else %if %sysfunc(find(&_display_current,HR,i))>0 %then %let _hr_check=1;
                    %end;
                %end;
            %end;
            %if &_pval_check=1 or &_pvalmv_check=1 or &_covpval_check=1 or &_covpvalmv_check=1 %then %do;
                select
                    max(&_pval_check*pval_lrcheck,&_pvalmv_check*pvalmv_lrcheck) as lrcheck,
                    max(&_pval_check*pval_scorecheck,&_pvalmv_check*pvalmv_scorecheck) as scorecheck,
                    max(&_pval_check*pval_logcheck) as scorecheck,
                    max(&_pval_check*pval_wilcheck) as wilcheck,
                    max(&_pval_check*pval_waldcheck,&_pvalmv_check*pvalmv_waldcheck,
                        &_covpval_check*covpval_waldcheck,&_covpvalmv_check*covpvalmv_waldcheck) as waldcheck,
                    max(&_pval_check*pval_graycheck) as graycheck
                    into :_lrcheck,:_scorecheck,:_logcheck,:_wilcheck,:_waldcheck,:_graycheck
                    from (select
                    /*Univarite p-values*/
                    max(find(pval,'#')) as pval_lrcheck,
                    max(find(pval,'$')) as pval_scorecheck,
                    max(find(pval,'*')) as pval_logcheck,
                    max(find(pval,'@')) as pval_wilcheck,
                    max(find(pval,'+')) as pval_waldcheck,
                    max(find(pval,'G')) as pval_graycheck,
                    /*Univarite covariate p-values*/
                    max(find(covpval,'+')) as covpval_waldcheck,

                    /*Multivariate p-values*/
                    max(find(pvalmv,'#')) as pvalmv_lrcheck,
                    max(find(pvalmv,'$')) as pvalmv_scorecheck,
                    max(find(pvalmv,'+')) as pvalmv_waldcheck,
                    /*Multivarite covariate p-values*/
                    max(find(covpvalmv,'+')) as covpvalmv_waldcheck            
                
                    from                
                    %if %sysevalf(%superq(out)=,boolean)=0 %then %do; &out %end;
                    %else %do; _summary %end;);
             %end;
        quit;
        options notes; 
        proc report 
            %if %sysevalf(%superq(out)=,boolean)=0 %then %do; data=&out %end;
            %else %do; data=_summary %end;
            nowd split='#' missing
            /**Set up table styles**/
            style(header)={background=cxE0E0E0 borderbottomcolor=black borderbottomwidth=0.5   
					fontfamily="&tableheaderfamily" fontsize=&tableheadersize
                	fontweight=&tableheaderweight} 
            style(report)={frame=void rules=groups borderrightcolor=black borderrightwidth=0.5 
			   borderleftcolor=black borderleftwidth=0.5 
			   bordertopcolor=black bordertopwidth=0.5  
			   borderbottomcolor=black borderbottomwidth=0.5 
				cellspacing=0 cellpadding=.25}
            style(column)={fontsize=&tabledatasize fontfamily="&tabledatafamily"  
                fontweight=&tabledataweight}
            style(lines)={fontsize=&tablefootnotesize fontfamily="&tablefootnotefamily"  
                fontweight=&tablefootnoteweight};
            
            columns
                /**Adds title to top of summary table**/
                (%if %sysevalf(%superq(tabletitle)=,boolean) =0 %then %do;
                "^S={bordertopcolor=white just=l}&tabletitle"
                %end;
                modelnum /**Used for sorting and distinguishing Models**/
                    /**Titles and Footnotes are listed first to be used in compute blocks later**/    
                    %do i = 1 %to &_tndisplay_model;
                        %superq(_tndisplay_model_&i)
                    %end;
                    modeltype /**Used to determine if KM or CIF**/
                    subind subtitle /*These are always shown*/
                    /*Statistics*/
                    %do i = 1 %to &_tndisplay_stat;
                        %superq(_tndisplay_stat_&i)
                    %end;);
            
            define subind / display noprint; /**Not Printed but defined**/
            define modelnum / order noprint;/**Used to keep models in order**/
            define modeltype / order noprint;/**Used to keep models in order**/
            
            %do i = 1 %to &_tndisplay_model;
                define %superq(_tndisplay_model_&i) / order noprint;/**Used in compute blocks later**/
                compute 
                    %if %sysevalf(%qupcase(%superq(_tndisplay_model_&i))=TITLE,boolean) %then %do;
                        before 
                    %end;
                    %else %do;
                        after 
                    %end;
                    %superq(_tndisplay_model_&i) / style={fontweight=bold just=l bordertopcolor=black bordertopwidth=0.5pt};
                    line %superq(_tndisplay_model_&i) $2000.;
                endcomp;
            %end;       
            /**If KM and CIF are both used, then add subtitle to each model**/
            %if &_multmethodcheck>1 %then %do;
                compute before modeltype / style={fontweight=bold just=l};
                    length text $150.;
                    if modeltype=0 then text='Kaplan-Meier methods';
                    else if modeltype=1 then text='(1-Kaplan-Meier) methods';
                    else if modeltype=2 then text='Cumulative incidence methods';
                    line @1 text $150.;
                endcomp;
            %end;
            /**Widths are set to 30 to avoid throwing line-size errors**/
            /**This Summary Table is not designed to be viewed in the output window**/
            define subtitle / display '' width=30 style={cellwidth=2in}; /**Class level descriptions**/
            %do i =1 %to &_tndisplay_stat;
                %if %qupcase(&&_tndisplay_stat_&i)=TIMELIST %then %do;
                    define timelist / display width=30 
                        %if %sysevalf(%superq(_multmethodlist)=0,boolean) %then %do;
                            "&ttimelistheader^{style [fontweight=medium]^{super KM}}"
                        %end;
                        %else %if %sysevalf(%superq(_multmethodlist)=1,boolean) %then %do;
                            "&ttimelistheader^{style [fontweight=medium]^{super 1-KM}}"
                        %end;
                        %else %if %sysevalf(%superq(_multmethodlist)=2,boolean) %then %do;
                            "&ttimelistheader^{style [fontweight=medium]^{super CIF}}"
                        %end;
                        %else %do;
                            "&ttimelistheader"
                        %end;
                    width=20 style={cellwidth=&ttimelistwidth};
                %end;
                %else %if %qupcase(&&_tndisplay_stat_&i)=HR %then %do;
                    define hr / display width=30 "&thrheader^{style [fontweight=medium]^{super Cox}}" center
                    style={cellwidth=&thrwidth};
                %end;
                %else %if %qupcase(&&_tndisplay_stat_&i)=HRMV %then %do;
                    define hrmv / display width=30 "&thrmvheader^{style [fontweight=medium]^{super Cox}}" center
                    style={cellwidth=&thrmvwidth};
                %end;
                %else %if %qupcase(&&_tndisplay_stat_&i)=MEDIAN %then %do;
                    define median / display                         
                        %if %sysevalf(%superq(_multmethodlist)=0,boolean) %then %do;
                            "&tmedianheader^{style [fontweight=medium]^{super KM}}"
                        %end;
                        %else %if %sysevalf(%superq(_multmethodlist)=1,boolean) %then %do;
                            "&tmedianheader^{style [fontweight=medium]^{super 1-KM}}"
                        %end;
                        %else %if %sysevalf(%superq(_multmethodlist)=2,boolean) %then %do;
                            "&tmedianheader^{style [fontweight=medium]^{super CIF}}"
                        %end;
                        %else %do;
                            "&tmedianheader"
                        %end; 
                    center width=30 style={cellwidth=&tmedianwidth};
                %end;
                %else %do;
                    define %qupcase(&&_tndisplay_stat_&i) / display "%superq(%sysfunc(compress(t&&_tndisplay_stat_&i..header)))" center 
                    width=30 style={cellwidth=%superq(%sysfunc(compress(t%superq(_tndisplay_stat_&i)width)))};
                %end;
            %end;
            /**Choose the furthest right column in the dataset**/
            /**This allows other columns to be manipulated as they are all to the left of this column**/
            /**PROC REPORT does not give values to columns to the right of the currently processed columns**/
            compute &&_tndisplay_stat_&_tndisplay_stat;
                %if &tableshading=1 %then %do;
                    /**Creates alternating-shading using modulo arithmatic**/
                    if subind=1 then shade=1;
                    else shade+1;
                    if mod(shade,2)=0 then call define(_row_, 'style/merge','style={background=GREYEF');
                %end;
                /**Creates an indented list of class levels using the subind variable**/
                if subind=1 then call define('subtitle','style/merge','style={fontweight=bold}');
                else if subind=0 then call define('subtitle','style/merge','style={indent=0.12in}');
                %if %sysfunc(find(&tabledisplay,title,i))=0 and
                    %sysfunc(find(&tabledisplay,footnote,i))=0 %then %do;
                    if subind=1 then call define(_row_,'style/merge','style={bordertopcolor=black bordertopwidth=0.5pt}');
                %end;
            endcomp;
            /**Creates the overall footnotes at the bottom of the table**/
            compute after /style={bordertopcolor=black bordertopwidth=0.5pt borderbottomcolor=black
                borderbottomwidth=0.5pt};
                /**Creates footnotes with symbols based on which columns are requested with TABLEDISPLAY**/
                %if &_hr_check=1 or &_pval_check=1 or &_med_check=1 or &_tl_check=1 %then %do;
                    line @1 %if (&_tl_check=1 or &_med_check=1) and &_multmethodcheck=1 %then %do;                    
                        %if %sysfunc(find(%superq(_multmethodlist),0))>0 %then %do;
                            "^{style [fontweight=medium]^{super KM}}Kaplan-Meier method;"
                        %end;
                        %if %sysfunc(find(%superq(_multmethodlist),1))>0 %then %do;
                            "^{style [fontweight=medium]^{super 1-KM}}1-Kaplan-Meier method;"
                        %end;
                        %if %sysfunc(find(%superq(_multmethodlist),2))>0 %then %do;
                            "^{style [fontweight=medium]^{super CIF}}Cumulative incidence method;"
                        %end;
                    %end;
                    %if &_hr_check=1 %then %do; " ^{style [fontweight=medium]^{super Cox}}Cox model;" %end;
                    %if &_pval_check=1 or &_pvalmv_check=1 or &_covpval_check=1 or &_covpvalmv_check=1 %then %do;
                        %if &_lrcheck > 0 %then %do; ' ^{super #}Likelihood-ratio test;' %end;
                        %if &_scorecheck > 0 %then %do; ' ^{super $}Score test;'%end;
                        %if &_logcheck > 0 %then %do; ' ^{super *}Logrank test;'%end;
                        %if &_wilcheck > 0 %then %do; ' ^{super @}Wilcoxon test;' %end;
                        %if &_waldcheck > 0 and %sysfunc(find(&tabledisplay,pval,i))>0 %then %do; ' ^{super +}Wald Chi-Square test;' %end;
                        %if &_graycheck > 0 and %sysfunc(find(&tabledisplay,pval,i))>0 %then %do; " ^{super G}Gray's k-sample test for equality of cumulative incidence functions;" %end;
                    %end;
                %end;;
                /**Lists the table footnote**/
                %if %sysevalf(%superq(tablefootnote)=,boolean) =0 %then %do;
                    line @1 "&tablefootnote";
                %end;
            endcomp;
        run;
        options nonotes; 
    %end;   
    /**Closes the ODS file**/
    %if %sysevalf(%superq(odsfile)=,boolean)=0 %then %do;
        ods &tablefmt close;
    %end;
    /**Outputs Plot Dataset**/
    %if %sysevalf(%superq(outp)=,boolean)=0 %then %do;
        data &outp;
            set _plot;
        run;
    %end; 
        
    %errhandl:
          
    /**Reset Graphics Options**/
    ods graphics / reset=all;
    ods select all;
    ods results;   
    /**Delete temporary datasets**/
    proc datasets nolist nodetails;
        delete _temp _options _plot
            %if %sysevalf(%superq(out)=,boolean) %then %do;
                _summary
            %end; ;
    quit;  
    /**Reload previous Options**/ 
    ods path &_odspath;
    options mergenoby=&_mergenoby &_notes &_qlm;
    goptions device=&_device gsfname=&_gsfname
        xmax=&_xmax ymax=&_ymax xpixels=&_xpixels ypixels=&_ypixels imagestyle=&_imagestyle iback=&_iback;
    %put NEWSURV has finished processing, runtime: %sysfunc(putn(%sysevalf(%sysfunc(TIME())-&_starttime.),mmss.)); 
%mend;


