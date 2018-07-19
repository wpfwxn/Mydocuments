********************************************************************************;
*  PROGRAM:   Macrolist_JR.SAS

   LOCATION:  H:\My Documents\EORTC\SAS\INTRANET LIST

   PURPOSE:   To produce macro lists in HTML for Intranet from SAS dataset.

   AUTHOR:    Jérôme RAPION  (adapted from KM's program print_list)
   DATE:      21 June 2007

   SOFTWARE:  SAS v9.1

********************************************************************************;

*%INCLUDE "K:\SAS\EORTC macros\Intranet Lists\Programs\Setup.sas";
%INCLUDE "K:\SAS\EORTC macros\Intranet Lists\TEST\Setup.sas";

* DATAFILE= "K:\SAS\EORTC macros\Intranet Lists\Data\listMACROS.csv" ;

PROC IMPORT OUT= WORK.LIST 
            DATAFILE= "K:\SAS\EORTC macros\Intranet Lists\TEST\listMACROS.csv" 
            DBMS=CSV REPLACE;
     GETNAMES=YES;
     DATAROW=2; 
RUN;

DATA LIST;
	LENGTH MACRO1 $100.;
	SET LIST;
	IF SIZE > 0;
	macro1=UPCASE(scan(name,1));
	use='Y';
RUN;
%sort(DATA=LIST,VAR=macro1);

DATA EORTC_LIST;
	LENGTH MACRO1 $100.;
	SET ML.macrolist;
	MACRO1=UPCASE(MACRO);
	IF NOT(MACRO1='KMPLOT2' AND current='N');
	IF MACRO1="QOL_COMP_F" then author2="";
RUN;

%sort(DATA=EORTC_LIST,VAR=macro1);

DATA MY_EORTC_LIST;
	MERGE EORTC_LIST LIST(KEEP=MACRO1 DATEMODIF USE IN=A);
	BY MACRO1;
	IF NOT A THEN current='N';
	IF DATEMODIF=. THEN MOD=moddate;
	IF MODDATE=. AND DATEMODIF NE . THEN MOD=DATEMODIF;
	IF DATEMODIF >= MODDATE THEN MOD= DATEMODIF;

	FORMAT MOD DDMMYY10.;

RUN;

DATA testMOD;
	length ADD $3. desc_sh $60. valid $1. ;
	set MY_EORTC_LIST;

	if type='' or MACRO1 in (
	"GBLABELS" "SUMMARY" "SUMMARYB2" "YEARLYFP" "CUMULMA" "FLEMING" "GALBRAIT" "RISKDIFF" 
		"TESTF" "TESTR" "VAXTOSAS" "MAINPUT" "MAINPUTB2" "OXPLOT" "TABLESR2" "FPLABELS" "PVAL" 
		"PVALRND" "NUMERIC" "GFIRST2"	"GLAST2" "GLAST"
	) then do;
			sas8='Y';
			CURRENT=USE;
		if MACRO1 in ("FPLABELS" "PVAL" "PVALRND" "NUMERIC") then do;
			origdate=moddate;
		end;
		if MACRO1="ARIS_READ" then do;
			ADD='LC';
			origdate='04MAY2005'd;
			desc_sh="Generated SAS Datastep Code for READING EXPORT ARISG";
			type="U";
		end;
		if MACRO1="GFIRST2" then do;
			desc_sh="First value (from seq forms) (non-missing or not)";
			type="GM";
			origdate='19JUL2010'd;
		end;

		if MACRO1="GLAST2" then do;
			desc_sh="Last value (from seq forms) (non-missing or not)";
			type="GM";
			origdate='20JUL2010'd;
		end;
		if MACRO1="GLAST" then do;
			desc_sh="Last non-missing value (from seq forms)";
		end;
		if MACRO1="SUMMARY2" then do;
			ADD='JRA';
			origdate='06MAR2007'd;
			desc_sh="Summary statistics per trial";
			type="MA";
			basedon="SUMMARY";
			desc_ln="Time to event endpoints";
			notes="Originally written by Laurence Collette";
		end;

		if MACRO1="QOL_COMP_V" then do;
			ADD='MM';
			origdate='12FEB2009'd;
			desc_sh="Computes compliance tables for QOL";
			type="QL";
			basedon="QOL_COMP_F";
			desc_ln="FIXED Timepoints and VARIABLE Timepoints design";
		end;

		if MACRO1="SUMMARYB3" then do;
			ADD='JRA';
			origdate='13MAR2007'd;
			desc_sh="Summary statistics per trial";
			type="MA";
			basedon="SUMMARYB2";
			desc_ln="Binary endpoints";
			notes="Originally written by Laurence Collette";
		end;
		if MACRO1="BRYADAY" then do;
			ADD='JRA';
			origdate='05DEC2007'd;
			desc_sh="Single stage and 2-stages Bryant-Day Phase II design";
			type="D";
			notes="based on proc IML program from Jan Bogaerts. (2006)";
		end;

		if MACRO1="TESTF2" then do;
			ADD='JRA';
			origdate='05MAR2007'd;
			desc_sh="Fixed Effects model (Peto)";
			type="MA";
			basedon="TESTF";
			desc_ln="Uses output from Summary2 or SummaryB3 macro";
			notes="Originally written by Laurence Collette";
		end;

		if MACRO1="CUTOFF" then do;
			ADD='LC';
			origdate='04DEC2008'd;
			desc_sh="Macro to apply a cut-off date for SAS database";
			type="U";
			desc_ln="Until the VISTA-EXPORT with the cutoff date is released, you may use this macro, to implement the data cutoff date in your SAS programs";
		end;

		if MACRO1 in (
		"SUMMARY" "SUMMARYB2" "YEARLYFP" "CUMULMA" "FLEMING" "GALBRAIT" "RISKDIFF" "TESTF"
		"TESTR" "VAXTOSAS"  "MAINPUT" "MAINPUTB2" "OXPLOT" "TABLESR2"
		) then do;
			current='N';
		end;
		if MACRO1="CLEAN_TABLE" then do;
			ADD='JRA';
			origdate='20MAR2007'd;
			desc_sh="Macro to perform cleaning of ODS RTF Files";
			type="U";
		end;
		if MACRO1="BOOTSTRAPCOX" then do;
			ADD='CCO';
			ADD1='KVS';
			origdate='12JAN2005'd;
			desc_sh="Macro to perform Bootstrap Model Averaging";
			type="A";
		end;

		if MACRO1="BOOTSTRAPCOX" then do;
			ADD='CCO';
			ADD1='KVS';
			origdate='12JAN2005'd;
			desc_sh="Macro to perform Bootstrap Model Averaging";
			type="A";
		end;
		if MACRO1="CORRTABLE" then do;
			ADD='CCO';
			origdate='12JAN2005'd;
			desc_sh="Overview table of the correlations between the specified variables";
			type="R";
		end;
		if MACRO1="CUTPOINT" then do;
			ADD='CCO';
			origdate='12JAN2005'd;
			desc_sh="Optimal cutpoint for a binary partition of a continuous/ordinal variable to reach the lowest p-value when performing logrank test";
			type="A";
		end;
		if MACRO1="DYNAMIC_BALANCING_SIMULATION" then do;
			ADD='JBO';
			origdate='01MAY2002'd;
			desc_sh="Simulate the options available in EORTC dynamic balancing";
			type="S";
		end;
		if MACRO1="SARGENT_GOLDBERG" then do;
			ADD='JBO';
			origdate='29JUN2005'd;
			desc_sh="Simulate the probabilities of the Sargent and Goldberg Phase II design";
			type="S";
		end;
end;

	if MACRO1 in ("ALLFIXED2"	"ALLFIXEDB2"	"CLEAN_TABLE"	"FOREST2"	"FORESTB2"	
	"FPLABELS" 	"GAVG"	"GCOUNT"	"GETAGE"	"GETBSA"	"GFIRST"	"GLAST" "GFIRST2"	"GLAST2"	"GMAX"	"GMIN"
	"GSEQ"	"GSUM" "INFERENCE"	"KMPLOT2"	"MEDIANNP"	"MEDIANR"	"PCTY2"	"PVALRND"
	"PVAL"	"QOL_COMP_F"	"RECODES" "RECODE"	"SORT"	"SUMMARY2"	"SUMMARYB3"
	"TABLES2"	"TESTF2" "BRYADAY" "CIPLOT" "QOL_COMP_V"
	) then valid='Y'; 

	if MACRO1 in ("ALLFIXED2"	"ALLFIXEDB2"	"CLEAN_TABLE"	"DROP" "FOREST2"	"FORESTB2"	
	"GAVG"	"GCOUNT"	"GETAGE"	"GETBSA"	"GFIRST"	"GLAST"	"GMAX"	"GMIN"
	"GSEQ"	"GSUM" "HAZARDPLOT" "INFERENCE"	"KMPLOT2"	"PCTY2" "GFIRST2"	"GLAST2"
	"PCTY2"	"PVAL"	"QOL_COMP_F"	"RECODES" "RECODE"	"SUMMARY2"	"SUMMARYB3"
	"TABLES2"	"TESTF2" "CIPLOT" 
	) then add='JRA';

	if MACRO1 in ("QOL_COMP_V") then add1='JRA';
	if MACRO1 in ("SORT") then add='LC';

	if CURRENT ne 'Y' then valid='N';

	FORMAT ORIGDATE DDMMYY10. valid yn.;
	KEEP MOD DATEMODIF moddate MACRO MACRO1 USE CURRENT ADD ADD1 origdate sas8 desc_sh desc_ln type valid notes basedon;
RUN;

DATA ML.macrolist2010;
	length desc_sh $60. Documentation $250.;
	MERGE MY_EORTC_LIST testMOD(
	KEEP=MACRO1 ADD ADD1 CURRENT origdate sas8 desc_sh type current valid notes desc_ln desc_sh basedon);
	BY MACRO1;


	if MACRO1 in ("ALLFIXED2"	"BRYADAY"	"CIPLOT" "FOREST2"	"FORESTB2"	
	"GAVG"	"GCOUNT"	"GETAGE"	"GETBSA"	"GFIRST"	"GLAST"	 "GFIRST2"	"GLAST2"	 "GMAX"	"GMIN"
	"GSEQ"	"GSUM" "INFERENCE"	"KMPLOT2"	"MEDIANNP" "MEDIANR" "PCTY2" 
	"PVAL"	"PVALRND" "QOL_COMP_F" "QOL_COMP_V"	"RECODES" "RECODE"	"SUMMARY2"	"SUMMARYB3"
	"TABLES2"	"TESTF2" 
	) then 	Documentation="link";

	if MACRO1="ALLFIXEDB2" then do;
		Notes="";
	end;
	if MACRO1="HAZARDPLOT" then do;
		Notes="Output is created as a cgm file or png file or tif file";
	end;
	if MACRO1="TABLES2" then do;
		Notes="TABLESR2 is now removed and included into TABLES2";
	end;
	if MACRO1="ALLFIXED2" then do;
		Notes="ALLFIXEDB2 is now removed and included into ALLFIXED2";
	end;

	if MACRO1="FOREST2" then do;
		Notes="FORESTB2 is now removed and included into FOREST2";
	end;

		if MACRO1="RECODES" then do;
			basedon="";
		end;
	IF ADD ne '' THEN DO;
		IF AUTHOR1 = '' THEN DO;
			AUTHOR1=ADD;
			AUTHOR2=ADD1;
		END;
		ELSE DO;
			IF AUTHOR2 = '' THEN DO;
				AUTHOR2=ADD;
				AUTHOR3=ADD1;
			END;
			ELSE DO;
				IF AUTHOR3 = '' THEN DO;
					AUTHOR3=ADD;
					AUTHOR4=ADD1;
				END;
				ELSE AUTHOR4=ADD;
			END;
		END;
	END;
	IF MACRO="" THEN MACRO=LOWCASE(MACRO1);
	FORMAT AUTHOR4 $fauthor.;
RUN;

data Processed;
	set ML.macrolist2010;
	length Name Name2 $50 NameType $70;
	Name=upcase(Macro);
	if macro1="DYNAMIC_BALANCING_SIMULATION" then do;
	name="DYNAMIC_"||'<BR>'||"BALANCING_"||'<BR>'||"SIMULATION";
	name2='<DIV>'!!"DYNAMIC_"||'<BR>'||"BALANCING_"||'<BR>'||"SIMULATION"!!'</div>';
	end;

	if macro1="SARGENT_GOLDBERG" then do;
	name="SARGENT_"||'<BR>'||"GOLDBERG";
	name2='<DIV>'!!"SARGENT_"||'<BR>'||"GOLDBERG"!!'</div>';
	end;

	if documentation="link" then do;
		** META ANALYSIS**;
	    if macro1="ALLFIXED2" then
		documentation="<A href='\\elvis\Pccommon\UNIT\Stat\SAS Macros Documentation\META ANALYSIS\ALLFIXED2 Documentation 1.0.doc'>link</A>";
	    if macro1="FOREST2" then
		documentation="<A href='\\elvis\Pccommon\UNIT\Stat\SAS Macros Documentation\META ANALYSIS\FOREST2 Documentation 1.0.doc'>link</A>";
	    if macro1="FORESTB2" then
		documentation="<A href='\\elvis\Pccommon\UNIT\Stat\SAS Macros Documentation\META ANALYSIS\FORESTB2 Documentation 1.0.doc'>link</A>";
	    if macro1="SUMMARY2" then
		documentation="<A href='\\elvis\Pccommon\UNIT\Stat\SAS Macros Documentation\META ANALYSIS\SUMMARY2 Documentation 1.0.doc'>link</A>";
	    if macro1="SUMMARYB3" then
		documentation="<A href='\\elvis\Pccommon\UNIT\Stat\SAS Macros Documentation\META ANALYSIS\SUMMARYB3 Documentation 1.0.doc'>link</A>";
	    if macro1="TESTF2" then
		documentation="<A href='\\elvis\Pccommon\UNIT\Stat\SAS Macros Documentation\META ANALYSIS\TESTF2 Documentation 1.0.doc'>link</A>";

		** DESIGN**;
	    if macro1="BRYADAY" then
		documentation="<A href='\\elvis\Pccommon\UNIT\Stat\SAS Macros Documentation\DESIGN\BRYADAY Documentation 1.0.doc'>link</A>";

		** G MACROS**;
	    if macro1="GAVG" then
		documentation="<A href='\\elvis\Pccommon\UNIT\Stat\SAS Macros Documentation\GMACROS\GAVG Documentation 1.0.doc'>link</A>";
	    if macro1="GMAX" then
		documentation="<A href='\\elvis\Pccommon\UNIT\Stat\SAS Macros Documentation\GMACROS\GMAX Documentation 1.0.doc'>link</A>";
	    if macro1="GMIN" then
		documentation="<A href='\\elvis\Pccommon\UNIT\Stat\SAS Macros Documentation\GMACROS\GMIN Documentation 1.0.doc'>link</A>";
	    if macro1="GCOUNT" then
		documentation="<A href='\\elvis\Pccommon\UNIT\Stat\SAS Macros Documentation\GMACROS\GCOUNT Documentation 1.0.doc'>link</A>";
	    if macro1="GFIRST" then
		documentation="<A href='\\elvis\Pccommon\UNIT\Stat\SAS Macros Documentation\GMACROS\GFIRST Documentation 1.0.doc'>link</A>";
	    if macro1="GLAST" then
		documentation="<A href='\\elvis\Pccommon\UNIT\Stat\SAS Macros Documentation\GMACROS\GLAST Documentation 1.0.doc'>link</A>";
	    if macro1="GSUM" then
		documentation="<A href='\\elvis\Pccommon\UNIT\Stat\SAS Macros Documentation\GMACROS\GSUM Documentation 1.0.doc'>link</A>";
	    if macro1="GSEQ" then
		documentation="<A href='\\elvis\Pccommon\UNIT\Stat\SAS Macros Documentation\GMACROS\GSEQ Documentation 1.0.doc'>link</A>";
	    if macro1="GETAGE" then
		documentation="<A href='\\elvis\Pccommon\UNIT\Stat\SAS Macros Documentation\GMACROS\GetAge Documentation 1.0.doc'>link</A>";
	    if macro1="GETBSA" then
		documentation="<A href='\\elvis\Pccommon\UNIT\Stat\SAS Macros Documentation\GMACROS\GetBSA Documentation 1.0.doc'>link</A>";
	    if macro1="PVAL" then
		documentation="<A href='\\elvis\Pccommon\UNIT\Stat\SAS Macros Documentation\GMACROS\PVAL Documentation 1.0.doc'>link</A>";
	    if macro1="PVALRND" then
		documentation="<A href='\\elvis\Pccommon\UNIT\Stat\SAS Macros Documentation\GMACROS\PVALRND Documentation 1.0.doc'>link</A>";
	    if macro1="RECODE" then
		documentation="<A href='\\elvis\Pccommon\UNIT\Stat\SAS Macros Documentation\GMACROS\RECODE Documentation 1.0.doc'>link</A>";
	    if macro1="RECODES" then
		documentation="<A href='\\elvis\Pccommon\UNIT\Stat\SAS Macros Documentation\GMACROS\RECODES Documentation 1.0.doc'>link</A>";
	    if macro1="GFIRST2" then
		documentation="<A href='\\elvis\Pccommon\UNIT\Stat\SAS Macros Documentation\GMACROS\GFIRST2 Documentation 1.0.doc'>link</A>";
	    if macro1="GLAST2" then
		documentation="<A href='\\elvis\Pccommon\UNIT\Stat\SAS Macros Documentation\GMACROS\GLAST2 Documentation 1.0.doc'>link</A>";

		** QOL**;
	    if macro1="QOL_COMP_F" then
		documentation="<A href='\\elvis\Pccommon\UNIT\Stat\SAS Macros Documentation\QOL\QoL_comp_F Documentation 1.0.doc'>link</A>";
	    if macro1="QOL_COMP_V" then
		documentation="<A href='\\elvis\Pccommon\UNIT\Stat\SAS Macros Documentation\QOL\QoL_comp_V Documentation 1.0.doc'>link1</A>  <A href='\\elvis\Pccommon\UNIT\Stat\SAS Macros Documentation\QOL\QoL_comp_V macro_JRA_26_01_2009.ppt'>link2</A>";

		** TABLES2**;
	    if macro1="TABLES2" then
		documentation="<A href='\\elvis\Pccommon\UNIT\Stat\SAS Macros Documentation\TABLE\TABLES2  Documentation 1.0.doc'>link</A>";

		** SURVIVAL ANALYSIS**;
	    if macro1="CIPLOT" then
		documentation="<A href='\\elvis\Pccommon\UNIT\Stat\SAS Macros Documentation\SURVIVAL ANALYSIS\CIPLOT Documentation - 1.0.doc'>link</A>";
	    if macro1="INFERENCE" then
		documentation="<A href='\\elvis\Pccommon\UNIT\Stat\SAS Macros Documentation\SURVIVAL ANALYSIS\INFERENCE Documentation 1.0.doc'>link</A>";
	    if macro1="KMPLOT2" then
		documentation="<A href='\\elvis\Pccommon\UNIT\Stat\SAS Macros Documentation\SURVIVAL ANALYSIS\KMPLOT2 Documentation 1.0.doc'>link</A>";
	    if macro1="MEDIANNP" then
		documentation="<A href='\\elvis\Pccommon\UNIT\Stat\SAS Macros Documentation\SURVIVAL ANALYSIS\MedianNP Documentation 1.0.doc'>link</A>";
	    if macro1="MEDIANR" then
		documentation="<A href='\\elvis\Pccommon\UNIT\Stat\SAS Macros Documentation\SURVIVAL ANALYSIS\MedianR Documentation 1.0.doc'>link</A>";
	    if macro1="PCTY2" then
		documentation="<A href='\\elvis\Pccommon\UNIT\Stat\SAS Macros Documentation\SURVIVAL ANALYSIS\PCTY2 Documentation 1.0.doc'>link</A>";


	end;

	if macro1 not in ("DYNAMIC_BALANCING_SIMULATION" "SARGENT_GOLDBERG") then name2=name;
	NameType='<DIV>'!!trim(left(Name))||'<BR>('||trim(left(put(Type,$FType.)))||')'!!'</div>';
	length Purpose $222;
	if not missing(Desc_sh) then Purpose=Desc_Sh;

 	if not missing(Desc_Ln) or not missing(Notes) or not missing(BasedOn) 
	then Purpose='<DIV>'!!trim(left(Purpose));

	if not missing(Desc_Ln) then do;
	  if missing(Purpose) then Purpose=Desc_Ln;
	  else Purpose=trim(left(Purpose))||'<BR>Details: '||trim(left(Desc_Ln));
	end;
	if not missing(Notes) then
	  Purpose=trim(left(Purpose))||'<BR>Notes: '||trim(left(Notes));
	if not missing(BasedOn) then
	  Purpose=trim(left(Purpose))||'<BR>Basen on: '||trim(left(BasedOn));
	  if not missing(Desc_Ln) or not missing(Notes) or not missing(BasedOn) 
	then Purpose=trim(left(Purpose))!!'</div>';
	length Authors $100;
	if not missing(Author1) then Authors=put(Author1,$FAuthor.);
	if not missing(Author2) then
	  Authors='<DIV>'!!trim(left(Authors))||',<BR>'||trim(left(put(Author2,$FAuthor.)));
	if not missing(Author3) then
	  Authors=trim(left(Authors))||',<BR>'||trim(left(put(Author3,$FAuthor.)));
	if not missing(Author4) then
	  Authors=trim(left(Authors))||',<BR>'||trim(left(put(Author4,$FAuthor.)));
	  if not missing(Author2) then Authors=trim(left(Authors))!!'</div>';
	keep Name name2 Type NameType Current Purpose Authors OrigDate Mod valid Documentation;
	format OrigDate Mod date9.;
	label Name='Macro' Name2='Macro'
	      NameType='<DIV>Macro<BR>(Category)</div>'
	      Purpose='Purpose'
	      Authors='Authors/Modifiers'
	      OrigDate='Original Date'
	      Mod='Last Modified'
		  valid="Valid."
		  Documentation="Doc.";
run;

options linesize=240 missing=' ';
ods escapechar='^';
ods listing close;
footnote;

* List macros currently in use (alphabetically);

proc template;                                                                
   define style mystyle;                                               
      parent = styles.journal;                                                
      replace fonts /                                                         
         'SASTitleFont' = ("Arial, Helvetica, sans-serif",2,Bold Italic)      
         'TitleFont2' = ("Verdana, Helvetica, Helv",5,Bold Italic)        
         'TitleFont' = ("Verdana, Helvetica, Helv",5,Bold Italic)         
         'StrongFont' = ("Arial, Helvetica, sans-serif",2,Bold Italic)        
         'EmphasisFont' = ("Arial, Helvetica, sans-serif",2,Italic)           
         'FixedEmphasisFont' = ("Courier New, Courier, monospace",2,Italic)   
         'FixedStrongFont' = ("Courier New, Courier, monospace",2,Bold)       
         'FixedHeadingFont' = ("Courier New, Courier, monospace",2,Bold)      
         'FixedFont' = ("Courier New, Courier, monospace",2)                  
         'headingEmphasisFont' = ("Arial, Helvetica, sans-serif",2,Bold Italic
         )                                                                    
         'headingFont' = ("Verdana, Helvetica",2,Bold)              
         'docFont' = ("Verdana, Helvetica, Helv",2);                      
       replace TitlesAndFooters from Container /                               
         font = Fonts('TitleFont2')                                           
         foreground = colors('fg');   
    replace Data from Cell /                                                
         font = Fonts('DocFont')                                              
         foreground = colors('fg3')                                           
         background = cxFFFFFF  ;
   replace Pages from Document /                                           
         bullet = "decimal"                                                   
         tagattr = " onload=""if (msie4 == 1)expandAll()"""                   
         pagebreakhtml = html('break')                                        
         background = cxFFFFFF                                           
         rightmargin = 8                                                      
         leftmargin = 8;                
      replace SystemTitle from TitlesAndFooters /                             
         font = Fonts('TitleFont'); 
                                          
   end;                                                                       
run;


proc sort data=Processed out=List;
by NameType;
where Current='Y';
run;

options pageno=1;
ods html file="K:\SAS\EORTC macros\Intranet Lists\Test\CurrentMacros.htm" style=mystyle;
title1 h=4  'EORTC MACROS Currently in Use';
proc print data=List label noobs
           style (header) = header [just=l]
           style (data) = data [just=l];
var NameType Purpose Authors OrigDate Mod valid Documentation;
run;
title1;
ods html close;


* List macros currently in use (by category);

proc sort data=Processed out=List;
by Type Name;
where Current='Y';
run;

options pageno=1;
ods html file="K:\SAS\EORTC macros\Intranet Lists\Test\CurrentMacrosByCategory.htm" style=mystyle;
title1 h=4  'EORTC MACROS Currently in Use by Category';
proc print data=List label noobs
           style (header) = header [just=l]
           style (data) = data [just=l];
by Type;
id Type;
var Name2 Purpose Authors OrigDate Mod valid Documentation;
run;
title1;
ods html close;

* List macros no longer in use;

proc sort data=Processed out=List;
by NameType;
where Current^='Y';
run;

options pageno=1;
ods html file="K:\SAS\EORTC macros\Intranet Lists\Test\PastMacros.htm" style=mystyle;
title1 h=4 'EORTC MACROS No Longer in Use';
proc print data=List label noobs
           style (header) = header [just=l]
           style (data) = data [just=l];
var NameType Purpose Authors OrigDate Mod valid;
run;
title1;
ods html close;

options linesize=81;
ods listing;


