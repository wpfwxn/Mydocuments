********************************************************************************;
*  PROGRAM:   Macrolist_JR.SAS

   LOCATION:  H:\My Documents\EORTC\SAS\INTRANET LIST

   PURPOSE:   To produce macro lists in HTML for Intranet from SAS dataset.

   AUTHOR:    Jérôme RAPION  (adapted from KM's program print_list)
   DATE:      21 June 2007

   SOFTWARE:  SAS v9.1

********************************************************************************;

%INCLUDE "K:\SAS\EORTC macros\Intranet Lists\Programs\Setup.sas";

PROC IMPORT OUT= WORK.LIST 
            DATAFILE= "K:\SAS\EORTC macros\Intranet Lists\Data\listMACROS.csv" 
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
RUN;

%sort(DATA=EORTC_LIST,VAR=macro1);

DATA MY_EORTC_LIST;
	MERGE EORTC_LIST LIST(KEEP=MACRO1 DATEMODIF USE);
	BY MACRO1;

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
		"PVALRND" "NUMERIC" 
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
		if MACRO1="SUMMARY2" then do;
			ADD='JRA';
			origdate='06MAR2007'd;
			desc_sh="Summary statistics per trial";
			type="MA";
			basedon="SUMMARY";
			desc_ln="Time to event endpoints";
			notes="Originally written by LAurence Collette";
		end;

		if MACRO1="SUMMARYB3" then do;
			ADD='JRA';
			origdate='13MAR2007'd;
			desc_sh="Summary statistics per trial";
			type="MA";
			basedon="SUMMARYB2";
			desc_ln="Binary endpoints";
			notes="Originally written by LAurence Collette";
		end;


		if MACRO1="TESTF2" then do;
			ADD='JRA';
			origdate='05MAR2007'd;
			desc_sh="Fixed Effects model (Peto)";
			type="MA";
			basedon="TESTF";
			desc_ln="Uses output from Summary2 or SummaryB3 macro";
			notes="Originally written by LAurence Collette";
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
	"FPLABELS" 	"GAVG"	"GCOUNT"	"GETAGE"	"GETBSA"	"GFIRST"	"GLAST"	"GMAX"	"GMIN"
	"GSEQ"	"GSUM" "INFERENCE"	"KMPLOT2"	"MEDIANNP"	"MEDIANR"	"PCTY2"	"PVALRND"
	"PVAL"	"QOL_COMP_F"	"RECODES" "RECODE"	"SORT"	"SUMMARY2"	"SUMMARYB3"
	"TABLES2"	"TESTF2"
	) then valid='Y';

	if MACRO1 in ("ALLFIXED2"	"ALLFIXEDB2"	"CLEAN_TABLE"	"DROP" "FOREST2"	"FORESTB2"	
	"GAVG"	"GCOUNT"	"GETAGE"	"GETBSA"	"GFIRST"	"GLAST"	"GMAX"	"GMIN"
	"GSEQ"	"GSUM" "HAZARDPLOT" "INFERENCE"	"KMPLOT2"	"PCTY2" 
	"PCTY2"	"PVAL"	"QOL_COMP_F"	"RECODES" "RECODE"	"SUMMARY2"	"SUMMARYB3"
	"TABLES2"	"TESTF2"
	) then add='JRA';

	if CURRENT ne 'Y' then valid='N';

	FORMAT ORIGDATE DDMMYY10. valid yn.;
	KEEP MOD DATEMODIF moddate MACRO MACRO1 USE CURRENT ADD ADD1 origdate sas8 desc_sh type valid;
RUN;

DATA ML.macrolist2007;
	length desc_sh $60. ;
	MERGE MY_EORTC_LIST testMOD(
	KEEP=MACRO1 ADD ADD1 CURRENT origdate sas8 desc_sh type current valid);
	BY MACRO1;

	if MACRO1="ALLFIXEDB2" then do;
		Notes="";
	end;
	if MACRO1="HAZARDPLOT" then do;
		Notes="Output is created as a cgm file or png file or tif file";
	end;
	if MACRO1="TABLES2" then do;
		Notes="TABLESR2 is now removed and included into TABLES2";
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
	set ML.macrolist2007;
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
	keep Name name2 Type NameType Current Purpose Authors OrigDate Mod valid;
	format OrigDate Mod date9.;
	label Name='Macro' Name2='Macro'
	      NameType='<DIV>Macro<BR>(Category)</div>'
	      Purpose='Purpose'
	      Authors='Authors/Modifiers'
	      OrigDate='Original Date'
	      Mod='Last Modified'
		  valid="Valid.";
run;

options linesize=240 missing=' ';
ods escapechar='^';
ods listing close;
footnote;

* List macros currently in use (alphabetically);

proc template;                                                                
   define style mystyle;                                               
      parent = styles.statdoc;                                                
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
      replace SystemTitle from TitlesAndFooters /                             
         font = Fonts('TitleFont'); 
                                          
   end;                                                                       
run;


proc sort data=Processed out=List;
by NameType;
where Current='Y';
run;

options pageno=1;
ods html file="K:\SAS\EORTC macros\Intranet Lists\Output\CurrentMacros.htm" style=mystyle;
title1 h=4  'EORTC MACROS Currently in Use';
proc print data=List label noobs
           style (header) = header [just=l]
           style (data) = data [just=l];
var NameType Purpose Authors OrigDate Mod valid;
run;
title1;
ods html close;


* List macros currently in use (by category);

proc sort data=Processed out=List;
by Type Name;
where Current='Y';
run;

options pageno=1;
ods html file="K:\SAS\EORTC macros\Intranet Lists\Output\CurrentMacrosByCategory.htm" style=mystyle;
title1 h=4  'EORTC MACROS Currently in Use by Category';
proc print data=List label noobs
           style (header) = header [just=l]
           style (data) = data [just=l];
by Type;
id Type;
var Name2 Purpose Authors OrigDate Mod valid;
run;
title1;
ods html close;

* List macros no longer in use;

proc sort data=Processed out=List;
by NameType;
where Current^='Y';
run;

options pageno=1;
ods html file="K:\SAS\EORTC macros\Intranet Lists\Output\PastMacros.htm" style=mystyle;
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


