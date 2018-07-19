********************************************************************************;
*  PROGRAM:   Macrolist_JR.SAS

   LOCATION:  H:\My Documents\EORTC\SAS\INTRANET LIST

   PURPOSE:   To produce macro lists in HTML for Intranet from SAS dataset.

   AUTHOR:    Jérôme RAPION  (adapted from KM's program print_list)
   DATE:      31 March 2014

   SOFTWARE:  SAS v9.4

********************************************************************************;

*%INCLUDE "K:\SAS\EORTC macros\Intranet Lists\Programs\Setup.sas";
%INCLUDE "K:\SAS\EORTC macros\Intranet Lists\Programs\Setup.sas";

* DATAFILE= "K:\SAS\EORTC macros\Intranet Lists\Data\listMACROS.csv" ;

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
	IF MACRO1="QOL_COMP_F" then author2="";

	** JRA, Feb 2013;
	IF MACRO1="HAZARDPLOT" then do;
	desc_sh="Plots the hazard function vs time";
    desc_ln="Possibility to generate two plots: 1.Hazard Fct by Intervals vs time 2.plot of Epanechnikov Kernel-Smoothed Hazard Fct vs time";
	notes=" ";
	author1='JRA';
	end;
RUN;

%sort(DATA=EORTC_LIST,VAR=macro1);

DATA MY_EORTC_LIST;
	MERGE EORTC_LIST LIST(KEEP=MACRO1 DATEMODIF USE IN=A);
	BY MACRO1;
	IF NOT A THEN current='N';
	IF DATEMODIF=. THEN MOD=moddate;
	IF MODDATE=. AND DATEMODIF NE . THEN MOD=DATEMODIF;
	IF DATEMODIF >= MODDATE THEN MOD= DATEMODIF;

	IF MACRO1="EORTC" then delete;

*	if moddate < origdate then moddate=origdate;

	FORMAT MOD DDMMYY10.;


RUN;

DATA testMOD;
	length ADD $3. desc_sh $60. valid $1. ;
	set MY_EORTC_LIST;

	if type='' or MACRO1 in (
	"GBLABELS" "SUMMARY" "SUMMARYB2" "YEARLYFP" "CUMULMA" "FLEMING" "GALBRAIT" "RISKDIFF" 
		"TESTF" "TESTR" "VAXTOSAS" "MAINPUT" "MAINPUTB2" "OXPLOT" "TABLESR2" "FPLABELS" "PVAL" 
		"PVALRND" "NUMERIC" "GFIRST2"	"GLAST2" "GLAST" "CINDEX2" "LAB_TOX" "CIPLOT2"
		"INPUT_UNIT" "PHREGTEMPLATE"
					  "MV_GEN"
					  "CONVERT_UNIT"
					  "INPUT_NORMALS"
					  "MV_LIST_GEN"
					  "RECOGNITION"
					  "LABLIST_GEN"
					  "COMPUTE_CTC_GRADE"
					  "DETECT_PROBLEMS" "GRAYTEST" "PEPEMORI" "FINEANDGRAY" "INFERENCE2"

	) then do;
			sas8='Y';
			CURRENT=USE;

		if MACRO1="INFERENCE2" then do;
			ADD='JRA';
			origdate='21AUG2013'd;
			desc_sh="Summarises survival data";
			type="A";
			notes="This macro is based on INFERENCE macro";
		end;

		if MACRO1="PHREGTEMPLATE" then do;
			ADD='JRA';
			origdate='21MAR2014'd;
			desc_sh="Tabulates results coming from PROC PHREG";
			type="A";
			notes="This macro works with PROC PHREG. It is advised to used ?phreg abbreviation before calling PHREGTEMPLATE";
		end;

		if MACRO1="PRED_FACT_SUMMARY" then do;
			ADD='JRA';
			origdate=DATEMODIF;
			desc_sh="Summarizes the results of an analysis of predictive factors";
			type="A";
			desc_ln="Sub-macro component of the big macro PRED_FACT";
			notes="Computes tests, summaries, hazard ratios and confidence intervals for the analysis of predictive factors";
		end;

		if MACRO1="PRED_FACT_FOREST" then do;
			ADD='JRA';
			origdate=DATEMODIF;
			desc_sh="Produces a Plot for predictive factors analysis";
			type="A";
			desc_ln="Sub-macro component of the big macro PRED_FACT";
			notes="Produces a sort of 'FOREST PLOT' based on results from PROC PHREG to perform an analysis of predictive factors";
		end;

		if MACRO1="PRED_FACT" then do;
			ADD='JRA';
			origdate=DATEMODIF;
			desc_sh="Computes results of predictive factors analysis and PLOT it";
			type="A";
			notes="Produces a sort of 'FOREST PLOT' based on results from PROC PHREG to perform an analysis of predictive factors";
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
		if MACRO1="CINDEX2" then do;
			desc_sh="Harrell's c-index computation";
			type="A";
			origdate='11OCT2010'd;
			ADD='JRA';
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
		if MACRO1 in ("INPUT_UNIT"
					  "MV_GEN"
					  "CONVERT_UNIT"
					  "INPUT_NORMALS"
					  "MV_LIST_GEN"
					  "RECOGNITION"
					  "LABLIST_GEN"
					  "COMPUTE_CTC_GRADE"
					  "DETECT_PROBLEMS"

		) then do;
			ADD='JRA';
			desc_sh="Handle and summarize laboratory data (Hemato. and Bioch.)";
			type="L";
		end;


		if MACRO1 in ("GRAYTEST") then do;
			ADD='JRA';
			desc_sh="Computes the Gray test";
			type="A";
			origdate='01AUG2012'd;
		end;


		if MACRO1 in ("PEPEMORI") then do;
			ADD='JRA';
			desc_sh="Computes the Pepe-Mori test";
			type="A";
			origdate='03AUG2012'd;
		end;

		if MACRO1 in ("FINEANDGRAY") then do;
			ADD='JRA';
			desc_sh="Computes the Fine and Gray model";
			type="A";
			origdate='23AUG2012'd;
		end;
		
		if MACRO1 in ("INPUT_UNIT") then notes="Sub-macro component of the big macro LAB_TOX. Inputation of the unit used for each LAB test";
		if MACRO1 in ( "MV_GEN") then notes="Sub-macro component of the big macro LAB_TOX. Create global macro variables for each LAB exam";
		if MACRO1 in ( "CONVERT_UNIT") then notes="Sub-macro component of the big macro LAB_TOX. Conversion of all the lab test in a single unit (SI unit)";
		if MACRO1 in ("INPUT_NORMALS") then notes="Sub-macro component of the big macro LAB_TOX. Inputationof LLN/ULN values carried forward";
		if MACRO1 in ( "MV_LIST_GEN") then notes="Sub-macro component of the big macro LAB_TOX. Creation of macro strings variables";
		if MACRO1 in ("RECOGNITION") then notes="Sub-macro component of the big macro LAB_TOX. Scanning of all variables names and checking";
		if MACRO1 in ("LABLIST_GEN") then notes="Sub-macro component of the big macro LAB_TOX. Transpose of LAB data into a new LABLIST dataset";
		if MACRO1 in ( "COMPUTE_CTC_GRADE") then notes="Sub-macro component of the big macro LAB_TOX. Computation of CTC grades";
		if MACRO1 in ( "DETECT_PROBLEMS") then notes="Sub-macro component of the big macro LAB_TOX. Produce listings to detect errors (for instance, detect outliers)";

	if MACRO1 in ( "GRAYTEST") then notes="Computes the Gray test for one event of interest accross groups (two or more than two groups) for competing risks data";
	if MACRO1 in ( "PEPEMORI") then notes="Computes the Pepe-Mori test for one event of interest accross two groups (and not more than 2 groups) for competing risks data";
	if MACRO1 in ( "FINEANDGRAY") then notes="Computes the Fine and Gray model: it performs regression modeling of subdistribution function in competing risk data";

		if MACRO1 in ("LAB_TOX"	) then do;
			ADD='JRA';
			ADD1='NMA';
			desc_sh="Handle and summarize laboratory data (Hemato. and Bioch.)";
			type="L";
			notes="Computes CTC grade according to CTCAE v.3 or v.4 from NCI";
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

		if MACRO1="WATERFALL" then do;
			ADD='JRA';
			desc_sh="Macro to perform waterfarfall plots";
			type="G";
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


		if MACRO1="CIPLOT2" then do;
			origdate='19FEB2013'd;
			type="G";
			desc_ln="Constructs ASCII data files for GraphLib to create cumulative incidence curves. Computes also Gray test, Pepe-Mori test or Fine and Gray model.";
			desc_sh="Plots cumulative incidence in case of competing risks";
			BasedOn="CIPLOT.sas";
		end;

end;

	if MACRO1 in ("ALLFIXED2"	"ALLFIXEDB2"	"CLEAN_TABLE"	"FOREST2"	"FORESTB2"	
	"FPLABELS" 	"GAVG"	"GCOUNT"	"GETAGE"	"GETBSA"	"GFIRST"	"GLAST" "GFIRST2"	"GLAST2"	"GMAX"	"GMIN"
	"GSEQ"	"GSUM" "INFERENCE"	"KMPLOT2"	"MEDIANNP"	"MEDIANR"	"PCTY2"	"PVALRND"
	"PVAL"	"QOL_COMP_F"	"RECODES" "RECODE"	"SORT"	"SUMMARY2"	"SUMMARYB3"
	"TABLES2"	"TESTF2" "BRYADAY" "CIPLOT2" "QOL_COMP_V" "CINDEX2" "INPUT_UNIT"
	"MV_GEN"   "CONVERT_UNIT"   "INPUT_NORMALS"  "MV_LIST_GEN"
	"RECOGNITION"  "LABLIST_GEN"  "COMPUTE_CTC_GRADE"  "DETECT_PROBLEMS" "LAB_TOX" "WATERFALL"	"GRAYTEST"
	"PEPEMORI" "FINEANDGRAY" "HAZARDPLOT" "INFERENCE2" "PHREGTEMPLATE" "PRED_FACT"
	"PRED_FACT_SUMMARY" "PRED_FACT_FOREST") then valid='Y'; 

	if MACRO1 in ("ALLFIXED2"	"ALLFIXEDB2"	"CLEAN_TABLE"	"DROP" "FOREST2"	"FORESTB2"	
	"GAVG"	"GCOUNT"	"GETAGE"	"GETBSA"	"GFIRST"	"GLAST"	"GMAX"	"GMIN"
	"GSEQ"	"GSUM" "INFERENCE"	"KMPLOT2"	"PCTY2" "GFIRST2"	"GLAST2"
	"PCTY2"	"PVAL" "PVALRND" "QOL_COMP_F"	"RECODES" "RECODE"	"SUMMARY2"	"SUMMARYB3"
	"TABLES2"	"TESTF2" "CIPLOT2" "CINDEX2" "INFERENCE2" "PHREGTEMPLATE"
	) then add='JRA';

	if MACRO1 in ("QOL_COMP_V") then add1='JRA';
	if MACRO1 in ("SORT") then add='LC';
	if MACRO1 in ("CINDEX2") then do;
				add1='CCO';
				add2='SC';
    end;

	if MACRO1 in ("CIPLOT2") then do;
				add1='KM';
				add2='LCO';
    end;

	if CURRENT ne 'Y' then valid='N';
	if MACRO1 in ("FPLABELS" "PVAL" "PVALRND" "NUMERIC")
	then do;
			origdate=moddate;
		end;
	if MACRO1 in (	 "LAB_TOX"
					  "INPUT_UNIT"
					  "MV_GEN"
					  "CONVERT_UNIT"
					  "INPUT_NORMALS"
					  "MV_LIST_GEN"
					  "RECOGNITION"
					  "LABLIST_GEN"
					  "COMPUTE_CTC_GRADE"
					  "DETECT_PROBLEMS" "WATERFALL" "HAZARDPLOT") 
	then do;
			origdate=mod;
		end;


	FORMAT ORIGDATE DDMMYY10. valid yn.;
	KEEP MOD DATEMODIF moddate MACRO MACRO1 USE CURRENT ADD ADD1 ADD2 origdate sas8 desc_sh desc_ln type valid notes basedon;
RUN;

DATA ML.macrolist2013;
	length desc_sh $60. Documentation $250.;
	MERGE MY_EORTC_LIST testMOD(
	KEEP=MACRO1 ADD ADD1 ADD2 CURRENT origdate sas8 desc_sh type current valid notes desc_ln desc_sh basedon);
	BY MACRO1;


	if MACRO1 in ("ALLFIXED2"	"BRYADAY"	"CIPLOT2" "FOREST2"	"FORESTB2"	
	"GAVG"	"GCOUNT"	"GETAGE"	"GETBSA"	"GFIRST"	"GLAST"	 "GFIRST2"	"GLAST2"	 "GMAX"	"GMIN"
	"GSEQ"	"GSUM" "INFERENCE"	"KMPLOT2"	"MEDIANNP" "MEDIANR" "PCTY2" "HAZARDPLOT"
	"PVAL"	"PVALRND" "QOL_COMP_F" "QOL_COMP_V"	"RECODES" "RECODE"	"SUMMARY2"	"SUMMARYB3"
	"TABLES2"	"TESTF2" "CINDEX2" "LAB_TOX" "WATERFALL" "GRAYTEST" "PEPEMORI" "FINEANDGRAY" "INFERENCE2"
	"PHREGTEMPLATE" "PRED_FACT"	"PRED_FACT_SUMMARY" "PRED_FACT_FOREST"
	) then 	Documentation="link";

	if MACRO1="ALLFIXEDB2" then do;
		Notes="";
	end;
	if MACRO1="HAZARDPLOT" then do;
		Notes="Output: png file by default";
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
	if MACRO1="CINDEX2" then do;
		Notes="Based on an external macro written by Mithat Gonen (gonenm@Mskcc.org) from Memorial Sloan-Kettering Cancer Center, New York, USA";
	end;


		if MACRO1="RECODES" then do;
			basedon="";
		end;
	IF ADD ne '' THEN DO;
		IF AUTHOR1 = '' THEN DO;
			AUTHOR1=ADD;
			AUTHOR2=ADD1;
			AUTHOR3=ADD2;
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

	if MACRO1="SCORE" THEN do;
	MACRO1="SCORES";
	macro="Scores";
	AUTHOR4="CCO";
	end;


	FORMAT AUTHOR4 $fauthor.;


RUN;

data Processed;
	set ML.macrolist2013;
	length Name Name2 $50 NameType $70 Purpose $250;
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
		documentation="<A href='\\elvis\Pccommon\UNIT\Stat\7. SAS and R\SAS Macros Documentation\META ANALYSIS\ALLFIXED2 Documentation 1.3.doc'>link</A>";
	    if macro1="FOREST2" then
		documentation="<A href='\\elvis\Pccommon\UNIT\Stat\7. SAS and R\SAS Macros Documentation\META ANALYSIS\FOREST2 Documentation 1.2.doc'>link</A>";
	    if macro1="FORESTB2" then
		documentation="<A href='\\elvis\Pccommon\UNIT\Stat\7. SAS and R\SAS Macros Documentation\META ANALYSIS\FORESTB2 Documentation 1.0.doc'>link</A>";
	    if macro1="SUMMARY2" then
		documentation="<A href='\\elvis\Pccommon\UNIT\Stat\7. SAS and R\SAS Macros Documentation\META ANALYSIS\SUMMARY2 Documentation 1.0.doc'>link</A>";
	    if macro1="SUMMARYB3" then
		documentation="<A href='\\elvis\Pccommon\UNIT\Stat\7. SAS and R\SAS Macros Documentation\META ANALYSIS\SUMMARYB3 Documentation 1.0.doc'>link</A>";
	    if macro1="TESTF2" then
		documentation="<A href='\\elvis\Pccommon\UNIT\Stat\7. SAS and R\SAS Macros Documentation\META ANALYSIS\TESTF2 Documentation 1.0.doc'>link</A>";

		** DESIGN**;
	    if macro1="BRYADAY" then
		documentation="<A href='\\elvis\Pccommon\UNIT\Stat\7. SAS and R\SAS Macros Documentation\DESIGN\BRYADAY Documentation 1.0.doc'>link</A>";

		** G MACROS**;
	    if macro1="GAVG" then
		documentation="<A href='\\elvis\Pccommon\UNIT\Stat\7. SAS and R\SAS Macros Documentation\GMACROS\GAVG Documentation 1.0.doc'>link</A>";
	    if macro1="GMAX" then
		documentation="<A href='\\elvis\Pccommon\UNIT\Stat\7. SAS and R\SAS Macros Documentation\GMACROS\GMAX Documentation 1.0.doc'>link</A>";
	    if macro1="GMIN" then
		documentation="<A href='\\elvis\Pccommon\UNIT\Stat\7. SAS and R\SAS Macros Documentation\GMACROS\GMIN Documentation 1.0.doc'>link</A>";
	    if macro1="GCOUNT" then
		documentation="<A href='\\elvis\Pccommon\UNIT\Stat\7. SAS and R\SAS Macros Documentation\GMACROS\GCOUNT Documentation 1.0.doc'>link</A>";
	    if macro1="GFIRST" then
		documentation="<A href='\\elvis\Pccommon\UNIT\Stat\7. SAS and R\SAS Macros Documentation\GMACROS\GFIRST Documentation 1.0.doc'>link</A>";
	    if macro1="GLAST" then
		documentation="<A href='\\elvis\Pccommon\UNIT\Stat\7. SAS and R\SAS Macros Documentation\GMACROS\GLAST Documentation 1.0.doc'>link</A>";
	    if macro1="GSUM" then
		documentation="<A href='\\elvis\Pccommon\UNIT\Stat\7. SAS and R\SAS Macros Documentation\GMACROS\GSUM Documentation 1.0.doc'>link</A>";
	    if macro1="GSEQ" then
		documentation="<A href='\\elvis\Pccommon\UNIT\Stat\7. SAS and R\SAS Macros Documentation\GMACROS\GSEQ Documentation 1.0.doc'>link</A>";
	    if macro1="GETAGE" then
		documentation="<A href='\\elvis\Pccommon\UNIT\Stat\7. SAS and R\SAS Macros Documentation\GMACROS\GetAge Documentation 1.0.doc'>link</A>";
	    if macro1="GETBSA" then
		documentation="<A href='\\elvis\Pccommon\UNIT\Stat\7. SAS and R\SAS Macros Documentation\GMACROS\GetBSA Documentation 1.0.doc'>link</A>";
	    if macro1="PVAL" then
		documentation="<A href='\\elvis\Pccommon\UNIT\Stat\7. SAS and R\SAS Macros Documentation\GMACROS\PVAL Documentation 2.0.docx'>link</A>";
	    if macro1="PVALRND" then
		documentation="<A href='\\elvis\Pccommon\UNIT\Stat\7. SAS and R\SAS Macros Documentation\GMACROS\PVALRND Documentation 2.0.docx'>link</A>";
	    if macro1="RECODE" then
		documentation="<A href='\\elvis\Pccommon\UNIT\Stat\7. SAS and R\SAS Macros Documentation\GMACROS\RECODE Documentation 1.0.doc'>link</A>";
	    if macro1="RECODES" then
		documentation="<A href='\\elvis\Pccommon\UNIT\Stat\7. SAS and R\SAS Macros Documentation\GMACROS\RECODES Documentation 1.0.doc'>link</A>";
	    if macro1="GFIRST2" then
		documentation="<A href='\\elvis\Pccommon\UNIT\Stat\7. SAS and R\SAS Macros Documentation\GMACROS\GFIRST2 Documentation 1.0.doc'>link</A>";
	    if macro1="GLAST2" then
		documentation="<A href='\\elvis\Pccommon\UNIT\Stat\7. SAS and R\SAS Macros Documentation\GMACROS\GLAST2 Documentation 1.0.doc'>link</A>";

		** QOL**;
	    if macro1="QOL_COMP_F" then
		documentation="<A href='\\elvis\Pccommon\UNIT\Stat\7. SAS and R\SAS Macros Documentation\QOL\QoL_comp_F Documentation 1.0.doc'>link</A>";
	    if macro1="QOL_COMP_V" then
		documentation="<A href='\\elvis\Pccommon\UNIT\Stat\7. SAS and R\SAS Macros Documentation\QOL\QoL_comp_V Documentation 1.1.doc'>link1</A>  <A href='\\elvis\Pccommon\UNIT\Stat\7. SAS and R\SAS Macros Documentation\QOL\QoL_comp_V macro_JRA_26_01_2009.ppt'>link2</A>";

		** TABLES2**;
	    if macro1="TABLES2" then
		documentation="<A href='\\elvis\Pccommon\UNIT\Stat\7. SAS and R\SAS Macros Documentation\TABLE\TABLES2  Documentation 1.0.doc'>link</A>";

		** SURVIVAL ANALYSIS**;
	    if macro1="CIPLOT2" then
		documentation="<A href='\\elvis\Pccommon\UNIT\Stat\7. SAS and R\SAS Macros Documentation\SURVIVAL ANALYSIS\CIPLOT2 Documentation - 1.2.docx'>link</A>";
	    if macro1="INFERENCE" then
		documentation="<A href='\\elvis\Pccommon\UNIT\Stat\7. SAS and R\SAS Macros Documentation\SURVIVAL ANALYSIS\INFERENCE Documentation 1.0.doc'>link</A>";
	    if macro1="KMPLOT2" then
		documentation="<A href='\\elvis\Pccommon\UNIT\Stat\7. SAS and R\SAS Macros Documentation\SURVIVAL ANALYSIS\KMPLOT2 Documentation 1.0.doc'>link</A>";
	    if macro1="MEDIANNP" then
		documentation="<A href='\\elvis\Pccommon\UNIT\Stat\7. SAS and R\SAS Macros Documentation\SURVIVAL ANALYSIS\MedianNP Documentation 1.0.doc'>link</A>";
	    if macro1="MEDIANR" then
		documentation="<A href='\\elvis\Pccommon\UNIT\Stat\7. SAS and R\SAS Macros Documentation\SURVIVAL ANALYSIS\MedianR Documentation 1.0.doc'>link</A>";
	    if macro1="PCTY2" then
		documentation="<A href='\\elvis\Pccommon\UNIT\Stat\7. SAS and R\SAS Macros Documentation\SURVIVAL ANALYSIS\PCTY2 Documentation 1.0.doc'>link</A>";
	    if macro1="CINDEX2" then
		documentation="<A href='\\elvis\Pccommon\UNIT\Stat\7. SAS and R\SAS Macros Documentation\SURVIVAL ANALYSIS\CINDEX2 Documentation 1.0.doc'>link</A>";
	    if macro1="GRAYTEST" then
		documentation="<A href='\\elvis\Pccommon\UNIT\Stat\7. SAS and R\SAS Macros Documentation\SURVIVAL ANALYSIS\GRAYTEST Documentation 2.0.docx'>link</A>";
	    if macro1="PEPEMORI" then
		documentation="<A href='\\elvis\Pccommon\UNIT\Stat\7. SAS and R\SAS Macros Documentation\SURVIVAL ANALYSIS\PEPEMORI Documentation 2.0.docx'>link</A>";
	    if macro1="FINEANDGRAY" then
		documentation="<A href='\\elvis\Pccommon\UNIT\Stat\7. SAS and R\SAS Macros Documentation\SURVIVAL ANALYSIS\FINEANDGRAY Documentation 3.0.docx'>link</A>";
	    if macro1="INFERENCE2" then
		documentation="<A href='\\elvis\Pccommon\UNIT\Stat\7. SAS and R\SAS Macros Documentation\SURVIVAL ANALYSIS\INFERENCE2 Documentation 1.0.doc'>link</A>";
	    if macro1="PHREGTEMPLATE" then
		documentation="<A href='\\elvis\Pccommon\UNIT\Stat\7. SAS and R\SAS Macros Documentation\SURVIVAL ANALYSIS\PHREGTEMPLATE Documentation 1.0.docx'>link</A>";

	    if macro1="PRED_FACT" then
		documentation="<A href='\\elvis\Pccommon\UNIT\Stat\7. SAS and R\SAS Macros Documentation\SURVIVAL ANALYSIS\PRED_FACT Documentation 1.0.docx'>link</A>";
	    if macro1="PRED_FACT_SUMMARY" then
		documentation="<A href='\\elvis\Pccommon\UNIT\Stat\7. SAS and R\SAS Macros Documentation\SURVIVAL ANALYSIS\PRED_FACT_SUMMARY Documentation 1.0.docx'>link</A>";
	    if macro1="PRED_FACT_FOREST" then
		documentation="<A href='\\elvis\Pccommon\UNIT\Stat\7. SAS and R\SAS Macros Documentation\SURVIVAL ANALYSIS\PRED_FACT_FOREST Documentation 1.0.docx'>link</A>";


		** LABTOX**;
	    if macro1="LAB_TOX" then
		documentation="<A href='\\elvis\Pccommon\UNIT\Stat\7. SAS and R\SAS Macros Documentation\LABTOX\How to use LAB_TOX macros v1.3.docx'>link1</A>  <A href='\\elvis\Pccommon\UNIT\Stat\7. SAS and R\SAS Macros Documentation\LABTOX\MACRO LABTOX JRA v3.ppt'>link2</A>";
		** WATERFALL**;
	    if macro1="WATERFALL" then
		documentation="<A href='\\elvis\Pccommon\UNIT\Stat\7. SAS and R\SAS Macros Documentation\GMACROS\WATERFALL Documentation 1.0.docx'>link</A>";

		** HAZARDPLOT**;
	    if macro1="HAZARDPLOT" then
		documentation="<A href='\\elvis\Pccommon\UNIT\Stat\7. SAS and R\SAS Macros Documentation\SURVIVAL ANALYSIS\HAZARDPLOT Documentation 1.0.docx'>link</A>";


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

	IF MACRO1 in ("GFIRST" "GLAST") then CURRENT= "N" ;
	IF MACRO1 in ("CIPLOT") then CURRENT= "N" ;

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

data Processed;
set Processed;
if Name="SCORES" and type="" then delete;
run;


proc sort data=Processed out=List;
by Name;
where Current='Y';
run;

options pageno=1;
ods html file="K:\SAS\EORTC macros\Intranet Lists\Output\CurrentMacros.htm" style=mystyle;
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

data List;
set List;
by Type Name;
if not(first.Type) then Type="";
run;

options pageno=1;
ods html file="K:\SAS\EORTC macros\Intranet Lists\Output\CurrentMacrosByCategory.htm" style=mystyle;
title1 h=4  'EORTC MACROS Currently in Use by Category';
proc print data=List label noobs
           style (header) = header [just=l]
           style (data) = data [just=l];
*by Type;
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


