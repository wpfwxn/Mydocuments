/*************************************************************************************

GetBSA.SAS
**********

Calculates BSA (body surface area) in m2 from height and weight.

Version date   : March 2003
Software       : SAS version 8.2
Original author: Kate Moncrieff

************************************************************************************

PARAMETERS

    Height     :  Variable containing patient's height (in cm).  Default=HEIGHT.
    Weight     :  Variable containing patient's weight (in kg).  Default=WEIGHT.  
    BSA        :  New variable to contain BSA (in m2).  Default=BSA.


************************************************************************************

EXAMPLES

   data New;
   set StudyLib.Patient (keep=SequentialID Height Weight);
   %GetBSA;
   keep SequentialID BSA;
   run;

   data New;
   set BaseChar (keep=SequentialID HtCM WtKG);
   %GetBSA(HtCM,WtKG,BaselineBSA);
   keep SequentialID BaselineBSA;
   run;

************************************************************************************/


%MACRO GetBSA(Height,Weight,BSA);

%IF &Height.=%STR() %THEN %LET Height=Height;
%IF &Weight.=%STR() %THEN %LET Weight=Weight;
%IF &BSA.=%STR() %THEN %LET BSA=BSA;

if missing(&Height.) or missing(&Weight.) then &BSA.=.;
else &BSA. = (&Weight.**0.425) * (&Height.**0.725) * 0.007184;

%MEND GetBSA;