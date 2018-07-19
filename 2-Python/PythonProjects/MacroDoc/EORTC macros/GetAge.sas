/*************************************************************************************

GetAGE.SAS
**********

Calculates age from date of birth and date at which age is required.

Version date   : September 2002
Software       : SAS version 8.2
Original author: Kate Moncrieff

************************************************************************************

PARAMETERS

    BirthDate  :  Variable containing date of birth.  Default=DOB.
    AtDate     :  Variable containing date age required.  Default=DOR.
    Age        :  New variable to contain age.  Default=Age.

************************************************************************************

EXAMPLES

   data New;
   set StudyLib.Patient (keep=SequentialID DOB DOR);
   %GETAGE;
   keep SequentialID Age;
   run;

   data New;
   set Patient (keep=SequentialID DateofBirth DateRegis);
   %GETAGE(DateofBirth,DateRegis,AgeAtRegis);
   keep SequentialID AgeAtRegis;
   run;

************************************************************************************

REVISIONS

27MAR2003,KM  Added default values for each parameter (DOB, DOR and Age).

************************************************************************************/


%MACRO GETAGE(BirthDate,AtDate,Age);

%IF &BirthDate.=%STR() %THEN %LET BirthDate=DOB;
%IF &AtDate.=%STR() %THEN %LET AtDate=DOR;
%IF &Age.=%STR() %THEN %LET Age=Age;

if missing(&BirthDate.) or missing(&AtDate.) then &Age.=.;
else do;
   if month(&AtDate.) > month(&BirthDate.)
     or (month(&AtDate.) = month(&BirthDate.) and day(&AtDate.) >= day(&BirthDate.)) then
        &Age. = intck('year',&BirthDate.,&AtDate.);
   else &Age. = intck('year',&BirthDate.,&AtDate.) - 1;
end;

%MEND GETAGE;