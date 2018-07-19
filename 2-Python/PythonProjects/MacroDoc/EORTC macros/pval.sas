**** Modified P-val macro in which both the input and the output are data sets ;
*** the macro reads the data pdata, rounds the variable pname;
*** the result is put into a new variable npname ;

*** Updated to ensure unknown (.U) values are handled correctly. KM, 30 May 2002.;

*** JRA, 27/09/2006: for the case of 10**-12 value, it should be rounded to "< 0.00000000001".
*** (or 1E-11). Indeed, for the computation of a rounded p-value lower than 0.00000000001 (or 10-11):
*** the rounded value should be equal to "< 0.00000000001" ;

*** LCO, JRA: JUN 2012: limit to 5 decimals: <0.00001;

%macro pval(pdata, pname, npname) ;


data &pdata (drop=_j_ _k_ _q_ _i_ _z_ _rpval_);
 set &pdata ;
 _rpval_='                  ' ;
 format _rpval_ &npname $20. ;
 _k_=. ;
 if (missing(&pname)) then do ;
     _rpval_='                 ' ;
     end ;
 else do ;
 if (&pname>0.1) then do ;
            _rpval_='>0.1' ;
            end ;
 if (&pname>0.009 and &pname<=0.1) then do ;
            _rpval_='='||left(trim(round(&pname,0.01)));
            end ;
 if (&pname>.Z and &pname<=0.009) then do ;
    _j_=1 ; * initialize the roundoff ;
    _q_=0 ;
    do until (_q_ NE 0) ;
      _j_=_j_+1 ;
      _i_=10**_j_ ;
      _z_=&pname*_i_ ;
      _q_=floor(_z_) ;
      end ;
    _k_=10**(-_j_) ;
    if (_j_>1 and _j_<6) then do ; *JRA, 27092006;
      if (&pname=_k_) then do ; _rPval_='='||left(trim(put(_k_,17.15))) ; end ;
      if (&pname>_k_) then do ;
                    _k_=_k_*10 ;
                    _rPval_='<'||left(trim(put(_k_,17.15))) ;
                    end ;
      _rpval_=scan(_rPval_,1,'1')||'1';
      end ;

     if (_j_>=6) then  _rpval_='<0.00001'  ; *JRA, 27092006;* LCO 03 11 2011: limit to 5 decimals ;
     end ;
 end ;
 &npname=put(_rpval_,$19.) ;
 run ;


%mend pval ;
