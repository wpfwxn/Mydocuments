* Macro to standardize the format of p-values like MRC does ;
* that is rounding the p-value to the first meaningful decimal ;
* up to 5 decimals the number is written full length ;
* from 6 on the scientific notation is used ;

** this is the second version which uses only data sets ;

*** Updated 30 May 2002 to ensure unknown (.U) values are handled correctly. KM.;

*** JRA, MAY 2012: limit to 5 decimals: <0.00001;

%macro pvalrnd(pdata, pname, npname) ;

data &pdata (drop= _rpval_ _k_ _q_ _i_ _j_ _z_ _t_ _rpval1_);
 set &pdata ;

 _rpval_='                  ' ;
 format _rpval_ &npname $17. ;
 format _k_ 20.18 ;
 _k_=. ;

if (missing(&pname)) then do ;
     _rpval_='                 ' ;
     end ;
 else do ;
  if (&pname>0.1) then do ;
            _q_=round(&pname,0.01) ; ***JRA, 12FEB2008: 2 figures instead of 1 after .; 
            _rpval_='='||left(trim(_q_)) ;
           * _rpval_='>0.1' ;
            end ;
  if (&pname>.Z and &pname<=0.1) then do ;
    _j_=1 ; * initialize the roundoff ;
    _q_=0 ;
    do until (_t_ NE 0) ;
      _j_=_j_+1 ;
      _i_=10**_j_ ;
      _z_=&pname*_i_ ;
      _t_=floor(_z_) ;
      _q_=round(_z_) ;
      *put _q_ ;
      end ;
    _k_=_q_*(10**(-_j_)) ;
    if (_j_>1 and _j_<=6) then do ;
       _rpval_='='||left(trim(put(_k_,17.15))) ;
       _rpval1_=scan(_rpval_,1,'123456789') ;
       _rpval_=compress(_rpval1_)||left(compress(_q_)) ;
       end ;
     if (_j_>=6) then  _rpval_='<0.00001'  ; *JRA, 31052012: limit to 5 decimals ;
   end ;
 end ;
 &npname=_rpval_ ;
  run ;

%mend pvalrnd ;

