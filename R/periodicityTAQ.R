

standardize = function(data,method)
{
   # standardized the return series by a daily location and scale estimate
   daily.RBPVar =  apply(data,1,'RBPVar');
   daily.M    =  apply(data,1,'length');
   std.data   = (data-0)/sqrt( daily.RBPVar/(daily.M-1)) ;
   return(std.data);
}

HRweight = function( d,k){
# Hard rejection weight function
   w = 1*(d<=k); return(w)
}

shorthscale = function( data )
{
   sorteddata = sort(data);
   n = length(data);
   h = floor(n/2)+1;
   M = matrix( rep(0,2*(n-h+1) ) , nrow= 2 );
   for( i in 1:(n-h+1) ){
       M[,i] = c( sorteddata[ i ], sorteddata[ i+h-1 ] )
   }
   return( 0.7413*min( M[2,]-M[1,] ) );
}

diurnal = function( stddata , approach="regression", P1 = 6 , P2 = 4)
{
   # Function that has as input the standardized returns and computes the seasonality factor
   # by the scale-based or regression approach as explained in 
   # Boudt, Croux and Laurent (2008)'s paper ``Robust estimation of intraweek periodicity and jump detection''

   # stddata is matrix that holds in row the standardized intraday returns
 
   cDays = dim(stddata)[1] ; intraT = dim(stddata)[2]; 

   # Estimation of periodicity pattern using the Weighted Standard Deviation technique
   seas = apply( stddata , 2 , 'shorthscale' )  ;
   shorthseas = (1/sqrt(mean(seas^2)))*seas  
   weights = matrix( HRweight( as.vector(t(stddata^2) / rep(shorthseas,cDays)^2) ,qchisq(0.99,df=1)) ,  
                                   ncol=dim(stddata)[2] , byrow=T)
   seas = sqrt(  1.081*colSums(  weights*stddata^2    ) / colSums( weights )     )
   seas = (1/sqrt(mean(seas^2)))*seas  

   if( approach=="regression"){
      # Do Truncated Maximum Likelihood estimation on the basis of a FFF specification + Almond Polynomials
      # with jump detection technique initialized using the WSD estimates

      #--Define variables--
      c=center();
      vstddata = as.vector(stddata); nobs = length(vstddata) ; 
      vi = rep( c(1:intraT) , each = cDays ) # c(1,...,1,2,....2,3...., ,intraT)
      # Remove the inliers
      selection = c(1:nobs)[vstddata!=0];  
      vstddata = vstddata[selection] ;  vi = vi[selection]; 
      firststepresids = log(abs(stddata))-c-log(seas);
      firststepresids = as.vector(firststepresids)[selection]; 

      # Dependent variable
      vy = matrix ( log( abs( vstddata  )) , ncol=1 )-c;
 
      # Regressors 
      X = c();
      if ( P1 > 0 ){ for( j in 1:P1 ){ X = cbind( X , cos(2*pi*j*vi/intraT) )   }  } ;
      M1 = (intraT+1)/2 ; M2 = (2*intraT^2 + 3*intraT + 1)/6;
      ADD = (vi/M1 ) ; X = cbind(X,ADD);
      ADD = (vi^2/M2); X = cbind(X,ADD);
      if ( P2 > 0 ){ ADD= c(); for( j in 1:P2 ){  ADD = cbind( ADD , sin(2*pi*j*vi/intraT)  ) }}; X = cbind( X , ADD ) ; 

      #openingeffect
      opening = vi-0 ; stdopening = (vi-0)/80 ;
      almond1_opening   = ( 1 - (stdopening)^3 ); almond2_opening   = ( 1 - (stdopening)^2 )*( opening);
      almond3_opening   = ( 1 - (stdopening)   )*( opening^2);   
      X = cbind(  X, almond1_opening , almond2_opening , almond3_opening   )  ;

      #closing effect
      closing = 81-vi ; stdclosing = (81-vi)/80 ;
      almond1_closing   = ( 1 - (stdclosing)^3 ); almond2_closing   = ( 1 - (stdclosing)^2 )*( closing);
      almond3_closing   = ( 1 - (stdclosing)   )*( closing^2);   
      X = cbind(  X, almond1_closing , almond2_closing , almond3_closing   )  ;

      #--Estimation--
      inittheta = rep(0,dim(X)[2]);       
      l =  -2.272; u = 1.6675;
      nonoutliers = c(1:length(vy))[(  firststepresids > l) & (firststepresids <u ) ]
      truncvy = vy[nonoutliers ] ; rm(vy); truncX = X[nonoutliers,] ; rm(X);
      # MLE on the basis of the truncated dataset
      negtruncLLH = function(theta){
                res = truncvy - truncX%*%matrix(theta,ncol=1)
                return( mean( -res - c +exp(2*(res+c))/2  )  )
      }   
      grnegtruncLLH = function(theta){ res = truncvy - truncX%*%matrix(theta,ncol=1) ; dres = -truncX;
                                       return( apply(  -dres  +as.vector(exp(2*(res+c)))*dres  , 2 , 'mean' )  )     
      }       
      est = optim(par=inittheta, fn= negtruncLLH, gr = grnegtruncLLH , method =  "BFGS" )  ;
      theta = est$par;
      rm(truncX); rm(truncvy); 

      #--Output--
      seas = diurnalfit(theta=theta, P1=P1 , P2=P2, intraT = intraT)
      return( list ( theta , seas  ) )
  }else{ return(seas) }
}

diurnalfit = function( theta , P1 , P2 , intraT )
{
     vi = c(1:intraT) ;  
     M1 = (intraT+1)/2 ; M2 = (2*intraT^2 + 3*intraT + 1)/6;

     # Regressors that do not depend on Day of Week:
     X = c()
     if ( P1 > 0 ){ for( j in 1:P1 ){ X = cbind( X , cos(2*pi*j*vi/intraT) )   }  } 

     ADD = (vi/M1 ) ; X = cbind(X,ADD);
     ADD = (vi^2/M2); X = cbind(X,ADD);
     if ( P2 > 0 ){ ADD= c(); for( j in 1:P2 ){  ADD = cbind( ADD , sin(2*pi*j*vi/intraT)  ) }}; X = cbind( X , ADD ) ; 

     #openingeffect
     opening = vi-0 ; stdopening = (vi-0)/80 ;
     almond1_opening   = ( 1 - (stdopening)^3 );  almond2_opening   = ( 1 - (stdopening)^2 )*( opening);
     almond3_opening   = ( 1 - (stdopening)   )*( opening^2);   
     X = cbind(  X, almond1_opening , almond2_opening , almond3_opening   )  ;

     #closing effect
     closing = 81-vi ; stdclosing = (81-vi)/80 ;
     almond1_closing   = ( 1 - (stdclosing)^3 ); almond2_closing   = ( 1 - (stdclosing)^2 )*( closing);
     almond3_closing   = ( 1 - (stdclosing)   )*( closing^2);   
     X = cbind(  X, almond1_closing , almond2_closing , almond3_closing   )  ;
 
     # Compute fit
     seas = exp( X%*%matrix(theta,ncol=1) );
     seas = seas/sqrt(mean( seas^2) )    
     return( seas )          
}

center = function()
{
    g=function(y){ return( sqrt(2/pi)*exp(y-exp(2*y)/2)  )}
    f=function(y){ return( y*g(y)    )  }
    return( integrate(f,-Inf,Inf)$value )
}

LeeMyklandCV = function( beta = 0.999 , M = 78 )
{
    # Critical value for Lee-Mykland jump test statistic
    # Based on distribution of Maximum of M absolute normal random variables
    a = function(n){ a1=sqrt(2*log(n)) ; a2= (log(pi)+log(log(n))  )/( 2*sqrt(2*log(n))   )   ; return(a1-a2)             };
    b = function(n){ return( 1/sqrt(2*log(n) )  ) ; return(b)} ;
    return( -log(-log(beta))*b(M) + a(M)     )
}



