

# Documented function:

spotVol = function (pdata, dailyvol = "medrv" , periodicvol = "TML" , on = "minutes" , k = 5 , 
           dummies = FALSE , P1 = 4 , P2 = 2, ...) 
{
    dates = unique(format(time(pdata),"%Y-%m-%d"))
    cDays = length(dates);
    rdata = mR = c();
    for(d in 1:cDays){
       pdatad = pdata[as.character(dates[d])]
       pdatad = aggregatePrice(pdatad, on=on , k = k )
       rdatad = makeReturns(pdatad)
       # remove the first zero returns
       rdatad = rdatad[time(rdatad)>min(time(rdatad))] 
       rdata  = rbind( rdata , rdatad )
       mR = rbind( mR , as.numeric(rdatad)) 
    }
    M = ncol(mR);
    if( cDays == 1){
       mR = as.numeric(rdata)
       estimdailyvol = switch( dailyvol , "bipower" = RBPVar(mR) , 
                                          "medrv"   = MedRV(mR) ,
                                          "rv"      = RV(mR) ) 
    }else{
       estimdailyvol = switch( dailyvol , "bipower" = apply(mR,1,'RBVar') , 
                                          "medrv"   = apply(mR,1,'MedRV') ,
                                          "rv"      = apply(mR,1,'RV') ) 
    }
    if( cDays <= 50 ){
       print("Periodicity estimation requires at least 50 observations. Periodic component set to unity")
       estimperiodicvol = rep(1,M)
    }else{
       mstdR = mR/estimdailyvol; 
       estimperiodicvol = diurnal( stddata = mstdR , method = periodicvol , dummies = dummies , P1 = P1 , P2 = P2 )[[1]]
       mfilteredR = mR/matrix( rep(estimperiodicvol,cDays) , byrow = T , nrow = cDays )
       estimdailyvol = switch( dailyvol , "bipower" = apply(mfilteredR,1,'RBVar') , 
                                          "medrv"   = apply(mfilteredR,1,'MedRV') ,
                                          "rv"      = apply(mfilteredR,1,'RV') )        
    }
    out  = cbind( rdata ,
                  rep( estimdailyvol,each=M)*rep(estimperiodicvol,cDays) ,
                  rep( estimdailyvol,each=M) ,
                  rep(estimperiodicvol,cDays) )
    out = xts( out , order.by = time(rdata) )
    names(out) = c("returns","vol","dailyvol","periodicvol")
    return( out )
}


# internal non documented functions: 


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

diurnal = function( stddata , method="TML", dummies = F, P1 = 6 , P2 = 4)
{
   # Function that has as input the standardized returns and computes the seasonality factor
   # by the scale-based or regression approach as explained in 
   # Boudt, Croux and Laurent (2008)'s paper ``Robust estimation of intraweek periodicity and jump detection''

   # stddata is matrix that holds in row the standardized intraday returns
 
   cDays = dim(stddata)[1] ; intraT = dim(stddata)[2]; 

   meannozero = function( series ){ return(mean(series[series!=0]) ) }
   shorthscalenozero = function( series ){ return(shorthscale(series[series!=0]) ) } 
   WSDnozero = function( weights , series ){ 
             out = sum( (weights*series^2)[series!=0] )/sum( weights[series!=0]) ;   
             return( sqrt( 1.081*out) )
   }
   if(method=="SD"|method=="OLS"){ seas = sqrt(apply( stddata^2 , 2 , 'meannozero'  ) ) }
   if(method=="WSD"|method=="TML"){
            seas = apply( stddata , 2 , 'shorthscalenozero' )  ;
            shorthseas =   seas/sqrt(mean( seas^2) )
            shorthseas[shorthseas==0] = 1; 
            weights = matrix( HRweight( as.vector(t(stddata^2) / rep(shorthseas,cDays)^2) ,qchisq(0.99,df=1)) ,  
                                   ncol=dim(stddata)[2] , byrow=T)
            for( c in 1:intraT ){ seas[c] = WSDnozero( weights[,c] , stddata[,c] ) }
   }
   seas = seas/sqrt(mean( seas^2) ) 
   if( method=="OLS"|method=="TML"){


      # Do periodicity estimation on the basis of a FFF specification + Almond Polynomials

      #--Define variables--
      c=center();
      vstddata = as.vector(stddata); nobs = length(vstddata) ; 
      vi = rep( c(1:intraT) , each = cDays ) # c(1,...,1,2,....2,3...., ,intraT)
      if(method=="TML"){ firststepresids = log(abs(vstddata))-c-log(rep(seas,each=cDays)) };

      # Regressors 
      X = c();
      if(!dummies){
        if ( P1 > 0 ){ for( j in 1:P1 ){ X = cbind( X , cos(2*pi*j*vi/intraT) )   }  } ;
        M1 = (intraT+1)/2 ; M2 = (2*intraT^2 + 3*intraT + 1)/6;
        ADD = (vi/M1 ) ; X = cbind(X,ADD);
        ADD = (vi^2/M2); X = cbind(X,ADD);
        if ( P2 > 0 ){ ADD= c(); for( j in 1:P2 ){  ADD = cbind( ADD , sin(2*pi*j*vi/intraT)  ) }}; X = cbind( X , ADD ) ; 

     }else{ 
         for( d in 1:intraT){
           dummy = rep(0,intraT); dummy[d]=1; dummy = rep(dummy,each=cDays)
           X = cbind(X,dummy); 
         }
     }

      # Remove the inliers
      selection = c(1:nobs)[vstddata!=0];  
      vstddata = vstddata[selection] ;  X = X[selection,]
      if(method == "TML" ){ firststepresids = firststepresids[selection]}; 

      # Dependent variable
      vy = matrix ( log( abs( vstddata  )) , ncol=1 )-c;
 
     if( method == "OLS" ){
          Z = try(solve(t(X)%*%X),silent=T)
          if( inherits(Z, "try-error") ){
             print("X'X is not invertible. Switch to TML")
          }else{
             theta =  solve( t(X)%*%X  )%*%t(X)%*%vy ;   rm(X);rm(vy);  
          }
      }  
      if( method == "TML" ){
         inittheta = rep(0,dim(X)[2]);       
         l =  -2.272; u = 1.6675;
         nonoutliers = c(1:length(vy))[(  firststepresids > l) & (firststepresids <u ) ]
         truncvy = vy[nonoutliers ] ; rm(vy); truncX = X[nonoutliers,] ; rm(X);
         # MLE on the basis of the truncated dataset
         negtruncLLH = function(theta){
                res = truncvy - truncX%*%matrix(theta,ncol=1)
                return( mean( -res - c +exp(2*(res+c))/2  )  )
         }   
         grnegtruncLLH = function(theta){
              res = truncvy - truncX%*%matrix(theta,ncol=1) ; dres = -truncX;
              return( apply(  -dres  +as.vector(exp(2*(res+c)))*dres  , 2 , 'mean' )  )     
         }       
         est = optim(par=inittheta, fn= negtruncLLH, gr = grnegtruncLLH , method =  "BFGS" )  ;
         theta = est$par;
         rm(truncX); rm(truncvy); 
      }
      #--Output--


      plot( seas , main = "Non-parametric (dashed line) and parametric (full line) periodicity" , xlab="intraday period" , type="l",lty=3)
      seas = diurnalfit(theta=theta, P1=P1 , P2=P2, intraT = intraT , dummies = dummies)
      lines( seas , lty = 1 )
      return( list ( seas , theta  ) )
  }else{ return( list(seas) ) }
}

diurnalfit = function( theta , P1 , P2 , intraT , dummies=F )
{
     vi = c(1:intraT) ;  
     M1 = (intraT+1)/2 ; M2 = (2*intraT^2 + 3*intraT + 1)/6;

     # Regressors that do not depend on Day of Week:
     X = c()
     if(!dummies){
        if ( P1 > 0 ){ for( j in 1:P1 ){ X = cbind( X , cos(2*pi*j*vi/intraT) )   }  } 

        ADD = (vi/M1 ) ; X = cbind(X,ADD);
        ADD = (vi^2/M2); X = cbind(X,ADD);
        if ( P2 > 0 ){ ADD= c(); for( j in 1:P2 ){  ADD = cbind( ADD , sin(2*pi*j*vi/intraT)  ) }}; X = cbind( X , ADD ) ; 
     }else{
        for( d in 1:intraT){
          dummy = rep(0,intraT); dummy[d]=1; 
          X = cbind(X,dummy); 
        }
     }
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



