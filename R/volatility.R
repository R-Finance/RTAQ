##UNIVARIATE:
#Realized Volatility (RV)
RV = function(data){
returns=as.numeric(data);
RV = sum(returns*returns);
return(RV);
}

#Realized Outlyingness Weighted Variance (ROWVar):
univariateoutlyingness = function(data){
#computes outlyingness of each obs compared to row location and scale
	location = 0;
	scale = mad(data);
		if(scale==0){
		scale = mean(data);
		}
	d = ((data - location)/scale)^2;
}


ROWVar =
function(R, seasadjR = NULL, wfunction = "HR" , alphaMCD = 0.5, alpha = 0.001) 
{
    require(robustbase)
    if (is.null(seasadjR)) {
        seasadjR = R
    }
    intraT = length(R); N=1;
    MCDcov = as.vector(covMcd( R , use.correction = FALSE )$raw.cov)
    outlyingness = seasadjR^2/MCDcov    
    k = qchisq(p = 1 - alpha, df = N)
    outlierindic = outlyingness > k
    weights = rep(1, intraT)
    if( wfunction == "HR" ){
       weights[outlierindic] = 0
       wR = sqrt(weights) * R
       return((conHR(di = N, alpha = alpha) * sum(wR^2))/mean(weights))
    }
    if( wfunction == "SR" ){
       weights[outlierindic] = k/outlyingness[outlierindic]
       wR = sqrt(weights) * R
       return((conhuber(di = N, alpha = alpha) * sum(wR^2))/mean(weights))
    }

}



#Realized BiPower Variation (RBPVar) (RBPVar)
RBPVar = function(data){
  returns = as.numeric(data);
  n = length(returns);
  rbpvar = (pi/2)*sum(abs(returns[1:(n-1)])*abs(returns[2:n]));
  return(rbpvar);
}

#MinRV:
MinRV = function(a){
  q = as.zoo(abs(as.numeric(a))); #absolute value
  q = as.numeric(rollapply(q, width=2, FUN=min,by = 1, align="left"));
  N = length(q)+1; #number of obs
  minrv = (pi/(pi-2))*(N/(N-1))*sum(q^2);
return(minrv)
}

#MedRV
MedRV = function(a){
  q = abs(as.numeric(a)); #absolute value
  q = as.numeric(rollmedian(q, k=3, align="center"));
  N = length(q) + 2;
  minrv = (pi/(6-4*sqrt(3)+pi))*(N/(N-2))*sum(q^2);
return(minrv)
}



##Multivariate measures:
#Realized Covariation (RCov):
RCov = function(ts){
  ts = na.locf(ts,na.rm=FALSE);
  covariance = t(ts)%*%ts;
  return(covariance);
}

#Realized Outlyingness Weighted Quadratic Covariation (ROWQCov)
conhuber = function(di,alpha=0.05)
{# consistency factor ROWQCov based on Huber weight function
   c = qchisq(p=1-alpha,df=di)
   fw2 = function(t){
      z=t^2; return(  huberweight(z,c)*( t^(di-1) )*exp(-z/2)    ) }
   fw1 = function(t){
      z=t^2; return(  huberweight(z,c)*( t^(di+1) )*exp(-z/2)   )}
   c2 = integrate(fw2,0,Inf)$value;  c1 = integrate(fw1,0,Inf)$value;
   return( di*c2/c1 )
}

conHR = function(di,alpha=0.05)
{
# consistency factor ROWQCov based on hard rejection weight function
   return( (1-alpha)/pchisq(qchisq(1-alpha,df=di),df=di+2)  )
}

huberweight = function(d,k){
# Huber or soft rejection weight function
   w = apply( cbind( rep(1,length(d) ) , (k/d) ),1,'min'); return(w);
}

countzeroes = function( series )
{
    return( sum( 1*(series==0) ) )
}


ROWCov =
function (R, seasadjR = NULL, wfunction = "HR" , alphaMCD = 0.5, alpha = 0.001) 
{
    require(robustbase)
    if( is.null(dim(R) )){ 
          return( ROWVar( R , seasadjR = seasadjR , wfunction = wfunction , alphaMCD = alphaMCD , alpha = alpha ))
    }else{
       if (is.null(seasadjR)) {
           seasadjR = R
       }
       intraT = nrow(R)
       N = ncol(R)
       perczeroes = apply(seasadjR, 2, countzeroes)/intraT
       select = c(1:N)[perczeroes < 0.5]
       seasadjRselect = seasadjR[, select]
       N = ncol(seasadjRselect)
       MCDobject = try(covMcd(x = seasadjRselect, alpha = alphaMCD))
       if (length(MCDobject$raw.mah) > 1) {
           betaMCD = 1-alphaMCD; asycor = betaMCD/pchisq( qchisq(betaMCD,df=N),df=N+2 )
           MCDcov = (asycor*t(seasadjRselect[MCDobject$best,])%*%seasadjRselect[MCDobject$best,])/length(MCDobject$best);  
           invMCDcov = solve(MCDcov) ; outlyingness = rep(0,intraT);
           for( i in 1:intraT ){ 
                    outlyingness[i] = matrix(seasadjRselect[i,],ncol=N)%*%invMCDcov%*%matrix(seasadjRselect[i,],nrow=N)    }
       }
       else {
          print(c("MCD cannot be calculated")); stop();
       }
       k = qchisq(p = 1 - alpha, df = N)
       outlierindic = outlyingness > k
       weights = rep(1, intraT)
       if( wfunction == "HR" ){
          weights[outlierindic] = 0
          wR = sqrt(weights) * R
          return((conHR(di = N, alpha = alpha) * t(wR) %*% wR)/mean(weights))
       }
       if( wfunction == "SR" ){
          weights[outlierindic] = k/outlyingness[outlierindic]
          wR = sqrt(weights) * R
          return((conhuber(di = N, alpha = alpha) * t(wR) %*% wR)/mean(weights))
       }
   }
}

#Realized BiPower Covariation (RBPCov)
RBPCov_bi = function(ts1,ts2){
  n = length(ts1);
  a = abs(ts1+ts2);
  b = abs(ts1-ts2);  
  first = a[1:(n-1)]*a[2:n];
  last = b[1:(n-1)]*b[2:n];
  result =  (pi/8)*sum(first-last);
  return(result);
}

RBPCov = 
function (ts) 
{
    if( is.null(dim(ts) )){ 
          return( RBPVar( ts ))
    }else{
       n = dim(ts)[2]
       cov = matrix(rep(0, n * n), ncol = n)
       diagonal = c()
       for (i in 1:n) {
          diagonal[i] = RBPVar(ts[, i])
       }
       diag(cov) = diagonal
       for (i in 2:n) {
           for (j in 1:(i - 1)) {
               cov[i, j] = cov[j, i] = RBPCov_bi(ts[, i], ts[, j])
           }
       }
       return(cov)
   }
}

thresholdcov = function(ts)	{
  n=dim(ts)[1];						#number of observations
  delta = 1/n;
  rbpvars = apply(ts,2,FUN=RBPVar);			#bipower variation per stock
  tresholds = 3*sqrt(rbpvars)*(delta^(0.49));	#treshold per stock
  tresmatrix = matrix(rep(tresholds,n),ncol=length(tresholds),nrow=n,byrow=TRUE);
  condition = ts>tresmatrix;
  ts[condition] = 0;
  cov = RCov(ts);
return(cov);	
				}

#Realized Correlation (RCor)
RCor = function(ts){
  ts = na.locf(ts,na.rm=FALSE);
  covariance = t(ts)%*%ts;
  sdmatrix = sqrt(diag(diag(covariance)));
  rcor = solve(sdmatrix)%*%covariance%*%solve(sdmatrix);
  return(rcor);
}
