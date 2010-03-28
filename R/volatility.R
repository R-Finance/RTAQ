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
function(data, seasadjR = NULL, wfunction = "HR" , alphaMCD = 0.5, alpha = 0.001) 
{
    require(robustbase)
    if (is.null(seasadjR)) {
        seasadjR = data;
    }

    data = as.vector(data); seasadjR = as.vector(seasadjR);
    intraT = length(data); N=1;
    MCDcov = as.vector(covMcd( data , use.correction = FALSE )$raw.cov)
    outlyingness = seasadjR^2/MCDcov    
    k = qchisq(p = 1 - alpha, df = N)
    outlierindic = outlyingness > k
    weights = rep(1, intraT)
    if( wfunction == "HR" ){
       weights[outlierindic] = 0
       wR = sqrt(weights) * data
       return((conHR(di = N, alpha = alpha) * sum(wR^2))/mean(weights))
    }
    if( wfunction == "SR" ){
       weights[outlierindic] = k/outlyingness[outlierindic]
       wR = sqrt(weights) * data
       return((conhuber(di = N, alpha = alpha) * sum(wR^2))/mean(weights))
    }

}



#Realized BiPower Variation (RBPVar) (RBPVar)
RBPVar = function(data){
  returns = as.vector(as.numeric(data));
  n = length(returns);
  rbpvar = (pi/2)*sum(abs(returns[1:(n-1)])*abs(returns[2:n]));
  return(rbpvar);
}

#MinRV:
MinRV = function(data){
  q = as.zoo(abs(as.numeric(data))); #absolute value
  q = as.numeric(rollapply(q, width=2, FUN=min,by = 1, align="left"));
  N = length(q)+1; #number of obs
  minrv = (pi/(pi-2))*(N/(N-1))*sum(q^2);
return(minrv)
}

#MedRV
MedRV = function(data){
  q = abs(as.numeric(data)); #absolute value
  q = as.numeric(rollmedian(q, k=3, align="center"));
  N = length(q) + 2;
  minrv = (pi/(6-4*sqrt(3)+pi))*(N/(N-2))*sum(q^2);
return(minrv)
}


##Multivariate measures:
#Realized Covariation (RCov):
RCov = function(data){
  data = na.locf(data,na.rm=FALSE);
  data = as.matrix(data);
  covariance = t(data)%*%data;
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
function (data, seasadjR = NULL, wfunction = "HR" , alphaMCD = 0.5, alpha = 0.001) 
{
    require(robustbase)
    if( is.null(dim(data) )){ 
          return( ROWVar( data , seasadjR = seasadjR , wfunction = wfunction , alphaMCD = alphaMCD , alpha = alpha ))
    }else{
       if (is.null(seasadjR)) {
           seasadjR = data
       }
	 data = as.matrix(data); seasadjR = as.matrix(seasadjR);
       intraT = nrow(data)
       N = ncol(data)
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
          wR = sqrt(weights) * data
          return((conHR(di = N, alpha = alpha) * t(wR) %*% wR)/mean(weights))
       }
       if( wfunction == "SR" ){
          weights[outlierindic] = k/outlyingness[outlierindic]
          wR = sqrt(weights) * data
          return((conhuber(di = N, alpha = alpha) * t(wR) %*% wR)/mean(weights))
       }
   }
}

#Realized BiPower Covariation (RBPCov)
RBPCov_bi = function(ts1,ts2){
  n = length(ts1);
  a = abs(ts1+ts2);
  b = abs(ts1-ts2);  
  first = as.numeric(a[1:(n-1)])*as.numeric(a[2:n]);
  last = as.numeric(b[1:(n-1)])*as.numeric(b[2:n]);
  result =  (pi/8)*sum(first-last);
  return(result);
}

RBPCov = 
function (data) 
{
    if( is.null(dim(data) )){ 
          return( RBPVar( data ))
    }else{
	 data  = as.matrix(data);
       n = dim(data)[2]
       cov = matrix(rep(0, n * n), ncol = n)
       diagonal = c()
       for (i in 1:n) {
          diagonal[i] = RBPVar(data[, i])
       }
       diag(cov) = diagonal
       for (i in 2:n) {
           for (j in 1:(i - 1)) {
               cov[i, j] = cov[j, i] = RBPCov_bi(data[, i], data[, j])
           }
       }
       return(cov)
   }
}

thresholdcov = function(data)	{
  data=as.matrix(data);
  n=dim(data)[1];						#number of observations
  delta = 1/n;
  rbpvars = apply(data,2,FUN=RBPVar);			#bipower variation per stock
  tresholds = 3*sqrt(rbpvars)*(delta^(0.49));	#treshold per stock
  tresmatrix = matrix(rep(tresholds,n),ncol=length(tresholds),nrow=n,byrow=TRUE);
  condition = data>tresmatrix;
  data[condition] = 0;
  cov = RCov(data);
return(cov);	
				}

#Realized Correlation (RCor)
RCor = function(data){
  data = na.locf(data,na.rm=FALSE);
  data = as.matrix(data);
  covariance = t(data)%*%data;
  sdmatrix = sqrt(diag(diag(covariance)));
  rcor = solve(sdmatrix)%*%covariance%*%solve(sdmatrix);
  return(rcor);
}
