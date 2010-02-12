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


ROWVar = function(data){
  returns = as.numeric(data)
  L = length(data);				#number of observations
  alpha = 0.05;
  k = qchisq(p=1-alpha,df=1);
  halfk = sqrt(k);
  out = univariateoutlyingness(returns); 	#returns outlyingness vector

  outindicator = out > k; 			#true if outlier, else false
  weights = matrix(rep(1,L),nrow=1);
  weights[1,outindicator] = k/out[outindicator];

  #correction factor:
  lambda1 = -2*halfk*dnorm(halfk)+2*pnorm(halfk)-1;
  lambda2 = 2*k*(1-pnorm(halfk));
  lambda4 = 2*halfk*dnorm(halfk)-2*k*(1-pnorm(halfk));
  c1_SR = 1/(lambda1+lambda2);
  c2_SR = 1/(2*(pnorm(halfk)-0.5)+lambda4);

  #result:
  ROWVar = (c1_SR/c2_SR)*rowSums(weights*returns^2)/mean(weights);
  return(ROWVar);
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


ROWCov = function( R , seasadjR=NULL , alphaMCD = 0.75, alpha = 0.05 )
{
   require(robustbase);
   # Function that computes the ROWQCov matrix, assumption of normality
   # R is a intraT*N matrix holding in column i de intraT returns of asset i
   if( is.null( seasadjR) ){ seasadjR = R };
   intraT = nrow(R); N = ncol(R)
   perczeroes = apply( seasadjR , 2, countzeroes )/intraT;
   select = c(1:N)[perczeroes<0.5] 
   seasadjRselect = seasadjR[,select];
   N = ncol(seasadjRselect); 
   MCDobject =  try(covMcd(x=seasadjRselect,alpha=alphaMCD)) ; 
   if(length(MCDobject$raw.mah)>1){
          outlyingness = MCDobject$raw.mah; 
   }else{
          print( c( "MCD cannot be calculated" ) )
   }
   k = qchisq(p=1-alpha,df=N) ;
   outlierindic = outlyingness>k;
   weights = rep(1,intraT);  
   weights[outlierindic] =  k/outlyingness[outlierindic]; 
   wR = sqrt(weights)*R;
   return( ( conhuber(di = N,alpha=alpha) * t(wR) %*% wR)/mean(weights) )
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

RBPCov = function(ts){
  n = dim(ts)[2];
  cov = matrix(rep(0,n*n),ncol=n);
  diagonal =c()

  for(i in 1:n)	{
  diagonal[i] = RBPVar(ts[,i]);
			}

  diag(cov) = diagonal;

  for(i in 2:n)		{
  for(j in 1:(i-1))	{
  cov[i,j] = cov[j,i] = RBPCov_bi(ts[,i],ts[,j]);
				}
				}

  return(cov)
}

#Realized Correlation (RCor)
RCor = function(ts){
  ts = na.locf(ts,na.rm=FALSE);
  covariance = t(ts)%*%ts;
  sdmatrix = sqrt(diag(diag(covariance)));
  rcor = solve(sdmatrix)%*%covariance%*%solve(sdmatrix);
  return(rcor);
}
