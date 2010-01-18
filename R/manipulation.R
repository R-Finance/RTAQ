#MANIPULATION FUNCTIONS:

TAQload = function(ticker,from,to,trades=TRUE,quotes=FALSE,datasource="V:\\Jobstudent\\TAQdata"){
##Function to load the taq data from a certain stock 
#From&to (both included) should be in the format "%Y-%m-%d" e.g."2008-11-30"
  dates = timeSequence(from,to, format = "%Y-%m-%d", FinCenter = "GMT")
  dates = dates[isBizday(dates, holidays = holidayNYSE(2004:2010))];

  if(trades){
  for(i in 1:length(dates)){
  datasourcex = paste(datasource,"\\",dates[i],sep="");
  filename = paste(datasourcex,"\\",ticker,"_trades.RData",sep="");

  ifmissingname = paste(datasourcex,"\\missing_",ticker,".RData",sep="");  
  if(file.exists(ifmissingname)){stop(paste("no trades available on ",dates[i],sep=""))}
  if(file.exists(ifmissingname)==FALSE){
  load(filename);
  if(i==1){totaldata=tdata};
  if(i>=1){totaldata=rbind(totaldata,tdata)};
  rm(tdata);
				}
				}
				}

  if(quotes){
  for(i in 1:length(dates)){
  datasourcex = paste(datasource,"\\",dates[i],sep="");
  filename = paste(datasourcex,"\\",ticker,"_quotes.RData",sep="");
  ifmissingname = paste(datasourcex,"\\missingquotes_",ticker,".RData",sep="");
  
  if(file.exists(ifmissingname)){stop(paste("no quotes available on ",dates[i],sep=""))}
  if(file.exists(ifmissingname)==FALSE){
  load(filename);
  if(i==1){totaldataq=qdata};
  if(i>=1){totaldataq=rbind(totaldataq,qdata)};
  rm(qdata);
				}
				}
				}

  if(trades&quotes){return(list(trades = totaldata,quotes=totaldataq))}
  if(trades==TRUE & quotes==FALSE){return(totaldata)}
  if(trades==FALSE & quotes==TRUE){return(totaldataq)}
  }


matchtq = function(tdata,qdata,adjustment=2){ ##FAST VERSION
  tt = dim(tdata)[2];  
  index(qdata) = index(qdata) + adjustment;

  #merge:
  merged = merge(tdata,qdata);

  ##fill NA's:
  merged[,((tt+1):dim(merged)[2])] = na.locf(as.zoo(merged[,((tt+1):dim(merged)[2])]), na.rm=FALSE);

  #Select trades:
  index(tdata)=as.POSIXct(index(tdata));
  index(merged)=as.POSIXct(index(merged));  
  merged = merged[index(tdata)];

  #return useful parts:
  merged = merged[,c((1:tt),((tt+3):(dim(merged)[2])))];

  ##a bit rough but otherwise opening price disappears...
  merged = as.xts(na.locf(as.zoo(merged),fromLast=TRUE));

  index(merged) = as.timeDate(index(merged));
  return(merged)
}

matchtq_old = function(tdata,qdata,adjustment=2){ ##FAST VERSION
  tt = dim(tdata)[2];  
  index(qdata) = index(qdata) + adjustment;
  
  #merge:
  counter = xts(as.character(1:dim(qdata)[1]),order.by=index(qdata))#an integer for every quote
  merged = cbind(qdata,counter);
  merged = merge(tdata,merged);
  
  ##fill NA's:
  merged[,((tt+1):dim(merged)[2])] = na.locf(as.zoo(merged[,((tt+1):dim(merged)[2])]), na.rm=FALSE);
  
  #Select trades:
  merged = merged[index(tdata)];
  
  #Remove duplicated quotes:
  merged = merged[!duplicated(merged[,dim(merged)[2]])];

  #return usefull parts:
  merged = merged[,c((1:tt),((tt+3):(dim(merged)[2]-1)))];

  return(merged)
}



gettradedir = function(data){
##Function returns a vector with the inferred trade direction:
##NOTE: the value of the first (and second) observation should be ignored if price=midpoint for the first (second) observation.
  bid = as.numeric(data$BID);
  offer = as.numeric(data$OFFER);
  midpoints = (bid + offer)/2;
  price = as.numeric(data$PRICE);
 
  buy1 = price > midpoints; #definitely a buy
  equal = price == midpoints;
  dif1 = c(TRUE,0 < price[2:length(price)]-price[1:(length(price)-1)]);#for trades=midpoints: if uptick=>buy
  equal1 = c(TRUE,0 == price[2:length(price)]-price[1:(length(price)-1)]);#for trades=midpoints: zero-uptick=>buy
  dif2 = c(TRUE,TRUE,0 < price[3:length(price)]-price[1:(length(price)-2)]);

  buy = buy1 | (dif1 & equal) | (equal1 & dif2 & equal);

  buy[buy==TRUE]=1;
  buy[buy==FALSE]=-1;
  
  return(buy);
}


es = function(data){
#returns the effective spread as xts object
  bid = as.numeric(data$BID);
  offer = as.numeric(data$OFFER);
  midpoints = (bid + offer)/2;
  price = as.numeric(data$PRICE);
  d = gettradedir(data);
 
  es=xts(2*d*(price-midpoints),order.by=index(data));
  return(es);
}


rs = function(data,tdata,qdata){
###Function returns the realized spread as an xts object
#Please note that the returned object can contain less observations that the original "data"
#because of the need to find quotes that match the trades 5 min ahead

#arguments
#data=> xts object containing matched trades and quotes
#tdata and qdata, the xts object containing the trades and quotes respectively

  ##First part solves the problem that unequal number of obs (in data and data2) is possible when computing the RS
  data2 = matchtq(tdata,qdata,300,maxit=50);
  if(dim(data2)[1]>dim(data)[1]){
  condition = as.vector(as.character(index(data2)))%in%as.vector(as.character(index(data)));
  data2 = subset(data2,condition,select=1:(dim(data)[2]));
  data = subset(data,as.vector(as.character(index(data)))%in%as.vector(as.character(index(data2))),select=1:(dim(data2)[2]));
  }

  if(dim(data2)[1]<dim(data)[1]){
  condition = as.vector(as.character(index(data)))%in%as.vector(as.character(index(data2)));
  data = subset(data,condition,select=1:(dim(data2)[2]));
  data2 = subset(data2,as.vector(as.character(index(data2)))%in%as.vector(as.character(index(data))),select=1:(dim(data)[2]));
  }


  bid = as.numeric(data2$BID);
  offer = as.numeric(data2$OFFER);
  midpoints = (bid + offer)/2;
  price = as.numeric(data$PRICE);
  d = gettradedir(data);
  rs = 2*d*(price-midpoints);

  rs_xts = xts(rs,order.by=index(data));
  return(rs_xts);
}

value_trade = function(data){
#returns the trade value as xts object
  price = as.numeric(data$PRICE);
  size = as.numeric(data$SIZE);
  
  value = xts(price*size,order.by=index(data));
  return(value);
}

signed_value_trade = function(data){
#returns the signed trade value as xts object
  price = as.numeric(data$PRICE);
  size = as.numeric(data$SIZE);
  d = gettradedir(data);

  value = xts(d*price*size,order.by=index(data));
  return(value);
}


signed_trade_size = function(data){
#returns the signed size of the trade as xts object
  size = as.numeric(data$SIZE);
  d = gettradedir(data);

  value = xts(d*size,order.by=index(data));
  return(value);
}

di_diff = function(data){
#returns the depth imbalance (as a difference) as xts object
  bidsize = as.numeric(data$BIDSIZE);
  offersize = as.numeric(data$OFFERSIZE);

  d = gettradedir(data);
  di = (d*(offersize-bidsize))/(offersize+bidsize);
  di_xts = xts(di,order.by=index(data));
  return(di_xts);
}

di_div = function(data){
#returns the depth imbalance (as a ratio) as xts object
  bidsize = as.numeric(data$BIDSIZE);
  offersize = as.numeric(data$OFFERSIZE);
  d = gettradedir(data);

  di = (offersize/bidsize)^d;
  di_xts = xts(di,order.by=index(data));
  return(di_xts);
}

pes = function(data){
#returns the Proportional Effective Spread as xts object
  es = es(data);
  bid = as.numeric(data$BID);
  offer = as.numeric(data$OFFER);
  midpoints = (bid + offer)/2;

  pes = 100*es/midpoints
  pes_xts = xts(pes,order.by=index(data));
  return(pes_xts);
}

prs = function(data){
#returns the Proportional Realized Spread as xts object
  rs = rs(data);
  bid = as.numeric(data$BID);
  offer = as.numeric(data$OFFER);
  midpoints = (bid + offer)/2;

  prs = 100*rs/midpoints
  prs_xts = xts(prs,order.by=index(data));
  return(prs_xts);
}

price_impact = function(data){
#returns the Price impact as xts object
  rs = rs(data);
  es = es(data);

  pi = (es-rs)/2;
  pi_xts = xts(pi,order.by=index(data));
  return(pi_xts);
}

prop_price_impact = function(data){
#returns the Proportional Price impact as xts object
  rs = rs(data);
  es = es(data);
  bid = as.numeric(data$BID);
  offer = as.numeric(data$OFFER);
  midpoints = (bid + offer)/2;

  prop_pi = (100*(es-rs)/2)/midpoints;
  prop_pi_xts = xts(prop_pi,order.by=index(data));
  return(prop_pi_xts);
}


tspread = function(data){
#returns the half traded spread as xts object
  bid = as.numeric(data$BID);
  offer = as.numeric(data$OFFER);
  midpoints = (bid + offer)/2;
  price = as.numeric(data$PRICE);
  d = gettradedir(data);

  ts = xts(d*(price-midpoints),order.by=index(data));
  return(ts);
}

pts = function(data){
#returns the proportional half traded spread as xts object
  bid = as.numeric(data$BID);
  offer = as.numeric(data$OFFER);
  midpoints = (bid + offer)/2;
  price = as.numeric(data$PRICE);
  d = gettradedir(data);
  pts = (d*(price-midpoints))/midpoints;

  pts_xts = xts(pts,order.by=index(data));
  return(ts);
}

p_return_sqr = function(data){
#returns the squared log return on Trade prices as xts object
  price = as.numeric(data$PRICE);
  return = c(0,log(price[2:length(price)])-log(price[1:length(price)-1]));
  sqr_return = return^2;

  sqr_return_xts = xts(sqr_return,order.by=index(data));
  return(sqr_return_xts);
}

p_return_abs = function(data){
#returns the absolute log return on Trade prices as xts object
  price = as.numeric(data$PRICE);
  return = c(0,log(price[2:length(price)])-log(price[1:length(price)-1]));
  abs_return = abs(return);

  abs_return_xts = xts(abs_return,order.by=index(data));
  return(sqr_return_xts);
}

qs = function(data){
#returns the quoted spread as xts object
  bid = as.numeric(data$BID);
  offer = as.numeric(data$OFFER);
  qs = offer-bid;

  qs_xts = xts(qs,order.by=index(data));
  return(qs_xts);
}

pqs = function(data){
#returns the proportional quoted spread as xts object
  bid = as.numeric(data$BID);
  offer = as.numeric(data$OFFER);
  midpoints = (bid + offer)/2;
  qs = offer-bid;
  pqs = 100*qs/midpoints;

  pqs_xts = xts(pqs,order.by=index(data));
  return(pqs_xts);
}

logqs = function(data){
#returns the logarithm of the quoted spread as xts object
  bid = as.numeric(data$BID);
  offer = as.numeric(data$OFFER);
  logqs = log(offer/bid);

  logqs_xts = xts(logqs,order.by=index(data));
  return(logqs_xts);
}

logsize = function(data){
#returns the log quoted size as xts object
  bidsize = as.numeric(data$BIDSIZE);
  offersize = as.numeric(data$OFFERSIZE);
  logsize = log(bidsize)+log(offersize);

  logsize_xts = xts(logsize,order.by=index(data));
  return(logsize_xts);
}

qslope = function(data){
#returns the quoted slope as xts object
  logsize = logsize(data);
  qs = qs(data);

  qslope = qs/logsize;

  qslope_xts = xts(qslope,order.by=index(data));
  return(qslope_xts);
}

logqslope = function(data){
#returns the log quoted slope as xts object
  logqs = logqs(data);
  logsize = logsize(data);
  
  logqslope = logqs/logsize;

  logqslope_xts = xts(logqslope,order.by=index(data));
  return(logqslope_xts);
}



mq_return_sqr = function(data){
#returns midquote squared returns slope as xts object
  mq_return = mq_return(data);
  
  mq_return_sqr = mq_return^2;

  mq_return_sqr_xts = xts(mq_return_sqr,order.by=index(data));
  return(mq_return_sqr_xts);
}

mq_return_abs = function(data){
#returns absolute midquote returns slope as xts object
  mq_return = mq_return(data);
  
  mq_return_abs = abs(mq_return);

  mq_return_abs_xts = xts(mq_return_abs,order.by=index(data));
  return(mq_return_abs_xts);
}


liquidity = function(data,tdata,qdata){
##Function computes many liquidity measures and returns an xts object containing them

##First part solves the problem that unequal number of obs (in data and data2) is possible when computing the RS
  data2 = matchtq(tdata,qdata,300,maxit=50);
  if(dim(data2)[1]>dim(data)[1]){
  condition = as.vector(as.character(index(data2)))%in%as.vector(as.character(index(data)));
  data2 = subset(data2,condition,select=1:(dim(data)[2]));
  data = subset(data,as.vector(as.character(index(data)))%in%as.vector(as.character(index(data2))),select=1:(dim(data2)[2]));
  }
  if(dim(data2)[1]<dim(data)[1]){
  condition = as.vector(as.character(index(data)))%in%as.vector(as.character(index(data2)));
  data = subset(data,condition,select=1:(dim(data2)[2]));
  data2 = subset(data2,as.vector(as.character(index(data2)))%in%as.vector(as.character(index(data))),select=1:(dim(data)[2]));
  }

##Variables needed for the computation of the liquidity measures
  bid = as.numeric(data$BID);
  offer = as.numeric(data$OFFER);
  midpoints = (bid + offer)/2;
  price = as.numeric(data$PRICE);
  size = as.numeric(data$SIZE);
  d = gettradedir(data);
  bidsize = as.numeric(data$BIDSIZE);
  offersize = as.numeric(data$OFFERSIZE);
  return = c(0,log(price[2:length(price)])-log(price[1:length(price)-1]));
  mq_return = mq_return(data);
  midpoints2 = (as.numeric(data2$BID)+as.numeric(data2$OFFER))/2;

##Liquidity measures:
  es = 2*d*(price-midpoints);
  rs = 2*d*(price-midpoints2);
  value_trade = price*size;
  signed_value_trade = d*price*size;
  signed_trade_size = d*size;
  di_diff = (d*(offersize-bidsize))/(offersize+bidsize);
  di_div = (offersize/bidsize)^d;
  pes = 100*es/midpoints;
  prs = 100*rs/midpoints;
  price_impact = (es-rs)/2;
  prop_price_impact = (100*price_impact)/midpoints;
  tspread = d*(price-midpoints);
  pts = ts/midpoints;
  p_return_sqr = return^2;
  p_return_abs = abs(return);
  qs = offer-bid;
  pqs = 100*qs/midpoints;
  logqs = log(offer/bid);
  logsize = log(bidsize)+log(offersize);
  qslope = qs/logsize;
  logqslope = logqs/logsize;
  mq_return_sqr = mq_return^2;
  mq_return_abs = abs(mq_return);

liquid = cbind(es,rs,value_trade,signed_value_trade,di_diff,di_div,pes,prs,price_impact,
prop_price_impact,ts,pts,p_return_sqr,p_return_abs,qs,pqs,logqs,logsize,qslope,
logqslope,mq_return_sqr,mq_return_abs);

names = c("es","rs","value_trade","signed_value_trade","di_diff","di_div","pes","prs","price_impact","prop_price_impact","ts","pts","p_return_sqr","p_return_abs","qs","pqs","logqs","logsize","qslope","logqslope","mq_return_sqr","mq_return_abs");
colnames(liquid) = names;

return(liquid);
}

##help_function:
mq_return = function(data){
#function returns the midquote logreturns as xts object
  bid = as.numeric(data$BID);
  offer = as.numeric(data$OFFER);
  midpoints = (bid + offer)/2;
  mq_return = c(0,log(midpoints[2:length(midpoints)])-log(midpoints[1:length(midpoints)-1]));

  mq_return_xts = xts(mq_return,order.by=index(data));
  return(mq_return_xts);
}