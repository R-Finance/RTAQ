previoustick = function(a){
a=as.vector(a);
b = a[length(a)];
return(b)
}

weightedaverage = function(a){
aa = as.vector(as.numeric(a[,1]));
bb = as.vector(as.numeric(a[,2]));
c = weighted.mean(aa,bb);
return(c)
}


##AGGREGATION;
aggregatets = function (ts, FUN = previoustick, on = "minutes", k = 1, weights = NULL,dropna=F)
{
  #Valid values for the argument "on" include: “secs” (seconds), “seconds”, “mins” (minutes), “minutes”,“hours”, “days”, “weeks”.

    if (is.null(weights)) {
        ep = endpoints(ts, on, k);
        ts2 = period.apply(ts, ep, FUN);
    }
    if (!is.null(weights)) {
        tsb = cbind(ts, weights)
        ep = endpoints(tsb, on, k)
        ts2 = period.apply(tsb, ep, FUN = weightedaverage)
    }
    if (on == "minutes" | on == "mins" | on == "secs" | on == 
        "seconds") {
        if (on == "minutes" | on == "mins") {
            secs = k * 60
        }
        if (on == "secs" | on == "seconds") {
            secs = k
        }
        a = .index(ts2) + (secs - .index(ts2)%%secs)
        ts3 = .xts(ts2, a)
    }
    if (on == "hours") {
        secs = 3600
        a = .index(ts2) + (secs - .index(ts2)%%secs)
        ts3 = .xts(ts2, a)
    }
    if (on == "days") {
        secs = 24 * 3600
        a = .index(ts2) + (secs - .index(ts2)%%secs) - (24 * 
            3600)
        ts3 = .xts(ts2, a)
    }
    if (on == "weeks") {
        secs = 24 * 3600 * 7
        a = (.index(ts2) + (secs - (.index(ts2) + (3L * 86400L))%%secs)) - 
            (24 * 3600)
        ts3 = .xts(ts2, a)
    }

    index(ts3) = as.timeDate(index(ts3));
	if(!dropna){
	if(on !="weeks"|on!="days"){
	if(on=="secs"|on=="seconds"){tby = "s"}
	if(on=="mins"|on=="minutes"){tby = "min"}
      if (on == "hours"){tby = "h"}
	by = paste(k,tby,sep=" ");
	allindex = as.timeDate(seq(start(ts3),end(ts3),by=by));
	xx = xts(rep(1,length(allindex)),order.by=allindex);
	ts3 = merge(ts3,xx)[,1];
	}#currently for weeks and days, na are still dropped
	}#end dropna if

    return(ts3)
}

#PRICE (specificity: opening price and previoustick)

agg_price = function(ts,FUN = previoustick,on="minutes",k=1){
##Return new timeseries as xts object where
##first observation is always the opening price
##subsequent observations are the closing prices over the interval with endpoint the timestamp of the result
##on indicates the type of period to aggregate over
##k indicates the number of periods to aggregate over
  ts2 = aggregatets(ts, FUN=previoustick, on, k);

  #adjustmet correct opening price
  date = strsplit(as.character(index(ts))," ")[[1]][1]
  realopen = "09:30:00";
  a = as.timeDate(paste(date,realopen));
  b = xts(ts[1],a);
  ts3 = c(b,ts2);

  ##adjustment for correct closing price:
  realclose = "16:00:00";
  aa = as.timeDate(paste(date,realclose));
  condition = index(ts3) < aa;
  ts3=ts3[condition];
  bb = xts(ts[length(ts)],aa);
  ts3 = c(ts3,bb);

  return(ts3);
}

#VOLUME: (specificity: always sum)
agg_volume = function(ts,FUN = sumN,on="minutes",k=5, includeopen=FALSE){

  if(!includeopen){ts3 = aggregatets(ts, FUN=sumN, on, k)}

  if(includeopen){
  ts2 = aggregatets(ts, FUN=sumN, on, k);
  date = strsplit(as.character(index(ts))," ")[[1]][1]
  realopen = "09:30:00";
  a = as.timeDate(paste(date,realopen));
  b = xts(as.numeric(ts[1]),a);
  ts3 = c(b,ts2);
  }
return(ts3)
}


###TRADES AGGREGATION:
agg_trades = function(tdata,on="minutes",k=5){
  ## Aggregates an entire trades xts object (tdata) over a "k"-minute interval.
  ## Returned xts-object contains: SYMBOL,EX,PRICE,SIZE.
  ## Variables COND, CR, G127 are dropped because aggregating them makes no sense.
  ## NOTE: first observation (opening price) always included.

  PRICE = agg_price(tdata$PRICE,on=on,k=k);
  SIZE = agg_volume(tdata$SIZE,on=on,k=k,includeopen=TRUE);
  EX = c(tdata$EX[1],aggregatets(tdata$EX, FUN=previoustick, on=on, k=k));
  SYMBOL = rep(tdata$SYMBOL[1],length(PRICE));
  all = data.frame(SYMBOL,EX,PRICE,SIZE);
  colnames(all) =c("SYMBOL","EX","PRICE","SIZE");
  ts = xts(all,index(SIZE));

  return(ts);
}


###QUOTES AGGREGATION:
agg_quotes = function(qdata,on="minutes",k=5){
  ## Aggregates an entire quotes xts object (qdata) object over a "k"-minute interval.
  ## Returned xts-object contains: SYMBOL,EX,BID,BIDSIZE,OFFER,OFFERSIZE.
  ## Variable MODE is dropped because aggregation makes no sense.
  ## "includeopen" determines whether to include the exact opening quotes.
  
  BID = agg_price(qdata$BID,on=on,k=k);
  OFFER = agg_price(qdata$OFFER,on=on,k=k);

  BIDSIZE = agg_volume(qdata$BIDSIZE,on=on,k=k,includeopen=TRUE);
  OFFERSIZE = agg_volume(qdata$OFFERSIZE,on=on,k=k,includeopen=TRUE);

  EX = agg_price(qdata$EX,on=on,k=k)
  SYMBOL = rep(qdata$SYMBOL[1],length(BIDSIZE));

  all = data.frame(SYMBOL,EX,BID,BIDSIZE,OFFER,OFFERSIZE);
  colnames(all) =c("SYMBOL","EX","BID","BIDSIZE","OFFER","OFFERSIZE");

  ts = xts(all,index(BIDSIZE));

  return(ts);
}

##LIQUIDITY AGGREGATION:
##Just combine aggregation functions and spot liquidity functions!

