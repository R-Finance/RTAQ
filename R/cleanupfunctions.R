##### Help functions
## help function to make all time notation consistent
adjtime = function(z){ 
  zz = unlist(strsplit(z,":")); 
  if(nchar(zz[1])!=2){
  return(paste(paste(0,zz[1],sep=""),zz[2],zz[3],sep=":"))}
  return(z);
  }

########## DATA CLEAN-UP: FOR ALL DATA #####################

####FUNCTION TO FILTER EXCHANGE HOURS ONLY: ExchangeHoursOnly
ExchangeHoursOnly = function(ts, daybegin = "09:30:00",dayend="16:00:00")
{
    # a function to excerpt data within exchange trading hours
    # daybegin and dayend: two characters in the format of "HH:MM:SS",
    #                specifying the starting hour and minute and sec of an exhange
    #               trading day and the closing hour and minute and sec
    #                   of the trading day repectively
        
    if(!is(ts, "xts"))
        stop("ts must be an xts object")

  gettime = function(z){unlist(strsplit(as.character(z)," "))[2]};
  times1 = as.matrix(as.vector(as.character(index(ts))));
  times = apply(times1,1,gettime); 
  tdtimes = timeDate(times,format = "%H:%M:%S",FinCenter = "GMT",zone="GMT");

  #create timeDate begin and end
  tddaybegin = timeDate(daybegin,format = "%H:%M:%S",FinCenter = "GMT",zone="GMT");
  tddayend = timeDate(dayend,format = "%H:%M:%S",FinCenter = "GMT",zone="GMT");

  #select correct observations
  filteredts = ts[tdtimes>=tddaybegin & tdtimes<=tddayend];
  return(filteredts);
}


nozeroprices = function(ts){
####FUNCTION TO DELETE ZERO PRICES: nozeroprices
filteredts = ts[as.numeric(ts$PRICE)!= 0];
return(filteredts);
}


selectexchange = function(ts,exch="N"){ 
###FUNCTION TO SELECT THE OBSERVATIONS OF A SINGLE EXCHANGE: selectexchange
filteredts = ts[ts$EX==exch];
return(filteredts);
}


autoselectexchange = function(ts){
## AUTOSELECT EXCHANGE WITH HIGHEST NUMBER OF SHARES TRADED (for trades) ON:
#function returns ts with obs of only 1 exchange
#searches exchange with a maximum on the variable "SIZE"
  nobs=c();

  exchanges = c("Q","A","P","B","C","N","D","X","I","M","W","Z");
  exchangenames = c("NASDAQ","AMEX","ARCA","Boston","NSX","NYSE","NASD ADF and TRF","Philadelphia","ISE","Chicago","CBOE","BATS");
  

  z1 = sum(as.numeric(selectexchange(ts,"Q")$SIZE));
  z2 = sum(as.numeric(selectexchange(ts,"T")$SIZE));
  z = max(z1,z2);
  watchout = z == z2;
  nobs = cbind(nobs,z);

  for(i in 2:length(exchanges)) {
  z = sum(as.numeric(selectexchange(ts,exchanges[i])$SIZE));
  nobs = cbind(nobs,z); 
                        }

  exch = exchanges[max(nobs)==nobs];

  as.character(ts$EX[1]) == exchanges;
  namechosen = exchangenames[exch==exchanges];
  print(paste("The information of the",namechosen,"exchange was collected"));
  
  if(exch=="Q"&watchout){exch="T"}
  filteredts = ts[ts$EX==exch];
}


##### TRADE DATA SPECIFIC FUNCTIONS: ###################################
salescond = function(ts){ 
###DELETE ENTRIES WITH AN ABONORMAL SALES CONDITION
filteredts = ts[ts$COND == "0"|ts$COND == "E"|ts$COND == "F"];
return(filteredts);
}

##Merge same timestamp:
sumN = function(a){
  a = sum(as.numeric(a));
  return(a)
}

medianN = function(a){
  a = median(as.numeric(a));
  return(a)
}

maxvol = function(a){
  p = as.numeric(a[,1]);
  s = as.numeric(a[,2]);

  b = median(p[s == max(s)]);
  return(b);
}

waverage = function(a){
  p = as.numeric(a[,1]);
  s = as.numeric(a[,2]);

  b = sum(p*s/sum(s));
  return(b);
}

mergesametimestamp = function(ts,selection="median"){
  #find end points:
  ep = endpoints(ts,"secs");

  #size per second:
  size = period.apply(ts$SIZE,ep,sumN);

  #price per second:
  if(selection=="median"){price = period.apply(ts$PRICE,ep,medianN)}
  if(selection=="maxvolume"){price = period.apply(cbind(ts$PRICE,ts$SIZE),ep,maxvol)}
  if(selection=="weightedaverage"){price = period.apply(cbind(ts$PRICE,ts$SIZE),ep,waverage)}

  ##merge everything:
  selection = ep[2:length(ep)];
  ts2 = ts[selection];
  ts2$PRICE = price;
  ts2$SIZE = size;

return(ts2)
}

rmtradeoutliers = function(tdata,qdata){
##Function to delete entries with prices that are above the ask plus the bid-ask
##spread. Similar for entries with prices below the bid minus the bid-ask
##spread.
  data = matchtq(tdata,qdata);
  price = as.numeric(data$PRICE);
  bid = as.numeric(data$BID);
  offer = as.numeric(data$OFFER);
  spread = offer - bid;

  upper = offer+spread;
  lower = bid-spread;

  tdata[(price<upper) & (price>lower)];
  return(tdata);
}


#################       QUOTE SPECIFIC FUNCTIONS:       #################

nozeroquotes = function(ts){
####FUNCTION TO DELETE ZERO QUOTES: nozeroquotes
filteredts = ts[as.numeric(ts$BID)!= 0& as.numeric(ts$OFFER)!= 0];
return(filteredts);
}


autoselectexchangeq = function(ts){
####Autoselect exchange with highest value for (bidsize+offersize)
  nobs=c();
  exchanges = c("Q","A","P","B","C","N","D","X","I","M","W","Z");
  exchangenames = c("NASDAQ","AMEX","ARCA","Boston","NSX","NYSE","NASD ADF and TRF","Philadelphia","ISE","Chicago","CBOE","BATS");

  selected1 = selectexchange(ts,"Q");
  selected2 = selectexchange(ts,"T");
  z1 = sum(as.numeric(selected1$BIDSIZE)+as.numeric(selected1$OFFERSIZE));
  z2 = sum(as.numeric(selected2$BIDSIZE)+as.numeric(selected2$OFFERSIZE));
  z = max(z1,z2);
  watchout = z == z2;
  nobs = cbind(nobs,z);

  for(i in 2:length(exchanges)) {
  selected = selectexchange(ts,exchanges[i]);
  z = sum(as.numeric(selected$BIDSIZE)+as.numeric(selected$OFFERSIZE));
  nobs = cbind(nobs,z); 
                        }

  exch=exchanges[max(nobs)==nobs];

  namechosen = exchangenames[exch==exchanges];  
  print(paste("The information of the",namechosen,"exchange was collected"));

  if(exch=="Q"&watchout){exch="T"}

  filteredts = ts[ts$EX==exch];
  return(filteredts);
}


mergequotessametimestamp = function(ts,selection="median"){  ##FAST
  condition=selection=="median"|selection=="maxvolume"|selection=="weightedaverage";
  if(!condition){print(paste("WARNING:The result will be corrupted. Check whether",selection,"is an existing option for the attribute selection."))}

  #find end points:
  ep = endpoints(ts,"secs");

  #size per second:
  bidsize = period.apply(ts$BIDSIZE,ep,sumN);
  offersize =  period.apply(ts$OFFERSIZE,ep,sumN);

  #median per second:
  if(selection=="median"){
  bid = period.apply(ts$BID,ep,medianN);
  offer = period.apply(ts$OFFER,ep,medianN);
  }

  #maxvolume per second:
  if(selection=="maxvolume"){
  bid = period.apply(cbind(ts$BID,ts$BIDSIZE),ep,maxvol);
  offer = period.apply(cbind(ts$OFFER,ts$OFFERSIZE),ep,maxvol);
  }

  if(selection=="weightedaverage"){
  bid = period.apply(cbind(ts$BID,ts$BIDSIZE),ep,waverage);
  offer = period.apply(cbind(ts$OFFER,ts$OFFERSIZE),ep,waverage);
  }

  ##merge everything:
  selection = ep[2:length(ep)];
  ts2 = ts[selection];
  ts2$BID = bid;
  ts2$OFFER = offer;

  ts2$BIDSIZE = bidsize;
  ts2$OFFERSIZE = offersize;

return(ts2)
}


rmnegspread = function(ts){
##function to remove observations with negative spread
  condition = as.numeric(ts$OFFER)>as.numeric(ts$BID);
  ts[condition];
}


rmlargespread = function(ts,maxi=50){
##function to remove observations with a spread larger than 50 times the median spread that day
###WATCH OUT: works only correct if supplied input data consists of 1 day...
  spread = as.numeric(ts$OFFER)-as.numeric(ts$BID);
  condition = ((maxi*median(spread))>spread);
  return(ts[condition])
}


rmoutliers = function(ts,maxi=10,window=50,type="advanced"){
##function to remove entries for which the mid-quote deviated by more than 10 median absolute deviations 
##from a rolling centered median (excluding the observation under consideration) of 50 observations if type = "standard".

##if type="advanced":
##function removes entries for which the mid-quote deviates by more than 10 median absolute deviations
##from the variable "mediani".
##mediani is defined as the value closest to the midquote of these three options:
##1. Rolling centered median (excluding the observation under consideration)
##2. Rolling median of the following "window" observations
##3. Rolling median of the previous "window" observations

##NOTE: Median Absolute deviation chosen contrary to Barndorff-Nielsen et al.
  print("NOTE: This function is only useful for quotes NOT for trades");
  condition = c();
  halfwindow = round(window/2);
  midquote = (as.numeric(ts$BID)+as.numeric(ts$OFFER))/2;
  if(type=="standard"){
  for(i in (halfwindow+1):(dim(ts)[1]-halfwindow)){
    mid = midquote[i];
    vec = c(midquote[(i-halfwindow):(i-1)],midquote[(i+1):(i+halfwindow)]);
    mad = mad(vec);
    maxcriterion = median(vec)+maxi*mad;    
    mincriterion = median(vec)-maxi*mad;  
    condition[i-halfwindow] = mincriterion < mid & mid< maxcriterion;
  }
  }

if(type=="advanced"){
  for(i in (window+1):(dim(ts)[1]-window)){
    mid = midquote[i];

    vec = c(midquote[(i-halfwindow):(i-1)],midquote[(i+1):(i+halfwindow)]);
    vec2 = midquote[(i-window):(i-1)];
    vec3 = midquote[(i+1):(i+window)];

    medianv = c(median(vec),median(vec2),median(vec3));
    difference = abs(medianv-mid);
    mediani = medianv[min(difference) == difference];   
    mad = mad(vec);
    
    maxcriterion = mediani+maxi*mad;    
    mincriterion = mediani-maxi*mad;  
          
    condition[i-halfwindow] = mincriterion < mid & mid< maxcriterion;
  }

}

  condition = c(rep(TRUE,halfwindow),condition,rep(TRUE,halfwindow));
  ts[condition];
}


##########################  JUNK  #############################################################
#conv =function(z){ 
#  zz = unlist(strsplit(z,",")); 
#  return(as.numeric(paste(zz[1],zz[2],sep=".")))
#}
### make prices numeric ###
#x = as.matrix(as.vector(test2$PRICE))
#xx = apply(x,1,conv)
#test2$PRICE=xx

##appropriate days selection:
#create list of all trading days
#start = unlist(strsplit(as.character(start(myxts))," "))[1];
#end = unlist(strsplit(as.character(end(myxts))," "))[1];
#alldays = timeSequence(from = start, to = end, by = "day");
#alldays = alldays[isWeekday(alldays)];
