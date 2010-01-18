#TRADES CLEANUP WRAPPER
tradescleanup = function(from="2008-01-03",to="2008-01-03",datasource,datadestination,ticker){
  dates = timeSequence(from,to, format = "%Y-%m-%d", FinCenter = "GMT");
  dates = dates[isBizday(dates, holidays = holidayNYSE(2004:2010))];

  for(j in 1:length(dates)){
  datasourcex = paste(datasource,"\\",dates[j],sep="");
  datadestinationx = paste(datadestination,"\\",dates[j],sep="");

  for(i in 1:length(ticker)){
  dataname = paste(ticker[i],"_trades.RData",sep="");
  load(paste(datasourcex,"\\",dataname,sep=""));

  if(class(tdata)!="try-error"){
  exchange = exchanges[(exchanges[,1]==ticker[i]),2];  

  ##actual clean-up: 
  ##general:
  tdata = try(nozeroprices(tdata));
  tdata = try(selectexchange(tdata,exch=exchange));

  ##trade specific:
  tdata = try(salescond(tdata));
  tdata = try(mergesametimestamp(tdata));

  save(tdata, file = paste(datadestinationx,"\\",dataname,sep=""));
  }

  if(class(tdata)=="try-error")	{
  abc=1;
  save(abc, file = paste(datadestinationx,"\\missing_",ticker[i],".RData",sep=""));
						}
}
}
}

#TRADES SPECIFIC
tradescleanup_finalop = function(from="2008-01-03",to="2008-01-03",datasource,datadestination,ticker){
  dates = timeSequence(from,to, format = "%Y-%m-%d", FinCenter = "GMT");
  dates = dates[isBizday(dates, holidays = holidayNYSE(2004:2010))];

  for(j in 1:length(dates)){
  datasourcex = paste(datasource,"\\",dates[j],sep="");
  datadestinationx = paste(datadestination,"\\",dates[j],sep="");

  for(i in 1:length(ticker)){
  dataname = paste(ticker[i],"_trades.RData",sep="");
  dataname2 = paste(ticker[i],"_quotes.RData",sep="");

  #Missing file??
  m1 = paste(datasourcex,"\\missing_",ticker[i],".RData",sep="");
  m2 = paste(datasourcex,"\\missingquotes_",ticker[i],".RData",sep="");
  miscondition = file.exists(m1)|file.exists(m1);
  a=FALSE;#check whether tried to clean

  if(!miscondition){
  #load trades and quotes
  load(paste(datasourcex,"\\",dataname,sep=""));
  load(paste(datasourcex,"\\",dataname2,sep=""));

  #1 cleaning procedure that needs cleaned trades and quotes
  tdata = try(rmtradeoutliers(tdata,qdata));


  #save
  save(tdata, file = paste(datadestinationx,"\\",dataname,sep=""));
  a=TRUE;
					}

  if(a==TRUE){a=(class(tdata)=="try-error")}

  if(miscondition|a)	{
  abc=1;
  save(abc, file = paste(datadestinationx,"\\missing_",ticker[i],".RData",sep=""));
						}
}
}
}

##QUOTES CLEAN-UP WRAPPER
quotescleanup = function(from,to,datasource,datadestination,ticker){
  dates = timeSequence(from,to, format = "%Y-%m-%d", FinCenter = "GMT");
  dates = dates[isBizday(dates, holidays = holidayNYSE(2004:2010))];

  for(j in 1:length(dates)){
  datasourcex = paste(datasource,"\\",dates[j],sep="");
  datadestinationx = paste(datadestination,"\\",dates[j],sep="");

  for(i in 1:length(ticker)){
  dataname = paste(ticker[i],"_quotes.RData",sep="");
  load(paste(datasourcex,"\\",dataname,sep=""));

  if(class(qdata)!="try-error"){
  exchange = exchanges[(exchanges[,1]==ticker[i]),2];  
  if(exchange=="Q"){exchange="T"}

  ##actual clean-up:
  ##general:
  qdata = try(nozeroquotes(qdata));
  qdata = try(selectexchange(qdata,exch=exchange));

  ##quote specific:
  qdata = try(rmnegspread(qdata));
  qdata = try(rmlargespread(qdata));
  qdata = try(mergequotessametimestamp(qdata));
  qdata = try(rmoutliers(qdata));

  save(qdata, file = paste(datadestinationx,"\\",dataname,sep=""));
  }

  if(class(qdata)=="try-error"){
  abc=1;
  save(abc, file = paste(datadestinationx,"\\missingquotes_",ticker[i],".RData",sep=""));
  }
}
}
}


