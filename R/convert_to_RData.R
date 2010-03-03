##########HELPFUNCTION######
readdata = function(path=NULL, extention="txt",header=F){
#extention should either be "txt" or "csv"
if(!(extention=="txt"|extention=="csv")){print("Please select a supported extention")}
#load txt
if(extention == "txt"){
fullpath = paste(path,".txt",sep="");
data = try(read.delim(fullpath,sep="",header=header,dec=","),silent=TRUE);
}
if(extention == "csv"){
fullpath = paste(path,".csv",sep="");
data = try(read.delim(fullpath,sep=",",header=header,dec="."),silent=TRUE);
}

return(data);
}
############################

convert = function(from,to,datasource,datadestination,trades=TRUE,quotes=TRUE,ticker,dir=F,extention="txt",header=F,tradecolnames=NULL,format="%m/%d/%Y %H:%M:%S"){
  dates = timeSequence(from,to, format = "%Y-%m-%d", FinCenter = "GMT")
  dates = dates[isBizday(dates, holidays = holidayNYSE(2004:2010))];

  if(dir)	{
  dir.create(datadestination);
  for(i in 1:length(dates))	{
  dirname = paste(datadestination,"\\",as.character(dates[i]),sep="")
  dir.create(dirname);
 					}			
		}
  
  for(i in 1:length(dates)){
  datasource = paste(datasource,"\\",dates[i],sep="");
  datadestination = paste(datadestination,"\\",dates[i],sep="");
  if(trades==TRUE){convert_trades(datasource,datadestination,ticker,extention=extention,header=header,tradecolnames=tradecolnames,format=format)}
  if(quotes==TRUE){convert_quotes(datasource,datadestination,ticker)}
  }
}

convert_trades = function(datasource,datadestination,ticker,extention="txt",header=F,tradecolnames=NULL,format="%m/%d/%Y %H:%M:%S"){
  setwd(datasource);
  adjtime = function(z){ 
  zz = unlist(strsplit(z,":")); 
  if(nchar(zz[1])!=2){
  return(paste(paste(0,zz[1],sep=""),zz[2],zz[3],sep=":"))}
  return(z);
  }

  for(i in 1:length(ticker)){
  tfile_name = paste(ticker[i],"_trades",sep="");
  tdata = try(readdata(path=tfile_name, extention=extention,header=header),silent=TRUE);
  error = is.null(dim(tdata)); 

  if(error)
  {print(paste("no trades for stock",ticker[i]));
  missingt = rbind(missingt,c(currentdate,ticker[i]));
  }
  if(error==FALSE){

  #assign column names
  if(header==FALSE){
  if(is.null(tradecolnames)){
  tradecolnames=c("SYMBOL","DATE","EX","TIME","PRICE","SIZE","COND","CR","G127");
  colnames(tdata)= tradecolnames;
  }else{
  colnames(tdata)= tradecolnames;
  }
  }

  ### solve issue when there is no COND ###
  cond=tdata$COND[is.na(tdata$G127)];
  cr=tdata$CR[is.na(tdata$G127)];

  tdata$COND[is.na(tdata$G127)]=0;
  tdata$CR[is.na(tdata$G127)]= cond;
  tdata$G127[is.na(tdata$G127)] = cr;
  rm(cond,cr);

  ## solve issue that time notation is inconsequent (no 09h but 9h)
  oldtime = as.matrix(as.vector(tdata$TIME));
  newtime = apply(oldtime,1,adjtime);
  tdata$TIME = newtime;
  rm(oldtime,newtime);

  ##make xts object ##
  tdobject=timeDate(paste(as.vector(tdata$DATE), as.vector(tdata$TIME)),format = format,FinCenter = "GMT",zone="GMT");
  tdata = xts(tdata,order.by=tdobject);
  tdata=tdata[,c("SYMBOL","EX","PRICE","SIZE","COND","CR","G127")];

  rm(tdobject);
  }

  xts_name = paste(ticker[i],"_trades.RData",sep="");
  setwd(datadestination);
  save(tdata, file = xts_name);
  }
  }


convert_quotes = function(datasource,datadestination,ticker){
  setwd(datasource);
  adjtime = function(z){ 
  zz = unlist(strsplit(z,":")); 
  if(nchar(zz[1])!=2){
  return(paste(paste(0,zz[1],sep=""),zz[2],zz[3],sep=":"))}
  return(z);
  }

  for(i in 1:length(ticker)){
  klassen = c(rep("character",4),rep("real",5));
  qfile_name = paste(ticker[i],"_quotes.txt",sep="");

  qdata=try(read.delim(qfile_name,sep="",header=F,dec=",",colClasses=klassen),silent=TRUE);

  error = qdata[1]== "Error in read.table(file = file, header = header, sep = sep, quote = quote,  : \n  no lines available in input\n";
  if(error[1])
  {print(paste("no quotes for stock",ticker[i])); 
  missingq=rbind(missingq,c(currentdate,ticker[i]));
  }
  if((error==FALSE)[1]){
  #assign column names
  tradecolnames=c("SYMBOL","DATE","EX","TIME","BID","BIDSIZE","OFFER","OFFERSIZE","MODE");
  colnames(qdata)=tradecolnames

  ####important because of data mistakes,must become something like "ticker"
  qdata = qdata[qdata$SYMBOL==ticker[i],]

  ## solve issue that time notation is inconsequent (no 09h but 9h)
  oldtime = as.matrix(as.vector(qdata$TIME));
  newtime = apply(oldtime,1,adjtime); #check if function in this file
  qdata$TIME = newtime;
  rm(oldtime,newtime);

  ##make xts object 
  test=paste(as.vector(qdata$DATE), as.vector(qdata$TIME))
  tdobject=timeDate(test,format = "%m/%d/%Y %H:%M:%S",FinCenter = "GMT",zone="GMT");
  qdata = xts(qdata,order.by=tdobject);
  qdata = qdata[,c(1,3,5,6,7,8,9)];
  }

  xts_name = paste(ticker[i],"_quotes.RData",sep="");
  setwd(datadestination);
  save(qdata, file = xts_name);
  }
  }



