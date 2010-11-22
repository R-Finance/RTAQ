to.milliseconds <- function(td, startDate) {
  # function to extract milliseconds from “timeDate” time index
  # Based on initial code from Eric Zivot

  ms = as.numeric(td@Data) - as.numeric(startDate@Data)
  return(ms*1000)
}


as.realizedObject = function( ts, cts = FALSE , makeReturns = TRUE ){
require('realized');
   # takes the RTAQ data as input and outputs a list with each element a realizedobj of the corresponding day
   # Based on initial code from Eric Zivot
   days = unique( format( time(ts) , "%Y-%m-%d"))
   i = 1;
   out = list();

  #one day only
   if(length(days)==1){
   startDate = as.timeDate(paste( days, "00:00:00")); #"%Y-%m-%d %H:%M:%S"
   temp = list(  data = as.character(ts[days]) ,
                   milliseconds = to.milliseconds(td=index(ts[days]), startDate) );
   out = realizedObject( x = temp , makeReturns=makeReturns, cts=cts,
                    millisstart=9.5*60*60*1000, millisend=16*60*60*1000)
   }

  #multiple days: make list
   if(length(days)!=1){
   for( day in days ){

        startDate = as.timeDate(paste( day, "00:00:00")); #"%Y-%m-%d %H:%M:%S"

        temp = list(  data = as.character(ts[day]) ,
                   milliseconds = to.milliseconds(td=index(ts[day]), startDate) )

        out[[i]] = realizedObject( x = temp , makeReturns=makeReturns, cts=cts,
                    millisstart=9.5*60*60*1000, millisend=16*60*60*1000)
        # important to set cts to false, otherwise all zero prices!
        i = i + 1;
   }
   }

   #if( cts ){ print("cts=TRUE: for the milliseconds without observations prices are inserted")}
   return(out)
}