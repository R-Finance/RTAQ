\name{ExchangeHoursOnly}
\Rdversion{1.1}
\alias{ExchangeHoursOnly}
\title{
Extract data from an xts object for the Exchange Hours Only
}
\description{
The function returns data within exchange trading hours
"daybegin" and "dayend". 
}

\usage{
ExchangeHoursOnly(data);
}

\arguments{
  \item{data}{ an xts object containing the time series data. }
  \item{daybegin}{ character in the format of "HH:MM:SS",
specifying the starting hour, minute and second of an exhange
trading day.}
  \item{dayend}{ character in the format of "HH:MM:SS",
specifying the closing hour, minute and second of an exhange
trading day.}
}

\section{Details}{}

\value{
xts object
}
\references{
}
\author{ Jonathan Cornelissen and Kris Boudt}