\name{hist_prices}
\alias{hist_prices}
\title{
  Historical Price Series.
}
\description{
  Retrieve historical data series and merge them.
}
\usage{
hist_prices(ticker, start, end = Sys.Date() - 1,
            adjust.return = NULL, return.class = "zoo")
}
\arguments{
  \item{ticker}{
    a character vector
}
  \item{start}{

    \code{\link{Date}}, a character string of format
    \code{"YYYY-MM-DD"}, or a year \code{"YYYY"}, which
    will be translated into \code{"YYYY-01-01"}
    
}
  \item{end}{

    \code{\link{Date}}, a character string of format
    \code{"YYYY-MM-DD"}, or a year \code{"YYYY"}, which
    will be translated into \code{"YYYY-12-31"}

  }
  \item{adjust.return}{

    if NULL (the default), \code{PX_LAST} will be
  loaded. Other valid values are \sQuote{net} or
  \sQuote{gross}.

}
  \item{return.class}{
    character
}
}
\details{

  The function calls \code{\link[Rblpapi]{bdh}} once
  for every ticker in ticker and merges the results.

  Currency tickers of the form \code{ABC Curncy}
  (i.e. cash) or \code{ABCABC Curncy} (i.e. base and
  counter currency are the same) always receive a price
  of 1. If data for only such tickers are requested,
  the returned data contain only two timestamps
  (\code{start} and \code{end}).  
  
}
\value{

  A series of class defined in
  \code{return.class}. Default and currently only
  supported option is \code{zoo}.

}
\author{
  Enrico Schumann
}
\seealso{
  \code{\link{closing_price}}
}
\examples{
\dontrun{
hist_prices("SPI Index", start = 2015, end = 2015)
}
}
