% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/lookfor_area.R
\name{lookfor_area}
\alias{lookfor_area}
\title{To Look for Area from Codes}
\usage{
lookfor_area(codes, data)
}
\arguments{
\item{codes}{id codes}

\item{data}{data after lookfor_area() function}
}
\value{
dataframe
}
\description{
To Look for Area from Codes
}
\examples{
\donttest{
df=get_data()
codes=c(32999999,320324,320323,320381)
lookfor_area(codes,df)
}
}
