% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/bootOob.R
\name{bootOob}
\alias{bootOob}
\title{The oob bootstrap (smooths leave-one-out CV)}
\usage{
bootOob(y, x, id, fitFun, predFun)
}
\arguments{
\item{y}{The vector of outcome values}

\item{x}{The matrix of predictors}

\item{id}{sample indices sampled with replacement}

\item{fitFun}{The function for fitting the prediction model}

\item{predFun}{The function for evaluating the prediction model}
}
\value{
matrix of errors and inclusion times
}
\description{
The oob bootstrap (smooths leave-one-out CV)
}
\details{
The implementation follows \insertCite{Efron1997}{oosse}
}
\references{
\insertAllCited{}
}
\seealso{
\link{estMSE} \link{boot632}
}
