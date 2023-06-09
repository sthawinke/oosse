% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/boot632.R
\name{boot632}
\alias{boot632}
\title{The .632 bootstrap estimation of the MSE}
\usage{
boot632(y, x, id, fitFun, predFun)
}
\arguments{
\item{y}{The vector of outcome values}

\item{x}{The matrix of predictors}

\item{id}{the sample indices resampled with replacement}

\item{fitFun}{The function for fitting the prediction model}

\item{predFun}{The function for evaluating the prediction model}
}
\value{
The MSE estimate
}
\description{
The .632 bootstrap estimation of the MSE
}
\details{
The implementation follows \insertCite{Efron1997}{oosse}
}
\references{
\insertAllCited{}
}
\seealso{
\link{estMSE} \link{bootOob}
}
