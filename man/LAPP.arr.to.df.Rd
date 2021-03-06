% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/LAPP.arr.to.df.R
\name{LAPP.arr.to.df}
\alias{LAPP.arr.to.df}
\title{Convert list of 3D arrays into a 2D data.frame}
\usage{
LAPP.arr.to.df(features)
}
\arguments{
\item{features}{list of 3D arrays from make.features.list().}
}
\value{
a 2D data.frame
}
\description{
\code{LAPP.arr.to.df()} takes in a list of array from make.features.list() and 
converts it into a 2D data.frame with sequences as rows for all HLA, and 
position-by-property features as columns
}
