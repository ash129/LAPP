% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/normalize.R
\name{normalize}
\alias{normalize}
\title{Normalize feature data and remove uninformative features}
\usage{
normalize(data)
}
\arguments{
\item{data}{data.frame with at least one numeric column}
}
\value{
new list with data.frame with normalized numeric columns and dropped constant numeric columns
and other information used to scale the data
}
\description{
\code{normalize()} takes a data.frame and normalizes all numeric columns, as well as
dropping constant ones.
}
