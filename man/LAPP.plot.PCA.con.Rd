% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/LAPP.plot.PCA.con.R
\name{LAPP.plot.PCA.con}
\alias{LAPP.plot.PCA.con}
\title{Plot PCA contributions}
\usage{
LAPP.plot.PCA.con(pca, dim1 = 1, dim2 = 2, top.n = 10)
}
\arguments{
\item{pca}{Modified PCA object returned by LAPP.PCA().}

\item{dim1}{Index of PC to plot along the x-axis. Defaults to 1}

\item{dim2}{Index of PC to plot along the y-axis. Defaults to 2}

\item{top.n}{Number of top contributions to highlight on the plot. Defaults to 10.}
}
\description{
\code{LAPP.plot.PCA.con()} takes in a modified PCA object returned by LAPP.PCA() and plots the
variable contributions of two principal components.
 Uses ggplot2 and ggrepel.
}
