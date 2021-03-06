% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/read_Parvo_data.R
\name{read_Parvo_data}
\alias{read_Parvo_data}
\title{Summary of maximal Parvo data}
\usage{
read_Parvo_data(path)
}
\arguments{
\item{path}{Location of the Parvo files}
}
\value{
Creates a dataframe summarizing all of the Parvo metabolic files.
}
\description{
Creates a summary file of maximal data from a Parvo metabolic cart.

\emph{Data from the Parvo must be downloaded/saved in .PRN or .CSV formats.}
}
\seealso{
To make it easier to define the path (find the location of the files),
use the function: \code{\link[FRIENDanalysis]{folder_location}}.
}
