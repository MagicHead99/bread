% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/bmeta.R
\name{bmeta}
\alias{bmeta}
\title{Helper function generating nrow and colnames for the target file without loading it in memory}
\usage{
bmeta(file = NULL, ...)
}
\arguments{
\item{file}{String. Name or full path to a file compatible with data.table::fread()}

\item{...}{Arguments that must be passed to data.table::fread() like 'sep'.}
}
\value{
A list of 2
}
\description{
Generates a list that can be passed to the bfile_split() function
in order to indicate the number of rows and the name of columns in the file.
The output can be saved in a variable and provided in the meta_output
argument to save time. Counting rows in very large files can take some time.
}
\examples{
file <- system.file('extdata', 'test.csv', package = 'bread')
## Filtering on 2 columns, using regex.
meta_output <- bmeta(file = file)
}
