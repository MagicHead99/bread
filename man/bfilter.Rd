% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/bfilter.R
\name{bfilter}
\alias{bfilter}
\title{Pre-filters a data file using column values before loading it in memory}
\usage{
bfilter(
  file = NULL,
  patterns = NULL,
  filtered_columns = NULL,
  fixed = FALSE,
  ...
)
}
\arguments{
\item{file}{String. Name or full path to a file compatible with data.table::fread()}

\item{patterns}{Vector of strings. One or several patterns used to filter the data from the input file. Each element of the vector should correspond to the column to be filtered. Can use regular expressions.}

\item{filtered_columns}{Vector of strings or numeric. The columns to be filtered should be indicated through their names or their index number. Each element of the vector should correspond to the pattern with which it will be filtered.}

\item{fixed}{Logical. If TRUE, pattern is a string to be matched as is. Overrides all conflicting arguments.}

\item{...}{Arguments that must be passed to data.table::fread() like 'sep' and 'dec'.}
}
\value{
A dataframe
}
\description{
Simple wrapper for data.table::fread() allowing to filter data from a file
with the Unix 'grep' command. This method is useful if you want to load a file
too large for your available memory (and encounter the 'cannot allocate vector of size' error
for example).
}
\examples{
file <- system.file('extdata', 'test.csv', package = 'bread')
## Filtering on 2 columns, using regex.
bfilter(file = file, patterns = c('200[4-6]', "red"),
      filtered_columns = c('YEAR', 'COLOR'), sep = ';')
bfilter(file = file, patterns = c('2004|2005', 'red'),
      filtered_columns = c('YEAR', 'COLOR'), sep = ';')
## You need to use fixed = T if some patterns contain special characters
## that mess with regex like '(' and ')'
bfilter(file = file, patterns = 'orange (purple)',
      filtered_columns = 'COLOR', fixed = TRUE, sep = ';')
## If you do not provide the filtered_columns, you risk encountering
## false positives because the grep command filters on the whole file,
## not column by column. Here, the value 2002 will be found in the 'PRICE'
## column as well. The filtered_column argument will just make the script
## do a second pass with dplyr::filter() to remove false positives.
bfilter(file = file, patterns = '2002', sep = ';')
}
\keyword{allocate}
\keyword{big}
\keyword{file}
\keyword{filter}
\keyword{grep}
\keyword{size}
\keyword{vector}
