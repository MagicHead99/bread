# bread

bread offers simple wrapper functions of data.table::fread() that aim at making it easier to use the "cmd" argument 
with shell Unix (and sometimes PowerShell if available) commands like grep, wd and sed. The functions 
auto-generate those commands from arguments provided to the function.
The main use is to allow computers with low memory to analyze big files (the "b" in bread stands for "big files") and count 
rows, look up column names, subset rows by index numbers or value and select columns without hitting the memory limit (and 
the "cannot allocate vector of size" error.)
bread functions allow to analyze a 50Gb file with a computer with 8Gb of memory and:
 - split it in several smaller ones by number of rows or by values in one or many columns
 - count the number of rows
 - subset it by row number or column values
 - select only the relevant variables/columns

## Best practices
There are other (better) ways to do that, like - for example - loading a large file in a SQLite database. 
Or not working on huge csv files in the first place. But I happened to use those commands often in order to exploire data. 
If you have to, you hopefully won't have to delve right away into the fascinating grammar of Unix commands.

## Pre-requisites
bread makes heavy use of Unix commands like grep, sed, wc and cut. They are available by default in all Unix environments.
For Windows, you need to install those commands externally in order to simulate a Unix environment and make sure that the executables are in the Windows PATH variable.
To my knowledge, the simplest ways are to install RTools, Git or Cygwin. If they have been correctly installed (with the expected registry entries), they will be detected on loading the package and the correct directories will be added automatically to the PATH.

## Installation
```r
# Install bread from CRAN
install.packages("bread")
# Or the development version from GitHub:
# install.packages("bread")
devtools::install_github("MagicHead99/bread")
```
