\name{UNF-package}
\alias{UNF-package}
\alias{UNF}
\docType{package}
\title{Tools for creating universal numeric fingerprints for data}
\description{Computes a univeral numeric fingerprint of data objects.}
\details{
This package calculates a Universal Numeric Fingerprint (UNF) on an R data object. UNF is a crypographic hash or signature that can be used to uniquely identify a (version of a) dataset, or a subset thereof. UNF is used by the \href{http://www.thedata.org}{Dataverse} archives and this package can be used to verify a dataset against one listed available in a Dataverse study (e.g., as returned by the \href{http://cran.r-project.org/web/packages/dvn/}{dvn} package).

A UNF is created by rounding data values (or truncating strings) to a known number of digits (or characters), representing those values in a standard form (as 8-bit [for versions 4.1 and 5] or 32-bit [for versions 3 and 4] unicode-formatted strings), and applying a fingerprinting method (a cryptographic hashing function) to this representation (md5 for versions 3 and 4 or sha256 for versions 4.1 and 5). UNFs are computed from data values (independent of variable naming and column arrangement), so they directly reflect the internal representation of the data.

A UNF differs from an ordinary file checksum in several important ways:

1. \emph{UNFs are format independent.} The UNF for a dataset will be the same regardless of whether the data is saved as a R binary format, SAS formatted file, Stata formatted file, etc., but file checksums will differ. The UNF is also independent of variable arrangement and naming, which can be unintentionally changed during file reading.

2. \emph{UNFs are robust to insignificant rounding error.} This important when dealing with floating-point numeric values. A UNF will also be the same if the data differs in non-significant digits, a file checksum not.

3. \emph{UNFs detect misinterpretation of the data by the statistical software.} If the statistical software misreads the file, the resulting UNF will not match the original, but the file checksums may match. For example, numeric values read as character will produce a different UNF than those values read in as numerics.

4. \emph{UNFs are strongly tamper resistant.} Any accidental or intentional changes to data values will change the resulting UNF. Most file checksums and descriptive statistics detect only certain types of changes.

\tabular{ll}{
Package: \tab UNF\cr
Type: \tab Package\cr
Version: \tab 2.0\cr
Date: \tab 2013-12-07\cr
License: \tab GPL-2\cr
}
}
\author{
Thomas J. Leeper, with original package version and C libraries by Micah Altman.

Maintainer: Thomas J. Leeper <thosjleeper@gmail.com>
}
%\references{}
\seealso{
\code{\link{unf}} 
\code{\link{\%unf\%}}
}
\keyword{ package }
