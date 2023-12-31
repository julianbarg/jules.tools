% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/strip_names.R
\name{strip_names}
\alias{strip_names}
\title{Preprocess Company Names}
\usage{
strip_names(company_name, remove_spaces = T)
}
\arguments{
\item{company_name}{A character string representing the company name to be processed.}

\item{remove_spaces}{A logical value indicating whether to remove all spaces (default is \code{TRUE}).}
}
\value{
A character string with the cleaned company name.
}
\description{
This function preprocesses company names by performing several cleaning operations:
\itemize{
  \item Removing common disconnected suffixes such as "Ltd.", "Inc.", "Corp.", and similar.
  \item Removing punctuation marks.
  \item Eliminating unnecessary white spaces.
  \item Transliterating special characters (e.g., umlauts) to their ASCII equivalents.
  \item Converting all characters to uppercase.
}
}
\examples{
\dontrun{
strip_names("Müller Ltd.")
# Expected output: "Muller"
}

}
