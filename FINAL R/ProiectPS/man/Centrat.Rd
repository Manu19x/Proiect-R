% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/2.6 e4.R
\name{Centrat}
\alias{Centrat}
\title{Calculează momentul centrat de ordinul specificat al unei funcții de densitate de probabilitate continue.}
\usage{
Centrat(f, ordin)
}
\arguments{
\item{f}{funcția de densitate de probabilitate}

\item{ordin}{ordinul momentului centrat}
}
\value{
valoarea momentului centrat
}
\description{
Calculează momentul centrat de ordinul specificat al unei funcții de densitate de probabilitate continue.
}
\examples{
Centrat(function(x) { x^2 }, 3)
}
