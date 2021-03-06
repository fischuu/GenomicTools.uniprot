% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/uniparc2uniprot.R
\name{uniparc2uniprot}
\alias{uniparc2uniprot}
\title{Get UniprotKB identifier from Uniparc accession number}
\usage{
uniparc2uniprot(x, dryrun = FALSE, verbose = TRUE)
}
\arguments{
\item{x}{uniparc identifier}

\item{dryrun}{logical, do not execute the function}

\item{verbose}{logical for function feedback}
}
\value{
A UniprotKB identifier
}
\description{
Get UniprotKB identifier from Uniparc accession number
}
\examples{
uniprotID2Sequence("EKY21295", dryrun=TRUE)
}
