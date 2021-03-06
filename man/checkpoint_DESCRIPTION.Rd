% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/checkpoint_Description.r
\name{checkpoint_DESCRIPTION}
\alias{checkpoint_DESCRIPTION}
\title{Set a Checkpoint Based on the DESCRIPTION File of a Package}
\usage{
checkpoint_DESCRIPTION(.filename = "./DESCRIPTION", .date = NULL,
  .install.suggestions = TRUE)
}
\arguments{
\item{.filename}{Path of the DESCRIPTION file that should be used.}

\item{.date}{The checkpoint date. If empty the "Date:" field from the
DESCRIPTION file is used.}

\item{.install.suggestions}{If FALSE packages from the "Suggests:" field
are not installed.}
}
\description{
This function sets a checkpoint in the sense of the function \code{checkpoint()}
from the checkpoint package based on a DESCRIPTION file.All packages listed
in Imports:/Suggests:/Depends: are installed. Packages installed from github
can be listed in "Additional_repositories:"
}
