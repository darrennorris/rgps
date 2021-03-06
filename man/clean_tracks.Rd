% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/clean_tracks.R
\name{clean_tracks}
\alias{clean_tracks}
\title{Clean tracklogs imported via function \code{load_tracks}}
\usage{
clean_tracks(x)
}
\arguments{
\item{x}{Data.frame as returned by function \code{load_tracks} .}
}
\value{
Data.frame with cleaned .gpx tracklog points.
}
\description{
Cleans tracklog points. Calculates a variety of metrics
necessary for summaries of effort (date, time, distance, speed etc).
}
\examples{
\dontrun{
}
}
