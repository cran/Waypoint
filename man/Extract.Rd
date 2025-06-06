% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/CoordBase.R
\name{Extract}
\alias{Extract}
\alias{[.coords}
\alias{[<-.coords}
\title{Extract or Replace Parts of a Coords Object}
\usage{
\method{[}{coords}(x, i)

\method{[}{coords}(x, i) <- value
}
\arguments{
\item{x}{a \code{"\link{coords}"} object.}

\item{i}{indices specifying elements to extract or replace—see
\code{\link[base:Extract]{Extract}}.}

\item{value}{a \code{numeric}, a \code{numeric} vector of coordinate values of \code{length(i)},
or a \code{"coords"} object, possibly named.}
}
\value{
a \code{"\link{coords}"} object.
}
\description{
Extract or replace subsets of coords.
}
\details{
Subsetting a \code{"\link{coords}"} object (except by an empty index) will drop all attributes
except \code{fmt}, \code{latlon}, \code{names} and \code{valid}. Indices referencing values
greater than \code{length(x)} will throw a \verb{subscript out of bounds} error. If names are not
required, use \code{\link[base:unname]{unname}()}, see \emph{examples}.

Replacement values may be a single \code{numeric}, a \code{numeric} vector of coordinate values
of \code{length(i)}, or a \code{"coords"} object, possibly with a \code{"latlon"} attribute.
However, the \code{"latlon"} attribute of the replacement value is ignored if the \code{"coords"}
object \code{x} has no corresponding attribute set. If replacement values are named, the names
are also ignored; to replace names, use \code{\link[base:names]{names<-}()} replacement form.
}
\note{
To extract and replace subsets of \code{"\link{waypoints}"} objects, simply use the \pkg{base}
package \code{\link[base:Extract]{[}} and \code{\link[base:Extract]{[<-}} operators, taking 
care not to exclude the \code{latitude} and \code{longitude} columns or \code{"Name"} column
(if present), which could lead to undefined results.
}
\examples{
## Continuing example from `as_coords()`...
\dontshow{
   dm <-
       c(5130.4659, 4932.7726, 4806.4339, 3853.3696, 0.0000, -3706.7044, -5306.2869, -2514.4093,
         -007.6754, 1823.9137, -12246.7203, -7702.1145, 0.0000, -1217.3178, 7331.0370, -5731.1536)
   names(dm) <- 
       rep(c("Nelson's Column", "Ostravice", "Tally Ho", "Washington Monument", "Null Island",
             "Tristan da Cunha", "Mawson Peak", "Silvio Pettirossi International Airport"), 2)
   invisible(as_coords(dm, fmt = 2))
   latlon(dm) <- rep(c(TRUE, FALSE), each = 8)
}

## Named "coords" object in degrees and minutes with
## eight values each of latitude and longitude
dm

## Extract the first eight values
dm[1:8]

## Exclude the first eight values
dm[-8:0]

## Index odd-numbered values
(index <- as.logical(1:16 \%\% 2))
dm[index]

## Extract values without names
unname(dm)[1:4]

## Create "coords" object with updated position of Tally Ho
newpos <- as_coords(c(4930.342, -12411.580), fmt = 2)
latlon(newpos) <- c(TRUE, FALSE)
newpos

## Update position using the "coords" object as replacement value
dm[c(3, 11)] <- newpos
dm[c(3, 11)]

## Or, as latlon didn't actually change, use simple numeric vector
dm[c(3, 11)] <- c(4930.342, -12411.580)
dm[c(3, 11)]

rm(dm, index, newpos)

}
\seealso{
\code{"\link{coords}"}, \code{\link[base:Extract]{Extract}},
\code{\link[base:unname]{unname}()}.
}
\concept{extract}
