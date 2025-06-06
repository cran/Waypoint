## Waypoint R Package
## Mark Eisler (c) May 2025
## For conversion and validation of geographic coordinates
##
## Requires R version 4.4.2 (2024-10-31) -- "Pile of Leaves" or later
##
## CoordBase.R

## __________________________________________________
#' @title Geographic Coordinate Class
#' 
#' @name coords
#' 
#' @description
#' \code{as_coords()} creates an object of class \code{"coords"}, a robust representation of a
#' series of geographic or GPS coordinate values.
#'
#' \code{latlon()<-} adds information to objects of class \code{"coords"} specifying whether
#' individual coordinate values represent latitude or longitude.
#'
#' @details
#' Individual values provided in a \code{numeric} vector argument \code{object} should have a
#' decimal point after the number of whole degrees in the case of \emph{decimal degrees}, after the
#' number of whole minutes in the case of \emph{degrees and minutes}, and after the number of whole
#' seconds in the case of \emph{degrees, minutes and seconds}.
#'
#' The \code{fmt} argument should be \code{1L} to represent decimal degrees, \code{2L} for degrees
#' and minutes, and \code{3L} for degrees, minutes and seconds and is used to provide the
#' format of values in the \code{numeric} vector argument \code{object} to be converted to class
#' \code{"coords"}.
#'
#' The values of a newly created \code{"coords"} object are checked to ensure they are valid
#' geographic locations as described under \code{\link{validate}()}.
#'
#' Individual coordinate values in a \code{Coords} object may be specified as representing latitude
#' or longitude using \code{latlon()<-}. The \code{value} argument may either be a single value,
#' \code{TRUE} signifying that all values are latitude, \code{FALSE} signifying that all values are
#' longitude, or a \code{logical} vector of the same length as as the \code{Coords} object signifying
#' whether individual values are latitude or longitude.
#'
#' @family coordsandway
#' @seealso
#' \code{\link[base:attr]{attr}()}, \code{\link[base:attributes]{attributes}}, and
#'   \code{\link{validate}()}.
#'
#' @param object a \code{numeric} vector of coordinate values, optionally named, or an object of
#'   class \code{"waypoints"}.
#'
#' @param \dots further arguments passed to or from other methods.
#'
#' @param fmt \code{integer}, \code{1L}, \code{2L} or \code{3L}, specifying the required coordinate
#'   format.
#'
#' @param value a \code{logical} vector of length \code{1} or \code{length(x)}.
#'
#' @param cd object of class \code{"coords"} created by function \code{\link{as_coords}()}.
#'
#' @param which \code{logical}, indicating whether the \code{as_coords()} method for class
#'   \code{"waypoints"} extracts the latitude component of argument \code{object} (if \code{TRUE}),
#'   or the longitude (if \code{FALSE}).
#'
#' @return
#' \code{as_cords()} returns an object of class \code{"coords"}, comprising a \code{numeric} vector
#' argument with additional attributes: –
#'
#' \item{\code{"class"}}{the \code{character} string "coords".}
#'
#' \item{\code{"fmt"}}{an \code{integer} representing the coordinate format.}
#'
#' \item{\code{"valid"}}{a \code{logical} vector indicating whether individual coordinate values
#'   are valid geographic locations.}
#'
#' The \code{as_cords()} method for class \code{"coords"} returns its \code{numeric} vector argument
#' \code{object} modified in place, whereas the method for class 'waypoints' returns a new
#' \code{numeric} vector.
#'
#' \code{latlon()<-} returns its \code{"coords"} argument \code{cd} with a \code{logical} vector
#' attribute \code{"latlon"} added or updated to reflect argument \code{value}.
#'
#' @examples
#' ## Numeric vector representing degrees and minutes, with
#' ## the decimal point after the number of whole minutes
#' dm <- c(5130.4659, 4932.7726, 4806.4339, 3853.3696, 0.0000, -3706.7044, -5306.2869, -2514.4093,
#'         -007.6754, 1823.9137, -12246.7203, -7702.1145, 0.0000, -1217.3178, 7331.0370, -5731.1536)
#'
#' ## Create an unnamed "coords" object in degrees and minutes (fmt = 2)
#' ## (Latitude and longitude unspecified)
#' as_coords(dm, fmt = 2)
#'
#' ## Name the "coords" object
#' names(dm) <- rep(c("Nelson's Column", "Ostravice", "Tally Ho", "Washington Monument", "Null Island",
#'                    "Tristan da Cunha", "Mawson Peak", "Silvio Pettirossi International Airport"), 2)
#' dm
#'
#' ## Set all values to represent longitude
#' ## ("latlon" attribute set to FALSE, length 1)
#' latlon(dm) <- FALSE
#' dm
#'
#' ## Set eight values each of latitude and longitude
#' ## ("latlon" attribute set to TRUE, n=8, and FALSE, n=8)
#' latlon(dm) <- rep(c(TRUE, FALSE), each = 8)
#' dm
#'
#' ## Show as an ordinary R numeric vector
#' as.numeric(dm)
#'
#' rm(dm)
#'

## ========================================
##  S3generic as_coords(object, ...)
#'
#' @export

as_coords <- function(object, ...) 
    UseMethod("as_coords")


## __________________________________________________
#' @title Geographic Waypoint Class
#' 
#' @name waypoints
#' 
#' @description
#' \code{as_waypoints()} creates an object of class \code{"waypoints"}, a robust representation of a
#' series of geographic or GPS waypoints of paired latitude and longitude values.
#'
#' @details
#' Data frame argument \code{object} should have \code{numeric} vector latitude and longitude
#' columns with individual values having a decimal point after the number of whole degrees in the
#' case of \emph{decimal degrees}, after the number of whole minutes in the case of
#' \emph{degrees and minutes}, and after the number of whole seconds in the case of
#' \emph{degrees, minutes and seconds}. These should be the first two columns of the data frame, or
#' the second and third columns if the first column contains waypoints names (see below).
#' Alternative columns may be specified for the latitude and longitude by setting \code{"llcols"} as
#' a length 2 \code{integer} vector attribute of \code{object} indicating their positions in the
#' data frame.
#'
#' The \code{fmt} argument should be \code{1L} to represent decimal degrees, \code{2L} for degrees
#' and minutes, and \code{3L} for degrees, minutes and seconds and is used to provide the
#' format of values in data frame argument \code{object} to be converted to class
#' \code{"waypoints"}.
#'
#' If the waypoints have names, these should be included in a "Name" column of data frame argument
#' \code{object}, by default immediately before (on the left-hand side of) the latitude and
#' longitude columns. An alternative column for waypoint names may be specified by setting an
#' \code{integer} \code{\link[base:attributes]{attribute}} named "namescol" indicating its position
#' in \code{object}. If neither a "Name" column nor a \code{"namescol"} attribute is present in
#' \code{object}, \code{\link[base:row.names]{row.names}} are used for waypoint names.
#'
#' The latitude and longitude values of a newly created \code{"waypoints"} object are checked to
#' ensure they are valid geographic locations as described under \code{\link{validate}()}.
#'
#' @family coordsandway
#' @seealso
#' \code{\link[base:attr]{attr}()}, \code{\link[base:data.frame]{data.frame}()}, and
#'   \code{\link{validate}()}.
#'
#' @param object a data frame with each row representing a waypoint, comprising at least two
#'   \code{numeric} columns containing values of latitude and longitude, and optionally a
#'   \code{character} column of waypoint names (see \emph{Details}). 
#'
#' @inheritParams coords
#'
#' @return
#' An object of classes \code{"waypoints"} and \code{"data.frame"}, comprising the original data
#' frame argument \code{object}, with additional attributes: –
#'
#' \item{\code{"class"}}{the \code{character} string "waypoints".}
#'
#' \item{\code{"fmt"}}{an \code{integer} indicating the coordinate format.}
#'
#' \item{\code{"namescol"}}{an \code{integer} indicating the position of a waypoint names column,
#'   if present.}
#'
#' \item{\code{"llcols"}}{a length 2 \code{integer} vector indicating the positions of latitude and
#'   longitude columns.}
#'
#' \item{\code{"validlat"} and \code{"validlon"}}{\code{logical} vectors indicating whether
#'   individual latitude and longitude values are valid geographic locations.}
#'
#' @examples
#' ## Dataframe representing waypoint names, and latitude and longitude values
#' ## in degrees, minutes and seconds
#' wp1 <- data.frame(
#'     name = c("Nelson's Column", "Ostravice", "Tally Ho", "Washington Monument", "Null Island",
#'              "Tristan da Cunha", "Mawson Peak", "Silvio Pettirossi International Airport"),
#'     lat = c(513027.95, 493246.36, 480626.04, 385322.18, 0, -370642.26, -530617.21, -251424.56),
#'     lon = c(-00740.53, 182354.82, -1224643.22, -770206.87, 0, -121719.07, 733102.22, -573109.21)
#' )
#'
#' ## Create "waypoints" object in degrees, minutes and seconds (fmt = 3)
#' as_waypoints(wp1, fmt = 3)
#'
#' ## Show as an ordinary R data frame
#' as.data.frame(wp1)
#'
#' ###
#'
#' ## Dataframe representing unnamed latitude and longitude
#' ## values in decimal degrees
#' wp2 <- data.frame(
#'     lat = c(51.507765, 49.54621, 48.107232, 38.889494, 0, -37.11174, -53.104781, -25.240156),
#'     lon = c(-0.127924, 18.398562, -122.778671, -77.035242, 0, -12.28863, 73.517283, -57.519227)
#' )
#'
#' ## Create unnamed "waypoints" object in decimal degrees (default fmt = 1)
#' as_waypoints(wp2)
#'
#' ## Add waypoint names as row.names
#' row.names(wp2) <-
#'     c("Nelson's Column", "Ostravice", "Tally Ho", "Washington Monument", "Null Island",
#'       "Tristan da Cunha", "Mawson Peak", "Silvio Pettirossi International Airport")
#'
#' wp2
#'
#' ## Show as an ordinary R data frame
#' as.data.frame(wp2)
#'
#' rm(wp1, wp2)
#'

## ========================================
##  S3generic as_waypoints(object, ...)
#'
#' @export

as_waypoints <- function(object, ...) 
    UseMethod("as_waypoints")


## __________________________________________________
#' @title Convert the Format of "coords" and "waypoints" Objects
#' 
#' @name convert
#' 
#' @description
#' Convert the format of objects of class \code{"coords"} or \code{"waypoints"} between (i)
#' decimal degrees, (ii) degrees and minutes, and (iii) degrees, minutes and seconds.
#'
#' @details
#' The \code{fmt} argument should be \code{1L} to convert to  \emph{decimal degrees}, \code{2L}, to
#' convert to \emph{degrees and minutes}, and \code{3L} to convert to \emph{degrees, minutes and
#' seconds}. On conversion of a \code{"coords"} object, the original argument \code{x} is modified
#' to have a decimal point after the number of whole degrees in the case of decimal degrees, after
#' the number of whole minutes in the case of degrees and minutes, and after the number of whole
#' seconds in the case of degrees, minutes and seconds.
#'
#' Prior to conversion, the \code{"coords"} or \code{"waypoints"} object to be converted is checked
#' to ensure its values represent valid geographic locations as described under
#' \code{\link{validate}()}.
#'
#' @note \code{convert()} modifies its argument \code{x} in place. To format or print
#'   \code{"coords"} or \code{"waypoints"} in another coordinate format without modifying the
#'   original object, use \code{\link{format}()} or \code{\link[Waypoint:format]{print}()}.
#'
#' @family coordsandway
#' @seealso
#' \code{"\link{coords}"}, \code{"\link{waypoints}"} and \code{\link{validate}()}.
#'
#' @param x object of class \code{"\link{coords}"} created by function \code{\link{as_coords}()}, or
#'   class \code{"\link{waypoints}"} created by function \code{\link{as_waypoints}()}.
#'
#' @inheritParams coords
#'
#' @return
#' The original argument \code{x}, an object of class \code{"coords"} or \code{"waypoints"} with
#' values converted as described under \emph{details} and a revised \code{"fmt"} attribute
#' reflecting the new format.
#'
#' @keywords programming
#'
#' @examples
#' ## Continuing example from `as_coords()`...
#' \dontshow{
#'    dm <-
#'        c(5130.4659, 4932.7726, 4806.4339, 3853.3696, 0.0000, -3706.7044, -5306.2869, -2514.4093,
#'          -007.6754, 1823.9137, -12246.7203, -7702.1145, 0.0000, -1217.3178, 7331.0370, -5731.1536)
#'    names(dm) <- 
#'        rep(c("Nelson's Column", "Ostravice", "Tally Ho", "Washington Monument", "Null Island",
#'              "Tristan da Cunha", "Mawson Peak", "Silvio Pettirossi International Airport"), 2)
#'    invisible(as_coords(dm, fmt = 2))
#'    latlon(dm) <- rep(c(TRUE, FALSE), each = 8)
#' }
#'
#' ## Named "coords" object in degrees and minutes with
#' ## eight values each of latitude and longitude
#' dm
#'
#' ## Convert to degrees, minutes and seconds (fmt = 3)
#' convert(dm, 3)
#'
#' ## Convert to decimal degrees (fmt = 1)
#' convert(dm, 1)
#'
#' ## Show converted values as an ordinary R numeric vector
#' as.numeric(dm)
#'
#' ###
#' ## Continuing example from `as_waypoints()`...
#' \dontshow{
#'    wp <- data.frame(
#'        name = c("Nelson's Column", "Ostravice", "Tally Ho", "Washington Monument", "Null Island",
#'                 "Tristan da Cunha", "Mawson Peak", "Silvio Pettirossi International Airport"),
#'        lat = c(513027.95, 493246.36, 480626.04, 385322.18, 0, -370642.26, -530617.21, -251424.56),
#'        lon = c(-00740.53, 182354.82, -1224643.22, -770206.87, 0, -121719.07, 733102.22, -573109.21)
#'    )
#'    invisible(as_waypoints(wp, fmt = 3))
#' }
#' 
#' ## "waypoints" object in degrees, minutes and seconds
#' wp
#'
#' ## Convert to degrees and minutes (fmt = 2)
#' convert(wp, 2)
#'
#' ## Convert to decimal degrees (fmt = 1)
#' convert(wp, 1)
#'
#' ## Show converted values as an ordinary R data frame
#' as.data.frame(wp)
#'
#' rm(dm, wp)
#'

## ========================================
##  S3generic convert(x, ...)
#'
#' @rdname convert
#' @export

convert <- function(x, ...) 
    UseMethod("convert")


## __________________________________________________
#' @title Format and Print Coords or Waypoints
#' 
#' @name format
#' 
#' @description
#' Format and print objects of class \code{"coords"} or \code{"waypoints"}.
#'
#' @details
#' The \code{format()} methods for \code{"\link{coords}"} and \code{"\link{waypoints}"} objects
#' output elegantly formatted \code{character} vector representations of their arguments, which are
#' used by their respective \code{print()} methods.
#'
#' Objects of class \code{"coords"} specified in \emph{degrees and minutes} or in \emph{degrees,
#' minutes and seconds} and with a \code{"latlon"} attribute, and similarly specified
#' \code{"waypoints"} objects are formatted with individual coordinate values followed by a capital
#' letter representing the \emph{cardinal direction} i.e., \samp{N}, \samp{E}, \samp{S} or \samp{W}.
#' \code{"coords"} objects lacking a \code{"latlon"} attribute have formatted values followed by two
#' possible cardinal directions in parentheses i.e., \samp{(N/E)} for positive values and
#' \samp{(S/W)} for negative values. Values of \code{"coords"} or \code{"waypoints"} objects in
#' \emph{decimal degrees} are formatted prefixed with their sign, if negative; cardinal direction is
#' not shown, but for \code{"coords"} objects with a \code{"latlon"} attribute, the formatted values
#' are suffixed by either \samp{lat} or \samp{lon}.
#'
#' Prior to formatting and printing, \code{"coords"} or \code{"waypoints"} objects are checked to
#' ensure that their \code{"valid"} attribute (in the case of a \code{"coords"} object), or
#' \code{"validlat"} and \code{"validlon"} attributes (in the case of a \code{"waypoints"} object)
#' are present and all \code{TRUE} i.e., valid. If these attributes are found to contain any
#' \code{FALSE} i.e. invalid values, a warning is issued and similarly, if these attributes are
#' missing, a warning is issued and the objects are re-validated as described under
#' \code{\link{validate}()}.
#'
#' The optional argument \code{fmt} may be used to specify the coordinate format desired for
#' formatting or printing \code{"coords"} or \code{"waypoints"} objects, see the \code{fmt} argument
#' for \code{\link[=coords]{as_coords}()} and \code{\link[=waypoints]{as_waypoints}()}; using the
#' default, \code{fmt = 0L}, will format or print in the existing coordinate format.
#'
#' \code{ll_headers()} outputs the headings \verb{"Latitude ... Longitude"} formatted to the width
#' of argument \code{width}, adjusted for format \code{fmt} and is primarily intended for use by the
#' \code{print()} method for class \code{"waypoints"}. Likewise argument \code{validate} is used by
#' the \code{print()} methods for classes \code{"coords"} and \code{"waypoints"} to prevent
#' unnecessary replicate validation and may otherwise be left as the default.
#'
#' @seealso
#' \code{\link[base:format]{format}()}, \code{\link[base:print]{print}()},
#' \code{"\link{coords}"} and \code{"\link{waypoints}"}.
#'
#' @param usenames \code{logical}, whether or not to include names in formatted output; default
#' \code{TRUE}.
#'
#' @param validate \code{logical}, whether or not to \code{\link{validate}} \code{x} before
#' formatting; default \code{TRUE}.
#'
#' @param width \code{character} vector, used to match width of headers to formatted output.
#'
#' @inheritParams coords
#' @inheritParams convert
#' @inheritParams base::print.data.frame
#'
#' @return
#' The \code{format()} methods for both classes \code{"coords"} and \code{"waypoints"} return a
#' \code{character} vector, respectively of length \code{length(x)} or \code{nrow(x)}, and
#' containing values formatted in decimal degrees, degrees and minutes, or degrees, minutes and
#' seconds as appropriate.
#'
#' @keywords character print
#'
#' @examples
#' ## Continuing example from `as_coords()`...
#' \dontshow{
#'    dm <-
#'        c(5130.4659, 4932.7726, 4806.4339, 3853.3696, 0.0000, -3706.7044, -5306.2869, -2514.4093,
#'          -007.6754, 1823.9137, -12246.7203, -7702.1145, 0.0000, -1217.3178, 7331.0370, -5731.1536)
#'    names(dm) <- 
#'        rep(c("Nelson's Column", "Ostravice", "Tally Ho", "Washington Monument", "Null Island",
#'              "Tristan da Cunha", "Mawson Peak", "Silvio Pettirossi International Airport"), 2)
#'    invisible(as_coords(dm, fmt = 2))
#'    latlon(dm) <- rep(c(TRUE, FALSE), each = 8)
#' }
#'
#' ## Print named "coords" object in degrees and minutes,
#' ## implicitly using S3 print() method
#' dm
#'
#' ## Print explicitly using S3 print() method, specifying
#' ## the maximal number of entries to be printed
#' print(dm, max = 14)
#'
#' ## Format as a fixed-width character vector,
#' ## with names...
#' format(dm)
#'
#' ## ...or without them
#' format(dm, usenames = FALSE)
#'
#' ## Format as decimal degrees,
#' format(dm, fmt = 1)
#'
#' ###
#' ## Continuing example from `as_waypoints()`...
#' \dontshow{
#'    wp <- data.frame(
#'        name = c("Nelson's Column", "Ostravice", "Tally Ho", "Washington Monument", "Null Island",
#'                 "Tristan da Cunha", "Mawson Peak", "Silvio Pettirossi International Airport"),
#'        lat = c(513027.95, 493246.36, 480626.04, 385322.18, 0, -370642.26, -530617.21, -251424.56),
#'        lon = c(-00740.53, 182354.82, -1224643.22, -770206.87, 0, -121719.07, 733102.22, -573109.21)
#'    )
#'    invisible(as_waypoints(wp, fmt = 3))
#' }
#'
#' ## Print named "waypoints" object in degrees, minutes and seconds
#' ## implicitly using S3 print() method
#' wp
#'
#' ## Print explicitly using S3 print() method, specifying
#' ## the maximal number of entries to be printed
#' print(wp, max = 21)
#'
#' ## Print as degrees and minutes
#' print(wp, fmt = 2)
#'
#' ## Format as a fixed-width character vector,
#' ## with names...
#' format(wp)
#'
#' ## ...or without them
#' format(wp, usenames = FALSE)
#'
#' rm(dm, wp)
#'

## ========================================
##  S3method print.coords(x, ...)
#'
#' @rdname format
#' @export

print.coords <- function (x, ..., max = NULL) {
    n <- length(x)
    validate(x, force = FALSE)
    max <- max %||% getOption("max.print", 99999L)
    if (!is.finite(max)) 
        stop("invalid 'max' / getOption(\"max.print\"): ", max)
    omit <- (n0 <- max %/% (if (is.null(names(x))) 1L else 2L)) < n
    if (omit)
        x0 <- x[seq_len(n0)]
    else
        x0 <- x
    writeLines(format(x0, validate = FALSE, ...))
    if (omit) 
        cat(" [ reached 'max' / getOption(\"max.print\") -- omitted", n - n0, "entries ]\n")
    invisible(x)
}


## ========================================
##  S3method print.waypoints(x, ..., fmt = NULL, max = NULL)
#'
#' @rdname format
#' @export

print.waypoints <- function (x, ..., fmt = NULL, max = NULL) {
    n <- length(row.names(x))
    validate(x, force = FALSE)
    max <- max %||% getOption("max.print", 99999L)
    if (!is.finite(max)) 
        stop("invalid 'max' / getOption(\"max.print\"): ", max)
    omit <- (n0 <- max %/% 3L) < n
    if (omit) 
         x <- x[seq_len(n0), , drop = FALSE]
    fmt <- fmt %||% attr(x, "fmt")
    if (0 == fmt) fmt <- attr(x, "fmt")
    fmtx <- format(x, validate = FALSE, fmt = fmt, ...)
    writeLines(ll_headers((nchar(fmtx[1])), fmt))
    writeLines(fmtx)
    if (omit) 
        cat(" [ reached 'max' / getOption(\"max.print\") -- omitted", n - n0, "rows ]\n")
    invisible(x)
}


## __________________________________________________
#' @encoding UTF-8
#' @title Validate Coords or Waypoints
#' 
#' @name validate
#' 
#' @description
#' Validate objects of class \code{"coords"} or \code{"waypoints"} as geographic locations.
#'
#' @details
#' Individual coordinate values within \code{"\link{coords}"} or \code{"\link{waypoints}"} objects
#' are checked to ensure they represent valid geographic locations.
#'
#' To be valid, the absolute values of coordinates in degrees must not exceed 180°, or 90° if
#' degrees of latitude and, similarly, the absolute values of the minutes and seconds components,
#' where given, must not exceed 60. Otherwise, a warning will be issued and the \code{"valid"}
#' attribute in the case of a \code{"coords"} object, or \code{"validlat"} and \code{"validlon"}
#' attributes in the case of a \code{"waypoints"} object will be set to \code{FALSE} for any
#' non-compliant coordinate values.
#'
#' Argument \code{force} is primarily intended for use by the \code{print()} methods for classes
#'   \code{"coords"} and \code{"waypoints"} and should otherwise left as the default value
#'   \code{TRUE}.
#'
#' @family validate
#' @seealso
#' \code{"\link{coords}"} and \code{"\link{waypoints}"}.
#'
#' @param force \code{logical} signifying whether, if \code{TRUE}, to perform full \emph{de novo}
#'   revalidation or, if \code{FALSE}, simply check existing \code{"valid"} attribute in the case
#'   of a \code{"coords"} object, or \code{"validlat"} and \code{"validlon"} attributes in the case
#'   of a \code{"waypoints"} object and only revalidate if any of these are missing; default
#'   \code{TRUE}.
#'
#' @inheritParams coords
#' @inheritParams convert
#'
#' @return
#' \code{validate()} returns its argument with \code{logical} vector attribute \code{"valid"},
#' or attributes \code{"validlat"} and \code{"validlon"} updated as appropriate for
#' \code{"coords"} and' \code{"waypoints"} objects respectively.
#'
#' @examples
#' ## Continuing example from `as_coords()`...
#' \dontshow{
#'    dm <-
#'        c(5130.4659, 4932.7726, 4806.4339, 3853.3696, 0.0000, -3706.7044, -5306.2869, -2514.4093,
#'          -007.6754, 1823.9137, -12246.7203, -7702.1145, 0.0000, -1217.3178, 7331.0370, -5731.1536)
#'    names(dm) <- 
#'        rep(c("Nelson's Column", "Ostravice", "Tally Ho", "Washington Monument", "Null Island",
#'              "Tristan da Cunha", "Mawson Peak", "Silvio Pettirossi International Airport"), 2)
#'    invisible(as_coords(dm, fmt = 2))
#'    latlon(dm) <- rep(c(TRUE, FALSE), each = 8)
#' }
#'
#' ## Validate "coords" object in degrees and minutes
#' validate(dm)
#'
#' ## Deliberately change the first coordinate
#' ## to a value greater than 60 minutes
#' dm[1] <- 5160.4659
#'
#' validate(dm)
#'
#' ## Examine "valid" attribute of dm
#' attr(dm, "valid")
#'
#' ###
#' ## Continuing second example from `as_waypoints()`...
#' \dontshow{
#'    wp <- data.frame(
#'        lat = c(51.507765, 49.54621, 48.107232, 38.889494, 0, -37.11174, -53.104781, -25.240156),
#'        lon = c(-0.127924, 18.398562, -122.778671, -77.035242, 0, -12.28863, 73.517283, -57.519227)
#'    )
#'    row.names(wp) <-
#'        c("Nelson's Column", "Ostravice", "Tally Ho", "Washington Monument", "Null Island",
#'          "Tristan da Cunha", "Mawson Peak", "Silvio Pettirossi International Airport")
#'    invisible(as_waypoints(wp))
#' }
#'
#' ## Validate "waypoints" object in decimal degrees
#'
#' validate(wp)
#'
#' ## Deliberately change the penultimate latitude
#' ## to an absolute value greater than 90 degrees
#' wp$lat[7] <- -93.104781
#'
#' validate(wp)
#'
#' ## Examine "validlat" attribute of wp
#' attr(wp, "validlat")
#'
#' rm(dm, wp)
#'

## ========================================
##  Validate Coordinates and Waypoints
##  S3generic validate(x, ...)
##
#' @rdname validate
#' @export

validate <- function(x, ...) 
    UseMethod("validate")

  
## __________________________________________________
#' @title
#' Review Coordinates and Waypoints Validity
#'
#' @description
#' Review validity of elements of \code{"coords"} and \code{"waypoints"} objects.
#'
#' @details
#' \code{review()} reveals elements of \code{"coords"} and  \code{"waypoints"} objects that do not
#' conform to the criteria checked by \code{\link{validate}()}, i.e. are not valid geographic
#' locations.
#'
#' @family validate
#' @seealso
#' \code{"\link{coords}"} and \code{"\link{waypoints}"}.
#'
#' @param show_n \code{integer}, the maximum number of invalid elements of argument \code{x} to
#' include in the output; default \code{20L}.
#'
#' @inheritParams coords
#' @inheritParams convert
#'
#' @return
#' The \code{review()} method for class \code{"coords"} returns a \code{\link[base:list]{list}}
#' comprising the following elements: -
#'
#' \item{allvalid}{\code{logical}, whether or not all the elements of argument \code{x} are valid.}
#'
#' \item{n_invalid}{\code{integer}, the number of invalid elements in argument \code{x}, if any.}
#'
#' \item{invalids}{\code{numeric} vector including invalid elements of argument \code{x}, if any.}
#'
#' \item{which_invalid}{\code{integer} vector specifying which elements of argument \code{x} are
#'   invalid, if any.}
#'
#' The method for class \code{"waypoints"} returns a list of two sub-lists, each sub-list with
#' elements as described above for the method for class \code{"coords"}, one each for latitude and
#' longitude.
#'
#' @export
#'
#' @examples
#' ## Continuing example from `validate()`...
#' \dontshow{suppressWarnings(dm <- (\(){
#'     tmp <- as_coords(c(5160.4659, 4932.7726, 4806.4339, 3853.3696, 0.0000, -3706.7044, -5306.2869, -2514.4093,
#'         -007.6754, 1823.9137, -12246.7203, -7702.1145, 0.0000, -1217.3178, 7331.0370, -5731.1536), fmt = 2)
#'     names(tmp) <- rep(c("Nelson's Column", "Ostravice", "Tally Ho", "Washington Monument", "Null Island",
#'         "Tristan da Cunha", "Mawson Peak", "Silvio Pettirossi International Airport"), 2)
#'     latlon(tmp) <- rep(c(TRUE, FALSE), each = 8)
#'     tmp
#' })())}
#'
#' ## Review "coords" object in degrees and minutes, having
#' ## an erroneous first value of more than 60 minutes
#' review(dm)
#'
#' ###
#' ## Continuing example from `validate()`...
#' \dontshow{suppressWarnings(
#'    wp <- as_waypoints(data.frame(
#'        name = c("Nelson's Column", "Ostravice", "Tally Ho", "Washington Monument", "Null Island",
#'                 "Tristan da Cunha", "Mawson Peak", "Silvio Pettirossi International Airport"),
#'        lat = c(51.507765, 49.54621, 48.107232, 38.889494, 0, -37.11174, -93.104781, -25.240156),
#'        lon = c(-0.127924, 18.398562, -122.778671, -77.035242, 0, -12.28863, 73.517283, -57.519227)
#'    ), fmt = 1)
#' )}
#'
#' ## Review "waypoints" object in  decimal degrees, having an erroneous
#' ## penultimate latitude absolute value greater than 90 degrees
#' review(wp)
#'
#' rm(dm, wp)
#'

## ========================================
##  S3generic review(x, ...)
##
review <- function(x, ...) 
    UseMethod("review")


## ========================================
##  S3method review.coords(x, ..., show_n = 20L)
#'
#' @rdname review
#' @export

review.coords <- function(x, ..., show_n = 20L)
{
    if (!inherits(x, "coords"))
        stop("Argument `coords` must have class `\"coords\"`\n", call. = FALSE)
    invalid <- !attr(x, "valid")
    n_invalid <- sum(invalid, na.rm = TRUE)
    last_invalid <- which(invalid)[show_n]
    if (n_invalid > show_n) {
        warning(
            n_invalid, " invalid coords, showing first ", show_n,
            "\n\t(use arg `show_n` to see more) ",
            call.= FALSE
        )
        x <- x[seq_len(last_invalid)]
        return(review(x, show_n = show_n))
    }
    fmt <- attr(x, "fmt");
    if (n_invalid)
        invalids <- x[invalid]
    else
        invalids <- NA_integer_
    list(
        allvalid = all(attr(x, "valid")),
        n_invalid = n_invalid,
        invalids = invalids,
        which_invalid = which(invalid) %L% NA_integer_
    )
}


## ========================================
##  S3method review.waypoints(x, ..., show_n = 20L)
#'
#' @rdname review
#' @export

review.waypoints <- function(x, ..., show_n = 20L)
    list(
        Lat = review(as_coords(x, TRUE), show_n = show_n),
        Lon = review(as_coords(x, FALSE), show_n = show_n)
    )


## __________________________________________________
#' @title
#' Extract or Replace Parts of a Coords Object
#'
#' @name Extract
#'
#' @description
#' Extract or replace subsets of coords.
#'
#' @details
#' Subsetting a \code{"\link{coords}"} object (except by an empty index) will drop all attributes
#' except \code{fmt}, \code{latlon}, \code{names} and \code{valid}. Indices referencing values
#' greater than \code{length(x)} will throw a \verb{subscript out of bounds} error. If names are not
#' required, use \code{\link[base:unname]{unname}()}, see \emph{examples}.
#'
#' Replacement values may be a single \code{numeric}, a \code{numeric} vector of coordinate values
#' of \code{length(i)}, or a \code{"coords"} object, possibly with a \code{"latlon"} attribute.
#' However, the \code{"latlon"} attribute of the replacement value is ignored if the \code{"coords"}
#' object \code{x} has no corresponding attribute set. If replacement values are named, the names
#' are also ignored; to replace names, use \code{\link[base:names]{names<-}()} replacement form.
#'
#' @note
#' To extract and replace subsets of \code{"\link{waypoints}"} objects, simply use the \pkg{base}
#' package \code{\link[base:Extract]{[}} and \code{\link[base:Extract]{[<-}} operators, taking 
#' care not to exclude the \code{latitude} and \code{longitude} columns or \code{"Name"} column
#' (if present), which could lead to undefined results.
#'
#' @family extract
#' @seealso \code{"\link{coords}"}, \code{\link[base:Extract]{Extract}},
#' \code{\link[base:unname]{unname}()}.
#'
#' @param x a \code{"\link{coords}"} object.
#'
#' @param i indices specifying elements to extract or replace—see
#'   \code{\link[base:Extract]{Extract}}.
#'
#' @param value a \code{numeric}, a \code{numeric} vector of coordinate values of \code{length(i)},
#'   or a \code{"coords"} object, possibly named.
#'
#' @return a \code{"\link{coords}"} object.
#'
#' @examples
#' ## Continuing example from `as_coords()`...
#' \dontshow{
#'    dm <-
#'        c(5130.4659, 4932.7726, 4806.4339, 3853.3696, 0.0000, -3706.7044, -5306.2869, -2514.4093,
#'          -007.6754, 1823.9137, -12246.7203, -7702.1145, 0.0000, -1217.3178, 7331.0370, -5731.1536)
#'    names(dm) <- 
#'        rep(c("Nelson's Column", "Ostravice", "Tally Ho", "Washington Monument", "Null Island",
#'              "Tristan da Cunha", "Mawson Peak", "Silvio Pettirossi International Airport"), 2)
#'    invisible(as_coords(dm, fmt = 2))
#'    latlon(dm) <- rep(c(TRUE, FALSE), each = 8)
#' }
#'
#' ## Named "coords" object in degrees and minutes with
#' ## eight values each of latitude and longitude
#' dm
#'
#' ## Extract the first eight values
#' dm[1:8]
#'
#' ## Exclude the first eight values
#' dm[-8:0]
#'
#' ## Index odd-numbered values
#' (index <- as.logical(1:16 %% 2))
#' dm[index]
#'
#' ## Extract values without names
#' unname(dm)[1:4]
#'
#' ## Create "coords" object with updated position of Tally Ho
#' newpos <- as_coords(c(4930.342, -12411.580), fmt = 2)
#' latlon(newpos) <- c(TRUE, FALSE)
#' newpos
#'
#' ## Update position using the "coords" object as replacement value
#' dm[c(3, 11)] <- newpos
#' dm[c(3, 11)]
#'
#' ## Or, as latlon didn't actually change, use simple numeric vector
#' dm[c(3, 11)] <- c(4930.342, -12411.580)
#' dm[c(3, 11)]
#'
#' rm(dm, index, newpos)

## ========================================
##  S3method `[.coords`(x, i)
#'
#' @export

`[.coords` <- function(x, i) 
{
    if (missing(i)) return(x)
    if (max(i) > length(x)) stop(gettext("subscript out of bounds"), domain = NA, call. = FALSE)
    attrx <- attributes(x)
    y <- NextMethod("[")
    attributes(y) <- lapply(attrx[names(attrx) %in% c("fmt", "latlon", "names", "valid")], \(ax) if(length(ax) == length(x)) ax[i] else ax)
    class(y) <- oldClass(x)
    y
}


## ========================================
##  S3method `[.coords<-`(x, i, value)
#'
#' @rdname Extract
#' @export

`[<-.coords` <- function(x, i, value) 
{
    if (max(i) > length(x)) stop(gettext("subscript out of bounds"), domain = NA, call. = FALSE)
    vfmt <- attr(value, "fmt", exact = TRUE)
    if (!is.null(vfmt) && attr(x, "fmt", exact = TRUE) != vfmt) stop("value has different fmt attribute\n", call. = FALSE)
    y <- NextMethod("[<-")
    vll <- attr(value, "latlon", exact = TRUE)
    if (!is.null(vll)) {
        if (length(vll) != length(value) && length(vll) != 1)
            stop("\"latlon\" attribute of replacement value must be either length 1 or length(value)\n", call. = FALSE)
        xll <- attr(x, "latlon", exact = TRUE)
        if (length(xll) == length(x))
            attr(y, "latlon")[i] <- vll
        else if (length(xll) == 1 && !all(vll == xll)) {
            attr(y, "latlon") <- rep(xll, length(x))
            attr(y, "latlon")[i] <- vll
        }
    }
    class(y) <- oldClass(x)
    validate(y)
}


## __________________________________________________
#' @title
#' Operator Providing Alternative to Zero-Length Object
#'
#' @name op-zero-length
#'
#' @description
#' Infix function implementing provision of an alternative if an object has zero length.
#'
#' @details
#' The infix function \code{\%L\%} may be useful in implementing \code{if (length(x)) x else y} and
#' was inspired by the null coalescing operator \code{\link[base:Control]{\%||\%}}.
#'
#' @family utils
#' @seealso \code{\link[base:Control]{\%||\%}}.
#'
#' @param x,y atomic vector arguments or other objects for which \code{length()} is defined.
#'
#' @return \code{x}, or if \code{length(x)} is zero, \code{y}.
#'
#' @keywords logic programming
#' @export
#' @examples
#' c4 <- letters[1:4]
#' c0 <- character(0)
#' n3 <- 1:3
#' n0 <- numeric(0)
#' 
#' c4 %L% n3
#' c0 %L% n3
#' 
#' n3 %L% c4
#' n0 %L% c4
#'
#' rm(c4, c0, n3, n0)

## ========================================
##  op-zero-length

`%L%` <- function (x, y) 
if (length(x)) x else y

