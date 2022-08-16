#' events
#' 
#' Constants for all allowed numbers in column "HÄNDELSE" in the Smoltreg format
#' 
#' @format Smoltreg::events data.frame
#' \describe{
#'   \item{UNKNOWN}{0, unknown event type}
#'   \item{CAUGHT}{1, the fish was caught but not released to be part of recapture experiment}
#'   \item{MARKED}{2, the fish was marked and released uppstreams, can be recaught}
#'   \item{RECAPTURED}{3, the fish was recaptured}
#'   \item{REMOVED}{4, the fish was removed, i.e. dead in trap, taken as sample...}
#' }
#'
"event"

#' allowed_species
#' 
#' A vector of character containing all species names that are allowed in Sötebasen.
#' 
"allowed_species"