# Variable, global to package's namespace. 
# This function is not exported to user space and does not need to be documented.
SMOLTREGLIMITS <- settings::options_manager(
  min_k = 0.5,
  max_k = 1.5,
  minlength = 100,
  maxlength = 250)



# User function that gets exported:

#' Set or get limits for allowed values for some values
#' 
#' @param ... Option names to retrieve option values or \code{[key]=[value]} pairs to set options.
#'
#' @section Supported options:
#' The following options are supported
#' \itemize{
#'  \item{\code{min_k}}{(\code{numeric};0.5) Minimum Fulton condition }
#'  \item{\code{max_k}}{(\code{numeric};1.5) Maximum Fulton condition }
#'  \item{\code{minlength}}{(\code{numeric};1.5) Minimum length to be considered as smolt }
#'  \item{\code{maxlength}}{(\code{numeric};1.5) Maximum length to be considered as smolt}
#' }
#'
#' @export
Smoltreg_limits <- function(...){
  # protect against the use of reserved words.
  settings::stop_if_reserved(...)
  SMOLTREGLIMITS(...)
}