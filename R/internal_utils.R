# Misc functions ----------------------------------------------------------------
#' Fulton condition factor
#'
#' Return Fulton's condition factor 
#' @param w Weight in grams
#' @param l Length in mm
#'
#' @return
#' A numeric
#'
fulton <- function(w, l){
  l <- l / 10 # Convert length to cm
  k <- 100 * (w / l^3)
  return(k)
}

#' Hex to dec pittag string
#' 
#' Convert vector of pittags in hex to dec
#' @param tags character vector of tags in hex format
#' 
#' @return character vector of tags in decimal format
#' 
hex2dec <- function(tags) {
  parts <- strsplit(tags, "\\.")
  res <- sapply(parts, function(x) {
    x[is.na(x)] <- 0
    if (is.na(x[2])) {
      x[2] <- 0
    }
    p1 <- strtoi(paste("0x", x[1], sep=""))
    p2 <- strtoi(paste("0x", x[2], sep=""))
    sprintf("%03d.%012d", p1, p2)
    #    paste(p1, p2, sep='.')
  })
  res <- ifelse(res == "000.000000000000", NA, res)
  return(res)
}

#' Decimal to hex pittag string
#' 
#' Convert vector of pittags in dec to hex
#' @param tags character vector of tags in hex format
#' 
#' @return character vector of tags in decimal format
#' 
dec2hex <- function(tags) {
  if (grepl(tags[1], pattern = "\\.")) { # We assume that all tags are formated either with or without "."
    tags <- ifelse(is.na(tags), "000.000000000000", tags)
    parts <- strsplit(tags, "\\.") }
  else {
    tags <- ifelse(is.na(tags), "000000000000000", tags)
    parts <- lapply(1:length(tags), function(x) {
      return(c(substr(tags[x], start = 1, stop = 3),
               substr(tags[x], start = 4, stop = nchar(tags[x]))))})
  }
  res <- sapply(parts, function(x) {
    p1 <- sprintf("%03X", as.numeric(x[1]))
    p2 <- sprintf("%010X", as.numeric(x[2])) # Pad with space may be platform dependent
    paste(p1, p2, sep='.')
  })
  res <- ifelse(res == "000.0000000000", NA, res)
  return(res)
}

##
#' remove_empty_row
#' 
#' Remove rows from a data.frame when all fields in a row are NA
#'
#' @param df 
#'
#' @return
#' a data.frame
#'
remove_empty_rows <- function(df) {
  all.NA.idx <- which(apply(df,1,function(x)all(is.na(x))))
  
  if (length(all.NA.idx) > 0) {
    df <- df[-all.NA.idx,] # Rem rows with all cols == NA
  }
  return(df)
}

##
#' which_coerce_NA
#' 
#' Return index of the elements that would coerce to NA in a vector
#'
#' @param x 
#' @param allow.orig.NA Should NA be allowed. E.g. only NA introduced by as.numeric should be returned
#' @param orig.NA if allow.orig.NA id TRUE this number is internally used for NA in the input
#'
#' @return
#' Return index of the elements that would coerce to NA in a vector
#'
which_coerce_NA <- function(x, allow.orig.NA = TRUE, orig.NA = -989898) {
    
  if (allow.orig.NA) x[is.na(x)] <- orig.NA
  return(which(is.na(as.numeric(x))))
}

##
#' is.possible.numeric
#'
#' Checks if a vector either is numeric or can be coerced to numeric without introducing NA
#' 
#' @param x 
#'
#' @return
#' Returns TRUE if all items can be coerced as numeric
#'
is.possible.numeric <- function(x) {
  x <- as.data.frame(x)[,1] # UGLY but tibble mess things up
  if (is.numeric(x)) return(TRUE)
  if (length(which_coerce_NA(x) > 0)) {
    return(FALSE)
  } else {
    return(TRUE)
  }
}

#' strip_time set components minutes and seconds to zero
#'
#' @param t a vector that can be coerced to time with as.POSIXlt
#'
#' @return
#' a vector of POSIXct
#'
strip_time <- function(t) {
  lt <- as.POSIXlt(t)
  lt[, "sec"] <- 0
  lt[, "min"] <- 0
  return(as.POSIXct(lt))
}


#' mean_nooutliers
#' 
#' Remove outliers from x using boxplot.stats and return the mean of remaining data
#'
#' @param x 
#'
#' @return
#'
mean_nooutliers <-  function(x) {
  return(mean(boxplot.stats(x)$stats))
}

# HOBO related functions ----
#' water_density
#' 
#' Return water density at water temp temp_water
#' Borrowed from https://github.com/GLEON/rLakeAnalyzer/blob/master/R/water.density.R
#'
#' @param temp_water 
#'
#' @return
#' water density at temperature
#'
water_density <- function(temp_water) {
  return((1000 * 
            (1 -(temp_water+288.9414) *
               (temp_water-3.9863)^2/(508929.2*(temp_water+68.12963)))))
}

#' Calculate water level 
#'
#' Calculate water level from barimetric pressure at meassured bottom,
# a refererence air pressure and water temperature
#' @param P_water pressure in water in kPa
#' @param P_ref reference air pressure in kPa
#' @param temp_water water temperature in degrees Celsius
#'
#' @return
#' A numeric. water level in meters
#
calc_depth <- function(P_water, P_ref, temp_water) {
  
  g <-  9.80665
  rho <- water_density(temp_water)
  depth  <- (P_water*1000 - P_ref*1000) / (rho * g) 
}