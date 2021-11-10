##
library(dplyr)
library(magrittr)
library(readxl)

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
remove_empty_rows <- function(df) {
  all.NA.idx <- which(apply(df,1,function(x)all(is.na(x))))
  
  if (length(all.NA.idx) > 0) {
    df <- df[-all.NA.idx,] # Rem rows with all cols == NA
  }
  return(df)
}
##
which_coerce_NA <- function(x, allow.orig.NA = TRUE, orig.NA = -989898) {
  ## Return index of the elements that would coerce to NA in a vector  
  if (allow.orig.NA) x[is.na(x)] <- orig.NA
  return(which(is.na(as.numeric(x))))
}
##
is.possible.numeric <- function(x) {
  ## Checks if a vector either is numeric or can be coerced to numeric without introducing NA
  x <- as.data.frame(x)[,1] # UGLY but tibble mess things up
  if (is.numeric(x)) return(TRUE)
  if (length(which_coerce_NA(x) > 0)) {
    return(FALSE)
  } else {
    return(TRUE)
  }
}
##
impute_date_time <- function(x) {
  if (!require(imputeTS)) {
    warning("Library imputeTS missing. Can not impute date.")
    return(x)
  }
  res <- as.character( # Back to character
    as.POSIXct( # Back to POSIX time
      na_locf( # Impute with Last Obs Carried Forward
        as.numeric( # Convert the date_time to number (secs since origin)
          as.POSIXct(x))), origin = "1970-01-01")) # Convert date_time to POSIXct
  return(res)
}

# Read sheet with meta data -------------------------------------------------------
read_meta <- function(xlsxfile, sheet = "Metadata" ) {
  sheets <- excel_sheets(xlsxfile)
  d <- read_excel(xlsxfile, sheet = sheet, col_names = TRUE)
  d <- as.data.frame(d)
  d <- remove_empty_rows(d)
  names(d) <- c("river", "loc_name", "dates", "coords", "contact", "email",
                "downtime", "dummy_tags")
  dummy_tags <- d$dummy_tags[!is.na(d$dummy_tags)]
  downtime <- as.Date(d$downtime[!is.na(d$downtime)])
  l1 <- list(river = d$river[1],
             loc_name = d$loc_name[1],
             startdate = as.Date(d$dates[1]),
             enddate = as.Date(d$dates[2]),
             N_coord = as.numeric(d$coords[1]),
             E_coord = as.numeric(d$coords[2]),
             contact = d$contact[1],
             email   = d$email[1],
             dummy_tags = dummy_tags,
             downtime = downtime
  )
  if ("Metadata2" %in% sheets) {
    d2 <- read_excel(xlsxfile, sheet = "Metadata2")
    l2 <- as.list(d2)
    # d2 <- remove_empty_rows(d2)
    # d2[,1] <- gsub(":", "", d2[,1])
    # 
    # l2 <- setNames(as.list(d2[,2]), d2[,1])
  } else {
    l2 <- NULL
  }
  return(c(l1, l2))
}

# read_meta_V1 <- function(xlsxfile, sheet = "Metadata" ) {
#   sheets <- excel_sheets(xlsxfile)
#   d <- read_excel(xlsxfile, sheet = sheet, col_names = FALSE)
#   d <- as.data.frame(d)
#   d <- remove_empty_rows(d)
#   d[,1] <- tolower(d[,1])
#   d[,1] <- gsub(":", "", d[,1])
#   d[,1] <- gsub("älv", "river", d[,1])
#   d[,1] <- gsub("fällplats", "loc_name", d[,1])
#   d[,1] <- gsub("startdatum", "startdate", d[,1])
#   d[,1] <- gsub("stoppdatum", "enddate", d[,1])
#   d[,1] <- gsub("nord", "N_coord", d[,1])
#   d[,1] <- gsub("öst", "E_coord", d[,1])
#   d[,1] <- gsub("driftansvarig", "contact", d[,1])
#   d[,1] <- gsub("e-post", "email", d[,1])
#   d[,1] <- gsub("dummy tag", "dummy_tags", d[,1])
#   
#   l1 <- list(river = d[d[,1]=="river",2],
#              loc_name = d[d[,1]=="loc_name",2],
#              startdate = as.Date(d[d[,1]=="startdate",2]),
#              enddate = as.Date(d[d[,1]=="enddate",2]),
#              N_coord = as.numeric(d[d[,1]=="N_coord",2]),
#              E_coord = as.numeric(d[d[,1]=="E_coord",2]),
#              contact = d[d[,1]=="contact",2],
#              email   = d[d[,1]=="email",2],
#              dummy_tags = d[d[,1]=="dummy_tags",2]
#   )
#   if ("Metadata2" %in% sheets) {
#     d2 <- read_excel(xlsxfile, sheet = "Metadata2", col_names = FALSE)
#     d2 <- as.data.frame(d2)
#     d2 <- remove_empty_rows(d2)
#     d2[,1] <- gsub(":", "", d2[,1])
#     
#     l2 <- setNames(as.list(d2[,2]), d2[,1])
#   } else {
#     l2 <- NULL
#   }
#   return(c(l1, l2))
# }

# Read sheet with fish data and do some basic cleanup. ------------------------- 
read_fish <- function(xlsxfile, dummy_tags = NULL, sheet = "Fiskdata",
                      date_formats = c('%m-%d-%Y %H:%M:%S',
                                       '%Y-%m-%d %H:%M:%S',
                                       '%m/%d/%y.%H:%M:%S')) {
  d <- read_excel(xlsxfile, sheet = sheet)
  d <- d[, 1:9] # Columns needed for database MUST be  columns 1:9
  names(d) <- c("pittag", "date_time", "species", "smoltstat", "length",
                    "weight", "event", "genid", "comment")
  d <- remove_empty_rows(d)
  d$species <- paste(toupper(substr(d$species, 1, 1)), 
                     tolower(substr(d$species, 2, nchar(d$species))), sep="") #tolower(d$species)
  # Casefolding above will introduce errorneous "NANA", fix them
  d$species <- ifelse(d$species == "NANA", NA, d$species)
  # Sötebasen wants "Gers" instead of "Gärs"
  d$species <- ifelse(d$species == "Gärs", "Gers", d$species)
  d$smoltstat <- toupper(d$smoltstat)
  d$genid <- toupper(as.character(d$genid))

  if (any(is.na(d$event))) {
    # If event is NA set it to UNKNOWN
    d[which(is.na(d$event)),]$event <- UNKNOWN
  }
  if (any(d$pittag %in% dummy_tags)) {
    # Unmarked fish should not have a pittag, remove them (they are scanned dummytags)
    d[which(d$pittag %in% dummy_tags), ]$pittag <- NA
  }
  
  # Standarise date_time
  d$date_time <- as.character(as.POSIXct(d$date_time, tryFormats=date_formats))
  if (any(is.na(d$date_time))) {
    ## In this file format unmarked fish might not have a timestamp. We impute the time
    ## by using na_locf (Last Observation Carried Forward) from imputeTS. This might
    ## not be fool proof but the influence if a couple of unmarked fish are registered
    ## one day off is probably ignorable.
    d$date_time <- impute_date_time(d$date_time)
  }
  d$date_time <- as.POSIXct(d$date_time)
  recaptues_id <- d[d$event == RECAPTURED & is.na(d$species), ]$pittag
  recaptues_id <- unique(recaptues_id) # Filter duplicates (maybe not the right thing to do)
  if (length(recaptues_id > 0)) { # Create a table with the data recorded for pittag at marking
    sp.df <- d[d$event == MARKED & d$pittag %in% recaptues_id,
               c("pittag", "species", "length", "weight", "smoltstat")] # Columns to extract
    for (i in seq_along(recaptues_id)){ # Set missing species to species from MARK event 
      d[!is.na(d$pittag) & d$pittag == recaptues_id[i] & is.na(d$species), ] %<>%
        mutate(species = sp.df[sp.df$pittag == recaptues_id[i],]$species,
               smoltstat = sp.df[sp.df$pittag == recaptues_id[i],]$smoltstat,
               length = sp.df[sp.df$pittag == recaptues_id[i],]$length,
               weight = sp.df[sp.df$pittag == recaptues_id[i],]$weight)
    }
  }
  return(d)
}

# Read environmetal data -----------------------------------------------------------
water_density <- function(temp_water) {
  # Return water density at water temp temp_water
  # Borrowed from https://github.com/GLEON/rLakeAnalyzer/blob/master/R/water.density.R
  return((1000 * 
            (1 -(temp_water+288.9414) *
               (temp_water-3.9863)^2/(508929.2*(temp_water+68.12963)))))
}

#´ Calculate water level 
#´
#´ Calculate water level from barimetric pressure at meassured bottom,
#´ a refererence air pressure and water temperature
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
read_hobo <- function(f, sheet, tz="CET") {
  new_names <-  c('date_time', 'pressure', 'temp') #, 'couplerDet',
#                  'couplerAtt', 'hostConn', 'stopped', 'EOF')
  d <- read_excel(f, sheet = sheet)
  names(d) <- new_names
  d$date_time <- as.POSIXct(as.character(d$date_time), tz="CET")
  return(d)
}

#' strip_time set components minutes and seconds to zero
#'
#' @param t a verctor of POSTIXct
#'
#' @return a vector of POSIXct
#' @export
#'
#' @examples
strip_time <- function(t) {
  lt <- as.POSIXlt(t)
  lt[, "sec"] <- 0
  lt[, "min"] <- 0
  return(as.POSIXct(lt))
}
mean_nooutliers <-  function(x) {
  # Use boxplot.stats to remove outliers and return mean of remaining data
  return(mean(boxplot.stats(x)$stats))
}

read_envdata <- function(xlsxfile, firstdate, lastdate,
                         sheet1 = "Envlogger_water", sheet2 = "Envlogger_land") {
  sheets <- excel_sheets(xlsxfile)
  if (all(c(sheet1, sheet2) %in% sheets)) { # Are both hobo-sheets there?
    water <- read_hobo(xlsxfile, sheet = sheet1) %>%
      mutate(date_time = strip_time(date_time)) %>%
      filter(as.Date(date_time) >= firstdate & as.Date(date_time) <= lastdate) %>%
      rename(water_p = pressure, water_t = temp)
    land <- read_hobo(xlsxfile, sheet = sheet2) %>%
      mutate(date_time = strip_time(date_time)) %>%
      filter(as.Date(date_time) >= firstdate & as.Date(date_time) <= lastdate) %>%
      rename(land_p = pressure, land_t = temp)
    alldata <- water %>% inner_join(land, by="date_time") %>%
      mutate(depth = calc_depth(water_p, land_p, water_t))
    per_day <- alldata %>%
      mutate(date = as.Date(date_time)) %>%
      group_by(date) %>%
      summarise(w_level = round(mean_nooutliers(depth) * 100, 1), # to centimeters
                w_temp  = round(mean_nooutliers(water_t), 1))
  } else { # Use sheet Miljödata instead
##    per_day <- read_excel(xlsxfile, sheet="Miljödata", skip=1) %>%
##      select(date = 1, w_level = 3, w_temp = 4) %>% # Select by column number
      per_day <- read_excel(xlsxfile, sheet="Miljödata") %>%
        select(date = 1, w_level = 2, w_temp = 3) %>% # Select by column number
        mutate(w_level = as.numeric(w_level), w_temp = as.numeric(w_temp))
  }
  
  return(per_day)
}


# Save data to SQLite ----------------------------------------------------------------
  save_to_sqlite <- function(dbname, table, x, overwrite = TRUE) {
    if (!require(DBI) | !require(RSQLite)) {
      warning("Missing package DBI and/or RSQLite. Can not save to local database.")
    }
    mydb <- dbConnect(RSQLite::SQLite(), dbname)
    dbWriteTable(mydb, table, as.data.frame(x), overwrite = overwrite)
    dbDisconnect(mydb)
  }

