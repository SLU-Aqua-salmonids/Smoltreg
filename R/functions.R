##
#library(dplyr)
#library(magrittr)
#library(readxl)


##
#' can_coerce_numeric
#'
#' Checks if a vector either is numeric or can be coerced to numeric without introducing NA
#' 
#' @param x 
#'
#' @return
#' Returns TRUE if all items can be coerced as numeric
#'
#' @export
can_coerce_numeric <- function(x) {
  x <- as.data.frame(x)[,1] # UGLY but tibble mess things up
  if (is.numeric(x)) return(TRUE)
  if (length(which_coerce_NA(x) > 0)) {
    return(FALSE)
  } else {
    return(TRUE)
  }
}


##
#' impute_date_time
#' 
#' Impute a time vector with dates using "last observation carried forward"
#' if package imputeTS is installed
#'
#' @param x Vector with dates
#'
#' @return imputed vector with dates
#' @export
#'
impute_date_time <- function(x) {
  if (!requireNamespace("imputeTS")) {
    warning("Library imputeTS missing. Can not impute date.")
    return(x)
  }
  res <- as.character( # Back to character
    as.POSIXct( # Back to POSIX time
      imputeTS::na_locf( # Impute with Last Obs Carried Forward
        as.numeric( # Convert the date_time to number (secs since origin)
          as.POSIXct(x))), origin = "1970-01-01")) # Convert date_time to POSIXct
  return(res)
}

# -------------------------------------------------------
#'  Read sheet with meta data
#'
#' @param xlsxfile Name on excel-file following the "Smoltreg"-format 
#'
#' @return
#' A list with variables from sheets "Metadata" and "Metadata2"
#' @export
#'
read_meta <- function(xlsxfile) {
  sheets <- readxl::excel_sheets(xlsxfile)
  d <- readxl::read_excel(xlsxfile, sheet = "Metadata", col_names = TRUE)
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
    d2 <- readxl::read_excel(xlsxfile, sheet = "Metadata2")
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

# read_fish() ------------------------- 
#'  Read sheet with fish data and do some basic cleanup.
#' 
#' Read an excel-file in the **Smoltreg**-format and do some basic cleanup before a data.frame
#' is returned.
#'  If a vector of dummy_tags is given all tags matching are removed. The species name
#'  is standardized to capital first letter and the rest lowercase. Columns **smoltstat** and
#'  **genid** are uppercased. The column **date_time** is standarised to POSIXct and
#'  missing values are imputed using a "last observation carried forward" algorithm.
#'  Missing Any NA in column **event** are replaced with the UNKNOWN event code. Recaptured
#'  fish without species get the species set to the same as the MARKED event for that pit tag.
#'
#' @param xlsxfile Name on excel-file following the "Smoltreg"-format
#' @param dummy_tags Vector of character with tags used as dummies that should be removed
#' @param sheet Name of sheet with the fish data. Default = "Fiskdata"
#' @param date_formats Character vector with DateTime formats that should be tried when reading dates
#'
#' @return
#' A data frame with fishdata from the smolt trap. Basic cleanup done.
#' @export
#'
read_fish <- function(xlsxfile, dummy_tags = NULL, sheet = "Fiskdata",
                      date_formats = c('%Y-%m-%d %H:%M:%S',
                                       '%Y-%m-%d',
                                       '%m-%d-%Y %H:%M:%S',
                                       '%m/%d/%y.%H:%M:%S')) {
  d <- readxl::read_excel(xlsxfile, sheet = sheet)
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
    d[which(is.na(d$event)),]$event <- Smoltreg::event$UNKNOWN
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
  recaptures_id <- d[d$event == Smoltreg::event$RECAPTURED & is.na(d$species), ]$pittag
  recaptures_id <- unique(recaptures_id) # Filter duplicates (maybe not the right thing to do)
  if (length(recaptures_id > 0)) { # Create a table with the data recorded for pittag at marking
    sp.df <- d[d$event == Smoltreg::event$MARKED & d$pittag %in% recaptures_id,
               c("pittag", "species", "length", "weight", "smoltstat")] # Columns to extract
    for (i in seq_along(recaptures_id)){ # Set missing species to species from MARK event 
      d[!is.na(d$pittag) & d$pittag == recaptures_id[i] & is.na(d$species), ] %<>% # Assignment pipe, see magrittr
        dplyr::mutate(species = sp.df[sp.df$pittag == recaptures_id[i],]$species,
               smoltstat = sp.df[sp.df$pittag == recaptures_id[i],]$smoltstat,
               length = sp.df[sp.df$pittag == recaptures_id[i],]$length,
               weight = sp.df[sp.df$pittag == recaptures_id[i],]$weight)
    }
  }
  return(d)
}

# Read environmetal data -----------------------------------------------------------

read_hobo <- function(f, sheet, tz="CET") {
  new_names <-  c('date_time', 'pressure', 'temp') #, 'couplerDet',
#                  'couplerAtt', 'hostConn', 'stopped', 'EOF')
  d <- readxl::read_excel(f, sheet = sheet)
  names(d) <- new_names
  d <- d[, 1:3]
  d$date_time <- as.POSIXct(as.character(d$date_time), tz="CET")
  return(d)
}



#' Read temp and pressure data
#' 
#' Read data with water temperature and pressure + reference pressure in air.
#' Calculate mean water depth and water temperature per day.
#'
#' @param xlsxfile Name of excel file
#' @param firstdate Date for first date in returned time series
#' @param lastdate  Date for last date in returned time series
#' @param sheet1 Name of sheet with data from logger in water
#' @param sheet2  Name of sheet with data from logger in air
#'
#' @return
#' return formated water temperature and depth
#' @export
#'
read_envdata <- function(xlsxfile, firstdate, lastdate,
                         sheet1 = "Envlogger_water", sheet2 = "Envlogger_land") {
  sheets <- readxl::excel_sheets(xlsxfile)
  if (all(c(sheet1, sheet2) %in% sheets)) { # Are both hobo-sheets there?
    water <- read_hobo(xlsxfile, sheet = sheet1) %>%
      dplyr::mutate(date_time = strip_time(date_time)) %>%
      dplyr::filter(as.Date(date_time) >= firstdate & as.Date(date_time) <= lastdate) %>%
      dplyr::rename(water_p = pressure, water_t = temp)
    land <- read_hobo(xlsxfile, sheet = sheet2) %>%
      dplyr::mutate(date_time = strip_time(date_time)) %>%
      dplyr::filter(as.Date(date_time) >= firstdate & as.Date(date_time) <= lastdate) %>%
      dplyr::rename(land_p = pressure, land_t = temp)
    alldata <- water %>% dplyr::inner_join(land, by="date_time") %>%
      dplyr::mutate(depth = calc_depth(water_p, land_p, water_t))
    per_day <- alldata %>%
      dplyr::mutate(date = as.Date(date_time)) %>%
      dplyr::group_by(date) %>%
      dplyr::summarise(w_level = round(mean_nooutliers(depth) * 100, 1), # to centimeters
                w_temp  = round(mean_nooutliers(water_t), 1))
  } else { # Use sheet Miljödata instead
##    per_day <- read_excel(xlsxfile, sheet="Miljödata", skip=1) %>%
##      dplyr::select(date = 1, w_level = 3, w_temp = 4) %>% # Select by column number
      per_day <- readxl::read_excel(xlsxfile, sheet="Miljödata") %>%
        dplyr::select(date = 1, w_level = 2, w_temp = 3) %>% # Select by column number
        dplyr::mutate(w_level = as.numeric(w_level), w_temp = as.numeric(w_temp))
  }
  
  return(per_day)
}


#' Convert event code to Sötebasen string
#'
#' @param x event codes
#'
#' @return
#' character strings 
#' @export
#'
#' @examples
#' event2Behandling(c(0,1,2,3,4))
event2Behandling <- function(x) {
  # Translate event codes to Sötebasens strings for Behandling
  return(as.character(
    factor(x,
           levels = c(Smoltreg::event$UNKNOWN, Smoltreg::event$CAUGHT,
                      Smoltreg::event$MARKED, Smoltreg::event$RECAPTURED,
                      Smoltreg::event$REMOVED),
           labels = c('', 'Utsatt', 'Märkt&utsatt',
                      'Återfångad&utsatt', 'Landad/avlivad/död')
    )
  )
  )
}

# Save data to SQLite ----------------------------------------------------------------
  # save_to_sqlite <- function(dbname, table, x, overwrite = TRUE) {
  #   if (!require(DBI) | !require(RSQLite)) {
  #     warning("Missing package DBI and/or RSQLite. Can not save to local database.")
  #   }
  #   mydb <- dbConnect(RSQLite::SQLite(), dbname)
  #   dbWriteTable(mydb, table, as.data.frame(x), overwrite = overwrite)
  #   dbDisconnect(mydb)
  # }
  # 
