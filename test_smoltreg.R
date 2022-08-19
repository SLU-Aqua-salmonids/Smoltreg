library(Smoltreg)
library(dplyr)
library(readxl)


smoltreg_file <- file.choose()

metadata <- read_meta(smoltreg_file)
fishdata <- read_fish(smoltreg_file, dummy_tags = metadata$dummy_tags)
#allowed_species <- read.table(system.file("extdata", "allowed_species.txt", package = "Smoltreg"),
#                              sep="\t", encoding="UTF-8")[,1]
river <- metadata$river
year <- format(metadata$startdate, "%Y")

#The number of fish per species:
sort(table(fishdata[fishdata$event != Smoltreg::event$RECAPTURED,]$species), decreasing =  TRUE)

#Check for unknown species in input.
as.data.frame(fishdata[!fishdata$species %in% allowed_species &
                         fishdata$event != Smoltreg::event$RECAPTURED,])

##
#cat(paste0("**Checking fish data format in selected columns read from *", params$origName, "*.**\n\n"))
cat(paste0("**Checking fish data format in selected columns read from *", smoltreg_file, "*.**\n\n"))
if (any(is.na(as.POSIXct(fishdata$date_time)))) {
  cat("+ Column *date_time* contains data that does not convert to a date, **FIX IT**.\n")
} else {
  cat("+ Column *date_time* looks OK.\n")
}
num.columns <- c("length", "weight")
LENGTH_WEIGHT_OK <- TRUE # First assume check will be OK
for (name in num.columns ) {
  if (is.possible.numeric(fishdata[ ,name])) {
    cat(paste("+ Column *", name, "* is numeric, good.\n", sep = ""))
    fishdata[ ,name] <- as.numeric(unlist(fishdata[ ,name]))
  } else {
    cat(paste("+ Column *", name, "* is **NOT** numeric, **FIX IT**.\n", sep = ""))
    LENGTH_WEIGHT_OK <- FALSE
  }
}

## Smolt status
stab <- fishdata %>%
  filter(!is.na(smoltstat)) %>%
  filter(!(smoltstat %in% c('S0', 'S1', 'S2', 'S3')))

if (nrow(stab) > 0) {
  knitr::kable(stab,
               caption = "Fish with unknown smoltstat.")
} else {
  cat("No fish with unknown smoltstat. :-)")
}


