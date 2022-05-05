# Define constants for the allowed event types
# Event  can be 0 = unknown, 1 = caught (but not marked), 2 = marked, 3 = recaptured
Smoltreg_event <- data.frame(
  UNKNOWN = 0,
  CAUGHT = 1,
  MARKED = 2,
  RECAPTURED = 3,
  REMOVED = 4)
usethis::use_data(Smoltreg_event, overwrite = TRUE)
##
# List of allowed spcies from Sötebasen
allowed_species <- read.table("./data-raw/allowed_species.txt",
                              sep="\t", encoding="UTF-8")[,1]
usethis::use_data(allowed_species, overwrite = TRUE)
