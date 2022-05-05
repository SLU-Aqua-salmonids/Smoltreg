is_odd <- function(x) {
  return(x %% 2 == 0)        # Create even/odd logical
}

event2Behandling <- function(x) {
  # Translate event codes to Sötebasens strings for Behandling
  return(as.character(
    factor(x,
           levels = c(Smoltreg_event$UNKNOWN, Smoltreg_event$CAUGHT,
                      Smoltreg_event$MARKED, Smoltreg_event$RECAPTURED,
                      Smoltreg_event$REMOVED),
           labels = c('', 'Utsatt', 'Märkt&utsatt',
                      'Återfångad&utsatt', 'Landad/avlivad/död')
    )
  )
  )
}

mk_species_table <- function(fish) {
  res <- as.data.frame(sort(
    table(fish[fish$event != Smoltreg_event$RECAPTURED,]$species), decreasing =  TRUE))
  names(res) <- c("Species", "N")
  return(res)
}

mk_unknown_table <- function(fish) {
  allowed_species <- read.table(system.file("extdata", "allowed_species.txt", package = "Smoltreg"),
                                sep="\t", encoding="UTF-8")[,1]
  return(
    as.data.frame(fish[!fish$species %in% allowed_species &
                         fish$event != Smoltreg_event$RECAPTURED,])
  )
}
