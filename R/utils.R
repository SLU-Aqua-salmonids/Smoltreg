is_odd <- function(x) {
  return(x %% 2 == 0)        # Create even/odd logical
}

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

mk_species_table <- function(fish) {
  res <- as.data.frame(sort(
    table(fish[fish$event != Smoltreg::event$RECAPTURED,]$species), decreasing =  TRUE))
  names(res) <- c("Species", "N")
  return(res)
}

mk_unknown_table <- function(fish) {
  return(
    as.data.frame(fish[!fish$species %in% Smoltreg::allowed_species &
                         fish$event != Smoltreg::event$RECAPTURED,])
  )
}

