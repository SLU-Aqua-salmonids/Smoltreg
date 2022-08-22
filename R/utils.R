is_odd <- function(x) {
  return(x %% 2 == 0)        # Create even/odd logical
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

