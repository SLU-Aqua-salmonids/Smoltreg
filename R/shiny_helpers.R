

mk_species_table <- function(fish) {
  res <- as.data.frame(
    table(fish[fish$event != Smoltreg::event$RECAPTURED,]$species))
  names(res) <- c("Species", "N")
  res <- res[order(res$N, decreasing = TRUE), ]
  return(res)
}

mk_unknown_table <- function(fish) {
  return(
    as.data.frame(fish[!fish$species %in% Smoltreg::allowed_species &
                         fish$event != Smoltreg::event$RECAPTURED,])
  )
}

