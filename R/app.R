
#library(shiny)


#' smoltregApp
#' 
#' Start the Smoltreg shiny app.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' library(Smoltreg)
#' smoltregApp()
#' }
smoltregApp <- function() {
  # GUI ---------------------------------------------------------------------
  ui <- shiny::fluidPage(
    title ="Smoltreg data checker and converter.",
    shiny::fluidRow(
      shiny::column(
        width = 12,
                    shiny::h1("Smoltreg data checker and converter."),
                    shiny::p("Check your Smoltreg file by running each of the tests below.
               When all test pass use the last button to generate a file suitable
               for upload to Sötebasen."))
    ),
    shiny::fluidRow(
      shiny::column(
        width = 4,
        shiny::fileInput("file1", "Upload Smoltreg file",
                       accept = c("application/vnd.openxmlformats-officedocument.spreadsheetml.sheet",
                                  "application/vnd.ms-excel"))),
      shiny::column(width = 8, shiny::tableOutput("file1"))),
    shiny::fluidRow(shiny::column(width = 4, shiny::actionButton("listspecies", "List species")),
                    shiny::column(width = 8, shiny::tableOutput("listspecies"))),
    shiny::fluidRow(shiny::column(width = 4, shiny::actionButton("unknownspecies", "Unknown species")),
                    shiny::column(width = 8, shiny::tableOutput("unknownspecies"))),
    shiny::fluidRow(shiny::column(width = 4, shiny::actionButton("checkdates", "Check dates")),
                    shiny::column(width = 8, shiny::tableOutput("checkdates"))),
    shiny::fluidRow(shiny::column(width = 4, shiny::actionButton("checknumeric", "Check numeric")),
                    shiny::column(width = 8, shiny::tableOutput("checknumeric"))),
    shiny::fluidRow(shiny::column(width = 4, shiny::actionButton("checksmoltstat", "Check smoltstat")),
                    shiny::column(width = 8, shiny::tableOutput("checksmoltstat"))),
    shiny::fluidRow(shiny::column(width = 4, shiny::actionButton("checkgenid", "Check genid")),
                    shiny::column(width = 8, shiny::tableOutput("checkgenid"))),
    shiny::fluidRow(shiny::column(width = 4, shiny::actionButton("checkevents", "Check events")),
                    shiny::column(width = 8, shiny::tableOutput("checkevents"))),
    shiny::fluidRow(shiny::column(width = 4, shiny::actionButton("checkpittags", "Check PIT tags")),
                    shiny::column(width = 8, shiny::tableOutput("checkpittags"))),
    shiny::fluidRow(shiny::column(width = 4, shiny::actionButton("checkduppittag", "Check duplicated PIT")),
                    shiny::column(width = 8, shiny::tableOutput("checkduppittag"))),
    shiny::fluidRow(shiny::column(width = 4, shiny::actionButton("checkrecapture", "Check unMARKED RECAPTURE events")),
                    shiny::column(width = 8, shiny::tableOutput("checkrecapture"))),
    shiny::fluidRow(shiny::column(width = 4, shiny::actionButton("checkdiffspecies", "Check same species")),
                    shiny::column(width = 8, shiny::tableOutput("checkdiffspecies"))),
    shiny::fluidRow(shiny::column(width = 4, shiny::actionButton("checkfulton", "List fulton outliers")),
                    shiny::column(width = 8, shiny::tableOutput("checkfulton"))),
    shiny::fluidRow(shiny::column(width = 4, shiny::downloadButton("XLSXfile", "Save Sötebasen-file")),
                    shiny::column(width = 8, shiny::checkboxInput("do_envdata", "Use envdata", value = TRUE)))
  )

  # Server ------------------------------------------------------------------------
    server <- function(input, output) {
    ##    source("config.R", encoding = "UTF-8")
    ##    source("functions.R", encoding = "UTF-8")
    #require(Smoltreg)
    as_result_df <- function(x) {
      return(data.frame(Result = x))
    }
    metadata <- shiny::reactive(read_meta(input$file1$datapath))
    fishdata <- shiny::reactive(read_fish(input$file1$datapath, dummy_tags = metadata()$dummy_tags))
    
    output$file1 <- shiny::renderTable({
      if (is.null(input$file1)) {
        return(data.frame(River = NA, Start = NA, End =  NA, N_fish = NA))
      }
#      metadata <<- read_meta(input$file1$datapath)
#      fishdata <<- read_fish(input$file1$datapath,
#                            dummy_tags = metadata$dummy_tags)
      infotab <- data.frame(River = metadata()$river,
                            Start = as.character(metadata()$startdate),
                            End = as.character(metadata()$enddate),
                            N_fish = nrow(fishdata()))
      return(infotab)}
    )
    
    output$listspecies <- shiny::renderTable({
      if (input$listspecies == 0 | is_odd(input$listspecies)) {
        return(as_result_df('Click "List species" button'))
      }
        res <- mk_species_table(fishdata())
        return(res)
    })
    
    output$unknownspecies <- shiny::renderTable({
      if (input$unknownspecies == 0 | is_odd(input$unknownspecies)) {
        return(as_result_df('Click "Unknown species" button'))
      }
      res <- mk_unknown_table(fishdata())
      return(res)
    })
    
    output$checkdates <- shiny::renderTable({
      if (input$checkdates == 0 | is_odd(input$checkdates)) {
        return(as_result_df('Click "Check dates" button to test date_time column format'))
      }
      if (any(is.na(as.POSIXct(fishdata()$date_time)))) {
        res <- as_result_df("+ Column *date_time* contains data that does not convert to a date, **FIX IT**.")
      } else {
        res <- as_result_df("+ Column *date_time* looks OK.\n")
      }
      return(res)
    })
 
    output$checknumeric <- shiny::renderTable({
      if (input$checknumeric == 0 | is_odd(input$checknumeric)) {
        return(as_result_df('Click "Check numeric" button'))
      } else {
        res <- NULL
        for (colname in c("length", "weight")) {
          res <- rbind(res,
                data.frame(Column = colname,
                           Numeric = can_coerce_numeric(fishdata()[, colname])))
        }
      }
      return(res)
    })
    
    output$checksmoltstat <- shiny::renderTable({
      if (input$checksmoltstat == 0 | is_odd(input$checksmoltstat)) {
        return(as_result_df('Click "Check smoltstat" button'))
      }
      stattab <- fishdata() %>%
        dplyr::filter(!is.na(smoltstat)) %>%
        dplyr::filter(!(smoltstat %in% c('S0', 'S1', 'S2', 'S3')))
      if (nrow(stattab) == 0) {
        return(as_result_df('Column "smoltstat" OK'))
      } else {
        return(stattab)
      }
    })
    
    output$checkgenid <- shiny::renderTable({
      if (input$checkgenid == 0 | is_odd(input$checkgenid)) {
        return(as_result_df('Click "Check genid" button to find duplicated genid'))
      }
      dups <- duplicated(fishdata()$genid) & !is.na(fishdata()$genid)
      if (any(dups)) {
        dupstable <- fishdata()[dups,]
      } else {
        dupstable <- as_result_df("No fish with duplicated genid. :-)") 
      }
      return(dupstable)
    })

    output$checkevents <- shiny::renderTable({
      if (input$checkevents == 0 | is_odd(input$checkevents)) {
        return(as_result_df('Click "Check event" button for summary of event types'))
      }
      events <- factor(fishdata()$event,
                       c(Smoltreg::event[1, ]),
                       labels = c(names(Smoltreg::event)))
      eventtable <- as.data.frame(table(events))
      return(eventtable)
    })
    
    output$checkpittags <- shiny::renderTable({
      if (input$checkpittags == 0 | is_odd(input$checkpittags)) {
        return(as_result_df('Click "Check pittags" button to find MARKED fish without tag'))
      }
      etab <- fishdata() %>%
        dplyr::filter(event %in% c(Smoltreg::event$MARKED, Smoltreg::event$RECAPTURED)) %>%
        dplyr::filter(is.na(pittag))
      if (nrow(etab) == 0) {
        etab <- as_result_df("No marked or recaptured fish without pittag. :-)")
      }
      return(etab)
    })
    
    output$checkduppittag <- shiny::renderTable({
      if (input$checkduppittag == 0 | is_odd(input$checkduppittag)) {
        return(as_result_df('Click "Check duplicate pit" button to find duplicated pittags'))
      }
      marked <- fishdata() %>%
        dplyr::filter(event == Smoltreg::event$MARKED) # all rows with marking event
      recap <- fishdata() %>%
        dplyr::filter(event == Smoltreg::event$RECAPTURED) # all rows with recapture event 
      marked_dups_IDs <- marked[duplicated(marked$pittag), ]$pittag # all 
      recap_dups_IDs <- recap[duplicated(recap$pittag), ]$pittag
      mtab <- marked %>%
        dplyr::filter(pittag %in% marked_dups_IDs) %>%
        dplyr::arrange(pittag)
      rtab <- recap %>%
        dplyr::filter(pittag %in% recap_dups_IDs) %>%
        dplyr::arrange(pittag)
      res <- dplyr::bind_rows(mtab, rtab)
      if (nrow(res) ==  0) {
        res <- as_result_df("No duplicates found in marked fish. :-)")
      }
      return(res)
    })
    
    output$checkrecapture <- shiny::renderTable({
      if (input$checkrecapture == 0 | is_odd(input$checkrecapture)) {
        return(as_result_df('Test that RECAPTUREs have an MARKED event'))
      }
      marked <- fishdata() %>%
        dplyr::filter(event == Smoltreg::event$MARKED) # all rows with marking event
      recap <- fishdata() %>%
        dplyr::filter(event == Smoltreg::event$RECAPTURED) # all rows with recapture event 
      r_ids <- unique(recap$pittag)
      m_ids <- unique(marked$pittag)
      recap_not_marked <- r_ids[!(r_ids %in% m_ids)]
      res <- fishdata() %>% # Create table with RECAPTUREs without MARKED event
        dplyr::filter(event == Smoltreg::event$RECAPTURED) %>%
        dplyr::filter(pittag %in% recap_not_marked)
      if (nrow(res) == 0) {
        res <- as_result_df("No unmarked recaptures found. :-)")
      }
      return(res)
    })
    
    output$checkdiffspecies <- shiny::renderTable({
      if (input$checkdiffspecies == 0 | is_odd(input$checkdiffspecies)) {
        return(as_result_df('Test that species is the same for MARKED and RECAPTURE'))
      }
      sp_err <- fishdata() %>% # If scount is > 1 we have 2 or more species on the same tag
        dplyr::filter(!is.na(pittag)) %>%
        dplyr::filter(!is.na(species)) %>%
        dplyr::group_by(pittag) %>%
        dplyr::mutate(scount = length(unique(species))) %>%
        dplyr::filter(scount > 1)
      sp_err_ids <- unique(sp_err$pittag)
      res <- fishdata() %>%
        dplyr::filter(pittag %in% sp_err_ids) %>%
        dplyr::arrange(pittag)
      if (nrow(res) == 0) {
        res <- "No duplicated species errors found. :-)"
      }
      return(res)
    })
    
    output$checkfulton <- shiny::renderTable({
      if (input$checkfulton == 0 | is_odd(input$checkfulton)) {
        return(as_result_df('List fish with fulton condition outside limits'))
      }
      min_k <- Smoltreg_limits()$min_k
      max_k <- Smoltreg_limits()$max_k
      res <- fishdata() %>%
        dplyr::filter(species %in% c("Lax", "Öring")) %>%
        dplyr::filter(!is.na(as.numeric(length)) & !is.na(as.numeric(weight))) %>%
        dplyr::filter(length < 400) %>%
        dplyr::mutate(k = fulton(weight, length)) %>%
        dplyr::filter(k < min_k | k > max_k)
      if (nrow(res) == 0) {
        res <- as_result_df("All fish have condition factors within limits. :-)")
      }
      return(res)
    })
    
    output$XLSXfile <- shiny::downloadHandler(
      filename = function() {paste0(tools::file_path_sans_ext(input$file1$name),
                                    "_sötebasen.xlsx")},
      content = function(file) {
        # Generate Excel in 'file' here.
        tmpDir <- tempdir()
        odir <- setwd(tmpDir)
        # metadata <- read_meta(input$file1$datapath)
        # fishdata <- read_fish(input$file1$datapath,
        #                       dummy_tags = metadata$dummy_tags)
        start_year <- as.numeric(format(metadata()$startdate, "%Y"))
        end_year <- as.numeric(format(metadata()$enddate, "%Y"))
        
        # We define "Insamling" as one season for one trap.
        # Start and stop year (Årtal och Årtal2) are set to the same year.
        Insamling <- data.frame(InsamlingID = 1,
                                Vatten = metadata()$river,
                                Årtal = start_year,
                                Årtal2 = start_year,
                                Metod = metadata()$Metod,
                                Ansvarig = metadata()$Ansvarig,
                                Syfte = metadata()$Syfte,
                                Sekretess = "Nej",
                                Signatur = metadata()$Signatur
        )
        # Also "Ansträngning" is one season for one trap.
        AnmAnstr <- ""
        if (!is.null(metadata()$AnmAnstr)) {
          AnmAnstr <- metadata()$AnmAnstr
        }
        Ansträngning <- data.frame(AnsträngningID = 1,
                                   InsamlingID = 1,
                                   AnstrTyp = metadata()$Metod,
                                   AnstrPlats = metadata()$loc_name,
                                   AnstrDatumStart = metadata()$startdate,
                                   AnstrDatumSlut = metadata()$enddate,
                                   AnstrS99TM_N_1 = metadata()$N_coord,
                                   AnstrS99TM_E_1 = metadata()$E_coord,
                                   Märkning = metadata()$Märkning,
                                   SignAnstr = metadata()$Signatur,
                                   AnmAnstr = AnmAnstr
        )
        ###
        ## Assume that catch_time == "00:00" equals missing time and set time to NA.
        catch_time <- format(fishdata()$date_time, "%H:%M")
        catch_time <- ifelse(catch_time == "00:00", NA, catch_time)
        Individ <- data.frame(#IndividID = 1:nrow(fishdata()),
          InsamlingId = 1,
          AnsträngningID = 1,
          FångstDatum = format(fishdata()$date_time, "%Y-%m-%d"),
          FångstTid = catch_time,
          Art = fishdata()$species,
          Åldersprov = ifelse(is.na(fishdata()$genid), 'Nej', 'Ja'),
          Provkod = fishdata()$genid,
          Längd1 = fishdata()$length,
          Vikt1 = fishdata()$weight,
          Behandling = event2Behandling(fishdata()$event),
          Stadium = fishdata()$smoltstat,
          MärkeNr = as.character(fishdata()$pittag),
          Märkning = ifelse(is.na(fishdata()$pittag),NA, metadata()$Märkning),
          SignIndivid = metadata()$Signatur,
          AnmIndivid = fishdata()$comment
        )
        
        sheetnames <- c('Insamling', enc2utf8('Ansträngning'), 'Individ')
        l <- list(Insamling, Ansträngning, Individ)
        if (input$do_envdata) {
          envdata <- read_envdata(input$file1$datapath, metadata()$startdate, metadata()$enddate)
          Temperatur <- data.frame(InsamlingID = 1,
                                   MätDatum = envdata$date,
                                   Tempbotten = envdata$w_temp,
                                   Vattennivå = envdata$w_level,
                                   SignTemp = metadata()$Signatur
          )
          sheetnames <- c(sheetnames, 'Temperatur')
          l <- c(l , list(Temperatur))
        }
        names(l) <- sheetnames
        writexl::write_xlsx(l, path = file)
        setwd(odir)
      }, # End function(file)
      contentType = "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet"
    ) # End output$XLSXfile

    }
    shiny::shinyApp(ui, server)
}

