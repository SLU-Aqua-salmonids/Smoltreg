shinyApp(
  # GUI ---------------------------------------------------------------------
  ui = fluidPage(
    titlePanel("Smoltreg data checker and converter."),
    fluidRow(
      column(12,
             p("Choose input file and upload it by clicking the button.
               Then you check your data file with the 'Generate data check report' button.
               Fix errors detected in data check and when the file is OK proceed and 
               'Generate ZIP-file' for delivery to Sötebasen."),
             #             a(href = "HOWTO.html", "HOWTO!"),
             hr()
      )),
    fluidRow(
      column(1,
             p("Step 1")),
      column(5,
             fileInput("file1", "Upload Smoltreg file",
                       accept = c("application/vnd.openxmlformats-officedocument.spreadsheetml.sheet",
                                  "application/vnd.ms-excel")
             )),
      column(6,
             checkboxInput("do_envdata", label="Have temp and water data.",
                           value = TRUE
             )),
      hr()
    ),
    fluidRow(
      column(1,
             p("Step 2")),
      column(11,
             downloadButton("report", "Generate data check report"),
             p("The report will be downloaded and you need to click on it to show it.
               Usually in the left, lower corner of the browser. Check and fix errors
               and repeat step 1. When everything is fixed proceed to step 3."),
             hr()
      )
    ),
    fluidRow(
      column(1,
             p("Step 3")),
      column(11,
             downloadButton("XLSXfile", "Generate Excel-file"),
             p("This will generate a Excel-file data formatted for Sötebasen import."),
             hr()
      )
    ),
    fluidRow(
      column(1,
             p("Step 4")),
      column(11,
             downloadButton("sqlite", "Generate SQLite database"),
             hr()
      )
    )
  ),
  # Server ------------------------------------------------------------------------
  server = function(input, output) {
    source("config.R", encoding = "UTF-8")
    source("functions.R", encoding = "UTF-8")
    event2Behandling <- function(x) {
      # Translate event codes to Sötebasens strings for Behandling
      return(as.character(
        factor(x,
               levels = c(UNKNOWN, CAUGHT, MARKED, RECAPTURED, REMOVED),
               labels = c('', 'Utsatt', 'Märkt&utsatt',
                          'Återfångad&utsatt', 'Landad/avlivad/död')
        )
      )
      )
    }

    output$report <- downloadHandler(
      filename = function() {paste0(tools::file_path_sans_ext(input$file1$name),
                                    "-check.html")},
      content = function(file) {
        # Copy the report file to a temporary directory before processing it, in
        # case we don't have write permissions to the current working dir (which
        # can happen when deployed).
        tmpDir <- tempdir()
        tempReport <- file.path(tmpDir, "Check_data_shiny.Rmd")
        tempConfig <- file.path(tmpDir, "config.R")
        tempFunctions <- file.path(tmpDir, "functions.R")
        tempAllowed <- file.path(tmpDir, "allowed_species.txt")
        file.copy("Check_data_shiny.Rmd", tempReport, overwrite = TRUE)
        file.copy("config.R", tempConfig, overwrite = TRUE)
        file.copy("functions.R", tempFunctions, overwrite = TRUE)
        file.copy("allowed_species.txt", tempAllowed, overwrite = TRUE)
        # Set up parameters to pass to Rmd document
        params <- list(smoltreg = input$file1$datapath, origName = input$file1$name)
        # Knit the document, passing in the `params` list, and eval it in a
        # child of the global environment (this isolates the code in the document
        tmpDir <- tempdir()
        odir <- setwd(tmpDir)
        # from the code in this app).
        rmarkdown::render(tempReport, output_file = file,
                          params = params,
                          envir = new.env(parent = globalenv())
        )
      },
      contentType = "text/html"
    ) # End output$report
 
    output$XLSXfile <- downloadHandler(
      filename = function() {paste0(tools::file_path_sans_ext(input$file1$name),
                                    "_sötebasen.xlsx")},
      content = function(file) {
        # Generate Excel in 'file' here.
        tmpDir <- tempdir()
        odir <- setwd(tmpDir)
        metadata <- read_meta(input$file1$datapath)
        fishdata <- read_fish(input$file1$datapath,
                              dummy_tags = metadata$dummy_tags)
        start_year <- as.numeric(format(metadata$startdate, "%Y"))
        end_year <- as.numeric(format(metadata$enddate, "%Y"))
        
        # We define "Insamling" as one season for one trap.
        # Start and stop year (Årtal och Årtal2) are set to the same year.
        Insamling <- data.frame(InsamlingID = 1,
                                Vatten = metadata$river,
                                Årtal = start_year,
                                Årtal2 = start_year,
                                Metod = metadata$Metod,
                                Ansvarig = metadata$Ansvarig,
                                Syfte = metadata$Syfte,
                                Sekretess = "Nej",
                                Signatur = metadata$Signatur
        )
        # Also "Ansträngning" is one season for one trap..
        Ansträngning <- data.frame(AnsträngningID = 1,
                                   InsamlingID = 1,
                                   AnstrTyp = metadata$Metod,
                                   AnstrPlats = metadata$loc_name,
                                   AnstrDatumStart = metadata$startdate,
                                   AnstrDatumSlut = metadata$enddate,
                                   AnstrS99TM_N_1 = metadata$N_coord,
                                   AnstrS99TM_E_1 = metadata$E_coord,
                                   Märkning = metadata$Märkning,
                                   SignAnstr = metadata$Signatur
        )
        ###
        ## Assume that catch_time == "00:00" equals missing time and set time to NA.
        catch_time <- format(fishdata$date_time, "%H:%M")
        catch_time <- ifelse(catch_time == "00:00", NA, catch_time)
        Individ <- data.frame(#IndividID = 1:nrow(fishdata),
                              InsamlingId = 1,
                              AnsträngningID = 1,
                              FångstDatum = format(fishdata$date_time, "%Y-%m-%d"),
                              FångstTid = catch_time,
                              Art = fishdata$species,
                              Åldersprov = ifelse(is.na(fishdata$genid), 'Nej', 'Ja'),
                              Provkod = fishdata$genid,
                              Längd1 = fishdata$length,
                              Vikt1 = fishdata$weight,
                              Behandling = event2Behandling(fishdata$event),
                              Stadium = fishdata$smoltstat,
                              MärkeNr = as.character(fishdata$pittag),
                              Märkning = ifelse(is.na(fishdata$pittag),NA, metadata$Märkning),
                              SignIndivid = metadata$Signatur,
                              AnmIndivid = fishdata$comment
        )
        
        sheetnames <- c('Insamling', enc2utf8('Ansträngning'), 'Individ')
        l <- list(Insamling, Ansträngning, Individ)
        if (input$do_envdata) {
          envdata <- read_envdata(input$file1$datapath)
          Temperatur <- data.frame(InsamlingID = 1,
                                   MätDatum = envdata$date,
                                   Tempbotten = envdata$w_temp,
                                   Vattennivå = envdata$w_level,
                                   SignTemp = metadata$Signatur
                                   )
          sheetnames <- c(sheetnames, 'Temperatur')
          l <- c(l , list(Temperatur))
        }
        names(l) <- sheetnames
        writexl::write_xlsx(l, path = file)
        setwd(odir)
      },
      contentType = "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet"
    ) # End output$XLSXfile
    
    output$sqlite <- downloadHandler(
      filename = function() {paste0("smolt_trap_", tools::file_path_sans_ext(input$file1$name),
                                    ".sqlite")},
      content = function(file) {
        tmpDir <- tempdir()
        odir <- setwd(tmpDir)
        metadata <- read_meta(input$file1$datapath)
        fishdata <- read_fish(input$file1$datapath,
                              dummy_tags = metadata$dummy_tags)
        fishdata$date_time <- as.character(fishdata$date_time)
        save_to_sqlite(file, "fish", fishdata)
        if (input$do_envdata) {
          envdata <- read_envdata(input$file1$datapath)
          envdata$date <- as.character(envdata$date)
          save_to_sqlite(file, "env", envdata)
        }
        setwd(odir)
      },
      contentType = "application/x-sqlite3"
    )
  }
)
