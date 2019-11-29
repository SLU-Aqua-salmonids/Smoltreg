---
title: "Manual Smoltreg2Sotebasen"
author: "Anders Kagervall"
date: "15 oktober 2018"
output:
  word_document: default
  html_document: default
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

## Initial preparation

Make copies of the delivered data files for a smolt trap. You will find the raw (as delivered) data files on the central file server \\\\storage-dh.slu.se\\restricted$\\Lax\Data\\*YYYY*\\Rådata\\IndexÄlvar\\*RIVER* (where *YYYY* and *RIVER* should be changed). You should have at least an Excel file with fishdata and an file from the temp-pressure logger with the file extension *.hobo*.

You must then compile one single Excel that contains several tabs with specific tabs for fishdata, environmental data (temp and water level) and metadata.

### Initial check and additional metadata

1. Open your copy of the Excel file and make an initial check of the tab *Fiskdata*. The tab *Fiskdata* should be a list of fish observations.
2. Do a quick scan of the data to find obvious error. For example check that the same date format, the prefered data format is *YY-MM-DD HH:MM*. Obvious errors should be fixed before you try to run the data checking script.
3. Switch to tab *Metadata* and check that it is filled by the trap personel, otherwise fill it yourself (*Dummy tag:* is optional).
4. Create a new tab named *Metadata2*. This tab contains fields that are required for the Sötebasen-import. Needed fields are: Metod, Beställare, Ansvarig, Syfte, Sekretess,  Märkning and Signatur. See an example from previous year for an example.


### Adding loggerdata

1. Use HOBOware to convert the hobo data files to excel-files. One file from logger in the water and one from logger in the air.
2. Add each file to a tab in the file. Name the tabs *Envlogger_water* and *Envlogger_land*. 

### Running Smoltreg2sotebasen

The script to run sanity checks on the *Smoltreg*-file can be run on the web and no software need to be installed on your computer (except a web-browser). Go to https://kagervall.shinyapps.io/smoltreg2sotebasen/ to run the script.

The normal procedure is to iterate steps 1 and 2 and when all errors are fixed run step 3 to generate a zip-file (containing several csv-files) that can be delivered to Sötebasen for import. Step 4 is optional and generates a *SQLite3* database that might be useful for your analyses.

