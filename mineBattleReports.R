library(stringr)
library(rjson)
source("ffReportMiningUsers.R")

readBF4DBPlayerBattleReports <- function(playerID) {
  #
  # Takes a vector of bf4db player ids and returns the raw html
  # from each playerID's bf4db battlereports page
  #
  # This function assumes all playerIDs are valid, and no validation is performed
  # on the incoming HTML data
  #
  bf4dburl.pre <- "http://bf4db.com/players"
  bf4dburl.suf <- "battlereports"
  paste(
    readLines(
      paste(bf4dburl.pre, playerID, bf4dburl.suf, sep="/")
      ), 
    collapse="\n")
}

extractBattleReportURLsFromHTML <- function(brHTMLdata) {
  #
  # Takes a vector of bf4db battlereport html data and
  # returns a vector of unique platformid/battlereportid 
  # strings 
  #
  urlPathPattern <- "/bf4/battlereport/show/[24]/[0-9]+"
  reportURLs <- unlist(str_extract_all(brHTMLdata, urlPathPattern))
  unique(unlist(str_extract(reportURLs, "[24]/[0-9]+")))
}

mineReportURLsFromBF4DBPlayers <- function(playerIDs) {
  #
  # takes a vector of bf4db player ids and returns
  # the unique set of battlereport platformid/battlereportid
  # strings mined from bf4db's battlereport page of all players
  #
  brHTMLdata <- sapply(playerIDs, readBF4DBPlayerBattleReports)
  extractBattleReportURLsFromHTML(brHTMLdata)
}

reportURLsToDF <- function(reportURLs) {
  #
  # convert a vector of platformid/battlereportid strings to
  # a data frame with columns report.id, platform.id
  #
  ssReportURLs <- strsplit(reportURLs, "/")
  platform.id <- sapply(ssReportURLs, function(s) s[1])
  report.id <- sapply(ssReportURLs, function(s) s[2])
  data.frame(report.id = report.id, platform.id = platform.id)
}

getBattleReportJSON <- function(report.id, platform.id) {
  #
  # takes a battlereport id and a platform id and returns
  # the JSON output from battlelog's battlereport API
  #
  # this function assumes report.id's are valid
  # valid platform.ids are:
  #   2 - Xbox 360
  #   4 - PS3 
  #
  bl.url <- "http://battlelog.battlefield.com"
  brAPI.path <- "bf4/battlereport/loadgeneralreport"
  paste(
    readLines(
      paste(bl.url, brAPI.path, report.id, platform.id, sep="/")
      ),
    collapse="\n")  
}

mineBattleReportJSONfromBL <- function(battlereport.df, verbose=FALSE) {
  #
  # takes a battlereport dataframe and extracts the raw battlereport 
  # JSON from battlelog's API
  # 
  # In order to minimize the chances of getting locked out from the 
  # battlelog API this function will sleep for 1.5s after every request 
  # to stay under the 20request/15s rate limit
  #
  # Can set verbose=TRUE to print the iteration and battlereport id
  #
  tmpfile <- "br_JSONminer_tmp"
  n <- nrow(battlereport.df)
  report.json <- character(n)
  for (i in 1:n) {
    r <- battlereport.df[i,]
    
    if (verbose) {
      print(paste("Collecting report", i, "with id", r$report.id))
    }
    
    report.json[i] <- getBattleReportJSON(r$report.id, r$platform.id)
    Sys.sleep(1.5)
    
    if (i %% 100 == 0) {
      battlereport.df$report.json <- report.json
      save(battlereport.df, file=tmpfile)
    }
  }
  
  if (file.exists(tmpfile)) {
    file.remove(tmpfile)
  }
  
  battlereport.df$report.json <- report.json
  battlereport.df
}

getMapMode <- function(jsonStrings) {
  mapModes <- integer(length(jsonStrings))
  for (i in seq_along(jsonStrings)) {
    jd <- fromJSON(jsonStrings[i])
    mapModes[i] <- jd$gameServer$mapMode
  }
  mapModes
}

returnConquestReports <- function(reportsDF) {
  mm <- getMapMode(reportsDF$report.json)
  reportsDF[mm==1,]
}

getJSONData <- function(jsonStrings){
  lapply(jsonStrings, fromJSON)
}

getUniqueUserNames <- function(jsonData) {
  un <- sapply(jsonData, function(jd) {
          sapply(jd$players, function(p) {
            p$persona$personaName
          })
        })
  unlist(un)
}

createReportDataSet <- function(reportsDF) {
  rjs <- getJSONData(as.character(reportsDF$report.json))
  
}