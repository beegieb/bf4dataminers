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
  urlPrefix <- "http://bf4db.com/players"
  urlSuffix <- "battlereports"
  paste(readLines(paste(urlPrefix, playerID, urlSuffix, sep="/")), collapse="\n")
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