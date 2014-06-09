library(rjson)

getJSONData <- function(jsonStrings){
  lapply(jsonStrings, fromJSON)
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

getReportMaps <- function(jsonData) {
  sapply(jsonData, function(jd) jd$gameServer$map)
}

getReportWinner <- function(jsonData) {
  sapply(jsonData, function(jd) {
    if (jd$teams$"1"$isWinner) { 1 } 
    else if (jd$teams$"2"$isWinner) { 2 } 
    else { 0 }
  })
}

getUniqueUsers <- function(jsonData) {
  un <- sapply(jsonData, function(jd) { 
    paste(sapply(jd$players, function(p) {
      paste(p$persona$personaId, p$persona$personaName, sep=",")
    }), jd$platform, sep=",")
  })
  p <- unique(unlist(un))
  p <- strsplit(p, ",")
  p <- p[sapply(p, length)==3]
  lp <- length(p)
  p <- data.frame(matrix(unlist(p), nrow=lp, byrow=T))
  colnames(p) <- c("id", "name", "platform")
  p <- p[!duplicated(p$id),]
  rownames(p) <- p$id
  p
}

getReportPlayers <- function(jsonData) {
  lapply(jsonData, function(jd) {
    playerList <- list(team1=list(), team2=list())
    for (player in jd$players) {
      if (!player$dnf) {
        if (player$team == 1) { 
          playerList$team1 <- c(playerList$team1, player)
        } else {
          playerList$team2 <- c(playerList$team2, player)
        }
      }
    }
    playerList
  })
}

createReportDataSet <- function(reportsDF) {
  rjs <- getJSONData(as.character(reportsDF$report.json))
  
}