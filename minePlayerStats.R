minePlayerStatsFromBF4Stats <- function(playerData) {
  #
  # Mines player statistics from BF4stats.com's API
  #
  # playerData is a dataframe containing columns name and plat
  # name - contains the personaNames of the players to be mined 
  # plat - contains the platform's integer identifier (2-Xbox, 4-PS3)
  #
  # Returns a character vector equal length to the number
  # of rows in playerData. Each element contains either
  # the raw JSON output from the API or the string "failed"
  #
  # If something goes wrong in the middle of this function, a
  # temp file is maintained in the working directory with
  # the name "_players.tmp". This file is removed at the end
  # of the function call
  #
  bf4statsurl <- "http://api.bf4stats.com/api/playerInfo"
  opt <- "stats,extra,vehicleCategory,weaponCategory"
  player.json <- character(nrow(playerData))
  tmpfile <- "_players.tmp"
  for (i in 1:nrow(playerData)) {
    name <- gsub(" ", "+", playerData$name[i])
    plat <- ifelse(playerData$platform[i]==4, "ps3", "xbox")
    print(paste(i, "... fetching player", name, "on", plat))
    query <- paste0("?plat=", plat, "&name=", name, "&opt=", opt)
    player.json[i] <- tryCatch(
      readLines(paste0(bf4statsurl, query)),
      error=function(e) "failed")
    write(player.json[i], file=tmpfile, append=TRUE)
  }
  file.remove(tmpfile)
  player.json
}