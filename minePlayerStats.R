minePlayerStatsFromBF4Stats <- function(playerData) {
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