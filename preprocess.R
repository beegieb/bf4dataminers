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
    l1 <- 0
    l2 <- 0
    for (player in jd$players) {
      if (player$team == 1) { 
        playerList$team1[[l1+1]] <- player
        l1 <- l1 + 1
      } else {
        playerList$team2[[l2+1]] <- player
        l2 <- l2 + 1
      }
    }
    playerList
  })
}

playerPersonaId <- function(player) {
  player$personaId
}

playerDNF <- function(player) {
  player$dnf
}

getTeamAvgKDR <- function(reportPlayers, playerData) {
  dnf1 <- sapply(reportPlayers$team1, playerDNF)
  dnf2 <- sapply(reportPlayers$team2, playerDNF)
  pid1 <- sapply(reportPlayers$team1, playerPersonaId)
  pid2 <- sapply(reportPlayers$team2, playerPersonaId)
  kdr1 <- playerData[pid1,"kdr"]
  kdr2 <- playerData[pid2,"kdr"]
  team1TotKDR <- (sum(kdr1[!dnf1], na.rm=T) + 0.5*sum(kdr1[dnf1], na.rm=T))
  team2TotKDR <- (sum(kdr2[!dnf2], na.rm=T) + 0.5*sum(kdr2[dnf2], na.rm=T))
  c(team1TotKDR, team2TotKDR) / (12 + 0.5*c(sum(dnf1), sum(dnf2)))
}

createReportDataSet <- function(reportsDF) {
  rjs <- getJSONData(as.character(reportsDF$report.json))
  
}

playerJSONtoDF <- function(playerJSON) {
  playerJSON <- gsub("null", "0", playerJSON)
  pj <- fromJSON(playerJSON)
  player <- pj$player
  stats <- pj$stats
  scores <- stats$scores
  kits <- stats$kits
  extra <- stats$extra
  weap <- pj$weaponCategory
  vehi <- pj$vehicleCategory
  data.frame(
    id = player$id,
    name = player$name,
    plat = player$plat,
    rank = stats$rank,
    skill = stats$skill,
    kdr = extra$kdr,
    wlr = extra$wlr,
    spm = extra$spm,
    gspm = extra$gspm,
    kpm = extra$kpm,
    sfpm = extra$sfpm,
    hkp = extra$hkp,
    khp = extra$khp,
    accuracy = extra$accuracy,
    timePlayed = stats$timePlayed,
    kills = stats$kills,
    deaths = stats$deaths,
    headshots = stats$headshots,
    shotsFired = stats$shotsFired,
    shotsHit = stats$shotsHit,
    suppressionAssists = stats$suppressionAssists,
    avengerKills = stats$avengerKills,
    saviorKills = stats$saviorKills,
    nemesisKills = stats$nemesisKills,
    numRounds = stats$numRounds,
    roundsFinished = extra$roundsFinished,
    numWins = stats$numWins,
    numLosses = stats$numLosses,
    killStreakBonus = stats$killStreakBonus,
    nemesisStreak = stats$nemesisStreak,
    resupplies = stats$resupplies,
    repairs = stats$repairs,
    heals = stats$heals,
    revives = stats$revives,
    longestHeadshot = stats$longestHeadshot,
    flagDefend = stats$flagDefend,
    flagCaptures = stats$flagCaptures,
    killAssists = stats$killAssists,
    vehicleDestroyed = stats$vehiclesDestroyed,
    vehicleDamage = stats$vehicleDamage,
    dogtagsTaken = stats$dogtagsTaken,
    score.total = scores$score,
    score.conquest = stats$mode[[1]]$score,
    score.award = scores$award,
    score.bonus = scores$bonus,
    score.unlock = scores$unlock,
    score.vehicle = scores$vehicle,
    score.team = scores$team,
    score.squad = scores$squad,
    score.general = scores$general,
    score.rank = scores$rankScore,
    score.combat = scores$combatScore,
    score.assault = kits$assault$score,
    score.engineer = kits$engineer$score,
    score.support = kits$support$score,
    score.recon = kits$recon$score,
    score.commander = kits$commander$score,
    time.assault = kits$assault$time,
    time.engineer = kits$engineer$time,
    time.support = kits$support$time,
    time.recon = kits$recon$time,
    time.commander = kits$commander$time,
    time.vehicle = extra$vehicleTime,
    time.vehicle.precent = extra$vehTimeP,
    time.weapon = extra$weaponTime,
    time.weapon.percent = extra$weaTimeP,
    stars.assault = kits$assault$stars,
    stars.engineer = kits$engineer$stars,
    stars.support = kits$support$stars,
    stars.recon = kits$recon$stars,
    stars.commander = kits$commander$stars,
    spm.assault = kits$assault$spm,
    spm.engineer = kits$engineer$spm,
    spm.support = kits$support$spm,
    spm.recon = kits$recon$spm,
    spm.commander = kits$commander$spm,
    kills.weapon = extra$weaKills,
    kills.weapon.percent = extra$weaKillsP,
    kills.vehicle = extra$vehKills,
    kills.vehicle.percent = extra$vehKillsP,
    kpm.weapon = extra$weaKpm,
    kpm.vehicle = extra$vehKpm,
    ribbons = extra$ribbons,
    medals = extra$medals
  )  
}

# This is not used because of bugs in extracting stats
# from weaponCategory and vehicleCategory
# playerJSONtoDF <- function(playerJSON) {
#   playerJSON <- gsub("null", "0", playerJSON)
#   pj <- fromJSON(playerJSON)
#   player <- pj$player
#   stats <- pj$stats
#   scores <- stats$scores
#   kits <- stats$kits
#   extra <- stats$extra
#   weap <- pj$weaponCategory
#   vehi <- pj$vehicleCategory
#   data.frame(
#     id = player$id,
#     name = player$name,
#     plat = player$plat,
#     rank = stats$rank,
#     skill = stats$skill,
#     kdr = extra$kdr,
#     wlr = extra$wlr,
#     spm = extra$spm,
#     gspm = extra$gspm,
#     kpm = extra$kpm,
#     sfpm = extra$sfpm,
#     hkp = extra$hkp,
#     khp = extra$khp,
#     accuracy = extra$accuracy,
#     timePlayed = stats$timePlayed,
#     kills = stats$kills,
#     deaths = stats$deaths,
#     headshots = stats$headshots,
#     shotsFired = stats$shotsFired,
#     shotsHit = stats$shotsHit,
#     suppressionAssists = stats$suppressionAssists,
#     avengerKills = stats$avengerKills,
#     saviorKills = stats$saviorKills,
#     nemesisKills = stats$nemesisKills,
#     numRounds = stats$numRounds,
#     roundsFinished = extra$roundsFinished,
#     numWins = stats$numWins,
#     numLosses = stats$numLosses,
#     killStreakBonus = stats$killStreakBonus,
#     nemesisStreak = stats$nemesisStreak,
#     resupplies = stats$resupplies,
#     repairs = stats$repairs,
#     heals = stats$heals,
#     revives = stats$revives,
#     longestHeadshot = stats$longestHeadshot,
#     flagDefend = stats$flagDefend,
#     flagCaptures = stats$flagCaptures,
#     killAssists = stats$killAssists,
#     vehicleDestroyed = stats$vehiclesDestroyed,
#     vehicleDamage = stats$vehicleDamage,
#     dogtagsTaken = stats$dogtagsTaken,
#     score.total = scores$score,
#     score.conquest = stats$mode[[1]]$score,
#     score.award = scores$award,
#     score.bonus = scores$bonus,
#     score.unlock = scores$unlock,
#     score.vehicle = scores$vehicle,
#     score.team = scores$team,
#     score.squad = scores$squad,
#     score.general = scores$general,
#     score.rank = scores$rankScore,
#     score.combat = scores$combatScore,
#     score.assault = kits$assault$score,
#     score.engineer = kits$engineer$score,
#     score.support = kits$support$score,
#     score.recon = kits$recon$score,
#     score.commander = kits$commander$score,
#     score.scout.heli = vehi[[11]]$stat$score,
#     score.attack.heli = vehi[[12]]$stat$score,
#     score.attack.boat = vehi[[15]]$stat$score,
#     score.tank = vehi[[10]]$stat$score,
#     score.ifv = vehi[[1]]$stat$score,
#     score.maa = vehi[[2]]$stat$score,
#     score.attack.jet = vehi[[4]]$stat$score,
#     score.stealth.jet = vehi[[3]]$stat$score,
#     time.assault = kits$assault$time,
#     time.engineer = kits$engineer$time,
#     time.support = kits$support$time,
#     time.recon = kits$recon$time,
#     time.commander = kits$commander$time,
#     time.vehicle = extra$vehicleTime,
#     time.vehicle.precent = extra$vehTimeP,
#     time.weapon = extra$weaponTime,
#     time.weapon.percent = extra$weaTimeP,
#     time.gadget = weap[[1]]$stat$time,
#     time.sniper = weap[[2]]$stat$time,
#     time.pdw = weap[[3]]$stat$time,
#     time.carbine = weap[[4]]$stat$time,
#     time.pickup = weap[[5]]$stat$time,
#     time.lmg = weap[[6]]$stat$time,
#     time.grenade = weap[[7]]$stat$time,
#     time.dmr = weap[[8]]$stat$time,
#     time.ar = weap[[9]]$stat$time,
#     time.shotgun = weap[[11]]$stat$time,
#     time.sidearm = weap[[12]]$stat$time,
#     time.scout.heli = vehi[[11]]$stat$time,
#     time.attack.heli = vehi[[12]]$stat$time,
#     time.attack.boat = vehi[[15]]$stat$time,
#     time.tank = vehi[[10]]$stat$time,
#     time.ifv = vehi[[1]]$stat$time,
#     time.maa = vehi[[2]]$stat$time,
#     time.attack.jet = vehi[[4]]$stat$time,
#     time.stealth.jet = vehi[[3]]$stat$time,
#     stars.assault = kits$assault$stars,
#     stars.engineer = kits$engineer$stars,
#     stars.support = kits$support$stars,
#     stars.recon = kits$recon$stars,
#     stars.commander = kits$commander$stars,
#     spm.assault = kits$assault$spm,
#     spm.engineer = kits$engineer$spm,
#     spm.support = kits$support$spm,
#     spm.recon = kits$recon$spm,
#     spm.commander = kits$commander$spm,
#     spm.sniper = weap[[2]]$extra$spm,
#     spm.pdw = weap[[3]]$extra$spm,
#     spm.carbine = weap[[4]]$extra$spm,
#     spm.pickup = weap[[5]]$extra$spm,
#     spm.lmg = weap[[6]]$extra$spm,
#     spm.grenade = weap[[7]]$extra$spm,
#     spm.dmr = weap[[8]]$extra$spm,
#     spm.ar = weap[[9]]$extra$spm,
#     spm.shotgun = weap[[11]]$extra$spm,
#     spm.sidearm = weap[[12]]$extra$spm,
#     spm.scout.heli = vehi[[11]]$extra$spm,
#     spm.attack.heli = vehi[[12]]$extra$spm,
#     spm.attack.boat = vehi[[15]]$extra$spm,
#     spm.tank = vehi[[10]]$extra$spm,
#     spm.ifv = vehi[[1]]$extra$spm,
#     spm.maa = vehi[[2]]$extra$spm,
#     spm.attack.jet = vehi[[4]]$extra$spm,
#     spm.stealth.jet = vehi[[3]]$extra$spm,
#     kills.weapon = extra$weaKills,
#     kills.weapon.percent = extra$weaKillsP,
#     kills.vehicle = extra$vehKills,
#     kills.vehicle.percent = extra$vehKillsP,
#     kills.gadget = weap[[1]]$stat$kills,
#     kills.sniper = weap[[2]]$stat$kills,
#     kills.pdw = weap[[3]]$stat$kills,
#     kills.carbine = weap[[4]]$stat$kills,
#     kills.pickup = weap[[5]]$stat$kills,
#     kills.lmg = weap[[6]]$stat$kills,
#     kills.grenade = weap[[7]]$stat$kills,
#     kills.dmr = weap[[8]]$stat$kills,
#     kills.ar = weap[[9]]$stat$kills,
#     kills.knife = weap[[10]]$stat$kills,
#     kills.shotgun = weap[[11]]$stat$kills,
#     kills.sidearm = weap[[12]]$stat$kills,
#     kills.scout.heli = vehi[[11]]$stat$kills,
#     kills.attack.heli = vehi[[12]]$stat$kills,
#     kills.attack.boat = vehi[[15]]$stat$kills,
#     kills.tank = vehi[[10]]$stat$kills,
#     kills.ifv = vehi[[1]]$stat$kills,
#     kills.maa = vehi[[2]]$stat$kills,
#     kills.attack.jet = vehi[[4]]$stat$kills,
#     kills.stealth.jet = vehi[[3]]$stat$kills,
#     kpm.weapon = extra$weaKpm,
#     kpm.vehicle = extra$vehKpm,
#     kpm.gadget = weap[[1]]$extra$kpm,
#     kpm.sniper = weap[[2]]$extra$kpm,
#     kpm.pdw = weap[[3]]$extra$kpm,
#     kpm.carbine = weap[[4]]$extra$kpm,
#     kpm.pickup = weap[[5]]$extra$kpm,
#     kpm.lmg = weap[[6]]$extra$kpm,
#     kpm.grenade = weap[[7]]$extra$kpm,
#     kpm.dmr = weap[[8]]$extra$kpm,
#     kpm.ar = weap[[9]]$extra$kpm,
#     kpm.shotgun = weap[[11]]$extra$kpm,
#     kpm.sidearm = weap[[12]]$extra$kpm,
#     kpm.scout.heli = vehi[[11]]$extra$kpm,
#     kpm.attack.heli = vehi[[12]]$extra$kpm,
#     kpm.attack.boat = vehi[[15]]$extra$kpm,
#     kpm.tank = vehi[[10]]$extra$kpm,
#     kpm.ifv = vehi[[1]]$extra$kpm,
#     kpm.maa = vehi[[2]]$extra$kpm,
#     kpm.attack.jet = vehi[[4]]$extra$kpm,
#     kpm.stealth.jet = vehi[[3]]$extra$kpm,
#     ribbons = extra$ribbons,
#     medals = extra$medals,
#     shots.gadget = weap[[1]]$stat$shots,
#     shots.sniper = weap[[2]]$stat$shots,
#     shots.pdw = weap[[3]]$stat$shots,
#     shots.carbine = weap[[4]]$stat$shots,
#     shots.pickup = weap[[5]]$stat$shots,
#     shots.lmg = weap[[6]]$stat$shots,
#     shots.grenade = weap[[7]]$stat$shots,
#     shots.dmr = weap[[8]]$stat$shots,
#     shots.ar = weap[[9]]$stat$shots,
#     shots.shotgun = weap[[11]]$stat$shots,
#     shots.sidearm = weap[[12]]$stat$shots,
#     hits.gadget = weap[[1]]$stat$hits,
#     hits.sniper = weap[[2]]$stat$hits,
#     hits.pdw = weap[[3]]$stat$hits,
#     hits.carbine = weap[[4]]$stat$hits,
#     hits.pickup = weap[[5]]$stat$hits,
#     hits.lmg = weap[[6]]$stat$hits,
#     hits.grenade = weap[[7]]$stat$hits,
#     hits.dmr = weap[[8]]$stat$hits,
#     hits.ar = weap[[9]]$stat$hits,
#     hits.shotgun = weap[[11]]$stat$hits,
#     hits.sidearm = weap[[12]]$stat$hits,
#     headshot.sniper = weap[[2]]$stat$hs,
#     headshot.pdw = weap[[3]]$stat$hs,
#     headshot.carbine = weap[[4]]$stat$hs,
#     headshot.pickup = weap[[5]]$stat$hs,
#     headshot.lmg = weap[[6]]$stat$hs,
#     headshot.dmr = weap[[8]]$stat$hs,
#     headshot.ar = weap[[9]]$stat$hs,
#     headshot.shotgun = weap[[11]]$stat$hs,
#     headshot.sidearm = weap[[12]]$stat$hs,
#     sfpm.gadget = weap[[1]]$extra$sfpm,
#     sfpm.sniper = weap[[2]]$extra$sfpm,
#     sfpm.pdw = weap[[3]]$extra$sfpm,
#     sfpm.carbine = weap[[4]]$extra$sfpm,
#     sfpm.pickup = weap[[5]]$extra$sfpm,
#     sfpm.lmg = weap[[6]]$extra$sfpm,
#     sfpm.grenade = weap[[7]]$extra$sfpm,
#     sfpm.dmr = weap[[8]]$extra$sfpm,
#     sfpm.ar = weap[[9]]$extra$sfpm,
#     sfpm.shotgun = weap[[11]]$extra$sfpm,
#     sfpm.sidearm = weap[[12]]$extra$sfpm,
#     hkp.gadget = weap[[1]]$extra$hkp,
#     hkp.sniper = weap[[2]]$extra$hkp,
#     hkp.pdw = weap[[3]]$extra$hkp,
#     hkp.carbine = weap[[4]]$extra$hkp,
#     hkp.pickup = weap[[5]]$extra$hkp,
#     hkp.lmg = weap[[6]]$extra$hkp,
#     hkp.grenade = weap[[7]]$extra$hkp,
#     hkp.dmr = weap[[8]]$extra$hkp,
#     hkp.ar = weap[[9]]$extra$hkp,
#     hkp.shotgun = weap[[11]]$extra$hkp,
#     hkp.sidearm = weap[[12]]$extra$hkp,
#     accuracy.gadget = weap[[1]]$extra$accuracy,
#     accuracy.sniper = weap[[2]]$extra$accuracy,
#     accuracy.pdw = weap[[3]]$extra$accuracy,
#     accuracy.carbine = weap[[4]]$extra$accuracy,
#     accuracy.pickup = weap[[5]]$extra$accuracy,
#     accuracy.lmg = weap[[6]]$extra$accuracy,
#     accuracy.grenade = weap[[7]]$extra$accuracy,
#     accuracy.dmr = weap[[8]]$extra$accuracy,
#     accuracy.ar = weap[[9]]$extra$accuracy,
#     accuracy.shotgun = weap[[11]]$extra$accuracy,
#     accuracy.sidearm = weap[[12]]$extra$accuracy
#     )  
# }