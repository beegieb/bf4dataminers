source("mineBattleReports.R")

reportURLs <- mineReportURLsFromBF4DBPlayers(bf4db.id)
battleReports <- reportURLsToDF(reportURLs)
battleReports <- mineBattleReportJSONfromBL(battleReports)