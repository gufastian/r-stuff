library(lubridate)
library(googleAnalyticsR)
library(zoo)


# Authorize Google Analytics. This should launch a browser and require you to
# log in.
ga_auth()

gaIDs <- array(c(
  c("Italy","ga:34201946","gaid::-1","gaid::UeQ3xisNQeakaHo4If-Dtw","gaid::LKSa2gEwT1KZkoYYh6wq-Q"),
  c("France","ga:36170028","gaid::-1","gaid::Qs87f7MiRJ6RPvdSNGb0oA","gaid::ctQTrYRjRzCBFF1WfrltwQ"),
  c("Germany","ga:94913382","gaid::-1","gaid::Qs87f7MiRJ6RPvdSNGb0oA","gaid::ctQTrYRjRzCBFF1WfrltwQ"),
  c("Austria","ga:36167215","gaid::-1","gaid::Qs87f7MiRJ6RPvdSNGb0oA","gaid::ctQTrYRjRzCBFF1WfrltwQ"),
  c("Argentina","ga:36132576","gaid::-1","gaid::Qs87f7MiRJ6RPvdSNGb0oA",""),
  c("Brazil","ga:36254510","gaid::-1","gaid::Qs87f7MiRJ6RPvdSNGb0oA",""),
  c("Spain","ga:36252653","gaid::-1","gaid::Qs87f7MiRJ6RPvdSNGb0oA","gaid::ctQTrYRjRzCBFF1WfrltwQ"),
  c("Switzerland (Tot)","ga:136650673","gaid::-1","gaid::Qs87f7MiRJ6RPvdSNGb0oA",""),
  c("Switzerland (FR)","ga:37303706","gaid::-1","gaid::Qs87f7MiRJ6RPvdSNGb0oA",""),
  c("Switzerland (DE)","ga:37303515","gaid::-1","gaid::Qs87f7MiRJ6RPvdSNGb0oA",""),
  c("Denmark","ga:36402740","gaid::-1","gaid::Qs87f7MiRJ6RPvdSNGb0oA","",""),
  c("Sweden","ga:36480884","gaid::-1","gaid::Qs87f7MiRJ6RPvdSNGb0oA","",""),
  c("USA","ga:36328344","gaid::-1","gaid::Qs87f7MiRJ6RPvdSNGb0oA","",""),
  c("Norway","ga:36359424","gaid::-1","gaid::Qs87f7MiRJ6RPvdSNGb0oA","",""),
  c("Finland","ga:36406563","gaid::-1","gaid::Qs87f7MiRJ6RPvdSNGb0oA","",""),
  c("Netherlands","ga:37264959","gaid::-1","gaid::Qs87f7MiRJ6RPvdSNGb0oA","",""),
  c("Belgium","ga:37265847","gaid::-1","gaid::Qs87f7MiRJ6RPvdSNGb0oA","",""),
  c("Russia","ga:6295774","gaid::-1","gaid::Qs87f7MiRJ6RPvdSNGb0oA","",""),
  c("Uk","ga:6246863","gaid::-1","gaid::Qs87f7MiRJ6RPvdSNGb0oA","",""),
  c("Eu","ga:36516113","gaid::-1","gaid::Qs87f7MiRJ6RPvdSNGb0oA","",""),
  c("Pt","ga:36322927","gaid::-1","gaid::Qs87f7MiRJ6RPvdSNGb0oA","","")
  ), dim=c(6,21))

# --- COSTA MONTHS ---
startingDate <- dmy("01/06/2018")
endingDate <- dmy("31/05/2018")


while (as.yearmon(startingDate) != as.yearmon(Sys.Date())) {
  endingDate <- as.Date(format(startingDate, '%Y-%m-01')) - 1
  startingDate <- startingDate %m-% months(1)
  print(paste(startingDate , " - " , endingDate));
  currentID <- gaIDs[2,1]
  fileName <- "C:/Users/montino/Documents/Italy.csv"
  ga_data <- google_analytics_4(viewId = currentID,
                                date_range = c(startingDate, endingDate),
                                metrics = c("sessions","bounces","bouncerate","pageviewspersession","avgsessionduration","pageviews","transactions","transactionrevenue","sessionduration"),
                                dimensions = c("year","month","devicecategory"),
                                segments = segment_ga4("All Users", segment_id = gaIDs[3,1]),
                                anti_sample = TRUE)
  write.table(ga_data, file = fileName, sep = ",", append = TRUE, quote = FALSE,
              col.names = FALSE, row.names = FALSE)

  if (gaIDs[4,1] != "") {
    ga_data <- google_analytics_4(viewId = currentID,
                                   date_range = c(startingDate, endingDate),
                                   metrics = c("sessions","bounces","bouncerate","pageviewspersession","avgsessionduration","pageviews","transactions","transactionrevenue","sessionduration"),
                                   dimensions = c("year","month","devicecategory"),
                                   segments = segment_ga4(gaIDs[4,1], segment_id = gaIDs[4,1]),
                                   anti_sample = TRUE)
    write.table(ga_data, file = fileName, sep = ",", append = TRUE, quote = FALSE,
                col.names = FALSE, row.names = FALSE)

  }

  if (gaIDs[5,1] != "") {
    ga_data <- google_analytics_4(viewId = currentID,
                                   date_range = c(startingDate, endingDate),
                                   metrics = c("sessions","bounces","bouncerate","pageviewspersession","avgsessionduration","pageviews","transactions","transactionrevenue","sessionduration"),
                                   dimensions = c("year","month","devicecategory"),
                                   segments = segment_ga4(gaIDs[5,1], segment_id = gaIDs[5,1]),
                                   anti_sample = TRUE)

    write.table(ga_data, file = fileName, sep = ",", append = TRUE, quote = FALSE,
                col.names = FALSE, row.names = FALSE)

  }
  startingDate <- startingDate %m+% months(2)
}
