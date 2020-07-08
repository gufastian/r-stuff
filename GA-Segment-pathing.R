# setting the stage
library(googleAnalyticsR)
library(ggplot2)
library(stringr)

## managing oAuth()
ga_auth(new_user = TRUE)

meta <- google_analytics_meta()
# get list of accounts
account_list <- ga_account_list()


id <- "ga:121373285"

date <- c("2017-07-20", "2017-08-05")

content_group <- google_analytics_4(id, date_range = date, metrics = "ga:pageviews", dimension = "ga:contentGroup1",filter = "ga:contentGroup1!=(not set)")
content_group <- content_group$contentGroup1
content_group[length(content_group)+1] <- ""

firstComb <- expand.grid(content_group,content_group,content_group)

isCombinationValid <- function(row,df) {
  isValid <- TRUE
  prevVal <- "LAST"
  for (cn in rev(colnames(df))) {
    if (prevVal!="LAST") {
      if (row[cn] == "" & prevVal != ""){
        isValid <- FALSE
        break
      }
    }
    prevVal <- row[cn]
  }
  isValid
}

firstComb$values <- as.numeric(apply(firstComb,1, function(x) {
  if (isCombinationValid(x,firstComb)) {
    tmp <- ""
    firstEmpty <- FALSE
    counter <- 0
    for (cn in colnames(firstComb)) {
      if (tmp == "") {
        tmp <- paste("sessions::sequence::ga:landingContentGroup1==",x[cn],sep="")
        if (x[cn] == "") firstEmpty <- TRUE
      } else if (x[cn]!="") {
        tmp <- paste(tmp,";->ga:contentGroup1==",x[cn],sep = "")
      }
      counter <- counter+1
    }
    tmp <- paste("sessions::condition::ga:pageviews==",counter,";",sep="",tmp)
    if (!firstEmpty) {
      return(google_analytics_4(id, date_range = date, metrics = "ga:sessions", segments=segment_ga4("Sequence", segment_id = tmp))$sessions)
    } else {
      return(NA)
    }
  } else {
    return(NA)
  }
}))

# CLEANING THE OUTPUT
output <- subset(firstComb, values!="NA")
rownames(output) <- seq(length=nrow(output))


write.table(as.matrix(output), file="C:/Users/sebastiano.montino/Documents/exp.csv",sep="\t", row.names = FALSE)

#Alluvial R
library("alluvial")
alluvial(output,freq=as.numeric(output$values))




