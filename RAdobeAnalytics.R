#More information in links below
#pdf - https://cran.r-project.org/web/packages/slackr/slackr.pdf
#github - https://github.com/hrbrmstr/slackr

#Note on slack:
#there is a setup needing to take place before you run the code below - you NEED to either declare this function below at beginning of 
#this project and run it every time - or you need to store it in .slackr in your home directory / c:/users/username folder under ."slackr"
#and in that folder you need to store this information (per the instructions in the github repo and pdf above):

#go to setup section in github repo link above

#in the .slackr file store just this text and save the file - without the # symbols of course!

#api_token: YOUR_FULL_API_TOKEN
#channel: hotjarfeedback
#username: slackr
#incoming_webhook_url: https://hooks.slack.com/services/XXXXX/XXXXX/XXXXX

#or install dev version  (I recommend it but leaving it optional)
#install.packages("devtools")
#devtools::install_github("hrbrmstr/slackr")

#I dont know if you need any other packages like curl, httr, etc.  It will throw some errors and you'll have to google it if you do need
#other packages, so just a forewarning.  Enjoy!

#install.packages("slackr")
# install.packages("RSiteCatalyst")
# install.packages("ggploto2")
#install.packages("forecast")
#install.packages("textshape")

library("slackr")

slackr_setup(channel = 'hotjarfeedback',  username = 'montino', incoming_webhook_url = 'https://hooks.slack.com/services/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', api_token = 'xoxp-4018123899-325850487108-596032423120-c0d2fc4eedbe558209f3261e127679a9', config_file = '~/.slackr', echo = FALSE)
library("RSiteCatalyst")

#I have my API keys stored in my .Renviron (In a file called .Renviron under my C:/Users/Username folder as that's where it typically lives in windows - or Home directory for your user in Linux/Ubuntu fyi)

#API Authentication -using last 35 days w/ variables as well - you can just put your web client credentials instead of ADOBE_KEY and ADOBE_SECRET
#But I recommend storing in .Renviron to save you time in the future

SCAuth("montino@costa.it:Carnival Brands", "1a838b4b6a00dbd58b3349c229e802dc")
dateFrom <- Sys.Date()-35
dateTo <- Sys.Date()-1
dateFromFC <- "2018-05-15"
dateToFC <- Sys.Date()-1

#API function call

visits_w_forecast <- QueueOvertime("vrs_carniv3_costacrociereproduct_0", date.from = dateFrom, date.to= dateTo, metrics = "visits", date.granularity ="day", anomaly.detection = TRUE )

#Plot data using ggplot2
library("ggplot2")

#Combine year/month/day together into POSIX
visits_w_forecast$date <- ISOdate(visits_w_forecast$year, visits_w_forecast$month, visits_w_forecast$day)

#Convert columns to numeric
visits_w_forecast$visits <- as.numeric(visits_w_forecast$visits)
visits_w_forecast$upperBound.visits <- as.numeric(visits_w_forecast$upperBound.visits)
visits_w_forecast$lowerBound.visits <- as.numeric(visits_w_forecast$lowerBound.visits)

#Calculate points crossing UCL or LCL
visits_w_forecast$outliers <- 
  ifelse(visits_w_forecast$visits > visits_w_forecast$upperBound.visits, visits_w_forecast$visits,
         ifelse(visits_w_forecast$visits < visits_w_forecast$lowerBound.visits, visits_w_forecast$visits, NA))

#Add LCL and UCL labels
LCL <- vector(mode = "character", nrow(visits_w_forecast))
LCL[nrow(visits_w_forecast)] <- "LCL"
UCL <- vector(mode = "character", nrow(visits_w_forecast))
UCL[nrow(visits_w_forecast)] <- "UCL"
visits_w_forecast <- cbind(visits_w_forecast, LCL)
visits_w_forecast <- cbind(visits_w_forecast, UCL)

#Create ggplot with actual, UCL, LCL, outliers - make sure to leave \n after your title in ggtitle function
ggplot(visits_w_forecast, aes(date)) +
  theme_bw(base_family="Garamond") + 
  theme(text = element_text(size=20)) + 
  ggtitle("Visits for www.costacrociere.it\n") + 
  geom_smooth(aes(y = visits), method= "auto") + 
  geom_line(aes(y = visits), colour = "grey40") + 
  geom_point(aes(y = visits), colour = "grey40", size=3) +
  geom_point(aes(y = outliers), colour = "red", size=4) + 
  geom_line(aes(y = visits_w_forecast$upperBound.visits), colour = "green4", linetype = "dashed") + 
  geom_line(aes(y = visits_w_forecast$lowerBound.visits), colour = "green4", linetype = "dashed") +
  xlab("\nDate\n\n Upper/Lower Control Limits: Adobe Analytics") + 
  
  #change visits to whatever metric you are pulling or just ctrl+f the whole thing and replace "visits" with metric used in api 
  #call to fastrack setup for your use case
  
  ylab("Visits\n") +
  geom_text(aes(label=UCL, family = "Garamond"), y = visits_w_forecast$upperBound.visits, size=4.5, hjust = -.1) +
  geom_text(aes(label=LCL, family = "Garamond"), y = visits_w_forecast$lowerBound.visits, size=4.5, hjust = -.1)


ggslackr(plot = last_plot(), channel = "hotjarfeedback")
text_slackr("Visits Trend w/ Anomaly Detection for www.costacrociere.it", channel = "hotjarfeedback")


#optional - to add title and create a report to reference if an anomaly is detected
#text_slackr("Click this link to go to Report: <insert_custom_url_link_to_report_here>", channel = "#same_channel_you_declared_in_slack_setup_at_top_of_this_script")


#For Prophet Forecasting/Predicting Future Visits
#install.packages("prophet")
library("prophet")
library("forecast")


visits_forecast <- QueueOvertime("vrs_carniv3_costacrociereproduct_0", date.from = dateFromFC, date.to= dateToFC, metrics = "visits", date.granularity ="day")
#convert each dataframe to the variable name data
data <- visits_forecast


#qplot(date, visits, data = data)
ds <- data$datetime
y <- data$visits
df <- data.frame(ds,y)

m <- prophet(df, daily.seasonality = TRUE) 
#View(m)
#make_future_dataframe(m, periods = 35)

future <- make_future_dataframe(m, periods = 60)
#tail(future)

forecast <- predict(m, future)
# library("textshape")
# inverse_forecast <- forecast
# inverse_forecast <- column_to_rownames(inverse_forecast, var = "ds")
# inverse_forecast$yhat_untransformed = InvBoxCox(forecast$yhat, lam)

#tail(forecast[c('ds', 'yhat', 'yhat_lower', 'yhat_upper')])
df <- prophet:::df_for_plotting(m, forecast)
gg <-ggplot(df, aes(x = ds, y = y)) + labs(x = "ds", y = "y")
gg <- gg + geom_ribbon(ggplot2::aes(ymin = yhat_lower, 
                                    ymax = yhat_upper), alpha = 0.2, fill = "#0072B2", 
                       na.rm = TRUE)
## replace first geom_point() with geom_line() in next line ...
gg <- gg + geom_line( color = "#0072B2",na.rm = TRUE) + geom_line(aes(y = yhat), 
                                               color = "red", na.rm = TRUE) + theme(aspect.ratio = 3/5)
#gg <- gg + xlim(as.Date(c('1/1/2011', '1/1/2013')))

plot(gg)
plot(m, forecast)

ggslackr(plot = last_plot(), channel = "hotjarfeedback")
text_slackr("Prophet Forecasting and Daily Seasonality Reports: Visits", channel = "hotjarfeedback")

# prophet_plot_components(m, forecast)
# prophet_plot_components()
# ggslackr(plot = last_plot(), channel = "hotjarfeedback")
# 
# text_slackr("Prophet Forecasting Trending Components: Visits", channel = "hotjarfeedback")

