#Predict Greece
gr_last <- Greece[date>="2020-12-17"]
gr_last <- gr_last[,c("deaths","confirmed.ind","death.inc"):=NULL]
gr_last$laged <- lag(gr_last$confirmed,default = 127557L)
#plot before model
ggplot(gr_last,aes(x = date, y = confirmed)) + geom_line(color = "blue") + labs(title = "Greece last-days cumulative confirmed cases", x = "Date", y = "Cumulative confirmed cases")
####################
model = lm(log(confirmed) ~ laged + I(laged^2) , data = gr_last)
summary(model)
dates <- as.data.table(seq(ymd("2021-01-04"), by = "day", to = today()))
setnames(dates,"date")
predictions <- as.data.table(matrix(0,today() - ymd("2021-01-03")))
setnames(predictions,"confirmed")
country <- as.data.table(rep("Predictions for Greece",today() - ymd("2021-01-03")))
setnames(country,"Country")
laged <- as.data.table(matrix(0,today() - ymd("2021-01-03")))
setnames(laged,"laged")
predictions <- cbind(country,dates,predictions,laged)
predictions[1,4] <- 140099
for (i in 1:dim(predictions)[1]){
  predictions$confirmed[i] <- exp(predict(model, predictions[i]))
  if (i!=dim(predictions)[1]){
    predictions$laged[i+1] <- predictions$confirmed[i]
  }
}
#plot after model
ggplot(gr_last,aes(x = date, y = confirmed)) + geom_line(aes(color = Country)) + labs(title = "Greece last-days cumulative confirmed cases", x = "Date", y = "Cumulative confirmed cases") + geom_line(data = predictions, aes(color = Country))
####################