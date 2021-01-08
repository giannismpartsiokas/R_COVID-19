#Home Assignment R 2020 - 2021
confirmed <- fread('https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv')
deaths <- fread('https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv')
#Question 1
confirmed <- confirmed[,c("Province/State","Lat","Long"):=NULL]
deaths <- deaths[,c("Province/State","Lat","Long"):=NULL]
#We saw tha Australia, Canada, China, Denmark,France,Netherlands and UK kad mulptiple Provices/States so we summarize
confirmed <- confirmed[, lapply(.SD, sum, na.rm=TRUE), by= .(`Country/Region`) ]
deaths <- deaths[, lapply(.SD, sum, na.rm=TRUE), by= .(`Country/Region`) ]
#Question 2
confirmed <- melt(confirmed,id.vars = "Country/Region",variable.name = "date",value.name = "confirmed")
deaths <- melt(deaths,id.vars = "Country/Region",variable.name = "date",value.name = "deaths")
#Question 3
setnames(confirmed, "Country/Region", "Country")
setnames(deaths, "Country/Region", "Country")
#Question 4 has been already done previously
#Question 5
confirmed[,2] <- mdy(as.character.Date(confirmed[,2]))
deaths[,2] <- mdy(as.character.Date(deaths[,2]))
#Question 6
confirmed <- confirmed[order(Country,date)]
deaths <- deaths[order(Country,date)]
#Question 7
overall <- cbind(confirmed,deaths[,3])
#Question 8
ans1 <- overall[,sum(confirmed),by = .(date)]
ans2 <- overall[,sum(deaths),by = .(date)]
setnames(ans1, "V1", "confirmed")
setnames(ans2, "V1", "deaths")
ans1[, Country :="World"]
ans2[, Country :="World"]
setcolorder(ans1,c("Country","date","confirmed"))
setcolorder(ans2,c("Country","date","deaths"))
ans3 <- cbind(ans1,ans2[,3])
overall <- rbind(overall,ans3)
#Question 9
overall <- overall[order(Country,date)]
#Question 10
overall[,confirmed.ind := confirmed - lag(confirmed,default = 0L), by=.(Country)]
overall[,death.inc := deaths - lag(deaths,default = 0L), by=.(Country)]