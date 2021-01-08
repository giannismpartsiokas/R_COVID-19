#Slices of the dataset that we will analyze
Greece <- overall[Country == "Greece"]
World <- overall[Country == "World"]
EU <- overall[Country == "Greece" | Country == "Cyprus" | Country == "Austria" | Country == "Latvia"| Country == "Belgium"| Country == "Lithuania"| Country == "Bulgaria"| Country == "Luxembourg"| Country == "France"| Country == "Malta"| Country == "Germany"| Country == "Hungary"| Country == "Denmark"| Country == "Poland"| Country == "Portugal"| Country == "Estonia"| Country == "Romania"| Country == "Ireland"| Country == "Slovakia"| Country == "Spain"| Country == "Slovenia"| Country == "Italy"| Country == "Sweden"| Country == "Netherlands"| Country == "Czech Republic"| Country == "Croatia"| Country == "Finland"]
EU <- EU[,c("Country"):=NULL]
EU <- EU[, lapply(.SD, sum, na.rm=TRUE), by= .(date) ]
EU <- EU[,Country :="EU"]
setcolorder(EU,c("Country","date","confirmed","deaths", "confirmed.ind","death.inc"))
interest_data <- rbind(EU,Greece,World)
#Basic Plots
#World
ggplot(World,aes(x = date, y = confirmed)) + geom_line(color = "blue") + labs(title = "World Cumulative confirmed cases", x = "Date", y = "Cumulative confirmed cases")
ggsave("world_cumulative_cases.png")
ggplot(World,aes(x = date, y = deaths)) + geom_line(color = "blue") + labs(title = "World Cumulative deaths", x = "Date", y = "Cumulative Deaths")
ggsave("world_deaths.png")
ggplot(World,aes(x = date, y = confirmed.ind)) + geom_line(color = "blue")  + labs(title = "World daily confirmed cases", x = "Date", y = "Daily confirmed cases")
ggsave("world_daily_cases.png")
ggplot(World,aes(x = date, y = death.inc)) + geom_line(color = "blue")  + labs(title = "World daily deaths", x = "Date", y = "Daily deaths")
ggsave("world_daily_deaths.png")
#EU
ggplot(EU,aes(x = date, y = confirmed)) + geom_line(color = "red") + labs(title = "EU Cumulative confirmed cases", x = "Date", y = "Cumulative confirmed cases")
ggsave("eu_cumulative_cases.png")
ggplot(EU,aes(x = date, y = deaths)) + geom_line(color = "red") + labs(title = "EU Cumulative deaths", x = "Date", y = "Cumulative Deaths")
ggsave("eu_deaths.png")
ggplot(EU,aes(x = date, y = confirmed.ind)) + geom_line(color = "red")  + labs(title = "EU daily confirmed cases", x = "Date", y = "Daily confirmed cases")
ggsave("eu_daily_cases.png")
ggplot(EU,aes(x = date, y = death.inc)) + geom_line(color = "red")  + labs(title = "EU daily deaths", x = "Date", y = "Daily deaths")
ggsave("eu_daily_deaths.png")
#Greece
#In order to plot and compare Greece with World and EU, I firstly create some new variables
#These variables are the mean values for World and EU in order to plot the mean plots 
mean_World = World[,.(confirmed = confirmed/length(unique(overall$Country)),deaths = deaths/length(unique(overall$Country)),confirmed.ind = confirmed.ind/length(unique(overall$Country)),death.inc = death.inc/length(unique(overall$Country)))]
mean_World = cbind(World[,1:2],mean_World)
mean_EU = EU[,.(confirmed = confirmed/27,deaths = deaths/27,confirmed.ind = confirmed.ind/27,death.inc = death.inc/27)]
mean_EU = cbind(EU[,1:2],mean_EU)
ggplot(Greece,aes(x = date, y = confirmed)) + geom_line(aes(color = Country)) + labs(title = "Greece Cumulative confirmed cases", subtitle = "Compared to mean World and EU values", x = "Date", y = "Cumulative confirmed cases") + geom_line(data = mean_World, aes(color = Country)) + geom_line(data = mean_EU, aes(color = Country))
ggsave("gr_cumulative_cases.png")
ggplot(Greece,aes(x = date, y = deaths)) + geom_line(aes(color = Country)) + labs(title = "Greece Cumulative deaths", subtitle = "Compared to mean World and EU values", x = "Date", y = "Cumulative confirmed cases") + geom_line(data = mean_World, aes(color = Country)) + geom_line(data = mean_EU, aes(color = Country))
ggsave("gr_deaths.png")
ggplot(Greece,aes(x = date, y = confirmed.ind)) + geom_line(aes(color = Country)) + labs(title = "Greece daily confirmed cases", subtitle = "Compared to mean World and EU values", x = "Date", y = "Cumulative confirmed cases") + geom_line(data = mean_World, aes(color = Country)) + geom_line(data = mean_EU, aes(color = Country))
ggsave("gr_daily_cases.png")
ggplot(Greece,aes(x = date, y = death.inc)) + geom_line(aes(color = Country)) + labs(title = "Greece daily deaths", subtitle = "Compared to mean World and EU values", x = "Date", y = "Cumulative confirmed cases") + geom_line(data = mean_World, aes(color = Country)) + geom_line(data = mean_EU, aes(color = Country))
ggsave("gr_daily_deaths.png")