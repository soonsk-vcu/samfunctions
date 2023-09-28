library(readr)
library(readxl)

lacrime <- read_csv("data-raw/Crime_Data_LA_2020_Sample.csv")

# clean vict age for impossible values and remove time from date column
lacrime <- lacrime[!is.na(lacrime$'Vict Age') & lacrime$'Vict Age' > 0,]
lacrime$`Date OCC Adj` <- gsub("(^[0-9]+/[0-9]+/[0-9]+) .+", "\\1", lacrime$`DATE OCC`)
lacrime$`Date OCC Adj` <- as.Date(lacrime$`Date OCC Adj`,format = "%m/%d/%y")
lacrime$`TIME OCC` <- as.numeric(lacrime$`TIME OCC`)


# season stuff
seasonbound <-as.numeric(as.Date(c("03-01-2020", "06-01-2020",
                                   "09-01-2020", "12-01-2020"),
                                 "%m-%d-%Y"))
seasonto <- c("Cold", "Warm", "Warm", "Cold", "Cold")
lacrime$Season <- convert(as.numeric(as.Date(lacrime$`Date OCC Adj`)),
                          seasonbound, seasonto, TRUE)

# create age groups
agebound <- c(13,18,30,65)
agecat <- c("Other", "Other", "Other", "Adult", "Other")
lacrime$`Age Group` <- convert(lacrime$`Vict Age`, agebound, agecat, interval=TRUE)

usethis::use_data(lacrime, overwrite = T)
