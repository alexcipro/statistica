
# income_pop.csv
#  id,income
#  88986,176000
#  181233,204000
# https://github.com/alexcipro/statistica/raw/master/income_pop.csv


# reg_pop.csv
#  id,age,gender,city_code
#  88986,28,1,46830
#  181233,15,1,98649
# https://github.com/alexcipro/statistica/raw/master/reg_pop.csv

# city_codes.csv
#  county,city_code,urban_rural,region
#  1,1017,1,7
#  1,1213,1,7
# https://github.com/alexcipro/statistica/raw/master/city_codes.csv


# 1. read de data in R environment

# 2. explore de data (head(), summary(), str())

# 3. indentify the duplicates

# 4. aggregate the incomme of each person

# 5. merge the data by ID

# 6. aggregate the average of incomme by county

# 7. create a ggplot graph with geom_point()

# 8. aggregate the average of incomme by region + ggplot()

# 9. aggregate the average of incomme by urban_rural

# 10. counts the persons by age group of 5 years + plot()





# 1. read de data in R environment
cale <- "https://github.com/alexcipro/statistica/raw/master/"
ip <- read.csv("income_pop.csv")
rpop <- read.csv("reg_pop.csv")
cities <- read.csv("city_codes.csv")

# 2. explore de data (head(), summary(), str())
head(ip)
head(rpop)
head(cities)
summary(rpop)

# 3. indentify the duplicates
sum(duplicated(ip$id))
sum(duplicated(rpop$id))

# 4. aggregate the incomme of each person
# eliminam observatiile care contin NA in orice variabila
ip <- na.omit(ip)
# eliminam observatiile care contin NA in variabila 'income'
ip <- ip[!is.na(ip$income),]

ip <- aggregate(ip$income, by = list(ip$id), FUN=sum)
head(ip)
names(ip) <- c("id", "income")
head(ip)
range(ip$income)
summary(ip)

# 5. merge the data by ID
ipop <- merge(ip, rpop, by = "id")
# ipop <- merge(ip, rpop, by.x = "id", by.y = "cnp")
head(ipop)
rm(ip,rpop)
summary(ipop)
mean(ipop$income)
mean(ipop[is.na(ipop$city_code),]$income)
ipop <- na.omit(ipop)

# 6. aggregate the average of incomme by county
# verific daca toate persoanele au codul localitatii corect, 
# din nomenclatorul de localitati
sum(!ipop$city_code %in% cities$city_code)

ipop <- merge(ipop, cities, by = "city_code", all.x = TRUE)
head(ipop)
ai_county <- aggregate(ipop$income, by = list(ipop$county), FUN=mean)
ai_county <- aggregate(data = ipop, income~county, FUN=mean)
ai_county
names(ai_county) <- c("county", "average_income")
head(ai_county)

# 7. create a ggplot graph with geom_point()
library(ggplot2)
ai_county$county <- as.factor(ai_county$county)
ggplot(ai_county,aes(county,average_income)) + geom_point() 

# 8. aggregate the average of incomme by region + ggplot()
ai_region <- aggregate(ipop$income, by = list(ipop$region), FUN=mean)
ai_region <- aggregate(data = ipop, income~region+gender, FUN=mean)
ai_region
names(ai_region) <- c("region", "average_income")
names(ai_region) <- c("region", "gender", "average_income")
head(ai_region)

ai_region$region <- as.factor(ai_region$region)
ai_region$gender <- as.factor(ai_region$gender)
ggplot(ai_region,aes(region,average_income)) + geom_point()
ggplot(ai_region,aes(region,average_income)) + geom_point(colour = ai_region$gender)
levels(ai_region$gender)

# 9. aggregate the average of incomme by urban_rural
ai_ur <- aggregate(ipop$income, by = list(ipop$urban_rural), FUN=mean)
ai_ur
names(ai_ur) <- c("urban_rural", "average_income")
head(ai_ur)
ai_ur$urban_rural <- factor(ai_ur$urban_rural, levels = c("1", "3"), labels = c("urban", "rural"))
head(ai_ur)

# 10. counts the persons by age group of 10 years + plot()
ageg <- seq(0, 110, by=10)
ipop$ageg <- cut(ipop$age, ageg, right = FALSE) 
head(ipop)
ipop$count <- 1
ai_ageg <- aggregate(ipop$count, by = list(ipop$ageg), FUN=sum)
ai_ageg
head(ai_ageg)
names(ai_ageg) <- c("Age_Group", "Average_Income")
head(ai_ageg)
plot(ai_ageg)








