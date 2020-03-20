#### Coronavirus Data Cleaning ####
#### February 25, 2020####


#### Load R Packages ####
library(tidyverse)
library(splines)

#### Read Data ####
setwd("D:/Users/profu/Documents/Schoolwork/PhD/Research Projects/coronavirus/exposure")
options(mc.cores=parallel::detectCores())

exposure.files <- (Sys.glob("./*.txt"))
exposures <- lapply(exposure.files, function(x) read_delim(x, delim = "\        ", col_names = FALSE)) 


#### Process Exposure Data ####
## Add monitor info
for (c in 1:length(exposure.files)) {
  exposures[[c]]$monitor <- exposure.files[c]
}

## Merge full dataset
exp <- Reduce(full_join, exposures)
exp <- as_tibble(exp)

## Data cleaning
exp <- 
  exp %>%
  rename(
    date = X1, 
    no2 = X2, 
    so2 = X3, 
    o3 = X4, 
    pm25 = X5, 
    pm10 = X6,
    co = X7
  )

exp$no2 <- as.numeric(exp$no2)
exp$so2 <- as.numeric(exp$so2)
exp$o3 <- as.numeric(exp$o3)
exp$pm25 <- as.numeric(exp$pm25)
exp$pm10 <- as.numeric(exp$pm10)
exp$co <- as.numeric(exp$co)
exp[exp == -999] <- NA

## Brute force edits
monitor <- exp$monitor
monitor <- unlist(strsplit(monitor, split="/", fixed = TRUE))
monitor <- monitor[monitor != "."]
monitor <- unlist(strsplit(monitor, split=".", fixed = TRUE))
monitor <- monitor[monitor != "txt"]
exp$siteid <- monitor

## Merge with City IDs
city <- read.csv("sites_for_r.csv", header=TRUE, stringsAsFactors = FALSE)
exp2 <- merge(exp, city, by = "siteid")
exp2$monitor <- NULL

## Date (Ignore hourly information for now)
exp2$newdate <- as.character(exp2$date)
exp2$newdate <- substr(exp2$newdate,1, nchar(exp2$newdate)-2)

#exp.dta <- aggregate(cbind(no2, so2, o3, pm25, pm10, co) ~ newdate + siteid + ecity, data = exp2, FUN=mean, na.rm=TRUE)
#head(exp.dta, 20)
#write.csv(exp.dta, "exposure.csv")

## Further aggregating exposures for locations with multiple sites
#exp.dta2 <- aggregate(cbind(no2, so2, o3, pm25, pm10, co) ~ newdate + ecity, data = exp2, FUN=mean, na.rm=TRUE)
#head(exp.dta2, 20)
#write.csv(exp.dta2, "exposure.csv")


#### Processing Outcome Data ####

## Data entry was done by hand via data scrapping from reports
setwd("D:/Users/profu/Documents/Schoolwork/PhD/Research Projects/coronavirus/outcome")
options(mc.cores=parallel::detectCores())
outcome <- read_csv("outcome.csv")

## Reload exposure
setwd("D:/Users/profu/Documents/Schoolwork/PhD/Research Projects/coronavirus/exposure")
exposure <- read_csv("wuhan.csv")

## Merge with all data we have so far
dta <- merge(exposure, outcome, by = c("ecity", "date"), all = TRUE)
dta$date <- as.character(dta$date)

## Dates
dta$date <- as.Date(dta$date, format="%Y%m%d")
dta$dow     <- as.POSIXlt(dta$date)$wday
dta <- dta[order(dta$ecity, dta$date),]

#### Additional Data Processing (lags) ####
dta <- dta %>% group_by(ecity) %>% mutate(lag.pm25 = lag(pm25, 1))
dta <- dta %>% group_by(ecity) %>% mutate(lag2.pm25 = lag(pm25, 2))
dta <- dta %>% group_by(ecity) %>% mutate(lag3.pm25 = lag(pm25, 3))
dta <- dta %>% group_by(ecity) %>% mutate(lag4.pm25 = lag(pm25, 4))
dta <- dta %>% group_by(ecity) %>% mutate(lag5.pm25 = lag(pm25, 5))
dta <- dta %>% group_by(ecity) %>% mutate(lag6.pm25 = lag(pm25, 6))
dta <- dta %>% group_by(ecity) %>% mutate(lag7.pm25 = lag(pm25, 7))
dta <- dta %>% group_by(ecity) %>% mutate(lag8.pm25 = lag(pm25, 8))
dta <- dta %>% group_by(ecity) %>% mutate(lag9.pm25 = lag(pm25, 9))
dta <- dta %>% group_by(ecity) %>% mutate(lag10.pm25 = lag(pm25, 10))
dta <- dta %>% group_by(ecity) %>% mutate(lag11.pm25 = lag(pm25, 11))
dta <- dta %>% group_by(ecity) %>% mutate(lag12.pm25 = lag(pm25, 12))
dta <- dta %>% group_by(ecity) %>% mutate(lag13.pm25 = lag(pm25, 13))
dta <- dta %>% group_by(ecity) %>% mutate(lag14.pm25 = lag(pm25, 14))


#### Initial Poisson Model ####
## Note: Loop all models later
model1 <- glm(cases ~ pm25 + as.factor(ecity) + as.factor(dow), data = dta, family = quasipoisson())
summary(model1)

model2 <- glm(cases ~ lag.pm25 + as.factor(ecity) + as.factor(dow), data = dta, family = quasipoisson())
summary(model2)

model3 <- glm(cases ~ lag2.pm25 + as.factor(ecity) + as.factor(dow), data = dta, family = quasipoisson())
summary(model3)

model4 <- glm(cases ~ lag3.pm25 + as.factor(ecity) + as.factor(dow), data = dta, family = quasipoisson())
summary(model4)

model5 <- glm(cases ~ lag4.pm25 + as.factor(ecity) + as.factor(dow), data = dta, family = quasipoisson())
summary(model5)

model6 <- glm(cases ~ lag5.pm25 + as.factor(ecity) + as.factor(dow) , data = dta, family = quasipoisson())
summary(model6)

model7 <- glm(cases ~ lag6.pm25 + as.factor(ecity) + as.factor(dow), data = dta, family = quasipoisson())
summary(model7)

model8 <- glm(cases ~ lag7.pm25 + as.factor(ecity) + as.factor(dow), data = dta, family = quasipoisson())
summary(model8)

model9 <- glm(cases ~ lag8.pm25 + as.factor(ecity) + as.factor(dow), data = dta, family = quasipoisson())
summary(model9)

model10 <- glm(cases ~ lag9.pm25 + as.factor(ecity) + as.factor(dow), data = dta, family = quasipoisson())
summary(model10)

model11 <- glm(cases ~ lag10.pm25 + as.factor(ecity) + as.factor(dow), data = dta, family = quasipoisson())
summary(model11)

model12 <- glm(cases ~ lag11.pm25 + as.factor(ecity) + as.factor(dow), data = dta, family = quasipoisson())
summary(model12)

model13 <- glm(cases ~ lag12.pm25 + as.factor(ecity) + as.factor(dow), data = dta, family = quasipoisson())
summary(model13)

model14 <- glm(cases ~ lag13.pm25 + as.factor(ecity) + as.factor(dow), data = dta, family = quasipoisson())
summary(model14)

model15 <- glm(cases ~ lag14.pm25 + as.factor(ecity) + as.factor(dow), data = dta, family = quasipoisson())
summary(model15)

## Loop this later
model1 <- glm(cases ~ pm25 + as.factor(ecity) + as.factor(dow)+ ns(date, df = 4), data = dta, family = quasipoisson())
summary(model1)

model2 <- glm(cases ~ lag.pm25 + as.factor(ecity) + as.factor(dow)+ ns(date, df = 4), data = dta, family = quasipoisson())
summary(model2)

model3 <- glm(cases ~ lag2.pm25 + as.factor(ecity) + as.factor(dow)+ ns(date, df = 4), data = dta, family = quasipoisson())
summary(model3)

model4 <- glm(cases ~ lag3.pm25 + as.factor(ecity) + as.factor(dow)+ ns(date, df = 4), data = dta, family = quasipoisson())
summary(model4)

model5 <- glm(cases ~ lag4.pm25 + as.factor(ecity) + as.factor(dow)+ ns(date, df = 4), data = dta, family = quasipoisson())
summary(model5)

model6 <- glm(cases ~ lag5.pm25 + as.factor(ecity) + as.factor(dow) + ns(date, df = 4), data = dta, family = quasipoisson())
summary(model6)

model7 <- glm(cases ~ lag6.pm25 + as.factor(ecity) + as.factor(dow)+ ns(date, df = 4), data = dta, family = quasipoisson())
summary(model7)

model8 <- glm(cases ~ lag7.pm25 + as.factor(ecity) + as.factor(dow)+ ns(date, df = 4), data = dta, family = quasipoisson())
summary(model8)

model9 <- glm(cases ~ lag8.pm25 + as.factor(ecity) + as.factor(dow)+ ns(date, df = 4), data = dta, family = quasipoisson())
summary(model9)

model10 <- glm(cases ~ lag9.pm25 + as.factor(ecity) + as.factor(dow)+ ns(date, df = 4), data = dta, family = quasipoisson())
summary(model10)

model11 <- glm(cases ~ lag10.pm25 + as.factor(ecity) + as.factor(dow)+ ns(date, df = 4), data = dta, family = quasipoisson())
summary(model11)

model12 <- glm(cases ~ lag11.pm25 + as.factor(ecity) + as.factor(dow)+ ns(date, df = 4), data = dta, family = quasipoisson())
summary(model12)

model13 <- glm(cases ~ lag12.pm25 + as.factor(ecity) + as.factor(dow)+ ns(date, df = 4), data = dta, family = quasipoisson())
summary(model13)

model14 <- glm(cases ~ lag13.pm25 + as.factor(ecity) + as.factor(dow)+ ns(date, df = 4), data = dta, family = quasipoisson())
summary(model14)

model15 <- glm(cases ~ lag14.pm25 + as.factor(ecity) + as.factor(dow)+ ns(date, df = 4), data = dta, family = quasipoisson())
summary(model15)

#### Calculating % Changes ####
for (i in 1:15){

  mymod <- get(paste("model",i,sep=""))
  
  print(paste("model", i))
  print(paste(round(100*(exp(mymod$coefficients[2]*10)-1), 2),
        round(100*(exp((summary(mymod)$coef[2,1] - 1.96* summary(mymod)$coef[2,2])*10)-1), 2),
        round(100*(exp((summary(mymod)$coef[2,1] + 1.96* summary(mymod)$coef[2,2])*10)-1), 2)))

  
  cat("\n")
  
  }


#### Plotting ####
setwd("D:/Users/profu/Documents/Schoolwork/PhD/Research Projects/coronavirus/r_files")
ci <- read_csv("confidence_intervals.csv")
ci$lag <- as.factor(ci$lag)
ci$df <- as.factor(ci$df)

ggplot(ci, aes(x=lag, y=pi, color = ci$df)) +
  geom_errorbar(aes(ymin=lpi, ymax=upi, width = 0.5), position = position_dodge2(), size=0.8) +
  geom_point(position = position_dodge2(width = 0.5), size=2.5) +
  theme(legend.position = "bottom") + 
  #ggtitle(expression("PM"[2.5]*" and Cardiovascular Hospitalization by Season")) +
  labs(y = expression("Percent Increase (Per 10 μg/m"^3*")"), x = "Lag") + 
  scale_color_discrete(name="Analysis Type", breaks=c("0", "1", "2", "3", "4"),
                   labels=c("No Time Trends", "Linear", "df=2", "df=3", "df=4")) +
  theme(text = element_text(size=15)) +
  geom_hline(yintercept=0, linetype="dashed")


ggplot(dta, aes(x=date, y=pm25, color = ecity)) +
  geom_line() +
  labs(y = expression("PM"[2.5]*""), x = "Date")

ggplot(dta, aes(x=date, y=cases, color = ecity)) +
  geom_line() + 
  scale_x_date(limits = c("1 day", NA))
  labs(y = expression("PM"[2.5]*""), x = "Date")
