options(stringsAsFactors=FALSE)

library(plyr)
library(ggplot2)
library(reshape)
library(maps)
library(RCurl)
library(stringr)

dat.key <- '0AjGTI9tB9Lv_dHA2LTZ3Y2JUbDNOOEpmZG83SjVEbVE'

dat.form = getForm("http://spreadsheets0.google.com/spreadsheet/pub",
              hl ="en", key = dat.key,
              single = "true", gid ="9",
              output = "csv",
             .opts = list(followlocation = TRUE, verbose = FALSE))

dat <- read.csv(textConnection(dat.form))
names(dat) <- tolower(names(dat))

# remove rows without counties
dat <- subset(dat, subset=str_detect(county, '\\\\'))
dat$state_name <- str_extract(dat$county, '[A-Za-z ]+')

ggplot(subset(dat, avg_govt_payments.2002>0 & tenure_2_or_less.2002>0 & tenure_2_or_less.2007>0), 
       aes(avg_govt_payments.2002/1000, log(tenure_2_or_less.2007/tenure_2_or_less.2002))) +
  geom_point() + stat_smooth(method='lm', se=FALSE, size=2) + 
  facet_wrap(~ state_name) +
  scale_x_continuous('2002 Average Government Subsidies ($K)', breaks=c(0,25,50,75)) +
  scale_y_continuous('log(2007 New Farmers / 2002 New Farmers)') +
  coord_cartesian(ylim=c(-2,2))

dat.tenure <- (dat[,3:10])

dat.tenure.2007 <- (dat.tenure[,grepl('2007', names(dat.tenure))])
dat.tenure.2007$year <- 2007
dat.tenure.2007$state_name <- dat$state_name
dat.tenure.2007$county_name <- dat$geo
names(dat.tenure.2007) <- str_replace(names(dat.tenure.2007), '.2007', '')

dat.tenure.2002 <- (dat.tenure[,grepl('2002', names(dat.tenure))])
dat.tenure.2002$year <- 2002
dat.tenure.2002$state_name <- dat$state_name
dat.tenure.2002$county_name <- dat$geo
names(dat.tenure.2002) <- str_replace(names(dat.tenure.2002), '.2002', '')

dat.tenure <- rbind(dat.tenure.2002, dat.tenure.2007)
dat.tenure <- melt(dat.tenure, measure.vars=1:4, variable_name='tenure')
dat.tenure$tenure_num <- as.numeric(str_extract(dat.tenure$tenure, '\\d+'))
dat.tenure$new <- dat.tenure$tenure_num < 10

dat.tenure.bystate <- ddply(dat.tenure, .(year, state_name),
                            summarise, prop_new_farmers=sum(value*new)/sum(value))
dat.tenure.bystate$state_name <- tolower(dat.tenure.bystate$state_name)
dat.tenure.bystate <- cast(dat.tenure.bystate, formula=state_name ~ year)
names(dat.tenure.bystate) <- c('state', 'prop.new.2002', 'prop.new.2007')