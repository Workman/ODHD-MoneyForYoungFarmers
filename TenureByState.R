options(stringsAsFactors=FALSE)

library(plyr)
library(ggplot2)
library(reshape)
library(maps)
library(RCurl)
library(stringr)

new.farmers.state.key <- '0AjGTI9tB9Lv_dHA2LTZ3Y2JUbDNOOEpmZG83SjVEbVE'
farmers.tenure.state.key <- '0AjGTI9tB9Lv_dHA2LTZ3Y2JUbDNOOEpmZG83SjVEbVE'

nfs.form = getForm("http://spreadsheets0.google.com/spreadsheet/pub",
              hl ="en", key = new.farmers.state.key,
              single = "true", gid ="1",
              output = "csv",
             .opts = list(followlocation = TRUE, verbose = TRUE))

new.farmers.state <- read.csv(textConnection(nfs.form))
names(new.farmers.state) <- tolower(names(new.farmers.state))
new.farmers.state <- mutate(new.farmers.state,
                            state = tolower(state),
                            newnumber = as.numeric(newnumber),
                            newlending = as.numeric(str_replace_all(newlending, '\\D', '')),
                            totalnumber = as.numeric(totalnumber),
                            totallending = as.numeric(str_replace_all(totallending, '\\D', '')),
                            pcttonew_num = newnumber/totalnumber,
                            pcttonew_lending = newlending/totallending)
new.farmers.state$pcttonew <- NULL

fts.form = getForm("http://spreadsheets0.google.com/spreadsheet/pub",
              hl ="en", key = farmers.tenure.state.key,
              single = "true", gid ="5",
              output = "csv",
             .opts = list(followlocation = TRUE, verbose = FALSE))

farmers.tenure.state <- read.csv(textConnection(fts.form))
names(farmers.tenure.state) <- tolower(names(farmers.tenure.state))

farmers.tenure.state$state <- tolower(farmers.tenure.state$state)
farmers.tenure.state$total <- rowSums(as.matrix(farmers.tenure.state[,2:5]))

dat.all <- join(new.farmers.state, farmers.tenure.state)


ggplot(dat.all, aes(pcttonew_num, less.than.3.years/total)) + geom_point()

# compare per-state lending to new farmers to total(increase) in new farmers
dat.2 <- merge(dat.all, dat.tenure.bystate)

ggplot(dat.2, aes(pcttonew_lending, prop.new.2007/prop.new.2002, label=state)) +
  geom_text() + stat_smooth() +
  xlab('% of Farm Service Agency Lending to New Farmers (2010)') +
  ylab('Change of Proportion of New Farmers (2007 vs 2002)')


ggplot(dat.2, aes(newlending, prop.new.2007/prop.new.2002)) + geom_point() + stat_smooth()
