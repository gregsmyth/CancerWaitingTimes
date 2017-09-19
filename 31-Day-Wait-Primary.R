library(tidyverse)

header <- list("DATE","ODS_CODE","PROVIDER","CARE_SETTING", "CANCER_TYPE",	"TOTAL",	"WITHIN_31_DAYS",	"AFTER_31_DAYS", "PERCENTAGE_TREATED_WITHIN_31_DAYS", "EMPTY", "WITHIN_31_DAYS_duplicate", "FROM_32_TO_38_DAYS", "FROM_39_TO_48_DAYS", "FROM_49_TO_62_DAYS", "AFTER_62_DAYS")

colClasses = c("integer", "character", "character","character", "character","integer", "integer","integer", "character","integer","integer","integer","integer","integer","integer")

allMonthsAllCancer <- read.table(text = "",
                                    colClasses = colClasses,
                                    col.names = header)

for (thisMonth in c(201506,201507,201508,201509,201510,201511,201512,201601,201602,201603,201604,201605,201606,201607,201608,201609,201610,201611,201612,201701,201702,201703,201704,201705,201706,201707)) {
  
  thisMonthFile <- paste("./Data/",thisMonth,"-5.-31-Day-Wait-for-first-treatment-All-Cancers-Provider-Data.csv", sep = "")
  
  
  thisMonthAllCancer <- read.csv(thisMonthFile, header = FALSE, col.names = header) #read first chunk corresponding to admitted patients
  thisMonthAllCancer[,1] <- thisMonth
  thisMonthAllCancer <- thisMonthAllCancer[!((thisMonthAllCancer$ODS_CODE=="" & thisMonthAllCancer$PROVIDER != "ALL ENGLISH PROVIDERS") | thisMonthAllCancer$ODS_CODE == "ODS CODE (1)" | thisMonthAllCancer$ODS_CODE == "ODS CODE (2)"),]
  
  allMonthsAllCancer <- rbind(allMonthsAllCancer, thisMonthAllCancer)
}


# convert percentage string to value
clean <- function(ttt){as.numeric( gsub('[^a-zA-Z0-9.]', '', ttt))}
allMonthsAllCancer$PERCENTAGE_TREATED_WITHIN_31_DAYS <- clean(allMonthsAllCancer$PERCENTAGE_TREATED_WITHIN_31_DAYS)

allMonthsAllCancer$EMPTY <- NULL
allMonthsAllCancer$WITHIN_31_DAYS_duplicate <- NULL



# but first need to make all characters so factors don't get corrupted when converting to numbers
allMonthsAllCancer <- sapply(allMonthsAllCancer, as.character)  

allMonthsAllCancer <- as_tibble(allMonthsAllCancer) #convert to a tibble

# convert each column type from fctr as appropriate
allMonthsAllCancer[, 6:8] <- sapply(allMonthsAllCancer[, 6:8], as.integer)
allMonthsAllCancer[,9] <- sapply(allMonthsAllCancer[,9],as.double)
allMonthsAllCancer[, 10:13] <- sapply(allMonthsAllCancer[, 10:13], as.integer)


allProviders <- unique(allMonthsAllCancer$PROVIDER)
for (thisProvider in allProviders) {
  providerData <- filter(allMonthsAllCancer, CARE_SETTING == "ALL CARE" & PROVIDER == thisProvider)
  
  thisMean <- mean(providerData$PERCENTAGE_TREATED_WITHIN_31_DAYS)
  ucl <- mean(providerData$PERCENTAGE_TREATED_WITHIN_31_DAYS) + 3*sd(providerData$PERCENTAGE_TREATED_WITHIN_31_DAYS)
  lcl <- mean(providerData$PERCENTAGE_TREATED_WITHIN_31_DAYS) - 3*sd(providerData$PERCENTAGE_TREATED_WITHIN_31_DAYS)
  
  filename <- paste("./31-Day-Wait-Primary/", thisProvider, ".eps", sep = "") 
  
  ggplot(providerData, mapping = aes(x=DATE,y=PERCENTAGE_TREATED_WITHIN_31_DAYS, group=1)) +
    geom_line() +
    geom_point() +
    geom_hline(yintercept=96, color = "red") + 
    geom_hline(yintercept = thisMean, color = "blue") +
    geom_hline(yintercept = lcl, color = "green") +
    theme_bw() +
    theme(axis.text.x=element_text(angle=90,hjust=1, vjust=0.5))
  
  ggsave(filename)
}

