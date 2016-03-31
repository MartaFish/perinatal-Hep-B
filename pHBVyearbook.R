#go to perinatal Hep B reports in Orpheus and do a "mom/baby pairs" report.
#babies born between 1/1/10 and 1/1/16 from Multnomah county
#download the report to Excel in the perinatal Hep B folder with the name 2010_2015_all_MultCo
#Open the report in Excel then save a new copy as a .csv file

#get any packages which will be needed
library(dplyr)
library(lubridate)

#retrieve the data and pull out only the parts which we're going to use
YearbookRawData <- read.csv("H:/Perinatal Hepatitis B Program/2010_2015_all_MultCo.csv")
YearbookEdit1 <- select(YearbookRawData, 
                        babyDOB = C_Contact_Person..DOB,
                        CompleteDate, 
                        IsDoneReason, 
                        VaxStatus, 
                        County, 
                        FollowupCounty)

#clean up the data 
#replace blanks in FollowupCounty with "Multnomah"  (coded as "", not na or NULL)
NotBlank <- function(County){
        if(County == ""){
                return("Multnomah")
        } else {
                return(as.character(County))
        }
}
pHBVCounty <- sapply(YearbookEdit1$FollowupCounty, NotBlank)
YearbookFinal <- cbind(YearbookEdit1, pHBVCounty)
#remove other county columns, leaving only the new data (baby/contact county)
YearbookFinal <- select(YearbookFinal, -County, -FollowupCounty)
#remove rows with Multnomah county case, but out-of-county baby/contact (i.e., county other than "Multnomah") 
YearbookFinal <- filter(YearbookFinal, pHBVCounty == "Multnomah")
#change babyDOB and CompleteDate columns to date format
YearbookFinal$CompleteDate <- as.POSIXct(strptime(YearbookFinal$CompleteDate, format = "%m/%d/%Y"))
YearbookFinal$babyDOB <- as.POSIXct(strptime(YearbookFinal$babyDOB, format = "%m/%d/%Y"))
#note: blanks in IsDone, CompleteDate, and IsDoneReason are NULL values

#table of babies by year, columns are:
#total pHBV babies, PVS completed w/in 18 mos, babies not yet 18 mos old, babies at least 9 mos old,
#unresolved cases, % completed in 18 mos
babiesbyyear <- summarise(group_by(YearbookFinal, year(babyDOB)), 
                         "totalbabies" = n(), 
                         "timelycompletion" = sum(IsDoneReason == "services completed" & ddays(548) > as.duration(CompleteDate - babyDOB)),
                         "babies<18mo" = sum(ddays(548) > as.duration(as.POSIXct(Sys.Date()) - babyDOB)),
                         "babies>9mo" = sum(ddays(273) < as.duration(as.POSIXct(Sys.Date()) - babyDOB)),
                         "unresolved" = sum(is.na(CompleteDate)))
mutate(babiesbyyear, percenttimely = timelycompletion / totalbabies)

#babies with successful PVS results who are at least 9 mos old and were born in 2013 or later
#will need to change the last date here to 9 mos before the date you downloaded from Orpheus
success <- sum(YearbookFinal$IsDoneReason == "services completed" & YearbookFinal$babyDOB >= "2013-01-01") / 
        sum(YearbookFinal$babyDOB >= "2013-01-01" & YearbookFinal$babyDOB < "2015-05-26")

#Table of IsDone Reason for babies born 2013 or later
YearbookRecent <- filter(YearbookFinal, year(babyDOB) >= 2013)
finalstatusrecent <- count(YearbookRecent, IsDoneReason)