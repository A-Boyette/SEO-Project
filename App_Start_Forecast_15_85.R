
library(sqldf)#sqldf
library(ggplot2)#qplot
library(e1071)#naiveBayes
library(caret)
library(plyr)
library(dplyr)
library(magrittr)#%>% equals

n <- c("Opportunity_ID", "Created_Date", "Created_Date_Time", "Opportunity_Name", "Email", "Days_Since_Created",
       "Birthdate", "Gender", "GPA", "Payment_Type", "Total_Yrs_Wrk_Exp", "Highest_Degree_Earned", "Motivation",  
       "Why_Now", "Discount", "Discount_Type",  "Employment_Status",  "Employer_Company", "Title",  "Mailing_Street", 
       "Mailing_City",  "Mailing_State",  "Mailing_Zip",  "Mailing_Country",  "Time_Zone",  "Parent_Campaign_Name", 
       "Campaign_Name", "Region", "Abbreviation", "Mature_Status",  "Program_Academic_Program_Name", 
       "Program_Degree_Type", "Program_Category", "Program_Maturity_Status",  "Opportunity_Owner",  
       "Opportunity_Owner_Title", "Employee_Start_Date",  "Admissions_Manager", "Stage", "Sub_Stage",
       "Drop_Denied_Reason", "Last_Stage_Change_Date", "Stage_Duration",  "Roll_Counter", "Initial_Attempt", 
       "Time_to_Initial_Attempt", "Quality_Contact",  "Time_to_Quality_Contact", "Appointment_Completed", 
       "Pending_Application_Stage_Date", "Date_Booked", "Lead_to_App",  "Days_To_File_Complete",  "Date_Submitted",
       "Date_Accepted", "Date_Registered",  "Initial_Start_Date", "Start_Date", "Other_School")

data_all <- tbl_df(read.csv("150612 CY13-CY15 Mass Data Sample.csv", stringsAsFactors = T, na.strings = c("", " ")))

names(data_all) <- n

#Formatting
data_all$Created_Date %<>% as.Date("%m/%d/%Y")
data_all$Year_Month <- data_all$Created_Date %>% format("%Y/%m") %>% as.factor()
data_all$App90 <- ifelse(is.na(data_all$Lead_to_App)==0 & data_all$Lead_to_App <= 90 , 1, 0 )


#Subsetting
sub <- data_all %>%
     filter(is.na(Created_Date)==0 & Created_Date <= as.Date("02/28/2015", "%m/%d/%Y") & 
                 Parent_Campaign_Name == "SEO" & Campaign_Name %in% c("Search Engine", "Organic", "SE- LiveBall", 
                                                                      "SE- Search Engine", "SE- Other", "SE - Other", "Search Engine(Google, Yahoo, MSN, etc.)") &
                 !(Abbreviation %in% c("JHU", "CSG", "CWU", "DTN", "GNZ", "UOB", "USP", "UTX", "JHU CAREY", "MHC", "NWC")) &
                 Program_Category != "0" & is.na(Program_Degree_Type)==0) %>%
     mutate(Start90 = ifelse(is.na(Start_Date)==0 & App90 == 1, 1, 0))%>%
     select(Lead_to_App, Start_Date, Created_Date, Abbreviation, Program_Academic_Program_Name, Program_Degree_Type, Program_Category, Year_Month, App90, Start90) #keep_vars will include Created_Date, Abbreviation, Program_Degree_Type, Program_Category

train <- sub %>%
     filter(Created_Date <= as.Date("12/31/2014", "%m/%d/%Y")) %>%
     select(Program_Academic_Program_Name, Abbreviation, Program_Degree_Type, Program_Category, Year_Month, App90, Start90)

test <- sub %>%
     filter(Created_Date <= as.Date("02/28/2015", "%m/%d/%Y") & Created_Date > as.Date("12/31/2014", "%m/%d/%Y")) %>%
     select(Program_Academic_Program_Name, Abbreviation, Program_Degree_Type, Program_Category, Year_Month, App90, Start90)




quants <- train %>% 
     group_by(Program_Academic_Program_Name, Abbreviation, Program_Category, Program_Degree_Type, Year_Month) %>%
     summarise(leads = n()) %>%
     summarise(lower_b = quantile(leads, probs=.15), upper_b = quantile(leads, probs=.85))

train_agg <- train %>% 
     group_by(Program_Academic_Program_Name, Abbreviation, Program_Category, Program_Degree_Type, Year_Month) %>%
     summarise(leads = n())

test_agg <- test %>%
     select(Program_Academic_Program_Name, Abbreviation, Program_Category, Program_Degree_Type, Year_Month, App90, Start90) %>%
     group_by(Program_Academic_Program_Name, Abbreviation, Program_Category, Program_Degree_Type, Year_Month) %>%
     summarise(leads = n(), apps = sum(App90), starts = sum(Start90))

train_result <-merge(train_agg, quants)

test_result <-tbl_df(merge(test_agg, quants))

train_result$lead_vol <- as.factor(ifelse(train_result$leads <= train_result$lower_b, "low", ifelse(train_result$leads > train_result$upper_b, "high", "average")))

test_result$lead_vol <- as.factor(ifelse(test_result$leads <= test_result$lower_b, "low", ifelse(test_result$leads > test_result$upper_b, "high", "average")))

test_done <- test_result#[, -c(9:10)]

train_done <- tbl_df(merge(train, train_result[, -c(6:8)]))

train_done$App90 <- as.factor(train_done$App90)



nb <- naiveBayes(App90 ~ Program_Academic_Program_Name + Abbreviation + Program_Degree_Type + Program_Category + lead_vol, data = train_done)

pred<-predict(nb, newdata=train_done, type=c("raw"))

train_done$prob_1 <- pred[,2]

train_final <- unique(train_done[,c(1:4, 8:9)])

final <- merge(test_done, train_final)

final$pred_apps <- round(final$leads*final$prob_1)

final<-final %>% arrange(Abbreviation, Program_Category, Program_Degree_Type)

final$resid <- final$pred_apps-final$apps

qplot(final$pred_apps, final$apps)+geom_jitter()+geom_abline()+geom_abline(intercept=2)+geom_abline(intercept=-2)

sse<- sum((final$pred_apps-final$apps)^2);sse

#.1/.9=1050 88%
#.15/.85=1046 90%
#.2/.8=1069 89%


nb <- naiveBayes(Start90 ~ Program_Academic_Program_Name + Abbreviation + Program_Degree_Type + Program_Category + lead_vol, data = train_done[train_done$App90==1,])

pred2<-predict(nb, newdata=train_done[train_done$App90==1,], type=c("raw"))

train_done2 <- train_done[train_done$App90==1,]

train_done2$prob_1 <- pred2[,2]

train_final2 <- unique(train_done2[,c(1:4, 8:9)])

final2 <- merge(test_done, train_final2)

final2$pred_starts <- round(final2$apps*final2$prob_1)

final2$resid <- final2$pred_starts - final2$starts

qplot(final2$pred_starts, final2$starts)+geom_jitter()+geom_abline()+geom_abline(intercept=2)+geom_abline(intercept=-2)

