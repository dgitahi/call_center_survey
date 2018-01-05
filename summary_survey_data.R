setwd("/Users/dgitahi/documents/survey")
library(dplyr)
library(ggplot2)
survey_data=read.csv("survey_01.csv")
colnames(survey_data)[14] = "Reason_for_calling"
colnames(survey_data)[21] = "What_problem"
colnames(survey_data)[7] = "Why_it_is_no"
colnames(survey_data)[8] = "Why_it_is_no1"
colnames(survey_data)[9] = "Number_trials"
colnames(survey_data)[11] = "Reason_for_no_permission"
colnames(survey_data)[17] = "Do_it_yourself"
colnames(survey_data)[18] = "What_did_you_do_yourself"
colnames(survey_data)[19] = "Why_did_you_call"
colnames(survey_data)[23] = "Did_you_resolve_issue"


colnames(survey_data)[24] = "q2_Ignore" #Ignore
colnames(survey_data)[25] = "q2_Buy_another_line" #Buy_another_line
colnames(survey_data)[26] = "q2_Call_help_line" #Call_help_line
colnames(survey_data)[27] = "q2_Vist_Retail_center" #Vist_Retail_center
colnames(survey_data)[28] = "q2_Contact_safaricom_staff" #Contact_safaricom_staff
colnames(survey_data)[29] = "q2_Vist_Dealer" #Vist_Dealer
colnames(survey_data)[30] = "q2_Call_a_friend" #Call_a_friend



survey_data$q2_Ignore =as.numeric(ifelse(survey_data$q2_Ignore == "Checked",1,0))
survey_data$q2_Buy_another_line =as.numeric(ifelse(survey_data$q2_Buy_another_line == "Checked",1,0))
survey_data$q2_Call_help_line =as.numeric(ifelse(survey_data$q2_Call_help_line == "Checked",1,0))
survey_data$q2_Vist_Retail_center =as.numeric(ifelse(survey_data$q2_Vist_Retail_center == "Checked",1,0))
survey_data$q2_Contact_safaricom_staff =as.numeric(ifelse(survey_data$q2_Contact_safaricom_staff == "Checked",1,0))
survey_data$q2_Vist_Dealer =as.numeric(ifelse(survey_data$q2_Vist_Dealer == "Checked",1,0))
survey_data$q2_Call_a_friend =as.numeric(ifelse(survey_data$q2_Call_a_friend == "Checked",1,0))



colnames(survey_data)[32] = "q1_sms" #sms
colnames(survey_data)[33] = "q1_Twitter" #Twitter
colnames(survey_data)[34] = "q1_Facebook" #Facebook
colnames(survey_data)[35] = "q1_Chat" #Chat
colnames(survey_data)[36] = "q1_Shop" #Shop_care_Desk
colnames(survey_data)[37] = "q1_Staff" #Safaricom Staff
colnames(survey_data)[38] = "q1_MpesaAgent" #MPESA.agent.Dealer
colnames(survey_data)[39] = "q1_Friend" #Friend_or_Family
colnames(survey_data)[40] = "q1_DIY" #DIY
colnames(survey_data)[41] = "q1_Call_Back" #Called_back_Callcenter

survey_data$q1_Facebook=replace(survey_data$q1_Facebook,is.na(survey_data$q1_Facebook),"")
survey_data$q1_Chat= replace(survey_data$q1_Chat,is.na(survey_data$q1_Chat),"")



survey_data$q1_sms =as.numeric(ifelse(survey_data$q1_sms == "Checked",1,0))
survey_data$q1_Twitter =as.numeric(ifelse(survey_data['q1_Twitter'] == "Checked",1,0))
survey_data$q1_Facebook =as.numeric(ifelse(survey_data$q1_Facebook == "Checked",1,0))
survey_data$q1_Chat =as.numeric(ifelse(survey_data$q1_Chat == "Checked",1,0))
survey_data$q1_Shop =as.numeric(ifelse(survey_data$q1_Shop == "Checked",1,0))
survey_data$q1_Staff =as.numeric(ifelse(survey_data$q1_Staff == "Checked",1,0))
survey_data$q1_MpesaAgent =as.numeric(ifelse(survey_data$q1_MpesaAgent == "Checked",1,0))
survey_data$q1_Friend =as.numeric(ifelse(survey_data$q1_Friend == "Checked",1,0))
survey_data$q1_DIY =as.numeric(ifelse(survey_data$q1_DIY == "Checked",1,0))
survey_data$q1_Call_Back =as.numeric(ifelse(survey_data$q1_Call_Back == "Checked",1,0))

survey_data$Reason_for_Calling_identity <-survey_data$Reason_for_calling
survey_data$Reason_for_Calling_identity = as.character(survey_data$Reason_for_Calling_identity)
survey_data$Reason_for_Calling_identity = replace(survey_data$Reason_for_Calling_identity,survey_data$Reason_for_Calling_identity=='(5) Calling on behalf (Individual)','1')
survey_data$Reason_for_Calling_identity = replace(survey_data$Reason_for_Calling_identity,survey_data$Reason_for_Calling_identity=='(6) Calling on behalf (M-PESA Agent)','1')
survey_data$Reason_for_Calling_identity = replace(survey_data$Reason_for_Calling_identity,survey_data$Reason_for_Calling_identity=='(7) Prank caller','2')
survey_data$Reason_for_Calling_identity = replace(survey_data$Reason_for_Calling_identity,survey_data$Reason_for_Calling_identity=='[1] Inquiry about a Safaricom Product & Services (Other than tariff & Billing)','3')
survey_data$Reason_for_Calling_identity = replace(survey_data$Reason_for_Calling_identity,survey_data$Reason_for_Calling_identity=='[2] Inquiry about tariff / price on a Product & Service','3')
survey_data$Reason_for_Calling_identity = replace(survey_data$Reason_for_Calling_identity,survey_data$Reason_for_Calling_identity=='[3] Product & Service Billing Inquiry  ','3')
survey_data$Reason_for_Calling_identity = replace(survey_data$Reason_for_Calling_identity,survey_data$Reason_for_Calling_identity=='[4] A specific problem that I wanted resolved','4')
survey_data$Reason_for_Calling_identity = replace(survey_data$Reason_for_Calling_identity,survey_data$Reason_for_Calling_identity=="",'0')

survey_data$Reason_for_Calling_identity =as.numeric(survey_data$Reason_for_Calling_identity)

#NUMBER OF MSISDN SURVEYED
surveyed = survey_data %>% group_by(Date.Created ) %>% summarise (n_distinct(MSISDN))
#COUNT OF THOSE REACHED per Day
reached =survey_data %>% group_by(Date.Created ) %>% filter(Reached == "Yes") %>% summarise (n_distinct(MSISDN))

#REACHED IN HOW MANY TRIALS
reached_nu_trials =survey_data %>% group_by(Number_trials) %>% filter(Reached == "Yes") %>% summarise (n_distinct(MSISDN))

#reached and gave a consent by date
reached_consent_date =survey_data %>% group_by(Date.Created ) %>% 
            filter(Reached == "Yes",Do.I.have.your.permission.to.proceed.== "Yes")%>% 
            summarise (n_distinct(MSISDN))
#reached and consent
reached_consent =survey_data %>% group_by(Do.I.have.your.permission.to.proceed. ) %>% 
  filter(Reached == "Yes")%>% 
  summarise (n_distinct(MSISDN))

#reason for not giving consent
no_consent_reason =survey_data %>% group_by(Reason_for_no_permission) %>% 
  filter(Reached == "Yes", Do.I.have.your.permission.to.proceed.== "No")%>% 
  summarise (n_distinct(MSISDN))


#the segment of the customers
segments = survey_data %>% group_by(Segment) %>%
  filter(Reached == "Yes")%>%
           summarise(n_distinct(MSISDN))

#the tariff
tariff = survey_data %>% group_by(Tariff) %>%
  filter(Reached == "Yes")%>%
  summarise(n_distinct(MSISDN))

#irrespective whether it was a problem or inquiry why did you call
Why_did_you_call= survey_data %>% group_by(Why_did_you_call) %>% 
  filter(Reached == "Yes",Do.I.have.your.permission.to.proceed.== "Yes",Reason_for_Calling_identity>=3)%>% 
  summarise (Number =n_distinct(MSISDN))

# reached and gave a consent[why did they call]

reason_for_calling =survey_data %>% group_by(Reason_for_calling ) %>% 
  filter(Reached == "Yes",Do.I.have.your.permission.to.proceed.== "Yes")%>% 
  summarise (Number =n_distinct(MSISDN))

ggplot(reason_for_calling,aes(Reason_for_calling,Number))+ geom_bar(stat = "identity")

#reached and gave a consent[reason for calling have specific problem]

what_problem =survey_data %>% group_by(What_problem) %>% 
  filter(Reached == "Yes",Do.I.have.your.permission.to.proceed.== "Yes",Reason_for_calling =="[4] A specific problem that I wanted resolved")%>% 
  summarise (Number =n_distinct(MSISDN))

#why prank callers
prank_caller= survey_data %>% group_by(Prank.caller.Issues) %>% 
  filter(Reached == "Yes",Do.I.have.your.permission.to.proceed.== "Yes",Reason_for_Calling_identity==2)%>% 
  summarise (Number =n_distinct(MSISDN))



#before contacting safcom did you try on your own
DOY =survey_data %>% group_by(Do_it_yourself) %>% 
  filter(Reached == "Yes",Do.I.have.your.permission.to.proceed.== "Yes",Reason_for_Calling_identity>=3)%>% 
  summarise (Number =n_distinct(MSISDN))

#what did you do[do it yourself]
What_did_you_do_yourself =survey_data %>% group_by(What_did_you_do_yourself) %>% 
  filter(Reached == "Yes",Do.I.have.your.permission.to.proceed.== "Yes",Reason_for_Calling_identity>=3,Do_it_yourself =="Yes")%>% 
  summarise (Number =n_distinct(MSISDN))

#did you finally resolve the issue
issue_resolved =survey_data %>% group_by(Did_you_resolve_issue) %>% 
  filter(Reached == "Yes",Do.I.have.your.permission.to.proceed.== "Yes",Reason_for_Calling_identity>=3)%>% 
  summarise (Number =n_distinct(MSISDN))

#if you resolved how did you resolve

issue_resolved =survey_data %>% group_by(Did_you_resolve_issue) %>% 
  filter(Reached == "Yes",Do.I.have.your.permission.to.proceed.== "Yes",Reason_for_Calling_identity>=3)%>% 
  summarise (Number =n_distinct(MSISDN))


multfreqtable = function(data, question.prefix) {
  # Find the columns with the questions
  a = grep(question.prefix, names(data))
  # Find the total number of responses
  b = sum(data[, a] != 0)
  # Find the totals for each question
  d = colSums(data[, a] != 0)
  # Find the number of respondents
  e = sum(rowSums(data[,a]) !=0)
  # d + b as a vector. This is your overfall frequency 
  f = as.numeric(c(d, b))
  data.frame(question = c(names(d), "Total"),
             freq = f,
             percent = (f/b)*100,
             percentofcases = (f/e)*100 )
}

multfreqtable(survey_data,"q2")

multfreqtable(survey_data,"q1")

###how many has the agents done
RA =survey_data %>% group_by(RA) %>% 
  filter(Reached == "Yes",Do.I.have.your.permission.to.proceed.== "Yes")%>% 
  summarise (Number =n_distinct(MSISDN))





####################
to_numeric <- function(df,col_name)
{
  df[col_name] = as.numeric(ifelse(df[col_name] == "Checked",1,0))
}

cols = c("q1a","qlb","q1c","q1d","q1e","q1f")

for( item in cols)
{
  to_numeric(df = survey_data,item)
  print(item)
}








