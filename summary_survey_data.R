setwd("/Users/dgitahi/documents/survey")
library(dplyr)
library(ggplot2)
survey_data=read.csv("survey_data.csv")
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


colnames(survey_data)[24] = "q2a" #Ignore
colnames(survey_data)[25] = "q2b" #Buy_another_line
colnames(survey_data)[26] = "q2c" #Call_help_line
colnames(survey_data)[27] = "q2d" #Vist_Retail_center
colnames(survey_data)[28] = "q2e" #Contact_safaricom_staff
colnames(survey_data)[29] = "q2f" #Vist_Dealer
colnames(survey_data)[30] = "q2g" #Call_a_friend



survey_data$q2a =as.numeric(ifelse(survey_data$q2a == "Checked",1,0))
survey_data$q2b =as.numeric(ifelse(survey_data$q2b == "Checked",1,0))
survey_data$q2c =as.numeric(ifelse(survey_data$q2c == "Checked",1,0))
survey_data$q2d =as.numeric(ifelse(survey_data$q2d == "Checked",1,0))
survey_data$q2e =as.numeric(ifelse(survey_data$q2e == "Checked",1,0))
survey_data$q2f =as.numeric(ifelse(survey_data$q2f == "Checked",1,0))
survey_data$q2g =as.numeric(ifelse(survey_data$q2g == "Checked",1,0))



colnames(survey_data)[32] = "q1a" #sms
colnames(survey_data)[33] = "q1b" #Twitter
colnames(survey_data)[34] = "q1c" #Facebook
colnames(survey_data)[35] = "q1d" #Chat
colnames(survey_data)[36] = "q1e" #Shop_care_Desk
colnames(survey_data)[37] = "q1f" #Safaricom Staff
colnames(survey_data)[38] = "q1g" #MPESA.agent.Dealer
colnames(survey_data)[39] = "q1h" #Friend_or_Family
colnames(survey_data)[40] = "q1i" #DIY
colnames(survey_data)[41] = "q1j" #Called_back_Callcenter

survey_data$q1c=replace(survey_data$q1c,is.na(survey_data$q1c),"")
survey_data$q1d= replace(survey_data$q1d,is.na(survey_data$q1d),"")



survey_data$q1a =as.numeric(ifelse(survey_data$q1a == "Checked",1,0))
survey_data$q1b =as.numeric(ifelse(survey_data['q1b'] == "Checked",1,0))
survey_data$q1c =as.numeric(ifelse(survey_data$q1c == "Checked",1,0))
survey_data$q1d =as.numeric(ifelse(survey_data$q1d == "Checked",1,0))
survey_data$q1e =as.numeric(ifelse(survey_data$q1e == "Checked",1,0))
survey_data$q1f =as.numeric(ifelse(survey_data$q1f == "Checked",1,0))
survey_data$q1g =as.numeric(ifelse(survey_data$q1g == "Checked",1,0))
survey_data$q1h =as.numeric(ifelse(survey_data$q1h == "Checked",1,0))
survey_data$q1i =as.numeric(ifelse(survey_data$q1i == "Checked",1,0))
survey_data$q1j =as.numeric(ifelse(survey_data$q1j == "Checked",1,0))

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








