library(readxl)
library(readr)
library(tidyverse)
#library(dplyr)
library(tidyr)
library(magrittr)
library(data.table)
library(labelled)
#library(purrr)
#library(lubridate)
library(forcats)
library(scales)

library(tm) 

#LOAD FILES####
setwd("Data/IPEDS/")

#CVS files
listcsv <- dir(pattern = "*.csv") # creates the list of all the csv files in the directory
ldf <- list() # creates a list
for (k in 1:length(listcsv)){
  ldf[[k]] <- read_csv(listcsv[k])
}

list(
  admissions_and_test_scores<-ldf[[1]],
  completitions<-ldf[[2]],
  employee<-ldf[[3]],
  fall_enrollment<-ldf[[4]],
  fall_enrollment_student.faculty.ratio<- ldf[[5]],
  graduation_rates<- ldf[[6]],
  name_of_university<- ldf[[7]],
  student_charges<-ldf[[8]],
  institutional_characteristics<-ldf[[9]],
  outcome_measures<-ldf[[10]],
  staff_race.gender<-ldf[[11]],
  staff_number.salary<- ldf[[12]])

#EXCEL
listxlsx <- dir(pattern = "*.xlsx")
ldf_dic<- list()
for (k in 1:length(listxlsx)){
  ldf_dic[[k]] <- read_excel(listxlsx[k], sheet = "varlist")
}

list(
  admissions_and_test_scores_dic<-ldf_dic[[1]], 
  completitions_dic<-ldf_dic[[2]],
  employee_dic<-ldf_dic[[3]],
  fall_enrollment_dic<-ldf_dic[[4]],
  fall_enrollment_student.faculty.ratio_dic<- ldf_dic[[5]],
  graduation_rates_dic<- ldf_dic[[6]],
  name_of_university_dic<- ldf_dic[[7]],
  student_charges_dic<-ldf_dic[[8]],
  institutional_characteristics_dic<-ldf_dic[[9]],
  outcome_measures_dic<-ldf_dic[[10]],
  staff_number.salary_dic<-ldf_dic[[12]],
  staff_race.gender_dic<- ldf_dic[[11]])

####Cleaning and data formatting ####
#Drop columns with type of data indicator

#lapply(ldf, function (y) {select(y, -starts_with("X"))})


admissions_and_test_scores%<>%
  select(-starts_with("X"))
completitions%<>%
  select(-starts_with("X"))
employee%<>%
  select(-starts_with("X"))
fall_enrollment %<>%
  select(-starts_with("X"))
fall_enrollment_student.faculty.ratio%<>%
  select(-starts_with("X"))
graduation_rates%<>%
  select(-starts_with("X"))
name_of_university%<>%
  select(-starts_with("X"))
student_charges%<>%
  select(-starts_with("X"))
institutional_characteristics%<>%
  select(-starts_with("X"))
outcome_measures%<>%
  select(-starts_with("X"))
staff_number.salary%<>%
  select(-starts_with("X"))
staff_race.gender%<>%
  select(-starts_with("X"))

#label columns
var_label(admissions_and_test_scores)<-admissions_and_test_scores_dic$varTitle
var_label(completitions)<-completitions_dic$varTitle
var_label(employee)<-employee_dic$varTitle
var_label(fall_enrollment)<-fall_enrollment_dic$varTitle
var_label(fall_enrollment_student.faculty.ratio)<-fall_enrollment_student.faculty.ratio_dic$varTitle
var_label(graduation_rates)<-graduation_rates_dic$varTitle
var_label(name_of_university)<-name_of_university_dic$varTitle
var_label(student_charges)<-student_charges_dic$varTitle
var_label(institutional_characteristics)<-institutional_characteristics_dic$varTitle
var_label(outcome_measures)<-outcome_measures_dic$varTitle
var_label(staff_number.salary)<-staff_number.salary_dic$varTitle
var_label(staff_race.gender)<-staff_race.gender_dic$varTitle



#IEGOR VERSION ####
# admissions_and_test_scores <- admissions_and_test_scores[,-grep(pattern="^X",colnames(admissions_and_test_scores))]
# ad_data_colnames<-colnames(admissions_and_test_scores)
# setnames(admissions_and_test_scores, old = ad_data_colnames, new = admissions_and_test_scores_dic$varTitle)
# colnames(admissions_and_test_scores) <- colnames(admissions_and_test_scores) %>% tolower() %>% gsub(pattern = " ", replacement = "_")
# admissions_and_test_scores <- admissions_and_test_scores %>% transmute(math25 = sat_math_25th_percentile_score)


#Assign UNIT ID code to the top universities ####
top_univ_THE <- read_xlsx("../USA_Top_universities_2017_THE.xlsx")
top_univ_QS<-read_xlsx("../USA_Top_universities_2017_QS.xlsx")

# top_check<-top_univ_THE%>%
# semi_join(name_of_university,by=c("Title"="INSTNM"),copy=FALSE)
# matched only 92 out pf 148 -> problem e.g. "Stanford University Explore" 
# in 'name_of_university' is called "Stanford University"
# str(top_check$UNITID)
# length(top_check$UNITID)


#->Reorder per university rank in USA

#Cleaning
top_univ_THE%>%
mutate(univ_name_match=Title%>%
  tolower() %>%
  removeWords(stopwords(kind = "en"))%>%
  {gsub('[[:punct:]]+','',.)})->top_univ_THE

name_of_university%>%
mutate(univ_name_match=INSTNM%>%
  iconv(from = "UTF-8", to = "ASCII//TRANSLIT") %>%
  {gsub('[[:punct:]]+','',.)}%>%#We need this code due to an special character in this data
  tolower() %>%
  removeWords(stopwords(kind = "en")))->name_of_university

#Create a match table
match_table<-top_univ_THE%>%
  left_join(name_of_university[,c(1:2,73)])%>%
  mutate(rank_USA=as.numeric(row.names(top_univ_THE)))

#%>%filter(rank_USA<=50)
#summary(match_table$UNITID)
#->With this cleaning the list was reduced only by 2 universities
# ->Now 54 instead of the previour 56 NA out of 148. We have 20 NA universities in the top.


#Application/Admission ratio 
admissions_and_test_scores%>%
  left_join(match_table)%>%
  filter(rank_USA<=50)%>%
  mutate(applic_adm_ratio=ADMSSN/APPLCN*100,
         Title=fct_reorder(Title,applic_adm_ratio),
         top_level=if_else(rank_USA<=15,"top 15",if_else(rank_USA<=30,"top 16-30","top 31-50")))%>%
  ggplot(aes(y=Title,x=applic_adm_ratio,color=top_level))+
  geom_point(size=5)+
  scale_x_continuous(labels = dollar_format(suffix = "%", prefix = ""),breaks=seq(0,100,by=5))+
  labs(x="Application/Admission",y="Universities",color="",title = "Percentage of aplications admitted")

#ADM bar
admissions_and_test_scores%>%
  left_join(match_table)%>%
  filter(rank_USA<=50)%>%
  select(2:10)%>%
  `colnames<-`(c("Sec. school GPA","Sec. school rank","Sec. school record","Completion of college-prep. program","Recommendation",
                 "Demonstration of competencies","Admission test","TOEFL","Other test"))%>%
  gather(key="admission_requirements",value="requirement")%>%
  filter(requirement==c(1,2))%>%
  mutate(requirement=factor(requirement,levels = c(1,2),labels = c("Require","Recommended")),
         admission_requirements = admission_requirements %>% fct_infreq() %>% fct_rev())%>%
  ggplot(aes(admission_requirements,fill = requirement))+
  geom_bar()+
  coord_flip()+
  scale_y_continuous(breaks=seq(0,50,by=1))+
  labs(x="Admission requirements",fill="", title = "Admission requirements")
  #Choose another color

#Institutional chracteristics:degrees offered
institutional_characteristics%>%
  left_join(match_table)%>%
  filter(rank_USA<=50)%>%
  remove_labels()%>%
  setnames(old = colnames(institutional_characteristics), new = institutional_characteristics_dic$varTitle)%>%
  select(123,122, 12:23)%>%
  mutate("Doctor's degree"=rowSums(.[12:14]))%>%
  select(-c(12:14))%>%
  mutate(top_level=if_else(rank_USA<=15,"top 15",
                           if_else(rank_USA<=30,"top 16-30","top 31-50")))->try2
#gather(c(3:12), key="Degree offered", value = "yes/no")
  
# ggplot(aes(x=INSTNM,y="Degree offered", color=top_level))+
#   geom_bar(stat = "count")+
#   labs(x="Degree offered",y="Universities",color="",title = "Degrees offered per university")
# 





