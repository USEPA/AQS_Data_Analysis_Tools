# Analyze collocated PM2.5 precision data by PQAO for selected methods
# Current configuration is to analyze data for Thermo 5014i monitors 
# across multiple PQAOs. Data retrieved from AQS API
# Authors: D. Garver and R. Brown, US EPA Region 4

library(httr)
library(jsonlite)
library(ggplot2)
library(dplyr)
library(lubridate)

####### Program run variables - update before running ########
setwd() # Insert local working directory
my_pqaos = c("0021", "1328", "0513", "0730", "0764", "1127") # Insert PQAO codes of interest
data_mart_api_user = ""
data_mart_api_password = ""
base_url<-"https://aqs.epa.gov/data/api/"
my_param<-"88101"
begin_date<-"20170101"
end_date<-"20181231"


# Query data for selected PQAOs
collo_by_pqao = tibble() #Initialize variable

for( pqao in my_pqaos){
  
  request_type<-"qaCollocatedAssessments/byPQAO?"
  
  my_api_query<-paste0(base_url,request_type,"email=",data_mart_api_user,
                       "&key=",data_mart_api_password,"&param=",my_param,
                       "&bdate=",begin_date,"&edate=",end_date,
                       "&pqao=",pqao)
  
  get_data<-GET(my_api_query)
  get_data_txt<-content(get_data,"text")
  flat_data = fromJSON(get_data_txt, flatten = TRUE)
  collo_by_pqao = collo_by_pqao %>% 
    bind_rows(as_data_frame(flat_data$Data, colClasses="character"))
  
}

# Convert data types
collo_by_pqao <- collo_by_pqao %>%
  mutate( site_code = paste0(state_code, "-", county_code, "-", site_number),
          date = ymd(assessment_date),
          year = year(date),
          quarter = quarter(date),
          percent_difference = as.numeric(percent_difference),
          assessment_value = as.numeric(assessment_value),
          primary_value = as.numeric(primary_value),
          conc_difference = primary_value - assessment_value
  )

# Select only data where primary monitor is Thermo 5014i
my_qa_data = collo_by_pqao %>%
  filter(primary_method_code == "183")

# Select Florida data
fl_qa_data = collo_by_pqao %>% filter(pqao_code == "1328")

my_year<-2017

my_qa_data %>% filter(year==my_year) %>%
  ggplot(aes(x=primary_value,y=assessment_value,color=pqao))+
  geom_abline(intercept=0, slope=1, alpha=.6, size=1.2,linetype="dashed")+
  geom_point(size=4, alpha = .5)+
  facet_wrap(~local_site_name#,scales="free"
  )+
  geom_text(aes(0,20,
                label=paste0("collocated method - ",colloctated_method_code,"\n",
                             collocated_method)),
            color="dark grey",
            hjust=0)+
  geom_text(aes(8,2,
                label=paste0("primary method - ", primary_method_code,"\n",
                             primary_method)),
            color="dark grey",
            hjust=0)+
  ggtitle(paste("Collocated PM2.5 comparison -",my_year))+
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5))


# Boxplot of absolute differences

my_qa_data %>% filter(year==my_year) %>%
  ggplot( aes(#x = pqao, 
              y = conc_difference, fill = pqao)) +
  geom_boxplot() +
  theme_bw() + 
  xlab('') + ylab('Concentration Difference: Primary - Collocated (ug/m3)') +
  ggtitle(paste0(my_year, ' PQAO Comparison of Thermo 5014i Precision')) +
  geom_hline(yintercept = 0, linetype='dashed')

# Boxplot by method for all pqaos
collo_by_pqao %>% filter(year==my_year) %>%
  ggplot( aes(x = primary_method_code, y = conc_difference, fill = primary_method)) +
  geom_boxplot() +
  theme_bw() + 
  xlab('') + ylab('Concentration Difference: Primary - Collocated (ug/m3)') +
  ggtitle(paste0(my_year, ' Comparison of PM2.5 Method Precision')) +
  geom_hline(yintercept = 0, linetype='dashed')

colloc_methods<-collo_by_pqao%>%group_by(year,data_local_site_name,data_pqao,data_primary_method,
                                        data_collocated_method)%>%
  summarize(number_samples=n(),
            mean_percent_d=mean(data_percent_difference),
            median_percent_d=median(data_percent_difference),
            std_percent_d=sd(data_percent_difference),
            min_percent_d=min(data_percent_difference),
            max_percent_d=max(data_percent_difference))





