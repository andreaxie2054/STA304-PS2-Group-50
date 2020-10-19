# STA304-PS2-Group-50
Group members: Ning Xie, Yuxuan Ju, Rong Zhang

knitr::opts_chunk$set(echo = FALSE)

## Title of your Report
Evaluation on Canadians' Feelings About Life

# Name(s) of Author(s) 
NING XIE (1005120691)
RONG ZHANG (1004726932)
YUXUAN JU (1004940521)

# Date
2020-10-18

## Abstract

In this report, we analyze how factors like family income, health, and minority affect people's feelings about life through a telephone survey on family in 2017 conducted by General Social Survey. We find out people with higher family income and higher self-rated health tend to have higher feelings about life, and males have lower feelings about lift in general.

## Introduction

The general social survey on Family in 2017 that collects data via telephone is a sample survey with cross-sectional design, where the target population is all non-institutionalized citizens living in ten provinces of Canada older than 15 years old. The purpose of this survey is to monitor changes in living conditions and well-being of Canadians in the long run by gathering data on social trends, and also provide information for social policy to adjust and improve. In our model, we focus on how the family income, self-rated health and sex affect the well-being of people and find out the significance of the relationship between peoples??? feelings about life each factor. While concluding, point out how this survey can improve in the future. 

## Data

#### Workspace set-up ####
library(janitor)
library(tidyverse)
library(brms)

# Load the data dictionary and the raw data and correct the variable names
raw_data <- read_csv("familydata.csv")
dict <- read_lines("gss_dict.txt", skip = 18) # skip is because of preamble content
# Now we need the labels because these are the actual responses that we need
labels_raw <- read_file("gss_labels.txt")


#### Set-up the dictionary ####
# What we want is a variable name and a variable definition
variable_descriptions <- as_tibble(dict) %>% 
  filter(value!="}") %>% 
  mutate(value = str_replace(value, ".+%[0-9].*f[ ]{2,}", "")) %>% 
  mutate(value = str_remove_all(value, "\"")) %>% 
  rename(variable_description = value) %>% 
  bind_cols(tibble(variable_name = colnames(raw_data)[-1]))
 
# Now we want a variable name and the possible values
labels_raw_tibble <- as_tibble(str_split(labels_raw, ";")[[1]]) %>% 
  filter(row_number()!=1) %>% 
  mutate(value = str_remove(value, "\nlabel define ")) %>% 
  mutate(value = str_replace(value, "[ ]{2,}", "XXX")) %>% 
  mutate(splits = str_split(value, "XXX")) %>% 
  rowwise() %>% 
  mutate(variable_name = splits[1], cases = splits[2]) %>% 
  mutate(cases = str_replace_all(cases, "\n [ ]{2,}", "")) %>%
  select(variable_name, cases) %>% 
  drop_na()

# Now we have the variable name and the different options e.g. age and 0-9, 10-19, etc.
labels_raw_tibble <- labels_raw_tibble %>% 
  mutate(splits = str_split(cases, "[ ]{0,}\"[ ]{0,}"))

# The function sets up the regex (I know, I know, but eh: https://xkcd.com/208/)
add_cw_text <- function(x, y){
  if(!is.na(as.numeric(x))){
    x_new <- paste0(y, "==", x,"~")
  }
  else{
    x_new <- paste0("\"",x,"\",")
  }
  return(x_new)
}

# The function will be in the row, but it'll get the job done
cw_statements <- labels_raw_tibble %>% 
  rowwise() %>% 
  mutate(splits_with_cw_text = list(modify(splits, add_cw_text, y = variable_name))) %>% 
  mutate(cw_statement = paste(splits_with_cw_text, collapse = "")) %>% 
  mutate(cw_statement = paste0("case_when(", cw_statement,"TRUE~\"NA\")")) %>% 
  mutate(cw_statement = str_replace(cw_statement, ",\"\",",",")) %>% 
  select(variable_name, cw_statement)
# So for every variable we now have a case_when() statement that will convert 
# from the number to the actual response.

# Just do some finally cleanup of the regex.
cw_statements <- 
  cw_statements %>% 
  mutate(variable_name = str_remove_all(variable_name, "\\r")) %>% 
  mutate(cw_statement = str_remove_all(cw_statement, "\\r"))


#### Apply that dictionary to the raw data ####
# Pull out a bunch of variables and then apply the case when statement for the categorical variables
gss <- raw_data %>% 
  select(CASEID, 
         agedc, 
         achd_1c, 
         achdmpl, 
         totchdc, 
         acu0c,
         agema1c,
         achb1c,
         rsh_131a,
         arretwk,
         slm_01, 
         sex, 
         brthcan, 
         brthfcan,
         brthmcan,
         brthmacr,
         brthprvc,
         yrarri,
         prv, 
         region, 
         luc_rst, 
         marstat, 
         amb_01, 
         vismin, 
         alndimmg,
         bpr_16, 
         bpr_19,
         ehg3_01b, 
         odr_10, 
         livarr12, 
         dwelc, 
         hsdsizec,
         brthpcan,
         brtpprvc, 
         visminpr,
         rsh_125a, 
         eop_200,
         uhw_16gr,
         lmam_01, 
         acmpryr,
         srh_110,
         srh_115,
         religflg, 
         rlr_110,
         lanhome, 
         lan_01,
         famincg2, 
         ttlincg2, 
         noc1610, 
         cc_20_1,
         cc_30_1,
         ccmoc1c,
         cor_031,
         cor_041,
         cu0rnkc,
         pr_cl,
         chh0014c,
         nochricc,
         grndpa,
         gparliv,
         evermar,
         ma0_220,
         nmarevrc,
         ree_02,
         rsh_131b,
         rto_101,
         rto_110,
         rto_120,
         rtw_300,
         sts_410,
         csp_105,
         csp_110a,
         csp_110b,
         csp_110c,
         csp_110d,
         csp_160,
         fi_110) %>% 
  mutate_at(vars(agedc:fi_110), .funs = funs(ifelse(.>=96, NA, .))) %>% 
  mutate_at(.vars = vars(sex:fi_110),
            .funs = funs(eval(parse(text = cw_statements %>%
                                      filter(variable_name==deparse(substitute(.))) %>%
                                      select(cw_statement) %>%
                                      pull()))))

# Fix the names
gss <- gss %>% 
  clean_names() %>% 
  rename(age = agedc,
         age_first_child = achd_1c,
         age_youngest_child_under_6 = achdmpl,
         total_children = totchdc,
         age_start_relationship = acu0c,
         age_at_first_marriage = agema1c,
         age_at_first_birth = achb1c,
         distance_between_houses = rsh_131a,
         age_youngest_child_returned_work = arretwk,
         feelings_life = slm_01,
         sex = sex,
         place_birth_canada = brthcan,
         place_birth_father = brthfcan,
         place_birth_mother = brthmcan,
         place_birth_macro_region = brthmacr,
         place_birth_province = brthprvc,
         year_arrived_canada = yrarri,
         province = prv,
         region = region,
         pop_center = luc_rst,
         marital_status = marstat,
         aboriginal = amb_01,
         vis_minority = vismin,
         age_immigration = alndimmg,
         landed_immigrant = bpr_16,
         citizenship_status = bpr_19,
         education = ehg3_01b,
         own_rent = odr_10,
         living_arrangement = livarr12,
         hh_type = dwelc,
         hh_size = hsdsizec,
         partner_birth_country = brthpcan,
         partner_birth_province = brtpprvc,
         partner_vis_minority = visminpr,
         partner_sex = rsh_125a,
         partner_education = eop_200,
         average_hours_worked = uhw_16gr,
         worked_last_week = lmam_01,
         partner_main_activity = acmpryr,
         self_rated_health = srh_110,
         self_rated_mental_health = srh_115,
         religion_has_affiliation = religflg,
         regilion_importance = rlr_110,
         language_home = lanhome,
         language_knowledge = lan_01,
         income_family = famincg2,
         income_respondent = ttlincg2,
         occupation = noc1610,
         childcare_regular = cc_20_1,
         childcare_type = cc_30_1,
         childcare_monthly_cost = ccmoc1c,
         ever_fathered_child = cor_031,
         ever_given_birth = cor_041,
         number_of_current_union = cu0rnkc,
         lives_with_partner = pr_cl,
         children_in_household = chh0014c,
         number_total_children_intention = nochricc,
         has_grandchildren = grndpa,
         grandparents_still_living = gparliv,
         ever_married = evermar,
         current_marriage_is_first = ma0_220,
         number_marriages = nmarevrc,
         religion_participation = ree_02,
         partner_location_residence = rsh_131b,
         full_part_time_work = rto_101,
         time_off_work_birth = rto_110,
         reason_no_time_off_birth = rto_120,
         returned_same_job = rtw_300,
         satisfied_time_children = sts_410,
         provide_or_receive_fin_supp = csp_105,
         fin_supp_child_supp = csp_110a,
         fin_supp_child_exp = csp_110b,
         fin_supp_lump = csp_110c,
         fin_supp_other = csp_110d,
         fin_supp_agreement = csp_160,
         future_children_intention = fi_110) 

#### Clean up ####
gss <- gss %>% 
  mutate_at(vars(age:future_children_intention), 
            .funs = funs(ifelse(.=="Valid skip"|.=="Refusal"|.=="Not stated", "NA", .))) 

gss <- gss %>% 
  mutate(is_male = ifelse(sex=="Male", 1, 0)) 

gss <- gss %>% 
  mutate_at(vars(fin_supp_child_supp:fin_supp_other), .funs = funs(case_when(
    .=="Yes"~1,
    .=="No"~0,
    .=="NA"~as.numeric(NA)
  )))

main_act <- raw_data %>% 
  mutate(main_activity = case_when(
    mpl_105a=="Yes"~ "Working at a paid job/business",
    mpl_105b=="Yes" ~ "Looking for paid work",
    mpl_105c=="Yes" ~ "Going to school",
    mpl_105d=="Yes" ~ "Caring for children",
    mpl_105e=="Yes" ~ "Household work", 
    mpl_105i=="Yes" ~ "Other", 
    TRUE~ "NA")) %>% 
  select(main_activity) %>% 
  pull()

age_diff <- raw_data %>% 
  select(marstat, aprcu0c, adfgrma0) %>% 
  mutate_at(.vars = vars(aprcu0c:adfgrma0),
            .funs = funs(eval(parse(text = cw_statements %>%
                                      filter(variable_name==deparse(substitute(.))) %>%
                                      select(cw_statement) %>%
                                      pull())))) %>% 
  mutate(age_diff = ifelse(marstat=="Living common-law", aprcu0c, adfgrma0)) %>% 
  mutate_at(vars(age_diff), .funs = funs(ifelse(.=="Valid skip"|.=="Refusal"|.=="Not stated", "NA", .))) %>% 
  select(age_diff) %>% 
  pull()

gss <- gss %>% mutate(main_activity = main_act, age_diff = age_diff)

# Change some from strings into numbers
gss <- gss %>% 
  rowwise() %>% 
  mutate(hh_size = str_remove(string = hh_size, pattern = "\\ .*")) %>% 
  mutate(hh_size = case_when(
    hh_size=="One" ~ 1,
    hh_size=="Two" ~ 2,
    hh_size=="Three" ~ 3,
    hh_size=="Four" ~ 4,
    hh_size=="Five" ~ 5,
    hh_size=="Six" ~ 6
  )) 

gss <- gss %>% 
  rowwise() %>% 
  mutate(number_marriages = str_remove(string = number_marriages, pattern = "\\ .*")) %>% 
  mutate(number_marriages = case_when(
    number_marriages=="No" ~ 0,
    number_marriages=="One" ~ 1,
    number_marriages=="Two" ~ 2,
    number_marriages=="Three" ~ 3,
    number_marriages=="Four" ~ 4
  )) 

gss <- gss %>% 
  rowwise() %>% 
  mutate(number_total_children_known = ifelse(number_total_children_intention=="Don't know"|number_total_children_intention=="NA", 0, 1)) %>% 
  mutate(number_total_children_intention = str_remove(string = number_total_children_intention, pattern = "\\ .*")) %>% 
  mutate(number_total_children_intention = case_when(
    number_total_children_intention=="None" ~ 0,
    number_total_children_intention=="One" ~ 1,
    number_total_children_intention=="Two" ~ 2,
    number_total_children_intention=="Three" ~ 3,
    number_total_children_intention=="Four" ~ 4,
    number_total_children_intention=="Don't" ~ as.numeric(NA)
  )) 

write_csv(gss, "gss.csv")

gss1 <- data.frame(gss$caseid, gss$feelings_life, gss$education, gss$income_family, gss$sex, gss$age, gss$vis_minority, gss$self_rated_health)

gss2 <- na.omit(gss1)
gss2 <- gss2[!gss2$gss.feelings_life == "NA",]
gss2 <- gss2[!gss2$gss.education == "NA",]
gss2 <- gss2[!gss2$gss.vis_minority == "NA",]
gss2 <- gss2[!gss2$gss.self_rated_health == "NA",]

gss3 <- data.frame(gss$caseid, gss$feelings_life, gss$income_family, gss$sex, gss$self_rated_health)
gss3 <- gss3[!gss3$gss.self_rated_health == "NA",]
gss3 <- na.omit(gss3)
#trying to get rid of NAs. gss1 is a dataset with only 8 varibles in which we are interested. gss2 is a dataset with these 8 variables without any NAs. gss3 is a dataset with only the 5 variables which the final model uses, and get rid of the NAs.

Mean value, maximum, and minimum values of the variable "feeelings of life".

mean(gss3$gss.feelings_life)
max(gss3$gss.feelings_life)
min(gss3$gss.feelings_life)
#get more information about the variable feelings_life.

After cleaning the original data with 20602 observations and 461 variables via deleting the majority of NAs and selecting the variables we want to analyze. The data reserves 20318 observations with response variable feelings_life and explanatory variables income_family, sex, vis_minority and self_rated_health. While processing these data, it has so many NAs which caused by the methodology placed on the survey -- telephone survey, this makes the results not excellent by accuracy since they have some residuals, and also can not analyze the impact of dependent variables on the response variable comprehensively. However, the data has a large sample size even after the cleaning process, besides both the categorical and numerical variables are designed appropriately for analyzing and plotting. By looking deeper into the data, we can find that the mean value of peoples??? feelings about life as a whole is 8.094497, with the range [0, 10].

## Model

Our final model

mod1 <- lm(gss.feelings_life ~ gss.income_family + gss.sex + gss.self_rated_health, data = gss3)
summary(mod1)

#model with multiple variables

Here We build linear regression models with response variable "feelings of life" and predictor variables "self rated health", "income of family", "sex" separately, in order to check our multivariable model.

model1 <- lm(gss.feelings_life ~ gss.self_rated_health, data = gss3)
model2 <- lm(gss.feelings_life ~ gss.income_family, data = gss1)
model3 <- lm(gss.feelings_life ~ gss.sex, data = gss1)
summary(model1)
summary(model2)
summary(model3)

#linear regression for single predictor variable

Here we build a normal QQ plot for our model to see how the data fit with the model. We can see that it fits well in the middle part, but does not fit at the beginning and end of the line.

qqnorm(rstudent(mod1))
qqline(rstudent(mod1))

#Normal qq plot of the model

Number of leverage points

h <- hatvalues(mod1)
threshold <- 2*(length(mod1$coefficients)/nrow(gss3))
w <- which(h > threshold)
count(gss3[w,])

Number of outliers

r <- rstandard(mod1)
summary(r)
which(r >= 2 | r <= -2)
b <- which(r >= 2 | r <= -2)
count(gss3[b,])

We build a bayesian linear model to check our multivariable model.

baymodel <- brm(gss.feelings_life ~ gss.income_family + gss.sex + gss.self_rated_health, data = gss3)
summary(baymodel)

mcmc_plot(baymodel,type="hist")
mcmc_plot(baymodel,type="trace")

posterior <- as.array(baymodel)
bayesplot::mcmc_intervals(posterior)

We use R programming to build the model.
Response variables: feelings_life  (Feelings about life as a whole).
Predictors: income_family, Sex, Self_rated_health.

We use the multiple linear regression model to explore the factors affecting peoples' feelings about their life as a whole. We fit three predictor variables, family wealth, sex and self-rated health into the linear model. To check the model, we check the p-value for each factor in the model. P-value means the probability of obtaining a sample that provides stronger evidence against the hypothesized value for the slope, so we want a small p-value. We also check the R square to check the percentage of variation of the sample that can be explained by the model. We also adjust R square with the number of factors in the model, since adding factors to the model will increase R square whether or not these factors are useful. There are no divergences to plot that can get from mcmc plot with trace in Bayesian model.

For diagnostic issues, we find outliers and leverage points for the data. We find 837 leverage points and 848 outliers. Even though it seems there are a lot of outliers and leverage points, we do not need to worry it may affect model fit too much because we have 20318 observations and outliers and leverages are a small proportion of the whole dataset. Also, we do not want to get rid of these outliers and leverage points, because peoples??? wealth, feelings about life and self-rated health can be very different from others in some cases. We consider that these samples are also representative of the whole population, so we include them in our model.

## Results

From the estimates of slope for the family income factors, we can see that there is a general trend that people with higher family income tend to have higher feelings about life, though the relationship is not very significant for the people with a family income of $125,000 and more. It is reasonable when we consider the reality because consumption can improve people's life situations, however, people with large amounts of wealth may feel more money does not add much to their feelings about life.
Also, we find that people with higher self-rated health give a higher comment on their life. By looking at the p values, we can see that the relationship is less significant for people with good health, and the least significant for people with fair health. We can fit this result in reality. People who rate their health as good or fair may not feel their health affecting their lives much. Such people would define their health as good or fair as long as they don't have a disease or obvious health problems.
We can find that the R square value is not very large, so there is still a large percentage of  the sample variance not being explained by the model, which is a weakness. Because of this, we may not trust much about the estimated values of the slopes. However, the model does a fair job on distinguishing whether there is a positive relationship or a negative relationship between these factors and people's feelings about life. Also, p values are small enough, which means that the relationships are significant for most of the factors.

## Discussion


The 2017 GSS  is a sample survey with a cross-sectional design. In this survey, the target population is all non-institutionalized people living in the ten provinces of Canada at the age of fifteen or older. The frame population is a combination of the lists of telephone numbers in use and the list of all dwellings within the ten provinces. The sample population is all households that include at least one person at the age of fifteen or older, with their telephone numbers available.
Data of the survey was collected via computer assisted telephone interviews (CATI). Interviewers would receive calls from 9:00 a.m. to 9:30 p.m. Mondays to Fridays and from 10:00 a.m. to 5:00 p.m. on Saturdays and 1:00 p.m. to 9:00 p.m. on Sundays.
Telephone interviews are convenient, quick, and low-cost. However, it contains lots of questions. First of all, telephone interviewing may make interviewers impatient. This ends up having lots of 'NA's in the data collected. Second,  The design of the survey can cause the sample not representative of the whole population, because those who were willing to answer all the questions(which is a lot) may tend to have more free time than those who were not. The questionnaire brings selection bias. Third, the overall response rate for the 2017 GSS was 52.4%, which means only half of the interviewers gave a response. 
To deal with the non-response, adjustments are set in three stages: adjustments were made for complete non-response independently within each stratum in the first stage, non-response with auxiliary information from sources available to Statistics Canada is adjusted in the second stage, and partial non-response is adjusted in the third stage.

# Weaknesses

1. The survey is done through telephone and contains lots of questions, which may make interviewees inpatient. This ends up having lots of 'NA's in the data collected.
2.The response rate is only 52.4%.

# Next Steps

The general social survey is a survey about Canadian citizens¡¯ well-beings. They collect information on large
amount of aspects, such as the current age of their first child. For this survey, there is a trade-off between
having lots of information and having interviewees being patient and answering to each question carefully.
We find many ¡°NA¡±s in the dataset. In order to build the model, we have to delete these observations even
when they have just one ¡°NA¡± in the factor we are interested about. This causes a large amount of data
being eliminated, which is a loss.
We may improve on data collection to decrease the ¡°NA¡± in the dataset. For the project we are doing, we
don¡¯t really need these information because our objective is to study the factors affecting Canadian people¡¯s
feelings of life. Therefore, we may want to only collect people¡¯s information about the factors may affecting
feelings of life in steads of collecting lots of unnecessary information. In this way, our questionnaire would
be lot shorter, so it would be easier for people to remain patient to answer carefully and honestly to all the
questions.

## References
1) gss_cleaning.R, Rohan Alexander and Sam Caetano(7 October 2020),License: MIT
2) General Social Survey Cycle 31 : Families Public Use Microdata File Documentation and User???s Guide, Diversity and Sociocultural Statistics(April 2020)
3) General Social Survey (Cycle 31) Families, Statistics Canada

# link to GitHub repo
https://github.com/andreaxie2054/STA304-PS2-Group-50.git

