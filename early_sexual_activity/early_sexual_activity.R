##################################################################################
#####                             1 Loading the data                         #####
#####                             1.1 The packages                           #####
##################################################################################

# The following code automatically downloads the packages in case they are not 
# installed in your computer already.

if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(readstata13)) install.packages("readstata13", repos = "http://cran.us.r-project.org")

#################################################################################
#####                            1.2 Downloading the data                   #####
#################################################################################

# The data is downloaded directly from the permanent link that contains the zip file with 
# all the datasets

# 1) The "people" dataset will contain the demographic and economic data for each of the 
# members of the household, contained in the "1_BDD_ENS2018_f1_personas.dta" file

# 2) The "women" dataset will contain the data about the sexual health of women aged 10 to
# 49 years, contained in the "4_BDD_ENS2018_f2_mef.dta" file

# 3) The "behavior" dataset will contain the data about behavioral risk factors of people 
# aged 5 to 18 years, contained in the "8_BDD_ENS2018_f4_fact_riesgo.dta" file

# 4) The "house" dataset will contain the data about the house the household lives in, 
# contained in the "2_BDD_ENS2018_f1_hogar.dta" file

# We give the url a name
url <- "https://www.ecuadorencifras.gob.ec/documentos/web-inec/Estadisticas_Sociales/ENSANUT/ENSANUT_2018/BDD_ENSANUT_2018_STATA_.zip"
# We create a temporary directory
td <- tempdir()
# We create the placeholder file
tf <- tempfile(tmpdir=td, fileext = ".zip")
# We download the data into the placeholder file
download.file(url,tf)

# We get the name of the file inside the zip file that contains the demographic and
# economic data, unzip it, get the full path name of it, and finally load it

# We can use this code to look at the files contained inside the zip file
unzip(tf, list=TRUE)$Name

# We get the name of the file, get its full path, unzip it, and then load it

people.f.name <- unzip(tf, list=TRUE)$Name[2] # The people dataset is number 2
women.f.name <- unzip(tf, list=TRUE)$Name[5] # The women dataset is number 5
behavior.f.name <- unzip(tf, list=TRUE)$Name[9] # The behavior dataset is number 9
house.f.name <- unzip(tf, list=TRUE)$Name[3] # The house dataset is the number 3

people.f.path <- file.path(td, people.f.name)
women.f.path <- file.path(td, women.f.name)
behavior.f.path <- file.path(td, behavior.f.name)
house.f.path <- file.path(td, house.f.name)

unzip(tf, files=c(people.f.name, women.f.name, behavior.f.name, house.f.name), 
      exdir=td, overwrite=TRUE)

# Now, we can load the three files
people <- read.dta13(people.f.path)
women <- read.dta13(women.f.path)
behavior <- read.dta13(behavior.f.path)
house <- read.dta13(house.f.path)

#################################################################################
#####                        1.3 Extracting the variable labels             #####
#################################################################################

# As these are STATA files, the label of each of the variables is stored inside the 
# datasets, we can extract them using the following code:
data.key.people <- data.frame(variable = names(people), 
                              label = attr(people,"var.labels"))

data.key.women <- data.frame(variable = names(women), 
                             label = attr(women,"var.labels"))

data.key.behavior <- data.frame(variable = names(behavior), 
                                label = attr(behavior,"var.labels"))

data.key.house <- data.frame(variable = names(house), 
                             label = attr(house,"var.labels"))

# Let's look at the first 12 variables of the people set and their labels
head(data.key.people, 12)

# The name of each variable is assigned according to its code in the survey. For example, 
# whether a woman between 12 and 49 years old has ever had sexual intercourse can be found 
# in the variable f2_s8_803 of the women set, which corresponds to form 2, section 8, 
# question 803
summary(women$f2_s8_803)

# We can see that the whole dataset is in Spanish. This should be no surprise since the 
# official language in Ecuador is Spanish. For your better understanding, I will rename 
# every variable of interest from Spanish into English

#################################################################################
#####                       2 Data wrangling                                #####
#####                       2.1 Analyzing the structure of the data         #####
#################################################################################

# We have to first see how the dataset is structured, I wil rename some variables first
people <- people %>% mutate(household_id = id_hogar,
                            subject_id = id_per,
                            person = as.integer(persona),
                            sex = sexo,
                            mother = f1_s2_15_1,
                            father = f1_s2_14_1,
                            age = f1_s2_3_1)

levels(people$sex) <- c("male", "female")

# Let's look at the first household in our dataset. This household has 6 members, one 
# female and five males. Who is whose mother and father? We have to look at the "person", 
# "mother", and "father" variables. Person 1 and person 2 are the mother and father of 
# persons 3 to 6
people_id <- people %>% select(household_id, subject_id, person, sex, age, mother, father)
people_id %>% filter(household_id == "010150000201011")

#################################################################################
#####                       2.2 Extracting the mother's Ids                 #####
#################################################################################

# Because the final dataset will require the observation for each subject (female 
# adolescent) and their respectivemother to be in one single row, we cannot work with 
# this data set simply as it is. To use the left_join() funtion, we first need to add 
# a column with the unique Id of each person's mother. We will then use then the 
# mother's unique Id to merge the data

# We first create a separate data.frame with the mothers' Ids
mothers_id <- people_id %>% group_by(household_id) %>% 
  slice(mother) %>%  # we take only the mothers
  distinct(person, .keep_all = TRUE) %>% # we eliminate repeated observations 
  ungroup() %>% mutate(mother_id = subject_id) %>% select(household_id, mother_id, person)

# We add the mothers' Id to the people_id data.frame we created
people_id <- left_join(people_id, mothers_id, by = c("household_id" = "household_id", 
                                                     "mother" = "person"))

people_id <- people_id %>% select(subject_id, mother_id) # we select only what we need 
head(people_id, 6) # we got what we wanted, an additional column with the Id of each 
# person's mother next to the Id of that person

#################################################################################
#####                       2.3 Merging the datasets                        #####
#####                       2.3.1 The "daughters" set                       #####
#################################################################################

# Now we can merge the four dataset and filter the girls between 12 and 18. 
# We will call this new dataset "daugthers"
daughters <- people %>% # the demographic and economic data
  left_join(women, by = c("subject_id" = "id_per")) %>% # the data about sexual health
  left_join(behavior, by = c("subject_id" = "id_per")) %>% # the behavioral variables
  left_join(house, by = c("household_id" = "id_hogar")) %>% # the data about the house
  left_join(people_id, by = "subject_id") %>% # the mothers' Ids (we'll use this to  
  # filter the "mothers" set)
  filter(sex == "female" & age == 16) # we filter the girls who are 16

nrow(daughters) # we have 11,446 girls in our dataset. This will not be final version 
# as we will continue cleaning the data (this includes eliminating NAs, errors, etc.)

n_distinct(daughters$mother_id) # we can also see we have data for 8,063 mothers. We have 
# more daughters than mothers because some are sisters, and there is missing data for 
# some mothers whether because they do not live with their daughters, were not at home 
# when the survey took place, etc.

#################################################################################
#####                       2.3.2 The "mothers" set                         #####
#################################################################################

# The data in the "mothers" set corresponds to the data of the mothers of those girls 
# included in the "daughters" set
mothers <- people %>% 
  left_join(women, by = c("subject_id" = "id_per")) %>% # the data about sexual health
  semi_join(daughters, by = c("subject_id" = "mother_id")) # we filter only the mothers  
# of those in the daugthers set. This is why we created the people_id data frame :)

#################################################################################
#####                       3. Variables                                    #####
#####                       3.1 Creating some useful functions              #####
#################################################################################

# Some of the answers are coded as 88 and 99 when respondents either do not remember or 
# do not want to answer. We can create a funtion to get rid of those values

ninenineTOna <- function(x){ 
  y = ifelse(x == 77 | x == 88 | x == 99 , NA, x)
  return(y)
}

# As the survey was done in Ecuador, answers are coded in Spanish, we can create a 
# function to translate the YES/NO questions and store them as factors
# We created two functions with the same purpose but with the levels inverted. We will
# apply different levels to different variables depending from what angle we want to 
# look at the variable

sinoTOyesno <- function(x){
  x = as.integer(x)
  y = factor(x, levels = c(1, 2), labels = c("yes", "no"))
  return(y)
}

sinoTOnoyes <- function(x){
  x = as.integer(x)
  y = factor(x, levels = c(2, 1), labels = c("no", "yes"))
  return(y)
}

#################################################################################
#####                       3.2 Dependent variable                          #####
#################################################################################

# early sexual acrtivity ------------------------------
daughters$early_sexual_activity <- sinoTOnoyes(daughters$f2_s8_803)

#################################################################################
#####                   3.3 Independent variables                           #####
#####                   3.3.1 Social, economic and demographic variabless   #####
#################################################################################

# a) income & number of members in the household 

# We calculated the total income for each household. We need to sum up the different sources
# of income of each member (scattered in many variables/columns), and then we need total 
# the income of each member to get the overall income of the whole household.

income <- select(people, household_id, f1_s3_15, f1_s3_16_2, f1_s3_17, f1_s3_18, 
                   f1_s3_19, f1_s3_20_2, f1_s3_22_2)

nineninetozero <- function(x){ # We create a variable to change the 999999s for zero
  x = ifelse(x == 999999, 0, x) # and apply it to all the columns
  return(x)
}

income[, c(2:8)] <- sapply(income[, c(2:8)], FUN = nineninetozero)
income$f1_s3_17 <- income$f1_s3_17 * (-1) # We changed the sign of the reported expenses
income <- income %>% mutate(income = rowSums(.[, 2:8], na.rm = TRUE)) # we sum the columns

# We sum the income of each houlsehold member
income <- income %>% group_by(household_id) %>% 
  summarize(h_income = sum(income, na.rm = TRUE),
            h_num_members = n()) 

# b) area (urban/rural) 

daughters$rural <- daughters$area.x
levels(daughters$rural) <- c("no", "yes")

# c) internet access
daughters$h_internet <- sinoTOyesno(daughters$f1_s1_42)

# d) ethnic minority
daughters$minority <- with(daughters,
  factor(ifelse(!as.integer(f1_s2_9) == 6 & !as.integer(f1_s2_9) == 7, "yes", "no"), 
         levels = c("no", "yes")))

# e) Misses school 
daughters$attends_school <- sinoTOyesno(daughters$f1_s2_17)

#################################################################################
#####                   3.3.2 Knowledge of sexual education                 #####
#################################################################################

# a) didn't know what was happening to her when she had their first period
daughters$period_knowledge <- sinoTOyesno(daughters$f2_s8_841)

# b) cannot answer correctly: can AIDS spread through handshake?
daughters$aids_knowledge <- with(daughters,
  factor(ifelse(as.integer(f2_s10_1011_1) == 1 | as.integer(f2_s10_1011_1) == 3, "no", "yes"), 
         levels = c("yes", "no")))

# c) cannot answer correctly: can a women get pregnant the first time she has sex?
daughters$pregnancy_knowledge <- sinoTOyesno(daughters$f2_s8_845)

# d) has ever received info about sexuality and primary source (school, home, other) 
daughters$sexuality_knowledge <- factor(with(daughters, 
  case_when(is.na(f2_s8_800d) ~ NA_character_, as.integer(f2_s8_800d) == 2 ~ "no info",
            as.integer(f2_s8_801d) == 1 ~ "family", as.integer(f2_s8_801d) == 2 ~ "school",
            TRUE ~ "other")), levels = c("no info", "family", "school", "other"))


#################################################################################
#####                   3.3.3 Behavioral risk factors                       #####
#################################################################################

# a) ever drunk alcohol ------------------------------
daughters$ever_drunk_alcohol <- sinoTOnoyes(daughters$f4_s5_500)

# b) ever smoked ------------------------------
daughters$ever_smoked <- sinoTOnoyes(daughters$f4_s6_600)

#################################################################################
#####                   3.3.4 Characteristics of the mother                 #####
#################################################################################

# a) mother's age at first birth
# We subtract the year of birth of the youngest child from the year of birth of the mother
mothers$m_age_1st_birth <- 
  with(mothers, pmin(f2_s2_218_1_b3, f2_s2_218_2_b3, f2_s2_218_3_b3, f2_s2_218_4_b3, 
                     f2_s2_218_5_b3, f2_s2_218_6_b3, f2_s2_218_7_b3, f2_s2_218_8_b3, 
                     f2_s2_218_9_b3, f2_s2_218_10_b3, na.rm = TRUE)) - mothers$f1_s2_4_3

# b) mother had teenage birth
mothers$m_teenage_birth <- 
  factor(ifelse(mothers$m_age_1st_birth <= 19, "yes", "no"), levels = c("no", "yes"))

# c) mother's age at first intercourse
mothers$m_age_1st_intercourse <- ninenineTOna(coalesce(mothers$f2_s8_804, mothers$f2_s8_831))

# d) mother's education attainment (no formal education, secondary, tertiary)
mothers$m_education <- mothers$f1_s2_19_1
levels(mothers$m_education) <- c("none", "none", "none", "primary", "primary", "secondary", 
                                 "secondary", "tertiary", "tertiary", "tertiary")

# e) mother finished high school
mothers$m_finished_HS <- mothers$m_education
levels(mothers$m_finished_HS) <- c("no", "no", "yes", "yes")

# f) empowerment & sexual decision making of the mother
# We measure empowerment as the ability of the mothers's to make their own sexual decisions
# We classify unempowered women as those who aren't able to turn down sex
# We also classify unempowerment as the inability to demand the use of contraception

mothers$m_empowerment <- factor(with(mothers, 
  case_when(is.na(f2_s6_604) ~ NA_character_, 
            as.integer(f2_s6_613) == 8  ~ "no", # partner does not allow contraception
            as.integer(f2_s8_835) == 6 ~ "no", # has unprotected sex because partner
            # does not like contraception
            as.integer(f2_s8_834) == 2 & ! as.integer(f2_s8_835) == 4 & 
            as.integer(f2_s8_836) == 1 & !as.integer(f2_s8_837) == 2 ~ "no",
            as.integer(f2_s8_839) == 2 ~ "no", # cannot turn down sex
            TRUE ~ "yes")), levels = c("yes", "no"))

# g) mother has a job 
mothers$m_job <- with(mothers,
  factor(case_when(is.na(f1_s3_1) ~ NA_character_,
                   as.integer(f1_s3_1) == 2 & as.integer(f1_s3_2) == 12 ~ "no", 
                   TRUE ~ "yes")), levels = c("yes", "no"))

#################################################################################
#####                       3.4 Merging the data frames                     #####
#################################################################################

daughters_tidy <- daughters %>%  select(household_id, subject_id, mother_id, 
  early_sexual_activity, rural, minority, h_internet, attends_school,  period_knowledge, 
  aids_knowledge, pregnancy_knowledge, sexuality_knowledge, ever_drunk_alcohol, ever_smoked) %>% 
  left_join(income, by = c("household_id" = "household_id"))

mothers_tidy <- mothers %>% select(subject_id, m_teenage_birth,
  m_empowerment, m_finished_HS, m_job, m_age_1st_intercourse)

data <- daughters_tidy %>% left_join(mothers_tidy, by = c("mother_id" = "subject_id")) %>%
  filter(!is.na(early_sexual_activity)) # we eliminate NAs

saveRDS(data, file = "early_sexual_activity.rds")

#################################################################################
#####                       4 The logit models                              #####
#################################################################################

# We will run logistic regressions to see what variables are most correlated 
# with early sexual activity.

# Model 1: ------------------------------
# mother empowerment + control variables

logit_m1 <- glm(early_sexual_activity ~ rural + h_income + h_num_members + h_internet + minority + 
                  attends_school + period_knowledge + aids_knowledge + pregnancy_knowledge + 
                  sexuality_knowledge + m_job + m_finished_HS +  m_empowerment,
                data = data, family = "binomial")

summary(logit_m1)

# Model 2: ------------------------------
# mother empowerment & m_teenage_birth + control variables

logit_m2 <- glm(early_sexual_activity ~ rural + h_income + h_num_members + h_internet + minority + 
                  attends_school + period_knowledge + aids_knowledge + pregnancy_knowledge + 
                  sexuality_knowledge + m_job + m_finished_HS +  m_empowerment + m_teenage_birth,
                data = data, family = "binomial")

summary(logit_m2)

# Model 3: ------------------------------ 
# mother empowerment & m_teenage_birth & m_age_1st_intercourse + control variables

logit_m3 <- glm(early_sexual_activity ~ rural + h_income + h_num_members + h_internet + minority + 
                  attends_school + period_knowledge + aids_knowledge + pregnancy_knowledge + 
                  sexuality_knowledge + m_job + m_finished_HS +  m_empowerment + m_teenage_birth +
                  m_age_1st_intercourse, data = data, family = "binomial")

summary(logit_m3)

# Model 4 ------------------------------
# mother empowerment & m_teenage_birth & m_age_1st_intercourse + control variables (including behavioral)

logit_m4 <- glm(early_sexual_activity ~ rural + h_income + h_num_members + h_internet + minority + 
                  attends_school + period_knowledge + aids_knowledge + pregnancy_knowledge + 
                  sexuality_knowledge + m_job + m_finished_HS +  m_empowerment + m_teenage_birth +
                  m_age_1st_intercourse + ever_smoked + ever_drunk_alcohol,
                data = data, family = "binomial")

summary(logit_m4)

#################################################################################
#####               5 Summary statistics                                    #####
#####               5.1 Percentages and means of each variable by gruop     #####
#################################################################################

# We are going to create a table with the summary statistics of each variable
# We first create a copy of the data, which we will use to create the table
data_copy <- data

# The variable sexuality_knowledge has several levels in it. We will split this
# variable into different columns. Because this is a factor, whose levels are 1 for the
# first level, 2 for the seconde, etc., we will code as 2 those who learned about
# sexuality from X source and 1 otherwise

data_copy$value <- 2
data_copy <- spread(data_copy, sexuality_knowledge, value, fill = 1, sep = "_")

# We will use the chi square test and t test to compare variables within groups (early
# sexual activity and no early sexual activity). For that we will apply some loops

# chi square test (categorical variables) ------------------------------

cat_var <- c("minority", "rural", "h_internet", "attends_school", "period_knowledge", 
             "pregnancy_knowledge", "aids_knowledge", "sexuality_knowledge_no info", 
             "sexuality_knowledge_family", "sexuality_knowledge_school", 
             "sexuality_knowledge_other", "ever_drunk_alcohol", "ever_smoked", "m_job", 
             "m_finished_HS", "m_teenage_birth", "m_empowerment")


chi_sq_test  <- sapply(cat_var, function(x){
  chi_sq <- chisq.test(data_copy[,"early_sexual_activity"], data_copy[,x])
  return(chi_sq$p.value)
})

mean_early_sex  <- sapply(cat_var, function(x){
  mean <- mean(as.integer(data_copy[data_copy$early_sexual_activity == "yes", x]) == 2, na.rm = TRUE)
  return(mean)
})

mean_no_early_sex  <- sapply(cat_var, function(x){
  mean <- mean(as.integer(data_copy[data_copy$early_sexual_activity == "no", x]) == 2, na.rm = TRUE)
  return(mean)
})

# We put the percentages and p values everything in one table
summary_statistics <- tibble(variable = cat_var, mean_early_sex = mean_early_sex,
                             mean_no_early_sex = mean_no_early_sex, p_value = chi_sq_test)

# t test (continuous variables) ------------------------------

cont_var <- c("h_income", "h_num_members", "m_age_1st_intercourse")

t_test  <- sapply(cont_var, function(x){
  t_test <- t.test(data_copy[,x] ~ data_copy[,"early_sexual_activity"], var.equal = TRUE)
  return(t_test$p.value)
})

mean_early_sex  <- sapply(cont_var, function(x){
  mean <- mean(data_copy[data_copy$early_sexual_activity == "yes", x], na.rm = TRUE)
  return(mean)
})

mean_no_early_sex  <- sapply(cont_var, function(x){
  mean <- mean(data_copy[data_copy$early_sexual_activity == "no", x], na.rm = TRUE)
  return(mean)
})

# We add the new means and p values to the table we already made
summary_statistics <- rbind(summary_statistics, tibble(variable = cont_var, 
  mean_early_sex = mean_early_sex, mean_no_early_sex = mean_no_early_sex, 
  p_value = t_test))

# Adding some format to the table ------------------------------

summary_statistics[, 2:3] <- round(summary_statistics[, 2:3], digits = 2)
summary_statistics[, 4] <- round(summary_statistics[, 4], digits = 3)

summary_statistics_copy <- summary_statistics

summary_statistics_copy$p_value <- 
  with(summary_statistics_copy, case_when(p_value < 0.0005 ~ paste("0.000", "***"),
                                          p_value <= 0.001 ~ paste(as.character(p_value), "***"),
                                          p_value <= 0.01 ~ paste(p_value, "**"),
                                          p_value <= 0.05 ~ paste(p_value, "*"),
                                          p_value <= 0.1 ~ paste(as.numeric(p_value), "  ."),
                                          TRUE ~ as.character(p_value)))

names(summary_statistics_copy) <- c("Variables", "Early sexual activity", "No early sexual activity", "p value")

summary_statistics_copy$Variables <- c("Ethnic minority", "Lives in a rural area", "Does not have internet", 
  "Misses school", "Lacks knowledge about period", "Lacks knowledge about pregnancy", 
  "Lacks knowledge about AIDs", "Does not know about sexuality", "Knows about sexuality from family", 
  "Knows about sexuality from school", "Knows about sexuality from other sources",
  "Has ever drunk alcohol", "Has ever smoked", "Mother has a job", "Mother finished HS",
  "Mother had a teenage birth", "Mother lacks sexual bargaining", "Household income", 
  "Number of members in the household", "Mother's age at first intercourse")

summary_statistics_copy

# What percent of our sample belongs to each group: early sexual activity and no early sexual activity

index <- as.integer(names(logit_m1$fitted.values)) # these are the observations we used in our model
data_copy <- data[index,]

prop.table(table(data_copy$early_sexual_activity))
length(index)

mean(data_copy[data_copy$early_sexual_activity == "yes", "m_age_1st_intercourse"], na.rm = TRUE)
sd(data_copy[data_copy$early_sexual_activity == "yes", "m_age_1st_intercourse"], na.rm = TRUE)

# Ns used in the logistic models
length(logit_m1$fitted.values)
length(logit_m4$fitted.values)

#################################################################################
#####       5.2 CDF of the mother's age at first intercourse by gruop       #####
#################################################################################

plot(ecdf(data_copy[data_copy$early_sexual_activity == "yes", "m_age_1st_intercourse"]),
          col = "cadetblue1", main = "", xlab = "Age at first intercourse", ylab = "Density",
     cex.axis=0.75, cex.lab=0.75)
plot(ecdf(data_copy[data_copy$early_sexual_activity == "no", "m_age_1st_intercourse"]),
     col = "palegreen", add = TRUE)
legend("left", c("Early sexual activity", "No early sexual activity"),
       col = c("cadetblue1", "palegreen"), lwd = 5, bty = "n", cex = 0.6)

# ------------------

