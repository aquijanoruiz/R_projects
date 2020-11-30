##################################################################################
#####                             1 Loading the data                         #####
#####                             1.1 The packages                           #####
##################################################################################

# The following code automatically downloads the packages in case they are not 
# installed in your computer already.

if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(readstata13)) install.packages("readstata13", repos = "http://cran.us.r-project.org")
if(!require(kableExtra)) install.packages("kableExtra", repos = "http://cran.us.r-project.org")

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

options(timeout=600) # we change the download timeout time to 600

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
# function to translate the YES/NO questions and store them as factors and order the levels
# as 

sinoTOnoyes <- function(x){
  x = as.integer(x)
  y = factor(x, levels = c(2, 1), labels = c("no", "yes"))
  return(y)
}

#################################################################################
#####                       3.2 Dependent variable                          #####
#################################################################################

# early sexual activity ------------------------------
daughters$early_sexual_activity <- sinoTOnoyes(daughters$f2_s8_803)

# coercion at first intercourse ------------------------------
# (Not the dependent variable but something we will look at)
daughters$coercion_1st_intercourse <- factor(with(daughters,
                                                  ifelse(as.integer(f2_s8_807) == 1 | as.integer(f2_s8_807) == 2, "no", "yes"), 
                                                  levels = c("no", "yes")))

# used contraception at 1st intercourse ------------------------------
daughters$contraception_1st_intercourse <- 
  factor(ifelse(as.integer(daughters$f2_s8_808) == 1, "yes", "no"), levels = c("no", "yes"))

# has ever been pregnant ------------------------------
daughters$teenage_pregnancy <- 
  (factor(ifelse(as.integer(daughters$f2_s2_200) == 1 | as.integer(daughters$f2_s2_207) == 1, 
                 "yes", "no"), levels = c("no", "yes")))

#################################################################################
#####                   3.3 Independent variables                           #####
#####                   3.3.1 Mother-related variables                      #####
#################################################################################

# a) empowerment & sexual decision making of the mother
# We measure empowerment as the ability of the mothers's to make their own sexual decisions
# We classify unempowered women as those who aren't able to turn down sex
# We also classify unempowerment as the inability to demand the use of contraception

mothers$m_lack_empowerment <- 
  factor(with(mothers, 
              case_when(is.na(f2_s6_604) ~ NA_character_, 
                        as.integer(f2_s6_613) == 8 ~ "yes", 
                        # partner does not allow contraception
                        as.integer(f2_s8_835) == 6 ~ "yes", 
                        # has unprotected sex because partner does not like contraception
                        as.integer(f2_s8_834) == 2 & ! as.integer(f2_s8_835) == 4 & 
                          as.integer(f2_s8_836) == 1 & as.integer(f2_s8_837) == 1 ~ "yes",
                        as.integer(f2_s8_839) == 2 ~ "yes", 
                        # cannot turn down sex
                        TRUE ~ "no")), levels = c("no", "yes"))

# b) mother had teenage birth
# We first calculate the year in which she had her first birth and then subtract that year
# from the year of birth of the mother
mothers$m_age_1st_birth <- 
  with(mothers, pmin(f2_s2_218_1_b3, f2_s2_218_2_b3, f2_s2_218_3_b3, f2_s2_218_4_b3, 
                     f2_s2_218_5_b3, f2_s2_218_6_b3, f2_s2_218_7_b3, f2_s2_218_8_b3, 
                     f2_s2_218_9_b3, f2_s2_218_10_b3, na.rm = TRUE)) - mothers$f1_s2_4_3

mothers$m_teenage_birth <- 
  factor(ifelse(mothers$m_age_1st_birth <= 19, "yes", "no"), levels = c("no", "yes"))

# c) mother's age
mothers$m_age <- mothers$edadanios.x

# d) mother's marital status
mothers$m_marital_status <- 
  factor(with(mothers,
              case_when(is.na(f2_s9_900) ~ NA_character_,
                        as.integer(f2_s9_900) == 1 ~ "married",
                        as.integer(f2_s9_900) == 2 | as.integer(f2_s9_900) == 3 ~ "cohabiting",
                        TRUE ~ "non_partnered")), levels = c("non_partnered", "cohabiting", "married"))

# e) mother has a job 
mothers$m_job <- 
  with(mothers, factor(case_when(is.na(f1_s3_1) ~ NA_character_,
                                 as.integer(f1_s3_1) == 2 & as.integer(f1_s3_2) == 12 ~ "no", 
                                 TRUE ~ "yes"), levels = c("no", "yes")))

# f) mother's education attainment (no formal education, secondary, tertiary)
mothers$m_education <- mothers$f1_s2_19_1
levels(mothers$m_education) <- c("none", "none", "none", "primary", "primary", "secondary", 
                                 "secondary", "tertiary", "tertiary", "tertiary")

#################################################################################
#####                   3.3.2 Daughter-related variables                    #####
#################################################################################

# a) not being enrolled in school
daughters$not_enrolled <- factor(ifelse(as.integer(daughters$f1_s2_17) == 1, "no", "yes"),
                                 levels = c("no", "yes"))

# b) lacked knoledge about the period
# Girls were asked whether they knew what was happening to them when they had their first period
daughters$lack_period_knowledge <- 
  factor(ifelse(as.integer(daughters$f2_s8_841) == 1, "no", "yes"), levels = c("no", "yes"))

# c) sexuality knowledge
# Girls were asked if they had ever received info about sexual intercourse, if they 
# answered "yes" they were asked from who they had received the most info (family, school, other)
daughters$sexuality_knowledge <- 
  factor(with(daughters, 
              case_when(is.na(f2_s8_800f) ~ NA_character_, 
                        as.integer(f2_s8_800f) == 2 ~ "no_info",
                        as.integer(f2_s8_801f) == 1 ~ "family", 
                        as.integer(f2_s8_801f) == 2 ~ "school",
                        TRUE ~ "other")), levels = c("no_info", "family", "school", "other"))

#################################################################################
#####                   3.3.3 Household-related variables                   #####
#################################################################################

# a) ethnic minority
daughters$minority <- with(daughters, factor(ifelse(!as.integer(f1_s2_9) == 6 
                                                    & !as.integer(f1_s2_9) == 7, "yes", "no"), 
                                             levels = c("no", "yes")))

# b) area (urban/rural) 
daughters$rural <- daughters$area.x
levels(daughters$rural) <- c("no", "yes")

# c) internet access
daughters$h_internet <- sinoTOnoyes(daughters$f1_s1_42)

# d) income & number of members in the household 
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
  summarize(h_income = sum(income, na.rm = TRUE)) # we also get the number of household members

#################################################################################
#####                       3.4 Merging the data frames                     #####
#################################################################################

daughters_tidy <- daughters %>%  
  select(household_id, subject_id, mother_id, early_sexual_activity, coercion_1st_intercourse, 
         contraception_1st_intercourse, teenage_pregnancy, rural, h_internet, minority, 
         not_enrolled, lack_period_knowledge, sexuality_knowledge) %>% 
  left_join(income, by = c("household_id" = "household_id"))

mothers_tidy <- mothers %>% select(subject_id, m_age, m_marital_status, m_job, m_education, 
                                   m_teenage_birth, m_lack_empowerment, m_marital_status)

data <- daughters_tidy %>% left_join(mothers_tidy, by = c("mother_id" = "subject_id"))

nrow(data) # the initial sample contains 1636 subjects

# Removing the NAs ------------------------------
sapply(data, function(x){
  na <- is.na(x)
  na_total <- sum(na)
  return(na_total)
}) # we create this function to know how many more NAs there are

# we eliminate those who didn't live with their mothers (289 individuals)
data <- data %>% filter(!is.na(mother_id))

# we eliminate those who didn't provide information about their sexual activity (120 individuals)
data <- data %>% filter(!is.na(early_sexual_activity)) %>%
  filter(!(early_sexual_activity == "yes" & is.na(contraception_1st_intercourse)))

# we eliminate those whose mother did not report their sexual activity (249 individuals)
data <- data %>% filter(!is.na(m_lack_empowerment) & !is.na(m_teenage_birth))

nrow(data) # the final sample has 978 people

# saveRDS(data, file = "early_sexual_activity_data.rds") # this code saves the data into an rds file
summary(data)
#################################################################################
#####                       4 Summary statistics                            #####
#################################################################################

# We first separate the factor variables into different variables
data_copy <- data
data_copy$value <- TRUE
data_copy <- spread(data_copy, sexuality_knowledge, value, fill = FALSE, sep = "_")

data_copy$value <- TRUE
data_copy <- spread(data_copy, m_education, value, fill = FALSE, sep = "_")

data_copy$value <- TRUE
data_copy <- spread(data_copy, m_marital_status, value, fill = FALSE, sep = "_")

# We create a variable to binarize the data
binarize <- function(x){ 
  x = ifelse(as.integer(x) == 2, TRUE, FALSE) # and apply it to all the columns
  return(x)
}

binary_var <- c("early_sexual_activity", "coercion_1st_intercourse", 
                "teenage_pregnancy", "contraception_1st_intercourse", "minority", "rural", 
                "h_internet", "not_enrolled", "lack_period_knowledge", "m_job", 
                "m_teenage_birth", "m_lack_empowerment")

data_copy[, binary_var] <- sapply(data_copy[, binary_var], FUN = binarize)

# if(!require(haven)) install.packages("haven", repos = "http://cran.us.r-project.org")
# write_dta(data_copy, "early_sexual_activity_data.dta")

# We will use the chi square test and t test to compare variables within groups (early
# sexual initiators and no early sexual initiators)

# percentages and chi-square test (categorical variables) ------------------------------

indep_var <- c("early_sexual_activity", "teenage_pregnancy", "contraception_1st_intercourse")

cat_var <- c("m_lack_empowerment", "m_teenage_birth", "minority", "rural", "h_internet", 
             "not_enrolled", "lack_period_knowledge", "sexuality_knowledge_no_info",
             "sexuality_knowledge_family", "sexuality_knowledge_school", "sexuality_knowledge_other",
             "m_job", "m_marital_status_non_partnered", "m_marital_status_cohabiting",
             "m_marital_status_married", "m_education_none", "m_education_primary", 
             "m_education_secondary", "m_education_tertiary")

# We calculate the chi-square of each combination of categorical variables
# We create an empty matrix where we will store the p-value of the chi-square tests
chi_sq_test <- matrix(NA, nrow = length(cat_var), ncol = length(indep_var))
rownames(chi_sq_test) <- cat_var
colnames(chi_sq_test) <- indep_var

for(x in cat_var) {
  for(y in indep_var) {
    chi_sq_test[x,y] <- chisq.test(data_copy[,x], data_copy[,y])$p.value
  }
}

colnames(chi_sq_test) <- paste("p_value", colnames(chi_sq_test), sep = "_")

# No, we take the percentage levels
cat_var_mean_T <- matrix(NA, nrow = length(cat_var), ncol = length(indep_var))
rownames(cat_var_mean_T) <- cat_var
colnames(cat_var_mean_T) <- indep_var

for(x in cat_var) {
  for(y in indep_var) {
    cat_var_mean_T[x,y] <- mean(data_copy[data_copy[,y] == TRUE, x], na.rm = TRUE)
  }
}

cat_var_mean_F <- matrix(NA, nrow = length(cat_var), ncol = length(indep_var))
rownames(cat_var_mean_F) <- cat_var
colnames(cat_var_mean_F) <- indep_var

for(x in cat_var) {
  for(y in indep_var) {
    cat_var_mean_F[x,y] <- mean(data_copy[data_copy[,y] == FALSE, x], na.rm = TRUE)
  }
}

colnames(cat_var_mean_F) <- paste("no", colnames(cat_var_mean_F), sep = "_")

# means and t-test (continuous variables) ------------------------------
# We calculate the t-test of each combination of variables
# We create an empty matrix where we will store the p-value of the t-tests

cont_var <- c("m_age", "h_income")

t_test <- matrix(NA, nrow = length(cont_var), ncol = length(indep_var))
rownames(t_test) <- cont_var
colnames(t_test) <- indep_var

for(x in cont_var) {
  for(y in indep_var) {
    t_test[x,y] <- t.test(data_copy[,x] ~ data_copy[,y], var.equal = TRUE)$p.value
  }
}

colnames(t_test) <- paste("p_value", colnames(t_test), sep = "_")

# Now, we take the mean levels
cont_var_mean_T <- matrix(NA, nrow = length(cont_var), ncol = length(indep_var))
rownames(cont_var_mean_T) <- cont_var
colnames(cont_var_mean_T) <- indep_var

for(x in cont_var) {
  for(y in indep_var) {
    cont_var_mean_T[x,y] <- mean(data_copy[data_copy[,y] == TRUE, x], na.rm = TRUE)
  }
}

cont_var_mean_F <- matrix(NA, nrow = length(cont_var), ncol = length(indep_var))
rownames(cont_var_mean_F) <- cont_var
colnames(cont_var_mean_F) <- indep_var

for(x in cont_var) {
  for(y in indep_var) {
    cont_var_mean_F[x,y] <- mean(data_copy[data_copy[,y] == FALSE, x], na.rm = TRUE)
  }
}

colnames(cont_var_mean_F) <- paste("no", colnames(cont_var_mean_F), sep = "_")

mean_total  <- sapply(c(cat_var, cont_var), function(x){
  mean <- mean(data_copy[, x], na.rm = TRUE)
  return(mean)
})


mean_total_indep_var  <- sapply(indep_var, function(x){
  mean <- mean(data_copy[, x], na.rm = TRUE)
  return(mean)
})

mean_total_indep_var <- round(mean_total_indep_var, digits = 2)

mean_total_indep_var <- 
  as.data.frame(matrix(c(mean_total_indep_var, rep("", 27)), nrow = 3, ncol = 10))

# Now, we combine all the matrixes into one matrix
table_sum_stat <- rbind(cbind(chi_sq_test, cat_var_mean_T, cat_var_mean_F), 
                          cbind(t_test, cont_var_mean_T, cont_var_mean_F))

table_sum_stat <- cbind(mean_total, table_sum_stat)

table_sum_stat <-
  table_sum_stat[,c("mean_total", "early_sexual_activity", "no_early_sexual_activity", 
                      "p_value_early_sexual_activity", "teenage_pregnancy", "no_teenage_pregnancy", 
                      "p_value_teenage_pregnancy", "contraception_1st_intercourse", 
                      "no_contraception_1st_intercourse", "p_value_contraception_1st_intercourse")]

# Adding some format to the table ------------------------------

table_sum_stat[, c(1:3, 5, 6, 8, 9)] <- 
  round(table_sum_stat[, c(1:3, 5, 6, 8, 9)], digits = 2)
table_sum_stat[, c(4, 7, 10)] <- round(table_sum_stat[, c(4, 7, 10)], digits = 3)

table_sum_stat_copy <- as.data.frame(table_sum_stat)

p_value_stars <- function(x){
  x <- case_when(x <= 0.01 ~ paste(format(x, digits = 3), "***"),
                 x <= 0.05 ~ paste(format(x, digits = 3), "**"),
                 x <= 0.1 ~ paste(format(x, digits = 3), " *"),
                 TRUE ~ format(x, digits = 3))
  return(x)
}

table_sum_stat_copy[,c(4,7,10)] <- sapply(table_sum_stat_copy[,c(4,7,10)], p_value_stars)

table_sum_stat_copy[, c(1:3, 5, 6, 8, 9)] <- 
  format(table_sum_stat_copy[, c(1:3, 5, 6, 8, 9)], digits = 2)

# We reorder the rows in the table
table_sum_stat_copy <- table_sum_stat_copy[
  c("m_lack_empowerment", "m_teenage_birth", "m_age", 
    "m_job", "m_marital_status_non_partnered", "m_marital_status_cohabiting",
    "m_marital_status_married", "m_education_none", "m_education_primary", 
    "m_education_secondary", "m_education_tertiary", "not_enrolled", 
    "lack_period_knowledge", "sexuality_knowledge_no_info", "sexuality_knowledge_family", 
    "sexuality_knowledge_school", "sexuality_knowledge_other", "minority", "rural", 
    "h_internet", "h_income"),]

colnames(mean_total_indep_var) <- colnames(table_sum_stat_copy)
rownames(mean_total_indep_var) <- indep_var
table_sum_stat_copy <- rbind(mean_total_indep_var, table_sum_stat_copy)

# this code saves the data into an rds file
# saveRDS(table_sum_stat_copy, file = "table_sum_stat_copy.rds") 

#################################################################################
#####                       5 The logit models                              #####
#################################################################################

# Model 1: ------------------------------
# early sexual activity

logit_m1 <- 
  glm(early_sexual_activity ~ m_lack_empowerment + m_teenage_birth + # explanatory variables
        m_age + m_job + m_marital_status + m_education + # mother-related variables
        not_enrolled + lack_period_knowledge  + sexuality_knowledge+ # daughter-related variables
        minority + rural + h_internet + h_income, # household-related variables
      data = data, family = "binomial")

# we create a function to transform log odds to odds ratios and add some confidence intervals
logit_table <- function(x){
  odds_logit <- cbind(exp(cbind(OR = coef(x), confint(x))),summary(x)$coef[,4])
  colnames(odds_logit) <- c("OR", "2.5%", "97.5%","p.value")
  odds_logit <- as.data.frame(odds_logit)
  odds_logit[,1:3] <- sapply(odds_logit[,1:3], round, digits = 2)
  odds_logit$CI <- paste(format(odds_logit[,2], digits = 2), 
                         format(odds_logit[,3], digits = 2), sep = " –")
  odds_logit$OR <- 
    case_when(odds_logit$p.value <= 0.01 ~ paste(format(odds_logit$OR, digits = 2), "***"),
              odds_logit$p.value <= 0.05 ~ paste(format(odds_logit$OR, digits = 2), "**"),
              odds_logit$p.value <= 0.1 ~ paste(format(odds_logit$OR, digits = 2), "*"),
              TRUE ~ format(odds_logit$OR, digits = 2))
  odds_logit <- odds_logit[,c(1,5)]
  return(odds_logit)
}

logit_m1 <- logit_table(logit_m1)

# Model 2: ------------------------------
# teenage pregnancy

logit_m2 <- 
  glm(teenage_pregnancy ~ m_lack_empowerment + m_teenage_birth + # explanatory variables
        m_age + m_job + m_marital_status + m_education + # mother-related variables
        not_enrolled + lack_period_knowledge + sexuality_knowledge + # daughter-related variables
        minority + rural + h_internet + h_income, # household-related variables
      data = data, family = "binomial")

logit_m2 <- logit_table(logit_m2)

# Model 3: ------------------------------
# contraception 1st intercourse

logit_m3 <- 
  glm(contraception_1st_intercourse ~ m_lack_empowerment + m_teenage_birth + # explanatory variables
        m_age + m_job + m_marital_status + m_education + # mother-related variables
        not_enrolled + lack_period_knowledge + sexuality_knowledge + # daughter-related variables
        minority + rural + h_internet + h_income, # household-related variables
      data = data, family = "binomial")

logit_m3 <- logit_table(logit_m3)
table_logit_results <-  cbind(logit_m1, logit_m2, logit_m3)
# saveRDS(table_logit_results, file = "table_logit_results.rds")