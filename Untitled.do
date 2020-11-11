clear

use "/Users/elocarinista/Documents/R_projects/R_projects/early_sexual_activity/early_sexual_activity_data.dta"

* We first used R to transform the raw data to tidy data. We will compute the mean and percentage for some variables and see the differences between gruous using the t test and chi square

summarize h_income

ttest h_income , by(early_sexual_activity)

* After loading the dta file with the tidy data we generated using R, we can run the logistic regressions

logit early_sexual_activity minority rural h_income h_num_members h_internet attends_school period_knowledge pregnancy_knowledge aids_knowledge sexuality_knowledge_family sexuality_knowledge_school sexuality_knowledge_other m_job m_education_primary m_education_secondary m_education_tertiary m_empowerment m_teenage_birth m_age_1st_intercourse

logit early_sexual_activity minority rural h_income h_num_members h_internet attends_school period_knowledge pregnancy_knowledge aids_knowledge sexuality_knowledge_family sexuality_knowledge_school sexuality_knowledge_other m_job m_education_primary m_education_secondary m_education_tertiary ever_drunk_alcohol ever_smoked m_empowerment m_teenage_birth m_age_1st_intercourse
