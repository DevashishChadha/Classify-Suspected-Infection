#-------------------------------TASK 1-------------------------------
install.packages("data.table") 
#installed data.table package

library(data.table)
#library of data.table i.e where the package resides

antibioticDT <- fread("C:\\Users\\Devashish Chadha\\OneDrive\\Desktop\\Data Analytics by Google\\Project\\project.data\\Classify Suspected Infection in Patients\\datasets\\antibioticDT.csv")
#made a data frame "antibiotics_df" and using fread function imported the data set

setorder(x = antibioticDT,patient_id, antibiotic_type, day_given)
#sorted on the basis of patient_id,antibiotic_type and then day_given

antibioticDT[1:40]
#Only included first 40 rows of the data set

#-------------------------------#TASK 2-------------------------------

antibioticDT[,last_administration_day := shift(day_given,n=1,fill = NA,type= "lag"),by = .(patient_id,antibiotic_type)]
#calculates the last administration day of antibiotics for each patient and antibiotic type by creating a new column last_administration_day 

antibioticDT[,days_since_last_admin := day_given - last_administration_day]
#Calculate the number of days since the antibiotic was administered to a patient

antibioticDT[,antibiotic_new := 1]
#created a new variable and initialized with 1

antibioticDT[days_since_last_admin <= 2,antibiotic_new := 0]
#one or two days since the last antibiotic was given + initialized antibiotics_new to 0

antibioticDT[1:40]
#Only included first 40 rows of the data set


#-------------------------------Task 3-------------------------------

blood_cultureDT <- fread("C:\\Users\\Devashish Chadha\\OneDrive\\Desktop\\Data Analytics by Google\\Project\\project.data\\Classify Suspected Infection in Patients\\datasets\\blood_cultureDT.csv")
#imported blood culture data

blood_cultureDT[1:30]
#print first 30 rows

#-------------------------------Task 4-------------------------------

merged_df <- merge(antibioticDT,blood_cultureDT,by = "patient_id",all = FALSE)
#merged two data sets having on the basis of common patient id

setorder(merged_df,patient_id, blood_culture_day, day_given, antibiotic_type)
#sorted merged data set on the basis of patient_id, blood_culture_day, day_given, and antibiotic_type

merged_df[1:30]
#print merged data

#-------------------------------Task 5-------------------------------

merged_df[,drug_in_bcx_window := as.numeric((day_given - blood_culture_day) <= 2 & (day_given - blood_culture_day) >= -2)]
#created a variable drug_in_bcx_window that checks if the difference of day_given and blood_culture_day is within 2 days

#-------------------------------Task 6-------------------------------

merged_df[,any_iv_in_bcx_window := as.numeric(any(route == "IV" & drug_in_bcx_window == 1)),by = .(patient_id,blood_culture_day)]
#indicates whether antibiotic was given through route "IV" or not in given day i.e blood_culture_day

merged_df <- merged_df[any_iv_in_bcx_window == 1]
#excluded rows that do not include route "IV"

#-------------------------------Task 7-------------------------------

merged_df[,day_of_first_new_abx_in_window := day_given[antibiotic_new == 1 & drug_in_bcx_window == 1][1],by = .(patient_id, blood_culture_day)]
#Created a new variable to find the first day of a new antibiotic sequence

merged_df <- merged_df[day_given >= day_of_first_new_abx_in_window]
#Removed data that is before first qualifying day

#-------------------------------Task 8-------------------------------

new_dt <- merged_df[,.(patient_id,blood_culture_day,day_given)]
#created a new data table which contains patient_id, blood_culture_day,and day_given from merge_dt

new_dt <- unique(new_dt)
#removed duplicate rows

new_dt[1:5]
#prints first 5 values

#-------------------------------Task 9-------------------------------

new_dt[,num_antibiotic_days := .N, by = .(patient_id,blood_culture_day)]
#counts number of antibiotic days

new_dt <- new_dt[num_antibiotic_days >= 4]
#removed blood_culture_day with count less than equal to 4

first_four_rows <- new_dt[, .SD[1:4], by = .(patient_id,blood_culture_day)]
#prints first four days of each blood culture

first_four_rows[1:5]

#-------------------------------Task 10-------------------------------

first_four_rows[,four_in_seq := as.numeric(max(diff(day_given)) < 3), by = .(patient_id,blood_culture_day)]
#antibiotic sequence no skips of more than one day

#-------------------------------Task 11-------------------------------

suspected_infection <- first_four_rows[four_in_seq == 1]
#rows with four_in_seq = 1

suspected_infection <- suspected_infection[,.(patient_id)]
#Retained patient_id column

suspected_infection <- unique(suspected_infection)
#removed duplicate values

suspected_infection <- suspected_infection[,infection  := 1]
#inserted a new column "infection" initializing it with 1

suspected_infection[1:5]
#prints first 5 values

##-------------------------------Task 12-------------------------------

all_patientsDT  <- fread("C:\\Users\\Devashish Chadha\\OneDrive\\Desktop\\Data Analytics by Google\\Project\\project.data\\Classify Suspected Infection in Patients\\datasets\\all_patients.csv")
#read "all_patients" data set

all_patientsDT <- merge(all_patientsDT, suspected_infection, all = TRUE)
#merged suspected_infections data frame and all_patients data frame

all_patientsDT <- all_patientsDT[is.na(infection), infection := 0]
#replaced NA in infection to 0

all_patientsDT[1:10]
#prints first 10 rows

percentage <- 100 * all_patientsDT[, mean(infection)]
#created a value which tells the percentage of patients who met the criteria for presumed infection
