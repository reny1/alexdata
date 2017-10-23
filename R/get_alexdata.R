#' Read data from Alex's database
#' @param fileName name of the file to read in
#' @param uselessColumns useless columns to be removed, default set to X and index
#' @return alexData a data frame
#' @examples
#' alexData = read_alex_database(fileName = "/Users/yue/Dropbox/Joy/tabacco_grant/data/data_fromalex/database/database10052017/external_records_protocols.csv")
#'
#'
#' @export
read_alex_database <- function(fileName, uselessColumns = c("X", "index")){
  alexData <- read.csv(file = fileName, stringsAsFactors = F)
  alexData <- alexData[,-which(colnames(alexData) %in% uselessColumns)]
  return(alexData)
}

#' Join two dataframe that contains subject level information together by using certain columns (default set to ebh_id, redcap_event_name)
#' return all rows and all columns from both data frames. Where there are not matching values, returns NA for the one missing.
#' @param subject_df1 the 1st data frame
#' @param subject_df2 the 2nd data frame to be joind
#' @param ID names of the columns used to merge the two data frames, default set to ehb_id, redcap_event_name
#' @examples
#' alex_demographics_information <- read_alex_database(fileName ="/Users/yue/Dropbox/Joy/tabacco_grant/data/data_fromalex/database/database10052017/demographics_information.csv")
#' alex_antiobiotics_timing <- read_alex_database(fileName = "/Users/yue/Dropbox/Joy/tabacco_grant/data/data_fromalex/database/database10052017/antiobiotics_timing.csv")
#' alex_subject <- join_fromalex_subject(alex_demographics_information, alex_antiobiotics_timing)
#'
#' @export
join_fromalex_subject <- function(subject_df1,subject_df2,ID = c("ehb_id", "redcap_event_name")){

  full_join(subject_df1, subject_df2, by = ID)

}

## join two data frames that contains sample level information together by using certain columns (default set to sample_subject_name)
#' return all rows and all columns from both data frames. Where there are not matching values, returns NA for the one missing.
#' @param sample_df1 the 1st data frame
#' @param sample_df2 the 2nd data frame to be joind
#' @param ID names of the columns used to merge the two data frames, default set to sample_subject_name
#' @examples
#' # load sample_visit_event
#' alex_sample_visit_event <- read_alex_database(fileName = "/Users/yue/Dropbox/Joy/tabacco_grant/data/data_fromalex/database/database10052017/sample_visit_event.csv", uselessColumns = c("X"))
#' alex_tobacco_grant_albumin <- read_alex_database(file = "//Users/yue/Dropbox/Joy/tabacco_grant/data/data_fromalex/database/database10052017/tobacco_grant_albumin.csv", uselessColumns = c("X", "index","rank_id")) %>% unique
#' alex_sample <- join_fromalex_sample(alex_sample_visit_event, alex_tobacco_grant_albumin)
#' @export

join_fromalex_sample <- function(sample_df1,sample_df2,ID = "sample_subject_name"){

  full_join(sample_df1,sample_df2, by = ID)

}


#' function used to clear up the medication log data, keep only the medication in the medList, remove the unused records.
#' @param dataFrame the raw medication log data frame contains the data from baseline_antibiotics.csv and medication_log.csv and a column of collect date time
#' @return medRaw return a data frame that contains only the medications in the medList
#' @examples
#' medList <- as.list(read.xlsx("/Users/yue/Dropbox/Joy/tabacco_grant/data/data_fromsarah/IBD Medication List_2.17.2017.xls", sheetIndex = 1,stringsAsFactors = F))
#' medList <- sapply(medList, function(x) x = x[!is.na(x)])
#' names(medList)[which(names(medList) == "Other")] = "Other.med"
#' names(medList)[which(names(medList) == "X5.ASA")] = "5.ASA"

#' alex_medication_log <- read_alex_database(fileName = "/Users/yue/Dropbox/Joy/tabacco_grant/data/data_fromalex/database/database10052017/medication_log.csv")
#' alex_medication_log %<>% mutate(start_date = as.Date(start_date)) %>%
#' mutate(stop_date = as.Date(stop_date)) %>%
#' mutate(duration = stop_date - start_date)

# skip the step to combine the baseline medication records from baseline_antibiotics.csv to keep the example simple

# add collect_date_time
#' a <- read_alex_database(fileName = "/Users/yue/Dropbox/Joy/tabacco_grant/data/data_fromalex/database/database10052017/sample_redcap.csv") %>%
#' rename(collect_date_time = Collected_Time)
#' alex_medication_log %<>% left_join(a, by = c("ehb_id")) %>%
#' rename(SampleType = Sample.Type)
#' remove(a)
#' medRaw = read_alex_database_med_log(dataFrame = alex_medication_log, medList = medList)
#' @export

read_alex_database_med_log <- function(dataFrame, medList){
  medRaw <- dataFrame %>%
    mutate(medCategory = ifelse(med_name == "Antibiotics", "antibiotics", NA))
  medRaw %<>% mutate(medCategory = ifelse(
    grepl(paste0(medList$Enteral.Steroids,collapse = "|"), med_name, ignore.case = T) | grepl(paste0(medList$Enteral.Steroids,collapse = "|"), med_free_text_other, ignore.case = T),
    "Enteral.Steroids", medCategory))
  medRaw %<>% mutate(medCategory = ifelse(
    grepl(paste0(medList$Systemic.Steroids,collapse = "|"), med_name, ignore.case = T) | grepl(paste0(medList$Systemic.Steroids,collapse = "|"), med_free_text_other, ignore.case = T),
    "Systemic.Steroids", medCategory))

  medRaw %<>% mutate(medCategory = ifelse(
    grepl(paste0(medList$Enteral.Nutrition,collapse = "|"), med_name, ignore.case = T)  | grepl(paste0(medList$Enteral.Nutrition,collapse = "|"), med_free_text_other, ignore.case = T),
    "Enteral.Nutrition", medCategory)) %>%
    mutate(medCategory = ifelse(
      !is.na(med_free_text_other) & med_free_text_other == "Enteral Nutrition", "Enteral.Nutrition",medCategory
    ))

  medRaw %<>% mutate(medCategory = ifelse(
    grepl(paste0(medList$Biologics, collapse = "|"), med_name, ignore.case = T) | grepl(paste0(medList$Biologics, collapse = "|"), med_free_text_other, ignore.case = T),
    "Biologics", medCategory))
  medRaw %<>% mutate(medCategory = ifelse(
    grepl(paste0(medList$Proton.Pump.Inhibitors, collapse = "|"), med_name, ignore.case = T) | grepl(paste0(medList$Proton.Pump.Inhibitors, collapse = "|"), med_free_text_other, ignore.case = T),
    "Proton.Pump.Inhibitors", medCategory))
  medRaw %<>% mutate(medCategory = ifelse(
    grepl(paste0(medList$Thiopurines, collapse = "|"), med_name, ignore.case = T) | grepl(paste0(medList$Thiopurines, collapse = "|"), med_free_text_other, ignore.case = T),
    "Thiopurines", medCategory))
  medRaw %<>% mutate(medCategory = ifelse(
    grepl(paste0(medList$Other.Immunomodulator, collapse = "|"), med_name, ignore.case = T)|grepl(paste0(medList$Other.Immunomodulator, collapse = "|"), med_free_text_other, ignore.case = T),
    "Other.Immunomodulator", medCategory))
  medRaw %<>% mutate(medCategory = ifelse(
    grepl(paste0(medList$`5.ASA`, collapse = "|"), med_name, ignore.case = T)  | grepl(paste0(medList$`5.ASA`, collapse = "|"), med_free_text_other, ignore.case = T),
    "5.ASA", medCategory)) %>%
    mutate(medCategory = ifelse(
      med_name %in% c("Rowasa (per rectum)", "Canasa (per rectum)") | med_free_text_other %in% c("Rowasa (per rectum)", "Canasa (per rectum)"),
      "5.ASA", medCategory))

  medRaw %<>% mutate(medCategory = ifelse(
    grepl(paste0(medList$Other.med, collapse = "|"), med_name, ignore.case = T) | grepl(paste0(medList$Other.med, collapse = "|"), med_free_text_other, ignore.case = T),
    "Other.med", medCategory))

  medRaw %<>% filter(!is.na(medCategory)) %>%
    mutate(collect_date_time = as.Date(collect_date_time)) %>%
    select(ehb_id, medCategory, everything())

  return(medRaw)
}


#' a function used to code antiotics, Biologics, Enteral.Steroids, en and ppi as yes, no, past use.
#' @param medRaw: a data frame that contains abx, Biologics, en, Enteral.Steroids, ppi information, contains ehb_id instead of nautilusid
#' @param medName: the name of the medication variable
#' @param period: used to define the current usage of that medication and the unit is days. for example, if period = 10, then the current usage will be defined as within 10 days (10 included) of the sample collect date
#' @return medConverted: a data frame that contains yes or no, one column for one medication
# medConverted <- convert_alex_database_med_1(medRaw = medRaw, medName = "antibiotics", period = 10)
#' @export

convert_alex_database_med_1 <- function(medRaw, medName, period){

  if (medName == "antibiotics"){
    medRaw %<>% mutate(antibiotic_name = ifelse(is.na(antibiotic_name) | antibiotic_name == "Other", med_free_text_other, antibiotic_name))
  }

  # Yes = 2, Same day = 1, Past use = 0. No = -1, set NA start date to 1 as the same day
  medRaw %<>% filter(medCategory == medName) %>%
    mutate(medUsage = ifelse(collect_date_time - start_date > 0 & is.na(stop_date), 2, NA)) %>%
    mutate(medUsage = ifelse(collect_date_time - start_date > 0 & !is.na(stop_date) & collect_date_time - stop_date <= 10, 2, medUsage)) %>%
    mutate(medUsage = ifelse(collect_date_time - start_date > 0 & !is.na(stop_date) & collect_date_time - stop_date > 10, 0, medUsage)) %>%
    mutate(medUsage = ifelse(collect_date_time - start_date < 0, -1, medUsage)) %>%
    mutate(medUsage = ifelse(collect_date_time - start_date == 0, 1, medUsage)) %>%
    mutate(medUsage = ifelse(is.na(start_date) & !is.na(stop_date) & collect_date_time - stop_date <=10 & collect_date_time - stop_date >0,2,medUsage)) %>%
    mutate(medUsage = ifelse(is.na(start_date) & !is.na(stop_date) & collect_date_time - stop_date <0,1,medUsage)) %>%
    mutate(medUsage = ifelse(is.na(start_date) & !is.na(stop_date) & collect_date_time - stop_date > 10,0,medUsage)) %>%
    mutate(medUsage = ifelse(is.na(start_date) & is.na(stop_date) ,1, medUsage)) %>%
    group_by(ehb_id, collect_date_time) %>%
    filter(medUsage == max(medUsage)) %>%
    ungroup %>%
    select(-start_date, - stop_date, -dose) %>%
    unique %>%
    mutate(medUsageStr = ifelse(medUsage == 2, "Yes", NA)) %>%
    mutate(medUsageStr = ifelse(medUsage == 1, "Same day", medUsageStr)) %>%
    mutate(medUsageStr = ifelse(medUsage == 0, "Past use", medUsageStr)) %>%
    mutate(medUsageStr = ifelse(medUsage == -1, "No", medUsageStr))

  if (medName == "antibiotics"){
    medRaw %<>% mutate(antibiotic_name = ifelse(is.na(antibiotic_name) | antibiotic_name == "Other", med_free_text_other, antibiotic_name)) %>%
      select( - med_name, - med_free_text_other)
    colnames(medRaw)[which(colnames(medRaw) == "antibiotic_name")] <- paste0(medName, "_name")
  } else {
    medRaw %<>% mutate(med_name = ifelse(is.na(med_name) | med_name == "Other", med_free_text_other, med_name)) %>%
      select(-antibiotic_name, - med_free_text_other)
    colnames(medRaw)[which(colnames(medRaw) == "med_name")] <- paste0(medName,"_name")
  }

  colnames(medRaw)[which(colnames(medRaw) == "medUsageStr")] <- medName

  medConverted = medRaw
  return(medConverted)
}


#' a function has the same input variable as the previous function convert_alex_database_med_1, but use the previous function(convert_med) and return only ehb_id, collet_date_time and corresponding med usage(not including different med names in a same med category, for example, a sample with three different abx will only have one record)
#' @param medRaw: a data frame that contains abx, Biologics, en, Enteral.Steroids, ppi information, contains ehb_id instead of nautilusid
#' @param medName: the name of the medication variable
#' @param period: used to define the current usage of that medication and the unit is days. for example, if period = 10, then the current usage will be defined as within 10 days (10 included) of the sample collect date
#' @return medConverted a data frame contains ehb_id, collect_date_time, SampleType and antibiotics.Note it does not contain different medication names under a certain medCategory.
#'
# medConverted <- convert_alex_database_med_2(medRaw = medRaw, medName = "antibiotics", period = 10)

convert_alex_database_med_2 <- function(medRaw, medName, period){
  medConverted <- convert_alex_database_med_1(medRaw = medRaw, medName = medName, period = period)
  medConverted <- medConverted[,which(colnames(medConverted) %in% c("ehb_id","SampleType", "collect_date_time", "medUsage",medName))]
  medConverted %<>% group_by(ehb_id, SampleType,collect_date_time) %>%
    filter(medUsage == max(medUsage)) %>%
    ungroup %>%
    unique %>%
    select(-medUsage)
  return(medConverted)
}

#' use convert_alex_database_med_2 to convert and combine all the medRaw data
#' @param medRaw: a data frame that contains abx, Biologics, en, Enteral.Steroids, ppi information, contains ehb_id instead of nautilusid
#' @return medData a data frame in wide format sample in rows and medication in columns.
## medData = process_alex_database_med_log(medRaw =medRaw)
#' @export

process_alex_database_med_log <- function(medRaw){
  medList <- unique(medRaw$medCategory)
  medData <- convert_alex_database_med_2(medRaw, medList[1], period = 10)
  for (i in c(2:length(medList))){
    medData %<>% full_join(convert_alex_database_med_2(medRaw, medList[i], period = 10), by = c("ehb_id", "collect_date_time","SampleType"))
  }
  return(medData)
}
