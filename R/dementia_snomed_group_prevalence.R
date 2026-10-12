library(data.table)
library(readxl)
library(openxlsx)
library(stringr)


## ==========================================================
## 1. Paths
## ==========================================================


main_dir <- Sys.getenv("CPRD_DATA_DIR", unset = "")
if (!nzchar(main_dir) || !dir.exists(main_dir)) stop("Set CPRD_DATA_DIR to the secure input directory.")


bulk_excel <- file.path(main_dir, "Copy of dementia_codes_MR.xlsx")
matched_txt <- file.path(main_dir, "dementia matched.txt")
practice_txt <- file.path(main_dir, "extracta1_Extract_Practice_001.txt")
region_txt <- file.path(main_dir, "Region.txt")
patient_txt <- file.path(main_dir, "extracta1_Extract_Patient_001.txt")


out_dir <- file.path(
  main_dir,
  paste0("dementia_snomed_group_prevalence_", format(Sys.time(), "%Y%m%d_%H%M%S"))
)


dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)


txt_files <- list.files(
  main_dir,
  pattern = "Observation_.*\\.txt$",
  full.names = TRUE
)


if (length(txt_files) == 0) {
  stop("No observation txt files found in main_dir.")
}


clean_id <- function(x) {
  x <- as.character(x)
  x <- trimws(gsub("\\.0$", "", x))
  x
}


normalise_gender <- function(x) {
  x_chr <- trimws(as.character(x))
  x_low <- tolower(x_chr)
  fifelse(
    x_low %in% c("1", "m", "male", "man"), "Male",
    fifelse(
      x_low %in% c("2", "f", "female", "woman"), "Female",
      "Other/Unknown"
    )
  )
}


## ==========================================================
## 2. Read BULK sheet
## ==========================================================


bulk_raw <- read_excel(bulk_excel, sheet = "BULK")
bulk_dt <- as.data.table(bulk_raw)
bulk_dt[, original_order := .I]
setnames(bulk_dt, tolower(names(bulk_dt)))


required_cols <- c("snomedctconceptid", "proposed_group", "secondary")
missing_cols <- setdiff(required_cols, names(bulk_dt))
if (length(missing_cols) > 0) {
  stop("Missing required columns in BULK: ", paste(missing_cols, collapse = ", "))
}


bulk_dt[, snomedctconceptid := clean_id(snomedctconceptid)]
if (!"medcodeid" %in% names(bulk_dt)) {
  bulk_dt[, medcodeid := NA_character_]
} else {
  bulk_dt[, medcodeid := clean_id(medcodeid)]
}
bulk_dt[, proposed_group := as.character(proposed_group)]

bulk_dt[, secondary := as.character(secondary)]
bulk_dt[, secondary_flag := fifelse(tolower(trimws(secondary)) == "y", "Y", "N")]

## remove no meaningful snomed codes (edited: 10-Aug-2026) ##
dim(bulk_dt) # 886 9
# Assessment of dementia (procedure) removed
bulk_dt <- bulk_dt[bulk_dt$snomedctconceptid!='473203000',]
dim(bulk_dt) # 883 9

# Assessment for dementia (procedure) removed
bulk_dt <- bulk_dt[bulk_dt$snomedctconceptid!='869561000000101',]
dim(bulk_dt) # 881 9

# Dementia test (procedure) removed
bulk_dt <- bulk_dt[bulk_dt$snomedctconceptid!='165320004',]
dim(bulk_dt) # 880 9

# Thirty-seven item dementia test (procedure) removed
bulk_dt <- bulk_dt[bulk_dt$snomedctconceptid!='165322007',]
dim(bulk_dt) # 878 9

# Ten item dementia test (procedure) removed
bulk_dt <- bulk_dt[bulk_dt$snomedctconceptid!='165321000',]
dim(bulk_dt) # 876 9

# Thirty-seven item dementia test (procedure) removed
bulk_dt <- bulk_dt[bulk_dt$snomedctconceptid!='142732001',]
dim(bulk_dt) # 875 9

# Ten item dementia test (procedure) removed
bulk_dt <- bulk_dt[bulk_dt$snomedctconceptid!='142731008',]
dim(bulk_dt) # 874 9

# Dementia test (procedure) removed
bulk_dt <- bulk_dt[bulk_dt$snomedctconceptid!='142730009',]
dim(bulk_dt) # 873 9

# Dementia screening questionnaire completed (finding) removed
bulk_dt <- bulk_dt[bulk_dt$snomedctconceptid!='885371000000102',]
dim(bulk_dt) # 871 9

# Dementia screening questionnaire declined (situation) removed
bulk_dt <- bulk_dt[bulk_dt$snomedctconceptid!='885051000000108',]
dim(bulk_dt) # 870 9

# Referral for dementia assessment (procedure) removed
bulk_dt <- bulk_dt[bulk_dt$snomedctconceptid!='939231000000107',]
dim(bulk_dt) # 868 9

# Dementia screening questionnaire sent to patient (finding) removed
bulk_dt <- bulk_dt[bulk_dt$snomedctconceptid!='885351000000106',]
dim(bulk_dt) # 867 9

# Dementia assessment service (qualifier value) removed
bulk_dt <- bulk_dt[bulk_dt$snomedctconceptid!='893331000000102',]
dim(bulk_dt) # 865 9


## ==========================================================
## 3. Read dementia matched.txt
## ==========================================================


matched_dt <- fread(matched_txt)
setnames(matched_dt, tolower(names(matched_dt)))
matched_dt[, medcodeid := clean_id(medcodeid)]
matched_dt[, snomedctconceptid := clean_id(snomedctconceptid)]


matched_lookup <- unique(
  matched_dt[
    !is.na(medcodeid) & medcodeid != "" &
      !is.na(snomedctconceptid) & snomedctconceptid != "",
    .(medcodeid, snomedctconceptid)
  ]
)


## ==========================================================
## 4. Read practice, region, and patient demographic lookup files
## ==========================================================


practice_dt <- fread(
  practice_txt,

 sep = "\t",
 colClasses = list(character = c("pracid", "region")),
 showProgress = FALSE
)
setnames(practice_dt, tolower(names(practice_dt)))
if (!all(c("pracid", "region") %in% names(practice_dt))) {
  stop("Practice file must contain pracid and region columns.")
}
practice_lookup <- unique(
  practice_dt[
    !is.na(pracid) & pracid != "",
    .(pracid = clean_id(pracid), region = clean_id(region))
  ]
)


region_dt <- fread(
  region_txt,
  sep = "\t",
  colClasses = list(character = c("regionid", "Description")),
  showProgress = FALSE
)
setnames(region_dt, tolower(names(region_dt)))
if (!all(c("regionid", "description") %in% names(region_dt))) {
  stop("Region file must contain regionid and Description columns.")
}
region_lookup <- unique(
  region_dt[
    !is.na(regionid) & regionid != "",
    .(region = clean_id(regionid), region_description = trimws(as.character(description)))
  ]
)


patient_dt <- fread(patient_txt, sep = "\t", showProgress = FALSE)
setnames(patient_dt, tolower(names(patient_dt)))
if (!"patid" %in% names(patient_dt)) stop("Patient file must contain patid.")
gender_col_candidates <- c("gender", "sex", "gndr_cd", "gender_cd", "sexid")
gender_col <- gender_col_candidates[gender_col_candidates %in% names(patient_dt)][1]
if (is.na(gender_col)) {
  stop("No gender/sex column found in Patient file. Available columns are: ", paste(names(patient_dt), collapse = ", "))
}
patient_gender_lookup <- unique(
  patient_dt[, .(patid = clean_id(patid), gender_raw = as.character(get(gender_col)))]
)
patient_gender_lookup <- patient_gender_lookup[!is.na(patid) & patid != ""]
patient_gender_lookup[, gender := normalise_gender(gender_raw)]


## ==========================================================
## 5. Fill missing BULK medcodeid using SNOMED -> MedCode mapping
## ==========================================================


bulk_snomed <- unique(
  bulk_dt[
    !is.na(snomedctconceptid) & snomedctconceptid != "",
    .(snomedctconceptid, original_order)
  ]
)


bulk_medcode_filled <- merge(
  bulk_snomed,
  matched_lookup,
  by = "snomedctconceptid",
  all.x = TRUE,
  allow.cartesian = TRUE
)


fwrite(
  bulk_medcode_filled,
  file.path(out_dir, "BULK_snomed_to_medcodeid_filled_from_dementia_matched.csv")
)


## ==========================================================
## 6. Scan observation txt files
## ==========================================================


all_patid_list <- list()
snomed_hit_list <- list()
patient_practice_count_list <- list()

for (i in seq_along(txt_files)) {
  message("Processing txt file ", i, "/", length(txt_files), ": ", basename(txt_files[i]))
  dt <- fread(
    txt_files[i],
    sep = "\t",
    select = c("patid", "medcodeid", "pracid"),
    colClasses = list(character = c("patid", "medcodeid", "pracid")),
    showProgress = TRUE
  )
  dt[, patid := clean_id(patid)]
  dt[, medcodeid := clean_id(medcodeid)]
  dt[, pracid := clean_id(pracid)]


    all_patid_list[[i]] <- unique(dt[!is.na(patid) & patid != "", .(patid)])


    patient_practice_count_list[[i]] <- dt[
      !is.na(patid) & patid != "" & !is.na(pracid) & pracid != "",
      .N,
      by = .(patid, pracid)
    ]


    dt_code <- dt[!is.na(medcodeid) & medcodeid != ""]


    hit_dt <- merge(
      dt_code[, .(patid, medcodeid)],
      matched_lookup,
      by = "medcodeid",
      all.x = FALSE,
      allow.cartesian = TRUE
    )


    hit_dt <- hit_dt[
      snomedctconceptid %in% bulk_dt$snomedctconceptid,
      .(patid, medcodeid, snomedctconceptid)
    ]


    hit_dt <- unique(hit_dt, by = c("patid", "snomedctconceptid"))
    if (nrow(hit_dt) > 0) {
      snomed_hit_list[[length(snomed_hit_list) + 1]] <- hit_dt
    }


    rm(dt, dt_code, hit_dt)
    gc()
}


all_patids_dt <- unique(rbindlist(all_patid_list, use.names = TRUE))
total_dementia_patients <- uniqueN(all_patids_dt$patid)


snomed_hits_dt <- unique(
  rbindlist(snomed_hit_list, use.names = TRUE, fill = TRUE),
  by = c("patid", "snomedctconceptid")
)


fwrite(snomed_hits_dt, file.path(out_dir, "all_patient_snomed_hits_long.csv"))


cat("Total dementia patients:", total_dementia_patients, "\n")
cat("Patients with >=1 matched SNOMED:", uniqueN(snomed_hits_dt$patid), "\n")
cat("Unique SNOMED concepts observed:", uniqueN(snomed_hits_dt$snomedctconceptid), "\n")


## ==========================================================
## 7. Link patient to practice region and gender
## ==========================================================


patient_practice_counts <- rbindlist(patient_practice_count_list, use.names = TRUE, fill = TRUE)[
  , .(n_observation_records = sum(N, na.rm = TRUE)),
  by = .(patid, pracid)
]
setorder(patient_practice_counts, patid, -n_observation_records, pracid)
patient_main_practice <- patient_practice_counts[, .SD[1], by = patid]


patient_region <- merge(patient_main_practice, practice_lookup, by = "pracid", all.x = TRUE)
patient_region <- merge(patient_region, region_lookup, by = "region", all.x = TRUE)
patient_region_full <- merge(all_patids_dt, patient_region, by = "patid", all.x = TRUE)
patient_region_full[is.na(region) | region == "", region := "Unknown"]
patient_region_full[is.na(region_description) | region_description == "", region_description := "Unknown"]

region_summary <- patient_region_full[
  , .(n_patients = uniqueN(patid)),
  by = .(region, region_description)
]
region_summary[, prevalence_pct := round(n_patients / total_dementia_patients * 100, 2)]
setorder(region_summary, -n_patients)


patient_gender_full <- merge(all_patids_dt, patient_gender_lookup, by = "patid", all.x = TRUE)
patient_gender_full[is.na(gender) | gender == "", gender := "Other/Unknown"]


gender_summary <- patient_gender_full[, .(n_patients = uniqueN(patid)), by = gender]
gender_summary[, prevalence_pct := round(n_patients / total_dementia_patients * 100, 2)]
setorder(gender_summary, -n_patients)


patient_demographic_region <- merge(
  patient_gender_full,
  patient_region_full[, .(patid, pracid, region, region_description, n_observation_records)],
  by = "patid",
  all.x = TRUE
)
patient_demographic_region[is.na(region) | region == "", region := "Unknown"]
patient_demographic_region[is.na(region_description) | region_description == "", region_description := "Unknown"]


gender_summary_known <- gender_summary[gender != "Other/Unknown"]
if (nrow(gender_summary_known) > 0) {
  majority_gender_row <- gender_summary_known[which.max(n_patients)]
  majority_gender <- majority_gender_row$gender
  majority_gender_n <- majority_gender_row$n_patients
  majority_gender_pct <- majority_gender_row$prevalence_pct
} else {
  majority_gender <- "unknown gender"
  majority_gender_n <- NA_integer_
  majority_gender_pct <- NA_real_
}


top_regions <- region_summary[region_description != "Unknown"][order(-n_patients)]
top_region_text <- paste(head(top_regions$region_description, 5), collapse = ", ")
if (top_region_text == "") top_region_text <- "unknown geographical regions"


summary_desc <- paste0(
  "The study cohort included ", format(total_dementia_patients, big.mark = ","),
  " dementia patients, with the majority being ", tolower(majority_gender),
  " (n = ", format(majority_gender_n, big.mark = ","), ", ", majority_gender_pct,
  "%). Records related to participants from diverse geographical localities, including ",
  top_region_text, "."
)
cat(summary_desc, "\n")


fwrite(patient_region_full, file.path(out_dir, "patient_practice_region_linked.csv"))
fwrite(region_summary, file.path(out_dir, "patient_region_summary.csv"))
fwrite(patient_gender_full, file.path(out_dir, "patient_gender_linked.csv"))
fwrite(gender_summary, file.path(out_dir, "patient_gender_summary.csv"))
fwrite(patient_demographic_region, file.path(out_dir, "patient_gender_practice_region_linked.csv"))


## ==========================================================
## 8. SNOMED-level prevalence
## ==========================================================


snomed_summary <- snomed_hits_dt[, .(n_patients = uniqueN(patid)), by = snomedctconceptid]
snomed_summary[, prevalence_pct := round(n_patients / total_dementia_patients * 100, 2)]


bulk_result <- merge(bulk_dt, snomed_summary, by = "snomedctconceptid", all.x = TRUE)
bulk_result[is.na(n_patients), n_patients := 0L]
bulk_result[is.na(prevalence_pct), prevalence_pct := 0]
setorder(bulk_result, original_order)


fwrite(bulk_result, file.path(out_dir, "BULK_with_snomed_prevalence_filled_medcodeid.csv"))


## ==========================================================
## 9. Group-level prevalence
## ==========================================================


group_lookup <- unique(
  bulk_dt[

     !is.na(snomedctconceptid) & snomedctconceptid != "" &
       !is.na(proposed_group) & proposed_group != "",
     .(snomedctconceptid, proposed_group, secondary_flag)
    ],
    by = c("snomedctconceptid", "proposed_group", "secondary_flag")
)


group_hits_dt <- merge(
  snomed_hits_dt,
  group_lookup,
  by = "snomedctconceptid",
  all.x = FALSE,
  allow.cartesian = TRUE
)


group_hits_unique <- unique(group_hits_dt, by = c("patid", "proposed_group", "secondary_flag"))


group_summary <- group_hits_unique[
  , .(n_patients = uniqueN(patid)),
  by = .(proposed_group, secondary_flag)
]


group_detail <- unique(
   group_hits_dt,
   by = c("patid", "proposed_group", "secondary_flag", "snomedctconceptid")
)[
   , .(
      n_snomedctconceptid_matched = uniqueN(snomedctconceptid),
      n_patient_snomed_pairs = .N
   ),
   by = .(proposed_group, secondary_flag)
]


group_summary <- merge(group_summary, group_detail, by = c("proposed_group", "secondary_flag"), all.x = TRUE)
group_summary[, prevalence_pct := round(n_patients / total_dementia_patients * 100, 2)]
group_summary[, total_dementia_patients := total_dementia_patients]


group_order <- bulk_dt[
  !is.na(proposed_group) & proposed_group != "",
  .(first_original_order = min(original_order)),
  by = .(proposed_group, secondary_flag)
]


group_summary_full <- merge(group_order, group_summary, by = c("proposed_group", "secondary_flag"), all.x = TRUE)
group_summary_full[is.na(n_patients), n_patients := 0L]
group_summary_full[is.na(n_snomedctconceptid_matched), n_snomedctconceptid_matched := 0L]
group_summary_full[is.na(n_patient_snomed_pairs), n_patient_snomed_pairs := 0L]
group_summary_full[is.na(prevalence_pct), prevalence_pct := 0]
group_summary_full[is.na(total_dementia_patients), total_dementia_patients := total_dementia_patients]
setorder(group_summary_full, first_original_order)


fwrite(group_summary_full, file.path(out_dir, "group_level_snomed_prevalence.csv"))


## ==========================================================
## 10. Save Excel workbook
## ==========================================================


wb <- createWorkbook()


addWorksheet(wb, "BULK_SNOMED_prevalence")
writeData(wb, "BULK_SNOMED_prevalence", bulk_result)
freezePane(wb, "BULK_SNOMED_prevalence", firstRow = TRUE)
setColWidths(wb, "BULK_SNOMED_prevalence", cols = 1:ncol(bulk_result), widths = "auto")


addWorksheet(wb, "Group_prevalence")
writeData(wb, "Group_prevalence", group_summary_full)
freezePane(wb, "Group_prevalence", firstRow = TRUE)
setColWidths(wb, "Group_prevalence", cols = 1:ncol(group_summary_full), widths = "auto")


addWorksheet(wb, "Gender_summary")
writeData(wb, "Gender_summary", gender_summary)
freezePane(wb, "Gender_summary", firstRow = TRUE)
setColWidths(wb, "Gender_summary", cols = 1:ncol(gender_summary), widths = "auto")


addWorksheet(wb, "Region_summary")

writeData(wb, "Region_summary", region_summary)
freezePane(wb, "Region_summary", firstRow = TRUE)
setColWidths(wb, "Region_summary", cols = 1:ncol(region_summary), widths = "auto")


addWorksheet(wb, "SNOMED_hits_long")
writeData(wb, "SNOMED_hits_long", snomed_hits_dt)
freezePane(wb, "SNOMED_hits_long", firstRow = TRUE)


saveWorkbook(
  wb,
  file.path(out_dir, "dementia_snomed_group_demographic_region_results.xlsx"),
  overwrite = TRUE
)


cat("Done. Outputs saved to:\n", out_dir, "\n")
