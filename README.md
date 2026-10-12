# CPRD Aurum dementia SNOMED CT prevalence analysis

Supplementary R script for within-cohort dementia SNOMED CT concept, subtype, gender and regional summaries.

## Method

The workflow uses CPRD Aurum observation records and a supplied dementia SNOMED CT codelist (BULK worksheet). It maps MedCodeIds to SNOMED CT concepts via a matching file; counts distinct patients per SNOMED concept and per proposed subtype/secondary flag; and reports proportions relative to the distinct patients in the observation cohort. Patient gender is linked using patient records. Practice region is linked using the practice and region lookup files, with the most frequently observed practice selected for each patient. Patients may appear in more than one subtype.

These are **within-dementia-cohort proportions**, not population dementia prevalence.

## Files

- `R/dementia_snomed_group_prevalence.R`: analysis code, separated from this README.
- `.gitignore`: excludes restricted inputs and analysis outputs.

## Dependencies

R packages: `data.table`, `readxl`, `openxlsx`, `stringr`.

```r
install.packages(c("data.table", "readxl", "openxlsx", "stringr"))
```

## Required restricted input files

Keep all of the following in an authorised secure directory:

- `Copy of dementia_codes_MR.xlsx` (BULK sheet)
- `dementia matched.txt` (MedCodeId–SNOMED mapping)
- `extracta1_Extract_Practice_001.txt` (practice geography)
- `Region.txt` (region lookup)
- `extracta1_Extract_Patient_001.txt` (patient demographics)
- `Observation_*.txt` (observation extracts)

## Run

Set `CPRD_DATA_DIR` to the authorised directory holding the input files. Run:

```sh
Rscript R/dementia_snomed_group_prevalence.R
```

The script writes CSV summaries and `dementia_snomed_group_demographic_region_results.xlsx` into a timestamped output directory.

## Caveats and governance

The script assumes that the observation extracts already represent the dementia cohort. Regional attribution is based on the modal observed practice, not necessarily residential location. Gender recoding and all input column names should be checked against the source data dictionary. The script has not been executed against the restricted CPRD datasets here.

**Never upload CPRD patient-level records, restricted mappings, or generated patient-level results to this public repository.**

Source: *Supplementary Methods: Calculation of Dementia SNOMED CT, Dementia Subtype, Gender, and Regional Prevalence (with R code)*.
