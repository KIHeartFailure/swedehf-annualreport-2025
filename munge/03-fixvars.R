# sexage <- sexage %>% group_by(patientreference) %>% slice(1) %>% ungroup()
rsdata <- left_join(
  rsdata,
  sexage %>% select(patientreference, SEX, DATE_OF_BIRTH, befdoddtm),
  by = "patientreference"
)

rsdata <- rsdata %>%
  mutate(
    indexdtm = coalesce(DATE_FOR_ADMISSION, VISIT_DATE),
    indexyear = factor(year(indexdtm)),
    age = as.numeric(floor((indexdtm - DATE_OF_BIRTH) / 365.25)),
    age_cat = factor(
      case_when(
        age < 65 ~ 1,
        age < 75 ~ 2,
        age < 85 ~ 3,
        age >= 85 ~ 4
      ),
      levels = 1:4, labels = c("<65", "65-74", "75-84", "\u226585")
    ),
    sex = factor(case_when(
      SEX == "FEMALE" ~ "Female",
      SEX == "MALE" ~ "Male"
    )),
    PRIMARY_ETIOLOGY = factor(case_when(
      PRIMARY_ETIOLOGY == "ALCOHOL" ~ 4,
      PRIMARY_ETIOLOGY == "DILATED" ~ 3,
      PRIMARY_ETIOLOGY == "HEARTVALVE" ~ 5,
      PRIMARY_ETIOLOGY == "HYPERTENSION" ~ 1,
      PRIMARY_ETIOLOGY == "ISCHEMIC" ~ 2,
      PRIMARY_ETIOLOGY == "OTHER" ~ 6,
    ), levels = 1:6, labels = c(
      "Hypertension", "ischemic heart disease", "Dilated cardiomyopathy",
      "Known alcoholic cardiomyopathy", "Heart valve disease", "Other"
    )),
    REVASCULARIZATION = factor(
      case_when(
        REVASCULARIZATION == "CABG" ~ 2,
        REVASCULARIZATION == "CABG_AND_PCI" ~ 4,
        REVASCULARIZATION == "NO" ~ 1,
        REVASCULARIZATION == "PCI" ~ 3
      ),
      levels = 1:4, labels = c("No", "CABG", "PCI", "CABG+PCI")
    ),
    DIABETES = factor(
      case_when(
        DIABETES == "NO" ~ 1,
        DIABETES == "TYPE_1" ~ 2,
        DIABETES == "TYPE_2" ~ 3
      ),
      levels = 1:3, labels = c("No", "Type 1", "Type 2")
    ),
    HEART_VALVE_SURGERY = factor(
      case_when(
        HEART_VALVE_SURGERY == "AORTA" ~ 2,
        HEART_VALVE_SURGERY == "AORTA_AND_MITRALIS" ~ 4,
        HEART_VALVE_SURGERY == "MITRALIS" ~ 3,
        HEART_VALVE_SURGERY == "NO" ~ 1,
        HEART_VALVE_SURGERY == "OTHER" ~ 5
      ),
      levels = 1:5, labels = c("No", "Aorta", "Mitralis", "Aorta + Mitralis", "Other")
    ),
    tmp_timedurationhf = indexdtm - DATE_FOR_DIAGNOSIS_HF,
    tmp_timedurationhf2 = case_when(
      tmp_timedurationhf < 6 * 30.5 ~ "LESS_THAN_6_MONTHS",
      tmp_timedurationhf >= 6 * 30.5 ~ "MORE_THAN_6_MONTHS"
    ),
    hfdur = coalesce(tmp_timedurationhf2, DURATION_OF_HF),
    hfdur = factor(case_when(
      hfdur == "LESS_THAN_6_MONTHS" ~ 1,
      hfdur == "MORE_THAN_6_MONTHS" ~ 2
    ), levels = 1:2, labels = c("HF duration < 6 mo at index", "HF duration \u2265 6 mo at index")),
    ef_cat = factor(case_when(
      LVEF_SEMIQUANTITATIVE == "NORMAL" | LVEF_PERCENT >= 50 ~ 1,
      LVEF_SEMIQUANTITATIVE == "MILD" | LVEF_PERCENT >= 40 ~ 1,
      LVEF_SEMIQUANTITATIVE == "MODERATE" | LVEF_PERCENT >= 30 ~ 2,
      LVEF_SEMIQUANTITATIVE == "SEVERE" | LVEF_PERCENT < 30 ~ 2
    ), labels = c(">=40", "<40"), levels = 1:2),
    ef_cat3 = factor(case_when(
      LVEF_SEMIQUANTITATIVE == "NORMAL" | LVEF_PERCENT >= 50 ~ 1,
      LVEF_SEMIQUANTITATIVE == "MILD" | LVEF_PERCENT >= 40 ~ 2,
      LVEF_SEMIQUANTITATIVE == "MODERATE" | LVEF_PERCENT >= 30 ~ 3,
      LVEF_SEMIQUANTITATIVE == "SEVERE" | LVEF_PERCENT < 30 ~ 3
    ), labels = c("HFpEF", "HFmrEF", "HFrEF"), levels = 1:3),
    efcrt_cat = factor(case_when(
      LVEF_SEMIQUANTITATIVE %in% c("NORMAL", "MILD") | LVEF_PERCENT > 35 ~ 1,
      LVEF_SEMIQUANTITATIVE %in% c("MODERATE", "SEVERE") | LVEF_PERCENT <= 35 ~ 2
    ), labels = c(">=40/>35", "<40/<=35"), levels = 1:2),
    FUNCTION_CLASS_NYHA = str_replace(FUNCTION_CLASS_NYHA, "NYHA_", " "),
    location = factor(case_when(
      vtype == "Primary care" ~ 3,
      PROCESS_DEFINITION_REFERENCE %in% c("IX_OV", "FO", "YFO") & vtype == "Hospital" ~ 2,
      PROCESS_DEFINITION_REFERENCE %in% c("IX_SV") ~ 1
    ), levels = 1:3, labels = c("In-hospital", "Out-patient hosptial", "Primary care"))
  )


ynvars <- c(
  "EARLIER_CARDIAC_ARREST", "HYPERTENSION", "ATRIAL_FIBRILLATION_FLUTTER",
  "CHRONIC_LUNG_DISEASE", "HEART_VALVE_DISEASE", "DILATED_CARDIOMYOPATHY"
)

yn_func <- function(var) {
  var <- factor(case_when(
    var == "NO" ~ 0,
    var == "YES" ~ 1
  ), levels = 0:1, labels = c("No", "Yes"))
}

rsdata <- rsdata %>%
  mutate(across(all_of(ynvars), yn_func))



# Imputation, LOCF --------------------------------------------------------

rsdata <- rsdata %>%
  group_by(patientreference) %>%
  arrange(indexdtm) %>%
  mutate(across(all_of(c("QRS_WIDTH", "LEFT_BRANCH_BLOCK", "EKG_RHYTHM", "efcrt_cat", "ef_cat3", "hfdur")), ~ zoo::na.locf(.x, na.rm = F), .names = "{.col}imp")) %>%
  ungroup()


# Type of visit -----------------------------------------------------------

rsdataindex <- rsdata %>%
  filter(TYPE == "INDEX") %>%
  group_by(patientreference) %>%
  arrange(indexdtm) %>%
  slice(1) %>% # 2 dups
  ungroup() %>%
  select(patientreference, indexdtm)

rsdata <- left_join(
  rsdata,
  rsdataindex,
  by = "patientreference",
  suffix = c("", "index")
) %>%
  mutate(
    ttype = case_when(
      TYPE == "INDEX" ~ 1,
      TYPE == "FOLLOWUP" ~ 2,
      TYPE == "YEARLY_FOLLOWUP" ~ 3
    ),
    diff_timeadmission = as.numeric(indexdtm - indexdtmindex),
    ttype = if_else(diff_timeadmission >= 1.5 * 365 & TYPE == "YEARLY_FOLLOWUP", 4, ttype),
    ttype = factor(ttype,
      levels = 1:4,
      labels = c("Index", "3-month follow-up", "1-year follow-up", "2+-year follow-up")
    )
  )

rsdata <- rsdata %>%
  mutate(across(where(is.character), as.factor))

# check dups --------------------------------------------------------------

koll2 <- rsdata %>%
  group_by(patientreference, ttype, indexyear) %>%
  slice(2) %>%
  ungroup() %>%
  count(indexyear, ttype)
