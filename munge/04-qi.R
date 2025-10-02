rsdata <- rsdata %>%
  mutate(
    qi_lvef = if_else(!is.na(LVEF_PERCENT) | !is.na(LVEF_SEMIQUANTITATIVE), 1, 0),
    qi_ntprobnp = if_else(!is.na(NT_PROBNP) | !is.na(NT_PROBNP_24H), 1, 0),
    qi_nyha = if_else(!is.na(FUNCTION_CLASS_NYHA), 1, 0),
    qi_qol = if_else(!is.na(LIFEQUALITY_SCORE), 1, 0),
    qi_tf = if_else(!is.na(P_TRANSFERRIN) & !is.na(S_FERRITIN), 1, 0),
    qi_ras = case_when(
      FOLLOWUP_UNIT == "DECEASED" ~ NA_real_,
      is.na(ef_cat3imp) | ef_cat3imp != "HFrEF" ~ NA_real_,
      is.na(ACE_INHIBITOR) | is.na(A2_BLOCKER_ARB) | is.na(ARNI) ~ NA_real_,
      ACE_INHIBITOR == "YES" | A2_BLOCKER_ARB == "YES" | ARNI == "YES" ~ 1,
      TRUE ~ 0
    ),
    qi_arni = case_when(
      FOLLOWUP_UNIT == "DECEASED" ~ NA_real_,
      is.na(ef_cat3imp) | ef_cat3imp != "HFrEF" ~ NA_real_,
      is.na(ARNI) ~ NA_real_,
      ARNI == "YES" ~ 1,
      TRUE ~ 0
    ),
    qi_bbl = case_when(
      FOLLOWUP_UNIT == "DECEASED" ~ NA_real_,
      is.na(ef_cat3imp) | ef_cat3imp != "HFrEF" ~ NA_real_,
      is.na(BETA_BLOCKER) ~ NA_real_,
      BETA_BLOCKER == "YES" ~ 1,
      TRUE ~ 0
    ),
    qi_mra = case_when(
      FOLLOWUP_UNIT == "DECEASED" ~ NA_real_,
      is.na(ef_cat3imp) | ef_cat3imp != "HFrEF" ~ NA_real_,
      is.na(MRA) ~ NA_real_,
      MRA == "YES" ~ 1,
      TRUE ~ 0
    ),
    qi_sglt2 = case_when(
      FOLLOWUP_UNIT == "DECEASED" ~ NA_real_,
      is.na(SGLT2) ~ NA_real_,
      SGLT2 == "YES" ~ 1,
      TRUE ~ 0
    ),
    qi_sglt2_ref = case_when(
      FOLLOWUP_UNIT == "DECEASED" ~ NA_real_,
      is.na(ef_cat3imp) | ef_cat3imp != "HFrEF" ~ NA_real_,
      is.na(SGLT2) ~ NA_real_,
      SGLT2 == "YES" ~ 1,
      TRUE ~ 0
    ),
    qi_sglt2_mref = case_when(
      FOLLOWUP_UNIT == "DECEASED" ~ NA_real_,
      is.na(ef_cat3imp) | ef_cat3imp != "HFmrEF" ~ NA_real_,
      is.na(SGLT2) ~ NA_real_,
      SGLT2 == "YES" ~ 1,
      TRUE ~ 0
    ),
    qi_sglt2_pef = case_when(
      FOLLOWUP_UNIT == "DECEASED" ~ NA_real_,
      is.na(ef_cat3imp) | ef_cat3imp != "HFpEF" ~ NA_real_,
      is.na(SGLT2) ~ NA_real_,
      SGLT2 == "YES" ~ 1,
      TRUE ~ 0
    ),
    qi_4 = case_when(
      is.na(qi_ras) | is.na(qi_bbl) | is.na(qi_mra) | is.na(qi_sglt2_ref) ~ NA_real_,
      qi_ras + qi_bbl + qi_mra + qi_sglt2_ref == 4 ~ 1,
      TRUE ~ 0
    ),
    # crt
    qi_crt = case_when(
      is.na(efcrt_catimp) | efcrt_catimp != "<40/<=35" ~ NA_real_,
      # is.na(QRS_WIDTHimp) | is.na(LEFT_BRANCH_BLOCKimp) | is.na(EKG_RHYTHMimp) ~ NA_real_,
      FOLLOWUP_UNIT == "DECEASED" ~ NA_real_,
      is.na(DEVICE_THERAPY) ~ NA_real_,
      DEVICE_THERAPY %in% c("CRT", "CRT_D") ~ 1,
      QRS_WIDTHimp > 130 & LEFT_BRANCH_BLOCKimp == "YES" ~ 0,
      TRUE ~ NA_real_
    ),

    # icd
    qi_icd = case_when(
      is.na(efcrt_catimp) | efcrt_catimp != "<40/<=35" ~ NA_real_,
      FOLLOWUP_UNIT == "DECEASED" ~ NA_real_,
      is.na(DEVICE_THERAPY) ~ NA_real_,
      DEVICE_THERAPY %in% c("ICD", "CRT_D") ~ 1,
      TRUE ~ 0
    ),
    qi_fys = case_when(
      # FOLLOWUP_UNIT == "DECEASED" ~ NA_real_,
      is.na(PARTICIPATION_HF_TRAINING) ~ NA_real_,
      PARTICIPATION_HF_TRAINING == "YES" ~ 1,
      TRUE ~ 0
    )
  )


# uppföljningar 3 mån

follow <- rsdata %>%
  filter(ttype == "3-month follow-up") %>%
  mutate(followup = 1) %>%
  select(patientreference, followup)

rsdata <- left_join(rsdata, follow, by = "patientreference")

rsdata <- rsdata %>%
  mutate(
    timedead = as.numeric(befdoddtm - indexdtm),
    qi_followreg3m = case_when(
      TYPE != "INDEX" ~ NA_real_,
      timedead < 30.5 * 3 & !is.na(timedead) | FOLLOWUP_UNIT == "DECEASED" ~ NA_real_,
      indexdtm > ymd(paste0(global_year, "-07-01")) ~ NA_real_,
      followup == 1 ~ 1,
      TRUE ~ 0
    )
  ) %>%
  select(-followup, -timedead)


rsdata <- rsdata %>%
  mutate(across(starts_with("qi_"), as.factor))


# Create info for qi variables with names and limits ----------------------

qivar <- names(rsdata %>% select(starts_with("qi_")))

qiinfo <- tibble(qivar = qivar)

qiinfo <- qiinfo %>%
  mutate(
    qishortname = case_when(
      qivar == "qi_lvef" ~ "LVEF",
      qivar == "qi_ntprobnp" ~ "NT-proBNP",
      qivar == "qi_nyha" ~ "NYHA",
      qivar == "qi_qol" ~ "QoL EQ-5D",
      qivar == "qi_tf" ~ "Transferrin and ferritin",
      qivar == "qi_ras" ~ "ACEI/ARB/ARNI",
      qivar == "qi_arni" ~ "ARNI",
      qivar == "qi_bbl" ~ "Beta-blockers",
      qivar == "qi_mra" ~ "MRA",
      qivar == "qi_4" ~ "Quadruple therapy",
      qivar == "qi_sglt2" ~ "SGLT2i",
      qivar == "qi_sglt2_ref" ~ "SGLT2i (HFrEF)",
      qivar == "qi_sglt2_mref" ~ "SGLT2i (HFmrEF)",
      qivar == "qi_sglt2_pef" ~ "SGLT2i (HFpEF)",
      qivar == "qi_crt" ~ "CRT",
      qivar == "qi_icd" ~ "ICD",
      qivar == "qi_fys" ~ "Physical exercise",
      qivar == "qi_followreg3m" ~ "Follow-up performed"
    ),
    qishortname_sv = case_when(
      qivar == "qi_lvef" ~ "LVEF",
      qivar == "qi_ntprobnp" ~ "NT-proBNP",
      qivar == "qi_nyha" ~ "NYHA",
      qivar == "qi_qol" ~ "Hälsotillstånd",
      qivar == "qi_tf" ~ "Transferrin och ferritin",
      qivar == "qi_ras" ~ "ACEI/ARB/ARNI",
      qivar == "qi_arni" ~ "ARNI",
      qivar == "qi_bbl" ~ "Betablockad",
      qivar == "qi_mra" ~ "MRA",
      qivar == "qi_4" ~ "Kvadrupelbehandling",
      qivar == "qi_sglt2" ~ "SGLT2i",
      qivar == "qi_sglt2_ref" ~ "SGLT2i (HFrEF)",
      qivar == "qi_sglt2_mref" ~ "SGLT2i (HFmrEF)",
      qivar == "qi_sglt2_pef" ~ "SGLT2i (HFpEF)",
      qivar == "qi_crt" ~ "CRT",
      qivar == "qi_icd" ~ "ICD",
      qivar == "qi_fys" ~ "Fysisk träning",
      qivar == "qi_followreg3m" ~ "Uppföljningsbesök"
    ),
    qiname = case_when(
      qivar == "qi_lvef" ~ "documentation of LVEF at index",
      qivar == "qi_ntprobnp" ~ "documentation of NT-proBNP at index",
      qivar == "qi_nyha" ~ "documentation of NYHA class at index",
      qivar == "qi_qol" ~ "assessment of QoL EQ-5D at index",
      qivar == "qi_tf" ~ "documentation of transferrin and ferritin",
      qivar == "qi_ras" ~ "prescribed ACEI/ARB/ARNI in patients with HFrEF",
      qivar == "qi_arni" ~ "prescribed ARNI in patients with HFrEF",
      qivar == "qi_bbl" ~ "prescribed beta-blocker in patients with HFrEF",
      qivar == "qi_mra" ~ "prescribed MRA in patients with HFrEF",
      qivar == "qi_4" ~ "prescribed quadruple therapy in patients with HFrEF",
      qivar == "qi_sglt2" ~ "prescribed SGLT2 inhibitor",
      qivar == "qi_sglt2_ref" ~ "prescribed SGLT2 inhibitor in patients with HFrEF",
      qivar == "qi_sglt2_mref" ~ "prescribed SGLT2 inhibitor in patients with HFmrEF",
      qivar == "qi_sglt2_pef" ~ "prescribed SGLT2 inhibitor in patients with HFpEF",
      qivar == "qi_crt" ~ "CRT implanted in patients with LVEF <36/40%, LBBB and QRS >130ms",
      qivar == "qi_icd" ~ "ICD implanted in patients with LVEF <36/40%",
      qivar == "qi_fys" ~ "participation in physical exercise for HF",
      qivar == "qi_followreg3m" ~ "follow-up visit performed and documented"
    ),
    ll = case_when(
      qivar == "qi_lvef" ~ 0.8,
      qivar == "qi_ntprobnp" ~ 0.7,
      qivar == "qi_nyha" ~ 0.9,
      qivar == "qi_qol" ~ 0.7,
      qivar == "qi_tf" ~ 0.4,
      qivar == "qi_ras" ~ 0.8,
      qivar == "qi_arni" ~ 0.1,
      qivar == "qi_bbl" ~ 0.8,
      qivar == "qi_mra" ~ 0.6,
      qivar == "qi_4" ~ 0.4,
      qivar == "qi_sglt2" ~ 0.6,
      qivar == "qi_sglt2_ref" ~ 0.6,
      qivar == "qi_sglt2_mref" ~ 0.3,
      qivar == "qi_sglt2_pef" ~ 0.3,
      qivar == "qi_crt" ~ 0.5,
      qivar == "qi_icd" ~ 0.5,
      qivar == "qi_fys" ~ 0.3,
      qivar == "qi_followreg3m" ~ 0.88
    ),
    ul = case_when(
      qivar == "qi_lvef" ~ 0.9,
      qivar == "qi_ntprobnp" ~ 0.8,
      qivar == "qi_nyha" ~ 0.95,
      qivar == "qi_qol" ~ 0.8,
      qivar == "qi_tf" ~ 0.6,
      qivar == "qi_ras" ~ 0.9,
      qivar == "qi_arni" ~ 0.3,
      qivar == "qi_bbl" ~ 0.9,
      qivar == "qi_mra" ~ 0.7,
      qivar == "qi_4" ~ 0.5,
      qivar == "qi_sglt2" ~ 0.7,
      qivar == "qi_sglt2_ref" ~ 0.7,
      qivar == "qi_sglt2_mref" ~ 0.4,
      qivar == "qi_sglt2_pef" ~ 0.4,
      qivar == "qi_crt" ~ 0.6,
      qivar == "qi_icd" ~ 0.6,
      qivar == "qi_fys" ~ 0.4,
      qivar == "qi_followreg3m" ~ 0.92
    ),
    timepoint = case_when(
      qivar == "qi_lvef" ~ "Index",
      qivar == "qi_ntprobnp" ~ "Index",
      qivar == "qi_nyha" ~ "Index",
      qivar == "qi_qol" ~ "Index",
      qivar == "qi_tf" ~ "3-month follow-up",
      qivar == "qi_ras" ~ "3-month follow-up",
      qivar == "qi_arni" ~ "3-month follow-up",
      qivar == "qi_bbl" ~ "3-month follow-up",
      qivar == "qi_mra" ~ "3-month follow-up",
      qivar == "qi_4" ~ "3-month follow-up",
      qivar == "qi_sglt2" ~ "3-month follow-up",
      qivar == "qi_sglt2_ref" ~ "3-month follow-up",
      qivar == "qi_sglt2_mref" ~ "3-month follow-up",
      qivar == "qi_sglt2_pef" ~ "3-month follow-up",
      qivar == "qi_followreg3m" ~ "Index",
      qivar == "qi_crt" ~ "3-month follow-up",
      qivar == "qi_icd" ~ "3-month follow-up",
      qivar == "qi_fys" ~ "3-month follow-up",
    ),
    qino = case_when(
      qivar == "qi_lvef" ~ 1,
      qivar == "qi_ntprobnp" ~ 2,
      qivar == "qi_nyha" ~ 3,
      qivar == "qi_qol" ~ 4,
      qivar == "qi_tf" ~ 6,
      qivar == "qi_ras" ~ 8,
      qivar == "qi_arni" ~ 9,
      qivar == "qi_bbl" ~ 10,
      qivar == "qi_mra" ~ 11,
      qivar == "qi_4" ~ 7,
      qivar == "qi_sglt2" ~ 12,
      qivar == "qi_sglt2_ref" ~ 13,
      qivar == "qi_sglt2_mref" ~ 14,
      qivar == "qi_sglt2_pef" ~ 15,
      qivar == "qi_crt" ~ 16,
      qivar == "qi_icd" ~ 17,
      qivar == "qi_fys" ~ 18,
      qivar == "qi_followreg3m" ~ 5
    )
  ) %>%
  arrange(qino)
