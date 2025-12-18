if (getRversion() >= "2.15.1") {
  utils::globalVariables(c(
    "crf_data_exclude_phi",
    "coenroll_exclude_phi",
    "dd_field_name",
    "dd_revision_field_value",
    "subject_label",
    "site_label",
    "Site",
    "items",
    "edc",
    "ptdemog",
    "inner_join",
    ".",
    "latest",
    "de_race",
    "abcds",
    "full_join",
    "data_dictionary"
  ))
}


.all_reports <- c(
  "enrollment_by_cycle",
  "demographics",
  "cognition_task_counts",
  "ntgedsd_counts",
  "imaging_scan_counts",
  "imaging_scan_counts_controls",
  "mri_sequence_counts",
  "mri_sequence_counts_controls",
  "imaging_scan_counts_by_site",
  "consensus_primary_by_cycle",
  "healthhistory",
  "apoe",
  "karyotype",
  "karyotype_by_site",
  "blood_draw_counts",
  "blood_draw_counts_controls",
  "csf_counts",
  "csf_counts_controls",
  "omics"
)


.cognition_tasks <- c(
  "blockhaxby",
  "blockwisc",
  "catdog",
  "dsmse",
  "moca",
  "pegboard",
  "ppvt5",
  "radd",
  "recall",
  "tbatgs",
  "verbal",
  "vmi"
)


.mri_sequence_key <- tibble::tribble(
  ~id , ~abbreviation      , ~description                                                                        ,
  "1" , "T1 MPRAGE/ISSPGR" , "T1 MPRAGE/ISSPGR Sagittal 3D Accelerated MPRAGE/IRSPGR (ADNI3 Sequence)"           ,
  "2" , "3D FLAIR"         , "3D FLAIR Sagittal 3D FLAIR (ADNI3 Sequence)"                                       ,
  "3" , "T2*/SWI"          , "T2*/SWI Axial 3D ME T2 GRE – Axial 3D T2 GRE – Axial T2 Star/GRE"              ,
  "4" , "DTI"              , "DTI Axial DTI PA (multiband if applicable) Axial DTI AP (multiband if applicable)" ,
  "5" , "ASL"              , "ASL Axial 3D pCASL or Axial 3D PASL"                                               ,
  "6" , "T2 FSE"           , "T2 FSE Sagittal 3D T2 Weighted Sequence"                                           ,
  "7" , "rs-fMRI"          , "rs-fMRI Axial fcMRI (multiband if applicable)"                                     ,
)

.imaging_key <-
  tibble::tribble(
    ~atri_scan_names , ~loni_scan_names , ~scan_labels       ,
    "amymeta"        , "amy"            , "Amyloid PET Scan" ,
    "taumeta"        , "tau"            , "Tau PET Scan"     ,
    "mrimeta"        , "mri"            , "MRI Imaging"      ,
    "fdgmeta"        , "fdg"            , "FDG PET Scan"     ,
  )


# fmt: skip
.disclosure_key <- list(
  abcds = list(
    demographics = c("de_eval_date", "de_gender", "age_at_visit"),
    dsmse = c("dsto2", "dsla2", "colshbx", "dsme1", "dsvs1"),
    recall = c("frssa", "frssb", "crssa", "crssb"),
    ntgedsd = c("adltot", "ntlctot", "ntswtot", "ntambtot", "ntmemtot", "ntbehtot", "ntsrptot", "ntootot"),
    exam = c("ht", "htu", "wt", "wtu", "bpsys", "bpdia")
  ),
  trcds = list()
)
