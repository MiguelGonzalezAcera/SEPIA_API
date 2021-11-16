box::use()

# Generate lists of options for the displays
#' @export
fullExp <- list(
  "DSSTC" = "DSS_TimeCourse",
  "WH" = "WoundHealing"
)

#' @export
singleExp <- list(
  "AcDSS" = list("project" = "MouseModelsInflammation", "tabid" = "MouseModelsInflammation_DSS_Cerl"),
  "cDSS" = list("project" = "MouseModelsInflammation", "tabid" = "MouseModelsInflammation_cDSS_Cerl"),
  "OxC" = list("project" = "MouseModelsInflammation", "tabid" = "MouseModelsInflammation_OxC_Cerl"),
  "TC" = list("project" = "MouseModelsInflammation", "tabid" = "MouseModelsInflammation_TC_RKO"),
  "TdAc" = list("project" = "TNFdARE_model", "tabid" = "TNFdARE_model_Col_dARE_Col_WT"),
  "TdAi" = list("project" = "TNFdARE_model", "tabid" = "TNFdARE_model_SI_dARE_SI_WT"),
  "AcTNBS" = list("project" = "TNBS_model", "tabid" = "TNBS_model_TNBS_Ac_Healthy"),
  "cTNBS" = list("project" = "TNBS_model", "tabid" = "TNBS_model_TNBS_Chr_Healthy"),
  "C8KOc" = list("project" = "Casp8Colon", "tabid" = "Casp8Colon_Col_Casp8dIEC_Col_Casp8flox"),
  "EvInf" = list("project" = "Eimeria_vermiformis_model", "tabid" = "Eimeria_vermiformis_model_EV_WT"),
  "HhInf" = list("project" = "HhColitis", "tabid" = "HhColitis_HhCol_SSt")
)