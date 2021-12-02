box::use(
  utils[...]
)

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

#' @export
displayNames <- list("Acute DSS" = "AcDSS",
                     "Chronic DSS" = "cDSS",
                     "Oxazolone colitis" = "OxC",
                     "T cell transfer colitis" = "TC",
                     "TNF-dARE colitis" = "TdAc",
                     "TNF-dARE ileitis" = "TdAi",
                     "Acute TNBS colitis" = "AcTNBS",
                     "Chronic TNBS colitis" = "cTNBS",
                     "Casp8 KO colitis" = "C8KOc",
                     "DSS time course" = "DSSTC", 
                     "Wound Healing" = "WH",
                     "Eimeria vermiformis infection" = "EvInf",
                     "Helicobacter colitis infection" = "HhInf")

#' @export
modelDescriptions <- list(
  "AcDSS" = "Ph'nglui mglw'nafh Cthulhu R'lyeh wgah'nagl fhtagn. Zhro li'hee, kadishtu naflmnahn' bug fhtagn ch' f''fhalma, Nyarlathotep grah'n athg Azathoth 'bthnk n'ghft. ",
  "cDSS" = "Ph'nglui mglw'nafh Cthulhu R'lyeh wgah'nagl fhtagn. Zhro li'hee, kadishtu naflmnahn' bug fhtagn ch' f''fhalma, Nyarlathotep grah'n athg Azathoth 'bthnk n'ghft. ",
  "OxC" = "Ph'nglui mglw'nafh Cthulhu R'lyeh wgah'nagl fhtagn. Zhro li'hee, kadishtu naflmnahn' bug fhtagn ch' f''fhalma, Nyarlathotep grah'n athg Azathoth 'bthnk n'ghft. ",
  "TC" = "Ph'nglui mglw'nafh Cthulhu R'lyeh wgah'nagl fhtagn. Zhro li'hee, kadishtu naflmnahn' bug fhtagn ch' f''fhalma, Nyarlathotep grah'n athg Azathoth 'bthnk n'ghft. ",
  "TdAc" = "Ph'nglui mglw'nafh Cthulhu R'lyeh wgah'nagl fhtagn. Zhro li'hee, kadishtu naflmnahn' bug fhtagn ch' f''fhalma, Nyarlathotep grah'n athg Azathoth 'bthnk n'ghft. ",
  "TdAi" = "Ph'nglui mglw'nafh Cthulhu R'lyeh wgah'nagl fhtagn. Zhro li'hee, kadishtu naflmnahn' bug fhtagn ch' f''fhalma, Nyarlathotep grah'n athg Azathoth 'bthnk n'ghft. ",
  "AcTNBS" = "Ph'nglui mglw'nafh Cthulhu R'lyeh wgah'nagl fhtagn. Zhro li'hee, kadishtu naflmnahn' bug fhtagn ch' f''fhalma, Nyarlathotep grah'n athg Azathoth 'bthnk n'ghft. ",
  "cTNBS" = "Ph'nglui mglw'nafh Cthulhu R'lyeh wgah'nagl fhtagn. Zhro li'hee, kadishtu naflmnahn' bug fhtagn ch' f''fhalma, Nyarlathotep grah'n athg Azathoth 'bthnk n'ghft. ",
  "C8KOc" = "Ph'nglui mglw'nafh Cthulhu R'lyeh wgah'nagl fhtagn. Zhro li'hee, kadishtu naflmnahn' bug fhtagn ch' f''fhalma, Nyarlathotep grah'n athg Azathoth 'bthnk n'ghft. ",
  "EvInf" = "Ph'nglui mglw'nafh Cthulhu R'lyeh wgah'nagl fhtagn. Zhro li'hee, kadishtu naflmnahn' bug fhtagn ch' f''fhalma, Nyarlathotep grah'n athg Azathoth 'bthnk n'ghft. ",
  "HhInf" = "Ph'nglui mglw'nafh Cthulhu R'lyeh wgah'nagl fhtagn. Zhro li'hee, kadishtu naflmnahn' bug fhtagn ch' f''fhalma, Nyarlathotep grah'n athg Azathoth 'bthnk n'ghft. "
)

#' @export
geneLabels <- list(
  'mouse_genes' = unique(as.vector(read.csv("/DATA/mouse_genes.tsv", sep = '\t', header = FALSE)[['V3']]))
)

#' @export
imgLabels <- list(
  'logo1' = "www/logo1.png"
)