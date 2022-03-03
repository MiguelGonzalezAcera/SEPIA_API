box::use(
  utils[...],
  RMariaDB[...]
)


# Ask database for the usernames and passwords for the tool
userQuery <- function(){
  # Establish the connection to the projects database
  usersDb <- dbConnect(RMariaDB::MariaDB(), user='sepia', password="sepia_TRR241", dbname='Users', host='localhost')
  
  # Create the query with the project name
  queryUserText <- "select * from SepiaUsers;"
  
  # Run the query against the database and fetch the resulting dataframe
  rsUserInsert <- dbSendQuery(usersDb, queryUserText)
  dbUserRows <- dbFetch(rsUserInsert)
  
  # Clear the query
  dbClearResult(rsUserInsert)
  
  # Disconnect the database
  dbDisconnect(usersDb)
  
  # return projects dataframe
  return(dbUserRows)
}

#Temporal table for usernames and passwords
#' @export
userBase <- userQuery()

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
  "DSSTCim" = list("project" = "DSS_TimeCourse", "tabid" = "DSS_TimeCourse_Inf_mid_Healthy"),
  "DSSTCih" = list("project" = "DSS_TimeCourse", "tabid" = "DSS_TimeCourse_Inf_hi_Healthy"),
  "DSSTCrm" = list("project" = "DSS_TimeCourse", "tabid" = "DSS_TimeCourse_Rec_mod_Healthy"),
  "DSSTCrf" = list("project" = "DSS_TimeCourse", "tabid" = "DSS_TimeCourse_Rec_ful_Healthy"),
  "OxC" = list("project" = "MouseModelsInflammation", "tabid" = "MouseModelsInflammation_OxC_Cerl"),
  "TC" = list("project" = "MouseModelsInflammation", "tabid" = "MouseModelsInflammation_TC_RKO"),
  "TdAc" = list("project" = "TNFdARE_model", "tabid" = "TNFdARE_model_Col_dARE_Col_WT"),
  "TdAi" = list("project" = "TNFdARE_model", "tabid" = "TNFdARE_model_SI_dARE_SI_WT"),
  "AcTNBS" = list("project" = "TNBS_model", "tabid" = "TNBS_model_TNBS_Ac_Healthy"),
  "cTNBS" = list("project" = "TNBS_model", "tabid" = "TNBS_model_TNBS_Chr_Healthy"),
  "C8KOc" = list("project" = "Casp8Colon", "tabid" = "Casp8Colon_Col_Casp8dIEC_Col_Casp8flox"),
  "C8KOi" = list("project" = "Casp8Ileum", "tabid" = "Casp8Ileum_D4_Casp8dIEC_D4_Casp8flox"),
  "AOMDSS_e" = list("project" = "AOMDSS", "tabid" = "AOMDSS_AcDSS_Healthy"),
  "AOMDSS_l" = list("project" = "AOMDSS", "tabid" = "AOMDSS_chrDSS_Healthy"),
  "AOMDSS_t" = list("project" = "AOMDSS", "tabid" = "AOMDSS_AOMDSS_Healthy"),
  "WH6" = list("project" = "WoundHealing", "tabid" = "WoundHealing_h6_h0"),
  "WH24" = list("project" = "WoundHealing", "tabid" = "WoundHealing_h24_h0"),
  "WH48" = list("project" = "WoundHealing", "tabid" = "WoundHealing_h48_h0"),
  "EvInf" = list("project" = "Eimeria_vermiformis_model", "tabid" = "Eimeria_vermiformis_model_EV_WT"),
  "HhInf" = list("project" = "HhColitis", "tabid" = "HhColitis_HhCol_SSt")
)

#' @export
displayNames <- list("Acute DSS" = "AcDSS",
                     "Chronic DSS" = "cDSS",
                     "DSS Time course - Mid inflammation" = "DSSTCim",
                     "DSS Time course - High inflammation" = "DSSTCih",
                     "DSS Time course - Moderate recovery" = "DSSTCrm",
                     "DSS Time course - Full recovery" = "DSSTCrf",
                     "Oxazolone colitis" = "OxC",
                     "T cell transfer colitis" = "TC",
                     "TNF-dARE colitis" = "TdAc",
                     "TNF-dARE ileitis" = "TdAi",
                     "Acute TNBS colitis" = "AcTNBS",
                     "Chronic TNBS colitis" = "cTNBS",
                     "Casp8 KO colitis" = "C8KOc",
                     "Casp8 KO ileitis" = "C8KOi",
                     "AOM DSS - early" = "AOMDSS_e",
                     "AOM DSS - late" = "AOMDSS_l",
                     "AOM DSS - tumor" = "AOMDSS_t",
                     "Wound Healing time course - h6" = "WH6",
                     "Wound Healing time course - h24" = "WH24",
                     "Wound Healing time course - h48" = "WH48",
                     "Eimeria vermiformis infection" = "EvInf",
                     "Helicobacter colitis infection" = "HhInf"
                     )

#' @export
modelDescriptions <- list(
  "AcDSS" = "Ph'nglui mglw'nafh Cthulhu R'lyeh wgah'nagl fhtagn. Zhro li'hee, kadishtu naflmnahn' bug fhtagn ch' f''fhalma, Nyarlathotep grah'n athg Azathoth 'bthnk n'ghft. ",
  "cDSS" = "Ph'nglui mglw'nafh Cthulhu R'lyeh wgah'nagl fhtagn. Zhro li'hee, kadishtu naflmnahn' bug fhtagn ch' f''fhalma, Nyarlathotep grah'n athg Azathoth 'bthnk n'ghft. ",
  "DSSTCim" = "Ph'nglui mglw'nafh Cthulhu R'lyeh wgah'nagl fhtagn. Zhro li'hee, kadishtu naflmnahn' bug fhtagn ch' f''fhalma, Nyarlathotep grah'n athg Azathoth 'bthnk n'ghft. ",
  "DSSTCih" = "Ph'nglui mglw'nafh Cthulhu R'lyeh wgah'nagl fhtagn. Zhro li'hee, kadishtu naflmnahn' bug fhtagn ch' f''fhalma, Nyarlathotep grah'n athg Azathoth 'bthnk n'ghft. ",
  "DSSTCrm" = "Ph'nglui mglw'nafh Cthulhu R'lyeh wgah'nagl fhtagn. Zhro li'hee, kadishtu naflmnahn' bug fhtagn ch' f''fhalma, Nyarlathotep grah'n athg Azathoth 'bthnk n'ghft. ",
  "DSSTCrf" = "Ph'nglui mglw'nafh Cthulhu R'lyeh wgah'nagl fhtagn. Zhro li'hee, kadishtu naflmnahn' bug fhtagn ch' f''fhalma, Nyarlathotep grah'n athg Azathoth 'bthnk n'ghft. ",
  "OxC" = "Ph'nglui mglw'nafh Cthulhu R'lyeh wgah'nagl fhtagn. Zhro li'hee, kadishtu naflmnahn' bug fhtagn ch' f''fhalma, Nyarlathotep grah'n athg Azathoth 'bthnk n'ghft. ",
  "TC" = "Ph'nglui mglw'nafh Cthulhu R'lyeh wgah'nagl fhtagn. Zhro li'hee, kadishtu naflmnahn' bug fhtagn ch' f''fhalma, Nyarlathotep grah'n athg Azathoth 'bthnk n'ghft. ",
  "TdAc" = "Ph'nglui mglw'nafh Cthulhu R'lyeh wgah'nagl fhtagn. Zhro li'hee, kadishtu naflmnahn' bug fhtagn ch' f''fhalma, Nyarlathotep grah'n athg Azathoth 'bthnk n'ghft. ",
  "TdAi" = "Ph'nglui mglw'nafh Cthulhu R'lyeh wgah'nagl fhtagn. Zhro li'hee, kadishtu naflmnahn' bug fhtagn ch' f''fhalma, Nyarlathotep grah'n athg Azathoth 'bthnk n'ghft. ",
  "AcTNBS" = "Ph'nglui mglw'nafh Cthulhu R'lyeh wgah'nagl fhtagn. Zhro li'hee, kadishtu naflmnahn' bug fhtagn ch' f''fhalma, Nyarlathotep grah'n athg Azathoth 'bthnk n'ghft. ",
  "cTNBS" = "Ph'nglui mglw'nafh Cthulhu R'lyeh wgah'nagl fhtagn. Zhro li'hee, kadishtu naflmnahn' bug fhtagn ch' f''fhalma, Nyarlathotep grah'n athg Azathoth 'bthnk n'ghft. ",
  "C8KOc" = "Ph'nglui mglw'nafh Cthulhu R'lyeh wgah'nagl fhtagn. Zhro li'hee, kadishtu naflmnahn' bug fhtagn ch' f''fhalma, Nyarlathotep grah'n athg Azathoth 'bthnk n'ghft. ",
  "C8KOi" = "Ph'nglui mglw'nafh Cthulhu R'lyeh wgah'nagl fhtagn. Zhro li'hee, kadishtu naflmnahn' bug fhtagn ch' f''fhalma, Nyarlathotep grah'n athg Azathoth 'bthnk n'ghft. ",
  "AOMDSS_e" = "Ph'nglui mglw'nafh Cthulhu R'lyeh wgah'nagl fhtagn. Zhro li'hee, kadishtu naflmnahn' bug fhtagn ch' f''fhalma, Nyarlathotep grah'n athg Azathoth 'bthnk n'ghft. ",
  "AOMDSS_l" = "Ph'nglui mglw'nafh Cthulhu R'lyeh wgah'nagl fhtagn. Zhro li'hee, kadishtu naflmnahn' bug fhtagn ch' f''fhalma, Nyarlathotep grah'n athg Azathoth 'bthnk n'ghft. ",
  "AOMDSS_t" = "Ph'nglui mglw'nafh Cthulhu R'lyeh wgah'nagl fhtagn. Zhro li'hee, kadishtu naflmnahn' bug fhtagn ch' f''fhalma, Nyarlathotep grah'n athg Azathoth 'bthnk n'ghft. ",
  "WH6" = "Ph'nglui mglw'nafh Cthulhu R'lyeh wgah'nagl fhtagn. Zhro li'hee, kadishtu naflmnahn' bug fhtagn ch' f''fhalma, Nyarlathotep grah'n athg Azathoth 'bthnk n'ghft. ",
  "WH24" = "Ph'nglui mglw'nafh Cthulhu R'lyeh wgah'nagl fhtagn. Zhro li'hee, kadishtu naflmnahn' bug fhtagn ch' f''fhalma, Nyarlathotep grah'n athg Azathoth 'bthnk n'ghft. ",
  "WH48" = "Ph'nglui mglw'nafh Cthulhu R'lyeh wgah'nagl fhtagn. Zhro li'hee, kadishtu naflmnahn' bug fhtagn ch' f''fhalma, Nyarlathotep grah'n athg Azathoth 'bthnk n'ghft. ",
  "EvInf" = "Ph'nglui mglw'nafh Cthulhu R'lyeh wgah'nagl fhtagn. Zhro li'hee, kadishtu naflmnahn' bug fhtagn ch' f''fhalma, Nyarlathotep grah'n athg Azathoth 'bthnk n'ghft. ",
  "HhInf" = "Ph'nglui mglw'nafh Cthulhu R'lyeh wgah'nagl fhtagn. Zhro li'hee, kadishtu naflmnahn' bug fhtagn ch' f''fhalma, Nyarlathotep grah'n athg Azathoth 'bthnk n'ghft. "
)

#' @export
imgLabels <- list(
  'logo1' = "www/logo1.png"
)