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
  "AcDSS" = "Dextran Sulfate Sodium (DSS) is an agent that can be used in mice in order to induce colitis. Mice that ingest DSS orally in drinking water develop colitis after a few days and exhibit symptoms of IBD, such as diarrhea, rectal bleeding and weight loss.<br/><br/>After 8 days of treatment, colon samples from the mouse are taken. The mouse exhibits the signs of an acute colitis process, and ulcers can be seen by endoscopy.",
  "cDSS" = "Dextran Sulfate Sodium (DSS) is an agent that can be used in mice in order to induce colitis. Mice that ingest DSS orally in drinking water develop colitis after a few days and exhibit symptoms of IBD, such as diarrhea, rectal bleeding and weight loss.<br/><br/>Interrupting the DSS treatment after 8 days allows the mouse to recover from the damage of the compound. This gives the opportunity to repeat the treatment two more times, with the mice developing symptoms more related to a chronic version of IBD, such as a lower immune response, or fibrosis.",
  "DSSTCim" = "Dextran Sulfate Sodium (DSS) is an agent that can be used in mice in order to induce colitis. Mice that ingest DSS orally in drinking water develop colitis after a few days and exhibit symptoms of IBD, such as diarrhea, rectal bleeding and weight loss.<br/><br/>After 8 days of treatment, the mouse exhibits the signs of an acute colitis process, and ulcers can be seen by endoscopy.<br/><br/>Because of the ability to recover from the acute DSS treatment, we can take samples over the whole process, to see how the expression of the transcriptome changes over the course of the increase of inflammation and the recovery from the symptoms. Mid inflammation samples are taken at 4 days since the beginning of the treatment.",
  "DSSTCih" = "Dextran Sulfate Sodium (DSS) is an agent that can be used in mice in order to induce colitis. Mice that ingest DSS orally in drinking water develop colitis after a few days and exhibit symptoms of IBD, such as diarrhea, rectal bleeding and weight loss.<br/><br/>After 8 days of treatment, the mouse exhibits the signs of an acute colitis process, and ulcers can be seen by endoscopy.<br/><br/>Because of the ability to recover from the acute DSS treatment, we can take samples over the whole process, to see how the expression of the transcriptome changes over the course of the increase of inflammation and the recovery from the symptoms. High inflammation samples are taken at 8 days since the beginning of the treatment.",
  "DSSTCrm" = "Dextran Sulfate Sodium (DSS) is an agent that can be used in mice in order to induce colitis. Mice that ingest DSS orally in drinking water develop colitis after a few days and exhibit symptoms of IBD, such as diarrhea, rectal bleeding and weight loss.<br/><br/>After 8 days of treatment, the mouse exhibits the signs of an acute colitis process, and ulcers can be seen by endoscopy.<br/><br/>Because of the ability to recover from the acute DSS treatment, we can take samples over the whole process, to see how the expression of the transcriptome changes over the course of the increase of inflammation and the recovery from the symptoms. Mid recovery samples are taken at 12 days since the beginning of the treatment, and 4 days after the interruption of it.",
  "DSSTCrf" = "Dextran Sulfate Sodium (DSS) is an agent that can be used in mice in order to induce colitis. Mice that ingest DSS orally in drinking water develop colitis after a few days and exhibit symptoms of IBD, such as diarrhea, rectal bleeding and weight loss.<br/><br/>After 8 days of treatment, the mouse exhibits the signs of an acute colitis process, and ulcers can be seen by endoscopy.<br/><br/>Because of the ability to recover from the acute DSS treatment, we can take samples over the whole process, to see how the expression of the transcriptome changes over the course of the increase of inflammation and the recovery from the symptoms. Full recovery samples are taken at 19 days since the beginning of the treatment, and 11 days after the interruption of it.",
  "OxC" = "Oxazolone is a haptenizing agent that causes episodes of inflammation in mice. It is administered vía injection in the lamina propria of the colonic epithelium.",
  "TC" = "Some of the symptoms observed in IBD are due the response of the immune system to the rupture of the epithelial barrier, so one of the strategies for model developing would be to trigger the same response in the mouse. For this end, we can isolate activated CD4+ T cells from mice, and transfer them into Rag knockout mice, lacking the T cell lineage. The transferred T cells will trigger an inflammatory response in the mouse intestine after a few days.",
  "TdAc" = "A deletion in the AU-rich elements of the TNF gene that are responsible for a translational repression of the gene causes a constitutive activation of the TNFa production. This activation triggers an underlying continuous immune response, that ends up with a form of ileitis and sometimes colitis in the mouse.",
  "TdAi" = "A deletion in the AU-rich elements of the TNF gene that are responsible for a translational repression of the gene causes a constitutive activation of the TNFa production. This activation triggers an underlying continuous immune response, that ends up with a form of ileitis and sometimes colitis in the mouse.",
  "AcTNBS" = "TNBS is a haptenizing agent that, when ingested by mice, can cause inflammation and other IBD like symptoms in the intestinal epithelium. The nature of the agent allows for a recovery after an acute phase",
  "cTNBS" = "TNBS is a haptenizing agent that, when ingested by mice, can cause inflammation and other IBD like symptoms in the intestinal epithelium. The nature of the agent allows for a recovery after an acute phase",
  "C8KOc" = "When studying the dynamics of cell death pathways, it was found when knocking out the Caspase 8 gene, that regulates the apoptosis cell death pathway, the cells in the epithelium undergo cell death using the necroptosis pathway. Mice born with this gene knocked out develop a form of ileitis when growing, as well as a form of colitis in some cases.",
  "C8KOi" = "When studying the dynamics of cell death pathways, it was found when knocking out the Caspase 8 gene, that regulates the apoptosis cell death pathway, the cells in the epithelium undergo cell death using the necroptosis pathway. Mice born with this gene knocked out develop a form of ileitis when growing, as well as a form of colitis in some cases.",
  "AOMDSS_e" = "Dextran Sulfate Sodium (DSS) is an agent that can be used in mice in order to induce colitis. Mice that ingest DSS orally in drinking water develop colitis after a few days and exhibit symptoms of IBD, such as diarrhea, rectal bleeding and weight loss.<br/><br/>After 8 days of treatment, colon samples from the mouse are taken. The mouse exhibits the signs of an acute colitis process, and ulcers can be seen by endoscopy.",
  "AOMDSS_l" = "Dextran Sulfate Sodium (DSS) is an agent that can be used in mice in order to induce colitis. Mice that ingest DSS orally in drinking water develop colitis after a few days and exhibit symptoms of IBD, such as diarrhea, rectal bleeding and weight loss.<br/><br/>Interrupting the DSS treatment after 8 days allows the mouse to recover from the damage of the compound. This gives the opportunity to repeat the treatment two more times, with the mice developing symptoms more related to a chronic version of IBD, such as a lower immune response, or fibrosis.",
  "AOMDSS_t" = "Dextran Sulfate Sodium (DSS) is an agent that can be used in mice in order to induce colitis. Mice that ingest DSS orally in drinking water develop colitis after a few days and exhibit symptoms of IBD, such as diarrhea, rectal bleeding and weight loss.<br/><br/>The chronic treatment with DSS puts a lot of genetic pressure in the colonic epithelium, making it an ideal candidate for a colon cancer model. After the last DSS treatment, we can administer the mouse with Azoximethane (AOM), a carcinogenic agent, that causes the mouse to develop colonic tumors.",
  "WH6" = "The processes of resolution play an important role in the dynamics of IBD, so, to observe the natural processes of resolution of wounds in the epithelium over the transcriptome, a small wound is made in the mouse’s colon and samples are taken at multiple points in time to observe the state of the healing. Samples are taken at Hour 6 since the infliction of the wound.",
  "WH24" = "The processes of resolution play an important role in the dynamics of IBD, so, to observe the natural processes of resolution of wounds in the epithelium over the transcriptome, a small wound is made in the mouse’s colon and samples are taken at multiple points in time to observe the state of the healing. Samples are taken at Hour 24 since the infliction of the wound.",
  "WH48" = "The processes of resolution play an important role in the dynamics of IBD, so, to observe the natural processes of resolution of wounds in the epithelium over the transcriptome, a small wound is made in the mouse’s colon and samples are taken at multiple points in time to observe the state of the healing. Samples are taken at Hour 48 since the infliction of the wound.",
  "EvInf" = "Infection with Eimeria vermiformis causes ileitis.",
  "HhInf" = "Infection with Helicobacter hepaticus causes colitis at 14 days."
  #"HhInf" = "Ph'nglui mglw'nafh Cthulhu R'lyeh wgah'nagl fhtagn. Zhro li'hee, kadishtu naflmnahn' bug fhtagn ch' f''fhalma, Nyarlathotep grah'n athg Azathoth 'bthnk n'ghft. "
)

#' @export
markerNames <- list(
  "Dystal enterocytes" = "markers_EnterocyteDist",
  "Proximal enterocytes" = "markers_EnterocyteProx",
  "Enteroendocrine cells" = "markers_Enteroendocrine",
  "Goblet cells" = "markers_Goblet",
  "M cells" = "markers_Mcells",
  "Paneth cells" = "markers_Paneth",
  "Stem cells" = "markers_Stem",
  "Transit Amplifying cells" = "markers_TAprog",
  "Tuft cells" = "markers_Tuft",
  "Fibroblasts" = "markers_Fibroblasts",
  "Monocyte derived dendritic cells" = "markers_MODC",
  "Plasma cells" = "markers_Plasma",
  "T lymphocytes" = "markers_Tcells",
  "B lymphocytes" = "markers_Bcells",
  "Mast cells" = "markers_Mast",
  "Mitochondrial genes" = "markers_Mitochondrial",
  "NK lymphocytes" = "markers_NK",
  "Endothelial cells" = "markers_Endothelial",
  "Neutrophils" = "markers_Neutrophils",
  "Smooth muscle" = "markers_SmoothMuscle",
  "Enteric glial cells" = "markers_EntericGlia",
  "Enteric neurons" = "markers_EntericNeuron"
)

#' @export
imgLabels <- list(
  'logo1' = "www/logo1.png"
)