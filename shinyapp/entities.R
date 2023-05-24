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
  "OxC_old" = list("project" = "MouseModelsInflammation", "tabid" = "MouseModelsInflammation_OxC_Cerl"),
  "OxC" = list("project" = "OxaCitro", "tabid" = "OxaCitro_OxC_Healthy"),
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
  "HhInf" = list("project" = "HhColitis", "tabid" = "HhColitis_HhCol_SSt"),
  "CB" = list("project" = "OxaCitro", "tabid" = "OxaCitro_CB_Healthy")
)

#' @export
displayNames <- list("Acute DSS (2%)" = "AcDSS",
                     "Acute DSS (1.5%)" = "AOMDSS_e",
                     "Chronic DSS (2%)" = "cDSS",
                     "Chronic DSS (1%)" = "AOMDSS_l",
                     "AOM DSS" = "AOMDSS_t",
                     "DSS Time course - Mid inflammation (DSS 3%, day 3)" = "DSSTCim",
                     "DSS Time course - High inflammation (DSS 3%, day 8)" = "DSSTCih",
                     "DSS Time course - Moderate recovery (after DSS, day 12)" = "DSSTCrm",
                     "DSS Time course - Full recovery (after DSS, day 19)" = "DSSTCrf",
                     "Oxazolone colitis - Old version" = "OxC_old",
                     "Oxazolone colitis" = "OxC",
                     "T cell transfer colitis" = "TC",
                     "TNF-dARE colitis" = "TdAc",
                     "TNF-dARE ileitis" = "TdAi",
                     "Acute TNBS colitis" = "AcTNBS",
                     "Chronic TNBS colitis" = "cTNBS",
                     "Casp8 KO colitis" = "C8KOc",
                     "Casp8 KO ileitis" = "C8KOi",
                     "Wound Healing time course - h6" = "WH6",
                     "Wound Healing time course - h24" = "WH24",
                     "Wound Healing time course - h48" = "WH48",
                     "Eimeria vermiformis infection" = "EvInf",
                     "Helicobacter colitis infection" = "HhInf",
                     "Citrobacter rodentium infection" = "CB"
                     )

#' @export
imgFormat <- list("PNG" = ".png",
                  "JPEG" = ".jpeg",
                  "PDF" = ".pdf",
                  "SVG" = ".svg"
)

#' @export
modelDescriptions <- list(
  "AcDSS" = "The DSS colitis model is based on the administration of the sulfated polysaccharide DSS to mice orally in drinking water. This compound induces severe colitis after a few days, with symptoms such as bloody diarrhea, weight loss, epithelial damage that can originate ulcers and immune cell infiltration, resembling symptoms observed in flares of ulcerative colitis in humans. Because of these injuries, the epithelial barrier becomes compromised, and the cells of the sub epithelial layer of the gut are exposed to the antigens present in the intestinal lumen, such as bacteria and other commensal microbiota, developing an inflammatory response.</br>Because of the models simplicity and reproducibility, we can modify the doses of DSS and treatment cycles in order to generate forms of acute and chronic intestinal inflammation, as well as use the model for the study of the regeneration and recovery of the intestinal epithelium. </br>In this database, the experiments included that are based in this model are the Acute and Chronic DSS colitis, and an acute DSS time course study, that samples mice over multiple time points, following the trancriptome as the inflammation develops, and then the mouse recovers from it (https://www.nature.com/articles/nprot.2017.044).",
  "cDSS" = "The DSS colitis model is based on the administration of the sulfated polysaccharide DSS to mice orally in drinking water. This compound induces severe colitis after a few days, with symptoms such as bloody diarrhea, weight loss, epithelial damage that can originate ulcers and immune cell infiltration, resembling symptoms observed in flares of ulcerative colitis in humans. Because of these injuries, the epithelial barrier becomes compromised, and the cells of the sub epithelial layer of the gut are exposed to the antigens present in the intestinal lumen, such as bacteria and other commensal microbiota, developing an inflammatory response.</br>Because of the models simplicity and reproducibility, we can modify the doses of DSS and treatment cycles in order to generate forms of acute and chronic intestinal inflammation, as well as use the model for the study of the regeneration and recovery of the intestinal epithelium. </br>In this database, the experiments included that are based in this model are the Acute and Chronic DSS colitis, and an acute DSS time course study, that samples mice over multiple time points, following the trancriptome as the inflammation develops, and then the mouse recovers from it (https://www.nature.com/articles/nprot.2017.044).",
  "DSSTCim" = "The DSS colitis model is based on the administration of the sulfated polysaccharide DSS to mice orally in drinking water. This compound induces severe colitis after a few days, with symptoms such as bloody diarrhea, weight loss, epithelial damage that can originate ulcers and immune cell infiltration, resembling symptoms observed in flares of ulcerative colitis in humans. Because of these injuries, the epithelial barrier becomes compromised, and the cells of the sub epithelial layer of the gut are exposed to the antigens present in the intestinal lumen, such as bacteria and other commensal microbiota, developing an inflammatory response.</br>Because of the models simplicity and reproducibility, we can modify the doses of DSS and treatment cycles in order to generate forms of acute and chronic intestinal inflammation, as well as use the model for the study of the regeneration and recovery of the intestinal epithelium. </br>In this database, the experiments included that are based in this model are the Acute and Chronic DSS colitis, and an acute DSS time course study, that samples mice over multiple time points, following the trancriptome as the inflammation develops, and then the mouse recovers from it (https://www.nature.com/articles/nprot.2017.044).",
  "DSSTCih" = "The DSS colitis model is based on the administration of the sulfated polysaccharide DSS to mice orally in drinking water. This compound induces severe colitis after a few days, with symptoms such as bloody diarrhea, weight loss, epithelial damage that can originate ulcers and immune cell infiltration, resembling symptoms observed in flares of ulcerative colitis in humans. Because of these injuries, the epithelial barrier becomes compromised, and the cells of the sub epithelial layer of the gut are exposed to the antigens present in the intestinal lumen, such as bacteria and other commensal microbiota, developing an inflammatory response.</br>Because of the models simplicity and reproducibility, we can modify the doses of DSS and treatment cycles in order to generate forms of acute and chronic intestinal inflammation, as well as use the model for the study of the regeneration and recovery of the intestinal epithelium. </br>In this database, the experiments included that are based in this model are the Acute and Chronic DSS colitis, and an acute DSS time course study, that samples mice over multiple time points, following the trancriptome as the inflammation develops, and then the mouse recovers from it (https://www.nature.com/articles/nprot.2017.044).",
  "DSSTCrm" = "The DSS colitis model is based on the administration of the sulfated polysaccharide DSS to mice orally in drinking water. This compound induces severe colitis after a few days, with symptoms such as bloody diarrhea, weight loss, epithelial damage that can originate ulcers and immune cell infiltration, resembling symptoms observed in flares of ulcerative colitis in humans. Because of these injuries, the epithelial barrier becomes compromised, and the cells of the sub epithelial layer of the gut are exposed to the antigens present in the intestinal lumen, such as bacteria and other commensal microbiota, developing an inflammatory response.</br>Because of the models simplicity and reproducibility, we can modify the doses of DSS and treatment cycles in order to generate forms of acute and chronic intestinal inflammation, as well as use the model for the study of the regeneration and recovery of the intestinal epithelium. </br>In this database, the experiments included that are based in this model are the Acute and Chronic DSS colitis, and an acute DSS time course study, that samples mice over multiple time points, following the trancriptome as the inflammation develops, and then the mouse recovers from it (https://www.nature.com/articles/nprot.2017.044).",
  "DSSTCrf" = "The DSS colitis model is based on the administration of the sulfated polysaccharide DSS to mice orally in drinking water. This compound induces severe colitis after a few days, with symptoms such as bloody diarrhea, weight loss, epithelial damage that can originate ulcers and immune cell infiltration, resembling symptoms observed in flares of ulcerative colitis in humans. Because of these injuries, the epithelial barrier becomes compromised, and the cells of the sub epithelial layer of the gut are exposed to the antigens present in the intestinal lumen, such as bacteria and other commensal microbiota, developing an inflammatory response.</br>Because of the models simplicity and reproducibility, we can modify the doses of DSS and treatment cycles in order to generate forms of acute and chronic intestinal inflammation, as well as use the model for the study of the regeneration and recovery of the intestinal epithelium. </br>In this database, the experiments included that are based in this model are the Acute and Chronic DSS colitis, and an acute DSS time course study, that samples mice over multiple time points, following the trancriptome as the inflammation develops, and then the mouse recovers from it (https://www.nature.com/articles/nprot.2017.044).",
  "OxC_old" = "The haptenizing agent oxazolone causes severe colitis after intrarectal administration in mice. The mice suffer of inflammation of the distal mucosa and submucosa, and experience ulcerations in the epithelium. Multiple immune cells, such as macrophages, neutrophils and lymphocytes can be found infiltrating the lamina propria. Multiple strains of mice can be used to run this model, some of them being more susceptible than others. Strains SJL/L or C57BL/10 have more sensibility to the treatment, but other strains, such as C57BL/6 might need some form of subcutaneous sensibilization beforehand.</br>Some of the histological features that are observed in the mice, such as inflammation of the distant colon, or an elevated production of cytokines are similar to the symptoms observed in ulcerative colitis in humans, so the model has been used to study such responses in the context of the disease.</br>Finally, changes in the dosage of oxazolone can also trigger different types of immune responses, like mixed Th1/Th2. In some strains, a chronic form of colitis can be obtained performing multiple cycles of treatment with oxazolone. (https://www.nature.com/articles/nprot.2017.044).",
  "OxC" = "The haptenizing agent oxazolone causes severe colitis after intrarectal administration in mice. The mice suffer of inflammation of the distal mucosa and submucosa, and experience ulcerations in the epithelium. Multiple immune cells, such as macrophages, neutrophils and lymphocytes can be found infiltrating the lamina propria. Multiple strains of mice can be used to run this model, some of them being more susceptible than others. Strains SJL/L or C57BL/10 have more sensibility to the treatment, but other strains, such as C57BL/6 might need some form of subcutaneous sensibilization beforehand.</br>Some of the histological features that are observed in the mice, such as inflammation of the distant colon, or an elevated production of cytokines are similar to the symptoms observed in ulcerative colitis in humans, so the model has been used to study such responses in the context of the disease.</br>Finally, changes in the dosage of oxazolone can also trigger different types of immune responses, like mixed Th1/Th2. In some strains, a chronic form of colitis can be obtained performing multiple cycles of treatment with oxazolone. (https://www.nature.com/articles/nprot.2017.044).",
  "TC" = "Given the role of the immune response in the development of inflammatory bowel disease, the adoptive transfer model of colitis in mice is one of the best suited models to study CD4+ T cell induced inflammation. The principle of this model is rooted in the capability of CD4+ CD45rbHigh T cells from spleen or lymph nodes to cause intestinal inflammatory disease in the gut. To obtain this effect, the cells are to be transfered into immune-defficient Rag KO mice. The cells then react, secreting cytokines that trigger an inflammatory response in the gut epithelium. Symptoms begin appearing somewhere around 6-8 weeks. (https://pubmed.ncbi.nlm.nih.gov/22262449/)",
  "TdAc" = "In the TNF gene, there is a region that contains AU-rich elements, responsible for TNF-mRNA destabilization and translational repression. A deletion in this area causes the expression of the TNF gene to become constitutive, and exhibit increased levels if TNF in serum. Because of the pro-inflammatory nature of TNF, mice with this deletion will develop a phenotype of inflammation in the intestinal epithelium with noticeable similarities with human CD. The location of this phenomenon is mainly in the ileum, but it can be extended into the colon. The inflammation does not take long to appear, as 4 month old heterozygous TNFdARE mice start showing symptoms in the ileum. The inflammatory process does not limit itself to the gut, as these mice also exhibit signs of arthritis (https://pubmed.ncbi.nlm.nih.gov/18266230/).",
  "TdAi" = "In the TNF gene, there is a region that contains AU-rich elements, responsible for TNF-mRNA destabilization and translational repression. A deletion in this area causes the expression of the TNF gene to become constitutive, and exhibit increased levels if TNF in serum. Because of the pro-inflammatory nature of TNF, mice with this deletion will develop a phenotype of inflammation in the intestinal epithelium with noticeable similarities with human CD. The location of this phenomenon is mainly in the ileum, but it can be extended into the colon. The inflammation does not take long to appear, as 4 month old heterozygous TNFdARE mice start showing symptoms in the ileum. The inflammatory process does not limit itself to the gut, as these mice also exhibit signs of arthritis (https://pubmed.ncbi.nlm.nih.gov/18266230/).",
  "AcTNBS" = "For this model, the haptenating agent TNBS in administrated intrarectally toughether with ethanol in mice. The ethanol impairs barrier function, and provides the tNBS access to the intestinal cells and penetrate the epithelium. This causes haptenization of some of the present proteins and triggers an immune response by CD4+ T cells. This type of response makes the model a good candidate to study the CD4+ T cell responses in intestinal inflammation.</br>The strain of the mouse, the microbiota and the concentration of the compound are determining factors in the observation of multiple responses. By modifying these factors, Th1, Th2 and Th17 responses can be observed. Because the histopathology and immunologically the model resembles the characteristics of human CD, it has been widely used for the study of aspects of the disease, such as fibrosis in the lamina propria that is also present in human CD. (https://www.nature.com/articles/nprot.2017.044).",
  "cTNBS" = "For this model, the haptenating agent TNBS in administrated intrarectally toughether with ethanol in mice. The ethanol impairs barrier function, and provides the tNBS access to the intestinal cells and penetrate the epithelium. This causes haptenization of some of the present proteins and triggers an immune response by CD4+ T cells. This type of response makes the model a good candidate to study the CD4+ T cell responses in intestinal inflammation.</br>The strain of the mouse, the microbiota and the concentration of the compound are determining factors in the observation of multiple responses. By modifying these factors, Th1, Th2 and Th17 responses can be observed. Because the histopathology and immunologically the model resembles the characteristics of human CD, it has been widely used for the study of aspects of the disease, such as fibrosis in the lamina propria that is also present in human CD. (https://www.nature.com/articles/nprot.2017.044).",
  "C8KOc" = "When studying the dynamics of cell death pathways, it was found when knocking out the Caspase 8 gene, that regulates the apoptosis cell death pathway, the cells in the epithelium undergo cell death using the necroptosis pathway. Mice born with this gene knocked out develop a form of ileitis when growing, as well as a form of colitis in some cases.",
  "C8KOi" = "When studying the dynamics of cell death pathways, it was found when knocking out the Caspase 8 gene, that regulates the apoptosis cell death pathway, the cells in the epithelium undergo cell death using the necroptosis pathway. Mice born with this gene knocked out develop a form of ileitis when growing, as well as a form of colitis in some cases.",
  "AOMDSS_e" = "As an extensión of the DSS inflammation model, the AOM/DSS colon cancer model uses the strain on the tissue of the multiple DSS colitis cycles to create an environment where a carcinogenic agent can generate tumors most effectively.</br>AOM (Methyl-methylimino-oxidoazanium) is a procarcinogen that is metabolized by cytochrome p450 into methylazocymethanol (MAM), a highly alkylating compound that induces G to A transitions in genetic material. This methabolized compound is excreted into the bile, and absorbed by the intestinal epithelium, where it starts inducing mutations, so its effect in the already damaged tissue of a mouse with chronic inflammation induced by cycles of DSS is greatly enhanced.</br>Some of the key features of this model consist in how relatively quick and accurately it is able to model colitis associated cancer, as tumors develop at around 10 weeks. The histopatology of the induced tumors also replicates multiple facets of human colitis induced cancer, so it has been widely used to study the development and pathogenesis of infllamatory colorectal cancer. (https://www.ncbi.nlm.nih.gov/pmc/articles/PMC5035391/)",
  "AOMDSS_l" = "As an extensión of the DSS inflammation model, the AOM/DSS colon cancer model uses the strain on the tissue of the multiple DSS colitis cycles to create an environment where a carcinogenic agent can generate tumors most effectively.</br>AOM (Methyl-methylimino-oxidoazanium) is a procarcinogen that is metabolized by cytochrome p450 into methylazocymethanol (MAM), a highly alkylating compound that induces G to A transitions in genetic material. This methabolized compound is excreted into the bile, and absorbed by the intestinal epithelium, where it starts inducing mutations, so its effect in the already damaged tissue of a mouse with chronic inflammation induced by cycles of DSS is greatly enhanced.</br>Some of the key features of this model consist in how relatively quick and accurately it is able to model colitis associated cancer, as tumors develop at around 10 weeks. The histopatology of the induced tumors also replicates multiple facets of human colitis induced cancer, so it has been widely used to study the development and pathogenesis of infllamatory colorectal cancer. (https://www.ncbi.nlm.nih.gov/pmc/articles/PMC5035391/)",
  "AOMDSS_t" = "As an extensión of the DSS inflammation model, the AOM/DSS colon cancer model uses the strain on the tissue of the multiple DSS colitis cycles to create an environment where a carcinogenic agent can generate tumors most effectively.</br>AOM (Methyl-methylimino-oxidoazanium) is a procarcinogen that is metabolized by cytochrome p450 into methylazocymethanol (MAM), a highly alkylating compound that induces G to A transitions in genetic material. This methabolized compound is excreted into the bile, and absorbed by the intestinal epithelium, where it starts inducing mutations, so its effect in the already damaged tissue of a mouse with chronic inflammation induced by cycles of DSS is greatly enhanced.</br>Some of the key features of this model consist in how relatively quick and accurately it is able to model colitis associated cancer, as tumors develop at around 10 weeks. The histopatology of the induced tumors also replicates multiple facets of human colitis induced cancer, so it has been widely used to study the development and pathogenesis of infllamatory colorectal cancer. (https://www.ncbi.nlm.nih.gov/pmc/articles/PMC5035391/)",
  "WH6" = "The processes of resolution play an important role in the dynamics of IBD, so, to observe the natural processes of resolution of wounds in the epithelium over the transcriptome, a small wound is made in the mouse’s colon and samples are taken at multiple points in time to observe the state of the healing. Samples are taken at Hour 6 since the infliction of the wound.",
  "WH24" = "The processes of resolution play an important role in the dynamics of IBD, so, to observe the natural processes of resolution of wounds in the epithelium over the transcriptome, a small wound is made in the mouse’s colon and samples are taken at multiple points in time to observe the state of the healing. Samples are taken at Hour 24 since the infliction of the wound.",
  "WH48" = "The processes of resolution play an important role in the dynamics of IBD, so, to observe the natural processes of resolution of wounds in the epithelium over the transcriptome, a small wound is made in the mouse’s colon and samples are taken at multiple points in time to observe the state of the healing. Samples are taken at Hour 48 since the infliction of the wound.",
  "EvInf" = "<i>Eimeria vermiformis</i> is an intracellular single cell protozoan pathogen, which specifically infects the small intestine epithelium. This pathogen is a natural self-limiting murine parasite that induces a strong non-lethal immune response and establishes a protective immunity in the host against future infections. Its life cycle is also completed in a single step, without the need of a second host, so the model is ideal to study the host mechanisms of defense against an infection in the small intestinal epithelium. </br>The mice infected with this pathogen excrete a non-infectious, unsporulated form of oocysts that mature and become infectious after 2-7 days in a moist environment. Mice who ingest these matured oocysts are then infected with the pathogen and disseminate it further.(https://www.ncbi.nlm.nih.gov/pmc/articles/PMC6485400/)",
  "HhInf" = "In the 1990s, a new microaerobic bacterium, <i>Helicobacter hepaticus</i>, was identified in murine models with chronic active hepatitis and IBD. Targeted infections with this bacteria showed its capability to induce colitis, CRC and other diseases in multiple immune deficient mouse strains. The model has, since then, been accepted and numerous studies are published that focus on the regulation of acquired and innate immunity in the intestine (https://www.nature.com/articles/mi201061#Sec12).",
  "CB" = "In order to model the intestinal response to some enteropathogenic Gram negative bacteria such as enteropahtogenic E. coli (EPEC) or enterohemorragic E. coli (EHEC), we can perform a controlled infection with the mouse pathogen Citrobacter rodentium, which shares the same pathogenic system. After the infection, most mice strains deveop an non-lethal inflammatory process after a few days, and allow for the study of the intestinal response to the infection."
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