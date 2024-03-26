# library(rstatix)
# library(googlesheets4)
# library(gsheet)
# library(stringr)
library(tidyverse)
library(igraph)
library(ggraph)
script_path <- dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(script_path)
 # source("../../mini-project/utils/data_structures.R")
dataset <- tibble::as_tibble(read.csv("../../mini-project/data/dataREanonymized.csv"))

append_vector_to_dataframe <- function(target_df, vector) {
  # Get the name of the vector
  vector_name <- deparse(substitute(vector))
  
  # Create a data frame from the vector
  df <- data.frame(TypeOfData = vector_name, ColumnName = vector)
  
  # Append the data frame to the target data frame
  target_df <- rbind(target_df, df)
  
  # Return the updated target data frame
  return(target_df)
}

#listOfParameterLists<-list(patientCharacteristics,strokeCharacteristics)

# set variable sets
# patientCharacteristics ---------------------------------------------------------
patientCharacteristics<-c(
  "subject_id",
  "age",
  "gender",
  "nihss_score",
  "sys_blood_pressure",
  "dis_blood_pressure",
  "glucose",
  "cholesterol",
  "prestroke_mrs",
  "covid_test"
)
# strokeCharacteristics ---------------------------------------------------------
strokeCharacteristics<- c(
  "stroke_type",
  "hospital_stroke",
  "prenotification",
  "arrival_mode",
  "first_hospital",
  "hospitalized_in",
  "department_type"
)
# patientRiskFactors ---------------------------------------------------------

patientRiskFactors <- c(
  "risk_hypertension",
  "risk_diabetes",
  "risk_hyperlipidemia",
  "risk_smoker",
  "risk_smoker_last_10_years",
  "risk_previous_ischemic_stroke",
  "risk_previous_hemorrhagic_stroke",
  "risk_atrial_fibrillation",
  "risk_coronary_artery_disease_or_myocardial_infarction",
  "risk_congestive_heart_failure",
  "risk_hiv"
)
# patientRiskMeds ---------------------------------------------------------

patientRiskMeds <- c(
  "before_onset_antidiabetics",
  "before_onset_antihypertensives",
  "before_onset_asa",
  "before_onset_cilostazol",
  "before_onset_clopidogrel",
  "before_onset_ticagrelor",
  "before_onset_ticlopidine",
  "before_onset_prasugrel",
  "before_onset_dipyridamol",
  "before_onset_warfarin",
  "before_onset_heparin",
  "before_onset_dabigatran",
  "before_onset_rivaroxaban",
  "before_onset_apixaban",
  "before_onset_edoxaban",
  "before_onset_statin"
)

# ifBleedingDetails ---------------------------------------------------------

ifBleedingDetails <- c(
  "infratentorial_source",
  "bleeding_source",
  "ich_score",
  "neurosurgery",
  "bleeding_reason_hypertension",
  "bleeding_reason_aneurysm",
  "bleeding_reason_malformation",
  "bleeding_reason_anticoagulant",
  "bleeding_reason_angiopathy",
  "bleeding_reason_other",
  "hunt_hess_score"
)

# imagingDetails ---------------------------------------------------------


imagingDetails <- c(
  "imaging_type",
  "occlusion_left_mca_m1",
  "occlusion_left_mca_m2",
  "occlusion_left_mca_m3",
  "occlusion_left_aca",
  "occlusion_left_pca_p1",
  "occlusion_left_pca_p2",
  "occlusion_left_cae",
  "occlusion_left_cai",
  "occlusion_right_mca_m1",
  "occlusion_right_mca_m2",
  "occlusion_right_mca_m3",
  "occlusion_right_aca",
  "occlusion_right_pca_p1",
  "occlusion_right_pca_p2",
  "occlusion_right_cae",
  "occlusion_right_cai",
  "occlusion_ba",
  "occlusion_va",
  "imaging_done",
  "imaging_type_elsewhere",
  "door_to_imaging",
  "perfusion_score",
  "hypoperfusion_score"
)
# carePerformance ---------------------------------------------------------
carePerformance<-c(
  "thrombolysis",
  "onset_to_door",
  "door_to_needle",
  "thrombectomy",
  "thrombectomy_or_thrombolysis",
  "door_to_groin",
  "tici_score"
)
# dischargtePrescriptions ---------------------------------------------------------
dischargePrescriptions <- c(
  "discharge_antidiabetics",
  "discharge_antihypertensives",
  "discharge_asa",
  "discharge_cilostazol",
  "discharge_clopidogrel",
  "discharge_ticagrelor",
  "discharge_ticlopidine",
  "discharge_prasugrel",
  "discharge_dipyridamol",
  "discharge_warfarin",
  "discharge_heparin",
  "discharge_dabigatran",
  "discharge_rivaroxaban",
  "discharge_apixaban",
  "discharge_edoxaban",
  "discharge_statin",
  "discharge_any_antiplatelet",
  "discharge_any_anticoagulant"
)
# dischargePatientScore ---------------------------------------------------------
dischargePatientScore <- c(
  "discharge_mrs",
  "discharge_nihss_score"
)
# postDischarge ---------------------------------------------------------
postDischarge<-c(  "discharge_destination",  "three_m_mrs")

# rest of code ---------------------------------------------------------

target_df<- data.frame(TypeOfData = character(0), ColumnName = character(0))

target_df<-append_vector_to_dataframe(target_df, patientCharacteristics)
target_df<-append_vector_to_dataframe(target_df, strokeCharacteristics)
target_df<-append_vector_to_dataframe(target_df, patientRiskFactors)
target_df<-append_vector_to_dataframe(target_df, patientRiskMeds)
target_df<-append_vector_to_dataframe(target_df, ifBleedingDetails)
target_df<-append_vector_to_dataframe(target_df, imagingDetails)
target_df<-append_vector_to_dataframe(target_df, carePerformance)
target_df<-append_vector_to_dataframe(target_df, dischargePrescriptions)
target_df<-append_vector_to_dataframe(target_df, dischargePatientScore)
target_df<-append_vector_to_dataframe(target_df, postDischarge)

target_df$row_number <- seq_len(nrow(target_df))
nodes<- target_df %>%
  group_by(TypeOfData) %>%
  summarize(Count = n(), OrderNumber = min(row_number)) %>%
  ungroup() %>%
  arrange(OrderNumber) %>%
  mutate(OrderNumber = 1:n(),
         TypeOfData=paste(OrderNumber,TypeOfData))

nodes$id<-nodes$TypeOfData
order<-   as.vector(nodes$TypeOfData)

pair_df <- merge(nodes, nodes, by = NULL) %>%
  filter(OrderNumber.x < OrderNumber.y)  # Filter out pairs where OrderNumber of the first row is lower

edges<- pair_df %>% select(from=TypeOfData.x,to=TypeOfData.y,weight=Count.x)

#create the network object for plotting
net<-graph_from_data_frame(d=edges, vertices=nodes, directed=T) 

ggraph(net, layout = 'manual', x = data$x, y = data$y) +
  geom_node_point() +
  geom_edge_link() +
  theme_void()

ggraph(net, layout = 'linear') + 
  geom_edge_arc(color = "orange", width=0.7) +
  geom_node_point(aes(size=Count), color="gray50") +
  theme_void()+ geom_node_text(aes(label = id), vjust = 1.5)

layouts <- grep("^layout_", ls("package:igraph"), value=TRUE)[-1] 
layouts <- layouts[!grepl("bipartite|merge|norm|sugiyama|tree", layouts)]

par(mfrow=c(4,4), mar=c(1,1,1,1))
for (layout in layouts) {
  print(layout)
  l <- do.call(layout, list(net)) 
  plot(net, edge.arrow.mode=2, layout=l, main=layout, edge.arrow.size = 0.2) }
dev.off()
plot(net, edge.arrow.mode=2, layout=do.call("layout_in_circle", list(net)) , main="layout_in_circle", edge.arrow.size = 0.2)

