pdffiles <- dir("../GGExperimentApr28/", pattern="pdf")
pdffiles <- gsub(".pdf","",pdffiles)
pdffiles <- gsub("-m","",pdffiles)
pdffiles <- gsub("-rep","",pdffiles)

dframe <- strsplit(pdffiles, split=" ") %>% plyr::ldply(function(x) x)
names(dframe) <- c("stimulus", "lineupid")

results <- read.csv("../GGExperimentApr28/responses_GGExpApr28.csv")
lus <- results %>% group_by(Lineup, ChosenLU) %>% summarize(
  tally = n(),
  data = Answer[1],
  reason = paste(Reasoning, collapse="|")
)
names(lus)[2] <- "panel"

dframe_res <- merge(dframe, lus, all=TRUE, by.x="stimulus", by.y="Lineup")
dframe_res$data_pick <- with(dframe_res, data==panel)
dframe_res$model <- gsub("(.*)-[0-9]*-.*", "\\1", dframe_res$lineupid)
dframe_res$data_pick <- factor(dframe_res$data_pick, levels=c("TRUE", "FALSE"))
dframe_res$res <- factor(dframe_res$data_pick, levels=c("TRUE", "FALSE"))
qplot(model, weight=tally, fill=data_pick, data=dframe_res) 
qplot(lineupid, weight=tally, fill=data_pick, data=dframe_res) +facet_grid(.~model, scales="free", space="free") +
  theme(axis.text.x = element_text(angle=90))


load("../Data/lus_ests_truth.rda")
# spread the lus_ests_truth data by parameter. There should be one line for each panel in each lineup
lus_ests_pilot <- merge(lus_ests_truth, dframe, all.x=TRUE, by="lineupid") # 
