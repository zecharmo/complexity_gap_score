library(data.table)
library(MASS)
library(reshape2)
library(ggplot2)
library(RColorBrewer)
library(stringr)

data = read.csv("path\\multipleChoiceResponses.csv", header=TRUE, sep = ",")
# Interested in difference between male and female wages, need to remove respondents who answered other options
data2 = subset(data, GenderSelect == 'Male' | GenderSelect == 'Female')

# Let's remove anyone who isn't of "working age" (18-70), reduce dataset by 395 respondents
data3 = subset(data2, Age > 17 & Age <= 70)

#Create top-2 box variables for work tools to identify if someone uses that tool 'Often' or 'Most of the time'
#Looking to count only people who use this tool fequently in their work

data3$WorkToolsFrequencyAmazonML_t2 = ifelse(data3$WorkToolsFrequencyAmazonML == 'Often' | data3$WorkToolsFrequencyAmazonML == 'Most of the time', 1,0)
data3$WorkToolsFrequencyAWS_t2 = ifelse(data3$WorkToolsFrequencyAWS == 'Often' | data3$WorkToolsFrequencyAWS == 'Most of the time', 1,0)
data3$WorkToolsFrequencyAngoss_t2 = ifelse(data3$WorkToolsFrequencyAngoss == 'Often' | data3$WorkToolsFrequencyAngoss == 'Most of the time', 1,0)
data3$WorkToolsFrequencyC_t2 = ifelse(data3$WorkToolsFrequencyC == 'Often' | data3$WorkToolsFrequencyC == 'Most of the time', 1,0)
data3$WorkToolsFrequencyCloudera_t2 = ifelse(data3$WorkToolsFrequencyCloudera == 'Often' | data3$WorkToolsFrequencyCloudera == 'Most of the time', 1,0)
data3$WorkToolsFrequencyDataRobot_t2 = ifelse(data3$WorkToolsFrequencyDataRobot == 'Often' | data3$WorkToolsFrequencyDataRobot == 'Most of the time', 1,0)
data3$WorkToolsFrequencyFlume_t2 = ifelse(data3$WorkToolsFrequencyFlume == 'Often' | data3$WorkToolsFrequencyFlume == 'Most of the time', 1,0)
data3$WorkToolsFrequencyGCP_t2 = ifelse(data3$WorkToolsFrequencyGCP == 'Often' | data3$WorkToolsFrequencyGCP == 'Most of the time', 1,0)
data3$WorkToolsFrequencyHadoop_t2 = ifelse(data3$WorkToolsFrequencyHadoop == 'Often' | data3$WorkToolsFrequencyHadoop == 'Most of the time', 1,0)
data3$WorkToolsFrequencyIBMCognos_t2 = ifelse(data3$WorkToolsFrequencyIBMCognos == 'Often' | data3$WorkToolsFrequencyIBMCognos == 'Most of the time', 1,0)
data3$WorkToolsFrequencyIBMSPSSModeler_t2 = ifelse(data3$WorkToolsFrequencyIBMSPSSModeler == 'Often' | data3$WorkToolsFrequencyIBMSPSSModeler == 'Most of the time', 1,0)
data3$WorkToolsFrequencyIBMSPSSStatistics_t2 = ifelse(data3$WorkToolsFrequencyIBMSPSSStatistics == 'Often' | data3$WorkToolsFrequencyIBMSPSSStatistics == 'Most of the time', 1,0)
data3$WorkToolsFrequencyIBMWatson_t2 = ifelse(data3$WorkToolsFrequencyIBMWatson == 'Often' | data3$WorkToolsFrequencyIBMWatson == 'Most of the time', 1,0)
data3$WorkToolsFrequencyImpala_t2 = ifelse(data3$WorkToolsFrequencyImpala == 'Often' | data3$WorkToolsFrequencyImpala == 'Most of the time', 1,0)
data3$WorkToolsFrequencyJava_t2 = ifelse(data3$WorkToolsFrequencyJava == 'Often' | data3$WorkToolsFrequencyJava == 'Most of the time', 1,0)
data3$WorkToolsFrequencyJulia_t2 = ifelse(data3$WorkToolsFrequencyJulia == 'Often' | data3$WorkToolsFrequencyJulia == 'Most of the time', 1,0)
data3$WorkToolsFrequencyJupyter_t2 = ifelse(data3$WorkToolsFrequencyJupyter == 'Often' | data3$WorkToolsFrequencyJupyter == 'Most of the time', 1,0)
data3$WorkToolsFrequencyKNIMECommercial_t2 = ifelse(data3$WorkToolsFrequencyKNIMECommercial == 'Often' | data3$WorkToolsFrequencyKNIMECommercial == 'Most of the time', 1,0)
data3$WorkToolsFrequencyKNIMEFree_t2 = ifelse(data3$WorkToolsFrequencyKNIMEFree == 'Often' | data3$WorkToolsFrequencyKNIMEFree == 'Most of the time', 1,0)
data3$WorkToolsFrequencyMathematica_t2 = ifelse(data3$WorkToolsFrequencyMathematica == 'Often' | data3$WorkToolsFrequencyMathematica == 'Most of the time', 1,0)
data3$WorkToolsFrequencyMATLAB_t2 = ifelse(data3$WorkToolsFrequencyMATLAB == 'Often' | data3$WorkToolsFrequencyMATLAB == 'Most of the time', 1,0)
data3$WorkToolsFrequencyAzure_t2 = ifelse(data3$WorkToolsFrequencyAzure == 'Often' | data3$WorkToolsFrequencyAzure == 'Most of the time', 1,0)
data3$WorkToolsFrequencyExcel_t2 = ifelse(data3$WorkToolsFrequencyExcel == 'Often' | data3$WorkToolsFrequencyExcel == 'Most of the time', 1,0)
data3$WorkToolsFrequencyMicrosoftRServer_t2 = ifelse(data3$WorkToolsFrequencyMicrosoftRServer == 'Often' | data3$WorkToolsFrequencyMicrosoftRServer == 'Most of the time', 1,0)
data3$WorkToolsFrequencyMicrosoftSQL_t2 = ifelse(data3$WorkToolsFrequencyMicrosoftSQL == 'Often' | data3$WorkToolsFrequencyMicrosoftSQL == 'Most of the time', 1,0)
data3$WorkToolsFrequencyMinitab_t2 = ifelse(data3$WorkToolsFrequencyMinitab == 'Often' | data3$WorkToolsFrequencyMinitab == 'Most of the time', 1,0)
data3$WorkToolsFrequencyNoSQL_t2 = ifelse(data3$WorkToolsFrequencyNoSQL == 'Often' | data3$WorkToolsFrequencyNoSQL == 'Most of the time', 1,0)
data3$WorkToolsFrequencyOracle_t2 = ifelse(data3$WorkToolsFrequencyOracle == 'Often' | data3$WorkToolsFrequencyOracle == 'Most of the time', 1,0)
data3$WorkToolsFrequencyOrange_t2 = ifelse(data3$WorkToolsFrequencyOrange == 'Often' | data3$WorkToolsFrequencyOrange == 'Most of the time', 1,0)
data3$WorkToolsFrequencyPerl_t2 = ifelse(data3$WorkToolsFrequencyPerl == 'Often' | data3$WorkToolsFrequencyPerl == 'Most of the time', 1,0)
data3$WorkToolsFrequencyPython_t2 = ifelse(data3$WorkToolsFrequencyPython == 'Often' | data3$WorkToolsFrequencyPython == 'Most of the time', 1,0)
data3$WorkToolsFrequencyQlik_t2 = ifelse(data3$WorkToolsFrequencyQlik == 'Often' | data3$WorkToolsFrequencyQlik == 'Most of the time', 1,0)
data3$WorkToolsFrequencyR_t2 = ifelse(data3$WorkToolsFrequencyR == 'Often' | data3$WorkToolsFrequencyR == 'Most of the time', 1,0)
data3$WorkToolsFrequencyRapidMinerCommercial_t2 = ifelse(data3$WorkToolsFrequencyRapidMinerCommercial == 'Often' | data3$WorkToolsFrequencyRapidMinerCommercial == 'Most of the time', 1,0)
data3$WorkToolsFrequencyRapidMinerFree_t2 = ifelse(data3$WorkToolsFrequencyRapidMinerFree == 'Often' | data3$WorkToolsFrequencyRapidMinerFree == 'Most of the time', 1,0)
data3$WorkToolsFrequencySalfrod_t2 = ifelse(data3$WorkToolsFrequencySalfrod == 'Often' | data3$WorkToolsFrequencySalfrod == 'Most of the time', 1,0)
data3$WorkToolsFrequencySAPBusinessObjects_t2 = ifelse(data3$WorkToolsFrequencySAPBusinessObjects == 'Often' | data3$WorkToolsFrequencySAPBusinessObjects == 'Most of the time', 1,0)
data3$WorkToolsFrequencySASBase_t2 = ifelse(data3$WorkToolsFrequencySASBase == 'Often' | data3$WorkToolsFrequencySASBase == 'Most of the time', 1,0)
data3$WorkToolsFrequencySASEnterprise_t2 = ifelse(data3$WorkToolsFrequencySASEnterprise == 'Often' | data3$WorkToolsFrequencySASEnterprise == 'Most of the time', 1,0)
data3$WorkToolsFrequencySASJMP_t2 = ifelse(data3$WorkToolsFrequencySASJMP == 'Often' | data3$WorkToolsFrequencySASJMP == 'Most of the time', 1,0)
data3$WorkToolsFrequencySpark_t2 = ifelse(data3$WorkToolsFrequencySpark == 'Often' | data3$WorkToolsFrequencySpark == 'Most of the time', 1,0)
data3$WorkToolsFrequencySQL_t2 = ifelse(data3$WorkToolsFrequencySQL == 'Often' | data3$WorkToolsFrequencySQL == 'Most of the time', 1,0)
data3$WorkToolsFrequencyStan_t2 = ifelse(data3$WorkToolsFrequencyStan == 'Often' | data3$WorkToolsFrequencyStan == 'Most of the time', 1,0)
data3$WorkToolsFrequencyStatistica_t2 = ifelse(data3$WorkToolsFrequencyStatistica == 'Often' | data3$WorkToolsFrequencyStatistica == 'Most of the time', 1,0)
data3$WorkToolsFrequencyTableau_t2 = ifelse(data3$WorkToolsFrequencyTableau == 'Often' | data3$WorkToolsFrequencyTableau == 'Most of the time', 1,0)
data3$WorkToolsFrequencyTensorFlow_t2 = ifelse(data3$WorkToolsFrequencyTensorFlow == 'Often' | data3$WorkToolsFrequencyTensorFlow == 'Most of the time', 1,0)
data3$WorkToolsFrequencyTIBCO_t2 = ifelse(data3$WorkToolsFrequencyTIBCO == 'Often' | data3$WorkToolsFrequencyTIBCO == 'Most of the time', 1,0)
data3$WorkToolsFrequencyUnix_t2 = ifelse(data3$WorkToolsFrequencyUnix == 'Often' | data3$WorkToolsFrequencyUnix == 'Most of the time', 1,0)

# calculate a raw count of how many tools a respondent uses frequently and examine the distribution
data3$tools = rowSums(data3[229:276])
summary(data3$tools)
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#0.000   0.000   0.000   1.534   3.000  25.000


#Create top-2 box variables for work methods to identify if someone uses that method 'Often' or 'Most of the time'
#Looking to count only people who use this method fequently in their work

data3$WorkMethodsFrequencyA/B_t2 = ifelse(data3$WorkMethodsFrequencyA/B == 'Often' | data3$WorkMethodsFrequencyA/B == 'Most of the time', 1,0)
data3$WorkMethodsFrequencyAssociationRules_t2 = ifelse(data3$WorkMethodsFrequencyAssociationRules == 'Often' | data3$WorkMethodsFrequencyAssociationRules == 'Most of the time', 1,0)
data3$WorkMethodsFrequencyBayesian_t2 = ifelse(data3$WorkMethodsFrequencyBayesian == 'Often' | data3$WorkMethodsFrequencyBayesian == 'Most of the time', 1,0)
data3$WorkMethodsFrequencyCNNs_t2 = ifelse(data3$WorkMethodsFrequencyCNNs == 'Often' | data3$WorkMethodsFrequencyCNNs == 'Most of the time', 1,0)
data3$WorkMethodsFrequencyCollaborativeFiltering_t2 = ifelse(data3$WorkMethodsFrequencyCollaborativeFiltering == 'Often' | data3$WorkMethodsFrequencyCollaborativeFiltering == 'Most of the time', 1,0)
data3$WorkMethodsFrequencyCross-Validation_t2 = ifelse(data3$WorkMethodsFrequencyCross-Validation == 'Often' | data3$WorkMethodsFrequencyCross-Validation == 'Most of the time', 1,0)
data3$WorkMethodsFrequencyDataVisualization_t2 = ifelse(data3$WorkMethodsFrequencyDataVisualization == 'Often' | data3$WorkMethodsFrequencyDataVisualization == 'Most of the time', 1,0)
data3$WorkMethodsFrequencyDecisionTrees_t2 = ifelse(data3$WorkMethodsFrequencyDecisionTrees == 'Often' | data3$WorkMethodsFrequencyDecisionTrees == 'Most of the time', 1,0)
data3$WorkMethodsFrequencyEnsembleMethods_t2 = ifelse(data3$WorkMethodsFrequencyEnsembleMethods == 'Often' | data3$WorkMethodsFrequencyEnsembleMethods == 'Most of the time', 1,0)
data3$WorkMethodsFrequencyEvolutionaryApproaches_t2 = ifelse(data3$WorkMethodsFrequencyEvolutionaryApproaches == 'Often' | data3$WorkMethodsFrequencyEvolutionaryApproaches == 'Most of the time', 1,0)
data3$WorkMethodsFrequencyGANs_t2 = ifelse(data3$WorkMethodsFrequencyGANs == 'Often' | data3$WorkMethodsFrequencyGANs == 'Most of the time', 1,0)
data3$WorkMethodsFrequencyGBM_t2 = ifelse(data3$WorkMethodsFrequencyGBM == 'Often' | data3$WorkMethodsFrequencyGBM == 'Most of the time', 1,0)
data3$WorkMethodsFrequencyHMMs_t2 = ifelse(data3$WorkMethodsFrequencyHMMs == 'Often' | data3$WorkMethodsFrequencyHMMs == 'Most of the time', 1,0)
data3$WorkMethodsFrequencyKNN_t2 = ifelse(data3$WorkMethodsFrequencyKNN == 'Often' | data3$WorkMethodsFrequencyKNN == 'Most of the time', 1,0)
data3$WorkMethodsFrequencyLiftAnalysis_t2 = ifelse(data3$WorkMethodsFrequencyLiftAnalysis == 'Often' | data3$WorkMethodsFrequencyLiftAnalysis == 'Most of the time', 1,0)
data3$WorkMethodsFrequencyLogisticRegression_t2 = ifelse(data3$WorkMethodsFrequencyLogisticRegression == 'Often' | data3$WorkMethodsFrequencyLogisticRegression == 'Most of the time', 1,0)
data3$WorkMethodsFrequencyMLN_t2 = ifelse(data3$WorkMethodsFrequencyMLN == 'Often' | data3$WorkMethodsFrequencyMLN == 'Most of the time', 1,0)
data3$WorkMethodsFrequencyNaiveBayes_t2 = ifelse(data3$WorkMethodsFrequencyNaiveBayes == 'Often' | data3$WorkMethodsFrequencyNaiveBayes == 'Most of the time', 1,0)
data3$WorkMethodsFrequencyNLP_t2 = ifelse(data3$WorkMethodsFrequencyNLP == 'Often' | data3$WorkMethodsFrequencyNLP == 'Most of the time', 1,0)
data3$WorkMethodsFrequencyNeuralNetworks_t2 = ifelse(data3$WorkMethodsFrequencyNeuralNetworks == 'Often' | data3$WorkMethodsFrequencyNeuralNetworks == 'Most of the time', 1,0)
data3$WorkMethodsFrequencyPCA_t2 = ifelse(data3$WorkMethodsFrequencyPCA == 'Often' | data3$WorkMethodsFrequencyPCA == 'Most of the time', 1,0)
data3$WorkMethodsFrequencyPrescriptiveModeling_t2 = ifelse(data3$WorkMethodsFrequencyPrescriptiveModeling == 'Often' | data3$WorkMethodsFrequencyPrescriptiveModeling == 'Most of the time', 1,0)
data3$WorkMethodsFrequencyRandomForests_t2 = ifelse(data3$WorkMethodsFrequencyRandomForests == 'Often' | data3$WorkMethodsFrequencyRandomForests == 'Most of the time', 1,0)
data3$WorkMethodsFrequencyRecommenderSystems_t2 = ifelse(data3$WorkMethodsFrequencyRecommenderSystems == 'Often' | data3$WorkMethodsFrequencyRecommenderSystems == 'Most of the time', 1,0)
data3$WorkMethodsFrequencyRNNs_t2 = ifelse(data3$WorkMethodsFrequencyRNNs == 'Often' | data3$WorkMethodsFrequencyRNNs == 'Most of the time', 1,0)
data3$WorkMethodsFrequencySegmentation_t2 = ifelse(data3$WorkMethodsFrequencySegmentation == 'Often' | data3$WorkMethodsFrequencySegmentation == 'Most of the time', 1,0)
data3$WorkMethodsFrequencySimulation_t2 = ifelse(data3$WorkMethodsFrequencySimulation == 'Often' | data3$WorkMethodsFrequencySimulation == 'Most of the time', 1,0)
data3$WorkMethodsFrequencySVMs_t2 = ifelse(data3$WorkMethodsFrequencySVMs == 'Often' | data3$WorkMethodsFrequencySVMs == 'Most of the time', 1,0)
data3$WorkMethodsFrequencyTextAnalysis_t2 = ifelse(data3$WorkMethodsFrequencyTextAnalysis == 'Often' | data3$WorkMethodsFrequencyTextAnalysis == 'Most of the time', 1,0)
data3$WorkMethodsFrequencyTimeSeriesAnalysis_t2 = ifelse(data3$WorkMethodsFrequencyTimeSeriesAnalysis == 'Often' | data3$WorkMethodsFrequencyTimeSeriesAnalysis == 'Most of the time', 1,0)

# calculate a raw count of how many methods a respondent uses frequently and examine the distribution
data3$methods = rowSums(data3[278:305])
summary(data3$methods)
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#0.000   0.000   0.000   1.955   3.000  28.000

# Create weighting vector to calculate work methods complexity
# All methods initially set to a weight of 1, will need expert input to adjust complexity

# WorkMethodsFrequencyA/B =1
# WorkMethodsFrequencyAssociationRules =1
# WorkMethodsFrequencyBayesian =1
# WorkMethodsFrequencyCNNs =1
# WorkMethodsFrequencyCollaborativeFiltering =1
# WorkMethodsFrequencyCross-Validation =1
# WorkMethodsFrequencyDataVisualization =1
# WorkMethodsFrequencyDecisionTrees =1
# WorkMethodsFrequencyEnsembleMethods =1
# WorkMethodsFrequencyEvolutionaryApproaches =1
# WorkMethodsFrequencyGANs =1
# WorkMethodsFrequencyGBM =1
# WorkMethodsFrequencyHMMs =1
# WorkMethodsFrequencyKNN =1
# WorkMethodsFrequencyLiftAnalysis =1
# WorkMethodsFrequencyLogisticRegression =1
# WorkMethodsFrequencyMLN =1
# WorkMethodsFrequencyNaiveBayes =1
# WorkMethodsFrequencyNLP =1
# WorkMethodsFrequencyNeuralNetworks =1
# WorkMethodsFrequencyPCA =1
# WorkMethodsFrequencyPrescriptiveModeling =1
# WorkMethodsFrequencyRandomForests =1
# WorkMethodsFrequencyRecommenderSystems =1
# WorkMethodsFrequencyRNNs =1
# WorkMethodsFrequencySegmentation =1
# WorkMethodsFrequencySimulation =1
# WorkMethodsFrequencySVMs =1
# WorkMethodsFrequencyTextAnalysis =1
# WorkMethodsFrequencyTimeSeriesAnalysis =1

methods_complexity_vec = c(1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1)

data3$methods_complexity_score = data3[278:305] %*% diag(methods_complexity_vec) 

# Create weighting vector to calculate work tools complexity
# All tools initially set to a weight of 1, will need expert input to adjust complexity

# WorkToolsFrequencyAmazonML =1
# WorkToolsFrequencyAWS =1
# WorkToolsFrequencyAngoss =1
# WorkToolsFrequencyC =1
# WorkToolsFrequencyCloudera =1
# WorkToolsFrequencyDataRobot =1
# WorkToolsFrequencyFlume =1
# WorkToolsFrequencyGCP =1
# WorkToolsFrequencyHadoop =1
# WorkToolsFrequencyIBMCognos =1
# WorkToolsFrequencyIBMSPSSModeler =1
# WorkToolsFrequencyIBMSPSSStatistics =1
# WorkToolsFrequencyIBMWatson =1
# WorkToolsFrequencyImpala =1
# WorkToolsFrequencyJava =1
# WorkToolsFrequencyJulia =1
# WorkToolsFrequencyJupyter =1
# WorkToolsFrequencyKNIMECommercial =1
# WorkToolsFrequencyKNIMEFree =1
# WorkToolsFrequencyMathematica =1
# WorkToolsFrequencyMATLAB =1
# WorkToolsFrequencyAzure =1
# WorkToolsFrequencyExcel =1
# WorkToolsFrequencyMicrosoftRServer =1
# WorkToolsFrequencyMicrosoftSQL =1
# WorkToolsFrequencyMinitab =1
# WorkToolsFrequencyNoSQL =1
# WorkToolsFrequencyOracle =1
# WorkToolsFrequencyOrange =1
# WorkToolsFrequencyPerl =1
# WorkToolsFrequencyPython =1
# WorkToolsFrequencyQlik =1
# WorkToolsFrequencyR =1
# WorkToolsFrequencyRapidMinerCommercial =1
# WorkToolsFrequencyRapidMinerFree =1
# WorkToolsFrequencySalfrod =1
# WorkToolsFrequencySAPBusinessObjects =1
# WorkToolsFrequencySASBase =1
# WorkToolsFrequencySASEnterprise =1
# WorkToolsFrequencySASJMP =1
# WorkToolsFrequencySpark =1
# WorkToolsFrequencySQL =1
# WorkToolsFrequencyStan =1
# WorkToolsFrequencyStatistica =1
# WorkToolsFrequencyTableau =1
# WorkToolsFrequencyTensorFlow =1
# WorkToolsFrequencyTIBCO =1
# WorkToolsFrequencyUnix =1

tools_complexity_vec = c(1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1)

data3$tools_complexity_score = data3[229:276] %*% diag(tools_complexity_vec) 
