variableProcessing <- function(fromVector)
{
  listOfDifferentAttributes = c("F", "M", "Not Specified", 
                                "American Indian/Alaska Native", 
                                "Asian", "Black/African American", 
                                "Hispanic/Latino", 
                                "Two or More Races", "White", "OUT-STATE", 
                                "IN-STATE", "", "Below 2.25", "2.25 - 2.49", 
                                "2.50 - 2.74", "2.75 - 2.99", "3.00 - 3.24", 
                                "3.25 - 3.49", "3.50 - 3.74", "3.75 - 4.00", 
                                "17", "18", "19", "20 or older", 
                                "Non-Resident-Alien", "yes", "16 or below", 
                                "17-18", "19-20", "21-22", "23-24", "25-29", 
                                "30 or higher", "COAL", "COB", "COEP", "COH", 
                                "CON", "COST", "Part-Time", "Full-Time", 
                                "No", "Yes", "Not Greek", "Greek", "Neither", 
                                "Honors & Luck Day", "Within First Year", 
                                "Enrolled In 2nd Year but not later", 
                                "Enrolled In 3rd Year but not later", 
                                "Enrolled In 4th Year but not later", 
                                "Enrolled In 5th Year but not later", 
                                "Enrolled In 6th Year but not later", 
                                "Enrolled In 7th or More", "no degree", 
                                "Within the 3rd Year", "Within the 4th Year", 
                                "Within the 5th Year", "Within the 6th Year", 
                                "Within the 7th Year", 
                                "In the 8th Year or more", "Below 1.00", 
                                "1.00 - 1.24", "1.25 - 1.49", "1.50 -1.74", 
                                "1.75 - 1.99", "2.00 - 2.24")
  attributeTranslation = c("F", "M", "N/A", "N", "A", "B", "H", 
                           "M", "W", 0, 1, -1, 2.24, 2.49, 2.74, 2.99,
                           3.24, 3.49, 3.74, 4.00, 17, 18, 19, 20, 0, 1, 16, 
                           18, 20, 22, 24, 29, 30, "AL", "B", "EP", "H", 
                        "N", "ST", "PT", "FT", 0, 1, 0, 1, 0, 1, 1, 2, 3, 4, 5, 
                        6, 7, 0, 3, 4, 5, 6, 7, 8, .99, 1.24, 1.49, 1.74, 1.99, 
                        2.24)
  numberOfRows = length(fromVector)
  toVector = numeric(numberOfRows)
  for(i in 1:numberOfRows)
  {
    checkAttribute = fromVector[i]
    attributeNum = match(checkAttribute, listOfDifferentAttributes)
    toVector[i] = attributeTranslation[attributeNum]
  }
  
  return(toVector)
}

#-------------------------------~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#factors Before attending University

# Gender
genderTrain = variableProcessing(inputtrain[,3])
genderTest = variableProcessing(inputtest[,3])
Gender = as.factor(append(genderTrain, genderTest))
# Ethnicity
ethnicityTrain = variableProcessing(inputtrain[,4])
ethnicityTest = variableProcessing(inputtest[,4])
Ethnicity = as.factor(c(ethnicityTrain, ethnicityTest))
# residency
inSTATETrain = variableProcessing(inputtrain[,5])
inSTATETest = variableProcessing(inputtest[,5])
InState = as.numeric(c(inSTATETrain, inSTATETest))
# High School GPA
HSGPATrain = variableProcessing(inputtrain[,6])
HSGPATest = variableProcessing(inputtest[,6])
HSGPA = as.numeric(c(HSGPATrain, HSGPATest))
# Age
ageTrain = variableProcessing(inputtrain[,7])
ageTest = variableProcessing(inputtest[,7])
Age = as.numeric(c(ageTrain, ageTest))
# Citizenship
citizenTrain = variableProcessing(inputtrain[,8])
citizenTest = variableProcessing(inputtest[,8])
Citizen = as.factor(c(citizenTrain, citizenTest))
# ACT Cumulative
ACTCumulativeTrain = variableProcessing(inputtrain[,15])
ACTCumulativeTest = variableProcessing(inputtest[,15])
ACTCumulative = as.numeric(c(ACTCumulativeTrain, ACTCumulativeTest))
# ACT Math
ACTMathTrain = variableProcessing(inputtrain[,16])
ACTMathTest = variableProcessing(inputtest[,16])
ACTMath = as.numeric(c(ACTMathTrain, ACTMathTest))
# ACT English
ACTEngTrain = variableProcessing(inputtrain[,17])
ACTEngTest = variableProcessing(inputtest[,17])
ACTEng = as.numeric(c(ACTEngTrain, ACTEngTest))
# ACT Read
ACTReadTrain = variableProcessing(inputtrain[,18])
ACTReadTest = variableProcessing(inputtest[,18])
ACTRead = as.numeric(c(ACTReadTrain, ACTReadTest))
# ACT Science
ACTSciTrain = variableProcessing(inputtrain[,19])
ACTSciTest = variableProcessing(inputtest[,19])
ACTSci = as.numeric(c(ACTSciTrain, ACTSciTest))

#-------------------------------~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#factors after attending university

# College
collegeTrain = variableProcessing(inputtrain[,2])
collegeTest = variableProcessing(inputtest[,2])
College = as.factor(c(collegeTrain, collegeTest))
# First term course load
fTCourseLoadTrain = variableProcessing(inputtrain[,9])
fTCourseLoadTest = variableProcessing(inputtest[,9])
fTCourseLoad = as.factor(c(fTCourseLoadTrain, fTCourseLoadTest))
# First term GPA
fixIntegerIssue = as.character(inputtrain[,10])
fixIntegerIssueTest = as.character(inputtest[,10])
fTGPATrain = as.double(fixIntegerIssue)
fTGPATest = as.double(fixIntegerIssueTest)
fTGPA = c(fTGPATrain, fTGPATest)
# First term GPA Range -- No need for this
#fTGPARange = as.numeric(variableProcessing(inputtrain[,11]))
#fTGPARangeTest = as.numeric(variableProcessing(inputtest[,11]))
# pell grant
pellfTTrain = variableProcessing(inputtrain[,12])
pellfTTest = variableProcessing(inputtest[,12])
PellfT = as.numeric(c(pellfTTrain, pellfTTest))
# Greek
greekTrain = variableProcessing(inputtrain[,13])
greekTest = variableProcessing(inputtest[,13])
Greek = as.numeric(c(greekTrain, greekTest))
# Honors & Lucky Day
HLDTrain = variableProcessing(inputtrain[,14])
HLDTest = variableProcessing(inputtest[,14])
HLD = as.numeric(c(HLDTrain, HLDTest))
# Last term course load
lTCourseloadTrain = variableProcessing(inputtrain[,22])
lTCourseloadTest = variableProcessing(inputtest[,22])
lTCourseload = as.factor(c(lTCourseloadTrain, lTCourseloadTest))
# last enrollment time
lEnrollTimeTrain = variableProcessing(inputtrain[,23])
lEnrollTimeTest = variableProcessing(inputtest[,23])
lEnrollTime = as.factor(c(lEnrollTimeTrain, lEnrollTimeTest))
# Degree Term
degreeTTrain = variableProcessing(inputtrain[,24])
degreeTTest = variableProcessing(inputtest[,24])
DegreeT = as.numeric(c(degreeTTrain, degreeTTest))

# Graduation Success (0 or 1)
Success <- numeric(length(degreeTTrain))
for (i in 1:length(degreeTTrain)) if (degreeTTrain[i] != 0) Success[i]<-1

SuccessTest = numeric(length(degreeTTest))
for (i in 1:length(degreeTTest))
{
  if (degreeTTest[i] != 0)
  {
    SuccessTest[i] = 1
  }
}

# Decision tree with partykit
if(!require(partykit)){
  install.packages("partykit")
  require(partykit)
}

processedTrainTest = data.frame(Gender, Ethnicity, InState, HSGPA, Age, 
                                Citizen, ACTCumulative, ACTMath, ACTEng, 
                                ACTRead, ACTSci, College, fTCourseLoad, 
                                fTGPA, PellfT, Greek, HLD, lTCourseload, 
                                lEnrollTime, DegreeT)
#-------------------------------~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# decision tree (before enrollment)
BFData = subset(processedTrainTest, select = c(Gender, HSGPA, ACTSci, 
                                               ACTCumulative, ACTEng, ACTMath, 
                                               ACTRead, Ethnicity, InState,
                                               Age, Citizen))
BFTree <- ctree(Success ~ ., data = BFData[1:1281,])
plot(BFTree, terminal_panel=node_barplot(BFTree, id = FALSE, text = "vertical", ymax=1.19), 
     main = "Before enrollment")
#-------------------------------~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# during freshmen
DFData = subset(processedTrainTest, select = c(College, fTCourseLoad, PellfT, 
                                               HLD, fTGPA, Greek))
DFTree <- ctree( Success ~ ., data = DFData[1:1281,])
plot(DFTree, terminal_panel=node_barplot(DFTree, id = FALSE, text = "vertical", ymax=1.19), 
     main = "During freshman year")
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#before freshmen and during freshmen
BDFData = subset(processedTrainTest, select = c(Gender, HSGPA, ACTSci, 
                                                ACTCumulative, ACTEng, ACTMath, 
                                                ACTRead, Ethnicity, InState,
                                                Age, Citizen, College, 
                                                fTCourseLoad, PellfT, HLD, 
                                                fTGPA, Greek))
BDFTree <- ctree( Success ~ ., data = BDFData[1:1281,])
plot(BDFTree, terminal_panel=node_barplot(BDFTree, id = FALSE, text = "vertical", ymax=1.19),
     main = "Before and during freshman year")
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# BDTree Exclude GPA since it appears to drown everything else out
noGPAData = subset(processedTrainTest, select = c(Gender, ACTSci, 
                                                ACTCumulative, ACTEng, ACTMath, 
                                                ACTRead, Ethnicity, InState,
                                                Age, Citizen, College, 
                                                fTCourseLoad, PellfT, HLD, 
                                                Greek))
noGPATree = ctree( Success ~ ., data = noGPAData[1:1281,])
plot(noGPATree, terminal_panel=node_barplot(noGPATree, id = FALSE, text = "vertical", ymax=1.19), 
     main = "Before and during freshman year excluding GPA")

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Tree with significant factors identified from Jesse Robinson's Honors Thesis
JRData = subset(processedTrainTest, select = c(College, fTGPA, Greek))
JRTree = ctree( Success ~ ., data = JRData[1:1281,])
plot(JRTree, terminal_panel=node_barplot(JRTree, id = FALSE, text = "vertical", ymax=1.19), 
     main = "Factors found significant from JR's analysis")

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

BFSuccessProb = predict(BFTree, newdata = BFData[1282:1600,])
BFSuccessTable = table(BFSuccessProb, SuccessTest)

DFSuccessProb = predict(DFTree, newdata = DFData[1282:1600,])
DFSuccessTable = table(DFSuccessProb, SuccessTest)

BDFSuccessProb = predict(BDFTree, newdata = BDFData[1282:1600,])
BDFSuccessTable = table(BDFSuccessProb, SuccessTest)

noGPASuccessProb = predict(noGPATree, newdata = noGPAData[1282:1600,])
noGPASuccessTable = table(noGPASuccessProb, SuccessTest)

JRSuccessProb = predict(JRTree, newdata = JRData[1282:1600,])
JRSuccessTable = table(JRSuccessProb, SuccessTest)

tableProcessing <- function(someTable)
{
  rows = nrow(someTable)
  calculateActualPercentSuccess = numeric(rows)
  for(i in 1:rows)
  {
    calculateActualPercentSuccess[i] = someTable[i,2]/(someTable[i,1] + 
                                                         someTable[i,2])
  }
  
  someTableReturn = cbind(someTable, actualPercentSuccess = 
                            calculateActualPercentSuccess)
  
  return(someTableReturn)
}

BFTable = tableProcessing(BFSuccessTable)
BFTable

DFTable = tableProcessing(DFSuccessTable)
DFTable

BDFTable = tableProcessing(BDFSuccessTable)
BDFTable

noGPATable = tableProcessing(noGPASuccessTable)
noGPATable

JRTable = tableProcessing(JRSuccessTable)
JRTable
