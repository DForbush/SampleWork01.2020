# Sample Work as of January 2020

This repository contains a sample of my work as of January, 2020. It is divided into two folders: work from inside the classroom and work from outside of the classroom that comes from my internship and fellowship experiences. This README file contains additional information to provide context for the work that is included in this repository. 

Please be aware that much of the R Code in this repository cannot be run. This is because it was either created in a specific context of additional code or it imports files that I could not take home with me and/or no longer have access to. 

# Outside Classroom Folder

This folder contains five files that are representative of three different projects. 

1. Sample code from the Cook County Assessor's Office.

In the file "CCAO_Code_Fragments.R" are samples of code written during my time as a Data Science Intern at the Cook County Asssessor's Office. It includes three sub-examples: 

(1), a function that was written to efficiently recode multiple variables from one data type to another in one line of code; 

(2), code classifying properties into three "price terciles" using a probit logistic regression model, where every property is predicted to be in the low, medium, or high price tercile; 

(3) code that is used to implement different models for predicting the value of every property within a given township. Specifically, I wrote code to implement Quantile regression and Gradient Boosting Machine models as part of the property assesssment algorithm.

The full code for the residential property assessment algorithm used by the Cook County Assessor's Office is publically available at https://gitlab.com/ccao-data-science---modeling/ccao_sf_cama_dev. Please note that the code they use has changed significnatly since my time as a Data Science Intern.

2. An analysis of how people travel to work in Chicago census tracts

The file "ModeAnalysis.R" contains the code that I wrote as a Mayoral Fellow to analyze the mode of transportation that Chicago residents use to get to work. This is an analysis of data collected by the Census Bureau for the American Community Survey. The results of this data analysis were compiled into Powerpoint slides, which are included in the file "eTOD Mode Data Analysis Slides.pdf"

3. Code used to identify City-Owned Laned that it is elligible for "transit-oriented development" incentives. 

The file "COLS_TOD_Data_Merge.R" contains R code I wrote as a Mayoral Fellow to filter the City of Chicago's City-Owned Land database based on properties that are within a 1/2 mile of a transit station (either CTA or Metra station) and are therefore eligible for transit-oriented development incentives. The results of this analysis has been mapped in an html file, "COLS_TOD_Map.html."

# Inside Classroom File

This folder contains samples from my graduate coursework. First, it includes three homework assignments from my Advanced Data Analysis class, as well as four datasets (Olympics.csv, TitanicData.csv, cell.csv, and fines.csv) that are used in those homework assignments. These homework assignments are included to demonstrate my competencies in the specifics of statistical methods, linear regression models, and probability.  

Second, there are three group reports which all essentially analyze a particular dataset and try to find insights using statistical analysis. First, the report for the Statistics for Management course (the "IDS 570_DanForbush_DemetriosCaoulas_Group6_Final Report.pdf" file) includes an analysis of Medicare outpatient treatment data. Second, the report for my Statistical Models and Methods for Business Analytics course (the "IDS 575 Final Report.pdf" file) analyzes a dataset of Divvy bikeshare trips. Finally, the report for my advanced data analysis course (the "PA 541 Final Paper.docx" file) includes an analysis of government spending at the state level. 
