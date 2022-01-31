*************************************************************************************************************************
*** 											WELCOME TO CU DATA WEEK!										      ***
*** StatsTalk: 		Basic Statistics Using SAS																	      ***
*** Instructor: 	Laura Grau																					      ***
*** Version date: 	2022-01-07																					      ***
************************************************************************************************************************;

************************************************************************************************************************
Some SAS Basics - Before we start
************************************************************************************************************************;
*Libraries are folders where SAS data is stored;

*To connect a folder on your computer (or on the cloud for SAS OnDemand), you must run a LIBNAME statement,
which has the following syntax;

*libname <libref> "Pathway";

*There are a few automatic SAS Libraries with sample data. 
For this StatsTalk, we will be using various datasets from the SASHELP library;


************************************************************************************************************************
Some SAS Basics - Descriptive Statistics
************************************************************************************************************************;
*There are many PROCs that can produce descriptive statistics, including:
	PROC MEANS, PROC UNIVARIATE, PROC TABULATE, PROC FREQ, and PROC REPORT;

*Here is a quick example of how to use PROC TABULATE;
proc tabulate data=sashelp.bweight;
class momsmoke;
var weight;
table weight, momsmoke*(mean std)*f=comma9.1/nocellmerge;
run;

proc tabulate data=sashelp.bweight;
class momsmoke momedlevel;
table momedlevel, momsmoke*(n colpctn)*f=comma9.1/nocellmerge;
run;

************************************************************************************************************************
Scenario 1: Is maternal smoking associated with low birth weight? (Chi Square test)
************************************************************************************************************************;

proc freq data=sashelp.birthwgt;
tables lowbirthwgt*smoking/chisq /*exact*/;
run;

************************************************************************************************************************
Scenario 2: Does infant birthweight differ by maternal smoking? (Two sample t test)
************************************************************************************************************************;

proc ttest data=sashelp.bweight;
class momsmoke;
var weight;
run;

************************************************************************************************************************
Scenario 2B: Does infant birthweight differ by maternal smoking? 
		What if you cannot assume normality? 
		(Mann-Whitney U test / Wilcoxon rank sum test)
************************************************************************************************************************;

proc npar1way data=sashelp.bweight wilcoxon;
class momsmoke;
var weight;
run;

************************************************************************************************************************
Scenario 3: Is there a significantdifference in cars' MPG City and MPG Highway? (Paired t-test)
************************************************************************************************************************;
proc ttest data=sashelp.cars;
paired mpg_city*mpg_highway;
run;

************************************************************************************************************************
Scenario 3B: Is there a significantdifference in cars' MPG City and MPG Highway?
		What is you cannot assume normality?
		(Wilcoxon Signed-Rank test)
************************************************************************************************************************;
*Step 1. Calculate the difference;
data analysis;
set sashelp.cars;
diff=mpg_city-mpg_highway;
run;

*Step 2. Run Proc Univariate;
proc univariate data=analysis;
var diff;
run;

************************************************************************************************************************
Scenario 4: Is there a significant linear relationship between weight and height? (Simple Linear Regression)
************************************************************************************************************************;
*Always visualize your data before you start;
*Scatter plot;
proc sgplot data=sashelp.class;
scatter x=height y=weight;
run;

*Histograms;
proc sgplot data=sashelp.class;
histogram height;
run;
proc sgplot data=sashelp.class;
histogram weight;
run;

*Linear regression;
proc reg data=sashelp.class;
model weight=height;
run;quit;



