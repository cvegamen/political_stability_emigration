// Political Stability and Emigration Proyect


version 	15.1
clear 		all
set 		linesize 80
set more 	off


***********************************************************************
// import data set 
***********************************************************************

import excel "C:\Users\catis\Box\Catalina and Natasha\PS and Emigration\Datasets\Clean Data\1997-2011.xlsx", sheet("Sheet1") firstrow

*********************************************************************
//set as panel dada
**********************************************************
xtset year
***********************************************************************
// create income  
***********************************************************************
generate high= 1 if gdp>=12736
replace high=0 if gdp<12736
generate medium= 1 if gdp<12736 & gdp>1045
replace medium=0 if gdp>=12736 
replace medium=0 if gdp<=1045
generate low=1 if gdp<=1045
replace low=0 if gdp>1045
by high, sort: summarize totalflw hsklflw lwsklflw
by medium, sort: summarize totalflw hsklflw lwsklflw
by low, sort: summarize totalflw hsklflw lwsklflw



***********************************************************************
// Correlarion Matrix
***********************************************************************
pwcorr polity totalflw lwsklflw hsklflw Oil popgrwt density gdp Trade fractional civilw stability, sig star (.05)

***********************************************************************
// Regression
***********************************************************************

regress  stability hsklflw Oil popgrwt density gdp Trade fractional civilw  
vif
regress  stability lwsklflw Oil popgrwt density gdp Trade fractional civilw  
vif
regress stability totalflw Oil popgrwt density gdp Trade fractional civilw 
vif
************************************************************************8
//sensitivity tests
******************************************************************

regress stability polity popgrwt density gdp Trade civilw, robust
vif

***********************************************************************
// Regression with interactions income 
**********************************************************************

//high income high skill
regress  stability c.hsklflw##c.high Oil popgrwt density gdp Trade fractional civilw
vif
//middle income high skill   

regress  stability c.hsklflw##c.medium Oil popgrwt density gdp Trade fractional civilw
vif
//low income high skill   

regress  stability c.hsklflw##c.low Oil popgrwt density gdp Trade fractional civilw
vif
//high income low skill
regress  stability c.lwsklflw##c.high Oil popgrwt density gdp Trade fractional civilw
vif
//middle income low skill   

regress  stability c.lwsklflw##c.medium Oil popgrwt density gdp Trade fractional civilw
vif
//low income low skill   

regress  stability c.lwsklflw##c.low Oil popgrwt density gdp Trade fractional civilw
vif
//high income total
regress  stability c.totalflw##c.high Oil popgrwt density gdp Trade fractional civilw
vif
//middle income total   

regress  stability c.totalflw##c.medium Oil popgrwt density gdp Trade fractional civilw
vif
//low income total   

regress  stability c.totalflw##c.low Oil popgrwt density gdp Trade fractional civilw
vif
***********************************************************************
// Fixed effects on continents
**********************************************************************
// africa

//dumies


regress  stability hsklflw Oil popgrwt density gdp Trade fractional civilw  NAMER AFRICA CSAMER EUROPE OCEANIA 
vif

regress  stability lwsklflw Oil popgrwt density gdp Trade fractional civilw  NAMER AFRICA CSAMER EUROPE OCEANIA 
vif

regress stability totalflw Oil popgrwt density gdp Trade fractional civilw NAMER AFRICA CSAMER EUROPE OCEANIA 
vif

******************************************************************************
xtreg  stability c.totalflw##c.AFRICA Oil popgrwt density gdp Trade fractional civilw, fe

xtreg  stability c.totalflw##c.NAMER Oil popgrwt density gdp Trade fractional civilw, fe

xtreg  stability c.totalflw##c.CSAMER Oil popgrwt density gdp Trade fractional civilw, fe

xtreg  stability c.totalflw##c.EUROPE Oil popgrwt density gdp Trade fractional civilw, fe

xtreg  stability c.totalflw##c.ASIA Oil popgrwt density gdp Trade fractional civilw, fe

xtreg  stability c.totalflw##c.OCEANIA Oil popgrwt density gdp Trade fractional civilw, fe

***********************************************************************
// check variance inflator factor
**********************************************************************

regress  stability hsklflw Oil popgrwt density gdp Trade fractional civilw 

vif 

regress  stability lwsklflw Oil popgrwt density gdp Trade fractional civilw  
vif 
regress stability totalflw Oil popgrwt density gdp Trade fractional civilw 
vif


***********************************************************************
// collinearity diagnostics
**********************************************************************

collinearity stability hsklflw Oil popgrwt density gdp Trade fractional civilw 