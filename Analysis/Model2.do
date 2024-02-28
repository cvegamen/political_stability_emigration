

// Political Stability and Emigration Proyect

version 	15.1
clear 		all
set 		linesize 80
set more 	off

*******************************************************************
// import dataset
********************************************************************
import excel "C:\Users\catis\Box\Catalina and Natasha\PS and Emigration\Datasets\Clean Data\2007-2018.xlsx", sheet("Sheet1") firstrow

*****************************************************************
//rename
*****************************************************************
rename e3_human_flight_and_brain_drain humanflight
rename c3_group_grievance groupgriev


***********************************************************************
// create income  
***********************************************************************
generate high= 1 if rgdpna>=12736
replace high=0 if rgdpna<12736
generate medium= 1 if rgdpna<12736 & rgdpna>1045
replace medium=0 if rgdpna>=12736 
replace medium=0 if rgdpna<=1045
generate low=1 if rgdpna<=1045
replace low=0 if rgdpna>1045
 

******Export Data Set*************
export excel using stata_model_2

******************************
//import data set with low income and continents

import excel "C:\Users\catis\Box\Catalina and Natasha\PS and Emigration\Analysis\stata_model_2.xls", sheet("Sheet1") firstrow clear

*********************************************************************
//set as panel dada
**********************************************************
xtset year

*****Descriptive income**********

by high, sort: summarize humanflight 
by medium, sort: summarize humanflight 
by low, sort: summarize humanflight 


***********************************************************************
// Correlation
***********************************************************************

pwcorr rgdpna density pop014 trade pop014 trade popgrwt stability humanflight groupgriev polity5 fh_total civilw, sig star (.05)

***********************************************************************
// Regression 
***********************************************************************

regress stability humanflight rgdpna density  pop014 trade popgrwt  groupgriev polity5 fh_total civilw
vif 

************************************************************************8
//sensitivity tests
******************************************************************
//no humanflight
regress stability  rgdpna density  pop014 trade popgrwt polity5 civilw, robust
vif 

regress stability  rgdpna density  pop014 trade popgrwt fh_total civilw, robust
vif 

//polity5

regress stability humanflight rgdpna density  pop014 trade popgrwt  groupgriev polity5  civilw, robust
vif 
regress stability humanflight rgdpna density  pop014 trade popgrwt  polity5  civilw,robust
vif
//freedom house

regress stability humanflight rgdpna density pop014 trade popgrwt  groupgriev fh_total  civilw, robust
vif 
regress stability humanflight rgdpna density  pop014 trade popgrwt  fh_total  civilw, robust
vif 
**********************************************************************

**********************************************************************
// Interaction income
**********************************************************************8

//high income 

regress stability c.humanflight##c.high rgdpna density  pop014 trade popgrwt  groupgriev  fh_total civilw
vif 

//medium income

regress stability c.humanflight##c.medium rgdpna density  pop014 trade popgrwt  groupgriev  fh_total civilw
vif 
// low income

regress stability c.humanflight##c.low rgdpna density  pop014 trade popgrwt  groupgriev  fh_total civilw
vif 

******************************************************************
//Fixed effects
*************************************************************

xtreg stability c.humanflight##c.africa rgdpna density  pop014 trade popgrwt  groupgriev  fh_total civilw, fe

xtreg stability c.humanflight##c.namerica rgdpna density  pop014 trade popgrwt  groupgriev  fh_total civilw, fe

xtreg stability c.humanflight##c.camerica rgdpna density  pop014 trade popgrwt  groupgriev  fh_total civilw, fe

xtreg stability c.humanflight##c.europe rgdpna density  pop014 trade popgrwt  groupgriev  fh_total civilw, fe

xtreg stability c.humanflight##c.asia rgdpna density  pop014 trade popgrwt  groupgriev  fh_total civilw, fe

xtreg stability c.humanflight##c.oceania rgdpna density  pop014 trade popgrwt  groupgriev  fh_total civilw, fe

*********************************************************************
//fixed effects as dummie variables

regress stability humanflight rgdpna density  pop014 trade popgrwt  groupgriev  fh_total civilw namerica africa camerica europe asia oceania
vif 



***********************************************************************
// collinearity diagnostics
**********************************************************************

collin stability humanflight rgdpna density  pop014 trade popgrwt  groupgriev polity5 fh_total civilw