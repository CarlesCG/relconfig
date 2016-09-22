# Reliability App introduction
The reliability analysis app contains the basic tools for an introduction to the [life data analysis](http://www.weibull.com/basics/lifedata.htm) field.  Among others, reliability analysis is extremely useful to statistically infer the Time-To-Failure of mechanical components.  
  
The aim is to inform, play around and give basic analysis capabilities in order to inspire the user to invest time and have a *deep divee* into the world of reliability analysis. For simplification and information purposes the sub-branch *'Weibull analysis'* has been chosen.   

- You can find an interesting introduction presentation [here](http://ecm.corp.vattenfall.com/sso_en/livelink.exe/open/86063742)  

The app relies heavily on the [abrem](http://www.openreliability.org/HTML/abrem.html) package for R.   



Features: 
--------
- Play with parameters for generation artificial data and reliability plot
- Upload your data for analysis & template download.
- Calculates on the fly if your data has one or more failure modes and does the plot accordingly. 
- Play with different paraders for the regression fit, the confident method and the confident percentage.


## App division
The app is divided into 3 functional tabs:

- **Reliability plot generator**  
In this tab is possible to generate some random data following a Weibull distributions and get a density and a reliability plot.  Is possible to play with:  
  -- Number of points to generate  
  -- [Shape and scale](https://en.wikipedia.org/wiki/Weibull_distribution) of the random numbers.  
  -- Confidence intervasl for the plot.  
  -- Different density kernel and bandwidth adjustment.  

- **Upload your data** functionality  
You can upload your own data (CSV file) in order to do a quick reliability plot, and see how your data look like. In order to do so, is necessary certain formatting is fulfilled. In order to do that you can download the CSV template, available in the tab.  
<br>
- **Reliability plot calculator**  
By default there is an full functional example with two failure modes load in this tab for aesthetics purposes. If you upload your own data in the *upload* tab, you will see the reliability plot here.  There are certain functionalists to play around with the parameters of the plot such as:  
 -- Confidence interval level  
 -- Method to fit the points: here is possible to choose between 3 methods.  
 -- Method to calculate the confidence intervals.  
 


<br><br><br>

#### About Abrem & Openreliability.org 
*"The R package 'abrem', a contraction of Abernethy Reliability Methods, is a rework of the original weibulltoolkit package now calling technical functions from pivotals (in future to be abremPivotals) and debias (to become abremDebias)."*
<br><br>
*"Openreliability is dedicated to the development of reliability engineering and operations research applications software under the open source paradigm.*
*As open source we invite a vigorous peer review of the technical accuracy of the software code in use. An open dialog on the use and application of this software is also encouraged. We appreciate your involvement on any level."*
<br><br>
*"Our first and most active project is Abernethy Reliability Methods based on "The New Weibull Handbook, Fifth Edition", by Robert B. Abernethy. This reference has been the textbook for a Weibull Analysis course that has successfully trained quality and reliability engineers for over three decades."* 
[Openreliability.org](http://www.openreliability.org/index.html)



