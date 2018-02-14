# Life Data Analysis & Reliability App


The reliability analysis app contains the basic tools for an introduction to the [life data analysis](http://www.weibull.com/basics/lifedata.htm) field.  Among others, reliability analysis is useful to statistically infer the Time-To-Failure of mechanical components.  
  
The aim is this app is to educate, inform, as well as, give analysis capabilities in order to inspire the user. The aim is to motivate the user to invest time and have a *deep divee* into the world of reliability analysis. For simplification and information purposes the topic *'Weibull analysis' and 'Life data'* has been chosen. This topic was covered by Dr. Abernethy in his book  "The New Weibull Handbook". 

[![](https://www.blog.google/static/blog/images/google-200x200.7714256da16f.png)](http://www.reliabledynamics.com)

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

- **Weibull Modeler**
    + **Upload your data** functionality  
You can upload your own data (CSV file) in order to do a reliability plot, and see how your data look like. In order to do so, is necessary certain formatting is fulfilled. In order to do that you can download the CSV template, available in the tab.  
<br>
    + **Reliability calculator**  
By default there is an full functional example with two failure modes load in this tab for aesthetics purposes. If you upload your own data in the *upload* tab, you will see the reliability plot here.  There are certain functionalists to play around with the parameters of the plot such as:  
 -- Confidence interval level  
 -- Method to fit the points: here is possible to choose between 3 methods.  
 -- Method to calculate the confidence intervals.  
<br><br>
  
- **Zero Failure Testing**
    + Time testing complete
    For Zero test complete test to failure, how much time do I need to test a component to demostrate certain reliability level? Given the failure mode, the life to demonstrate, number of components available will output the amount of time that is needed to test those components wirth zero failure.
    + Time testing partial
    In case that the Zero test can not be completed, what is the level of reliability achievied?  



## Coming soon    
- **Forecast Modeler**
    + Model generator
    + Train models
    + Compare models
 


<br><br><br>

#### Inspired by Openreliability 
*"Openreliability is dedicated to the development of reliability engineering and operations research applications software under the open source paradigm.*
*As open source we invite a vigorous peer review of the technical accuracy of the software code in use. An open dialog on the use and application of this software is also encouraged. We appreciate your involvement on any level."*  
*"Our first and most active project is Abernethy Reliability Methods based on "The New Weibull Handbook, Fifth Edition", by Robert B. Abernethy. This reference has been the textbook for a Weibull Analysis course that has successfully trained quality and reliability engineers for over three decades."* 
[Openreliability.org](http://www.openreliability.org/index.html)



