---
title: "README"
author: "William King"
date: "10/15/2019"
output: html_document
---
<h1> Project URL </h1>

<body>
https://kingwilliam87.shinyapps.io/project_2/
</body>

<h1>Description of Data</h1>

<body>
The data used for this project was provided by Urban Ministries, a charity organization that has worked to help those in need in our community for decades.  

The data they provided was collected over multiple decades and is stored as a tsv file. It includes numerical and categorical data about their clients and the different types of assistance they received at different times.
</body>

<h1>Project 2 Overview</h1>

<body>
In this project, the question I hope to help answer is, what useful information can this data tell us? This may seem like a very vague and open-ended question, but I think it is an important one. Urban Ministries have been collecting this data for decades, and I am sure they want to be able to better understand what information they can take from it.
  
This project displays information about services provided, trends over time, and compares the service count and duration among clients.
</body>

<h1>Audience</h1>

<body>
My audience for this project will be the staff at Urban Ministries. My hope is that they can view and explore this project themselves to be able to see the information they want to see.
</body>

<h1>Analysis Methods</h1>

<body>
For this project I will be relying on R, specifically packages including tidyverse, ggplot, dplyr and a few others. I have attempted to wrangle the data into forms that can be used to create straightforward visualizations to help us learn more about what exactly the data has to tell us.
</body>

<h1>Interactivity</h1>

<body>
All tables and plots displayed are based on variables chosen by the user. To begin, the user selects a service and a time period. A plot is displayed with the total annual amount of that particular service provided over each year in the time period.
  
A second plot displays all instances of that service being provided over the time period. The user can then use the mouse cursor to capture the points they wish to have a closer look at. These points are used to populate a table showing Client ID, Service Count and Service Duration for the clients represented in the captured points. A third plot displays the count of clients in different strata of Service Duration from the captured points.

My hope is that users (specifically, our clients at UMD) are able to explore the project on their own to learn more helpful information about their data.
</body>
