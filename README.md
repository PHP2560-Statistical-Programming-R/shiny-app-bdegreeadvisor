 
---
title: "Shiny App Description"
output: html_document
---

# Overview

BDegreeAdvisor is an interactive interface for undergraduate students and faculty advisors at Brown University to explore undergraduate concentrations, their requirements, current course offerings, and job postings all in one place . This app is meant to be a tool to assist students in academic planning as well as advisors in guiding students. We hope this app helps students along the confusing path of picking a concentration. 

# Why this is important

Throughout our time at Brown, we have struggled to navigate Brown's online resources to design an education plan that allows us to meet all our concentration requirements but also to explore Brown's Open Curriculum. Often times, we have multiple tabs open to compare different concentration requirements and available courses of interest. Especially, when we were freshmen, we found that navigating Brown's curriculum was overwhelming and confusing. We believe that our app will not only help students to thoughtfully design their academic plans, but also to provide resources to faculty and peer advisors to support students. Additionally, our package will expand the career options for seniors and graduate students, who often wrestle with their plans after graduation, by allowing them to search for jobs in an easy and concise way.

# Components

In this app users are able to select a concentration from a drop down menu and get a table of the required courses for the selected concentration. They are also able to select a second concentration and compare requirements between the two in order to facilitate making a choice or to encourage other users to double concentrate wisely. Users are able to plan their semesters by choosing a concentration and a particular semester and view which of their required courses are being taught in the selected semester. Lastly users are also able to explore the current job market by searching for a job/keyword/skill and a particular location and an up to date table of current jobs will populate with information such as the job title and description as well as a link that users can follow to apply for the job online. Users are able to download any output that they would like from the app and are able to save it to their device for easy viewing.

# Necessary Packages to Run the App
Please make sure the following packages are installed before you run the app:

1.`library(shiny)`

2.`library(DT)`

3.`library(rvest)`

4.`library(shinythemes)`

5.`library(dplyr)`

6.`library(devtools)`
  `devtools::install_github("PHP2560-Statistical-Programming-R/r-package-courses-brown/BDegreeAdvisor")`
  `library(BDegreeAdvisor)`


# Work Breakdown

Requirements Tab: Carolina & Weiqi

Comparison Tab: Carolina & Weiqi

Scheduling Tab: Maggie

Job Tab: Joyce
