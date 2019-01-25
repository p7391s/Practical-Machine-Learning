---
title: "Prediction Using Writeup Lifting Exercises Dataset"
author: "ps7391"
date: '25 january 2019'
output: 
        html_document:
                keep_md: yes
        md_document:
                variant: markdown_github
        pdf_document: default
---



### Background

Using devices such as Jawbone Up, Nike FuelBand, and Fitbit it is now possible to collect a large amount of data about personal activity relatively inexpensively. These type of devices are part of the quantified self movement – a group of enthusiasts who take measurements about themselves regularly to improve their health, to find patterns in their behavior, or because they are tech geeks. One thing that people regularly do is quantify how much of a particular activity they do, but they rarely quantify how well they do it. In this project, your goal will be to use data from accelerometers on the belt, forearm, arm, and dumbell of 6 participants. They were asked to perform barbell lifts correctly and incorrectly in 5 different ways. More information is available from the website here: http://groupware.les.inf.puc-rio.br/har (see the section on the Weight Lifting Exercise Dataset).

### Executive summary

Based on a dataset provide by HAR http://groupware.les.inf.puc-rio.br/har we will try to train a predictive model to perdict what exercise was performed using a dataset with 159 features.

We'll take the following steps:

* Process the data, for use to this project
* Explore the data, especially focussing on the two parameters we are interested in
* Model selection, where we try different models to help us answere our questions
* Model examination, to see wether our best model holds up to our standards
* Conclusion where we answere the questions based on the data
* Predicting the classification of the model on test set
