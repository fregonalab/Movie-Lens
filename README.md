# HarvardX Professional Certificate - Final Project

## Table of contents
* [Overview](#overview)
* [Data Understanding](#EDA)
* [Resultados e Conclusões](#Resultados)
* [Tecnologias](#tecnologias)

## Overview
This project aims to develop a movie recommendation system by using a small subset of the original data provided by Netflix. To create it, two distinct algorithms where performed, a model-based approach and matrix factorization. The model-based approach was less accurate, but much faster. The matrix factorization have got a excellent RMSE score, however demanded more computational power.

## Data Source
The data is open source and can be found at the following website: https://grouplens.org/datasets/movielens/10m/.

## Data Understanding

File: projeto_parte1.ipynb

## Conclusions
This project has examined a couple of predictive techniques applied to a movie review data set. Through data exploration, we have seen that all predictors appeared to present a clear variability of rating values. Those remarks were used while modeling. After evaluating each model, the biggest improvements were achieved due to the movie and user effects. Regularization did not improve considerably the RMSE, but it was performed anyway. Using this final model we achieved an RMSE of 0.865. After we fitted our model, a new analysis in the residuals was performed. During the data exploration analysis, we observed a strong correlation between distinct movies, which highlighted structures in the residuals that were not addressed so far. It led us to apply parallel matrix factorization. This technique improved significantly the accuracy of our model which was able to achieve an RMSE of 0.781. 

Future work will focus on applying new algorithms present in the recommenderlab package, such as user-based and item-based collaborative filtering. Also, we did not take into account all possible arrange of predictors present in the movielens data set, and further analysis of it will be done later. As an example, the timestamp and release year could be studied separately, and the genres split into single units.
	
## Technologies
Project is created with:
* R: 4.1.0
  * Libraries: Tidyverse, data.table, lubridate, stringr, ggrepel, kableExtra, caret, recosystem
  * Rmarkdown: 2.11
* RStudio: 1.4.1717
* Unix: OS version 18.7.0
* Git and Github
