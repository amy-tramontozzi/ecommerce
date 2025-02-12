# E-commerce Clustering and Revenue Estimation

This project demonstrates the application of K-means clustering to 33,000 records ofe-commerce customer data and estimates the business impact of targeted marketing based on customer segments.

## Overview

- **Dataset**: The project uses a dataset of e-commerce customers stored in the `ecommerce.xlsx` file.
- **Goal**: To apply K-means clustering to group customers based on their purchasing behavior and estimate the business impact of marketing efforts targeting each cluster.
  
- **Method**: 
  - Clustering using K-means.
  - Normalization of features.
  - Estimation of incremental revenue from marketing efforts targeting each cluster.

## Prerequisites

Make sure to install and load the required libraries:
```r
install.packages("readxl")
install.packages("stats")
install.packages("tidyverse")
