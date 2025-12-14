# TBI-outcomes

## 1. Setup

This was performed using **R version 4.5.2 (2025-10-31)** and the following R packages:

<pre>required_packages <- c(
  "tidyverse",
  "dplyr",
  "stringr",
  "readr",
  "MatchIt",
  "cobalt",
  "sandwich",
  "lmtest"
)</pre>

Install any missing packages by running:

<pre>install.packages(
  setdiff(required_packages, rownames(installed.packages()))
)</pre>

## 2. Clean and organize data

Make sure your data files are in a directory named **TQIP 2007-2023**; for example, the relevant CSV files for year 2023 should be located at **./TQIP 2007-2023/PUF AY 2023/CSV/**.

We'll process years 2018-2023 using the following:

We'll run the followin

<pre>Rscript PUF_process.R 2023
Rscrnipt </pre>

The output will be stored in a directory named **final_data**

## 3. Merge the data

