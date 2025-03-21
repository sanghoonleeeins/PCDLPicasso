---
title: "PCDLPicasso"
author: "SanghoonLee"
format: pdf
editor: visual
date: 2025-02-26
---

## Plasma Cell Disorder Library - Picasso (PCDLPicasso)

This ShinyApp will help users search for available lab sequencing datasets and clinical data. Explore available lab seq datasets and count patient samples per clinical data type

**Plasma Cell Disorder Library - Picasso** was built on R version 4.4.1 (2024-06-15) -- "**Race for Your Life**" This means you need to install R version \>= 4.4.1

## A. Introduction

The lab has so many sequencing datasets. So, we need a tool to navigate the datasets and count the number of patient samples per disease type or sequencing type.

### $List of Morgan-Davies lab datasets$ - These names will be revised by unique names.

|  No.1 UK myeloma (463) exome data - Patrick\
|  No.2 MGP data exomes and expression (1273) - Patrick\
|  No.3 GEP datasets Little rock (?) - Patrick\
|  No.4 SMM sequential WGS (Eileen)\
|  No.5 WGS Ancestry/ncPaper (307-294) - Patrick\
|  No.6 WGS next set - Sanghoon\
|  No.7 COMMPASS low pass WGS (794?)\
|  No.8 COMMPASS exomes\
|  No.9 COMMPASS expression\
|  No.10 WMMumtiome_Katahdin - Dylan\
|  No.11 MM cell line ATAC, HiC - Patrick\
|  No.12 PDX mice HiC, RNAseq, - Patrick\
|  No.13 scATAC, scRNA, WGS (MM, SMM, MGUS) - Di\
|  No.14 micropinocytosis resistant cells RNA - Dylan\
|  No.15 NSD2 dTAG cut and tag; SLAMseq - Sanxiong\
|  No.16 Cody structural paper - Patrick\
|  No.17 Chromosome 1 paper Eileen - Patrick\
|  No.18 dbMP - Patrick\
|  No.19 External Datasets – Patrick\
|  No.20 scDatasets – Dylan\
|  No.21 Foundation 1 data/Caleb paper\

## B. Basic requirements

### Step1. Download R and Rstudio, and install them.

You can just Google or Youtube for "R download and install" and "Rstudio download" to complete this step. It is not that difficult. [Youtube clip for Mac](https://www.youtube.com/watch?v=I5WIMX4LK8M)

If you have some experience already, you can go install [R from CRAN](https://cran.r-project.org/)

You can install a user-friendly interface, R-Studio, from [rstudio.com](https://www.rstudio.com/products/rstudio/download/)

### Step2. Start Rstudio

What does 'start Rstudio' mean? Visit the Youtube lecture I introduced in Step1.

### Step3. In the R console, install necessary R packages first.

If you have some experience already, just go below the code line. Just copy the lines, paste them to console, and hit enter key.

If you don't much experience yet; What are 'R console' and 'R package'? Try to what this 3min [Youtube lecture](https://www.youtube.com/watch?v=BLpbGlYzUiQ) Or, you can find more in Youtube by searching "How to install R packages?"

```{r}
utils::install.packages("pacman", repos="http://cran.us.r-project.org")
library(pacman)

### Install and load multiple packages at a time.  
### This will take 10~15 min if you are installing any packages for the first time.
utils::install.packages(c("data.table","BiocManager","dplyr","stringr","tidyverse",
  "EnvStats","ggbeeswarm","htmltools","ggplot2","ggpubr","purrr","rstatix","shiny",
  "plotrix","shinyWidgets","shinycssloaders","DT"), repos="http://cran.us.r-project.org")
pacman::p_load(devtools,usethis)
```

![image](https://github.com/user-attachments/assets/b01e6d03-491e-4a01-9eab-71c279c5c756)


## C. Install PCDLPicasso R package

### Step4. Install PCDLPicasso R package and load it

```{r}
### Install PCDLPicasso R package
install_github("sanghoonleeeins/PCDLPicasso")

### Load the R package. Note you DON'T need quotation. 
### The package name is PCDLPicasso, not Sanghoon-Lee_NYULH/PCDLPicasso
library(PCDLPicasso)
```

If you get an error like this below, I can help you solve it.

WARNING: Rtools is required to build R packages, but is not currently installed.

![image](https://github.com/user-attachments/assets/a621a1f0-aeca-4903-823f-b55abb1e0d2e)


## D. Play with PCDLPicasso

### Step5. Run ShinyApp and Play

In R console, run the command below.

```{r}
shiny::shinyApp(ui=PCDD_UserInterface, server=PCDD_Server)
```

![image](https://github.com/user-attachments/assets/f56acbba-f7df-45a0-bbc5-ade6fe580acc)

![image](https://github.com/user-attachments/assets/24e0ba4e-68ef-406c-b5d4-f23c2a721a62)
