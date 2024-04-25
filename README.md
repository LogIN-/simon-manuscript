
<p align="center">
    <img src="http://unmaintained.tech/badge.svg">
</p>

## :warning: Deprecation Notice Please note that the SIMON platform has been officially deprecated and is now part of the PANDORA suite. We encourage all users to transition to PANDORA for future updates and continued support. For more details, visit [PANDORA's GitHub repository](https://github.com/genular/pandora).

<p align="center">
    <img src="https://raw.githubusercontent.com/LogIN-/simon-manuscript/master/images/icon.png"
      width="128"
      style="float:left;">
</p>

# SIMON manuscript source code

Here you can find the code and data from the `SIMON` publication. If you have any questions, please feel free to send [us an email](mailto:atomic.research.lab@gmail.com)

## General Usage Notes

### Description
`SIMON` (Sequential Iterative Modeling "OverNight") is a tool for pattern recognition and knowledge extraction from high-dimensional biological data.
[Publication](http://www.jimmunol.org/content/early/2019/06/13/jimmunol.1900033.abstract) for more details.


## Installation & Quickstart

### Dependencies
This project uses a number of open source projects to work properly ¯\\_(ツ)_/¯

To install and get this project running you will need basic knowledge of `Linux`, `bash`, `MySQL` and `R` (a free software environment for statistical computing and graphics). Please prepare Linux workstation with `R version 3.4.4` (2018-03-15) -- "Someone to Lean On" and MySQL preinstalled.

### Quickstart

```bash
export R_MAX_NUM_DLLS=1000

## STEP 1
# 1. Prepare MySQL database
# 2. Import database schema located in "./data/database.sql" file
# 3. Adjust MySQL credentials in "main.R" file DATABASE_CREDS variable
# 4. Download file containing data needed for processing and place it in ./data directory named as follows "data/data_transposed.csv"

## STEP 2
# 1. Make sure you can run main.R file and that all R packages needed are pre-installed (install machine learning R packages necessary for building models). Packages are defined in main.R file
# 2. RUN ANALYSIS with following command: "Rscript main.R"
```

### Analysis workflow

### Starting from the scratch

The influenza datasets were obtained from the `Stanford Data Miner` maintained by the `Human Immune Monitoring Center at Stanford University`. 
This included total of 177 csv files, which were automatically imported to the MySQL database to facilitate further analysis. 
The database, named `FluPRINT` and its source code, including the installation tutorial are freely available [here](https://github.com/LogIN-/fluprint)
and on project's [website](https://fluprint.com/#/about).
Following database installation, you can obtain data used in the `SIMON` publication by following MySQL database query:

```sql
SELECT donors.id                        AS donor_id,
       donor_visits.age                 AS age,
       donor_visits.vaccine_resp        AS outcome,
       experimental_data.name_formatted AS data_name,
       experimental_data.data           AS data
FROM   donors
       LEFT JOIN donor_visits
              ON donors.id = donor_visits.donor_id
                 AND donor_visits.visit_id = 1
       INNER JOIN experimental_data
               ON donor_visits.id = experimental_data.donor_visits_id
                  AND experimental_data.donor_id = donor_visits.donor_id
WHERE  donors.gender IS NOT NULL
       AND donor_visits.vaccine_resp IS NOT NULL
       AND donor_visits.vaccine = 4
ORDER  BY donors.study_donor_id DESC
```

### Starting from the initial dataset

For those with limited or no programming experience to install MySQL database, we recommend to start immediately with the initial dataset. 
The initial dataset used in the publication is published and avaliable for download here [![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.2578166.svg)](https://doi.org/10.5281/zenodo.2578166)

### Generation of datasets using the mulset algorithm

In the publication, we developed a novel approach to deal with missing data based on finding multi-set intersections. 
The `mulset` algorithm is implemented in the first step of `SIMON` to identify features shared across donors and generate datasets containing all possible combinations of features and donors across the entire initial dataset. Additionally, the `mulset` algorithm is available as an R package in CRAN repository and an open source code, including the installation instructions are available [here](https://github.com/LogIN-/mulset). 

### SIMON - Sequential Iterative Modeling "Overnight"

The pseudocode explains all the steps of the `SIMON` analysis:

```bash
% Step 1: generate re-sampled intersection datasets suitable for analysis 
for {each subject in data} do: 
    Calculate intersection between subject and all other subjects using mulset algorithm 
    Skip sets that have less than 5 features and less than 15 donors in common 
end for; 
# Save all shared intersections to corresponding datasets 
    
% Step 2: automated machine learning 
avaliableModels – install machine learning R packages necessary for building models (128 ML algorithms described in the manuscript, Supplementary Table S6) 

for {dataset in sets} do: 
    Create balanced partitioning of the data 
    data: 75% training, 25% test 
    Skip dataset if test set has less than 10 subjects 
    
    for {model in avaliableModels} do: 
        Perform model training and get all model performance variables 
        Using test data make predictions on the trained model, retrieve ROC from confusion matrix 
        Using trained model calculate variable importance score 
        Save all data metrics to corresponding fields in the database 
    end for; 
end for;
```
### SIMON output data


* Data obtained after datasets generation step using the `mulset` algorithm and data partitioning function is published here [![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.2580414.svg)](https://doi.org/10.5281/zenodo.2580414)

* To reproduce results from the publication, you must use models that were built by `SIMON`. All models are published here [![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.2580416.svg)](https://doi.org/10.5281/zenodo.2580416)

* Exploratory analysis is available freely at [project's website](https://www.fluprint.com). Since [the website](https://www.fluprint.com) is available as an [open source project](https://github.com/LogIN-/fluprint.com), one can easily reproduce the data from the exploratory analysis using [data provided](https://raw.githubusercontent.com/LogIN-/fluprint.com/master/static/data.json) in the repository.

## License
See our `LICENSE` file.


## Citation
This software can be used for research purposes, you should cite following publication:

```
    Adriana Tomic, Ivan Tomic, Yael Rosenberg-Hasson, Cornelia L. Dekker, Holden T. Maecker, Mark M. Davis.
    SIMON, an Automated Machine Learning System, Reveals Immune Signatures of Influenza Vaccine Responses
    http://www.jimmunol.org/content/early/2019/06/13/jimmunol.1900033.abstract
    doi: 10.4049/jimmunol.1900033
    
    Adriana Tomic, Ivan Tomic, Levi Waldron, Ludwig Geistlinger, Max Kuhn, Rachel L. Spreng, Lindsay C. Dahora, Kelly E. Seaton, Georgia Tomaras, Jennifer Hill, Niharika A. Duggal, Ross D. Pollock, Norman R. Lazarus, Stephen D.R. Harridge, Janet M. Lord, Purvesh Khatri, Andrew J. Pollard, Mark M. Davis.
    SIMON: Open-Source Knowledge Discovery Platform
    https://www.cell.com/patterns/fulltext/S2666-3899(20)30242-7
    doi:10.1016/j.patter.2020.100178    
```

## :exclamation: Important notice
SIMON is now part of PANDORA! In the continuing evolution of this project, SIMON has been integrated into the PANDORA platform to enhance our commitment to providing a user-friendly knowledge discovery platform. This transition aims to offer a more comprehensive suite of tools for our users. For the latest updates and features, please visit our new project page on [PANDORA's GitHub repository](https://github.com/genular/pandora).
