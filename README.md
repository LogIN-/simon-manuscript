<h1>

<img src="icon.png"
  width="128"
  style="float:left;"> \# SIMON manuscript source code
</h1>

<p align="center">
    <img src="http://unmaintained.tech/badge.svg">
</p>

Here you can find the code and data from the `SIMON` publication.
If you have any questions, please feel free to [contact Adriana Tomic](mailto:atomic@stanford.edu).

### General Usage Notes

#### Description
`SIMON` (Sequential Iterative Modeling "OverNight") is a tool for pattern recognition and knowledge extraction from high-dimensional biological data.
Check out [our preprint](https://www.biorxiv.org/content/10.1101/545186v1) for more details.


### Installation & Quickstart

#### Dependencies
This project uses a number of open source projects to work properly ¯\\_(ツ)_/¯

To install and get this project running you will need basic knowledge of `Linux`, `bash`, `MySQL` and `R` (a free software environment for statistical computing and graphics). Please prepare Linux workstation with `R version 3.4.4` (2018-03-15) -- "Someone to Lean On" and MySQL preinstalled.

#### Quickstart

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

#### Starting from the scratch

The influenza datasets were obtained from the `Stanford Data Miner` maintained by the `Human Immune Monitoring Center at Stanford University`. 
This included total of 177 csv files, which were automatically imported to the MySQL database to facilitate further analysis. 
The database, named `FluPRINT` and its source code, including the installation tutorial are freely available [here](https://github.com/LogIN-/fluprint)
and on project’s [website](www.fluprint.com).
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

#### Starting from the initial dataset

For those with limited or no programming experience to install MySQL database, we recommend to start immediately with the initial dataset. 
The initial dataset used in the publication is available at [Zenodo](https://zenodo.org/record/2578166#.XHWDibh7lPY) for download.

#### Generation of datasets using the mulset algorithm

In the publication, we developed a novel approach to deal with missing data based on finding multi-set intersections. 
The `mulset` algorithm is implemented in the first step of `SIMON` to identify features shared across donors and generate datasets containing all possible combinations of features and donors across the entire initial dataset. Additionally, the `mulset` algorithm is available as an R package in CRAN repository and an open source code, including the installation instructions are available [here](https://github.com/LogIN-/mulset). 

#### SIMON - Sequential Iterative Modeling "Overnight"

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
#### SIMON output data


* Data obtained after datasets generation step using the `mulset` algorithm and data partitioning function is available for download from [Zenodo](https://zenodo.org/record/2580414#.XHh1drh7lPY).

* To reproduce results from the publication, you must use models that were built by `SIMON`. All models are available at [Zenodo](https://zenodo.org/record/2580416#.XHiItLh7lPY) for direct download.

* Exploratory analysis is available freely at [project's website](www.fluprint.com). Since [the website](https://www.fluprint.com) is available as an [open source project](https://github.com/LogIN-/fluprint.com), one can easily reproduce the data from the exploratory analysis using [data provided](https://raw.githubusercontent.com/LogIN-/fluprint.com/master/static/data.json) in the repository.

### License
See our LICENSE file.


## Citation
This software can be used for research purposes, you should cite following publication:

```
@article {Tomic545186,
    author = {Tomic, Adriana and Tomic, Ivan and Rosenberg-Hasson, Yael and Dekker, Cornelia L. and Maecker, Holden T. and Davis, Mark M.},
    title = {SIMON, an automated machine learning system reveals immune signatures of influenza vaccine responses},
    elocation-id = {545186},
    year = {2019},
    doi = {10.1101/545186},
    publisher = {Cold Spring Harbor Laboratory},
    abstract = {Machine learning holds considerable promise for understanding complex biological processes such as vaccine responses. Capturing interindividual variability is essential to increase the statistical power necessary for building more accurate predictive models. However, available approaches have difficulty coping with incomplete datasets which is often the case when combining studies. Additionally, there are hundreds of algorithms available and no simple way to find the optimal one. Here, we developed Sequential Iterative Modelling "OverNight" or SIMON, an automated machine learning system that compares results from 128 different algorithms and is particularly suitable for datasets containing many missing values. We applied SIMON to data from five clinical studies of seasonal influenza vaccination. The results reveal previously unrecognized CD4+ and CD8+ T cell subsets strongly associated with a robust antibody response to influenza antigens. These results demonstrate that SIMON can greatly speed up the choice of analysis modalities. Hence, it is a highly useful approach for data-driven hypothesis generation from disparate clinical datasets. Our strategy could be used to gain biological insight from ever-expanding heterogeneous datasets that are publicly available.},
    URL = {https://www.biorxiv.org/content/early/2019/02/10/545186},
    eprint = {https://www.biorxiv.org/content/early/2019/02/10/545186.full.pdf},
    journal = {bioRxiv}
}
```

## :exclamation: Important notice
Since original author’s work, project has developed and moved on from its original, in direction to bring ease of use knowledge discovery platform to more general public. 
Please find new derived open source software here on this [link](https://github.com/genular/simon-frontend).
