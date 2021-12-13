# Tracking excess mortality in Switzerland :ch:, Sweden :se: & Spain :es:.  

## Citations and links

### Paper

Data & code to reproduce paper:  

> ADD CITATION

Code version [2.1]().

Archived on [![DOI](https://zenodo.org/badge/341273510.svg)](https://zenodo.org/badge/latestdoi/341273510)  

### Preprint 

Earlier version of preprint:  

> Staub K*, Panczak R*, Matthes KL, Floris J, Berlin C, Junker C, Weitkunat R, Mamelund SE, Egger M, Zwahlen M*, Riou J* (2021) Pandemic excess mortality in Spain, Sweden, and Switzerland during the COVID-19 pandemic in 2020 was at its highest since 1918. *medRxiv* 2021.08.12.21261825. doi: https://doi.org/10.1101/2021.08.12.21261825  
> <sub>* Equal contribution.</sub>

Code version [1.0](https://github.com/RPanczak/ISPM_excess-mortality/releases/tag/v1.0).

Archived on [![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.5206521.svg)](https://doi.org/10.5281/zenodo.5206521)  

### Online documentation

Details of the data sources, data preparation and main as well as supplementary analyses are available [on the project pages](https://rpanczak.github.io/ISPM_excess-mortality/). 

Visual summary of repo content [here](https://octo-repo-visualization.vercel.app/?repo=RPanczak%2FISPM_excess-mortality). 


## Content of this repository

### Structure

```
.
+-- analyses
+-- data
+-- data-raw
+-- docs
+-- paper
|   \-- supplementary
+-- R
+-- stan
\-- ubelix
```

### `analyses` folder 

This folder contains set of literate programming R scripts written either in R or in`R` flavoured `markdown`.  The files are numbered sequentially in order in which they should be executed. The key parts of this folder include:  

  - `01_data-prepare.Rmd` this script download, prepares data from *The Human Mortality Database* and also ancillary, country-specific datasets containing updates of monthly death counts, yearly age-specific death counts and population figures from statistical agencies. The output of these analyses is presented [here](https://rpanczak.github.io/ISPM_excess-mortality/01_data-prepare.html).  
  - `02_BfS-data-weekly.Rmd` this script prepares and compares weekly data on death counts for Switzerland from two sources: *Short-term Mortality Fluctuations* and *Swiss Federal Statistical Office*   
  - `03_joint-model-cmdstan.R` this script runs the main analyses of the paper; it sources functions from `R` folder which in turn source and compile Stan programs from `stan` directory. One main model is fitted and then three sensitivity analyses performed for each country and analysis year combination. The script can be run from command line with a parameter in range `1-3` to obtain country specific results - examples of such calls can be found in `ubelix` folder scripts; results will be stored in folder `outputs_YYYY-MM-DD` in `data directory. Note that depending on the architecture these analyses might take many hours or days to finish and we strongly recommend running them in appropriate computing environment - server, cloud or HPC.  
  -`04_joint-model-boot.R` this script performs additional sensitivity analysis; due to lower computational demands of this analyses they can be performed reasonably fast on a decently equipped laptop.  
  - `05_joint-combined-means.Rmd` this script combines results obtained by running scripts `03` and `04` and prepares outputs (figures and tables) for the paper and supplement. The output of these analyses is presented [here](https://rpanczak.github.io/ISPM_excess-mortality/05_joint-combined-means.html)
  - `06_comparison_week_month.Rmd` this script performs additional sensitivity analysis comparing estimates from weekly models to monthly models.  

### `data-raw` & `data` folders

Raw, unprocessed data are stored and accessed from the `data-raw` folder; prepared data are stored in `data`.  Both of these folders contain subdirectories with names of the data source, for instance `mortality_org` folder stores data from the HMD whereas `BfS` folder stores data from Swiss Federal Statistical Office [Bundesamt für Statistik (BfS)]. `outputs_YYYY-MM-DD` folders in `data` directory hold results of the analyses runs and can be reused in order to save the computational time.    

### `docs` folder

This folder stores `html` files generated from the scripts in the `analyses` folder, with the names equivalent to the scripts that are used to generate them. These files are used on the [project pages](https://rpanczak.github.io/ISPM_excess-mortality/).  

### `paper` folder

This folder stores figures and tables presented in the paper and in the supplementary materials.  

### `R` folder

This folder stores various scripts used for data manipulation or analyses by the scripts from the `analyses` folder. Most important scripts include:  

- `fn_age_serfling_nb_cmdstan.R` used for the main analyses (age adjusted, negative binomial model implemented in Stan)  
- `fn_global_serfling_nb_cmdstan.R` used for the sensitivity analyses (unadjusted, negative binomial model implemented in Stan)  
- `fn_global_serfling.R` and `fn_boot_pi.R` used for sensitivity analyses (unadjusted Poisson model)  

### `stan` folder

Scripts from `R` script above make calls to Stan programs stored in this directory. Names of this programs are matching the scripts calling them (`age_serfling_nb.stan` & 
`global_serfling_nb.stan`). Both of the scripts have versions with `21` in the name of the file which are special cases of each script designed to handle incomplete 2021 data. Note that the programs will be compiled on their first run and will create additional files with the same name and extensions depending on the platform (for instance `.exe` on Windows OS; shell scripts on *NIX systems).  

### `ubelix` folder

This folder stores example OpenPBS scripts that can be used to submit the `analyses` scripts to HPC cluster.  

## Computing environemtn

Details on the version of R software and packages used are presented at the bottom of the output documents stored in `docs` folder. Script `03_joint-model-cmdstan.R` was run on HPC cluster using `R` version *4.1.0* with packages versions `cmdstanr` *0.4.0* & `cmdstan` *2.27.0*.  

## Visual summary of main results

### Estimates of excess

Yearly differences

![Yearly differences](https://github.com/RPanczak/ISPM_excess-mortality/blob/main/paper/Figure_1.png)

Monthly differences

![Monthly differences](https://github.com/RPanczak/ISPM_excess-mortality/blob/main/paper/Figure_S2b.png)

Four pandemics

![Four pandemics](https://github.com/RPanczak/ISPM_excess-mortality/blob/main/paper/Figure_2.png)

![Four pandemics - percent](https://github.com/RPanczak/ISPM_excess-mortality/blob/main/paper/Figure_S3b.png)

Age effects

![Four pandemics](https://github.com/RPanczak/ISPM_excess-mortality/blob/main/paper/Figure_3.png)
 
