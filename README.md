
# GenreDiscourseAnalysis

<!-- badges: start -->
<!-- badges: end -->

The question of the "correct" genre classification of popular music can lead to heated discussions among music fans, music journalists, and musicologists. Nevertheless, empirical studies of music preferences based on self-reported information often assume a uniform understanding among all respondents. In music psychology and music sociology, this assumption often leads to discussions about validity, replication problems, and the inability to compare studies. However, cultural studies research on popular music emphasizes the context- and perspective-dependent attribution of genres as a central phenomenon of everyday and historical discourse on popular music. So why not make a virtue out of necessity? Using genre classifications from different online music communities for 231,370 popular music titles listened to in Germany in recent decades, we present a method for creating a hierarchical genre family tree based on network analysis. 

The `GenreDiscourseAnalysis` repository provides the necessary functions to analyze and visualize genre discourse. It includes tools for data cleaning, network analysis, and visualization of genre relationships.

## Installation

Please clone this repository to your local machine using:
```
git clone "https://github.com/markusradke/GenreDiscourseAnalysis.git"
```
Please make sure you have R installed on your machine and restore the renv environment using (make sure you are in the project directory):
```R
# install renv if you haven't already with install.packages("renv")s
renv::restore()
```

## Structure and Data
All necessary data is included in the 'data' folder. The main scripts for analysis are located in the 'analysis' folder. The 'reports' folder contains the final reports, tables and visualizations.

## Replicate the analysis

To replicate the analysis, please run the file ['scripts/run_full_replication.R'](scripts/run_full_replication.R) in the 'scripts' folder. This script will run all necessary steps to reproduce the results presented in our study.

## License
This project is licensed under the CC BY 4.0 License - see the LICENSE file for details.

## Contact the author
Markus Radke

[markus.radke.1@campus.tu-berlin.de](mailto::markus.radke.1@campus.tu-berlin.de)

## Status
This project is currently in active development. Please check back for updates and new features.