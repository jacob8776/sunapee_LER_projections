# sunapee_LER_projections
Repository for creating projections of lake thermal metrics at Lake Sunapee using GCM/RCP scenarios from 2006 - 2099. These scripts were developed and run on R.4.0.2. 


To run this repository using premade projections and reproduce the results in the study "Uncertainty in projections of future lake thermal dynamics is differentially driven by global climate models and lake models", run the script "install.R" in the main sunapee_LER_projections repository and step through the scripts folder from steps 5-6. In order for steps 5-6 to be successfully run without completing steps 1-4, PLEASE RUN "s1-4_projection_outputs.R" in the "step_5_processing_projections" folder. 

To run this repository from scratch and reproduce the results in the study "Uncertainty in projections of future lake thermal dynamics is differentially driven by global climate models and lake models", run the script "install.R" in the main sunapee_LER_projections repository and then step through steps through the scripts folder from step 1-6. DO NOT RUN "s1-4_projection_outputs.R" in the "step_5_processing_projections" folder. This script will externally download projection files published on Zenodo. RUNNING STEPS 1-6 will take multiple days on a local computer with the speed varying from 
computer to computer.


Folders:
`install.R`\: Script with required libraries to run and analyze projections. A SUCCESSFUL RUN OF THIS SCRIPT IS REQUIRED
FOR ALL OTHER SCRIPTS.\
`LER_analysis`\: Folder containing ice on and off files to analyze calibration and validation periods.\
`LER_calibration`\: folder containing output from Latin Hypercube calibration of lake models (2005-2015). All required inputs for model configuration are also included in this folder.\
`LER_validation`\: folder containing output from  the validation period (2015-2020) for all lake models. all required inputs for model configuration are also included in this folder.\
`LER_inputs`\: Files used as the inputs while running meteorological analyses as well as long-term projections (1938-2099). 
This includes temperature profiles, meteorological data, and hypsography data.\
`LER_projections`\: Scaffolded structure with a universal .yaml configuration file and 4 GCM folders. 
Each GCM folder contains 3 RCP scenarios which are stepped into in order to deposit the appropriate RCP/GCM combination for 
all lake models within LakeEnsemblR.\
`anomaly_calculations`\: Folder where the projection anomalies are placed after calculation.\
`figures`\:Contains all figure output in the study. Does not include conceptual figures.\
`met_data_comparison`\:Contains ERA5 and EWEMBI data in order to check that these two datasets
fit well with each other and can be used in an interchangable method.\
`met_files_nc`\: Contains all decompressed .nc meteorological files for all GCMs. Each GCM's files
are separated by their own folder.\
`met_files_processed`\: Contains .csv meterological files for all GCMs. Each GCM's files are separated by their own
folder.\
`scripts`\: folder for R scripts needed to run this analysis. Each folder, or "step" contains
sub-steps (e.g., 1.1, 1.2), which must be run in order for this analysis to be successful.\
 &nbsp;&nbsp;`step_1_input_data`\: this folder includes scripts which format input data to be used in further steps.\
 &nbsp;&nbsp;`step_2_calibration`\: this folder contains scripts which carry out Latin Hypercube calibration of all lake models. The current scripts 2.1-2.5 only analyze currently used calibration. In order to run a new
 LHC calibration, uncomment after the "RUNNING CALIBRATION WILL CHANGE THE YAML FILE AND AFFECT PROJECTIONS."
 As stated in the scripts, changing calibration output will affect the projections and result in different figures
 from those in our study.\
 &nbsp;&nbsp; `step_3_validation`\: This folder contains the scripts used to run and analyze the validation from 2015-2020.\
 &nbsp;&nbsp; `step_4_running_projections`\: This folder contains the scripts used to run long term projections from 1938-2099. It also contains a script that moves the projections to an output folder and deletes the duplicates in their respective original places.\
 &nbsp;&nbsp; `step_5_processing_projections`\: This folder contains scripts required to process each metric's anomaly values from the projections. It also contains the script "s1-4_projection_outputs.R" which can be run to to download previously run projection output from Zenodo.\
 &nbsp;&nbsp; `step_6_study_results_figures`\: This folder contains the scripts that generate all of the figures and table 
 in the study (with the exception of conceptual figures). Each script is labeled with the corresponding figure or table 
 numbering in the study. 

