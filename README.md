## FMDV Preclinical

### Supporting Information  
This site provides supporting information for the manuscript:  
**Incubation phase transmission of foot-and-mouth disease virus in cattle: experimental evidence and simulated impacts**  
  
Stenfeldt, Humphreys, and Arzt     [*in review*]  

---

### Contents  
This repository is organized as an **RStudio** Project (see, *fmdv-preclinical.Rproj*)  
```
fmdv-preclinical/
├── assets/               # Executed data outputs from .qmd scripts
├── config/               # YAML configuration files for simulations
├── docs/                 # Quarto html outputs for website
├── empirical/            # Empirical results from animal experiment   
├── herd.qmd              # Annotated demo script for within-herd simulation
├── incubation.qmd        # Annotated script for phase duration models
├── network.qmd           # Annotated demo script for between-farm simulation
├── preprocess.qmd        # Annotated demo script for data prep and ANOVA analysis
├── rooms.qmd             # Annotated demo script for room-to-room simulation
├── shell_scripts/        # Example .sh and .R scripts for simulation
├── shiny/                # Shiny app demo for room-to-room transmission
├── R/                    # Functions used in analysis
└── README.md             # README
```
---
  
### Shiny App  
A simple shiny app is provided to demonstrate the **Room-to-Room** simulation.  
  
Run the app and click the *Run Experiment* button. The Room-to-Room simulation will run in the background in 10-15 seconds and plot results.  Each click of *Run Experiment* will generate a random seed to show varied results.  
```
shiny::runApp(here("shiny/app.R"))
```
        
### External Resources    
     
A webpage based version of the code and results are available at the following link:    
[https://geoepi.github.io/fmdv-preclinical/](https://geoepi.github.io/fmdv-preclinical/)  
  
A hosted version of the *Room-to-Room* simulation is available at [geoepi.shinyapps.io/fmdv-preclinical](https://geoepi.shinyapps.io/fmdv-preclinical/)  
  
Archives of versioned data and related supporting documents are available on the Open Science Framework (OSF):   
[FMDV Preclinical Transmission (BOV)](https://osf.io/qf2wr/)  
   
The manuscript describes simulations conducted with the [**challengeABM** R-package](https://github.com/geoepi/challengeABM)  


[Overview, Design concepts, Details](https://github.com/geoepi/challengeABM/blob/main/ODD_Decription.md) for simulation code   
  
  
   

