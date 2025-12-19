# The energy, material and carbon handprint of lithium-ion batteries in electric vehicles - A US fleet model

Replication materials for Busch *et al.* (*submitted*). 

A comparative life cycle assessment (LCA) of the United States (US) projected light duty electric vehicle (EV) fleet (2025-2050) and a counter-factual scenario of an internal combustion engine vehicle (ICEV) fleet to determine environmental benefits enabled by LIBs and lithium. Metrics include are primary energy consumption, material extraction and greenhouse gas (GHG) emissions.

If you identify any error in the source code or have any further suggestions please contact Pablo Busch at pmbuschh@gmail.com.


# Organization

* **Inputs**: Data inputs used in the analysis. 
* **Parameters**: Intermediate Results needed to run Optimization or re-create figures.
* **Results**: Aggregated results stored to recreate tables and figures.
* **Scripts**: All code to process the data, run models and create figures. Each script starts with a description of the file purpose. Through the file there are several explanatory  comments.  
* **Figures**: Figures of the article main body. 

This GitHub contains organization notes in each folder describing the, and each scripts is properly docummented. Users can run all the code for replication using the scripts and data inputs. 

# Data Inputs

Data used to support the findings of this study were retrieved from the following resources available in the public domain: [GREET](https://greet.anl.gov/publications), [EIA Annual Energy Outlook 2025](https://www.eia.gov/outlooks/aeo/tables_ref.php), [NREL Cambium](https://www.nrel.gov/analysis/cambium) and [TEDB](https://tedb.ornl.gov/). Other data used to support the findings of this study are subject to third-party restrictions: ICCT roadmap, EV Volumes and ecoinvent 3.11. 

# Instructions

The repository is ~15Mb fully unzipped. Downloading and unzipping everything should take less than 5 minutes on a normal computer.

Users can run all the code for replication using the "USA-EV-Lithium-GHG.Rproj" file, or by setting their own working directory and running scripts independently.

This GitHub contains organization notes in each folder describing the, and each scripts is properly docummented.

# Software required

The script code was developed with **R** software version 4.5.1. 

The R code requires the following packages: *tidyverse*, *readr*,*readxl*,*ggplot2*,*data.table*,*dplyr*,*gridExtra*,*reshape2*,*scales*,*RColorBrewer*,*sf*,*ggrepel*. All libraries can be installed with the following command: 
```
install.packages(c("tidyverse","readr","readxl","ggplot2","data.table","dplyr","gridExtra","reshape2","scales","RColorBrewer","sf","ggrepel"), dependencies = T)
```

The model has only been tested using OS Windows 10 and 11, but it should work on Mac and Linux as well using **R**.

# License
This project is covered under the **MIT License**
