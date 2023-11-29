# eDITH_IBCH
Code supporting "Measuring the state of aquatic environments using eDNA — upscaling spatial resolution of biotic indices" by Blackman, Carraro, Keck &amp; Altermatt

## Content

- "run_eDITH.R": extracts the river network, prepares input for the eDITH model and runs the eDITH model for the 56 taxa found in the eDNA dataset.
- "analyze_data.R": produces synthetized output of the ensemble of 56 eDITH models; reads and processes kick-net data, measured IBCH data and river network status data; performs comparative analyses and produces manuscript figures.
- "data": folder containing original datasets.
- "results": folder containing synthetized output of the 56 eDITH models. If run_eDITH is executed, extensive results for each model run is saved here.
- "support": folder containing support functions and intermediate data structures to speed up execution of main scripts "run_eDITH.R" and "analyze_data.R".
