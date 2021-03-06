# Supplement to Praet et al. (2022) [![DOI](https://zenodo.org/badge/441716761.svg)](https://zenodo.org/badge/latestdoi/441716761)


This repository holds the data and R jupyter notebook used in the analysis of sediment cores from Sklilak Lake, Alaska. Supplement to the research article in Sedimentology titled "Unravelling a 2300 year long sedimentary record of megathrust and intraslab earthquakes in proglacial Skilak Lake, south-central Alaska" by Praet et al. (2022). doi: [10.1111/sed.12986](https://doi.org/10.1111/sed.12986).


This analysis can be reproduced on your local machine by cloning this github repository and setting up the R environment using the [conda package manager](https://conda.io). You can then run the R script in the detectoutliers.ipynb by opening jupyter notebook.

```bash
git clone https://github.com/tim-collart/Praet_et_al_2022.git
cd Praet_et_al_2022
conda env create -f environment.yml
conda activate praetetal2022
jupyter notebook
```

Alternatively, you reproduce the analysis in an interactive online binder notebook [![Binder](https://mybinder.org/badge_logo.svg)](https://mybinder.org/v2/gh/tim-collart/Praet_et_al_2022/HEAD?urlpath=lab%2Ftree%2Fdetectoutliers.ipynb)

