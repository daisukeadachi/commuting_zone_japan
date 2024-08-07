# Commuting Zones in Japan

- Initial version: 29 Apr, 2019. Last updated: 19 Sep, 2022.
- This repository contains the CZ delineation, source codes and raw data based on [Adachi, Fukai, Kawaguchi, and Saito (2020)](https://daisukeadachi.github.io/assets/papers/commuting_zones_rietidp.pdf).

# `output` folder

- This folder contains the files of CZ delineations.



- Each file is an output of hierarchical agglomerative clustering (HAC) algorithm with an input of aggregated Population Census data in a year `(yyyy)` with municipality codes `(mcodes)`, either `original` or `harmonized`. 
  - Thus, they follow a naming convention `(yyyy)_(mcodes).csv`.



- Details in municipality codes `(mcodes)`:
  -  `original` means that the raw municipality codes in current census years are used to perform clustering. If you have data at the level of current-year municipality codes, delineation with `original` codes is relevant.
  - `harmonized`  means that the municipality codes are harmonized across census years and then cluster municipalities according to current commuting flows each year. The harmonized codes are based on 2015 municipality codes. This procedure controls the [changes of administrative municipalities](https://en.wikipedia.org/wiki/Municipal_mergers_and_dissolutions_in_Japan) and focuses on the actual evolution of commuting patterns over the years. If your focus is to analyze such patterns, delineation with `harmonized` codes is relevant.



- In `replication_by_tree_heights` sub-folder, results of other configurations of "tree heights" (see the paper for the detail) are stored. One can replicate them using the shared code in the `codes` folder.
  - Our preferred choice of tree height is 0.98, following [Tolbert and Sizer (1996)](https://ageconsearch.umn.edu/record/278812/). One can replicate with this tree height.
  - However, CZ delineations users can replicate are different from ones in our paper published in this `output` folder. See "important note" below for detail.

# `codes` folder

- This folder contains R source code, `MASTER.R`. After reading raw data, it creates distance matrices, performs HAC algorithm and, generates files of CZ delineation.
- To replicate, one should first create `data` folder and put commuting matrix in that folder. Then
  1. Open the `commuting_zone_japan.Rproj` in the mother folder.
  2. Run `MASTER.R`
- The output is in the `output/replicate_by_tree_heights` folder (replace the existing files).

# `data` folder

- This folder contains data needed to run the codes above. 
  - CSV files are aggregated and annonymized commuting flow matrices that are produced and published by KAKENHI project [15H05692](https://kaken.nii.ac.jp/en/grant/KAKENHI-PROJECT-15H05692/).
  - In sub-folder `mmm` are the municipality concordance files produced in [Municipality Map Maker](http://www.tkirimura.com/mmm/).