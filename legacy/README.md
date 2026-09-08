# The discussion-paper version

This folder holds the commuting zones of the 2020 RIETI discussion paper and the code that produced them. It is kept because the discussion paper points at it. The delineation in `delineation/` supersedes it, and the two are not interchangeable: they are built on different municipality units, at a different cutoff, and without the contiguity restriction that the current delineation imposes.

`codes/MASTER.R` reads a commuting matrix, forms the proportional-flow dissimilarity, runs average-linkage hierarchical clustering and cuts the tree at a height set at the top of the file. The discussion paper cut at 0.98, following Tolbert and Sizer (1996); the current delineation cuts at 0.977, the value the 2010 and 2020 United States delineations use.

`data/` holds the aggregated, anonymized commuting matrices that the KAKENHI project 15H05692 published, one per census year and municipality code universe. The municipality code crosswalks that `MASTER.R` also reads came from Municipality Map Maker, <http://tkirimura.com/mmm/>, and are not redistributed here.

`output/` holds one delineation per census year and code universe, named `<year>_<harmonized|original>.csv`, each a municipality code and its zone. `output/replication_by_tree_heights/` holds the same at other cutoffs. The harmonized codes of this version are the municipality units of 2015, not of 2020.
