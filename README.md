# Commuting zones for Japan, 1980 to 2020

This repository releases a delineation of commuting zones for Japan and the code that produces it. A commuting zone is a group of municipalities within which most residents work and out of which few commute. The zones are built from the commuting flows recorded in every Population Census from 1980 to 2020 by hierarchical cluster analysis on the proportional-flow dissimilarity of Tolbert and Sizer (1996), with merges restricted to municipalities that adjoin on the ground. They are mutually exclusive and exhaustive of the national territory.

The delineation and this code accompany the data descriptor Adachi, Fukai, Kawaguchi and Saito, *Commuting zones in Japan*.

## What to read first

`delineation/` holds the released tables and a README that describes every column. That folder is what most users need. `harmonized.csv` carries one delineation per census year, all built on the municipality units in force on 1 October 2020, and suits a panel. `original.csv` carries one delineation per census year on the units in force at that census date, and suits a cross-section of a single year.

## Layout

| Folder | What is in it |
| --- | --- |
| `delineation/` | The released concordances, the wide crosswalk behind them, and their documentation |
| `code/` | Every step from the commuting matrices to the delineation, and to the paper's figures and tables |
| `data/` | The inputs the repository can carry, and a README naming the inputs it cannot and where to obtain them |
| `paper/` | The figures, table fragments and statistics the paper reports |
| `legacy/` | The 2020 discussion-paper version: its delineations and the code that produced them |

## Reproducing the delineation

The census commuting matrices and the municipality boundary layer are not in this repository. `data/README.md` says what they are and where each comes from. Point `CZ_SHARED_ROOT` at the folder holding them, then run the scripts in `code/` in the order `code/README.md` gives. Everything runs in R, apart from four Python scripts that fetch and reshape external tables.

## Citing

Please cite the data descriptor when you use the delineation. If you use the discussion-paper version under `legacy/`, cite the discussion paper instead, and note that the two delineations are built on different municipality units and are not interchangeable.
