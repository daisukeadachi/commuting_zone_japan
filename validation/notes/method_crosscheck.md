# The delineation formula across four implementations

Issue #11 asks for this comparison before any table or figure is produced. The four columns are the original method as described in the source paper and its replication code, the 2016 revision from which the cutoff of 0.977 comes, the source paper's own code, and the existing Japanese pipeline. The original and the 2016 revision are not read directly here; what is recorded for them is what the source paper and its code state about them, at <https://doi.org/10.1038/s41597-024-03829-5> and <https://github.com/csfowler/CommutingZones2020>.

## The formula

For municipalities i and j, write the directed commuting flows as f_ij and f_ji and the resident labour forces as W_i and W_j. Every implementation computes the proportional flow as the sum of the two directed flows over the smaller of the two resident labour forces, converts it to a dissimilarity by subtracting from one, and sets the diagonal to zero.

| | Original method | 2016 revision | Source paper's code | Existing Japanese pipeline |
| --- | --- | --- | --- | --- |
| Numerator | f_ij + f_ji | f_ij + f_ji | f_ij + f_ji | f_ij only, when f_ij is present in the flow file |
| Denominator | min(W_i, W_j) | min(W_i, W_j) | min(W_i, W_j), computed as the row sum of the flow matrix | min(W_i, W_j), taken from the reported resident labour force |
| Values at or above one | reduced to 0.999 | reduced to 0.999 | reduced to 0.999 whenever the value reaches 0.999 | dissimilarity floored at 0.001 |
| Diagonal | zero | zero | zero | zero |
| Linkage | average | average | average | average |
| Cutoff | not fixed | 0.977 | 0.977 | 0.980 |

The replication divides by the row sum over every destination the census records, which is the quantity the source paper's code computes, and `validation/notes/denominator.md` records the reasoning and what a wider count of who is working would do.

Three of the four rows raise nothing. The floor of 0.001 on the dissimilarity is algebraically the same as the cap of 0.999 on the proportional flow, so the Japanese pipeline and the source paper's code agree exactly there; the paper's prose says only that values above one are reduced, while its code reduces anything from 0.999 upward, and the difference concerns proportional flows in the half-open interval from 0.999 to 1, which no pair in the Japanese data reaches other than through the cap itself. The denominator agrees as well: the reported resident labour force equals the row sum of the flow matrix exactly, in every census year from 1980 to 2020, so the two ways of computing it are the same number.

## The numerator does not agree

The existing Japanese pipeline builds the bilateral flow table by joining the flow file to its own transpose on the pair (i, j). The join is a left join onto the rows of the untransposed table, so a pair survives only when the flow from i to j appears in the file. When i sends no recorded commuters to j but j sends commuters to i, the pair is absent from the joined table, is filled with zero when the table is expanded to the full grid, and enters the dissimilarity matrix as one, meaning no connection at all. The reverse flow is discarded.

The resulting matrix is not symmetric. The cell for (i, j) keeps the reverse flow while the cell for (j, i) does not, or the other way round, depending on which direction the file records. Clustering then reads the matrix through `as.dist`, which retains the lower triangle, so for about half of the affected pairs the recorded commuting link is erased before the tree is built.

The size of the effect, measured on the harmonized municipality universe of the existing pipeline, which counts the twenty-three special wards of Tokyo as twenty-three units, with the "mainly working" matrices. The replication merges those wards, so its own delineation is not the one tabulated here; the point of the table is what the asymmetry does to the pipeline that carries it.

| Year | Pairs entering the clustering | Pairs altered | Share | Largest discarded proportional flow | Commuting zones at 0.980, corrected | Existing pipeline | Municipalities changing zone |
| --- | --- | --- | --- | --- | --- | --- | --- |
| 1980 | 1,514,670 | 18,804 | 1.24% | 0.081 | 401 | 405 | 83 |
| 1985 | 1,514,670 | 28,297 | 1.87% | 0.100 | 367 | 367 | 0 |
| 1990 | 1,514,670 | 29,759 | 1.97% | 0.129 | 333 | 334 | 2 |
| 1995 | 1,514,670 | 32,791 | 2.17% | 0.216 | 315 | 315 | 108 |
| 2000 | 1,512,930 | 32,728 | 2.16% | 0.198 | 289 | 292 | 45 |
| 2005 | 1,514,670 | 35,346 | 2.33% | 0.077 | 278 | 277 | 11 |
| 2010 | 1,514,670 | 30,413 | 2.01% | 0.103 | 265 | 267 | 27 |
| 2015 | 1,505,980 | 66,406 | 4.41% | 0.999 | 263 | 265 | 18 |
| 2020 | 1,512,930 | 38,365 | 2.54% | 0.374 | 257 | 258 | 86 |

Most discarded flows are small: the median is below 0.0002 of the smaller resident labour force in every year, and the ninetieth percentile below 0.0008. The tail is what moves the delineation. Between thirty and eighty pairs a year lose more than 0.02, which is a fifth of the distance between the cutoff and complete disconnection, and the year in which the largest single discarded flow reaches 0.999 is 2015. The count of commuting zones moves by up to four and the number of municipalities whose zone membership changes reaches 108.

The replication uses the symmetric numerator, f_ij + f_ji in both cells, which is what the original method, the 2016 revision and the source paper all specify. Every delineation reported here therefore differs from the published Japanese delineation by more than the change of sample and cutoff, and the table above is the measure of that difference.

## Cutoff

The existing Japanese pipeline cuts at 0.980 and the source paper at 0.977. Neither value is defended on its own terms in the sources; the 0.977 was chosen in the 2016 revision to approximate the 1990 delineation and then carried forward. This replication takes both as anchors and sweeps the later year of each comparison pair around them, which is the design recorded in issue #11.
