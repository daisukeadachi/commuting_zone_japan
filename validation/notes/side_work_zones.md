# Commuting zones for the people who work alongside housework

The delineation is built on the flows of residents whose labour-force status is "mainly working". The census records about eight million people whose status is instead work alongside housework, and their commuting is not the same commuting. In 2015, 82 percent of them worked in the municipality they lived in, against 60 percent of the mainly working; in 1980 the two figures were 93 and 72 percent. The gap has been roughly constant for forty years while both have fallen. A delineation built on their flows is therefore a different object, and this records what it looks like.

`validation/code/build_side_work_flows.R` builds their commuting matrix from the attribute-level census tabulation, in the layout the pipeline already reads. `validation/code/make_numerator_comparison.R` compares the delineations and writes `validation/output/numerator_comparison.csv`. Both reach 1980 to 2015, the years the tabulation covers.

## Two ways to divide

The proportional flow divides the flows by a count of workers, and once the numerator changes there are two defensible counts. Dividing by the baseline workforce, the residents mainly working, holds everything but the flows at the baseline and so isolates the change of numerator. Dividing by the workers the new flows themselves count is the delineation of this group on its own terms. Both are built, and they are far apart, because the group is about a sixth the size of the baseline workforce: at a fixed cutoff, dividing eight million people's flows by fifty million people leaves almost every link below the threshold.

That shows in the counts. At the baseline cutoff in 2015 the delineation on the group's own workforce gives 421 zones, of which 76 hold a single municipality and the largest holds 14. Holding the denominator at the baseline gives 1,069 zones, of which 652 hold a single municipality and the largest holds 5. The second is a statement about scale rather than about labour markets, and it is the first that answers what these people's commuting zones are.

## What the zones look like

At the baseline cutoff the delineation on the group's own workforce gives roughly twice as many zones as the baseline in every year: 938 against 385 in 1980, 708 against 307 in 1990, and 421 against 223 in 2015. Both series consolidate over time, and they consolidate at similar rates, so the ratio is stable. The largest zone holds between 13 and 15 municipalities throughout, against a baseline largest zone that runs to several times that.

The two delineations are genuinely different partitions. The municipality-level Jaccard similarity between them is 0.42 in 1980 and rises steadily to 0.57 in 2015. Every other comparison this project reports sits far above that: two decades apart on the baseline sample scores 0.78, the widened denominator scores 0.89, and the contiguity constraint scores above 0.98. Where these people work is not a small perturbation of where the mainly working work.

## Reading containment here

Containment measured on the group's own flows is 0.947 under their own delineation in 2015 and 0.971 under the baseline delineation. The baseline scores higher, and that is not evidence that the baseline fits this group better. Mean containment rises mechanically with zone size, and the baseline zones are twice as large; a delineation putting every municipality in one zone would score one. In the denominator comparison the two delineations differed in zone count by 8 to 14 percent and containment could be read as a fit measure. Here they differ by a factor of two and it cannot.

What the level does say is that the published zones already hold 95.5 percent of this group's commuters inside their own zone in 2015, against 85.6 percent of the mainly working commuters in the same year, both measured as a share of all commuters rather than as an average across municipalities. A user who applies the published zones to this population is not badly served by them. The case for a separate delineation rests on the partitions being different, not on containment.
