# Starting prompt for the agent taking over

Paste the block below as the first message to the new agent. It is written to be self-contained: it points at the handoff note rather than repeating it, so the two do not drift.

---

You are taking over the commuting zone project in `C:\Users\au698627\GitHub\commuting_zone_japan`. Work on the branch `11-fowler-replication`, which already has `main` merged into it.

Read these three first, in order, before doing anything else:

1. `validation/notes/handoff.md` — where the work stands, every decision already settled and the reasoning behind it, the data on hand, and a list of traps that have already cost time. Do not rediscover them.
2. GitHub issue #11 in `daisukeadachi/commuting_zone_japan` — the specification for the task, including its comment thread, which carries two corrections to the issue body.
3. `validation/notes/core_definition.md` — why the Urban Employment Area was chosen as the source for the Core measures and what the alternatives were.

Then read the source paper. It is in Zotero at `C:\Users\au698627\Zotero\storage\DDVIC6G6`. Its replication code is at <https://github.com/csfowler/CommutingZones2020>; `CZ2020_Functions.R` settles several definitions the paper leaves implicit, and it is worth reading rather than inferring.

Your task is issue #11: reproduce on Japanese data every table and figure of Fowler (2024) that the available data supports. The pairwise wage correlation is out of scope, since the wage microdata application is pending. The first deliverable is material for a coauthor meeting, not a draft, so working figures and tables with honest notes matter more than polish.

Two inputs are not in the repository and must be read from the shared project folder, rooted at `C:\Users\au698627\Dropbox\projects\Kawaguchi_Saito\CommutingZone\adachi\clustering`: the commuting matrices under `data/raw/commuteCensusData/data/use/WORK_MAIN/`, and the boundary layer at `data/raw/mmm/shapefiles/mmm20151001_ku_aggregate/mmm20151001.shp`. The handoff note explains why the repository's own `data/` folder is the wrong one to read.

Note that the cutoff sweep needs the dendrogram itself, not the published cuts, so plan to build the dissimilarity matrix and run the clustering yourself for each year rather than reading `output/`.

Work in R where the existing project does, `sf` and `igraph` and `adespatial` are installed. Put new code and outputs under `validation/`; almost everything under `Ikuta_RA/` is gitignored.

Do not start issue #12, the contiguity-constrained clustering, until #11 is done. It reuses the adjacency construction from #11.

Working conventions: everything written into the repository is in English, chat with the user is in Japanese, one branch per task and never force-push, and replies end at the answer asked for without appended menus of suggested next steps. When a choice would change the work materially and the handoff note does not settle it, ask rather than guess.

Start by reading the three documents above and telling me what you understand the task to be and what you intend to build first.
