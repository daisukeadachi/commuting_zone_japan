"""Municipality boundary layers for the census dates the original codes are read on.

The contiguity-constrained clustering needs an adjacency graph on the municipalities of
the census year it runs on. Under the harmonized codes that is one graph, built once from
the 2015 layer. Under each year's own codes it is one graph per year, and the boundaries
have to come from somewhere.

Municipality Map Maker for Web, <http://tkirimura.com/mmm/>, generates the layer for any
date between 1 January 1970 and 1 May 2019. This drives it for 1 October of every census
year up to 2015. The parameters match the 2015 layer the project already holds:
designated cities as single cities with the Tokyo special wards kept as wards, and the
lightweight world-geodetic output.

2020 lies past the tool's coverage. Nothing is lost: no municipal boundary moved between
October 2015 and October 2020, and the two towns that took new codes on incorporation are
handled where the codes are read rather than here.

Writes validation/data/boundaries/mmm<date>/, one shapefile per date, and is skipped for
a date already present. The directory is not committed.

Cite Kirimura, Nakaya and Yano (2011), <https://doi.org/10.5638/thagis.19.139>.
"""

import io
import json
import sys
import time
import urllib.parse
import urllib.request
import zipfile
from pathlib import Path

BASE = "http://tkirimura.com/mmm/"
OUT = Path("validation/data/boundaries")
DATES = [(y, 10, 1) for y in (1980, 1985, 1990, 1995, 2000, 2005, 2010, 2015)]

# Designated cities as one city, Tokyo special wards as wards. The census commuting
# matrices use exactly this universe.
DESIGNATED_CITY_HANDLING = 2
# World geodetic, lightweight. The 2015 layer the project already holds is this one.
OUTPUT_TYPE = 0


def post(page, params):
    """POST a form to one of the tool's endpoints and parse its JSON reply."""
    params = dict(params, t=int(time.time() * 1000))
    data = urllib.parse.urlencode(params).encode()
    request = urllib.request.Request(
        BASE + page, data=data,
        headers={"Content-Type": "application/x-www-form-urlencoded;charset=UTF-8"})
    with urllib.request.urlopen(request, timeout=120) as reply:
        body = reply.read().decode("utf-8", "replace").strip()
    return json.loads(body)


def request_layer(year, month, day):
    """Ask for one date's layer and return the URL of the archive holding it.

    The tool answers either with the archive, when it happens to be cached, or with the
    identifiers of a job it has just started, which is then polled until the shapefile is
    written and asked to be compressed.
    """
    started = post("getmap.php", {"y1": year, "m1": month, "d1": day,
                                  "s": DESIGNATED_CITY_HANDLING, "dt": OUTPUT_TYPE})
    # The tool answers with the result code as a string.
    if str(started.get("result")) == "1":
        return started["url"]
    if str(started.get("result")) != "2":
        raise RuntimeError(f"the tool refused {year}-{month:02d}-{day:02d}: {started}")

    job = {"d": started["dirname"], "f": started["filename"], "c": started["reccnt"]}
    for _ in range(180):
        time.sleep(5)
        if str(post("checkfile.php", job).get("result")) == "1":
            break
    else:
        raise RuntimeError(f"{year}-{month:02d}-{day:02d} was still building after 15 minutes")

    compressed = post("getmapurl.php", dict(job, n=started["tmptb"]))
    if str(compressed.get("result")) != "1":
        raise RuntimeError(f"the archive of {year}-{month:02d}-{day:02d} was refused: {compressed}")
    return compressed["url"]


def fetch(year, month, day):
    destination = OUT / f"mmm{year}{month:02d}{day:02d}"
    if destination.exists() and any(destination.glob("*.shp")):
        print(f"{destination} is already here", flush=True)
        return
    print(f"asking for {year}-{month:02d}-{day:02d}", flush=True)
    url = urllib.parse.urljoin(BASE, request_layer(year, month, day))
    with urllib.request.urlopen(url, timeout=600) as reply:
        archive = reply.read()
    destination.mkdir(parents=True, exist_ok=True)
    with zipfile.ZipFile(io.BytesIO(archive)) as bundle:
        bundle.extractall(destination)
    written = sorted(p.name for p in destination.iterdir())
    print(f"wrote {destination}: {', '.join(written)}", flush=True)


if __name__ == "__main__":
    for date in DATES:
        try:
            fetch(*date)
        except Exception as failure:  # keep going; a failed date is retried on the next run
            print(f"failed on {date}: {failure}", file=sys.stderr, flush=True)
