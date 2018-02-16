# PlexMovieRatings

## Description

This shiny application can be cloned into a directory on your machine and can connect to your Plex Media Server RSQLite database fie and load it for analysis into R. This shiny App displays three different presentations of the same data in plots that are selectable with the raw data being viewable in the bottom table.

## Using your own Data

Please uncomment the ommented out code in the `get_plex_metadata()` function that connects to the database, and uncomment the commented out `packages` definition. You'll also want to delete the file `dat.rds` so the funciton will populate the `rds` file with your data.

*NOTE* Please don't run this script while media scanning is underway.

## Requires

This app requires the following R packages:
* `shiny`
* `DT`
* `ggplot2`
* Optional (for using your own Plex database):
  * `DBI`
  * `RSQLite`
