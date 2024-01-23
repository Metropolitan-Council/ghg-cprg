This folder contains metrics for the Origin-Destination trips between the zones of the named analysis.


TERMS
=====
Origin zone: Trips analyzed that started in or initially passed through any of the origin zones.

Destination zone: Trips analyzed that ended in or passed through any of the destination zones after starting in or passing through an origin zone.

Pass-Through: A zone setting indicating how to analyze how trips interact with the zone. Zones marked as pass-through use trips that pass through the zone but do not start or stop in it. Zones not marked as pass-through use trips that start or stop in the zone.

Zone Direction: A pass-through zone can have applied direction which limits the trips analyzed for the zone: only trips that pass through the zone within -20/+20 degrees of the direction will be analyzed for the zone. Values are provided in degrees from 0 to 359, where 0 is due north, 90 is east, 180 is due south, etc. A value of "Null" refers to zones with no applied direction therefore all trips that pass through the zone will be used.

Vehicle Weight: The weight of the vehicles analyzed. Weight values can either be “Medium” or “Heavy”. This column is present only if the commercial analysis is segmented by weight class.


OUTPUT UNIT TERMS
=================
StreetLight Volume: The estimated trip counts as calculated by StreetLight Data's machine learning algorithm.

StreetLight Index: The relative trip activity. The StreetLight Index does not indicate the actual number of trips or vehicles. Trip Index values for different modes of travel (All Vehicles, Trucks, Bicycle, Pedestrian, etc.)  weight classes (such as Trucks Heavy-Duty/Medium-Duty), and countries are based on different sample populations and therefore cannot be compared with each other.

StreetLight Calibrated Index: The estimated number of trips or vehicles derived from the StreetLight Index. Calibration is performed with either StreetLight AADT or user-provided counts.

StreetLight Sample Trip Counts: The total StreetLight sample trip counts for the zone (or set of zones) for all days in the entire data period.

*Note that, while most output units are represented as an average day per its day type definition, Trip Counts are not converted to an average day but rather represent the total Trip Counts. For example, a Trip Count value of 100 for an O-D pair, on an average weekday in March 2017, represents the sum of all trips used from the StreetLight data set for all the weekdays in March 2017.

*More information of the output unit methodology can be found in StreetLight Data's Support Center (https://support.streetlightdata.com).


FILES
=====
Analysis.txt
==============
This file lists information about the analysis as a whole, including the full analysis name, organization and user that created the analysis, and the analysis options configuration.

od_*.csv
========
This files contains the OD metrics.

The fields are:
- Data Periods: Ranges of dates analyzed.
- Mode of Travel: Mode of travel analyzed.
- Origin zone ID: Numeric ID for the origin zone as provided by the user.
- Origin zone Name: Name for the origin zone.
- Origin zone Is Pass-Through: Indicates if the origin zone is pass-through or not as described above under Terms. Is either marked as “Yes” or “No."
- Origin zone Direction (degrees): This refers to the direction in which trips pass through the origin zone as described above under Terms.
- Origin zone is Bi-Direction: Indicates if the zones are bi-directional. Is either marked as “Yes” or “No."
- Destination zone ID: Numeric ID for the destination zone as provided by the user.
- Destination zone Name: Name for the destination zone.
- Destination zone Is Pass-Through: Indicates if the destination zone is pass-through or not as described above under Terms. Is either marked as “Yes” or “No."
- Destination zone Direction (degrees): This refers to the direction in which trips pass through the destination zone as described above in the Terms.
- Destination zone is Bi-Direction: Indicates if the zones are bi-directional. Is either marked as “Yes” or “No."
- Day Type: All Days (traffic from Monday through Sunday), and other day type traffic segmentation as defined by user.
- Day Part: Segments of the day defined by the user in intervals of hours to analyze traffic (All Day is always included as entire 24 hours). The day parts reflect the local time at the origin zone.
- O-D Traffic: The volume of trips from the origin zone to the destination zone, measured using one of the output units described above under the Output Unit Terms.
- Origin Zone Traffic: The volume of trips from the origin zone with no limitation on where they went, measured using one of the output units described above under the Output Unit Terms.
- Destination Zone Traffic: The volume of trips to the destination zone with no limitation on where they came from, measured using one of the output units described above under the Output Unit Terms.
- Avg Travel Time (sec): Average time (in seconds) for the trips from the origin zone to the destination zone. Avg Travel Time can sometimes have an N/A value when the origin zone and destination zone are the, overlapping, or when all the intersecting trips fail StreetLight's data quality checks for travel time.

zone_od_*.csv
=============
This file contains information about each zone used in the analysis. The count represents all trips appropriate to each zone with no limitations on where they came or where they went.

The fields are:
- Data Periods: Ranges of dates analyzed.
- Mode of Travel: Mode of travel analyzed.
- Zone Type: Indicates if the zone is an origin or destination zone for this analysis.
- Zone ID: Numeric ID for the zone as provided by the user.
- Zone Name: Name for the zone.
- Zone Is Pass-Through: Indicates if the zone is marked as pass-through or not as described above under Terms. Is either marked as “Yes” or “No."
- Zone Direction (degrees): This refers to the direction in which trips pass through the zone as described above in the Terms.
- Zone is Bi-Direction: Indicates if the zones are bi-directional. Is either marked as "Yes" or "No".
- Day Type: All Days (traffic from Monday through Sunday), and other day type traffic segmentation as defined by user.
- Day Part: Segments of the day defined by the user in intervals of hours to analyze traffic (All Day is always included as entire 24 hours). The day parts reflect the local time at the origin zone.
- Zone Traffic: The volume of trips starting in or ending in the zone, or trips that pass-through the zone-- based on whether the zone is or is not pass-through as described above under Terms. Zone traffic is measured in its chosen output unit as described above under Output Unit Terms.

zones.csv
=========
This file contains information about the zones used in this analysis.

The fields are:
- Data Periods: Ranges of dates analyzed.
- Zone Type: Indicates if the zone is an origin or destination zone for this analysis.
- Zone ID: Numeric ID for the zone as provided by the user.
- Zone Name: Name for the zone.
- Zone is Pass-Through: Indicates if the zone is marked as pass-through or not as described above under Terms. Is either marked as “Yes” or “No."
- Zone Direction (degrees): This refers to the direction in which trips pass through the zone as described above under Terms.
- Zone is Bi-Direction: Indicates if the zones are bi-directional. Is either marked as "Yes" or "No".
- Fingerprint1: A 64-bit signed integer assigned by StreetLight based on key spatial characteristics of the zone. Combination of Fingerprint1 and Fingerprint2 make up the fingerprint of the zone and indicate if two zones are the same or unique.
- Fingerprint2: A 64-bit signed integer assigned by StreetLight based on key spatial characteristics of the zone. Combination of Fingerprint1 and Fingerprint2 make up the fingerprint of the zone and indicate if two zones are the same or unique.

*.(dbf|prj|shp|shx|cpg)
=======================
These files comprise the shapefiles for the analysis's zone sets.

A shapefile consists of the following several files:
.shp file contains the feature geometries and can be viewed in a geographic information systems application such as QGIS.
.dbf file contains the attributes in dBase format and can be opened in Microsoft Excel.
.shx file contains the data index.
.prj file contains the projection information.
.cpg file contains the encoding applied to create the shapefile.

These shapefiles have the following attributes/columns:
- id: Numeric ID for the zone as provided by the user. This may be null as the field is optional.
- name: Name for the zone.*
- direction (degrees): This refers to the direction in which trips pass through the zone as described above under Terms.
- is_pass: Indicates if the zone is pass-through or not as described above under Terms. 1 = “Yes” and 0 = “No”.
- geom: Polygon of the zone.
- is_bidi: A value of 1 indicates traffic measured in two opposite directions for a single set of Metric values. 0 value indicates traffic is in single direction specified by users.

*_line.(dbf|prj|shp|shx|cpg)
============================
These shapefiles have the following attributes/columns:
- id: Numeric ID for the zone as provided by the user. This may be null as the field is optional.
- name: Name for the zone.*
- geom: LineString of the zone.
- road_type: Type of the road that is represented by the LineString, as entered upon creation of the zone.
- gate_lat: The latitude for the location of the gate on the line segment, as entered upon creation of the zone.
- gate_lon: The longitude for the location of the gate on the line segment, as entered upon creation of the zone.
- gate_width: The total width of the gate, in meters, as entered upon creation of the zone.

* If the zone_name format is [OSM Name] / [OSM ID] / #### (for example: 'Market Street / 9570488 / 1'), the zone file is an OSM Derivative Database, and subject to OSM terms. Refer to https://www.streetlightdata.com/open-source/ for more information.

sample_size.csv
===============
This file contains information about the size of the data sample that was analyzed for this analysis and its data period.

The fields are:
- Data Periods: Ranges of dates analyzed.
- Mode of Travel: Mode of travel analyzed.
- Vehicle Weight: Type of commercial vehicle analyzed.
- Approximate Device Count: Calculated as the number of unique devices in the analysis, rounded to nearest 10 (if below 100), and to nearest 100 (if above 100). N/A for analysis run with Navigaton-GPS data source.
- Approximate Trip Count: An estimated value (calculated during processing) of the number of unique device trips in the StreetLight Data database that were analyzed for this analysis and its Data Period.

ipf_input_counts.csv
====================
This file contains information about input provided to calibrate with Iterative Proportional Fitting used for weave analyses and other matrix scaling applications.

The fields are:
- Data Periods: Ranges of dates analyzed.
- Zone Type: Indicates if the zone is an origin or destination zone for this analysis.
- Zone ID: Numeric ID for the zone as provided by the user.
- Zone Name: Name for the zone.
- Day Type: All Days (traffic from Monday through Sunday), and other day type traffic segmentation as defined by user.
- Day Part: Segments of the day defined by the user in intervals of hours to analyze traffic (All Day is always included as entire 24 hours). The day parts reflect the local time at the origin zone.
- Expected Count: Expected origin and destination counts. For best results, make sure sum(origin) = sum(destination). Calibration is provided for the day types and day parts where total counts are supplied. Absence of value must be indicated with N/A.

skipped_time_attributes.csv
==================
This file lists the zones and the day type/day part combination that were not analyzed for a particular reason. These "skipped" zones and day types/day parts will not appear in the metric download files or in Interactive Visualizations.

The fields are:
- Data Periods: Ranges of dates analyzed.
- Day Type: Average Day (average traffic of Monday through Sunday), and other day type traffic segmentation as defined by user.
- Day Part: Segments of the day defined by the user in intervals of hours to analyze traffic (All Day is always included as entire 24 hours).
- Reason: Information about why the particular zone and day type/day part combination was not included in the analysis.
Run with output type StreetLight Volume with more data months or select output types StreetLight Index or Trip Counts”

NOTES
=====
OD Pairs with No Values
=======================
If the output unit values for an OD pair for a specific time period (e.g. Average Weekday, Early AM) are below StreetLight's significance threshold, no results will be shown in the od_*.csv file.

Day Part Calculations
=====================
The Day Part calculations are done in relation to the zones used in the analysis.
The O-D Trip Count values Day Parts are calculated in relation to the origin zone. The Day Part is determined when trips either start in the origin zone or pass by the centroid of the origin zone, if the origin zone is designated as pass-through.
The origin zone values Day Parts are also calculated in relation to the origin zone.
The destination zone values Day Parts are calculated in relation to the destination zone. The Day Part is determined when trips either end in the destination zone or pass by the centroid of the destination zone, if the destination zone is designated as pass-through.


Copyright © 2011 - 2023, StreetLight Data, Inc. All rights reserved.
