# Waypoint 1.1.1

* S3 `format()` method for `"waypoints"` objects `usenames` argument fixed.

* S3 `print()` methods for `"coords"` and `"waypoints"` objects print widths correctly when `max` argument / `getOption("max.print")` is exceeded.

* Validate S3 methods for `"coords"` and `"waypoints"` objects now have `force` argument signifying whether to perform full _de novo_ revalidation
  or simply check existing `"valid"`, `"validlat"` and `"validlon"` attributes, essentially to enable the fix to S3 `print()` methods above.

# Waypoint 1.1.0

* Initial CRAN submission.
