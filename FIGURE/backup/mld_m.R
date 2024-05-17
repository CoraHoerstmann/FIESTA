mld_m <- function(y, density, depth, ref.depth_min, ref.depth_max, criteria) {

  iref <- y[y$depth > ref.depth_min & y$depth < ref.depth_max, ]
  xref <- mean(iref$density, na.rm=TRUE)
  if (is.na(xref)) {
    warning("No data at reference depth(s).")
    m <- NA
  } else {
    for (crit in criteria) {
      i <- y[y$density > (xref + crit) & y$depth > ref.depth_max,]
      return(min(i$depth))
}}}
