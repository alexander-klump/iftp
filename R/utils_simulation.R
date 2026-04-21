# Internal simulation helpers (used across all simulation functions) ----------

# Resolve seed for reproducibility.
# If seed is given, set it. If capture_seed is TRUE and seed is NULL,
# generate a random seed and set it. Returns the seed (or NULL).
resolve_seed <- function(seed, capture_seed) {
  if (!is.null(seed)) {
    set.seed(seed)
    return(seed)
  }
  if (capture_seed) {
    seed <- sample.int(.Machine$integer.max, 1L)
    set.seed(seed)
    return(seed)
  }
  NULL
}
