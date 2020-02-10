library('tidyverse')

isOdd <- function(x)
  as.logical(x %% 2)

# Alternating sort within groupvar
altsort <- function(df, groupvar, sortvar) {
  return(
    df %>%
      mutate(.grpid = group_indices(df, get(groupvar))) %>%
      group_by(.grpid) %>%
      arrange(if_else(
        isOdd(.grpid), get(sortvar), desc(get(sortvar))
      ), .by_group =
        TRUE) %>%
      ungroup() %>%
      select(-'.grpid')
  )
  
}

# Generate group ids without sorting
groupids <- function(df, groupvars) {
  vals <- unique(df[groupvars])
  vals$id <- vals %>% nrow() %>% seq.int()
  return(df %>% inner_join(vals) %>% pluck('id'))
}

# Hierarchical serpentine sort
serpsort <- function(df, varlist) {
  if (NROW(varlist) == 0)
    return(df)
  
  if (NROW(varlist) == 1)
    return(arrange(df, get(varlist[1])))
  
  sorted <- altsort(df, varlist[1], varlist[2])
  
  if (NROW(varlist) == 2)
    return(sorted)
  
  stratvar <- paste0(varlist[1], varlist[2])
  sorted[stratvar] <- sorted %>% groupids(varlist[1:2])
  return(serpsort(sorted, c(stratvar, varlist[-c(1, 2)])) %>% select(-stratvar))
}

stratify <- function(df, stratvars, varname = 'stratid') {
  return(
    mutate(df,!!varname := group_by_at(df, vars(stratvars)) %>% group_indices()) %>% arrange(get(varname)) %>% ungroup()
  )
}

nonrespondents <-
  function(df, varname)
    df %>% filter(is.na(get(varname)))

respondents <-
  function(df, varname)
    df %>% filter(!is.na(get(varname)))

getNAGroups <- function(df, varname, groupvars) {
  return(df %>% mutate(.gid = groupids(df, groupvars)) %>% group_by(.gid) %>% filter(some(get(varname), is.na)))
}

normed <- function(vals)
  vals / sum(abs(vals))

toCDF <- function(vals)
  vals %>% normed() %>% cumsum()

# Returns the magnitude of the intersection of cell_a and cell_b
overlap <- function(cell_a, cell_b) {
  if (NROW(cell_a) < 2 |
      NROW(cell_b) < 2)
    stop('cell_a and cell_b must each contain at least two numbers.')
  return((min(max(cell_a), max(cell_b)) - max(min(cell_a), min(cell_b))) %>%
           max(0))
}

# Returns the magnitude of the intersection of cell_a and cell_b as a proportion of cell_b
propof <-
  function(cell_a, cell_b)
    overlap(cell_a, cell_b) / (max(cell_b) - min(cell_b))

# Applies `propof` to a list of cells
propsof <-
  function(cells, cell_b)
    cells %>% map_dbl(partial(propof, cell_b = cell_b))

# Returns a list with one element per cell in `cells_b` containing `propsof(cells_a, cell)` for that cell.
# If cells_a are respondent cells and cells_b are non-respondent cells, each list entry consists of the
# WSHD respondent-selection probabilities for the corresponding non-respondent in cells_b
props <-
  function(cells_a, cells_b)
    cells_b %>% map(partial(propsof, cells = cells_a))

# Given an (ordered) vector of weights, partitions [0, 1] proportional to each weight and returns a list whose elements are the low and high bounds of the partition cells for each weight
cdfcells <- function(wts) {
  return(list(start = lag(toCDF(wts), default = 0), end = toCDF(wts)) %>% transpose() %>% map(as.numeric))
}

autoname <-
  function(lst, prefix = 'col')
    setNames(lst, paste0(prefix, 1:NROW(lst)))

# Get strata with at least 1 NR
# For each stratum:
#   Serpsort on selected columns
#   Get R and NR subsets
#   Compute partitions for R and NR

selectable <-
  function(probs, selected)
    as.logical(rowSums(probs) - rowSums(selected)  > 0)

updateProbs <-
  function(prob_df, selected)
    prob_df %>% map_dfr(~ (.x * selectable(prob_df, selected)) %>% normed())

mult <- function(a, b)
  a * b

drawmn <- partial(rmultinom, n = 1, size = 1)
cbindright <- function(a, b)
  cbind(b, a)

selectRespondents <- function(prob_df) {
  selected <- tibble(.rows = NROW(prob_df))
  
  getselectable <- partial(selectable, probs = prob_df)
  
  
  return(prob_df %>% reduce(function(.last, .curr) {
    return(
      .last %>% getselectable() %>% mult(.curr) %>% normed() %>% drawmn() %>% cbindright(.last)
    )
  }
  ,
  .init = selected) %>% setNames(colnames(prob_df)))
}

imputeSingle <- function(df, varname, weightvar) {
  df_list <- list(NR = nonrespondents, R = respondents) %>%
    map(~ .x(df, varname))
  
  # Either no non-respondents or no respondents, so return
  if (NROW(df_list$NR) * NROW(df_list$R) == 0)
    return(mutate(df,!!paste0('IM_', varname) := get(varname)))
  
  print(paste0(
    'Imputing ',
    varname,
    '; NR=',
    NROW(df_list$NR),
    ', R=',
    NROW(df_list$R)
  ))
  
  donors <- df_list %>%
    map(~ pull(.x, weightvar)) %>%
    map(cdfcells) %>%
    (function(.l)
      props(.l$R, .l$NR)) %>%
    autoname('NR') %>%
    as_tibble() %>%
    selectRespondents() %>%
    map(as.logical) %>%
    map(which) %>%
    as.numeric()
  
  getDonorVal <- function(donor)
    df_list$R[[donor, varname]]
  
  nrs <- cbind(df_list$NR, donor = donors)
  nrs[paste0('IM_', varname)] <- nrs$donor %>% map_dbl(getDonorVal)
  
  rs <- df_list$R
  rs[paste0('IM_', varname)] <- rs[varname]
  rs$donor <- NA
  
  return(df %>% inner_join(rbind(rs, nrs)))
}

wshd <-
  function(df,
           imputeVar,
           weightVar,
           stratvars,
           sortvars = NULL) {
    suppressMessages(suppressWarnings(return(
      (
        df %>% stratify(stratvars) %>% group_by(stratid) %>% group_modify( ~ serpsort(.x, sortvars) %>% group_modify(
          ~ imputeSingle(.x, imputeVar, weightVar)
        )) %>% ungroup()
      )
    )))
  }