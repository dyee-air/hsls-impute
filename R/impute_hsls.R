library('haven')
source('wshd.R')

IN_FILE <- 'G:/IES Coursetaking/tmp_hsls_16_student_impute.dta'
OUT_FILE <- IN_FILE

# Read file and remove internally-used colnames
src_df <- read_stata(IN_FILE)
src_df <- src_df %>% filter(W4W1STUP1 > 0)
src_df <- src_df[, which(!(names(src_df) %in% c('stratid', 'donor')))]

# Get columns with missing data, sorted by missing freq
mi_N <- colSums(is.na(src_df))
mi_cols <- mi_N[mi_N > 0] %>% sort() %>% names()

# Impute, using the same strata & sort vars for all variables (for now)
im_df <- src_df
for (mcol in mi_cols) {
  im_df <-
    wshd(im_df,
         mcol,
         'W4W1STUP1',
         c('X1SEX', 'X1RACE'),
         c('X1CONTROL', 'X1TXMTH'))
  im_df$donor <- NULL
}

# Write to file
write_dta(im_df, OUT_FILE)