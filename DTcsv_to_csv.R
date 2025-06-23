library(readr)

# File by file ------------------------------------------------------------
# folder <- r'-(C:\Users\claud\Desktop\Admintere_Psiho2024\compare_csv teste)-'
# setwd(folder)
# 
# csv_files <- dir(pattern = ".csv$")
# 
# df_csv <- readr::read_csv(csv_files[1], col_types = cols(.default = col_character())) 
# df_csv <- df_csv[, -1]  
# 
# df_csv_name <- paste0(sub(".csv$", "", csv_files[1]), "_E", ".csv")
# readr::write_csv2(df_csv, df_csv_name)


# Batch -------------------------------------------------------------------
# setwd(utils::choose.dir())
folder <- r'-(C:\Users\claud\Desktop\Admintere_Psiho2024\compare_csv teste)-'
setwd(folder)

csv_files <- dir(pattern = ".csv$")
csv_files <- csv_files[!csv_files %in% grep("_E.csv$", csv_files, value = TRUE)]

for(i in seq_along(csv_files)) {
  df_csv <- readr::read_csv(csv_files[i], col_types = cols(.default = col_character()))
  df_csv <- df_csv[, -1]
  df_csv_name <- paste0(sub(".csv$", "", csv_files[i]), "_E", ".csv")
  readr::write_csv2(df_csv, df_csv_name)
}
