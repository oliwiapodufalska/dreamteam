library(readr)

# Import danych
dane <- read_csv("sklep_rowerowy.csv")

# Wyświetlenie kilku pierwszych wierszy
head(dane)
# Sprawdzenie liczby braków w każdej kolumnie
colSums(is.na(dane))
summary(dane)
str(dane)
#liczba brakow
na_counts <- data.frame(liczba_brakow = colSums(is.na(dane)))
na_counts
#unikatowe wartosci
unique_value <- data.frame(liczba_unikatowych_wartosci = sapply(dane, function(x) length(unique(na.omit(x)))))
unique_value
names(dane)
length(unique(dane$ID))
table(dane$`Marital Status`)
table_with_na <- function(column) {
  tab <- table(column, useNA = "ifany")  # Uwzględnienie braków danych
  names(tab)[is.na(names(tab))] <- "NA"  # Zamiana NA na tekst "NA"
  return(tab)
}
table_with_na(dane$`Marital Status`)

