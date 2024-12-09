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
proportions <- data.frame(
  kategorie = c(
    "ID",
    rep("Marital Status", length(table_with_na(dane$`Marital Status`))),
    rep("Gender", length(table_with_na(dane$Gender))),
    rep("Income", length(table_with_na(dane$IncomeCategory))),
    rep("Children", length(table_with_na(dane$Children))),
    rep("Education", length(table_with_na(dane$Education))),
    rep("Occupation", length(table_with_na(dane$Occupation))),
    rep("Home Owner", length(table_with_na(dane$`Home Owner`))),
    rep("Cars", length(table_with_na(dane$CarsCategory))),
    rep("Commute Distance", length(table_with_na(dane$`Commute Distance`))),
    rep("Region", length(table_with_na(dane$Region))),
    rep("Age", length(table_with_na(dane$AgeCategory))),
    rep("Purchased Bike", length(table_with_na(dane$`Purchased Bike`)))
  ),
  odpowiedzi = c(
    "Unique Records",
    names(table_with_na(dane$`Marital Status`)),
    names(table_with_na(dane$Gender)),
    names(table_with_na(dane$IncomeCategory)),
    names(table_with_na(dane$Children)),
    names(table_with_na(dane$Education)),
    names(table_with_na(dane$Occupation)),
    names(table_with_na(dane$`Home Owner`)),
    names(table_with_na(dane$CarsCategory)),
    names(table_with_na(dane$`Commute Distance`)),
    names(table_with_na(dane$Region)),
    names(table_with_na(dane$AgeCategory)),
    names(table_with_na(dane$`Purchased Bike`))
  ),
  liczba_obserwacji = c(
    length(unique(dane$ID)),
    as.vector(table_with_na(dane$`Marital Status`)),
    as.vector(table_with_na(dane$Gender)),
    as.vector(table_with_na(dane$IncomeCategory)),
    as.vector(table_with_na(dane$Children)),
    as.vector(table_with_na(dane$Education)),
    as.vector(table_with_na(dane$Occupation)),
    as.vector(table_with_na(dane$`Home Owner`)),
    as.vector(table_with_na(dane$CarsCategory)),
    as.vector(table_with_na(dane$`Commute Distance`)),
    as.vector(table_with_na(dane$Region)),
    as.vector(table_with_na(dane$AgeCategory)),
    as.vector(table_with_na(dane$`Purchased Bike`))
  )
)

# Wyświetlenie tabeli proporcji
print(proportions)
View(proportions)

# Sprawdzenie struktury danych
str(dane)

# Sprawdzenie typów danych w każdej kolumnie
typy_danych <- sapply(dane, class)
print(typy_danych)
# Konwersja na factor dla zmiennych kategorycznych
dane$`Marital Status` <- as.factor(dane$`Marital Status`)
dane$Gender <- as.factor(dane$Gender)
dane$Education <- as.factor(dane$Education)
dane$Occupation <- as.factor(dane$Occupation)
dane$`Home Owner` <- as.factor(dane$`Home Owner`)
dane$`Commute Distance` <- as.factor(dane$`Commute Distance`)
dane$Region <- as.factor(dane$Region)
dane$`Purchased Bike` <- as.factor(dane$`Purchased Bike`)

typy_danych <- sapply(dane, class)
print(typy_danych)

# Zastępowanie braków w zmiennych liczbowych
dane$Income[is.na(dane$Income)] <- median(dane$Income, na.rm = TRUE)
dane$Age[is.na(dane$Age)] <- mean(dane$Age, na.rm = TRUE)
dane$Children[is.na(dane$Children)] <- median(dane$Children, na.rm = TRUE)
dane$Cars[is.na(dane$Cars)] <- median(dane$Cars, na.rm = TRUE)



