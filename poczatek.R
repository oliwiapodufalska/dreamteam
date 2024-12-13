library(readr)

# Import danych
dane <- read_csv("sklep_rowerowy.csv")

# Wyświetlenie kilku pierwszych wierszy
head(dane)
# Sprawdzenie liczby braków w każdej kolumnie
install.packages("naniar")
library(naniar)
levels(dane$Marital.Status) <- c("Married", "Single")
levels(dane$Gender) <- c("Male", "Female")
levels(dane$Home.Owner) <- c("No", "Yes")
str(dane)

# Wizualizacja braków danych
vis_miss(dane)

# Podsumowanie braków w każdej kolumnie
miss_var_summary(dane)

vis_miss(dane, cluster = TRUE)  # Klasteryzacja braków, aby znaleźć powiązania między kolumnami
brak_summary <- miss_var_summary(dane)
print(brak_summary)
View(brak_summary)  # Otwiera podsumowanie w zakładce Viewer w RStudio

# Liczba braków dla każdej kolumny
na_counts <- miss_var_summary(dane)
print(na_counts)
install.packages("dplyr")


# Liczba unikatowych wartości w każdej kolumnie
unique_value <- data.frame(liczba_unikatowych_wartosci = sapply(dane, n_distinct))
print(unique_value)

library(dplyr)

# Funkcja do liczenia proporcji dla zmiennych kategorycznych
table_with_na <- function(column) {
  tab <- table(column, useNA = "ifany")  # Uwzględnienie braków danych
  prop <- prop.table(tab) * 100  # Proporcje w procentach
  data.frame(Odpowiedzi = names(tab), Liczba_Obserwacji = as.vector(tab), Proporcje = as.vector(prop))
}
dane <- dane %>%
  mutate(
    Marital.Status = as.factor(Marital.Status),
    Gender = as.factor(Gender),
    Home.Owner = as.factor(Home.Owner)
  )

# Automatyczne liczenie dla wszystkich zmiennych kategorycznych
proportions_all <- dane %>%
  select(where(is.factor)) %>%
  summarise(across(everything(), ~ list(table_with_na(.))))  # Zbiera proporcje dla każdej zmiennej
print(proportions_all)
install.packages("tidyr")


# Tworzenie tabeli proporcji z dplyr
proportions <- dane %>%
  select(where(is.factor)) %>%
  summarise(across(everything(), ~ list(table_with_na(.)))) %>%
  pivot_longer(cols = everything(), names_to = "Kategoria", values_to = "Tabela") %>%
  unnest(Tabela)

print(n=29,proportions)
library(ggplot2)

# Wizualizacja braków danych
ggplot(na_counts, aes(x = variable, y = n_miss)) +
  geom_bar(stat = "identity") +
  labs(title = "Liczba braków danych w każdej kolumnie", x = "Kolumna", y = "Liczba braków") +
  theme_minimal()

required_columns <- c("Purchased Bike", "Income", "Age", "Marital Status")
# Sprawdzenie braków w nowych zmiennych kluczowych
missing_summary <- sapply(required_columns, function(col) sum(is.na(dane[[col]])))
print(missing_summary)

library(ggplot2)

# Wizualizacja braków dla zmiennych kluczowych
missing_summary_df <- data.frame(
  Zmienna = required_columns,
  Braki = sapply(required_columns, function(col) sum(is.na(dane[[col]])))
)

ggplot(missing_summary_df, aes(x = Zmienna, y = Braki)) +
  geom_bar(stat = "identity") +
  labs(title = "Liczba braków w zmiennych kluczowych", x = "Zmienna", y = "Liczba braków") +
  theme_minimal()
library(dplyr)

# Konwersja wybranych kolumn na factor
factor_cols <- c("Marital Status", "Gender", "Education", "Occupation", "Home Owner", "Commute Distance", "Region", "Purchased Bike")
dane <- dane %>%
  mutate(across(all_of(factor_cols), as.factor))

# Sprawdzenie typów danych po konwersji
typy_danych <- sapply(dane, class)
print(typy_danych)
# Zastępowanie braków w zmiennych liczbowych za pomocą średniej adaptacyjnej
dane <- dane %>%
  mutate(
    Income = ifelse(is.na(Income), mean(Income, na.rm = TRUE, trim = 0.1), Income),
    Age = ifelse(is.na(Age), mean(Age, na.rm = TRUE, trim = 0.1), Age),
    Children = ifelse(is.na(Children), mean(Children, na.rm = TRUE, trim = 0.1), Children),
    Cars = ifelse(is.na(Cars), mean(Cars, na.rm = TRUE, trim = 0.1), Cars)
  )
install.packages("mice")
library(mice)

# Tworzenie modelu imputacji
imputed_data <- mice(dane, m = 5, method = 'pmm', seed = 123)
colnames(dane) <- make.names(colnames(dane))

# Uzupełnienie braków
dane <- complete(imputed_data)
plot(imputed_data)  # Wizualizacja konwergencji imputacji
library(dplyr)

# Lista zmiennych kategorycznych
categorical_vars <- c("Marital.Status", "Gender", "Home.Owner")


# Zastępowanie braków dominantą
dane <- dane %>%
  mutate(across(all_of(categorical_vars), ~ ifelse(is.na(.), names(which.max(table(., useNA = "no"))), .)))

colnames(dane)
library(rpart)

# Funkcja do imputacji zmiennych kategorycznych za pomocą drzewa decyzyjnego
impute_with_tree <- function(data, target_var) {
  fit <- rpart(as.formula(paste(target_var, "~ .")), data = data, method = "class", na.action = na.exclude)
  data[[target_var]][is.na(data[[target_var]])] <- predict(fit, data[is.na(data[[target_var]]), ], type = "class")
  return(data)
}
impute_with_tree 
# Imputacja dla zmiennych kategorycznych
for (var in categorical_vars) {
  dane <- impute_with_tree(dane, var)
}
library(mice)

# Ograniczenie do zmiennych kategorycznych
categorical_data <- dane %>% select(all_of(categorical_vars))

# Imputacja wielowymiarowa
imputed_data <- mice(categorical_data, m = 5, method = "logreg", seed = 123)
if (sum(is.na(dane)) == 0) {
  cat("Braki zostały całkowicie usunięte.\n")
} else {
  cat("Pozostały braki w danych.\n")
}

# Zastąpienie braków
dane[categorical_vars] <- complete(imputed_data)
# Sprawdzenie braków po imputacji
colSums(is.na(dane[categorical_vars]))
str(dane)
table(dane$Marital.Status)
table(dane$Gender)
table(dane$Home.Owner)
library(ggplot2)

# Marital.Status
ggplot(dane, aes(x = Marital.Status)) +
  geom_bar() +
  labs(title = "Rozkład stanu cywilnego", x = "Stan cywilny", y = "Liczba osób")

# Gender
ggplot(dane, aes(x = Gender)) +
  geom_bar() +
  labs(title = "Rozkład płci", x = "Płeć", y = "Liczba osób")

# Home.Owner
ggplot(dane, aes(x = Home.Owner)) +
  geom_bar() +
  labs(title = "Rozkład własności domu", x = "Czy posiada dom", y = "Liczba osób")
# Sprawdzenie, czy wszystkie braki zostały wypełnione
colSums(is.na(dane))
install.packages("tidyverse")
install.packages("gcookbook")
