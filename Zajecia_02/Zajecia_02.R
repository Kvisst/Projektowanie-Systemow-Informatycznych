# 1. Import danych i instalacja ----
getwd()
setwd("C:/Users/mg471937/Desktop/R")


# install.packages("readxl") 
# install.packages("dplyr")
# install.packages("ggplot2")
# install.packages("writexl")

library(writexl)
library(ggplot2)
library(dplyr)
library(readxl)



kraje_1 = read.table("kraje_makro_1.csv", header=TRUE, sep=",", dec=".")
kraje_2 = read.table("kraje_makro_2.csv", header=TRUE, sep=",", dec=".")


# 2. Przygotowanie danych ----

summary(kraje_1)
summary(kraje_2)

kraje_1$X = NULL
kraje_2$X = NULL

colnames(kraje_2) = c("Kod_kraju", "Nazwa", "Region", "Urbanizacja_proc.", "Internet_proc.")

is.numeric(kraje_2$Region) 
is.character(kraje_2$Region) 	

kraje_2$Region = as.factor(kraje_2$Region)

summary(kraje_2)
levels(kraje_2$Region)


colSums(is.na(kraje_1))	
colSums(is.na(kraje_2))	

sum(is.na(kraje_2$Internet_proc.))

kraje_2[is.na(kraje_2$Internet_proc.), ]



# Czyszczenie danych
# W ramce danych kraje_2, w kolumnie Region s¹ kategorie, w których nazwie jest znak &:
levels(kraje_2$Region)

kraje_2$Region <- gsub("&", "and", kraje_2$Region)

kraje_2$Region = as.factor(kraje_2$Region)
levels(kraje_2$Region)

#   £¹czenie (scalanie) ramek danych kraje_1 i kraje_2
kraje = merge(kraje_1, kraje_2, by.x="Kod", by.y="Kod_kraju")

# usuniêcie kolumny po z³¹czeniu
kraje$Nazwa = NULL

summary(kraje)
str(kraje)


# 3.  Podstawowa analiza danych ----

head(kraje_1)
head(kraje_2)      

tail(kraje_1)
tail(kraje_2)


mean(kraje_1$Przyrost_populacji)	
median(kraje_1$Przyrost_populacji)
min(kraje_1$Przyrost_populacji)	
max(kraje_1$Przyrost_populacji)		



# Tworzenie nowej zmiennej Populacja_w_mln w dplyr:
kraje = kraje %>%
  mutate(Populacja_mln = Populacja / 1e6)


# Tworzenie nowej zmiennej PKB_per_capita w dplyr:
kraje = kraje %>%
  mutate(PKB_per_capita = PKB / Populacja)

# Wyœwietl kraje, w których % poziom urbanizacji jest wiêkszy ni¿ 50
kraje %>%
  filter(Urbanizacja_proc. > 50)


# Wyœwietl tylko dane pokazuj¹ce zmienne Panstwo, Region, PKB, Populacja_mln
kraje %>%
  select(Panstwo, Region, PKB, Populacja_mln)

# Posortuj kraje wed³ug przyrostu populacji rosn¹co
kraje %>%
  arrange(Przyrost_populacji)


# Posortuj kraje wed³ug przyrostu populacji malej¹co
kraje %>%
  arrange(desc(Przyrost_populacji))


# Wybierz kraje z PKB wiêkszym ni¿ 1 bilion, posortuj je rosn¹co wzglêdem PKB 
# i wyœwietl nazwê pañstwa, PKB i PKB per capita. Ile jest takich krajów?
kraje %>%
  filter(PKB > 1e12) %>%
  arrange(PKB) %>%
  select(Panstwo, PKB, PKB_per_capita)



# Wybierz kraje z regionu Afryki Subsaharyjskiej, 
# wybierz zmienne Panstwo, PKB_per_capita, Populacja_mln, Urbanizacja,
# a nastêpnie posortuj malej¹co po PKB per capita
kraje %>%
  filter(Region == "Sub-Saharan Africa") %>%
  select(Panstwo, PKB_per_capita, Populacja_mln, Urbanizacja_proc.) %>%
  arrange(desc(PKB_per_capita))

# Wyœwietl tylko te kraje, które s¹ bogatsze ni¿ œrednia regionu
bogate = kraje %>%
  group_by(Region) %>%
  filter(PKB_per_capita > mean(PKB_per_capita, na.rm = TRUE))



# ZnajdŸ najwiêksz¹ wartoœæ PKB per capita w ca³ym zbiorze krajów
kraje %>%
  summarise(max_PKB_per_capita = max(PKB_per_capita, na.rm = TRUE))



# ZnajdŸ najwiêksz¹ i najmniejsz¹ wartoœæ Populacji w mln w ca³ym zbiorze krajów
kraje %>%
  summarise(
    min_populacja = min(Populacja_mln, na.rm = TRUE),
    max_populacja = max(Populacja_mln, na.rm = TRUE))



# Oblicz œredni¹ populacjê w ca³ym zbiorze krajów (jedna liczba dla ca³ej ramki)
kraje %>%
  summarise(srednia_populacja = mean(Populacja_mln, na.rm = TRUE))


# Ile krajów jest w ca³ym zbiorze danych?
kraje %>%
  summarise(liczba_krajow = n())



# Policz, ile krajów jest w ka¿dym regionie
kraje %>%
  group_by(Region) %>%
  summarise(liczba_krajow = n())


# Dla ka¿dego regionu œwiata: oblicz liczbê krajów (n), œredni % dostêp do internetu i œredni % poziom urbanizacji, a nastêpnie posortuj regiony malej¹co wg œredniego % dostêpu do internetu
kraje %>%
  group_by(Region) %>%
  summarise(
    liczba_krajow = n(),
    sredni_internet = mean(Internet_proc., na.rm = TRUE),
    srednia_urbanizacja = mean(Urbanizacja_proc., na.rm = TRUE)
  ) %>%
  arrange(desc(sredni_internet))


# 4. Wizualizacja ----


# 1. Prosty wykres punktowy: urbanizacja a PKB per capita
ggplot(kraje, aes(x = Urbanizacja_proc., y = PKB_per_capita)) +
  geom_point() +
  labs(
    title = "Urbanizacja a PKB per capita",
    x = "Urbanizacja (%)",
    y = "PKB per capita")



# 2. Zaawansowany wykres punktowy: urbanizacja a PKB per capita 
ggplot(kraje, aes(x = Urbanizacja_proc., y = PKB_per_capita, color = Region)) +
  geom_point(size = 3, alpha = 0.7) +
  scale_y_log10(labels = scales::comma) +
  labs(
    title = "Urbanizacja a PKB per capita",
    subtitle = "Czy bardziej zurbanizowane kraje s¹ bogatsze?",
    x = "Urbanizacja (% ludnoœci miejskiej)",
    y = "PKB per capita (USD, skala log)",
    color = "Region œwiata"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    legend.position = "bottom")



# 3. Zaawansowany wykres punktowy: rozmiar gospodarki a populacja 

ggplot(kraje, aes(x = Populacja_mln, y = PKB, size = PKB_per_capita, color = Region)) +
  geom_point(alpha = 0.7) +
  scale_x_log10() +
  scale_y_log10() +
  labs(
    title = "Skala gospodarki i demografia",
    x = "Populacja (mln, log10)",
    y = "PKB (USD, log10)",
    size = "PKB per capita"
  ) +
  theme_minimal()



# 4. Prosty wykres s³upkowy: liczba krajów w regionach 
ggplot(kraje, aes(x = Region)) +
  geom_bar(fill = "steelblue", color = "white") +
  labs(
    title = "Liczba krajów w regionach œwiata",
    x = "Region",
    y = "Liczba krajów"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(hjust = 0.5))


# 5. Zaawansowany wykres s³upkowy poziomy: TOP 15 najbogatszych krajów 
kraje %>%
  arrange(desc(PKB_per_capita)) %>%
  head(15) %>%
  ggplot(aes(x = reorder(Panstwo, PKB_per_capita), y = PKB_per_capita, fill = Region)) +
  geom_col() +
  coord_flip() +
  scale_y_continuous(labels = scales::comma) +
  labs(
    title = "TOP 15 najbogatszych krajów œwiata (2016)",
    subtitle = "PKB per capita w USD",
    x = NULL,
    y = "PKB per capita (USD)",
    fill = "Region"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    axis.text.y = element_text(size = 10))



# 6. Wykres pude³kowy (boxplot): dostêp do internetu wed³ug regionów 
ggplot(kraje, aes(x = reorder(Region, Internet_proc., FUN = median), 
                  y = Internet_proc., fill = Region)) +
  geom_boxplot(alpha = 0.7) +
  geom_jitter(width = 0.2, alpha = 0.3, size = 1) +
  coord_flip() +
  labs(
    title = "Dostêp do internetu wed³ug regionów œwiata",
    subtitle = "(punkty to poszczególne kraje)",
    x = NULL,
    y = "Dostêp do internetu (% populacji)",
    fill = "Region"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    legend.position = "none")

# 7. Wykres pude³kowy (boxplot): przyrost populacji wed³ug regionów 
# (mediana, rozrzut i obserwacje odstaj¹ce)
ggplot(kraje, aes(x = Region, y = Przyrost_populacji)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_boxplot(outlier.alpha = 0.3) +
  geom_jitter(width = 0.15, alpha = 0.5) +
  coord_flip() +
  labs(
    title = "Tempo przyrostu populacji w regionach œwiata",
    subtitle = "(punkty to poszczególne kraje, linia przerywana = 0%)",
    x = "Region",
    y = "Przyrost populacji (%)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14))


# 5. Eksport danych ----

# Zapisanie ramki danych do pliku CSV
write.csv(kraje, "kraje_analiza.csv") 

# Zapisanie ramki danych do pliku Excel wymaga pakietu writexl:
write_xlsx(kraje, "kraje_wynik.xlsx")




































