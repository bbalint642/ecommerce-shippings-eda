#   LIBEK ----
library(dplyr)
library(plotly)
library(ggplot2)     
library(hrbrthemes)  
library (tidyr)      
library(viridis)     
library(gmodels)
library(datasets)

#   BEOLVASÁS ----
sales_data <- read.csv(file.path("C:/Users/bbali/Desktop/kaggle/projects/e-commerce-shipping/ecommerce-shipping-eda/input", "e-commerce-shipping.csv"))

# ÁTTEKINTÉS ----
str(sales_data)
head(sales_data)

#   ADATOK ELŐKÉSZÍTÉSE ----

# dekódolási problémák miatt az ID oszlop "ď.żID" néven kerül beolvasásra, de ezt könnyen kijavítjuk
sales_data <- sales_data %>% 
  rename(
    ID = ď.żID
  )

# a numerikus valtozok kivetelevel minden valtozo faktor
for (i in setdiff(1:12, c(1,4,5,6,7,10,11)))
  sales_data[[i]] <- factor(sales_data[[i]])

# faktorokon belüli szintek meghatározása
levels(sales_data$Warehouse_block) <- c(
  "A",
  "B",
  "C",
  "D",
  "E",
  "F")

levels(sales_data$Mode_of_Shipment)  <- c(
  "Repülőn",
  "Úton",
  "Hajón")


levels(sales_data$Product_importance) <- c(
  "Fontos",
  "Alacsony",
  "Közepes"
)

levels(sales_data$Gender) <- c(
  "Nő",
  "Férfi"
)

levels(sales_data$Reached.on.Time_Y.N) <- c(
  "Igen",
  "Nem"
)
#   ÖSSZEFOGLALÓK ----

# súly vs szállítás módja

# megrendelők telefonos hívásai
by(sales_data$Customer_care_calls, sales_data$Mode_of_Shipment, summary)
mean(sales_data$Customer_care_calls)

# megrendelők értékelése szállítási mód szerint
by(sales_data$Customer_rating, sales_data$Mode_of_Shipment, summary)
# átlag megrendelői értékelés az összes rekordra nézve
mean(sales_data$Customer_rating)


# csomagok súlya szállítási módok szerint
by(sales_data$Weight_in_gms, sales_data$Mode_of_Shipment, summary)
# átlag súly az összes csomagra nézve
mean(sales_data$Weight_in_gms)



# VIZUALIZÁCIÓK ----

# szállított mennyiségek arányai
plot_ly(x = ~sales_data$Mode_of_Shipment, type = "histogram")%>%
  layout(title="A rendelések száma szállítási mód szerint")

# csomagok fontossága
plot_ly(x = ~sales_data$Product_importance, type = "histogram")%>%
  layout(title="A csomagok fontossága")

# szállítás határidőn belül
plot_ly(x = ~sales_data$Reached.on.Time_Y.N, type = "histogram")%>%
  layout(title="Szállítás határidőn belül")

# rendelések nemek szerint
plot_ly(x = ~sales_data$Gender, type = "histogram")%>%
  layout(title="Rendelések eloszlása nemek szerint")


# vásárlók értékelései <-- ezt érdemes közelebbről megvizsgálni
    #   ha a vásárló értékelése <= 2, akkor a vásárló rossz
    #   ha az értékelés >= 3, akkor a vásárló jó
plot_ly(x = ~sales_data$Customer_rating, type = "histogram")%>%
  layout(title="Vásárlók értékelései (>=3-tól számít jónak)")

# korábbi vásárlások, visszatérő vásárlók
plot_ly(x = ~sales_data$Prior_purchases, type = "histogram") %>%
  layout(title="Korábbi vásárlások arányai")


# raktárépület és rendelések
plot_ly(x = ~sales_data$Warehouse_block, type = "histogram") %>%
  layout(title="Rendelések raktárépületenként")


# vásárlók hívásai
plot_ly(x = ~sales_data$Customer_care_calls, type = "histogram")%>%
  layout(title="Megrendelők telefonos hívásai")


# KOMPLEXEBB VIZUALIZÁCIÓK ----

mode_weight_means <- plot_ly(
  x = c("Repülőn", "Úton","Hajón"),
  y = c(3629, 3650, 3631),
  name = "status",
  type = "bar"
) %>%
  layout(title="A csomagok átlag súlya szállítási mód szerint")
mode_weight_means



# szállítás módja és csomag fontossága
mode_importance_fig1 <- sales_data
mode_importance_fig1 <- mode_importance_fig1 %>% count(Mode_of_Shipment, Product_importance)
mode_importance_fig1 <- mode_importance_fig1 %>% plot_ly(x = ~Mode_of_Shipment, y = ~n, color = ~Product_importance) %>%
  layout(title="Csomagok szállítási módja és fontossága")

mode_importance_fig1





# MODELLEK ----