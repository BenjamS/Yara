library(tidyverse)
library(pracma)

this_folder <- "C:/Users/bensc/OneDrive/Documents/Data/FAO Data/"
# this_file <- "trade_data_temp.csv"
# this_filepath <- paste0(this_folder, this_file)
# df_trade <- read.csv(this_filepath, stringsAsFactors = F)
# df_trade <- df_trade[, c("Area", "Year", "Element", "Item", "Unit", "Value")]
#---
this_file <- "Value_of_Production_E_All_Data.csv"
this_filepath <- paste0(this_folder, this_file)
df_raw <- read.csv(this_filepath, stringsAsFactors = F)
#df_raw <- df_raw[, c("Area", "Year", "Element", "Item", "Unit", "Value")]
df_raw$Area.Code <- NULL
df_raw$Item.Code <- NULL
df_raw$Element.Code <- NULL
colnames(df_raw) <- gsub("Y", "", colnames(df_raw))
ind_rm <- grep("F", colnames(df_raw))
df_raw <- df_raw[, -ind_rm]
df_raw <- df_raw %>% gather(Year, Value, `1961`:`2018`)
these_areas <- c("World", "Colombia", "South America", "Central America")
df_vap <- subset(df_raw, Area %in% these_areas)
df_vap$Item[grep("Vegetables and Fruit Primary", df_vap$Item)] <- "Fruit & Veg."
df_vap$Item[grep("Oilcrops", df_vap$Item)] <- "Oilcrops"

pulses_vec <- c("Beans, dry", "Bambara beans", "Lentils", "Chick peas", "Peas, dry",
                "Cow peas, dry", "Pigeon peas", "Pulses nes")

df_pulses <- subset(df_vap, Item %in% pulses_vec)
df_pulses <- df_pulses %>% group_by(Area, Element, Unit, Year) %>%
  summarise(Value = sum(Value, na.rm = T)) %>% as.data.frame()
df_pulses$Item <- "Pulses, Total"
df_pulses <- df_pulses[, colnames(df_vap)]
df_vap <- as.data.frame(rbind(df_vap, df_pulses))
#----------------------------------------------------------------------------
this_file <- "Production_Crops_E_All_Data.csv"
this_filepath <- paste0(this_folder, this_file)
df_crop <- read.csv(this_filepath, stringsAsFactors = F)
df_crop$Area.Code <- NULL
df_crop$Item.Code <- NULL
df_crop$Element.Code <- NULL
colnames(df_crop) <- gsub("Y", "", colnames(df_crop))
not_these <- grep("F|N", colnames(df_crop))
df_crop <- df_crop[, -not_these]
df_crop <- df_crop %>% gather(Year, Value, `1961`:`2018`)
u <- df_crop$Item
df_crop$Item[grep("Fruit|Vegetable", u)] <- "Fruit & Veg."
#df_crop$Item <- gsub(", Total", "", df_crop$Item)
df_crop <- df_crop %>% group_by(Area, Year, Item, Element) %>%
  summarize(Value = sum(Value)) %>% as.data.frame()
#---------------------------------------------------------------------------
df_price <- subset(df_vap, Element == "Gross Production Value (constant 2014-2016 1000 I$)")
#"Gross Production Value (constant 2014-2016 1000 I$)"
colnames(df_price)[ncol(df_price)] <- "VAP"
df_prod <- subset(df_crop, Element == "Production")
colnames(df_prod)[ncol(df_prod)] <- "Production"
df_prod$Element <- NULL
df_prod$Unit <- NULL
df_price$Element <- NULL
df_price$Unit <- NULL
df_price <- merge(df_price, df_prod, by = c("Area", "Year", "Item"))
df_price$Price <- 10^3 * df_price$VAP / df_price$Production

# unique(df_crop$Item[grep("Pulses", df_crop$Item, ignore.case = T)])
# unique(df_vap$Item[grep("Pulses", df_vap$Item, ignore.case = T)])
# unique(df_price$Item[grep("Fruit", df_price$Item, ignore.case = T)])
# unique(df_raw$Item[grep("pea", df_raw$Item, ignore.case = T)])

these_crops <- c("Cereals, Total", "Oilcrops", "Pulses, Total",
                 "Roots and Tubers, Total",
                 "Fruit & Veg.")

df_plot <- subset(df_price, Area %in% these_areas &
                    Item %in% these_crops)

df_plot$Item <- gsub(", Total", "", df_plot$Item)


df_plot <- df_plot %>% group_by(Area, Item) %>%
  mutate(VAP = detrend(VAP)) %>% as.data.frame()

df_plot$Year <- as.integer(df_plot$Year)
gg <- ggplot(df_plot, aes(x = Year, y = VAP,
                          group = Item, color = Item))
gg <- gg + geom_line()
#gg <- gg + scale_x_log10() + scale_y_log10()
gg <- gg + facet_wrap(~Area, scales = "free_y")
gg

#----------------------------------------------------------------------------
df_mu <- df_price %>% group_by(Area, Item) %>%
  summarise(mu = mean(VAP, na.rm = T)) %>% as.data.frame()
df_sd <- df_price %>% group_by(Area, Item) %>%
  summarise(sd = sd(VAP, na.rm = T)) %>% as.data.frame()
df_plot <- merge(df_mu, df_sd, by = c("Area", "Item"))
df_plot <- subset(df_plot, Area %in% these_areas)
df_plot$lmu <- log(df_plot$mu)
df_plot$lsd <- log(df_plot$sd)

gg <- ggplot(df_plot, aes(x = sd, y = mu))
gg <- gg + geom_point()
gg <- gg + scale_x_log10() + scale_y_log10()
gg <- gg + facet_wrap(~Area, scales = "free")
gg

df_mod <- subset(df_plot, Area == "Colombia")
mod <- lm(lmu ~ lsd, df_mod)
summary(mod)

df_q <- subset(df_price, Year == 2017 &
                 Area == "Colombia")
df_q <- df_q[-grep(", Total", df_q$Item), ]

q <- quantile(df_q$VAP, probs = 0.95, na.rm = T)
df_q[which(df_q$VAP > q), ]
#----------------------------------------------------------------------------


# df_plot <- subset(df_crop, Area %in% these_areas &
#                     Item %in% these_crops)
# df_plot$Item <- gsub(", Total", "", df_plot$Item)
# 
# df_plot <- df_plot %>% group_by(Area, Item, Element) %>%
#   mutate(Value = detrend(Value)) %>% as.data.frame()
# 
# df_plot$Year <- as.integer(df_plot$Year)
# gg <- ggplot(df_plot, aes(x = Year, y = Value,
#                           group = Item, color = Item))
# gg <- gg + geom_line()
# #gg <- gg + scale_x_log10() + scale_y_log10()
# gg <- gg + facet_grid(Element~Area, scales = "free")
# gg
# 
# 
# df_mu <- df_crop %>% group_by(Area, Item, Element) %>%
#   summarise(mu = mean(Value, na.rm = T)) %>% as.data.frame()
# df_sd <- df_crop %>% group_by(Area, Item, Element) %>%
#   summarise(sd = sd(Value, na.rm = T)) %>% as.data.frame()
# df_plot <- merge(df_mu, df_sd, by = c("Area", "Item", "Element"))
# df_plot <- subset(df_plot, Area %in% these_areas)
# #unique(df_plot$Item)
# df_plot$lmu <- log(df_plot$mu)
# df_plot$lsd <- log(df_plot$sd)
# df_plot$lsd2 <- df_plot$lsd^2
# 
# 
# 
# gg <- ggplot(df_plot, aes(x = lsd, y = lmu))
# gg <- gg + geom_point()
# #gg <- gg + scale_x_log10() + scale_y_log10()
# gg <- gg + facet_grid(Element~Area, scales = "free")
# gg
# 
# 
# df_mod <- subset(df_plot, Area == "Colombia" &
#                    Element == "Area harvested" &
#                    Item != "Fruit & Veg.")
# 
# gg <- ggplot(df_mod, aes(x = lsd, y = lmu))
# gg <- gg + geom_point()
# #gg <- gg + scale_x_log10() + scale_y_log10()
# #gg <- gg + facet_grid(Element~Area, scales = "free")
# gg
# 
# 
# mod <- lm(lmu ~ lsd, df_mod)
# summary(mod)
# plot(mod$fitted.values, mod$residuals)
# ind_rm <- which(abs(mod$residuals) > 1)
# df_mod <- df_mod[-ind_rm, ]
# mod <- lm(lmu ~ lsd, df_mod)
# summary(mod)
# plot(mod$fitted.values, mod$residuals)
# 
# 
# gg <- ggplot(df_mod, aes(x = lsd, y = lmu))
# gg <- gg + geom_point()
# gg <- gg + geom_abline(intercept = 0.741,
#                        slope = 1.016)
# #gg <- gg + geom_smooth(se = F)
# gg

#---------------------------------------------------------------------------
# this_file <- "landcover_temp.csv"
# this_filepath <- paste0(this_folder, this_file)
# df_cover <- read.csv(this_filepath, stringsAsFactors = F)
# df_cover <- df_cover[, c("Area", "Year", "Element", "Item", "Unit", "Value")]
# df_cover <- subset(df_cover, Item %in% c("Herbaceous crops", "Grassland") &
#                      Element == "Area from CCI_LC")
# #unique(df_cover$Element)
this_file <- "Environment_LandCover_E_All_Data.csv"
this_filepath <- paste0(this_folder, this_file)
df_raw <- read.csv(this_filepath, stringsAsFactors = F)
rm_cols <- grep("Code|F", colnames(df_raw))
df_raw <- df_raw[, -rm_cols]
colnames(df_raw) <- gsub("Y", "", colnames(df_raw))
df_raw <- df_raw %>% gather(Year, Value, `1992`:`2018`)
df_cover <- subset(df_raw, Element == "Area from CCI_LC")
df_cover$Element <- NULL 
df_cover$Value <- 1000 * df_cover$Value
df_cover$Unit <- NULL
df_cropCover <- subset(df_cover, Item == "Herbaceous crops")
df_grassCover <- subset(df_cover, Item == "Grassland")
df_cropCover$Item <- NULL
df_grassCover$Item <- NULL
colnames(df_cropCover)[ncol(df_cropCover)] <- "Total cropland"
colnames(df_grassCover)[ncol(df_grassCover)] <- "Total grassland"
#----------------------------------------------------------------------------
df_area <- subset(df_crop, Element == "Area harvested")
df_area$Element <- NULL
colnames(df_area)[ncol(df_area)] <- "Area harv"
df_area <- merge(df_area, df_cropCover, by = c("Area", "Year"))
df_area$`Area share` <- 100 * df_area$`Area harv` / df_area$`Total cropland`

df_q <- subset(df_area, Area == "Colombia" & Year == 2017)
df_q <- df_q[-grep(", Total|Rice Milled Eqv", df_q$Item), ]

ind_top <- which(df_q$`Area share` > 8)
df_q[ind_top, ]
sum(df_q$`Area share`[ind_top])

q <- quantile(df_q$`Area share`, probs = 0.9, na.rm = T)
ind_top <- which(df_q$`Area share` > q)
df_q[ind_top, ]
sum(df_q$`Area share`[ind_top])








#---
# df_areaLs <- subset(df_cover, Item == "Grassland")
# df_areaLs$Item <- "Livestock"
# df_areaLs <- df_areaLs[, colnames(df_area)]
# # df_area <- as.data.frame(rbind(df_area, df_areaLs))
# # colnames(df_area)[ncol(df_area)] <- "Area harv"
# # df_cover <- df_cover %>% group_by(Area, Year) %>%
# #   summarise(Value = sum(Value)) %>% as.data.frame()
# # colnames(df_cover)[ncol(df_cover)] <- "Total area"
#---






#---
this_file <- "ls_temp.csv"
this_filepath <- paste0(this_folder, this_file)
df_ls <- read.csv(this_filepath, stringsAsFactors = F)
df_ls <- df_ls[, c("Area", "Year", "Element", "Item", "Unit", "Value")]
#---
df_cropProd <- subset(df_crop, Element == "Production")
df_ls$Unit <- NULL
#df_ls$Area <- NULL
df_ls <- df_ls[, colnames(df_cropProd)]
df_ls$Item[grep("Meat", df_ls$Item)] <- "Livestock"
df_prod <- as.data.frame(rbind(df_cropProd, df_ls))
df_prod <- subset(df_prod, Item != "Milk, Total")
df_prod <- df_prod[, c("Area", "Year", "Item", "Value")]
colnames(df_prod)[ncol(df_prod)] <- "Production"
#unique(df_prod$Item)
#---
colnames(df_vap)[ncol(df_vap)] <- "VAP"
df_vap$Item <- gsub(", Total", "", df_vap$Item)
df_vap$Item[grep("Fruit", df_vap$Item)] <- "Fruit & Veg."
df_vap$Item[grep("Oilcrops", df_vap$Item)] <- "Oilcrops"
df_vap$Item[grep("Livestock", df_vap$Item)] <- "Livestock"
#df_vap$Area <- NULL

df_price <- merge(df_area, df_vap, by = c("Area", "Year", "Item"))
df_price$RevYd <- df_price$VAP / df_price$`Area harv`
df_price <- merge(df_price, df_prod, by = c("Area", "Year", "Item"))
df_price$Price <- df_price$VAP / df_price$Production
#---------------------------------------------------------------------------
#df_plot <- subset(df_price, Year == 2017)
df_plot <- df_price
df_plot$Unit <- NULL
df_plot$Element <- NULL
df_plot$`Total area` <- NULL

df_plot <- df_plot %>% gather(Var, Value, `Area harv`:Price)
df_plot$Year <- as.integer(df_plot$Year)
df_plot <- subset(df_plot, Var %in% c("Price", "Area share", "Area harv", "RevYd") &
#                    Area %in% c("Northern Africa", "Central America") &
                    Item != "Livestock")


gg <- ggplot(df_plot, aes(x = Year, y = Value, group = Item, color = Item))
gg <- gg + geom_line()
gg <- gg + facet_grid(Var~Area, scales = "free_y")
gg



these_vars <- c("Area share", "Price")
df_mu <- df_plot %>% group_by(Area, Item, Var) %>%
  summarise(Value = mean(Value)) %>% as.data.frame()
df_sd <- df_plot %>% group_by(Area, Item, Var) %>%
  summarise(Value = sd(Value)) %>% as.data.frame()

ind <- ncol(df_mu)
colnames(df_mu)[ind] <- "mu"
colnames(df_sd)[ind] <- "sd"

df_plot <- merge(df_mu, df_sd, by = c("Area", "Item", "Var"))
df_plot <- subset(df_plot, Var == "Area harv")

gg <- ggplot(df_plot, aes(x = `sd`,
                          y = `mu`,
                          label = Item))
gg <- gg + geom_point()
#gg <- gg + geom_text()
gg <- gg + scale_x_log10() + scale_y_log10()
#gg <- gg + coord_trans(x="log10", y="log10")
gg <- gg + facet_wrap(~Area, scales = "free")
gg


df_mod <- subset(df_plot, Item == "Oilcrops")
df_mod$lArea <- log(df_mod$Area)
df_mod$lRevYd <- log(df_mod$RevYd)
mod <- lm(lArea ~ lRevYd, df_mod)
summary(mod)


#============================================================================
#============================================================================
#============================================================================
#============================================================================

GDP_targ <- 10^6 * runif(1)
A_targ <- 10^7 * runif(1)
targ_vec <- c(GDP_targ, A_targ)
n <- 5
r <- 10^2 * runif(n)
ones <- rep(1, n)
Q <- matrix(runif(n^2), n, n)
S <- t(Q) %*% Q
S_inv <- solve(S)

mat_nab <- cbind(r, ones)

M <- t(mat_nab) %*% S_inv %*% mat_nab 
M_inv <- solve(M)

outvec <- 2 * M_inv %*% targ_vec

l_s <- 1 / outvec[1]
l_A <- -l_s * outvec[2]

l_s

aStar <- 1 / 2 * S_inv %*% mat_nab %*% outvec
aStar



sum(aStar)
