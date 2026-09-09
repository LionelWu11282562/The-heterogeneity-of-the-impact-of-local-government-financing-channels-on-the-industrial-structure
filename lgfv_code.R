###### I. Import the Packages and Data######
library("readxl")
library("fixest")
library("dplyr")
library("psych")
library("ggplot2")
train_df <- read_excel("C:/Users/hp/Desktop/城投债/数据/LGFV_bonds.xlsx",
                        sheet=2)
df <- read_excel("C:/Users/hp/Desktop/城投债/数据/LGFV_bonds.xlsx",
                 sheet=1)

###### II. Run the Regression to Train the Weights######

#define the average average gdp
train_df$gdp_per_person <- train_df$gdp/train_df$population

#run the regression
train_weight <- feols(gdp_per_person~second_share+tertiary_share|year,
      data=train_df,cluster=~province)

summary(train_weight)

#output the coefficients
weights <- coef(train_weight)
weights <- weights/weights[2]

###### III. Define the Key Variables######
#ISS: Index of Industrial Upgrade
df$IIS <- df$second_share*weights[1]+
  df$tertiary_share*weights[2]

#DSI: Debt Structure Index
df$DSI <- log(df$LGFV_bond)-log(df$statutory_debt)

#rDebt: Ratio of the debts
df$rDebt <- (df$statutory_debt+df$LGFV_bond)/df$gdp

#rtrade: The ratio of the international trades
df$rtrade <- df$trade/df$gdp

#view the data
summary_df <- data.frame("IIS"=df$IIS,
                         "lnLGFV"=log(df$LGFV_bond),
                         "lnStatu"=log(df$statutory_debt),
                         "DSI"=df$DSI,
                         "rDebt",df$rDebt,
                         "lnPopulation"=log(df$population),
                         "rtrade"=df$rtrade
)

describe(summary_df)

###### IV. Run the Basic Regressions######
#model.1: IIS ~ statutory_debt+LFGV_bond
lm.1  <- feols(IIS~log(statutory_debt)+log(LGFV_bond)|year+province,
               data=df,cluster=~province)

summary(lm.1)

#model. IIS ~ rDebt
lm.2 <- feols(IIS~rDebt|year+province,
              data=df,cluster=~province)

summary(lm.2)

#model.3 IIS ~ DSI
lm.3 <- feols(IIS~DSI|year+province,
              data=df,cluster=~province)

summary(lm.3)

###### V. Run the Main Regression######

#add controls one by one

# main model.1: Add rDebt as control
lm.main.1 <- feols(IIS~DSI+rDebt|year+province,
                 data=df,cluster=~province)

summary(lm.main.1)

# main model.2: Add log(population) as control
lm.main.2 <- feols(IIS~DSI+rDebt+log(population)|year+province,
                 data=df,cluster=~province)

summary(lm.main.2)

lm.main <- feols(IIS~DSI+rDebt+log(population)+rtrade|province+year,
                 data=df,cluster=~province)

summary(lm.main)

###### VI. Check the Robustness######

# Robust model.1: Replace the response variable
# Robust model.1: Use log(tertiary_share/(1-tertiary_share)) as response variable
rlm.1 <- feols(log(tertiary_share/(1-tertiary_share))~DSI+rDebt+
                 log(population)+rtrade|province+year,
               data=df,cluster=~province)

summary(rlm.1)

# Robust model.1.alt: Use Use log(second_share/(1-second_share)) as response variable
rlm.1.alt <- feols(log(second_share/(1-second_share))~DSI+rDebt+
                     log(population)+rtrade|province+year,
                   data=df,cluster=~province)

summary(rlm.1.alt)

# Robust model.2: Change the main explain variable
# Robust model.2: Change the DSI with LGFV_bond/(LGFV_bond+statutary_debt)
# define the DSI_alt
df$DSI_alt <- df$LGFV_bond/(df$LGFV_bond+df$statutory_debt)

rlm.2 <- feols(IIS~DSI_alt+rDebt+log(population)+rtrade|province+year,
                 data=df,cluster=~province)

summary(rlm.2)


# Robust model.3: Delete the municipality
# delete the municipality data in df
municipality <- which(df$province=="Beijing"|
                        df$province=="Chongqing"|
                        df$province=="Shanghai"|
                        df$province=="Tianjin")

df_province <- df[-municipality,]

# Run the regression again
rlm.3 <- feols(IIS~DSI+rDebt+log(population)+rtrade|year+province,
               data=df_province,cluster=~province)

summary(rlm.3)

# Robust model.4: Lag model
rlm.4 <- feols(
  IIS ~ l(DSI,1)+l(rDebt,1)+log(population)+rtrade|province + year,
  data=df,
  panel.id=~ province + year,
  cluster=~province
)

summary(rlm.4)

###### VII. Model Diagnosis######

# output the residual plot
ggplot(data=data.frame(x=fitted(lm.main),y=resid(lm.main)),
       aes(x,y))+geom_point(col="steelblue")+
  geom_hline(yintercept=0,col="red",lwd=1.5)+
  labs(x="Fitted Values",y="Residuals",title="Residuals vs Fitted")+
  theme_classic()

# check why the residuals high since the fitted values are small
diag_df <- df %>%
  mutate(
    fitted = fitted(lm.main),
    residual = resid(lm.main)
  ) %>%
  arrange(desc(abs(residual)))

head(
  diag_df %>%
    select(province, year, IIS, DSI, rDebt, fitted, residual),
  10
)

# output the qqplot 
ggplot(data.frame(sample=resid(lm.main)),aes(sample=sample))+
  stat_qq_line(lwd=1.5,col="red")+stat_qq(size=1,col="steelblue")+
  labs(x="Theretical Quantiles",y="Residual Quantiles",title="QQplot")+
  theme_classic()








