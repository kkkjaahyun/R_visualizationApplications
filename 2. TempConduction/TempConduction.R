getwd()
setwd('./TempConduction')
TempConduction = read.csv('TempConduction.csv')
head(TempConduction)
str(TempConduction)

library(tidyverse)


#range값 구하기 for normalization
CuDiff = diff(range(TempConduction$Cu))
GlassDiff = diff(range(TempConduction$Glass))
FeDiff = diff(range(TempConduction$Fe))
Cu.CarbonDiff = diff(range(TempConduction$Cu.Carbon))
Glass.CarbonDiff = diff(range(TempConduction$Glass.Carbon))
Fe.CarbonDiff = diff(range(TempConduction$Fe.Carbon))

---
TempConductionN = bind_cols(TempConduction$Cu/CuDiff,TempConduction$Glass/GlassDiff,
                            TempConduction$Fe/FeDiff,TempConduction$Cu.Carbon/Cu.CarbonDiff,
                            TempConduction$Glass.Carbon/Glass.CarbonDiff,
                            TempConduction$Fe.Carbon/Fe.CarbonDiff)

TempConductionN %>% rename('CuN' = '...1','GlassN'= '...2','FeN'= '...3',
                           'Cu.CarbonN'= '...4','Glass.CarbonN'= '...5',
                           'Fe.CarbonN'= '...6') -> TempConductionN
TempConductionN$No = c(1:162)

#wide to long type

TempConductionN %>% pivot_longer(c(CuN:Fe.CarbonN),names_to = 'materials',
                                 values_to = 'temp')->TidyTempConN

ggplot(TidyTempConN) + geom_line(aes(x = No, y = temp, color = materials)) +
  coord_cartesian(xlim = c(140,160)) + 
  geom_vline(xintercept = 154, linetype = 'dotted', color = 'red') +
  theme_classic()

# plot을 그려보니, 160행부터 156, 157행까지 온도가 줄어들다가, 154행에서 온도가 정점을 찍는다.
# 해당 실험 열 화상 이미지를 확인해봤을 때, hotplate와 막대가 닿는 곳에서 온도가 다시 올라가는 것을 확인했다.
# 결과분석에서는 막대의 온도분포를 확인해야하기 때문에 154행부터 1행까지의 결과만 활용하는 것이 합당하다고 판단했다.

# TempConduction의 154 다음 행 부터 제거 + No를 길이로 변경 + 후 다시 normalize

TempConduction = TempConduction[-155:-162,]

#range값 구하기 for normalization
CuDiff = diff(range(TempConduction$Cu))
GlassDiff = diff(range(TempConduction$Glass))
FeDiff = diff(range(TempConduction$Fe))
Cu.CarbonDiff = diff(range(TempConduction$Cu.Carbon))
Glass.CarbonDiff = diff(range(TempConduction$Glass.Carbon))
Fe.CarbonDiff = diff(range(TempConduction$Fe.Carbon))

TempConductionN2 = bind_cols(TempConduction$Cu/CuDiff,
                             TempConduction$Glass/GlassDiff,
                             TempConduction$Fe/FeDiff,
                             TempConduction$Cu.Carbon/Cu.CarbonDiff,
                             TempConduction$Glass.Carbon/Glass.CarbonDiff,
                             TempConduction$Fe.Carbon/Fe.CarbonDiff)

TempConductionN2 %>% rename('CuN' = '...1','GlassN'= '...2','FeN'= '...3',
                           'Cu.CarbonN'= '...4','Glass.CarbonN'= '...5',
                           'Fe.CarbonN'= '...6') -> TempConductionN2

TempConductionN2$length = rev(c(1:154))
TempConductionN2$length = (300/154)*TempConductionN2$length 

TempConductionN2 %>% pivot_longer(c(CuN:Fe.CarbonN),names_to = 'materials',
                                 values_to = 'temp')->TidyTempConN2

ggplot(TidyTempConN2) + geom_line(aes(x = length, y = temp, color = materials)) +
  ylab('Normalized Temparature') +
  xlab('Length(mm)') +
  labs(title = 'Length - Normalized Temparature', caption = 'length는 hot plate를 기준으로 막대 축방향 거리' ) +
  #coord_cartesian(xlim = c(140,160)) + 
  theme_classic(base_family = 'NanumGothic')

## Thermal Conductivity 계산

# length를 meter 로 단위변환
TempConduction$Length = TempConductionN2$length*10^(-3)
# Temp 단위를 kelvin으로 변환(celcius +273.15)
TempConduction$CuK = TempConduction$Cu + 273.15
TempConduction$GlassK = TempConduction$Glass + 273.15
TempConduction$FeK = TempConduction$Fe + 273.15

TempConduction$Cu.CarbonK = TempConduction$Cu.Carbon + 273.15
TempConduction$Glass.CarbonK = TempConduction$Glass.Carbon + 273.15
TempConduction$Fe.CarbonK = TempConduction$Fe.Carbon + 273.15




#Cu
TempConduction$CuThermalC = 0.01257*25/
  (2*0.00001257*TempConduction$CuK)*
  (TempConduction$CuK - 293.35) * 
  (TempConduction$Length)^2

#Glass
TempConduction$GlassThermalC = 0.01257*25/
  (2*0.00001257*TempConduction$GlassK)*
  (TempConduction$GlassK - 293.35) * 
  (TempConduction$Length)^2

#Fe
TempConduction$FeThermalC = 0.01257*25/
  (2*0.00001257*TempConduction$FeK)*
  (TempConduction$FeK - 293.35) * 
  (TempConduction$Length)^2

#Cu.Carbon
TempConduction$Cu.CarbonThermalC = 0.01257*25/
  (2*0.00001257*TempConduction$Cu.CarbonK)*
  (TempConduction$Cu.CarbonK - 293.35) * 
  (TempConduction$Length)^2

#Glass.Carbon
TempConduction$Glass.CarbonThermalC = 0.01257*25/
  (2*0.00001257*TempConduction$Glass.CarbonK)*
  (TempConduction$Glass.CarbonK - 293.35) * 
  (TempConduction$Length)^2

#Fe.Carbon
TempConduction$Fe.CarbonThermalC = 0.01257*25/
  (2*0.00001257*TempConduction$Fe.CarbonK)*
  (TempConduction$Fe.CarbonK - 293.35) * 
  (TempConduction$Length)^2

mean(TempConduction$CuThermalC)
# 구리 1.993109 실제 열전도도 385 
#방사율 보정
1.993109 * 1/0.04
# 49.82773

mean(TempConduction$GlassThermalC)
# 유리 2.080402 실제 열전도도 1
# 방사율 보정 
2.080402 * 1/0.95
# 2.189897
mean(TempConduction$FeThermalC)
# 철 2.560583 실제 열전도도 79.5
# 방사율 보정 
2.560583 * 1/0.06
# 42.67638

#그럼에도 오차가 많이 난다. -> 열적 평형 이뤄지지 않아서 충분히 열이 전파되지 않은 상황일수 있다.

#Carbon묻힌건?

mean(TempConduction$Cu.CarbonThermalC)
# 2.731238
mean(TempConduction$Glass.CarbonThermalC)
# 2.278669
mean(TempConduction$Fe.CarbonThermalC)
# 3.69476

# wrap up
1.993109
2.080402
2.560583
2.731238
2.278669
3.69476

---
# 일부 데이터 추출 : length, 구리, 유리, 철
  
library(tidyverse)
TempC = tibble(Length = rev(TempConduction$Length), Cu = rev(TempConduction$Cu),
       Glass = rev(TempConduction$Glass), Fe = rev(TempConduction$Fe) )

head(TempC)

TempC %>% pivot_longer(c(Cu:Fe), names_to = 'materials',values_to = 'temp') ->
  TempCtidy

head(TempCtidy)
  