Laboration 3
================

Från SCBs statistik kan vi skapa en tabell som innehåller alla svenska
kommuner och deras folkmängd varje år sen 1968. Från början står
kommunens namn och kod i samma kolumn och jag delar upp dem för att
enklare kunna jobba tabeller som bara innehåller antingen kod eller
namn.

``` r
kommun_data <- read_csv("data/kommun_pop_2022-06-15.csv") %>% separate(col=region, into=c("code","name"),sep=" ", extra="merge")
kommun_data$code <- as.numeric(kommun_data$code)
colnames(kommun_data) <- c("Kod", "Kommun", "År", "Folkmängd")
```

Från en tabell som innehöll mäns inkomst, kvinnors inkomst, antal
arbetande män och antal arbetande kvinnor skapade jag en tabell med
varje kommuns medelinkomst i tusentals kronor. Jag skapade först två
tabeller för att kommunnamn som innehöll mellanslag annars blev två
olika kolumner.

Genom att gruppera efter år och kommun kunde männen och kvinnorna och
deras inkomster adderas för att räkna ut hela kommunens medelinkomst.
Medelinkomsten ändras oc´kså från miljontals kronor per år till
tusentals kronor per månad.

``` r
inkomsta <- na.omit(read_table2("data/inkomst_2022-06-15.csv",col_names = FALSE, locale = locale(encoding = "WINDOWS-1252"),skip = 1)) %>%
  select(X1, X9, X10 ,X11) %>%
  group_by(X1, X9) %>%
  summarise(Inkomst= (sum(X10)/sum(X11))*(1000/12)) %>%
  ungroup() %>% 
  mutate(Kod = parse_number(X1)) %>%
  mutate(År = parse_number(X9)) %>%
  select(Kod, År, Inkomst)

  
inkomstb <- na.omit(read_table2("data/inkomst_2022-06-15.csv",col_names = FALSE, locale = locale(encoding = "WINDOWS-1252"),skip = 45)) %>%
  select(X1, X8, X9, X10) %>%
  group_by(X1, X8) %>%
  summarise(Inkomst= (sum(X9)/sum(X10))*(1000/12)) %>%
  ungroup() %>% 
  mutate(Kod = parse_number(X1)) %>%
  mutate(År = parse_number(X8)) %>%
  select(Kod, År, Inkomst)

kommun_medelinkomst <- full_join(inkomsta, inkomstb)
```

Vi har en tabell som innehåller antalet invånare med svensk bakgrund och
antalet invånare med utländsk bakgrund. Från den skapar jag en tabell
med andelen invånare med utländsk bakgrund. Datan behandlas för att få
en tabell där kommun och år listas radvis för att kunna användas ihop
med de andra tabellerna.

``` r
harkomst <- read_csv("data/utl_harkomst_2022-06-15.csv", locale = locale(encoding = "WINDOWS-1252"),skip = 1) %>%
  pivot_longer(c(`2002`,`2003`,`2004`,`2005`,`2006`,`2007`,`2008`,`2009`,`2010`,`2011`,`2012`,`2013`,`2014`,`2015`,`2016`,`2017`,`2018`,`2019`,`2020`,`2021`), names_to = 'År') %>%
  pivot_wider(names_from = `utländsk/svensk bakgrund`, values_from = value) %>%
  mutate(Andel=(`utländsk bakgrund`/(`utländsk bakgrund`+`svensk bakgrund`))*100) %>%
  select(region, `År`, Andel) %>%
  separate(col=region, into=c("Kod","Kommun"),sep=" ", extra="merge") %>%
  mutate ("Kod" = parse_number(Kod)) %>%
  select ("Kod", "År", "Andel")
```

Jag läser in en tabell med antal våldsbrott per 100000 invånare.

``` r
brott <- readxl::read_excel("data/Kolada_vldsbrott_2022-06-15.xlsx", skip = 1)[-c(1),] %>%
  pivot_longer(c(`2000`,`2001`,`2002`,`2003`,`2004`,`2005`,`2006`,`2007`,`2008`,`2009`,`2010`,`2011`,`2012`,`2013`,`2014`,`2015`,`2016`,`2017`,`2018`,`2019`,`2020`,`2021`) ,names_to = 'year') %>%
  select(`Område`, year, value)
colnames(brott)=c("Kommun", "År", "Våldsbrott")
```

När alla tabeller listar informationen efter kommun och år kan jag slå
ihop tabellerna. Sedan behåller jag bara de kommuner som ligger i
Värmland.

``` r
Värmland <- c(1715, 1730, 1737, 1760, 1761, 1762, 1763, 1764, 1765, 1766, 1780, 1781, 1782, 1783, 1784, 1785)

kommun_data <- kommun_data %>% 
  merge(kommun_medelinkomst, by = c("Kod", "År")) %>% 
  merge(harkomst, by = c("Kod", "År")) %>%
  merge(brott, by = c("Kommun", "År")) %>%
  filter(Kod %in% Värmland)
```

Med det här diagrammet kan vi undersöka om det finns ett samband mellan
medelinkomst och hur många våldsbrott som begås. Här ser vi inget
tydligt samband.

``` r
ggplot(filter(kommun_data, År == 2002)) +
  geom_point(aes(x = Inkomst, y = Våldsbrott, size = Folkmängd))
```

![](laboration3_files/figure-gfm/unnamed-chunk-6-1.png)<!-- -->

Här kan vi se om medelinkomster hänger ihop med andelen utrikes födda i
en kommun. Här är det också svårt att se ett samband.

``` r
ggplot(filter(kommun_data, År == 2002)) +
  geom_point(aes(x = Inkomst, y = Andel, size = Folkmängd))
```

![](laboration3_files/figure-gfm/unnamed-chunk-7-1.png)<!-- -->

Här visas alla kommuner i Värmland och deras medelinkomster. Inkomsterna
är högst i Hammarö och Karlstad och dess omgivning.

``` r
karta <- st_read("data/KommunSweref99TM/Kommun_Sweref99TM_region.shp",quiet = TRUE)
colnames(karta)=c("Kod","Namn","geometry")
karta <-karta %>%
  filter(Kod %in% Värmland) %>%
  mutate ("Kod" = parse_number(Kod)) %>%
  merge(filter(kommun_data, År == 2002), by = "Kod")
ggplot(karta) + 
  geom_sf(aes(fill = Inkomst)) + coord_sf() + 
  geom_sf_text(aes(label=Namn)) 
```

![](laboration3_files/figure-gfm/unnamed-chunk-8-1.png)<!-- -->

``` r
Line <- read_csv("data/Line.csv")
LinePlatform <- read_csv("data/LinePlatform.csv")
Platform <- read_csv("data/Platform.csv")
Stockholms_Tunnelbana <- left_join(LinePlatform, Line, by="LineNumber") 
Stockholms_Tunnelbana <- left_join(Stockholms_Tunnelbana, Platform, by = "PlatformNumber")
Linjer <- distinct(Stockholms_Tunnelbana, StationName, LineName) %>%
  count(StationName) %>% 
  filter(n>1)
Plattformar <- distinct(Stockholms_Tunnelbana, LineName, PlatformNumber) %>%
  dplyr::count(LineName)
Plattformar
```

    ## # A tibble: 3 × 2
    ##   LineName                     n
    ##   <chr>                    <int>
    ## 1 tunnelbanans blå linje      45
    ## 2 tunnelbanans gröna linje   107
    ## 3 tunnelbanans röda linje     77
