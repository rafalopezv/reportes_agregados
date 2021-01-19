
# sobre: compilación de ciclos

library(highcharter)
library(tidyverse)
library(magrittr)
library(reactable)

bases <- list.files(recursive = T)
bases <- bases[grep("FRR/ciclo_", bases)]
bases <- bases[grep("xlsx", bases)]
bases <- bases[grep("Pulsante_ todas las propuestas FRR 2020", bases, invert = T)]

# ordenar
bases <- bases[c(1, 8, 9:15, 2:7)]

datos1 <- rio::import_list(bases[1], rbind = T)
datos2 <- rio::import_list(bases[2], rbind = T)
datos3 <- map(bases[3:15], rio::import) %>% map(., janitor::clean_names)
datos <- list(datos1, datos2)
datos <- c(datos, datos3)


# numero de proyectos
tibble(
  Ciclo = paste0(rep("Ciclo ", 15), seq(1,15, 1)),
  Postulaciones = map_int(datos, nrow)
) %>% 
  hchart("line", hcaes(Ciclo, Postulaciones)) %>% 
  hc_add_theme(hc_theme_smpl()) %>% 
  hc_chart(style = list(fontFamily = "Open Sans")) %>% 
  hc_plotOptions(line = list(
    lineWidth = 6,
    connectNulls = F,
    animation = list(
      duration = 3000 
    ),
    marker = list(
      enabled = T
    ),
    dataLabels = list(
      enabled = T,
      format = "{point.Postulaciones:.0f}"
    ))
  ) %>% 
  hc_tooltip(enabled = T, borderWidth = 0.01, 
             pointFormat=paste("
                               Ciclo: <b>{point.Ciclo}</b><br>
                               Postulaciones: <b>{point.Postulaciones}</b><br>")) -> numero_portulaciones

# ggplot : numero de postulaciones
tibble(
  Ciclo = paste0(rep("Ciclo ", 15), seq(1,15, 1)),
  Postulaciones = map_int(datos, nrow)
) %>%
  ggplot(aes(Ciclo, Postulaciones)) +
  geom_line(color = "red", group = 1, size = 2) + 
  geom_point(color = "red", size = 3) + 
  geom_text(aes(label = Postulaciones), family = "Open Sans", vjust = -0.5) + 
  hrbrthemes::theme_ipsum_rc() + 
  labs( 
    title = "15 Ciclos, 169 postulaciones",
    subtitle = "Fondo de Respuesta Rápida"
  ) + 
  ggsave(here::here("numero_postulaciones.jpg"), width = 10, height = 6)


# de donde viene
map(datos[3:15], 14) -> temp

for(i in 1:length(temp)) {
  temp[[i]] %<>% as.data.frame()
  temp[[i]] %<>% mutate(ciclo = i + 2)
}

temp %>% 
  bind_rows() %>% 
  separate(".", into = letters[1:15], sep = ",") %>% 
  janitor::remove_empty() %>% 
  gather(key, value, -ciclo) %>% 
  filter(!is.na(value))  %>% 
  select(-key) -> temp



datos[[1]]$`País(es)` %>% 
  as.data.frame() %>% 
  separate(".", into = letters[1:15], sep = ",") %>% 
  janitor::remove_empty() %>% 
  gather() %>% 
  filter(!is.na(value)) %>% 
  separate("value", into = letters[1:15], sep = " y ") %>% 
  janitor::remove_empty() %>% 
  select(-key) %>% 
  gather() %>% 
  filter(!is.na(value)) %>% 
  mutate(
    ciclo = 1, 
    value = trimws(value)
  ) %>% 
  select(-key) %>% 
  bind_rows(temp, .) -> temp



datos[[2]]$`7. Menciona el país(es) donde se realizará el proyecto` %>% 
  as.data.frame() %>% 
  separate(".", into = letters[1:15], sep = ",") %>% 
  janitor::remove_empty() %>% 
  gather() %>% 
  filter(!is.na(value)) %>% 
  separate("value", into = letters[1:15], sep = " y ") %>% 
  janitor::remove_empty() %>% 
  select(-key) %>% 
  gather() %>% 
  filter(!is.na(value)) %>% 
  mutate(
    ciclo = 2, 
    value = trimws(value)
  ) %>% 
  select(-key) %>% 
  bind_rows(temp, .) -> temp

mapa <- sf::st_read("input/latam.json")

temp$value %>% unique -> nombres
nombres[!nombres %in% mapa$name] 

temp$value %<>% gsub("é", "e", .)
temp$value %<>% gsub("á", "a", .)
temp$value %<>% gsub("í", "i", .)
temp$value %<>% gsub("ó", "o", .)
temp$value %<>% gsub("ú", "u", .)

temp$value %>% unique -> nombres
nombres[!nombres %>% unique %in% mapa$name] 

temp$value %<>% gsub("Brasil", "Brazil", .)
temp$value %<>% gsub("COLOMBIA", "Colombia", .)
temp$value %<>% gsub("Republica Dominicana", "Dominican Rep.", .)
temp$value %<>% gsub("España", "Spain", .)
temp$value %<>% gsub("Oaxaca", "Mexico", .)
temp$value %<>% gsub("La sede central desde donde se ejecutara el proyecto es la ciudad de Lima-Peru", "Peru", .)
temp$value %<>% gsub("ARGENTINA", "Argentina", .)
temp$value %<>% gsub("colombia", "Colombia", .)
temp$value %<>% gsub("Yajalon", "Mexico", .)
temp$value %<>% gsub("10 regiones del Peru", "Peru", .)
temp$value %<>% gsub("BOLIVIA. El proyecto de realizara en los municipios de Santa Rosa de Yacuma", "Bolivia", .)
temp$value %<>% gsub("Mexico\\.", "Mexico", .)
temp$value %<>% gsub("correspondiente al Estado de Chiapas en el pais de Mexico", "Mexico", .)
temp$value %<>% gsub("tendra alcance en todo el territorio peruano\\.", "Peru", .)
temp$value %<>% gsub("Francia", "France", .)
temp$value %<>% gsub("Reyes de la provincia Ballivian del departamento de Beni\\.", "Bolivia", .)
temp$value %<>% gsub("En el municipio de Mexico", "Mexico", .)
temp$value %<>% gsub("para Colombia", "Colombia", .)
temp$value %<>% gsub("com posibilidades de envio das mulheres", "Brazil", .)
temp$value %<>% gsub("cuja situação não se enquadra nas exceções legais brasileiras da criminalização do aborto", "Brazil", .)


temp %>% 
  count(value) %>% 
  rename(
    name = value, 
    proyectos = n
  ) -> temp1

mapa %<>% merge(., temp1, all.x = T)

ggplot(mapa) +
  geom_sf(aes(fill = proyectos), color = "black", size = 0.01) + 
  coord_sf(
    xlim = c(-140, 0)
  ) +
  scale_fill_viridis_c(na.value = "#dcdcdc", option = "A", direction = -1, begin = 0.2, end = 0.9) + 
  ggthemes::theme_map(base_family = "Open Sans") +
  theme(
    legend.position = "bottom", 
    legend.title = element_text(size = 7, family = "Roboto Condensed Light")
  ) + 
  labs( 
    title = "8 Ciclos: ¿Dónde se ejecutan las propuestas?",
    subtitle = "Fondo de Respuesta Rápida",
    caption = "Se excluyen proeyctos fuera de América Latina"
  ) + 
  ggsave(here::here("donde_postulaciones_total.jpg"), width = 10, height = 8)


# por ciclo 
temp %>% 
  group_by(ciclo) %>% 
  count(value) %>% 
  rename(
    name = value, 
    proyectos = n
  ) -> temp1

mapa <- sf::st_read("input/latam.json")

mapa %<>% merge(., temp1, all.x = T)

# mapa
mapa <- jsonlite::fromJSON("input/latam.json", simplifyVector = F)


temp %>% 
  count(value) %>% 
  rename(
    name = value, 
    value = n
  ) -> temp1


highchart(type = "map") %>%
  hc_add_series(mapData = mapa, data = temp1, value = "value", 
                joinBy = "name", borderColor = "transparent") %>% 
  hc_colorAxis(stops = color_stops(colors = viridisLite::viridis(10, option = "A", direction = -1, begin = 0.2, end = 0.9))) %>% 
  hc_tooltip(enabled = T, borderWidth = 0.01, 
             pointFormat=paste("
                               País: <b>{point.name}</b><br>
                               Postulaciones: <b>{point.value}</b><br>")) %>% 
  hc_chart(style = list(fontFamily = "Open Sans")) -> donde_postulaciones
  


# animado por ciclo
temp %>% 
  group_by(ciclo, value) %>% 
  count() %>% 
  ungroup() %>% 
  rename(
    `País` = value,
    postulaciones = n
  ) -> temp_1

mapa <- sf::st_read("input/latam.json")

temp_1 %>% 
  filter(`País` %in% mapa$name) %>% 
  filter(`País` != "Canada" ) %>% 
  hchart("column", hcaes(`País`, postulaciones, group = ciclo)) %>% 
  hc_chart(style = list(fontFamily = "Open Sans")) -> numero_portulaciones_ciclo


temp_1 %>% 
  filter(`País` %in% mapa$name) %>% 
  filter(`País` != "Canada" ) %>% 
  mutate(ciclo1 = paste0("Ciclo: ", ciclo)) %>% 
  ggplot(aes(`País`, postulaciones)) + 
  geom_col() + 
  facet_wrap(~ciclo1, scales = "free", ncol = 2) + 
  hrbrthemes::theme_ipsum_rc(base_family = "Open Sans") + 
  theme(
    axis.text.x = element_text(size  = 8)
  ) + 
  coord_flip() + 
  ggsave("ciclos.jpg", height = 12)



# temas.
temas <- map(datos, "tema") %>% unlist() %>% as.data.frame() %>% 
  rename(tema = ".")

read_csv("input/temas.csv") %>% 
  select(tema = Tema) %>% 
  bind_rows(., temas) %>% 
  count(tema) %>% 
  arrange(desc(n)) %>% 
  remove_missing() %>% 
  mutate(prop = prop.table(n)*100) -> temp1


lvl_opts <-  list(
  level = 1,
  borderWidth = 0,
  borderColor = "transparent",
  dataLabels = list(
    enabled = TRUE,
    align = "left",
    verticalAlign = "top",
    style = list(fontSize = "13px", textOutline = FALSE, color = "black")
  )
)

cols <- rep(c("#E01F52", "#C6A659", "#06D6A0", "#466B77", "#073B4C", 
              "#D8B970", "#5FA1B7", "#118AB2", "#BAAB89", "#1E4D5C", 
              "#2C6E49", "#E07A5F", "#3D405B", "#81B29A", "#63585F", 
              "#B4B5BA", "#261F23", "#575D7C", "#9194C6", "#75184D"), 3)


cols <- cols[1:47]




hchart(
  data_to_hierarchical(temp1, tema, prop, colors = cols),
  type = "treemap",
  levelIsConstant = T,
  allowDrillToNode = T,
  borderWidth = 0,
  drillUpButton = list(
    text = "< Volver"
  ),
  levels = lvl_opts,
  tooltip = list(valueDecimals = 2, style = list(fontFamily = "Open Sans")) 
) %>% 
  hc_chart(
    style = list(fontFamily = "Open Sans")
  ) -> temas

temp1 %>%
  mutate_if(is.numeric, round, 1) %>% 
  mutate(
    etiqueta = paste0(tema, ":", "\n", prop, "%")
  ) -> aa
  
library(treemapify)

aa %>% 
  ggplot(aes(area = prop, label = etiqueta, fill = n)) +
  geom_treemap(color = "white", start = "topleft") +
  geom_treemap_text(min.size = 1, place = "center", family = "Open Sans", color = "white", start = "topleft") +
  #scale_fill_gradientn(colors = c("blue","green3")) +
  theme(legend.position = "NA") +
  theme(text = element_text(family = "Open Sans")) + 
  scale_fill_viridis_c(option = "A", direction = -1, begin = 0.1, end = 0.7) +
  ggsave("temas.jpg", width = 12, height = 10)
  

map_dfr(datos[3:15], extract, c(7, 14, 19, 21)) -> temp

temp %<>% select(-5, -6)
colnames(temp) <- c("Organización lider", "País", "Tema", "Resumen del proyecto")

temp %>% 
  reactable(searchable = TRUE,
            striped = TRUE,
            highlight = TRUE,
            bordered = TRUE,
            theme = reactableTheme(
              borderColor = "#dfe2e5",
              stripedColor = "#f6f8fa",
              highlightColor = "#f0f5f9",
              cellPadding = "8px 12px",
              style = list(
                fontFamily = "Open Sans",
                fontSize = "14px")
            )
  ) -> tabla




# cada ciclo por país
map(datos[3:15], 14) -> aa

stream <- function(.data) {
  .data %<>% 
    as.data.frame() %>% 
    rename(pais = ".") %>% 
    separate(col = pais, sep = ",", into = letters[1:15]) %>% 
    janitor::remove_empty() %>% 
    gather() %>% 
    filter(!is.na(value)) %>% 
    count(value)
  
  return(.data)
}

map(aa, stream) -> aa

for(i in 1:length(aa)) {
  aa[[i]] %<>% mutate(Ciclo = i + 2)
}


temp <- datos[[1]]$`País(es)` 
  
temp %<>% 
  as.data.frame() %>% 
  rename(value = ".") 
  
temp$value %<>% gsub("Brasil, com posibilidades de envio das mulheres, cuja situação não se enquadra nas exceções legais brasileiras da criminalização do aborto, para Colombia, Argentina e México", "Brasil", .)

temp %<>% 
  separate(col = value, sep = ",", into = letters[1:15]) %>% 
  janitor::remove_empty() %>% 
  gather() %>% 
  filter(!is.na(value)) %>% 
  count(value) %>% 
  mutate(value = trimws(value)) %>% 
  count(value) %>% 
  filter(!value %in% c("Zimbabwe", "India", "DRC", "Latinoamérica", "Cameroon", "México y España", 
                       "Nicaragua y Uruguay", "Paraguay y Perú"))

temp$value %<>% gsub("COLOMBIA", "Colombia", .) 
temp[temp$value == "México", "n"] <- 4
temp[temp$value == "Nicaragua", "n"] <- 4

tibble(
  value = c("Uruguay", "Paraguay", "Perú"), 
  n = 1
) %>% 
  bind_rows(., temp) %>% 
  count(value) %>% 
  mutate(Ciclo = 1) -> temp
  

temp1 <- datos[[2]]$`7. Menciona el país(es) donde se realizará el proyecto`

temp1 %<>% 
  as.data.frame() %>% 
  rename(value = ".") 

temp1$value %<>% gsub("Oaxaca, México","México", .)
temp1$value %<>% gsub("La sede central desde donde se ejecutará el proyecto es la ciudad de Lima-Perú, pero al ser una propuesta que se realizará de manera virtual, tendrá alcance en todo el territorio peruano.","Perú", .)
temp1$value %<>% gsub("En el municipio de Yajalon, correspondiente al Estado de Chiapas en el país de México","México", .)
temp1$value %<>% gsub("Chile, en todas sus regiones.","Chile", .)
temp1$value %<>% gsub("10 regiones del Perú", "Perú", .)
temp1$value %<>% gsub("BOLIVIA. El proyecto de realizará en los municipios de Santa Rosa de Yacuma y Reyes de la provincia Ballivian del departamento de Beni.", "Bolivia", .)


temp1 %<>% 
  separate(col = value, sep = ",", into = letters[1:15]) %>% 
  janitor::remove_empty() %>% 
  gather() %>% 
  filter(!is.na(value)) %>% 
  count(value) %>% 
  mutate(value = trimws(value)) %>% 
  count(value)


temp1$value %<>% gsub("ARGENTINA", "Argentina", .) 
temp1$value %<>% gsub("colombia", "Colombia", .) 
temp1$value %<>% gsub("México.", "México", .)
temp1$value %<>% gsub("Peru", "Perú", .)

temp1 %<>% 
  filter(!value %in% c("Canadá", "Estados Unidos", "Francia", "Inglaterra", "Noruega"))

temp1 %>% 
  count(value) %>% 
  mutate(Ciclo = 2) %>% 
  bind_rows(., temp) -> temp

bind_rows(aa) %>% 
  bind_rows(., temp) -> temp 


temp %<>% arrange(Ciclo)

temp$value %>% unique %>% view

temp$value %<>% gsub("Bolívia", "Bolivia", .) 
temp$value %<>% gsub("Brazil", "Brasil", .) 
temp$value %<>% gsub("Haiti", "Haití", .) 
temp$value %<>% gsub("Peru", "Perú", .) 


temp %>% 
  group_by(value, Ciclo) %>% 
  summarise(n = sum(n)) -> temp
  

hchart(temp, "streamgraph", hcaes(Ciclo, n, group = value),
       label = list(
         enabled = TRUE, minFontSize = 5, maxFontSize = 20,
         style = list(
           fontWeight = 70,
           textOutline = "0.5px gray",
           color = "white"
         )
       )
) %>% 
  hc_tooltip(shared = T, table = T, sort = T, borderWidth = 0.001,
             style = list(fontFamily = "Open Sans"), backgroundColor = "white") %>% 
  hc_yAxis(visible = F) %>% 
  hc_xAxis(visible = T) %>% 
  hc_chart(style = list(fontFamily = "Open Sans")) %>% 
  hc_plotOptions(
    series = list(
      marker = list(radius = 3, enabled = FALSE, symbol = "circle"),
      states = list(hover = list(halo = list(size = 1)))
    )
  ) %>% 
  hc_size(height = 800) %>% 
  hc_legend(layout = "horizontal") -> rio_tipo







