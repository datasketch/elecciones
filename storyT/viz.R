library(hgchmagic)
library(htmlwidgets)

colores <- c("#fdb731","#0a446b", "#137fc0", "#c250c2", "#fa8223", "#64c6f2", "#f49bf9", "#fc7e5b", "#ffe566", "#64f4c8", "#137fc0", "#c250c2", "#f03a47", "#fdb731", "#36c16f", "#022f40", "#691f6b", "#931c4d", "#fa8223", "#2b6d46")

opts_viz <- list(#title = titulo(),
  caption = '<b>Fuente:</b> SECOP y cuentas claras',
  orientation = 'hor',
  allow_point = TRUE,
  cursor =  'pointer',
  color_hover = "#fa8223",
  color_click  = "#fa8223",
  labelWrap = 100,
  labelWrapV = c(100, 100),
  startAtZero = TRUE,
  spline = FALSE,
  fill_opacity = 0.9,
  agg_text = " ",
  export =  FALSE,
  border_color = '#000000',
  theme = tma(custom = list(stylesX_lineWidth = 0, 
                            background = 'transparent',
                            colors = colores,
                            font_family = "Raleway",
                            font_size = '11px',
                            font_color = '#000000',
                            stylesTitleY_fontWeight = 'bold',
                            stylesTitleX_fontWeight = 'bold')))


anonimos <- read_csv('data/anonimos.csv')
v0 <- hgch_bar_CatNum(anonimos, opts = opts_viz)
saveWidget(v0, 'anonimos.html', background = 'transparent')


data_2015 <- read_csv('data/total_fin_2015.csv')
data_2015 <- data_2015 %>% select(`TIPO PERSONA`, everything())
v1 <- hgch_bar_CatCatNum(data_2015, opts = opts_viz)
saveWidget(v1, 'data_2015_viz1.html', background = 'transparent')

data_2018 <- read_csv('data/total_fin_2018.csv')
data_2018 <- data_2018 %>% select(`TIPO PERSONA`, everything())
v2 <- hgch_bar_CatCatNum(data_2018, opts = opts_viz)
saveWidget(v2, 'data_2018_viz2.html', background = 'transparent')


# tabla 1
library(DT)
tabla_1 <- read_csv('data/tabla_1.csv')
dt <-     datatable(tabla_1,
                    rownames = F,
                    options = list(
                      language = list(url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Spanish.json'),
                      lengthChange = F,
                      initComplete = JS(
                        "function(settings, json) {",
                        "$(this.api().table().header()).css({'background-color': '#0A446B', 'color': '#fff'});
                        $('body').css({'font-family': 'Raleway', 'background-color':'transparent', 'height': '500px'});",
                        "}"),
                      searching = FALSE
                    )) %>% 
  formatStyle( 0, target= 'row',color = '#0A446B', fontSize ='13px', lineHeight='15px')
saveWidget(dt, 'tabla1.html', background = 'transparent')



tabla_2 <- read_csv('data/tabla_2.csv')
dt_2  <-   datatable(tabla_2,
                     rownames = F,
                     options = list(
                       pageLength = 5, 
                       scrollX = T,
                       scrollY = T,
                       language = list(url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Spanish.json'),
                       lengthChange = F,
                       initComplete = JS(
                         "function(settings, json) {",
                         "$(this.api().table().header()).css({'background-color': '#0A446B', 'color': '#fff'});
                         $('body').css({'font-family': 'Raleway', 'background-color':'transparent', 'height': '500px;'});
                         ",

                         "}"),
                       searching = FALSE
                     )) %>% 
  formatStyle( 0, target= 'row',color = '#0A446B', fontSize ='13px', lineHeight='15px')
saveWidget(dt_2, 'tabla2.html',background = 'transparent')
