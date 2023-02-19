# install.packages(c("shiny","tidyverse"))
library(shiny)
library(tidyverse)
library(xml2)
library(rvest)
library(lubridate)
library(DT)


# Modry dvere -------------------------------------------------------------
adresa <- "https://www.modrydvere.cz/poledni-menu"

modry_dvere <- read_html(adresa)

modry_dvere_tabulka_A <- xml_find_all(modry_dvere, "//td[@class='bg_cervena2']//table")

modry_dvere_tabulka_pondeli <- html_table(modry_dvere_tabulka_A[2], header = TRUE, fill = TRUE) %>% 
  do.call(rbind.data.frame, .) %>%
  as_tibble(.,.name_repair  = ~c("Množství", "Pokrm", "Cena", "Den")) %>%
  mutate_all(na_if,"") %>%
  drop_na(Množství) %>%
  mutate(Den = 1) %>%
  mutate_all(as.character)
modry_dvere_tabulka_utery <- html_table(modry_dvere_tabulka_A[3], header = TRUE, fill = TRUE) %>% 
  do.call(rbind.data.frame, .) %>%
  as_tibble(.,.name_repair  = ~c("Množství", "Pokrm", "Cena", "Den")) %>%
  mutate_all(na_if,"") %>%
  drop_na(Množství)%>%
  mutate(Den = 2)%>%
  mutate_all(as.character)
modry_dvere_tabulka_streda <- html_table(modry_dvere_tabulka_A[4], header = TRUE, fill = TRUE) %>%
  do.call(rbind.data.frame, .) %>%
  as_tibble(.,.name_repair  = ~c("Množství", "Pokrm", "Cena", "Den")) %>%
  mutate_all(na_if,"") %>%
  drop_na(Množství)%>%
  mutate(Den = 3)%>%
  mutate_all(as.character)
modry_dvere_tabulka_ctvrtek <- html_table(modry_dvere_tabulka_A[5], header = TRUE, fill = TRUE) %>%
  do.call(rbind.data.frame, .) %>%
  as_tibble(.,.name_repair  = ~c("Množství", "Pokrm", "Cena", "Den")) %>%
  mutate_all(na_if,"") %>%
  drop_na(Množství) %>%
  mutate(Den = 4)%>%
  mutate_all(as.character)
modry_dvere_tabulka_patek <- html_table(modry_dvere_tabulka_A[6], header = TRUE, fill = TRUE) %>%
  do.call(rbind.data.frame, .) %>%
  as_tibble(.,.name_repair  = ~c("Množství", "Pokrm", "Cena", "Den")) %>%
  mutate_all(na_if,"") %>%
  drop_na(Množství)%>%
  mutate(Den = 5)%>%
  mutate_all(as.character)

Modry_dvere_dohromady <- bind_rows(modry_dvere_tabulka_pondeli
                                   ,modry_dvere_tabulka_utery
                                   ,modry_dvere_tabulka_streda
                                   ,modry_dvere_tabulka_ctvrtek
                                   ,modry_dvere_tabulka_patek) %>%
  mutate(Cena = gsub("[^0-9]", "", Cena)
         ,Cena = paste0(Cena, " Kč")) # %>%
  # filter(Den == wday(as.Date(Sys.Date()), week_start = 1)) %>%
  # mutate(Den = NULL)

# Snyt -------------------------------------------------------------

adresa <- "http://www.snyt-cb.cz/denni-menu"

snyt <- read_html(adresa)

snyt_tabulka_A <- xml_find_all(snyt, "//p[@class='MsoNormal']//.") %>%
  map_chr(. %>% xml_find_all("span") %>% xml_text() %>% paste(collapse = ", ")) %>%
  as_tibble() %>%
  mutate(Value = gsub("\u00A0", "|", str_trim(value))  #nahradit skryty mezery pipou
         ,Value1 = gsub("\\s*([/\\|/])\\s*\\1+", '\\1', Value) #odstranit mezery mezi pipama
         ,Value2 = gsub("([/\\|/])\\1+", '\\1', Value1)
         ,Value3 = gsub("\\s*([/\\|/])\\s*\\1+", '\\1', Value2)
         ,Value4 = gsub("(?<=[a-zA-Zžščříáěé])[|](?=[a-zA-Zžščříáěé])",' ', Value3, perl = TRUE)) %>% #odstranit pipy mezi slovy
  separate(., col = Value4, into = c("Množství", "Pokrm", "Cena"), sep = "[/\\|/]") %>%
  select("Množství", "Pokrm", "Cena") %>%
  mutate_all(na_if, "") %>%
  mutate_all(str_trim) %>%
  mutate(Cena = gsub("[^0-9]", "", Cena)
         ,Cena = paste0(Cena, " Kč")) %>%
  filter(!is.na(Pokrm)
         , !Množství == ",")

# Jidelna u Zimaku----------------------------------------------------------

adresa <- "https://docs.google.com/spreadsheets/d/e/2PACX-1vSUVXZLSJe-mCO-cUbhKXq7wgqIl6JEEPEVtKpBQ6n071kk37ocWCjmy9EcbCBIi1ZRmFrfQNTKIgmR/pubhtml?gid=1003084368&single=true&single=true&range=A1:H136&headers=false&chrome=false"

# adresa se odkazuje na zmineny google dokument "http://officefood.cz/kucb-uzimnihostadionu/"
jidelna_zimak <- read_html(adresa)
#document
jidelna_zimak_table <- xml_find_all(jidelna_zimak, "//table")

jidelna_zimak_table1 <- html_table(jidelna_zimak_table, fill = TRUE, header = TRUE) %>%
  do.call(rbind.data.frame, .) %>%
  as_tibble(.,.name_repair = ~c(paste0("X", 1:9))) %>%
  select("Množství" = X2, "Pokrm" = X3, "Cena" = X9) %>%
  mutate_all(tolower) %>%
  mutate(Den = case_when( as.character(str_trim(gsub("[0-9\\.\\-]", "", Množství))) == "pondělí" ~ 1
                          ,as.character(str_trim(gsub("[0-9\\.\\-]", "", Množství))) == "úterý" ~ 2
                          ,as.character(str_trim(gsub("[0-9\\.\\-]", "", Množství))) == "středa" ~ 3
                          ,as.character(str_trim(gsub("[0-9\\.\\-]", "", Množství))) == "čtvrtek" ~ 4
                          ,as.character(str_trim(gsub("[0-9\\.\\-]", "", Množství))) == "pátek" ~ 5
                         )) %>%
  mutate_all(na_if,"") %>%
  mutate(Cena = gsub("[^0-9]", "", Cena)
         ,Cena = paste0(Cena, " Kč")) %>%
  fill(Den, .direction = "down") %>%
  filter(!is.na(Pokrm)
         , !Pokrm == "změny v jídelním lístku vyhrazeny")
         #, Den == wday(as.Date(Sys.Date()), week_start = 1) ) %>%
  # mutate(Den = NULL)


# dohromady-------------------------------------------------------------

Jidelnicek_snyt <- snyt_tabulka_A
Jidelnicek_Modry_dvere <- Modry_dvere_dohromady
Jidelnicek_Zimak <- jidelna_zimak_table1

# shiny -------------------------------------------------------------------

ui <- fluidPage(
  # App title ----
  titlePanel(
    h1("Restaurant menu", align = "center")
    , windowTitle = "Restaurant menu - EON a okolí"),
  
    sidebarLayout(
      # Sidebar with a slider input
      sidebarPanel(h3("Info:"),
                   helpText(paste0("Dnes je: ", format(today() , "%A %d %B %Y") ))
                   ,helpText("Jídelníček podniků v blízkém okolí EONu v Českých Budějovicích")
                   ,helpText( " ")
                   ,helpText("Kontakt: TŠ")
                   ,width = 2
                   ,radioButtons("radio", h3("Výběr dne v týdnu"),
                                 choices = list("Pondělí" = 1, "Úterý" = 2,
                                                "Středa" = 3, "Čtvrtek" = 4,
                                                "Pátek" = 5), selected = wday(as.Date(Sys.Date()), week_start = 1))
      ),
      # Show a plot of the generated distribution
      mainPanel(
         DT::dataTableOutput("tabulka_Jidelnicek_Modry_dvere")
        ,hr()
        ,hr()
        ,DT::dataTableOutput("tabulka_Jidelnicek_Zimak")
        ,hr()
        ,hr()
        ,DT::dataTableOutput("tabulka_Jidelnicek_snyt")
      )
    )
)


caption_font <- function(caption, color = "black", px_size = 26) {
  caption_final <- shiny::tags$caption(style = paste0("color: ", color, ";font-size: ", px_size, "px;"), caption)
  return(caption_final)
}

server <- function(input, output){
  
  Jidelnicek_Modry_dvere_filtered <- reactive({ # <-- Reactive function here
    Jidelnicek_Modry_dvere %>% 
      filter(Den == input$radio) %>%
      mutate(Den = NULL) %>%
      datatable(options = list(searching = FALSE
                               , paging = FALSE
                               , lengthChange = FALSE
                               , info = FALSE
                               , autoWidth = TRUE
                               # , scrollX=TRUE
                               ,columnDefs = list(list(width = '5%', targets = c(0)),
                                                  list(width = '90%', targets = c(1)),
                                                  list(width = '10%', targets = c(2)))
                               )
                , caption = caption_font("Modrý dveře")
                , rownames = FALSE)})
  
  Jidelnicek_Zimak_filtered <- reactive({ # <-- Reactive function here
    Jidelnicek_Zimak%>% 
      filter(Den == input$radio) %>%
      mutate(Den = NULL) %>%
      datatable(options = list(searching = FALSE
                               , paging = FALSE
                               , lengthChange = FALSE
                               , info = FALSE
                               , autoWidth = TRUE
                               # , scrollX=TRUE
                               ,columnDefs = list(list(width = '5%', targets = c(0)),
                                                  list(width = '90%', targets = c(1)),
                                                  list(width = '10%', targets = c(2)))
                               )
                , caption = caption_font("Jídelna u zimáku / Krajský úřad")
                , rownames = FALSE)})
  
  Jidelnicek_snyt_filtered <- # <-- Reactive function here
    Jidelnicek_snyt%>% 
      # filter(Den == input$radio) %>% #they dont have menu for whole week
      mutate(Den = NULL) %>%
    datatable(options = list(searching = FALSE
                             , paging = FALSE
                             , lengthChange = FALSE
                             , info = FALSE
                             , autoWidth = TRUE
                             # , scrollX=TRUE
                             ,columnDefs = list(list(width = '5%', targets = c(0)),
                                                list(width = '90%', targets = c(1)),
                                                list(width = '10%', targets = c(2)))
                             )
                , caption = caption_font("Šnyt")
                , rownames = FALSE)
  
  output$tabulka_Jidelnicek_Modry_dvere <- renderDataTable({
    Jidelnicek_Modry_dvere_filtered()})
  
  output$tabulka_Jidelnicek_Zimak <- renderDataTable({
    Jidelnicek_Zimak_filtered()})
  
  output$tabulka_Jidelnicek_snyt <- renderDataTable(
    Jidelnicek_snyt_filtered)
}

shinyApp(ui, server)
