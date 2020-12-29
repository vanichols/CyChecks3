library(shiny)
library(ggplot2)
library(dplyr)
library(tidyr)
library(ggpubr)
library(shinythemes)
library(plotly)
library(scales) #--to get $ on y axis, so easy!
library(stringr)
library(forcats)
library(tidytext)
library(markdown)
#devtools::install_github("vanichols/CyChecks3")
library(CyChecks3)


# create data and dropdowns -----------------------------------------------------

mytheme <- theme_bw() +
  theme(strip.background = element_rect(fill = NA),
        strip.text = element_text(size = rel(1.5)),
        strip.text.y = element_text(angle = 0),
        axis.text = element_text(size = rel(1.2)),
        axis.title = element_text(size = rel(1.5)),
        plot.title = element_text(size = rel(2)))


data("professors")
data("cystats")

#--salaries


clean_punc <- function(x) {
  stringr::str_replace_all(x, "\\'", "") %>%
    stringr::str_replace_all("\\.", "") %>%
    stringr::str_replace_all(" ", "") %>%
    stringr::str_replace_all("[^[:alnum:]]", "") %>%
    stringr::str_trim(.) %>%
    stringr::str_squish(.)
}

#--problem - some depts are listed under multiple colleges

mul_col <-
  professors %>%
  select(college, dept) %>%
  distinct() %>%
  group_by(dept) %>%
  mutate(n = n()) %>%
  filter(n > 1) %>%
  pull(dept)


sals <-
  professors %>%
  filter(base50 == "Y") %>%
  filter(base_salary > 0) %>%
  mutate(college2 = ifelse(dept %in% mul_col, "Multiple", college)) %>%
  mutate_if(is.character, str_to_title) %>%
  mutate(
    gender = recode(gender,
                    "F" = "Female",
                    "M" = "Male"),
    title_simp = factor(title_simp,
                            levels = c("Asst Prof",
                                       "Assoc Prof",
                                       "Prof",
                                       "Awarded Prof"))) %>%
  pivot_longer(cols = base_salary:total_salary_paid,
               names_repair = "minimal",
               names_to = "salary_type",
               values_to = "amount") %>%
  mutate(salary_type_nice = str_replace_all(salary_type, "_", " ") %>% str_to_title(.),
         year = as.character(year),
         dept_clean = clean_punc(dept),
         dept_chair2 = ifelse(dept_chair == "Y", "Dept Chair", ""))

malecolor <- "deepskyblue3"
femalecolor <- "goldenrod"

dd_college <- sals %>% arrange(college2) %>% pull(college2) %>% unique()
dd_dept <-  sals %>% arrange(dept) %>% pull(dept) %>% unique()
dd_year <-  sals %>% arrange(year) %>% pull(year) %>% unique() %>% as.character()
dd_saltype <-  sals %>% arrange(salary_type_nice) %>% pull(salary_type_nice) %>% unique() %>% as.character()
dd_pos <-  sals %>% arrange(title_simp) %>% pull(title_simp) %>% unique() %>% as.character()


###practice graph1----
prac_gend <-
  sals %>%
#  filter(college == dd_college[1]) %>%
  select(year, college, dept, gender, name, title, title_simp) %>%
  distinct() %>%
  group_by(year, college, dept, gender) %>%
  summarise(n = n()) %>%
  ungroup() %>%
  pivot_wider(names_from = gender, values_from = n) %>%
  mutate(
    Female = ifelse(is.na(Female), 0, Female),
    Male = ifelse(is.na(Male), 0, Male),
    tot = Female + Male,
    fracM = Male/tot,
    dept = fct_reorder(dept, fracM)) %>%
  arrange(dept)

###practice graph2----
prac_gend %>%
  arrange(college, fracM) %>%
  mutate(dept = fct_inorder(dept)) %>%
  filter(college == "College Of Agriculture & Life Sciences") %>%
  ggplot(aes(dept, fracM,
         group = 1,
         text = paste("Total Faculty:", tot)
         )) +
  geom_segment(aes(x = dept, xend = dept, y = 0, yend = fracM)) +
  geom_point(aes(size = tot), color = "red4") +
  geom_hline(yintercept = 0.5, linetype = "dashed") +
  scale_y_continuous(labels = label_percent()) +
  mytheme +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(y = "Male Faculty (% of Total)", x = NULL)



###practice graph3----
prac_gend %>%
  group_by(year, dept) %>%
  summarise(Female = sum(Female, na.rm = T),
            Male = sum(Male, na.rm = T)) %>%
  mutate( tot = Female + Male,
          fracM = Male/tot) %>%
  arrange(fracM) %>%
  mutate(dept = fct_inorder(dept),
         dept = fct_rev(dept)) %>%
  ggplot(aes(dept, fracM,
             group = 1,
             text = paste("Total Faculty:", tot)
  )) +
  geom_hline(yintercept = 0.5, linetype = "dashed") +
  geom_segment(aes(x = dept, xend = dept, y = 0, yend = fracM)) +
  geom_point(aes(size = tot, fill = fracM), pch = 21) +
  scale_y_continuous(labels = label_percent(), limits = c(0, 1)) +
  mytheme +
  scale_fill_gradient2(low = femalecolor, mid = "white", high = malecolor,
                          midpoint = 0.5) +
  coord_flip() +
  theme(legend.position = "none") +
  labs(y = "Male Faculty (% of Total)", x = NULL)

prac_gend %>%
  group_by(dept) %>%
  summarise(n = n()) %>%
  arrange(-n)

prac_gend %>%
  filter(dept == "Eeob")


#plotbp <-
  sals %>%
  filter(salary_type != "travel_subsistence") %>%
  filter(salary_type != "total_salary") %>%
    group_by(title_simp, gender, salary_type_nice) %>%
    mutate(n = n()) %>%
  ggplot(aes(x = gender, y = amount, color = gender,
             group = 1,
             text = paste("Name:", name,
                          "<br>Dept:", dept,
                          "<br> Dept Chair? ", dept_chair,
                          "<br>Salary: $", round(amount / 1000, digits = 0), "thou"))) +
  geom_boxplot(outlier.color = NA,
               outlier.size = 0,
               outlier.shape = NA,
               aes(color = gender)) +
  geom_jitter(alpha = 0.5, width = 0.2, size = 3) +
  geom_text(aes(x = gender, y = 100, label = n)) +
  guides(color = F) +
  scale_color_manual(values = c("Female" = femalecolor, "Male" = malecolor)) +
  facet_grid(title_simp ~ salary_type_nice) +
  scale_y_continuous(labels = dollar_format()) +
  mytheme +
  theme(legend.position = "none",
        strip.background.y = element_rect(color = NA))+
  labs(x = NULL,
       y = NULL)



# user interface ----------------------------------------------------------



ui <- fluidPage(theme = shinytheme("united"),

                # Application title
                navbarPage(
                  "CyChecks3 Professor Explorer",

                  #--start tab
                  tabPanel("What is this?",
                           column(
                             3,
                             #offset = 4,
                             tags$img(
                               src = "CyChecks_hexsticker.png",
                               height = 300,
                               width = 250,
                               align = "center"
                             )),
                             column(9,
                                    #offset = 1,
                                  includeMarkdown("about.md"))

                  ),
                  #--end about tab-


                    #--start tab
                  tabPanel("University Salaries by Gender",
                           fluidRow(
                             column(
                               width = 3,
                              #--input, select year
                               selectInput(
                                 "sel_year_univ",
                                 label = ("Year:"),
                                 choices = dd_year,
                                 selected = dd_year[1]),
                              #--input, select salary type
                              selectInput(
                                "sel_saltype_univ",
                                label = ("Salary Type:"),
                                choices = dd_saltype,
                                selected = dd_saltype[1]),
                               #--download data
                               downloadButton("downloadData_univ", "Download")
                             ),
                             column(width = 9,
                                    plotlyOutput("fig_univ",
                                               height = "700px", width = "1200px")))
                           ),
                  #--end univ tab-

                  #--start tab
                  tabPanel("Salaries by College",
                           fluidRow(
                             column(
                               width = 3,
                               #--input, select dept
                               selectInput(
                                 "sel_college_salsc",
                                 label = ("College:"),
                                 choices = dd_college,
                                 selected = dd_college[6]),
                               #--input, select salary type
                               selectInput(
                                 "sel_saltype_salsc",
                                 label = ("Salary Type:"),
                                 choices = dd_saltype,
                                 selected = dd_saltype[1]),

                               #--input, select year
                               selectInput(
                                 "sel_year_salsc",
                                 label = ("Year:"),
                                 choices = dd_year,
                                 selected = dd_year[1]),
                               #--download data
                               downloadButton("downloadData_salsc", "Download")
                             ),
                             column(width = 9,
                                    plotlyOutput("fig_salsc", height = "700px", width = "1200px"))
                           )),
                  #--end sals tab-



                  #--start tab
                  tabPanel("Salaries by Department",
                           fluidRow(
                             column(
                               width = 3,
                               #--input, select dept
                               selectInput(
                                 "sel_dept_sals",
                                 label = ("Department:"),
                                 choices = dd_dept,
                                 selected = dd_dept[6]),
                               #--input, select salary type
                               selectInput(
                                 "sel_saltype_sals",
                                 label = ("Salary Type:"),
                                 choices = dd_saltype,
                                 selected = dd_saltype[1]),

                               #--input, select year
                               selectInput(
                                 "sel_year_sals",
                                 label = ("Year:"),
                                 choices = dd_year,
                                 selected = dd_year[1]),
                               #--download data
                               downloadButton("downloadData_sals", "Download")
                             ),
                             column(width = 9,
                                    plotlyOutput("fig_sals", height = "700px", width = "1200px"))
                           )),
                  #--end sals tab-


                  #--start gender rep tab-
                  tabPanel("Gender Makeup By Department",
                           fluidRow(
                             column(
                               width = 2,
                               #--input, select college
                               selectInput(
                                 "sel_college_gend",
                                 label = ("College:"),
                                 choices = c("All", dd_college),
                                 selected = "All"),
                               #--input, select year
                               selectInput(
                                 "sel_year_gend",
                                 label = ("Year:"),
                                 choices = dd_year,
                                 selected = dd_year[1])
                               ),
                             column(width = 10,
                                    fluidRow(
                                    plotlyOutput("fig_gend",  height = "700px", width = '1000px')
                                    )
                           ))
                  ),
                  #--end gender rep tab-

                  #--start stats tab-
                  tabPanel("Statistics",
                           fluidRow(
                             column(
                               width = 2,
                               #--select title
                               selectInput(
                                 "sel_title_stats",
                                 label = ("Position:"),
                                 choices = dd_pos,
                                 selected = dd_pos[1]),
                             #--select year
                             selectInput(
                               "sel_year_stats",
                               label = ("Year:"),
                               choices = dd_year,
                               selected = dd_year[1])
                           ),
                             column(width = 8,
                                    offset = 2,
                                    fluidRow(
                                      plotOutput("fig_stats",  height = "700px", width = '600px')
                                    )
                             ))
                  )
                  #--end stats tab-


                )
                )




# server ------------------------------------------------------------------

server <- function(input, output) {

  #--university tab-------------------

  liq_univ <- reactive({
    sals %>%
      filter(
        year == input$sel_year_univ,
        salary_type_nice == input$sel_saltype_univ) %>%
      group_by(title_simp, salary_type_nice, gender) %>%
      mutate(n = n())

  })


  output$fig_univ <- renderPlotly({


    puniv <-
      liq_univ() %>%
      ggplot(aes(x = gender, y = amount, color = gender,
                 group = 1,
                 text = paste("Name:", name,
                              "<br>Dept:", dept,
                              "<br>",   dept_chair2,
                              "<br>Salary: $", round(amount / 1000, digits = 0), "thou"))) +
      geom_boxplot(outlier.color = NA, aes(color = gender)) +
      geom_jitter(alpha = 0.5, width = 0.2, size = 2) +
      geom_text(aes(x = gender, y = 100, label = n)) +
      guides(color = F) +
      scale_color_manual(values = c("Female" = femalecolor, "Male" = malecolor)) +
      facet_grid(. ~title_simp) +
      scale_y_continuous(labels = dollar_format()) +
      # coord_flip() +
      mytheme +
      theme(legend.position = "none",
            strip.background.y = element_rect(color = NA)) +
      labs(x = NULL,
           y = NULL)

    ggplotly(puniv, tooltip = "text") %>% layout(margin = list(r = 160)) #--r = right


  })

  #--univ data download

  output$downloadData_univ <- downloadHandler(
    filename = function() {
      paste("ISUProfSals_AllDepts-", input$sel_year_univ, ".csv", sep = "")
    },
    content = function(file) {
      write.csv(liq_univ(), file, row.names = FALSE)
    }
  )



  #--dept salary tab-------------------

   liq_sals <- reactive({
    sals %>%
      filter(
         dept == input$sel_dept_sals,
         salary_type_nice == input$sel_saltype_sals,
         year == input$sel_year_sals)
  })

  output$fig_sals <- renderPlotly({

    p1 <-
      liq_sals() %>%
      ggplot(
        aes(gender, amount, group = 1,
        text = paste("Name:", name, "(", stringr::str_sub(gender, 1, 1), ")",
                     "<br>",   dept_chair2,
                     "<br>Salary: $", round(amount / 1000, digits = 0), "thou"))
        ) +
      stat_summary(fun = mean, geom = "bar", aes(fill = gender), width = 0.9) +
      geom_jitter(aes(color = dept_chair, pch = dept_chair), width = 0.2, size = 3, stroke = 1) +
      guides(fill = F,
             color = F,
             pch = F) +
      scale_y_continuous(labels = label_dollar()) +
      scale_fill_manual(values = c("Female" = femalecolor, "Male" = malecolor)) +
      scale_color_manual(values = c("N" = "black", "Y" = "red4")) +
      scale_shape_manual(values = c(21, 17)) +
      facet_grid(. ~ title_simp,
                 scales = "free_y",
                 switch = "y") +
      mytheme +
      theme(legend.position = "none") +
      labs(x = NULL, y = NULL)

    ggplotly(p1, tooltip = "text") %>% layout(margin = list(r = 200)) #--r = right


  })

  output$downloadData_sals <- downloadHandler(
    filename = function() {
      paste0("ISUProfSals_",
           clean_punc(input$sel_dept_sals),
            "-",
            input$sel_year_sals,
            ".csv")
    },
    content = function(file) {
      write.csv(liq_sals(), file, row.names = FALSE)
    }
  )


  #--college salary tab-------------------

  liq_salsc <- reactive({
    sals %>%
      filter(
        college2 == input$sel_college_salsc,
        salary_type_nice == input$sel_saltype_salsc,
        year == input$sel_year_salsc)
  })

  output$fig_salsc <- renderPlotly({

    p1 <-
      liq_salsc() %>%
      ggplot(
        aes(gender, amount, group = 1,
            text = paste("Name:", name, "(", stringr::str_sub(gender, 1, 1), ")",
                         "<br>Dept:",   dept, dept_chair2,
                         "<br>Salary: $", round(amount / 1000, digits = 0), "thou"))
      ) +
      stat_summary(fun = mean, geom = "bar", aes(fill = gender), width = 0.9) +
      geom_jitter(aes(color = dept_chair, pch = dept_chair), width = 0.2, size = 3, stroke = 1) +
      guides(fill = F,
             color = F,
             pch = F) +
      scale_y_continuous(labels = label_dollar()) +
      scale_fill_manual(values = c("Female" = femalecolor, "Male" = malecolor)) +
      scale_color_manual(values = c("N" = "black", "Y" = "red4")) +
      scale_shape_manual(values = c(21, 17)) +
      facet_grid(. ~ title_simp,
                 scales = "free_y",
                 switch = "y") +
      mytheme +
      theme(legend.position = "none") +
      labs(x = NULL, y = NULL)

    ggplotly(p1, tooltip = "text") %>% layout(margin = list(r = 200)) #--r = right


  })

  output$downloadData_salsc <- downloadHandler(
    filename = function() {
      paste0("ISUProfSals_",
             clean_punc(input$sel_college_salsc),
             "-",
             input$sel_year_salsc,
             ".csv")
    },
    content = function(file) {
      write.csv(liq_sals(), file, row.names = FALSE)
    }
  )



  #--gender tab---------------

  liq_gend1 <- reactive({

    if(input$sel_college_gend == "All") {

    sals %>%
      filter(
        year == input$sel_year_gend) %>%
      select(year, dept, gender, name, title, title_simp) %>%
      distinct() %>%
      group_by(year, dept, gender) %>%
      summarise(n = n()) %>%
      ungroup() %>%
      pivot_wider(names_from = gender, values_from = n) %>%
      mutate(
        Female = ifelse(is.na(Female), 0, Female),
        Male = ifelse(is.na(Male), 0, Male),
        tot = Female + Male,
        fracM = Male/tot,
        clr = ifelse(fracM > 0.5, "Male", "Female")) %>%
        arrange(fracM) %>%
        mutate(dept = fct_inorder(dept))

    } else {

      sals %>%
        filter(
          college2 == input$sel_college_gend,
          year == input$sel_year_gend) %>%
        select(year, college2, dept, gender, name, title, title_simp) %>%
        distinct() %>%
        group_by(year, college2, dept, gender) %>%
        summarise(n = n()) %>%
        ungroup() %>%
        pivot_wider(names_from = gender, values_from = n) %>%
        mutate(
          Female = ifelse(is.na(Female), 0, Female),
          Male = ifelse(is.na(Male), 0, Male),
          tot = Female + Male,
          fracM = Male/tot,
          dept = fct_reorder(dept, fracM)) %>%
        arrange(dept) %>%
        mutate(clr = ifelse(fracM > 0.5, "Male", "Female"))
      }

  })

  output$fig_gend <- renderPlotly({

    if(input$sel_college_gend == "All") {

    pgend <-
    liq_gend1() %>%
      ggplot(aes(dept, fracM,
                 group = 1,
                 text = paste("Total Faculty:", tot,
                              "<br>Dept:", dept,
                              "<br>F:", Female,
                              "<br>M:", Male))
      ) +
      geom_hline(yintercept = 0.5, linetype = "dashed") +
      geom_segment(aes(x = dept, xend = dept, y = 0, yend = fracM)) +
      geom_point(aes(size = tot, fill = clr), pch = 21) +
      scale_y_continuous(labels = label_percent(), limits = c(0, 1)) +
      mytheme +
      scale_fill_manual(values = c("Female" = femalecolor,
                                   "Male" = malecolor)) +
      theme(legend.position = "none",
            axis.text.x = element_blank()) +
      labs(y = "Male Faculty (% of Total)", x = "Department")

    ggplotly(pgend, tooltip = "text") %>% layout(margin = list(l = 160, b = 160))

    } else {
        pgend <-
          liq_gend1() %>%
          #mutate(dept = fct_rev(dept)) %>%
          ggplot(aes(dept, fracM,
                     group = 1,
                     text = paste("Total Faculty:", tot,
                                  "<br>F:", Female,
                                  "<br>M:", Male))
          ) +
          geom_hline(yintercept = 0.5, linetype = "dashed") +
          geom_segment(aes(x = dept, xend = dept, y = 0, yend = fracM)) +
          geom_point(aes(size = tot, fill = clr), pch = 21) +
          scale_fill_manual(values = c("Female" = femalecolor, "Male" = malecolor)) +
          scale_y_continuous(labels = label_percent(), limits = c(0, 1)) +
          facet_grid(.~college2, scales = "free") +
          mytheme +
          theme(legend.position = "none",
                axis.text.x = element_text(angle = 45, hjust = 1)) +
          labs(y = "Male Faculty (% of Total)", x = NULL)

        ggplotly(pgend, tooltip = "text") %>% layout(margin = list(l = 160))

      }

  })

  #--stats tab-------------------

  liq_stats <- reactive({
    cystats %>%
      filter(
        title_simp == input$sel_title_stats,
        year == input$sel_year_stats) %>%
      mutate(mycolor = ifelse(estimate > 0, "male", "female"))
  })

  output$fig_stats <- renderPlot({

    liq_stats() %>%
      ggplot(aes(dept2, estimate)) +
      geom_hline(yintercept = 0) +
      geom_linerange(aes(ymin = lower.CL, ymax = upper.CL), color = "gray60") +
      geom_point(aes(color = mycolor, size = abs(estimate))) +
      scale_x_reordered() +
      scale_color_manual(values = c("female" = femalecolor, "male" = malecolor)) +
      guides(size = F, color = F) +
      facet_wrap(~title_simp, scales = "free") +
      coord_flip() +
      mytheme +
      labs(title = "Effect of Being Male on Salary (Log-scale)",
           x = NULL,
           y = NULL)


  })


}

shinyApp(ui, server)
