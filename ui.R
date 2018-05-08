source("helpers.R" , encoding = "UTF8")

library(shiny)

navbarPage(title = h4(strong('禁食',style = "font-family:Microsoft JhengHei")),
           tabPanel(h5('資料表',style = "font-family:Microsoft JhengHei"),style = "font-family:Microsoft JhengHei",
                    column(1),
                    column(2,br(),br(),checkboxGroupInput("VAR", "選取欄位",
                                                c("電阻式體脂計","血壓","體能","HRV","問卷得分",
                                                  "陽虛體質","陰虛體質","痰瘀體質"), selected = "電阻式體脂計")
                    ),
                    column(8,tabsetPanel(
                                       tabPanel(h5('4/11',style = "font-family:Microsoft JhengHei"),style = "font-family:Microsoft JhengHei",
                                                br(),DT::dataTableOutput("T1")),
                                       tabPanel(h5('4/14',style = "font-family:Microsoft JhengHei"),style = "font-family:Microsoft JhengHei",
                                                br(),DT::dataTableOutput("T2")),
                                       tabPanel(h5('4/16',style = "font-family:Microsoft JhengHei"),style = "font-family:Microsoft JhengHei",
                                                br(),DT::dataTableOutput("T3")),
                                       tabPanel(h5('4/18',style = "font-family:Microsoft JhengHei"),style = "font-family:Microsoft JhengHei",
                                                br(),DT::dataTableOutput("T4"))
                                                                            ))),
           navbarMenu(h5('盒形圖',style = "font-family:Microsoft JhengHei"),
           tabPanel(h5('n=11(數值)',style = "font-family:Microsoft JhengHei"),style = "font-family:Microsoft JhengHei",
                    column(1),column(2,br(),br(),
                                     selectInput(inputId = "group3",
                                                 label = "器材分類",
                                                 choices = c("電阻式體脂計","血壓","體能","HRV","問卷得分","次domain"),
                                                 selected = "電阻式體脂計"),
                                     conditionalPanel(
                                       condition = "input.group3 == 'HRV'",
                                       selectInput(inputId = "vvrr13",
                                                   label = "選取變數",
                                                   choices = colnames(DD)[26:39],
                                                   selected = colnames(DD)[26])
                                     ),
                                     conditionalPanel(
                                       condition = "input.group3 == '電阻式體脂計'",
                                       selectInput(inputId = "vvrr14",
                                                   label = "選取變數",
                                                   choices = colnames(DD)[8:16],
                                                   selected = colnames(DD)[8])
                                     ),
                                     conditionalPanel(
                                       condition = "input.group3 == '血壓'",
                                       selectInput(inputId = "vvrr15",
                                                   label = "選取變數",
                                                   choices = colnames(DD)[17:20],
                                                   selected = colnames(DD)[17])
                                     ),
                                     conditionalPanel(
                                       condition = "input.group3 == '體能'",
                                       selectInput(inputId = "vvrr16",
                                                   label = "選取變數",
                                                   choices = colnames(DD)[21:25],
                                                   selected = colnames(DD)[21])
                                     ),
                                     conditionalPanel(
                                       condition = "input.group3 == '問卷得分'",
                                       selectInput(inputId = "vvrr17",
                                                   label = "選取變數",
                                                   choices = c("飲食得分","壓力得分","簡式得分","睡眠得分","陽虛得分","陰虛得分","痰瘀得分"),
                                                   selected = "飲食得分")
                                     ),
                                     conditionalPanel(
                                       condition = "input.group3 == '次domain'",
                                       selectInput(inputId = "vvrr18",
                                                   label = "選取變數",
                                                   choices = colnames(DD)[c(40:44,46:50,52:55)],
                                                   selected = colnames(DD)[40])
                                     )
                    ),
                    column(8,plotOutput("P12")),column(3),column(2),
                    column(3,br(),br(),verbatimTextOutput("S12"))),
           tabPanel(h5('n=15(數值)',style = "font-family:Microsoft JhengHei"),style = "font-family:Microsoft JhengHei",
           column(1),column(2,br(),br(),
                            selectInput(inputId = "group",
                                        label = "器材分類",
                                        choices = c("電阻式體脂計","血壓","體能","HRV","問卷得分","次domain"),
                                        selected = "電阻式體脂計"),
                            conditionalPanel(
                              condition = "input.group == 'HRV'",
                              selectInput(inputId = "vvrr1",
                                          label = "選取變數",
                                          choices = colnames(DD)[26:39],
                                          selected = colnames(DD)[26])
                            ),
                            conditionalPanel(
                              condition = "input.group == '電阻式體脂計'",
                              selectInput(inputId = "vvrr2",
                                          label = "選取變數",
                                          choices = colnames(DD)[8:16],
                                          selected = colnames(DD)[8])
                            ),
                            conditionalPanel(
                              condition = "input.group == '血壓'",
                              selectInput(inputId = "vvrr3",
                                          label = "選取變數",
                                          choices = colnames(DD)[17:20],
                                          selected = colnames(DD)[17])
                            ),
                            conditionalPanel(
                              condition = "input.group == '體能'",
                              selectInput(inputId = "vvrr4",
                                          label = "選取變數",
                                          choices = colnames(DD)[21:25],
                                          selected = colnames(DD)[21])
                            ),
                            conditionalPanel(
                              condition = "input.group == '問卷得分'",
                              selectInput(inputId = "vvrr5",
                                          label = "選取變數",
                                          choices = c("飲食得分","壓力得分","簡式得分","睡眠得分","陽虛得分","陰虛得分","痰瘀得分"),
                                          selected = "飲食得分")
                            ),
                            conditionalPanel(
                              condition = "input.group == '次domain'",
                              selectInput(inputId = "vvrr6",
                                          label = "選取變數",
                                          choices = colnames(DD)[c(40:44,46:50,52:55)],
                                          selected = colnames(DD)[40])
                            )
           ),
           column(8,plotOutput("P11")),column(3),column(2),
           column(3,br(),br(),verbatimTextOutput("S11")))),
           
           navbarMenu(h5('雷達圖',style = "font-family:Microsoft JhengHei"),
                      tabPanel(h5('n=15(數值)',style = "font-family:Microsoft JhengHei"),style = "font-family:Microsoft JhengHei",
                               column(1),
                               column(1, br(),br(),
                                      textInput("Num"," 編號1~15", 1), align = "center"),
                               column(1),
                               column(8,
                                      tabsetPanel(
                                        tabPanel(h5('陽虛分數',style = "font-family:Microsoft JhengHei"),
                                                 plotOutput("P21")),
                                        tabPanel(h5('陰虛分數',style = "font-family:Microsoft JhengHei"),
                                                 plotOutput("P22")),
                                        tabPanel(h5('痰瘀分數',style = "font-family:Microsoft JhengHei"),
                                                 plotOutput("P23"))
                                        
                                      ))),
                      tabPanel(h5('n=11(百分比)',style = "font-family:Microsoft JhengHei"),style = "font-family:Microsoft JhengHei",
                               column(1),
                               column(1, br(),br(),
                               selectInput(inputId = "Num1",
                                label = "編號",
                                choices = c(1,2,3,4,6,7,8,9,10,11,13),
                                selected = 1)),
                               column(1),
                               column(8,
                                      tabsetPanel(
                                        tabPanel(h5('陽虛分數',style = "font-family:Microsoft JhengHei"),
                                                 plotOutput("P2222")),
                                        tabPanel(h5('陰虛分數',style = "font-family:Microsoft JhengHei"),
                                                 plotOutput("PPAP")),
                                        tabPanel(h5('痰瘀分數',style = "font-family:Microsoft JhengHei"),
                                                 plotOutput("PPPP"))
                                        
                                      )))),
           
           navbarMenu(h5('折線圖',style = "font-family:Microsoft JhengHei"),
                      tabPanel(h5('n=5 (百分比)',style = "font-family:Microsoft JhengHei"),style = "font-family:Microsoft JhengHei",
                               column(1),column(2,br(),br(),
                                                selectInput(inputId = "group5",
                                                            label = "器材分類",
                                                            choices = c("電阻式體脂計","血壓","體能","HRV","問卷得分","次domain"),
                                                            selected = "電阻式體脂計"),
                                                conditionalPanel(
                                                  condition = "input.group5 == 'HRV'",
                                                  selectInput(inputId = "vvrr25",
                                                              label = "選取變數",
                                                              choices = colnames(DD)[26:39],
                                                              selected = colnames(DD)[26])
                                                ),
                                                conditionalPanel(
                                                  condition = "input.group5 == '電阻式體脂計'",
                                                  selectInput(inputId = "vvrr26",
                                                              label = "選取變數",
                                                              choices = colnames(DD)[8:16],
                                                              selected = colnames(DD)[8])
                                                ),
                                                conditionalPanel(
                                                  condition = "input.group5 == '血壓'",
                                                  selectInput(inputId = "vvrr27",
                                                              label = "選取變數",
                                                              choices = colnames(DD)[17:20],
                                                              selected = colnames(DD)[17])
                                                ),
                                                conditionalPanel(
                                                  condition = "input.group5 == '體能'",
                                                  selectInput(inputId = "vvrr28",
                                                              label = "選取變數",
                                                              choices = colnames(DD)[21:25],
                                                              selected = colnames(DD)[21])
                                                ),
                                                conditionalPanel(
                                                  condition = "input.group5 == '問卷得分'",
                                                  selectInput(inputId = "vvrr29",
                                                              label = "選取變數",
                                                              choices = c("飲食得分","壓力得分","簡式得分","睡眠得分","陽虛得分","陰虛得分","痰瘀得分"),
                                                              selected = "飲食得分")
                                                ),
                                                conditionalPanel(
                                                  condition = "input.group5 == '次domain'",
                                                  selectInput(inputId = "vvrr30",
                                                              label = "選取變數",
                                                              choices = colnames(DD)[c(40:44,46:50,52:55)],
                                                              selected = colnames(DD)[40])
                                                )),
                               column(8,plotOutput("P33"))),
                      
                      tabPanel(h5('n=11(百分比)',style = "font-family:Microsoft JhengHei"),style = "font-family:Microsoft JhengHei",
                               column(1),column(10,
                                  tabsetPanel(
                                  tabPanel(h5('全體',style = "font-family:Microsoft JhengHei"),style = "font-family:Microsoft JhengHei",
                               column(2,br(),br(),
                                                selectInput(inputId = "group4",
                                                            label = "器材分類",
                                                            choices = c("電阻式體脂計","血壓","體能","HRV","問卷得分","次domain"),
                                                            selected = "電阻式體脂計"),
                                                conditionalPanel(
                                                  condition = "input.group4 == 'HRV'",
                                                  selectInput(inputId = "vvrr19",
                                                              label = "選取變數",
                                                              choices = colnames(DD)[26:39],
                                                              selected = colnames(DD)[26])
                                                ),
                                                conditionalPanel(
                                                  condition = "input.group4 == '電阻式體脂計'",
                                                  selectInput(inputId = "vvrr20",
                                                              label = "選取變數",
                                                              choices = colnames(DD)[8:16],
                                                              selected = colnames(DD)[8])
                                                ),
                                                conditionalPanel(
                                                  condition = "input.group4 == '血壓'",
                                                  selectInput(inputId = "vvrr21",
                                                              label = "選取變數",
                                                              choices = colnames(DD)[17:20],
                                                              selected = colnames(DD)[17])
                                                ),
                                                conditionalPanel(
                                                  condition = "input.group4 == '體能'",
                                                  selectInput(inputId = "vvrr22",
                                                              label = "選取變數",
                                                              choices = colnames(DD)[21:25],
                                                              selected = colnames(DD)[21])
                                                ),
                                                conditionalPanel(
                                                  condition = "input.group4 == '問卷得分'",
                                                  selectInput(inputId = "vvrr23",
                                                              label = "選取變數",
                                                              choices = c("飲食得分","壓力得分","簡式得分","睡眠得分","陽虛得分","陰虛得分","痰瘀得分"),
                                                              selected = "飲食得分")
                                                ),
                                                conditionalPanel(
                                                  condition = "input.group4 == '次domain'",
                                                  selectInput(inputId = "vvrr24",
                                                              label = "選取變數",
                                                              choices = colnames(DD)[c(40:44,46:50,52:55)],
                                                              selected = colnames(DD)[40])
                                                )),
                               column(1),
                               column(8,plotOutput("P32"))),
                               tabPanel(h5('個人',style = "font-family:Microsoft JhengHei"),style = "font-family:Microsoft JhengHei",
                                        column(2,br(),br(),
                                                         selectInput(inputId = "group6",
                                                                     label = "器材分類",
                                                                     choices = c("電阻式體脂計","血壓","體能","HRV","問卷得分","次domain"),
                                                                     selected = "電阻式體脂計"),
                                                         conditionalPanel(
                                                           condition = "input.group6 == 'HRV'",
                                                           selectInput(inputId = "vvrr31",
                                                                       label = "選取變數",
                                                                       choices = colnames(DD)[26:39],
                                                                       selected = colnames(DD)[26])
                                                         ),
                                                         conditionalPanel(
                                                           condition = "input.group6 == '電阻式體脂計'",
                                                           selectInput(inputId = "vvrr32",
                                                                       label = "選取變數",
                                                                       choices = colnames(DD)[8:16],
                                                                       selected = colnames(DD)[8])
                                                         ),
                                                         conditionalPanel(
                                                           condition = "input.group6== '血壓'",
                                                           selectInput(inputId = "vvrr33",
                                                                       label = "選取變數",
                                                                       choices = colnames(DD)[17:20],
                                                                       selected = colnames(DD)[17])
                                                         ),
                                                         conditionalPanel(
                                                           condition = "input.group6 == '體能'",
                                                           selectInput(inputId = "vvrr34",
                                                                       label = "選取變數",
                                                                       choices = colnames(DD)[21:25],
                                                                       selected = colnames(DD)[21])
                                                         ),
                                                         conditionalPanel(
                                                           condition = "input.group6== '問卷得分'",
                                                           selectInput(inputId = "vvrr35",
                                                                       label = "選取變數",
                                                                       choices = c("飲食得分","壓力得分","簡式得分","睡眠得分","陽虛得分","陰虛得分","痰瘀得分"),
                                                                       selected = "飲食得分")
                                                         ),
                                                         conditionalPanel(
                                                           condition = "input.group6== '次domain'",
                                                           selectInput(inputId = "vvrr36",
                                                                       label = "選取變數",
                                                                       choices = colnames(DD)[c(40:44,46:50,52:55)],
                                                                       selected = colnames(DD)[40])
                                                         )),
                                        column(1),
                                        column(8, plotOutput("P34")),
                                        column(1,
                                               br(),
                                               checkboxGroupInput("WhoseNum2", "編號",
                                                                  unique(t$ID), selected = unique(t$ID)))
                               )))),
           
           tabPanel(h5('n=15(數值)',style = "font-family:Microsoft JhengHei"),style = "font-family:Microsoft JhengHei",
                    column(1),column(2,br(),br(),
                           selectInput(inputId = "group2",
                                       label = "器材分類",
                                       choices = c("電阻式體脂計","血壓","體能","HRV","問卷得分","次domain"),
                                       selected = "電阻式體脂計"),
                           conditionalPanel(
                             condition = "input.group2 == 'HRV'",
                             selectInput(inputId = "vvrr7",
                                         label = "選取變數",
                                         choices = colnames(DD)[26:39],
                                         selected = colnames(DD)[26])
                           ),
                           conditionalPanel(
                             condition = "input.group2 == '電阻式體脂計'",
                             selectInput(inputId = "vvrr8",
                                         label = "選取變數",
                                         choices = colnames(DD)[8:16],
                                         selected = colnames(DD)[8])
                           ),
                           conditionalPanel(
                             condition = "input.group2 == '血壓'",
                             selectInput(inputId = "vvrr9",
                                         label = "選取變數",
                                         choices = colnames(DD)[17:20],
                                         selected = colnames(DD)[17])
                           ),
                           conditionalPanel(
                             condition = "input.group2 == '體能'",
                             selectInput(inputId = "vvrr10",
                                         label = "選取變數",
                                         choices = colnames(DD)[21:25],
                                         selected = colnames(DD)[21])
                           ),
                           conditionalPanel(
                             condition = "input.group2 == '問卷得分'",
                             selectInput(inputId = "vvrr11",
                                         label = "選取變數",
                                         choices = c("飲食得分","壓力得分","簡式得分","睡眠得分","陽虛得分","陰虛得分","痰瘀得分"),
                                         selected = "飲食得分")
                           ),
                           conditionalPanel(
                             condition = "input.group2 == '次domain'",
                             selectInput(inputId = "vvrr12",
                                         label = "選取變數",
                                         choices = colnames(DD)[c(40:44,46:50,52:55)],
                                         selected = colnames(DD)[40])
                           )),
                    column(7, plotOutput("P31")),
                    column(1,
                    br(),
                    checkboxGroupInput("WhoseNum", "編號",
                                       c(1:15), selected = c(1:15)))
                    ))
                    
           )
                            
                            
                            

                            
