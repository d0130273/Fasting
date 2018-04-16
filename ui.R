library(shiny)

navbarPage(title = h4(strong('禁食',style = "font-family:Microsoft JhengHei")),style = "font-family:Microsoft JhengHei",
           tabPanel(h5('資料表',style = "font-family:Microsoft JhengHei"),style = "font-family:Microsoft JhengHei",
                    column(1),column(10,
                                     tabsetPanel(
                                       tabPanel(h5('起始',style = "font-family:Microsoft JhengHei"),style = "font-family:Microsoft JhengHei",
                                                DT::dataTableOutput("T1")),
                                       tabPanel(h5('中繼',style = "font-family:Microsoft JhengHei"),style = "font-family:Microsoft JhengHei",
                                                DT::dataTableOutput("T2")),
                                       tabPanel(h5('終焉',style = "font-family:Microsoft JhengHei"),style = "font-family:Microsoft JhengHei",
                                                DT::dataTableOutput("T3")),
                                       tabPanel(h5('復食1',style = "font-family:Microsoft JhengHei"),style = "font-family:Microsoft JhengHei",
                                                DT::dataTableOutput("T4")),
                                       tabPanel(h5('復食2',style = "font-family:Microsoft JhengHei"),style = "font-family:Microsoft JhengHei",
                                                DT::dataTableOutput("T5"))
                                     ))),
           tabPanel(h5('盒形圖',style = "font-family:Microsoft JhengHei"),style = "font-family:Microsoft JhengHei",
           column(1),column(2,
                            selectInput(inputId = "Num",
                                        label = "第幾次",
                                        choices = c("平均","1","2","3","4","5"),
                                        selected = "平均"),
                            selectInput(inputId = "group",
                                        label = "器材分類",
                                        choices = c("HRV","電阻式體脂計","血壓","體能","飲食","壓力","簡式"),
                                        selected = "HRV"),
                            conditionalPanel(
                              condition = "input.group == 'HRV'",
                              selectInput(inputId = "vvrr1",
                                          label = "選取變數",
                                          choices = c("1","2","3","4","5"),
                                          selected = "1")
                            ),
                            conditionalPanel(
                              condition = "input.group == '電阻式體脂計'",
                              selectInput(inputId = "vvrr1",
                                          label = "選取變數",
                                          choices = c("1","2","3","4","7"),
                                          selected = "1")
                            ),
                            conditionalPanel(
                              condition = "input.group == '血壓'",
                              selectInput(inputId = "vvrr1",
                                          label = "選取變數",
                                          choices = c("1","2","3","4","6"),
                                          selected = "1")
                            ),
                            conditionalPanel(
                              condition = "input.group == '體能'",
                              selectInput(inputId = "vvrr1",
                                          label = "選取變數",
                                          choices = c("1","2","3","4","545"),
                                          selected = "1")
                            ),
                            conditionalPanel(
                              condition = "input.group == '飲食'",
                              selectInput(inputId = "vvrr1",
                                          label = "選取變數",
                                          choices = c("1","2","3","4","5"),
                                          selected = "1")
                            ),
                            conditionalPanel(
                              condition = "input.group == '壓力'",
                              selectInput(inputId = "vvrr1",
                                          label = "選取變數",
                                          choices = c("1","2","3","4","5"),
                                          selected = "1")
                            ),
                            conditionalPanel(
                              condition = "input.group == '簡式'",
                              selectInput(inputId = "vvrr1",
                                          label = "選取變數",
                                          choices = c("1","2","3","4","5"),
                                          selected = "1")
                            )
           )))
                            
                            
                            
                            
                            
