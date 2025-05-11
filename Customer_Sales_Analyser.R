library(shiny)
library(bslib)
library(shinythemes)
library(arules)
library(arulesViz)




#هنا بنكود الاحنا عايزينه يظهر للمستخدم
#user interface
ui<-page_sidebar(
  theme=shinytheme("united"),
  
  #عشان نعرف نتحكم بيها ياترسلنا المدخلات يا تطلعنا النتائج وطبعا حسب الميثود ياما 
  #input or output
  
  title="Customer Sales Data Analysis",
  #هنا بنحط المدخلات المستخدم هيستخدمها عشان يتفاعل مع التطبيق
  sidebar=sidebar(
    #to separate between data in file's user
    radioButtons("data_t","select which data type to upload",c(csv=",",Tab="\t")),
    #user input his file data .path
    fileInput("data_f","upload your data"),
    #من اسمها عامله زي    label
    helpText("K_means panel"),
    #image
    tags$hr(),
    #user input number only 
    numericInput(
      inputId="nCluster",
      label="Enter (n) groups between 2 & 4",
      value=2,
      min = 2,
      max = 4,
      width = NULL
    ),
    helpText("Apriori panel"),
    numericInput(
      inputId="minsupport",
      label="Enter min support between .001 & 1",
      value=.001,
      min = .001,
      max = 1,
      width = NULL,
      step = .001
    ),
    numericInput(
      inputId="minconfidence",
      label="Enter min confidence between .001 & 1",
      value=.001,
      min = .001,
      max = 1,
      width = NULL,
      step = .001
    )
  )
  ,
  #we used it to display tab panels or other things
  #بتسمح لينا نتحكم بالصفحه اكثر زي اننا نظهر بانلز جديده وهتعرف ازاي بنستخدمها في الاكواد القادمه
  uiOutput("ui")
  
)
#backend



#وهنا بتعمل العمليات المابيشوفهاش المستخدم عشان نطلع النتائج الهو عايزها 
#وانا استخدمت الخطوات العملتوها وحطتها هنا 
#we using (output$(inputID)) when we want using each method in (front end)to display data ,
#or we can use (variable<-(reactive)method to do some processes to use it in (output$(inputID))  )


server <- function(input, output) {
  
  #bs_themer()
  
  
  #to display data path, file name and file size when user inputs his file 
  
  output$out_data_f1<-renderTable({
    if(is.null(input$data_f)){
      return()
    }
    else{
      input$data_f
    }
  })
  
  
  
  
  
  
  
  #cleaning data 
  
  cleaned_data<-reactive({
    
    if(is.null(input$data_f)){#if the file doesn't contain usable data, the program will return nothing until the user inputs a file with usable data
      return()
    }
    else{
      
      
      df=data_table()
      
      
      ##### a-first check duplicated entries######
      dff= unique(df)
      sum(duplicated(df))
      
      #######b-second check data structure (data type) ######
      is.character(df$items)
      
      is.integer (df$count)
      
      is.integer(df$total)
      
      is.integer(df$rnd)
      
      is.character(df$customer)
      
      is.integer(df$age)
      
      is.character(df$city)
      
      is.character(df$paymentType)
      
      
      #######c-third finding missing values (NA) #######
      #df = na.omit(NA)#مش لازم لا معنديش NA
      sum(is.na(df))
      
      
      
      #######d-fourth Handling outliers ######
      #boxplot(df)  >>>>>>>>> error because there is no character column
      
      
      #عاوز بقا احدد القسم العشوائيه وامسحها علشان مبيوظليش تحليل البيانات بتعتي  فبحددها وامسحها 
      boxplot(df[,c("age","count","rnd","total")])
      
      
      
      #lonely check
      outliers= boxplot(df$count)$out
      
      print(outliers)
      
      summary(df$count)
      
      df <- df[-which(df$count%in%outliers),]
      
      
      
      
      return(df)
    }
  })
  
  
  #to do Algorithm we used [reactive({})] each Graph and return it like when we define function 
  
  #we used (cleaned_data) in each Graph 
  
  #FIRST----TABPANEL++++++++++++++++++  
  
  #####################################($visualization by GUI$)###############################################################
  
  # compare between limit  number of types(2,3,....,10) in one columns use (different values(cash&credit) to one variable(payment type))>>>>> pie chart
  
  # count the frequencies & get probabilities
  
  ######################FIRST---GRAPH
  
  countt<-reactive({
    df=cleaned_data()
    countt<- table(df$paymentType)
    percentage_countt<- paste0(round(100*countt/sum(countt),2),"%")# حطيت 2 علشان يبان الفرق علشان التقريب هيطلع 50  50
    
    
    
    
    
    return(countt)
  })
  
  percentage_countt<-reactive({
    df=cleaned_data()
    countt<- table(df$paymentType)
    percentage_countt<- paste0(round(100*countt/sum(countt),2),"%")# حطيت 2 علشان يبان الفرق علشان التقريب هيطلع 50  50
    
    
    
    
    
    return(percentage_countt)
  })
  
  
  
  
  
  # رسم الرسم البياني الدائري `pie`
  
  output$Graph1<-renderPlot({
    pie(
      countt(),
      labels = percentage_countt(),
      main = "Comparing between cash and card",
      col = c("red", "blue"),
      border="white"  
    )
    
    legend(
      "bottomleft",
      legend=c("Cash","Credit"),
      fill=c("red","blue") ,
      angle = 30,
      cex = 0.8   #  للتحكم في حجم النص
      
      
    )
    
    
  })
  
  ##########################SECOND--GRAPH
  
  
  ####b-Show each city's total spending and arrange the totals in a descending order####
  
  
  #هنا بنحسب مجموع الذي تم صرفه لكل مدينه وهو مرتب ترتيب تنازلي
  
  
  ssum<-reactive({
    
    
    ####b-Show each city total spending and arrange it by total descending  ####
    df=cleaned_data()
    
    ggroup <- unique(df$city)# اي هي المدن الي عندي
    #print(ggroup) مش لازم اطبعها
    
    
    ssum <- tapply(df$total , df$city , FUN=sum)
    
    #print(ssum)
    ssum <- sort(ssum , decreasing =TRUE)#رتب من الاكبر للاصغر
    
    
    
    return(ssum)
    
    
  })
  
  output$Graph2<-renderPlot({
    
    barplot(
      ssum(),
      xlab="city",
      #ylab="total",
      col=c("#87CEEB","#D1D1D1"),
      main="TOTAL SPENDING FOR EACH CITY",
      space = 0.5,  # المسافة بين الأعمدة
      las = 2,  # تدوير التسميات المحورية لعرض أسماء المدن بشكل أفقي
      cex.names = 0.8  # تكبير حجم النص في تسميات المحور السفلي
      
    )
  })
  
  
  ############################THIRD-----Graph
  
  
  ####c-Compare (each) age and sum of total spending. ####
  
  summ<-reactive({
    # tapply function to sum & summarize data in base package
    # tapply(<<column you need to sum it>> , << column you need summarize it for sum>>)
    df=cleaned_data()
    
    # to show levels of age
    groupp <- (df$age)
    summ <- tapply(df$total , df$age , FUN=sum)
    
    
    return(summ)
    
  })
  
  
  output$Graph3<-renderPlot({
    
    par(mar = c(5, 8, 4, 2) + 0.1)
    
    df=cleaned_data()
    
    
    plot(
      x = unique(df$age),  # The unique ages
      y = summ(),  # Sum of total spending for each age
      main = "CLASSIFY AGE & SUM OF ITS TOTAL SPENDING",
      xlab = "Age",
      ylab = "",
      pch=19,  # Plot character
      col = "navy",  # Color of the points  
      xlim = c(min(df$age), max(df$age)),  # Set the x-axis range from minimum to maximum age
      las=2 , # تدوير تسميات المحور x إلى الوضع الأفقي
      
      ylim = c(0, 2500000)  # Set the y-axis range to match what you had in barplot()
    )
    
    
    
    axis(
      side = 1,  # محور x
      at = unique(df$age),  # تحديد النقاط الفريدة للأعمار
      labels = unique(df$age),  # تحديد التسميات الفريدة لكل نقطة عمر
      las=2,  # تدوير تسميات المحور x إلى الوضع الأفقي
      cex.axis = 0.9, # تعديل حجم تسميات المحور x
      
      
    )
    
    # إضافة خطوط الشبكة إلى الرسم البياني
    grid(
      col = "gray",  # لون خطوط الشبكة
      lty = "solid",  # نوع خطوط الشبكة (مخططة)
      lwd = 1.5 # عرض خطوط الشبكة
    )
    
  })
  
  
  ####################################FOURTH----Graph
  
  ####d-Display the distribution of total spending. #### 
  
  # distribution >>>> boxplot
  
  
  
  output$Graph4<-renderPlot({
    df=cleaned_data()
    
    boxplot(
      x=df$total ,
      main="THE DISTRIBUTION OF TOTAL SPENDING",
      xlab="total spending",
      col = "#4A708B"
      
      #ومش هيظهر OUTLIERSعلشان مسحتهم 
      
    )
  })
  
  
  ################################################($K_MEANS$)#######################################
  
  
  output$K_means<-renderTable({
    df=cleaned_data()
    
    km <- kmeans(df[,c("age","total")],centers =  input$nCluster  )
    
    #from outputs of km data frame column called cluster that has classified clusters
    #adding this column to database
    df$cluster <- km$cluster
    
        print(df[,c("customer","age","total","cluster")])
    
  })

  
  
  
  
  
  
  
  
  #########################################($ASSOCIATION$)############################################## 
  
  
  
  ############ 4-Generate association rules ###############
  
  
  
  
  
  
  
  # if we convert this column to transaction by as()  
  # dff <- as(dff, "transactions")  >>> error:  [ no method or default for coercing “character” to “transactions” ]
  # The error you're encountering indicates that there isn't a direct method or default behavior to convert a character vector directly into a transactions object.
  
  
  #This splits the character vector df$Items into a list of vectors, where each vector corresponds to a row in the data frame df. It splits the strings based on a separator, which is by default any whitespace character. In this case, seq(nrow(df)) generates a sequence of numbers from 1 to the number of rows in the data frame, and each number is used as a group identifier for splitting.
  
  
  #####dff <- as(df$item , "transactions")
  
  #Error in as(df$item, "transactions") :    
  #no method or default for coercing “character” to “transactions”   #هيقرأ بدون فواصل
  
  output$association<-renderTable({
    df=cleaned_data()
    
    dff <- strsplit(as.character(df$items),",")
    tr <- as(dff ,"transactions")
    
    
    # as(split , "r..) >>> Wrong, it does not return what's needed because can not have a function in it
    
   #هاخد المعلومات من المستخدم    
    
    
    AP <- apriori(tr, parameter = list(supp = as.numeric(input$minsupport), conf = as.numeric(input$minconfidence), minlen = 2))
    print(AP)
    inspect(AP)
  }) 
  
  
  
  ######################################################################################################
  
  #data_table used it to display data user before cleaning and implement Algorithm
  
  
  data_table<-reactive({
    if(is.null(input$data_f)){#if the entered file doesn't have data will return nothing until the user inputs a file with data
      return()
    }
    else{
      file_space<-input$data_f
      tt<-read.table(file_space$datapath,header = TRUE,sep=input$data_t)
      return(tt)
    }
  }) 
  
  
  
  
  
  
  
  
  
  #display data file when user input file   
  output$out_data_f2<-renderTable({
    if(is.null(input$data_f)){
      return()
    }
    else{
      data_table()
    }
  })
  
  ###it creating dynamically three tab panel when user input file data with main tab panel and first panel is about file's user,second dashboard for 4Graphs,third K_means and fourth association
  
  output$ui <- renderUI({
    if(is.null(input$data_f))
      #image
      h5("Powered by", tags$img(src="RStudio-Ball.png", height=200, width=200))
    else
      tabsetPanel(tabPanel("About file", tableOutput("out_data_f1")
                           ,tableOutput("out_data_f2")
      ),
      tabPanel("Graphs",layout_columns( card("Graph1",plotOutput("Graph1")),
                                        card("Graph2",plotOutput("Graph2"))),
               layout_columns(card("Graph3",plotOutput("Graph3")),
                              card("Graph4",plotOutput("Graph4")))),
      tabPanel("K_mean", tableOutput("K_means")),
      tabPanel("Apriori",tableOutput("association")))
  })
  
}









shinyApp(ui, server)


