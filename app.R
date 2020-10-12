
library(shiny)
library(sets)

#Define Universo
sets_options("universe", seq(1,100, 1))

#criacao de variaveis
variaveis <- set(
    Gosta_exatas = fuzzy_partition(varnames = c( gemin = 10, gemen = 25, gemed = 50, gemaior=75, gemax=100), sd = 10),
    Rel_interpess = fuzzy_partition(varnames = c( rimin = 10, rimen = 25, rimed = 50, rimaior=75,rimax=100), sd = 10),
    Escre_codigo = fuzzy_partition(varnames = c( ecmin = 10, ecmen = 25, ecmed = 50, ecmaior=75,ecmax=100),  sd = 10),
    Pefil_lider = fuzzy_partition(varnames = c( plmin = 10, plmen = 25, plmed = 50, plmaior=75,plmax=100), sd = 10),
    Gosta_Estudar = fuzzy_partition(varnames = c( gsmin = 10, gsmen = 25, gsmed = 50, gsmaior=75,gsmax=100), sd = 10),
    Habilidade_comunicao = fuzzy_partition(varnames = c( hcmin = 10, hcmen = 25, hcmed = 50, hcmaior=75, hcmax=100), sd = 10),
    Classificacao = fuzzy_partition(varnames = c( baixa = 10, media = 50, alta = 75, altissima=95), sd = 10)
)

#definicao das regras
regras <-
    set(
        fuzzy_rule( Gosta_exatas %is%	gemax	&& Rel_interpess %is%	rimin	&& Escre_codigo %is%	ecmax	&& Pefil_lider %is%	plmin	&& Gosta_Estudar %is%	gsmax	&& Habilidade_comunicao %is%	hcmin	, Classificacao %is%	altissima	),
        fuzzy_rule( Gosta_exatas %is%	gemax	&& Rel_interpess %is%	rimen	&& Escre_codigo %is%	ecmaior	&& Pefil_lider %is%	plmin	&& Gosta_Estudar %is%	gsmax	&& Habilidade_comunicao %is%	hcmen	, Classificacao %is%	altissima	),
        fuzzy_rule( Gosta_exatas %is%	gemaior	&& Rel_interpess %is%	rimen	&& Escre_codigo %is%	ecmaior	&& Pefil_lider %is%	plmin	&& Gosta_Estudar %is%	gsmax	&& Habilidade_comunicao %is%	hcmed	, Classificacao %is%	altissima	),
        fuzzy_rule( Gosta_exatas %is%	gemaior	&& Rel_interpess %is%	rimen	&& Escre_codigo %is%	ecmaior	&& Pefil_lider %is%	plmen	&& Gosta_Estudar %is%	gsmaior	&& Habilidade_comunicao %is%	hcmed	, Classificacao %is%	alta	),
        fuzzy_rule( Gosta_exatas %is%	gemaior	&& Rel_interpess %is%	 rimed	&& Escre_codigo %is%	ecmed	&& Pefil_lider %is%	plmed	&& Gosta_Estudar %is%	gsmaior	&& Habilidade_comunicao %is%	hcmaior	, Classificacao %is%	alta	),
        fuzzy_rule( Gosta_exatas %is%	gemaior	&& Rel_interpess %is%	 rimed	&& Escre_codigo %is%	ecmed	&& Pefil_lider %is%	plmaior	&& Gosta_Estudar %is%	gsmaior	&& Habilidade_comunicao %is%	hcmaior	, Classificacao %is%	alta	),
        fuzzy_rule( Gosta_exatas %is%	gemed	&& Rel_interpess %is%	rimaior	&& Escre_codigo %is%	ecmen	&& Pefil_lider %is%	plmaior	&& Gosta_Estudar %is%	gsmed	&& Habilidade_comunicao %is%	hcmaior	, Classificacao %is%	media	),
        fuzzy_rule( Gosta_exatas %is%	gemed	&& Rel_interpess %is%	rimaior	&& Escre_codigo %is%	ecmen	&& Pefil_lider %is%	plmaior	&& Gosta_Estudar %is%	gsmed	&& Habilidade_comunicao %is%	hcmax	, Classificacao %is%	media	),
        fuzzy_rule( Gosta_exatas %is%	gemen	&& Rel_interpess %is%	rimax	&& Escre_codigo %is%	ecmin	&& Pefil_lider %is%	plmax	&& Gosta_Estudar %is%	gsmed	&& Habilidade_comunicao %is%	hcmax	, Classificacao %is%	media	),
        fuzzy_rule( Gosta_exatas %is%	gemen	&& Rel_interpess %is%	rimax	&& Escre_codigo %is%	ecmin	&& Pefil_lider %is%	plmax	&& Gosta_Estudar %is%	gsmen	&& Habilidade_comunicao %is%	hcmax	, Classificacao %is%	baixa	),
        fuzzy_rule( Gosta_exatas %is%	gemin	&& Rel_interpess %is%	rimax	&& Escre_codigo %is%	ecmin	&& Pefil_lider %is%	plmax	&& Gosta_Estudar %is%	gsmen	&& Habilidade_comunicao %is%	hcmax	, Classificacao %is%	baixa	),
        fuzzy_rule( Gosta_exatas %is%	gemin	&& Rel_interpess %is%	rimax	&& Escre_codigo %is%	ecmin	&& Pefil_lider %is%	plmax	&& Gosta_Estudar %is%	gsmin	&& Habilidade_comunicao %is%	hcmax	, Classificacao %is%	baixa	)
    )

sistema = fuzzy_system(variaveis, regras)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Teste Vocacional para Cientista de Dados"),
    helpText("Ajuste os sliders e clique em processar"),
    fluidRow(
        column(
            4,
            sliderInput(inputId = "exatas", label = "Gosto por Exatas", min = 10, max = 100, step = 5, value = 50)
        ),
        column(
            4,
            sliderInput(inputId = "inter", label = "Relacionamento Interpessoal", 
                        min = 10, max = 100, step = 5, value = 50)
        ),
        column(
            4,
            sliderInput(inputId = "codigo", label = "Gosto por escrever Código", 
                        min = 10, max = 100, step = 5, value = 50)
        )
    ),
    fluidRow(
        column(
            4,
            sliderInput(inputId = "lider", label = "Perfil de Liderança", min = 10, max = 100, step = 5, value = 50)
        ),
        column(
            4,
            sliderInput(inputId = "estudar", label = "Gosta de Estudar", min = 10, max = 100, step = 5, value = 50)
        ),
        column(
            4,
            sliderInput(inputId = "comunica", label = "Habilidade de Comunicação", 
                        min = 10, max = 100, step = 5, value = 50)
        )
    ),
    fluidRow(
        column(
            6,
            h1("Sistema:"),
            plotOutput(outputId = "grafSistema")
        ),
        column(
            6,
            actionButton(inputId = "processar", label = "Processar"),
            helpText("A linha vermalha mostra sua aderência a profissão de Ciêntista de Dados"),
            plotOutput(outputId = "grafResultado")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    output$grafSistema = renderPlot({
        plot(sistema)
    })
    
    observeEvent(input$processar, {
        inferencia = fuzzy_inference(sistema, list(
            Gosta_exatas = input$exatas,
            Rel_interpess = input$inter,
            Escre_codigo = input$codigo,
            Pefil_lider = input$lider,
            Gosta_Estudar = input$estudar,
            Habilidade_comunicao = input$comunica
        ))
        
        output$grafResultado = renderPlot({
            plot(sistema$variables$Classificacao)
            lines(inferencia, col='red', lwd=4)
        })
    })
    
   
}

# Run the application 
shinyApp(ui = ui, server = server)
