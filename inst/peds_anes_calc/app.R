#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
library(shiny)

# Define UI for dataset viewer application
ui <- fluidPage(
  # Application title
  titlePanel("Pediatric Anesthesia calculator"),
  # Sidebar with controls to select a dataset and specify the number
  # of observations to view
  sidebarLayout(
    sidebarPanel(
      radioButtons(inputId = "gender", label = "Gender:",
                   choices = c("Male" = "male", "Female" = "female"),
                   selected = "male"),
      # TODO: grey out the other box: months or years, e.g., using
      # https://stackoverflow.com/questions/19611254/r-shiny-disable-able-shinyui-elements
      radioButtons(inputId = "age_unit", label = "Age in months or years:",
                   choices = c("Months" = "months", "Years" = "years"),
                   selected = "years"),
      numericInput("age_months", "Age in 1-12 months", min = 0, max = 12, value = 0),
      # input: age 1 to 12 years
      numericInput("age_years", "Age between 1-12 years", min = 1, max = 12, value = 5),
      #radioButtons(inputId = "premature", "Prematurity?",
      #             choices = c("Full term" = "term", "Premature" = "premature"))
      checkboxInput("premature", "Prematurity", FALSE),
      #weight of patient # TODO: can use physiology or other package to estimate
      #median values here automatically
      sliderInput("weight", "Weight (kg):",
                  min =  0, max = 500 , value = 0, step = 1, post = " kg"),
      sliderInput("height", "Body Length or Height (cm):",
                  min = 2, max = 300,
                  value = 0, step = 1, post = " cm"),
      sliderInput("fasting_hours", "NPO duration:",
                  min =  1, max = 8 ,
                  value = 0, step = 1, post = "hour"),
      radioButtons("fluid_trauma", label = h5("Severity of surgical trauma"),
                   choices = list("None", "Mild", "Severe"),
                   selected = 0),
      #duration of surgery # TODO: start a timer automatically once app is
      #openned: do before induction!
      sliderInput("surgerytime", "Duration of surgery",
                  min = 0, max = 10,
                  value = 1, post = "hours"),
      sliderInput("Hb_value", "Hemoglobin level",
                  min = 1, max = 25,
                  value = 0.1, step = 0.1, post = "(mg/dL)"),
      sliderInput("Hct_value", "Hematocrit level",
                  min = 15, max = 70,
                  value = 0.1, step = 0.1, post = "(%)")
    ),
    # input: weight
    # Main panel for displaying outputs ----
    mainPanel(
      tags$b(tags$h4("BMI and estimated blood volume")),
      textOutput("BMI"),
      textOutput("est_blood_volume"),
      tags$br(),
      tags$b(tags$h4("Maintenance fluid requirements")),
      tags$i(tags$h5("Please indicate patient's age/weight/fasting duration/severity of surgical tissue trauma to avoid error warning.")),
      textOutput("elective_fasting_infant"),
      textOutput("elective_fasting_child"),
      tags$br(),
      textOutput("fluid_deficit"),
      textOutput("fluid_trauma"),
      tags$br(),
      textOutput("first_hour_fluid"),
      textOutput("second_hour_fluid"),
      textOutput("third_hour_fluid"),
      textOutput("hypovolemia_bolus"),
      tags$br(),
      tags$b(tags$h4("Pediatric Airway Equipment")),
      textOutput("ETT_uncuffed"),
      textOutput("ETT_cuffed"),
      textOutput("ETT_depth"),
      tags$br(),
      tags$b(tags$h4("Estimated Blood Volume and Blood Transfusion Requirements")),
      textOutput("EBV"),
      textOutput("ABL"),
      #widget for fluid amount  - deficit therapy (421 rule based on weight), maintenance therapy (IWL+ urine), replacement therapy(blood loss + third space losses)
      # adjust the maintenance to neonate vs child
      #hematocrit level corresponds to fluid replacement
      tags$br(),
      tags$b(tags$h4("Preoperative Sedatives")),
      textOutput("chloral_hydrate"), #??? only used in US by dentists, occasionally
      textOutput("methohexital"), # rectal methohexital v unusual in USA.
      textOutput("midazolam"),
      textOutput("pentobarbital"),
      textOutput("dexmedetomidine"),
      tags$br(),
      tags$b(tags$h4("Induction Dose")),
      textOutput("induction_ketamine_IV"),
      textOutput("induction_ketamine_IM"),
      textOutput("induction_propofol"),
      textOutput("induction_thiopentone"),
      tags$br(),
      tags$b(tags$h4("Muscle Relaxants")),
      textOutput("relaxant_sux"),
      textOutput("relaxant_rox"),
      textOutput("relaxant_atracurium"),
      textOutput("relaxant_pancuronium"),
      tags$br(),
      tags$b(tags$h4("Reversals")),
      textOutput("neostigmine"),
      textOutput("atropine"),
      textOutput("glycopyrrolate"),
      textOutput("sugammadex"),
      tags$br(),
      tags$b(tags$h4("Resuscitation Drugs")),
      textOutput("resus_adenosine"),
      tags$br(),
      textOutput("resus_adrenaline"),
      textOutput("resus_amiodarone"),
      textOutput("resus_atropine"),
      textOutput("resus_bicarbonate"),
      textOutput("resus_ca_chloride"),
      textOutput("resus_ephedrine"),
      textOutput("resus_magnesium"),
      textOutput("resus_phenyl"),
      textOutput("resus_sux"),
      tags$br(),
      tags$b(tags$h4("Anaphylaxis Drugs")),
      textOutput("ana_epinephrine"),
      textOutput("ana_fluid_bolus"),
      textOutput("ana_albuterol"),
      textOutput("ana_hydrocortisone"),
      textOutput("ana_diphenhydramine"),
      textOutput("ana_ranitidine"),
      tags$br(),
      tags$b(tags$h4("Local Anesthetic Dose")),
      textOutput("ana_lidocaine"),
      textOutput("ana_bupivacaine"),
      textOutput("ana_ropivacaine"),
      textOutput("la_toxicity"),
      tags$br(),
      tags$br(),
      tags$br(),
      tags$br(),
      tags$br(),
      tags$br(),
      tags$br(),
      tags$i(textOutput("disclaimer"))
    )
  )
)

#textoutput blood product transfusion guideline

#pRBC 10-15 mL/kg should raise Hgb 2-3 g/dL, Hct by 6-9%
#For > 20 mL/kg transfusion or cardiac cases, request < 5 day old or washed RBCs
#FFP 10-15 mL/kg should raise factor levels 15-20%
#Platelets 10-15 mL/kg should raise platelet count by 30-50,000
#Cryoprecipitate: 1-2 units/kg should increase fibrinogen level to 60-100 mg/dL
#DDAVP 0.1-0.3 mcg/kg given 30 min prior to procedure


# Define server logic required to plot various variables against mpg
server <- shinyServer(function(input, output, session) {

  # need age from months/years in a single unit for rest. we can still use
  # infant-specific when needed.
  age <- reactive({
    req(input$age_months) # ensure availablity of value before proceeding
    req(input$age_years)
    input$age_years <- ifelse(input$age_unit == "months",
                              input$age_months / 12,
                              input$age_years)
  })

  output$BMI <- renderText({
    # TODO: use physiology package function for this
    Height <- (input$height/100)*(input$height/100)
    Weight <- input$weight
    paste("BMI is =", Weight/Height)
  })
  output$est_blood_volume <- renderText({
    physiology::bloo
    if (age < 4 / 12) {
      low_est_blood_volume <- 80*input$weight
      high_est_blood_volume <- 90*input$weight
    } else {
      low_est_blood_volume <- 70*input$weight
      high_est_blood_volume <- 80*input$weight
    }
    if (input$child_age > 2) {
      high_est_blood_volume <- 80*input$weight
      low_est_blood_volume <- NULL
    } else {
      low_est_blood_volume <- 70*input$weight
      high_est_blood_volume <- 80*input$weight
    }
    paste("Estimated blood volume according to age and weight is :", low_est_blood_volume, "to", high_est_blood_volume , "mL")
  })
  output$elective_fasting_infant <- renderText({
    if (input$infant_age) {
      elective_fasting_milk <- 4
      elective_fasting_liquids <- 2
      elective_fasting_meals <- "none"
    }
    paste(c("Elective fasting time for infant-", elective_fasting_milk, " hours for breast milk, ", elective_fasting_liquids, " hours for liquids"))
  })
  output$elective_fasting_child <- renderText({
    if (input$child_age) {
      elective_fasting_liquids <- 2
      elective_fasting_meals <- 8
    }
    paste(c("Elective fasting time for child-", elective_fasting_meals, " hours for meals, ", elective_fasting_liquids, " hours for liquids"))
  })
  output$ETT_cuffed <- renderText({
    ETT_uncuffed <- (input$child_age/4 + 4)
    paste("Uncuffed ETT size : ", ETT_uncuffed )
  })
  output$ETT_uncuffed <- renderText({
    ETT_cuffed <- (input$child_age/4 + 3)
    paste("Cuffed ETT size : ", ETT_cuffed )
  })
  output$ETT_depth <- renderText({
    ETT_depth <- (input$child_age/2 + 12)
    paste("ETT depth for child is =", ETT_depth , "cm")
  })
  output$EBV <- renderText({
    EBV <- (input$weight * 80)
    paste("Estimated blood volume :", EBV , "ml")
  })
  output$ABL <- renderText({
    ABL <- ((input$Hct_value - 40)*(input$weight * 80)/(input$Hct_value))
    paste("Estimated allowable blood loss :", ABL, "ml")
  })
  output$chloral_hydrate <- renderText({
    chloral_hydrate <- input$weight * 75
    paste("Dose of Chloral Hydrate (mg):", chloral_hydrate)
  })
  output$midazolam <- renderText({
    midazolam  <- input$weight * 0.1
    paste("Dose of Midazolam (mg):", midazolam )
  })
  output$pentobarbital <- renderText({
    pentobarbital  <- input$weight * 4
    paste("Dose of Pentobarbital (mg):", pentobarbital)
  })
  output$methohexital <- renderText({
    methohexital  <- input$weight * 4
    paste("Dose of Methohexital (mg):", methohexital)
  })
  output$precedex <- renderText({
    precedex  <- input$weight * 2
    precedex2  <- input$weight * 2
    paste(c("Bolus dose of Dexmedetomidine over 5-10 mins:", precedex, " mcg,","followed by infusion dose of",precedex2, "mcg/kg/hour"))
  })
  output$fluid_deficit <- renderText({
    if (input$weight < 11) {
      fluid_deficit <- input$weight * 4*(input$fasting_hours)
    } else if (input$weight > 19) {
      fluid_deficit <- input$weight * (input$fasting_hours) + 60
    } else {
      fluid_deficit <- (input$weight * 2)*(input$fasting_hours) + 40
    }
    paste("Preoperative fluid losses based on fasting time:", fluid_deficit , "mL/hour")
  })
  output$hypovolemia_bolus <- renderText({
    low_range_bolus <- input$weight * 10
    high_range_bolus <- input$weight * 15
    paste("If patient is hypovolemic, administer",low_range_bolus , "to", high_range_bolus, "ml of fluid bolus")
  })
  output$fluid_trauma <- renderText({
    if (input$fluid_trauma == "Mild") {
      fluid_trauma <- input$weight * 2
    } else if (input$fluid_trauma == "Severe") {
      fluid_trauma <- input$weight * 10
    } else if (input$fluid_trauma == "None") {
      fluid_trauma <- input$weight * 0
    }
    paste("Hourly fluid requirement based on level of trauma: ", fluid_trauma , "mL/hour")
  })
  output$first_hour_fluid <- renderText({
    ({
      if (input$fluid_trauma == "Mild") {
        fluid_trauma <- input$weight * 2
      } else if (input$fluid_trauma == "Severe") {
        fluid_trauma <- input$weight * 10
      } else if (input$fluid_trauma == "None") {
        fluid_trauma <- input$weight * 0
      }
    })
    if (input$weight < 11) {
      #half of the deficit + maintenance fluid requirement + level of trauma from surgery
      first_hour_fluid <- input$weight * 2*(input$fasting_hours) + input$weight * 4  + fluid_trauma
    } else if (input$weight > 19) {
      first_hour_fluid  <- input$weight * (input$fasting_hours)*0.5 + 30 + 60 + fluid_trauma
    } else {
      first_hour_fluid  <- (input$weight * 2)*(input$fasting_hours)*0.5 + 20 + 40 + (input$weight * 2) + fluid_trauma
    }
    paste("Total fluid requirement in 1st hour : ", first_hour_fluid , "mL/hour", " + blood loss")
  })
  output$second_hour_fluid <- renderText({
    ({
      if (input$fluid_trauma == "Mild") {
        fluid_trauma <- input$weight * 2
      } else if (input$fluid_trauma == "Severe") {
        fluid_trauma <- input$weight * 10
      } else if (input$fluid_trauma == "None") {
        fluid_trauma <- input$weight * 0
      }
    })
    if (input$weight < 11) {
      second_hour_fluid <- input$weight * 4 + input$weight * 2*(input$fasting_hours)*0.5 + fluid_trauma
    } else if (input$weight > 19) {
      second_hour_fluid  <- input$weight * (input$fasting_hours)*0.5*0.5 + 30 + 60 + fluid_trauma
    } else {
      second_hour_fluid  <- (input$weight * 2)*(input$fasting_hours)*0.5*0.5 + 20 + 40 + (input$weight * 2) + fluid_trauma
    }
    paste("Total fluid requirement in 2nd hour: ", second_hour_fluid , "mL/hour", " + blood loss")
  })
  output$third_hour_fluid <- renderText({
    ({
      if (input$fluid_trauma == "Mild") {
        fluid_trauma <- input$weight * 2
      } else if (input$fluid_trauma == "Severe") {
        fluid_trauma <- input$weight * 10
      } else if (input$fluid_trauma == "None") {
        fluid_trauma <- input$weight * 0
      }
    })
    if (input$weight < 11) {
      third_hour_fluid <- input$weight * 4 + input$weight * 2*(input$fasting_hours)*0.5 + fluid_trauma
    } else if (input$weight > 19) {
      third_hour_fluid  <- input$weight * (input$fasting_hours)*0.5*0.5 + 30 + 60 + fluid_trauma
    } else {
      third_hour_fluid  <- (input$weight * 2)*(input$fasting_hours)*0.5*0.5 + 20 + 40 + (input$weight * 2) + fluid_trauma
    }
    paste("Total fluid requirement in 3nd hour: ", third_hour_fluid , "mL/hour", " + blood loss")
  })
  output$resus_atropine <- renderText({
    resus_atropine  <- input$weight * 0.02
    paste("Dose of Atropine for symptomatic bradycardia or pre-treatment:", resus_atropine, "mg")
  })
  output$resus_sux <- renderText({
    resus_sux_low  <- input$weight
    resus_sux_high <- input$weight * 2
    paste("Dose of Succinylcholine for full paralysis :", resus_sux_low, "mg to ", resus_sux_high, "mg")
  })
  output$resus_phenyl <- renderText({
    resus_phenyl_low  <- input$weight * 0.5
    resus_phenyl_high <- input$weight
    paste("Dose of Phenylephrine for hypotension (bolus):", resus_phenyl_low, "mcg to ", resus_phenyl_high,"mcg")
  })
  output$resus_adrenaline <- renderText({
    resus_adrenaline  <- input$weight * 0.01
    paste("Dose of Adrenaline for cardiac arrest:", resus_adrenaline, "mg")
  })
  output$resus_ephedrine <- renderText({
    resus_ephedrine_low  <- input$weight * 0.1
    resus_ephedrine_high <- input$weight * 0.2
    paste("Dose of Ephedrine for hypotesion/bradycardia:", resus_ephedrine_low, "mg to", resus_ephedrine_high, "mg")
  })
  output$resus_adenosine <- renderText({
    resus_adenosine_first  <- input$weight * 100
    resus_adenosine_second <- input$weight * 200
    paste("First dose of Adenosine is", resus_adenosine_first, "mcg in rapid IV push and flush. Max 6 mg.", "Second dose is", resus_adenosine_second, "mcg. Max dose is 12 mg.")
  })
  output$resus_amiodarone <- renderText({
    resus_amiodarone <- input$weight * 5
    paste("Dose of Amiodarone:", resus_amiodarone, "mg IV. Max of 300mg for Vfib / Vtach.")
  })
  output$resus_magnesium <- renderText({
    resus_magnesium_low <- input$weight * 25
    resus_magnesium_high <- input$weight * 50
    paste("Dose of Magnesium:", resus_magnesium_low, "to" , resus_magnesium_high, "mg IV for Torsades de Pointes(max 2g).")
  })
  output$resus_bicarbonate <- renderText({
    resus_bicarbonate_low <- input$weight
    resus_bicarbonate_high <- input$weight * 2
    paste("Dose of Bicarbonate:", resus_bicarbonate_low, "to" , resus_bicarbonate_high, "mEq IV guided by blood gas analysis.")
  })
  output$resus_ca_chloride <- renderText({
    resus_ca_chloride_low <- input$weight * 10
    resus_ca_chloride_high <- input$weight * 20
    paste("Dose of Calcium Chloride:", resus_ca_chloride_low, "to" , resus_ca_chloride_high, "mg (0.1 - 0.2 mL/kg of a 10% solution.")
  })
  output$induction_thiopentone <- renderText({
    induction_thiopentone  <- input$weight * 7 #6 - 7
    paste("Bolus of Thiopentone :", induction_thiopentone, "mg")
  })
  output$induction_propofol <- renderText({
    induction_propofol  <- input$weight * 3 # 2-3
    maintenance_propofol <- input$weight * (100/1000)*60
    paste("Bolus of Propofol :", induction_propofol, "mg", "followed by infusion dose of",maintenance_propofol, "mg", "per hour")
  })
  output$induction_ketamine_IV <- renderText({
    induction_ketamine_IV  <- input$weight * 2
    maintenance_ketamine_IV <- input$weight
    paste("Bolus of Ketamine in IV:", induction_ketamine_IV, "mg", "followed by infusion dose of",maintenance_ketamine_IV, "mg", " per hour") # 2 # 4
  })
  output$induction_ketamine_IM <- renderText({
    induction_ketamine_IM <-  input$weight * 4
    maintenance_ketamine_IM <- input$weight * 2
    paste("Bolus of Ketamine in IM:", induction_ketamine_IM, "mg", "followed by infusion dose of",maintenance_ketamine_IM, "mg", " per hour") # 4
  })
  output$relaxant_sux <- renderText({
    relaxant_sux_low  <- input$weight
    relaxant_sux_high <- input$weight * 2
    paste("Dose of Succinylcholine for full paralysis :", relaxant_sux_low, "mg to ", relaxant_sux_high, "mg")
  })
  output$relaxant_pancuronium <- renderText({
    relaxant_pancuronium <-  input$weight * 0.1
    paste("Dose of Pancuronium for full paralysis : ", relaxant_pancuronium, "mg IV in 3 min, 60-90 min until reversible (80% renal)")
  })
  output$relaxant_atracurium <- renderText({
    relaxant_atracurium_low <-  input$weight * 0.1
    relaxant_atracurium_high <- input$weight * 0.2
    paste("Dose of Atracurium for paralysis :", relaxant_atracurium_low, "to " , relaxant_atracurium_high, "mg IV in 1-2 minutes, 20-40 minutes until reversible.")
  })
  output$relaxant_rox <- renderText({
    relaxant_rox_low <-  input$weight * 0.6
    relaxant_rox_high <- input$weight * 1.2
    paste("Dose of Rocuronium for paralysis:", relaxant_rox_low, "to ", relaxant_rox_high, "mg IV for paralysis in 1-2 min. 20-40 min until reversible (80% hepatic).")
  })
  output$neostigmine <- renderText({
    neostigmine_low <-  input$weight * 30
    neostigmine_high <- input$weight * 70
    paste("Dose of Neostigmine for reversal:", neostigmine_low, "to ", neostigmine_high, "mcg IV ; max 5mg. Add atropine or glycopyrrolate.")
  })
  output$atropine_reversal <- renderText({
    atropine_reversal_low <-  input$weight * 10
    atropine_reversal_high <- input$weight * 20
    paste("Dose of Atropine for reversal:", atropine_reversal_low, "to ", atropine_reversal_high, "mcg IV")
  })
  output$glycopyrrolate <- renderText({
    glycopyrrolate_low <-  input$weight * 0.01
    glycopyrrolate_high <- input$weight * 0.15
    paste("Dose of Glycopyrrolate for reversal:", glycopyrrolate_low, "to ", glycopyrrolate_high, "mg IV")
  })
  output$sugammadex <- renderText({
    sugammadex_1twitch <-  input$weight * 2
    sugammadex_0twitch <- input$weight * 4
    sugammadex_immediate <- input$weight * 16
    paste("Dose of Sugammadex for reversal of rocuronium/vecuronium:", sugammadex_1twitch, "mg IV if 2 twitches on TOF.", sugammadex_0twitch, " mg IV if 1-2 twitches on PTC but none on TOF. Immediate reversal :", sugammadex_immediate, "mg IV")
  })
  output$ana_epinephrine <- renderText({
    ana_epinephrine_IM <-  input$weight * 10
    ana_epinephrine_IV <-  input$weight
    paste("Dose of Epinephrine in IM is" , ana_epinephrine_IM, "mg, max 300 ug."," Dose of Epinephrine in IV is ", ana_epinephrine_IV, ",repeat with increasing doses every 3-5 mins PRN.") # 4
  })
  output$ana_fluid_bolus <- renderText({
    ana_fluid_bolus <- input$weight * 20
    paste("Fluid bolus of ", ana_fluid_bolus, "mg isotonic fluids, give as necessary.")
  })
  output$ana_albuterol <- renderText({
    paste("10 puffs of Albuterol if patient has bronchospasm")
  })
  output$ana_hydrocortisone <- renderText({
    ana_hydrocortisone_low <- input$weight * 2
    ana_hydrocortisone_high <- input$weight * 3
    paste("Dose of Hydrocortisone is ", ana_hydrocortisone_low, "mg to", ana_hydrocortisone_high, "mg IV.")
  })
  output$ana_diphenhydramine <- renderText({
    ana_diphenhydramine_low <- input$weight
    ana_diphenhydramine_high <- input$weight * 2
    paste("Dose of Diphenhydramine is ", ana_diphenhydramine_low, "mg to", ana_diphenhydramine_high, "mg IV. Max 50 mg.")
  })
  output$ana_ranitidine <- renderText({
    ana_ranitidine <- input$weight * 1.5
    paste("Dose of Ranitidine is ", ana_ranitidine, "mg IV. Max 50 mg.")
  })
  output$ana_lidocaine <- renderText({
    ana_lidocaine_plain <- input$weight * 5
    ana_lidocaine_epi <- input$weight * 7
    paste("Dose of lidocaine(plain) is", ana_lidocaine_plain, "mg. Dose of lidocaine with epi 1:200,000 is", ana_lidocaine_epi)
  })
  output$ana_bupivacaine <- renderText({
    ana_bupivacaine_plain <- input$weight * 2.5
    ana_bupivacaine_epi <- input$weight * 3
    paste("Dose of bupivacaine(plain) is", ana_bupivacaine_plain, "mg. Dose of bupivacaine with epi 1:200,000 is", ana_bupivacaine_epi)
  })
  output$ana_ropivacaine <- renderText({
    ana_ropivacaine_plain <- input$weight * 2.5
    ana_ropivacaine_epi <- input$weight * 3
    paste("Dose of ropivacaine(plain) is", ana_ropivacaine_plain, "mg. Dose of ropivacaine with epi 1:200,000 is", ana_ropivacaine_epi)
  })
  output$la_toxicity <- renderText({
    la_toxicity_first <- input$weight * 1.5
    la_toxicity_infusion <- input$weight * 0.25
    la_toxicity_max <- input$weight * 0.5
    paste("If LA systemic toxicity, give", la_toxicity_first, "mL of 20% Intralipid followed by continuous infusion", la_toxicity_infusion, "mL/minute up to", la_toxicity_max, "per minute until hemodynamics restored.")
  })
  output$disclaimer <- renderText({
    paste("The contents of this app has not been reviewed for accuracy and validity. The author's credentials have not been verified. It is strongly advisable to use peer-reviewed materials and to apply sound clinical judgement in decision-making in patient care.")
  })
})

shinyApp(ui = ui, server = server)

