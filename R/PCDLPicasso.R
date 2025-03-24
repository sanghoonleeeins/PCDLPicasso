#' Title Func_CombineDataset
#'
#' @param MyDatasetName DatasetName
#' @param FirstDataset PatientDemophgraphy
#' @param SecondDataset SampleInformation
#'
#' @returns Joinned PatientDemography and SampleInformation
#' @export
#'
#' @examples Func_CombineDataset(MyDatasetName="N18_dbMP", FirstDataset="PatientDemography", SecondDataset="SampleInformation")
Func_CombineDataset= function(MyDatasetName="N10_WMMultiomeKatahdin", FirstDataset="PatientDemography", SecondDataset="SampleInformation") {
  # EachDataset <- PCDDictionary$DatasetName[18]; print(EachDataset) # "18_dbMP"

  ### PatientDemographyTable
  #PatientDemographyTableFile <- paste0("./data/", MyDatasetName, "_PatientDemographyTable.rds"); print(PatientDemographyTableFile) # "18_dbMP_PatientDemographyTable.rds"
  # PatientDemographyTableFile <- "/Users/lees130/Library/CloudStorage/OneDrive-NYULangoneHealth/N11_PlasmaCellDisorderDB/11b_PCDL_Rpackage/PCDLPicasso/inst/extdata/N10_WMMultiomeKatahdin_PatientDemographyTable.rds"
  PatientDemographyTableFile <- system.file('extdata', paste0(MyDatasetName, "_PatientDemographyTable.rds"), package='PCDLPicasso')
  PatientDemographyTable <- readRDS(PatientDemographyTableFile)

  ### SampleInformationTable
  # SampleInformationTableFile<- paste0("./data/", MyDatasetName, "_SampleInformationTable.rds"); print(SampleInformationTableFile) # "18_dbMP_SampleInformationTable.rds"
  # SampleInformationTableFile <- "/Users/lees130/Library/CloudStorage/OneDrive-NYULangoneHealth/N11_PlasmaCellDisorderDB/11b_PCDL_Rpackage/PCDLPicasso/inst/extdata/N10_WMMultiomeKatahdin_SampleInformationTable.rds"
  SampleInformationTableFile <- system.file('extdata', paste0(MyDatasetName, "_SampleInformationTable.rds"), package='PCDLPicasso')
  SampleInformationTable <- readRDS(SampleInformationTableFile)

  ### full_join by "PatientID"
  PatientDemography_SampleInformation <- dplyr::full_join(PatientDemographyTable, SampleInformationTable)
  dim(PatientDemography_SampleInformation); PatientDemography_SampleInformation[1:2,] # 89 9

  ### Convert NA to "NotAvail"
  table(is.na(PatientDemography_SampleInformation)) # TRUE: 84
  PatientDemography_SampleInformation[is.na(PatientDemography_SampleInformation)] <- "NotAvail"
  table(is.na(PatientDemography_SampleInformation)) # FALSE 5124 TRUE: 0

  return(PatientDemography_SampleInformation)
}


#' Title Func_ClinicalDataTable
#'
#' @param MyDatasetName DatasetName
#' @param ClinicalDataType Race or Gender
#' @param SubType Subtypes in clinical data type
#'
#' @returns CombinedData_BySubType
#' @export
#'
#' @examples Func_ClinicalDataTable(MyDatasetName="N18_dbMP",  ClinicalDataType="Race",  SubType="Asian")
Func_ClinicalDataTable = function(MyDatasetName="N10_WMMultiomeKatahdin",  ClinicalDataType="Race",  SubType="White") {
    CombinedData_18_dbMP <- Func_CombineDataset(MyDatasetName, FirstDataset="PatientDemography", SecondDataset="SampleInformation")
    dim(CombinedData_18_dbMP) # 427 12
    ############# =============== ############## =====================
    ### Check the proportion of Demography or Diagnosis: Race, Gender, Diagnosis.
    ############# =============== ############## =====================
    if (ClinicalDataType=="Sequence") {
        CombinedData_18_dbMP_NoDup <- CombinedData_18_dbMP
    } else if (ClinicalDataType!="Sequence") {
        CombinedData_18_dbMP_NoDup <- CombinedData_18_dbMP[!duplicated(CombinedData_18_dbMP$PatientID),]
        dim(CombinedData_18_dbMP_NoDup) # 244  12
    }

    if (ClinicalDataType %in% c("InstituteName", "Race","Gender","Diagnosis", "SampleType","Hyperdiploid","TumorTreatment",  "WM", "Sequence")) {

        ## Filter the table by SubType
        if(SubType=="AllSubType") {
            CombinedData_BySubType <- CombinedData_18_dbMP_NoDup
        } else {
            ## Change the column name by ClinicalDataType
            colnames(CombinedData_18_dbMP_NoDup)[colnames(CombinedData_18_dbMP_NoDup)==ClinicalDataType] <- "InterestClinicalData"
            CombinedData_BySubType <- dplyr::filter(CombinedData_18_dbMP_NoDup, InterestClinicalData==SubType); dim(CombinedData_BySubType)

            ## Going back to the original column name
            colnames(CombinedData_BySubType)[colnames(CombinedData_BySubType)=="InterestClinicalData"] <- ClinicalDataType
        }

        return(CombinedData_BySubType)
    } else if (ClinicalDataType %in% c("Age")) {   # ClinicalDataType="Age"; SubType="LowerThan50";
        ## Change the column name by ClinicalDataType
        colnames(CombinedData_18_dbMP_NoDup)[colnames(CombinedData_18_dbMP_NoDup)==ClinicalDataType] <- "InterestClinicalData"
        ## Filter the table by SubType
        if(SubType=="AllSubType") {
          CombinedData_BySubType <- CombinedData_18_dbMP_NoDup
        } else if (SubType=="LowerThan50" ) {
          CombinedData_BySubType <- dplyr::filter(CombinedData_18_dbMP_NoDup, InterestClinicalData < 50); dim(CombinedData_BySubType) # 20 12
        } else if (SubType=="HigherEqual50" ) {
          CombinedData_BySubType <- dplyr::filter(CombinedData_18_dbMP_NoDup, InterestClinicalData >= 50); dim(CombinedData_BySubType) # 224 12
        } else if (SubType=="NotAvail" ) {
          CombinedData_BySubType <- dplyr::filter(CombinedData_18_dbMP_NoDup, InterestClinicalData==SubType); dim(CombinedData_BySubType) # 224 12
        }

        ## Going back to the original column name
        colnames(CombinedData_BySubType)[colnames(CombinedData_BySubType)=="InterestClinicalData"] <- ClinicalDataType
        return(CombinedData_BySubType)
    }
}



#' Title Func_SummaryPiechart
#'
#' @param MyDatasetName DatasetName
#' @param InterestDataType Datatype of my interest
#'
#' @returns Piechart
#' @export  Piechart
#'
#' @examples Func_SummaryPiechart(MyDatasetName="N18_dbMP",  InterestDataType="Race")
Func_SummaryPiechart = function(MyDatasetName="N18_dbMP",  InterestDataType="Race") {
  CombinedData_18_dbMP <- Func_CombineDataset(MyDatasetName, FirstDataset="PatientDemography", SecondDataset="SampleInformation")
  dim(CombinedData_18_dbMP) # 427 12

  ############# =============== ############## =====================
  ### Check the proportion of Demography or Diagnosis: Race, Gender, Age, Diagnosis.
  ############# =============== ############## =====================
  CombinedData_18_dbMP_NoDup <- CombinedData_18_dbMP[!duplicated(CombinedData_18_dbMP$PatientID),]
  dim(CombinedData_18_dbMP_NoDup) # 244  12

  if (InterestDataType %in% c("InstituteName", "Race","Gender","Diagnosis","Hyperdiploid","TumorTreatment", "WM", "Sequence")) {   ## Piechart
    ### Pie chart for MyInterestType
    colnames(CombinedData_18_dbMP_NoDup)[colnames(CombinedData_18_dbMP_NoDup)==InterestDataType] <- "MyInterestType"
    GroupProportion <- table(CombinedData_18_dbMP_NoDup$MyInterestType)
    GroupLabel <- paste0(names(GroupProportion), ": ",GroupProportion)

    ### Return colname
    colnames(CombinedData_18_dbMP_NoDup)[colnames(CombinedData_18_dbMP_NoDup)=="MyInterestType"] <- InterestDataType
    ### Piechart
    MyPie3D <- plotrix::pie3D(GroupProportion, explode=0.05, main=paste0("Piechart for ", InterestDataType), labelcex = 1.8) # explode, to give space between pie
    MyPieChart <- plotrix::pie3D.labels(MyPie3D, radius=1.3, labels = GroupLabel, labelcex = 1.8) # radius: distance of txt from pie    # https://stackoverflow.com/questions/19452617/increasing-space-between-plot-labels
    return(MyPieChart)
  } else if(InterestDataType %in% c("Age")) {   ### Histogram
    ## Remove patients that don't have Age data.
    CombinedData_18_dbMP_WithAge <- dplyr::filter(CombinedData_18_dbMP_NoDup, Age!="NotAvail"); dim(CombinedData_18_dbMP_WithAge) # 236 12
    CombinedData_18_dbMP_NoAge <-  dplyr::filter(CombinedData_18_dbMP_NoDup, Age=="NotAvail"); dim(CombinedData_18_dbMP_NoAge) # 8 12
    NumbNoAgeSmp <- nrow(CombinedData_18_dbMP_NoAge); print(NumbNoAgeSmp) # 8

    CombinedData_18_dbMP_WithAge$AgeRange <- ggplot2::cut_width(CombinedData_18_dbMP_WithAge$Age,width=10,boundary=0)
    table(CombinedData_18_dbMP_WithAge$AgeRange)
    # [30,40]  (40,50]  (50,60]  (60,70]  (70,80]  (80,90] (90,100]
    #   5       19       50       74       74       12        2
    # plot(CombinedData_18_dbMP_WithAge$AgeRange)


    MyHistPlot <-  ggplot2::ggplot(data.frame(CombinedData_18_dbMP_WithAge), ggplot2::aes(x=as.factor(AgeRange), fill=as.factor(AgeRange) )) +
      ggplot2::geom_bar() + ggplot2::scale_fill_brewer(palette = "Set1") + ggplot2::theme(legend.position="none") +
      ggplot2::stat_count(geom = "text", colour = "black", size=8, ggplot2::aes(label = ..count..),position=ggplot2::position_stack(vjust=0.5)) +
      ggplot2::theme_bw()

    # Create a text
    grob <- grid::grobTree(grid::textGrob(paste0("The number of samples Age not available: ",NumbNoAgeSmp, "\n",
                                                 names(table(CombinedData_18_dbMP_WithAge$AgeRange))[1], ", ",
                                                 names(table(CombinedData_18_dbMP_WithAge$AgeRange))[2], ", ",
                                                 names(table(CombinedData_18_dbMP_WithAge$AgeRange))[3], ", ",
                                                 names(table(CombinedData_18_dbMP_WithAge$AgeRange))[4], ", ",
                                                 names(table(CombinedData_18_dbMP_WithAge$AgeRange))[5], ", ",
                                                 names(table(CombinedData_18_dbMP_WithAge$AgeRange))[6], ", ",
                                                 names(table(CombinedData_18_dbMP_WithAge$AgeRange))[7], ", ",
                                                 names(table(CombinedData_18_dbMP_WithAge$AgeRange))[8]
    ), x=0.1,  y=0.90, hjust=0,
    gp=grid::gpar(col="Black", fontsize=20, fontface="italic")))
    # Plot
    MyHistPlotAnnot <- MyHistPlot + ggplot2::annotation_custom(grob)
    return(MyHistPlotAnnot)
  } else if (InterestDataType %in% c("SampleType")) {   ## Piechart
    ############# =============== ############## =====================
    ### Check the proportion of InterestDataType="SampleType"
    ############# =============== ############## =====================
    ### Pie chart for MyInterestType
    colnames(CombinedData_18_dbMP)[colnames(CombinedData_18_dbMP)==InterestDataType] <- "MyInterestType"
    GroupProportion <- table(CombinedData_18_dbMP$MyInterestType)
    GroupLabel <- paste0(names(GroupProportion), ": ",GroupProportion)

    ### Return colname
    colnames(CombinedData_18_dbMP)[colnames(CombinedData_18_dbMP)=="MyInterestType"] <- InterestDataType
    ### Piechart
    MyPie3D <- plotrix::pie3D(GroupProportion, explode=0.05, main=paste0("Piechart for ", InterestDataType)) # explode, to give space between pie
    MyPieChart<-plotrix::pie3D.labels(MyPie3D, radius = 1.3, labels = GroupLabel, labelcex = 1.5) # radius: distance of txt from pie    # https://stackoverflow.com/questions/19452617/increasing-space-between-plot-labels
    return(MyPieChart)
  }
} # end of Func_SummaryPiechart



###############################################################################################
## Step3. ShinyApp Server
###############################################################################################
# input$Dataset="N18_dbMP" # N01_UKMyeloma_WEX

#' Title PCDD_Server
#'
#' @param input DatasetName
#' @param output Piechart
#'
#' @returns PCDL plots
#' @export
#'
#' @examples PCDD_Server(input,output)
PCDD_Server <- function(input, output) {
  output$FakePlot <- shiny::renderPlot({
    input$button
    Sys.sleep(1)
    # plot(runif(10))
  }, width=100, height=0)

  shiny::observe({
    ############# Clinical Subtype table ########################### $$$$$$$$$$$$$$$ ========================== $$$$$$$$$$$$$$$$$ ========================== $$$$$$$$$$$$$$$$$ ========================== $$$$$$$$$$$$$$$$$
    # ClinicalDataTable by InstituteName: "AllSubType", "NYU","UAB","UMiami","NotAvail"
    ClinicalTable_ByClinicalSubType_InstituteName <- shiny::eventReactive(input$ClinicalTable_InstituteName, {
        if(req(input$ClinicalTable_InstituteName) %in% c("AllSubType","NYU","UAB","UMiami","Gustave_Roussy","NotAvail"  ) ) {
            ClinicalData_SubtypeInstituteName <- Func_ClinicalDataTable(MyDatasetName=input$Dataset,  ClinicalDataType="InstituteName",  SubType=input$ClinicalTable_InstituteName)
        }
        return(ClinicalData_SubtypeInstituteName)
    })

    # ClinicalDataTable by Race: "AllSubType","Asian","Black_or_African_American","Hispanic","White","NotAvail"
    ClinicalTable_ByClinicalSubType_Race <- shiny::eventReactive(input$ClinicalTable_Race, {
      if(req(input$ClinicalTable_Race) %in% c("AllSubType","Asian","Black_or_African_American","Hispanic","White","NotAvail")) {
          ClinicalData_SubtypeRace <- Func_ClinicalDataTable(MyDatasetName=input$Dataset,  ClinicalDataType="Race",  SubType=input$ClinicalTable_Race)
      }
      # else if(req(input$ClinicalTable_Race)=="Asian") {
      #          ClinicalData_SubtypeRace <- Func_ClinicalDataTable(MyDatasetName=input$Dataset,  ClinicalDataType="Race",  SubType=input$ClinicalTable_Race)
      #     } else if(req(input$ClinicalTable_Race)=="Black_or_African_American") {
      #          ClinicalData_SubtypeRace <- Func_ClinicalDataTable(MyDatasetName=input$Dataset,  ClinicalDataType="Race",  SubType=input$ClinicalTable_Race)
      #     }  else if(req(input$ClinicalTable_Race)=="Hispanic") {
      #          ClinicalData_SubtypeRace <- Func_ClinicalDataTable(MyDatasetName=input$Dataset,  ClinicalDataType="Race",  SubType=input$ClinicalTable_Race)
      #     }  else if(req(input$ClinicalTable_Race)=="White") {
      #          ClinicalData_SubtypeRace <- Func_ClinicalDataTable(MyDatasetName=input$Dataset,  ClinicalDataType="Race",  SubType=input$ClinicalTable_Race)
      #     } else if(req(input$ClinicalTable_Race)=="NotAvail") {
      #          ClinicalData_SubtypeRace <- Func_ClinicalDataTable(MyDatasetName=input$Dataset,  ClinicalDataType="Race",  SubType=input$ClinicalTable_Race)
      #     }
      return(ClinicalData_SubtypeRace)
    })

    # ClinicalDataTable matrix by Gender
    ClinicalTable_ByClinicalSubType_Gender <- shiny::eventReactive(input$ClinicalTable_Gender, {
      if(req(input$ClinicalTable_Gender) %in% c("AllSubType","Male","Female","NotAvail")) {
        ClinicalData_SubtypeGender <- Func_ClinicalDataTable(MyDatasetName=input$Dataset,  ClinicalDataType="Gender",  SubType=input$ClinicalTable_Gender)
      }
      return(ClinicalData_SubtypeGender)
    })
    # ClinicalDataTable matrix by Age
    ClinicalTable_ByClinicalSubType_Age <- shiny::eventReactive(input$ClinicalTable_Age, {
      if(req(input$ClinicalTable_Age) %in% c("AllSubType","LowerThan50","HigherEqual50","NotAvail")) {
        ClinicalData_SubtypeAge <- Func_ClinicalDataTable(MyDatasetName=input$Dataset,  ClinicalDataType="Age",  SubType=input$ClinicalTable_Age)
      }
      return(ClinicalData_SubtypeAge)
    })
    # ClinicalDataTable matrix by Diagnosis: "AllSubType","MGUS","SMM","MM","RR","NotAvail"
    ClinicalTable_ByClinicalSubType_Diagnosis <- shiny::eventReactive(input$ClinicalTable_Diagnosis, {
      if(req(input$ClinicalTable_Diagnosis) %in% c("AllSubType","MGUS","MM","RR","NotAvail"  )) {
        ClinicalData_SubtypeDiagnosis <- Func_ClinicalDataTable(MyDatasetName=input$Dataset,  ClinicalDataType="Diagnosis",  SubType=input$ClinicalTable_Diagnosis)
      }
      return(ClinicalData_SubtypeDiagnosis)
    })
    # ClinicalDataTable matrix by SampleType: "AllSubType","BM","PB","ST","NotAvail"), selected="AllSubType"
    ClinicalTable_ByClinicalSubType_SampleType <- shiny::eventReactive(input$ClinicalTable_SampleType, {
      if(req(input$ClinicalTable_SampleType) %in% c("AllSubType","BM","PB","ST","NotAvail" )) {
        ClinicalData_SubtypeSampleType <- Func_ClinicalDataTable(MyDatasetName=input$Dataset,  ClinicalDataType="SampleType",  SubType=input$ClinicalTable_SampleType)
      }
      return(ClinicalData_SubtypeSampleType)
    })
    # ClinicalDataTable matrix by Hyperdiploid: "AllSubType","No","Yes","NotAvail"), selected="AllSubType"
    ClinicalTable_ByClinicalSubType_Hyperdiploid <- shiny::eventReactive(input$ClinicalTable_Hyperdiploid, {
      if(req(input$ClinicalTable_Hyperdiploid) %in% c("AllSubType","No","Yes","NotAvail" )) {
        ClinicalData_SubtypeHyperdiploid <- Func_ClinicalDataTable(MyDatasetName=input$Dataset,  ClinicalDataType="Hyperdiploid",  SubType=input$ClinicalTable_Hyperdiploid)
      }
      return(ClinicalData_SubtypeHyperdiploid)
    })
    # ClinicalDataTable matrix by TumorTreatment: "AllSubType","Treated","Untreated","NotAvail"), selected="AllSubType"
    ClinicalTable_ByClinicalSubType_TumorTreatment <- shiny::eventReactive(input$ClinicalTable_TumorTreatment, {
      if(req(input$ClinicalTable_TumorTreatment) %in% c("AllSubType","Treated","Untreated","NotAvail" )) {
        ClinicalData_SubtypeTumorTreatment <- Func_ClinicalDataTable(MyDatasetName=input$Dataset,  ClinicalDataType="TumorTreatment",  SubType=input$ClinicalTable_TumorTreatment)
      }
      return(ClinicalData_SubtypeTumorTreatment)
    })

    # ClinicalDataTable matrix by WM: "AllSubType","MBC_LIKE","PC_LIKE","Ambiguous","NotAvail"), selected="AllSubType"
    ClinicalTable_ByClinicalSubType_WM <- shiny::eventReactive(input$ClinicalTable_WM, {
      if(req(input$ClinicalTable_WM) %in% c("AllSubType","MBC_LIKE","PC_LIKE","Ambiguous","NotAvail")) {
        ClinicalData_SubtypeWM <- Func_ClinicalDataTable(MyDatasetName=input$Dataset,  ClinicalDataType="WM",  SubType=input$ClinicalTable_WM)
      }
      return(ClinicalData_SubtypeWM)
    })

    # ClinicalDataTable matrix by Sequence: "AllSubType","scRNAscATAC","WGS","BulkRNA","NotAvail"), selected="AllSubType"
    ClinicalTable_ByClinicalSubType_Sequence <- shiny::eventReactive(input$ClinicalTable_Sequence, {
      if(req(input$ClinicalTable_Sequence) %in% c("AllSubType","scRNAscATAC","WGS","BulkRNA","NotAvail" )) {
        ClinicalData_SubtypeSequence <- Func_ClinicalDataTable(MyDatasetName=input$Dataset,  ClinicalDataType="Sequence",  SubType=input$ClinicalTable_Sequence)
      }
      return(ClinicalData_SubtypeSequence)
    })

    #### renderDataTable
    if(req(input$Dataset) %in% c("N06_RRPC49Elsa","N10_WMMultiomeKatahdin", "N18_dbMP")) {
      output$outputtableInstituteName <- DT::renderDataTable( ClinicalTable_ByClinicalSubType_InstituteName()    )
      output$outputtableRace <- DT::renderDataTable( ClinicalTable_ByClinicalSubType_Race()    )
      output$outputtableRace <- DT::renderDataTable( ClinicalTable_ByClinicalSubType_Race()    )
      output$outputtableGender <- DT::renderDataTable( ClinicalTable_ByClinicalSubType_Gender()    )
      output$outputtableAge <- DT::renderDataTable( ClinicalTable_ByClinicalSubType_Age()    )
      output$outputtableDiagnosis <- DT::renderDataTable( ClinicalTable_ByClinicalSubType_Diagnosis()    )
      output$outputtableSampleType <- DT::renderDataTable(ClinicalTable_ByClinicalSubType_SampleType()    )
      output$outputtableHypderdiploid <- DT::renderDataTable(ClinicalTable_ByClinicalSubType_Hyperdiploid()    )
      output$outputtableTumorTreatment <- DT::renderDataTable(ClinicalTable_ByClinicalSubType_TumorTreatment()    )
      output$outputtableWM <- DT::renderDataTable(ClinicalTable_ByClinicalSubType_WM()    )
      output$outputtableSequence <- DT::renderDataTable(ClinicalTable_ByClinicalSubType_Sequence()    )
    }

    if(req(input$ClinicalTable_InstituteName) %in% c("AllSubType", "NYU", "UAB","UMiami","Gustave_Roussy","NotAvail")) {
      output$outputtableInstituteName <- DT::renderDataTable( ClinicalTable_ByClinicalSubType_InstituteName()    )
    }
    if(req(input$ClinicalTable_Race) %in% c("AllSubType", "Asian", "Black_or_African_American","Hispanic","White", "NotAvail")) {
      output$outputtableRace <- DT::renderDataTable( ClinicalTable_ByClinicalSubType_Race()    )
    }
    if(req(input$ClinicalTable_Gender) %in% c("AllSubType", "Male", "Female", "NotAvail")) {
      # output$outputtableInstituteName<-NULL; output$outputtableRace<-NULL; output$outputtableAge<-NULL;
      # output$outputtableDiagnosis<-NULL; output$outputtableSampleType<-NULL; output$outputtableHypderdiploid<-NULL; output$outputtableTumorTreatment<-NULL;
      output$outputtableGender <- DT::renderDataTable( ClinicalTable_ByClinicalSubType_Gender()    )
    }
    if(req(input$ClinicalTable_Age) %in% c("AllSubType", "LowerThan50", "HigherEqual50", "NotAvail")) {
      output$outputtableAge <- DT::renderDataTable( ClinicalTable_ByClinicalSubType_Age()    )
    }
    if(req(input$ClinicalTable_Diagnosis) %in% c("AllSubType","MGUS","SMM","MM","RR","NotAvail")) {
      output$outputtableDiagnosis <- DT::renderDataTable( ClinicalTable_ByClinicalSubType_Diagnosis()    )
    }
    if(req(input$ClinicalTable_SampleType) %in% c("AllSubType","BM","PB","ST","NotAvail")) {
      output$outputtableSampleType <- DT::renderDataTable(ClinicalTable_ByClinicalSubType_SampleType()    )
    }
    if(req(input$ClinicalTable_Hyperdiploid) %in% c("AllSubType","No","Yes","NotAvail")) {
      output$outputtableHypderdiploid <- DT::renderDataTable(ClinicalTable_ByClinicalSubType_Hyperdiploid()    )
    }
    if(req(input$ClinicalTable_TumorTreatment) %in% c("AllSubType","Treated","Untreated","NotAvail")) {
      output$outputtableTumorTreatment <- DT::renderDataTable(ClinicalTable_ByClinicalSubType_TumorTreatment()    )
    }
    if(req(input$ClinicalTable_WM) %in% c("AllSubType","MBC_LIKE","PC_LIKE","Ambiguous","NotAvail")) {
      output$outputtableWM <- DT::renderDataTable(ClinicalTable_ByClinicalSubType_WM()    )
    }
    if(req(input$ClinicalTable_Sequence) %in% c("AllSubType","scRNAscATAC","WGS","BulkRNA","NotAvail")) {
      output$outputtableSequence <- DT::renderDataTable(ClinicalTable_ByClinicalSubType_Sequence()    )
    }


    ############## $$$$$$$$$$$$$$$ ############## $$$$$$$$$$$$$$$ ############## $$$$$$$$$$$$$$$ ############## $$$$$$$$$$$$$$$ ############## $$$$$$$$$$$$$$$ ############## $$$$$$$$$$$$$$$
    ############# Piechart by clinical subtype ########################### $$$$$$$$$$$$$$$ ========================== $$$$$$$$$$$$$$$$$ ========================== $$$$$$$$$$$$$$$$$ ========================== $$$$$$$$$$$$$$$$$
    if(length(input$ClinicalSubtype)==1)  {
      if(req(input$ClinicalSubtype)=="ByInstituteName" ) {
        output$FakePlot<-NULL;output$InstituteName_Piechart<-NULL; output$Race_Piechart<-NULL; output$Gender_Piechart<-NULL;
        output$Age_Piechart<-NULL; output$Diagnosis_Piechart<-NULL; output$SampleType_Piechart<-NULL;output$Hyerdiploid_Piechart<-NULL;output$TumorTreatment_Piechart<-NULL;output$Image<-NULL;
        output$WM_Piechart<-NULL; output$Sequence_Piechart<-NULL;
        output$MainPlot <- renderPlot({
          Sys.sleep(1)
          Func_SummaryPiechart(MyDatasetName=input$Dataset,  InterestDataType="InstituteName") }, width=800, height=500)
      }
      if(req(input$ClinicalSubtype)=="ByRace" ) {
        output$FakePlot<-NULL; output$InstituteName_Piechart<-NULL; output$Gender_Piechart<-NULL; output$Age_Piechart<-NULL;
        output$Diagnosis_Piechart<-NULL; output$SampleType_Piechart<-NULL;output$Hyerdiploid_Piechart<-NULL;output$TumorTreatment_Piechart<-NULL;output$Image<-NULL;
        output$WM_Piechart<-NULL; output$Sequence_Piechart<-NULL;
        output$MainPlot <- renderPlot({
          Sys.sleep(1)
          Func_SummaryPiechart(MyDatasetName=input$Dataset,  InterestDataType="Race") }, width=800, height=500)
      }
      if (req(input$ClinicalSubtype)=="ByGender" ) {
        output$FakePlot<-NULL; output$InstituteName_Piechart<-NULL; output$Race_Piechart<-NULL; output$Age_Piechart<-NULL;
        output$Diagnosis_Piechart<-NULL; output$SampleType_Piechart<-NULL;output$Hyerdiploid_Piechart<-NULL;output$TumorTreatment_Piechart<-NULL;output$Image<-NULL;
        output$WM_Piechart<-NULL; output$Sequence_Piechart<-NULL;
        output$MainPlot <- renderPlot({
          Sys.sleep(1)
          Func_SummaryPiechart(MyDatasetName=input$Dataset,  InterestDataType="Gender") }, width=800, height=500)
      }
      if (req(input$ClinicalSubtype)=="ByAge" ) {
        output$FakePlot<-NULL;output$InstituteName_Piechart<-NULL; output$Race_Piechart<-NULL; output$Gender_Piechart<-NULL;
        output$Diagnosis_Piechart<-NULL; output$SampleType_Piechart<-NULL;output$Hyerdiploid_Piechart<-NULL;output$TumorTreatment_Piechart<-NULL;output$Image<-NULL;
        output$WM_Piechart<-NULL; output$Sequence_Piechart<-NULL;
        output$MainPlot <- renderPlot({
          Sys.sleep(1)
          Func_SummaryPiechart(MyDatasetName=input$Dataset,  InterestDataType="Age") }, width=800, height=600)
      }
      if (req(input$ClinicalSubtype)=="ByDiagnosis" ) {
        output$FakePlot<-NULL; output$InstituteName_Piechart<-NULL;output$Race_Piechart<-NULL;  output$Gender_Piechart<-NULL;
        output$Age_Piechart<-NULL; output$SampleType_Piechart<-NULL;output$Hyerdiploid_Piechart<-NULL;output$TumorTreatment_Piechart<-NULL;output$Image<-NULL;
        output$MainPlot <- renderPlot({
          Sys.sleep(1)
          Func_SummaryPiechart(MyDatasetName=input$Dataset,  InterestDataType="Diagnosis") }, width=800, height=500)
      }
      if (req(input$ClinicalSubtype)=="BySampletype" ) {
        output$FakePlot<-NULL; output$InstituteName_Piechart<-NULL;output$Race_Piechart<-NULL;  output$Gender_Piechart<-NULL;
        output$Age_Piechart<-NULL; output$Diagnosis_Piechart<-NULL;output$Hyerdiploid_Piechart<-NULL;output$TumorTreatment_Piechart<-NULL;output$Image<-NULL;
        output$WM_Piechart<-NULL; output$Sequence_Piechart<-NULL;
        output$MainPlot <- renderPlot({
          Sys.sleep(1)
          Func_SummaryPiechart(MyDatasetName=input$Dataset,  InterestDataType="Sampletype") }, width=800, height=500)
      }
      if (req(input$ClinicalSubtype)=="ByHyperdiploid" ) {
        output$FakePlot<-NULL; output$InstituteName_Piechart<-NULL;output$Race_Piechart<-NULL;  output$Gender_Piechart<-NULL;
        output$Age_Piechart<-NULL; output$Diagnosis_Piechart<-NULL;output$TumorTreatment_Piechart<-NULL;output$Image<-NULL; output$WM_Piechart<-NULL; output$Sequence_Piechart<-NULL;
        output$MainPlot <- renderPlot({
          Sys.sleep(1)
          Func_SummaryPiechart(MyDatasetName=input$Dataset,  InterestDataType="Hyperdiploid") }, width=800, height=500)
      }
      if (req(input$ClinicalSubtype)=="ByTuorTreatment" ) {
        output$FakePlot<-NULL; output$InstituteName_Piechart<-NULL;output$Race_Piechart<-NULL;  output$Gender_Piechart<-NULL;
        output$Age_Piechart<-NULL; output$Diagnosis_Piechart<-NULL;output$Hyerdiploid_Piechart<-NULL;output$Image<-NULL; output$WM_Piechart<-NULL; output$Sequence_Piechart<-NULL;
        output$MainPlot <- renderPlot({
          Sys.sleep(1)
          Func_SummaryPiechart(MyDatasetName=input$Dataset,  InterestDataType="TuorTreatment") }, width=800, height=500)
      }
      if (req(input$ClinicalSubtype)=="ByWM" ) {
        output$MainPlot <- renderPlot({
          Sys.sleep(1)
          Func_SummaryPiechart(MyDatasetName=input$Dataset,  InterestDataType="WM") }, width=800, height=500)
        output$FakePlot<-NULL; output$InstituteName_Piechart<-NULL;output$Race_Piechart<-NULL;  output$Gender_Piechart<-NULL; output$Age_Piechart<-NULL; output$Diagnosis_Piechart<-NULL;
        output$Sequence_Piechart<-NULL;
      }
      if (req(input$ClinicalSubtype)=="BySequence" ) {
        output$MainPlot <- renderPlot({
          Sys.sleep(1)
          Func_SummaryPiechart(MyDatasetName=input$Dataset,  InterestDataType="Sequence") }, width=800, height=500)
        output$FakePlot<-NULL; output$InstituteName_Piechart<-NULL;output$Race_Piechart<-NULL;  output$Gender_Piechart<-NULL; output$Age_Piechart<-NULL; output$Diagnosis_Piechart<-NULL;
        output$WM_Piechart<-NULL;
      }


      ## All options
      if (req(input$ClinicalSubtype)=="All Visions" ) {
        output$FakePlot<-NULL; output$InstituteName_Piechart<-NULL;output$Race_Piechart<-NULL;  output$Gender_Piechart<-NULL;
        output$Age_Piechart<-NULL; output$Diagnosis_Piechart<-NULL;output$Hyerdiploid_Piechart<-NULL;output$TumorTreatment_Piechart<-NULL;output$Image<-NULL;
        output$SampleType_Piechart<-NULL;

        output$InstituteName_Piechart <- renderPlot({
          Sys.sleep(1)
          Func_SummaryPiechart(MyDatasetName=input$Dataset,  InterestDataType="InstituteName") }, width=800, height=500)
        output$Race_Piechart <- renderPlot({
          Sys.sleep(1)
          Func_SummaryPiechart(MyDatasetName=input$Dataset,  InterestDataType="Race") }, width=800, height=500)
        output$Gender_Piechart <- renderPlot({
          Sys.sleep(1)
          Func_SummaryPiechart(MyDatasetName=input$Dataset,  InterestDataType="Gender") }, width=800, height=500)
        output$Age_Piechart <- renderPlot({
          Sys.sleep(1)
          Func_SummaryPiechart(MyDatasetName=input$Dataset,  InterestDataType="Age")}, width=800, height=600)
        output$Diagnosis_Piechart <- renderPlot({
          Sys.sleep(1)
          Func_SummaryPiechart(MyDatasetName=input$Dataset,  InterestDataType="Diagnosis") }, width=800, height=500)
        output$SampleType_Piechart <- renderPlot({
          Sys.sleep(1)
          Func_SummaryPiechart(MyDatasetName=input$Dataset,  InterestDataType="SampleType") }, width=800, height=500)
        output$Hyperdiploid_Piechart <- renderPlot({
          Sys.sleep(1)
          Func_SummaryPiechart(MyDatasetName=input$Dataset,  InterestDataType="Hyperdiploid") }, width=800, height=500)
        output$TumorTreatment_Piechart <- renderPlot({
          Sys.sleep(1)
          Func_SummaryPiechart(MyDatasetName=input$Dataset,  InterestDataType="TumorTreatment") }, width=800, height=500)
        output$WM_Piechart <- renderPlot({
          Sys.sleep(1)
          Func_SummaryPiechart(MyDatasetName=input$Dataset,  InterestDataType="WM") }, width=800, height=500)
        output$Sequence_Piechart <- renderPlot({
          Sys.sleep(1)
          Func_SummaryPiechart(MyDatasetName=input$Dataset,  InterestDataType="Sequence") }, width=800, height=500)

        output$MainPlot<-NULL; output$FakePlot<-NULL;
      }
    }
    ##########  When a user chose 2 ClinicalSubtype #################
    if(length(input$ClinicalSubtype)==2)  {
      if(input$ClinicalSubtype[1]=="ByInstituteName" & input$ClinicalSubtype[2]=="ByRace")  { # Error in if: the condition has length > 1   # I should put [1] at then end of 'input$ClinicalSubtype'
        output$FakePlot<-NULL; output$MainPlot<-NULL; output$Age_Piechart<-NULL; output$Diagnosis_Piechart<-NULL; output$SampleType_Piechart<-NULL;output$Image<-NULL;
        output$Caption <- renderText({
          Sys.sleep(1)
          #input$Caption
          # paste0("Scroll down to see the plots")
        })
        output$MainPlot <- renderPlot({  ## I will use output$MainPlot   This indicates just position, not represent mainplot.
          Sys.sleep(1)
          Func_SummaryPiechart(MyDatasetName=input$Dataset,  InterestDataType="InstituteName")}, width=800, height=500)
        output$InstituteName_Piechart <- renderPlot({
          Sys.sleep(1)
          Func_SummaryPiechart(MyDatasetName=input$Dataset,  InterestDataType="Race")}, width=800, height=500)
      }
      if(input$ClinicalSubtype[1]=="ByRace" & input$ClinicalSubtype[2]=="ByGender")  { # Error in if: the condition has length > 1   # I should put [1] at then end of 'input$ClinicalSubtype'
        output$FakePlot<-NULL; output$MainPlot<-NULL;output$Age_Piechart<-NULL; output$Diagnosis_Piechart<-NULL; output$SampleType_Piechart<-NULL;output$Image<-NULL;
        output$MainPlot <- renderPlot({
          Sys.sleep(1)
          Func_SummaryPiechart(MyDatasetName=input$Dataset,  InterestDataType="Race")}, width=800, height=500)
        output$InstituteName_Piechart <- renderPlot({
          Sys.sleep(1)
          Func_SummaryPiechart(MyDatasetName=input$Dataset,  InterestDataType="Gender")}, width=800, height=500)
      }
      if(input$ClinicalSubtype[1]=="ByRace" & input$ClinicalSubtype[2]=="ByAge")  { # Error in if: the condition has length > 1   # I should put [1] at then end of 'input$ClinicalSubtype'
        output$FakePlot<-NULL; output$MainPlot<-NULL;output$Gender_Piechart<-NULL; output$Diagnosis_Piechart<-NULL; output$SampleType_Piechart<-NULL;output$Image<-NULL;
        output$MainPlot <- renderPlot({
          Sys.sleep(1)
          Func_SummaryPiechart(MyDatasetName=input$Dataset,  InterestDataType="Race")}, width=800, height=500)
        output$InstituteName_Piechart <- renderPlot({
          Sys.sleep(1)
          Func_SummaryPiechart(MyDatasetName=input$Dataset,  InterestDataType="Age")}, width=800, height=500)
      }
      if(input$ClinicalSubtype[1]=="ByRace" & input$ClinicalSubtype[2]=="ByDiagnosis")  { # Error in if: the condition has length > 1   # I should put [1] at then end of 'input$ClinicalSubtype'
        output$Age_Piechart<-NULL;output$MainPlot<-NULL;  output$Diagnosis_Piechart<-NULL; output$SampleType_Piechart<-NULL;output$Image<-NULL;
        output$MainPlot <- renderPlot({
          Sys.sleep(1)
          Func_SummaryPiechart(MyDatasetName=input$Dataset,  InterestDataType="Race")}, width=800, height=500)
        output$InstituteName_Piechart <- renderPlot({
          Sys.sleep(1)
          Func_SummaryPiechart(MyDatasetName=input$Dataset,  InterestDataType="Diagnosis")}, width=800, height=500)      }
      if(input$ClinicalSubtype[1]=="ByRace" & input$ClinicalSubtype[2]=="BySampleType")  { # Error in if: the condition has length > 1   # I should put [1] at then end of 'input$ClinicalSubtype'
        output$FakePlot<-NULL; output$MainPlot<-NULL; output$Gender_Piechart<-NULL; output$Age_Piechart<-NULL; output$Diagnosis_Piechart<-NULL;
        output$Image = renderUI({
          tags$img(src="https://github.com/sanghoonleepitt/CutRunSeq_Aanalysis/blob/main/FaceSoothingMask.jpeg?raw=true",height="400px", width="400px")        })
        output$MainPlot <- renderPlot({
          Sys.sleep(1)
          Func_SummaryPiechart(MyDatasetName=input$Dataset,  InterestDataType="Race")}, width=800, height=500)
        output$InstituteName_Piechart <- renderPlot({
          Sys.sleep(1)
          Func_SummaryPiechart(MyDatasetName=input$Dataset,  InterestDataType="SampleType")}, width=800, height=500)
      }
      if(input$ClinicalSubtype[1]=="ByGender" & input$ClinicalSubtype[2]=="ByAge")  { # Error in if: the condition has length > 1   # I should put [1] at then end of 'input$ClinicalSubtype'
        output$FakePlot<-NULL;output$MainPlot<-NULL;  output$Race_Piechart<-NULL; output$Gender_Piechart<-NULL;output$Age_Piechart<-NULL; output$Diagnosis_Piechart<-NULL;
        output$SampleType_Piechart<-NULL;output$Image<-NULL;
        output$Caption <- renderText({ Sys.sleep(1)
          #input$Caption
          paste0("I am lazy to enable to select every possible combination of options.Can you select just \"All Options\" ?")
        })

      }
    } # End of length(input$ClinicalSubtype)==2
  }) # End of shiny::observe
}

