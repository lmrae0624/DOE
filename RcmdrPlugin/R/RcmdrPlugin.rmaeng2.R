aanova <- function () {
  defaults <- list(initial.group = NULL, initial.response = NULL, initial.formul = "", initial.ranfac = "")
  dialog.values <- getDialog("aanova", defaults)
  initializeDialog(title = gettextRcmdr("분산분석"))
  dataFrame <- tkframe(top)
  groupBox <- variableListBox(dataFrame, selectmode = "multiple",
                              title = gettextRcmdr("요인 (하나 이상 선택)"),
                              initialSelection = varPosn(dialog.values$initial.group, "all"))
  responseBox <- variableListBox(dataFrame, title = gettextRcmdr("Response Variable (pick one)"),
                                 initialSelection = varPosn(dialog.values$initial.response, "all"))

  onOK <- function() {
    groups <- getSelection(groupBox)
    response <- getSelection(responseBox)
    formull <- as.character(tclvalue(formul))
    ranfacc <- as.character(tclvalue(ranfac))

    if (length(groups) == 0) {
      errorCondition(recall = aanova, message = gettextRcmdr("You must select at least one factor."))
      return()}
    if (length(response) == 0) {
      errorCondition(recall = aanova, message = gettextRcmdr("You must select at least one factor."))
      return()}
    putDialog ("aanova", list (initial.group = groups, initial.response = response, initial.formul = formull, initial.ranfac = ranfacc))
    closeDialog()

    .activeDataSet <- ActiveDataSet()
    groups.list <- paste(paste(groups, sep = ""), collapse = ", ")
    doItAndPrint(paste(paste(rep(groups),rep('<-'),rep("as.factor("),rep(.activeDataSet),rep('$'),groups,")", sep = ""), collapse = "
"))
    doItAndPrint(paste(paste(rep(.activeDataSet),rep('$'),rep(groups),rep('<-'),rep("as.factor("),rep(.activeDataSet),rep('$'),groups,")", sep = ""), collapse = "
"))
    if (ranfacc == ""){
      doItAndPrint(paste("aov.t(", .activeDataSet,"$",response,"~", formull,")", sep = ""))
    }
    if (ranfacc != ""){
      doItAndPrint(paste("aov.t(", .activeDataSet,"$",response,"~", formull,",ranfac=",'"',.activeDataSet,"$",ranfacc,'"',")", sep = ""))

    }
    tkfocus(CommanderWindow())
  }

  OKCancelHelp(helpSubject = "Anova", model = TRUE, reset = "aanova", apply = "aanova")

  formulFrame <- tkframe(dataFrame)
  formul <- tclVar(dialog.values$initial.formul)
  formulField <- ttkentry(formulFrame, width = "20", textvariable = formul)

  ranfacFrame <- tkframe(dataFrame)
  ranfac <- tclVar(dialog.values$initial.ranfac)
  ranfacField <- ttkentry(ranfacFrame, width = "20", textvariable = ranfac)

  tkgrid(labelRcmdr(formulFrame, text = gettextRcmdr("Formula / ex) a+b+c+a*b+b*c+c*a+a*b*c"), fg = getRcmdr("title.color"), font = "RcmdrTitleFont"), sticky = "w")
  tkgrid(labelRcmdr(ranfacFrame, text = gettextRcmdr("변량인자"), fg = getRcmdr("title.color"), font = "RcmdrTitleFont"), sticky = "w")
  tkgrid(formulField, sticky="w")
  tkgrid(ranfacField, sticky="w")
  tkgrid(formulFrame, labelRcmdr(dataFrame, text = " "), sticky = "nw")
  tkgrid(ranfacFrame, labelRcmdr(dataFrame, text = " "), sticky = "nw")
  tkgrid(getFrame(groupBox), labelRcmdr(dataFrame, text="  "), getFrame(responseBox), sticky = "nw")
  tkgrid(dataFrame, sticky="w")
  tkgrid(buttonsFrame, sticky = "w")
  dialogSuffix(use.tabs = FALSE, grid.buttons = TRUE)
}

onerandomtable <- function () {
  defaults <- list(initial.factorname = 'c("a","b","c")', initial.repetition = "", initial.seed="0", initial.std="FALSE")
  dialog.values <- getDialog("onerandomtable", defaults)
  initializeDialog(title = gettextRcmdr("일원배치법 난수표"), use.tabs = FALSE)
  mainFrame <- tkframe(top)
  UpdateModelNumber()
  modelName <- tclVar(paste("AnovaModel.", getRcmdr("modelNumber"),
                            sep = ""))
  modelFrame <- tkframe(top)
  model <- ttkentry(modelFrame, width = "20", textvariable = modelName)

  onOK <- function() {
    modelValue <- trim.blanks(tclvalue(modelName))

    fac<-as.character(tclvalue(factorname))
    repet<-tclvalue(repetition)
    sed<-tclvalue(seed)
    sttd<-as.character(tclvalue(std))

    putDialog ("onerandomtable", list (initial.factorname = fac, initial.repetition = repet, initial.seed = sed, initial.std = sttd))
    closeDialog()

    doItAndPrint(paste("random.one(", "fac=", fac,",", "r=", repet,",", "seed=", sed,",", "std=", sttd, ")", sep = ""))
    justDoIt(paste(modelValue, "<-random.one(","fac=", fac,",", "r=", repet,",", "seed=", sed,",", "std=", sttd, ")", sep = ""))
    activeModel(modelValue)

    tkfocus(CommanderWindow())
  }

  OKCancelHelp(helpSubject = "Anova", model = TRUE, reset = "multiWayAnova", apply = "multiWayAnova")

  factorFrame <- tkframe(mainFrame)
  factorname <- tclVar(dialog.values$initial.factorname)
  factorField <- ttkentry(factorFrame, width = "12", textvariable = factorname)

  repetFrame <- tkframe(mainFrame)
  repetition <- tclVar(dialog.values$initial.repetition)
  repetField <- ttkentry(repetFrame, width = "12", textvariable = repetition)

  seedFrame <- tkframe(mainFrame)
  seed <- tclVar(dialog.values$initial.seed)
  seedField <- ttkentry(seedFrame, width = "12", textvariable = seed)

  stdFrame <- tkframe(mainFrame)
  std <- tclVar(dialog.values$initial.std)
  stdField <- ttkentry(stdFrame, width = "12", textvariable = std)

  tkgrid(labelRcmdr(mainFrame, text="  "), sticky = "nw")
  tkgrid(mainFrame, sticky="nw")
  tkgrid(labelRcmdr(modelFrame, text = gettextRcmdr("Enter name for model: ")),
         model, sticky = "w")
  tkgrid(modelFrame, sticky = "w")
  tkgrid(labelRcmdr(factorFrame, text = gettextRcmdr("요인"), fg = getRcmdr("title.color"), font = "RcmdrTitleFont"), sticky = "w")
  tkgrid(labelRcmdr(repetFrame, text = gettextRcmdr("반복수"), fg = getRcmdr("title.color"), font = "RcmdrTitleFont"), sticky = "w")
  tkgrid(labelRcmdr(seedFrame, text = gettextRcmdr("Seed"), fg = getRcmdr("title.color"), font = "RcmdrTitleFont"), sticky = "w")
  tkgrid(labelRcmdr(stdFrame, text = gettextRcmdr("표준순서 / TRUE : 표준순서 or FALSE : 실행 순서"), fg = getRcmdr("title.color"), font = "RcmdrTitleFont"), sticky = "w")
  tkgrid(factorField, sticky="w")
  tkgrid(repetField, sticky="w")
  tkgrid(seedField, sticky="w")
  tkgrid(stdField, sticky="w")
  tkgrid(factorFrame, labelRcmdr(mainFrame, text = " "), sticky = "nw")
  tkgrid(repetFrame, labelRcmdr(mainFrame, text = " "), sticky = "nw")
  tkgrid(seedFrame, labelRcmdr(mainFrame, text = " "), sticky = "nw")
  tkgrid(stdFrame, labelRcmdr(mainFrame, text = " "), sticky = "nw")
  tkgrid(buttonsFrame, sticky = "w")
  dialogSuffix(use.tabs = FALSE, grid.buttons = TRUE)
}


latinrandomtable <- function () {
  defaults <- list(initial.factornumber = "3", initial.greco = "FALSE", initial.seed="0", initial.std="FALSE")
  dialog.values <- getDialog("latinrandomtable", defaults)
  initializeDialog(title = gettextRcmdr("라틴 및 그레코라틴 난수표"), use.tabs = FALSE)
  mainFrame <- tkframe(top)
  UpdateModelNumber()
  modelName <- tclVar(paste("AnovaModel.", getRcmdr("modelNumber"),
                            sep = ""))
  modelFrame <- tkframe(top)
  model <- ttkentry(modelFrame, width = "20", textvariable = modelName)

  onOK <- function() {
    modelValue <- trim.blanks(tclvalue(modelName))

    n<-tclvalue(factornumber)
    greco<-as.character(tclvalue(greco))
    sed<-tclvalue(seed)
    sttd<-as.character(tclvalue(std))

    putDialog ("grecorandomtable", list (initial.factornumber = n, initial.greco = greco, initial.seed = sed, initial.std = sttd))
    closeDialog()

    doItAndPrint(paste("random.latin(", "n=", n,",", "greco=", greco,",", "seed=", sed,",", "std=", sttd, ")", sep = ""))
    justDoIt(paste(modelValue, "<-random.latin(", "n=", n,",", "greco=", greco,",", "seed=", sed,",", "std=", sttd, ")", sep = ""))
    activeModel(modelValue)

    tkfocus(CommanderWindow())
  }

  OKCancelHelp(helpSubject = "Anova", model = TRUE, reset = "multiWayAnova", apply = "multiWayAnova")

  factorFrame <- tkframe(mainFrame)
  factornumber <- tclVar(dialog.values$initial.factornumber)
  factorField <- ttkentry(factorFrame, width = "12", textvariable = factornumber)

  grecoFrame <- tkframe(mainFrame)
  greco <- tclVar(dialog.values$initial.greco)
  grecoField <- ttkentry(grecoFrame, width = "12", textvariable = greco)

  seedFrame <- tkframe(mainFrame)
  seed <- tclVar(dialog.values$initial.seed)
  seedField <- ttkentry(seedFrame, width = "12", textvariable = seed)

  stdFrame <- tkframe(mainFrame)
  std <- tclVar(dialog.values$initial.std)
  stdField <- ttkentry(stdFrame, width = "12", textvariable = std)

  tkgrid(labelRcmdr(mainFrame, text="  "), sticky = "nw")
  tkgrid(mainFrame, sticky="nw")
  tkgrid(labelRcmdr(modelFrame, text = gettextRcmdr("Enter name for model: ")),
         model, sticky = "w")
  tkgrid(modelFrame, sticky = "w")
  tkgrid(labelRcmdr(factorFrame, text = gettextRcmdr("요인의 수 / N*N 라틴방격"), fg = getRcmdr("title.color"), font = "RcmdrTitleFont"), sticky = "w")
  tkgrid(labelRcmdr(grecoFrame, text = gettextRcmdr("라틴, 그레코 / TRUE = 그레코, FALSE = 라틴"), fg = getRcmdr("title.color"), font = "RcmdrTitleFont"), sticky = "w")
  tkgrid(labelRcmdr(seedFrame, text = gettextRcmdr("Seed"), fg = getRcmdr("title.color"), font = "RcmdrTitleFont"), sticky = "w")
  tkgrid(labelRcmdr(stdFrame, text = gettextRcmdr("표준순서 / TRUE : 표준순서 or FALSE : 실행 순서"), fg = getRcmdr("title.color"), font = "RcmdrTitleFont"), sticky = "w")
  tkgrid(factorField, sticky="w")
  tkgrid(grecoField, sticky="w")
  tkgrid(seedField, sticky="w")
  tkgrid(stdField, sticky="w")
  tkgrid(factorFrame, labelRcmdr(mainFrame, text = " "), sticky = "nw")
  tkgrid(grecoFrame, labelRcmdr(mainFrame, text = " "), sticky = "nw")
  tkgrid(seedFrame, labelRcmdr(mainFrame, text = " "), sticky = "nw")
  tkgrid(stdFrame, labelRcmdr(mainFrame, text = " "), sticky = "nw")
  tkgrid(buttonsFrame, sticky = "w")
  dialogSuffix(use.tabs = FALSE, grid.buttons = TRUE)
}


multirandomtable <- function () {
  defaults <- list(initial.factornumber = "c(2,3)", initial.repetition = "", initial.seed="0", initial.std="FALSE")
  dialog.values <- getDialog("multirandomtable", defaults)
  initializeDialog(title = gettextRcmdr("다원배치법 난수표"), use.tabs = FALSE)
  mainFrame <- tkframe(top)
  UpdateModelNumber()
  modelName <- tclVar(paste("AnovaModel.", getRcmdr("modelNumber"),
                            sep = ""))
  modelFrame <- tkframe(top)
  model <- ttkentry(modelFrame, width = "20", textvariable = modelName)

  onOK <- function() {
    modelValue <- trim.blanks(tclvalue(modelName))

    fac<-as.character(tclvalue(factornumber))
    repet<-tclvalue(repetition)
    sed<-tclvalue(seed)
    sttd<-as.character(tclvalue(std))

    putDialog ("onerandomtable", list (initial.factornumber = fac, initial.repetition = repet, initial.seed = sed, initial.std = sttd))
    closeDialog()

    doItAndPrint(paste("random.multi(", "fac=", fac,",", "r=", repet,",", "seed=", sed,",", "std=", sttd, ")", sep = ""))
    justDoIt(paste(modelValue, "<-random.multi(", "fac=", fac,",", "r=", repet,",", "seed=", sed,",", "std=", sttd, ")", sep = ""))
    activeModel(modelValue)
    tkfocus(CommanderWindow())
  }

  OKCancelHelp(helpSubject = "Anova", model = TRUE, reset = "multiWayAnova", apply = "multiWayAnova")

  factorFrame <- tkframe(mainFrame)
  factornumber <- tclVar(dialog.values$initial.factornumber)
  factorField <- ttkentry(factorFrame, width = "12", textvariable = factornumber)

  repetFrame <- tkframe(mainFrame)
  repetition <- tclVar(dialog.values$initial.repetition)
  repetField <- ttkentry(repetFrame, width = "12", textvariable = repetition)

  seedFrame <- tkframe(mainFrame)
  seed <- tclVar(dialog.values$initial.seed)
  seedField <- ttkentry(seedFrame, width = "12", textvariable = seed)

  stdFrame <- tkframe(mainFrame)
  std <- tclVar(dialog.values$initial.std)
  stdField <- ttkentry(stdFrame, width = "12", textvariable = std)

  tkgrid(labelRcmdr(mainFrame, text="  "), sticky = "nw")
  tkgrid(mainFrame, sticky="nw")
  tkgrid(labelRcmdr(modelFrame, text = gettextRcmdr("Enter name for model: ")),
         model, sticky = "w")
  tkgrid(modelFrame, sticky = "w")
  tkgrid(labelRcmdr(factorFrame, text = gettextRcmdr("요인 / c(A인자 수준수, B인자 수준수)"), fg = getRcmdr("title.color"), font = "RcmdrTitleFont"), sticky = "w")
  tkgrid(labelRcmdr(repetFrame, text = gettextRcmdr("반복수"), fg = getRcmdr("title.color"), font = "RcmdrTitleFont"), sticky = "w")
  tkgrid(labelRcmdr(seedFrame, text = gettextRcmdr("Seed"), fg = getRcmdr("title.color"), font = "RcmdrTitleFont"), sticky = "w")
  tkgrid(labelRcmdr(stdFrame, text = gettextRcmdr("표준순서 / TRUE : 표준순서 or FALSE : 실행 순서"), fg = getRcmdr("title.color"), font = "RcmdrTitleFont"), sticky = "w")
  tkgrid(factorField, sticky="w")
  tkgrid(repetField, sticky="w")
  tkgrid(seedField, sticky="w")
  tkgrid(stdField, sticky="w")
  tkgrid(factorFrame, labelRcmdr(mainFrame, text = " "), sticky = "nw")
  tkgrid(repetFrame, labelRcmdr(mainFrame, text = " "), sticky = "nw")
  tkgrid(seedFrame, labelRcmdr(mainFrame, text = " "), sticky = "nw")
  tkgrid(stdFrame, labelRcmdr(mainFrame, text = " "), sticky = "nw")
  tkgrid(buttonsFrame, sticky = "w")
  dialogSuffix(use.tabs = FALSE, grid.buttons = TRUE)
}


mainplot <- function () {
  defaults <- list(initial.group = NULL,initial.response = NULL)
  dialog.values <- getDialog("mainplot", defaults)
  initializeDialog(title = gettextRcmdr("주효과도"), use.tabs = FALSE)
  dataFrame <- tkframe(top)
  groupBox <- variableListBox(dataFrame, selectmode = "single",
                              title = gettextRcmdr("요인 (하나 선택)"),
                              initialSelection = varPosn(dialog.values$initial.group, "all"))
  responseBox <- variableListBox(dataFrame, title = gettextRcmdr("Response Variable (pick one)"),
                                 initialSelection = varPosn(dialog.values$initial.response, "all"))

  onOK <- function() {
    groups <- getSelection(groupBox)
    response <- getSelection(responseBox)
    if (length(groups) == 0) {
      errorCondition(recall = mainplot, message = gettextRcmdr("You must select at least one factor."))
      return()}
    putDialog ("mainplot", list (initial.group = groups, initial.response = response))
    closeDialog()

    .activeDataSet <- ActiveDataSet()
    doItAndPrint(paste("main.effect(", .activeDataSet,',',.activeDataSet,"$",groups,',',.activeDataSet,"$",response, ")"))
    tkfocus(CommanderWindow())
  }

  OKCancelHelp(helpSubject = "Anova", model = TRUE, reset = "mainplot", apply = "mainplot")

  tkgrid(getFrame(groupBox), labelRcmdr(dataFrame, text="  "), sticky = "nw")
  tkgrid(getFrame(responseBox), labelRcmdr(dataFrame, text=" "), sticky = "nw")
  tkgrid(dataFrame, sticky="nw")
  tkgrid(buttonsFrame, sticky = "w")
  dialogSuffix(use.tabs = FALSE, grid.buttons = TRUE)
}

interactionplot <- function () {
  defaults <- list(initial.group = NULL,initial.response = NULL)
  dialog.values <- getDialog("interactionplot", defaults)
  initializeDialog(title = gettextRcmdr("교호작용도"), use.tabs = FALSE)
  dataFrame <- tkframe(top)
  groupBox <- variableListBox(dataFrame, selectmode = "multiple",
                              title = gettextRcmdr("요인 (두개 선택)"),
                              initialSelection = varPosn(dialog.values$initial.group, "all"))
  responseBox <- variableListBox(dataFrame, title = gettextRcmdr("Response Variable (pick one)"),
                                 initialSelection = varPosn(dialog.values$initial.response, "all"))

  onOK <- function() {
    groups <- getSelection(groupBox)
    response <- getSelection(responseBox)
    if (length(groups) == 0) {
      errorCondition(recall = interactionplot, message = gettextRcmdr("You must select two factor."))
      return()}
    if (length(groups) == 1) {
      errorCondition(recall = interactionplot, message = gettextRcmdr("You must select two factor."))
      return()}
    if (length(response) == 0) {
      errorCondition(recall = interactionlot, message = gettextRcmdr("You must select at least one factor."))
      return()}
    putDialog ("interactionplot", list (initial.group = groups, initial.response = response))
    closeDialog()

    .activeDataSet <- ActiveDataSet()
    groups.list <- paste(paste(rep(ActiveDataSet()),rep('$'),groups, sep = ""), collapse = ",")
    doItAndPrint(paste("interaction.effect(", .activeDataSet,',',groups.list,',',.activeDataSet,"$",response, ")"))
    tkfocus(CommanderWindow())
  }

  OKCancelHelp(helpSubject = "Anova", model = TRUE, reset = "interactionplot", apply = "interactionplot")

  tkgrid(getFrame(groupBox), labelRcmdr(dataFrame, text="  "), sticky = "nw")
  tkgrid(getFrame(responseBox), labelRcmdr(dataFrame, text=" "), sticky = "nw")
  tkgrid(dataFrame, sticky="nw")
  tkgrid(buttonsFrame, sticky = "w")
  dialogSuffix(use.tabs = FALSE, grid.buttons = TRUE)
}

sscatterplot <- function () {
  defaults <- list(initial.group = NULL,initial.response = NULL)
  dialog.values <- getDialog("sscatterplot", defaults)
  initializeDialog(title = gettextRcmdr("산점도"), use.tabs = FALSE)
  dataFrame <- tkframe(top)
  groupBox <- variableListBox(dataFrame, selectmode = "multiple",
                              title = gettextRcmdr("요인 (두개 이하 선택)"),
                              initialSelection = varPosn(dialog.values$initial.group, "all"))
  responseBox <- variableListBox(dataFrame, title = gettextRcmdr("Response Variable (pick one)"),
                                 initialSelection = varPosn(dialog.values$initial.response, "all"))

  onOK <- function() {
    groups <- getSelection(groupBox)
    response <- getSelection(responseBox)
    if (length(groups) == 0) {
      errorCondition(recall = sscatterplot, message = gettextRcmdr("You must select at least one factor."))
      return()}
    if (length(response) == 0) {
      errorCondition(recall = sscatterplot, message = gettextRcmdr("You must select at least one factor."))
      return()}
    putDialog ("sscatterplot", list (initial.group = groups, initial.response = response))
    closeDialog()

    .activeDataSet <- ActiveDataSet()
    groups.list <- paste(paste(rep(ActiveDataSet()),rep('$'),groups, sep = ""), collapse = ",")
    doItAndPrint(paste("scatter.plot(", .activeDataSet,",",groups.list,",y=",.activeDataSet,"$",response, ")"))
    tkfocus(CommanderWindow())
  }

  OKCancelHelp(helpSubject = "Anova", model = TRUE, reset = "sscatterplot", apply = "sscatterplot")

  tkgrid(getFrame(groupBox), labelRcmdr(dataFrame, text="  "), sticky = "nw")
  tkgrid(getFrame(responseBox), labelRcmdr(dataFrame, text=" "), sticky = "nw")
  tkgrid(dataFrame, sticky="nw")
  tkgrid(buttonsFrame, sticky = "w")
  dialogSuffix(use.tabs = FALSE, grid.buttons = TRUE)
}

oneerror <- function () {
  defaults <- list(initial.group = NULL,initial.response = NULL,initial.alpha="0.05")
  dialog.values <- getDialog("oneerror", defaults)
  initializeDialog(title = gettextRcmdr("일원배치법 오차분산의 추정"), use.tabs = FALSE)
  dataFrame <- tkframe(top)
  groupBox <- variableListBox(dataFrame, selectmode = "single",
                              title = gettextRcmdr("요인 (하나 선택)"),
                              initialSelection = varPosn(dialog.values$initial.group, "all"))
  responseBox <- variableListBox(dataFrame, title = gettextRcmdr("반응변수 (하나 선택)"),
                                 initialSelection = varPosn(dialog.values$initial.response, "all"))

  onOK <- function() {

    groups <- getSelection(groupBox)
    response <- getSelection(responseBox)
    al <- tclvalue(alpha)

    if (length(groups) == 0) {
      errorCondition(recall = sscatterplot, message = gettextRcmdr("You must select at least one factor."))
      return()}
    if (length(response) == 0) {
      errorCondition(recall = sscatterplot, message = gettextRcmdr("You must select at least one factor."))
      return()}

    putDialog ("oneerror", list (initial.group = groups, initial.response = response, initial.alpha = al))
    closeDialog()

    .activeDataSet <- ActiveDataSet()
    groups.list <- paste(paste(groups, sep = ""), collapse = ", ")
    doItAndPrint(paste("error.var(", .activeDataSet,"$",response,"~",.activeDataSet,"$",groups,",",.activeDataSet,",",al, ")", sep = ""))
    tkfocus(CommanderWindow())
  }

  OKCancelHelp(helpSubject = "Anova", model = TRUE, reset = "oneerror", apply = "oneerror")

  alphaFrame <- tkframe(dataFrame)
  alpha <- tclVar(dialog.values$initial.alpha)
  alphaField <- ttkentry(alphaFrame, width = "12", textvariable = alpha)

  tkgrid(getFrame(groupBox), labelRcmdr(dataFrame, text="  "), sticky = "nw")
  tkgrid(labelRcmdr(alphaFrame, text = gettextRcmdr("알파"), fg = getRcmdr("title.color"), font = "RcmdrTitleFont"), sticky = "w")
  tkgrid(alphaField, sticky="w")
  tkgrid(alphaFrame, labelRcmdr(dataFrame, text = " "), sticky = "nw")
  tkgrid(getFrame(responseBox), labelRcmdr(dataFrame, text=" "), sticky = "nw")
  tkgrid(dataFrame, sticky="nw")
  tkgrid(buttonsFrame, sticky = "w")
  dialogSuffix(use.tabs = FALSE, grid.buttons = TRUE)
}

nomeandiff <- function () {
  defaults <- list(initial.group = NULL,initial.response = NULL,initial.formul = "",initial.method="hsd", initial.ran = "")
  dialog.values <- getDialog("nomeandiff", defaults)
  initializeDialog(title = gettextRcmdr("모평균차의 추정"), use.tabs = FALSE)
  dataFrame <- tkframe(top)
  groupBox <- variableListBox(dataFrame, selectmode = "multiple",
                              title = gettextRcmdr("요인 (하나 이상 선택)"),
                              initialSelection = varPosn(dialog.values$initial.group, "all"))
  responseBox <- variableListBox(dataFrame, title = gettextRcmdr("반응변수 (하나 선택)"),
                                 initialSelection = varPosn(dialog.values$initial.response, "all"))

  onOK <- function() {
    groups <- getSelection(groupBox)
    response <- getSelection(responseBox)
    formull <- as.character(tclvalue(formul))
    methodd <- as.character(tclvalue(method))
    rann <- as.character(tclvalue(ran))

    if (length(groups) == 0) {
      errorCondition(recall = nomeandiff, message = gettextRcmdr("You must select at least one factor."))
      return()}
    if (length(response) == 0) {
      errorCondition(recall = nomeandiff, message = gettextRcmdr("You must select at least one factor."))
      return()}

    putDialog ("nomeandiff", list (initial.group = groups, initial.response = response, initial.formul = formull, initial.method = methodd, initial.ran = rann))
    closeDialog()

    .activeDataSet <- ActiveDataSet()
    doItAndPrint(paste(paste(rep(.activeDataSet),rep('$'),rep(groups),rep('<-'),rep("as.factor("),rep(.activeDataSet),rep('$'),groups,")", sep = ""), collapse = "
"))
    doItAndPrint(paste(paste(rep(groups),rep('<-'),rep(.activeDataSet),rep('$'),groups, sep = ""), collapse = "
"))
    doItAndPrint(paste(paste(rep(response),rep('<-'),rep(.activeDataSet),rep('$'),response, sep = ""), collapse = "
"))
    if (rann!=""){
      doItAndPrint(paste("mean.diff(data=", .activeDataSet,",formula=",response,"~",formull,",method=",'"',methodd,'"',",ranfac=",'"',rann,'"',")", sep = ""))
    }
    if (rann==""){
      doItAndPrint(paste("mean.diff(data=", .activeDataSet,",formula=",response,"~",formull,",method=",'"',methodd,'"',")", sep = ""))
    }
    tkfocus(CommanderWindow())
  }

  OKCancelHelp(helpSubject = "Anova", model = TRUE, reset = "nomeandiff", apply = "nomeandiff")

  formulFrame <- tkframe(dataFrame)
  formul <- tclVar(dialog.values$initial.formul)
  formulField <- ttkentry(formulFrame, width = "12", textvariable = formul)

  methodFrame <- tkframe(dataFrame)
  method <- tclVar(dialog.values$initial.method)
  methodField <- ttkentry(methodFrame, width = "12", textvariable = method)

  ranFrame <- tkframe(dataFrame)
  ran <- tclVar(dialog.values$initial.ran)
  ranField <- ttkentry(ranFrame, width = "12", textvariable = ran)

  tkgrid(getFrame(groupBox), labelRcmdr(dataFrame, text="  "), getFrame(responseBox), sticky = "nw")
  tkgrid(labelRcmdr(formulFrame, text = gettextRcmdr("Formula / ex) a+b+a*b"), fg = getRcmdr("title.color"), font = "RcmdrTitleFont"), sticky = "w")
  tkgrid(labelRcmdr(methodFrame, text = gettextRcmdr("방법
(입력 : hsd / lsd / bonferroni / scheffe / newmankeuls / duncan)"), fg = getRcmdr("title.color"), font = "RcmdrTitleFont"), sticky = "w")
  tkgrid(labelRcmdr(ranFrame, text = gettextRcmdr("변량 인자"), fg = getRcmdr("title.color"), font = "RcmdrTitleFont"), sticky = "w")
  tkgrid(formulField, sticky="w")
  tkgrid(methodField, sticky="w")
  tkgrid(ranField, sticky="w")
  tkgrid(formulFrame, labelRcmdr(dataFrame, text = " "), sticky = "nw")
  tkgrid(methodFrame, labelRcmdr(dataFrame, text = " "), sticky = "nw")
  tkgrid(ranFrame, labelRcmdr(dataFrame, text = " "), sticky = "nw")
  tkgrid(dataFrame, sticky="nw")
  tkgrid(buttonsFrame, sticky = "w")
  dialogSuffix(use.tabs = FALSE, grid.buttons = TRUE)
}



facrandomtable <- function () {
  defaults <- list(initial.level = "c(3,3,2)", initial.nvars = "3", initial.repet="2", initial.std="FALSE")
  dialog.values <- getDialog("facrandomtable", defaults)
  initializeDialog(title = gettextRcmdr("요인배치법 난수표"), use.tabs = FALSE)
  mainFrame <- tkframe(top)
  UpdateModelNumber()
  modelName <- tclVar(paste("AnovaModel.", getRcmdr("modelNumber"),
                            sep = ""))
  modelFrame <- tkframe(top)
  model <- ttkentry(modelFrame, width = "20", textvariable = modelName)

  onOK <- function() {
    modelValue <- trim.blanks(tclvalue(modelName))


    levell<-as.character(tclvalue(level))
    nvar<-tclvalue(nvars)
    repe<-tclvalue(repet)
    sttd<-as.character(tclvalue(std))

    putDialog ("facrandomtable", list (initial.level = levell, initial.nvars = nvar, initial.repet = repe, initial.std = sttd))
    closeDialog()

    doItAndPrint(paste("fac.design(", "levels=", levell,",", "nVars=", nvar,",", "r=", repe,",","std=", sttd, ")", sep = ""))
    justDoIt(paste(modelValue, "<-fac.design(", "levels=", levell,",", "nVars=", nvar,",", "r=", repe,",","std=", sttd, ")", sep = ""))
    activeModel(modelValue)
    tkfocus(CommanderWindow())
  }

  OKCancelHelp(helpSubject = "Anova", model = TRUE, reset = "facrandomtable", apply = "facrandomtable")

  levelFrame <- tkframe(mainFrame)
  level <- tclVar(dialog.values$initial.level)
  levelField <- ttkentry(levelFrame, width = "12", textvariable = level)

  nvarsFrame <- tkframe(mainFrame)
  nvars <- tclVar(dialog.values$initial.nvars)
  nvarsField <- ttkentry(nvarsFrame, width = "12", textvariable = nvars)

  repetFrame <- tkframe(mainFrame)
  repet <- tclVar(dialog.values$initial.repet)
  repetField <- ttkentry(repetFrame, width = "12", textvariable = repet)

  stdFrame <- tkframe(mainFrame)
  std <- tclVar(dialog.values$initial.std)
  stdField <- ttkentry(stdFrame, width = "12", textvariable = std)

  tkgrid(labelRcmdr(mainFrame, text="  "), sticky = "nw")
  tkgrid(mainFrame, sticky="nw")
  tkgrid(labelRcmdr(modelFrame, text = gettextRcmdr("Enter name for model: ")),
         model, sticky = "w")
  tkgrid(modelFrame, sticky = "w")
  tkgrid(labelRcmdr(levelFrame, text = gettextRcmdr("각 요인의 수준 / c(A인자 수준,B인자 수준,...)"), fg = getRcmdr("title.color"), font = "RcmdrTitleFont"), sticky = "w")
  tkgrid(labelRcmdr(nvarsFrame, text = gettextRcmdr("요인의 수"), fg = getRcmdr("title.color"), font = "RcmdrTitleFont"), sticky = "w")
  tkgrid(labelRcmdr(repetFrame, text = gettextRcmdr("반복수"), fg = getRcmdr("title.color"), font = "RcmdrTitleFont"), sticky = "w")
  tkgrid(labelRcmdr(stdFrame, text = gettextRcmdr("표준순서 / TRUE : 표준순서 or FALSE : 실행 순서"), fg = getRcmdr("title.color"), font = "RcmdrTitleFont"), sticky = "w")
  tkgrid(levelField, sticky="w")
  tkgrid(nvarsField, sticky="w")
  tkgrid(repetField, sticky="w")
  tkgrid(stdField, sticky="w")
  tkgrid(levelFrame, labelRcmdr(mainFrame, text = " "), sticky = "nw")
  tkgrid(nvarsFrame, labelRcmdr(mainFrame, text = " "), sticky = "nw")
  tkgrid(repetFrame, labelRcmdr(mainFrame, text = " "), sticky = "nw")
  tkgrid(stdFrame, labelRcmdr(mainFrame, text = " "), sticky = "nw")
  tkgrid(buttonsFrame, sticky = "w")
  dialogSuffix(use.tabs = FALSE, grid.buttons = TRUE)
}


boxbenken <- function () {
  defaults <- list(initial.kfactor = "3", initial.center = 4, initial.random="TRUE")
  dialog.values <- getDialog("boxbenken", defaults)
  initializeDialog(title = gettextRcmdr("Box-Benken 설계"), use.tabs = FALSE)
  mainFrame <- tkframe(top)
  UpdateModelNumber()
  modelName <- tclVar(paste("AnovaModel.", getRcmdr("modelNumber"),
                            sep = ""))
  modelFrame <- tkframe(top)
  model <- ttkentry(modelFrame, width = "20", textvariable = modelName)

  onOK <- function() {
    modelValue <- trim.blanks(tclvalue(modelName))

    kfactors<-tclvalue(kfactor)
    centers<-tclvalue(center)
    randoms<-as.character(tclvalue(random))

    putDialog ("boxbenken", list (initial.kfactor = kfactors, initial.center = centers, initial.random = randoms))
    closeDialog()

    doItAndPrint(paste("kbbd(", "k=", kfactors,",", "n0=", centers,",","randomize=", randoms, ")", sep = ""))
    justDoIt(paste(modelValue, "<-kbbd(", "k=", kfactors,",", "n0=", centers,",","randomize=", randoms, ")", sep = ""))
    activeModel(modelValue)
    tkfocus(CommanderWindow())
  }

  OKCancelHelp(helpSubject = "Anova", model = TRUE, reset = "boxbenken", apply = "boxbenken")

  kfactorFrame <- tkframe(mainFrame)
  kfactor <- tclVar(dialog.values$initial.kfactor)
  kfactorField <- ttkentry(kfactorFrame, width = "12", textvariable = kfactor)

  centerFrame <- tkframe(mainFrame)
  center <- tclVar(dialog.values$initial.center)
  centerField <- ttkentry(centerFrame, width = "12", textvariable = center)

  randomFrame <- tkframe(mainFrame)
  random <- tclVar(dialog.values$initial.random)
  randomField <- ttkentry(randomFrame, width = "12", textvariable = random)

  tkgrid(labelRcmdr(mainFrame, text="  "), sticky = "nw")
  tkgrid(mainFrame, sticky="nw")
  tkgrid(labelRcmdr(modelFrame, text = gettextRcmdr("Enter name for model: ")),
         model, sticky = "w")
  tkgrid(modelFrame, sticky = "w")
  tkgrid(labelRcmdr(kfactorFrame, text = gettextRcmdr("요인의 수"), fg = getRcmdr("title.color"), font = "RcmdrTitleFont"), sticky = "w")
  tkgrid(labelRcmdr(centerFrame, text = gettextRcmdr("중심점의 수"), fg = getRcmdr("title.color"), font = "RcmdrTitleFont"), sticky = "w")
  tkgrid(labelRcmdr(randomFrame, text = gettextRcmdr("랜덤화 / TRUE or FALSE"), fg = getRcmdr("title.color"), font = "RcmdrTitleFont"), sticky = "w")
  tkgrid(kfactorField, sticky="w")
  tkgrid(centerField, sticky="w")
  tkgrid(randomField, sticky="w")
  tkgrid(kfactorFrame, labelRcmdr(mainFrame, text = " "), sticky = "nw")
  tkgrid(centerFrame, labelRcmdr(mainFrame, text = " "), sticky = "nw")
  tkgrid(randomFrame, labelRcmdr(mainFrame, text = " "), sticky = "nw")
  tkgrid(buttonsFrame, sticky = "w")
  dialogSuffix(use.tabs = FALSE, grid.buttons = TRUE)
}


central <- function () {
  defaults <- list(initial.basis = "3", initial.center = "4", initial.wbreps="1", initial.bbreps="1", initial.random="TRUE", initial.oneblock="FALSE")
  dialog.values <- getDialog("central", defaults)
  initializeDialog(title = gettextRcmdr("중심합성계획"), use.tabs = FALSE)
  mainFrame <- tkframe(top)
  UpdateModelNumber()
  modelName <- tclVar(paste("AnovaModel.", getRcmdr("modelNumber"),
                            sep = ""))
  modelFrame <- tkframe(top)
  model <- ttkentry(modelFrame, width = "20", textvariable = modelName)

  onOK <- function() {
    modelValue <- trim.blanks(tclvalue(modelName))

    basiss<-tclvalue(basis)
    centers<-tclvalue(center)
    wbrep<-tclvalue(wbreps)
    bbrep<-tclvalue(bbreps)
    randoms<-as.character(tclvalue(random))
    oneblocks<-as.character(tclvalue(oneblock))

    putDialog ("central", list (initial.basis = basiss, initial.center = centers, initial.wbreps = wbrep, initial.bbreps = bbrep,initial.random = randoms, initial.oneblock = oneblocks))
    closeDialog()

    doItAndPrint(paste("kccd(", "basis=", basiss,",", "n0=", centers,",","wbreps=",wbrep,",","bbreps=",bbrep,",","randomize=", randoms,"," ,"oneblock=",oneblocks,")", sep = ""))
    justDoIt(paste(modelValue, "<-kccd(", "basis=", basiss,",", "n0=", centers,",","wbreps=",wbrep,",","bbreps=",bbrep,",","randomize=", randoms,"," ,"oneblock=",oneblocks,")", sep = ""))
    activeModel(modelValue)
    tkfocus(CommanderWindow())
  }

  OKCancelHelp(helpSubject = "Anova", model = TRUE, reset = "central", apply = "central")

  basisFrame <- tkframe(mainFrame)
  basis <- tclVar(dialog.values$initial.basis)
  basisField <- ttkentry(basisFrame, width = "12", textvariable = basis)

  centerFrame <- tkframe(mainFrame)
  center <- tclVar(dialog.values$initial.center)
  centerField <- ttkentry(centerFrame, width = "12", textvariable = center)

  wbrepsFrame <- tkframe(mainFrame)
  wbreps <- tclVar(dialog.values$initial.wbreps)
  wbrepsField <- ttkentry(wbrepsFrame, width = "12", textvariable = wbreps)

  bbrepsFrame <- tkframe(mainFrame)
  bbreps <- tclVar(dialog.values$initial.bbreps)
  bbrepsField <- ttkentry(bbrepsFrame, width = "12", textvariable = bbreps)

  randomFrame <- tkframe(mainFrame)
  random <- tclVar(dialog.values$initial.random)
  randomField <- ttkentry(randomFrame, width = "12", textvariable = random)

  oneblockFrame <- tkframe(mainFrame)
  oneblock <- tclVar(dialog.values$initial.oneblock)
  oneblockField <- ttkentry(oneblockFrame, width = "12", textvariable = oneblock)


  tkgrid(labelRcmdr(mainFrame, text="  "), sticky = "nw")
  tkgrid(mainFrame, sticky="nw")
  tkgrid(labelRcmdr(modelFrame, text = gettextRcmdr("Enter name for model: ")),
         model, sticky = "w")
  tkgrid(modelFrame, sticky = "w")
  tkgrid(labelRcmdr(basisFrame, text = gettextRcmdr("요인의 수"), fg = getRcmdr("title.color"), font = "RcmdrTitleFont"), sticky = "w")
  tkgrid(labelRcmdr(centerFrame, text = gettextRcmdr("중심점의 수"), fg = getRcmdr("title.color"), font = "RcmdrTitleFont"), sticky = "w")
  tkgrid(labelRcmdr(wbrepsFrame, text = gettextRcmdr("블록 내 반복의 수"), fg = getRcmdr("title.color"), font = "RcmdrTitleFont"), sticky = "w")
  tkgrid(labelRcmdr(bbrepsFrame, text = gettextRcmdr("블록 간 반복의 수"), fg = getRcmdr("title.color"), font = "RcmdrTitleFont"), sticky = "w")
  tkgrid(labelRcmdr(randomFrame, text = gettextRcmdr("랜덤화 / TRUE or FALSE"), fg = getRcmdr("title.color"), font = "RcmdrTitleFont"), sticky = "w")
  tkgrid(labelRcmdr(oneblockFrame, text = gettextRcmdr("블록이 하나인가?/ TRUE or FALSE"), fg = getRcmdr("title.color"), font = "RcmdrTitleFont"), sticky = "w")
  tkgrid(basisField, sticky="w")
  tkgrid(centerField, sticky="w")
  tkgrid(wbrepsField, sticky="w")
  tkgrid(bbrepsField, sticky="w")
  tkgrid(randomField, sticky="w")
  tkgrid(oneblockField, sticky="w")
  tkgrid(basisFrame, labelRcmdr(mainFrame, text = " "), sticky = "nw")
  tkgrid(centerFrame, labelRcmdr(mainFrame, text = " "), sticky = "nw")
  tkgrid(wbrepsFrame, labelRcmdr(mainFrame, text = " "), sticky = "nw")
  tkgrid(bbrepsFrame, labelRcmdr(mainFrame, text = " "), sticky = "nw")
  tkgrid(randomFrame, labelRcmdr(mainFrame, text = " "), sticky = "nw")
  tkgrid(oneblockFrame, labelRcmdr(mainFrame, text = " "), sticky = "nw")
  tkgrid(buttonsFrame, sticky = "w")
  dialogSuffix(use.tabs = FALSE, grid.buttons = TRUE)
}


designsplit <- function () {
  defaults <- list(initial.factor1 = "c(1,2,3)", initial.factor2 = "c(1,2,3)", initial.repet="1", initial.seed="0", initial.random="TRUE", initial.std="FALSE")
  dialog.values <- getDialog("designsplit", defaults)
  initializeDialog(title = gettextRcmdr("분할법 난수표"), use.tabs = FALSE)
  mainFrame <- tkframe(top)
  UpdateModelNumber()
  modelName <- tclVar(paste("AnovaModel.", getRcmdr("modelNumber"),
                            sep = ""))
  modelFrame <- tkframe(top)
  model <- ttkentry(modelFrame, width = "20", textvariable = modelName)

  onOK <- function() {
    modelValue <- trim.blanks(tclvalue(modelName))

    factor11<-as.character(tclvalue(factor1))
    factor22<-as.character(tclvalue(factor2))
    repe<-tclvalue(repet)
    sed<-tclvalue(seed)
    randoms<-as.character(tclvalue(random))
    sttd<-as.character(tclvalue(std))

    putDialog ("designsplit", list (initial.factor1 = factor11, initial.factor2 = factor22, initial.repet = repe, initial.seed = sed,initial.random = randoms, initial.std = sttd))
    closeDialog()

    doItAndPrint(paste("design.split(", factor11,",", factor22,",","r=",repe,",","seed=",sed,",","randomization=", randoms,"," ,"std=",sttd,")", sep = ""))
    doItAndPrint(paste(modelValue, "<-design.split(", factor11,",", factor22,",","r=",repe,",","seed=",sed,",","randomization=", randoms,"," ,"std=",sttd,")", sep = ""))
    activeModel(modelValue)
    tkfocus(CommanderWindow())
  }

  OKCancelHelp(helpSubject = "Anova", model = TRUE, reset = "designsplit", apply = "designsplit")

  factor1Frame <- tkframe(mainFrame)
  factor1 <- tclVar(dialog.values$initial.factor1)
  factor1Field <- ttkentry(factor1Frame, width = "12", textvariable = factor1)

  factor2Frame <- tkframe(mainFrame)
  factor2 <- tclVar(dialog.values$initial.factor2)
  factor2Field <- ttkentry(factor2Frame, width = "12", textvariable = factor2)

  repetFrame <- tkframe(mainFrame)
  repet <- tclVar(dialog.values$initial.repet)
  repetField <- ttkentry(repetFrame, width = "12", textvariable = repet)

  seedFrame <- tkframe(mainFrame)
  seed <- tclVar(dialog.values$initial.seed)
  seedField <- ttkentry(seedFrame, width = "12", textvariable = seed)

  randomFrame <- tkframe(mainFrame)
  random <- tclVar(dialog.values$initial.random)
  randomField <- ttkentry(randomFrame, width = "12", textvariable = random)

  stdFrame <- tkframe(mainFrame)
  std <- tclVar(dialog.values$initial.std)
  stdField <- ttkentry(stdFrame, width = "12", textvariable = std)


  tkgrid(labelRcmdr(mainFrame, text="  "), sticky = "nw")
  tkgrid(mainFrame, sticky="nw")
  tkgrid(labelRcmdr(modelFrame, text = gettextRcmdr("Enter name for model: ")),
         model, sticky = "w")
  tkgrid(modelFrame, sticky = "w")
  tkgrid(labelRcmdr(factor1Frame, text = gettextRcmdr("요인1의 수준"), fg = getRcmdr("title.color"), font = "RcmdrTitleFont"), sticky = "w")
  tkgrid(labelRcmdr(factor2Frame, text = gettextRcmdr("요인2의 수준"), fg = getRcmdr("title.color"), font = "RcmdrTitleFont"), sticky = "w")
  tkgrid(labelRcmdr(repetFrame, text = gettextRcmdr("반복수"), fg = getRcmdr("title.color"), font = "RcmdrTitleFont"), sticky = "w")
  tkgrid(labelRcmdr(seedFrame, text = gettextRcmdr("Seed"), fg = getRcmdr("title.color"), font = "RcmdrTitleFont"), sticky = "w")
  tkgrid(labelRcmdr(randomFrame, text = gettextRcmdr("랜덤화 / TRUE or FALSE"), fg = getRcmdr("title.color"), font = "RcmdrTitleFont"), sticky = "w")
  tkgrid(labelRcmdr(stdFrame, text = gettextRcmdr("표준순서 / TRUE : 표준순서 or FALSE : 실행 순서"), fg = getRcmdr("title.color"), font = "RcmdrTitleFont"), sticky = "w")
  tkgrid(factor1Field, sticky="w")
  tkgrid(factor2Field, sticky="w")
  tkgrid(repetField, sticky="w")
  tkgrid(seedField, sticky="w")
  tkgrid(randomField, sticky="w")
  tkgrid(stdField, sticky="w")
  tkgrid(factor1Frame, labelRcmdr(mainFrame, text = " "), sticky = "nw")
  tkgrid(factor2Frame, labelRcmdr(mainFrame, text = " "), sticky = "nw")
  tkgrid(repetFrame, labelRcmdr(mainFrame, text = " "), sticky = "nw")
  tkgrid(seedFrame, labelRcmdr(mainFrame, text = " "), sticky = "nw")
  tkgrid(randomFrame, labelRcmdr(mainFrame, text = " "), sticky = "nw")
  tkgrid(stdFrame, labelRcmdr(mainFrame, text = " "), sticky = "nw")
  tkgrid(buttonsFrame, sticky = "w")
  dialogSuffix(use.tabs = FALSE, grid.buttons = TRUE)
}


qualaov <- function () {
  defaults <- list(initial.group = NULL,initial.response = NULL,initial.repet="1",initial.row = "1", initial.column="1", initial.fac="1")
  dialog.values <- getDialog("qualaov", defaults)
  initializeDialog(title = gettextRcmdr("계수치 분산분석"), use.tabs = FALSE)
  dataFrame <- tkframe(top)
  groupBox <- variableListBox(dataFrame, selectmode = "multiple",
                              title = gettextRcmdr("Factors (pick one or more)"),
                              initialSelection = varPosn(dialog.values$initial.group,"all"))
  responseBox <- variableListBox(dataFrame, selectmode = "single"
                                 ,title = gettextRcmdr("Response Variable (pick one)"),
                                 initialSelection = varPosn(dialog.values$initial.response,"all"))

  onOK <- function(){
    groups <- getSelection(groupBox)
    response <- getSelection(responseBox)
    repe <- tclvalue(repet)
    rows <- tclvalue(row)
    columns <- tclvalue(column)
    facc <- tclvalue(fac)

    if (length(groups) == 0) {
      errorCondition(recall = sscatterplot, message = gettextRcmdr("You must select at least one factor."))
      return()}
    if (length(response) == 0) {
      errorCondition(recall = sscatterplot, message = gettextRcmdr("You must select at least one factor."))
      return()}

    putDialog ("qualaov", list (initial.group = groups, initial.response = response, initial.repet = repe, initial.row = rows, initial.column = columns, initial.fac = facc))
    closeDialog()

    .activeDataSet <- ActiveDataSet()
    groups.list <- paste(paste(groups, sep = ""), collapse = "+")
    doItAndPrint(paste(paste(rep(groups),rep('<-'),rep("as.factor("),rep(.activeDataSet),rep('$'),groups,")", sep = ""), collapse = "
"))
    doItAndPrint(paste(paste(rep(.activeDataSet),rep('$'),rep(groups),rep('<-'),rep("as.factor("),rep(.activeDataSet),rep('$'),groups,")", sep = ""), collapse = "
"))
    doItAndPrint(paste("qual.aov(", .activeDataSet,'$',response,"~",groups.list,",",.activeDataSet,",","r=",repe,",", "i=",rows,",","j=",columns,",","fac=",facc,")", sep = ""))
    tkfocus(CommanderWindow())
  }

  OKCancelHelp(helpSubject = "Anova", model = TRUE, reset = "qualaov", apply = "qualaov")

  repetFrame <- tkframe(dataFrame)
  repet <- tclVar(dialog.values$initial.repet)
  repetField <- ttkentry(repetFrame, width = "12", textvariable = repet)

  rowFrame <- tkframe(dataFrame)
  row <- tclVar(dialog.values$initial.row)
  rowField <- ttkentry(rowFrame, width = "12", textvariable = row)

  columnFrame <- tkframe(dataFrame)
  column <- tclVar(dialog.values$initial.column)
  columnField <- ttkentry(columnFrame, width = "12", textvariable = column)

  facFrame <- tkframe(dataFrame)
  fac <- tclVar(dialog.values$initial.fac)
  facField <- ttkentry(facFrame, width = "12", textvariable = fac)

  tkgrid(getFrame(groupBox), labelRcmdr(dataFrame, text="  "), sticky = "nw")
  tkgrid(getFrame(responseBox), labelRcmdr(dataFrame, text=" "), sticky = "nw")
  tkgrid(labelRcmdr(repetFrame, text = gettextRcmdr("반복수"), fg = getRcmdr("title.color"), font = "RcmdrTitleFont"), sticky = "w")
  tkgrid(labelRcmdr(rowFrame, text = gettextRcmdr("행의 수"), fg = getRcmdr("title.color"), font = "RcmdrTitleFont"), sticky = "w")
  tkgrid(labelRcmdr(columnFrame, text = gettextRcmdr("열의 수"), fg = getRcmdr("title.color"), font = "RcmdrTitleFont"), sticky = "w")
  tkgrid(labelRcmdr(facFrame, text = gettextRcmdr("1 : 일원배치 / 2 : 이원배치"), fg = getRcmdr("title.color"), font = "RcmdrTitleFont"), sticky = "w")
  tkgrid(repetField, sticky="w")
  tkgrid(rowField, sticky="w")
  tkgrid(columnField, sticky="w")
  tkgrid(facField, sticky="w")
  tkgrid(repetFrame, labelRcmdr(dataFrame, text = " "), sticky = "nw")
  tkgrid(rowFrame, labelRcmdr(dataFrame, text = " "), sticky = "nw")
  tkgrid(columnFrame, labelRcmdr(dataFrame, text = " "), sticky = "nw")
  tkgrid(facFrame, labelRcmdr(dataFrame, text = " "), sticky = "nw")
  tkgrid(dataFrame, sticky="nw")
  tkgrid(buttonsFrame, sticky = "w")
  dialogSuffix(use.tabs = FALSE, grid.buttons = TRUE)
}

orth <- function () {
  defaults <- list(initial.design = "",initial.randomize = "TRUE",initial.replicates = "1")
  dialog.values <- getDialog("orth", defaults)
  initializeDialog(title = gettextRcmdr("직교배열법 난수표"), use.tabs = FALSE)
  dataFrame <- tkframe(top)

  onOK <- function() {
    designn <- as.character(tclvalue(design))
    ran <- as.character(tclvalue(randomize))
    repli <- tclvalue(replicates)

    if (length(designn) == 0) {
      errorCondition(recall = orth, message = gettextRcmdr("You must Fill in the Blank."))
      return()}

    putDialog ("orth", list (initial.design = designn, initial.randomize = ran, initial.replicates = repli))
    closeDialog()

    doItAndPrint(paste("kdesign(",'"',designn,'"',",","randomize=",ran,",","replicates=",repli,")", sep = ""))
    tkfocus(CommanderWindow())
  }

  OKCancelHelp(helpSubject = "taguchiDesign", model = TRUE, reset = "orth", apply = "orth")

  designFrame <- tkframe(dataFrame)
  design <- tclVar(dialog.values$initial.design)
  designField <- ttkentry(designFrame, width = "12", textvariable = design)

  randomizeFrame <- tkframe(dataFrame)
  randomize <- tclVar(dialog.values$initial.randomize)
  randomizeField <- ttkentry(randomizeFrame, width = "12", textvariable = randomize)

  replicatesFrame <- tkframe(dataFrame)
  replicates <- tclVar(dialog.values$initial.replicates)
  replicatesField <- ttkentry(replicatesFrame, width = "12", textvariable = replicates)

  tkgrid(labelRcmdr(designFrame, text = gettextRcmdr("다구찌 디자인(입력 : Help 버튼에서 디자인을 참고하십시오)"), fg = getRcmdr("title.color"), font = "RcmdrTitleFont"), sticky = "w")
  tkgrid(labelRcmdr(randomizeFrame, text = gettextRcmdr("랜덤화 / TRUE or FALSE"), fg = getRcmdr("title.color"), font = "RcmdrTitleFont"), sticky = "w")
  tkgrid(labelRcmdr(replicatesFrame, text = gettextRcmdr("반복수"), fg = getRcmdr("title.color"), font = "RcmdrTitleFont"), sticky = "w")
  tkgrid(designField, sticky="w")
  tkgrid(randomizeField, sticky="w")
  tkgrid(replicatesField, sticky="w")
  tkgrid(designFrame, labelRcmdr(dataFrame, text = " "), sticky = "nw")
  tkgrid(randomizeFrame, labelRcmdr(dataFrame, text = " "), sticky = "nw")
  tkgrid(replicatesFrame, labelRcmdr(dataFrame, text = " "), sticky = "nw")
  tkgrid(dataFrame, sticky="nw")
  tkgrid(buttonsFrame, sticky = "w")
  dialogSuffix(use.tabs = FALSE, grid.buttons = TRUE)
}

ffr.f2 <- function () {
  defaults <- list(initial.row = "0", initial.facnum = "0", initial.random = "TRUE", initial.block = 'c("ABC","BCD")')
  dialog.values <- getDialog("ffr.f2", defaults)
  initializeDialog(title = gettextRcmdr("교락법 난수표"))
  mainFrame <- tkframe(top)
  UpdateModelNumber()
  modelName <- tclVar(paste("AnovaModel.", getRcmdr("modelNumber"),
                            sep = ""))
  modelFrame <- tkframe(top)
  model <- ttkentry(modelFrame, width = "20", textvariable = modelName)

  onOK <- function() {
    modelValue <- trim.blanks(tclvalue(modelName))

    roww <- tclvalue(row)
    facnums <- tclvalue(facnum)
    randoms <- as.character(tclvalue(random))
    blocks <- as.character(tclvalue(block))

    putDialog ("ffr.f2", list (initial.row = roww, initial.facnum = facnums, initial.random = randoms, initial.block = blocks))
    closeDialog()

    doItAndPrint(paste("FrF2(", roww,"," ,facnums,",randomize=",randoms, ",blocks=",blocks,")", sep = ""))
    justDoIt(paste(modelValue, "<-FrF2(", roww,"," ,facnums,",randomize=",randoms, ",blocks=",blocks,")", sep = ""))
    activeModel(modelValue)

    tkfocus(CommanderWindow())
  }

  OKCancelHelp(helpSubject = "Anova", model = TRUE, reset = "ffr.f2", apply = "ffr.f2")

  rowFrame <- tkframe(mainFrame)
  row <- tclVar(dialog.values$initial.row)
  rowField <- ttkentry(rowFrame, width = "12", textvariable = row)

  facnumFrame <- tkframe(mainFrame)
  facnum <- tclVar(dialog.values$initial.facnum)
  facnumField <- ttkentry(facnumFrame, width = "12", textvariable = facnum)

  randomFrame <- tkframe(mainFrame)
  random <- tclVar(dialog.values$initial.random)
  randomField <- ttkentry(randomFrame, width = "12", textvariable = random)

  blockFrame <- tkframe(mainFrame)
  block <- tclVar(dialog.values$initial.block)
  blockField <- ttkentry(blockFrame, width = "12", textvariable = block)

  tkgrid(labelRcmdr(mainFrame, text="  "), sticky = "nw")
  tkgrid(mainFrame, sticky="nw")
  tkgrid(labelRcmdr(modelFrame, text = gettextRcmdr("Enter name for model: ")),
         model, sticky = "w")
  tkgrid(modelFrame, sticky = "w")
  tkgrid(labelRcmdr(rowFrame, text = gettextRcmdr("행의 수"), fg = getRcmdr("title.color"), font = "RcmdrTitleFont"), sticky = "w")
  tkgrid(labelRcmdr(facnumFrame, text = gettextRcmdr("인자의 수"), fg = getRcmdr("title.color"), font = "RcmdrTitleFont"), sticky = "w")
  tkgrid(labelRcmdr(randomFrame, text = gettextRcmdr("랜덤화 / TRUE or FALSE"), fg = getRcmdr("title.color"), font = "RcmdrTitleFont"), sticky = "w")
  tkgrid(labelRcmdr(blockFrame, text = gettextRcmdr("블록"), fg = getRcmdr("title.color"), font = "RcmdrTitleFont"), sticky = "w")
  tkgrid(rowField, sticky="w")
  tkgrid(facnumField, sticky="w")
  tkgrid(randomField, sticky="w")
  tkgrid(blockField, sticky="w")
  tkgrid(rowFrame, labelRcmdr(mainFrame, text = " "), sticky = "nw")
  tkgrid(facnumFrame, labelRcmdr(mainFrame, text = " "), sticky = "nw")
  tkgrid(randomFrame, labelRcmdr(mainFrame, text = " "), sticky = "nw")
  tkgrid(blockFrame, labelRcmdr(mainFrame, text = " "), sticky = "nw")
  tkgrid(buttonsFrame, sticky = "w")
  dialogSuffix(use.tabs = FALSE, grid.buttons = TRUE)
}

fr.f2 <- function () {
  defaults <- list(initial.row = "0", initial.facnum = "0", initial.random = "TRUE", initial.gene = "")
  dialog.values <- getDialog("fr.f2", defaults)
  initializeDialog(title = gettextRcmdr("일부실시법 난수표"))
  mainFrame <- tkframe(top)
  UpdateModelNumber()
  modelName <- tclVar(paste("AnovaModel.", getRcmdr("modelNumber"),
                            sep = ""))
  modelFrame <- tkframe(top)
  model <- ttkentry(modelFrame, width = "20", textvariable = modelName)

  onOK <- function() {
    modelValue <- trim.blanks(tclvalue(modelName))

    roww <- tclvalue(row)
    facnums <- tclvalue(facnum)
    randoms <- as.character(tclvalue(random))
    genee <- as.character(tclvalue(gene))

    putDialog ("fr.f2", list (initial.row = roww, initial.facnum = facnums, initial.random = randoms, initial.gene = genee))
    closeDialog()

    doItAndPrint(paste("FrF2(", roww,"," ,facnums,",randomize=",randoms, ",generators =c(",genee,')',")", sep = ""))
    doItAndPrint(paste(modelValue, "<-FrF2(", roww,"," ,facnums,",randomize=",randoms, ",generators =c(",genee,')' ,")", sep = ""))
    activeModel(modelValue)

    tkfocus(CommanderWindow())
  }

  OKCancelHelp(helpSubject = "FrF2", model = TRUE, reset = "fr.f2", apply = "fr.f2")

  rowFrame <- tkframe(mainFrame)
  row <- tclVar(dialog.values$initial.row)
  rowField <- ttkentry(rowFrame, width = "12", textvariable = row)

  facnumFrame <- tkframe(mainFrame)
  facnum <- tclVar(dialog.values$initial.facnum)
  facnumField <- ttkentry(facnumFrame, width = "12", textvariable = facnum)

  randomFrame <- tkframe(mainFrame)
  random <- tclVar(dialog.values$initial.random)
  randomField <- ttkentry(randomFrame, width = "12", textvariable = random)

  geneFrame <- tkframe(mainFrame)
  gene <- tclVar(dialog.values$initial.gene)
  geneField <- ttkentry(geneFrame, width = "12", textvariable = gene)

  tkgrid(labelRcmdr(mainFrame, text="  "), sticky = "nw")
  tkgrid(mainFrame, sticky="nw")
  tkgrid(labelRcmdr(modelFrame, text = gettextRcmdr("Enter name for model: ")),
         model, sticky = "w")
  tkgrid(modelFrame, sticky = "w")
  tkgrid(labelRcmdr(rowFrame, text = gettextRcmdr("행의 수"), fg = getRcmdr("title.color"), font = "RcmdrTitleFont"), sticky = "w")
  tkgrid(labelRcmdr(facnumFrame, text = gettextRcmdr("인자의 수"), fg = getRcmdr("title.color"), font = "RcmdrTitleFont"), sticky = "w")
  tkgrid(labelRcmdr(randomFrame, text = gettextRcmdr("랜덤화 / TRUE or FALSE"), fg = getRcmdr("title.color"), font = "RcmdrTitleFont"), sticky = "w")
  tkgrid(labelRcmdr(geneFrame, text = gettextRcmdr('generators /
ex) 1/4 일부실시법 : "AB","AC", 1/8 일부실시법 : "AB","AC","BC" '), fg = getRcmdr("title.color"), font = "RcmdrTitleFont"), sticky = "w")
  tkgrid(rowField, sticky="w")
  tkgrid(facnumField, sticky="w")
  tkgrid(randomField, sticky="w")
  tkgrid(geneField, sticky="w")
  tkgrid(rowFrame, labelRcmdr(mainFrame, text = " "), sticky = "nw")
  tkgrid(facnumFrame, labelRcmdr(mainFrame, text = " "), sticky = "nw")
  tkgrid(randomFrame, labelRcmdr(mainFrame, text = " "), sticky = "nw")
  tkgrid(geneFrame, labelRcmdr(mainFrame, text = " "), sticky = "nw")
  tkgrid(buttonsFrame, sticky = "w")
  dialogSuffix(use.tabs = FALSE, grid.buttons = TRUE)
}

steep <- function () {
  defaults <- list(initial.group = NULL, initial.response = NULL)
  dialog.values <- getDialog("steep", defaults)
  initializeDialog(title = gettextRcmdr("최대경사법"))
  mainFrame <- tkframe(top)
  UpdateModelNumber()
  modelName <- tclVar(paste("AnovaModel.", getRcmdr("modelNumber"),
                            sep = ""))
  modelFrame <- tkframe(top)
  model <- ttkentry(modelFrame, width = "20", textvariable = modelName)
  groupBox <- variableListBox(mainFrame, selectmode = "multiple",
                              title = gettextRcmdr("Factors (pick one or more)"),
                              initialSelection = varPosn(dialog.values$initial.group, "all"))
  responseBox <- variableListBox(mainFrame, title = gettextRcmdr("Response Variable (pick one)"),
                                 initialSelection = varPosn(dialog.values$initial.response, "all"))

  onOK <- function() {
    groups <- getSelection(groupBox)
    response <- getSelection(responseBox)
    modelValue <- trim.blanks(tclvalue(modelName))


    if (length(groups) == 0) {
      errorCondition(recall = steep, message = gettextRcmdr("You must select at least one factor."))
      return()}
    if (length(response) == 0) {
      errorCondition(recall = steep, message = gettextRcmdr("You must select at least one factor."))
      return()}
    putDialog ("steep", list (initial.group = groups, initial.response = response))
    closeDialog()

    .activeDataSet <- ActiveDataSet()
    groups.list <- paste(paste(groups, sep = ""), collapse = ",")
    justDoIt(paste("rsm(",response,"~SO(",groups.list,"),data=",.activeDataSet,")", sep = ""))
    justDoIt(paste(modelValue, "<-rsm(",response,"~SO(",groups.list,"),data=",.activeDataSet,")", sep = ""))
    doItAndPrint(paste("steepest(", modelValue,")", sep = ""))

    tkfocus(CommanderWindow())
  }

  OKCancelHelp(helpSubject = "Anova", model = TRUE, reset = "conto", apply = "conto")

  tkgrid(labelRcmdr(mainFrame, text="  "), sticky = "nw")
  tkgrid(mainFrame, sticky="nw")
  tkgrid(getFrame(groupBox), labelRcmdr(mainFrame, text="  "), getFrame(responseBox), sticky = "nw")
  tkgrid(labelRcmdr(modelFrame, text = gettextRcmdr("Enter name for model: ")),
         model, sticky = "w")
  tkgrid(modelFrame, sticky = "w")
  tkgrid(buttonsFrame, sticky = "w")
  dialogSuffix(use.tabs = FALSE, grid.buttons = TRUE)
}

rsmanova <- function () {
  defaults <- list(initial.response = "", initial.fac1 = "", initial.fac2 = "", initial.fac3 = "", initial.fac4 = "",
                   initial.for1 = "", initial.for2 = "", initial.for3 = " ", initial.for4 = "")
  dialog.values <- getDialog("rsmanova", defaults)
  initializeDialog(title = gettextRcmdr("반응표면 분산분석"))
  dataFrame <- tkframe(top)

  onOK <- function() {
    responses <- as.character(tclvalue(response))
    fac11 <- as.character(tclvalue(fac1))
    fac22 <- as.character(tclvalue(fac2))
    fac33 <- as.character(tclvalue(fac3))
    fac44 <- as.character(tclvalue(fac4))
    for11 <- as.character(tclvalue(for1))
    for22 <- as.character(tclvalue(for2))
    for33 <- as.character(tclvalue(for3))
    for44 <- as.character(tclvalue(for4))

    putDialog ("rsmanova", list (initial.response = responses, initial.fac1 = fac11,initial.fac2 = fac22,initial.fac3 = fac33,initial.fac4 = fac44,
                                 initial.for1 = for11, initial.for2 = for22, initial.for3 = for33, initial.for4 = for44))
    closeDialog()

    .activeDataSet <- ActiveDataSet()
    if (fac11!="" & fac22=="" & fac33=="" & fac44==""){
      doItAndPrint(paste("kk<-coded.data(", .activeDataSet,",x1~",for11,")", sep = ""))
      doItAndPrint(paste("ho<-rsm(","kk$",responses,"~SO(x1)",",data=kk)", sep = ""))
    }
    if (fac11!=""&fac22!=""&fac33=="" & fac44==""){
      doItAndPrint(paste("kk<-coded.data(", .activeDataSet,",x1~",for11,",x2~",for22,")", sep = ""))
      doItAndPrint(paste("ho<-rsm(","kk$",responses,"~SO(x1,x2)",",data=kk)", sep = ""))
    }
    if (fac11!=""&fac22!=""&fac33!=""&fac44==""){
      doItAndPrint(paste("kk<-coded.data(", .activeDataSet,",x1~",for11,",x2~",for22,",x3~",for33, ")", sep = ""))
      doItAndPrint(paste("ho<-rsm(","kk$",responses,"~SO(x1,x2,x3)",",data=kk)", sep = ""))
    }
    if (fac11!=""&fac22!=""&fac33!=""&fac44!=""){
      doItAndPrint(paste("kk<-coded.data(", .activeDataSet,",x1~",for11,",x2~",for22,",x3~",for33,",x4~",for44, ")", sep = ""))
      doItAndPrint(paste("ho<-rsm(","kk$",responses,"~SO(x1,x2,x3,x4)",",data=kk)", sep = ""))
      tkfocus(CommanderWindow())
    }
    doItAndPrint(paste("summary(ho)", sep = ""))
  }

  OKCancelHelp(helpSubject = "Anova", model = TRUE, reset = "rsmanova", apply = "rsmanova")


  responseFrame <- tkframe(dataFrame)
  response <- tclVar(dialog.values$initial.response)
  responseField <- ttkentry(responseFrame, width = "20", textvariable = response)

  fac1Frame <- tkframe(dataFrame)
  fac1 <- tclVar(dialog.values$initial.fac1)
  fac1Field <- ttkentry(fac1Frame, width = "20", textvariable = fac1)

  fac2Frame <- tkframe(dataFrame)
  fac2 <- tclVar(dialog.values$initial.fac2)
  fac2Field <- ttkentry(fac2Frame, width = "20", textvariable = fac2)

  fac3Frame <- tkframe(dataFrame)
  fac3 <- tclVar(dialog.values$initial.fac3)
  fac3Field <- ttkentry(fac3Frame, width = "20", textvariable = fac3)

  fac4Frame <- tkframe(dataFrame)
  fac4 <- tclVar(dialog.values$initial.fac4)
  fac4Field <- ttkentry(fac4Frame, width = "20", textvariable = fac4)

  for1Frame <- tkframe(dataFrame)
  for1 <- tclVar(dialog.values$initial.for1)
  for1Field <- ttkentry(for1Frame, width = "20", textvariable = for1)

  for2Frame <- tkframe(dataFrame)
  for2 <- tclVar(dialog.values$initial.for2)
  for2Field <- ttkentry(for2Frame, width = "20", textvariable = for2)

  for3Frame <- tkframe(dataFrame)
  for3 <- tclVar(dialog.values$initial.for3)
  for3Field <- ttkentry(for3Frame, width = "20", textvariable = for3)

  for4Frame <- tkframe(dataFrame)
  for4 <- tclVar(dialog.values$initial.for4)
  for4Field <- ttkentry(for4Frame, width = "20", textvariable = for4)

  tkgrid(labelRcmdr(responseFrame, text = gettextRcmdr("Response"), fg = getRcmdr("title.color"), font = "RcmdrTitleFont"), sticky = "w")
  tkgrid(labelRcmdr(fac1Frame, text = gettextRcmdr("Factor 1"), fg = getRcmdr("title.color"), font = "RcmdrTitleFont"), sticky = "w")
  tkgrid(labelRcmdr(fac2Frame, text = gettextRcmdr("Factor 2"), fg = getRcmdr("title.color"), font = "RcmdrTitleFont"), sticky = "w")
  tkgrid(labelRcmdr(fac3Frame, text = gettextRcmdr("Factor 3"), fg = getRcmdr("title.color"), font = "RcmdrTitleFont"), sticky = "w")
  tkgrid(labelRcmdr(fac4Frame, text = gettextRcmdr("Factor 4"), fg = getRcmdr("title.color"), font = "RcmdrTitleFont"), sticky = "w")
  tkgrid(labelRcmdr(for1Frame, text = gettextRcmdr("x1 Formula / ex) (a1-12)/8"), fg = getRcmdr("title.color"), font = "RcmdrTitleFont"), sticky = "w")
  tkgrid(labelRcmdr(for2Frame, text = gettextRcmdr("x2 Formula / ex) (a2-12)/8"), fg = getRcmdr("title.color"), font = "RcmdrTitleFont"), sticky = "w")
  tkgrid(labelRcmdr(for3Frame, text = gettextRcmdr("x3 Formula / ex) (a3-12)/8"), fg = getRcmdr("title.color"), font = "RcmdrTitleFont"), sticky = "w")
  tkgrid(labelRcmdr(for4Frame, text = gettextRcmdr("x4 Formula / ex) (a4-12)/8"), fg = getRcmdr("title.color"), font = "RcmdrTitleFont"), sticky = "w")
  tkgrid(responseField, sticky="w")
  tkgrid(fac1Field, sticky="w")
  tkgrid(fac2Field, sticky="w")
  tkgrid(fac3Field, sticky="w")
  tkgrid(fac4Field, sticky="w")
  tkgrid(for1Field, sticky="w")
  tkgrid(for2Field, sticky="w")
  tkgrid(for3Field, sticky="w")
  tkgrid(for4Field, sticky="w")
  tkgrid(responseFrame, labelRcmdr(dataFrame, text = " "), sticky = "nw")
  tkgrid(fac1Frame, labelRcmdr(dataFrame, text = " "), sticky = "nw")
  tkgrid(fac2Frame, labelRcmdr(dataFrame, text = " "), sticky = "nw")
  tkgrid(fac3Frame, labelRcmdr(dataFrame, text = " "), sticky = "nw")
  tkgrid(fac4Frame, labelRcmdr(dataFrame, text = " "), sticky = "nw")
  tkgrid(for1Frame, labelRcmdr(dataFrame, text = " "), sticky = "nw")
  tkgrid(for2Frame, labelRcmdr(dataFrame, text = " "), sticky = "nw")
  tkgrid(for3Frame, labelRcmdr(dataFrame, text = " "), sticky = "nw")
  tkgrid(for4Frame, labelRcmdr(dataFrame, text = " "), sticky = "nw")
  tkgrid(dataFrame, sticky="w")
  tkgrid(buttonsFrame, sticky = "w")
  dialogSuffix(use.tabs = FALSE, grid.buttons = TRUE)
}

equalvar <- function () {
  defaults <- list(initial.group = NULL, initial.response = NULL)
  dialog.values <- getDialog("equalvar", defaults)
  initializeDialog(title = gettextRcmdr("등분산성 검정"))
  dataFrame <- tkframe(top)
  groupBox <- variableListBox(dataFrame, selectmode = "single",
                              title = gettextRcmdr("요인 (하나 선택)"),
                              initialSelection = varPosn(dialog.values$initial.group, "all"))
  responseBox <- variableListBox(dataFrame, title = gettextRcmdr("Response Variable (pick one)"),
                                 initialSelection = varPosn(dialog.values$initial.response, "all"))

  onOK <- function() {
    groups <- getSelection(groupBox)
    response <- getSelection(responseBox)

    if (length(groups) == 0) {
      errorCondition(recall = equalvar, message = gettextRcmdr("You must select at least one factor."))
      return()}
    if (length(response) == 0) {
      errorCondition(recall = equalvar, message = gettextRcmdr("You must select at least one factor."))
      return()}
    putDialog ("equalvar", list (initial.group = groups, initial.response = response))
    closeDialog()

    .activeDataSet <- ActiveDataSet()
    groups.list <- paste(paste(groups, sep = ""), collapse = ", ")
    doItAndPrint(paste("equal.var(", .activeDataSet,"$",response,"~", .activeDataSet,"$",groups,")", sep = ""))
    tkfocus(CommanderWindow())
  }

  OKCancelHelp(helpSubject = "Anova", model = TRUE, reset = "equlvar", apply = "equalvar")

  tkgrid(getFrame(groupBox), labelRcmdr(dataFrame, text="  "), getFrame(responseBox), sticky = "nw")
  tkgrid(dataFrame, sticky="w")
  tkgrid(buttonsFrame, sticky = "w")
  dialogSuffix(use.tabs = FALSE, grid.buttons = TRUE)
}

conto <- function () {
  defaults <- list(initial.group = NULL, initial.response = NULL)
  dialog.values <- getDialog("conto", defaults)
  initializeDialog(title = gettextRcmdr("등고선도"))
  mainFrame <- tkframe(top)
  UpdateModelNumber()
  modelName <- tclVar(paste("AnovaModel.", getRcmdr("modelNumber"),
                            sep = ""))
  modelFrame <- tkframe(top)
  model <- ttkentry(modelFrame, width = "20", textvariable = modelName)
  groupBox <- variableListBox(mainFrame, selectmode = "multiple",
                              title = gettextRcmdr("Factors (pick one or more)"),
                              initialSelection = varPosn(dialog.values$initial.group, "all"))
  responseBox <- variableListBox(mainFrame, title = gettextRcmdr("Response Variable (pick one)"),
                                 initialSelection = varPosn(dialog.values$initial.response, "all"))

  onOK <- function() {
    groups <- getSelection(groupBox)
    response <- getSelection(responseBox)
    modelValue <- trim.blanks(tclvalue(modelName))


    if (length(groups) == 0) {
      errorCondition(recall = conto, message = gettextRcmdr("You must select at least one factor."))
      return()}
    if (length(response) == 0) {
      errorCondition(recall = conto, message = gettextRcmdr("You must select at least one factor."))
      return()}
    putDialog ("conto", list (initial.group = groups, initial.response = response))
    closeDialog()

    .activeDataSet <- ActiveDataSet()
    groups.list <- paste(paste(groups, sep = ""), collapse = ",")
    doItAndPrint(paste("rsm(",response,"~SO(",groups.list,"),data=",.activeDataSet,")", sep = ""))
    doItAndPrint(paste(modelValue, "<-rsm(",response,"~SO(",groups.list,"),data=",.activeDataSet,")", sep = ""))
    {
      groups.list <- paste(paste(groups, sep = ""), collapse = "+")
      doItAndPrint(paste("contour(", modelValue,",~",groups.list,',image=TRUE)', sep = ""))
    }
    tkfocus(CommanderWindow())
  }

  OKCancelHelp(helpSubject = "Anova", model = TRUE, reset = "conto", apply = "conto")

  tkgrid(labelRcmdr(mainFrame, text="  "), sticky = "nw")
  tkgrid(mainFrame, sticky="nw")
  tkgrid(getFrame(groupBox), labelRcmdr(mainFrame, text="  "), getFrame(responseBox), sticky = "nw")
  tkgrid(labelRcmdr(modelFrame, text = gettextRcmdr("Enter name for model: ")),
         model, sticky = "w")
  tkgrid(modelFrame, sticky = "w")
  tkgrid(buttonsFrame, sticky = "w")
  dialogSuffix(use.tabs = FALSE, grid.buttons = TRUE)
}

dispwin <- function () {
  defaults <- list(initial.group = NULL, initial.response = NULL, initial.formul = "", initial.ranfac = "", initial.rc = "TRUE", initial.nest = "TRUE", initial.fac = 3)
  dialog.values <- getDialog("dispwin", defaults)
  initializeDialog(title = gettextRcmdr("분할법 산포 추정"))
  dataFrame <- tkframe(top)
  groupBox <- variableListBox(dataFrame, selectmode = "multiple",
                              title = gettextRcmdr("요인 및 반복 (하나 이상 선택)"),
                              initialSelection = varPosn(dialog.values$initial.group, "all"))
  responseBox <- variableListBox(dataFrame, title = gettextRcmdr("Response Variable (pick one)"),
                                 initialSelection = varPosn(dialog.values$initial.response, "all"))

  onOK <- function() {
    groups <- getSelection(groupBox)
    response <- getSelection(responseBox)
    formull <- as.character(tclvalue(formul))
    ranfacc <- as.character(tclvalue(ranfac))
    rcc <- as.character(tclvalue(rc))
    spp <- as.character(tclvalue(sp))
    nestt <- as.character(tclvalue(nest))
    facc <- tclvalue(fac)

    if (length(groups) == 0) {
      errorCondition(recall = dispwin, message = gettextRcmdr("You must select at least one factor."))
      return()}
    if (length(response) == 0) {
      errorCondition(recall = dispwin, message = gettextRcmdr("You must select at least one factor."))
      return()}
    putDialog ("dispwin", list (initial.group = groups, initial.response = response, initial.formul = formull, initial.ranfac = ranfacc, initial.rc = rcc,
                                initial.nest = nestt, initial.fac = facc))
    closeDialog()

    .activeDataSet <- ActiveDataSet()
    groups.list <- paste(paste(groups, sep = ""), collapse = ", ")
    doItAndPrint(paste(paste(rep(groups),rep('<-'),rep("as.factor("),rep(.activeDataSet),rep('$'),groups,")", sep = ""), collapse = "
"))
    doItAndPrint(paste(paste(rep(.activeDataSet),rep('$'),rep(groups),rep('<-'),rep("as.factor("),rep(.activeDataSet),rep('$'),groups,")", sep = ""), collapse = "
"))
    if (ranfacc == ""){
      doItAndPrint(paste("dispersion(", .activeDataSet,"$",response,"~", formull,",data=",.activeDataSet,",ranfac=NULL",",rc=",rcc,",sp=TRUE", ",nest=",nestt, ",fac=",facc,")", sep = ""))
    }
    if (ranfacc != ""){
      doItAndPrint(paste("dispersion(", .activeDataSet,"$",response,"~", formull,",data=",.activeDataSet,",ranfac=c(",ranfacc,')',",rc=",rcc,",sp=TRUE", ",nest=",nestt, ",fac=",facc,")", sep = ""))
    }
    tkfocus(CommanderWindow())
  }

  OKCancelHelp(helpSubject = "Anova", model = TRUE, reset = "dispwin", apply = "dispwin")

  formulFrame <- tkframe(dataFrame)
  formul <- tclVar(dialog.values$initial.formul)
  formulField <- ttkentry(formulFrame, width = "20", textvariable = formul)

  ranfacFrame <- tkframe(dataFrame)
  ranfac <- tclVar(dialog.values$initial.ranfac)
  ranfacField <- ttkentry(ranfacFrame, width = "20", textvariable = ranfac)

  rcFrame <- tkframe(dataFrame)
  rc <- tclVar(dialog.values$initial.rc)
  rcField <- ttkentry(rcFrame, width = "20", textvariable = rc)

  nestFrame <- tkframe(dataFrame)
  nest <- tclVar(dialog.values$initial.nest)
  nestField <- ttkentry(nestFrame, width = "20", textvariable = nest)

  facFrame <- tkframe(dataFrame)
  fac <- tclVar(dialog.values$initial.fac)
  facField <- ttkentry(facFrame, width = "20", textvariable = fac)

  tkgrid(labelRcmdr(formulFrame, text = gettextRcmdr("Formula / ex) r*a*b*c"), fg = getRcmdr("title.color"), font = "RcmdrTitleFont"), sticky = "w")
  tkgrid(labelRcmdr(ranfacFrame, text = gettextRcmdr('변량인자 / ex) "b","c"'), fg = getRcmdr("title.color"), font = "RcmdrTitleFont"), sticky = "w")
  tkgrid(labelRcmdr(nestFrame, text = gettextRcmdr('지분실험법 / TRUE : 지분실험법 / FALSE : 지분실험법이 아니다'), fg = getRcmdr("title.color"), font = "RcmdrTitleFont"), sticky = "w")
  tkgrid(labelRcmdr(rcFrame, text = gettextRcmdr("반복수 / TRUE : 일정 or FALSE : 일정하지 않다"), fg = getRcmdr("title.color"), font = "RcmdrTitleFont"), sticky = "w")
  tkgrid(labelRcmdr(facFrame, text = gettextRcmdr("선택 : 1=단일분할법 일원배치, 2=단일분할법 이원배치, 3=이단분할법"), fg = getRcmdr("title.color"), font = "RcmdrTitleFont"), sticky = "w")
  tkgrid(formulField, sticky="w")
  tkgrid(ranfacField, sticky="w")
  tkgrid(nestField, sticky="w")
  tkgrid(rcField, sticky="w")
  tkgrid(facField, sticky="w")
  tkgrid(formulFrame, labelRcmdr(dataFrame, text = " "), sticky = "nw")
  tkgrid(ranfacFrame, labelRcmdr(dataFrame, text = " "), sticky = "nw")
  tkgrid(nestFrame, labelRcmdr(dataFrame, text = " "), sticky = "nw")
  tkgrid(rcFrame, labelRcmdr(dataFrame, text = " "), sticky = "nw")
  tkgrid(facFrame, labelRcmdr(dataFrame, text = " "), sticky = "nw")
  tkgrid(getFrame(groupBox), labelRcmdr(dataFrame, text="  "), getFrame(responseBox), sticky = "nw")
  tkgrid(dataFrame, sticky="w")
  tkgrid(buttonsFrame, sticky = "w")
  dialogSuffix(use.tabs = FALSE, grid.buttons = TRUE)
}

meanwin <- function () {
  defaults <- list(initial.group = NULL, initial.response = NULL, initial.formul = "",initial.fac = "", initial.ranfac = "", initial.alpha = 0.05)
  dialog.values <- getDialog("meanwin", defaults)
  initializeDialog(title = gettextRcmdr("모평균 추정"))
  dataFrame <- tkframe(top)
  groupBox <- variableListBox(dataFrame, selectmode = "multiple",
                              title = gettextRcmdr("요인 (하나 이상 선택)"),
                              initialSelection = varPosn(dialog.values$initial.group, "all"))
  responseBox <- variableListBox(dataFrame, title = gettextRcmdr("Response Variable (pick one)"),
                                 initialSelection = varPosn(dialog.values$initial.response, "all"))

  onOK <- function() {
    groups <- getSelection(groupBox)
    response <- getSelection(responseBox)
    formull <- as.character(tclvalue(formul))
    facc <- as.character(tclvalue(fac))
    ranfacc <- as.character(tclvalue(ranfac))
    alp <- tclvalue(alpha)

    if (length(groups) == 0) {
      errorCondition(recall = meanwin, message = gettextRcmdr("You must select at least one factor."))
      return()}
    if (length(response) == 0) {
      errorCondition(recall = meanwin, message = gettextRcmdr("You must select at least one factor."))
      return()}
    putDialog ("meanwin", list (initial.group = groups, initial.response = response, initial.formul = formull ,initial.fac = facc, initial.ranfac = ranfacc, initial.alpha = alp))
    closeDialog()

    .activeDataSet <- ActiveDataSet()
    groups.list <- paste(paste(groups, sep = ""), collapse = ", ")
    doItAndPrint(paste(paste(rep(groups),rep('<-'),rep(.activeDataSet),rep('$'),groups, sep = ""), collapse = "
"))
    doItAndPrint(paste(paste(rep(response),rep('<-'),rep(.activeDataSet),rep('$'),response, sep = "")))

    if (ranfacc == ""){
      doItAndPrint(paste("mean.est(", response,"~", formull,",data =",.activeDataSet,",factor =",'"',facc,'"',",alpha = ",alp,")", sep = ""))
    }
    if (ranfacc != ""){
      doItAndPrint(paste("mean.est(", response,"~", formull,",data =",.activeDataSet,",factor =",'"',facc,'"',",ranfac=",'"',ranfacc,'"',",alpha = ",alp,")", sep = ""))
    }

    tkfocus(CommanderWindow())
  }

  OKCancelHelp(helpSubject = "Anova", model = TRUE, reset = "meanwin", apply = "meanwin")

  formulFrame <- tkframe(dataFrame)
  formul <- tclVar(dialog.values$initial.formul)
  formulField <- ttkentry(formulFrame, width = "20", textvariable = formul)

  facFrame <- tkframe(dataFrame)
  fac <- tclVar(dialog.values$initial.fac)
  facField <- ttkentry(facFrame, width = "20", textvariable = fac)

  ranfacFrame <- tkframe(dataFrame)
  ranfac <- tclVar(dialog.values$initial.ranfac)
  ranfacField <- ttkentry(ranfacFrame, width = "20", textvariable = ranfac)

  alphaFrame <- tkframe(dataFrame)
  alpha <- tclVar(dialog.values$initial.alpha)
  alphaField <- ttkentry(alphaFrame, width = "20", textvariable = alpha)

  tkgrid(labelRcmdr(formulFrame, text = gettextRcmdr("Formula / ex) a+b+c+a*b+b*c+c*a+a*b*c"), fg = getRcmdr("title.color"), font = "RcmdrTitleFont"), sticky = "w")
  tkgrid(labelRcmdr(facFrame, text = gettextRcmdr("모평균 추정 인자(하나 선택)"), fg = getRcmdr("title.color"), font = "RcmdrTitleFont"), sticky = "w")
  tkgrid(labelRcmdr(ranfacFrame, text = gettextRcmdr("변량인자"), fg = getRcmdr("title.color"), font = "RcmdrTitleFont"), sticky = "w")
  tkgrid(labelRcmdr(alphaFrame, text = gettextRcmdr("Alpha"), fg = getRcmdr("title.color"), font = "RcmdrTitleFont"), sticky = "w")
  tkgrid(formulField, sticky="w")
  tkgrid(facField, sticky="w")
  tkgrid(ranfacField, sticky="w")
  tkgrid(alphaField, sticky="w")
  tkgrid(formulFrame, labelRcmdr(dataFrame, text = " "), sticky = "nw")
  tkgrid(facFrame, labelRcmdr(dataFrame, text = " "), sticky = "nw")
  tkgrid(ranfacFrame, labelRcmdr(dataFrame, text = " "), sticky = "nw")
  tkgrid(alphaFrame, labelRcmdr(dataFrame, text = " "), sticky = "nw")
  tkgrid(getFrame(groupBox), labelRcmdr(dataFrame, text="  "), getFrame(responseBox), sticky = "nw")
  tkgrid(dataFrame, sticky="w")
  tkgrid(buttonsFrame, sticky = "w")
  dialogSuffix(use.tabs = FALSE, grid.buttons = TRUE)
}

disp1win <- function () {
  defaults <- list(initial.group = NULL, initial.response = NULL, initial.formul = "", initial.nest = "TRUE", initial.fac = 3)
  dialog.values <- getDialog("disp1win", defaults)
  initializeDialog(title = gettextRcmdr("분할법 분산분석"))
  dataFrame <- tkframe(top)
  groupBox <- variableListBox(dataFrame, selectmode = "multiple",
                              title = gettextRcmdr("요인 및 반복 (하나 이상 선택)"),
                              initialSelection = varPosn(dialog.values$initial.group, "all"))
  responseBox <- variableListBox(dataFrame, title = gettextRcmdr("Response Variable (pick one)"),
                                 initialSelection = varPosn(dialog.values$initial.response, "all"))

  onOK <- function() {
    groups <- getSelection(groupBox)
    response <- getSelection(responseBox)
    formull <- as.character(tclvalue(formul))
    nestt <- as.character(tclvalue(nest))
    facc <- tclvalue(fac)

    if (length(groups) == 0) {
      errorCondition(recall = disp1win, message = gettextRcmdr("You must select at least one factor."))
      return()}
    if (length(response) == 0) {
      errorCondition(recall = disp1win, message = gettextRcmdr("You must select at least one factor."))
      return()}
    putDialog ("disp1win", list (initial.group = groups, initial.response = response, initial.formul = formull,initial.nest = nestt, initial.fac = facc))
    closeDialog()

    .activeDataSet <- ActiveDataSet()
    groups.list <- paste(paste(groups, sep = ""), collapse = ", ")
    doItAndPrint(paste(paste(rep(.activeDataSet),rep('$'),rep(groups),rep('<-'),rep("as.factor("),rep(.activeDataSet),rep('$'),groups,")", sep = ""), collapse = "
"))

    doItAndPrint(paste("disperaov(", .activeDataSet,"$",response,"~", formull,",data=",.activeDataSet,",nest=",nestt, ",fac=",facc,")", sep = ""))
    tkfocus(CommanderWindow())
  }

  OKCancelHelp(helpSubject = "Anova", model = TRUE, reset = "disp1win", apply = "disp1win")

  formulFrame <- tkframe(dataFrame)
  formul <- tclVar(dialog.values$initial.formul)
  formulField <- ttkentry(formulFrame, width = "20", textvariable = formul)

  nestFrame <- tkframe(dataFrame)
  nest <- tclVar(dialog.values$initial.nest)
  nestField <- ttkentry(nestFrame, width = "20", textvariable = nest)

  facFrame <- tkframe(dataFrame)
  fac <- tclVar(dialog.values$initial.fac)
  facField <- ttkentry(facFrame, width = "20", textvariable = fac)

  tkgrid(labelRcmdr(formulFrame, text = gettextRcmdr("Formula / ex) r*a*b*c"), fg = getRcmdr("title.color"), font = "RcmdrTitleFont"), sticky = "w")
  tkgrid(labelRcmdr(nestFrame, text = gettextRcmdr('지분실험법 / TRUE : 지분실험법 / FALSE : 지분실험법이 아니다'), fg = getRcmdr("title.color"), font = "RcmdrTitleFont"), sticky = "w")
  tkgrid(labelRcmdr(facFrame, text = gettextRcmdr("선택 : 1=단일분할법 일원배치, 2=단일분할법 이원배치, 3=이단분할법"), fg = getRcmdr("title.color"), font = "RcmdrTitleFont"), sticky = "w")
  tkgrid(formulField, sticky="w")
  tkgrid(nestField, sticky="w")
  tkgrid(facField, sticky="w")
  tkgrid(formulFrame, labelRcmdr(dataFrame, text = " "), sticky = "nw")
  tkgrid(nestFrame, labelRcmdr(dataFrame, text = " "), sticky = "nw")
  tkgrid(facFrame, labelRcmdr(dataFrame, text = " "), sticky = "nw")
  tkgrid(getFrame(groupBox), labelRcmdr(dataFrame, text="  "), getFrame(responseBox), sticky = "nw")
  tkgrid(dataFrame, sticky="w")
  tkgrid(buttonsFrame, sticky = "w")
  dialogSuffix(use.tabs = FALSE, grid.buttons = TRUE)
}

optwin <- function () {
  defaults <- list(initial.group = NULL, initial.response = NULL, initial.fac = "", initial.ranfac = "", initial.alpha = 0.05)
  dialog.values <- getDialog("optwin", defaults)
  initializeDialog(title = gettextRcmdr("최적 수준 조합"))
  dataFrame <- tkframe(top)
  groupBox <- variableListBox(dataFrame, selectmode = "multiple",
                              title = gettextRcmdr("요인 (하나 이상 선택)"),
                              initialSelection = varPosn(dialog.values$initial.group, "all"))
  responseBox <- variableListBox(dataFrame, title = gettextRcmdr("Response Variable (pick one)"),
                                 initialSelection = varPosn(dialog.values$initial.response, "all"))

  onOK <- function() {
    groups <- getSelection(groupBox)
    response <- getSelection(responseBox)
    facc <- as.character(tclvalue(fac))
    ranfacc <- as.character(tclvalue(ranfac))
    alp <- tclvalue(alpha)

    if (length(groups) == 0) {
      errorCondition(recall = optwin, message = gettextRcmdr("You must select at least one factor."))
      return()}
    if (length(response) == 0) {
      errorCondition(recall = optwin, message = gettextRcmdr("You must select at least one factor."))
      return()}
    putDialog ("optwin", list (initial.group = groups, initial.response = response,initial.fac = facc, initial.ranfac = ranfacc, initial.alpha = alp))
    closeDialog()

    .activeDataSet <- ActiveDataSet()
    groups.list <- paste(paste(groups, sep = ""), collapse = ", ")
    doItAndPrint(paste(paste(rep(.activeDataSet),rep('$'),rep(groups),rep('<-'),rep("as.factor("),rep(.activeDataSet),rep('$'),groups,")", sep = ""), collapse = "
"))
    doItAndPrint(paste(response,"<-",.activeDataSet,'$',response, sep = ""))
    if (ranfacc == ""){
      doItAndPrint(paste("optcom(data=",.activeDataSet,",fac =c(",facc,"),x=",'"',response,'"',",alpha = ",alp,")", sep = ""))
    }
    if (ranfacc != ""){
      doItAndPrint(paste("optcom(data=",.activeDataSet,",fac =c(",facc,"),ranfac =",'"',ranfacc,'"',",x=",'"',response,'"',",alpha = ",alp,")", sep = ""))
    }

    tkfocus(CommanderWindow())
  }


  OKCancelHelp(helpSubject = "Anova", model = TRUE, reset = "optwin", apply = "optwin")

  facFrame <- tkframe(dataFrame)
  fac <- tclVar(dialog.values$initial.fac)
  facField <- ttkentry(facFrame, width = "20", textvariable = fac)

  ranfacFrame <- tkframe(dataFrame)
  ranfac <- tclVar(dialog.values$initial.ranfac)
  ranfacField <- ttkentry(ranfacFrame, width = "20", textvariable = ranfac)

  alphaFrame <- tkframe(dataFrame)
  alpha <- tclVar(dialog.values$initial.alpha)
  alphaField <- ttkentry(alphaFrame, width = "20", textvariable = alpha)

  tkgrid(labelRcmdr(facFrame, text = gettextRcmdr('유의한 인자 및 조합 / ex) "a","b","a*b" '), fg = getRcmdr("title.color"), font = "RcmdrTitleFont"), sticky = "w")
  tkgrid(labelRcmdr(ranfacFrame, text = gettextRcmdr("변량인자 / ex) b"), fg = getRcmdr("title.color"), font = "RcmdrTitleFont"), sticky = "w")
  tkgrid(labelRcmdr(alphaFrame, text = gettextRcmdr("Alpha"), fg = getRcmdr("title.color"), font = "RcmdrTitleFont"), sticky = "w")
  tkgrid(facField, sticky="w")
  tkgrid(ranfacField, sticky="w")
  tkgrid(alphaField, sticky="w")
  tkgrid(facFrame, labelRcmdr(dataFrame, text = " "), sticky = "nw")
  tkgrid(ranfacFrame, labelRcmdr(dataFrame, text = " "), sticky = "nw")
  tkgrid(alphaFrame, labelRcmdr(dataFrame, text = " "), sticky = "nw")
  tkgrid(getFrame(groupBox), labelRcmdr(dataFrame, text="  "), getFrame(responseBox), sticky = "nw")
  tkgrid(dataFrame, sticky="w")
  tkgrid(buttonsFrame, sticky = "w")
  dialogSuffix(use.tabs = FALSE, grid.buttons = TRUE)
}

perwin <- function () {
  defaults <- list(initial.group = NULL, initial.response = NULL)
  dialog.values <- getDialog("perwin", defaults)
  initializeDialog(title = gettextRcmdr("3D 도표"))
  mainFrame <- tkframe(top)
  UpdateModelNumber()
  modelName <- tclVar(paste("AnovaModel.", getRcmdr("modelNumber"),
                            sep = ""))
  modelFrame <- tkframe(top)
  model <- ttkentry(modelFrame, width = "20", textvariable = modelName)
  groupBox <- variableListBox(mainFrame, selectmode = "multiple",
                              title = gettextRcmdr("Factors (pick one or more)"),
                              initialSelection = varPosn(dialog.values$initial.group, "all"))
  responseBox <- variableListBox(mainFrame, title = gettextRcmdr("Response Variable (pick one)"),
                                 initialSelection = varPosn(dialog.values$initial.response, "all"))

  onOK <- function() {
    groups <- getSelection(groupBox)
    response <- getSelection(responseBox)
    modelValue <- trim.blanks(tclvalue(modelName))


    if (length(groups) == 0) {
      errorCondition(recall = perwin, message = gettextRcmdr("You must select at least one factor."))
      return()}
    if (length(response) == 0) {
      errorCondition(recall = perwin, message = gettextRcmdr("You must select at least one factor."))
      return()}
    putDialog ("perwin", list (initial.group = groups, initial.response = response))
    closeDialog()

    .activeDataSet <- ActiveDataSet()
    groups.list <- paste(paste(groups, sep = ""), collapse = ",")
    doItAndPrint(paste("rsm(",response,"~SO(",groups.list,"),data=",.activeDataSet,")", sep = ""))
    doItAndPrint(paste(modelValue, "<-rsm(",response,"~SO(",groups.list,"),data=",.activeDataSet,")", sep = ""))
    doItAndPrint(paste("par(mfrow = c(2,3)",sep=""))
    {
      groups.list <- paste(paste(groups, sep = ""), collapse = "+")
      doItAndPrint(paste("persp(", modelValue,",~",groups.list,',contours="col",col="rainbow(40)"',")", sep = ""))
    }
    tkfocus(CommanderWindow())
  }

  OKCancelHelp(helpSubject = "Anova", model = TRUE, reset = "perwin", apply = "perwin")

  tkgrid(labelRcmdr(mainFrame, text="  "), sticky = "nw")
  tkgrid(mainFrame, sticky="nw")
  tkgrid(getFrame(groupBox), labelRcmdr(mainFrame, text="  "), getFrame(responseBox), sticky = "nw")
  tkgrid(labelRcmdr(modelFrame, text = gettextRcmdr("Enter name for model: ")),
         model, sticky = "w")
  tkgrid(modelFrame, sticky = "w")
  tkgrid(buttonsFrame, sticky = "w")
  dialogSuffix(use.tabs = FALSE, grid.buttons = TRUE)
}

aliwin <- function () {
  defaults <- list(initial.rows = 16, initial.fac = 5)
  dialog.values <- getDialog("aliwin", defaults)
  initializeDialog(title = gettextRcmdr("별칭 구조"))
  dataFrame <- tkframe(top)

  onOK <- function() {
    rowss <- tclvalue(rows)
    facc <- tclvalue(fac)

    putDialog ("aliwin", list (initial.rows = rowss,initial.fac = facc))
    closeDialog()

    doItAndPrint(paste("mk<-FrF2(",rowss,',',facc,')', sep = ""))
    doItAndPrint(paste("mp<-runif(",rowss,',0,1)', sep = ""))
    doItAndPrint(paste("aliases(lm(mp~(.)^4,data=mk))", sep = ""))
    tkfocus(CommanderWindow())
  }


  OKCancelHelp(helpSubject = "aliases", model = TRUE, reset = "aliwin", apply = "aliwin")

  facFrame <- tkframe(dataFrame)
  fac <- tclVar(dialog.values$initial.fac)
  facField <- ttkentry(facFrame, width = "20", textvariable = fac)

  rowsFrame <- tkframe(dataFrame)
  rows <- tclVar(dialog.values$initial.rows)
  rowsField <- ttkentry(rowsFrame, width = "20", textvariable = rows)

  tkgrid(labelRcmdr(facFrame, text = gettextRcmdr('요인의 수'), fg = getRcmdr("title.color"), font = "RcmdrTitleFont"), sticky = "w")
  tkgrid(labelRcmdr(rowsFrame, text = gettextRcmdr("실행횟수"), fg = getRcmdr("title.color"), font = "RcmdrTitleFont"), sticky = "w")
  tkgrid(facField, sticky="w")
  tkgrid(rowsField, sticky="w")
  tkgrid(facFrame, labelRcmdr(dataFrame, text = " "), sticky = "nw")
  tkgrid(rowsFrame, labelRcmdr(dataFrame, text = " "), sticky = "nw")
  tkgrid(dataFrame, sticky="w")
  tkgrid(buttonsFrame, sticky = "w")
  dialogSuffix(use.tabs = FALSE, grid.buttons = TRUE)
}

disp2win <- function () {
  defaults <- list(initial.group = NULL, initial.response = NULL, initial.formul = "", initial.ranfac = "")
  dialog.values <- getDialog("disp2win", defaults)
  initializeDialog(title = gettextRcmdr("산포 추정"))
  dataFrame <- tkframe(top)
  groupBox <- variableListBox(dataFrame, selectmode = "multiple",
                              title = gettextRcmdr("요인 및 반복 (하나 이상 선택)"),
                              initialSelection = varPosn(dialog.values$initial.group, "all"))
  responseBox <- variableListBox(dataFrame, title = gettextRcmdr("Response Variable (pick one)"),
                                 initialSelection = varPosn(dialog.values$initial.response, "all"))

  onOK <- function() {
    groups <- getSelection(groupBox)
    response <- getSelection(responseBox)
    formull <- as.character(tclvalue(formul))
    ranfacc <- as.character(tclvalue(ranfac))

    if (length(groups) == 0) {
      errorCondition(recall = disp2win, message = gettextRcmdr("You must select at least one factor."))
      return()}
    if (length(response) == 0) {
      errorCondition(recall = disp2win, message = gettextRcmdr("You must select at least one factor."))
      return()}
    putDialog ("disp2win", list (initial.group = groups, initial.response = response, initial.formul = formull, initial.ranfac = ranfacc))
    closeDialog()

    .activeDataSet <- ActiveDataSet()
    groups.list <- paste(paste(groups, sep = ""), collapse = ", ")
    doItAndPrint(paste(paste(rep(groups),rep('<-'),rep("as.factor("),rep(.activeDataSet),rep('$'),groups,")", sep = ""), collapse = "
"))
    doItAndPrint(paste(paste(rep(.activeDataSet),rep('$'),rep(groups),rep('<-'),rep("as.factor("),rep(.activeDataSet),rep('$'),groups,")", sep = ""), collapse = "
"))
    if (ranfacc == ""){
      doItAndPrint(paste("dispersion(", .activeDataSet,"$",response,"~", formull,",data=",.activeDataSet,",ranfac=NULL",",sp=FALSE",")", sep = ""))
    }
    if (ranfacc != ""){
      doItAndPrint(paste("dispersion(", .activeDataSet,"$",response,"~", formull,",data=",.activeDataSet,",ranfac=c(",ranfacc,')',",sp=FALSE",")", sep = ""))
    }

    tkfocus(CommanderWindow())
  }

  OKCancelHelp(helpSubject = "Anova", model = TRUE, reset = "disp2win", apply = "disp2win")

  formulFrame <- tkframe(dataFrame)
  formul <- tclVar(dialog.values$initial.formul)
  formulField <- ttkentry(formulFrame, width = "20", textvariable = formul)

  ranfacFrame <- tkframe(dataFrame)
  ranfac <- tclVar(dialog.values$initial.ranfac)
  ranfacField <- ttkentry(ranfacFrame, width = "20", textvariable = ranfac)

  tkgrid(labelRcmdr(formulFrame, text = gettextRcmdr("Formula / ex) r*a*b*c"), fg = getRcmdr("title.color"), font = "RcmdrTitleFont"), sticky = "w")
  tkgrid(labelRcmdr(ranfacFrame, text = gettextRcmdr('변량인자 / ex) "b","c"'), fg = getRcmdr("title.color"), font = "RcmdrTitleFont"), sticky = "w")
  tkgrid(formulField, sticky="w")
  tkgrid(ranfacField, sticky="w")
  tkgrid(formulFrame, labelRcmdr(dataFrame, text = " "), sticky = "nw")
  tkgrid(ranfacFrame, labelRcmdr(dataFrame, text = " "), sticky = "nw")
  tkgrid(getFrame(groupBox), labelRcmdr(dataFrame, text="  "), getFrame(responseBox), sticky = "nw")
  tkgrid(dataFrame, sticky="w")
  tkgrid(buttonsFrame, sticky = "w")
  dialogSuffix(use.tabs = FALSE, grid.buttons = TRUE)
}
