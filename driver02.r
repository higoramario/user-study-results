################
# 
#  Analise dados Higor
#  27/03/2017
#

#install.packages("orddom")
library(orddom)

source('fbst.ct.R')

v = read.table('dados_20170322.txt', header=TRUE, sep='\t', stringsAsFactors=FALSE, dec=',')

v$Jag_Success = as.numeric(v$Jag_Result=="Found")
v$Ecl_Success = as.numeric(v$Ecl_Result=="Found")

# colnames(v)
# [1] "id"                "group"             "Use_Intention"    
# [4] "Use_Consideration" "Debug_Improv"      "Debug_Product"    
# [7] "Debug_Effectiv"    "Debug_Usefulln"    "Use_Learning"     
# [10] "Mental_Effort"     "Use_Ease"          "Debug_Ease"       
# [13] "Java_Level"        "IDE_Level"         "Junit_Level"      
# [16] "Prog_Exper"        "Prof_Exper"        "Java_Exper"       
# [19] "IDE_Exper"         "Junit_Exper"       "Jag_Task"         
# [22] "Jag_First"         "Jag_Time"          "Jag_Begin"        
# [25] "Jag_Middle"        "Jag_End"           "Jag_FrList"       
# [28] "Jag_FrSlide"       "Jag_FrText"        "Jag_ElemInspec"   
# [31] "Jag_ShEditor"      "Jag_ShJUnit"       "Jag_Breakp"       
# [34] "Jag_FaultyElem"    "Jag_FaultyLine"    "Jag_FaultyLineEcl"
# [37] "Jag_FaultyMeth"    "Jag_FaultyMethEcl" "Jag_DiffScores"   
# [40] "Jag_InspectOrder"  "Jag_YellowElem"    "Jag_GreenElem"    
# [43] "Jag_SuspElemFirst" "Jag_Start"         "Jag_FrJUnit"      
# [46] "Jag_FrDebugger"    "Jag_BreakElem"     "Jag_BreakFMethod" 
# [49] "Jag_BreakFLine"    "Jag_BreakAll"      "Jag_AllLines"     
# [52] "Jag_AllMethods"    "Ecl_Task"          "Ecl_Time"         
# [55] "Ecl_FrJunit"       "Ecl_FrDebugger"    "Ecl_BreakFMethod" 
# [58] "Ecl_BreakFLine"    "Ecl_BreakAll"      "Ecl_MethInspect"  
# [61] "Ecl_LineInspect"   "Ecl_AllLines"      "Ecl_AllMethods"   
# [64] "Jag_MethOrLine"    "Jag_Result"        "Ecl_Result"       
# [67] "Jag_Success"       "Ecl_Success"   

#
#  Verificacoes de controle   
#

# 1: As proporcoes dos tipos de defeito (jsoup e xtream) foram as 
# mesmas para Jaguar e Eclipse? 

tbfreq = table(v$Jag_Task) # Frequencias dos tipos de defeito no Jaguar
pvalor_def = fbst.ct(X=matrix(tbfreq, nrow=1), ttype='prop', p = c(0.5, 0.5), 
                     simulate.p.value=TRUE, B=300)$emp.p.value

# 2: Houve tendencia na ordem das tarefas com Jaguar e Eclise?

tbfreq = table(v$Jag_First) # Frequencias das ordens do Jaguar
pvalor_ord = fbst.ct(X=matrix(tbfreq, nrow=1), ttype='prop', p = c(0.5, 0.5), 
                 simulate.p.value=TRUE, B=300)$emp.p.value

# 3: As proporcoes dos tipos de defeito (jsoup e xtream) foram as mesmas 
# para metodo e linha no jaguar?

tbfreq = table(v$Jag_MethOrLine, v$Jag_Task)
pvalor_def_ML = fbst.ct(X=tbfreq, ttype='homo', 
                     simulate.p.value=TRUE, B=300)$emp.p.value

#
#  Questoes principais do estudo  
#
# Q1: comparacao das eficacias entre Jaguar e Eclipse

tbfreq = table(v$Jag_Result, v$Ecl_Result)
pvalor_efic_JE = fbst.ct(X=tbfreq, ttype='diagsym', 
                        simulate.p.value=TRUE, B=300)$emp.p.value

##effect size for binomial data is measured by risk ratio (RR)
#expected probability of success from control group
control_prob = sum(v$Ecl_Success)/length(v$Ecl_Success)
#expected probability of success from treatment group
treat_prob = sum(v$Jag_Success)/length(v$Jag_Success)

#risk ratio for success using jaguar
rr_JE = treat_prob/control_prob

#odds ratio
odd_control = sum(v$Ecl_Success)/(length(v$Ecl_Success)-sum(v$Ecl_Success))
odd_treat = sum(v$Jag_Success)/(length(v$Jag_Success)-sum(v$Jag_Success))
or_JE = odd_treat/odd_control


# Q2: comparacao das eficiencias entre Jaguar e Eclipse

timespent_jag = v$Jag_Time[which(v$Jag_Result=='Found')]
timespent_ecl = v$Ecl_Time[which(v$Ecl_Result=='Found')]
#the higher time spent value for the Eclipse tasks is wrong 366.1 -> 126.0
timespent_ecl = replace(timespent_ecl,timespent_ecl==366.1,126.0)

timespent = c(timespent_jag, timespent_ecl)
treatments = c(rep(0,length(timespent_jag)), rep(1,length(timespent_ecl)))
pvalue_efficiency = wilcox.test(timespent~treatments)

treatments_labels = replace(treatments,treatments==0,"Jaguar")
treatments_labels = replace(treatments_labels,treatments==1,"Eclipse")
boxplot(timespent ~ treatments_labels, main="Time spent to find the bugs", ylab="Time spent (min)", col="lightgray")
boxplot(timespent ~ treatments_labels, main="Time spent to find the bugs", ylab="Time spent (min)", col="lightgray", outline=FALSE)
boxplot(timespent ~ treatments_labels, ylab="Time spent (min)", col="lightgray")
boxplot(timespent ~ treatments_labels, ylab="Time spent (min)", col="lightgray", outline=FALSE)

boxplot.stats(timespent_jag, do.out = FALSE)
boxplot.stats(timespent_ecl, do.out = FALSE)

md_time_jag = median(timespent_jag)
md_time_ecl = median(timespent_ecl)

#Cliff's delta Dominance
dmes(timespent_ecl,timespent_jag)
delta_gr(timespent_ecl,timespent_jag,paired=FALSE)


# Q3: comparacao das eficacias entre Metodo e Linha
tbfreq = table(v$Jag_MethOrLine, v$Jag_Result)
pvalor_efic_JLM = fbst.ct(X=tbfreq, ttype='homo', 
                         simulate.p.value=TRUE, B=300)$emp.p.value

##effect size for binomial data is measured by risk ratio (RR)
#expected probability of success from control group
control_prob = sum(v$Ecl_Success)/length(v$Ecl_Success)
#expected probability of success from treatment group using the method list
treat_method_prob = sum(v$Jag_Success[which(v$Jag_MethOrLine=='M')])/length(v$Jag_Success[which(v$Jag_MethOrLine=='M')])
#expected probability of success from treatment group using the line list
treat_line_prob = sum(v$Jag_Success[which(v$Jag_MethOrLine=='L')])/length(v$Jag_Success[which(v$Jag_MethOrLine=='L')])

#risk ratio for success using jaguar
rr_LM = treat_method_prob/treat_line_prob
rr_ML = treat_line_prob/treat_method_prob

#odds ratio
odd_control = sum(v$Ecl_Success)/(length(v$Ecl_Success)-sum(v$Ecl_Success))
odd_treat_method = sum(v$Jag_Success[which(v$Jag_MethOrLine=='M')])/(length(v$Jag_Success[which(v$Jag_MethOrLine=='M')])-sum(v$Jag_Success[which(v$Jag_MethOrLine=='M')]))
odd_treat_line = sum(v$Jag_Success[which(v$Jag_MethOrLine=='L')])/(length(v$Jag_Success[which(v$Jag_MethOrLine=='L')])-sum(v$Jag_Success[which(v$Jag_MethOrLine=='L')]))
or_LM = odd_treat_line/odd_treat_method
or_ML = odd_treat_method/odd_treat_line


# Q3.1: effectiveness Jaguar Line x Eclipse
tbfreq_jl_ecl = table(v$Jag_Result[which(v$Jag_MethOrLine=='L')], v$Ecl_Result[which(v$Jag_MethOrLine=='L')])
pvalor_efic_jl_ecl = fbst.ct(X=tbfreq_jl_ecl, ttype='diagsym', simulate.p.value=TRUE, B=300)$emp.p.value

##effect size
rr_LE = treat_line_prob/control_prob
or_LE = odd_treat_line/odd_control

# Q3.2: effectiveness Jaguar Method x Eclipse
tbfreq_jm_ecl = table(v$Jag_Result[which(v$Jag_MethOrLine=='M')], v$Ecl_Result[which(v$Jag_MethOrLine=='M')])
pvalor_efic_jm_ecl = fbst.ct(X=tbfreq_jm_ecl, ttype='diagsym', simulate.p.value=TRUE, B=300)$emp.p.value

##effect size
rr_ME = treat_method_prob/control_prob
or_ME = odd_treat_method/odd_control


# Q4: comparacao das eficiencias entre Jaguar usando linhas e metodos
timespent_jl = v$Jag_Time[which(v$Jag_Result=='Found' & v$Jag_MethOrLine=='L')]
timespent_jm = v$Jag_Time[which(v$Jag_Result=='Found' & v$Jag_MethOrLine=='M')]
timespent_ecl_l = v$Ecl_Time[which(v$Ecl_Result=='Found'& v$Jag_MethOrLine=='L')]
#the higher time spent value for the Eclipse tasks is wrong 366.1 -> 126.0
timespent_ecl_l = replace(timespent_ecl_l,timespent_ecl_l==366.1,126.0)
timespent_ecl_m = v$Ecl_Time[which(v$Ecl_Result=='Found'& v$Jag_MethOrLine=='M')]

timespent_lm = c(timespent_jl, timespent_jm)
treatments_lm = c(rep(0,length(timespent_jl)), rep(1,length(timespent_jm)))
pvalue_efficiency_lm = wilcox.test(timespent_lm~treatments_lm)

treatments_lm_labels = replace(treatments_lm,treatments_lm==0,"Jaguar Line")
treatments_lm_labels = replace(treatments_lm_labels,treatments_lm_labels==1,"Jaguar Method")
boxplot(timespent_lm ~ treatments_lm_labels, main="Time spent to find the bugs", ylab="Time spent (min)", col="lightgray")
boxplot(timespent_lm ~ treatments_lm_labels, main="Time spent to find the bugs", ylab="Time spent (min)", col="lightgray", outline=FALSE)

#Cliff's delta Dominance
dmes(timespent_jm,timespent_jl)
delta_gr(timespent_jm,timespent_jl,paired=FALSE)


#Q4.1 Efficiency Jaguar Line x Eclipse
timespent_le = c(timespent_jl, timespent_ecl_l)
treatments_le = c(rep(0,length(timespent_jl)), rep(1,length(timespent_ecl_l)))
pvalue_efficiency_le = wilcox.test(timespent_le~treatments_le)

treatments_le_labels = replace(treatments_le,treatments_le==0,"Jaguar Line")
treatments_le_labels = replace(treatments_le_labels,treatments_le_labels==1,"Eclipse")
boxplot(timespent_le ~ treatments_le_labels, main="Time spent to find the bugs", ylab="Time spent (min)", col="lightgray")
boxplot(timespent_le ~ treatments_le_labels, main="Time spent to find the bugs", ylab="Time spent (min)", col="lightgray", outline=FALSE)

#Cliff's delta Dominance
dmes(timespent_jl,timespent_ecl_l)
delta_gr(timespent_jl,timespent_ecl_l,paired=FALSE)


#Q4.2 Efficiency Jaguar Method x Eclipse
timespent_me = c(timespent_jm, timespent_ecl_m)
treatments_me = c(rep(0,length(timespent_jm)), rep(1,length(timespent_ecl_m)))
pvalue_efficiency_me = wilcox.test(timespent_me~treatments_me)

treatments_me_labels = replace(treatments_me,treatments_me==0,"Jaguar Method")
treatments_me_labels = replace(treatments_me_labels,treatments_me_labels==1,"Eclipse")
boxplot(timespent_me ~ treatments_me_labels, main="Time spent to find the bugs", ylab="Time spent (min)", col="lightgray")
boxplot(timespent_me ~ treatments_me_labels, main="Time spent to find the bugs", ylab="Time spent (min)", col="lightgray", outline=FALSE)

#Cliff's delta Dominance
dmes(timespent_jm,timespent_ecl_m)
delta_gr(timespent_jm,timespent_ecl_m,paired=FALSE)


#Q: Na Jaguar, os anos de experiencia afetam a eficacia?

#Q5 Java
model_exp_java = glm(Jag_Success~Java_Exper, data=v, family=binomial)
summary(model_exp_java)
#plot(model_exp_java)

#Q6 IDE
model_exp_ide = glm(Jag_Success~IDE_Exper, data=v, family=binomial)
summary(model_exp_ide)

#Q7 JUnit
model_exp_junit = glm(Jag_Success~Junit_Exper, data=v, family=binomial)
summary(model_exp_junit)

#Q8 Programacao
model_exp_prog = glm(Jag_Success~Prog_Exper, data=v, family=binomial)
summary(model_exp_prog)

#Q9 Profissional
model_exp_prof = glm(Jag_Success~Prof_Exper, data=v, family=binomial)
summary(model_exp_prof)

#Q: Na Jaguar, o nivel de experiencia afeta a eficacia?

#Q10 Java
model_lev_java = glm(Jag_Success~Java_Level, data=v, family=binomial)
summary(model_lev_java)

#Q11 IDE
model_lev_ide = glm(Jag_Success~IDE_Level, data=v, family=binomial)
summary(model_lev_ide)

#Q12 JUnit
model_lev_junit = glm(Jag_Success~Junit_Level, data=v, family=binomial)
summary(model_lev_junit)


#Q: No Eclipse, os anos de experiencia afetam a eficacia?

#Q13 Java
model_exp_java_ecl = glm(Ecl_Success~Java_Exper, data=v, family=binomial)
summary(model_exp_java_ecl)

#Q14 IDE
model_exp_ide_ecl = glm(Ecl_Success~IDE_Exper, data=v, family=binomial)
summary(model_exp_ide_ecl)

#Q15 JUnit
model_exp_junit_ecl = glm(Ecl_Success~Junit_Exper, data=v, family=binomial)
summary(model_exp_junit_ecl)

#Q16 Programacao
model_exp_prog_ecl = glm(Ecl_Success~Prog_Exper, data=v, family=binomial)
summary(model_exp_prog_ecl)

#Q17 Profissional
model_exp_prof_ecl = glm(Ecl_Success~Prof_Exper, data=v, family=binomial)
summary(model_exp_prof_ecl)

#Q: No Eclipse, o nivel de experiencia afeta a eficacia?

#Q18 Java
model_lev_java_ecl = glm(Ecl_Success~Java_Level, data=v, family=binomial)
summary(model_lev_java_ecl)

#Q19 IDE
model_lev_ide_ecl = glm(Ecl_Success~IDE_Level, data=v, family=binomial)
summary(model_lev_ide_ecl)

#Q20 JUnit
model_lev_junit_ecl = glm(Ecl_Success~Junit_Level, data=v, family=binomial)
summary(model_lev_junit_ecl)



#Q21: Uso de breakpoints afeta a eficacia de localizacao no uso da Jaguar?

model_breakall = glm(Jag_Success~Jag_BreakAll, data=v, family=binomial)
summary(model_breakall)

tbfreq = table(as.numeric(v$Jag_BreakAll>0), v$Jag_Result)
pvalor_def_ML = fbst.ct(X=tbfreq, ttype='homo', 
                        simulate.p.value=TRUE, B=300)$emp.p.value


####################################################################
# Control factors assessment
#
# Regressao logistica para identificar associacao entre os 
# sucessos/fracassos em encontrar os erros e se as taxas de 
# cliques nas linhas defeituosas
#

DebugResult = c(v$Jag_Success, v$Ecl_Success)
ClickRateLine = c(logit_ecr_jag_line,logit_ecr_ecl_line)
#ClickRateLine = c(ecr_jag_line,ecr_ecl_line)
id_xord_line = order(ClickRateLine)
ClickRateLine = ClickRateLine[id_xord_line]
DebugResult = DebugResult[id_xord_line]

glm_effect_line = glm(DebugResult~ClickRateLine, family=binomial)
summary(glm_effect_line)

eps = 0.1
ClickSimLine = seq(from=min(ClickRateLine), to=max(ClickRateLine), length.out=200)
SuccessProbLine = predict(glm_effect_line,newdata=data.frame(ClickRateLine=ClickSimLine),type='response')
#plot(ClickRateLine+runif(length(ClickRateLine))*2*eps-eps,DebugResult, pch=20, main="ECR x Effectiveness - faulty line",xlab="ECR (logit scale)",ylab="Effectiveness", axes = FALSE)
plot(ClickRateLine, DebugResult+(-1)^DebugResult*runif(length(ClickRateLine))*eps, pch=20, main="ECR x Effectiveness - faulty line",xlab="ECR (logit scale)",ylab="Effectiveness", axes = FALSE) #y variation
lines(ClickSimLine, SuccessProbLine, col='red', lwd=2)
axis(1)
axis(side=2,at=c(0,1),by=1)
box()

thres_crline = median(ClickRateLine)
bin_crline = as.numeric(ClickRateLine>thres_crline)

ft_crline = table(bin_crline,DebugResult)
pvalor_crline = fbst.ct(X=ft_crline, ttype='homo', 
                         simulate.p.value=TRUE, B=300)$emp.p.value

# Regressao logistica para identificar associacao entre os 
# sucessos/fracassos em encontrar os erros e se as taxas de 
# cliques nos metodos defeituosos
#

ClickRateMethod = c(logit_ecr_jag_method,logit_ecr_ecl_method)
#ClickRateMethod = c(ecr_jag_method,ecr_ecl_method)
id_xord_method = order(ClickRateMethod)
ClickRateMethod = ClickRateMethod[id_xord_method]
DebugResult = DebugResult[id_xord_method]

glm_effect_method = glm(DebugResult~ClickRateMethod, family=binomial)
summary(glm_effect_method)

eps = 0.1
ClickSimMethod = seq(from=min(ClickRateMethod), to=max(ClickRateMethod), length.out=200)
SuccessProbMethod = predict(glm_effect_method,newdata=data.frame(ClickRateMethod=ClickSimMethod),type='response')
#plot(ClickRateMethod+runif(length(ClickRateMethod))*2*eps-eps,DebugResult, pch=20, main="ECR x Effectiveness - faulty method",xlab="ECR (logit scale)",ylab="Effectiveness", axes = FALSE)
plot(ClickRateMethod, DebugResult+(-1)^DebugResult*runif(length(ClickRateMethod))*eps, pch=20, main="ECR x Effectiveness - faulty method",xlab="ECR (logit scale)",ylab="Effectiveness", axes = FALSE) #y variation
lines(ClickSimMethod, SuccessProbMethod, col='red', lwd=2)
axis(1)
axis(side=2,at=c(0,1),by=1)
box()


thres_crmethod = median(ClickRateMethod)
bin_crmethod = as.numeric(ClickRateMethod>thres_crmethod)

ft_crmethod = table(bin_crmethod,DebugResult)
pvalor_crmethod = fbst.ct(X=ft_crmethod, ttype='homo', 
                        simulate.p.value=TRUE, B=300)$emp.p.value

#
# Regressao logistica para identificar associacao entre os 
# sucessos/fracassos em encontrar os erros e os tempos dedicados aos experimentos 
#

DebugResultTime = c(v$Jag_Success, v$Ecl_Success)
TimeSpent = c(v$Jag_Time, v$Ecl_Time)
id_xord_time = order(TimeSpent,decreasing=TRUE)
TimeSpent = TimeSpent[id_xord_time]
DebugResultTime = DebugResultTime[id_xord_time]

glm_time = glm(DebugResultTime~TimeSpent, family=binomial)
summary(glm_time)

eps = 0.1
TimeSim = seq(from=min(TimeSpent), to=max(TimeSpent), length.out=200)
SuccessProbTime = predict(glm_time,newdata=data.frame(TimeSpent=TimeSim),type='response')
#plot(TimeSim+runif(length(TimeSim))*2*eps-eps,DebugResultTime, pch=20) #simulated values
#plot(TimeSpent, DebugResultTime+(-1)^DebugResultTime*runif(length(TimeSpent))*eps, pch=20, main="Efficiency (Time) x Effectiveness",xlab="Time (min)",ylab="Effectiveness", axes = FALSE) #y variation
plot(TimeSpent, DebugResultTime+(-1)^DebugResultTime*runif(length(TimeSpent))*eps, xlim=rev(range(TimeSpent)), pch=20, main="Efficiency (Time) x Effectiveness",xlab="Time (min)",ylab="Effectiveness", axes = FALSE) #y variation, reverse time
lines(TimeSim, SuccessProbTime, col='red', lwd=2)
axis(1)
axis(side=2,at=c(0,1),by=1)
box()


thres_time = median(TimeSpent)
bin_time = as.numeric(TimeSpent>thres_time)

ft_time = table(bin_time,DebugResultTime)
pvalor_time = fbst.ct(X=ft_time, ttype='homo', 
                         simulate.p.value=TRUE, B=300)$emp.p.value


#fixing the higher value of the time array, which is wrong 366.1 -> 126.0
TimeSpentFix = TimeSpent
TimeSpentFixedOutlier = replace(TimeSpentFix,TimeSpentFix==366.1,126.0)

glm_time_fixed_outlier = glm(DebugResultTime~TimeSpentFixedOutlier, family=binomial)
summary(glm_time_fixed_outlier)

TimeSimFixedOutlier = seq(from=min(TimeSpentFixedOutlier), to=max(TimeSpentFixedOutlier), length.out=200)
SuccessProbTimeFixedOutlier = predict(glm_time_fixed_outlier,newdata=data.frame(TimeSpentFixedOutlier=TimeSimFixedOutlier),type='response')
plot(TimeSpentFixedOutlier, DebugResultTime+(-1)^DebugResultTime*runif(length(TimeSpentFixedOutlier))*eps, pch=20, main="Efficiency (Time) x Effectiveness",xlab="Time (min)",ylab="Effectiveness", axes = FALSE) #y variation, without the outlier
plot(TimeSpentFixedOutlier, DebugResultTime+(-1)^DebugResultTime*runif(length(TimeSpentFixedOutlier))*eps, xlim=rev(range(TimeSpentFixedOutlier)), pch=20, main="Efficiency (Time) x Effectiveness",xlab="Time (min)",ylab="Effectiveness", axes = FALSE) #y variation, reverse time, without the outlier
lines(TimeSimFixedOutlier, SuccessProbTimeFixedOutlier, col='red', lwd=2)
axis(1)
axis(side=2,at=c(0,1),by=1)
box()


###Control factors assessment - At least one click

###Relationship between Effectiveness and click on faulty line

DebugResult = c(v$Jag_Success, v$Ecl_Success)
FaultyLineClickJaguar = replace(v$Jag_FaultyLine,v$Jag_FaultyLine>0,1)
FaultyLineClickEclipse = replace(v$Jag_FaultyLineEcl,v$Jag_FaultyLineEcl>0,1)
FaultyLineClick = c(FaultyLineClickJaguar,FaultyLineClickEclipse)

ft_click_faulty_line = table(FaultyLineClick, DebugResult)
pvalue_fline_effect = fbst.ct(X=ft_click_faulty_line, ttype='prop', simulate.p.value=TRUE, B=300)$emp.p.value


###Relationship between Effectiveness and click on faulty method

DebugResult = c(v$Jag_Success, v$Ecl_Success)
FaultyMethodClickJaguar = replace(v$Jag_FaultyMeth,v$Jag_FaultyMeth>0,1)
FaultyMethodClickEclipse = replace(v$Jag_FaultyMethEcl,v$Jag_FaultyMethEcl>0,1)
FaultyMethodClick = c(FaultyMethodClickJaguar,FaultyMethodClickEclipse)

ft_click_faulty_method = table(FaultyMethodClick, DebugResult)
pvalue_fmethod_effect = fbst.ct(X=ft_click_faulty_method, ttype='prop', simulate.p.value=TRUE, B=300)$emp.p.value



#Q22: Did the use of Jaguar lead the participants to inspect the faulty line more than using Eclipse?

# Analysis of the Effective Click Rate (ECR)
# ECR: #clicks on the faulty line / #all clicks

ecr_jag_line = v$Jag_FaultyLine/(v$Jag_AllLines+1e-3) # effective click rate on the faulty line using jaguar
hist(ecr_jag_line,main="Jaguar - ECR for faulty line",xlab="ECR",ylab="Participants",col="gray")
ecr_jag_line = ecr_jag_line/max(ecr_jag_line)
hist(ecr_jag_line,main="Jaguar - ECR for faulty line (prop)",xlab="ECR",ylab="Participants",col="gray")
logit_ecr_jag_line = log((ecr_jag_line+1e-3)/(1-ecr_jag_line+1e-3))
hist(logit_ecr_jag_line,main="Jaguar - ECR for faulty line (logit)",xlab="ECR",ylab="Participants",col="gray",xlim=c(-7,7))

ecr_ecl_line = v$Jag_FaultyLineEcl/(v$Ecl_AllLines+1e-3) # effective click rate on the faulty line using eclipse
hist(ecr_ecl_line,main="Eclipse - ECR for faulty line",xlab="ECR",ylab="Participants",col="gray")
ecr_ecl_line = ecr_ecl_line/max(ecr_ecl_line)
hist(ecr_ecl_line,main="Eclipse - ECR for faulty line (prop)",xlab="ECR",ylab="Participants",col="gray")
logit_ecr_ecl_line = log((ecr_ecl_line+1e-3)/(1-ecr_ecl_line+1e-3))
hist(logit_ecr_ecl_line,main="Eclipse - ECR for faulty line (logit)",xlab="ECR",ylab="Participants",col="gray",xlim=c(-7,7))

#plotting both histograms
hist_ecr_line = hist(c(logit_ecr_jag_line, logit_ecr_ecl_line))
hist_ecr_line_jag = hist(logit_ecr_jag_line, breaks=hist_ecr_line$breaks)
hist_ecr_line_ecl = hist(logit_ecr_ecl_line, breaks=hist_ecr_line$breaks)
freq_ecr_line = cbind(hist_ecr_line_jag$counts, hist_ecr_line_ecl$counts)
barplot(t(freq_ecr_line), names.arg=hist_ecr_line$mids, legend.text=c('jaguar', 'eclipse'), beside=TRUE, main="Jaguar x Eclipse - ECR for faulty line (logit)",xlab="ECR",ylab="Participants",ylim=c(0,20))

#statistical tests
wilcox_ecr_line = wilcox.test(logit_ecr_jag_line,logit_ecr_ecl_line,paired=TRUE)

#effect size - strength of association
participants=26
zscore_ecr_line = qnorm(wilcox_ecr_line$p.value/2)
effect_size_ecr_line = zscore_ecr_line / sqrt(participants*2)

#Cliff's delta Dominance
dmes(ecr_ecl_line,ecr_jag_line)
delta_gr(ecr_ecl_line,ecr_jag_line,paired=TRUE)

#More clicks in the faulty line than the median
#we are using the median of ECR as threshold
thres_line = median(c(logit_ecr_jag_line, logit_ecr_ecl_line))
x_jag_line = as.numeric(logit_ecr_jag_line>thres_line)
y_ecl_line = as.numeric(logit_ecr_ecl_line>thres_line)

ft_ecr_line = table(x_jag_line, y_ecl_line)
pvalor_ecr_line = fbst.ct(X=ft_ecr_line, ttype='diagsym', simulate.p.value=TRUE, B=300)$emp.p.value

#effect size
#risk ratio
control_prob_ecr_line = sum(y_ecl_line)/length(y_ecl_line)
treat_prob_ecr_line = sum(x_jag_line)/length(x_jag_line)
rr_ecr_line = treat_prob_ecr_line/control_prob_ecr_line

#odds ratio
odd_control_ecr_line = sum(y_ecl_line)/(length(y_ecl_line)-sum(y_ecl_line))
odd_treat_ecr_line = sum(x_jag_line)/(length(x_jag_line)-sum(x_jag_line))
or_ecr_line = odd_treat_ecr_line/odd_control_ecr_line



####At least one click in the faulty line
ft_oneclick_line = table(FaultyLineClickJaguar,FaultyLineClickEclipse)
pvalue_oneclick_line = fbst.ct(X=ft_oneclick_line, ttype='diagsym', simulate.p.value=TRUE, B=300)$emp.p.value

#effect size
#risk ratio
control_prob_oneclick_line = sum(FaultyLineClickEclipse)/length(FaultyLineClickEclipse)
treat_prob_oneclick_line = sum(FaultyLineClickJaguar)/length(FaultyLineClickJaguar)
rr_oneclick_line = treat_prob_oneclick_line/control_prob_oneclick_line

#odds ratio
odd_control_oneclick_line = sum(FaultyLineClickEclipse)/(length(FaultyLineClickEclipse)-sum(FaultyLineClickEclipse))
odd_treat_oneclick_line = sum(FaultyLineClickJaguar)/(length(FaultyLineClickJaguar)-sum(FaultyLineClickJaguar))
or_oneclick_line = odd_treat_oneclick_line/odd_control_oneclick_line



#Q23: Did the use of Jaguar lead the participants to inspect the faulty method more than using Eclipse?

# Analysis of the Effective Click Rate (ECR)
# ECR: #clicks on the faulty method / #all clicks

ecr_jag_method = v$Jag_FaultyMeth/(v$Jag_AllMethods+1e-3) # effective click rate on the faulty method using jaguar
hist(ecr_jag_method,main="Jaguar - ECR for faulty method",xlab="ECR",ylab="Participants",col="gray")
ecr_jag_method = ecr_jag_method/max(ecr_jag_method)
hist(ecr_jag_method,main="Jaguar - ECR for faulty method (prop)",xlab="ECR",ylab="Participants",col="gray")
logit_ecr_jag_method = log((ecr_jag_method+1e-3)/(1-ecr_jag_method+1e-3))
hist(logit_ecr_jag_method,main="Jaguar - ECR for faulty method (logit)",xlab="ECR",ylab="Participants",col="gray",xlim=c(-7,7))

ecr_ecl_method = v$Jag_FaultyMethEcl/(v$Ecl_AllMethods+1e-3)# effective click rate on the faulty method using eclipse
hist(ecr_ecl_method,main="Eclipse - ECR for faulty method",xlab="ECR",ylab="Participants",col="gray")
ecr_ecl_method = ecr_ecl_method/max(ecr_ecl_method)  # ecr_ecl_method = taxa no eclipse
hist(ecr_ecl_method,main="Eclipse - ECR for faulty method (prop)",xlab="ECR",ylab="Participants",col="gray")
logit_ecr_ecl_method = log((ecr_ecl_method+1e-3)/(1-ecr_ecl_method+1e-3))
hist(logit_ecr_ecl_method,main="Eclipse - ECR for faulty method (logit)",xlab="ECR",ylab="Participants",col="gray",xlim=c(-7,7))

hist_ecr_method = hist(c(logit_ecr_jag_method, logit_ecr_ecl_method))
hist_ecr_method_jag = hist(logit_ecr_jag_method, breaks=hist_ecr_method$breaks)
hist_ecr_method_ecl = hist(logit_ecr_ecl_method, breaks=hist_ecr_method$breaks)
freq_ecr_method = cbind(hist_ecr_method_jag$counts, hist_ecr_method_ecl$counts)
barplot(t(freq_ecr_method), names.arg=hist_ecr_method$mids, legend.text=c('jaguar', 'eclipse'), beside=TRUE, main="Jaguar x Eclipse - ECR for faulty method (logit)",xlab="ECR",ylab="Participants",ylim=c(0,10))

wilcox_ecr_method = wilcox.test(logit_ecr_jag_method,logit_ecr_ecl_method,paired=TRUE)

#effect size
participants=26
zscore_ecr_method = qnorm(wilcox_ecr_method$p.value/2)
effect_size_ecr_method = zscore_ecr_method / sqrt(participants*2)

#Cliff's delta Dominance
dmes(ecr_ecl_method,ecr_jag_method)
delta_gr(ecr_ecl_method,ecr_jag_method,paired=TRUE)


#More clicks in the faulty line than the median
#we are using the median of ECR as threshold
thres_method = median(c(logit_ecr_jag_method, logit_ecr_ecl_method))
x_jag_method = as.numeric(logit_ecr_jag_method>thres_method)
y_ecl_method = as.numeric(logit_ecr_ecl_method>thres_method)

ft_ecr_method = table(x_jag_method, y_ecl_method)
pvalor_ecr_method = fbst.ct(X=ft_ecr_method, ttype='diagsym', simulate.p.value=TRUE, B=300)$emp.p.value

#effect size
#risk ratio
control_prob_ecr_method = sum(y_ecl_method)/length(y_ecl_method)
treat_prob_ecr_method = sum(x_jag_method)/length(x_jag_method)
rr_ecr_method = treat_prob_ecr_method/control_prob_ecr_method

#odds ratio
odd_control_ecr_method = sum(y_ecl_method)/(length(y_ecl_method)-sum(y_ecl_method))
odd_treat_ecr_method = sum(x_jag_method)/(length(x_jag_method)-sum(x_jag_method))
or_ecr_method = odd_treat_ecr_method/odd_control_ecr_method

####At least one click in the faulty method
ft_oneclick_method = table(FaultyMethodClickJaguar,FaultyMethodClickEclipse)
pvalue_oneclick_method = fbst.ct(X=ft_oneclick_method, ttype='diagsym', simulate.p.value=TRUE, B=300)$emp.p.value

#effect size
#risk ratio
control_prob_oneclick_method = sum(FaultyMethodClickEclipse)/length(FaultyMethodClickEclipse)
treat_prob_oneclick_method = sum(FaultyMethodClickJaguar)/length(FaultyMethodClickJaguar)
rr_oneclick_method = treat_prob_oneclick_method/control_prob_oneclick_method

#odds ratio
odd_control_oneclick_method = sum(FaultyMethodClickEclipse)/(length(FaultyMethodClickEclipse)-sum(FaultyMethodClickEclipse))
odd_treat_oneclick_method = sum(FaultyMethodClickJaguar)/(length(FaultyMethodClickJaguar)-sum(FaultyMethodClickJaguar))
or_oneclick_method = odd_treat_oneclick_method/odd_control_oneclick_method


#Q24: Did the use of Jaguar Line lead the participants to inspect the faulty line more than using Jaguar Method?

# Analysis of the Effective Click Rate (ECR)
# ECR: #clicks on the faulty line / #all clicks

ecr_jl_line = v$Jag_FaultyLine[which(v$Jag_MethOrLine=='L')]/(v$Jag_AllLines[which(v$Jag_MethOrLine=='L')]+1e-3)
hist(ecr_jl_line)
ecr_jl_line = ecr_jl_line/max(ecr_jl_line)
hist(ecr_jl_line)
logit_ecr_jl_line = log((ecr_jl_line+1e-3)/(1-ecr_jl_line+1e-3))


ecr_jm_line = v$Jag_FaultyLine[which(v$Jag_MethOrLine=='M')]/(v$Jag_AllLines[which(v$Jag_MethOrLine=='M')]+1e-3)
hist(ecr_jm_line)
ecr_jm_line = ecr_jm_line/max(ecr_jm_line)
hist(ecr_jm_line)
logit_ecr_jm_line = log((ecr_jm_line+1e-3)/(1-ecr_jm_line+1e-3))

#plotting both histograms
hist_ecr_line_lm = hist(c(logit_ecr_jl_line, logit_ecr_jm_line))
hist_ecr_line_jl = hist(logit_ecr_jl_line, breaks=hist_ecr_line_lm$breaks)
hist_ecr_line_jm = hist(logit_ecr_jm_line, breaks=hist_ecr_line_lm$breaks)
freq_ecr_line_lm = cbind(hist_ecr_line_jl$counts, hist_ecr_line_jm$counts)
barplot(t(freq_ecr_line_lm), names.arg=hist_ecr_line_lm$mids, legend.text=c('jaguarline', 'jaguarmethod'), beside=TRUE)

#statistical tests
wilcox_ecr_line_lm = wilcox.test(logit_ecr_jl_line,logit_ecr_jm_line,paired=FALSE)

#effect size
participants_lm = 14+12
zscore_ecr_line_lm = qnorm(wilcox_ecr_line_lm$p.value/2)
effect_size_ecr_line_lm = zscore_ecr_line_lm / sqrt(participants_lm)

#Cliff's delta Dominance
dmes(ecr_jm_line,ecr_jl_line)
delta_gr(ecr_jm_line,ecr_jl_line,paired=FALSE)


#More clicks in the faulty line than the median
#line and click dont have the same size

####At least one click in the faulty line
#line and click dont have the same size


#Q25: Did the use of Jaguar Line lead the participants to inspect the faulty method more than using Jaguar Method?

# Analysis of the Effective Click Rate (ECR)
# ECR: #clicks on the faulty method / #all clicks

ecr_jl_method = v$Jag_FaultyMeth[which(v$Jag_MethOrLine=='L')]/(v$Jag_AllMethods[which(v$Jag_MethOrLine=='L')]+1e-3)
hist(ecr_jl_method)
ecr_jl_method = ecr_jl_method/max(ecr_jl_method)
hist(ecr_jl_method)
logit_ecr_jl_method = log((ecr_jl_method+1e-3)/(1-ecr_jl_method+1e-3))

ecr_jm_method = v$Jag_FaultyMeth[which(v$Jag_MethOrLine=='M')]/(v$Jag_AllMethods[which(v$Jag_MethOrLine=='M')]+1e-3)
hist(ecr_jm_method)
ecr_jm_method = ecr_jm_method/max(ecr_jm_method)
hist(ecr_jm_method)
logit_ecr_jm_method = log((ecr_jm_method+1e-3)/(1-ecr_jm_method+1e-3))

#plotting both histograms
hist_ecr_method_lm = hist(c(logit_ecr_jl_method, logit_ecr_jm_method))
hist_ecr_method_jl = hist(logit_ecr_jl_method, breaks=hist_ecr_method_lm$breaks)
hist_ecr_method_jm = hist(logit_ecr_jm_method, breaks=hist_ecr_method_lm$breaks)
freq_ecr_method_lm = cbind(hist_ecr_method_jl$counts, hist_ecr_method_jm$counts)
barplot(t(freq_ecr_method_lm), names.arg=hist_ecr_method_lm$mids, legend.text=c('jaguarline', 'jaguarmethod'), beside=TRUE)

#statistical tests
wilcox_ecr_method_lm = wilcox.test(logit_ecr_jl_method,logit_ecr_jm_method,paired=FALSE)
wilcox.test(logit_ecr_jm_method,logit_ecr_jl_method,paired=FALSE)

#effect size
zscore_ecr_method_lm = qnorm(wilcox_ecr_method_lm$p.value/2)
effect_size_ecr_method_lm = zscore_ecr_method_lm / sqrt(participants_lm)

#Cliff's delta Dominance
dmes(ecr_jm_method,ecr_jl_method)
delta_gr(ecr_jm_method,ecr_jl_method,paired=FALSE)

#More clicks in the faulty line than the median
#line and click dont have the same size

####At least one click in the faulty line
#line and click dont have the same size


#Q26: Did the use of Jaguar Line lead the participants to inspect the faulty line more than using Eclipse?

# Analysis of the Effective Click Rate (ECR)
# ECR: #clicks on the faulty line / #all clicks

ecr_jll = v$Jag_FaultyLine[which(v$Jag_MethOrLine=='L')]/(v$Jag_AllLines[which(v$Jag_MethOrLine=='L')]+1e-3)
hist(ecr_jll)
ecr_jll = ecr_jll/max(ecr_jll)  # ecr_jll = taxa no jaguar line
hist(ecr_jll)
logit_ecr_jll = log((ecr_jll+1e-3)/(1-ecr_jll+1e-3))

ecr_ell = v$Jag_FaultyLineEcl[which(v$Jag_MethOrLine=='L')]/(v$Ecl_AllLines[which(v$Jag_MethOrLine=='L')]+1e-3)
hist(ecr_ell)
ecr_ell = ecr_ell/max(ecr_ell)  # ecr_ell = taxa no eclipse
hist(ecr_ell)
logit_ecr_ell = log((ecr_ell+1e-3)/(1-ecr_ell+1e-3))

#plotting both histograms
hist_ecr_jlel = hist(c(logit_ecr_jll, logit_ecr_ell))
hist_ecr_jll = hist(logit_ecr_jll, breaks=hist_ecr_jlel$breaks)
hist_ecr_ell = hist(logit_ecr_ell, breaks=hist_ecr_jlel$breaks)
freq_ecr_jlel = cbind(hist_ecr_jll$counts, hist_ecr_ell$counts)
barplot(t(freq_ecr_jlel), names.arg=hist_ecr_jlel$mids, legend.text=c('jaguarline', 'eclipse'), beside=TRUE)

#statistical tests
wilcox_ecr_jlel = wilcox.test(logit_ecr_jll,logit_ecr_ell,paired=TRUE)

#effect size
participants_jl = 12
zscore_ecr_jlel = qnorm(wilcox_ecr_jlel$p.value/2)
effect_size_ecr_jlel = zscore_ecr_jlel / sqrt(participants_jl*2)

#Cliff's delta Dominance
dmes(ecr_ell,ecr_jll)
delta_gr(ecr_ell,ecr_jll,paired=TRUE)

#More clicks in the faulty line than the median
#we are using the median of ECR as threshold
thres_jlel = median(c(logit_ecr_jll, logit_ecr_ell))
x_jag_jlel = as.numeric(logit_ecr_jll>thres_jlel)
y_ecl_jlel = as.numeric(logit_ecr_ell>thres_jlel)

ft_ecr_jlel = table(x_jag_jlel, y_ecl_jlel)
pvalor_ecr_jlel = fbst.ct(X=ft_ecr_jlel, ttype='diagsym', simulate.p.value=TRUE, B=300)$emp.p.value


#effect size
#risk ratio
control_prob_ecr_jlel = sum(y_ecl_jlel)/length(y_ecl_jlel)
treat_prob_ecr_jlel = sum(x_jag_jlel)/length(x_jag_jlel)
rr_ecr_jlel = treat_prob_ecr_jlel/control_prob_ecr_jlel

#odds ratio
odd_control_ecr_jlel = sum(y_ecl_jlel)/(length(y_ecl_jlel)-sum(y_ecl_jlel))
odd_treat_ecr_jlel = sum(x_jag_jlel)/(length(x_jag_jlel)-sum(x_jag_jlel))
or_ecr_jlel = odd_treat_ecr_jlel/odd_control_ecr_jlel


####At least one click in the faulty line
FaultyLineClickJL = replace(v$Jag_FaultyLine[which(v$Jag_MethOrLine=='L')],v$Jag_FaultyLine[which(v$Jag_MethOrLine=='L')]>0,1)
FaultyLineClickEclipseJL = replace(v$Jag_FaultyLineEcl[which(v$Jag_MethOrLine=='L')],v$Jag_FaultyLineEcl[which(v$Jag_MethOrLine=='L')]>0,1)

ft_oneclick_jlel = table(FaultyLineClickJL,FaultyLineClickEclipseJL)
pvalue_oneclick_jlel = fbst.ct(X=ft_oneclick_jlel, ttype='diagsym', simulate.p.value=TRUE, B=300)$emp.p.value

#effect size
#risk ratio
control_prob_oneclick_jlel = sum(FaultyLineClickEclipseJL)/length(FaultyLineClickEclipseJL)
treat_prob_oneclick_jlel = sum(FaultyLineClickJL)/length(FaultyLineClickJL)
rr_oneclick_jlel = treat_prob_oneclick_jlel/control_prob_oneclick_jlel

#odds ratio
odd_control_oneclick_jlel = sum(FaultyLineClickEclipseJL)/(length(FaultyLineClickEclipseJL)-sum(FaultyLineClickEclipseJL))
odd_treat_oneclick_jlel = sum(FaultyLineClickJL)/(length(FaultyLineClickJL)-sum(FaultyLineClickJL))
or_oneclick_jlel = odd_treat_oneclick_jlel/odd_control_oneclick_jlel



#Q27: Did the use of Jaguar Method lead the participants to inspect the faulty line more than using Eclipse?

# Analysis of the Effective Click Rate (ECR)
# ECR: #clicks on the faulty line / #all clicks

ecr_jml = v$Jag_FaultyLine[which(v$Jag_MethOrLine=='M')]/(v$Jag_AllLines[which(v$Jag_MethOrLine=='M')]+1e-3)
hist(ecr_jml)
ecr_jml = ecr_jml/max(ecr_jml)  # ecr_jml = taxa no jaguar method
hist(ecr_jml)
logit_ecr_jml = log((ecr_jml+1e-3)/(1-ecr_jml+1e-3))

ecr_eml = v$Jag_FaultyLineEcl[which(v$Jag_MethOrLine=='M')]/(v$Ecl_AllLines[which(v$Jag_MethOrLine=='M')]+1e-3)
hist(ecr_eml)
ecr_eml = ecr_eml/max(ecr_eml)  # ecr_eml = taxa no eclipse
hist(ecr_eml)
logit_ecr_eml = log((ecr_eml+1e-3)/(1-ecr_eml+1e-3))

#plotting both histograms
hist_ecr_jmel = hist(c(logit_ecr_jml, logit_ecr_eml))
hist_ecr_jml = hist(logit_ecr_jml, breaks=hist_ecr_jmel$breaks)
hist_ecr_eml = hist(logit_ecr_eml, breaks=hist_ecr_jmel$breaks)
freq_ecr_jmel = cbind(hist_ecr_jml$counts, hist_ecr_eml$counts)
barplot(t(freq_ecr_jmel), names.arg=hist_ecr_jmel$mids, legend.text=c('jaguarmethod', 'eclipse'), beside=TRUE)

#statistical tests
wilcox_ecr_jmel = wilcox.test(logit_ecr_jml,logit_ecr_eml,paired=TRUE)

#effect size
participants_jm = 14
zscore_ecr_jmel = qnorm(wilcox_ecr_jmel$p.value/2)
effect_size_ecr_jmel = zscore_ecr_jmel / sqrt(participants_jm*2)

#Cliff's delta Dominance
dmes(ecr_eml,ecr_jml)
delta_gr(ecr_eml,ecr_jml,paired=TRUE)

#More clicks in the faulty line than the median
#we are using the median as threshold
thres_jmel = median(c(logit_ecr_jml, logit_ecr_eml))
x_jag_jmel = as.numeric(logit_ecr_jml>thres_jmel)
y_ecl_jmel = as.numeric(logit_ecr_eml>thres_jmel)

ft_ecr_jmel = table(x_jag_jmel, y_ecl_jmel)
pvalor_ecr_jmel = fbst.ct(X=ft_ecr_jmel, ttype='diagsym', simulate.p.value=TRUE, B=300)$emp.p.value


#effect size
#risk ratio
control_prob_ecr_jmel = sum(y_ecl_jmel)/length(y_ecl_jmel)
treat_prob_ecr_jmel = sum(x_jag_jmel)/length(x_jag_jmel)
rr_ecr_jmel = treat_prob_ecr_jmel/control_prob_ecr_jmel

#odds ratio
odd_control_ecr_jmel = sum(y_ecl_jmel)/(length(y_ecl_jmel)-sum(y_ecl_jmel))
odd_treat_ecr_jmel = sum(x_jag_jmel)/(length(x_jag_jmel)-sum(x_jag_jmel))
or_ecr_jmel = odd_treat_ecr_jmel/odd_control_ecr_jmel


####At least one click in the faulty line
FaultyLineClickJM = replace(v$Jag_FaultyLine[which(v$Jag_MethOrLine=='M')],v$Jag_FaultyLine[which(v$Jag_MethOrLine=='M')]>0,1)
FaultyLineClickEclipseJM = replace(v$Jag_FaultyLineEcl[which(v$Jag_MethOrLine=='M')],v$Jag_FaultyLineEcl[which(v$Jag_MethOrLine=='M')]>0,1)

ft_oneclick_jmel = table(FaultyLineClickJM,FaultyLineClickEclipseJM)
pvalue_oneclick_jmel = fbst.ct(X=ft_oneclick_jmel, ttype='diagsym', simulate.p.value=TRUE, B=300)$emp.p.value

#effect size
#risk ratio
control_prob_oneclick_jmel = sum(FaultyLineClickEclipseJM)/length(FaultyLineClickEclipseJM)
treat_prob_oneclick_jmel = sum(FaultyLineClickJM)/length(FaultyLineClickJM)
rr_oneclick_jmel = treat_prob_oneclick_jmel/control_prob_oneclick_jmel

#odds ratio
odd_control_oneclick_jmel = sum(FaultyLineClickEclipseJM)/(length(FaultyLineClickEclipseJM)-sum(FaultyLineClickEclipseJM))
odd_treat_oneclick_jmel = sum(FaultyLineClickJM)/(length(FaultyLineClickJM)-sum(FaultyLineClickJM))
or_oneclick_jmel = odd_treat_oneclick_jmel/odd_control_oneclick_jmel



#Q28: Did the use of Jaguar Line lead the participants to inspect the faulty method more than using Eclipse?

# Analysis of the Effective Click Rate (ECR)
# ECR: #clicks on the faulty method / #all clicks

ecr_jlm = v$Jag_FaultyMeth[which(v$Jag_MethOrLine=='L')]/(v$Jag_AllMethods[which(v$Jag_MethOrLine=='L')]+1e-3)
hist(ecr_jlm)
ecr_jlm = ecr_jlm/max(ecr_jlm)  # ecr_jlm = taxa no jaguar line
hist(ecr_jlm)
logit_ecr_jlm = log((ecr_jlm+1e-3)/(1-ecr_jlm+1e-3))

ecr_elm = v$Jag_FaultyMethEcl[which(v$Jag_MethOrLine=='L')]/(v$Ecl_AllMethods[which(v$Jag_MethOrLine=='L')]+1e-3)
hist(ecr_elm)
ecr_elm = ecr_elm/max(ecr_elm)  # ecr_elm = taxa no eclipse
hist(ecr_elm)
logit_ecr_elm = log((ecr_elm+1e-3)/(1-ecr_elm+1e-3))

#plotting both histograms
hist_ecr_jlem = hist(c(logit_ecr_jlm, logit_ecr_elm))
hist_ecr_jlm = hist(logit_ecr_jlm, breaks=hist_ecr_jlem$breaks)
hist_ecr_elm = hist(logit_ecr_elm, breaks=hist_ecr_jlem$breaks)
freq_ecr_jlem = cbind(hist_ecr_jlm$counts, hist_ecr_elm$counts)
barplot(t(freq_ecr_jlem), names.arg=hist_ecr_jlem$mids, legend.text=c('jaguarline', 'eclipse'), beside=TRUE)

#statistical tests
wilcox_ecr_jlem = wilcox.test(logit_ecr_jlm,logit_ecr_elm,paired=TRUE)


#effect size
zscore_ecr_jlem = qnorm(wilcox_ecr_jlem$p.value/2)
effect_size_ecr_jlem = zscore_ecr_jlem / sqrt(participants_jl*2)

#Cliff's delta Dominance
dmes(ecr_elm,ecr_jlm)
delta_gr(ecr_elm,ecr_jlm,paired=TRUE)


#More clicks in the faulty method than the median
#we are using the median of ECR as threshold
thres_jlem = median(c(logit_ecr_jlm, logit_ecr_elm))
x_jag_jlem = as.numeric(logit_ecr_jlm>thres_jlem)
y_ecl_jlem = as.numeric(logit_ecr_elm>thres_jlem)

ft_ecr_jlem = table(x_jag_jlem, y_ecl_jlem)
pvalor_ecr_jlem = fbst.ct(X=ft_ecr_jlem, ttype='diagsym', simulate.p.value=TRUE, B=300)$emp.p.value

#effect size
#risk ratio
control_prob_ecr_jlem = sum(y_ecl_jlem)/length(y_ecl_jlem)
treat_prob_ecr_jlem = sum(x_jag_jlem)/length(x_jag_jlem)
rr_ecr_jlem = treat_prob_ecr_jlem/control_prob_ecr_jlem

#odds ratio
odd_control_ecr_jlem = sum(y_ecl_jlem)/(length(y_ecl_jlem)-sum(y_ecl_jlem))
odd_treat_ecr_jlem = sum(x_jag_jlem)/(length(x_jag_jlem)-sum(x_jag_jlem))
or_ecr_jlem = odd_treat_ecr_jlem/odd_control_ecr_jlem


####At least one click in the faulty method
FaultyMethodClickJL = replace(v$Jag_FaultyMeth[which(v$Jag_MethOrLine=='L')],v$Jag_FaultyMeth[which(v$Jag_MethOrLine=='L')]>0,1)
FaultyMethodClickEclipseJL = replace(v$Jag_FaultyMethEcl[which(v$Jag_MethOrLine=='L')],v$Jag_FaultyMethEcl[which(v$Jag_MethOrLine=='L')]>0,1)

ft_oneclick_jlem = table(FaultyMethodClickJL,FaultyMethodClickEclipseJL)
pvalue_oneclick_jlem = fbst.ct(X=ft_oneclick_jlem, ttype='diagsym', simulate.p.value=TRUE, B=300)$emp.p.value

#effect size
#risk ratio
control_prob_oneclick_jlem = sum(FaultyMethodClickEclipseJL)/length(FaultyMethodClickEclipseJL)
treat_prob_oneclick_jlem = sum(FaultyMethodClickJL)/length(FaultyMethodClickJL)
rr_oneclick_jlem = treat_prob_oneclick_jlem/control_prob_oneclick_jlem

#odds ratio
odd_control_oneclick_jlem = sum(FaultyMethodClickEclipseJL)/(length(FaultyMethodClickEclipseJL)-sum(FaultyMethodClickEclipseJL))
odd_treat_oneclick_jlem = sum(FaultyMethodClickJL)/(length(FaultyMethodClickJL)-sum(FaultyMethodClickJL))
or_oneclick_jlem = odd_treat_oneclick_jlem/odd_control_oneclick_jlem



#Q29: Did the use of Jaguar Method lead the participants to inspect the faulty method more than using Eclipse?

# Analysis of the Effective Click Rate (ECR)
# ECR: #clicks on the faulty method / #all clicks

ecr_jmm = v$Jag_FaultyMeth[which(v$Jag_MethOrLine=='M')]/(v$Jag_AllMethods[which(v$Jag_MethOrLine=='M')]+1e-3)
hist(ecr_jmm)
ecr_jmm = ecr_jmm/max(ecr_jmm)  # ecr_jmm = taxa no jaguar method
hist(ecr_jmm)
logit_ecr_jmm = log((ecr_jmm+1e-3)/(1-ecr_jmm+1e-3))

ecr_emm = v$Jag_FaultyMethEcl[which(v$Jag_MethOrLine=='M')]/(v$Ecl_AllMethods[which(v$Jag_MethOrLine=='M')]+1e-3)
hist(ecr_emm)
ecr_emm = ecr_emm/max(ecr_emm)  # ecr_emm = taxa no eclipse
hist(ecr_emm)
logit_ecr_emm = log((ecr_emm+1e-3)/(1-ecr_emm+1e-3))

#plotting both histograms
hist_ecr_jmem = hist(c(logit_ecr_jmm, logit_ecr_emm))
hist_ecr_jmm = hist(logit_ecr_jmm, breaks=hist_ecr_jmem$breaks)
hist_ecr_emm = hist(logit_ecr_emm, breaks=hist_ecr_jmem$breaks)
freq_ecr_jmem = cbind(hist_ecr_jmm$counts, hist_ecr_emm$counts)
barplot(t(freq_ecr_jmem), names.arg=hist_ecr_jmem$mids, legend.text=c('jaguarmethod', 'eclipse'), beside=TRUE)

#statistical tests
wilcox_ecr_jmem = wilcox.test(logit_ecr_jmm,logit_ecr_emm,paired=TRUE)


#effect size
zscore_ecr_jmem = qnorm(wilcox_ecr_jmem$p.value/2)
effect_size_ecr_jmem = zscore_ecr_jmem / sqrt(participants_jm*2)

#Cliff's delta Dominance
dmes(ecr_emm,ecr_jmm)
delta_gr(ecr_emm,ecr_jmm,paired=TRUE)

#More clicks in the faulty method than the median
#we are using the median of ECR as threshold
thres_jmem = median(c(logit_ecr_jmm, logit_ecr_emm))
x_jag_jmem = as.numeric(logit_ecr_jmm>thres_jmem)
y_ecl_jmem = as.numeric(logit_ecr_emm>thres_jmem)

ft_ecr_jmem = table(x_jag_jmem, y_ecl_jmem)
pvalor_ecr_jmem = fbst.ct(X=ft_ecr_jmem, ttype='diagsym', simulate.p.value=TRUE, B=300)$emp.p.value

#effect size
#risk ratio
control_prob_ecr_jmem = sum(y_ecl_jmem)/length(y_ecl_jmem)
treat_prob_ecr_jmem = sum(x_jag_jmem)/length(x_jag_jmem)
rr_ecr_jmem = treat_prob_ecr_jmem/control_prob_ecr_jmem

#odds ratio
odd_control_ecr_jmem = sum(y_ecl_jmem)/(length(y_ecl_jmem)-sum(y_ecl_jmem))
odd_treat_ecr_jmem = sum(x_jag_jmem)/(length(x_jag_jmem)-sum(x_jag_jmem))
or_ecr_jmem = odd_treat_ecr_jmem/odd_control_ecr_jmem


####At least one click in the faulty method
FaultyMethodClickJM = replace(v$Jag_FaultyMeth[which(v$Jag_MethOrLine=='M')],v$Jag_FaultyMeth[which(v$Jag_MethOrLine=='M')]>0,1)
FaultyMethodClickEclipseJM = replace(v$Jag_FaultyMethEcl[which(v$Jag_MethOrLine=='M')],v$Jag_FaultyMethEcl[which(v$Jag_MethOrLine=='M')]>0,1)

ft_oneclick_jmem = table(FaultyMethodClickJM,FaultyMethodClickEclipseJM)
pvalue_oneclick_jmem = fbst.ct(X=ft_oneclick_jmem, ttype='diagsym', simulate.p.value=TRUE, B=300)$emp.p.value

#effect size
#risk ratio
control_prob_oneclick_jmem = sum(FaultyMethodClickEclipseJM)/length(FaultyMethodClickEclipseJM)
treat_prob_oneclick_jmem = sum(FaultyMethodClickJM)/length(FaultyMethodClickJM)
rr_oneclick_jmem = treat_prob_oneclick_jmem/control_prob_oneclick_jmem

#odds ratio
odd_control_oneclick_jmem = sum(FaultyMethodClickEclipseJM)/(length(FaultyMethodClickEclipseJM)-sum(FaultyMethodClickEclipseJM))
odd_treat_oneclick_jmem = sum(FaultyMethodClickJM)/(length(FaultyMethodClickJM)-sum(FaultyMethodClickJM))
or_oneclick_jmem = odd_treat_oneclick_jmem/odd_control_oneclick_jmem


############## Effectiveness - Methods

#Q1.1 Jaguar x Eclipse - Method effectiveness analysis
tbfreqmethod = table(c(1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,0,0,0,0,0,0,0,0),c(1,1,1,1,1,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1))

pvalor_efic_method_JE = fbst.ct(X=tbfreqmethod, ttype='diagsym', 
                                simulate.p.value=TRUE, B=300)$emp.p.value

#odds ratio
odd_control_method = 8/(26-8)
odd_treat_method = 18/(26-18)
or_method_JE = odd_treat_method/odd_control_method


#Q1.1 Jaguar Line x Eclipse - Method effectiveness analysis
tbfreqmethod_jl = table(c(1,1,1,1,1,1,1,1,0,0,0,0),c(1,1,0,0,0,0,0,0,0,0,0,0))

pvalor_efic_method_JL = fbst.ct(X=tbfreqmethod_jl, ttype='diagsym', 
                                simulate.p.value=TRUE, B=300)$emp.p.value

#odds ratio
odd_control_method_jl = 2/(12-2)
odd_treat_method_jl = 8/(12-8)
or_method_JL = odd_treat_method_jl/odd_control_method_jl


#Q1.2 Jaguar Method x Eclipse - Method effectiveness analysis
tbfreqmethod_jm = table(c(1,1,1,1,1,1,1,1,1,1,0,0,0,0),c(1,1,1,1,1,0,0,0,0,0,0,0,0,1))

pvalor_efic_method_JM = fbst.ct(X=tbfreqmethod_jm, ttype='diagsym', 
                                simulate.p.value=TRUE, B=300)$emp.p.value

#odds ratio
odd_control_method_jm = 6/(14-6)
odd_treat_method_jm = 10/(14-10)
or_method_JM = odd_treat_method_jm/odd_control_method_jm

