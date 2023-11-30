

################################################################################
#------------------------------------------------------------------------------#
#   HIVCAre: ANALYSIS (SITE)                                                   #
#------------------------------------------------------------------------------#
################################################################################




#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Time-to-Event-Analysis
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

model_Output_TimeCancerToMetastasis_CoxPH <- coxph(formula = Surv(TimeCancerToMetastasis, PatientHoldsMetastasisCodes) ~ PatientSubgroup,
                                                   data = df_ADM_PatientsCancer)

model_Output_TimeCancerToMetastasis_Curve <- survfit(formula = model_Output_TimeCancerToMetastasis_CoxPH$formula,
                                                     data = df_ADM_PatientsCancer)



model_Output_TimeChemoToComplication_CoxPH <- coxph(formula = Surv(TimeChemoToFirstComplication, HadComplicationAfterChemo) ~ PatientSubgroup,
                                                    data = df_ADM_PatientsCancer)

model_Output_TimeChemoToComplication_Curve <- survfit(formula = model_Output_TimeChemoToComplication_CoxPH$formula,
                                                      data = df_ADM_PatientsCancer)


