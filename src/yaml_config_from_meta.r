yaml_config_from_meta <- function(settingsFile, calibrationFile){
  
  #Description: Produce YAML configuration file for VICGL simulations
  
  #Arguments:
  # settingsFile    - YAML file containing global metadata for model settings
  # calibrationFile - YAML file containing calibration settings by domain and basin
  
  library(yaml)
  
  #Read YAML files into list objects
  settings <- read_yaml(settingsFile)
  calib <-    read_yaml(calibrationFile)
  
  #Specify directory containing configuration files
  settingsDir <- "/Users/mschnorbus/Documents/GitHub/configuration_files/model_settings"
  
  #Construct list of configuration file dependencies
  configlist <- list()
  configlist$model_settings_key_VICGL  <- file.path(settingsDir, "model_configurations_VICGL.yml")
  configlist$model_settings_key_RVIC   <- file.path(settingsDir, "model_configurations_RVIC.yml")
  configlist$model_settings_key_RGM    <- file.path(settingsDir, "model_configurations_RGM.yml")
  configlist$model_settings_key_HyCond <- file.path(settingsDir, "model_configurations_HyCond.yml")
  configlist$domain_description_key       <- file.path(settingsDir, "HI_theme_domains.yml")
  configlist$vicgl_calibration_configuration_key <- file.path(settingsDir, "vicgl_calibration_configurations.yml")
  
  # Specify main VICGL input directory for parameter files
  inputDir <- settings$model_settings$input_dir
  
  # Loop through domains in settings and create output list
  domains <- names(settings$model_settings$parameter_settings_by_domain)
  outlist <- list()
  for(d in 1:length(domains)){
    #Loop through basins
    #Index to match domain in calibration metadata list
    dind <- match(domains[d], names(calib))
    cat("Processing domain ", domains[d], "\n")
    basins <- names(calib[[dind]])
    #Create sub-list to hold settings for all basins for a given domain
    tmp2 <- list()
    for(b in 1:length(basins)){
      #Index to match basin in calibration metadata domain sub-list
      bind <- match(basins[b], names(calib[[dind]]))
      basin <- names(calib[[dind]][bind])
      cat("  processing basin ", basin, "\n")
      # Set glacier flag
      isglac <- calib[[dind]][[bind]]$glacier
      # Set list index for glacier state; assumes input file always places 'dynamics_true'
      # node before 'dynamics_false' node
      gind <- ifelse(isglac,1,2)
      # Subset settings list - makes subsequent code shorter
      subsettings <- settings$model_settings$parameter_settings_by_domain[[d]][[gind]]
      # Populate info for each basin into a sub-sub-list
      tmp2[[b]] <- list(dynamic_glaciers = isglac,
                        VICGL_model_configuration =
                          settings$model_settings$model_configuration[[gind]]$VICGL_model_configuration,
                        HydCnd_model_configuration =
                          settings$model_settings$model_configuration[[gind]]$HydCnd_model_configuration,
                        RGM_model_configuration =
                          settings$model_settings$model_configuration[[gind]]$RGM_model_configuration,
                        VICGL_calibration_configuration =
                          settings$model_settings$model_configuration[[gind]]$VICGL_calibration_configuration,
                        VICGL_calibration_parameter_selection = list(
                          run_id = calib[[dind]][[bind]]$run_id,
                          run_no = calib[[dind]][[bind]]$run_no
                        ),
                        initialization_method = subsettings$initialization_method,
                        parameter_files=list(
                          soil_parameters = 
                            gsub("{BASIN}", basin, file.path(inputDir, subsettings$soil_parameters),fixed=TRUE),
                          vegetation_library = file.path(inputDir, subsettings$vegetation_library),
                          vegetation_parameters = file.path(inputDir, subsettings$vegetation_parameters),
                          snowband_parameters = file.path(inputDir, subsettings$snowband_parameters),
                          surf_dem = 
                            ifelse(isglac,
                                   gsub("{BASIN}", basin,
                                        file.path(inputDir, subsettings$surf_dem), fixed = TRUE),
                                   NA),
                          bed_dem = 
                            ifelse(isglac,
                                   gsub("{BASIN}", basin,
                                        file.path(inputDir, subsettings$bed_dem), fixed = TRUE),
                                   NA),
                          glac_mask = 
                            ifelse(isglac,
                                   gsub("{BASIN}", basin,
                                        file.path(inputDir, subsettings$glac_mask), fixed = TRUE),
                                   NA),
                          pixel_map = 
                            ifelse(isglac,
                                   gsub("{BASIN}", basin,
                                        file.path(inputDir, subsettings$pixel_map), fixed = TRUE),
                                   NA)
                        ))
    }
    names(tmp2) <- basins
    dname <- paste(settings$model_settings$parameter_settings_by_domain[[d]]$nested_domain,
                   domains[d], sep="__")
    outlist[[dname]] <- tmp2[sort(names(tmp2))]
  }
  return(list(
    title = "HI Theme model run metadata",
    author = settings$author,
    contact = settings$contact,
    institution = settings$institution,
    write_date = date(),
    input_configurations = list(model_metadata =settingsFile, calibration_metadata = calibrationFile),
    configuration_dependency = configlist,
    configuration_by_domain = outlist[sort(names(outlist))]))
}
