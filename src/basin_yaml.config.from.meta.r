basin_yaml.config.from.meta <- function(basin,
                                        domain,
                                        settingsFile,
                                        calibrationFile,
                                        outFile=paste("./", basin, ".yml",sep="")){
  
  #DESCRIPTION: Produce basin-specific configuration metadata for VICGL simulations
  
  #AARGUMENTS:
  # basin           - target basin
  # domain -        - domain containing basin
  # settingsFile    - YAML template file containing global model configuration settings
  # calibrationFile - YAML file containing calibration run information by domain and basin
  # outFile         - output file name (including path)
  
  #VALUE:
  # List of VICGL configuration metadata. Side effect is to write list as YAML file
  
  library(yaml)
  
  #Read YAML files into list objects
  settings <- read_yaml(settingsFile)
  calib <-    read_yaml(calibrationFile)
  
  #Specify directory containing configuration files
  settingsDir <- "https://github.com/mschnorb/configuration_files/tree/main/model_settings"
  
  #Construct list of configuration file dependencies
  configlist <- list()
  configlist$model_settings_key_VICGL  <- file.path(settingsDir, "model_settings_VICGL.yml")
  configlist$model_settings_key_RVIC   <- file.path(settingsDir, "model_settings_RVIC.yml")
  configlist$model_settings_key_RGM    <- file.path(settingsDir, "model_settings_RGM.yml")
  configlist$model_settings_key_HyCond <- file.path(settingsDir, "model_settings_HyCond.yml")
  configlist$domain_description_key <- "https://github.com/mschnorb/configuration_files/blob/main/misc_attributes/HI_theme_domains.yml"
  configlist$vicgl_calibration_configuration_key <- "https://github.com/mschnorb/configuration_files/blob/main/calibration_settings/vicgl_calibration_configurations.yml"
  
  # Specify main VICGL input directory for parameter files
  inputDir <- settings$model_settings$input_dir
  isglac <- calib[[domain]][[basin]]$glacier
  gind   <- ifelse(isglac,1,2)
  params <- settings$model_settings$parameter_settings_by_domain[[domain]][[gind]]

  config = list(
    dynamic_glaciers=ifelse(gind==1,"yes","no"),
    VICGL_model_settings = settings$model_settings$model_configuration[[gind]]$VICGL_model_configuration,
    HyCond_model_settings = settings$model_settings$model_configuration[[gind]]$HydCnd_model_configuration,
    RGM_model_settings = settings$model_settings$model_configuration[[gind]]$RGM_model_configuration,
    VICGL_calibration_configuration = settings$model_settings$model_configuration[[gind]]$VICGL_calibration_configuration,
    VICGL_calibration_parameter_selection = list(
      run_id = calib[[domain]][[basin]]$run_id,
      run_no = calib[[domain]][[basin]]$run_no),
    initialization_method = params$initialization_method,
    parameter_files = list(
      soil_parameters = gsub("{BASIN}", basin, file.path(inputDir, params$soil_parameters),fixed=TRUE),
      vegetation_library = file.path(inputDir, params$vegetation_library),
      vegetation_parameters = 
        ifelse(isglac,
               gsub("{BASIN}", basin,
                    file.path(inputDir, params$vegetation_parameters), fixed = TRUE),
               NA),
      snowband_parameters = 
        ifelse(isglac,
               gsub("{BASIN}", basin,
                    file.path(inputDir, params$snowband_parameters), fixed = TRUE),
               NA),
      surf_dem = 
        ifelse(isglac,
               gsub("{BASIN}", basin,
                    file.path(inputDir, params$surf_dem), fixed = TRUE),
               NA),
      bed_dem = 
        ifelse(isglac,
               gsub("{BASIN}", basin,
                    file.path(inputDir, params$bed_dem), fixed = TRUE),
               NA),
      glac_mask = 
        ifelse(isglac,
               gsub("{BASIN}", basin,
                    file.path(inputDir, params$glac_mask), fixed = TRUE),
               NA),
      pixel_map = 
        ifelse(isglac,
               gsub("{BASIN}", basin,
                    file.path(inputDir, params$pixel_map), fixed = TRUE),
               NA))
  )

  rslt <- list(
    title = "HI Theme model run metadata",
    author = settings$author,
    contact = settings$contact,
    institution = settings$institution,
    write_date = date(),
    basin = basin,
    nested_domain = paste(settings$model_settings$parameter_settings_by_domain[[domain]]$nested_domain, domain, sep="__"),
    input_configurations = list(model_metadata =settingsFile, calibration_metadata = calibrationFile),
    configuration_dependency = configlist,
    model_configuration = config)
  
  write_yaml(rslt, file = outFile)
  
  return(rslt)
}
