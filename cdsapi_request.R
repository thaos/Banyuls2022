library(reticulate)
# use_python("/usr/bin/python3.9")

cdsapi <- import("cdsapi")

c <- cdsapi$Client()

# c$retrieve(
#     'projections-cordex-domains-single-levels',
#     list(
#         'domain' = 'europe',
#         'experiment' = 'historical',
#         'horizontal_resolution' = '0_11_degree_x_0_11_degree',
#         'temporal_resolution' = 'daily_mean',
#         'variable' = c(
#             '10m_wind_speed', '2m_air_temperature', '2m_surface_specific_humidity',
#             'maximum_2m_temperature_in_the_last_24_hours', 'mean_precipitation_flux', 'minimum_2m_temperature_in_the_last_24_hours'
#         ),
#         'gcm_model' = 'ipsl_cm5a_mr',
#         'rcm_model' = 'ipsl_wrf381p',
#         'ensemble_member' = 'r1i1p1',
#         'start_year' = '1951',
#         'end_year' = '1955',
#         'format' = 'tgz'
#     ),
#     'download.tar.gz'
# )

for (variable in c(
	'10m_wind_speed', '2m_air_temperature', '2m_surface_specific_humidity',
	'maximum_2m_temperature_in_the_last_24_hours', 'mean_precipitation_flux', 'minimum_2m_temperature_in_the_last_24_hours'
)) { 
	print(variable)
	for (startyear in seq(1951, 2001, by = 5)){
		endyear <- startyear + 4
		print(startyear)
		c$retrieve(
			   'projections-cordex-domains-single-levels',
			   list(
				'domain' = 'europe',
				'experiment' = 'historical',
				'horizontal_resolution' = '0_11_degree_x_0_11_degree',
				'temporal_resolution'= 'daily_mean',
				'variable'= variable,
				'gcm_model' = 'ipsl_cm5a_mr',
				'rcm_model' = 'ipsl_wrf381p',
				'ensemble_member' = 'r1i1p1',
				'start_year' = as.character(startyear),
				'end_year' = as.character(endyear),
				'format'= 'tgz'
				),
			   paste0(variable, "_", startyear, '.tar.gz')
		)
	}
}
